#!/usr/bin/env python3
"""reddigg websocket server.

This is the Python half of the reddigg browser bridge.  It owns a
websocket connection to one or more reddit tabs running
``userscript/reddigg-ws.user.js`` and exposes a small CRUD API to
whatever wants to drive reddit (Emacs, curl, another script...).

The browser tab is the only thing that can actually talk to reddit
with the user's real, logged-in session, so this server never touches
reddit itself: it just relays typed requests to the tab and returns
the JSON the tab produced.  That mirrors reddigg.el's browser-gt path
(``EVAL_IN_ACTIVE_TAB`` + same-origin ``fetch``), except the transport
is a websocket on localhost:1979 instead of browser-gt.

Requires ``websockets`` >= 10.1 (works with both the legacy and the
new asyncio API, including 13/14/15).

Protocol (JSON, one object per websocket message)
-------------------------------------------------

server -> page (request)::

    {"id": "<uuid>", "action": "get|session|ping|vote|comment|edit|
                                delete|submit", "params": {...}}

page -> server (response)::

    {"id": "<uuid>", "ok": true,  "result": <json>}
    {"id": "<uuid>", "ok": false, "error": "message"}

page -> server (unsolicited, on connect and every 20s)::

    {"type": "hello", "url": ..., "user": ..., "modhash": ...}

The server multiplexes all connected tabs through one request/response
coroutine (:meth:`Bridge.call`).  If more than one tab is connected,
the most recently-hello'd one is used by default; pass ``client`` to
pick a specific connection.

Usage
-----

    python3 server/reddigg_server.py            # listen on 127.0.0.1:1979
    python3 server/reddigg_server.py --port 1979 --host 127.0.0.1

As a library::

    from reddigg_server import Bridge, serve
    bridge = Bridge()
    asyncio.create_task(serve(bridge, host="127.0.0.1", port=1979))
    data = await bridge.call("get", {"path": "/r/emacs.json?count=25"})
    await bridge.call("vote", {"id": "t3_abc", "dir": 1})
    await bridge.call("comment", {"parent": "t3_abc", "text": "hi"})
"""

from __future__ import annotations

import argparse
import asyncio
import concurrent.futures
import json
import logging
import threading
import time
import uuid
from dataclasses import dataclass, field
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from typing import Any, Dict, Optional
from urllib.parse import parse_qs, urlparse

import websockets

log = logging.getLogger("reddigg")

DEFAULT_HOST = "127.0.0.1"
DEFAULT_PORT = 1979
DEFAULT_REST_PORT = 1980
REST_ROOT = "/reddigg"

# Actions the userscript knows how to perform.  Kept in sync with
# ``userscript/reddigg-ws.user.js``.
ACTIONS = {
    "ping",
    "session",
    "get",
    "vote",
    "comment",
    "edit",
    "delete",
    "submit",
}


class ReddiggError(RuntimeError):
    """Raised when a relayed action fails on the browser side."""


class NoClientError(ReddiggError):
    """Raised when a request is made but no reddit tab is connected."""


class TimeoutError(ReddiggError):  # noqa: A001 - intentional public name
    """Raised when the browser does not answer in time."""


@dataclass
class Client:
    """A single connected reddit tab."""

    ws: Any  # a websockets connection (legacy or new asyncio API)
    id: str = field(default_factory=lambda: uuid.uuid4().hex[:8])
    url: Optional[str] = None
    user: Optional[str] = None
    modhash: Optional[str] = None
    last_seen: float = field(default_factory=time.time)

    def describe(self) -> Dict[str, Any]:
        return {
            "id": self.id,
            "url": self.url,
            "user": self.user,
            "logged_in": bool(self.user),
        }


class Bridge:
    """Multiplexes action requests over the connected reddit tabs.

    One :class:`Bridge` may have many tabs connected.  Requests are sent
    to a chosen tab (most-recent by default) and awaited by ``id``.
    """

    def __init__(self) -> None:
        self._clients: Dict[str, Client] = {}
        self._pending: Dict[str, asyncio.Future] = {}
        self._default: Optional[str] = None
        self._lock = asyncio.Lock()

    # -- connection bookkeeping -------------------------------------------

    async def register(self, ws: Any) -> Client:
        client = Client(ws=ws)
        async with self._lock:
            self._clients[client.id] = client
            self._default = client.id
        log.info("client %s connected (%d total)", client.id, len(self._clients))
        return client

    async def unregister(self, client: Client) -> None:
        async with self._lock:
            self._clients.pop(client.id, None)
            if self._default == client.id:
                self._default = next(iter(self._clients), None)
        log.info("client %s disconnected (%d left)", client.id, len(self._clients))

    def clients(self):
        """Snapshot of connected tabs."""
        return list(self._clients.values())

    def default_client(self) -> Optional[Client]:
        if self._default and self._default in self._clients:
            return self._clients[self._default]
        return next(iter(self._clients.values()), None)

    # -- message handling --------------------------------------------------

    async def on_message(self, client: Client, raw: str) -> None:
        try:
            msg = json.loads(raw)
        except (ValueError, TypeError):
            log.warning("client %s sent non-JSON: %r", client.id, raw[:200])
            return

        # Unsolicited "hello" keeps our view of the tab fresh.
        if msg.get("type") == "hello":
            client.url = msg.get("url")
            client.user = msg.get("user")
            client.modhash = msg.get("modhash")
            client.last_seen = time.time()
            log.debug("hello from %s: user=%s url=%s", client.id, client.user, client.url)
            return

        req_id = msg.get("id")
        if not req_id:
            log.warning("client %s sent message without id: %r", client.id, msg)
            return

        fut = self._pending.pop(req_id, None)
        if fut is None or fut.done():
            log.debug("late/unknown response id=%s from %s", req_id, client.id)
            return
        if msg.get("ok"):
            fut.set_result(msg.get("result"))
        else:
            fut.set_exception(ReddiggError(msg.get("error") or "unknown browser error"))

    # -- request/response --------------------------------------------------

    async def call(
        self,
        action: str,
        params: Optional[Dict[str, Any]] = None,
        *,
        client: Optional[str] = None,
        timeout: float = 30.0,
    ) -> Any:
        """Send ACTION with PARAMS to a tab and return its JSON result.

        Raises :class:`NoClientError`, :class:`TimeoutError`, or
        :class:`ReddiggError` (from the browser) as appropriate.
        """
        if action not in ACTIONS:
            raise ValueError(f"unknown action: {action!r} (expected one of {sorted(ACTIONS)})")

        if client is not None:
            target = self._clients.get(client)
            if target is None:
                raise NoClientError(f"no connected client with id {client!r}")
        else:
            target = self.default_client()
        if target is None:
            raise NoClientError(
                "no reddit tab is connected; open old.reddit.com with the "
                "reddigg-ws userscript enabled"
            )

        req_id = uuid.uuid4().hex
        loop = asyncio.get_running_loop()
        fut: asyncio.Future = loop.create_future()
        self._pending[req_id] = fut

        payload = {"id": req_id, "action": action, "params": params or {}}
        try:
            await target.ws.send(json.dumps(payload))
            return await asyncio.wait_for(fut, timeout=timeout)
        except asyncio.TimeoutError as exc:
            raise TimeoutError(
                f"timed out after {timeout}s waiting for {action} on client {target.id}"
            ) from exc
        finally:
            self._pending.pop(req_id, None)

    # -- convenience wrappers (the CRUD API) -------------------------------

    async def ping(self, **kw) -> Any:
        return await self.call("ping", {}, **kw)

    async def session(self, **kw) -> Any:
        """Return {"modhash": ..., "user": ...} from the tab."""
        return await self.call("session", {}, **kw)

    async def get(self, path: str, **kw) -> Any:
        """GET a reddit JSON path (e.g. ``/r/emacs.json?count=25``)."""
        return await self.call("get", {"path": path}, **kw)

    async def vote(self, thing_id: str, direction: int, **kw) -> Any:
        """Vote on THING_ID: direction 1 up, -1 down, 0 clear."""
        return await self.call("vote", {"id": thing_id, "dir": direction}, **kw)

    async def comment(self, parent: str, text: str, **kw) -> Any:
        """Reply to PARENT (a fullname like ``t3_abc``/``t1_abc``) with TEXT."""
        return await self.call("comment", {"parent": parent, "text": text}, **kw)

    async def edit(self, thing_id: str, text: str, **kw) -> Any:
        """Replace the body of THING_ID with TEXT."""
        return await self.call("edit", {"id": thing_id, "text": text}, **kw)

    async def delete(self, thing_id: str, **kw) -> Any:
        """Delete THING_ID (post or comment)."""
        return await self.call("delete", {"id": thing_id}, **kw)

    async def submit(
        self,
        subreddit: str,
        title: str,
        *,
        kind: str = "self",
        text: str = "",
        url: str = "",
        **kw,
    ) -> Any:
        """Create a submission in SUBREDDIT.

        KIND is ``"self"`` (text post, uses TEXT) or ``"link"``
        (link post, uses URL).
        """
        return await self.call(
            "submit",
            {
                "subreddit": subreddit,
                "title": title,
                "kind": kind,
                "text": text,
                "url": url,
            },
            **kw,
        )


# ---------------------------------------------------------------------------
# websocket server glue
# ---------------------------------------------------------------------------


async def handler(bridge: Bridge, ws: Any) -> None:
    client = await bridge.register(ws)
    try:
        async for raw in ws:
            await bridge.on_message(client, raw)
    except websockets.ConnectionClosed:
        pass
    finally:
        await bridge.unregister(client)


async def serve(
    bridge: Optional[Bridge] = None,
    host: str = DEFAULT_HOST,
    port: int = DEFAULT_PORT,
) -> None:
    """Run the websocket server until cancelled."""
    bridge = bridge or Bridge()

    # One-argument handler: accepted by the legacy API (websockets >= 10.1)
    # and required by the new asyncio API (websockets >= 13), where the
    # old ``(ws, path)`` signature no longer works.
    async def ws_handler(ws: Any) -> None:
        await handler(bridge, ws)

    async with websockets.serve(ws_handler, host, port):
        log.info("reddigg ws server listening on ws://%s:%d", host, port)
        await asyncio.Future()  # run forever


# ---------------------------------------------------------------------------
# HTTP/REST glue
# ---------------------------------------------------------------------------

def _route(path: str) -> Optional[str]:
    """Map an HTTP path to a bridge action name.

    Returns the bridge action name for ``/reddigg/<action>``, or
    ``None`` if the path is not a known REST route.  All state-changing
    verbs take their arguments as JSON in the request body, so only the
    read-only ``ping``/``session``/``get`` are meaningful via GET.
    """
    if not path.startswith(REST_ROOT + "/"):
        return None
    name = path[len(REST_ROOT) + 1 :].strip("/")
    if name in {"ping", "session", "get"}:
        return name
    if name in {"vote", "comment", "edit", "delete", "submit"}:
        return name
    return None


class _RESTHandler(BaseHTTPRequestHandler):
    """One HTTP request handler; bridges onto the asyncio Bridge.

    Runs on a worker thread inside :class:`ThreadingHTTPServer`.  Every
    call is handed to the asyncio event loop via
    ``asyncio.run_coroutine_threadsafe`` and its result awaited from
    this thread.  ``server.bridge`` and ``server.loop`` are set by
    :class:`RESTServer`.
    """

    server_version = "reddigg/0.1"

    # keep the terminal tidy: log through our logger, not stderr
    def log_message(self, fmt: str, *args: Any) -> None:  # noqa: A003
        log.debug("rest %s - %s", self.address_string(), fmt % args)

    # -- helpers ----------------------------------------------------------

    def _send(self, code: int, payload: Any) -> None:
        body = json.dumps(payload).encode("utf-8")
        self.send_response(code)
        self.send_header("Content-Type", "application/json; charset=utf-8")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def _read_body(self) -> Dict[str, Any]:
        length = int(self.headers.get("Content-Length") or 0)
        if length <= 0:
            return {}
        raw = self.rfile.read(length)
        try:
            data = json.loads(raw)
        except (ValueError, TypeError):
            raise ValueError("request body is not valid JSON")
        if not isinstance(data, dict):
            raise ValueError("request body must be a JSON object")
        return data

    def _dispatch(self, action: str, params: Dict[str, Any], query: Dict[str, Any]) -> None:
        server = self.server  # type: ignore[assignment]
        if action == "get":
            # /reddigg/get?path=/r/emacs.json?count=5 : the whole reddit
            # path (including its own query) arrives as one `path` value.
            params.setdefault("path", query.get("path", ""))

        async def _run() -> Any:
            return await server.bridge.call(action, params, timeout=server.timeout)

        fut = asyncio.run_coroutine_threadsafe(_run(), server.loop)
        try:
            # Slightly longer than the bridge's own timeout, so the bridge
            # normally reports the timeout itself (as our TimeoutError).
            result = fut.result(timeout=server.timeout + 5)
        except concurrent.futures.TimeoutError:
            # Backstop: the coroutine did not finish even after the bridge
            # timeout.  This is a *different* class from the module's own
            # TimeoutError below, so it needs its own handler -- otherwise
            # the worker thread dies and the client never gets a reply.
            fut.cancel()
            self._send(504, {"error": f"timed out waiting for {action}"})
            return
        except ValueError as exc:
            self._send(400, {"error": str(exc)})
            return
        except NoClientError as exc:
            self._send(503, {"error": str(exc)})
            return
        except TimeoutError as exc:
            self._send(504, {"error": str(exc)})
            return
        except ReddiggError as exc:
            self._send(502, {"error": str(exc)})
            return
        except Exception as exc:  # noqa: BLE001 - never leave the client hanging
            log.exception("unexpected error handling %s", action)
            self._send(500, {"error": f"internal error: {exc}"})
            return
        self._send(200, {"ok": True, "result": result})

    # -- verbs ------------------------------------------------------------

    def do_GET(self) -> None:  # noqa: N802 - http.server API
        parsed = urlparse(self.path)
        action = _route(parsed.path)
        if action is None:
            self._send(404, {"error": "not found"})
            return
        query = {k: v[0] for k, v in parse_qs(parsed.query).items()}
        self._dispatch(action, {}, query)

    def do_POST(self) -> None:  # noqa: N802 - http.server API
        parsed = urlparse(self.path)
        action = _route(parsed.path)
        if action is None:
            self._send(404, {"error": "not found"})
            return
        try:
            params = self._read_body()
        except ValueError as exc:
            self._send(400, {"error": str(exc)})
            return
        query = {k: v[0] for k, v in parse_qs(parsed.query).items()}
        self._dispatch(action, params, query)


class RESTServer:
    """Serves the :class:`Bridge` CRUD API over HTTP on a worker thread.

    ``ThreadingHTTPServer`` is not asyncio-aware, so it is run in a
    daemon thread and each request is marshalled back onto the event
    loop that owns the bridge (``loop``) with
    ``asyncio.run_coroutine_threadsafe``.
    """

    def __init__(
        self,
        bridge: Bridge,
        loop: asyncio.AbstractEventLoop,
        host: str = DEFAULT_HOST,
        port: int = DEFAULT_REST_PORT,
        timeout: float = 30.0,
    ) -> None:
        self.bridge = bridge
        self.loop = loop
        self.timeout = timeout
        self._httpd = ThreadingHTTPServer((host, port), _RESTHandler)
        self._httpd.bridge = bridge  # type: ignore[attr-defined]
        self._httpd.loop = loop  # type: ignore[attr-defined]
        self._httpd.timeout = timeout  # type: ignore[attr-defined]
        self._thread = threading.Thread(
            target=self._httpd.serve_forever,
            name="reddigg-rest",
            daemon=True,
        )

    def start(self) -> None:
        self._thread.start()
        log.info(
            "reddigg rest server listening on http://%s:%d%s",
            *self._httpd.server_address[:2],
            REST_ROOT,
        )

    def stop(self) -> None:
        self._httpd.shutdown()
        self._httpd.server_close()


# ---------------------------------------------------------------------------
# optional demo REPL: drive reddit from the terminal
# ---------------------------------------------------------------------------


async def _repl(bridge: Bridge) -> None:
    """Tiny interactive driver so you can poke the API by hand."""
    help_text = """commands:
  ping
  session
  get <path>                 e.g. get /r/emacs.json?count=5
  vote <id> <dir>            dir = 1 | -1 | 0
  comment <parent> <text>
  edit <id> <text>
  delete <id>
  submit <sr> <title> [url]   (with a url -> link post)
  clients
  quit
"""
    print(help_text)
    while True:
        try:
            line = await asyncio.to_thread(input, "reddigg> ")
        except (EOFError, KeyboardInterrupt):
            print()
            return
        line = line.strip()
        if not line:
            continue
        if line in {"quit", "exit", "q"}:
            return
        if line == "help":
            print(help_text)
            continue
        if line == "clients":
            for c in bridge.clients():
                print(" ", c.describe())
            continue

        parts = line.split(maxsplit=3)
        cmd = parts[0]
        try:
            if cmd == "ping":
                print(await bridge.ping())
            elif cmd == "session":
                print(await bridge.session())
            elif cmd == "get":
                print(json.dumps(await bridge.get(parts[1]), indent=2)[:2000])
            elif cmd == "vote":
                print(await bridge.vote(parts[1], int(parts[2])))
            elif cmd == "comment":
                print(await bridge.comment(parts[1], parts[2]))
            elif cmd == "edit":
                print(await bridge.edit(parts[1], parts[2]))
            elif cmd == "delete":
                print(await bridge.delete(parts[1]))
            elif cmd == "submit":
                sr, title = parts[1], parts[2]
                url = parts[3] if len(parts) > 3 else ""
                kind = "link" if url else "self"
                print(await bridge.submit(sr, title, kind=kind, text=title if not url else "", url=url))
            else:
                print("unknown command; try 'help'")
        except (ReddiggError, IndexError, ValueError) as exc:
            print(f"error: {exc}")


async def amain(args: argparse.Namespace) -> None:
    bridge = Bridge()
    server_task = asyncio.create_task(serve(bridge, args.host, args.port))

    rest: Optional[RESTServer] = None
    if not args.no_rest:
        rest = RESTServer(
            bridge,
            asyncio.get_running_loop(),
            host=args.host,
            port=args.rest_port,
        )
        rest.start()

    try:
        if args.repl:
            await _repl(bridge)
        else:
            await server_task
    finally:
        if rest is not None:
            rest.stop()
        server_task.cancel()


def main(argv: Optional[list] = None) -> None:
    parser = argparse.ArgumentParser(description="reddigg websocket bridge server")
    parser.add_argument("--host", default=DEFAULT_HOST)
    parser.add_argument("--port", type=int, default=DEFAULT_PORT)
    parser.add_argument(
        "--rest-port",
        type=int,
        default=DEFAULT_REST_PORT,
        help=f"port for the HTTP/REST API (default {DEFAULT_REST_PORT})",
    )
    parser.add_argument(
        "--no-rest",
        action="store_true",
        help="do not start the HTTP/REST API",
    )
    parser.add_argument(
        "--repl",
        action="store_true",
        help="also start an interactive prompt to drive the API by hand",
    )
    parser.add_argument(
        "-v", "--verbose", action="store_true", help="debug logging"
    )
    args = parser.parse_args(argv)

    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.INFO,
        format="%(asctime)s %(levelname)s %(name)s: %(message)s",
    )
    try:
        asyncio.run(amain(args))
    except KeyboardInterrupt:
        log.info("shutting down")


if __name__ == "__main__":
    main()
