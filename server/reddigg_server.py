#!/usr/bin/env python3
"""reddigg server: relays typed requests to the reddigg userscript running
in a logged-in reddit tab.  One port, one event loop (aiohttp):

    ws://127.0.0.1:1979/ws                    the userscript connects here
                                              (the root path "/" works too, so
                                              the userscript needs no change)
    GET  http://127.0.0.1:1979/reddigg/tabs     list connected tabs
    GET  http://127.0.0.1:1979/reddigg/ping
    GET  http://127.0.0.1:1979/reddigg/session
    GET  http://127.0.0.1:1979/reddigg/get?path=/r/emacs.json%3Fcount%3D25
    POST http://127.0.0.1:1979/reddigg/vote     {"id": "t3_abc", "dir": 1}
    POST http://127.0.0.1:1979/reddigg/comment  {"parent": "t3_abc", "text": "hi"}
    POST http://127.0.0.1:1979/reddigg/edit     {"id": "t1_abc", "text": "..."}
    POST http://127.0.0.1:1979/reddigg/delete   {"id": "t1_abc"}
    POST http://127.0.0.1:1979/reddigg/submit   {"subreddit": ..., "title": ...,
                                                 "kind": "self|link", "text": ...,
                                                 "url": ...}

Every REST route also accepts ``?tab=<id>`` to pick a specific tab and
``?timeout=<seconds>`` (max 120).  Replies are ``{"ok": true, "result": ...}``
or ``{"error": "..."}`` with a 4xx/5xx status.

The browser tab is the only thing that can talk to reddit with the user's
real, logged-in session, so this server never touches reddit itself: it
relays requests to the tab and returns the JSON the tab produced.

Protocol (JSON, one object per websocket message; unchanged)
------------------------------------------------------------

server -> page (request)::

    {"id": "<uuid>", "action": "get|session|ping|vote|comment|edit|
                                delete|submit", "params": {...}}

page -> server (response)::

    {"id": "<uuid>", "ok": true,  "result": <json>}
    {"id": "<uuid>", "ok": false, "error": "message"}

page -> server (unsolicited, on connect and every 20s)::

    {"type": "hello", "url": ..., "user": ..., "modhash": ...}

If more than one tab is connected, the most recently seen one is used
by default.

Security model:
  * binds to 127.0.0.1 only
  * REST requires a secret token header (X-Bridge-Token), a Host header
    naming this server (blocks DNS rebinding), and no Origin header
    (browsers always send Origin on cross-origin requests; curl and Emacs
    don't), so web pages can't drive it
  * the websocket handshake checks Origin against an allow-list (browsers
    can't forge Origin; other local processes can, which is accepted for a
    single-user machine)

Requires: aiohttp >= 3.9
"""

from __future__ import annotations

import argparse
import asyncio
import json
import logging
import os
import re
import secrets
import time
import uuid
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Dict, List, Optional, Set, Tuple

from aiohttp import WSMsgType, web

log = logging.getLogger("reddigg")

DEFAULT_HOST = "127.0.0.1"
DEFAULT_PORT = 1979
WS_PATHS = ("/ws", "/")  # "/" so a userscript pointing at ws://host:port keeps working
REST_ROOT = "/reddigg"
MAX_WS_MESSAGE = 2**24  # 16 MiB; aiohttp's 4 MiB default can be too small for big listings
TOKEN_FILE = Path(
    os.environ.get(
        "REDDIGG_TOKEN_FILE",
        str(Path.home() / ".config" / "reddigg" / "token"),
    )
)
DEFAULT_ORIGINS = [r"https://([a-z0-9-]+\.)?reddit\.com"]

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
READ_ONLY = {"ping", "session", "get"}
WRITE = {"vote", "comment", "edit", "delete", "submit"}


class ReddiggError(RuntimeError):
    """The browser side reported an error."""


class NoClientError(ReddiggError):
    """No matching tab is connected."""


class ReddiggTimeout(ReddiggError):
    """The tab did not answer in time."""


def load_token() -> str:
    if TOKEN_FILE.exists():
        return TOKEN_FILE.read_text().strip()
    TOKEN_FILE.parent.mkdir(parents=True, exist_ok=True)
    token = secrets.token_urlsafe(32)
    fd = os.open(TOKEN_FILE, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
    with os.fdopen(fd, "w") as f:
        f.write(token + "\n")
    return token


@dataclass
class Client:
    """A single connected reddit tab."""

    ws: web.WebSocketResponse
    id: str = field(default_factory=lambda: uuid.uuid4().hex[:8])
    url: Optional[str] = None
    user: Optional[str] = None
    modhash: Optional[str] = None
    last_seen: float = field(default_factory=time.time)
    pending: Set[str] = field(default_factory=set)

    def describe(self) -> Dict[str, Any]:
        return {
            "id": self.id,
            "url": self.url,
            "user": self.user,
            "logged_in": bool(self.user),
        }


class Bridge:
    """Multiplexes action requests over the connected reddit tabs."""

    def __init__(self) -> None:
        self._clients: Dict[str, Client] = {}
        # request id -> (client id, future); the client id lets us reject
        # responses that come from a tab the request was never sent to.
        self._pending: Dict[str, Tuple[str, asyncio.Future]] = {}

    # -- connections -------------------------------------------------------

    def register(self, ws: web.WebSocketResponse) -> Client:
        client = Client(ws=ws)
        self._clients[client.id] = client
        log.info("tab %s connected (%d total)", client.id, len(self._clients))
        return client

    def unregister(self, client: Client) -> None:
        self._clients.pop(client.id, None)
        # Fail in-flight requests immediately instead of letting them
        # sit until their timeout.
        for rid in list(client.pending):
            entry = self._pending.pop(rid, None)
            if entry and not entry[1].done():
                entry[1].set_exception(NoClientError("tab disconnected"))
        log.info("tab %s disconnected (%d left)", client.id, len(self._clients))

    def clients(self) -> List[Client]:
        return list(self._clients.values())

    def _pick(self, tab: Optional[str]) -> Client:
        if tab:
            c = self._clients.get(tab)
            if c is None:
                raise NoClientError(f"no connected tab with id {tab!r}")
            return c
        if not self._clients:
            raise NoClientError(
                "no reddit tab is connected; open old.reddit.com with the "
                "reddigg-ws userscript enabled"
            )
        return max(self._clients.values(), key=lambda c: c.last_seen)

    # -- incoming ----------------------------------------------------------

    async def on_message(self, client: Client, raw: str) -> None:
        try:
            msg = json.loads(raw)
        except (ValueError, TypeError):
            log.warning("tab %s sent non-JSON: %r", client.id, raw[:200])
            return
        if not isinstance(msg, dict):
            return

        # Unsolicited "hello" keeps our view of the tab fresh.
        if msg.get("type") == "hello":
            client.url = msg.get("url")
            client.user = msg.get("user")
            client.modhash = msg.get("modhash")
            client.last_seen = time.time()
            log.debug("hello from %s: user=%s url=%s", client.id, client.user, client.url)
            return

        rid = msg.get("id")
        entry = self._pending.get(rid) if rid else None
        if entry is None or entry[0] != client.id:
            log.debug("ignoring response %r from tab %s", rid, client.id)
            return
        self._pending.pop(rid, None)
        client.pending.discard(rid)
        fut = entry[1]
        if fut.done():
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
        tab: Optional[str] = None,
        timeout: float = 30.0,
    ) -> Any:
        """Send ACTION with PARAMS to a tab and return its JSON result.

        Raises :class:`NoClientError`, :class:`ReddiggTimeout`, or
        :class:`ReddiggError` (from the browser) as appropriate.
        """
        if action not in ACTIONS:
            raise ValueError(f"unknown action: {action!r} (expected one of {sorted(ACTIONS)})")
        target = self._pick(tab)
        if target.ws.closed:
            raise NoClientError("tab disconnected")

        rid = uuid.uuid4().hex
        fut: asyncio.Future = asyncio.get_running_loop().create_future()
        self._pending[rid] = (target.id, fut)
        target.pending.add(rid)
        try:
            await target.ws.send_str(
                json.dumps({"id": rid, "action": action, "params": params or {}})
            )
            return await asyncio.wait_for(fut, timeout=timeout)
        except asyncio.TimeoutError as exc:
            raise ReddiggTimeout(
                f"timed out after {timeout}s waiting for {action} on tab {target.id}"
            ) from exc
        except ConnectionError as exc:  # aiohttp: sending on a closing socket
            raise NoClientError("tab disconnected") from exc
        finally:
            self._pending.pop(rid, None)
            target.pending.discard(rid)

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
            {"subreddit": subreddit, "title": title, "kind": kind, "text": text, "url": url},
            **kw,
        )


# ---------------------------------------------------------------------------
# HTTP app
# ---------------------------------------------------------------------------


def _err(code: int, message: str) -> web.Response:
    return web.json_response({"error": message}, status=code)


def make_app(bridge: Bridge, token: str, origins: List[re.Pattern], port: int) -> web.Application:
    allowed_hosts = {f"127.0.0.1:{port}", f"localhost:{port}"}

    @web.middleware
    async def guard(request: web.Request, handler: Any) -> web.StreamResponse:
        if request.path in WS_PATHS and request.headers.get("Upgrade", "").lower() == "websocket":
            return await handler(request)  # origin-checked in the route itself
        if request.headers.get("Host") not in allowed_hosts:
            return _err(403, "bad Host header")
        if "Origin" in request.headers:
            return _err(403, "browser-originated requests are not allowed")
        supplied = request.headers.get("X-Bridge-Token") or ""
        if not secrets.compare_digest(supplied, token):
            return _err(403, "missing or invalid X-Bridge-Token")
        try:
            return await handler(request)
        except web.HTTPException as exc:  # 404/405 etc.: keep every reply JSON
            return _err(exc.status, exc.reason)

    async def ws_route(request: web.Request) -> web.StreamResponse:
        origin = request.headers.get("Origin")
        if not origin or not any(p.fullmatch(origin) for p in origins):
            log.warning("rejected websocket from origin %r", origin)
            return _err(403, "origin not allowed")
        ws = web.WebSocketResponse(max_msg_size=MAX_WS_MESSAGE)
        await ws.prepare(request)
        client = bridge.register(ws)
        try:
            async for msg in ws:
                if msg.type == WSMsgType.TEXT:
                    await bridge.on_message(client, msg.data)
                elif msg.type == WSMsgType.ERROR:
                    log.warning("tab %s websocket error: %s", client.id, ws.exception())
        finally:
            bridge.unregister(client)
        return ws

    async def tabs_route(request: web.Request) -> web.Response:
        return web.json_response({"ok": True, "result": [c.describe() for c in bridge.clients()]})

    async def action_route(request: web.Request) -> web.Response:
        action = request.match_info["action"]
        if request.method == "POST":
            if request.content_type != "application/json":
                return _err(415, "Content-Type must be application/json")
            try:
                params = await request.json()
            except ValueError:
                return _err(400, "request body is not valid JSON")
            if not isinstance(params, dict):
                return _err(400, "request body must be a JSON object")
        else:
            params = {}
            if action == "get":
                # /reddigg/get?path=/r/emacs.json%3Fcount%3D5 : the whole reddit
                # path (including its own query) arrives as one `path` value.
                params["path"] = request.query.get("path", "")

        try:
            timeout = min(float(request.query.get("timeout") or 30), 120.0)
        except ValueError:
            return _err(400, "`timeout` must be a number")

        try:
            result = await bridge.call(
                action, params, tab=request.query.get("tab"), timeout=timeout
            )
        except ValueError as exc:
            return _err(400, str(exc))
        except NoClientError as exc:
            return _err(503, str(exc))
        except ReddiggTimeout as exc:
            return _err(504, str(exc))
        except ReddiggError as exc:
            return _err(502, str(exc))
        except Exception as exc:  # noqa: BLE001 - never leave the client hanging
            log.exception("unexpected error handling %s", action)
            return _err(500, f"internal error: {exc}")
        return web.json_response({"ok": True, "result": result})

    app = web.Application(middlewares=[guard])
    app.add_routes(
        [web.get(p, ws_route) for p in WS_PATHS]
        + [
            web.get(f"{REST_ROOT}/tabs", tabs_route),
            web.get(f"{REST_ROOT}/{{action:{'|'.join(sorted(READ_ONLY))}}}", action_route),
            web.post(f"{REST_ROOT}/{{action:{'|'.join(sorted(WRITE))}}}", action_route),
        ]
    )
    return app


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


# ---------------------------------------------------------------------------


async def amain(args: argparse.Namespace) -> None:
    token = load_token()
    log.info("auth token is in %s", TOKEN_FILE)
    origins = [re.compile(p) for p in (args.origin or DEFAULT_ORIGINS)]
    bridge = Bridge()
    app = make_app(bridge, token, origins, args.port)
    runner = web.AppRunner(app, access_log=None)
    await runner.setup()
    try:
        await web.TCPSite(runner, args.host, args.port).start()
        log.info(
            "listening on http://%s:%d (websocket at %s, REST at %s)",
            args.host, args.port, WS_PATHS[0], REST_ROOT,
        )
        if args.repl:
            await _repl(bridge)
        else:
            await asyncio.Event().wait()
    finally:
        await runner.cleanup()


def main(argv: Optional[list] = None) -> None:
    p = argparse.ArgumentParser(description="reddigg server")
    p.add_argument("--host", default=DEFAULT_HOST)
    p.add_argument("--port", type=int, default=DEFAULT_PORT)
    p.add_argument(
        "--origin",
        action="append",
        help="regex of allowed websocket Origin (repeatable); "
        f"default {DEFAULT_ORIGINS[0]!r}",
    )
    p.add_argument(
        "--repl",
        action="store_true",
        help="also start an interactive prompt to drive the API by hand",
    )
    p.add_argument("-v", "--verbose", action="store_true", help="debug logging")
    args = p.parse_args(argv)
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
