// ==UserScript==
// @name         reddigg websocket bridge
// @namespace    https://github.com/thanhvg/emacs-reddigg
// @version      0.1.0
// @description  Bridge between a local Python websocket server (port 1979) and old.reddit.com. The server sends CRUD action requests; this script performs them in the logged-in reddit tab and streams JSON results back.
// @author       reddigg
// @match        https://old.reddit.com/*
// @match        https://www.reddit.com/*
// @match        https://reddit.com/*
// @grant        none
// @run-at       document-idle
// @connect      127.0.0.1
// ==/UserScript==

/*
 * reddigg-ws.user.js
 * ------------------
 * This is the browser half of a browser-gt-style setup for reddigg.
 *
 * Instead of Emacs embedding JavaScript and pushing it through
 * browser-gt, a small Python server owns a websocket connection to
 * every reddit tab running this script. The server exposes a CRUD
 * API (get/vote/comment/edit/delete/submit/session) and forwards
 * each request to the page, which executes it *same-origin* with
 * `credentials: 'include'` so the already-authenticated reddit
 * session (cookies + modhash) is used -- exactly what reddigg's
 * browser-gt path does with EVAL_IN_ACTIVE_TAB.
 *
 * Wire protocol (JSON, one object per websocket message):
 *
 *   server -> page   request:
 *     { "id": "<uuid>", "action": "get|session|vote|comment|edit|
 *                                   delete|submit", "params": { ... } }
 *
 *   page -> server   response:
 *     { "id": "<uuid>", "ok": true,  "result": <json> }
 *     { "id": "<uuid>", "ok": false, "error": "message" }
 *
 *   page -> server   (on connect, then every 20s):
 *     { "type": "hello", "url": location.href,
 *       "user": <name|null>, "modhash": <modhash|null> }
 *
 * The script reconnects automatically with exponential backoff, and
 * queues nothing: requests fail fast if the socket is down so the
 * server can retry/open a tab.
 */

(function () {
  'use strict';

  // ---- configuration -----------------------------------------------------

  const DEFAULT_PORT = 1979;
  const HELLO_INTERVAL_MS = 20000;
  const RECONNECT_BASE_MS = 1000;
  const RECONNECT_MAX_MS = 30000;

  // Allow overriding the port via a query string (?reddigg_ws_port=1979)
  // or a userscript storage value, so multiple servers can coexist.
  const portFromQuery = (() => {
    try {
      return new URLSearchParams(location.search).get('reddigg_ws_port');
    } catch (_) {
      return null;
    }
  })();
  const PORT = parseInt(portFromQuery, 10) || DEFAULT_PORT;
  const WS_URL = `ws://127.0.0.1:${PORT}`;

  // ---- state -------------------------------------------------------------

  let socket = null;
  let reconnectDelay = RECONNECT_BASE_MS;
  let reconnectTimer = null;
  let helloTimer = null;
  let closedByUs = false;

  // ---- logging -----------------------------------------------------------

  const TAG = '%c[reddigg-ws]';
  const TAG_CSS = 'color:#ff4500;font-weight:bold';
  const log = (...a) => console.log(TAG, TAG_CSS, ...a);
  const warn = (...a) => console.warn(TAG, TAG_CSS, ...a);

  // ---- reddit session helpers -------------------------------------------

  /**
   * Read the CSRF modhash + username the same way reddigg.el does:
   * prefer the legacy `r.config` object, fall back to scraping the DOM.
   * @returns {{modhash: (string|null), user: (string|null)}}
   */
  function sessionInfo() {
    let modhash = null;
    let user = null;
    try {
      if (typeof r !== 'undefined' && r.config) {
        modhash = r.config.modhash || null;
        user = r.config.cur_user || null;
      }
    } catch (_) { /* r not defined yet */ }

    if (!modhash) {
      const el = document.querySelector('input[name="uh"]');
      if (el && el.value) modhash = el.value;
    }
    if (!user) {
      const el = document.querySelector('.user a');
      if (el) user = el.textContent.trim();
    }
    return { modhash, user };
  }

  /**
   * Build an absolute old.reddit.com URL from an action `path`.
   * `path` may be absolute ("https://old.reddit.com/...") or a
   * pathname ("/api/vote"); protocol-relative and bare paths are
   * resolved against the current origin.
   */
  function resolveUrl(path) {
    if (/^https?:\/\//i.test(path)) return path;
    if (path.startsWith('//')) return location.protocol + path;
    if (path.startsWith('/')) return location.origin + path;
    return location.origin + '/' + path;
  }

  /**
   * Same-origin JSON GET, riding the tab's cookies. Returns the
   * parsed JSON when the body is JSON, otherwise the raw text.
   */
  async function jsonGet(path) {
    const url = resolveUrl(path);
    const r = await fetch(url, {
      method: 'GET',
      credentials: 'include',
      headers: { Accept: 'application/json' },
    });
    const text = await r.text();
    if (!r.ok) {
      throw new Error(`HTTP ${r.status} for ${url}: ${text.slice(0, 500)}`);
    }
    return parseMaybeJson(text);
  }

  /**
   * Same-origin form POST, exactly like reddigg--post-js: attaches
   * the modhash as `uh` and `api_type=json`, then checks the
   * {"json":{"errors":[...]}} envelope for API errors.
   *
   * @param {string} path
   * @param {Object<string,string|number>} fields
   * @param {boolean} needModhash
   */
  async function formPost(path, fields, needModhash) {
    const url = resolveUrl(path);
    const params = new URLSearchParams();
    for (const [k, v] of Object.entries(fields || {})) {
      if (v !== undefined && v !== null) params.append(k, String(v));
    }

    if (needModhash) {
      const { modhash } = sessionInfo();
      if (!modhash) {
        throw new Error(
          'no CSRF modhash found on the reddit tab (are you logged into old.reddit.com?)'
        );
      }
      params.set('uh', modhash);
      if (!params.has('api_type')) params.set('api_type', 'json');
    }

    const r = await fetch(url, {
      method: 'POST',
      credentials: 'include',
      headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
      body: params.toString(),
    });
    const text = await r.text();
    if (!r.ok) {
      throw new Error(`HTTP ${r.status} for ${url}: ${text.slice(0, 500)}`);
    }
    const data = parseMaybeJson(text);
    checkApiErrors(data);
    return data;
  }

  function parseMaybeJson(text) {
    const t = (text || '').trim();
    if (!t) return null;
    if (t[0] !== '{' && t[0] !== '[') return text;
    try {
      return JSON.parse(t);
    } catch (_) {
      return text;
    }
  }

  /**
   * Mirror of reddigg--check-api-errors: raise when reddit returns
   * {"json":{"errors":[["CODE","message",...],...]}}.
   */
  function checkApiErrors(data) {
    if (!data || typeof data !== 'object') return;
    const json = data.json;
    const errors = json && json.errors;
    if (Array.isArray(errors) && errors.length > 0) {
      const msg = errors
        .map((e) => (Array.isArray(e) && e.length >= 2 ? e[1] : String(e)))
        .join('; ');
      throw new Error(`reddit API error: ${msg}`);
    }
  }

  // ---- action handlers ---------------------------------------------------

  //
  // Each handler receives `params` and returns a JSON-serialisable
  // result. Throwing produces an `ok:false` response.
  //

  const actions = {
    /** { type:"hello" }-style probe; also usable as a health check. */
    async ping() {
      const s = sessionInfo();
      return {
        url: location.href,
        user: s.user,
        modhash: s.modhash,
        loggedIn: !!s.user,
      };
    },

    /** Re-scrape {modhash,user} from the page. */
    async session() {
      const s = sessionInfo();
      if (!s.modhash) {
        throw new Error(
          'no CSRF modhash found on the reddit tab (are you logged into old.reddit.com?)'
        );
      }
      return s;
    },

    /**
     * READ. Fetch any reddit JSON endpoint.
     * params: { path }  (e.g. "/r/emacs.json?count=25")
     */
    async get({ path }) {
      if (!path) throw new Error('get: missing `path`');
      return jsonGet(path);
    },

    /**
     * VOTE. dir: 1 up, -1 down, 0 clear.
     * params: { id, dir }
     */
    async vote({ id, dir }) {
      if (!id) throw new Error('vote: missing `id`');
      if (![1, -1, 0].includes(Number(dir))) {
        throw new Error('vote: `dir` must be 1, -1 or 0');
      }
      return formPost('/api/vote', { id, dir: Number(dir) }, true);
    },

    /**
     * CREATE COMMENT.
     * params: { parent, text }
     *   parent = fullname of the thing being replied to ("t3_xxx"/"t1_xxx")
     */
    async comment({ parent, text, thing_id }) {
      const thingId = parent || thing_id;
      if (!thingId) throw new Error('comment: missing `parent`');
      if (typeof text !== 'string' || !text.trim()) {
        throw new Error('comment: empty `text`');
      }
      return formPost('/api/comment', { thing_id: thingId, text }, true);
    },

    /**
     * UPDATE (edit a comment or self-post).
     * params: { id, text }
     */
    async edit({ id, text }) {
      if (!id) throw new Error('edit: missing `id`');
      if (typeof text !== 'string') throw new Error('edit: missing `text`');
      // /api/editusertext expects the thing fullname as `thing_id`.
      return formPost('/api/editusertext', { thing_id: id, text }, true);
    },

    /**
     * DELETE a comment or post.
     * params: { id }
     */
    async delete({ id }) {
      if (!id) throw new Error('delete: missing `id`');
      return formPost('/api/del', { id }, true);
    },

    /**
     * CREATE a submission.
     * params: { subreddit, title, kind, text?, url? }
     *   kind = "self" or "link"
     */
    async submit({ subreddit, title, kind, text, url }) {
      if (!subreddit) throw new Error('submit: missing `subreddit`');
      if (!title) throw new Error('submit: missing `title`');
      const k = kind === 'link' ? 'link' : 'self';
      const fields = { sr: subreddit, title, kind: k };
      if (k === 'self') fields.text = text || '';
      else fields.url = url || '';
      return formPost('/api/submit', fields, true);
    },
  };

  // ---- websocket plumbing ------------------------------------------------

  function wsSend(obj) {
    if (socket && socket.readyState === WebSocket.OPEN) {
      socket.send(JSON.stringify(obj));
      return true;
    }
    return false;
  }

  function sendHello() {
    const s = sessionInfo();
    wsSend({
      type: 'hello',
      url: location.href,
      user: s.user,
      modhash: s.modhash,
      ts: Date.now(),
    });
  }

  async function handleRequest(msg) {
    const { id, action, params } = msg || {};
    if (!id) {
      warn('dropping message without id', msg);
      return;
    }
    const fn = actions[action];
    if (!fn) {
      wsSend({ id, ok: false, error: `unknown action: ${action}` });
      return;
    }
    try {
      const result = await fn(params || {});
      wsSend({ id, ok: true, result });
    } catch (err) {
      wsSend({ id, ok: false, error: (err && err.message) || String(err) });
    }
  }

  function connect() {
    if (closedByUs) return;
    clearTimeout(reconnectTimer);

    log(`connecting to ${WS_URL} ...`);
    try {
      socket = new WebSocket(WS_URL);
    } catch (err) {
      warn('WebSocket construction failed', err);
      scheduleReconnect();
      return;
    }

    socket.addEventListener('open', () => {
      log('connected');
      reconnectDelay = RECONNECT_BASE_MS;
      sendHello();
      clearInterval(helloTimer);
      helloTimer = setInterval(sendHello, HELLO_INTERVAL_MS);
    });

    socket.addEventListener('message', (ev) => {
      let msg;
      try {
        msg = JSON.parse(ev.data);
      } catch (err) {
        warn('non-JSON message from server', ev.data);
        return;
      }
      handleRequest(msg);
    });

    socket.addEventListener('close', () => {
      log('disconnected');
      clearInterval(helloTimer);
      helloTimer = null;
      scheduleReconnect();
    });

    socket.addEventListener('error', () => {
      // `close` always follows, so just log here.
      warn('socket error');
    });
  }

  function scheduleReconnect() {
    if (closedByUs) return;
    clearTimeout(reconnectTimer);
    reconnectTimer = setTimeout(() => {
      connect();
    }, reconnectDelay);
    log(`reconnecting in ${reconnectDelay}ms`);
    reconnectDelay = Math.min(reconnectDelay * 2, RECONNECT_MAX_MS);
  }

  // ---- boot --------------------------------------------------------------

  // Expose a tiny control surface for debugging from the devtools console.
  window.reddiggWS = {
    connect,
    disconnect() {
      closedByUs = true;
      clearInterval(helloTimer);
      clearTimeout(reconnectTimer);
      if (socket) socket.close();
    },
    session: sessionInfo,
    actions,
    get socket() { return socket; },
  };

  connect();
})();
