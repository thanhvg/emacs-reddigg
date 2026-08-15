;;; reddigg.el --- A reader for redditt -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Thanh Vuong

;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/thanhvg/emacs-reddigg
;; Package-Requires: ((emacs "26.3") (promise "1.1") (ht "2.3") (org "9.2"))
;; Version: 0.7.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package allows you to browse reddit in org-mode.
;;
;; Reddit now requires a logged-in, browser-driven session for its
;; JSON endpoints, so reddigg no longer talks to old.reddit.com
;; directly with `url-retrieve'.  Instead it uses the `browsel'
;; package (https://github.com/dmgerman/browsel) to run the fetch
;; *inside* an already-open, already-authenticated reddit tab in
;; your real browser, and reads the resulting JSON text back into
;; Emacs.  You must:
;;   1. Have `browsel' set up and running (see its README) with both
;;      the Emacs side (`browsel-start') and the browser extension
;;      loaded and connected.
;;   2. Be logged into old.reddit.com in that browser.
;; reddigg will look for an already-open reddit tab; if it can't
;; find one it will offer to open old.reddit.com for you and wait
;; for it to finish loading before continuing.
;;
;; Buffers:
;; There are three buffers which are on org-mode. They show links and elisp
;; commands which will run when you enter/click (org-open-at-point) on them.
;; *reddigg-main*: show your subreddit list, enter on them will fetch the
;; subreddit posts and show them on *reddigg*. On *reddigg* when you enter on a
;; post will fetch the comments and show them on *reddigg-comments* buffer.
;;
;; Variables:
;; reddigg-subs: list of subreddits you want to show on *reddigg-main*
;;
;; * Commands
;; reddigg-view-main: show your subreddit list in *reddigg-main*, r/all and
;; r/popular are included.
;;
;; reddigg-view-sub: prompt for a subreddits and show it,
;;
;; reddigg-view-frontpage: view frontpage
;;
;; reddigg-view-comments: prompt for a post (eg:
;; r/emacs/comments/lfww57/weekly_tipstricketc_thread/ or
;; https://old.reddit.com/r/emacs/comments/lfww57/weekly_tipstricketc_thread/)
;; and show it.
;;
;; * Remarks
;; This mode only lets you view reddit. For a complete interaction with reddit check
;; out md4rd at https://github.com/ahungry/md4rd.

;;; Code:

(require 'promise)
(require 'cl-lib)
(require 'seq)
(require 'ht)
(require 'org)
(require 'json)
;; Soft-required: only needed once `reddigg-start' / a fetch actually
;; runs, but we want a clear error message rather than a void-function
;; error if it's missing.
(require 'browsel nil t)

(defgroup reddigg nil
  "Search and read stackoverflow and sisters's sites."
  :group 'convenience
  :link '(emacs-commentary-link "reddigg.el"))

(defcustom reddigg-subs '(acmilan emacs starcraft)
  "List of subreddits."
  :type 'list
  :group 'reddigg)

(defcustom reddigg-browsel-client nil
  "Browser client name (\"chrome\" or \"firefox\") to address via browsel.
Leave nil to let browsel pick automatically; only required when more
than one browser is connected to Emacs at the same time."
  :type '(choice (const :tag "Auto" nil) string)
  :group 'reddigg)

(defcustom reddigg-reddit-host-regexp "\\(?:old\\.\\)?reddit\\.com"
  "Regexp matched against a tab's URL to recognise it as a reddit tab."
  :type 'regexp
  :group 'reddigg)

(defcustom reddigg-reddit-open-url "https://old.reddit.com"
  "URL to open when no reddit tab is found and the user agrees to open one."
  :type 'string
  :group 'reddigg)

(defcustom reddigg-browsel-tab-wait-timeout 20
  "Seconds to wait for a freshly opened reddit tab to finish loading."
  :type 'number
  :group 'reddigg)

(defvar reddigg--modhash nil
  "Cached CSRF modhash scraped from the reddit tab.")

(defvar reddigg--current-user nil
  "Cached logged-in reddit username, scraped from the reddit tab.")

(defconst reddigg--json-false :reddigg-false
  "Sentinel representing JSON `false', kept distinct from `null'/missing
when parsing reddit API responses. Needed for fields like \"likes\",
where true = upvoted, this sentinel = downvoted, and nil = no vote /
not present -- collapsing false and null together (the old behavior)
made \"downvoted\" indistinguishable from \"never voted\".")

(defun reddigg--parse-json-string (text)
  "Parse JSON TEXT the way reddit's API responses should be read:
hash-tables for objects, nil for JSON null, and `reddigg--json-false'
for JSON false."
  (if (fboundp 'json-parse-string)
      (json-parse-string text
                          :object-type 'hash-table
                          :null-object nil
                          :false-object reddigg--json-false)
    (let ((json-array-type 'vector)
          (json-object-type 'hash-table)
          (json-false reddigg--json-false))
      (json-read-from-string text))))

(defun reddigg--parse-json-buffer ()
  "Read json from the current buffer (from point to the end)."
  (reddigg--parse-json-string
   (buffer-substring-no-properties (point) (point-max))))

(defconst reddigg--sub-url
  "https://old.reddit.com/r/%s.json?count=25"
  "Sub reddit template.")

(defconst reddigg--sub-view-sort-url
  "https://old.reddit.com/r/%s/%s.json?count=25"
  "Sub reddit template for new and rising.")

(defconst reddigg--sub-view-sort-scope-url
  "https://old.reddit.com/r/%s/%s.json?count=25&sort=%s&t=%s"
  "Sub reddit template for top and controversial.")

(defconst reddigg--cmt-url
  "https://old.reddit.com/%s.json"
  "Comment link template.")

(defconst reddigg--cmt-more-url
  "https://api.reddit.com/api/morechildren?api_type=json&link_id=%s&children=%s"
  "More comment link template.")

(defconst reddigg--template-sub "[[elisp:(reddigg-view-sub \"%s\")][%s]]\n"
  "Template string for main.")

(defconst reddigg--template-sub* "[[elisp:(reddigg-view-sub \"%s\")][%s]]"
  "Template string for refresh sub post list.")

(defconst reddigg--template-sub-sort "[[elisp:(reddigg--view-sub \"%s\" %s)][%s]]"
  "Template string for sub sort.")

(defun reddigg--ensure-modes ()
  "Get a bunch of modes up and running."
  (if (equal major-mode 'org-mode)
      (org-set-startup-visibility)
    (org-mode)
    (font-lock-flush))
  (visual-line-mode)
  (reddigg-view-mode 1))

(defvar reddigg-replacement-list
  '(("^\\* " . "- ")
    ("&gt;" . ">")
    ("&lt;" . "<")
    ("&amp;#x200B;" . "\n")
    ("&amp;nbsp;" . "\n")
    ("&amp;" . "&"))
  "List of (find . replace) to sanitize the text in range.")

(defvar-local reddigg--cmt-list-id nil
  "ID/name of the current comment list.")

(cl-defun reddigg--promise-posts (sub &key after before sort scope)
  "Promise SUB post list with keywords.
AFTER: fetch post after name.
BEFORE: fetch posts before name.
SORT: top, hot, best, rising, controversial.
SCOPE: hour, day, week, year, all."
  (reddigg--promise-json
   ;; create the url
   (concat
    (cond
     ((or (eq sort 'new )
          (eq sort 'rising))
      (format reddigg--sub-view-sort-url sub sort))
     ((or (eq sort 'top )
          (eq sort 'controversial))
      (format reddigg--sub-view-sort-scope-url sub sort sort scope))
     (t (format reddigg--sub-url sub sort sort)))
    (when after
      (concat "&after=" after))
    (when before
      (concat "&before=" before)))))

(defun reddigg--promise-comments (cmt)
  "Promise CMT list."
  (reddigg--promise-json (format reddigg--cmt-url cmt)))

(defun reddigg--promise-more-comments (children)
  "Promise more comment list for CHILDREN."
  (reddigg--promise-json (format reddigg--cmt-more-url
                                 reddigg--cmt-list-id
                                 children)))

;;; --- JSON fetching via browsel ------------------------------------------
;;
;; Reddit's JSON endpoints now require a real, logged-in browser
;; session, so all fetches go through `browsel', running inside an
;; open reddit tab.

(defun reddigg--browsel-plist-tab-p (x)
  "Heuristic: does X look like a single tab plist (i.e. has an :id)?"
  (and (consp x) (plist-member x :id)))

(defun reddigg--browsel-as-tab-list (x)
  "Coerce a browsel response X into a list of tab plists.

The exact response envelope for GET_ALL_TABS / OPEN_TAB isn't
pinned down from the README alone (it's shown both as a bare list
of tabs and as a `:status'/`:result' plist), so this accepts
either: a bare list, a vector, a single tab plist, or an envelope
plist whose payload lives under :tabs, :result, or :tab."
  (cond
   ((null x) nil)
   ((reddigg--browsel-plist-tab-p x) (list x))
   ((vectorp x) (append x nil))
   ((and (consp x) (plist-member x :status))
    (reddigg--browsel-as-tab-list (or (plist-get x :tabs)
                                       (plist-get x :result)
                                       (plist-get x :tab))))
   ((listp x) x)
   (t nil)))

(defun reddigg--ensure-browsel ()
  "Signal a clear error if `browsel' isn't loaded."
  (unless (featurep 'browsel)
    (user-error "reddigg: `browsel' is not loaded; install it and \
require it (or add it to your `use-package browsel' config) before \
using reddigg")))

(defun reddigg--find-reddit-tab-id ()
  "Return the id of an already-open tab that looks like reddit, or nil."
  (let* ((resp (browsel-request "GET_ALL_TABS" nil reddigg-browsel-client))
         (tabs (reddigg--browsel-as-tab-list resp))
         (tab (seq-find (lambda (tb)
                          (string-match-p reddigg-reddit-host-regexp
                                          (or (plist-get tb :url) "")))
                        tabs)))
    (plist-get tab :id)))

(defun reddigg--tab-ready-p (id)
  "Non-nil when tab ID exists, is done loading, and is on reddit."
  (let* ((resp (browsel-request "GET_ALL_TABS" nil reddigg-browsel-client))
         (tabs (reddigg--browsel-as-tab-list resp))
         (tab (seq-find (lambda (tb) (equal (plist-get tb :id) id)) tabs)))
    (and tab
         (or (null (plist-get tab :status))
             (equal (plist-get tab :status) "complete"))
         (string-match-p reddigg-reddit-host-regexp (or (plist-get tab :url) "")))))

(defun reddigg--wait-tab-ready (id)
  "Block (with `sit-for') until tab ID is ready or we time out."
  (let ((deadline (+ (float-time) reddigg-browsel-tab-wait-timeout)))
    (while (and (< (float-time) deadline)
                (not (reddigg--tab-ready-p id)))
      (sit-for 0.5))
    (unless (reddigg--tab-ready-p id)
      (user-error "reddigg: timed out waiting for the reddit tab to finish loading"))))

(defun reddigg--open-reddit-tab-and-wait ()
  "Ask the user, then open `reddigg-reddit-open-url' and wait for it."
  (unless (y-or-n-p (format "reddigg: no reddit tab found; open %s now? "
                            reddigg-reddit-open-url))
    (user-error "reddigg: open %s in your browser and retry"
               reddigg-reddit-open-url))
  (let* ((resp (browsel-request "OPEN_TAB" (list :url reddigg-reddit-open-url)
                                reddigg-browsel-client))
         (tab (car (reddigg--browsel-as-tab-list resp)))
         (id (plist-get tab :id)))
    (unless id
      (user-error "reddigg: could not open a reddit tab (response: %S)" resp))
    (reddigg--wait-tab-ready id)
    id))

(defun reddigg--reddit-tab-id (&optional prompt-if-missing)
  "Return the id of a reddit tab, prompting to open one if PROMPT-IF-MISSING."
  (or (reddigg--find-reddit-tab-id)
      (when prompt-if-missing
        (reddigg--open-reddit-tab-and-wait))))

(defun reddigg--fetch-js (url)
  "JS to run inside the reddit tab: same-origin fetch of URL (so it
rides along with the tab's own cookies/session), returning the raw
JSON response body as a string."
  (let ((json-url (json-encode url)))
    (format "(async () => {
  const r = await fetch(%s, { credentials: 'include' });
  if (!r.ok) throw new Error('reddigg: HTTP ' + r.status + ' for ' + %s);
  return await r.text();
})()"
            json-url json-url)))

(defun reddigg--handle-eval-response (response resolve reject)
  "Unpack an EVAL_IN_ACTIVE_TAB RESPONSE and RESOLVE/REJECT accordingly."
  (if (not (equal (plist-get response :status) "ok"))
      (funcall reject (or (plist-get response :message) response))
    (let* ((results (plist-get response :result))
           (first (cond ((vectorp results) (aref results 0))
                       ((consp results) (car results))
                       (t results)))
           (text (plist-get first :result)))
      (if (not (stringp text))
          (funcall reject (format "reddigg: unexpected eval result shape: %S" response))
        (condition-case err
            (funcall resolve (reddigg--parse-json-string text))
          (error (funcall reject err)))))))

(defun reddigg--eval-json-promise (code &optional tab-id)
  "Run JS CODE in a reddit tab (finding/opening one unless TAB-ID is
given). CODE must return a JSON string. Promise the parsed JSON."
  (reddigg--ensure-browsel)
  (promise-new
   (lambda (resolve reject)
     (condition-case err
         (let ((id (or tab-id (reddigg--reddit-tab-id t))))
           (browsel-request-async
            "EVAL_IN_ACTIVE_TAB"
            (list :tabId id :code code)
            (lambda (response)
              (reddigg--handle-eval-response response resolve reject))
            reddigg-browsel-client))
       (error (funcall reject err))))))

(defun reddigg--promise-json (url)
  "Promise the JSON at URL, fetched from inside a live reddit tab via browsel."
  (reddigg--eval-json-promise (reddigg--fetch-js url)))

;;; --- CSRF / session info ------------------------------------------------
;;
;; Reddit's POST endpoints require a modhash: a CSRF token embedded
;; in the page itself (window.r.config.modhash on old.reddit.com),
;; alongside the session cookie the browser already attaches
;; automatically. The cookie alone proves you're logged in; the
;; modhash proves the request was actually issued by code running on
;; reddit's own page (cross-origin script can't read it), which is
;; what blocks a forged cross-site request. reddit's own "vote"
;; button reads this same value before submitting -- we're just
;; doing what it does.

(defun reddigg--session-info-js ()
  "JS returning {modhash, user} as a JSON string, scraped from the
loaded reddit page. Tries the legacy `r.config' object first, falls
back to scraping the DOM in case that object ever goes away."
  "(() => {
  let modhash = null, user = null;
  try {
    if (typeof r !== 'undefined' && r.config) {
      modhash = r.config.modhash || null;
      user = r.config.cur_user || null;
    }
  } catch (e) {}
  if (!modhash) {
    const el = document.querySelector('input[name=\"uh\"]');
    if (el) modhash = el.value;
  }
  if (!user) {
    const el = document.querySelector('.user a');
    if (el) user = el.textContent.trim();
  }
  return JSON.stringify({ modhash: modhash, user: user });
})()")

(defun reddigg--promise-session-info (&optional force)
  "Promise (MODHASH . USER). Uses cached values unless FORCE."
  (if (and (not force) reddigg--modhash reddigg--current-user)
      (promise-resolve (cons reddigg--modhash reddigg--current-user))
    (promise-chain (reddigg--eval-json-promise (reddigg--session-info-js))
      (then (lambda (data)
              (let ((modhash (gethash "modhash" data))
                    (user (gethash "user" data)))
                (unless (and modhash (> (length modhash) 0))
                  (error "reddigg: no CSRF modhash found on the reddit tab \
(are you logged into old.reddit.com?)"))
                (setq reddigg--modhash modhash
                      reddigg--current-user user)
                (cons modhash user)))))))

;;;###autoload
(defun reddigg-refresh-session ()
  "Force re-reading the CSRF modhash and username from the reddit tab.
Use this if actions start failing after you log out/in or switch
accounts in the browser."
  (interactive)
  (promise-chain (reddigg--promise-session-info t)
    (then (lambda (info)
            (message "reddigg: session refreshed (logged in as %s)" (cdr info))))
    (promise-catch (lambda (reason)
                     (message "reddigg: could not refresh session: %s" reason)))))

;;; --- Generic authenticated POST ------------------------------------------

(defun reddigg--post-js (path fields)
  "JS that POSTs FIELDS (an elisp alist) to PATH on the current
origin, same-origin with credentials, and returns the response body
as a string."
  (format "(async () => {
  const params = new URLSearchParams(%s);
  const r = await fetch(%s, {
    method: 'POST',
    credentials: 'include',
    headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
    body: params.toString()
  });
  const text = await r.text();
  if (!r.ok) throw new Error('reddigg: HTTP ' + r.status + ' for ' + %s + ': ' + text);
  return text;
})()"
          (json-encode fields) (json-encode path) (json-encode path)))

(defun reddigg--check-api-errors (data)
  "Signal a readable error if DATA (parsed reddit API JSON) carries
errors in the {\"json\":{\"errors\":[...]}} envelope; else return DATA."
  (when (hash-table-p data)
    (let* ((json (gethash "json" data))
           (errors (and (hash-table-p json) (gethash "errors" json))))
      (when (and errors (> (length errors) 0))
        (error "reddigg: reddit API error: %s"
               (mapconcat (lambda (e) (format "%s" (if (>= (length e) 2) (aref e 1) e)))
                          (append errors nil) "; ")))))
  data)

(defun reddigg--promise-post (path fields)
  "Promise the parsed JSON result of POSTing FIELDS to PATH (e.g.
\"/api/vote\") on old.reddit.com, run from inside the reddit tab.
Automatically attaches the CSRF modhash once session info is known,
and signals an error if the API reports one."
  (promise-chain (reddigg--promise-session-info)
    (then (lambda (info)
            (let ((full-fields (append fields
                                        (list (cons "uh" (car info))
                                              (cons "api_type" "json")))))
              (reddigg--eval-json-promise (reddigg--post-js path full-fields)))))
    (then #'reddigg--check-api-errors)))

;;; --- Thing identity (posts/comments) at point ----------------------------

(defun reddigg--thing-at-point ()
  "Return (:id ID :author AUTHOR :subreddit SUBREDDIT) for the reddigg
entry at point, or user-error."
  (save-excursion
    (condition-case nil
        (org-back-to-heading t)
      (error (user-error "reddigg: point is not on a reddigg entry")))
    (let ((id (org-entry-get (point) "REDDIGG_ID"))
          (author (org-entry-get (point) "REDDIGG_AUTHOR"))
          (subreddit (org-entry-get (point) "REDDIGG_SUBREDDIT")))
      (unless id
        (user-error "reddigg: no reddit item at point (missing REDDIGG_ID)"))
      (list :id id :author author :subreddit subreddit))))

(defun reddigg--mark-body-overlay (start end)
  "Wrap START..END (markers or positions) in an overlay tagging it as
this entry's editable body text, so `reddigg-edit-at-point' can find
and replace it later without needing to re-fetch from reddit."
  (let ((ov (make-overlay start end nil t nil)))
    (overlay-put ov 'reddigg-body t)
    ov))

(defun reddigg--find-body-overlay ()
  "Find the body overlay for the reddigg entry at point, or nil."
  (save-excursion
    (org-back-to-heading t)
    (let ((subtree-end (save-excursion (org-end-of-subtree t t) (point))))
      (car (seq-filter (lambda (ov) (overlay-get ov 'reddigg-body))
                       (overlays-in (point) subtree-end))))))

(defun reddigg--likes->string (value)
  "Convert a raw reddit \"likes\" JSON VALUE into \"up\"/\"down\"/\"none\".
VALUE is t (upvoted), `reddigg--json-false' (downvoted), or nil (no
vote / not present)."
  (cond
   ((eq value t) "up")
   ((eq value reddigg--json-false) "down")
   (t "none")))

(defun reddigg--likes-string->number (s)
  "Inverse of `reddigg--likes->string': \"up\"/\"down\"/\"none\" (or nil,
treated as \"none\") to 1/-1/0."
  (cond ((equal s "up") 1)
        ((equal s "down") -1)
        (t 0)))

(defun reddigg--bump-score (delta)
  "Add DELTA to the :REDDIGG_SCORE: property of the heading at point."
  (let* ((current (org-entry-get (point) "REDDIGG_SCORE"))
         (current-n (if current (string-to-number current) 0)))
    (org-entry-put (point) "REDDIGG_SCORE" (number-to-string (+ current-n delta)))))

(defun reddigg--format-created (epoch)
  "Format EPOCH (reddit's \"created_utc\", seconds since epoch) as a
readable timestamp, or \"unknown\" if EPOCH isn't a number."
  (if (numberp epoch)
      (format-time-string "%Y-%m-%d %H:%M:%S" epoch)
    "unknown"))

;;; --- Voting ---------------------------------------------------------------

(defconst reddigg--vote-path "https://old.reddit.com/api/vote")

(defun reddigg--vote (dir)
  "Vote DIR (1, -1, or 0) on the reddigg entry at point."
  (let* ((thing (reddigg--thing-at-point))
         (id (plist-get thing :id))
         (marker (point-marker)))
    (promise-chain (reddigg--promise-post reddigg--vote-path
                                          (list (cons "id" id) (cons "dir" dir)))
      (then (lambda (_data)
              (with-current-buffer (marker-buffer marker)
                (save-excursion
                  (goto-char marker)
                  (org-back-to-heading t)
                  (let* ((old-numeric (reddigg--likes-string->number
                                       (org-entry-get (point) "REDDIGG_LIKES")))
                         (delta (- dir old-numeric)))
                    (reddigg--bump-score delta)
                    (org-entry-put (point) "REDDIGG_LIKES"
                                   (pcase dir (1 "up") (-1 "down") (_ "none")))
                    (message "reddigg: %s (score %s%s)"
                            (pcase dir (1 "upvoted") (-1 "downvoted") (_ "vote cleared"))
                            (if (>= delta 0) "+" "")
                            delta))))))
      (promise-catch (lambda (reason)
                       (message "reddigg: vote failed: %s" reason))))))

;;;###autoload
(defun reddigg-vote-up ()
  "Upvote the post or comment at point."
  (interactive)
  (reddigg--vote 1))

;;;###autoload
(defun reddigg-vote-down ()
  "Downvote the post or comment at point."
  (interactive)
  (reddigg--vote -1))

;;;###autoload
(defun reddigg-vote-clear ()
  "Clear your vote on the post or comment at point."
  (interactive)
  (reddigg--vote 0))

;;; --- Compose buffers (reply/edit/submit) -----------------------------------

(defvar-local reddigg-compose--kind nil
  "Symbol: `comment', `edit', or `submit'. Decides what C-c C-c does.")
(defvar-local reddigg-compose--parent-id nil
  "Fullname of the thing being replied to (kind `comment').")
(defvar-local reddigg-compose--target-marker nil
  "Marker in the source buffer where the result should be inserted/updated.")
(defvar-local reddigg-compose--extra-fields nil
  "Alist of extra POST fields the specific kind needs (e.g. thing_id for edit).")

(defvar-local reddigg-compose--submit-sr nil
  "Subreddit name for a `submit' compose buffer.")

(defvar-local reddigg-compose--submit-kind nil
  "Reddit submission kind (`self' or `link') for a `submit' compose buffer.")

(defvar-local reddigg-compose--submit-title nil
  "Submission title for a `submit' compose buffer.")

(defvar reddigg-compose-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-c") #'reddigg-compose-send)
    (define-key m (kbd "C-c C-k") #'reddigg-compose-abort)
    m)
  "Keymap for reddigg compose buffers.")

(define-minor-mode reddigg-compose-mode
  "Minor mode for composing reddit comments/posts/edits."
  :lighter " reddigg-compose"
  :keymap reddigg-compose-mode-map)

(defun reddigg--compose-body ()
  "Return the compose buffer's text, stripping leading `# reddigg:' lines."
  (save-excursion
    (goto-char (point-min))
    (while (looking-at "^# reddigg:.*\n") (forward-line 1))
    (string-trim (buffer-substring-no-properties (point) (point-max)))))

(defun reddigg-compose-abort ()
  "Abandon the current reddigg compose buffer."
  (interactive)
  (when (y-or-n-p "reddigg: discard this? ")
    (kill-buffer)))

(defun reddigg-compose-send ()
  "Submit the current reddigg compose buffer, dispatching on its kind."
  (interactive)
  (let ((kind reddigg-compose--kind)
        (body (reddigg--compose-body))
        (buf (current-buffer)))
    (when (string-empty-p body)
      (user-error "reddigg: empty body, nothing to send"))
    (pcase kind
      ('comment (reddigg--send-comment buf body))
      ('edit (reddigg--send-edit buf body))
      ('submit (reddigg--send-submit buf body))
      (_ (error "reddigg: compose kind %S not implemented yet" kind)))))

(defun reddigg--insert-new-comment (parent-marker comment-data)
  "Insert COMMENT-DATA (a hash-table from reddit's api/comment response)
as a new child heading under PARENT-MARKER's entry."
  (with-current-buffer (marker-buffer parent-marker)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char parent-marker)
        (org-back-to-heading t)
        (let ((level (org-current-level))
              (author (gethash "author" comment-data))
              (body (gethash "body" comment-data))
              (name (gethash "name" comment-data)))
          (org-end-of-subtree t t)
          (unless (bolp) (insert "\n"))
          (insert (make-string (1+ level) ?*) " " author "\n")
          (insert ":PROPERTIES:\n")
          (insert (format ":REDDIGG_ID: %s\n" name))
          (insert (format ":REDDIGG_AUTHOR: %s\n" author))
          (insert (format ":REDDIGG_SUBREDDIT: %s\n" (gethash "subreddit" comment-data)))
          (insert (format ":REDDIGG_LIKES: %s\n" (reddigg--likes->string (gethash "likes" comment-data))))
          (insert (format ":REDDIGG_SCORE: %s\n" (gethash "score" comment-data)))
          (insert (format ":REDDIGG_CREATED: %s\n" (reddigg--format-created (gethash "created_utc" comment-data))))
          (insert ":END:\n")
          (insert body "\n"))))))

(defconst reddigg--comment-path "https://old.reddit.com/api/comment")

(defun reddigg--send-comment (compose-buffer body)
  (let ((parent-id (buffer-local-value 'reddigg-compose--parent-id compose-buffer))
        (target-marker (buffer-local-value 'reddigg-compose--target-marker compose-buffer)))
    (promise-chain (reddigg--promise-post reddigg--comment-path
                                          (list (cons "thing_id" parent-id)
                                                (cons "text" body)))
      (then (lambda (data)
              (let* ((json (gethash "json" data))
                     (things (and json (gethash "data" json)
                                  (gethash "things" (gethash "data" json))))
                     (comment (and things (> (length things) 0)
                                   (gethash "data" (aref things 0)))))
                (if comment
                    (reddigg--insert-new-comment target-marker comment)
                  (message "reddigg: comment posted, but couldn't parse the \
response; refresh to see it.")))
              (when (buffer-live-p compose-buffer)
                (kill-buffer compose-buffer))
              (message "reddigg: reply posted")))
      (promise-catch (lambda (reason)
                       (message "reddigg: reply failed: %s" reason))))))

;;;###autoload
(defun reddigg-reply-at-point ()
  "Compose a reply to the post or comment at point."
  (interactive)
  (let* ((thing (reddigg--thing-at-point))
         (parent-id (plist-get thing :id))
         (parent-marker (point-marker))
         (buf (generate-new-buffer "*reddigg-reply*")))
    (with-current-buffer buf
      (org-mode)
      (reddigg-compose-mode 1)
      (setq-local reddigg-compose--kind 'comment)
      (setq-local reddigg-compose--parent-id parent-id)
      (setq-local reddigg-compose--target-marker parent-marker)
      (insert (format "# reddigg: replying to %s\n" parent-id))
      (insert "# reddigg: C-c C-c to send, C-c C-k to abort\n\n"))
    (pop-to-buffer buf)
    (goto-char (point-max))))

(defconst reddigg--submit-path "https://old.reddit.com/api/submit")

(defun reddigg--submit-comments-path (post-id)
  "Return a reddigg comments path for the submitted POST-ID."
  (format "comments/%s" (replace-regexp-in-string "\\`t3_" "" post-id)))

(defun reddigg--send-submit (compose-buffer body)
  "Submit the post described by COMPOSE-BUFFER with BODY."
  (let ((subreddit (buffer-local-value 'reddigg-compose--submit-sr compose-buffer))
        (kind (buffer-local-value 'reddigg-compose--submit-kind compose-buffer))
        (title (buffer-local-value 'reddigg-compose--submit-title compose-buffer)))
    (promise-chain
        (reddigg--promise-post
         reddigg--submit-path
         (append (list (cons "sr" subreddit)
                       (cons "kind" kind)
                       (cons "title" title))
                 (list (cons (if (equal kind "link") "url" "text") body))))
      (then
       (lambda (data)
         (let* ((json (gethash "json" data))
                (payload (and (hash-table-p json) (gethash "data" json)))
                (post-id (and (hash-table-p payload) (gethash "name" payload)))
                (post-url (and (hash-table-p payload) (gethash "url" payload)))
                (comments-path (and post-id
                                    (reddigg--submit-comments-path post-id))))
           (when (buffer-live-p compose-buffer)
             (kill-buffer compose-buffer))
           (message "reddigg: post submitted%s%s"
                    (if post-id (format " (%s)" post-id) "")
                    (if post-url (format " %s" post-url) ""))
           (when (and comments-path
                      (y-or-n-p "reddigg: view the new post's comments? "))
             (reddigg--view-comments comments-path t)))))
      (promise-catch
       (lambda (reason)
         (message "reddigg: submit failed: %s" reason))))))

;;;###autoload
(defun reddigg-submit-post ()
  "Compose and submit a new Reddit post."
  (interactive)
  (let* ((choices (mapcar (lambda (sub) (if (symbolp sub)
                                            (symbol-name sub)
                                          sub))
                          reddigg-subs))
         (subreddit (string-trim
                     (completing-read "Subreddit: " choices nil nil)))
         (kind (completing-read "Post kind: " '("self" "link") nil t))
         (title (string-trim (read-string "Title: ")))
         (buf (generate-new-buffer "*reddigg-submit*")))
    (when (string-empty-p subreddit)
      (user-error "reddigg: subreddit cannot be empty"))
    (when (string-empty-p title)
      (user-error "reddigg: title cannot be empty"))
    (with-current-buffer buf
      (org-mode)
      (reddigg-compose-mode 1)
      (setq-local reddigg-compose--kind 'submit)
      (setq-local reddigg-compose--submit-sr subreddit)
      (setq-local reddigg-compose--submit-kind kind)
      (setq-local reddigg-compose--submit-title title)
      (insert (format "# reddigg: submitting to r/%s\n" subreddit))
      (insert (format "# reddigg: %s post: %s\n" kind title))
      (insert "# reddigg: C-c C-c to submit, C-c C-k to abort\n\n")
      (insert (if (equal kind "link")
                  "# Paste the link URL below.\n"
                "# Write the post body below.\n")))
    (pop-to-buffer buf)
    (goto-char (point-max))))

;;; --- Edit / delete your own content ----------------------------------------

(defconst reddigg--editusertext-path "https://old.reddit.com/api/editusertext")
(defconst reddigg--del-path "https://old.reddit.com/api/del")

(defun reddigg--replace-body-at-marker (heading-marker new-text)
  "Replace the editable body text of the reddigg entry at HEADING-MARKER
with NEW-TEXT (the raw markdown just submitted to reddit)."
  (with-current-buffer (marker-buffer heading-marker)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char heading-marker)
        (org-back-to-heading t)
        (let ((ov (reddigg--find-body-overlay)))
          (if (not ov)
              (message "reddigg: edit saved on reddit, but couldn't find the \
body in this buffer to update it locally; refresh to see the change.")
            (let ((start (overlay-start ov)))
              (goto-char start)
              (delete-region start (overlay-end ov))
              (insert new-text "\n")
              (move-overlay ov start (point))
              (reddigg--sanitize-range start (point)))))))))

(defun reddigg--send-edit (compose-buffer body)
  (let ((thing-id (buffer-local-value 'reddigg-compose--parent-id compose-buffer))
        (target-marker (buffer-local-value 'reddigg-compose--target-marker compose-buffer)))
    (promise-chain (reddigg--promise-post reddigg--editusertext-path
                                          (list (cons "thing_id" thing-id)
                                                (cons "text" body)))
      (then (lambda (_data)
              (reddigg--replace-body-at-marker target-marker body)
              (when (buffer-live-p compose-buffer)
                (kill-buffer compose-buffer))
              (message "reddigg: edit saved")))
      (promise-catch (lambda (reason)
                       (message "reddigg: edit failed: %s" reason))))))

;;;###autoload
(defun reddigg-edit-at-point ()
  "Compose an edit to the post or comment at point.
Only works on entries you authored; only self-posts and comments have
editable text on reddit (link posts don't)."
  (interactive)
  (let* ((thing (reddigg--thing-at-point))
         (id (plist-get thing :id))
         (author (plist-get thing :author)))
    (unless (and reddigg--current-user (equal author reddigg--current-user))
      (user-error "reddigg: this entry isn't yours to edit (author: %s)"
                 (or author "unknown")))
    (let* ((ov (reddigg--find-body-overlay))
           (existing-text (when ov
                            (buffer-substring-no-properties
                             (overlay-start ov) (overlay-end ov))))
           (marker (point-marker))
           (buf (generate-new-buffer "*reddigg-edit*")))
      (with-current-buffer buf
        (org-mode)
        (reddigg-compose-mode 1)
        (setq-local reddigg-compose--kind 'edit)
        (setq-local reddigg-compose--parent-id id)
        (setq-local reddigg-compose--target-marker marker)
        (insert (format "# reddigg: editing %s\n" id))
        (insert "# reddigg: C-c C-c to save, C-c C-k to abort\n\n")
        (when existing-text (insert existing-text)))
      (pop-to-buffer buf)
      (goto-char (point-max)))))

;;;###autoload
(defun reddigg-delete-at-point ()
  "Delete the post or comment at point. Only works on entries you authored."
  (interactive)
  (let* ((thing (reddigg--thing-at-point))
         (id (plist-get thing :id))
         (author (plist-get thing :author))
         (marker (point-marker)))
    (unless (and reddigg--current-user (equal author reddigg--current-user))
      (user-error "reddigg: this entry isn't yours to delete (author: %s)"
                 (or author "unknown")))
    (when (y-or-n-p "reddigg: really delete this? ")
      (promise-chain (reddigg--promise-post reddigg--del-path (list (cons "id" id)))
        (then (lambda (_data)
                (with-current-buffer (marker-buffer marker)
                  (let ((inhibit-read-only t))
                    (save-excursion
                      (goto-char marker)
                      (org-back-to-heading t)
                      (org-cut-subtree))))
                (message "reddigg: deleted")))
        (promise-catch (lambda (reason)
                         (message "reddigg: delete failed: %s" reason)))))))

;;; --- View-buffer keymap (voting, replying, editing, deleting) --------------

(defvar reddigg-view-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-v u") #'reddigg-vote-up)
    (define-key m (kbd "C-c C-v d") #'reddigg-vote-down)
    (define-key m (kbd "C-c C-v 0") #'reddigg-vote-clear)
    (define-key m (kbd "C-c C-v r") #'reddigg-reply-at-point)
    (define-key m (kbd "C-c C-v e") #'reddigg-edit-at-point)
    (define-key m (kbd "C-c C-v x") #'reddigg-delete-at-point)
    m)
  "Keymap for reddigg's generated browsing buffers.")

(define-minor-mode reddigg-view-mode
  "Minor mode for reddigg's generated org buffers (posts, comments)."
  :lighter " reddigg"
  :keymap reddigg-view-mode-map)

;;; --- rest of the package (unchanged) ------------------------------------

(defvar reddigg--main-buffer "*reddigg-main*"
  "Buffer for main page.")

(defvar reddigg--buffer "*reddigg*"
  "Buffer for main page.")

(defvar reddigg--cmt-buffer "*reddigg-comments*"
  "Comment buffer.")

(defun reddigg--get-buffer ()
  "Get buffer for sub."
  (get-buffer-create reddigg--buffer))

(defun reddigg--get-cmt-buffer ()
  "Get buffer for comments."
  (get-buffer-create reddigg--cmt-buffer))

(defun reddigg--get-main-buffer ()
  "Get main buffer."
  (get-buffer-create reddigg--main-buffer))

(cl-defun reddigg--print-sub (data sub &optional append &key sort scope)
  "Print sub post list in DATA for SUB.
SORT: top, hot, best, rising, controversial.
SCOPE: hour, day, week, year, all.
When APPEND is non-nil, will not delete buffer but append to it,
after deleting the current line which should be the More button."
  (with-current-buffer (reddigg--get-buffer)
    (save-excursion
      (if append
          (kill-whole-line)
        (erase-buffer)
        ;; insert header and links
        (insert "#+startup: overview indent\n")
        (insert (format "#+title: %s sorted by %s%s\n"
                        (if (< (length sub) 30)
                            sub
                          "posts")
                        (if sort sort 'default)
                        (if scope
                            (format " %s" scope)
                          "")))
        (insert (format reddigg--template-sub* sub "refresh"))
        (insert " ")
        (insert (format reddigg--template-sub-sort
                        sub
                        ":sort 'new"
                        "new"))
        (insert " ")
        (insert (format reddigg--template-sub-sort
                        sub
                        ":sort 'rising"
                        "rising"))
        (insert "\n")
        ;; top
        (insert (format reddigg--template-sub-sort
                        sub
                        ":sort 'top :scope 'hour"
                        "top-hour"))
        (insert " ")
        (insert (format reddigg--template-sub-sort
                        sub
                        ":sort 'top :scope 'day"
                        "top-day"))
        (insert " ")
        (insert (format reddigg--template-sub-sort
                        sub
                        ":sort 'top :scope 'week"
                        "top-week"))
        (insert " ")
        (insert (format reddigg--template-sub-sort
                        sub
                        ":sort 'top :scope 'month"
                        "top-month"))
        (insert " ")
        (insert (format reddigg--template-sub-sort
                        sub
                        ":sort 'top :scope 'year"
                        "top-year"))
        (insert " ")
        (insert (format reddigg--template-sub-sort
                        sub
                        ":sort 'top :scope 'all"
                        "top-all"))
        (insert "\n")
        ;; controversial
        (insert (format reddigg--template-sub-sort
                        sub
                        ":sort 'controversial :scope 'hour"
                        "controversial-hour"))
        (insert " ")
        (insert (format reddigg--template-sub-sort
                        sub
                        ":sort 'controversial :scope 'day"
                        "con-day"))
        (insert " ")
        (insert (format reddigg--template-sub-sort
                        sub
                        ":sort 'controversial :scope 'week"
                        "con-week"))
        (insert " ")
        (insert (format reddigg--template-sub-sort
                        sub
                        ":sort 'controversial :scope 'month"
                        "con-month"))
        (insert " ")
        (insert (format reddigg--template-sub-sort
                        sub
                        ":sort 'controversial :scope 'year"
                        "con-year"))
        (insert " ")
        (insert (format reddigg--template-sub-sort
                        sub
                        ":sort 'controversial :scope 'all"
                        "con-all"))
        (insert "\n"))

      (seq-do
       (lambda (it)
         (let ((my-it (gethash "data" it)))
           (insert "* " (gethash "title" my-it) "\n")
           (insert ":PROPERTIES:\n")
           (insert (format ":REDDIGG_ID: %s\n" (gethash "name" my-it)))
           (insert (format ":REDDIGG_AUTHOR: %s\n" (gethash "author" my-it)))
           (insert (format ":REDDIGG_SUBREDDIT: %s\n" (gethash "subreddit" my-it)))
           (insert (format ":REDDIGG_LIKES: %s\n" (reddigg--likes->string (gethash "likes" my-it))))
           (insert (format ":REDDIGG_SCORE: %s\n" (gethash "score" my-it)))
           (insert (format ":REDDIGG_CREATED: %s\n" (reddigg--format-created (gethash "created_utc" my-it))))
           (insert ":END:\n")
           (insert "| " (ht-get my-it "subreddit_name_prefixed") " | ")
           (insert "score: " (format "%s" (gethash "score" my-it) ) " | ")
           (insert "comments: " (format "%s" (gethash "num_comments" my-it)) " | ")
           (insert "created: " (format-time-string "%Y-%m-%d" (gethash "created_utc" my-it)) "\n")
           (let ((selftext (gethash "selftext" my-it)) begin end)
             (if (string-empty-p selftext)
                 (insert (format "%s \n[[eww:%s][view in eww]]\n"
                                 (gethash "url" my-it) (gethash "url" my-it)))
               (setq begin (point-marker))
               (insert "\n" selftext "\n")
               (setq end (point-marker))
               (reddigg--sanitize-range begin end)
               (reddigg--mark-body-overlay begin end)))
           (insert (format "[[elisp:(reddigg--view-comments \"%s\")][view comments]]\n"
                           (ht-get my-it "permalink")))))
       (ht-get data "children"))
      (let ((after (ht-get data "after")))
        (when after
          (insert (format "* [[elisp:(reddigg--view-sub-more \"%s\" \"%s\" '%s '%s)][More]]"
                          sub after sort scope))))
      (reddigg--ensure-modes))))

(defun reddigg--sanitize-range (begin end)
  "Remove heading * inside rang between BEGIN and END."
  (save-excursion
    (dolist (it reddigg-replacement-list)
      (goto-char begin)
      (while (re-search-forward (car it) end t)
        (replace-match (cdr it))))))

(defun reddigg--print-comment-list (cmt-list level)
  "Print comments from CMT-LIST with LEVEL."
  (seq-do
   (lambda (it)
     (let* ((kind (ht-get it "kind"))
            (data (ht-get it "data"))
            (replies (ht-get data "replies"))
            (depth (ht-get data "depth"))
            (my-level (make-string (+ 2 depth) ?*))
            begin end)
       (if (string= kind "more")
           ;; (insert level " reddigg: too many subcomments\n")
           (insert my-level
                   (format " [[elisp:(reddigg--view-more-cmts \"%s\" \"%s\")][load more comments (%s)]]\n"
                           level
                           (mapconcat #'identity (ht-get data "children") ",")
                           (ht-get data "count")))

         (insert my-level " " (ht-get data "author") "\n")
         (insert ":PROPERTIES:\n")
         (insert (format ":REDDIGG_ID: %s\n" (ht-get data "name")))
         (insert (format ":REDDIGG_AUTHOR: %s\n" (ht-get data "author")))
         (insert (format ":REDDIGG_SUBREDDIT: %s\n" (ht-get data "subreddit")))
         (insert (format ":REDDIGG_LIKES: %s\n" (reddigg--likes->string (ht-get data "likes"))))
         (insert (format ":REDDIGG_SCORE: %s\n" (ht-get data "score")))
         (insert (format ":REDDIGG_CREATED: %s\n" (reddigg--format-created (ht-get data "created_utc"))))
         (insert ":END:\n")
         (setq begin (point-marker))
         (insert (ht-get data "body") "\n")
         (setq end (point-marker))
         (reddigg--sanitize-range begin end)
         (reddigg--mark-body-overlay begin end)
         (when (hash-table-p replies)
           (reddigg--print-comment-list (ht-get* replies "data" "children") (concat level "*"))))))
   cmt-list))

(defun reddigg--print-comment-1 (data)
  "Print the post itself as a level-1 heading, so it's a full reddigg
entry like any post or comment -- votable, replyable, editable, and
deletable, same as everything nested under it.
Return the value of `reddigg--cmt-list-id'."
  (let ((cmt (ht-get* (aref (ht-get* data "data" "children") 0) "data")) begin end)
    (insert "* " (ht-get cmt "title") "\n")
    (insert ":PROPERTIES:\n")
    (insert (format ":REDDIGG_ID: %s\n" (ht-get cmt "name")))
    (insert (format ":REDDIGG_AUTHOR: %s\n" (ht-get cmt "author")))
    (insert (format ":REDDIGG_SUBREDDIT: %s\n" (ht-get cmt "subreddit")))
    (insert (format ":REDDIGG_LIKES: %s\n" (reddigg--likes->string (ht-get cmt "likes"))))
    (insert (format ":REDDIGG_SCORE: %s\n" (ht-get cmt "score")))
    (insert (format ":REDDIGG_CREATED: %s\n" (reddigg--format-created (ht-get cmt "created_utc"))))
    (insert ":END:\n")
    (insert "| " (ht-get cmt "subreddit_name_prefixed") " | ")
    (insert "score: " (format "%s" (ht-get cmt "score")) " | ")
    (insert "author: " (ht-get cmt "author") " | ")
    (insert "created: " (reddigg--format-created (ht-get cmt "created_utc")) "\n")
    (insert (ht-get cmt "url") "\n")
    (insert (format "[[elisp:(reddigg--view-comments \"%s\" t)][refresh]]\n"
                    (ht-get cmt "permalink")))
    (setq begin (point-marker))
    (insert (gethash "selftext" cmt) "\n")
    (setq end (point-marker))
    (reddigg--sanitize-range begin end)
    (reddigg--mark-body-overlay begin end)
    ;; get value for `reddigg--cmt-list-id'
    (ht-get cmt "name")))

(defun reddigg--print-comment-2 (data level)
  "Extrac comment list from DATA and pass it along with LEVEL."
  (reddigg--print-comment-list (ht-get* data "data" "children") level))

(defun reddigg--print-comments (data)
  "Print comments DATA to buffer."
  (with-current-buffer (reddigg--get-cmt-buffer)
    (erase-buffer)
    (insert "#+startup: show2levels indent\n")
    (insert (format "#+title: comments for '%s'\n"
                    (ht-get* (aref (ht-get* (aref data 0) "data" "children") 0) "data" "title")))
    (let ((post-id (reddigg--print-comment-1 (aref data 0))))
      (reddigg--print-comment-2 (aref data 1) "*")
      (reddigg--ensure-modes)
      ;; `overview' startup visibility (set by `reddigg--ensure-modes' via
      ;; `org-set-startup-visibility') folds the post's own body along
      ;; with all the comments underneath it -- reveal just the post's
      ;; own entry (heading + body, not its comment children) so it
      ;; still reads the way it always has.
      (goto-char (point-min))
      (when (re-search-forward "^\\* " nil t)
        (beginning-of-line)
        (org-show-entry))
      ;; must set here after org-mode is in otherwise when org-mode kicks in all
      ;; local variables will be killed
      (setq reddigg--cmt-list-id post-id))))

;;;###autoload
(defun reddigg-view-comments (cmt)
  "Ask and print CMT to buffer."
  (interactive "sComment: ")
  (when (string-prefix-p "https" cmt)
    (setq cmt
          (substring cmt
                     (length "https://old.reddit.com/") nil)))
  (reddigg--view-comments cmt))

(defun reddigg--view-comments (cmt &optional new-window)
  "Ask and print CMT to buffer. When NEW-WINDOW will show in new buffer."
  (promise-chain (reddigg--promise-comments cmt)
    (then #'reddigg--print-comments)
    (then (lambda (&rest _)
            (if new-window
                (switch-to-buffer (reddigg--get-cmt-buffer))
              (select-window
               (display-buffer
                (reddigg--get-cmt-buffer)
                '(display-buffer-use-some-window (inhibit-same-window . t)))))))
    (promise-catch (lambda (reason)
                     (message "catch error in promise: %s" reason)))))


(cl-defun reddigg--view-sub (sub &key after before append sort scope)
  "Fetch SUB and print its post list.
AFTER: fetch post after name.
BEFORE: fetch posts before name.
APPEND: tell `reddigg--print-sub' to append.
SORT: top, hot, best, rising, controversial.
SCOPE: hour, day, week, year, all."
  (promise-chain (reddigg--promise-posts sub
                                         :after after
                                         :before before
                                         :sort sort
                                         :scope scope)
    (then (lambda (result)
            (ht-get result "data")))
    (then (lambda (data)
            (reddigg--print-sub data sub append
                                :sort sort
                                :scope scope)))
    (then (lambda (&rest _)
            (switch-to-buffer (reddigg--get-buffer))))
    (promise-catch (lambda (reason)
                     (message "catch error in promise: %s" reason)))))

(defun reddigg--view-more-cmts (level children)
  "Get more comments from CHILDREN and print at LEVEL."
  (promise-chain (reddigg--promise-more-comments children)
    (then (lambda (result)
            (ht-get* result "json" "data" "things")))
    (then (lambda (result)
            (kill-whole-line)
            (save-excursion
              (reddigg--print-comment-list result level))))
    (promise-catch (lambda (reason)
                     (message "catch error in promise: %s" reason)))))

;;;###autoload
(defun reddigg-view-sub (sub)
  "Prompt SUB and print its post list."
  (interactive (list (completing-read "Select sub reddit:" reddigg-subs)))
  (reddigg--view-sub sub))

;;;###autoload
(defun reddigg-view-frontpage ()
  "View frontpage."
  (interactive)
  (reddigg--view-sub (mapconcat #'symbol-name reddigg-subs "+")))

(defun reddigg--view-sub-more (sub after sort scope)
  "Fetch SUB from AFTER and append."
  (reddigg--view-sub sub :after after :append t :sort sort :scope scope))

;;;###autoload
(defun reddigg-view-main ()
  "View main page."
  (interactive)
  (switch-to-buffer (or (get-buffer reddigg--main-buffer)
                        (with-current-buffer (reddigg--get-main-buffer)
                          (erase-buffer)
                          (insert "#+startup: overview indent\n")
                          (insert "#+title: main\n\n")
                          (insert (format reddigg--template-sub "all" "all"))
                          (insert (format reddigg--template-sub "popular" "popular"))
                          (insert (format reddigg--template-sub (mapconcat #'symbol-name reddigg-subs "+") "main"))
                          (dolist (sub reddigg-subs)
                            (insert (format reddigg--template-sub sub sub)))
                          (reddigg--ensure-modes)
                          reddigg--main-buffer))))

(provide 'reddigg)
;;; reddigg.el ends here
