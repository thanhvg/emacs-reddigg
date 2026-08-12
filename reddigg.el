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
;; * OAuth setup (required as of 0.7.0)
;; Reddit's unauthenticated old.reddit.com/*.json endpoints are no longer a
;; reliable free option, so this package now talks to oauth.reddit.com using
;; an app-only ("client_credentials") OAuth2 token. This is still free and
;; does not require your reddit username/password, since reddigg only ever
;; reads public data.
;;
;; To set it up:
;; 1. Go to https://www.reddit.com/prefs/apps and click "create app" /
;;    "create another app".
;; 2. Choose type "script". Redirect URI can be anything, e.g.
;;    http://localhost:8080 -- it is not actually used by this flow.
;; 3. After creating it you'll see a client ID (short string under the app
;;    name) and a "secret" field.
;; 4. Set `reddigg-client-id' and `reddigg-client-secret' accordingly, e.g.
;;    via customize, or better, via auth-source (~/.authinfo.gpg):
;;      machine reddigg.el login <client-id> password <client-secret>
;;    and let `reddigg--auth-from-auth-source' pick it up automatically.
;; 5. Set `reddigg-user-agent' to something unique identifying you, per
;;    Reddit's API rules, e.g. "emacs:reddigg:0.7.0 (by /u/yourname)".
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
;; reddigg-client-id: OAuth client id from your reddit "script" app
;; reddigg-client-secret: OAuth client secret from your reddit "script" app
;; reddigg-user-agent: a unique user agent string, required by reddit
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
(require 'url)
(require 'cl-lib)
(require 'ht)
(require 'org)
(require 'json)
(require 'url-util)
(require 'auth-source)

(defgroup reddigg nil
  "Search and read stackoverflow and sisters's sites."
  :group 'convenience
  :link '(emacs-commentary-link "reddigg.el"))

(defcustom reddigg-subs '(acmilan emacs starcraft)
  "List of subreddits."
  :type 'list
  :group 'reddigg)

(defcustom reddigg-client-id nil
  "Client id of your reddit \"script\" app.
Create one at https://www.reddit.com/prefs/apps.
Prefer setting this via auth-source instead of customize/plain text."
  :type '(choice (const nil) string)
  :group 'reddigg)

(defcustom reddigg-client-secret nil
  "Client secret of your reddit \"script\" app.
Prefer setting this via auth-source instead of customize/plain text."
  :type '(choice (const nil) string)
  :group 'reddigg)

(defcustom reddigg-user-agent "emacs:reddigg:0.7.0 (by /u/unknown)"
  "User agent string sent with every request.
Reddit requires a unique, descriptive user agent; requests with a
generic or missing one are aggressively rate limited.  Recommended
form: \"platform:app-id:version (by /u/your-username)\"."
  :type 'string
  :group 'reddigg)

(defcustom reddigg-auth-source-host "reddigg.el"
  "Host key to look up client id/secret in auth-source.
Add a line like the following to ~/.authinfo(.gpg):
  machine reddigg.el login YOUR_CLIENT_ID password YOUR_CLIENT_SECRET"
  :type 'string
  :group 'reddigg)

(defvar reddigg--token nil
  "Cached OAuth2 access token.")

(defvar reddigg--token-expiry 0
  "Float-time at which `reddigg--token' expires.")

(defun reddigg--parse-json-buffer ()
  "Read json from buffer."
  (if (fboundp 'json-parse-buffer)
      (json-parse-buffer
       :object-type 'hash-table
       :null-object nil
       :false-object nil)
    (let ((json-array-type 'vector)
          (json-object-type 'hash-table)
          (json-false nil))
      (json-read))))

(defconst reddigg--token-url
  "https://www.reddit.com/api/v1/access_token"
  "OAuth2 token endpoint.")

(defconst reddigg--sub-url
  "https://oauth.reddit.com/r/%s?count=25&raw_json=1"
  "Sub reddit template.")

(defconst reddigg--sub-view-sort-url
  "https://oauth.reddit.com/r/%s/%s?count=25&raw_json=1"
  "Sub reddit template for new and rising.")

(defconst reddigg--sub-view-sort-scope-url
  "https://oauth.reddit.com/r/%s/%s?count=25&raw_json=1&sort=%s&t=%s"
  "Sub reddit template for top and controversial.")

(defconst reddigg--cmt-url
  "https://oauth.reddit.com/%s?raw_json=1"
  "Comment link template.")

(defconst reddigg--cmt-more-url
  "https://oauth.reddit.com/api/morechildren?api_type=json&raw_json=1&link_id=%s&children=%s"
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
  (visual-line-mode))

(defvar reddigg-replacement-list
  '(("^\\* " . "- ")
    ("&gt;" . ">")
    ("&lt;" . "<")
    ("&amp;#x200B;" . "\n")
    ("&amp;nbsp;" . "\n")
    ("&amp;" . "&"))
  "List of (find . replace) to sanitize the text in range.
With raw_json=1 (used by the OAuth API) reddit no longer double-escapes
entities, but this is kept for safety and for the heading-clash fix.")

(defvar-local reddigg--cmt-list-id nil
  "ID/name of the current comment list.")


;;; OAuth2 token handling

(defun reddigg--auth-from-auth-source ()
  "Return (client-id . client-secret) from auth-source, or nil.
Looks up `reddigg-auth-source-host' as the host/machine."
  (let ((found (car (auth-source-search :host reddigg-auth-source-host
                                        :require '(:user :secret)
                                        :max 1))))
    (when found
      (cons (plist-get found :user)
            (let ((secret (plist-get found :secret)))
              (if (functionp secret) (funcall secret) secret))))))

(defun reddigg--client-id-secret ()
  "Resolve (client-id . client-secret) from customize vars or auth-source."
  (if (and reddigg-client-id reddigg-client-secret)
      (cons reddigg-client-id reddigg-client-secret)
    (or (reddigg--auth-from-auth-source)
        (user-error
         "reddigg: set `reddigg-client-id'/`reddigg-client-secret', or add a %S entry to auth-source. See reddigg.el commentary for setup instructions"
         reddigg-auth-source-host))))

(defun reddigg--fetch-token ()
  "Fetch a fresh app-only OAuth2 token, cache it, and return a promise of it."
  (promise-new
   (lambda (resolve reject)
     (let* ((cred (reddigg--client-id-secret))
            (client-id (car cred))
            (client-secret (cdr cred))
            (url-request-method "POST")
            (url-request-extra-headers
             `(("Authorization" . ,(concat "Basic "
                    (base64-encode-string
                     (concat client-id ":" client-secret) t)))
               ("Content-Type" . "application/x-www-form-urlencoded")
               ("User-Agent" . ,reddigg-user-agent)))
            (url-request-data "grant_type=client_credentials"))
       (url-retrieve
        reddigg--token-url
        (lambda (status)
          (if (plist-get status :error)
              (funcall reject (plist-get status :error))
            (condition-case ex
                (with-current-buffer (current-buffer)
                  (if (not (url-http-parse-headers))
                      (funcall reject (buffer-string))
                    (goto-char url-http-end-of-headers)
                    (let* ((json (reddigg--parse-json-buffer))
                           (token (gethash "access_token" json))
                           (expires (gethash "expires_in" json))
                           (err (gethash "error" json)))
                      (cond
                       (err (funcall reject (format "reddit oauth error: %s" err)))
                       ((not token) (funcall reject "reddigg: no access_token in response"))
                       (t
                        (setq reddigg--token token
                              reddigg--token-expiry (+ (float-time) (or expires 3600)))
                        (funcall resolve token))))))
              (error (funcall reject ex)))))
        nil t)))))

(defun reddigg--ensure-token ()
  "Return a promise resolving to a valid, non-expired access token."
  (if (and reddigg--token (< (float-time) (- reddigg--token-expiry 30)))
      (promise-resolve reddigg--token)
    (reddigg--fetch-token)))

(defun reddigg--invalidate-token ()
  "Drop the cached token, forcing a refetch on next request."
  (setq reddigg--token nil
        reddigg--token-expiry 0))


;;; HTTP / promise plumbing

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

(defun reddigg--promise-json-request (url)
  "Perform a single authenticated GET to URL and promise the parsed JSON.
Rejects with the symbol `reddigg--unauthorized' on a 401 response so
the caller can refresh the token and retry."
  (promise-then
   (reddigg--ensure-token)
   (lambda (token)
     (promise-new
      (lambda (resolve reject)
        (let ((url-request-extra-headers
               `(("Authorization" . ,(concat "Bearer " token))
                 ("User-Agent" . ,reddigg-user-agent))))
          (url-retrieve
           (url-encode-url url)
           (lambda (status)
             (if (plist-get status :error)
                 (funcall reject (plist-get status :error))
               (condition-case ex
                   (with-current-buffer (current-buffer)
                     (let ((code (url-http-parse-headers)))
                       (cond
                        ((not code) (funcall reject (buffer-string)))
                        ((eq url-http-response-status 401)
                         (funcall reject 'reddigg--unauthorized))
                        ((eq url-http-response-status 429)
                         (funcall reject "reddigg: rate limited (429) by reddit, try again shortly"))
                        (t
                         (goto-char url-http-end-of-headers)
                         (funcall resolve (reddigg--parse-json-buffer))))))
                 (error (funcall reject ex)))))
           nil t)))))))

(defun reddigg--promise-json (url)
  "Promise a json from URL, transparently refreshing the token once on 401."
  (promise-catch
   (reddigg--promise-json-request url)
   (lambda (reason)
     (if (eq reason 'reddigg--unauthorized)
         (progn
           (reddigg--invalidate-token)
           (reddigg--promise-json-request url))
       (promise-new (lambda (_resolve reject) (funcall reject reason)))))))

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
           (insert "| " (ht-get my-it "subreddit_name_prefixed") " | ")
           (insert "score: " (format "%s" (gethash "score" my-it) ) " | ")
           (insert "comments: " (format "%s" (gethash "num_comments" my-it)) " | ")
           (insert "created: " (format-time-string "%Y-%m-%d" (gethash "created_utc" my-it)) "\n")
           (let ((selftext (gethash "selftext" my-it)) begin end)
             (if (string-empty-p selftext)
                 (insert (format "%s \n[[eww:%s][view in eww]]\n"
                                 (gethash "url" my-it) (gethash "url" my-it)))
               (setq begin (point))
               (insert "\n" selftext "\n")
               (setq end (point))
               (reddigg--sanitize-range begin end)))
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
            (my-level (make-string (1+ depth) ?*))
            begin end)
       (if (string= kind "more")
           ;; (insert level " reddigg: too many subcomments\n")
           (insert my-level
                   (format " [[elisp:(reddigg--view-more-cmts \"%s\" \"%s\")][load more comments (%s)]]\n"
                           level
                           (mapconcat #'identity (ht-get data "children") ",")
                           (ht-get data "count")))

         (insert my-level " " (ht-get data "author") "\n")
         (setq begin (point))
         (insert (ht-get data "body") "\n")
         (setq end (point))
         (reddigg--sanitize-range begin end)
         (when (hash-table-p replies)
           (reddigg--print-comment-list (ht-get* replies "data" "children") (concat level "*"))))))
   cmt-list))

(defun reddigg--print-comment-1 (data)
  "Print the post content from DATA.
Return a value of `reddigg--cmt-list-id'"
  (let ((cmt (ht-get* (aref (ht-get* data "data" "children") 0) "data")) begin end)
    (insert (ht-get cmt "url") "\n")
    (insert "author: " (ht-get cmt "author") "\n")
    (insert (format "[[elisp:(reddigg--view-comments \"%s\" t)][refresh]]\n"
                    (ht-get cmt "permalink")))
    (setq begin (point))
    (insert (gethash "selftext" cmt) "\n")
    (setq end (point))
    (reddigg--sanitize-range begin end)
    ;; get value for `reddigg--cmt-list-id'
    (ht-get cmt "name")))

(defun reddigg--print-comment-2 (data level)
  "Extrac comment list from DATA and pass it along with LEVEL."
  (reddigg--print-comment-list (ht-get* data "data" "children") level))

(defun reddigg--print-comments (data)
  "Print comments DATA to buffer."
  (with-current-buffer (reddigg--get-cmt-buffer)
    (erase-buffer)
    (insert "#+startup: overview indent\n")
    (insert (format "#+title: comments for '%s'\n"
                    (ht-get* (aref (ht-get* (aref data 0) "data" "children") 0) "data" "title")))
    (let ((post-id (reddigg--print-comment-1 (aref data 0))))
      (reddigg--print-comment-2 (aref data 1) "*")
      (reddigg--ensure-modes)
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
