;;; reddigg.el --- A reader for redditt -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Thanh Vuong

;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/thanhvg/emacs-reddigg
;; Package-Requires: ((emacs "26.3") (promise "1.1") (ht "2.3") (org "9.2"))
;; Version: 0.6.0

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

(defun reddigg--parse-json-string (text)
  "Parse JSON TEXT the way reddit's API responses should be read:
hash-tables for objects, nil for both JSON null and false."
  (if (fboundp 'json-parse-string)
      (json-parse-string text
                          :object-type 'hash-table
                          :null-object nil
                          :false-object nil)
    (let ((json-array-type 'vector)
          (json-object-type 'hash-table)
          (json-false nil))
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
  (visual-line-mode))

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

(defun reddigg--promise-json (url)
  "Promise the JSON at URL, fetched from inside a live reddit tab via browsel."
  (reddigg--ensure-browsel)
  (promise-new
   (lambda (resolve reject)
     (condition-case err
         (let ((tab-id (reddigg--reddit-tab-id t)))
           (browsel-request-async
            "EVAL_IN_ACTIVE_TAB"
            (list :tabId tab-id :code (reddigg--fetch-js url))
            (lambda (response)
              (reddigg--handle-eval-response response resolve reject))
            reddigg-browsel-client))
       (error (funcall reject err))))))

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
