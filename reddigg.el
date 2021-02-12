;;; reddigg.el --- A reader for redditt -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Thanh Vuong

;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/thanhvg/emacs-hnreader/
;; Package-Requires: ((emacs "26.3") (promise "1.1") (ht "2.3") (request "0.3.0") (org "9.2"))
;; Version: 0.1

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
;; TODO


;;; Code:

(require 'promise)
(require 'request)
(require 'cl-lib)
(require 'ht)
(require 'org)
(require 'json)



;; https://github.com/flycheck/flycheck/pull/1723/files
(defconst reddigg--json-parser
  (if (and (functionp 'json-parse-buffer)
           ;; json-parse-buffer only supports keyword arguments in Emacs 27+
           (>= emacs-major-version 27))
      'json-parse-buffer
    'json-read)
  "Function to use to parse JSON strings.")

(defun reddigg--json-parser-fn (&rest args)
  "Parse json from buffer with ARGS."
  (apply reddigg--json-parser args))

(defconst reddigg--sub-url
  "https://www.reddit.com/r/%s.json"
  "Sub reddit template.")

(defconst reddigg--cmt-url
  "https://www.reddit.com/%s.json"
  "Comment link template.")


(defun reddigg--promise-posts (sub)
  "Promise SUB post list."
  (reddigg--promise-json (format reddigg--sub-url sub)))

(defun reddigg--promise-comments (cmt)
  "Promise CMT list."
  (reddigg--promise-json (format reddigg--cmt-url cmt)))

(defun reddigg--promise-json (url)
  "Promise a json from URL."
  (promise-new
   (lambda (resolve reject)
     (request url
       :headers `(("User-Agent" . "fun"))
       :parser 'reddigg--json-parser-fn
       ;; :parser 'json-read
       :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                             (funcall reject  error-thrown)))
       :success (cl-function (lambda (&key data &allow-other-keys)
                               (message "got result")
                               (funcall resolve data)))))))

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

(defun reddigg--print-sub (data)
  "Print sub post list in DATA."
  (with-current-buffer (reddigg--get-buffer)
    (erase-buffer)
    (insert "#+startup: overview indent\n")
    (insert "#+title: subreddit\n")
    (seq-do
     (lambda (it)
       (let ((my-it (gethash "data" it)))
         (insert "* " (gethash "title" my-it) "\n")
         (insert "score: " (format "%s" (gethash "score" my-it) ) "\n")
         (insert "comments: " (format "%s" (gethash "num_comments" my-it)) "\n")
         (let ((selftext (gethash "selftext" my-it)) begin end)
           (if (string-empty-p selftext)
               (insert "url: " (gethash "url" my-it) "\n")
             (setq begin (point))
             (insert "\n" selftext "\n")
             (setq end (point))
             (reddigg--sanitize-range begin end)))))
     data)))

(defun reddigg--sanitize-range (begin end)
  "Remove heading * inside rang between BEGIN and END."
  (save-excursion
    (goto-char begin)
    (while (re-search-forward "^\\* " end t)
      (replace-match "- "))))

(defun reddigg-show-sub (sub)
  "Prompt SUB and print its post list."
  (interactive "sQuery: ")
  (promise-chain (reddigg--promise-posts sub)
    (then (lambda (result)
            ;; (setq thanh nil)
            ;; (setq thanh result)
            (gethash "children" (gethash "data" result))))
    (then #'reddigg--print-sub)
    (then (lambda (&rest _)
            (switch-to-buffer (reddigg--get-buffer))))
    (promise-catch (lambda (reason)
                     (message "catch error in promise: %s" reason)))))

(defun reddigg--print-comment-1 (data)
  "Print the post content from DATA."
  (let ((cmt (ht-get* (aref (ht-get* data "data" "children") 0) "data")))
    (insert (gethash "selftext" cmt) "\n")))


(defun reddigg--print-comment-list (cmt-list level)
  "Pritn comments from CMT-LIST with LEVEL."
  (seq-do
   (lambda (it)
     (let* ((data (ht-get it "data"))
            (replies (ht-get data "replies"))
            begin end)
       (insert level " " (ht-get data "author") "\n")
       (setq begin (point))
       (insert (ht-get data "body") "\n")
       (setq end (point))
       (reddigg--sanitize-range begin end)
       (when (hash-table-p replies)
         (reddigg--print-comment-list (ht-get* replies "data" "children") (concat level "*")))))
   cmt-list))

(defun reddigg--print-comment-2 (data level)
  "Extrac comment list from DATA and pass it along with LEVEL."
  (reddigg--print-comment-list (ht-get* data "data" "children") level))

(defun reddigg--print-comments (data)
  "Print comments DATA to buffer."
  (with-current-buffer (reddigg--get-cmt-buffer)
    (erase-buffer)
    (insert "#+startup: overview indent\n")
    (insert "#+title: comments\n")
    (reddigg--print-comment-1 (aref data 0))
    (reddigg--print-comment-2 (aref data 1) "*")))

;; r/emacs/comments/lg4iw6/emacs_keyboard_shortcuts_in_a_table_that_can_be

(defun reddigg-show-comments (cmt)
  "Aske and print CMT to buffer."
  (interactive "sQuery: ")
  (promise-chain (reddigg--promise-comments cmt)
    (then #'reddigg--print-comments)
    (then (lambda (&rest _)
            (switch-to-buffer (reddigg--get-cmt-buffer))))
    (promise-catch (lambda (reason)
                     (message "catch error in promise: %s" reason)))))


(reddigg-show-comments "r/emacs/comments/lg4iw6/emacs_keyboard_shortcuts_in_a_table_that_can_be")

(provide 'reddigg)
;;; reddigg.el ends here
