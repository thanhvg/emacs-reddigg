;;; reddigg.el --- A reader for redditt -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Thanh Vuong

;; Author: Thanh Vuong <thanhvg@gmail.com>
;; Package-Requires: ((emacs "25.1") (promise "1.1") (request "0.3.0") (org "9.2"))
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


(require 'promise)
(require 'request)
(require 'cl-lib)
(require 'org)

;; https://github.com/flycheck/flycheck/pull/1723/files
(defconst reddigg--json-parser
  (if (and (functionp 'json-parse-buffer)
           ;; json-parse-buffer only supports keyword arguments in Emacs 27+
           (>= emacs-major-version 27))
      #'json-parse-buffer
    #'json-read)
  "Function to use to parse JSON strings.")

(defun reddigg--json-parser-fn (&rest args)
  (apply reddigg--json-parser args))

(defconst reddigg--sub-url
  "https://www.reddit.com/r/%s.json")


(defun reddigg--get-post (subreddit)
  (promise-new
   (lambda (resolve reject)
     (request (format reddigg--sub-url subreddit)
       :headers `(("User-Agent" . "fun"))
       :parser 'reddigg--json-parser-fn
       ;; :parser 'json-read
       :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                             (funcall reject  error-thrown)))
       :success (cl-function (lambda (&key data &allow-other-keys)
                               (funcall resolve data)))))))

(promise-chain (reddigg--get-post 'emacs)
  (then (lambda (result)
          ;; (setq thanh result)
          (gethash "children" (gethash "data" result))))
  (then #'reddigg--print-subreddit)
  (promise-catch (lambda (reason)
                   (message "catch error in promise: %s" reason))))

;; (gethash "data" thanh)
(setq thanh-3 (gethash "children" (gethash "data" thanh)))
(vectorp thanh-3)
(setq thanh-4 (aref thanh-3 0))

(gethash "url_overridden_by_dest" (gethash "data" thanh-4))
(gethash "author_fullname" (gethash "data" thanh-4))
(gethash "author" (gethash "data" thanh-4))
(gethash "ups" (gethash "data" thanh-4))
(gethash "score" (gethash "data" thanh-4))
(gethash "num_comments" (gethash "data" thanh-4))
(gethash "url" (gethash "data" thanh-4))
(gethash "title" (gethash "data" thanh-4))
(gethash "permalink" (gethash "data" thanh-4))


(defvar reddigg--buffer "*reddigg*"
  "Buffer for main page.")

(defvar reddigg--cmt-buffer "*reddigg-comments*"
  "Comment buffer.")

(defun reddigg--get-buffer ()
  (get-buffer-create reddigg--buffer))

(defun reddigg--get-cmt-buffer ()
  (get-buffer-create reddigg--cmt-buffer))

(defun reddigg--print-subreddit (data)
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
  (save-excursion
    (goto-char begin)
    (while (re-search-forward "^\\* " end t)
      (replace-match "- "))))

(if (string-empty-p "") (message "yay"))
(reddigg--print-subreddit thanh-3)
(switch-to-buffer (reddigg--get-buffer))
