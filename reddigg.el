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
    (request (format reddigg--sub-url 'acmilan)
      :headers `(("User-Agent" . "fun"))
      :parser 'reddigg--json-parser-fn
      ;; :parser 'json-read
      :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                            (funcall reject  error-thrown)))
      :success (cl-function (lambda (&key data &allow-other-keys)
                              (funcall resolve data)))))))

(promise-chain (reddigg--get-post 'acmilan)
  (then (lambda (result)
          (setq thanh result)))
  (promise-catch (lambda (reason)
                   (message "catch error in promise: %s" reason))))

;; (gethash "data" thanh)
(gethash "children" (gethash "data" thanh))
