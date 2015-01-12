;;; youdao-dictionary.el --- Youdao Dictionary interface for Emacs

;; Copyright © 2015 Chunyang Xu

;; Author: Chunyang Xu <xuchunyang56@gmail.com>
;; URL: https://github.com/xuchunyang/youdao-dictionary.el
;; Package-Requires: ((popup "0.5.0") (chinese-word-at-point "0.2"))
;; Version: 0.1
;; Created: 11 Jan 2015
;; Keywords: convenience, Chinese, dictionary

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
;;
;; A simple Interface for Youdao Dictionary (http://dict.youdao.com)
;;
;; Below are commands you can use:
;;
;; `youdao-dictionary-search-point'
;; Search word at point and show result in echo area
;; `youdao-dictionary-search-point+'
;; Search word at point and show result with popup widget
;; `youdao-dictionary-search-input'
;; Search input word and show result in echo area
;; `youdao-dictionary-search-input+'
;; Search input word and show result with popup widget

;;; Installation:
;;
;; This package requires `popup' and `chinese-word-at-point'
;;

;;; Code:
(require 'json)
(require 'url)

(require 'chinese-word-at-point)
(require 'popup)

(defconst youdao-dictionary-api-url
  "http://fanyi.youdao.com/openapi.do?keyfrom=YouDaoCV&key=659600698&type=data&doctype=json&version=1.1&q=%s"
  "Youdao dictionary API template, URL `http://dict.youdao.com/'.")

(defun youdao-dictionary--format-request-url (query-word)
  "Format QUERY-WORD as a HTTP request URL."
  (format youdao-dictionary-api-url query-word))

(defun youdao-dictionary--request-word (word)
  "Request WORD, return JSON as an alist if successes."
  (let (json)
    (with-current-buffer (url-retrieve-synchronously
                          (youdao-dictionary--format-request-url word))
      (set-buffer-multibyte t)
      (goto-char (point-min))
      (when (not (string-match "200 OK" (buffer-string)))
        (error "Problem connecting to the server"))
      (re-search-forward "^$" nil 'move)
      (setq json (json-read-from-string
                  (buffer-substring-no-properties (point) (point-max))))
      (kill-buffer (current-buffer)))
    json))

(defun youdao-dictionary--translation (json)
  "Return translation as a string extracted from JSON (alist)."
  (elt (cdr (assoc 'translation json)) 0))

(defun youdao-dictionary--phonetic (json)
  "Return phonetic as a string extracted from JSON."
  (cdr (assoc 'phonetic (cdr (assoc 'basic json)))))

(defun youdao-dictionary--explains (json)
  "Return explains as a vector extracted from JSON."
  (cdr (assoc 'explains (cdr (assoc 'basic json)))))

(defun youdao-dictionary--web-phrases (json)
  "Return web phrases as a vector extracted from JSON."
  (cdr (assoc 'web json)))

(defun youdao-dictionary--prompt-input ()
  "Prompt input object for translate."
  (let ((current-word (thing-at-point 'chinese-or-other-word)))
    (read-string (format "Word (%s): "
                         (or current-word ""))
                 nil nil
                 current-word)))

;;;###autoload
(defun youdao-dictionary-search-point ()
  "Search word at point and display in echo area."
  (interactive)
  (message (youdao-dictionary--translation
            (youdao-dictionary--request-word
             (thing-at-point 'chinese-or-other-word) ; TODO: deal with nil
             ))))

;;;###autoload
(defun youdao-dictionary-search-point+ ()
  "Search word at point and display in popup."
  (interactive)
  (popup-tip (youdao-dictionary--translation
              (youdao-dictionary--request-word
               (thing-at-point 'chinese-or-other-word) ; TODO: deal with nil
               ))))
;;;###autoload
(defun youdao-dictionary-search-input ()
  "Search input word and display in echo area."
  (interactive)
  (let ((word (youdao-dictionary--prompt-input)))
    (message (youdao-dictionary--translation
              (youdao-dictionary--request-word word)))))
;;;###autoload
(defun youdao-dictionary-search-input+ ()
  "Search input word and display in popup."
  (interactive)
  (let ((word (youdao-dictionary--prompt-input)))
    (popup-tip (youdao-dictionary--translation
                (youdao-dictionary--request-word word)))))

(provide 'youdao-dictionary)

;; Local Variables:
;; coding: utf-8
;; End:

;;; youdao-dictionary.el ends here
