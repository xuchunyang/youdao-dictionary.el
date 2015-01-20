;;; youdao-dictionary.el --- Youdao Dictionary interface for Emacs

;; Copyright © 2015 Chunyang Xu

;; Author: Chunyang Xu <xuchunyang56@gmail.com>
;; URL: https://github.com/xuchunyang/youdao-dictionary.el
;; Package-Requires: ((popup "0.5.0") (chinese-word-at-point "0.2") (names "0.5") (emacs "24"))
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

;;; Code:
(require 'json)
(require 'url)
(require 'chinese-word-at-point)
(require 'popup)

;;;###autoload
(define-namespace youdao-dictionary-

(defconst api-url
  "http://fanyi.youdao.com/openapi.do?keyfrom=YouDaoCV&key=659600698&type=data&doctype=json&version=1.1&q=%s"
  "Youdao dictionary API template, URL `http://dict.youdao.com/'.")

(defun -format-request-url (query-word)
  "Format QUERY-WORD as a HTTP request URL."
  (format api-url query-word))

(defun -request (word)
  "Request WORD, return JSON as an alist if successes."
  (let (json)
    (with-current-buffer (url-retrieve-synchronously
                          (-format-request-url word))
      (set-buffer-multibyte t)
      (goto-char (point-min))
      (when (not (string-match "200 OK" (buffer-string)))
        (error "Problem connecting to the server"))
      (re-search-forward "^$" nil 'move)
      (setq json (json-read-from-string
                  (buffer-substring-no-properties (point) (point-max))))
      (kill-buffer (current-buffer)))
    json))

(defun -translation (json)
  "Return translation as a string extracted from JSON (alist)."
  (elt (cdr (assoc 'translation json)) 0))

(defun -phonetic (json)
  "Return phonetic as a string extracted from JSON."
  (cdr (assoc 'phonetic (cdr (assoc 'basic json)))))

(defun -explains (json)
  "Return explains as a vector extracted from JSON."
  (cdr (assoc 'explains (cdr (assoc 'basic json)))))

(defun -web-phrases (json)
  "Return web phrases as a vector extracted from JSON."
  (cdr (assoc 'web json)))

(defun -prompt-input ()
  "Prompt input object for translate."
  (let ((current-word (-region-or-word)))
    (read-string (format "Word (%s): "
                         (or current-word ""))
                 nil nil
                 current-word)))

(defun -strip-explain (explain)
  "Remove unneed info in EXPLAIN for replace.

i.e. `[语][计] dictionary' => 'dictionary'."
  (replace-regexp-in-string "^[[].* " "" explain))

(defun -region-or-word ()
  "Return region or word at point."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning)
                                      (region-end))
    (thing-at-point 'chinese-or-other-word t)))

:autoload
(defun search-point ()
  "Search word at point or region and display in echo area."
  (interactive)
  (let ((word (-region-or-word)))
    (if word
        (message (-translation
                  (-request word)))
      (message "No word at point."))))

:autoload
(defun search-point+ ()
  "Search word at point and display in popup."
  (interactive)
  (let ((word (-region-or-word)))
    (if word
        (popup-tip (-translation
                    (-request word)))
      (message "No word at point"))))

:autoload
(defun search-input ()
  "Search input word and display in echo area."
  (interactive)
  (let ((word (-prompt-input)))
    (if (not (string= word ""))
        (message (-translation
                  (-request word)))
      (message "No word inputted."))))

:autoload
(defun search-input+ ()
  "Search input word and display in popup."
  (interactive)
  (let ((word (-prompt-input)))
    (if (not (string= word ""))
        (popup-tip (-translation
                    (-request word)))
      (message "No word inputted."))))

;; TODO: show result in new buffer


:autoload
(defun search-and-replace ()
  "Search word at point and replace this word with popup menu."
  (interactive)
  (if (region-active-p)
      (let ((region-beginning (region-beginning)) (region-end (region-end))
            (selected (popup-menu* (mapcar '-strip-explain
                                           (append (-explains
                                                    (-request
                                                     (-region-or-word)))
                                                   nil)))))
        (when selected
          (insert selected)
          (kill-region region-beginning region-end)))
    (let* ((bounds (bounds-of-thing-at-point 'chinese-or-other-word))
           (beginning-of-word (car bounds))
           (end-of-word (cdr bounds)))
      (when bounds
        (let ((selected (popup-menu* (mapcar
                                      '-strip-explain
                                      (append (-explains
                                               (-request
                                                (thing-at-point 'chinese-or-other-word)))
                                              nil)))))
          (when selected
            (insert selected)
            (kill-region beginning-of-word end-of-word)))))))

)


(provide 'youdao-dictionary)

;; Local Variables:
;; coding: utf-8
;; End:

;;; youdao-dictionary.el ends here
