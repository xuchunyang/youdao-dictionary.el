;;; youdao-dictionary.el --- Youdao Dictionary interface for Emacs

;; Copyright © 2015-2017 Chunyang Xu

;; Author: Chunyang Xu <xuchunyang56@gmail.com>
;; URL: https://github.com/xuchunyang/youdao-dictionary.el
;; Package-Requires: ((popup "0.5.0") (pos-tip "0.4.6") (chinese-word-at-point "0.2") (names "0.5") (emacs "24"))
;; Version: 0.4
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
;; A simple Youdao Dictionary interface for Emacs
;;
;; Below are commands you can use:
;; `youdao-dictionary-search-at-point'
;; Search word at point and display result with buffer
;; `youdao-dictionary-search-at-point+'
;; Search word at point and display result with popup-tip
;; `youdao-dictionary-search-from-input'
;; Search word from input and display result with buffer
;; `youdao-dictionary-search-and-replace'
;; Search word at point and display result with popup-menu, replace word with
;; selected translation.
;; `youdao-dictionary-play-voice-at-point'
;; Play voice of word at point (by [[https://github.com/snyh][@snyh]])
;; `youdao-dictionary-play-voice-from-input'
;; Play voice of word from input (by [[https://github.com/snyh][@snyh]])
;; `youdao-dictionary-search-at-point-tooltip'
;; Search word at point and display result with pos-tip

;;; Code:
(require 'json)
(require 'url)
(require 'org)
(require 'chinese-word-at-point)
(require 'popup)
(require 'posframe)
(require 'pos-tip)
(eval-when-compile (require 'names))

(defgroup youdao-dictionary nil
  "Youdao dictionary interface for Emacs."
  :prefix "youdao-dictionary-"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/xuchunyang/youdao-dictionary.el"))

;;;###autoload
(define-namespace youdao-dictionary-

(defconst api-url
  "http://fanyi.youdao.com/openapi.do?keyfrom=YouDaoCV&key=659600698&type=data&doctype=json&version=1.1&q=%s"
  "Youdao dictionary API template, URL `http://dict.youdao.com/'.")

(defconst voice-url
  "http://dict.youdao.com/dictvoice?type=2&audio=%s"
  "Youdao dictionary API for query the voice of word.")

(defcustom -tooltip-name "*youdao*"
  "The name of youdao tooltip name."
  :type 'string
  :group 'youdao-dictionary)

(defvar -tooltip-last-point 0
  "Hold last point when show tooltip, use for hide tooltip after move point.")

(defvar -tooltip-last-scroll-offset 0
  "Hold last scroll offset when show tooltip, use for hide tooltip after window scroll.")

(defcustom buffer-name "*Youdao Dictionary*"
  "Result Buffer name."
  :type 'string)

(defcustom search-history-file nil
  "If non-nil, the file be used for saving searching history."
  :type '(choice (const :tag "Don't save history" nil)
                 (string :tag "File path")))

(defcustom use-chinese-word-segmentation nil
  "If Non-nil, support Chinese word segmentation(中文分词).

See URL `https://github.com/xuchunyang/chinese-word-at-point.el' for more info."
  :type 'boolean)

(defun -hide-tooltip-after-move ()
  (ignore-errors
    (when (get-buffer youdao-dictionary--tooltip-name)
      (unless (and
               (equal (point) youdao-dictionary--tooltip-last-point)
               (equal (window-start) youdao-dictionary--tooltip-last-scroll-offset))
        (posframe-delete youdao-dictionary--tooltip-name)
        (kill-buffer youdao-dictionary--tooltip-name)))))

(defun -show-posframe-tooltip (result)
  "Show string on posframe buffer."
  ;; Show tooltip at point if word fetch from user cursor.
  (posframe-show youdao-dictionary--tooltip-name
                 :string result
                 :position (point)
                 :timeout 5
                 :internal-border-width 10)
  (add-hook 'post-command-hook 'youdao-dictionary--hide-tooltip-after-move)
  (setq youdao-dictionary--tooltip-last-point (point))
  (setq youdao-dictionary--tooltip-last-scroll-offset (window-start)))

(defun -format-voice-url (query-word)
  "Format QUERY-WORD as voice url."
  (format voice-url (url-hexify-string query-word)))

(defun -format-request-url (query-word)
  "Format QUERY-WORD as a HTTP request URL."
  (format api-url (url-hexify-string query-word)))

(defun -request (word)
  "Request WORD, return JSON as an alist if successes."
  (when (and search-history-file (file-writable-p search-history-file))
    ;; Save searching history
    (append-to-file (concat word "\n") nil search-history-file))
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

(defun -explains (json)
  "Return explains as a vector extracted from JSON."
  (cdr (assoc 'explains (cdr (assoc 'basic json)))))

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
  "Return word in region or word at point."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning)
                                      (region-end))
    (thing-at-point (if use-chinese-word-segmentation
                        'chinese-or-other-word
                      'word)
                    t)))

(defun -format-result (word)
  "Format request result of WORD."
  (let* ((json (-request word))
         (query        (assoc-default 'query       json)) ; string
         (translation  (assoc-default 'translation json)) ; array
         (errorCode    (assoc-default 'errorCode   json)) ; number
         (web          (assoc-default 'web         json)) ; array
         (basic        (assoc-default 'basic       json)) ; alist
         ;; construct data for display
         (phonetic (assoc-default 'phonetic basic))
         (translation-str (mapconcat
                           (lambda (trans) (concat "- " trans))
                           translation "\n"))
         (basic-explains-str (mapconcat
                              (lambda (explain) (concat "- " explain))
                              (assoc-default 'explains basic) "\n"))
         (web-str (mapconcat
                   (lambda (k-v)
                     (format "- %s :: %s"
                             (assoc-default 'key k-v)
                             (mapconcat 'identity (assoc-default 'value k-v) "; ")))
                   web "\n")))
    (if basic
        (format "%s [%s]\n\n* Basic Explains\n%s\n\n* Web References\n%s\n"
                query phonetic basic-explains-str web-str)
      (format "%s\n\n* Translation\n%s\n"
              query translation-str))))

(defun -pos-tip (string)
  "Show STRING using pos-tip-show."
  (pos-tip-show string nil nil nil 0)
  (unwind-protect
      (push (read-event) unread-command-events)
    (pos-tip-hide)))

(defun play-voice-of-current-word ()
  "Play voice of current word shown in *Youdao Dictionary*."
  (interactive)
  (if (local-variable-if-set-p 'youdao-dictionary-current-buffer-word)
      (-play-voice current-buffer-word)))

(defvar current-buffer-word nil)

(define-derived-mode mode org-mode "Youdao-dictionary"
  "Major mode for viewing Youdao dictionary result.
\\{youdao-dictionary-mode-map}"
  (read-only-mode 1)
  (define-key mode-map "q" 'quit-window)
  (define-key mode-map "p" 'youdao-dictionary-play-voice-of-current-word)
  (define-key mode-map "y" 'youdao-dictionary-play-voice-at-point))

(defun -search-and-show-in-buffer (word)
  "Search WORD and show result in `youdao-dictionary-buffer-name' buffer."
  (if word
      (with-current-buffer (get-buffer-create buffer-name)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (mode)
          (insert (-format-result word))
          (goto-char (point-min))
          (set (make-local-variable 'youdao-dictionary-current-buffer-word) word))
        (unless (get-buffer-window (current-buffer))
          (switch-to-buffer-other-window buffer-name)))
    (message "Nothing to look up")))

:autoload
(defun search-at-point ()
  "Search word at point and display result with buffer."
  (interactive)
  (let ((word (-region-or-word)))
    (-search-and-show-in-buffer word)))

:autoload
(defun search-at-point+ ()
  "Search word at point and display result with popup-tip."
  (interactive)
  (let ((word (-region-or-word)))
    (if word
        (popup-tip (-format-result word))
      (message "Nothing to look up"))))

:autoload
(defun search-at-point++ ()
  "Search word at point and display result with posframe."
  (interactive)
  (let ((word (-region-or-word)))
    (if word
        (youdao-dictionary--show-posframe-tooltip (-format-result word))
      (message "Nothing to look up"))))

:autoload
(defun search-at-point-tooltip ()
  "Search word at point and display result with pos-tip."
  (interactive)
  (let ((word (-region-or-word)))
    (if word
        (-pos-tip (-format-result word))
      (message "Nothing to look up"))))

:autoload
(defun search-from-input ()
  "Search word from input and display result with buffer."
  (interactive)
  (let ((word (-prompt-input)))
    (-search-and-show-in-buffer word)))

:autoload
(defun search-and-replace ()
  "Search word at point and replace this word with popup menu."
  (interactive)
  (if (use-region-p)
      (let ((region-beginning (region-beginning)) (region-end (region-end))
            (selected (popup-menu* (mapcar #'-strip-explain
                                           (append (-explains
                                                    (-request
                                                     (-region-or-word)))
                                                   nil)))))
        (when selected
          (insert selected)
          (kill-region region-beginning region-end)))
    ;; No active region
    (let* ((bounds (bounds-of-thing-at-point (if use-chinese-word-segmentation
                                                 'chinese-or-other-word
                                               'word)))
           (beginning-of-word (car bounds))
           (end-of-word (cdr bounds)))
      (when bounds
        (let ((selected
               (popup-menu* (mapcar
                             #'-strip-explain
                             (append (-explains
                                      (-request
                                       (thing-at-point
                                        (if use-chinese-word-segmentation
                                            'chinese-or-other-word
                                          'word))))
                                     nil)))))
          (when selected
            (insert selected)
            (kill-region beginning-of-word end-of-word)))))))

(defvar history nil)

:autoload
(defun search (query)
  "Show the explanation of QUERY from Youdao dictionary."
  (interactive
   (let* ((string (or (if (use-region-p)
                          (buffer-substring
                           (region-beginning) (region-end))
                        (thing-at-point 'word))
                      (read-string "Search Youdao Dictionary: " nil 'youdao-dictionary-history))))
     (list string)))
  (-search-and-show-in-buffer query))


(defun -play-voice (word)
  "Play voice of the WORD if there has mplayer or mpg123 program."
  (let ((player (or (executable-find "mpv")
                    (executable-find "mplayer")
                    (executable-find "mpg123"))))
    (if player
        (start-process player nil player (-format-voice-url word))
      (user-error "mplayer or mpg123 is needed to play word voice"))))

:autoload
(defun play-voice-at-point ()
  "Play voice of the word at point."
  (interactive)
  (let ((word (-region-or-word)))
    (-play-voice word)))

:autoload
(defun play-voice-from-input ()
  "Play voice of user input word."
  (interactive)
  (let ((word (-prompt-input)))
    (-play-voice word)))


)


(provide 'youdao-dictionary)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; youdao-dictionary.el ends here
