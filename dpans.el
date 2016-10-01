;;; dpans.el --- Browse description for Forth Standard words
;;
;; Written by Helmut Eller in 2007
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;; This package makes it convenient to browse documentation for
;; standard Forth words from within Emacs.  The command `dpans-lookup'
;; asks for the word name and invokes the HTML browser with the right
;; URL.  You may want to add a snipped like this to your .emacs:
;;
;; (defun dpans-bind-key () (local-set-key (kbd "C-c C-d") 'dpans-lookup))
;; (add-hook 'forth-mode-hook 'dpans-bind-key)
;;
;;; Code:

(defcustom dpans-root "http://www.forth200x.org/documents/html/"
  "The URL which contains the HTML version of the standard.
If you have a local copy set this variable to
something like \"file://home/joe/docs/ANS-Forth/\".

Note: the string should have a trailing backslash."
  :type 'file
  :group 'dpans)

(defcustom dpans-browse-url #'browse-url
  "Just in case you want to use a special browser."
  :type 'function
  :group 'dpans)

(defun dpans-lookup (name)
  "View the documentation on NAME from the ANS Forth Standard."
  (interactive (list (dpans--read-name)))
  (funcall dpans-browse-url (dpans--build-url name)))

(defvar dpans--lookup-history '())

(defun dpans--read-name ()
  "Read a word-name in the minibuffer, with completion."
  (let ((completion-ignore-case t))
    (completing-read "Word: " (dpans--index)
		     nil t (thing-at-point 'symbol)
		     'dpans--lookup-history)))

(defun dpans--build-url (name)
  "Return the URL for the word NAME."
  (concat dpans-root (elt (assoc name (dpans--index)) 1)))

(defvar dpans--index-cache nil)

(defun dpans--index ()
  "Return a list ((NAME HREF PRONUNCIATION) ...)."
  (or dpans--index-cache
      (setq dpans--index-cache (dpans--parse-index))))

(defun dpans--index-url () (concat dpans-root "alpha.html"))

(defun dpans--parse-index (&optional url)
  (let ((buffer (url-retrieve-synchronously (or url (dpans--index-url))
					    nil t 4)))
    (unwind-protect
	(with-current-buffer buffer
	  (dpans--parse-index%))
      (kill-buffer buffer))))

(defun dpans--parse-index% ()
  (let ((index '()) (case-fold-search t)
	(rx "<td>\
<a href=\"\\([^\"]+\\)\">\\([^<]+\\)</a>\
</td><td>\"\\([^\"]+\\)\""))
    (search-forward "<table")
    (while (re-search-forward rx nil t)
      (push (list (dpans--decode-entities (match-string 2))
		  (match-string 1)
		  (match-string 3))
	    index))
    (reverse index)))

(declare-function mm-url-decode-entities "gnus/mm-url")

(defun dpans--decode-entities (string)
  (autoload 'mm-url-decode-entities "gnus/mm-url")
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (save-match-data
      (mm-url-decode-entities))
    (buffer-string)))

(provide 'dpans)

;;; dpans.el ends here
