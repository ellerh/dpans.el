;;; dpans.el --- Browse descriptions in Forth Standard -*-lexical-binding:t-*-
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
;; (defun dpans-bind-key () (local-set-key (kbd "C-c C-d") 'dpans-lookup-2012))
;; (add-hook 'forth-mode-hook 'dpans-bind-key)
;;
;;; Code:

(require 'cl-lib)

(defcustom dpans-url-2012 "http://www.forth200x.org/documents/html/"
  "The URL which contains the HTML version of the standard.
If you have a local copy set this variable to
something like \"file://home/joe/docs/ANS-Forth/\".

Note: the string should have a trailing backslash."
  :type 'file
  :group 'dpans)

(defcustom dpans-url-1994 "http://lars.nocrew.org/dpans/"
  "URL for 1994 version of standard."
  :type 'file
  :group 'dpans)

(defcustom dpans-browse-url #'browse-url
  "Just in case you want to use a special browser."
  :type 'function
  :group 'dpans)

(defun dpans-lookup-2012 (name)
  "View the documentation on NAME from the ANS Forth Standard."
  (interactive (list (dpans--read-name 2012)))
  (dpans--lookup name 2012))

(defun dpans-lookup-1994 (name)
  "View the documentation on NAME from the ANS Forth Standard."
  (interactive (list (dpans--read-name 1994)))
  (dpans--lookup name 1994))

(defun dpans--lookup (name version)
  (funcall dpans-browse-url (dpans--build-url name version)))

(defvar dpans--lookup-history '())

(defun dpans--read-name (version)
  "Read a word-name in the minibuffer, with completion."
  (let ((completion-ignore-case t))
    (completing-read "Word: " (dpans--index version)
		     nil t (thing-at-point 'symbol)
		     'dpans--lookup-history)))
(eval-and-compile
  (defvar dpans--versioned-info
    '((2012 dpans-url-2012 "alpha.html" #'dpans--parse-index-2012)
      (1994 dpans-url-1994 "dpansf.htm" #'dpans--parse-index-1994))))

(defmacro dpans--versioned (name version)
  (let ((index (cl-ecase name
		 (url 1)
		 (index 2)
		 (parse-index 3))))
    `(cl-ecase ,version
       (2012 ,(elt (assoc 2012 dpans--versioned-info) index))
       (1994 ,(elt (assoc 1994 dpans--versioned-info) index)))))

(defun dpans--root (version)
  (dpans--versioned url version))

(defun dpans--build-url (name version)
  "Return the URL for the word NAME."
  (concat (dpans--root version)
	  (elt (assoc name (dpans--index version)) 1)))

(defvar dpans--index-cache nil)

(defun dpans--index (version)
  "Return a list ((NAME HREF PRONUNCIATION) ...)."
  (let ((entry (assoc version dpans--index-cache)))
    (cond (entry (cdr entry))
	  (t
	   (let ((index (dpans--parse-index version)))
	     (push (cons version index) dpans--index-cache)
	     index)))))

(defun dpans--index-url (version)
  (concat (dpans--root version) (dpans--versioned index version)))

(defun dpans--parse-index (version)
  (dpans--call/url-buffer (dpans--index-url version)
			  (dpans--versioned parse-index version)))

(defun dpans--call/url-buffer (url fun)
  (let ((buffer (url-retrieve-synchronously url nil t 4)))
    (unwind-protect
	(with-current-buffer buffer
	  (funcall fun))
      (kill-buffer buffer))))

(defun dpans--parse-index-2012 ()
  (let ((index '())
	(case-fold-search nil)
	(rx "<td>\
<a href=\"\\([^\"]+\\)\">\\([^<]+\\)</a>\
</td><td>\\(?:\"\\([^\"]+\\)\"\\)?</td>"))
    (search-forward "<table")
    (while (re-search-forward rx nil t)
      (push (list (dpans--decode-entities (match-string 2))
		  (match-string 1)
		  (match-string 3))
	    index))
    (reverse index)))

(defun dpans--parse-index-1994 ()
  (let ((index '())
	(case-fold-search nil)
	(rx "<A href=\\(dpans[^>]+\\)>[^<]+</A>[ ]*\\([^ ]+\\)[ ]*\
\\(?:<B>\\([^\<]+\\)</B>\\)?"))
    (search-forward "<PRE>")
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
