;;; w3-print.el,v --- Printing support for emacs-w3
;; Author: wmperry
;; Created: 1995/06/14 23:09:18
;; Version: 1.12
;; Keywords: faces, help, printing, hypermedia

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993, 1994 by William M. Perry (wmperry@spry.com)
;;;
;;; This file is part of GNU Emacs.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar w3-use-ps-print nil
  "*If non-nil, then printing will be done via the ps-print package by
James C. Thompson <thompson@wg2.waii.com>.")

(defun w3-face-type (face)
  "Return a list specifying what a face looks like.  ie: '(bold italic)"
  (let ((font (or (face-font face) (face-font 'default)))
	(retval nil))
    (if (not (stringp font))
	(setq font
	      (cond
	       ((and (fboundp 'fontp) (not (fontp font))) nil)
	       ((fboundp 'font-truename) (font-truename font))
	       ((fboundp 'font-name) (font-name font))
	       (t nil))))
    (cond
     ((not font) nil)
     ((string-match "^-\\([^-]+\\)-\\([^-]+\\)-\\([^-]+\\)-\\([^-]+\\)-" font)
      (let ((wght (substring font (match-beginning 3) (match-end 3)))
	    (slnt (substring font (match-beginning 4) (match-end 4))))
	(if (string-match "bold" wght)
	    (setq retval (cons 'bold retval)))
	(if (or (string-match "i" slnt) (string-match "o" slnt))
	    (setq retval (cons 'italic retval)))
	(if (and (fboundp 'face-underline-p)
		 (face-underline-p face))
	    (setq retval (cons 'underline retval)))))
     ((and (symbolp face) (string-match "bold" (symbol-name face)))
      (setq retval '(bold)))
     ((and (symbolp face) (string-match "italic" (symbol-name face)))
      (setq retval '(italic)))
     (t nil))
    retval))

(defun w3-print-with-ps-print (&optional buffer function)
  "Print a buffer using `ps-print-buffer-with-faces'.
This function wraps `ps-print-buffer-with-faces' so that the w3 faces
will be correctly listed in ps-bold-faces and ps-italic-faces"
  (interactive)
  (require 'ps-print)
  (setq buffer (or buffer (current-buffer))
	function (or function 'ps-print-buffer-with-faces))
  (let ((ps-bold-faces ps-bold-faces)
	(ps-italic-faces ps-italic-faces)
	(ps-underline-faces (cond
			     ((boundp 'ps-underline-faces)
			      (symbol-value 'ps-underline-faces))
			     ((boundp 'ps-underlined-faces)
			      (symbol-value 'ps-underlined-faces))
			     (t nil)))
	(ps-underlined-faces nil)
	(ps-left-header '(ps-get-buffer-name url-view-url))
	(faces (face-list))
	(data nil)
	(face nil))
    (if (string< ps-print-version "1.6")
	(while faces
	  (setq face (car faces)
		data (w3-face-type face)
		faces (cdr faces))
	  (if (and (memq 'bold data) (not (memq face ps-bold-faces)))
	      (setq ps-bold-faces (cons face ps-bold-faces)))
	  (if (and (memq 'italic data) (not (memq face ps-italic-faces)))
	      (setq ps-italic-faces (cons face ps-italic-faces)))
	  (if (and (memq 'underline data) (not (memq face ps-underline-faces)))
	      (setq ps-underline-faces (cons face ps-underline-faces))))
      (setq ps-underlined-faces ps-underline-faces))
    (save-excursion
      (set-buffer buffer)
      (funcall function))))

(defun w3-print-this-url (&optional url format)
  "Print out the current document (in LaTeX format)"
  (interactive)
  (if (not url) (setq url (url-view-url t)))
  (let* ((completion-ignore-case t)
	 (format (or format
		     (completing-read
		      "Format: "
		      '(("HTML Source")		; The raw HTML code
			("Formatted Text") 	; Plain ASCII rendition
			("PostScript")		; Pretty PostScript
			("LaTeX'd")		; LaTeX it, then print
			)
		      nil t))))
    (save-excursion
      (cond
       ((equal "HTML Source" format)
	(if w3-current-source
	    (let ((x w3-current-source))
	      (set-buffer (get-buffer-create url-working-buffer))
	      (erase-buffer)
	      (insert x))
	  (url-retrieve url))
	(lpr-buffer))
       ((or (equal "Formatted Text" format)
	    (equal "" format))
	(lpr-buffer))
       ((equal "PostScript" format)
	(w3-print-with-ps-print (current-buffer)))
       ((equal "LaTeX'd" format)
	(if w3-current-source
	    (let ((x w3-current-source))
	      (set-buffer (get-buffer-create url-working-buffer))
	      (erase-buffer)
	      (insert x))
	  (url-retrieve url))
	(w3-convert-html-to-latex)
	(save-window-excursion
	  (write-region (point-min) (point-max)
			(expand-file-name "w3-tmp.latex"
					  w3-temporary-directory) nil 5)
	  (shell-command
	   (format
	    "cd %s ; latex w3-tmp.latex ; %s w3-tmp.dvi ; rm -f w3-tmp*"
	    w3-temporary-directory
	    w3-print-command))
	  (kill-buffer "*Shell Command Output*")))))))

(defun w3-print-url-under-point ()
  "Print out the url under point (in LaTeX format)"
  (interactive)
  (w3-print-this-url (w3-view-this-url t)))

(defun w3-convert-html-to-latex ()
  ;; Convert an html document into LaTeX - this is pretty much the same as the
  ;; sed scripts from ftp.w3.org"
  (set-buffer url-working-buffer)
  (if w3-use-html2latex
      (shell-command-on-region (point-min) (point-max)
			       (format "%s %s" w3-html2latex-prog
				       w3-html2latex-args) t)
    (let ((case-fold-search t))
      (goto-char (point-min))
      (w3-replace-regexp "\\\\" "\\\\backslash ")
      (w3-replace-regexp "{" "\\\\{")
      (w3-replace-regexp "}" "\\\\}")
      (goto-char (point-min))
      (w3-insert (concat "\\documentstyle" w3-latex-docstyle "\n"))
      (w3-insert "\\begin{document}\n")
      (goto-char (point-max))
      (w3-insert "\\end{document}")
      (w3-replace-regexp "<\\(XMP\\|LISTING\\)>" "\\\\begin{verbatim}")
      (w3-replace-regexp "</\\(XMP\\|LISTING\\)>" "\\\\end{verbatim}")
      (w3-replace-regexp "<\\(ISINDEX\\|NEXTID\\)[^>]*>" "")
      (w3-replace-regexp (regexp-quote "$") "\\\\$")
      (w3-replace-regexp (regexp-quote "&gt;") "$>$")
      (w3-replace-regexp "%" "\\\\%")
      (w3-replace-regexp "#" "\\\\#")
      (w3-replace-regexp "_" "\\\\_")
      (w3-replace-regexp "~" "\\\\~")
      (w3-replace-regexp "<LI> *" "\\\\item ")
      (w3-replace-regexp (regexp-quote "^") "\\\\^")
      (w3-replace-regexp "<P>" "\\\\par")
      (w3-replace-regexp "<TITLE>\\([^<]*\\)</TITLE>" "\\\\section{\\1}")
      (w3-replace-regexp "<IMG *SRC=\"\\([^\"]*.ps\\)\">"
			 "\\\\psfig{figure=\\1,width=\\\\columnwidth}")
      (w3-replace-regexp "<H1>" "\\\\section{")
      (w3-replace-regexp "<H2>" "\\\\subsection{")
      (w3-replace-regexp "<H3>" "\\\\subsubsection{")
      (w3-replace-regexp "<H4>" "\\\\subsubsection{")
      (w3-replace-regexp "<H5>" "\\\\paragraph{")
      (w3-replace-regexp "<H6>" "\\\\subparagraph{")
      (w3-replace-regexp "</H[0-9]*>" "}")
      (w3-replace-regexp "<\\(UL\\|DIR\\|MENU\\)>" "\\\\begin{itemize}")
      (w3-replace-regexp "</\\(UL\\|DIR\\|MENU\\)>" "\\\\end{itemize}")
      (w3-replace-regexp "<OL>" "\\\\begin{enumerate}")
      (w3-replace-regexp "</OL>" "\\\\end{enumerate}")
      (w3-replace-regexp "<DL>" "\\\\begin{description}")
      (w3-replace-regexp "</DL>" "\\\\end{description}")
      (w3-replace-regexp "<DT>\\([^<]*$\\)" "\\\\item[\\1]")
      (w3-replace-regexp "<DD>" "")
      (w3-replace-regexp "<A[ \t\n]+[^>]*>" "")   ;; get rid of anchors
      (w3-replace-regexp "</A>" "")
      (w3-replace-regexp
       "<\\(EM\\|B\\|STRONG\\|DFN\\)>\\([^<]*\\)</\\(EM\\|B\\|STRONG\\|DFN\\)>"
       "{\\\\bf \\2}")
      (w3-replace-regexp
       "<\\(CODE\\|SAMP\\|TT\\|KBD\\|VAR\\)>\\([^<]*\\)</\\(CODE\\|SAMP\\|TT\\|KBD\\|VAR\\)>"
       "{\\\\tt \\2}")
      (w3-replace-regexp
       "<\\(CITE\\|U\\)>\\([^<]*\\)</\\(CITE\\|U\\)>" "{\\\\underline \\2}")
      (w3-replace-regexp
       "<\\(I\\|ADDRESS\\)>\\([^<]*\\)</\\(I\\|ADDRESS\\)>" "{\\\\it \\2}")
      (w3-replace-regexp "<IMG[^>]*>" "")
      (w3-replace-regexp (regexp-quote "&lt;") "$<$")
      (w3-replace-regexp (regexp-quote "&amp;") " and ")
      (w3-replace-regexp "<[^>]*>" ""))))

(provide 'w3-print)
