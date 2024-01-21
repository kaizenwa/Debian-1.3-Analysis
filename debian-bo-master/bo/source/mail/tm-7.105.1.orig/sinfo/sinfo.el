;;; sinfo.el --- sinfo to Texinfo converter

;; Copyright (C) 1996,1997 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Version: $Id: sinfo.el,v 3.7 1997/01/31 13:46:10 morioka Exp $
;; Keywords: SGML, Texinfo

;; This file is part of sinfo (SGML based info system).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Comment:

;; This program is for Emacs/mule (mule-19.33-delta) and requires
;; sgmls.

;;; Code:

(require 'tl-list)
(require 'tl-str)
(require 'texinfmt)
(require 'texi-util)

(defvar sinfo-texi-mapping-file
  "/usr/local/share/sgml/rep/sinfo/texi-mapping"
  "*SGML mapping file to convert into Texinfo.")

(defun sinfo-texi-swap-node ()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward
	  "^\\(@\\(chapter\\|\\(sub\\)*section\\) .*\\)
\\(@node .*\\)\n" nil t)
    (let* ((md (match-data))
	   (nd (last md 2))
	   (nb (car nd))
	   (ne (second nd))
	   )
      (replace-match (format "%s\n%s"
			     (buffer-substring nb ne)
			     (buffer-substring (match-beginning 0) nb)
			     ))
      )))

(defun sinfo-filter-for-standard ()
  (goto-char (point-min))
  (while (re-search-forward
	  "@noindent\n\\[\\([^][]*\\)\\]\n@quotation\n\\([^,]*\\)," nil t)
    (let ((name (buffer-substring (match-beginning 1)(match-end 1)))
	  (org (buffer-substring (match-beginning 2)(match-end 2)))
	  (dest "@quotation\n")
	  (b (match-beginning 0))
	  )
      (save-restriction
	(and (eq (aref name 0) ?:)
	     (eq (aref name 1) ?\ )
	     (setq name (substring name 2))
	     )
	(or (string= name "")
	    (setq dest (concat "@noindent\n[" name "]\n" dest))
	    )
	(or (string= org "")
	    (setq dest (concat dest org ","))
	    )
	(replace-match dest)
	(search-forward "@end quotation")
	(narrow-to-region b (match-beginning 0))
	(goto-char b)
	(if (search-forward " (obsolete RFC )" nil t)
	    (replace-match "")
	  )
	(goto-char b)
	(while (search-forward "『』" nil t)
	  (replace-match "")
	  )
	(goto-char b)
	(while (search-forward "``''" nil t)
	  (replace-match "")
	  )
	(goto-char b)
	(while (re-search-forward ",[ \t\n]*," nil t)
	  (replace-match ",")
	  (goto-char b)
	  )
	(and (re-search-forward "^,[ \t\n]*" nil t)
	     (replace-match "")
	     )
	(if (search-forward ", ." nil t)
	    (replace-match ".")
	  )
	(goto-char (point-max))
	(fill-paragraph nil)
	))))

(defun sinfo-to-texi ()
  (interactive)
  (let* ((the-buf (current-buffer))
	 (src-name (buffer-file-name))
	 (name (file-name-non-extension src-name))
	 (dst-name (concat name ".texi"))
	 (fname (concat (file-name-nondirectory name) ".info"))
	 (cs buffer-file-coding-system)
	 status)
    (find-file dst-name)
    (erase-buffer)
    (insert-buffer the-buf)
    (goto-char (point-min))
    (while (re-search-forward "[@{}]" nil t)
      (replace-match (concat "@" (buffer-substring (match-beginning 0)
						   (match-end 0))))
      )
    (let ((coding-system-for-read 'internal)
	  (coding-system-for-write 'internal)
	  )
      (setq status
	    (call-process-region (point-min)(point-max)
				 "sh" t t t
				 "-c"
				 (format "sgmls|sgmlsasp %s"
					 (expand-file-name
					  sinfo-texi-mapping-file)
					 )
				 )
	    )
      )
    (goto-char (point-min))
    (if (and (search-forward "sgmls:" nil t)
	     (re-search-forward "line \\([0-9]+\\)" nil t)
	     )
	(let ((line (string-to-number
		     (buffer-substring (match-beginning 1)
				       (match-end 1)))
		    ))
	  (progn
	    (pop-to-buffer the-buf)
	    (goto-line line)
	    ))
      (set-buffer-file-coding-system cs)
      (sinfo-texi-swap-node)
      (let ((title
	     (progn
	       (goto-char (point-min))
	       (and (re-search-forward "@title \\(.*\\)\n" nil t)
		    (buffer-substring (match-beginning 1)(match-end 1))
		    )))
	    )
	(goto-char (point-min))
	(and (re-search-forward "@setfilename$" nil t)
	     (replace-match
	      (format "@setfilename %s" fname)
	      ))
	(and (re-search-forward "@settitle{}" nil t)
	     (replace-match
	      (format "@settitle{%s}" title)
	      ))
	(and (re-search-forward "@top$" nil t)
	     (replace-match
	      (format "@top %s" title)
	      ))
	)
      (goto-char (point-min))
      (while (re-search-forward
	      "@DREF{\\(([^{}()]*)\\)\\([^{}]+\\)}"
	      nil t)
	(let ((file (buffer-substring (match-beginning 1)(match-end 1)))
	      (word (buffer-substring (match-beginning 2)(match-end 2)))
	      )
	  (replace-match "")
	  (re-search-forward "@end DREF")
	  (replace-match
	   (concat word " (@ref{"
		   (if (string= file "()")
		       word
		     (concat file word)
		     ) "})") 'fixed-case)
	  ))
      (goto-char (point-min))
      (while (re-search-forward
	      "@AREF{\\([^{}]*\\)}{<URL:\\([^<>()]*\\)>}\n*"
	      nil t)
	(let ((node (buffer-substring (match-beginning 1)(match-end 1)))
	      (url  (buffer-substring (match-beginning 2)(match-end 2)))
	      )
	  (replace-match "")
	  (re-search-forward "@end AREF")
	  (replace-match
	   (if (string= node "()")
	       (concat "(" url ")")
	     (concat "(@ref{" node "})")
	     ) 'fixed-case)
	  ))
      (sinfo-filter-for-standard)
      (goto-char (point-min))
      (while (re-search-forward "@CONCEPT{\\([^{}]+\\)}" nil t)
	(let ((name (buffer-substring (match-beginning 1) (match-end 1))))
	  (replace-match (format "@cindex{%s}@strong{%s}" name name)
			 'fixed-case)
	  ))
      (goto-char (point-min))
      (while (search-forward "{<URL:>}" nil t)
	(replace-match "")
	)
      (goto-char (point-min))
      (while (search-forward "@ref{()" nil t)
	(replace-match "@ref{")
	)
      (goto-char (point-min))
      (while (re-search-forward "@cindex{\\([^}]*\\)}" nil t)
	(let ((str (buffer-substring (match-beginning 1)(match-end 1))))
	  (replace-match "")
	  (if (string= str "")
	      ""
	    (save-excursion
	      (if (re-search-backward
		   "@\\(chapter\\|\\(sub\\)*section\\)[ \t][^\n]*\n" nil t)
		  (progn
		    (goto-char (match-end 0))
		    (insert
		     (concat "@cindex "
			     (mapconcat (function (lambda (chr)
						    (if (eq chr ?\n)
							" "
						      (char-to-string chr)
						      )))
					str "")
			     "\n"))
		    ))))))
      (goto-char (point-min))
      (while (re-search-forward "{@refill}" nil t)
	(replace-match "")
	(if (= (current-column) 0)
	    (delete-char 1)
	  ;;(fill-paragraph nil)
	  (if (looking-at "\n\n")
	      (insert "@refill")
	    )
	  (fill-paragraph nil)
	  ))
      (goto-char (point-min))
      (while (re-search-forward "@ref{\\([^}]*\\)}" nil t)
	(let ((str (buffer-substring (match-beginning 1)(match-end 1))))
	  (replace-match
	   (if (string= str "")
	       ""
	     (concat "@ref{"
		     (mapconcat (function (lambda (chr)
					    (if (eq chr ?\n)
						" "
					      (char-to-string chr)
					      )))
				str "")
		     "}")) 'fixed-case)
	  ))
      (goto-char (point-min))
      (while (re-search-forward "@strong{\\([^}]*\\)}" nil t)
	(let ((str (buffer-substring (match-beginning 1)(match-end 1))))
	  (replace-match
	   (if (string= str "")
	       ""
	     (concat "@strong{"
		     (mapconcat (function (lambda (chr)
					    (if (eq chr ?\n)
						" "
					      (char-to-string chr)
					      )))
				str "")
		     "}")
	     ))))
      (texinfo-every-node-update)
      (texinfo-all-menus-update)
      (texinfo-all-menu-titles-update)
      (goto-char (point-min))
      )))


;;; @ end
;;;

(provide 'sinfo)

;;; sinfo.el ends here
