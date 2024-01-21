;;; ol-to-texi.el --- Emacs outline-mode format to Texinfo converter

;; Copyright (C) 1996 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Version: $Id: ol-to-texi.el,v 2.4 1996/10/10 14:21:35 morioka Exp $
;; Keywords: outline-mode, Texinfo, plain2

;; This file is part of tl (Tiny Library).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'tl-list)
(require 'texi-util)

(defvar ol-to-texi-section-commands
  ["chapter" "section" "subsection" "subsubsection"])

(defun ol-to-texi-convert-a-function ()
  (let (args beg end)
    (save-restriction
      (narrow-to-region (point)(progn (end-of-line)(point)))
      (goto-char (point-min))
      (re-search-forward "[^ \t]+" nil t)
      (while (re-search-forward "[^ \t]+" nil t)
	(setq args (cons
		    (buffer-substring (match-beginning 0)(match-end 0))
		    args))
	)
      (untabify (point-min)(point-max))
      )
    (search-forward "\n")
    (setq beg (point))
    (while (or
	    (and (< (point)(point-max))(looking-at "^[ \t]*$"))
	    (and (looking-at "^\t")
		 (progn (replace-match "") t)
		 ))
      (forward-line)
      (beginning-of-line)
      )
    (setq end (point))
    (insert "@end deffn\n\n")
    (save-restriction
      (narrow-to-region beg end)
      (mapcar (function
	       (lambda (arg)
		 (goto-char (point-min))
		 (while (search-forward (concat "`" arg "'") nil t)
		   (replace-match (concat "@var{" arg "}") t)
		   )))
	      args)
      )))

(defun ol-to-texi-convert-function ()
  (goto-char (point-min))
  (while (re-search-forward "^\\[\\(関数\\|Function\\)\\][ \t]+" nil t)
    (replace-match "@deffn{Function} ")
    (ol-to-texi-convert-a-function)
    )
  (goto-char (point-min))
  (while (re-search-forward "^\\[\\(Macro\\)\\][ \t]+" nil t)
    (replace-match "@deffn{Macro} ")
    (ol-to-texi-convert-a-function)
    )
  )

(defun ol-to-texi-convert-variable ()
  (goto-char (point-min))
  (while (re-search-forward "^\\[\\(変数\\|Variable\\)\\][ \t]+" nil t)
    (replace-match "@defvar ")
    (search-forward "\n")
    (while (or
	    (looking-at "^[ \t]*$")
	    (and (looking-at "^\t")
		 (progn (replace-match "") t)
		 ))
      (forward-line)
      (beginning-of-line)
      )
    (insert "@end defvar\n\n")
    ))

(defun ol-to-texi ()
  (interactive)
  (let* ((the-buf (current-buffer))
	 (src-name (buffer-file-name))
	 (name (file-name-non-extension src-name))
	 (dst-name (concat name ".texi"))
	 (fname (concat (file-name-nondirectory name) ".info"))
	 )
    (find-file dst-name)
    (erase-buffer)
    (insert-buffer the-buf)
    (let (title author)
      (goto-char (point-min))
      (setq title
	    (cond ((looking-at "^『\\(.+\\)』\n")
		   (prog1
		       (buffer-substring (match-beginning 1)(match-end 1))
		     (delete-region (match-beginning 0)(match-end 0))
		     ))))
      (goto-char (point-min))
      (setq author
	    (if (looking-at "^by \\(.+\\)\n")
		(prog1
		    (buffer-substring (match-beginning 1)(match-end 1))
		  (delete-region (match-beginning 0)(match-end 0))
		  )))
      (goto-char (point-min))
      (if (looking-at "^comment .*\n")
	  (delete-region (match-beginning 0)(match-end 0))
	)
      (if (looking-at "^$Id:.*\n")
	  (delete-region (match-beginning 0)(match-end 0))
	)
      (insert (format "\\input texinfo.tex
@setfilename %s

@titlepage
@title{%s}
@author{%s}
@end titlepage

@node Top, , (dir), (dir)\n" fname (or title "")(or author "")))
      ))
  (goto-char (point-min))
  (let (beg end)
    (while (and (re-search-forward "^\\[\\[E$" nil t)
		(setq beg (match-beginning 0))
		(setq end (match-end 0))
		(re-search-forward "^\\]\\]E$" nil t)
		)
      (replace-match "@end example")
      (delete-region beg end)
      (goto-char beg)
      (insert "@example")
      ))
  (ol-to-texi-convert-variable)
  (ol-to-texi-convert-function)
  (goto-char (point-min))
  (let ((the-level 0))
    (while (re-search-forward
	    "^\\(\\*+\\) \\[\\(.+\\)\\][ \t]*\\(.*\\)$" nil t)
      (let* ((ast (buffer-substring (match-beginning 1)(match-end 1)))
	     (title (buffer-substring (match-beginning 2)(match-end 2)))
	     (subj (buffer-substring (match-beginning 3)(match-end 3)))
	     (level (length ast))
	     (cmd (aref ol-to-texi-section-commands (1- level)))
	     (p (match-beginning 0))
	     )
	(if (string= subj "")
	    (setq subj title)
	  )
	(replace-match (format "@node %s\n@%s %s" title cmd subj) 'fixed)
	(if (> level the-level)
	    (progn
	      (goto-char p)
	      (insert "@menu\n@end menu\n\n")
	      )
	  )
	(setq the-level level)
	)))
  (goto-char (point-min))
  (while (re-search-forward "^\\(\\*+\\) \\(.+\\)$" nil t)
    (let* ((ast (buffer-substring (match-beginning 1)(match-end 1)))
	   (subj (buffer-substring (match-beginning 2)(match-end 2)))
	   (level (length ast))
	   (cmd (aref ol-to-texi-section-commands (1- level)))
	   )
      (replace-match (format "@%s %s" cmd subj) 'fixed)
      ))
  (goto-char (point-max))
  (insert "
@node Concept Index
@chapter Concept Index

@printindex cp


@node Command Index
@chapter Command Index

@printindex fn


@node Variable Index
@chapter Variable Index

@printindex vr

@summarycontents
@contents

@bye\n")
  
  (texinfo-all-menus-update)
  (texinfo-all-menu-titles-update)
  (texinfo-every-node-update)
  )

(defun ol-to-texi-add-menu-title ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n@menu\n" nil t)
    (goto-char (match-end 0))
    (while (looking-at "* \\([^:]+\\)::")
      (let ((title (buffer-substring (match-beginning 1)(match-end 1)))
	    subj)
	(save-excursion
	  (let ((ret
		 (re-search-forward
		  (format
		   "@node %s.*\n@\\(chapter\\|\\(sub\\)*section\\) \\(.+\\)"
		   (regexp-quote title)))))
	    (if ret
		(let ((data (last (match-data) 2)))
		  (setq subj (buffer-substring (car data)
					       (car (cdr data))))
		  ))
	    ))
	(if subj
	    (or (string= subj title)
		(progn
		  (end-of-line)
		  (insert subj)
		  )))
	(end-of-line)
	(forward-char)
	))))

	       
;;; @ end
;;;

(provide 'ol-to-texi)

;;; ol-to-texi.el ends here
