;;
;; hyperlatex-1.4.el PL 1
;;
;; A common input format for LaTeX and Html documents
;; This file realizes the translation to Html format.
;;
;;     $Modified: Tue Dec 19 03:39:25 1995 by otfried $
;;
;; This file is part of Hyperlatex
;; Copyright (C) 1994, 1995 Otfried Schwarzkopf	
;;  
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or (at
;; your option) any later version.
;;      
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;     
;; A copy of the GNU General Public License is available on the World
;; Wide web at "http://graphics.postech.ac.kr/otfried/txt/copying.txt".
;; You can also obtain it by writing to the Free Software Foundation,
;; Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; -------------------------------------------------------------------
;;
;; To run conversion from within Emacs, put the following lines in your
;; `.emacs' file:
;;
;;   (setq hyperlatex-html-icons "HYPERLATEX_ICON_URL")
;;   (setq hyperlatex-elisp-directory  "HYPERLATEX_DIR"
;;   (autoload 'hyperlatex-format-buffer "HYPERLATEX_DIR/hyperlatex1")
;;
;; where you replace HYPERLATEX_ICON_URL and HYPERLATEX_DIR by the
;; right values (see the `hyperlatex' script or the README file).
;;
;; Then you can call `hyperlatex-format-buffer' in the buffer
;; containing the LaTeX input file.  But be aware that the shell script
;; version produces better error messages.
;;
;;------------------------------------------------------------------------

;; to make both Emacs 18 and the Emacs 19 byte compiler happy
(if (fboundp 'buffer-disable-undo)
    ()
  (fset 'buffer-disable-undo 'buffer-flush-undo))
     
(defvar	hyperlatex-produced-from)
(defvar hyperlatex-active-space)
(defvar hyperlatex-address)
(defvar hyperlatex-attributes)
(defvar hyperlatex-auto-menu)
(defvar hyperlatex-basename)
(defvar hyperlatex-bibitem-number)
(defvar hyperlatex-cite-names)
(defvar hyperlatex-command-name)
(defvar hyperlatex-command-start)
(defvar hyperlatex-continue-scan)
(defvar hyperlatex-elisp-directory)
(defvar hyperlatex-input-directory)
(defvar hyperlatex-extension-dirs)
(defvar hyperlatex-final-pass)
(defvar hyperlatex-footnote-number)
(defvar hyperlatex-footnotes)
(defvar hyperlatex-group-stack)
(defvar hyperlatex-html-directory)
(defvar hyperlatex-html-icons)
(defvar hyperlatex-html-level)
(defvar hyperlatex-index)
(defvar hyperlatex-is-article)
(defvar hyperlatex-label-number)
(defvar hyperlatex-labels)
(defvar hyperlatex-made-panel)
(defvar hyperlatex-make-panel)
(defvar hyperlatex-math-mode)
(defvar hyperlatex-max-depth)
(defvar hyperlatex-menu-in-section)
(defvar hyperlatex-new-commands)
(defvar hyperlatex-node-names)
(defvar hyperlatex-node-number)
(defvar hyperlatex-node-section)
(defvar hyperlatex-packages)
(defvar hyperlatex-produced-from-file)
(defvar hyperlatex-rev-sections)
(defvar hyperlatex-recursion-depth)
(defvar hyperlatex-example-depth)
(defvar hyperlatex-sect-number)
(defvar hyperlatex-sections)
(defvar hyperlatex-special-chars-regexp)
(defvar hyperlatex-stack)
(defvar hyperlatex-tabular-column-descr)
(defvar hyperlatex-title)

(defvar hyperlatex-known-packages
  '(hyperlatex a4 xspace)
  "Names of packages for which no hyperlatex extension exists.")

(defvar hyperlatex-default-depth 4)

(defvar hyperlatex-default-toppanel 
  "\\IfLink{#1#2#3}{%
\\IfLink{#1}{\\xlink{\\htmlimage[ALT=\"\"]{\\thehtmlicons/previous.xbm}}{#1}}{%
  \\htmlimage[ALT=\"\"]{\\thehtmlicons/previous.xbm}}
  \\IfLink{#2}{\\xlink{\\htmlimage[ALT=\"\"]{\\thehtmlicons/up.xbm}}{#2}}{%
    \\htmlimage[ALT=\"\"]{\\thehtmlicons/up.xbm}}
  \\IfLink{#3}{\\xlink{\\htmlimage[ALT=\"\"]{\\thehtmlicons/next.xbm}}{#3}}{%
    \\htmlimage[ALT=\"\"]{\\thehtmlicons/next.xbm}}\\\\
  \\IfLink{#1}{\\textbf{Go backward to }\\xlink{#4}{#1}\\\\}{}%
  \\IfLink{#2}{\\textbf{Go up to }\\xlink{#5}{#2}\\\\}{}%
  \\IfLink{#3}{\\textbf{Go forward to }\\xlink{#6}{#3}}{}
  \\htmlrule{}}{}")

(defvar hyperlatex-default-bottommatter
  "\\htmlrule\\thehtmladdress\\\\\n")

(defvar hyperlatex-default-bottompanel
  "\\IfLink{#1#2#3}{%
    \\IfLink{#1}
     {\\xlink{\\htmlimage[ALT=\"Prev\"]{\\thehtmlicons/previous.xbm}}{#1}}
     {\\htmlimage[ALT=\"\"]{\\thehtmlicons/previous.xbm}}
    \\IfLink{#2}
      {\\xlink{\\htmlimage[ALT=\"Up\"]{\\thehtmlicons/up.xbm}}{#2}}
      {\\htmlimage[ALT=\"\"]{\\thehtmlicons/up.xbm}}
    \\IfLink{#3}
      {\\xlink{\\htmlimage[ALT=\"Next\"]{\\thehtmlicons/next.xbm}}{#3}}
      {\\htmlimage[ALT=\"\"]{\\thehtmlicons/next.xbm}}}{}")

(defvar hyperlatex-html-levels
  '(("html2" 20) ("html3" 30) ("html2+" 25) ("netscape" 26)))

(defvar hyperlatex-html-accents
  '(( "'A" "Aacute"	)
    ( "^A" "Acirc"	)
    ( "`A" "Agrave"	)
    ( "~A" "Atilde"	)
    ( "\"A" "Auml"	)
    ( "'E" "Eacute"	)
    ( "^E" "Ecirc"	)
    ( "`E" "Egrave"	)
    ( "\"E" "Euml"	)
    ( "'I" "Iacute"	)
    ( "^I" "Icirc"	)
    ( "`I" "Igrave"	)
    ( "\"I" "Iuml"	)
    ( "~N" "Ntilde"	)
    ( "'O" "Oacute"	)
    ( "^O" "Ocirc"	)
    ( "`O" "Ograve"	)
    ( "~O" "Otilde"	)
    ( "\"O" "Ouml"	)
    ( "'U" "Uacute"	)
    ( "^U" "Ucirc"	)
    ( "`U" "Ugrave"	)
    ( "\"U" "Uuml"	)
    ( "'Y" "Yacute"	)
    ( "'a" "aacute"	)
    ( "^a" "acirc"	)
    ( "`a" "agrave"	)
    ( "~a" "atilde"	)
    ( "\"a" "auml"	)
    ( "'e" "eacute"	)
    ( "^e" "ecirc"	)
    ( "`e" "egrave"	)
    ( "\"e" "euml"	)
    ( "'\\i" "iacute"	)
    ( "^\\i" "icirc"	)
    ( "`\\i" "igrave"	)
    ( "\"\\i" "iuml"	)
    ( "~n" "ntilde"	)
    ( "'o" "oacute"	)
    ( "^o" "ocirc"	)
    ( "`o" "ograve"	)
    ( "~o" "otilde"	)
    ( "\"o" "ouml"	)
    ( "'u" "uacute"	)
    ( "^u" "ucirc"	)
    ( "`u" "ugrave"	)
    ( "\"u" "uuml"	)
    ( "'y" "yacute"	)
    ( "\"y" "yuml"	)))

(defvar hyperlatex-special-chars-basic-regexp
  "\\\\%{}]\\|\n[ ]*\n\\|---?\\|``\\|''\\|\\?`\\|!`")

(defvar hyperlatex-log-like-functions
  '("arccos" "arcsin" "arctan" "arg" "cos" "cosh" "cot" "coth"
    "csc" "deg" "det" "dim" "exp" "gcd" "hom" "inf" "ker"
    "lg" "lim" "liminf" "limsup" "ln" "log" "max" "min"
    "Pr" "sec" "sin" "sinh" "sup" "tan" "tanh"))

(defun hyperlatex-standard-definitions ()
  (hyperlatex-define-macro "toppanel" 6 hyperlatex-default-toppanel)
  (hyperlatex-define-macro "bottommatter" 0 hyperlatex-default-bottommatter)
  (hyperlatex-define-macro "bottompanel" 6 hyperlatex-default-bottompanel)
  (hyperlatex-define-macro "TeX" 0 "TeX")
  (hyperlatex-define-macro "LaTeX" 0 "LaTeX")
  (hyperlatex-define-macro "LaTeXe" 0 "LaTeX2e")
  (hyperlatex-define-macro "ldots" 0 "...")
  (hyperlatex-define-macro "quad" 0 "    ")
  (hyperlatex-define-macro "qquad" 0  "      ")
  (hyperlatex-define-macro "copyright" 0 "\\htmlsym{##169}")
  (hyperlatex-define-macro "ss" 0 "\\htmlsym{szlig}")
  (hyperlatex-define-macro "o"  0 "\\htmlsym{oslash}")
  (hyperlatex-define-macro "O"  0 "\\htmlsym{Oslash}")
  (hyperlatex-define-macro "oe" 0 "oe")
  (hyperlatex-define-macro "OE" 0 "OE")
  (hyperlatex-define-macro "ae" 0 "\\htmlsym{aelig}")
  (hyperlatex-define-macro "AE" 0 "\\htmlsym{AElig}")
  (hyperlatex-define-macro "aa" 0 "\\htmlsym{aring}")
  (hyperlatex-define-macro "AA" 0 "\\htmlsym{Aring}")
  (hyperlatex-define-macro "S"  0 "\\htmlsym{##167}")
  (hyperlatex-define-macro "P"  0 "\\htmlsym{##182}")
  (hyperlatex-define-macro "pounds"  0 "\\htmlsym{##163}")

  (hyperlatex-define-environment
   "abstract" 0
   "\\htmlheading[2]{Abstract}\\begin{blockquote}"
   "\\end{blockquote}")

  (hyperlatex-define-macro "caption" 1 "{\\par}#1{\\par}")
  (hyperlatex-define-environment "figure" 0 "\\htmlskipopt{\\par}" "\\par{}")
  (hyperlatex-define-environment "figure*" 0 "\\htmlskipopt{\\par}" "\\par{}")
  (hyperlatex-define-environment "table" 0 "\\htmlskipopt{\\par}" "\\par{}")
  (hyperlatex-define-environment "table*" 0 "\\htmlskipopt{\\par}" "\\par{}")

  (hyperlatex-define-macro "file"  0 "\\textit")

  (hyperlatex-define-macro "htmlfootnotemark" 1
			   "\\link{(#1)}{footnote-#1}")
  (hyperlatex-define-environment
   "thefootnotes" 0 "\\chapter{Footnotes}\n\\begin{description}\n"
   "\\end{description}")
  (hyperlatex-define-macro "htmlfootnoteitem" 2
			   "\\label{footnote-#1}\\item[(#1)]#2\n")
			   

  (hyperlatex-define-environment "verse" 0 "\\blockquote" "\\endblockquote")
  (hyperlatex-define-environment "quote" 0 "\\blockquote" "\\endblockquote")
  (hyperlatex-define-environment "quotation" 0
				 "\\blockquote" "\\endblockquote")
  (hyperlatex-define-environment "center" 0
				 "\\htmlcenter" "\\endhtmlcenter")
  
  (hyperlatex-define-environment
   "thebibliography" 1
   "\\chapter{References}\n\\begin{description}\n"
   "\\end{description}")
  (hyperlatex-define-macro "newblock" 0 "")
  (hyperlatex-define-macro "htmlbibitem" 2 "\\label{#2}\\item[{[#1]}]")
  (hyperlatex-define-macro "cite" 1 "\\link{\\htmlcite{#1}}{#1}")

  ;;
  ;; ------------------------ MATH ---------------------------
  ;;
  
  (hyperlatex-define-macro "[" 0 "\\begin{blockquote}$")
  (hyperlatex-define-macro "]" 0 "$\\end{blockquote}")

  (hyperlatex-define-environment "displaymath" 0
				 "\\begin{blockquote}$" "$\\end{blockquote}")
  (hyperlatex-define-environment "equation" 0
				 "\\begin{blockquote}$" "$\\end{blockquote}")
  (hyperlatex-define-environment "equation*" 0
				 "\\begin{blockquote}$" "$\\end{blockquote}")

  (hyperlatex-define-math-entity "alpha")
  (hyperlatex-define-math-entity "beta")
  (hyperlatex-define-math-entity "gamma")
  (hyperlatex-define-math-entity "Gamma")
  (hyperlatex-define-math-entity "delta")
  (hyperlatex-define-math-entity "Delta")
  (hyperlatex-define-math-entity "epsilon"	"epsi"	"eps")
  (hyperlatex-define-math-entity "varepsilon"	"epsi"	"eps")
  (hyperlatex-define-math-entity "zeta")
  (hyperlatex-define-math-entity "eta")
  (hyperlatex-define-math-entity "theta"	"thetav"	"theta")
  (hyperlatex-define-math-entity "Theta")
  (hyperlatex-define-math-entity "vartheta"	"theta"		"theta")
  (hyperlatex-define-math-entity "iota")
  (hyperlatex-define-math-entity "kappa")
  (hyperlatex-define-math-entity "lambda")
  (hyperlatex-define-math-entity "Lambda")
  (hyperlatex-define-math-entity "mu"		"mu"	"\\htmlsym{##181}")
  (hyperlatex-define-math-entity "nu")
  (hyperlatex-define-math-entity "xi")
  (hyperlatex-define-math-entity "Xi")
  (hyperlatex-define-math-entity "pi"		"pi"	"pi")
  (hyperlatex-define-math-entity "Pi")
  (hyperlatex-define-math-entity "varpi"	"piv"	"pi")
  (hyperlatex-define-math-entity "rho")
  (hyperlatex-define-math-entity "varrho"	"rho"		"rho")
  (hyperlatex-define-math-entity "sigma")
  (hyperlatex-define-math-entity "Sigma")
  (hyperlatex-define-math-entity "varsigma"	"sigmav"	"sigma")
  (hyperlatex-define-math-entity "tau")
  (hyperlatex-define-math-entity "upsilon"	"upsi"		"upsilon")
  (hyperlatex-define-math-entity "Upsilon"	"Upsi"		"Upsilon")
  (hyperlatex-define-math-entity "phi")
  (hyperlatex-define-math-entity "Phi")
  (hyperlatex-define-math-entity "varphi"	"phiv"		"phi")
  (hyperlatex-define-math-entity "chi")
  (hyperlatex-define-math-entity "psi")
  (hyperlatex-define-math-entity "omega")
  (hyperlatex-define-math-entity "Omega")
  
  (hyperlatex-define-math-entity "infty"	"inf"		"infty")
  (hyperlatex-define-math-entity "partial"	"pd"		"pd")
  (hyperlatex-define-math-entity "le"		"le"		"<=")
  (hyperlatex-define-math-entity "ge"		"ge"		">=")
  (hyperlatex-define-math-entity "leq"		"le"		"<=")
  (hyperlatex-define-math-entity "geq"		"ge"		">=")
  (hyperlatex-define-math-entity "equiv"	"equiv"		"==")
  (hyperlatex-define-math-entity "approx"	"ap"		"approx")
  (hyperlatex-define-math-entity "neq"		"ne"		"!=")
  (hyperlatex-define-math-entity "forall")
  (hyperlatex-define-math-entity "exists"	"exist")
  (hyperlatex-define-math-entity "Uparrow"	"uArr")
  (hyperlatex-define-math-entity "Downarrow"	"dArr")
  (hyperlatex-define-math-entity "uparrow"	"uarr")
  (hyperlatex-define-math-entity "downarrow"	"darr")
  (hyperlatex-define-math-entity "leftarrow"	"larr"		"<--")
  (hyperlatex-define-math-entity "rightarrow"	"rarr"		"-->")
  (hyperlatex-define-math-entity "leftrightarrow" "harr"	"<-->")

  (hyperlatex-define-math-macro "sum"  0 "sum"  "SUM")
  (hyperlatex-define-math-macro "prod" 0 "prod" "PROD")
  (hyperlatex-define-math-macro "int"  0 "int"  "INT")

  (let ((fn hyperlatex-log-like-functions))
    (while fn
      (hyperlatex-define-macro (car fn) 0 (car fn))
      (setq fn (cdr fn))))

  (hyperlatex-define-macro "pm" 0 "\\htmlsym{##177}")
  (hyperlatex-define-macro "cdot" 0 "\\htmlsym{##183}")
  (hyperlatex-define-macro "cdots" 0 "\\cdot \\cdot \\cdot")
  (hyperlatex-define-macro "times" 0 "\\htmlsym{##215}")
  (hyperlatex-define-macro "div" 0 "\\htmlsym{##247}")
  (hyperlatex-define-macro "ast" 0 "*")
  (hyperlatex-define-macro "setminus" 0 "~\\back~")
  (hyperlatex-define-macro "mid" 0 "~|~")
  (hyperlatex-define-macro "parallel" 0 "~||~")
  
  (hyperlatex-define-math-macro
   "frac" 2 "\\html{BOX} #1 \\html{OVER} #2 \\html{/BOX}"
   "(#1)/(#2)")
  (hyperlatex-define-math-macro
   "htmlsqrt" 1 "\\html{SQRT} #1 \\html{/SQRT}" "sqrt(#1)")
  (hyperlatex-define-math-macro
   "htmlroot" 2 "\\html{ROOT} #1 \\html{OF} #2 \\html{/ROOT}"
   "root^{#1}(#2)")
  
  )

(defvar hyperlatex-format-syntax-table nil)

;;;
;;; Hmm. I will have to think about the syntax table
;;;
(progn
  (setq hyperlatex-format-syntax-table (copy-syntax-table))
  (modify-syntax-entry ?\\ "\\" hyperlatex-format-syntax-table)
  
  (modify-syntax-entry ?{ "(}" hyperlatex-format-syntax-table)
  (modify-syntax-entry ?} "){" hyperlatex-format-syntax-table)
  (modify-syntax-entry ?\[ "_" hyperlatex-format-syntax-table)
  (modify-syntax-entry ?\] "_" hyperlatex-format-syntax-table)
  
  (modify-syntax-entry ?!  "." hyperlatex-format-syntax-table)
  (modify-syntax-entry ?#  "_" hyperlatex-format-syntax-table)
  (modify-syntax-entry ?$  "_" hyperlatex-format-syntax-table)
  (modify-syntax-entry ?%  "_" hyperlatex-format-syntax-table)
  (modify-syntax-entry ?&  "_" hyperlatex-format-syntax-table)
  (modify-syntax-entry ?.  "." hyperlatex-format-syntax-table)

  (modify-syntax-entry ?0  "_" hyperlatex-format-syntax-table)
  (modify-syntax-entry ?1  "_" hyperlatex-format-syntax-table)
  (modify-syntax-entry ?2  "_" hyperlatex-format-syntax-table)
  (modify-syntax-entry ?3  "_" hyperlatex-format-syntax-table)
  (modify-syntax-entry ?4  "_" hyperlatex-format-syntax-table)
  (modify-syntax-entry ?5  "_" hyperlatex-format-syntax-table)
  (modify-syntax-entry ?6  "_" hyperlatex-format-syntax-table)
  (modify-syntax-entry ?7  "_" hyperlatex-format-syntax-table)
  (modify-syntax-entry ?8  "_" hyperlatex-format-syntax-table)
  (modify-syntax-entry ?9  "_" hyperlatex-format-syntax-table)

  (modify-syntax-entry ?:  "." hyperlatex-format-syntax-table)
  (modify-syntax-entry ?@  "_" hyperlatex-format-syntax-table)
  (modify-syntax-entry ?\" " " hyperlatex-format-syntax-table)
  (modify-syntax-entry ?\' " " hyperlatex-format-syntax-table)

  (modify-syntax-entry ?\( " " hyperlatex-format-syntax-table)
  (modify-syntax-entry ?\) " " hyperlatex-format-syntax-table)
)

(defun batch-hyperlatex-format ()
  "Runs  hyperlatex-format-buffer  on the files remaining on the command line.
Must be used only with -batch, and kills emacs on completion.
Each file will be processed even if an error occurred previously."
  (if (not noninteractive)
      (error "batch-hyperlatex-format may only be used -batch."))
  (let ((auto-save-default nil)
	(find-file-run-dired nil)
	(hyperlatex-elisp-directory (getenv "HYPERLATEX_DIR"))
	(hyperlatex-html-icons (getenv "HYPERLATEX_ICON_URL")))
    (let ((error 0)
	  file
	  (files ()))
      (while command-line-args-left
	(setq file (expand-file-name (car command-line-args-left)))
	(cond ((not (file-exists-p file))
	       (message ">> %s does not exist!" file)
	       (setq error 1
		     command-line-args-left (cdr command-line-args-left)))
	      ((file-directory-p file)
	       (setq command-line-args-left
		     (nconc (directory-files file)
			    (cdr command-line-args-left))))
	      (t
	       (setq files (cons file files)
		     command-line-args-left (cdr command-line-args-left)))))
      (while files
	(setq file (car files)
	      files (cdr files))
	(condition-case err
	    (progn
	      (if buffer-file-name (kill-buffer (current-buffer)))
	      (find-file file)
	      (buffer-disable-undo (current-buffer))
	      (message "Hyperlatex formatting %s..." file)
	      (hyperlatex-format-buffer))
	  (error
	   (message ">> Error: %s" (prin1-to-string err))
	   (message ">>  point at")
	   (let ((s (buffer-substring (point)
				      (min (+ (point) 100)
					   (point-max))))
		 (tem 0))
	     (while (setq tem (string-match "\n+" s tem))
	       (setq s (concat (substring s 0 (match-beginning 0))
			       "\n>>  "
			       (substring s (match-end 0)))
		     tem (1+ tem)))
	     (message ">>  %s" s))
	   (message
	    "Hint: Try running Latex, it may give a better error message.")
	   (setq error 1))))
      (kill-emacs error))))

(defun hyperlatex-format-buffer ()
  "Process the current buffer as hyperlatex code, into a Html document.
The Html file output is generated in a directory specified in the
 \\htmldirectory command, or in the current directory."
  (interactive)
  (let ((lastmessage "Formatting Html file..."))
    (message lastmessage)
    (hyperlatex-format-buffer-1)
    (message "%s done." lastmessage)))

(defun hyperlatex-format-buffer-1 ()
  (let ((hyperlatex-html-directory ".")
	hyperlatex-basename
	hyperlatex-produced-from
	hyperlatex-title
	hyperlatex-address
	
	(hyperlatex-html-level 25)
	(hyperlatex-extension-dirs
	 (list (expand-file-name "~/.hyperlatex")
	       (expand-file-name hyperlatex-elisp-directory)))
	
	(hyperlatex-index nil)
	(hyperlatex-labels nil)
	(hyperlatex-sections nil)
	(hyperlatex-node-names nil)
	(hyperlatex-cite-names nil)
	
	hyperlatex-command-start
	hyperlatex-command-name
	hyperlatex-stack
	hyperlatex-group-stack
	(hyperlatex-active-space nil)
	(hyperlatex-tabular-column-descr nil)
	hyperlatex-special-chars-regexp
	
	(hyperlatex-packages nil)
	
	(hyperlatex-produced-from-file
	 (if (buffer-file-name (current-buffer))
	     (file-name-sans-versions (file-name-nondirectory
				       (buffer-file-name (current-buffer))))
	   ""))

	hyperlatex-rev-sections
	;; used to set levels of headings correctly:
	(hyperlatex-is-article nil)
	;; the depth of automatic menus, 0 for none
	(hyperlatex-auto-menu 1)
	(hyperlatex-max-depth hyperlatex-default-depth)
	(input-buffer (current-buffer))
	(hyperlatex-input-directory default-directory))

    (setq hyperlatex-produced-from
	  (if hyperlatex-produced-from-file
	      (concat "file: " hyperlatex-produced-from-file)
	    (concat "buffer " (buffer-name input-buffer))))
    (set-buffer (get-buffer-create " *Hyperlatex Html output*"))
    (fundamental-mode)
    (set-syntax-table hyperlatex-format-syntax-table)
    (erase-buffer)
    (insert-buffer-substring input-buffer)
    (setq case-fold-search nil)
    ;; Find end of preamble
    (goto-char (point-min))
    (re-search-forward "^[ \t]*\\\\begin[ \t]*{document}")
    (beginning-of-line)
    (let ((end (point-marker)))
      ;; figure out whether this is `article' style
      (goto-char (point-min))
      (if (re-search-forward
	   "^[ \t]*\\\\document\\(style\\|class\\)[^{]*{arti[a-zA-Z0-9]*}" end t)
	  (setq hyperlatex-is-article t))
      (message "Highest level sections are %s"
	       (if hyperlatex-is-article "sections" "chapters"))
      ;; \\htmltitle MUST be set
      (goto-char (point-min))
      (re-search-forward "^[ \t]*\\\\htmltitle" end)
      (setq hyperlatex-title (hyperlatex-get-arg-here))
      (message "Title of work is %s" hyperlatex-title)
      ;; \\htmldirectory
      (goto-char (point-min))
      (if (re-search-forward "^[ \t]*\\\\htmldirectory" end t)
	  (progn
	    (setq hyperlatex-html-directory (hyperlatex-get-arg-here))
	    (if (not (file-exists-p hyperlatex-html-directory))
		(progn
		  (message "Making directory %s" hyperlatex-html-directory)
		  (make-directory hyperlatex-html-directory t)))))
      ;; \\htmlname
      (goto-char (point-min))
      (setq hyperlatex-basename
	    (if (re-search-forward "^[ \t]*\\\\htmlname" end t)
		(hyperlatex-get-arg-here)
	      (string-match "\\.tex$" hyperlatex-produced-from-file)
	      (substring hyperlatex-produced-from-file 
			 0 (match-beginning 0))))
      (message "Using filename %s/%s.html" hyperlatex-html-directory
	       hyperlatex-basename)
      ;; \\htmldepth
      (goto-char (point-min))
      (if (re-search-forward "^[ \t]*\\\\htmldepth" end t)
	  (progn
	    (setq hyperlatex-max-depth 
		  (string-to-int (hyperlatex-get-arg-here)))
	    (if (<= hyperlatex-max-depth 0)
		(error "Illegal value of \\htmldepth"))
	    (message "Maximum depth for single documents is %d"
		     hyperlatex-max-depth)))
      ;; \\htmlautomenu
      (goto-char (point-min))
      (if (re-search-forward "^[ \t]*\\\\htmlautomenu" end t)
	  (setq hyperlatex-auto-menu
		(string-to-int (hyperlatex-get-arg-here))))
      ;; \\htmlicons
      (goto-char (point-min))
      (if (re-search-forward "^[ \t]*\\\\htmlicons" end t)
	  (setq hyperlatex-html-icons (hyperlatex-get-arg-here)))
      ;; \\htmladdress
      (goto-char (point-min))
      (if (re-search-forward "^[ \t]*\\\\htmladdress" end t)
	  (setq hyperlatex-address (hyperlatex-get-arg-here)))
      ;; \\NotSpecial
      (goto-char (point-min))
      (let ((str ""))
	(if (re-search-forward "^[ \t]*\\\\NotSpecial" end t)
	    (setq str (hyperlatex-get-arg-here)))
	;; compute hyperlatex-special-chars-regexp
	(setq  hyperlatex-special-chars-regexp
	       (concat "["
		       (if (string-match "\\\\do\\\\~" str) "" "~")
		       (if (string-match "\\\\do\\\\\\$" str) "" "$")
		       (if (string-match "\\\\do\\\\\\^" str) "" "^")
		       (if (string-match "\\\\do\\\\_" str) "" "_")
		       (if (string-match "\\\\do\\\\&" str) "" "&")
		       hyperlatex-special-chars-basic-regexp)))
      ;; \\htmllevel
      (goto-char (point-min))
      (if (re-search-forward "^[ \t]*\\\\htmllevel" end t)
	  (let* ((level (hyperlatex-get-arg-here))
		 (match (assoc level hyperlatex-html-levels)))
	    (if (null match)
		(error "Illegal \\htmllevel"))
	    (setq hyperlatex-html-level (car (cdr match)))))
      (message "Html level is %d" hyperlatex-html-level)
      ;; now hyperlatex-find-package-hooks may have a look
      (goto-char (point-min))
      (setq hyperlatex-packages
	    (nreverse (hyperlatex-find-package-hooks end)))
      (set-marker end nil))
    ;; init indices
    (setq hyperlatex-index nil)
    (setq hyperlatex-labels nil)
    (setq hyperlatex-sections nil)
    (setq hyperlatex-node-names nil)
    ;; run first pass
    (hyperlatex-format-buffer-2 nil)
    ;; generate link and node tables
    (setq hyperlatex-rev-sections (reverse hyperlatex-sections))
    ;; run second pass
    (erase-buffer)
    (insert-buffer-substring input-buffer)
    (hyperlatex-format-buffer-2 t)
    ;; now generate real Html and save nodes 
    (hyperlatex-final-substitutions)
    (hyperlatex-write-files)))

(defun hyperlatex-format-buffer-2 (hyperlatex-final-pass)
  (let (;; did we create a menu in this section?
	hyperlatex-menu-in-section
	(hyperlatex-attributes nil)
	(hyperlatex-new-commands nil)
	(hyperlatex-make-panel t)
	(hyperlatex-made-panel nil)
	(hyperlatex-math-mode nil)
	(hyperlatex-footnotes nil)
	(hyperlatex-footnote-number 0)
	(hyperlatex-node-number 0)
	(hyperlatex-sect-number 0)
	(hyperlatex-node-section 0)
	(hyperlatex-recursion-depth 0)
	hyperlatex-continue-scan
	(hyperlatex-label-number 0)
	(hyperlatex-bibitem-number 0))
    ;; Find end of preamble
    (hyperlatex-standard-definitions)
    (hyperlatex-run-package-hooks hyperlatex-packages)
    (goto-char (point-min))
    (re-search-forward "^[ \t]*\\\\begin[ \t]*{document}")
    (beginning-of-line)
    (let ((end (point-marker)))
      ;; \\htmlpanel
      (goto-char (point-min))
      (if (re-search-forward "^[ \t]*\\\\htmlpanel" end t)
	  (setq hyperlatex-make-panel
		(string= (hyperlatex-get-arg-here) "1")))
      ;; \\htmlattributes
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*\\\\htmlattributes" end t)
	(setq hyperlatex-command-start (point))
	(hyperlatex-format-htmlattributes))
      ;; newcommand and newenvironment
      (goto-char (point-min))
      (while (re-search-forward
	      "^[ \t]*\\(\\\\[WH][ \t]*\\)?\\\\\\(re\\)?newcommand" end t)
	(setq hyperlatex-command-start (point))
	(hyperlatex-format-newcommand))
      (goto-char (point-min))
      (while (re-search-forward
	      "^[ \t]*\\(\\\\[WH][ \t]*\\)?\\\\\\(re\\)?newenvironment" end t)
	(setq hyperlatex-command-start (point))
	(hyperlatex-format-newenvironment))
      (set-marker end nil))
    ;; insert linefeed at end of file
    (goto-char (point-max))
    (insert "\n")
    ;; Scan the buffer, start from \\topnode
    (goto-char (point-min))
    (re-search-forward "^[ \t]*\\\\topnode")
    (delete-region (point-min) (1- (match-beginning 0)))
    ;; now convert
    (hyperlatex-prelim-substitutions)
    (goto-char (point-min))
    (insert "\\begin{document}")
    (hyperlatex-format-region (point-min) (point-max))))

;;;
;;; ----------------------------------------------------------------------
;;;
;;; prelim-substitutions is called before scanning the file
;;; convert-html is called before saving to generate real HTML
;;; write-files saves the nodes in separate files

(defun hyperlatex-prelim-substitutions ()
  "Saves ISO characters with the 8th bit set."
  (goto-char (point-min))
  (while (re-search-forward "[\200-\377]" nil t)
    (replace-match (format "\M-C\M-{%d\M-}" (preceding-char)))))

(defun hyperlatex-final-substitutions ()
  "Scan buffer and replace the characters special for Html.
Replace PAR entries by \\html{P}, unless there is a magic NOPAR
next to it. Finally, remove or convert all magic entries."
  ;; replace PAR entries by <P>, if okay
  (goto-char (point-min))
  (while (search-forward "\M-p" nil t)
    (replace-match "")
    (if (= (following-char) ?\n)
	(delete-char 1))
    (or (progn
	  (goto-char (match-beginning 0))
	  (skip-chars-backward " \n\M-l\M-p")
	  (equal (preceding-char) (+ 128 ?n)))
	(progn
	  (goto-char (match-beginning 0))
	  (skip-chars-forward " \n\M-l\M-p")
	  (equal (following-char) (+ 128 ?n)))
	(progn
	  (goto-char (match-beginning 0))
	  (hyperlatex-gen "P" t))))
  ;; remove magic NOPAR, LABEL
  (goto-char (point-min))
  (while (re-search-forward "[\M-n\M-p\M-l]" nil t)
    (replace-match ""))
  ;; fixup &, <, >
  (goto-char (point-min))
  (while (search-forward "&" nil t)
    (replace-match "\M-&amp;" t))
  (goto-char (point-min))
  (while (search-forward ">" nil t)
    (replace-match "\M-&gt;" t))
  (goto-char (point-min))
  (while (search-forward "<" nil t)
    (replace-match "\M-&lt;" t))
  ;; finally, convert the magic chars to the real HTML control sequences
  (goto-char (point-min))
  (while (re-search-forward
	  "[\M-&\M-<\M->\M-%\M-{\M-}\M-~\M- \M--\M-'\M-`]" nil t)
    (replace-match (char-to-string (- (preceding-char) 128))))
  ;; and last, put back ISO characters
  (goto-char (point-min))
  (while (re-search-forward "\M-C{\\([0-9]+\\)}" nil t)
    (replace-match (char-to-string
		    (string-to-int
		     (buffer-substring (match-beginning 1)
				       (match-end 1)))))))
  
(defun hyperlatex-write-files ()
  "Saves the different Html documents in the corresponding files."
  (goto-char (point-max))
  (let ((fin (point))
	(magic-regexp (concat "^\M-XHtml:\\([^\n]+\\)\n")))
    (while (re-search-backward magic-regexp nil t)
      (let ((fname (concat hyperlatex-html-directory "/"
			   (buffer-substring (match-beginning 1)
					     (match-end 1))))
	    (beg (match-end 0))
	    (end fin))
	(setq fin (match-beginning 0))
	(write-region beg end fname)))))

;;;
;;; ----------------------------------------------------------------------
;;;
;;; These functions generate protected Html
;;;

(defun hyperlatex-gen (str &optional nopar after)
  "Inserts Html command STR. If optional second argument NOPAR is t,
surrounds it by magic markers that inhibit an automatic \\par before or
after this command. Optional third argument AFTER is inserted behind
the marker after the command."
  (let ((noparstr (if nopar "\M-n" ""))
	(afterstr (if after after "")))
    (insert noparstr "\M-<" str "\M->" noparstr afterstr)))

(defun hyperlatex-gensym (str)
  "Inserts Html command to generate special characters. Use
`(hyperlatex-gensym \"amp\")' to generate `&amp;'."
  (insert "\M-&" str ";"))

;;;
;;; ----------------------------------------------------------------------
;;;
;;; Parsing Hyperlatex
;;;

(defun hyperlatex-format-region (begin end)
  "This function formats the region from BEGIN to END into Html.
It is reentrant, so environments can call it recursively."
  (save-restriction
    (narrow-to-region begin end)
    (goto-char (point-min))
    (let ((hyperlatex-recursion-depth (1+ hyperlatex-recursion-depth))
	  (foochar nil))
      (setq hyperlatex-continue-scan hyperlatex-recursion-depth)
      (while (and (= hyperlatex-continue-scan hyperlatex-recursion-depth)
		  (re-search-forward hyperlatex-special-chars-regexp nil t))
	(setq foochar (preceding-char))
	(delete-region (1- (point)) (point))
	(cond
	 ((= foochar ?\\ )
	  ;; \command
	  ;; Handle a few special \-followed-by-one-char commands.
	  (setq foochar (following-char))
	  (cond ((memq foochar '(?, ?- ?/ ?@))
		 ;; \\, \\- \\/ \\@ are ignored
		 (delete-char 1))
		((memq foochar '(?^ ?' ?` ?\" ?~))
		 ;; replace general accents
		 (forward-char 1)
		 (let ((arg (hyperlatex-get-arg-here t)))
		   (if (string= arg "")
		       ()
		     (let ((match (assoc (concat (char-to-string foochar) arg)
					 hyperlatex-html-accents)))
		       (if (not match) (error "Unknown or unsupported accent"))
		       (delete-char -1)
		       (hyperlatex-gensym (nth 1 match))))))
		((memq foochar '(?\{ ?} ?\  ?\. ?% ?_ ?& ?# ?$))
		 ;; These characters are simply quoted
		 (forward-char 1))
		;; it is a command. parse it
		(t
		 (setq hyperlatex-command-start (point))
		 (if (/= (char-syntax foochar) ?w)
		     ;; a single letter command
		     (forward-char 1)
		   ;; \ is followed by a word; find the end of the word.
		   (forward-word 1)
		   ;; and delete white space
		   (hyperlatex-delete-whitespace))
		 (setq hyperlatex-command-name
		       (intern (buffer-substring hyperlatex-command-start
						 (point))))
		 ;; remove command
		 (delete-region hyperlatex-command-start (point))
		 (let ((cmd (get hyperlatex-command-name 'hyperlatex)))
		   (if cmd
		       (funcall cmd)
		     (hyperlatex-unsupported)
		     (goto-char hyperlatex-command-start))))))
	 ((= foochar ?\n)
	  (hyperlatex-empty-line))
	 ((= foochar ?-))
	 ((= foochar ?`)
	  (let ((prechar (preceding-char)))
	    (delete-char -1)
	    (cond ((= prechar ?`)
		   (insert "\""))
		  ((= prechar ??)
		   (hyperlatex-gensym "#191"))
		  ((= prechar ?!)
		   (hyperlatex-gensym "#161")))))
	 ((= foochar ?')
	  (delete-char -1)
	  (insert "\""))
	 ((= foochar ?%)
	  (delete-region (point) (progn (forward-line 1) (point)))
	  (hyperlatex-delete-whitespace))
	 ((= foochar ?\{)
	  (hyperlatex-begin-group))
	 ((= foochar ?\})
	  (hyperlatex-end-group))
	 ((= foochar ?~)
	  (hyperlatex-gensym "#160"))
	 ((= foochar ?&)
	  (hyperlatex-format-tab))
	 ((= foochar ?$)
	  (hyperlatex-math-mode))
	 ((= foochar ?_)
	  (hyperlatex-subscript))
	 ((= foochar ?^)
	  (hyperlatex-superscript))))
      (if (= hyperlatex-continue-scan hyperlatex-recursion-depth)
	  (setq hyperlatex-continue-scan (1- hyperlatex-recursion-depth)))
      (goto-char (point-max)))))
  
;;;
;;; ----------------------------------------------------------------------
;;;
;;; Parse arguments to commands
;;;

(defun hyperlatex-get-arg-here (&optional char)
  (setq hyperlatex-command-start (point))
  (hyperlatex-parse-required-argument char))
	
(defun hyperlatex-parse-required-argument (&optional char)
  "Parses the argument enclosed in braces after the commands.
Deletes command and returns argument.

If optional argument CHAR is not nil, also accept a single char."
  (goto-char hyperlatex-command-start)
  (hyperlatex-delete-comment)
  (cond ((looking-at "{") (forward-sexp 1))
	(char (forward-char 1))
	(t (error "missing argument!")))
  (prog1
      (buffer-substring (1+ hyperlatex-command-start) (1- (point)))
    (delete-region hyperlatex-command-start (point))))

(defun hyperlatex-parse-optional-argument ()
  "Parses the argument enclosed in brackets after the commands.
Deletes command and returns argument (nil if none)."
  (goto-char hyperlatex-command-start)
  (hyperlatex-delete-comment)
  (if (= (following-char) ?\[ )
      (progn
	(goto-char (1+ (point)))
	(while (/= (following-char) ?\])
	  (if (= (following-char) ?\{)
	      (forward-sexp 1)
	    (goto-char (1+ (point)))))
	(prog1
	    (buffer-substring (1+ hyperlatex-command-start) (point))
	  (delete-region hyperlatex-command-start (1+ (point)))))))

(defun hyperlatex-starred-p ()
  "Is current command starred? Remove star, and skip whitespace."
  (cond ((= (following-char) ?*)
	 (delete-char 1)
	 (hyperlatex-delete-whitespace)
	 t)))

(defvar hyperlatex-beginning-new-line nil)

(defun hyperlatex-delete-whitespace (&optional at-begin-line)
  (setq hyperlatex-beginning-new-line at-begin-line)
  (if hyperlatex-active-space
      ;; if space is active, we should not skip it
      ()
    (let ((beg (point)))
      (skip-chars-forward " \t")
      (delete-region beg (point))
      (if (looking-at "\n")
	  ;; if in mode N (TeXBook Chapter 8), make <P>
	  (cond (hyperlatex-beginning-new-line
		 (insert "\M-p")
		 (goto-char beg))
		;; else eat it and continue
		(t
		 (delete-char 1)
		 (hyperlatex-delete-whitespace t)))))))

(defun hyperlatex-insert-required-argument ()
  (save-excursion (insert (hyperlatex-parse-required-argument))))

(defun hyperlatex-delete-comment ()
  "When looking at % character, deletes the comment."
  (hyperlatex-delete-whitespace)
  (while (looking-at "%")
    (delete-region (point) (progn (forward-line 1) (point)))
    (hyperlatex-delete-whitespace t)))

(defun hyperlatex-format-T ()
  "Discards the comment line, but tries to be more TeX like by deleting 
the following whitespace."
  (goto-char hyperlatex-command-start)
  (if hyperlatex-beginning-new-line
      ;; the comment line was empty
      ()
    (delete-region (point) (progn (forward-line 1) (point)))
    (hyperlatex-delete-whitespace t)))

;;;
;;; ----------------------------------------------------------------------
;;;
;;; \newcommand, \newenvironment, \xxx
;;;

(put 'newcommand	'hyperlatex 'hyperlatex-format-newcommand)
(put 'newenvironment	'hyperlatex 'hyperlatex-format-newenvironment)
(put 'renewcommand	'hyperlatex 'hyperlatex-format-newcommand)
(put 'renewenvironment	'hyperlatex 'hyperlatex-format-newenvironment)
(put 'htmlskipopt	'hyperlatex 'hyperlatex-skip-optional-argument)

(defun hyperlatex-format-newcommand ()
  (let ((name (hyperlatex-parse-required-argument))
	(nbargs (hyperlatex-parse-optional-argument))
	(expansion (hyperlatex-parse-required-argument)))
    (hyperlatex-define-macro (substring name 1)
			     (if nbargs (string-to-int nbargs) 0)
			     expansion)))

(defun hyperlatex-format-newenvironment ()
  (let ((name (hyperlatex-parse-required-argument))
	(nbargs (hyperlatex-parse-optional-argument))
	(beginexp (hyperlatex-parse-required-argument))
	(endexp   (hyperlatex-parse-required-argument)))
    (hyperlatex-define-environment name
				   (if nbargs (string-to-int nbargs) 0)
				   beginexp endexp)))

(defun hyperlatex-unsupported (&optional silent)
  "Called for \\commands not defined in Hyperlatex. Looks them up in
`hyperlatex-new-commands' and inserts them at point.
 Complains if not found, unless optional argument SILENT is non-nil."
  (let ((match (assoc (symbol-name hyperlatex-command-name)
		      hyperlatex-new-commands)))
    (if match
	(let* ((nbargs (car (cdr match)))
	       (count nbargs)
	       (expansion (car (cdr (cdr match))))
	       arguments)
	  (while (> count 0)
	    (setq count (1- count))
	    (setq arguments
		  (cons (hyperlatex-parse-required-argument) arguments)))
	  (insert expansion)
	  ;; replace arguments in expansion
	  (let ((end (point-marker)))
	    (goto-char hyperlatex-command-start)
	    (while (search-forward "#" end t)
	      (if (looking-at "[1-9]")
		  (let ((narg (- (following-char) ?1)))
		    (delete-region (1- (point)) (1+ (point)))
		    (insert-before-markers
		     (nth (- (1- nbargs) narg) arguments)))
		(delete-char -1)
		(skip-chars-forward "#")))
	    (goto-char end)
	    (set-marker end nil)))
      (if silent
	  ()
	(error "Unknown command: %s" (symbol-name hyperlatex-command-name))))))

(defun hyperlatex-define-macro (name nbargs expansion)
  (setq hyperlatex-new-commands
	(cons (list name nbargs expansion)
	      hyperlatex-new-commands)))

(defun hyperlatex-define-environment (name nbargs beginexp endexp)
  (setq hyperlatex-new-commands
	(cons (list name nbargs beginexp)
	      (cons (list (concat "end" name) 0 endexp)
		    hyperlatex-new-commands))))

(defun hyperlatex-define-math-entity (name &optional with-math no-math)
  (if (>= hyperlatex-html-level 29)
      (hyperlatex-define-macro
       name 0
       (format "\\htmlsym{%s}" (if with-math with-math name)))
    (hyperlatex-define-macro name 0 (if no-math no-math name))))

(defun hyperlatex-define-math-macro (name nbarg &optional with-math no-math)
  (if (>= hyperlatex-html-level 29)
      (hyperlatex-define-macro name nbarg (if with-math with-math name))
    (hyperlatex-define-macro name nbarg (if no-math no-math name))))

(defun hyperlatex-skip-optional-argument ()
  (let ((arg  (hyperlatex-parse-required-argument))
	(oarg (hyperlatex-parse-optional-argument)))
    (insert arg)
    (goto-char hyperlatex-command-start)))
  
;;;
;;; ----------------------------------------------------------------------
;;;
;;; Grouping and Environments
;;;

(put 'begin	'hyperlatex 'hyperlatex-format-begin)
(put 'end	'hyperlatex 'hyperlatex-format-end)
(put 'group	'hyperlatex 'hyperlatex-format-ignore)

;; \begin{xxx} pushes 'xxx on hyperlatex-stack.
;; \end{yyy} checks whether the proper environment is terminated.
;; { and } is treated as `group' environment
;; \begin adds new entry "" to hyperlatex-group-stack
;; \end pops the top string and inserts it

(defun hyperlatex-format-begin ()
  (setq hyperlatex-command-name (intern (hyperlatex-parse-required-argument)))
  (setq hyperlatex-stack
	(cons hyperlatex-command-name hyperlatex-stack))
  (setq hyperlatex-group-stack
	(cons "" hyperlatex-group-stack))
  (let ((cmd (get hyperlatex-command-name 'hyperlatex)))
    (if cmd
	(funcall cmd)
      (hyperlatex-unsupported)
      (goto-char hyperlatex-command-start))))

(defun hyperlatex-format-end ()
  (let* ((env    (hyperlatex-parse-required-argument))
	 (endenv (intern (concat "end" env)))
	 (cmd    (get endenv 'hyperlatex)))
    (setq hyperlatex-command-name endenv)
    ;; (insert (car hyperlatex-group-stack))
    ;; (setcar hyperlatex-group-stack "")
    (if cmd
	(funcall cmd)
      (hyperlatex-unsupported t)
      (hyperlatex-format-region hyperlatex-command-start (point)))
    (if (not (eq (car hyperlatex-stack) (intern env)))
	(error "\\end{%s} matches \\begin{%s}"
	       env (car hyperlatex-stack)))
    (setq hyperlatex-stack (cdr hyperlatex-stack))
    (setq hyperlatex-command-start (point))
    (save-excursion
      (insert (car hyperlatex-group-stack))
      (setq hyperlatex-group-stack (cdr hyperlatex-group-stack)))))

(defun hyperlatex-begin-group ()
  (setq hyperlatex-stack
	(cons 'group hyperlatex-stack))
  (setq hyperlatex-group-stack
	(cons "" hyperlatex-group-stack)))

(defun hyperlatex-end-group ()
  (if (or (null hyperlatex-stack)
	  (not (eq (car hyperlatex-stack) 'group)))
      (error "Too many }'s."))
  (setq hyperlatex-stack (cdr hyperlatex-stack))
  (insert (car hyperlatex-group-stack))
  (setq hyperlatex-group-stack (cdr hyperlatex-group-stack)))

(defun hyperlatex-pop-stacks ()
  (setq hyperlatex-stack (cdr hyperlatex-stack))
  (setq hyperlatex-group-stack (cdr hyperlatex-group-stack)))

(defun hyperlatex-in-stack (tag)
  (memq tag hyperlatex-stack))

;;;
;;; ----------------------------------------------------------------------
;;;
;;; Simple Tex/Html choices, par
;;;

(put 'par	'hyperlatex 'hyperlatex-format-par)
(put 'T		'hyperlatex 'hyperlatex-format-T)
(put 'W		'hyperlatex 'hyperlatex-format-ignore)
(put 'texonly	'hyperlatex 'hyperlatex-parse-required-argument)
(put 'htmlonly	'hyperlatex 'hyperlatex-insert-required-argument)
(put 'texorhtml	'hyperlatex 'hyperlatex-format-texorhtml)
(put 'input	'hyperlatex 'hyperlatex-format-input)

(defun hyperlatex-format-ignore ()
  "Function that does not do anything.")

(defun hyperlatex-format-par ()
  (insert "\M-p"))

(defun hyperlatex-empty-line ()
  (delete-region (match-beginning 0) (point))
  (insert "\n\M-p"))

(defun hyperlatex-format-texorhtml ()
  (hyperlatex-parse-required-argument)
  (hyperlatex-insert-required-argument))

(defun hyperlatex-format-input ()
  (save-excursion
    (let* ((arg (hyperlatex-parse-required-argument))
	   (file-name
	    (cond ((file-readable-p
		    (expand-file-name arg hyperlatex-input-directory))
		   (expand-file-name arg hyperlatex-input-directory))
		  ((file-readable-p (expand-file-name
				     (concat arg ".tex")
				     hyperlatex-input-directory))
		   (expand-file-name (concat arg ".tex")
				     hyperlatex-input-directory))
		  (t (error "I can't find the file %s" arg)))))
      (message "Inserting file %s..." file-name)
      (insert-file file-name)
      (exchange-point-and-mark)
      (narrow-to-region hyperlatex-command-start (point))
      (hyperlatex-prelim-substitutions)
      (goto-char (point-min))
      (widen)
      (message "Inserting file %s...done" file-name))))

;;;
;;; ----------------------------------------------------------------------
;;;
;;; Make sections and nodes
;;;

(put 'topnode		'hyperlatex 'hyperlatex-format-topnode)
(put 'chapter		'hyperlatex 'hyperlatex-format-chapter)
(put 'section		'hyperlatex 'hyperlatex-format-section)
(put 'subsection	'hyperlatex 'hyperlatex-format-subsection)
(put 'subsubsection	'hyperlatex 'hyperlatex-format-subsubsection)
(put 'paragraph		'hyperlatex 'hyperlatex-format-paragraph)
(put 'subparagraph	'hyperlatex 'hyperlatex-format-subparagraph)
(put 'xname		'hyperlatex 'hyperlatex-format-xname)
(put 'htmlpanel		'hyperlatex 'hyperlatex-format-htmlpanel)
(put 'htmlheading	'hyperlatex 'hyperlatex-format-htmlheading)
(put 'htmlattributes	'hyperlatex 'hyperlatex-format-htmlattributes)
(put 'IfLink		'hyperlatex 'hyperlatex-format-iflink)

(defun hyperlatex-format-xname ()
  (setq hyperlatex-node-names
	(cons (cons (1+ hyperlatex-node-number)
		    (format "%s.html" (hyperlatex-parse-required-argument)))
	      hyperlatex-node-names)))

(defun hyperlatex-format-htmlpanel ()
  (setq hyperlatex-make-panel
	(string= (hyperlatex-parse-required-argument) "1")))

(defun hyperlatex-format-htmlattributes ()
  (let* ((starp (hyperlatex-starred-p))
	 (tag (hyperlatex-parse-required-argument))
	 (attr (hyperlatex-parse-required-argument))
	 (match (assoc tag hyperlatex-attributes)))
    (if (or (null match)
	    starp)
	(setq hyperlatex-attributes
	      (cons (list tag starp attr)
		    hyperlatex-attributes))
      (setcdr match (list starp attr)))))

(defun hyperlatex-get-attributes (tag)
  (let ((match (assoc tag hyperlatex-attributes)))
    (if (null match)
	tag
      (let ((remove (nth 1 match))
	    (atr (nth 2 match)))
	(if remove
	    (setq hyperlatex-attributes
		  (delq match hyperlatex-attributes)))
	(if (string= atr "")
	    tag
	  (concat tag " " atr))))))

(defun hyperlatex-format-htmlheading ()
  (let ((oarg (hyperlatex-parse-optional-argument))
	(arg (hyperlatex-parse-required-argument)))
    (if oarg () (setq oarg "1"))
    (hyperlatex-gen (concat "H" oarg) t arg)
    (hyperlatex-gen (concat "/H" oarg))))

(defun hyperlatex-format-chapter ()
  (hyperlatex-format-chapter-1 1))

(defun hyperlatex-format-section ()
  (hyperlatex-format-chapter-1 2))

(defun hyperlatex-format-subsection ()
  (hyperlatex-format-chapter-1 3))

(defun hyperlatex-format-subsubsection ()
  (hyperlatex-format-chapter-1 4))

(defun hyperlatex-format-paragraph ()
  (hyperlatex-format-chapter-1 5))

(defun hyperlatex-format-subparagraph ()
  (hyperlatex-format-chapter-1 6))

(defun hyperlatex-format-topnode ()
  "Create the Top node of the Html document."
  (let ((arg (hyperlatex-parse-required-argument)))
    (setq hyperlatex-node-number 0)
    (setq hyperlatex-sect-number 0)
    (setq hyperlatex-menu-in-section nil)
    (hyperlatex-make-node-header nil)
    (if hyperlatex-final-pass
	()
      (setq hyperlatex-sections
	    (cons (list 0 0 "Top" 0 0) hyperlatex-sections)))
    (if (string= arg "")
	(insert "\M-l")
      (save-excursion
	(hyperlatex-gen "H1" t)
	(insert arg)
	(hyperlatex-gen "/H1" t "\M-l\n")))))

(defun hyperlatex-new-node (level head)
  "Finish up the previous node, and start a new node.
Assumes that the command starting the new node has already been removed,
and that we are at the beginning of a new line."
  ;; finish up old node
  (hyperlatex-finish-node)
  (setq hyperlatex-node-number (1+ hyperlatex-node-number))
  (setq hyperlatex-sect-number (1+ hyperlatex-sect-number))
  (hyperlatex-make-node-header head))

(defun hyperlatex-format-chapter-1 (in-level)
  (hyperlatex-starred-p)
  (let* ((optarg (hyperlatex-parse-optional-argument))
	 (reqarg (hyperlatex-parse-required-argument))
	 (level  (cond ((= in-level 1) 1)
		       (hyperlatex-is-article (1- in-level))
		       (t in-level)))
	 (head   (if optarg optarg reqarg))
	 (new-node (< level hyperlatex-max-depth)))
    (hyperlatex-delete-whitespace)
    ;; So we can see where we are.
    (message (if hyperlatex-final-pass "Formatting: %s ... " "Parsing: %s ... ") reqarg)
    ;; if level is high enough, start new node
    (if new-node
	(hyperlatex-new-node level head)
      ;; otherwise add a new label
      (setq hyperlatex-sect-number (1+ hyperlatex-sect-number))
      (setq hyperlatex-label-number (1+ hyperlatex-label-number)))
    ;; finally, add new heading
    (if hyperlatex-final-pass
	()
      (setq hyperlatex-sections
	    (cons (list hyperlatex-sect-number
			hyperlatex-node-number
			head
			level
			(1- hyperlatex-label-number))
		  hyperlatex-sections)))
    (setq hyperlatex-menu-in-section nil)
    (if (string= reqarg "")
	(if new-node
	    (insert "\M-l")
	  (hyperlatex-gen
	   (format "A NAME=\"%d\"" (1- hyperlatex-label-number)) t)
	  (hyperlatex-gensym "#160")
	  (hyperlatex-gen "/A" t "\M-l"))
      (save-excursion
	(if new-node
	    (progn
	      (hyperlatex-gen (format "H%d" level) t reqarg)
	      (hyperlatex-gen (format "/H%d" level) t "\M-l\n"))
	  (hyperlatex-gen (format "H%d" level) t)
	  (hyperlatex-gen
	   (format "A NAME=\"%d\"" (1- hyperlatex-label-number)) t)
	  (insert reqarg)
	  (hyperlatex-gen "/A" t "\M-l")
	  (hyperlatex-gen (format "/H%d" level) t "\M-l\n"))))))

;;;
;;; ----------------------------------------------------------------------
;;;
;;; Final Pass: Insert Panels and Menus
;;;

(defun hyperlatex-make-node-header (head)
  "Creates header for new node, with filename, title etc."
  (insert "\n\M-XHtml:" (hyperlatex-fullname hyperlatex-node-number) "\n")
  (setq hyperlatex-made-panel hyperlatex-make-panel)
  (hyperlatex-gen
   (if (= hyperlatex-html-level 30)
       "!DOCTYPE HTML PUBLIC \"-//W3O//DTD W3 HTML 3.0//EN\""
     "!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\"")
   nil "\n")
  (hyperlatex-gen "HTML" nil "\n")
  (hyperlatex-gen
   (concat "!-- HTML file produced from " hyperlatex-produced-from
	   " --\n -- using Hyperlatex v 1.4 (c) Otfried Schwarzkopf --")
	   nil "\n")
  (hyperlatex-gen "HEAD" nil "\n")
  (hyperlatex-gen "TITLE")
  (insert hyperlatex-title (if head (concat " -- " head) ""))
  (hyperlatex-gen "/TITLE" nil "\n")
  ;;  (and hyperlatex-made-panel
  ;;       hyperlatex-final-pass
  ;;       (hyperlatex-insert-links hyperlatex-sect-number))
  (hyperlatex-gen "/HEAD")
  (hyperlatex-gen (hyperlatex-get-attributes "BODY") nil "\n")
  (setq hyperlatex-label-number 1)
  (setq hyperlatex-node-section hyperlatex-sect-number)
  (and hyperlatex-made-panel
       hyperlatex-final-pass
       (hyperlatex-make-panel "toppanel" hyperlatex-node-section)))

(defun hyperlatex-finish-node ()
  "Finish up the previous node."
  ;; insert automatic menu, if desired
  (and (not hyperlatex-menu-in-section)
       (not (zerop hyperlatex-auto-menu))
       hyperlatex-final-pass
       (hyperlatex-insert-menu hyperlatex-sect-number hyperlatex-auto-menu))
  ;; and finish with bottom panel
  (if hyperlatex-final-pass
      (let ((start (point)))
	(insert "\\bottommatter{}")
	(hyperlatex-format-region start (point))
	(if hyperlatex-made-panel
	    (hyperlatex-make-panel "bottompanel" hyperlatex-node-section))))
  (hyperlatex-gen "/BODY")
  (hyperlatex-gen "/HTML" nil "\n"))

(defun hyperlatex-fullname (node-number)
  (if (zerop node-number)
      (concat hyperlatex-basename ".html")
    (let ((m (assoc node-number hyperlatex-node-names)))
      (if m
	  (cdr m)
	(format "%s_%d.html" hyperlatex-basename node-number)))))

;;;
;;; ----------------------------------------------------------------------
;;;

(defun hyperlatex-sect-head (sect)
  "Returns heading of SECT, a pointer into either list."
  (nth 2 sect))

(defun hyperlatex-sect-level (sect)
  "Returns level of SECT, a pointer into either list."
  (nth 3 sect))

(defun hyperlatex-sect-node (sect)
  "Returns node number of SECT, a pointer into either list."
  (nth 1 sect))

(defun hyperlatex-sect-num (sect)
  "Returns section number of SECT, a pointer into either list."
  (car sect))

(defun hyperlatex-sect-label (sect)
  "Returns label of SECT, a pointer into either list."
  (nth 4 sect))

;;;
;;; ----------------------------------------------------------------------
;;;

(defun hyperlatex-prev-node (sect)
  "Returns the previous node of section number SECT."
  (if (zerop sect)
      ()
    (let ((sp hyperlatex-sections))
      (while (/= (hyperlatex-sect-num (car sp)) sect)
	(setq sp (cdr sp)))
      ;; sp points to section 
      (let ((lev (hyperlatex-sect-level (car sp))))
	(setq sp (cdr sp))
	(while (> (hyperlatex-sect-level (car sp)) lev)
	  (setq sp (cdr sp)))
	;; now sp points at previous section with level equal or higher
	(if (= (hyperlatex-sect-level (car sp)) lev)
	    (car sp)
	  ())))))

(defun hyperlatex-up-node (sect)
  "Returns the up node of section number SECT."
  (if (zerop sect)
      ()
    (let ((sp hyperlatex-sections))
      (while (/= (hyperlatex-sect-num (car sp)) sect)
	(setq sp (cdr sp)))
      ;; sp points to section 
      (let ((lev (hyperlatex-sect-level (car sp))))
	(setq sp (cdr sp))
	(while (and sp (>= (hyperlatex-sect-level (car sp)) lev))
	  (setq sp (cdr sp)))
	;; now sp points at previous section with higher level
	(car sp)))))
  
(defun hyperlatex-next-node (sect)
  "Returns the next node of section number SECT."
  (if (zerop sect)
      ()
    (let ((sp hyperlatex-rev-sections))
      (while (/= (hyperlatex-sect-num (car sp)) sect)
	(setq sp (cdr sp)))
      ;; sp points to section 
      (let ((lev (hyperlatex-sect-level (car sp))))
	(setq sp (cdr sp))
	(while (and sp (> (hyperlatex-sect-level (car sp)) lev))
	  (setq sp (cdr sp)))
	;; now sp points at next section with higher or same level, or is nil
	(if (and sp (= (hyperlatex-sect-level (car sp)) lev))
	    (car sp)
	  ())))))

(defun hyperlatex-make-panel (command secnum)
  "Inserts macro to make navigation panel for node SECNUM."
  (let ((prev (hyperlatex-prev-node secnum))
	(next (hyperlatex-next-node secnum))
	(up   (hyperlatex-up-node secnum))
	(start (point)))
    (let ((prev-url
	   (if prev (hyperlatex-fullname (hyperlatex-sect-node prev)) ""))
	  (prev-title
	   (if prev (hyperlatex-sect-head prev) ""))
	  (next-url
	   (if next (hyperlatex-fullname (hyperlatex-sect-node next)) ""))
	  (next-title
	   (if next (hyperlatex-sect-head next) ""))
	  (up-url
	   (if up (hyperlatex-fullname (hyperlatex-sect-node up)) ""))
	  (up-title
	   (if up (hyperlatex-sect-head up) ""))
	  (here (point)))
      (insert "\\" command "{" prev-url "}{" up-url "}{" next-url "}{"
	      prev-title "}{" up-title "}{" next-title "}\n"))
    (hyperlatex-format-region start (point))))

(defun hyperlatex-format-iflink ()
  (let ((link (hyperlatex-parse-required-argument))
	(exists (hyperlatex-parse-required-argument))
	(void  (hyperlatex-parse-required-argument)))
    (insert (if (string= link "")
		void
	      exists))
    (goto-char hyperlatex-command-start)))

;;
;; this is currently not used --- do we still want this?
;;
(defun hyperlatex-insert-html-links (secnum)
  "Inserts Html `link' entries for node SECNUM."
  (let ((prev (hyperlatex-prev-node secnum))
	 (next (hyperlatex-next-node secnum))
	 (up   (hyperlatex-up-node secnum)))
    (if prev
	(progn
	  (hyperlatex-gen
	   (format "LINK REV=\"Precedes\" HREF=\"%s\""
		   (hyperlatex-fullname (hyperlatex-sect-node prev)))
	   nil "\n")))
    (if up
	(progn
	  (hyperlatex-gen
	   (format "LINK REV=\"Subdocument\" HREF=\"%s\""
		   (hyperlatex-fullname (hyperlatex-sect-node up)))
    	   nil "\n")))
    (if next
	(progn
	  (hyperlatex-gen
	   (format "LINK REL=\"Precedes\" HREF=\"%s\""
		   (hyperlatex-fullname (hyperlatex-sect-node next)))
	   nil "\n")))))

;;;
;;; ----------------------------------------------------------------------
;;;
;;; Make menus
;;;

(put 'htmlmenu	'hyperlatex 'hyperlatex-format-makemenu)

(defun hyperlatex-format-makemenu ()
  "We want a menu here, with given depth."
  (let ((depth (string-to-int (hyperlatex-parse-required-argument))))
    (setq hyperlatex-menu-in-section t)
    (if hyperlatex-final-pass
	(hyperlatex-insert-menu hyperlatex-sect-number depth))))

(defun hyperlatex-close-menus (newlev lastlev)
  "Inserts enough MENU or /MENU tags to get to NEWLEV (from LASTLEV)."
  (let ((oldlev lastlev))
    (while (> newlev oldlev)
      (hyperlatex-gen "MENU" nil "\n")
      (setq oldlev (1+ oldlev)))
    (while (< newlev oldlev)
      (hyperlatex-gen "/MENU" nil "\n")
      (setq oldlev (1- oldlev)))))

(defun hyperlatex-insert-menu (secnum depth)
  "Insert a menu for section SECNUM of depth DEPTH."
  (let ((sp hyperlatex-rev-sections)
	(here (point)))
    (while (/= (hyperlatex-sect-num (car sp)) secnum)
      (setq sp (cdr sp)))
    ;; sp points to section 
    (let* ((lev (hyperlatex-sect-level (car sp)))
	   (nodenum (hyperlatex-sect-node (car sp)))
	   (lastlev lev))
      (setq sp (cdr sp))
      (while (and sp (> (hyperlatex-sect-level (car sp)) lev))
	;; sp points to a subsection of mine!
	(if (<= (hyperlatex-sect-level (car sp)) (+ lev depth))
	    ;; make a menu entry
	    (let ((newlev (hyperlatex-sect-level (car sp))))
	      (hyperlatex-close-menus newlev lastlev)
	      (setq lastlev newlev)
	      (hyperlatex-gen "LI")
	      (hyperlatex-gen (format "A HREF=\"%s\""
				      (hyperlatex-gen-url
				       (hyperlatex-sect-node (car sp))
				       (hyperlatex-sect-label (car sp))
				       nodenum)))
	      (insert (hyperlatex-sect-head (car sp)))
	      (hyperlatex-gen "/A" nil "\n")))
	(setq sp (cdr sp)))
      (hyperlatex-close-menus lev lastlev))
    (hyperlatex-format-region here (point))))
    
;;;
;;; ----------------------------------------------------------------------
;;;
;;; Cross referencing, hypertext links
;;;

(put 'label	'hyperlatex 'hyperlatex-format-label)
(put 'xlabel	'hyperlatex 'hyperlatex-format-xlabel)
(put 'link	'hyperlatex 'hyperlatex-format-link)
(put 'xlink	'hyperlatex 'hyperlatex-format-xlink)

(defun hyperlatex-drop-label ()
  "Drop a label at the current position and return its number. Reuse last label
if there is one."
  (if (save-excursion
	(skip-chars-backward " \t\n\M-p\M-n")
	(= (preceding-char) (+ ?l 128)))
      ()
    ;; else make a new label at current position
    (insert "\M-<A NAME=\"" (format "%d" hyperlatex-label-number)
	    "\"\M->\M-&#160;\M-</A\M->\M-l")
    (setq hyperlatex-label-number (1+ hyperlatex-label-number)))
  (1- hyperlatex-label-number))

(defun hyperlatex-format-label ()
  "Creates a label at current position... But if we are directly behind
a section heading, uses section's label instead."
  (let ((label (hyperlatex-parse-required-argument))
	(number (hyperlatex-drop-label)))
    (if hyperlatex-final-pass
	()
      (setq hyperlatex-labels
	    (cons (list label number hyperlatex-node-number)
		  hyperlatex-labels)))))

(defun hyperlatex-format-xlabel ()
  "Creates an external label at current position."
  (let ((label (hyperlatex-parse-required-argument)))
    (insert "\M-<A NAME=\"" label "\"\M->\M-&#160;\M-</A\M->")))

(defun hyperlatex-gen-url (label-node label-number &optional current)
  "Generates a URL for a label in NODE with NUMBER. If node is the same as the
CURRENT node, simply returns `#NUMBER', else returns `NAME#NUMBER', unless
NUMBER is zero, in which case the returned url is `NAME`.
CURRENT is optional, and defaults to the current node."
  (if (zerop label-number)
      (hyperlatex-fullname label-node)
    (format "%s#%d"
	    (if (= (if current current hyperlatex-node-number) label-node)
		""
	      (hyperlatex-fullname label-node))
	    label-number)))

(defun hyperlatex-format-link-1 (is-url)
  (hyperlatex-starred-p)
  (let* ((text (hyperlatex-parse-required-argument))
	 (latex-text (hyperlatex-parse-optional-argument))
	 (label-or-url (hyperlatex-parse-required-argument))
	 (url (if is-url
		  label-or-url
		(if hyperlatex-final-pass
		    (hyperlatex-label-to-url label-or-url
					     hyperlatex-node-number)
		  "")))
	 (here (point)))
    (hyperlatex-gen (concat "A HREF=\"" url "\""))
    (insert text)
    (hyperlatex-gen "/A")
    (goto-char hyperlatex-command-start)))

(defun hyperlatex-format-link ()
  (hyperlatex-format-link-1 nil))
  
(defun hyperlatex-format-xlink ()
  (hyperlatex-format-link-1 t))

(defun hyperlatex-label-to-url (label node-number)
  "Generates the url for label LABEL in node NODE-NUMBER."
  (let ((match (assoc label hyperlatex-labels)))
    (if (null match)
	(if (not noninteractive)
	    (error "Unknown label %s" label)
	  (message "WARNING: Unknown label %s " label))
      (hyperlatex-gen-url (nth 2 match) (nth 1 match) node-number))))

;;;
;;; ----------------------------------------------------------------------
;;;
;;; word breaking
;;;

(put 'mbox	  'hyperlatex	'hyperlatex-format-mbox)

(defun hyperlatex-format-mbox ()
  (let ((arg (hyperlatex-parse-required-argument)))
    (insert arg)
    (let ((end (point-marker)))
      (goto-char hyperlatex-command-start)
      (while (re-search-forward "[ \t\n]+" end t)
	(replace-match "\M-&160;"))
      (goto-char hyperlatex-command-start)
      (set-marker end nil))))

;;;
;;; ----------------------------------------------------------------------
;;;
;;; Environments
;;;

(put 'document	  'hyperlatex	'hyperlatex-delete-whitespace)
(put 'enddocument 'hyperlatex	'hyperlatex-end-document)

(put 'item	  'hyperlatex	'hyperlatex-item)

(put 'itemize	  'hyperlatex	'hyperlatex-itemize)
(put 'enditemize  'hyperlatex	'hyperlatex-end-itemize)
(put 'itemize	  'hyperlatex-item	'hyperlatex-itemize-item)
(put 'menu	  'hyperlatex	'hyperlatex-menu)
(put 'endmenu	  'hyperlatex	'hyperlatex-end-menu)
(put 'menu	  'hyperlatex-item	'hyperlatex-itemize-item)
(put 'enumerate   'hyperlatex	'hyperlatex-enumerate)
(put 'endenumerate 'hyperlatex	'hyperlatex-end-enumerate)
(put 'enumerate	  'hyperlatex-item	'hyperlatex-itemize-item)
(put 'description 'hyperlatex	'hyperlatex-description)
(put 'enddescription 'hyperlatex	'hyperlatex-end-description)
(put 'description 'hyperlatex-item	'hyperlatex-description-item)


(defun hyperlatex-end-document ()
  ;; it has already been checked whether this terminates the right environment
  (delete-region (point) (point-max))
  (hyperlatex-finish-node))

;;
;; dispatch for \\item
;;

(defun hyperlatex-item ()
  (funcall (get (car hyperlatex-stack) 'hyperlatex-item)))

(defun hyperlatex-generate-list (name)
  (hyperlatex-delete-whitespace)
  (hyperlatex-gen (hyperlatex-get-attributes name) t))

;;;
;;; ITEMIZE
;;;

(defun hyperlatex-itemize ()
  (hyperlatex-generate-list "UL"))

(defun hyperlatex-end-itemize ()
  (hyperlatex-gen "/UL" t "\n"))

(defun hyperlatex-itemize-item ()
  (hyperlatex-gen "LI" t))

;;;
;;; MENU
;;;

(defun hyperlatex-menu ()
  (hyperlatex-generate-list "MENU"))

(defun hyperlatex-end-menu ()
  (hyperlatex-gen "/MENU" t "\n"))

;;;
;;; ENUMERATE
;;;

(defun hyperlatex-enumerate ()
  (hyperlatex-generate-list "OL"))

(defun hyperlatex-end-enumerate ()
  (hyperlatex-gen "/OL" t "\n"))

;;;
;;; DESCRIPTION
;;; 

(defun hyperlatex-description ()
  (hyperlatex-generate-list "DL"))

(defun hyperlatex-end-description ()
  (hyperlatex-gen "/DL" t "\n"))

(defun hyperlatex-description-item ()
  (let ((arg (hyperlatex-parse-optional-argument)))
    (if (null arg)
	(error "Missing argument for \\item in description environment"))
    (hyperlatex-gen "DT" t)
    (hyperlatex-gen "B" t arg)
    (hyperlatex-gen "/B" t "\n")
    (hyperlatex-gen "DD" t))
  (goto-char hyperlatex-command-start))

;;;
;;; ----------------------------------------------------------------------
;;;
;;; Tables and figures
;;;

(put 'tabular	  'hyperlatex 'hyperlatex-format-tabular)
(put 'endtabular  'hyperlatex 'hyperlatex-end-tabular)
(put 'htmltab	  'hyperlatex 'hyperlatex-format-tab)
(put 'hline	  'hyperlatex 'hyperlatex-format-hline)
(put 'multicolumn 'hyperlatex 'hyperlatex-format-multicolumn)

(put 'htmlcaption 'hyperlatex 'hyperlatex-format-htmlcaption)
(put 'multicolumn 'hyperlatex 'hyperlatex-format-multicolumn)

;;;
;;; The tabular environment
;;;

(defun hyperlatex-tabular-posn (string)
  "Check the tabular column descriptor and generate a list of
align tags CENTER, LEFT, RIGHT."
  (if (not (string-match "^[|lrc]+$" string))
      (error "Illegal column descriptor.")
    (let ((i (length string))
	  (result nil))
      (while (> i 0)
	(setq i (1- i))
	(let ((chr (elt string i)))
	  (if (= chr ?|)
	      ()
	    (setq result (cons (cond ((= chr ?c) "CENTER")
				     ((= chr ?l) "LEFT")
				     ((= chr ?r) "RIGHT"))
			       result)))))
      result)))

(defun hyperlatex-format-tabular ()
  (hyperlatex-parse-optional-argument)
  (setq hyperlatex-tabular-column-descr
	(cons (cons 1 (hyperlatex-tabular-posn
		       (hyperlatex-parse-required-argument)))
	      hyperlatex-tabular-column-descr))
  (if (<= hyperlatex-html-level 20)
      (hyperlatex-gen "PRE" t)
    (hyperlatex-gen (hyperlatex-get-attributes "TABLE") t)
    (hyperlatex-gen "TR" t)
    (hyperlatex-gen
     (format "%s COLSPAN=\"1\" ALIGN=\"%s\""
	     (hyperlatex-get-attributes "TD")
	     (car (cdr (car hyperlatex-tabular-column-descr)))) t "\n")))

(defun hyperlatex-end-tabular ()
  (setq hyperlatex-tabular-column-descr
	(cdr hyperlatex-tabular-column-descr))
  (if (<= hyperlatex-html-level 20)
      (hyperlatex-gen "/PRE" t "\n")
    (hyperlatex-gen "/TD" t)
    (hyperlatex-gen "/TR" t)
    (hyperlatex-gen "/TABLE" t "\n")))

(defun hyperlatex-format-tab ()
  (if (hyperlatex-in-stack 'tabular)
      ()
    (error "Used Tab character `&' outside of tabular environment."))
  (if (<= hyperlatex-html-level 20)
      ()
    (hyperlatex-gen "/TD" t)
    (hyperlatex-gen
     (format "%s COLSPAN=\"1\" ALIGN=\"%s\""
	     (hyperlatex-get-attributes "TD")
	     (nth (car (car hyperlatex-tabular-column-descr))
		  (cdr (car hyperlatex-tabular-column-descr)))) t)
    (setcar (car hyperlatex-tabular-column-descr)
	    (1+ (car (car hyperlatex-tabular-column-descr))))))

(defun hyperlatex-format-tab-\\ ()
  (if (<= hyperlatex-html-level 20)
      (if hyperlatex-beginning-new-line
	  (insert "\n"))
    (hyperlatex-gen "/TD" t)
    (hyperlatex-gen "/TR" t "\n")
    (hyperlatex-gen "TR" t)
    (hyperlatex-gen
     (format "%s COLSPAN=\"1\" ALIGN=\"%s\""
	     (hyperlatex-get-attributes "TD")
	     (car (cdr (car hyperlatex-tabular-column-descr)))) t "\n")
    (setcar (car hyperlatex-tabular-column-descr) 1)))

(defun hyperlatex-format-hline ()
  (if (<= hyperlatex-html-level 20)
      (hyperlatex-format-htmlrule)
    ()))

(defun hyperlatex-format-htmlcaption ()
  (let ((caption (hyperlatex-parse-required-argument)))
    (if (<= hyperlatex-html-level 20)
	(error "\\htmlcaption not available in Html2")
      (search-backward "\M-<TR\M->")
      (let ((here (point)))
	(hyperlatex-gen (hyperlatex-get-attributes "CAPTION") t)
	(insert caption)
	(hyperlatex-gen "/CAPTION" t)
	(goto-char here)))))
      
(defun hyperlatex-format-multicolumn ()
  (let ((cols (hyperlatex-parse-required-argument))
	(posn (hyperlatex-tabular-posn (hyperlatex-parse-required-argument)))
	(item (hyperlatex-parse-required-argument))
	(here (point-marker)))
    (if (<= hyperlatex-html-level 20)
	(error "\\multicolumn not available in Html2")
      (re-search-backward "COLSPAN=\"1\" ALIGN=\"[A-Z]+\"")
      (replace-match 
       (format "COLSPAN=\"%s\" ALIGN=\"%s\"" cols (car posn)))
      (goto-char here)
      (insert item)
      (goto-char here)
      (set-marker here nil))))

;;;
;;; ----------------------------------------------------------------------
;;;
;;; Quotations, example's and verbatim environments
;;;

(put '\\	    'hyperlatex	'hyperlatex-format-\\)
(put 'example	    'hyperlatex	'hyperlatex-format-example)
(put 'endexample    'hyperlatex	'hyperlatex-end-recursion)
(put 'blockquote    'hyperlatex	'hyperlatex-format-blockquote)
(put 'endblockquote 'hyperlatex	'hyperlatex-end-blockquote)
(put 'htmlcenter    'hyperlatex	'hyperlatex-format-htmlcenter)
(put 'endhtmlcenter 'hyperlatex	'hyperlatex-end-htmlcenter)
(put 'verbatim	    'hyperlatex	'hyperlatex-format-verbatim)
(put 'endverbatim   'hyperlatex	'hyperlatex-end-verbatim)
(put 'verb	    'hyperlatex	'hyperlatex-format-verb)

(defun hyperlatex-end-recursion ()
  (setq hyperlatex-continue-scan hyperlatex-example-depth))

(defun hyperlatex-format-htmlcenter ()
  (hyperlatex-delete-whitespace)
  (cond ((= hyperlatex-html-level 26)
	 (hyperlatex-gen "BLOCKQUOTE" t)
	 (hyperlatex-gen "CENTER" t))
	(t
	 (hyperlatex-format-blockquote))))

(defun hyperlatex-end-htmlcenter ()
  (hyperlatex-delete-whitespace)
  (cond ((= hyperlatex-html-level 26)
	 (hyperlatex-gen "/CENTER" t)
	 (hyperlatex-gen "/BLOCKQUOTE" t "\n"))
	(t
	 (hyperlatex-end-blockquote))))

(defun hyperlatex-format-\\ ()
  "Insert a <BR> tag, except in example, where it does nothing,
and in tabular, where it does something else."
  (hyperlatex-starred-p)
  (hyperlatex-parse-optional-argument)
  (if hyperlatex-active-space
      ()
    (if (hyperlatex-in-stack 'tabular)
	(hyperlatex-format-tab-\\)
      (hyperlatex-gen "BR"))))
  
(defun hyperlatex-format-example ()
  (hyperlatex-gen "BLOCKQUOTE" t)
  (hyperlatex-gen "PRE" t)
  (let ((hyperlatex-special-chars-regexp "[\\\\{}%]")
	(hyperlatex-example-depth hyperlatex-recursion-depth)
	(hyperlatex-active-space t))
    ;; recursive call returns after processing \end{example}
    (hyperlatex-format-region (point) (point-max)))
  (goto-char hyperlatex-command-start)
  (hyperlatex-delete-whitespace)
  (hyperlatex-gen "/PRE" t)
  (hyperlatex-gen "/BLOCKQUOTE" t "\n"))

(defun hyperlatex-format-blockquote ()
  (hyperlatex-delete-whitespace)
  (hyperlatex-gen "BLOCKQUOTE" t))

(defun hyperlatex-end-blockquote ()
  (hyperlatex-delete-whitespace)
  (hyperlatex-gen "/BLOCKQUOTE" t "\n"))

(defun hyperlatex-format-verb ()
  "Handle the LaTeX \\verb command."
  (hyperlatex-gen "CODE")
  (let ((the-char (following-char))
	(from (point)))
    (delete-char 1)
    (search-forward (char-to-string the-char))
    (delete-char -1))
  (hyperlatex-gen "/CODE"))

(defun hyperlatex-format-verbatim ()
  (hyperlatex-pop-stacks)
  (hyperlatex-gen "PRE" t)
  (let ((beg (point))
	(end (progn (search-forward  "\\end{verbatim}" nil nil)
		    (point))))
    ;; get rid of \\end{verbatim}
    (goto-char end)
    (delete-char -14))
  (hyperlatex-gen "/PRE" t))

(defun hyperlatex-end-verbatim ()
  (error "Nested verbatim environments!"))

;;;
;;; ----------------------------------------------------------------------
;;;
;;; Emphasize, some little macros, and things that are ignored in HTML
;;;

(put 'back	'hyperlatex 'hyperlatex-format-backslash)
(put 'c 	'hyperlatex 'hyperlatex-format-c)
(put 'today	'hyperlatex 'hyperlatex-format-today)

(put 'em	'hyperlatex 'hyperlatex-format-em)
(put 'it	'hyperlatex 'hyperlatex-format-it)
(put 'bf	'hyperlatex 'hyperlatex-format-bf)
(put 'tt	'hyperlatex 'hyperlatex-format-tt)

(put 'normalsize 'hyperlatex 'hyperlatex-format-normalsize)
(put 'large	'hyperlatex 'hyperlatex-format-large)
(put 'Large	'hyperlatex 'hyperlatex-format-Large)
(put 'LARGE	'hyperlatex 'hyperlatex-format-LARGE)
(put 'huge	'hyperlatex 'hyperlatex-format-huge)
(put 'Huge	'hyperlatex 'hyperlatex-format-Huge)
(put 'small	'hyperlatex 'hyperlatex-format-small)
(put 'footnotesize 'hyperlatex 'hyperlatex-format-footnotesize)
(put 'scriptsize 'hyperlatex 'hyperlatex-format-scriptsize)
(put 'tiny	'hyperlatex 'hyperlatex-format-tiny)

(put 'emph	'hyperlatex 'hyperlatex-format-emph)
(put 'textbf	'hyperlatex 'hyperlatex-format-textbf)
(put 'textit	'hyperlatex 'hyperlatex-format-textit)
(put 'texttt	'hyperlatex 'hyperlatex-format-texttt)
(put 'textsc	'hyperlatex 'hyperlatex-format-textsc)
(put 'underline	'hyperlatex 'hyperlatex-format-underline)

(put 'cit	'hyperlatex 'hyperlatex-format-cit)
(put 'code	'hyperlatex 'hyperlatex-format-code)
(put 'kbd	'hyperlatex 'hyperlatex-format-kbd)
(put 'samp	'hyperlatex 'hyperlatex-format-samp)
(put 'strong	'hyperlatex 'hyperlatex-format-strong)
(put 'var	'hyperlatex 'hyperlatex-format-var)
(put 'dfn	'hyperlatex 'hyperlatex-format-dfn)

(put 'protect	'hyperlatex 'hyperlatex-format-ignore)
(put 'noindent	'hyperlatex 'hyperlatex-format-ignore)

(put 'xspace	'hyperlatex 'hyperlatex-format-xspace)

(defun hyperlatex-format-backslash ()
  "replace \\back and \\- by \\"
  (insert "\\"))

(defun hyperlatex-format-today ()
  (let* ((date-string (current-time-string))
	 (month-alist   '(("Jan" . "January") ("Feb" . "February") 
			  ("Mar" . "March") ("Apr" . "April")
			  ("May" . "May") ("Jun" . "June") 
			  ("Jul" . "July") ("Aug" . "August")
			  ("Sep" . "September") ("Oct" . "October")
			  ("Nov" . "November") ("Dec" . "December")))
	 )
    (string-match "\\(...\\) \\(...\\) \\(..\\).*\\(19..\\)"
		  (current-time-string) nil)
    (insert
     (concat (cdr (assoc (substring date-string 
				    (match-beginning 2) (match-end 2))
			 month-alist))
	     " " (substring date-string (match-beginning 3) (match-end 3))
	     ", " (substring date-string (match-beginning 4) (match-end 4))))))

(defun hyperlatex-format-c ()
  (let ((arg (hyperlatex-parse-required-argument t)))
    (if (or (string= arg "c")
	    (string= arg "C"))
	(hyperlatex-gensym (concat arg "cedil"))
      (error "Invalid cedilla accent"))))

;;
;; font changes
;;

(defun hyperlatex-format-bf ()
  (hyperlatex-format-font-1 "B"))

(defun hyperlatex-format-em ()
  (hyperlatex-format-font-1 "EM"))

(defun hyperlatex-format-it ()
  (hyperlatex-format-font-1 "I"))

(defun hyperlatex-format-tt ()
  (hyperlatex-format-font-1 "TT"))

(defun hyperlatex-format-emph ()
  (hyperlatex-format-font "EM"))

(defun hyperlatex-format-underline ()
  (hyperlatex-format-font "U"))

(defun hyperlatex-format-textbf ()
  (hyperlatex-format-font "B"))

(defun hyperlatex-format-textit ()
  (hyperlatex-format-font "I"))

(defun hyperlatex-format-textsc ()
  (insert (upcase (hyperlatex-parse-required-argument)))
  (goto-char hyperlatex-command-start))

(defun hyperlatex-format-texttt ()
  (hyperlatex-format-font "TT"))

(defun hyperlatex-format-cit ()
  (hyperlatex-format-font "CITE"))
(defun hyperlatex-format-code ()
  (hyperlatex-format-font "CODE"))
(defun hyperlatex-format-kbd ()
  (hyperlatex-format-font "KBD"))
(defun hyperlatex-format-samp ()
  (hyperlatex-format-font "SAMP"))
(defun hyperlatex-format-strong ()
  (hyperlatex-format-font "STRONG"))
(defun hyperlatex-format-var ()
  (hyperlatex-format-font "VAR"))
(defun hyperlatex-format-dfn ()
  (hyperlatex-format-font
   (if (> hyperlatex-html-level 20) "DFN" "EM")))

;;
;;

(defun hyperlatex-format-normalsize ()
  (hyperlatex-format-fontsize "+0"))

(defun hyperlatex-format-large ()
  (hyperlatex-format-fontsize "+1"))

(defun hyperlatex-format-Large ()
  (hyperlatex-format-fontsize "+2"))

(defun hyperlatex-format-LARGE ()
  (hyperlatex-format-fontsize "+3"))

(defun hyperlatex-format-huge ()
  (hyperlatex-format-fontsize "+4"))

(defun hyperlatex-format-Huge ()
  (hyperlatex-format-fontsize "+5"))

(defun hyperlatex-format-small ()
  (hyperlatex-format-fontsize "-1"))

(defun hyperlatex-format-footnotesize ()
  (hyperlatex-format-fontsize "-2"))

(defun hyperlatex-format-scriptsize ()
  (hyperlatex-format-fontsize "-3"))

(defun hyperlatex-format-tiny ()
  (hyperlatex-format-fontsize "-4"))

;;
;;

(defun hyperlatex-format-font (font)
  (let ((arg (hyperlatex-parse-required-argument)))
    (hyperlatex-gen font)
    (insert arg)
    (hyperlatex-gen (concat "/" font)))
  (goto-char hyperlatex-command-start))

(defun hyperlatex-format-font-1 (font)
  (hyperlatex-gen font)
  (setq hyperlatex-group-stack
	(cons (concat "\M-</" font "\M->" (car hyperlatex-group-stack))
	      (cdr hyperlatex-group-stack)))
  (goto-char hyperlatex-command-start))

(defun hyperlatex-format-fontsize (size)
  (if (<= hyperlatex-html-level 25)
      ()
    (hyperlatex-gen (format "font size=%s" size))
    (setq hyperlatex-group-stack
	  (cons (concat "\M-</font size=+0\M->" (car hyperlatex-group-stack))
		(cdr hyperlatex-group-stack)))
    (goto-char hyperlatex-command-start)))

(defun hyperlatex-format-xspace ()
  (hyperlatex-delete-whitespace)
  (if (looking-at "[{}/ ~.,:;?')-]")
      ()
    (insert " ")))

;;;
;;; ----------------------------------------------------------------------
;;;
;;; Math mode
;;;

(put 'math	'hyperlatex 'hyperlatex-format-math)
(put (intern "(") 'hyperlatex 'hyperlatex-math-on)
(put (intern ")") 'hyperlatex 'hyperlatex-math-off)
(put 'sqrt	'hyperlatex 'hyperlatex-format-sqrt)

(defun hyperlatex-math-mode ()
  (setq hyperlatex-math-mode (not hyperlatex-math-mode))
  (if hyperlatex-math-mode
      (hyperlatex-gen (if (>= hyperlatex-html-level 30) "MATH" "I"))
    (hyperlatex-gen (if (>= hyperlatex-html-level 30) "/MATH" "/I"))))

(defun hyperlatex-math-on ()
  (if hyperlatex-math-mode
      ()
    (hyperlatex-gen (if (>= hyperlatex-html-level 30) "MATH" "I")))
  (setq hyperlatex-math-mode t))

(defun hyperlatex-math-off ()
  (if hyperlatex-math-mode
      (hyperlatex-gen (if (>= hyperlatex-html-level 30) "/MATH" "/I")))
  (setq hyperlatex-math-mode nil))

(defun hyperlatex-subscript ()
  (hyperlatex-sub-super "sub" "_"))
  
(defun hyperlatex-superscript ()
  (hyperlatex-sub-super "sup" "^"))

(defun hyperlatex-sub-super (where char)
  (if (null hyperlatex-math-mode)
      (insert char)
    (let ((arg (hyperlatex-get-arg-here t))
	  (here (point)))
      (if (or (< hyperlatex-html-level 25)
	      (= hyperlatex-html-level 26))
	  (if (eq (length arg) 1)
	      (insert char arg)
	    (insert char "{" arg "}"))
	(hyperlatex-gen where)
	(insert arg)
	(hyperlatex-gen (concat "/" where))
	(goto-char here)))))

(defun hyperlatex-format-math ()
  "Format \\math{} and \\math[]{}."
  (if hyperlatex-math-mode
      (error "Cannot use \\math in math mode!"))
  (let ((opt (hyperlatex-parse-optional-argument))
	(req (hyperlatex-parse-required-argument))
	(hyperlatex-math-mode t))
    (hyperlatex-gen (if (>= hyperlatex-html-level 30) "MATH" "I"))
    (insert (if opt opt req))
    (hyperlatex-format-region hyperlatex-command-start (point))
    (hyperlatex-gen (if (>= hyperlatex-html-level 30) "/MATH" "/I"))))

(defun hyperlatex-format-sqrt ()
  (let ((opt (hyperlatex-parse-optional-argument))
	(req (hyperlatex-parse-required-argument)))
    (insert
     (if opt
	 (format "\\htmlroot{%s}{%s}" opt req)
       (format "\\htmlsqrt{%s}" req)))
    (goto-char hyperlatex-command-start)))

;;;
;;; ----------------------------------------------------------------------
;;;
;;; Index generation
;;;

(put 'index		'hyperlatex 'hyperlatex-format-index)
(put 'cindex		'hyperlatex 'hyperlatex-format-index)
(put 'htmlprintindex	'hyperlatex 'hyperlatex-format-printindex)

(defun hyperlatex-single-line (str)
  "Replaces newlines in STRING by spaces."
  (if str
      (let ((mystr (copy-sequence str)))
	(while (string-match "\n" mystr)
	  (aset mystr (string-match "\n" mystr) 32))
	mystr)
    nil))
  
(defun hyperlatex-format-index ()
  "Adds an index entry."
  (let ((opt (hyperlatex-single-line (hyperlatex-parse-optional-argument)))
	(arg (hyperlatex-single-line (hyperlatex-parse-required-argument)))
	(label (hyperlatex-drop-label)))
    (if hyperlatex-final-pass
	()
      (setq hyperlatex-index
	    (cons (list arg (if opt opt arg) hyperlatex-node-number label)
		  hyperlatex-index)))))

(defun hyperlatex-format-printindex ()
  (let ((indexelts hyperlatex-index)
	opoint)
    (hyperlatex-gen "MENU" t "\n")
    (setq opoint (point))
    (while indexelts
      ;; put search-key 
      (insert (nth 1 (car indexelts)) " \M-I")
      (hyperlatex-gen (concat "A HREF="
			      (hyperlatex-gen-url (nth 2 (car indexelts))
						  (nth 3 (car indexelts)))
			      ""))
      (insert (car (car indexelts)))
      (hyperlatex-gen "/A" nil "\n")
      (setq indexelts (cdr indexelts)))
    (shell-command-on-region opoint (point) "sort -f" 1)
    (narrow-to-region opoint (point))
    (goto-char (point-min))
    (while (re-search-forward "^.* \M-I" nil t)
      (replace-match "")
      (hyperlatex-gen "LI"))
    (goto-char (point-max))
    (widen)
    (hyperlatex-gen "/MENU" t "\n")
    (goto-char opoint)))
  
;;;
;;; ----------------------------------------------------------------------
;;;
;;; iftex, ifhtml, tex, ifset, ifclear
;;;

(put 'ifset      'hyperlatex 'hyperlatex-if-set)
(put 'endifset   'hyperlatex 'hyperlatex-format-ignore)
(put 'ifclear    'hyperlatex 'hyperlatex-if-clear)
(put 'endifclear 'hyperlatex 'hyperlatex-format-ignore)
(put 'ifhtml     'hyperlatex 'hyperlatex-format-ignore)
(put 'endifhtml  'hyperlatex 'hyperlatex-format-ignore)
(put 'comment    'hyperlatex 'hyperlatex-format-comment)
(put 'iftex      'hyperlatex 'hyperlatex-format-iftex)
(put 'tex        'hyperlatex 'hyperlatex-format-tex)

(defun hyperlatex-ifset-flag ()
  (let* ((arg (hyperlatex-parse-required-argument))
	 (match (assoc arg hyperlatex-new-commands)))
    (if (null match)
	nil
      (let ((expansion (car (cdr (cdr match)))))
	(and (not (string= expansion ""))
	     (not (string= expansion "0")))))))

(defun hyperlatex-if-set ()
  "If set, continue formatting; else do not format region up to \\end{ifset}"
  (if (hyperlatex-ifset-flag)
      ;; flag is set, don't do anything
      () 
    (delete-region hyperlatex-command-start
		   (progn (search-forward "\\end{ifset}") (point)))
    (hyperlatex-delete-whitespace)
    (hyperlatex-pop-stacks)))

(defun hyperlatex-if-clear ()
  "If clear, continue formatting; else do not format region up
 to \\end{ifclear}."
  (if (not (hyperlatex-ifset-flag))
      ;; flag is clear, don't do anything
      () 
    (delete-region hyperlatex-command-start
		   (progn (search-forward "\\end{ifclear}") (point)))
    (hyperlatex-delete-whitespace)
    (hyperlatex-pop-stacks)))

(defun hyperlatex-format-iftex ()
  (delete-region hyperlatex-command-start
		 (progn (search-forward "\\end{iftex}") (point)))
  (hyperlatex-delete-whitespace)
  (hyperlatex-pop-stacks))

(defun hyperlatex-format-tex ()
  (delete-region hyperlatex-command-start
		 (progn (search-forward "\\end{tex}") (point)))
  (hyperlatex-delete-whitespace)
  (hyperlatex-pop-stacks))

(defun hyperlatex-format-comment ()
  (delete-region hyperlatex-command-start
		 (progn (search-forward "\\end{comment}") (point)))
  (hyperlatex-delete-whitespace)
  (hyperlatex-pop-stacks))

;;;
;;; ----------------------------------------------------------------------
;;;
;;; Footnotes
;;;

(put 'footnote		'hyperlatex 'hyperlatex-format-footnote)
(put 'htmlfootnotes	'hyperlatex 'hyperlatex-format-htmlfootnotes)

(defun hyperlatex-format-footnote ()
  (let ((fn (hyperlatex-parse-required-argument)))
    (setq hyperlatex-footnote-number (1+ hyperlatex-footnote-number))
    (setq hyperlatex-footnotes (cons fn hyperlatex-footnotes))
    (insert (format "\\htmlfootnotemark{%d}" hyperlatex-footnote-number))
    (goto-char hyperlatex-command-start)))

(defun hyperlatex-format-htmlfootnotes ()
  (if (null hyperlatex-footnotes)
      ()
    (let ((here (point))
	  (fn (nreverse hyperlatex-footnotes))
	  (num 1))
      (insert "\n\\begin{thefootnotes}\n")
      (while fn
	(insert (format "\\htmlfootnoteitem{%d}{%s}\n" num (car fn)))
	(setq num (1+ num))
	(setq fn (cdr fn)))
      (insert "\\end{thefootnotes}\n")
      (goto-char here))))
      
;;;
;;; ----------------------------------------------------------------------
;;;
;;; Bibliography support, for included .bbl files
;;;

(put 'bibliography	'hyperlatex 'hyperlatex-format-bibliography)
(put 'bibitem		'hyperlatex 'hyperlatex-format-bibitem)
(put 'htmlcite		'hyperlatex 'hyperlatex-format-htmlcite)
(put 'bibliographystyle 'hyperlatex 'hyperlatex-parse-required-argument)

(defun hyperlatex-format-bibliography ()
  (let* ((tex-name (buffer-file-name input-buffer))
	 (base-name (progn
		      (if (string-match "^.*\\(\\.[a-zA-Z0-9]+\\)$" tex-name)
			  (substring tex-name 0 (match-beginning 1))
			tex-name)))
	 (hyperlatex-bbl-filename (concat base-name ".bbl")))
    (hyperlatex-parse-required-argument)
    (if (file-exists-p hyperlatex-bbl-filename)
	(progn
	  (insert-file hyperlatex-bbl-filename)
	  (goto-char hyperlatex-command-start))
      (message "Formatted bibliography file not found: %s"
	       hyperlatex-bbl-filename))))

(defun hyperlatex-format-bibitem ()
  (let ((mnemonic (hyperlatex-parse-optional-argument))
	(label (hyperlatex-parse-required-argument)))
    (setq hyperlatex-bibitem-number (1+ hyperlatex-bibitem-number))
    (if mnemonic
	()
      (setq mnemonic (int-to-string hyperlatex-bibitem-number)))
    (insert (format "\\htmlbibitem{%s}{%s}" mnemonic label))
    (if hyperlatex-final-pass
	()
      (setq hyperlatex-cite-names
	    (cons (cons label mnemonic) hyperlatex-cite-names)))
    (goto-char hyperlatex-command-start)))

(defun hyperlatex-format-htmlcite ()
  (let ((label (hyperlatex-parse-required-argument)))
    (if hyperlatex-final-pass
	(let ((match (assoc label hyperlatex-cite-names)))
	  (if (null match)
	      (if (not noninteractive)
		  (error "Unknown city key %s" label)
		(message "WARNING: Unknown cite key %s" label))
	    (insert "[" (cdr match) "]")
	    (goto-char hyperlatex-command-start))))))

;;;
;;; ----------------------------------------------------------------------
;;;
;;; Html specialties---\html, \htmlsym, bitmaps etc.
;;;

(put 'html		'hyperlatex 'hyperlatex-format-html)
(put 'htmlsym		'hyperlatex 'hyperlatex-format-htmlsym)
(put 'rawhtml		'hyperlatex 'hyperlatex-format-rawhtml)
(put 'thehtmladdress	'hyperlatex 'hyperlatex-format-thehtmladdress)
(put 'thehtmlicons	'hyperlatex 'hyperlatex-format-thehtmlicons)
(put 'htmlrule		'hyperlatex 'hyperlatex-format-htmlrule)
(put 'htmlimage		'hyperlatex 'hyperlatex-format-image)
(put 'gif		'hyperlatex 'hyperlatex-format-gif)

(defun hyperlatex-format-html ()
  (let ((arg (hyperlatex-parse-required-argument)))
    (hyperlatex-gen arg)))

(defun hyperlatex-format-htmlsym ()
  (let ((arg (hyperlatex-parse-required-argument)))
    (hyperlatex-gensym arg)))

(defun hyperlatex-format-rawhtml
  (let ((end (progn (search-forward "\\end{rawhtml}") (match-beginning 0))))
    (replace-match "")
    (goto-char hyperlatex-command-start)
    (while (re-search-forward "[<>&]" end t)
      (replace-match (char-to-string (+ (preceding-char) 128))))
    (goto-char end)
    (hyperlatex-delete-whitespace)
    (hyperlatex-pop-stacks)))

(defun hyperlatex-format-thehtmladdress ()
  (hyperlatex-gen "ADDRESS" t)
  (insert (if hyperlatex-address hyperlatex-address ""))
  (hyperlatex-gen "/ADDRESS" t)
  (goto-char hyperlatex-command-start))

(defun hyperlatex-format-thehtmlicons ()
  (insert hyperlatex-html-icons)
  (goto-char hyperlatex-command-start))

(defun hyperlatex-format-htmlrule ()
  (let ((oarg (hyperlatex-parse-optional-argument))
	(tag (hyperlatex-get-attributes "HR")))
    (if (not (bolp)) (insert "\n"))
    (hyperlatex-gen (if oarg (concat tag " " oarg) tag) t)))

(defvar hyperlatex-html-alignments
  '( ( "t" "ALIGN=TOP"    )
     ( "c" "ALIGN=MIDDLE" )
     ( "b" "ALIGN=BOTTOM" )
     ( "l" "ALIGN=LEFT"   )
     ( "r" "ALIGN=RIGHT"  )
     ))

(defun hyperlatex-format-image ()
  (let* ((opt (hyperlatex-parse-optional-argument))
	 (reqopt (if opt opt ""))
	 (align (assoc reqopt hyperlatex-html-alignments))
	 (tags (concat (hyperlatex-get-attributes "IMG") " "
		       (if align (nth 1 align) reqopt)))
	 (url (hyperlatex-parse-required-argument)))
    (hyperlatex-gen (format "%s SRC=\"%s\"" tags url))
    (goto-char (- (point) (+ 5 (length url))))))

(defun hyperlatex-format-gif ()
  (let* ((opt (hyperlatex-parse-optional-argument))
	 (reqopt (if opt opt "b"))
	 (align (assoc reqopt hyperlatex-html-alignments))
	 (tags (if align (nth 1 align) reqopt))
	 (resolution (hyperlatex-parse-optional-argument))
	 (dpi (hyperlatex-parse-optional-argument))
	 (url (hyperlatex-parse-required-argument)))
    (delete-region hyperlatex-command-start
		   (progn (search-forward "\\end{gif}") (point)))
    (hyperlatex-delete-whitespace)
    (hyperlatex-pop-stacks)
    (hyperlatex-gen (format "IMG SRC=\"%s.gif\" %s" url tags))))

;;;
;;; ----------------------------------------------------------------------
;;;
;;; Extending Hyperlatex
;;;

(defun hyperlatex-find-package-hooks (end)
  "Foreach Latex package PACKAGE, look for hlx-PACKAGE.[el,elc] in the
load-path `hyperlatex-extension-dirs'. Return list of all 
`hyperlatex-PACKAGE-hook' that are bound."
  (let ((packages nil))
    (while (re-search-forward
	    "^[ \t]*\\\\usepackage[^{]*{\\([a-zA-Z0-9]+\\)}" end t)
      (let ((pkg (buffer-substring (match-beginning 1) (match-end 1)))
	    (load-path hyperlatex-extension-dirs))
	(if (memq (intern pkg) hyperlatex-known-packages)
	    ()
	  (message "Checking package %s" pkg)
	  (if (load (concat "hlx-" pkg) t) ;dont report errors
	      (let ((pkg-symbol
		     (intern (concat "hyperlatex-" pkg "-hook"))))
		(if (fboundp pkg-symbol)
		    (progn
		      (message "Found package %s hooks" pkg)
		      (setq packages (cons pkg-symbol packages))))
		(message "Done loading package %s" pkg))
	    (message "Package %s not found" pkg)))))
    packages))

(defun hyperlatex-run-package-hooks (packages)
  "Runs all the hooks in the list PACKAGES."
  (let ((hk packages))
    (while hk
      (funcall (car hk))
      (setq hk (cdr hk)))))

;;;
;;; ----------------------------------------------------------------------
;;;

(defun hyperlatex-insert-hyperlatex ()
  (interactive)
  (insert "hyperlatex-"))
;;; (local-set-key "\C-s\C-h" 'hyperlatex-insert-hyperlatex)

(defun hyperlatex-compile ()
  "Byte compile Hyperlatex. 
Unix usage:
     emacs -batch -no-init-file -no-site-file \
           -l hyperlatex-1.4.el -f hyperlatex-compile."
  (setq byte-compile-verbose nil)
  (if (not noninteractive)
      (error "This command must be used in batch mode."))
  (byte-compile-file "hyperlatex-1.4.el"))

;;;
;;; ----------------------------------------------------------------------
;;;
;;; Local Variables:
;;; update-last-edit-date: t
;;; End:
;;;
