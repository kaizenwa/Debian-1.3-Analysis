(require 'sgml-mode)

(defvar jfw-sgml-mode-map nil "Keymap for JFW-SGML Mode")


(setq jfw-sgml-mode-map '(keymap
			  (3 keymap (22 . sgml-validate))
			  (47 . sgml-slash) ; not for version 19.31
			  ;(62 . sgml-close-angle)
			  (f12 . hilit-highlight-buffer)))

; JFW-SGML Mode
(defun jfw-sgml-mode ()
  "Major-Mode for editing sgml-source using the jfw-dtd's"
  (interactive)
  (indented-text-mode)
  (outline-minor-mode t)
  (setq local-abbrev-table text-mode-abbrev-table)
  (setq mode-name "JFWX")
  (setq major-mode 'jfw-sgml-mode)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start
	"^[ \t\n]\\|\\(</?\\([A-Za-z]\\([-.A-Za-z0-9= \t\n]\\|\"[^\"]*\"\\|'[^']*'\\)*\\)?>$\\)")
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate
	"^[ \t\n]*$\\|^</?\\([A-Za-z]\\([-.A-Za-z0-9= \t\n]\\|\"[^\"]*\"\\|'[^']*'\\)*\\)?>$")
  (make-local-variable 'comment-start)
  (setq comment-start "<!-- ")
  (make-local-variable 'comment-end)
  (setq comment-end " -->")
;  (make-local-variable 'comment-indent-function)
;  (setq comment-indent-function 'sgml-comment-indent)
  (make-local-variable 'comment-start-skip)
  ;; This will allow existing comments within declarations to be
  ;; recognized. I'ts very undesirable as the coomon use of "--" within
  ;; text is a sligtly longer dash
  ;(setq comment-start-skip "--[ \t]*")
  (make-local-variable 'outline-regexp)
  (setq outline-regexp "\\(<doc\\)\\|\\(<boo\\)\\|\\(<sect\\)\\|\\(<sect1\\)\\|\\(<sect2[ >]\\)\\|\\(<!--\\)")

  (use-local-map jfw-sgml-mode-map)
) 

;
; jfw-sgml-mode hilit
;

(if window-system
    (progn
      (require 'hilit19)
      (hilit-set-mode-patterns
       'jfw-sgml-mode
       '(("<!--" "-->" comment)
	 ("<![Ee]ntity" ">" include)
	 ("<verb>" "</verb>" string)
	 ("<ref" "/.*/" glob-struct)
	 ("&[a-zA-Z]+[; ]" nil glob-struct)
	 ("<[a-zA-Z]+/" ".*/" include)
	 ("<.*>" nil include)
	 ("</[a-zA-Z]+" nil include)
	 ("^<chapt>\\|^<sect>\\|^<sect1>\\|^<sect2>\\|^<appendix>"
	  ".*$" defun))
       )))
