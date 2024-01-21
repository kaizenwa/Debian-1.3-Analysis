;;; Compilation utility for emacs-czech
;;; Copyright (C) 1996, 1997 Milan Zamazal
;;; Version: $Id: compile-czech.el,v 3.4 1997/02/26 20:20:03 pdm Exp $
;;; Optimized for no warnings in GNU Emacs under Debian GNU/Linux.


;;; Setup

(setq load-path (cons "." load-path))

(setq files
      '("czech-misc.el" "czech-convert.el" "czech-sort.el" "czech-message.el"
	"czech.el" "czech-keyboard.el"))

;;; Avoid most warnings

;;; Load files with definitions
(load "gnus")
(load "sort")
(load "mime-setup" t)
(load "bbdb-init" t)
(load "bbdb-com" t)
(setq cz-verbose-level 0)
;;; Bind functions
(defun dummy-0 ())
(defun dummy-1 (x))
(defun dummy-2 (x y))
(defun dummy-3 (x y z))
(setq dummy-list '(dummy-0 dummy-1 dummy-2 dummy-3))
(let ((functions '((sortcar . 2)
		   (next-command-event . 0)
		   (event-key . 1)
		   (specifier-spec-list . 1)
		   (set-specifier . 2)
		   (add-minor-mode . 2)
		   (set-menubar-dirty-flag . 0)
		   (add-submenu . 3)
		   (set-default-font . 1)
		   (this-single-command-keys . 0)
		   (read-event . 0)
		   (event-basic-type . 1)
		   (find-buffer-file-type . 1)
		   (redraw-modeline . 0)
		   )))
  (while functions
    (or (fboundp (car (car functions)))
	(fset (car (car functions)) (nth (cdr (car functions)) dummy-list)))
    (setq functions (cdr functions))))
;;; Bind variables
(let ((variables '(cz-xemacs-minibuffer-hack
		   current-display-table
		   :included
		   current-menubar
		   menu-bar-final-items
		   )))
  (while variables
    (or (boundp (car variables))
	(set (car variables) nil))
    (setq variables (cdr variables))))

;;; Byte compile

(while files
  (byte-compile-file (car files))
  (setq files (cdr files)))

;;; All done

(kill-emacs)

