;;;; YORICK.EL
;;;; $Id$

;;;; Note- use c-mode to edit Yorick source code:
;;;; (setq auto-mode-alist (append '(("\\.i$" . c-mode)) auto-mode-alist))

;;;; Detects parser and runtime error messages and automatically pops
;;;; up the offending file positioned to the offending line.
;;;; Bind the function yorick-pop to a key in shell mode 
;;;; (or a comint derivative mode specialized for Yorick):
;;;; (setq shell-load-hook
;;;;      '((lambda ()
;;;;	      (load "alt-shell" t t)
;;;;	      (define-key shell-mode-map "\C-c\C-y" 'yorick-pop)
;;;;          (define-key shell-mode-map "\C-c\C-l" 'lasnex-pop))))

(require 'shell)

;; ------------------------------------------------------------------------

(defvar popfile-regexp ""
  "Regexp to recognize line number and file for popfile.
popfile-line and popfile-file are the subexpression numbers for
the line number and file name, respectively.")

(defvar popfile-line 1
  "Subexpression number of line number in popfile-regexp.")

(defvar popfile-file 2
  "Subexpression number of file in popfile-regexp.")

(defvar popfile-expander 'popfile-default-expander
  "Nil or a function which takes a filename argument and expands it
to a full pathname for use by the popfile function.")

(defvar popfile-expander-context nil
  "Passed to popfile-expander after the name to be expanded -- typically
a list of directory names to be searched.")

(defun popfile-expand (name context)
  "Expands NAME by popfile-expander, or just returns name.  The CONTEXT
argument is typically nil, but might be a list of directories to search."
  (if popfile-expander (funcall popfile-expander name context) name))

(defun popfile ()
  "Pop up file mentioned in most recent occurrence of popfile-regexp
and put point at the specified line number.  The regexp can be on the
current line, or any previous line."
  (let ((p (point)))
    (end-of-line)
    (unwind-protect
	(re-search-backward popfile-regexp)
      (goto-char p))
    (let ((line (buffer-substring (match-beginning popfile-line)
				  (match-end popfile-line)))
	  (file (popfile-expand
		 (buffer-substring (match-beginning popfile-file)
				   (match-end popfile-file))
		 popfile-expander-context))
	  (pop-up-windows t)
	  window)
      (pop-to-buffer (let ((find-file-existing-other-name t))
		       (find-file-noselect file)))
      (goto-line (string-to-int line)))))

(defun popfile-default-expander (name context)
  "The default popfile-expander expands NAME according to the CONTEXT.  If
CONTEXT is nil, NAME is returned.  Otherwise, CONTEXT is a list of directory
pathnames; these are searched until a file of the given NAME is found."
  (if (not context)
      (if (file-readable-p name) name)
    (popfile-locate name context)))

(defun popfile-locate (name context)
  (while (and context
	      (not (file-readable-p (concat (car context) name))))
    (setq context (cdr context)))
  (if context (concat (car context) name)))

;; ------------------------------------------------------------------------

(defun yorick-pop ()
  "*Pop up the file for the most recent Yorick error message."
  (interactive)
  (let ((popfile-regexp "LINE: *\\([0-9]+\\) *FILE: *\\([^ \n]+\\)")
	(popfile-line 1)
	(popfile-file 2)
	popfile-expander)
    (popfile)))

;; ------------------------------------------------------------------------

(provide 'yorick)
