;;;; sgmls.el --- LISP front end for SGMLS and a postprocessor.

;;; Copyright (C) 1994 David Megginson

;;; Author: David Megginson <dmeggins@aix1.uottawa.ca>

;;; Like Gnu Emacs, this program is free software; you can redistribute
;;; it and/or modify it under the terms of the GNU General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with Gnu Emacs, which is required to run it; if not, write to
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.


;;;; Commentary

;;; A LISP front end for the SGMLS parser and any postprocessors.

;;; This package provides a convenient front end for the free SGMLS
;;; parser and any postprocessors (such as perl scripts or the simple
;;; SGMLSASP program supplied with SGMLS).  With a single command,
;;; emacs will cause an SGML source file to be parsed and processed in
;;; the background, placing the final output in a separate buffer.


;;;; USAGE

;;; Place this file somewhere in your load path, byte-compile it if
;;; you wish (it is not speed-critical), and add one of the following
;;; sequences to your default.el or .emacs file:
;;;
;;;   (autoload 'sgmls "sgmls" nil t)
;;;   (autoload 'sgmls-edit-spec "sgmls" nil t)
;;;   (autoload 'sgmls-start-process "sgmls")
;;;
;;; __OR__
;;;
;;;   (require 'sgmls)
;;;
;;; The first will load sgmls.el only upon demand, while the second
;;; will load it unconditionally whenever emacs starts.  You may then
;;; run the package simply by typing "M-x sgmls" (you may, of course,
;;; prefer to bind it to a keystroke).


;;;; CONFIGURATION

;;; This package is highly configurable, but its default setup should
;;; work well for the average user.  All of the options are documented
;;; in the next section under "User Options" -- the most important are
;;; `sgmls-spec', which contains the name of the file containing a
;;; specification for the postprocessor; `sgmls-spec-directory', which
;;; contains a default directory for the specs; `sgmls-postprocessor',
;;; which contains the name of the postprocessor program ("sgmlsasp"
;;; by default); and `sgmls-decl' (or `sgml-declaration' from
;;; psgml.el), which contains the name of the file containing an SGML
;;; declaration.  All of these options are buffer-local, and may be
;;; set in the "Local Variables:" section of a file.
;;;
;;; If you call `sgmls' with prefix argument, you will be given an
;;; opportunity to review and override all of the default settings.


;;;; KNOWN BUGS

;;; Because of the way that Emacs and this package handles the
;;; processes, errors are not handled cleanly.  The exit status
;;; displayed when the process terminates applies only to the
;;; postprocessor; the exit status of the SGMLS parser itself, which
;;; is the first element in the pipeline, is not indicated unless the
;;; postprocessor is smart enough to notice that something is wrong.
;;; Furthermore, since Emacs always mixes the stdout and stderr of its
;;; subprocesses together, any error messages will simply be embedded
;;; in the middle of the output buffer.



;;;; User Options.

(defvar sgmls-parser-command nil
  "*Command for running SGMLS.")
(make-variable-buffer-local 'sgmls-parser-command)

(defvar sgmls-postprocessor-command nil
  "*Command for running SGMLS postprocessor")
(make-variable-buffer-local 'sgmls-postprocessor-command)

(defvar sgmls-parser "sgmls"
  "*Name of SGMLS program on local system (buffer-local).")
(make-variable-buffer-local 'sgmls-parser)

(defvar sgmls-parser-options ""
  "*Options for SGMLS parser.")
(make-variable-buffer-local 'sgmls-parser-options)

(defvar sgmls-declaration nil
  "*Default SGML declaration (buffer-local).
If this variable is not set, the variable `sgml-declaration' (from
psgml.el) will be tried.")
(make-variable-buffer-local 'sgmls-declaration)

(defvar sgmls-source-file nil
  "*Default SGML source file (buffer-local).
By default, this will usually be the file associated with the current
buffer, but it can also be generated from the `sgml-parent-document'
variable (from psgml.el), or set manually.")
(make-variable-buffer-local 'sgmls-source-file)

(defvar sgmls-postprocessor "sgmlsasp"
  "*Name of SGMLS postprocessor on local system (buffer-local).")
(make-variable-buffer-local 'sgmls-postprocessor)

(defvar sgmls-postprocessor-options ""
  "*Options for SGMLS postprocessor.")
(make-variable-buffer-local 'sgmls-postprocessor-options)

(defvar sgmls-spec nil
  "*Default specification file or argument for postprocessor (buffer-local).
This may be specified relative to `sgmls-spec-directory'.")
(make-variable-buffer-local 'sgmls-spec)

(defvar sgmls-spec-directory nil
  "*Default directory for sgmls-spec.")
(make-variable-buffer-local 'sgmls-spec-directory)

(defvar sgmls-output-file nil
  "*Default file name for sgmls output file (buffer-local).
This may be specified relative to `sgmls-output-directory', and may be
generated automatically from the source file using `sgmls-output-file-ext'.")
(make-variable-buffer-local 'sgmls-output-file)

(defvar sgmls-output-file-ext nil
  "*Default extension for sgmls output file (buffer-local).
If `sgmls-output-file' is not set, it will be generated from the source
file using the extension provided here.")
(make-variable-buffer-local 'sgmls-output-file-ext)

(defvar before-sgmls-hooks nil
  "*Hooks to run on the output buffer before SGMLS starts (buffer-local).")
(make-variable-buffer-local 'before-sgmls-hooks)

(defvar after-sgmls-hooks nil
  "*Hooks to run on the output buffer after SGMLS finishes (buffer-local).")
(make-variable-buffer-local 'after-sgmls-hooks)


;;; Internal variables.

(defvar sgmls::parser-error-file nil)
(make-variable-buffer-local 'sgmls::parser-error-file)

(defvar sgmls::postprocessor-error-file)
(make-variable-buffer-local 'sgmls::postprocessor-error-file)



;;;; User-level commands and functions.

;;
;; Interactive function to set up command line and run sgmls.
;;
(defun sgmls (flag)
  "Run sgmls and a postprocessor, putting the output into a file buffer.
With a prefix argument, allow the caller to review and override any
default values.  The variables `sgmls-parser' and
`sgmls-postprocessor' contain the actual names of the programs which
will be run (in a shell command sent to `shell-file-name')."

  (interactive "P")

  (if (or flag (not sgmls-parser-command))
      (setq sgmls-parser-command
	    (format "%s %s %s %s"
		    sgmls-parser
		    sgmls-parser-options
		    (sgmls::generate-declaration)
		    (sgmls::generate-source-file))))
  (setq sgmls-parser-command
	(read-string "Parser command: " 
		     (cons sgmls-parser-command 1)))

  (if (or flag (not sgmls-postprocessor-command))
      (setq sgmls-postprocessor-command
	    (format "%s %s %s"
		    sgmls-postprocessor
		    sgmls-postprocessor-options
		    (sgmls::generate-spec flag))))
  (setq sgmls-postprocessor-command
	(read-string "Postprocessor command: " 
		     (cons sgmls-postprocessor-command 1)))

  (sgmls-start-process
   (format "%s 2>%s | %s 2>%s"
	   sgmls-parser-command
	   (sgmls::generate-parser-error-file)
	   sgmls-postprocessor-command
	   (sgmls::generate-postprocessor-error-file))
   (find-file-noselect (sgmls::generate-output-file flag))))


;;
;; Edit the spec associated with a buffer.
;;
(defun sgmls-edit-spec ()
  "Edit the SGMLS spec associated with a buffer.
If the local variable `sgmls-spec' is set, the file will be loaded with
`find-file-other-window'; otherwise, an error will be signaled."
  (interactive)
  (if sgmls-spec
      (find-file-other-window
       (expand-file-name sgmls-spec sgmls-spec-directory))
    (error "No spec is currently assigned (see `sgmls-spec').")))
     

;;
;; Actually run sgmls and the sgmls post-processor.
;;
(defun sgmls-start-process (command buffer)
  "Run an SGMLS command, placing its output into the given buffer.
The command should be a string which will be passed to the shell."
  (save-some-buffers)
  (let ((old-buffer (current-buffer))
	(parser-error-file sgmls::parser-error-file)
	(postprocessor-error-file sgmls::postprocessor-error-file)
	(before-hooks before-sgmls-hooks)
	(after-hooks after-sgmls-hooks)
	proc)
    (set-buffer buffer)
    (display-buffer buffer)
    (setq sgmls::parser-error-file parser-error-file)
    (setq sgmls::postprocessor-error-file postprocessor-error-file)
    (setq before-sgmls-hooks before-hooks)
    (setq after-sgmls-hooks after-hooks)
    (if (and (> (point-max) (point-min))
	     (yes-or-no-p 
	      (format "Discard current contents of buffer %s? "
		      (buffer-name buffer))))
	(erase-buffer))
    (goto-char (point-max))
    (run-hooks 'before-sgmls-hooks)
    (set-buffer old-buffer)
    (setq proc (start-process "sgmls" 
			      buffer 
			      "nice"
			      shell-file-name 
			      "-c" 
			      command))
    (set-process-sentinel proc (function sgmls::process-sentinel))
    (message "Converting from %s to %s (process: %s)..."
	     (file-name-nondirectory sgmls-source-file)
	     (file-name-nondirectory sgmls-output-file)
	     (process-name proc))
    proc))



;;;; Internal functions.

;;
;; Sentinel for end of program run.
;;
(defun sgmls::process-sentinel (proc message)
  (let ((old-buffer (current-buffer))
	(stat (process-status proc))
	msg)
    (unwind-protect
	(progn
	  (set-buffer (process-buffer proc))
	  (cond ((eq stat 'exit)
		 (setq msg
		       (format 
			"SGMLS: process \"%s\" finished with status %d." 
			(process-name proc)
			(process-exit-status proc)))
		 (goto-char (point-min))
		 (run-hooks 'after-sgmls-hooks)
		 (display-buffer (process-buffer proc)))
		((or (eq stat 'signal) (eq stat 'closed))
		 (error "SGMLS: %S %s." proc message)))
	  (sgmls::check-error-files proc
				    sgmls::parser-error-file
				    sgmls::postprocessor-error-file))
    (delete-file sgmls::parser-error-file)
    (delete-file sgmls::postprocessor-error-file)
    (set-buffer old-buffer)
    (if msg (message msg)))))

;;
;; Check whether there is anything in the error files.
;;
(defun sgmls::check-error-files (proc parser-file postprocessor-file)
  (let ((old-buffer (current-buffer))
	(parser-buffer 
	 (get-buffer-create (concat "**"
				    (buffer-name
				     (process-buffer proc))
				    ": parser errors**")))
	(postprocessor-buffer
	 (get-buffer-create (concat "**"
				    (buffer-name
				     (process-buffer proc))
				    ": postprocessor errors**")))
	window)
    (set-buffer parser-buffer)
    (erase-buffer)
    (insert-file parser-file)
    (if (> (point-max) (point-min))
	(setq window (display-buffer parser-buffer))
      (kill-buffer parser-buffer))
    (if window
	(set-window-dedicated-p window t))
    (set-buffer postprocessor-buffer)
    (erase-buffer)
    (insert-file postprocessor-file)
    (if (> (point-max) (point-min))
	(display-buffer postprocessor-buffer)
      (kill-buffer postprocessor-buffer))
    (set-buffer old-buffer)
    (cond (window
	   (set-window-dedicated-p window nil)
	   (balance-windows)))))

;;
;; Generate a declaration to use for sgmls.  By default, return the
;; empty string unless `sgmls-decl' or `sgml-declaration' is set to
;; something.  Flag currently has no effect.
;;
(defun sgmls::generate-declaration ()
  (cond (sgmls-declaration)
	((and (boundp 'sgml-declaration) sgml-declaration) sgml-declaration)
	(t "")))

;;
;; Return the name of a source file to use.  Will try
;; `sgml-parent-document' (from psgml.el) first, then will look for
;; the buffer's file name, then will prompt only if all else fails.
;;
(defun sgmls::generate-source-file ()
  (cond ((boundp 'sgmls-parent-document)
	 (if (consp sgml-parent-document)
	     (setq sgmls-source-file (car sgml-parent-document))
	   (setq sgmls-source-file sgml-parent-document)))
	(t (setq sgmls-source-file 
		 (file-name-nondirectory (buffer-file-name)))))
  sgmls-source-file)

;;
;; Return the name of a spec to use.
;;
(defun sgmls::generate-spec (flag)
  (let ((buffer-file-name nil))
    (if sgmls-spec-directory
	(setq sgmls-spec-directory 
	      (file-name-as-directory sgmls-spec-directory)))
    (if (or flag (null sgmls-spec))
	(setq sgmls-spec
	      (read-file-name 
	       "SGMLS spec: "
	       (if sgmls-spec
		   (file-name-directory sgmls-spec)
		 sgmls-spec-directory)
	       nil
	       1
	       (if sgmls-spec
		   (file-name-nondirectory sgmls-spec)))))
    (if (and (file-readable-p sgmls-spec)
	     (not (file-directory-p sgmls-spec)))
	sgmls-spec
      (if (and (file-readable-p 
		(expand-file-name sgmls-spec sgmls-spec-directory))
	       (not (file-directory-p
		     (expand-file-name sgmls-spec sgmls-spec-directory))))
	  (setq sgmls-spec (expand-file-name sgmls-spec sgmls-spec-directory))
	(error "SGMLS spec \"%s\" is not readable or is a directory." 
	       sgmls-spec)))))

;;
;; Return the name of a temporary file to use for recording errors
;; from the parser or the postprocessor.
;;
(defun sgmls::generate-parser-error-file ()
  (setq sgmls::parser-error-file 
	(make-temp-name "/tmp/sgmlspa")))

;;
;; Return the name of a file for SGMLS postprocessor output.
;;
(defun sgmls::generate-output-file (flag)

  ;; Try to set up default values.
  (if (and (not sgmls-output-file)
	   sgmls-output-file-ext 
	   (buffer-file-name)
	   (or (string-match "^\\(.*\\)\\(\\.[^.]*\\)$" (buffer-file-name))
	       (string-match "^\\(.+\\)$" (buffer-file-name))))
      (setq sgmls-output-file
	    (expand-file-name
	     (concat (substring (buffer-file-name)
				(match-beginning 1)
				(match-end 1))
		     "."
		     sgmls-output-file-ext))))

  ;; Prompt if necessary.
  (setq sgmls-output-file
	(read-file-name "SGMLS output file: "
			nil
			sgmls-output-file
			nil
			(if sgmls-output-file
			    (file-name-nondirectory 
			     sgmls-output-file))))

  (if (string= (expand-file-name sgmls-source-file)
	       (expand-file-name sgmls-output-file))
      (progn
	(setq sgmls-output-file nil)
	(error "SGMLS: source file and output file are the same.")))

  sgmls-output-file)

;;
;; Generate the name of a temporary file to use for postprocessor errors.
;;
(defun sgmls::generate-postprocessor-error-file ()
  (setq sgmls::postprocessor-error-file 
	(make-temp-name (concat "/tmp/sgmlspp"))))

(provide 'sgmls)
