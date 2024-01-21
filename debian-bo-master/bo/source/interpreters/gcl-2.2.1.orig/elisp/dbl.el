;; Run gcl under Emacs
;; Author: W. Schelter, University of Texas
;;     wfs@rascal.ics.utexas.edu

;; This file is part of GNU Emacs.
;; Copyright (C) 1988 Free Software Foundation, Inc.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
;; to anyone for the consequences of using it or for whether it serves
;; any particular purpose or works at all, unless he says so in writing.
;; Refer to the GNU Emacs General Public License for full details.

;; Everyone is granted permission to copy, modify and redistribute GNU
;; Emacs, but only under the conditions described in the GNU Emacs
;; General Public License.  A copy of this license is supposed to have
;; been given to you along with GNU Emacs so you can know your rights and
;; responsibilities.  It should be in a file named COPYING.  Among other
;; things, the copyright notice and this notice must be preserved on all
;; copies.

;; Description of DBL interface:

;; A facility is provided for the simultaneous display of the source code
;; in one window, while using dbl to step through a function in the
;; other.  A small arrow in the source window, indicates the current
;; line.

;; Starting up:

;; In order to use this facility, invoke the command DBL to obtain a
;; shell window with the appropriate command bindings.  You will be asked
;; for the name of a file to run.  Dbl will be invoked on this file, in a
;; window named *dbl-foo* if the file is foo.

;; M-s steps by one line, and redisplays the source file and line.

;; You may easily create additional commands and bindings to interact
;; with the display.  For example to put the dbl command next on \M-n
;; (def-dbl next "\M-n")

;; This causes the emacs command dbl-next to be defined, and runs
;; dbl-display-frame after the command.

;; dbl-display-frame is the basic display function.  It tries to display
;; in the other window, the file and line corresponding to the current
;; position in the dbl window.  For example after a dbl-step, it would
;; display the line corresponding to the position for the last step.  Or
;; if you have done a backtrace in the dbl buffer, and move the cursor
;; into one of the frames, it would display the position corresponding to
;; that frame.

;; dbl-display-frame is invoked automatically when a filename-and-line-number
;; appears in the output.


(require 'sshell)
(require 'gcl)

(defvar dbl-prompt-pattern "\\(^(.*gdb[+]*) *\\)\\|\\(^[^#$%>]*[#$%>]+ *\\)"
  "A regexp to recognize the prompt for dbl or dbl+.") 

(defvar dbl-filter-accumulator nil)
(defvar dbl-mode-map nil
  "Keymap for dbl-mode.")

(if dbl-mode-map
   nil
  (setq dbl-mode-map (copy-keymap sshell-mode-map))
  (define-key dbl-mode-map "\C-cl" 'dbl-find-line)
  )

(define-key ctl-x-map " " 'dbl-break)
;(define-key ctl-x-map "&" 'send-dbl-command)

;;Of course you may use `def-dbl' with any other dbl command, including
;;user defined ones.   

(defmacro def-dbl (name key &optional doc)
  (let* ((fun (intern (format "dbl-%s" name)))
	 (cstr (list 'if '(not (= 1 arg))
		     (list 'format "%s %s" name 'arg)
		     name)))
    (list 'progn
 	  (list 'defun fun '(arg)
		(or doc "")
		'(interactive "p")
		(list 'dbl-call cstr))
	  (list 'define-key 'dbl-mode-map key  (list 'quote fun)))))

(def-dbl ":step"   "\M-s" "Step one source line with display")
(def-dbl ":step"  "\M-i" "Step one instruction with display")
(def-dbl ":next"   "\M-n" "Step one source line (skip functions)")
(def-dbl ":r"   "\M-c" "Continue with display")

(def-dbl "finish" "\C-c\C-f" "Finish executing current function")
(def-dbl ":up"     "\M-u"   "Go up N stack frames (numeric arg) with display")
(def-dbl ":down"   "\M-d"   "Go down N stack frames (numeric arg) with display")


(defun dbl-mode ()
  "Major mode for interacting with an inferior Dbl process.
The following commands are available:

\\{dbl-mode-map}

\\[dbl-display-frame] displays in the other window
the last line referred to in the dbl buffer.

\\[dbl-:step] and \\[dbl-:next] in the dbl window,
call dbl to step and next and then update the other window
with the current file and position.

If you are in a source file, you may select a point to break
at, by doing \\[dbl-break].

Commands:
Many commands are inherited from shell mode. 
Additionally we have:

\\[dbl-display-frame] display frames file in other window
\\[dbl-:step] advance one line in program
\\[dbl-:next] advance one line in program (skip over calls).
\\[send-dbl-command] used for special printing of an arg at the current point.
C-x SPACE sets break point at current line."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'dbl-mode)
  (setq mode-name "Inferior Dbl")
  (setq mode-line-process '(": %s"))
  (use-local-map dbl-mode-map)
  (make-local-variable 'last-input-start)
  (setq last-input-start (make-marker))
  (make-local-variable 'last-input-end)
  (setq last-input-end (make-marker))
  (make-local-variable 'dbl-last-frame)
  (setq dbl-last-frame nil)
  (make-local-variable 'dbl-last-frame-displayed-p)
  (setq dbl-last-frame-displayed-p t)
  (make-local-variable 'dbl-delete-prompt-marker)
  (setq dbl-delete-prompt-marker nil)
  (make-local-variable 'dbl-filter-accumulator)
  (setq dbl-filter-accumulator nil)
  (make-local-variable 'shell-prompt-pattern)
  (setq shell-prompt-pattern dbl-prompt-pattern)
  (run-hooks 'sshell-mode-hook 'dbl-mode-hook))

(defvar current-dbl-buffer nil)

(defvar dbl-command-name (if (file-exists-p "/bin/csh") "/bin/csh"
			   "/bin/sh")
  "Pathname for executing dbl.")


(defun dbl (arg)
 
 "Makes a dbl buffer, suitable for running an inferior
  gcl.  You are prompted for a name for the buffer.  After the shell
  starts you should start up your lisp program (eg gcl).  The bufferd
  has special keybindings for stepping and viewing sources.  Enter the
  debug loop with (si::dbl) or :dbl in a debug loop.  "

  (interactive "sName: ")
  (let ((file arg) tem
	(dir default-directory))
    (switch-to-buffer (concat "*dbl-" file "*"))
    (setq default-directory
	  (if (and (setq tem (file-name-directory file))
		   (file-directory-p  tem))
	      tem    dir))
    (or (bolp) (newline))
    (insert "Current directory is " default-directory "\n")
    (make-sshell (concat "dbl-" file) dbl-command-name nil )
    (dbl-mode)
    (set-process-filter (get-buffer-process (current-buffer)) 'dbl-filter)
    (set-process-sentinel (get-buffer-process (current-buffer)) 'dbl-sentinel)
    (dbl-set-buffer)))

(defun dbl-set-buffer ()
  (cond ((eq major-mode 'dbl-mode)
	(setq current-dbl-buffer (current-buffer)))))

;; This function is responsible for inserting output from DBL
;; into the buffer.
;; Aside from inserting the text, it notices and deletes
;; each filename-and-line-number;
;; that DBL prints to identify the selected frame.
;; It records the filename and line number, and maybe displays that file.
(defun dbl-filter (proc string)
  (let ((inhibit-quit t))
    (if dbl-filter-accumulator
	(dbl-filter-accumulate-marker proc
				      (concat dbl-filter-accumulator string))
	(dbl-filter-scan-input proc string))))

(defun dbl-filter-accumulate-marker (proc string)
  (setq dbl-filter-accumulator nil)
  (if (> (length string) 1)
      (if (= (aref string 1) ?\032)
	  (let ((end (string-match "\n" string)))
	    (if end
		(progn
		  (let* ((first-colon (string-match ":" string 2))
			 (second-colon
			  (string-match ":" string (1+ first-colon))))
		    (setq dbl-last-frame
			  (cons (substring string 2 first-colon)
				(string-to-int
				 (substring string (1+ first-colon)
					    second-colon)))))
		  (setq dbl-last-frame-displayed-p nil)
		  (dbl-filter-scan-input proc
					 (substring string (1+ end))))
	      (setq dbl-filter-accumulator string)))
	(dbl-filter-insert proc "\032")
	(dbl-filter-scan-input proc (substring string 1)))
    (setq dbl-filter-accumulator string)))

(defun dbl-filter-scan-input (proc string)
  (if (equal string "")
      (setq dbl-filter-accumulator nil)
      (let ((start (string-match "\032" string)))
	(if start
	    (progn (dbl-filter-insert proc (substring string 0 start))
		   (dbl-filter-accumulate-marker proc
						 (substring string start)))
	    (dbl-filter-insert proc string)))))

(defun dbl-filter-insert (proc string)
  (let ((moving (= (point) (process-mark proc)))
	(output-after-point (< (point) (process-mark proc)))
	(old-buffer (current-buffer))
	start)
    (set-buffer (process-buffer proc))
    (unwind-protect
	(save-excursion
	  ;; Insert the text, moving the process-marker.
	  (goto-char (process-mark proc))
	  (setq start (point))
	  (insert string)
	  (set-marker (process-mark proc) (point))
	  (dbl-maybe-delete-prompt)
	  ;; Check for a filename-and-line number.
	  (dbl-display-frame
	   ;; Don't display the specified file
	   ;; unless (1) point is at or after the position where output appears
	   ;; and (2) this buffer is on the screen.
	   (or output-after-point
	       (not (get-buffer-window (current-buffer))))
	   ;; Display a file only when a new filename-and-line-number appears.
	   t)
	  (goto-char (point-max))

	  )
      (set-buffer old-buffer))
    (if moving (goto-char (process-mark proc)))))
(setq me nil)
(defun dbl-sentinel (proc msg)
  (cond ((null (buffer-name (process-buffer proc)))
	 ;; buffer killed
	 ;; Stop displaying an arrow in a source file.
	 (setq overlay-arrow-position nil)
	 (set-process-buffer proc nil))
	((memq (process-status proc) '(signal exit))
	 ;; Stop displaying an arrow in a source file.
	 (setq overlay-arrow-position nil)
	 ;; Fix the mode line.
	 (setq mode-line-process
	       (concat ": "
		       (symbol-name (process-status proc))))
	 (let* ((obuf (current-buffer)))
	   ;; save-excursion isn't the right thing if
	   ;;  process-buffer is current-buffer
	   (unwind-protect
	       (progn
		 ;; Write something in *compilation* and hack its mode line,
		 (set-buffer (process-buffer proc))
		 ;; Force mode line redisplay soon
		 (set-buffer-modified-p (buffer-modified-p))
		 (if (eobp)
		     (insert ?\n mode-name " " msg)
		   (save-excursion
		     (goto-char (point-max))
		     (insert ?\n mode-name " " msg)))
		 ;; If buffer and mode line will show that the process
		 ;; is dead, we can delete it now.  Otherwise it
		 ;; will stay around until M-x list-processes.
		 (delete-process proc))
	     ;; Restore old buffer, but don't restore old point
	     ;; if obuf is the dbl buffer.
	     (set-buffer obuf))))))


(defun dbl-refresh ()
  "Fix up a possibly garbled display, and redraw the arrow."
  (interactive)
  (redraw-display)
  (dbl-display-frame))

(defun dbl-display-frame (&optional nodisplay noauto)
  "Find, obey and delete the last filename-and-line marker from DBL.
The marker looks like \\032\\032FILENAME:LINE:CHARPOS\\n.
Obeying it means displaying in another window the specified file and line."
  (interactive)
  (dbl-set-buffer)
  (and dbl-last-frame (not nodisplay)
       (or (not dbl-last-frame-displayed-p) (not noauto))
       (progn (dbl-display-line (car dbl-last-frame) (cdr dbl-last-frame))
	      (setq dbl-last-frame-displayed-p t))))

	     
;; Make sure the file named TRUE-FILE is in a buffer that appears on the screen
;; and that its line LINE is visible.
;; Put the overlay-arrow on the line LINE in that buffer.



(defun dbl-find-file (file)
  (cond ((file-exists-p file)
	 (find-file-noselect file))
	((get-buffer file))
	(t (find-file-noselect file))))

(defvar dbl-dirs  nil)

(defun search-path (file dirs)
  (let ((paths (symbol-value dirs))
	true-file)
    (cond ((file-exists-p file) (setq true-file file))
	  (t
	   (while paths
	     (let ((tem (expand-file-name file (or (car paths) default-directory))))
	       (if  (file-exists-p tem) (setq true-file tem))
	       (setq paths (cdr paths))))))
   
   (cond (true-file)
	 (t (setq paths (symbol-value dirs))
	  (set dirs
		  (append paths
			  (list (file-name-directory
				 (read-file-name
				  (format "%s = %s, add path :" dirs paths))))))
	     (search-path file dirs)))))

(defun dbl-find-line ()
  (interactive)
  (save-excursion
    (end-of-line)
    (cond ((re-search-backward "[a-zA-Z] line [0-9]+)" 300 nil)
	   (let (at)  
	     (setq at (point))
	     (forward-char 1)
	     (forward-sexp 1)
	     (setq line (read (current-buffer)))
	     (search-backward "(")
	     (setq file  (buffer-substring (+ 1 (point)) (+ 1 at)))
	     (and (integerp line)
		  (setq file (search-path file 'dbl-dirs))
		  (dbl-display-line
		   file line)))))))
		   
    
(defun dbl-display-line (true-file line)
  (let* ((buffer (dbl-find-file true-file))
	 (window (display-buffer buffer t))
	 (pos))
    (save-excursion
      (set-buffer buffer)
      (save-restriction
	(widen)
	(goto-line line)
	(setq pos (point))
	(setq overlay-arrow-string "=>")
	(or overlay-arrow-position
	    (setq overlay-arrow-position (make-marker)))
	(set-marker overlay-arrow-position (point) (current-buffer)))
      (cond ((or (< pos (point-min)) (> pos (point-max)))
	     (widen)
	     (goto-char pos))))
    (set-window-point window overlay-arrow-position)))

(defun dbl-call (command)
  "Invoke dbl COMMAND displaying source in other window."
  (interactive)
  (goto-char (point-max))
  (setq dbl-delete-prompt-marker (point-marker))
  (dbl-set-buffer)
  (send-string (get-buffer-process current-dbl-buffer)
	       (concat command "\n")))

(defun dbl-maybe-delete-prompt ()
  (if (and dbl-delete-prompt-marker
	   (> (point-max) (marker-position dbl-delete-prompt-marker)))
      (let (start)
	(goto-char dbl-delete-prompt-marker)
	(setq start (point))
	(beginning-of-line)
	(delete-region (point) start)
	(setq dbl-delete-prompt-marker nil))))

(defun dbl-break ()
  "Set DBL breakpoint at this source line."
  (interactive)
  (cond ((eq major-mode 'lisp-mode)
	 (save-excursion
	   (end-of-line)
	   (let (name
		 at where)
	     (setq where (point))
	     (mark-defun)
	     (search-forward "(def")
	     (forward-sexp 2)
	     (setq at (point))
	     (forward-sexp -1)
	     (setq name (buffer-substring (point) at))
	     (beginning-of-line)
;	     (setq at
;		   (progn (re-search-forward "^[^;\"\n]+([a-zA-Z ]" (mark) t)
;			  (forward-char -2)
;			  (point)))
	     (setq name (format "(si::break-function '%s %s t)"  name (count-lines 1 where)))
	     (other-window 1)
	     (if (get-buffer-process (current-buffer))
		 (setq current-dbl-buffer (current-buffer)))
	     (message name)
	     (send-string (get-buffer-process current-dbl-buffer)
			  (concat name "\n"))
	     (other-window 1)
	     )))
	(t
  
	 (let ((file-name (file-name-nondirectory buffer-file-name))
	       (line (save-restriction
		       (widen)
		       (1+ (count-lines 1 (point))))))
	   (send-string (get-buffer-process current-dbl-buffer)
			(concat "break " file-name ":" line "\n"))))))
	
	
(defun dbl-read-address()
  "Return a string containing the core-address found in the buffer at point."
  (save-excursion
   (let ((pt (dot)) found begin)
     (setq found (if (search-backward "0x" (- pt 7) t)(dot)))
     (cond (found (forward-char 2)(setq result
			(buffer-substring found
				 (progn (re-search-forward "[^0-9a-f]")
					(forward-char -1)
					(dot)))))
	   (t (setq begin (progn (re-search-backward "[^0-9]") (forward-char 1)
				 (dot)))
	      (forward-char 1)
	      (re-search-forward "[^0-9]")
	      (forward-char -1)
	      (buffer-substring begin (dot)))))))


(defvar dbl-commands nil
  "List of strings or functions used by send-dbl-command.
It is for customization by you.")

(defun send-dbl-command (arg)

  "This command reads the number where the cursor is positioned.  It
 then inserts this ADDR at the end of the dbl buffer.  A numeric arg
 selects the ARG'th member COMMAND of the list dbl-print-command.  If
 COMMAND is a string, (format COMMAND ADDR) is inserted, otherwise
 (funcall COMMAND ADDR) is inserted.  eg. \"p (rtx)%s->fld[0].rtint\"
 is a possible string to be a member of dbl-commands.  "


  (interactive "P")
  (let (comm addr)
    (if arg (setq comm (nth arg dbl-commands)))
    (setq addr (dbl-read-address))
    (if (eq (current-buffer) current-dbl-buffer)
	(set-mark (point)))
    (cond (comm
	   (setq comm
		 (if (stringp comm) (format comm addr) (funcall comm addr))))
	  (t (setq comm addr)))
    (switch-to-buffer current-dbl-buffer)
    (goto-char (dot-max))
    (insert-string comm)))
