;;; t-mouse.el --- mouse support within the text terminal

;;; Copyright (C) 1994,1995 Alessandro Rubini <rubini@ipvvis.unipv.it>
;;;               parts are by Ian T Zimmermann <itz@rahul.net>, 1995

;; Maintainer: Alessandro Rubini
;; Keywords: mouse gpm linux

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This package provides access to mouse event as reported by the
;; gpm-Linux package. It uses the program "mev" to get mouse events.
;; It tries to reproduce the functionality offered by emacs under X.
;; The "gpm" server runs under Linux, so this package is rather
;; Linux-dependent.

;; Tested only under emacs-19.25

;;; Code:

(defvar t-mouse-process nil 
  "Embeds the process which passes mouse events to emacs. It is used
by 't-mouse.el")


;;; useful for debugging
(defun t-mouse-print (name item)
  (save-excursion
    (set-buffer "*scratch*")
    (end-of-buffer)
    (insert (format "%s\t ==> %s\n" name item)))
  (message "%s" item))
 


;; get the number of the current virtual console
(defun t-mouse-tty ()
  (let (tty
	(buffer (generate-new-buffer "*t-mouse*")))
    (call-process "ps" nil buffer nil
		  "-h" (format "%s" (emacs-pid)))
    (save-excursion
      (set-buffer buffer)
      (goto-char 0)
      (or
       (re-search-forward "p \\([0-9a-f]\\)" nil t)
       (re-search-forward "v0\\([0-9a-f]\\)" nil t)
       ;; Hmmmm... ps-0.99 changed it again. No key letter any more
       ;; I must look for the second field...
       (re-search-forward "[0-9]+ +\\([0-9]+\\)" nil t))
      (and
       (match-beginning 1)
       (setq tty (buffer-substring (match-beginning 1) (match-end 1)))))
    (kill-buffer buffer)
    tty
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; this is the firing function
(defun t-mouse-run ()
  (interactive)
  (let ((process-connection-type nil) (tty (t-mouse-tty)))
    ;; remove any existing mouse process
    (and (boundp 't-mouse-process)
	 (processp t-mouse-process)
	 (delete-process t-mouse-process))
    (and
     tty
     (progn 
       (setq t-mouse-process 
	     ;; use only press events
	     (start-process "t-mouse" nil "mev" "-E" "-C" tty "-M" "leftAlt"
			    "-e" "press,drag,release,hard" "-d" "move" "-f"))
       (set-process-filter t-mouse-process 't-mouse-process-filter)
       (process-kill-without-query t-mouse-process))
       t-mouse-process)
))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; this is the filter
(defun t-mouse-process-filter (proc string)
  (let* ((event ;;(condition-case nil
	  ;; protected
	  (t-mouse-make-event string)
	  ;; protection
	  ;;(error (progn (delete-process t-mouse-process)
	  ;;	  (error "Mouse error: %s" string)))
	  );;)
	 (cmd (lookup-key t-mouse-keymap (car event)))
	 (window (car (nth 1 event)))
	 local-cmd)
    
    ;;(t-mouse-print "event" event)
    ;;(t-mouse-print "cmd" cmd)
    
    (let ((old-window (selected-window)))
      (or (equal (selected-window) window)
	  (select-window window))
      (setq local-cmd (lookup-key t-mouse-localmap (car event)))
      (and local-cmd (not (numberp local-cmd)) (setq cmd local-cmd))
      (select-window old-window))
    
    (if cmd
	(prog2 
	    (funcall cmd (nth 1 event))
	    (run-hooks 'post-command-hook)))
    ))


;;; This fun is partly Copyright (C) 1994 Per Abrahamsen <abraham@iesd.auc.dk>
(defun t-mouse-make-event (string)
  (let* ((string (car (read-from-string string)))
	(mouse (vector (car string)))
	(point (nth 1 string))					
	(x (car point))
	(y (cdr point))
	(window (window-at x y))
	(where (coordinates-in-window-p point window))
	(pos (if (consp where)
		 (let ((old-window (selected-window))
		       (dummy (select-window window))
		       (pos (save-excursion
			      (goto-char (window-start window))
			      (move-to-window-line  (cdr where))
			      (move-to-column (+ (car where) (current-column)
						 (max 0 (1- (window-hscroll)))))
			      (point))
			    ))
		   (select-window old-window)
		   pos)
	       where)))
    ;;(t-mouse-print "where" where)
    ;; manage the scroll-bar
	
    ;; does work with horizontally-split windows
    (if t-mouse-mode
	(prog2
	    (setq pos t-mouse-mode)
	    (if t-mouse-window (setq window t-mouse-window)))
      (if (and (consp where) (>= (car where) (1- (window-width window))))
	  (setq pos 'scroll-bar)))
	
		
    ;; make the event
    (or (integerp pos) (setq mouse (vector pos (car string))))
    (list mouse (list window pos
		      (if (and
			   (not (eq pos 'mode-line))
			   (not (eq pos 'vertical-line))
			   (consp where))
			  where
			point)))
	))


;; Restore normal mouse behaviour outside Emacs.
(defun t-mouse-suspend ()
  (and
   t-mouse-process
   (process-send-string t-mouse-process "push -e 0 -d any\n")))

(defun t-mouse-resume ()
  (and
   t-mouse-process
   (process-send-string t-mouse-process "pop\n")))

(add-hook 'suspend-hook 't-mouse-suspend)
(add-hook 'suspend-resume-hook 't-mouse-resume)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; make your keymap
;;
;; Sure, It'd be better to use the exixtent keymaps, but gpm events
;; can't easily be mapped onto X events, at least, not always. Some X event
;; modify the 'face' of characters, while this is not possible here.

(defvar t-mouse-keymap (make-sparse-keymap) "Keymap used by t-mouse")
(defvar t-mouse-modeline-keymap (make-sparse-keymap)
	"Keymap for modeline mouse events")
(defvar t-mouse-scrollbar-keymap (make-sparse-keymap)
	"Keymap for scrollbar mouse events")
(defvar t-mouse-vertical-keymap (make-sparse-keymap)
	"Keymap for vertical line events")
(defvar t-mouse-scroll nil "The last position when scrolling")
(defvar t-mouse-window nil "Window where dragging started")
(defvar t-mouse-mode nil "Area where dragging started")
(defvar t-mouse-word-syntax "w_" "The syntax set to select with double-click")
  
(defvar t-mouse-localmap (make-sparse-keymap) "The local map for mouse events")
(make-variable-buffer-local 't-mouse-localmap)

(define-key t-mouse-keymap [mode-line]				t-mouse-modeline-keymap)
(define-key t-mouse-keymap [scroll-bar]				t-mouse-scrollbar-keymap)
(define-key t-mouse-keymap [vertical-line]		t-mouse-vertical-keymap)

(define-key t-mouse-keymap [down-mouse-1]			'tm-goto)
(define-key t-mouse-keymap [drag-mouse-1]			'tm-drag)
(define-key t-mouse-keymap [mouse-1]				'tm-regn)
(define-key t-mouse-keymap [down-mouse-2]			'tm-yank)
(define-key t-mouse-keymap [down-mouse-3]			'tm-copy)
(define-key t-mouse-keymap [double-mouse-1]	        'tm-word)
(define-key t-mouse-keymap [triple-mouse-1]		    'tm-line)
(define-key t-mouse-keymap [double-mouse-3]		    'tm-kill)

;; mode-line actions

(define-key t-mouse-keymap [mode-line mouse-1]      'tm-scroll-done)
(define-key t-mouse-keymap [mode-line mouse-2]      'tm-win-single)
(define-key t-mouse-keymap [mode-line mouse-3]      'tm-win-delete)
(define-key t-mouse-keymap [mode-line M-mouse-2]    'tm-win-split-horizontally)
(define-key t-mouse-keymap [mode-line M-mouse-3]    'tm-win-split)
(define-key t-mouse-keymap [mode-line down-mouse-1] 'tm-win-size)
(define-key t-mouse-keymap [mode-line drag-mouse-1] 'tm-win-drag)

;; vertical-line actions

(define-key t-mouse-keymap [vertical-line mouse-2]  'tm-win-split-horizontally)
(define-key t-mouse-keymap [vertical-line mouse-3]  'tm-win-split-horizontally)
(define-key t-mouse-keymap [vertical-line down-mouse-1] 'tm-win-size-horizontally)
(define-key t-mouse-keymap [vertical-line drag-mouse-1] 'tm-win-drag-horizontally)
(define-key t-mouse-keymap [vertical-line mouse-1] 'tm-scroll-done)

;; scroll-bar actions

(define-key t-mouse-keymap [scroll-bar down-mouse-1]     'tm-scroll-up)
(define-key t-mouse-keymap [scroll-bar down-mouse-3]     'tm-scroll-down)
(define-key t-mouse-keymap [scroll-bar down-mouse-2]     'tm-scroll-jump)
(define-key t-mouse-keymap [scroll-bar drag-mouse-2]     'tm-scroll-drag)
(define-key t-mouse-keymap [scroll-bar mouse-2]	         'tm-scroll-done)
(define-key t-mouse-keymap [scroll-bar double-mouse-2]   'tm-scroll-done)
(define-key t-mouse-keymap [scroll-bar triple-mouse-2]   'tm-scroll-done)
(define-key t-mouse-keymap [scroll-bar M-down-mouse-1]   'tm-scroll-up)
(define-key t-mouse-keymap [scroll-bar M-down-mouse-3]   'tm-scroll-down)
(define-key t-mouse-keymap [scroll-bar M-down-mouse-2]   'tm-scroll-jump)
(define-key t-mouse-keymap [scroll-bar M-drag-mouse-2]   'tm-scroll-drag)
(define-key t-mouse-keymap [scroll-bar M-mouse-2]	     'tm-scroll-done)
(define-key t-mouse-keymap [scroll-bar M-double-mouse-2] 'tm-scroll-done)
(define-key t-mouse-keymap [scroll-bar M-triple-mouse-2] 'tm-scroll-done)

;; meta-mouse on the window makes scroll-bar actions

(define-key t-mouse-keymap [M-down-mouse-1]		    'tm-scroll-up)
(define-key t-mouse-keymap [M-down-mouse-2]		    'tm-scroll-jump)
(define-key t-mouse-keymap [M-drag-mouse-2]		    'tm-scroll-drag)
(define-key t-mouse-keymap [M-down-mouse-3]		    'tm-scroll-down)



;;; first, the scrollbar

;;; -up and -down don't set any mode, 'cause they're impulsive events

(defun tm-scroll-up (event)
  ;;(message "up")
  (select-window (car event))
  (scroll-up (cdr (nth 2 event)))
)

(defun tm-scroll-down (event)
  ;;(message "down")
  (select-window (car event))
  (scroll-down (cdr (nth 2 event)))
)

(defun tm-scroll-jump (event)
  ;;(message "jump")
	(select-window (car event))
  (let* ((lines (+ (count-lines (point-min) (point-max)) (window-height)))
	 (target (/ (* lines (cdr (nth 2 event))) (window-height)))
	 (step (/ lines (window-height))))
    ;;(goto-line target)
    ;;(recenter)
    (setq t-mouse-scroll (cons step  (cdr (nth 2 event))))
    (setq t-mouse-mode 'scroll-bar)
    (setq t-mouse-window (car event))
))

(defun tm-scroll-drag (event)
  ;;(message "drag")
  (let* ((step (car t-mouse-scroll))
	 (steplet (/ (* step (car (nth 2 event))) (window-width)))
	 (y (cdr (nth 2 event))))
    (scroll-down (* steplet (- (cdr t-mouse-scroll) y)))
    (setq t-mouse-scroll (cons step y))
    (setq t-mouse-mode 'scroll-bar)
    (setq t-mouse-window (car event))
))

(defun tm-scroll-done (event)
  ;;(message "done")
  (setq t-mouse-scroll nil)
	(setq t-mouse-window nil)
	(setq t-mouse-mode nil))

;;; modeline events
(defun tm-win-size (event)
  (setq t-mouse-scroll (nth 2 event))
  (setq t-mouse-window
	(let
	    ((window-bottom (nth 3 (window-edges (car event))))
	     (mini-top (nth 1 (window-edges (minibuffer-window)))))
	  (if (eq window-bottom mini-top)
	      (minibuffer-window)
	    (car event))))
  (setq t-mouse-mode 'mode-line))

(defun tm-win-drag (event)
  (let ((old-window (selected-window)))
	(select-window (car event))
	(let ((s-diff (- (cdr (nth 2 event)) (cdr t-mouse-scroll))))
	  (if (equal (car event) (minibuffer-window))
	      (shrink-window s-diff)
	    (enlarge-window s-diff)))
	(if (and
	     (window-live-p old-window)
	     (window-live-p (car event)))
	    (progn
	      (setq t-mouse-scroll (nth 2 event))
	      (setq t-mouse-window (car event))
	      (setq t-mouse-mode 'mode-line)
	      (select-window old-window))
	  (tm-scroll-done event))))


;;; vertical line events
(defun tm-win-size-horizontally (event)
	(setq t-mouse-scroll (nth 2 event))
	(setq t-mouse-window (car event))
	(setq t-mouse-mode 'vertical-line))

(defun tm-win-drag-horizontally (event)
	(let ((old-window (selected-window)))
	  (select-window (car event))
	  (let ((s-diff (- (car (nth 2 event)) (car t-mouse-scroll))))
	    (enlarge-window-horizontally s-diff))
	  (if (and
	       (window-live-p old-window)
	       (window-live-p (car event)))
	      (progn
		(setq t-mouse-scroll (nth 2 event))
		(setq t-mouse-window (car event))
		(setq t-mouse-mode 'vertical-line)
		(select-window old-window))
	    (tm-scroll-done event))))

;;; and then conventional events

(defun tm-goto (event)
	(select-window (car event))
  (goto-char (nth 1 event))
  (push-mark (point) 'nomsg 'activate))

(defun tm-drag (event)
  (goto-char (nth 1 event)))

(defun tm-regn (event)
  (let ((current (nth 1 event)))
    (or
     (equal current (mark))
     (progn
       (goto-char (mark)) 
       (sit-for 0.5) 
       (goto-char current) 
       (tm-copy event)))))

(defun tm-word (event)
  (let* ((current (nth 1 event))
	(previous (mark))
	(backward (> previous current)))
    (pop-mark)
    ;; update previous and set mark
    (goto-char previous)
    (if backward (skip-syntax-forward  t-mouse-word-syntax)
      (skip-syntax-backward t-mouse-word-syntax))
    (setq previous (point))
    (push-mark (point) 'nomsg 'activate)
    (sit-for 0.5)
    ;; update current
    (goto-char current)
    (if backward (skip-syntax-backward t-mouse-word-syntax) 
      (skip-syntax-forward  t-mouse-word-syntax))
    (setq current (point))
    (kill-ring-save (mark) current)))
  
(defun tm-line (event)
  (let* ((current (nth 1 event))
	 (previous (mark))
	 (backward (> previous current)))
    (tm-drag event)
    (pop-mark)
    ;; update previous and set mark
    (goto-char previous)
    (if backward (end-of-line) (beginning-of-line))
    (setq previous (point))
    (push-mark (point) 'nomsg 'activate)
    (sit-for 0.5)
    ;; update current
    (goto-char current)
    (if backward (beginning-of-line) (end-of-line))
    (setq current (point))
    (kill-ring-save (mark) current)))


(defun tm-yank (event)
	(select-window (car event))
  (let ((current (nth 1 event)))
     (goto-char current)
    (yank)))

(defun tm-copy (event)
  (select-window (car event))
  (let ((current (nth 1 event)))
    (kill-ring-save (mark) current)
    (goto-char current)))

(defun tm-kill (event)
  (select-window (car event))
  (let ((current (nth 1 event)))
    (kill-region (mark) current)))

(defun tm-win-single (event)
  (select-window (car event))
  (delete-other-windows))

(defun tm-win-delete (event)
  (select-window (car event))
  (if (one-window-p 'no-mini)
      (kill-buffer (window-buffer (car event)))
    (delete-window)))

(defun tm-win-split (event)
  (let ((old-window (selected-window)))
    (select-window (car event))
    (split-window)
    (select-window old-window)))

(defun tm-win-split-horizontally (event)
  (let ((old-window (selected-window)))
    (select-window (car event))
    (split-window-horizontally)
    (select-window old-window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; This is mode-specific code

(defun t-mouse-make-keymap (fun)
  (kill-local-variable 't-mouse-localmap)
  (define-key
	(set (make-local-variable 't-mouse-localmap) (make-sparse-keymap))
	[down-mouse-2]
	fun))

(add-hook 'Info-mode-hook            't-mouse-Info-mode)
(add-hook 'dired-mode-hook           't-mouse-dired-mode)
(add-hook 'completion-list-mode-hook 't-mouse-completion-list-mode)

;;; Info (info.el)

(defun t-mouse-Info-mode () (t-mouse-make-keymap 'tm-Info-follow))

(defun tm-Info-follow (event)
	(select-window (car event))
  (tm-goto event)
  (and (not (Info-try-follow-nearest-node))
       (save-excursion (forward-line 1) (eobp))
       (Info-next-preorder)))

;;; dired (dired.el)

(defun t-mouse-dired-mode () (t-mouse-make-keymap 'tm-dired-ff-other-window))

(defun tm-dired-ff-other-window (event)
	(select-window (car event))
  (let (file)
	(save-excursion
	  (tm-goto event)
	  (setq file (dired-get-filename))
	  (find-file-other-window (file-name-sans-versions file t)))))

;;; completion list (simple.el and mouse.el)
;;;
;;; Doesn't work, because I've already selected the event window


(defun t-mouse-completion-list-mode ()
  (t-mouse-make-keymap 'tm-mouse-choose-completion))

(defun tm-mouse-choose-completion (event)
  (let (
		(buffer (window-buffer)) 
		(owindow (selected-window))
		choice cwindow)
	(save-excursion 
	  (tm-goto event)
	  (setq cwindow (selected-window))
	  (let (beg end)
	    (skip-chars-forward "^ \t\n")
	    (while (looking-at " [^ \n\t]")
	      (forward-char 1)
	      (skip-chars-forward "^ \t\n"))
	    (setq end (point))
	    (skip-chars-backward "^ \t\n")
	    (while (and (= (preceding-char) ?\ )
			(not (and (> (point) (1+ (point-min)))
				  (= (char-after (- (point) 2)) ?\ ))))
	      (backward-char 1)
	      (skip-chars-backward "^ \t\n"))
	    (setq beg (point))
	    (setq choice (buffer-substring beg end))))
	;;(t-mouse-print "choice" choice)
	(select-window cwindow)
	(bury-buffer)
	(select-window owindow)
	(choose-completion-string choice buffer)))


;;;    These files use "mouse" with "keymap"
;;;
;;; buffer-menu.el -- no menus with t-mouse
;;; cal-menu.el
;;; compile.el
;;; derived.el
;;; ebuff-menu.el -- no menus with t-mouse
;;; foldout.el
;;; isearch.el
;;; loaddefs.el
;;; replace.el
;;; simple.el
;;; tar-mode.el

;;;    These files name the "mouse"
;;; avoid.el
;;; bibtex.el
;;; buff-menu.el
;;; byte-opt.el
;;; bytecomp.el
;;; calendar.el
;;; cc-mode.el
;;; edmacro.el
;;; finder-inf.el
;;; finder.el
;;; frame.el
;;; gomoku.el
;;; imenu.el
;;; isearch.el
;;; ispell4.el
;;; levents.el
;;; lmenu.el
;;; loadup.el
;;; lselect.el
;;; lucid.el
;;; map-ynp.el
;;; mldrag.el
;;; subr.el
;;; thingatpt.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Done

(t-mouse-run)
(provide 't-mouse)

;(setq debug-on-error t)

;;; t-mouse.el ends here
