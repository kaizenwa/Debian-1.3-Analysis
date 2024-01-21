;; @(#) czech-misc.el -- miscellaneous functions

;; @(#) $Id: czech-misc.el,v 3.4 1997/02/26 20:46:34 pdm Exp $	
;; @(#) $Keywords: i18n, Czech, fonts, autodetection $
;; $KnownCompatibility: 19.34, XEmacs 19.14$

;; This file is *NOT* part of GNU Emacs nor XEmacs.

;; Copyright (C) 1995, 1996, 1997 Milan Zamazal

;; Author:       Milan Zamazal <pdm@fi.muni.cz>
;; Maintainer:   Milan Zamazal <pdm@fi.muni.cz>
;; Requires:     czech.el
;; Remark:       Don't laugh too loudly while reading this file, please.

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, version 2 of the License.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs and/or this package.  If you did not, write to the
;; Free Software Foundation, Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;;; Commentary:

;; This defines less important and smaller things of emacs-czech package, which
;; do not belong to another part of that package.
;;
;; It contains: font handling, correction of words or regions written with
;; other keyboard, selection of characters, auto-detection of Czech texts,
;; displaying Czech in 7-bit characters, and saving of settings.
;;
;; It requires `czech.el' to be loaded first.


;;; History:

;; So long, so very long...

;;; Code:


(require 'czech)

(defconst cz-misc-version "$Id: czech-misc.el,v 3.4 1997/02/26 20:46:34 pdm Exp $"
  "Latest modification time and version number.")


;;; *** Fonts ***

;;;###autoload
(defun cz-set-fonts (cz-font-default
		     &optional cz-font-faces-list cz-font-info-list)
  "Set Czech fonts.
See documentation for variables `cz-font-default', `cz-font-faces-list',
and `cz-font-info-list'."
  (if window-system
      (progn
	;; Set default font (only simple for XEmacs)
	(if cz-font-default
	    (if cz-xemacs
		;; XEmacs
		(set-face-font 'default cz-font-default)
	      ;; GNU Emacs
	      (set-default-font cz-font-default)
	      (setq default-frame-alist
		    (cz-replace-by-car default-frame-alist
				       (cons 'font cz-font-default))))
	  (if (not cz-xemacs)
	      (setq default-frame-alist
		    (delete (cons 'font cz-font-default)
			    default-frame-alist))))
	;; Set fonts of faces
	(mapcar
	 (lambda (pair) (set-face-font (car pair) (car (cdr pair))))
	 cz-font-faces-list)
	;; Set fonts for info
	(defun cz-info-hook ()
	  "Hook to run info with proper Czech fonts."
	  (mapcar
	   (lambda (pair)
	     (set-face-font (car pair) (car (cdr pair))))
	   cz-font-faces-list))
	(add-hook 'Info-mode-hook 'cz-info-hook))))


;;; *** Word and region correction ***

;;;###autoload
(defun cz-correct-word-on (&optional count)
  "Translates last word(s) typed on non-Czech keyboard.
It translates last COUNT words typed on non-Czech keyboard as they would be
typed on Czech keyboard.  COUNT must be positive.
It does not translate all characters, but most of those defined in the list
`cz-keys'."
  (interactive "*p")
  (if (null current-prefix-arg)
      (setq count 1)
    (if (<= count 0) (error "Positive argument expected.")))
  (save-excursion
    (let ((end (point))
	  special-letters
	  (aux (car cz-keyboard)))
      ;; Find all "non-word" keys of Czech keyboard
      (while aux
	(if (not (eq (char-syntax (car (car aux))) 119))
	    (setq special-letters (cons (car (car aux)) special-letters)))
	(setq aux (cdr aux)))
      (setq aux (cdr cz-keyboard))
      (while aux
	(if (not (eq (char-syntax (car (car aux))) 119))
	    (setq special-letters (cons (car (car aux)) special-letters)))
	(setq aux (cdr aux)))
      ;; Do search
      (while (> count 0)
	(forward-word -1)		; faster than backward-word
	(while (member (preceding-char) special-letters)
	  (backward-char)
	  (if (and (preceding-char)
		   (eq (char-syntax (preceding-char)) 119))
	      (forward-word -1)))
	(setq count (1- count)))
      (cz-correct-region (point) end 'czech))))

;;;###autoload
(defun cz-correct-word-off (&optional count)
  "Translates last word(s) typed on Czech keyboard.
It translates last COUNT words typed on Czech keyboard as they would be
typed on non-Czech keyboard.  COUNT must be positive.
It does not translate all characters, but most of those defined in the list
`cz-keys'."
  (interactive "*p")
  (if (null current-prefix-arg)
      (setq count 1)
    (if (<= count 0) (error "Positive argument expected.")))
  (save-excursion
    (let ((end (point)))
      (forward-word (- count))		; faster than `backward-word'
      (cz-correct-region (point) end 'us))))

;;;###autoload
(defun cz-correct-word (&optional count)
  "Translates last word(s) typed on other keyboard.
It translates last COUNT words typed on other keyboard as they would be
typed on current keyboard.  COUNT must be positive."
  (interactive "*p")  ; default value `1' is ok
  (if cz-keyboard-mode
      (cz-correct-word-on count)
    (cz-correct-word-off count)))

;;;###autoload
(defun cz-correct-word-switch (&optional count)
  "Translates last word(s) typed on other keyboard.
It is like `cz-correct-word', but also switches keyboard.
It translates last COUNT words typed on other keyboard as they would be
typed on current keyboard.  COUNT must be positive."
  (interactive "*p")  ; default value `1' is ok
  (cz-keyboard-mode)
  (cz-correct-word count))

;;;###autoload
(defun cz-correct-switch (&optional count)
  "Translates part of text typed on other keyboard.
It switches to the other keyboard mode and translates text from the current
position to the beginning of line and COUNT lines before.
COUNT must be non-negative.
If there is no non-space character at the current line and no prefix argument
was given, then current line is not included into COUNT."
  (interactive "*p")
  (setq count (if (null current-prefix-arg) 1 (1+ (- count))))
  (if (> count 1) (error "Non-negative argument expected."))
  ;; Switch to proper mode
  (cz-keyboard-mode)
  ;; Do translation
  (save-excursion
    (let ((point (point)))		; save original point
      ;; Test for non-space characters
      (beginning-of-line)
      (if (and (null current-prefix-arg)
	       (not (re-search-forward "\\S-" point t)))
	  (setq count (1- count)))
      ;; Do it
      (beginning-of-line count)
      (cz-correct-region (point) point))))

;;;###autoload
(defun cz-correct-region (begin end &optional type)
  "Translates region typed on other keyboard.
An optional argument TYPE says, what translation should be performed:
`czech' means to Czech, `us' means from Czech."
  (interactive "*r")
  (if (not type)
      (setq type (if cz-keyboard-mode 'czech 'us)))
  (save-excursion
    (if (eq type 'us)
	(translate-region (point) end cz-correct-table-off)
      (save-restriction
	(narrow-to-region begin end)
	(goto-char (point-min))
	(let ((regexp (regexp-quote
		       (char-to-string (car (car (cdr cz-keyboard))))))
	      (keyb (cdr (cdr cz-keyboard)))
	      (pos begin)
	      aux)
	  (while keyb
	    (setq regexp (concat regexp
				 "\\|"
				 (regexp-quote
				  (char-to-string (car (car keyb))))))
	    (setq keyb (cdr keyb)))
	  (while (re-search-forward regexp nil t)
	    (translate-region pos (1- (point)) cz-correct-table-on)
	    (if (eq (point) (point-max))
		(delete-region (1- (point)) (point))
	      (setq aux (cdr (assoc (following-char)
				    (assoc (preceding-char) cz-keyboard))))
	      (cond ((vectorp aux)
		     (setq aux (aref aux 0)))
		    ((not aux)
		     (setq aux (following-char))))
	      (delete-region (1- (point)) (1+ (point)))
	      (insert aux)
	      (setq pos (point))))
	  (translate-region pos (point-max) cz-correct-table-on))))))


;;; *** Character selection ***

(defvar cz-select-char-last nil
  "Last inserted character by `cz-select-char'.")

(defvar cz-select-char-buffer "*select char*"
  "Name of the buffer for char selection")

(defvar cz-select-buffer nil
  "Auxiliary for `cz-select-char'.")

;;;###autoload
(defun cz-select-char (&optional last)
  "Popups buffer for selecting any ISO-8859-2 accent character.
Character can be selected by pressing `RET', `C-c C-c', or middle mouse button
on it.
If an optional prefix argument is given, last selected character is inserted."
  (interactive "*P")
  (if last
      (cz-select-insert cz-select-char-last)
    (let ((buf (get-buffer-create cz-select-char-buffer)))
      (setq cz-select-buffer (current-buffer))
      ;; Create buffer if nonexistent and fill it
      (save-excursion
	(set-buffer buf)
	(if (= (point-min) (point-max))
	    (progn
	      (if cz-windoze
		  (progn
		    (insert
		     "¡¬√ƒ•∆«»œ–… ÀÃÕŒ£º≈—“”‘’÷¿ÿåä™çﬁŸ⁄€‹›èéØ
")
		    (insert
		     "·‚„‰πÊÁËÔÈÍÎÏÌÓ≥æÂÒÚÛÙıˆ‡¯úö∫ù˛˘˙˚¸˝üûøﬂ"))
		(insert "¡¬√ƒ°∆«»œ–… ÀÃÕŒ£•≈—“”‘’÷¿ÿ¶©™´ﬁŸ⁄€‹›¨ÆØ
")
		(insert "·‚„‰±ÊÁËÔÈÍÎÏÌÓ≥µÂÒÚÛÙıˆ‡¯∂π∫ª˛˘˙˚¸˝ºæøﬂ"))
	      ;; Go to the middle of buffer
	      (goto-char (/ (* (+ (point-min) (point-max)) 3) 4)))))
      (display-buffer buf)
      ;; Go to selection window
      (select-window (get-buffer-window cz-select-char-buffer))
      (cz-select-mode))))

(defvar cz-select-mode-map (make-sparse-keymap)
  "Mode map for cz-select major mode.")
(define-key cz-select-mode-map "\r" 'cz-select-select)
(define-key cz-select-mode-map "\C-c\C-c" 'cz-select-cancel)
(define-key cz-select-mode-map [mouse-2] 'cz-select-mouse-select)

(defun cz-select-mode ()
  "Major mode for char selection.
`RET' or middle mouse button selects char; `C-c C-c' cancels selection window."
  (use-local-map cz-select-mode-map)
  (setq mode-name "select char")
  (setq major-mode 'cz-select-mode))

(defun cz-select-select ()
  "Inserts selected char to calling buffer."
  (interactive "*")
  (setq cz-select-char-last (following-char))
  (cz-select-cancel)
  (cz-select-insert cz-select-char-last))

(defun cz-select-cancel ()
  "Deletes char selection window."
  (interactive)
  (let ((buf (get-buffer-window cz-select-buffer)))
    (if (not buf)
	(switch-to-buffer cz-select-buffer))
    (delete-window (selected-window))
    (select-window buf)))

(defun cz-select-mouse-select (event)
  "Inserts clicked char to calling buffer."
  (interactive "e")
  (goto-char (car (cdr (car (cdr event)))))
  (cz-select-select))

(defun cz-select-insert (char)
  "Inserts CHAR into buffer with respect to overwrite mode."
  (if (and overwrite-mode (not (eobp)))
      (delete-region (point) (1+ (point))))
  (insert char))


;;; *** Testing for Czech text ***

;;;###autoload
(defun cz-is-cz-text (&optional return-ratio test-all)
  "Tries to recognize, whether current buffer contains ISO-8859-2 text.
If the buffer is narrowed, only the narrowed part is considered.
If an optional argument RETURN-RATIO is non-`nil', list
`(ratio . ratio-nonczech)' is returned (floating point numbers)
instead of boolean value.
If TEST-ALL is non-`nil', test region without considering variables
`cz-is-buffer-empty' and `cz-is-skip-start' (but `cz-is-test-size' is always
considered).
The recognition is absolutely magical and nothing should hard depend on it.
See also variable `cz-is-czech-characters'."
  (let ((size (- (point-max) (point-min))))
    (if (or (eq size 0)
	    (and (null test-all)
		 (<= size cz-is-buffer-empty)))
	(if return-ratio 0 cz-is-empty-czech)
      (save-excursion
	(let ((czech-chars 0)		; score of high Czech chars found
	      (nonczech-chars 0)	; score of high non-Czech chars found
	      (char-count 0)		; number of characters tested
	      char
	      ratio
	      ratio-nonczech)
	  (goto-char (if test-all
			 (point-min)
		       (min (+ (point-min) cz-is-skip-start) (point-max))))
	  (while (and (<= (point) (point-max))
		      (<= char-count cz-is-test-size))
	    (setq char (following-char))
	    (if (>= char 128)
		(if (member char cz-is-czech-characters)
		    (setq czech-chars (1+ czech-chars))
		  (setq nonczech-chars (1+ nonczech-chars))))
	    (setq char-count (1+ char-count))
	    (if (< (point) (point-max))
		(forward-char)))
	  (setq ratio (/ (float czech-chars) char-count))
	  (setq ratio-nonczech (/ (float nonczech-chars) char-count))
	  (if return-ratio
	      (cons ratio ratio-nonczech)
	    (and (>= ratio cz-is-czech-min)
		 (<= ratio-nonczech cz-is-nonczech-max))))))))


;;; *** ASCII display mode ***

;; Make ASCII display table 
(defun cz-display-ascii ()
  "Modifies `standard-display-table' for ASCII display."
  (let ((table (if cz-xemacs
		   (cdr (car
			 (cdr (car
			       (specifier-spec-list current-display-table)))))
		 (or standard-display-table (make-display-table)))))
    (aset table ?¡ (vector ?A))
    (aset table ?¬ (vector ?A))
    (aset table ?√ (vector ?A))
    (aset table ?ƒ (vector ?A))
    (aset table ?∆ (vector ?C))
    (aset table ?« (vector ?C))
    (aset table ?» (vector ?C))
    (aset table ?œ (vector ?D))
    (aset table ?– (vector ?D))
    (aset table ?… (vector ?E))
    (aset table ?  (vector ?E))
    (aset table ?À (vector ?E))
    (aset table ?Ã (vector ?E))
    (aset table ?Õ (vector ?I))
    (aset table ?Œ (vector ?I))
    (aset table ?£ (vector ?L))
    (aset table ?≈ (vector ?L))
    (aset table ?— (vector ?N))
    (aset table ?“ (vector ?N))
    (aset table ?” (vector ?O))
    (aset table ?‘ (vector ?O))
    (aset table ?’ (vector ?O))
    (aset table ?÷ (vector ?O))
    (aset table ?¿ (vector ?R))
    (aset table ?ÿ (vector ?R))
    (aset table ?™ (vector ?S))
    (aset table ?ﬁ (vector ?T))
    (aset table ?Ÿ (vector ?U))
    (aset table ?⁄ (vector ?U))
    (aset table ?€ (vector ?U))
    (aset table ?‹ (vector ?U))
    (aset table ?› (vector ?Y))
    (aset table ?Ø (vector ?Z))
    (aset table ?· (vector ?a))
    (aset table ?‚ (vector ?a))
    (aset table ?„ (vector ?a))
    (aset table ?‰ (vector ?a))
    (aset table ?Ê (vector ?c))
    (aset table ?Á (vector ?c))
    (aset table ?Ë (vector ?c))
    (aset table ?Ô (vector ?d))
    (aset table ? (vector ?d))
    (aset table ?È (vector ?e))
    (aset table ?Í (vector ?e))
    (aset table ?Î (vector ?e))
    (aset table ?Ï (vector ?e))
    (aset table ?Ì (vector ?i))
    (aset table ?Ó (vector ?i))
    (aset table ?≥ (vector ?l))
    (aset table ?Â (vector ?l))
    (aset table ?Ò (vector ?n))
    (aset table ?Ú (vector ?n))
    (aset table ?Û (vector ?o))
    (aset table ?Ù (vector ?o))
    (aset table ?ı (vector ?o))
    (aset table ?ˆ (vector ?o))
    (aset table ?‡ (vector ?r))
    (aset table ?¯ (vector ?r))
    (aset table ?∫ (vector ?s))
    (aset table ?˛ (vector ?t))
    (aset table ?˘ (vector ?u))
    (aset table ?˙ (vector ?u))
    (aset table ?˚ (vector ?u))
    (aset table ?¸ (vector ?u))
    (aset table ?˝ (vector ?y))
    (aset table ?ø (vector ?z))
    (aset table ?ﬂ (vector ?s ?s))
    (if cz-windoze
	(progn
	  (aset table ?• (vector ?A))
	  (aset table ?º (vector ?L))
	  (aset table ?å (vector ?S))
	  (aset table ?ä (vector ?S))
	  (aset table ?ç (vector ?T))
	  (aset table ?è (vector ?Z))
	  (aset table ?é (vector ?Z))
	  (aset table ?π (vector ?a))
	  (aset table ?æ (vector ?l))
	  (aset table ?ú (vector ?s))
	  (aset table ?ö (vector ?s))
	  (aset table ?ù (vector ?t))
	  (aset table ?ü (vector ?z))
	  (aset table ?û (vector ?z)))
      (aset table ?° (vector ?A))
      (aset table ?• (vector ?L))
      (aset table ?¶ (vector ?S))
      (aset table ?© (vector ?S))
      (aset table ?´ (vector ?T))
      (aset table ?¨ (vector ?Z))
      (aset table ?Æ (vector ?Z))
      (aset table ?± (vector ?a))
      (aset table ?µ (vector ?l))
      (aset table ?∂ (vector ?s))
      (aset table ?π (vector ?s))
      (aset table ?ª (vector ?t))
      (aset table ?º (vector ?z))
      (aset table ?æ (vector ?z)))
    (if cz-xemacs (set-specifier current-display-table table))))

(defvar cz-ascii-display-mode nil
  "Control variable for cz-ascii-display-mode.")

(defvar cz-orig-display-table nil
  "Auxiliary for `cz-ascii-display-mode.")
  
;;;###autoload
(defun cz-ascii-display-mode (&optional arg)
  "Minor mode for displaying Czech texts in 7-bit ASCII."
  (interactive)
  (if (or (null arg)
	  (not (equal cz-ascii-display-mode (> (prefix-numeric-value arg) 0))))
      ;; Mode must be changed
      (progn
	(if (setq cz-ascii-display-mode (not cz-ascii-display-mode))
	    (progn
	      (setq cz-orig-display-table
		    (if cz-xemacs
			(cdadar (specifier-spec-list current-display-table))
		      (copy-sequence standard-display-table)))
	      (cz-display-ascii))
	  (if cz-xemacs
	      (set-specifier current-display-table cz-orig-display-table)
	    (setq standard-display-table cz-orig-display-table)))
	(force-mode-line-update))))

(defun cz-ascii-display-mode-on ()
  "Switches ASCII display mode on."
  (interactive)
  (cz-ascii-display-mode 1))
(defun cz-ascii-display-mode-off ()
  "Switches ASCII display mode off."
  (interactive)
  (cz-ascii-display-mode -1))

;; Minor mode name at modeline
(cz-add-minor-mode 'cz-ascii-display-mode " 7!")


;;; *** Saving settings ***

(defun cz-insert-setting (variable)
  "Inserts variable VARIABLE setting on current point."
  (insert "(setq " (symbol-name variable) " ")
  (let ((value (eval variable)))
    (if (and (not (null value))
	     (not (eq value t))
	     (or (symbolp value)
		 (listp value)))
	(insert "'"))
    (prin1 value (current-buffer))
    (insert ")\n")))

;;;###autoload
(defun cz-save-settings ()
  "Saves values of some user variables to `~/.emacs'.
This is saved there before last found `(load \"czech\")' or (if no load found)
before first `(require 'czech)'.  If no of the two variants is found, options
are saved to the end of file and `(load \"czech\")' is inserted.
This function is not much clever and may produce incorrect results in unusual
situations."
  (interactive)
  (save-excursion
    (let ((buffer (find-file-noselect "~/.emacs")))
      (set-buffer buffer)
      ;; Try to find last load
      (goto-char (point-max))
      (if (re-search-backward
	   "^(load +\"czech\\(\\|\\.el\\|\\.elc\\)\"" nil t)
	  ()
	;; Try to find first require
	(goto-char (point-min))
	(if (re-search-forward "^(require +'czech" nil t)
	    (beginning-of-line)
	  ;; Insert new load
	  (goto-char (point-max))
	  (insert "\n(load \"czech\")\n")
	  (cz-message 6 "`(load \"czech\")' not found - so inserted.")
          (beginning-of-line 0)))
      ;; Insert options
      (cz-insert-setting 'cz-verbose-level))
      (cz-insert-setting 'cz-enable-keybindings)
      (cz-insert-setting 'cz-font-default)
      (cz-insert-setting 'cz-font-faces-list)
      (cz-insert-setting 'cz-font-info-list)
      (cz-insert-setting 'cz-global-mode)
      (cz-insert-setting 'cz-keyboard-default-on)
      (cz-insert-setting 'cz-minibuffer-nonczech)
      (cz-insert-setting 'cz-keyboard-nonstandard)
      (cz-insert-setting 'cz-redefine-alt-digits)
      (cz-insert-setting 'cz-redefine-keypad)
      (cz-insert-setting 'cz-is-empty-czech)
      (cz-insert-setting 'cz-is-buffer-empty)
      (cz-insert-setting 'cz-is-test-size)
      (cz-insert-setting 'cz-is-skip-start)
      (cz-insert-setting 'cz-is-czech-min)
      (cz-insert-setting 'cz-is-nonczech-max)
      (cz-insert-setting 'cz-use-cstocs-limit)
      (cz-insert-setting 'cz-use-cstocs-program)
      (cz-insert-setting 'cz-encoding-files-dir)
      (cz-insert-setting 'cz-convert-accent-file)
      (cz-insert-setting 'cz-convert-onebymore)
      (cz-insert-setting 'cz-convert-unknown-char)
      (cz-insert-setting 'cz-convert-cache-file)
      (cz-insert-setting 'cz-convert-verbose-limit)
      (cz-insert-setting 'cz-sort)
      (cz-insert-setting 'cz-gnus)
      (cz-insert-setting 'cz-enable-menu)
      (cz-message 5 "Options saved.")
      ;; Save file
      (save-buffer)))


;;; *** Announce ***

(provide 'czech-misc)


;;; czech-misc.el ends here

