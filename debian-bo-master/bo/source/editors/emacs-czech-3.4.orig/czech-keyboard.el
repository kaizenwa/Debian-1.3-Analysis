;; @(#) czech-keyboard.el -- keyboard handling

;; @(#) $Id: czech-keyboard.el,v 3.4 1997/02/26 20:50:05 pdm Exp $	
;; @(#) $Keywords: i18n, Czech, keyboard $
;; $KnownCompatibility: 19.34, XEmacs 19.14 $

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

;; This defines all thing necessary to handle Czech keyboard.
;;
;; Keyboard is defined by redefining some keys in key-translation-map.  These
;; keys are handled by special functions which do proper things.
;;
;; It requires `czech.el' to be loaded first.

;;; History:

;; So long, so very long...

;;; Code:


(require 'czech)

(defconst cz-keyboard-version "$Id: czech-keyboard.el,v 3.4 1997/02/26 20:50:05 pdm Exp $"
  "Latest modification time and version number.")


;;; *** Initialize environment ***

;; We have to assign keybindings for 128-159 in Windoze
(if cz-windoze
    (let ((i 128))
      (while (< i 160)
	(define-key global-map (vector i) 'self-insert-command)
	(setq i (1+ i)))))


;;; *** Create proper keyboard ***

(if (not key-translation-map)
    (setq key-translation-map (make-sparse-keymap)))

;; key-translation-map overrides bindings in function-key-map
;; So we have to support some keys on Linux console in another way.
(if (string= (getenv "TERM") "linux")
    (progn
      (define-key key-translation-map "[A" [up])
      (define-key key-translation-map "[B" [down])
      (define-key key-translation-map "[C" [right])
      (define-key key-translation-map "[D" [left])
      (define-key key-translation-map "[2~" [insert])
      (define-key key-translation-map "[3~" [delete])
      (define-key key-translation-map "[1~" [home])
      (define-key key-translation-map "[4~" [end])
      (define-key key-translation-map "[5~" [next])
      (define-key key-translation-map "[6~" [prior])
      (define-key key-translation-map "[[A" [f1])
      (define-key key-translation-map "[[B" [f2])
      (define-key key-translation-map "[[C" [f3])
      (define-key key-translation-map "[[D" [f4])
      (define-key key-translation-map "[[E" [f5])
      (define-key key-translation-map "[17~" [f6])
      (define-key key-translation-map "[18~" [f7])
      (define-key key-translation-map "[19~" [f8])
      (define-key key-translation-map "[20~" [f9])
      (define-key key-translation-map "[21~" [f10])
      (define-key key-translation-map "[23~" [f11])
      (define-key key-translation-map "[24~" [f12])))

(defvar cz-keyboard nil
  "Current keyboard layout.
Do not set this variable directly, use function `cz-set-keyboard' instead.")

(defvar cz-correct-table-on nil
  "Table for word correction.
Do not use and/or set it.")
(defvar cz-correct-table-off nil
  "Table for word correction.
Do not use and/or set it.")

(defun cz-set-keyboard (&optional keyboard not-redefine-keypad)
  "Define Czech keyboard.
KEYBOARD can be `nil', one of symbols `standard' and `programmer', or a list.

If `nil', set default keyboard according to variable `cz-keyboard-nonstandard'.
If `standard', set standard Czech keyboard defined by variable
  `cz-standard-keyboard'.
If `programmer', set default non-standard Czech keyboard defined by variables
`cz-keys', `cz-dead-key-1', `cz-dead-key-2', `cz-keys-dead-1', and
`cz-keys-dead-2'.

If KEYBOARD is a list, then it looks like
  (SIMPLE-KEYS [ DEAD-KEY-LIST [ DEAD-KEY-LIST ... ] ] ),
where
  SIMPLE-KEYS is an association list of elements (KEY . REDEFINITION)
    KEY must be character.
    REDEFINITION should be string or vector.
  DEAD-KEY-LIST is an association list (DEAD-KEY . DEAD-SIMPLE-KEYS) which
    defines key behavior after pressing DEAD-KEY.
    DEAD-KEY should be character.
    Format of DEAD-SIMPLE-KEYS is the same as for SIMPLE-KEYS.

See variables `cz-standard-keyboard', `cz-keys', `cz-dead-key-1', and
`cz-keys-dead-1' for some examples.

If an optional argument NOT-REDEFINE-KEYPAD is non-`nil', keypad keys will be
not redefined to work as they are (and will probably act as accent and/or
dead keys).

Note that this function is called while loading package `czech' -- see
variables `cz-keyboard-nonstandard' and  `cz-redefine-keypad'."
  (interactive)
  ;; Get proper keyboard
  (let ((keyb (cond ((or (eq keyboard 'standard)
			 (and (null keyboard) (not cz-keyboard-nonstandard)))
		     cz-standard-keyboard)
		    ((or (eq keyboard 'programmer)
			 (and (null keyboard) cz-keyboard-nonstandard))
		     (list cz-keys
			   (cons cz-dead-key-1 cz-keys-dead-1)
			   (cons cz-dead-key-2 cz-keys-dead-2)))
		    (t keyboard)))
	(handle-func (if cz-xemacs 'cz-handle-key-xemacs 'cz-handle-key))
	k)
    ;; Update `iso-languages' standard variable
    ;; It must be done first otherwise loading `iso-acc' redefines our keys.
    (if (not cz-xemacs)
	(progn
	  (require 'iso-acc)
	  (setq iso-languages
		(cz-replace-by-car iso-languages (list "czech" (cdr keyb))))))
    ;; Define keys in key-translation-map
    (setq k (car keyb))
    (while k
      (define-key key-translation-map (vector (car (car k))) handle-func)
      (setq k (cdr k)))
    (setq k (cdr keyb))
    (while k
      (define-key key-translation-map (vector (car (car k))) handle-func)
      (setq k (cdr k)))
    ;; Handle keypad keys
    (if cz-redefine-keypad
	(let ((cz-keypad '(([kp-1] . "1") ([kp-2] . "2") ([kp-3] . "3")
			   ([kp-4] . "4") ([kp-5] . "5") ([kp-6] . "6")
			   ([kp-7] . "7") ([kp-8] . "8") ([kp-9] . "9")
			   ([kp-0] . "0") ([kp-add] . "+")
			   ([kp-subtract] . "-") ([kp-multiply] . "*")
			   ([kp-divide] . "/") ([kp-decimal] . ".")))
	      aux)
	  (while cz-keypad
	    (setq aux (car cz-keypad))
	    (define-key function-key-map (car aux) (car aux))
	    (define-key key-translation-map (car aux) (cdr aux))
	    (setq cz-keypad (cdr cz-keypad)))))
    ;; Redefine alt-digits
    (if cz-xemacs
	(progn
	  (define-key key-translation-map '(meta ?1) 'cz-handle-key-altdigit)
	  (define-key key-translation-map '(meta ?2) 'cz-handle-key-altdigit)
	  (define-key key-translation-map '(meta ?3) 'cz-handle-key-altdigit)
	  (define-key key-translation-map '(meta ?4) 'cz-handle-key-altdigit)
	  (define-key key-translation-map '(meta ?5) 'cz-handle-key-altdigit)
	  (define-key key-translation-map '(meta ?6) 'cz-handle-key-altdigit)
	  (define-key key-translation-map '(meta ?7) 'cz-handle-key-altdigit)
	  (define-key key-translation-map '(meta ?8) 'cz-handle-key-altdigit)
	  (define-key key-translation-map '(meta ?9) 'cz-handle-key-altdigit)
	  (define-key key-translation-map '(meta ?0) 'cz-handle-key-altdigit))
      (define-key key-translation-map "\M-1" 'cz-handle-key-altdigit)
      (define-key key-translation-map "\M-2" 'cz-handle-key-altdigit)
      (define-key key-translation-map "\M-3" 'cz-handle-key-altdigit)
      (define-key key-translation-map "\M-4" 'cz-handle-key-altdigit)
      (define-key key-translation-map "\M-5" 'cz-handle-key-altdigit)
      (define-key key-translation-map "\M-6" 'cz-handle-key-altdigit)
      (define-key key-translation-map "\M-7" 'cz-handle-key-altdigit)
      (define-key key-translation-map "\M-8" 'cz-handle-key-altdigit)
      (define-key key-translation-map "\M-9" 'cz-handle-key-altdigit)
      (define-key key-translation-map "\M-0" 'cz-handle-key-altdigit))
    ;; Update word correction tables
    (let ((i 0)
	  (simple-keys (car keyb)))
      (if (not cz-correct-table-on)
	  (setq cz-correct-table-on (make-string 256 0)))
      (if (not cz-correct-table-off)
	  (setq cz-correct-table-off (make-string 256 0)))
      (while (< i 256)
	(aset cz-correct-table-on i i)
	(aset cz-correct-table-off i i)
	(setq i (1+ i)))
      (while simple-keys
	(aset cz-correct-table-on
	      (car (car simple-keys))
	      (elt (cdr (car simple-keys)) 0))
	(aset cz-correct-table-off
	      (elt (cdr (car simple-keys)) 0)
	      (car (car simple-keys)))
	(setq simple-keys (cdr simple-keys))))
    ;; Save the definition in global variable for later handling
    (setq cz-keyboard keyb)))


;;; *** Keyboard handling functions ***

(defun cz-handle-key (prompt)
  "Handles accented and dead keys."
  (let* ((event last-input-event)
	 (definition (assoc event (car cz-keyboard)))
	 (dead (assoc event cz-keyboard)))
    ;; Following condition is mostly taken from `iso-acc.el'
    (if (or (not cz-keyboard-mode)
	    prompt
	    (and (not (eq (key-binding "a") 'self-insert-command))
		 (not isearch-mode))
	    (if (cz-test-emacs-version 19 31)
		(> (length (this-single-command-keys)) 1)
	      (> (length (this-command-keys)) 1))
	    this-command
	    (and (not definition)
		 (not dead)))
	;; Key handled as regular
	(vector last-input-event)
      (if definition
	  ;; Accent key
	  (cdr definition)
	;; Dead key
	(if (and (not isearch-mode) (not overwrite-mode))
	    (progn
	      (insert (or (cdr (assoc (car dead) cz-dead-symbols))
			  " "))
	      (backward-char)))
	(let ((event (read-event))
	      aux)
	  (if (eq event 32)		; space: leave accent here
	      [right]
	    (if (and (not isearch-mode) (not overwrite-mode))
		(delete-region (point) (1+ (point))))
	    (if (setq aux (assoc event (cdr dead)))
		(cdr aux)
	      (vector event))))))))

;; XEmacs requires too different approach
;; (especially XEmacs doesn't like 8-bit characters and is "slow")
(defun cz-handle-key-xemacs (prompt)
  "Handles accented and dead keys."
  (let* ((event last-input-event)
	 (definition
	   (assoc (or (event-modifiers event) ; dummy for cz-keyboard
		      (event-key event))
		  (car cz-keyboard)))
	 (dead
	  (assoc (or (event-modifiers event) ; dummy for cz-keyboard
		     (event-key event))
		 cz-keyboard))
	 (hack cz-xemacs-minibuffer-hack))
    (setq cz-xemacs-minibuffer-hack nil)
    ;; Following condition is mostly taken from `iso-acc.el'
    (if (or (not cz-keyboard-mode)
	    prompt
	    (and (not (eq (key-binding "a") 'self-insert-command))
		 (not isearch-mode))
	    (and (> (length (this-command-keys)) 1)
		 (not hack))
	    (and (not definition)
		 (not dead)))
	;; Key handled as regular
	(vector last-input-event)
      (if definition
	  ;; Accent key
	  (progn
	    (setq definition (cdr definition))
	    (if (arrayp definition)
		(vector (list (aref definition 0)))
	      definition))
	;; Dead key
	(if (not isearch-mode)
	    (progn
	      (insert (or (cdr (assoc (car dead) cz-dead-symbols))
			  " "))
	      (backward-char)))
	(let ((event (next-command-event))
	      aux)
	  (if (eq (event-key event) 'space) ; space: leave accent here
	      [right]
	    (if (not isearch-mode)
		(delete-region (point) (1+ (point))))
	    (if (setq aux (assoc
			   (or (event-modifiers event) ; dummy for cz-keyboard
			       (event-key event))
			   (cdr dead)))
		(progn
		  (setq aux (cdr aux))
		  (if (arrayp aux)
		      (vector (list (aref aux 0)))
		    aux))
	      (vector event))))))))

(defun cz-handle-key-altdigit (prompt)
  "Handles `M-<digit>' keys."
  (if (and cz-keyboard-mode cz-redefine-alt-digits)
      (char-to-string
       (if cz-xemacs
	   (event-key last-input-event)
	 (event-basic-type last-input-event)))
    (vector last-input-event)))

(defun cz-handle-key-isearch ()
  "Switches cz-keyboard minor mode in isearch."
  (interactive)
  (cz-keyboard-mode)
  (isearch-update))


;;; *** New minor mode: cz-keyboard-mode ***

;; Define control variable
(defvar cz-keyboard-mode nil
  "Control variable for cz-keyboard-mode.")
(if cz-global-mode
    (kill-local-variable 'cz-keyboard-mode)
  (make-variable-buffer-local 'cz-keyboard-mode)
  (setq-default cz-keyboard-mode cz-keyboard-default-on))

;; Define mode function
(defun cz-keyboard-mode (&optional arg)
  "Mode for using Czech or another user defined keyboard."
  (interactive)
  (if (or (null arg)
	  (not (equal cz-keyboard-mode (> (prefix-numeric-value arg) 0))))
      ;; Mode must be changed
      (progn
	(if (setq cz-keyboard-mode (not cz-keyboard-mode))
	    (run-hooks 'cz-keyboard-mode-hook))
	(force-mode-line-update))))
(defun cz-keyboard-mode-on ()
  "Switches cz-keyboard mode on."
  (interactive)
  (cz-keyboard-mode 1))
(defun cz-keyboard-mode-off ()
  "Switches cz-keyboard mode off."
  (interactive)
  (cz-keyboard-mode -1))

;; Minor mode name at modeline
(cz-add-minor-mode 'cz-keyboard-mode " CZ")

;; We will need an auxiliary variable for XEmacs...
(if cz-xemacs
    (setq cz-xemacs-minibuffer-hack nil))

;; Minibuffer handling - let's inherit mode from regular buffer
(defun cz-minibuffer-hook ()
  "Set proper kind of keyboard when entering minibuffer."
  (let (czech-keyboard)
    (save-excursion
      (if cz-xemacs
	  (progn
	    (setq cz-xemacs-minibuffer-hack t)
	    (set-buffer (other-buffer nil nil t)))
	(set-buffer (other-buffer nil t))) ; hopefully sets "calling" buffer
      (setq czech-keyboard cz-keyboard-mode))
    (if czech-keyboard
	(cz-keyboard-mode 1)
      (cz-keyboard-mode -1))))
(if (and (not cz-minibuffer-nonczech)
	 (not cz-global-mode)
	 (not cz-keyboard-default-on))
    (add-hook 'minibuffer-setup-hook 'cz-minibuffer-hook)
  (remove-hook 'minibuffer-setup-hook 'cz-minibuffer-hook))


;;; *** Announce ***

(provide 'czech-keyboard)


;;; czech-keyboard.el ends here

