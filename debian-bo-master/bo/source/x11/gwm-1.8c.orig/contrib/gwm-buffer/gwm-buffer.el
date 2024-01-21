;; -*-Emacs-Lisp-*-
;; WOOL Interactive mode
;; Copyright (C) 1992 Mike Fletcher 
;;			gt0293b@prism.gatech.edu, fletch@cad.gatech.edu,
;;			ccastmf@prism.gatech.edu

;;; File:		wool-mode.el
;;; Description:	WOOL interactive editing mode for Epoch & GWM
;;; Author:		Mike Fletcher <gt0293b@prism.gatech.edu>
;;; Idea taken from:    Lisp interaction mode from std. distribution
;;; First created:	May 26, 1992
;;; Last Modified:	May 26, 1992
;;; Version:		1.0

;;   This program is free software; you can redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published by
;;   the Free Software Foundation; either version 1, or (at your option)
;;   any later version.

;;   This program is distributed in the hope that it will be useful,
;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;   GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; WOOL interaction mode is for use with Epoch and the GWM window
;; manager.  It provides functionallity similar to the builtin
;; lisp-interaction-mode of Emacs for GWM WOOL code.  Basically the
;; only change was making a new function (gwm-eval-last-sexp) to grab
;; the last sexp and send it to GWM by way of the 'GWM_EXECUTE' X
;; property.  See the GWM manual for more details on GWM and WOOL.

(if (not (boundp 'emacs-lisp-mode-map))	; Need to make sure standard
    (load-library "lisp-mode"))		; lisp mode stuff has been loaded

(defvar wool-interaction-mode-map ()
  "Keymap for WOOL interaction mode.")

(if wool-interaction-mode-map		; If need to bind keys
    ()
  (setq wool-interaction-mode-map (make-sparse-keymap))
  (lisp-mode-commands wool-interaction-mode-map)
  (define-key wool-interaction-mode-map "\n" 'gwm-eval-last-sexp))

(defun gwm-eval-last-sexp (arg)
  "Sends sexp before point to GWM via the GWM_EXECUTE property of the
window.  Output is sent to stderr of the GWM process." 
  (interactive "P")
  (copy-to-register 24
    (let ((stab (syntax-table)))
      (unwind-protect 
	  (save-excursion
	    (set-syntax-table emacs-lisp-mode-syntax-table)
	    (forward-sexp -1)
	    (point))
	(set-syntax-table stab)))
    (point) ())
  (epoch::set-property "GWM_EXECUTE" (get-register 24)))

(defun wool-interaction-mode ()
  "Major mode for typing and evaluating WOOL code for the GWM window
manager.  Mostly a direct rip off of Lisp-interaction mode from the
Emacs distribution.  Only works under Epoch.

Commands:
Same as Lisp-interaction mode, except LFD sends the current sexp to
GWM to be executed (by means of the GWM_EXECUTE property).
\\{wool-interaction-mode-map}"

  (interactive)
  (if (boundp 'epoch::version)		; See if running under epoch
      (progn
	(kill-all-local-variables)
	(use-local-map wool-interaction-mode-map)
	(set-syntax-table emacs-lisp-mode-syntax-table)
	(setq major-mode 'wool-interaction-mode)
	(setq mode-name "WOOL Interaction")
	(lisp-mode-variables ())
	(run-hooks 'wool-interaction-mode-hook))
    (message "Sorry, need to be running Epoch to work.")))

(defun gwm-buffer ()
  "Opens up a new buffer named *GWM* in WOOL interaction mode."
  (interactive)
  (switch-to-buffer (get-buffer-create "*GWM*"))
  (wool-interaction-mode))
