;;; $Id: emacspeak-view-process.el,v 5.0 1996/11/22 18:04:26 raman Exp $
;;; $Author: raman $ 
;;; Description: Emacspeak extension for flexible viewing of processes
;;; Keywords:emacspeak, audio interface to emacs administering processes
;;{{{  LCD Archive entry: 

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@adobe.com
;;; A speech interface to Emacs |
;;; $date: $ |
;;;  $Revision: 5.0 $ | 
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (c) 1995 by T. V. Raman Adobe Systems Incorporated 
;;; All Rights Reserved. 
;;;
;;; This file is not part of GNU Emacs, but the same permissions apply.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-sounds)
(require 'emacspeak-speak)
;;{{{  Introduction

;;; Powerful speech interface to viewing and administering processes

;;}}}
;;{{{  keybindings

(add-hook 'View-process-mode-hook
          (function (lambda ()
(declare (special View-process-mode-map))
                      (define-key View-process-mode-map "\C-m"
          'View-process-goto-first-field-next-line)
                      )))

;;}}}
;;{{{ Advice interactive commands:

(defadvice View-process-goto-first-field-next-line (after emacspeak pre act
                                                          comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (View-process-show-pid-and-command)))

(defadvice  View-process-next-field (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (let ((dtk-stop-immediately nil))
      (View-process-which-field-name)
      (emacspeak-speak-current-field))))


(defadvice  View-process-previous-field (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (let ((dtk-stop-immediately nil))
      (View-process-which-field-name)
      (emacspeak-speak-current-field))))

(defadvice View-process-sort-by-current-field-g (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (message "Sorted processes by current field")))

(defadvice View-process-sort-output-by-current-field (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (message "Sorted processes by current field")))

(defadvice View-process-reverse-output (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)
    (message "Reversed output lines")))



(defadvice View-process-quit (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))
(defadvice View-process-output-end (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (View-process-show-pid-and-command)))

(defadvice View-process-output-start (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (View-process-show-pid-and-command)))

(defadvice View-process-start-itimer (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when  (interactive-p)
    (emacspeak-auditory-icon 'on)
    (message "Started itimer")))

(defadvice View-process-delete-itimer (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when  (interactive-p)
    (emacspeak-auditory-icon 'off)
    (message "Deleted itimer")))

;;}}}
(provide  'emacspeak-view-process)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; end: 

;;}}}
