;;; $Id: emacspeak-ispell.el,v 5.0 1996/11/22 18:04:26 raman Exp $
;;; $Author: raman $ 
;;; Description:  Emacspeak extension to speech enable ispell
;;; Keywords: Emacspeak, Ispell, Spoken Output, Ispell version 2.30
;;{{{  LCD Archive entry: 
;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@crl.dec.com 
;;; A speech interface to Emacs |
;;; $date: $ |
;;;  $Revision: 5.0 $ | 
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (c) 1994, 1995 by Digital Equipment Corporation.
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'advice)
(require 'emacspeak-speak)
(require 'emacspeak-sounds)
;;{{{  Introduction:

;;; This module speech enables ispell.
;;; Implementation note: This is hard because of how  ispell.el is written
;;; Namely, all of the work is done by one huge hairy function.
;;; This makes advising it hard. 

;;; Original version of this extension was written under emacs-19.28
;;; for ispell.el version 2.30
;;; Now updating it for ispell.el version 2.37.
;;; Support for 2.30 will wither away

;;}}}
;;{{{  define personalities

(defvar ispell-highlight-personality 'harry
  "Voice used to highlight spelling errors. ")

;;}}}
;;{{{  first set up voice  highlighting in 2.30:
(declaim (special ispell-version))
(when  (string-lessp ispell-version "2.37")
     (fset 'ispell-highlight-spelling-error
           (symbol-function 'ispell-highlight-spelling-error-overlay))

 (defadvice ispell-highlight-spelling-error (after emacspeak act )
   "Use voice locking to highlight the error.
Will clobber any existing personality property defined on start end"
   (let ((start (ad-get-arg 0))
         (end (ad-get-arg 1 ))
         (highlight (ad-get-arg 2 )))
     (if highlight
         (put-text-property  start end
                             'personality  ispell-highlight-personality )
       (put-text-property start end
                          'personality  nil ))))
)

;;}}}
;;{{{  ispell command loop:
;;;Signature for  ispell-command-loop in 2.30
;;;defun ispell-command-loop (miss guess word)
Signature in 2.37:
;;; defun ispell-command-loop (miss guess word start end)

;;; Advice speaks the line containing the error with the erroneous
;;; word highlighted.
(if (string-lessp ispell-version "2.37")
;;{{{  old version

(defadvice ispell-command-loop (before emacspeak pre act )
  "Speak the line containing the incorrect word.
 Then speak  the possible corrections. "
  (let ((choices  (ad-get-arg 0 ))
(save-dtk-capitalize dtk-capitalize)
        (position 0))
(or dtk-capitalize 
(dtk-toggle-capitalization))
    (emacspeak-speak-line nil )
    (dtk-force )
    (while (and choices)
      (dtk-say (format "%s %s" position (car choices )))
      (incf position)
      (setq choices (cdr choices )))
    (dtk-force)
(unless save-dtk-capitalize
(dtk-toggle-capitalization))))

;;}}}
;;{{{  new version

(defadvice ispell-command-loop (before emacspeak pre act )
  "Speak the line containing the incorrect word.
 Then speak  the possible corrections. "
  (let ((choices  (ad-get-arg 0 ))
        (start (ad-get-arg 3))
        (end (ad-get-arg 4))
        (save-dtk-capitalize dtk-capitalize)
        (position 0))
    (or dtk-capitalize 
        (dtk-toggle-capitalization))
    (ems-set-personality-temporarily start end ispell-highlight-personality
                                     (emacspeak-speak-line nil )
                                     (dtk-force ))
    (while (and choices)
      (dtk-say (format "%s %s" position (car choices )))
      (incf position)
      (setq choices (cdr choices )))
    (dtk-force)
    (unless save-dtk-capitalize
      (dtk-toggle-capitalization))))

;;}}}
)
(defadvice ispell-help (before emacspeak pre act)
  "Speak the help message. "
  (let ((dtk-stop-immediately nil))
    (dtk-speak (documentation 'ispell-help ))))

;;}}}
;;{{{  Advice top-level ispell commands:

(defadvice ispell-buffer (around emacspeak pre act comp)
  "Produce auditory icons for ispell."
  (cond
   ((interactive-p)
    (let ((dtk-stop-immediately nil )
          (voice-lock-mode t)
          (emacspeak-speak-messages nil))
      ad-do-it
      (emacspeak-auditory-icon 'task-done)))
   (t ad-do-it))
  ad-return-value)


(defadvice ispell-region (around emacspeak pre act comp)
  "Produce auditory icons for ispell."
  (cond
   ((interactive-p)
    (let ((dtk-stop-immediately nil )
          (voice-lock-mode t)
          (emacspeak-speak-messages nil))
      ad-do-it
      (emacspeak-auditory-icon 'task-done)))
   (t ad-do-it))
  ad-return-value)

(defadvice ispell-word (around emacspeak pre act comp)
  "Produce auditory icons for ispell."
  (cond
   ((interactive-p)
    (let ((dtk-stop-immediately nil )
          (voice-lock-mode t)
          (emacspeak-speak-messages nil))
      ad-do-it
      (emacspeak-auditory-icon 'task-done)))
   (t ad-do-it))
  ad-return-value)

;;}}}

(provide 'emacspeak-ispell)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; end: 

;;}}}
