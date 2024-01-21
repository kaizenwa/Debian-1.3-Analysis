;;; $Id: emacspeak-forms.el,v 5.0 1996/11/22 18:04:26 raman Exp $
;;; $Author: raman $ 
;;; DescriptionEmacspeak extensions for forms-mode 
;;; Keywords:emacspeak, audio interface to emacs forms 
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

;;; Copyright (c) 1996 by T. V. Raman 
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
(require 'dtk-voices)
(require 'emacspeak-fix-interactive)
(require 'emacspeak-speak)
(require 'emacspeak-sounds)
;;{{{  Introduction:

;;; Provide additional advice to forms-mode 

;;}}}
;;{{{ Advise interactive  commands

(defadvice forms-next-record (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (emacspeak-forms-summarize-current-record)))

(defadvice forms-prev-record (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (emacspeak-forms-summarize-current-record)))

(defadvice forms-first-record (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (emacspeak-forms-summarize-current-record)))


(defadvice forms-last-record (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (emacspeak-forms-summarize-current-record)))


(defadvice forms-jump-record (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (emacspeak-forms-summarize-current-record)))

(eval-when (load)
  (emacspeak-fix-interactive-command-if-necessary 'forms-jump-record))

(defadvice forms-search (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'search-hit)
    (emacspeak-forms-summarize-current-record)))

(defadvice forms-exit (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defadvice forms-next-field (around emacspeak pre act comp)
  "Provide auditory feedback."
  (cond
   ((interactive-p)
    ad-do-it
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-forms-speak-field))
   (t ad-do-it))
  ad-return-value)

(defadvice forms-prev-field (after emacspeak pre act comp)
  "Provide auditory feedback."
  (cond
   ((interactive-p)
      ad-do-it
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-forms-speak-field))
   (t ad-do-it))
  ad-return-value)

(defadvice forms-kill-record (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    ))


(defadvice forms-insert-record (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    ))

(defadvice forms-save-buffer (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when  (interactive-p)
    (emacspeak-auditory-icon 'save-object)))

(eval-when (load)
  (emacspeak-fix-interactive-command-if-necessary 'forms-find-file))
;;}}}
;;{{{ Helper functions

(defun emacspeak-forms-summarize-current-record ()
"Summarize the current record by speaking an ntelligent message."
(declare (special forms--current-record forms--total-records
                  forms-file))
(dtk-speak
 (format "Record %s of %s from %s"
         forms--current-record forms--total-records forms-file)))

(dtk-define-voice-alias 'emacspeak-forms-rw-voice 'paul)
(dtk-define-voice-alias 'emacspeak-forms-ro-voice 'annotation-voice)


(defun emacspeak-forms-speak-field ()
  "Speak current form field name and value.
Assumes that point is at the front of a field value."
  (interactive)
  (let ((voice-lock-mode t)
        (name
         (buffer-substring
          (point)
          (or
           (previous-single-property-change (point) 'read-only)
           (point))))
        (value
         (buffer-substring
          (point)
          (or
           (next-single-property-change (point) 'read-only)
           (point)))  ))
    (put-text-property 0 (length name)
                       'personality
                       'emacspeak-forms-ro-voice name)
    (put-text-property 0 (length value )
                       'personality 'emacspeak-forms-rw-voice value)
    (dtk-speak (concat name " " value ))))

      ;;}}}
(provide  'emacspeak-forms)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; end: 

;;}}}
