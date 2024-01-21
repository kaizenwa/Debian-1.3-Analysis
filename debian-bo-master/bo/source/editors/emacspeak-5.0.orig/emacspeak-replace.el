;;; $Id: emacspeak-replace.el,v 5.0 1996/11/22 18:04:26 raman Exp $
;;; $Author: raman $ 
;;; Description:  Emacspeak extension for replace.el
;;; Keywords: Emacspeak, Speech feedback, query replace (replace.el)
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
(require 'dtk-voices)
(require 'emacspeak-speak)
(require 'emacspeak-sounds)
(eval-when (compile)
  (load-library "replace"))
;;{{{  Introduction:

;;; This module causes emacs' replacement functions to use voice locking

;;}}}
;;{{{  define personalities

(dtk-define-voice 'emacspeak-replace-personality
                  "[:np :dv ap 142]" )

;;}}}
;;{{{  Advice

(defvar emacspeak-replace-highlight-on nil
  "Flag that says if replace highlight is on.")

(defvar emacspeak-replace-saved-personality nil
  "Value saved before replace-highlight changed the personality. ")

(defvar emacspeak-replace-start nil)
(defvar emacspeak-replace-end nil)

(defadvice query-replace (around emacspeak pre act compile)
  "Stop message from chattering.
 Turn on voice lock temporarily. "
  (declare (special voice-lock-mode ))
  (let ((saved-voice-lock voice-lock-mode)
        (emacspeak-speak-messages nil))
    (dtk-stop)
    (unwind-protect
        (progn
          (setq voice-lock-mode 1)
          (setq emacspeak-replace-start nil 
                emacspeak-replace-end nil 
                emacspeak-replace-highlight-on nil )
          (save-match-data ad-do-it))
      (emacspeak-auditory-icon 'task-done)
      (setq voice-lock-mode saved-voice-lock
            emacspeak-speak-messages t))))

(defadvice replace-highlight (before  emacspeak pre act)
  "Voicify and speak the line containing the replacement. "
  (declare (special emacspeak-replace-highlight-on
                    emacspeak-replace-saved-personality
                    emacspeak-replace-start emacspeak-replace-end))
  (let ((from (ad-get-arg 0))
        (to (ad-get-arg 1 ))
        (dtk-stop-immediately nil))
    (condition-case nil
        (progn 
          (and emacspeak-replace-highlight-on
               emacspeak-replace-start 
               emacspeak-replace-end
               (put-text-property 
                (max emacspeak-replace-start  (point-min))
                (min emacspeak-replace-end   (point-max))
                'personality   emacspeak-replace-saved-personality))
          (setq emacspeak-replace-highlight-on t
                emacspeak-replace-start from
                emacspeak-replace-end  to 
                emacspeak-replace-saved-personality
                (get-text-property  from 'personality))
          (and from to 
               (put-text-property from to 'personality
                                  'emacspeak-replace-personality ))
          (emacspeak-speak-line))
((error nil )))))

(defadvice replace-dehighlight (after emacspeak pre act)
  "Turn off the replacement highlight. "
  (declare (special emacspeak-replace-highlight-on
                    emacspeak-replace-saved-personality
                    emacspeak-replace-start emacspeak-replace-end))
  (condition-case nil
      (progn
  (and emacspeak-replace-highlight-on
       emacspeak-replace-start
       emacspeak-replace-end
       (put-text-property 
        (max emacspeak-replace-start  (point-min))
        (min emacspeak-replace-end (point-max ))
        'personality   emacspeak-replace-saved-personality)
       (setq emacspeak-replace-start nil
             emacspeak-replace-end nil
             emacspeak-replace-highlight-on nil)))
    ((error  nil ))))
    

;;}}}
(provide 'emacspeak-replace)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; end: 

;;}}}
