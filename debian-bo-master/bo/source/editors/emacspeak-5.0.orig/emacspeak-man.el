;;; $Id: emacspeak-man.el,v 5.0 1996/11/22 18:04:26 raman Exp $
;;; $Author: raman $ 
;;; DescriptionEmacspeak extensions for man-mode
;;; Keywords:emacspeak, audio interface to emacs man 
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

;;; Copyright (c) 1995 by T. V. Raman 
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
(require 'emacspeak-speak)
(require 'emacspeak-sounds)
(require 'voice-lock)
;;{{{  Introduction:

;;; Provide additional advice to man-mode 

;;}}}
;;{{{  Configure man

;;; Please show it to me when you are ready:
(declaim (special Man-notify))
(setq Man-notify 'bully)
(defvar Man-voice-lock-keywords nil
  "Keywords to highlight in Man mode")
(declaim (special Man-voice-lock-keywords
                  Man-heading-regexp Man-first-heading-regexp
                  Man-see-also-regexp
                  ))
(setq Man-voice-lock-keywords
      (list
              ;; Regexp describing section headers 
	 (cons Man-heading-regexp   'voice-lock-underline-personality)
                      ;;Regexp for SEE ALSO section (or your equiv) 
         (cons Man-see-also-regexp    'voice-lock-italic-personality)
                 ;;Regexp for first heading on a manpage 
         (cons Man-first-heading-regexp     'voice-lock-underline-personality )
                     ;; Regexp matching a references in SEE ALSO 
         ;(cons Man-reference-regexp    'voice-lock-bold-personality)
         ))
(voice-lock-set-major-mode-keywords 'Man-mode    Man-voice-lock-keywords)
;;}}}
;;{{{  advice interactive commands 

(defadvice  Man-mode (after emacspeak pre act )
  "Fixup variables paragraph-start and paragraph-separate.
Also provide an auditory icon"
  (setq paragraph-start "^[\011\012\014]*$"
        paragraph-separate "^[\011\012\014]*$")
  (emacspeak-auditory-icon 'help))
  

(defadvice   Man-goto-section  (after emacspeak pre act )
  "Speak the line"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line )))


(defadvice Man-next-section (after emacspeak pre act )
  "Speak the line"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line )))

(defadvice Man-previous-section (after emacspeak pre act )
  "Speak the line"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line )))

(defadvice Man-goto-see-also-section (after emacspeak pre act )
  "Speak the line"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line )))

(defadvice Man-quit (after emacspeak pre act )
  "Announce buffer that is current"
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line )))

(defadvice manual-entry (after emacspeak pre act )
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-speak-mode-line )))

;;}}}
;;{{{  Additional commands

(defun emacspeak-man-browse-man-page ()
  "Browse the man page --read it a paragraph at a time"
  (interactive)
  (emacspeak-execute-repeatedly 'forward-paragraph))

(declaim (special  Man-mode-map))
(define-key Man-mode-map "." 'emacspeak-man-browse-man-page)
(define-key Man-mode-map "[" 'backward-paragraph)
(define-key Man-mode-map "]" 'forward-paragraph)

;;}}}


(provide  'emacspeak-man)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; end: 

;;}}}
