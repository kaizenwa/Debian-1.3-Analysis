;;; $Id: emacspeak-view.el,v 5.0 1996/11/22 18:04:26 raman Exp $
;;; $Author: raman $ 
;;; DescriptionEmacspeak extensions for view
;;; Keywords:emacspeak, audio interface to emacs, view-mode
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

;;; Provide additional advice to view-mode

;;}}}
;;{{{  Setup view mode to work with emacspeak

;;; restore emacspeak keybindings:
(declaim (special emacspeak-prefix))
(add-hook 'view-mode-hook
          (function (lambda ()
                      (local-unset-key emacspeak-prefix ))))
;;; Generate automatic advise:

(mapcar 'emacspeak-fix-interactive-command-if-necessary
        '(view-buffer
view-file-other-window
view-file
View-search-regexp-backward
view-buffer-other-window
View-search-regexp-forward))

;;}}}
;;{{{ Advise additional interactive commands:

(defadvice View-search-regexp-forward (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (let ((emacspeak-show-point t))
    (emacspeak-speak-line ))
    (emacspeak-auditory-icon 'search-hit)))

(defadvice View-search-regexp-backward (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (let ((emacspeak-show-point t))
    (emacspeak-speak-line ))
    (emacspeak-auditory-icon 'search-hit)))


(defadvice View-search-last-regexp-forward (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (let ((emacspeak-show-point t))
    (emacspeak-speak-line ))
    (emacspeak-auditory-icon 'search-hit)))

(defadvice View-search-last-regexp-backward (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (let ((emacspeak-show-point t))
    (emacspeak-speak-line ))
    (emacspeak-auditory-icon 'search-hit)))


(defadvice view-exit (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defadvice View-scroll-one-more-line (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (emacspeak-speak-line)))


(defadvice View-scroll-lines-forward-set-scroll-size (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (let ((start (point )))
      (emacspeak-auditory-icon 'scroll)
      (save-excursion
        (forward-line (window-height))
        (emacspeak-speak-region start (point ))))))

(defadvice View-scroll-lines-forward (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
      (emacspeak-auditory-icon 'scroll)
      (dtk-speak (emacspeak-get-window-contents))))

(defadvice View-scroll-lines-backward (around  emacspeak pre act comp)
  "provide auditory feedback"
  (cond
   ((interactive-p)
    (let ((buffer (current-buffer)))
      ad-do-it
      (cond
       ((not (eq buffer (current-buffer))) ;we exitted view mode 
        (emacspeak-auditory-icon 'close-object)
        (emacspeak-speak-mode-line))
       (t (emacspeak-auditory-icon 'scroll)
          (dtk-speak (emacspeak-get-window-contents))))))
   (t ad-do-it))
  ad-return-value)

(defadvice View-back-to-mark ( after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (let ((emacspeak-show-point t))
      (emacspeak-speak-line))))

(defadvice View-goto-line (after emacspeak pre act comp)
  "Provide spoken feedback"
  (when (interactive-p)
    (let ((line-number
           (format "line %s"
                   (ad-get-arg 0 )))
          (voice-lock-mode t))
      (put-text-property 0 (length line-number)
                         'personality 'annotation-voice line-number)
    (emacspeak-auditory-icon 'large-movement)
    (dtk-speak
     (concat line-number
             (thing-at-point 'line))))))

;;}}}
(provide  'emacspeak-view)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; end: 

;;}}}
