;;; $Id: emacspeak-vm.el,v 5.0 1996/11/22 18:04:26 raman Exp $
;;; $Author: raman $ 
;;; Description:  Emacspeak extension to speech enhance vm
;;; Keywords: Emacspeak, VM, Email, Spoken Output, Voice annotations
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
(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'dtk-voices)
(require 'voice-lock)
(require 'dtk-speak)
(require 'emacspeak-sounds)
(require 'emacspeak-speak)
(eval-when (compile)
  (condition-case nil
      (load-library "vm/vm")
    ((error nil ))))
;;{{{  Introduction:
;;; This module extends the mail reader vm.
;;; Uses voice locking for message headers and cited messages

;;}}}
;;{{{ voice locking:

(defvar vm-voice-lock-keywords nil
  "Keywords to highlight in vm")

(defvar vm-summary-voice-lock-keywords
  "Additional expressions to highlight in vm  Summary mode.")

;;;  Set vm-voice-lock-keywords

(setq vm-voice-lock-keywords
      (append vm-voice-lock-keywords
              '(("^From: \\(.*\\)$" 1  'emacspeak-vm-from-voice )
                ("^To: \\(.*\\)$" 1 'emacspeak-vm-to-voice)
                ("^Subject: \\(.*\\)$" 1 'emacspeak-vm-subject-voice)
                ("^|?[a-zA-Z]*>+\\(.*\\)$" 1 'emacspeak-vm-cite-voice )
                )))

(voice-lock-set-major-mode-keywords 'vm-mode      vm-voice-lock-keywords)

(voice-lock-set-major-mode-keywords 'vm-summary-mode
		   vm-summary-voice-lock-keywords)

		  (add-hook 'vm-mode-hook
          (function (lambda ()
                      (setq dtk-punctuation-mode "some")
                      (emacspeak-dtk-sync)
                      (condition-case nil
                          (voice-lock-mode )
                        ((error nil ))))))

;;}}}
;;{{{  vm voices:

(dtk-define-voice 'emacspeak-vm-from-voice  "[:nh]")
(dtk-define-voice 'emacspeak-vm-to-voice  "[:nu]")
(dtk-define-voice 'emacspeak-vm-subject-voice  "[:nb]" )
(dtk-define-voice 'emacspeak-vm-cite-voice  "[:dv sm 40 ri 70 ]")

;;}}}
;;{{{ Advice completions

(defadvice vm-minibuffer-complete-word (around emacspeak pre act)
  "Say what you completed."
  (let ((prior (point ))
        (dtk-stop-immediately t))
    (emacspeak-kill-buffer-carefully "*Completions*")
    ad-do-it
    (let ((completions-buffer (get-buffer "*Completions*")))
    (if (> (point) prior)
      (dtk-speak (buffer-substring prior (point )))
    (when (and completions-buffer
           (window-live-p (get-buffer-window completions-buffer )))
      (save-excursion
        (set-buffer completions-buffer )
        (emacspeak-prepare-completions-buffer)
        (dtk-speak (buffer-string ))))))
    ad-return-value))

(defadvice vm-minibuffer-complete-word-and-exit (around emacspeak pre act)
  "Say what you completed."
  (let ((prior (point ))
        (dtk-stop-immediately t))
    (emacspeak-kill-buffer-carefully "*Completions*")
    ad-do-it
    (let ((completions-buffer (get-buffer "*Completions*")))
    (if (> (point) prior)
      (dtk-speak (buffer-substring prior (point )))
    (when (and completions-buffer
           (window-live-p (get-buffer-window completions-buffer )))
      (save-excursion
        (set-buffer completions-buffer )
        (emacspeak-prepare-completions-buffer)
        (dtk-speak (buffer-string ))))))
    ad-return-value))

;;}}}
;;{{{  Helper functions:

(defun emacspeak-vm-summarize-message ()
  "Summarize the current vm message. "
  (declare (special vm-message-pointer))
  (when vm-message-pointer
    (dtk-stop)
    (let*  ((dtk-stop-immediately t )
            (message (car vm-message-pointer ))
            (from(or (vm-su-full-name message)
                     (vm-su-from message )))
            (subject (vm-so-sortable-subject message ))
            (to(or (vm-su-to-names message)
                   (vm-su-to message )))
            (lines (vm-su-line-count message)))
      (dtk-speak
       (format "%s %s   %s %s "
               (or from "")
               (if (and to (< (length to) 80))
                   (format "to %s" to) "")
               (if subject (format "on %s" subject) "")
               (if lines (format "%s lines" lines) ""))))))

(defun emacspeak-vm-speak-labels ()
  "Speak a message's labels"
  (interactive)
  (declare (special vm-message-pointer))
  (when vm-message-pointer
  (message "Labels: %s"
           (vm-su-labels (car vm-message-pointer )))))

(defun emacspeak-vm-mode-line ()
  "VM mode line information. "
  (declare (special vm-ml-message-attributes-alist
                    vm-ml-message-read vm-ml-message-unread
                    vm-ml-message-new
                    vm-ml-message-number vm-ml-highest-message-number ))
  (when (eq major-mode 'vm-mode)
    (dtk-stop)
    (let ((dtk-stop-immediately nil ))
      (dtk-speak 
       (format "Message %s of %s,    %s %s %s  %s"
               vm-ml-message-number vm-ml-highest-message-number
               (if vm-ml-message-new "new" "")
               (if vm-ml-message-unread "unread" "")
               (if vm-ml-message-read "read" "")
               (mapconcat
                (function (lambda(item)
                            (let ((var (car item))
                                  (value (cadr item )))
                              (cond
                               ((and (boundp var) (eval var ))
                                (if (symbolp value)
                                    (eval value)
                                  value))
                               (t "")))))
                (cdr vm-ml-message-attributes-alist)   " "))))))

;;}}}
;;{{{  Moving between messages

(add-hook 'vm-select-message-hook
 (function (lambda nil 
(emacspeak-vm-summarize-message)
 (emacspeak-auditory-icon 'select-object ))))

;;}}}
;;{{{  Scrolling messages:

(defun emacspeak-vm-locate-subject-line()
  "Locates the subject line in a message being read.
Useful when you're reading a message
that has been forwarded multiple times."
  (interactive)
  (re-search-forward "^Subject:" nil t )
  (emacspeak-speak-line))

(defadvice vm-scroll-forward (after emacspeak pre act)
  "Produce auditory feedback.
Then speak the screenful. "
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (save-excursion
        (let ((start  (point ))
              (window (get-buffer-window (current-buffer ))))
          (forward-line (window-height window))
          (emacspeak-speak-region start (point ))))))

(defadvice vm-scroll-backward (after emacspeak pre act)
  "Produce auditory feedback.
Then speak the screenful. "
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (save-excursion
        (let ((start  (point ))
              (window (get-buffer-window (current-buffer ))))
          (forward-line(-  (window-height window)))
          (emacspeak-speak-region start (point ))))))
(defun emacspeak-vm-browse-message ()
  "Browse an email message --read it paragraph at a time. "
  (interactive)
  (emacspeak-execute-repeatedly 'forward-paragraph ))

(declaim (special vm-mode-map))
(define-key vm-mode-map "." 'emacspeak-vm-browse-message)

;;}}}
;;{{{  vm message attributes

(defadvice emacspeak-speak-mode-line (after emacspeak-vm act)
  "If in vm mode, speak the current message attributes. "
  (emacspeak-vm-mode-line))

;;}}}
;;{{{  deleting and killing

(defadvice vm-delete-message (after emacspeak pre act)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (message "Message discarded.")))

(defadvice vm-undelete-message (after emacspeak pre act)
  "Provide auditory feedback."
  (when (interactive-p)
    (message "Message recovered.")))

(defadvice vm-kill-subject (after emacspeak pre act)
  "Provide auditory feedback. "
  (when (interactive-p)
    (dtk-speak "Killed this thread. ")
    (emacspeak-auditory-icon 'delete-object)))

;;}}}
;;{{{  Sending mail:

(defadvice vm-forward-message (around emacspeak pre act)
  "Provide aural feedback."
  (if (interactive-p)
    (let ((dtk-stop-immediately nil))
      (message "Forwarding message")
      (emacspeak-vm-summarize-message)
      ad-do-it
      (emacspeak-speak-line ))
    ad-do-it)
  ad-return-value )

(defadvice vm-reply (after emacspeak pre act)
  "Provide aural feedback."
  (when (interactive-p)
    (emacspeak-speak-mode-line)))

(defadvice vm-followup (after emacspeak pre act)
  "Provide aural feedback."
  (when (interactive-p)
    (message "Folluwing up")
    (emacspeak-speak-mode-line)))
      
(defadvice vm-reply-include-text (after emacspeak pre act)
  "Provide aural feedback."
  (when (interactive-p)
    (emacspeak-speak-mode-line )))

(defadvice vm-followup-include-text (after emacspeak pre act)
  "Provide aural feedback."
  (when (interactive-p)
    (message "Following up")
    (emacspeak-speak-mode-line )))

(defadvice vm-mail (after emacspeak pre act)
  "Provide aural feedback."
  (when (interactive-p)
    (let ((dtk-stop-immediately nil))
    (message "Composing a message")
    (emacspeak-speak-line ))))

;;}}}
;;{{{ quitting

(defadvice vm-quit (after emacspeak pre act )
  "Provide an auditory icon if requested"
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)))

;;}}}
;;{{{  Keybindings:
(declaim  (special vm-mode-map))
(define-key vm-mode-map "\M-j" 'emacspeak-vm-locate-subject-line)
(define-key vm-mode-map "\M-l" 'emacspeak-vm-speak-labels)

;;}}}
;;{{{ advise searching:
(defadvice vm-isearch-forward (around emacspeak pre act comp)
  "Provide auditory feedback"
  (declare (special vm-message-pointer))
  (cond
   ((interactive-p)
    (let ((orig (point)))
      ad-do-it
      (cond
       ((not (= orig (point)))
        (emacspeak-auditory-icon 'search-hit)
        (emacspeak-speak-line))
       (t (emacspeak-auditory-icon 'search-miss)))))
   (t ad-do-it))
  ad-return-value)


(defadvice vm-isearch-backward (around emacspeak pre act comp)
  "Provide auditory feedback"
  (declare (special vm-message-pointer))
  (cond
   ((interactive-p)
    (let ((orig (point)))
      ad-do-it
      (cond
       ((not (= orig (point)))
        (emacspeak-auditory-icon 'search-hit)
        (emacspeak-speak-line))
       (t (emacspeak-auditory-icon 'search-miss)))))
   (t ad-do-it))
  ad-return-value)

;;}}}

(provide 'emacspeak-vm)
;;{{{  local variables

;;; local variables:
;;; folded-file: t
;;; end: 

;;}}}
