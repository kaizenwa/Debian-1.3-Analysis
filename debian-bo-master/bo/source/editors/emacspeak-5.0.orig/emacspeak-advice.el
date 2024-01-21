;;; $Id: emacspeak-advice.el,v 5.0 1996/11/22 18:04:26 raman Exp $
;;; $Author: raman $ 
;;; Description:  Core advice forms that make emacspeak work
;;; Keywords: Emacspeak, Speech, Advice, Spoken  output
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

(require 'advice)
(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'dtk-speak)
(require 'emacspeak-speak)
(require 'emacspeak-sounds)
(eval-when (load compile)
  (require 'emacspeak-fix-interactive))
;;{{{  Introduction:

;;; This module defines the advice forms for making the core of Emacs speak
;;; Advice forms that are specific to Emacs subsystems do not belong here!
;;; I violate this at present by advicing completion here. 

;;}}}
;;{{{  advice cursor movement commands to speak

(defadvice next-line (after emacspeak pre act)
  "Speak line that you just moved to. "
  (when (interactive-p)
    (emacspeak-speak-line  )))

(defadvice previous-line (after emacspeak pre act)
  "Speak line that you just moved to. "
  (when (interactive-p) (emacspeak-speak-line  )))

(defadvice forward-word (after emacspeak pre act)
  "Speak the word you just moved to. "
  (when (interactive-p)
    (skip-syntax-forward  " ")
    (emacspeak-speak-word )))

(defadvice backward-word (after emacspeak pre act)
  "Speak the word you just moved to. "
  (when (interactive-p) (emacspeak-speak-word )))

(defadvice beginning-of-buffer (after emacspeak pre act)
  "Speak the line. "
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line  )))

(defadvice end-of-buffer (after emacspeak pre act)
  "Speak the line. "
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line   )))

(defadvice back-to-indentation (after emacspeak pre act)
  "Speak the entire line. "
  (when (interactive-p) (emacspeak-speak-line  )))

(defadvice lisp-indent-line (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-speak-line )))
(defadvice tab-to-tab-stop (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-speak-current-column)))






(defadvice forward-sentence (after emacspeak pre act)
  "Speak  sentence  after moving. "
  (when (interactive-p) (emacspeak-speak-sentence    )))

(defadvice backward-sentence (after emacspeak pre act)
  "Speak  sentence  after moving. "
  (when (interactive-p) (emacspeak-speak-sentence    )))

(defadvice forward-sexp (around emacspeak pre act)
  "Speak sexp after moving.
  If you move by more than a line, just speak the target line."
  (if (interactive-p)
      (let ((start (point))
            (same-line nil))
        ad-do-it
        (emacspeak-auditory-icon 'large-movement)
        (skip-syntax-forward " ")
        (setq same-line (count-lines start (point)))
        (cond
         ((> same-line 1)
  (emacspeak-speak-line))
         (t (emacspeak-speak-sexp))))
  ad-do-it)
  ad-return-value)

(defadvice backward-sexp (around  emacspeak pre act )
  "Speak sexp  after moving.
  If you move more than a line,
  only speak the target line. "
  (if   (interactive-p)
      (let ((start (point))
            (same-line nil))
        ad-do-it
        (emacspeak-auditory-icon 'large-movement)
        (setq same-line (count-lines (point) start ))
        (cond 
         ((> same-line 1) (emacspeak-speak-line))
         (t (emacspeak-speak-region start (point )))))
    ad-do-it)
  ad-return-value)


(defadvice forward-paragraph (after emacspeak pre act )
  "Speak the paragraph. "
  (when(interactive-p)
    (emacspeak-speak-paragraph))
  )

(defadvice backward-paragraph (after emacspeak pre act )
  "Speak the paragraph. "
  (when(interactive-p) 
    (emacspeak-speak-paragraph  nil )))

(defadvice forward-list (around  emacspeak pre act)
  "Speak the list.
  If you moved more than a line,  
  only speak the target line. "
  (if   (interactive-p)
      (let ((start (point))
            (same-line nil))
        ad-do-it
        (setq same-line (count-lines (point) start ))
        (cond 
         ((> same-line 1) (emacspeak-speak-line))
         (t (emacspeak-speak-region start (point )))))
    ad-do-it))

(defadvice backward-list (around  emacspeak pre act)
  "Speak the list.
  If you moved more than a line,
  just speak the target line. "
  (if   (interactive-p)
      (let ((start (point))
            (same-line nil))
        ad-do-it
        (setq same-line (count-lines (point) start ))
        (cond 
         ((> same-line 1) (emacspeak-speak-line))
         (t (emacspeak-speak-region start (point )))))
    ad-do-it))


(defadvice up-list (around  emacspeak pre act)
  "Speak the list.
  If you moved more than a line,
  only speak the target line "
  (if   (interactive-p)
      (let ((start (point))
            (same-line nil))
        ad-do-it
        (setq same-line (count-lines (point) start ))
        (cond 
         ((> same-line 1) (emacspeak-speak-line))
         (t (emacspeak-speak-region start (point )))))
    ad-do-it)
  )
(defadvice backward-up-list (around  emacspeak pre act)
  "Speak the list.
  If you moved more than a line,
  only speak the target line "
  (if   (interactive-p)
      (let ((start (point))
            (same-line nil))
        ad-do-it
        (setq same-line (count-lines (point) start ))
        (cond 
         ((> same-line 1) (emacspeak-speak-line))
         (t (emacspeak-speak-region start (point )))))
    ad-do-it)
  )



(defadvice down-list (around  emacspeak pre act)
  "Speak the list.
  If you moved more than a line,
  only speak the target line. "
  (if   (interactive-p)
      (let ((start (point))
            (same-line nil))
        ad-do-it
        (setq same-line (count-lines (point) start ))
        (cond 
         ((> same-line 1) (emacspeak-speak-line))
         (t (emacspeak-speak-region start (point )))))
    ad-do-it))

(defadvice forward-page (after emacspeak pre act)
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (emacspeak-speak-page )))
(defadvice backward-page (after emacspeak pre act)
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (emacspeak-speak-page )))
(defadvice scroll-up (after emacspeak pre act comp)
  "Speak the next screenful"
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (dtk-speak (emacspeak-get-window-contents))))

(defadvice scroll-down (after emacspeak pre act comp)
  "Speak the screenful"
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (dtk-speak (emacspeak-get-window-contents))))

(defadvice  beginning-of-defun (after emacspeak pre act)
  "Speak the line."
  (when (interactive-p)
    (emacspeak-speak-line)))

(defadvice  end-of-defun (after emacspeak pre act)
  "Speak the line."
  (when (interactive-p)
    (emacspeak-speak-line)))

;;}}}
;;{{{  Advice deletion commands:

(defadvice delete-backward-char (before emacspeak pre act)
  "Speak character you're deleting. " 
  (when (interactive-p )
    (emacspeak-speak-this-char (preceding-char ))
    (dtk-tone 500 30)
    (dtk-force)))

(defadvice delete-char (before emacspeak pre act)
  "Speak character you're deleting. "
  (when (interactive-p )
    (emacspeak-speak-char t)
    (dtk-tone 500 30)
    (dtk-force))
  )

(defadvice backward-delete-char-untabify (before emacspeak pre act)
  "Speak character you're deleting. " 
  (when (interactive-p )
    (emacspeak-speak-this-char(preceding-char ))
    (dtk-tone 500 30)
    (dtk-force)))

(defadvice backward-delete-char (before emacspeak pre act)
  "Speak character you're deleting. " 
  (when (interactive-p )
    (emacspeak-speak-this-char (preceding-char ))
    (dtk-tone 500 30)
    (dtk-force)))

(defadvice kill-word (before emacspeak pre act )
  "Speak word before killing it. "
  (when (interactive-p )
    (save-excursion
      (skip-syntax-forward " ")
    (emacspeak-speak-word 1 ))
    (dtk-tone 500 30)
    (dtk-force)))

(defadvice backward-kill-word (before emacspeak pre act)
  "Speak word before killing it. "
  (when (interactive-p )
    (let ((start (point )))
      (save-excursion
        (forward-word -1)
        (emacspeak-speak-region (point) start )
        )
      (dtk-tone 500 30)
      (dtk-force)))
  )
;;; Large deletions also produce auditory icons if possible
(defadvice kill-line(before emacspeak pre act)
  "Speak line before killing it. " 
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (emacspeak-speak-line 1)
    (dtk-tone 500 30)
    (dtk-force)))

(defadvice kill-sexp (before emacspeak pre act )
  "Speak the sexp you killed. "
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (emacspeak-speak-sexp 1 )
    (dtk-tone 500 30)
    (dtk-force)))
(defadvice kill-sentence (before emacspeak pre act )
  "Speak the line  you killed. "
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (emacspeak-speak-line 1 )
    (dtk-tone 500 30)
    (dtk-force)))

(defadvice delete-blank-lines (before   emacspeak  pre act )
  "Provide auditory feedback"
  (when (interactive-p)
    (let (thisblank singleblank)
      (save-match-data 
    (save-excursion
      (beginning-of-line)
      (setq thisblank (looking-at "[ \t]*$"))
      ;; Set singleblank if there is just one blank line here.
      (setq singleblank
	    (and thisblank
		 (not (looking-at "[ \t]*\n[ \t]*$"))
		 (or (bobp)
		     (progn (forward-line -1)
			    (not (looking-at "[ \t]*$"))))))))
    (cond
     ((and thisblank singleblank )
      (message "Deleting current blank line"))
     (  thisblank (message "Deleting surrounding  blank lines"))
                  (t (message "Deleting possible subsequent blank lines"))))))

;;}}}
;;{{{  advice insertion commands to speak.
;;; there appears to be a bug in newer emacsuns e.g. 19.30 when using
;;; completion.el
;;;We'll fix it hear for the general good.
(defadvice completion-separator-self-insert-command (around fix-bug pre act comp)
    (condition-case nil
        ad-do-it
      (error (set-syntax-table cmpl-saved-syntax)
       (emacspeak-self-insert-command last-input-char ))))

(defadvice completion-separator-self-insert-autofilling (around fix-bug pre act comp)
    (condition-case nil
        ad-do-it
      (error (set-syntax-table cmpl-saved-syntax)
       (emacspeak-self-insert-command last-input-char ))))

(defadvice completion-separator-self-insert-autofilling (after emacspeak
                                                               pre act)
  "Say what you inserted. "
  (declare (special emacspeak-word-echo))
  (when (and emacspeak-word-echo  (interactive-p ))
    (let ((dtk-stop-immediately nil ))
      (save-excursion
        (condition-case nil
            (progn (backward-char 2)
                   (if (string= "w"
                                (char-to-string (char-syntax
                                                 (preceding-char ))))
                       (backward-char 1))
                   (emacspeak-speak-word nil  ))
          (error nil )
          ))))
  )

(defadvice completion-separator-self-insert-command (after emacspeak act comp)
  "Speak char after inserting it. "
  (declare (special emacspeak-character-echo))
  (when (and emacspeak-character-echo  (interactive-p))
    (emacspeak-speak-this-char
     (preceding-char ))))

(defadvice quoted-insert  (after emacspeak pre act )
  "Speak the character that was inserted."
(when (interactive-p)
(emacspeak-speak-this-char
(preceding-char ))))

;;}}}
;;{{{  advice minibuffer to speak

(defadvice previous-history-element (after emacspeak pre act)
  "Speak the history element just inserted"
  (when (interactive-p)
    (dtk-stop)
    (emacspeak-speak-line )))

(defadvice next-history-element (after emacspeak  pre act)
  "Speak the history element just inserted"
  (when (interactive-p)
    (dtk-stop)
    (emacspeak-speak-line )))

(defadvice previous-matching-history-element (after emacspeak pre act)
  "Speak the history element just inserted"
  (when (interactive-p)
    (dtk-stop)
    (emacspeak-speak-line )))

(defadvice next-matching-history-element (after emacspeak pre act)
  "Speak the history element just inserted"
  (when (interactive-p)
    (dtk-stop)
    (emacspeak-speak-line )))

(defvar emacspeak-last-message nil
  "Holds the last output generated by the Emacs 'message function. ")




(defvar emacspeak-lazy-message-time 0
  "Records when we last spoke a message. ")


(defadvice message (around  emacspeak pre act)
  "Speak the message. "
  (declare (special emacspeak-last-message
                    emacspeak-speak-messages emacspeak-lazy-message-time))
  (let ((dtk-stop-immediately t ))
    ad-do-it
    (setq emacspeak-last-message ad-return-value )
    (when (and   emacspeak-speak-messages ; speaking messages 
                 (/= emacspeak-lazy-message-time;; previous message not recent 
                     (setq emacspeak-lazy-message-time  (nth 1 (current-time)))))
      ;; so we really need to speak it 
      (dtk-with-punctuation-mode "all" 
                                 (dtk-speak ad-return-value)))))
(defadvice message (after emacspeak-flush pre act ) (dtk-force ))
(defvar emacspeak-ange-ftp-last-percent nil
  "Cache the last percentage that emacspeak spoke.")


(defadvice ange-ftp-process-handle-hash (around emacspeak pre act )
  "Jibber intelligently"
  (declare (special emacspeak-ange-ftp-last-percent
                    ange-ftp-last-percent ))
  (let ((emacspeak-speak-messages nil ))
    ad-do-it
    (when (or (null emacspeak-ange-ftp-last-percent)
        (>= (abs (- ange-ftp-last-percent emacspeak-ange-ftp-last-percent ))
           5))
      (setq emacspeak-ange-ftp-last-percent ange-ftp-last-percent )
      (emacspeak-auditory-icon 'progress)
      (dtk-speak
       (format " %s percent" ange-ftp-last-percent )))))
  
;;; Also play a warning auditory icon 
(defadvice signal (before emacspeak pre act compile)
  "Speak the error message as well.
  Produces an auditory icon if possible."
  (let ((dtk-stop-immediately nil ))
    (emacspeak-auditory-icon 'warn-user)
    (dtk-speak
     (format "%s. %s" 
             (or (get (ad-get-arg 0) 'error-message)
                 "Peculiar error. ")
             (mapconcat 'identity
                        (ad-get-arg 1)
                        ", ")))))

(defadvice error (before emacspeak pre act)
  "Speak the error message.
  Also produces an auditory icon if possible."
  (let ((dtk-stop-immediately nil ))
    (emacspeak-auditory-icon 'warn-user)
                    (dtk-speak (apply 'format  (ad-get-args 0)))))

(defadvice read-from-minibuffer (around emacspeak pre act)
  "Prompt using speech as well. "
  (let((prompt (ad-get-arg 0))
       (default (ad-get-arg 1 )))
    (dtk-speak
     (format "%s  %s"
             prompt
             (if  default
                 (format "Default: %s" default)
               "")))
    ad-do-it
    (dtk-speak ad-return-value )
    ad-return-value))

(defadvice read-no-blanks-input (around emacspeak pre act)
  "Prompt using speech as well. "
  (let ((prompt (ad-get-arg 0))
        (default  (ad-get-arg 1 )))
    (dtk-speak
     (format "%s  %s"
             prompt
             (if  default 
                 (format "Default: %s" default)
               "")))
    ad-do-it
    (dtk-speak ad-return-value )
    ad-return-value))


(defadvice read-minibuffer (around emacspeak pre act)
  "Prompt using speech as well. "
  (let ((prompt (ad-get-arg 0))
        (default  (ad-get-arg 1 )))
    (dtk-speak 
     (format "%s %s"
             prompt 
             (if default 
                 (format "Default %s" default)
               " ")))
    ad-do-it
    (dtk-speak ad-return-value )
    ad-return-value ))

(defadvice y-or-n-p (around emacspeak pre act )
  "Use speech when prompting.
  Produce an auditory icon if possible."
  (emacspeak-auditory-icon 'ask-short-question )
  (dtk-speak (format "%s  y or n? " (ad-get-arg  0 )))
  ad-do-it
  (cond
   (ad-return-value
    (emacspeak-auditory-icon 'y-answer )
    (dtk-say "y"))
   (t (emacspeak-auditory-icon  'n-answer )
      (dtk-say "n" )))
  ad-return-value )

(defadvice yes-or-no-p (around emacspeak pre act )
  "Use speech when prompting.
  Produce an auditory icon as well. "
  (emacspeak-auditory-icon 'ask-question)
  (dtk-speak (format "%s  yes or no? " (ad-get-arg  0 )))
  ad-do-it
  (cond
   (ad-return-value
    (emacspeak-auditory-icon 'yes-answer )
    (dtk-say "yes"))
   (t (emacspeak-auditory-icon  'no-answer )
      (dtk-say "no" )))
  ad-return-value )

;;}}}
;;{{{  advice various input functions to speak:

(defadvice completing-read (around emacspeak pre act )
  "Prompt using speech. "
  (let ((dtk-stop-immediately t )
        (prompt (ad-get-arg 0))
        (initial (ad-get-arg 4 )))
    (dtk-speak 
     (format "%s %s"
             (or prompt " ")
             (or initial " ")))
    ad-do-it
    (dtk-speak (format "%s" ad-return-value ))
    ad-return-value ))

(defadvice read-buffer(around emacspeak pre act )
  "Prompt using speech as well. "
  (let ((prompt (ad-get-arg 0))
        (default (ad-get-arg 1 )))
    (dtk-speak
     (format "%s %s" 
             prompt
             (or default " ")))
    ad-do-it
    (dtk-speak ad-return-value)
    ad-return-value)) 

(defadvice read-command(around emacspeak pre act )
  "Prompt using speech as well. "
  (let ((prompt (ad-get-arg 0)))
    (dtk-speak prompt)
    ad-do-it
    (dtk-speak (format "%s" ad-return-value))
    ad-return-value))

(defadvice read-key-sequence(around emacspeak pre act )
  "Prompt using speech as well. "
  (let ((prompt (ad-get-arg 0)))
    (dtk-speak prompt)
    ad-do-it
    (dtk-speak (format "%s" ad-return-value))
    ad-return-value))

(defadvice read-string(around emacspeak pre act )
  "Prompt using speech as well. "
  (let ((prompt (ad-get-arg 0 ))
        (default (ad-get-arg 1 )))
    (dtk-speak 
     (format "%s %s" 
             prompt
             (or default " ")))
    ad-do-it
    (dtk-speak (format "%s" ad-return-value))
    ad-return-value))

(defadvice read-variable(around emacspeak pre act )
  "Prompt using speech as well. "
  (let ((prompt (ad-get-arg 0)))
    (dtk-speak prompt)
    ad-do-it
    (dtk-speak (format "%s" ad-return-value))
    ad-return-value))

(defadvice read-file-name (around emacspeak pre act )
  "Prompt using speech as well. "
  (let ((directory (ad-get-arg 1))
        (default (ad-get-arg 2 )))
    (dtk-speak
     (format "%s %s %s"
             (ad-get-arg 0 )
             (or directory "")
             (if default 
                 (format "Default %s" default ) 
               "")))
    ad-do-it
    (dtk-speak ad-return-value)
    ad-return-value))

;;}}}
;;{{{  advice completion functions to speak:

(defadvice completion-list-mode (after emacspeak pre act )
  "Emacspeak splits chunks based on both white space and punctuations
in completion buffers"
  (dtk-chunk-on-white-space-and-punctuations))
;;; remove the default emacs banner from the completions buffer,
;; and produce an auditory icon

(defsubst emacspeak-prepare-completions-buffer()
  (goto-char (point-min))
        (forward-line 3)
        (delete-region (point-min) (point))
        (emacspeak-auditory-icon 'help))

(defadvice dabbrev-expand (around emacspeak pre act)
  "Say what you completed."
  (let ((prior (point ))
        (dtk-stop-immediately t))
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


(defadvice minibuffer-complete-word (around emacspeak pre act)
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

(defadvice minibuffer-complete (around emacspeak pre act)
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

(defadvice lisp-complete-symbol (around emacspeak pre act)
  "Say what you completed."
  (let ((prior (point ))
        (dtk-stop-immediately nil))
    ad-do-it
    (when (> (point) prior)
      (dtk-speak (buffer-substring prior (point ))))
    ad-return-value))

(defadvice complete (around emacspeak pre act)
  "Say what you completed."
  (let ((emacspeak-speak-messages nil)
        (emacspeak-last-message nil))
    ad-do-it
    (when  (interactive-p)
      (dtk-speak
       (format "%s %s"
               (save-excursion (backward-char 1)
                               (sexp-at-point ))
               (or emacspeak-last-message "")))
      ad-return-value)))

(defadvice  next-completion (after emacspeak  pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (dtk-speak (emacspeak-get-current-completion-from-completions))))


(defadvice  previous-completion (after emacspeak  pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (dtk-speak
     (emacspeak-get-current-completion-from-completions ))))

(defadvice choose-completion (after emacspeak pre act )
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line )))
(defadvice minibuffer-complete-and-exit (before emacspeak pre act
                                                comp)
  "Provide an auditory icon"
  (when (interactive-p)
    (emacspeak-auditory-icon 'button)))

(defadvice tmm-menubar (before emacspeak pre act comp)
  "Provide an auditory icon"
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object )))

;;}}}
;;{{{  Advice comint:

(add-hook 'comint-mode-hook
          (function (lambda ()
                      (modify-syntax-entry 10 ">"  ))))

(defadvice comint-output-filter (around emacspeak pre act)
  "Make comint speak its output. "
  (declare (special emacspeak-comint-autospeak))
  (let ((prior (point ))
        (dtk-stop-immediately nil))
    ad-do-it 
    (when (and  emacspeak-comint-autospeak
                (window-live-p
                 (get-buffer-window (process-buffer (ad-get-arg 0)))))
      (emacspeak-dtk-sync)
      (condition-case nil
          (emacspeak-speak-region prior (point ))
        (error (emacspeak-auditory-icon 'scroll)
               (dtk-stop ))))
    ad-return-value))

(defadvice comint-dynamic-complete (around emacspeak pre act)
  "Say what you completed."
  (let ((prior (point ))
        (dtk-stop-immediately nil))
    (emacspeak-kill-buffer-carefully "*Completions*")
    ad-do-it
    (let ((completions-buffer (get-buffer "*Completions*"))
          (dtk-stop-immediately t))
      (if (> (point) prior)
          (dtk-speak (buffer-substring prior (point )))
        (when (and completions-buffer
                   (window-live-p (get-buffer-window completions-buffer )))
          (save-excursion
            (set-buffer completions-buffer )
            (dtk-speak (buffer-string ))))))
    ad-return-value))

(defadvice comint-next-input (after emacspeak pre act)
  "Speak the line. "
  (when (interactive-p)
    (emacspeak-speak-line )))

(defadvice comint-next-matching-input (after emacspeak pre act)
  "Speak the line. "
  (when (interactive-p)
    (emacspeak-speak-line )))


(defadvice comint-previous-input (after emacspeak pre act)
  "Speak the line. "
  (when (interactive-p)
    (emacspeak-speak-line )))

(defadvice comint-previous-matching-input (after emacspeak pre act)
  "Speak the line. "
  (when (interactive-p)
    (emacspeak-speak-line )))

(defadvice comint-send-input (after emacspeak pre act)
  "Flush any ongoing speech"
  (when (interactive-p)
    (dtk-stop)))

(defadvice comint-previous-prompt (after emacspeak pre act )
  "Provide spoken feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (if (eolp)
        (emacspeak-speak-line)
    (emacspeak-speak-line 1))))

(defadvice comint-next-prompt (after emacspeak pre act )
  "Provide spoken feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (if (eolp)
        (emacspeak-speak-line)
    (emacspeak-speak-line 1))))
(defadvice comint-dynamic-list-input-ring (after emacspeak pre act)
  "Provide auditory feedback"
  (message  "Switch to the other window to browse the input history. "))

(defadvice comint-kill-output (after emacspeak pre act )
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (message "Nuked output of last command ")))

(defadvice comint-quit-subjob (after emacspeak pre act )
  "Provide auditory feedback"
  (when (interactive-p)
    (message "Sent quit signal to subjob ")))

(defadvice comint-stop-subjob (after emacspeak pre act )
  "Provide auditory feedback"
  (when (interactive-p)
    (message "Stopped the subjob")))

(defadvice comint-interrupt-subjob (after emacspeak pre act )
  "Provide auditory feedback"
  (when (interactive-p)
    (message "Interrupted  the subjob")))

(defadvice comint-kill-input (before emacspeak pre act )
  "Provide spoken feedback"
  (when (interactive-p)
    (let ((pmark (process-mark (get-buffer-process (current-buffer)))))
    (when  (> (point) (marker-position pmark))
      (emacspeak-auditory-icon 'delete-object )
	(emacspeak-speak-region  pmark (point))))))

(defadvice comint-dynamic-list-filename-completions (after emacspeak pre act )
  "Provide auditory feedback"
  (when (interactive-p)
    (message "Switch to the completions window to browse the possible
completions for filename at point")))

;;}}}
;;{{{  Advice centering and filling commands:

(defadvice center-line (after emacspeak pre act)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (message"Centered current line")))

(defadvice center-region (after emacspeak pre act)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (message"Centered current region")))

(defadvice center-paragraph (after emacspeak pre act)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (message"Centered current paragraph")))

(defadvice fill-paragraph (after emacspeak pre act)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'fill-object )
    (message "Filled current paragraph")))

(defadvice lisp-fill-paragraph (after emacspeak pre act)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'fill-object )
    (message "Filled current paragraph")))


(defadvice indent-sexp  (after emacspeak pre act)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'fill-object )
    (message "Indented current s expression ")))
(defadvice fill-region (after emacspeak pre act)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'fill-object )
    (message "Filled current region")))

;;}}}

;;{{{  vc:
;;; helper function: find out vc version:

;;; guess the vc version number from the variable used in minor mode alist 
(defsubst emacspeak-vc-get-version-id ()
  (declare (special vc-mode ))
  (let ((id vc-mode ))
    (cond
     ((and vc-mode
           (stringp vc-mode))
           (substring id 5  nil ))
     (t " "))))

;;; Advice for most used vc functions:
(defadvice vc-toggle-read-only (around emacspeak pre act) 
  "Provide auditory feedback. "
  (cond
   ((interactive-p)
    (let ((message (format  "Checking %s version %s "
           (if buffer-read-only  "out previous " " in new  ")
           (emacspeak-vc-get-version-id))))
  (if buffer-read-only 
      (emacspeak-auditory-icon 'open-object )
    (emacspeak-auditory-icon 'close-object))
  ad-do-it
  (message message )))
   (t ad-do-it ))
  ad-return-value )

(defadvice vc-next-action (around  emacspeak pre act)
  "Provide auditory feedback. "
  (cond
   ((interactive-p)
    (let ((message (format  "Checking %s version %s "
                            (if buffer-read-only  "out previous " " in new  ")
                            (emacspeak-vc-get-version-id))))
      (if buffer-read-only 
          (emacspeak-auditory-icon 'close-object)
        (emacspeak-auditory-icon 'open-object ))
      ad-do-it
      (message message)))
   (t ad-do-it ))
  ad-return-value )

(defadvice vc-revert-buffer (after emacspeak pre act)
  "Provide auditory feedback."
  (when (interactive-p  )
    (emacspeak-auditory-icon 'open-object)))

(defadvice vc-finish-logentry (after emacspeak pre act)
  "Provide auditory feedback. "
  (when (interactive-p)
    (emacspeak-auditory-icon  'close-object)
    (message "Checked   in  version %s "
           (emacspeak-vc-get-version-id))))

(progn
  (require 'vc)
  (emacspeak-fix-interactive-command-if-necessary 'vc-create-snapshot)
  (emacspeak-fix-interactive-command-if-necessary 'vc-retrieve-snapshot))
  


;;}}}
;;{{{  misc functions that have to be hand fixed:

(defadvice not-modified (after emacspeak pre act )
"Provide an auditory icon"
(when (interactive-p)
(if (ad-get-arg 0)
(emacspeak-auditory-icon 'modified-object )
(emacspeak-auditory-icon 'unmodified-object))))
(defadvice comment-region (after emacspeak pre act )
  "Provide spoken feedback."
  (when (interactive-p)
    (message "Commented region containing %s lines"
             (count-lines (point) (mark)))))

(defadvice bury-buffer (after emacspeak pre act)
  "Announce the buffer that becomes current. "
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line )))

(defadvice save-buffer (before emacspeak pre act)
  "Produce an auditory icon if possible. "
  (when(and  (interactive-p)
             (buffer-modified-p ))
    (emacspeak-auditory-icon 'save-object)))

(defadvice kill-region (after emacspeak pre act)
  "Indicate region has been killed. 
  Use an auditory icon if possible. "
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object )
    (message "Killed region ")))
(defadvice completion-kill-region (after emacspeak pre act)
  "Indicate region has been killed. 
  Use an auditory icon if possible. "
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object )
    (message "Killed region ")))
(defadvice kill-ring-save (after emacspeak pre act)
  "Indicate that region has been copied to the kill ring.
  Produce an auditory icon if possible."
  (when (interactive-p ) 
    (emacspeak-auditory-icon 'mark-object )
    (message "region copied to kill ring ")))


(defadvice find-file (after emacspeak-icon pre act )
  "Play an auditory icon if possible. "
  (when (interactive-p) 
    (emacspeak-auditory-icon 'open-object)))

;;; These functions have to be advised by hand:
(defadvice kill-buffer (before emacspeak pre act)
  "Speak the prompt, please:"
  (when (interactive-p )
    (let ((dtk-stop-immediately nil ))
      (dtk-speak (format "Kill buffer:  default  %s" 
                         (buffer-name))))))

(defadvice kill-buffer (after emacspeak pre act)
  " Produce an auditory icon to indicate closing of an object.
  Then indicate current buffer by 
  speaking  the modeline." 
  (when (interactive-p )
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defadvice other-window (after emacspeak pre act )
  "Speak modeline.
  Indicate change of selection with an auditory icon 
  if possible. "
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line)))

(defadvice move-to-window-line (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line )))
(defadvice execute-extended-command (before emacspeak pre act)
  "Prompt using speech. "
  (dtk-speak "Execute command: "))

(defadvice rename-buffer  (around emacspeak pre act)
  "Provide spoken feedback"
  (cond
   ((interactive-p)
    (message "Rename buffer to new name ")
    ad-do-it
    (emacspeak-speak-mode-line))
   (t ad-do-it )))


(defadvice switch-to-buffer (before existing-buffers-only pre act)
  "When called interactively switch to existing buffers only, unless 
  when called with a prefix argument."
  (interactive 
   (list
    (let ((dtk-stop-immediately t))
      (read-buffer "Switch to buffer: " (other-buffer) 
                   (null current-prefix-arg))))))

(defadvice switch-to-buffer  (after emacspeak pre act)
  "Speak the modeline.
  Indicate change of selection with 
  an auditory icon if possible. " 
  (when (interactive-p )
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line)))


(defadvice switch-to-buffer-other-window  (after emacspeak pre act)
  "Speak the modeline.
  Indicate change of selection with 
  an auditory icon if possible. " 
  (when (interactive-p )
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line)))

(defadvice local-set-key (before  emacspeak pre act)
  "Prompt using speech"
  (interactive
   (list
    (read-key-sequence "Locally bind key:")
    (read-command "To command:" )))
  )

(defadvice global-set-key (before  emacspeak pre act)
  (interactive
   (list
    (read-key-sequence "Globally  bind key:")
    (read-command "To command:" ))))

(defadvice local-unset-key (before  emacspeak pre act)
  (when (interactive-p)
    (dtk-speak "Press key sequence to unset. ")
    ))

(defadvice global-unset-key (before  emacspeak pre act)
  (when (interactive-p)
    (dtk-speak "Press key sequence to unset. ")
    ))
(defadvice describe-function (after emacspeak pre act)
  "Speak the help"
  (when (interactive-p) (emacspeak-speak-help )))

(defadvice key-description (around emacspeak pre act )
  (let ((emacspeak-scratch (get-buffer-create  " *dtk-scratch-buffer* "))
        (ctrl-regexp "C-\\(.\\)"))
    ad-do-it
    (save-excursion
      (set-buffer emacspeak-scratch )
      (erase-buffer)
      (insert  (format "%s" ad-return-value ))
      (goto-char (point-min))
      (save-match-data
        (while (search-forward "SPC"  nil t )
          (replace-match "Space"))
        (while (search-forward "ESC"  nil t )
          (replace-match "escape"))
        (goto-char (point-min))
        (while (re-search-forward ctrl-regexp  nil t )
          (replace-match "Control-\\1 "))
        )
      (setq ad-return-value (buffer-string )))
    ad-return-value))

(defadvice goto-mark (after emacspeak pre act )
  "Speak the line.
  Indicate movement with an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon  'large-movement)
    (emacspeak-speak-line )))

(defadvice exchange-point-and-mark (after emacspeak pre act)
  "Speak the line
  Indicate large movement with an auditory icon if possible.
Auditory highlight indicates position of point."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement )
    (ems-set-personality-temporarily  (point) (1+ (point))
                                                  'paul-animated
    (emacspeak-speak-line))))

(defadvice newline (before emacspeak pre act)
  "Speak the previous line if line echo is on. 

  See command \\[emacspeak-toggle-line-echo]. "
  (declare (special emacspeak-line-echo ))
  (when (and emacspeak-line-echo 
	     (interactive-p))
    (emacspeak-speak-line ))
  )
(defadvice newline-and-indent (before emacspeak pre act)
  "Speak the previous line if line echo is on. 

  See command \\[emacspeak-toggle-line-echo]. "
  (declare (special emacspeak-line-echo ))
  (when (and emacspeak-line-echo 
	     (interactive-p))
    (emacspeak-speak-line ))
  )
;;; Shut up when keyboard-quit is pressed

(defadvice keyboard-quit (before emacspeak pre act)
  "Stop speech first. "
  (dtk-stop ))

(defadvice delete-indentation (after emacspeak pre act)
  "Speak the line. "
  (when (interactive-p) (emacspeak-speak-line)))
(defadvice eval-last-sexp (after emacspeak pre act)
  "Also speaks the result of evaluation."
 (let ((dtk-chunk-separator-syntax " .<>()$\"\'"))
  (dtk-speak 
   (format "%s" ad-return-value ))))

(defadvice eval-expression (after emacspeak pre act)
  "Also speaks the result of evaluation."
  (let ((dtk-chunk-separator-syntax " .<>()$\"\'"))
  (dtk-speak 
   (format "%s" ad-return-value ))))

(defadvice shell (after emacspeak pre act )
  "announce switching to shell mode.
  Provide an auditory icon if possible. "
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object )
    (emacspeak-dtk-sync)
    (emacspeak-speak-line)))
(defadvice save-buffers-kill-emacs (after emacspeak pre act)
  "Play an auditory icon"
  (when (interactive-p )
    (emacspeak-auditory-icon 'quit)))
;;{{{  composing mail 

(defadvice mail (after emacspeak pre act)
  "Give some auditory feedback. "
  (emacspeak-auditory-icon 'open-object)
  (let ((emacspeak-speak-messages nil))
    (save-excursion
      (beginning-of-buffer)
      (emacspeak-speak-line))))
  

(defadvice mail-other-window (after emacspeak pre act)
  "Give some auditory feedback. "
  (emacspeak-auditory-icon 'open-object)
  (let ((emacspeak-speak-messages nil))
    (save-excursion
      (beginning-of-buffer)
      (emacspeak-speak-line))))

(defadvice mail-other-frame (after emacspeak pre act)
  "Give some auditory feedback. "
  (emacspeak-auditory-icon 'open-object)
(let ((emacspeak-speak-messages nil))
  (save-excursion
    (beginning-of-buffer)
    (emacspeak-speak-line))))

(defadvice mail-subject (after emacspeak pre act)
  "Speak the subject line."
  (emacspeak-speak-line ))

(defadvice mail-cc   (after emacspeak pre act)
  "Speak the cc  line."
  (emacspeak-speak-line ))

(defadvice mail-bcc (after emacspeak pre act)
  "Speak the subject line."
  (emacspeak-speak-line ))

(defadvice mail-to (after emacspeak pre act)
  "Speak the subject line."
  (emacspeak-speak-line ))

(defadvice mail-signature  (after emacspeak pre act)
  "Announce you signed the message"
  (message "Signed your message"))


(defadvice mail-send-and-exit (after emacspeak pre act)
  "Speak the modeline of active buffer."
  (when (interactive-p)
    (emacspeak-speak-mode-line )))
;;}}}

(defadvice goto-line (after emacspeak pre act)
  "Speak the line. "
  (when (interactive-p)
    (emacspeak-speak-line )))

(defadvice find-tag (after emacspeak pre act)
  "Speak the line please. "
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line )))

(defadvice tags-loop-continue (after emacspeak pre act)
  "Speak the line please. "
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line )))

(defadvice call-last-kbd-macro (around emacspeak pre act)
  "Provide spoken feedback."
  (cond
   ((interactive-p)
    (message "Started executing keyboard macro")
    ad-do-it
    (message "Done "))
   (t ad-do-it))
  ad-return-value )

(defadvice kbd-macro-query (after emacspeak pre act)
  "Announce yourself"
  (when (interactive-p)
    (message "Will prompt at this point in macro")))
(defadvice start-kbd-macro (before emacspeak pre act)
  "Announce yourself."
  (when (interactive-p)
    (dtk-speak "Started defining a keyboard macro.")))

(defadvice end-kbd-macro (after emacspeak pre act)
  "Announce yourself."
  (when (interactive-p)
    (dtk-speak "Finished defining keyboard macro. ")))

;;; yOU DONT WANT TO SUSPEND EMACS WITHOUT CONFIRMATION
(defadvice suspend-emacs (around emacspeak pre act)
  "Ask for confirmation. "
  (let ((confirmation (yes-or-no-p "Do you want to suspend emacs ")))
    (cond
     (confirmation 
      (message "Suspending Emacs ")
      ad-do-it)
     (t (message "Not suspending emacs. Continuing ")))))

(defadvice  downcase-region (after emacspeak pre act)
  "Give spoken confirmation."
  (when (interactive-p) 
    (message "Downcased region")))

(defadvice  upcase-region (after emacspeak pre act)
  "Give spoken confirmation."
  (when (interactive-p) 
    (message "Upcased  region")))

(defadvice narrow-to-region (after emacspeak pre act)
  "Announce yourself."
  (when (interactive-p)
    (message "Narrowed editing region ")))

(defadvice narrow-to-page  (after emacspeak pre act)
  "Announce yourself."
  (when (interactive-p)
    (message "Narrowed editing region to current page ")))

(defadvice widen (after emacspeak pre act)
  "Announce yourself."
  (when (interactive-p)
    (message "You can now edit the entire buffer ")))


(defadvice bookmark-set (after emacspeak pre act)
  "Announce yourself."
  (when (interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (message "Set bookmark ")))

(defadvice bookmark-yank-word (after emacspeak pre act )
  "Speak what has been yanked so far"
  (when (interactive-p)
    (emacspeak-auditory-icon 'yank-object )
    (emacspeak-speak-line )))

(defadvice bookmark-insert-current-bookmark (after emacspeak pre act )
  "Speak what has been yanked so far"
  (when (interactive-p)
    (emacspeak-auditory-icon 'yank-object )
    (emacspeak-speak-line )))

(defadvice bookmark-insert-current-file-name (after emacspeak pre act )
  "Speak what has been yanked so far"
  (when (interactive-p)
    (emacspeak-auditory-icon 'yank-object )
    (emacspeak-speak-line )))

(defadvice  bookmark-jump (after emacspeak pre act)
  "Announce what happened."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line )))

(defadvice delete-other-windows (after emacspeak pre act)
  "Provide spoken feedback."
  (when (interactive-p)
    (message "Deleted all other windows")
    (emacspeak-speak-mode-line)))

(defadvice split-window-vertically (after emacspeak pre act)
  "Provide spoken feedback."
  (when (interactive-p)
    (message "Split window vertically, current window has %s lines "
             (window-height))
    (emacspeak-speak-mode-line)))
(defadvice delete-window (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line ))  )

(defadvice shrink-window (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (message "Current window has %s lines  and %s columns"
             (window-height ) (window-width))))

(defadvice shrink-window-if-larger-than-buffer (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (message "Current window has %s lines  and %s columns"
             (window-height ) (window-width))))

(defadvice balance-windows (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (message "Current window has %s lines  and %s columns"
             (window-height ) (window-width))))

(defadvice split-window-horizontally (after emacspeak pre act)
  "Provide spoken feedback."
  (when (interactive-p)
    (message "Split window horizontally, current window has %s columns "
             (window-width))
    (emacspeak-speak-mode-line)))

(defadvice load-library (after   emacspeak pre act) 
  "Announce what you're doing."
  (let ((dtk-stop-immediately nil))
    (message "Loading  %s" (ad-get-arg 0))))
(defadvice transpose-chars (after emacspeak pre act )
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'yank-object)
    (emacspeak-speak-char  t)))

(defadvice transpose-lines (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'yank-object )
    (emacspeak-speak-line )))
(defadvice transpose-words (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'yank-object )
    (emacspeak-speak-word )))

(defadvice transpose-sexps (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'yank-object )
    (emacspeak-speak-sexp )))
(defadvice open-line (after emacspeak pre act )
  "Provide auditory feedback"
  (when (interactive-p)
    (let ((count (ad-get-arg 0)))
      (emacspeak-auditory-icon 'open-object)
      (message "Opened %s blank line%s"
               (if (= count 1) "a" count)
               (if (= count 1 ) "" "s")))))

(defadvice abort-recursive-edit (after emacspeak pre act comp)
  "Provide  auditory feedback"
  (when (interactive-p)
    (message "Aborting recursive edit")))

(defadvice undo  (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (ems-set-personality-temporarily
 (max (point-min) (1- (point)))
                                     (min (point-max) (1+ (point)))
                                     'harry 
                                     (emacspeak-speak-line ))
    (if (buffer-modified-p)
        (emacspeak-auditory-icon 'modified-object)
      (emacspeak-auditory-icon 'unmodified-object ))))

;;}}}
;;{{{  Emacs server

(defadvice server-start (after emacspeak pre act )
  "Provide auditory confirmation"
  (when (interactive-p)
    (emacspeak-auditory-icon 'task-done)))


(defadvice server-edit (after emacspeak pre act )
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-speak-mode-line )))
;;}}}
;;{{{  list buffers 

(defadvice list-buffers (after emacspeak pre act )
"Provide auditory feedback"
(when (interactive-p)
  (other-window 1)
  (emacspeak-speak-mode-line)
(emacspeak-auditory-icon 'task-done)))

(defadvice buffer-menu (after emacspeak pre act )
"Provide auditory feedback"
(when (interactive-p)
(emacspeak-auditory-icon 'task-done)
(message "Displayed list of buffers in other window")))

;;{{{  buffer manipulation commands 

(defadvice Buffer-menu-delete-backwards (after emacspeak pre act)
"Provide auditory feedback"
(when (interactive-p)
(emacspeak-auditory-icon 'delete-object)
(emacspeak-speak-line )))



(defadvice Buffer-menu-delete (after emacspeak pre act)
  "Provide spoken and auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (emacspeak-speak-line )))
(defadvice Buffer-menu-mark (after emacspeak pre act)
  "Provide spoken and auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (emacspeak-speak-line )))


(defadvice Buffer-menu-quit (after emacspeak pre act)
  "Speak the modeline of the newly visible buffer."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defadvice Buffer-menu-save (after emacspeak pre act)
"Provide auditory feedback"
(when (interactive-p)
(emacspeak-auditory-icon 'save-object)
(emacspeak-speak-mode-line )))

(defadvice Buffer-menu-select (after emacspeak pre act)
"Provide auditory feedback"
(when (interactive-p)
(emacspeak-auditory-icon 'select-object)
(emacspeak-speak-mode-line )))

(defadvice Buffer-menu-unmark (after emacspeak pre act)
"Provide auditory feedback"
(when (interactive-p)
(emacspeak-auditory-icon 'deselect-object)
(emacspeak-speak-line )))

(defadvice Buffer-menu-backup-unmark (after emacspeak pre act)
"Provide auditory feedback"
(when (interactive-p)
(emacspeak-auditory-icon 'deselect-object)
(emacspeak-speak-line )))
(defadvice Buffer-menu-execute (after emacspeak pre act )
"Provide auditory feedback"
(when (interactive-p)
(emacspeak-auditory-icon 'task-done)))

(defadvice Buffer-menu-toggle-read-only (after emacspeak pre act )
"Provide auditory feedback"
(when (interactive-p)
(emacspeak-speak-line )))

(defadvice Buffer-menu-not-modified (after emacspeak pre act )
  "Provide auditory feedback "
  (when (interactive-p)
    (emacspeak-speak-line)
    (if (ad-get-arg 0)
        (emacspeak-auditory-icon 'modified-object )
      (emacspeak-auditory-icon 'unmodified-object))))

(defadvice Buffer-menu-visit-tags-table (before emacspeak pre act )
"Provide auditory feedback"
(when (interactive-p)
(message "Visiting tags table on current line")))

;;}}}
;;{{{  display buffers 

(defadvice Buffer-menu-1-window (after emacspeak pre act)
  "Announce the newly selected buffer."
  (when (interactive-p )
    (emacspeak-speak-mode-line )
    (emacspeak-auditory-icon 'select-object)))

(defadvice Buffer-menu-2-window (after emacspeak pre act)
  "Announce the newly selected buffer."
  (when (interactive-p )
    (emacspeak-speak-mode-line )
    (emacspeak-auditory-icon 'select-object)))

(defadvice Buffer-menu-this-window (after emacspeak pre act)
  "Announce the newly selected buffer."
  (when (interactive-p )
    (emacspeak-speak-mode-line )
    (emacspeak-auditory-icon 'select-object)))
(defadvice Buffer-menu-other-window (after emacspeak pre act)
  "Provide auditory feedback"
  (when (interactive-p )
    (emacspeak-auditory-icon 'open-object)
(emacspeak-speak-mode-line)))

;;}}}

;;}}}
;;{{{  avoid chatter when byte compiling etc 

(defadvice byte-compile-file  (around emacspeak pre act)
  "Announce one message, quietly compile, and announce termination.
   Produce an auditory icon if possible. "
  (let ((emacspeak-speak-messages nil))
    (dtk-speak "Byte compiling. ")
    ad-do-it 
    (emacspeak-auditory-icon 'task-done)
    (dtk-speak "Done byte compiling. ")))

;;}}}
;;{{{  Stop talking if activity

(defadvice beginning-of-line (before emacspeak pre act)
  "Stop speech first. "
  (when (interactive-p) (dtk-stop )))

(defadvice end-of-line (before emacspeak pre act)
  "Stop speech first. "
  (when (interactive-p)  (dtk-stop )))

(defadvice recenter (before emacspeak pre act)
  "Stop speech first. "
  (when (interactive-p) (dtk-stop )))

;;}}}
;;{{{  yanking and popping

(defadvice yank (after emacspeak pre act)
  "Say what you yanked.
   Produce an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'yank-object )
    (emacspeak-speak-region (mark) (point))))

(defadvice yank-pop (after emacspeak pre act)
  "Say what you yanked.
  Also produce an auditory icon if possible. "
  (when (interactive-p )
    (emacspeak-auditory-icon 'yank-object)
    (emacspeak-speak-region (point) (mark))))

;;}}}
;;{{{  simple searching:

(defadvice search-forward (around emacspeak pre act)
  "Prompt using speech"
  (cond
   ((interactive-p )
    (dtk-speak "Search forward for   ")
    ad-do-it
    (ems-set-personality-temporarily
     (match-beginning 0) (match-end 0) 'harry
     (emacspeak-speak-line))
    (if ad-return-value
        (emacspeak-auditory-icon 'search-hit)
      (emacspeak-auditory-icon 'search-miss)))
   (t ad-do-it))
  ad-return-value)

(defadvice search-backward (before emacspeak pre act)
  "Prompt using speech"
(cond
   ((interactive-p )
    (dtk-speak "Search backward  for ")
    ad-do-it
    (ems-set-personality-temporarily
     (match-beginning 0) (match-end 0) 'harry
     (emacspeak-speak-line))
    (if ad-return-value
        (emacspeak-auditory-icon 'search-hit)
      (emacspeak-auditory-icon 'search-miss)))
   (t ad-do-it))  
  )

;;}}}
;;{{{  customize isearch:

;;{{{  temporarily disable message advice during searches. 

(add-hook 'isearch-mode-hook 
          (function (lambda () 
                      (setq emacspeak-speak-messages nil))))
(add-hook 'isearch-mode-end-hook
          (function (lambda ()
                      (setq emacspeak-speak-messages t ))))

;;}}}
;;{{{  Advice isearch-search to speak

(defadvice isearch-search (after emacspeak pre act)
  "Speak the search hit.
  Produce auditory icons if possible. "
  (dtk-speak 
   (format "Searching %s  for %s" 
           (if isearch-forward "forward" "backward" )
           isearch-string))
  (when (sit-for 0.5)
    (emacspeak-auditory-icon 'search-hit)
    (ems-set-personality-temporarily 
     (point) 
     (if  isearch-forward 
         (- (point) (length isearch-string ))
       (+ (point) (length isearch-string )))
     'harry
     (emacspeak-speak-line nil ))))

;;}}}

;;}}}
;;{{{  marking objects produces auditory icons 

(defadvice set-mark-command (after emacspeak pre act)
  "Produce an auditory icon if possible. "
  (when (interactive-p )
    (emacspeak-auditory-icon 'mark-object )
    (ems-set-personality-temporarily (point) (1+ (point))
                                     'paul-animated
                                       (emacspeak-speak-line ))))

(defadvice pop-global-mark (after emacspeak pre act)
  "Produce an auditory icon if possible. "
  (when (interactive-p )
    (emacspeak-auditory-icon 'mark-object )
    (ems-set-personality-temporarily (point) (1+ (point))
                                     'paul-animated
                                       (emacspeak-speak-line ))
    (emacspeak-speak-mode-line)))

(defadvice mark-defun (after emacspeak pre act)
  "Produce an auditory icon if possible. "
  (when (interactive-p )
    (emacspeak-auditory-icon 'mark-object)))

(defadvice mark-whole-buffer (after emacspeak pre act)
  "Produce an auditory icon if possible. "
  (when (interactive-p )
    (emacspeak-auditory-icon 'mark-object)))

(defadvice mark-paragraph (after emacspeak pre act)
  "Produce an auditory icon if possible. "
  (when (interactive-p )
    (emacspeak-auditory-icon 'mark-object)))

(defadvice mark-page (after emacspeak pre act)
  "Produce an auditory icon if possible. "
  (when (interactive-p )
    (emacspeak-auditory-icon 'mark-object)))
(defadvice mark-word (after emacspeak pre act)
  "Produce an auditory icon if possible. "
  (when (interactive-p )
    (emacspeak-auditory-icon 'mark-object)))

(defadvice mark-perl-function (after emacspeak pre act)
  "Produce an auditory icon if possible. "
  (when (interactive-p )
    (emacspeak-auditory-icon 'mark-object)))

(defadvice mark-sexp (after emacspeak pre act)
  "Produce an auditory icon if possible. "
  (when (interactive-p )
    (emacspeak-auditory-icon 'mark-object)))

(defadvice mark-end-of-sentence (after emacspeak pre act)
  "Produce an auditory icon if possible. "
  (when (interactive-p )
    (emacspeak-auditory-icon 'mark-object)))
;;}}}
;;{{{  emacs registers

(defadvice copy-to-register (before emacspeak pre act)
  "Acknowledge the copy"
  (when (interactive-p)
    (let ((start (ad-get-arg 1))
          (end (ad-get-arg 2 ))
          (register (ad-get-arg 0)))
      (message "Copied %s characters to register %c"
               (abs (- start end )) register ))))

(defadvice jump-to-register (after emacspeak pre act)
  "Speak the line you jumped to. "
  (when (interactive-p) (emacspeak-speak-line )))
(defadvice insert-register (after emacspeak pre act )
  "Speak the  first line of the inserted text. "
  (when (interactive-p)
    (emacspeak-auditory-icon 'yank-object)
    (emacspeak-speak-line )))

(defadvice window-configuration-to-register (after emacspeak pre act )
  "Provide auditory feedback"
  (when (interactive-p)
    (message "Copied window configuration to register %c"
             (ad-get-arg 0 ))))

;;}}}
;;{{{  Modify syntax entries for modes where necessary:

(add-hook 'text-mode-hook
          (function (lambda ()
                      (modify-syntax-entry 10 " "))))

;;}}}
(provide 'emacspeak-advice)

;;{{{ end of file 

;; local variables:
;; folded-file: t
;; end: 

;;}}}
