;;; $Id: emacspeak-w3.el,v 5.0 1996/11/22 18:04:26 raman Exp $
;;; $Author: raman $ 
;;; Description:  Emacspeak enhancements for W3
;;; Keywords: Emacspeak, W3, WWW, Spoken Output, Dectalk
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

(require 'emacspeak-load-path)
(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'advice)
(require 'dtk-voices)
(require 'emacspeak-speak)
(require 'emacspeak-sounds)
(eval-when (compile)
  (require 'emacspeak-fix-interactive))
(require 'w3-util)
;;{{{  Introduction:
;;; This module extends W3.
;;; Uses voice locking to display WWW pages.

;;}}}
;;{{{  set up the style sheet
(declaim (special w3-auto-image-alt))
(setq w3-auto-image-alt "image %s")
;;}}}
;;{{{  advice for voice locking:

;;; w3 2.3.xx: translate faces back to descriptions
;;; check if color-spec is of the form #XXXXXX
(defsubst  emacspeak-w3-rgb-color-p (color-spec)
  (string-match "#" color-spec))

(defsubst  emacspeak-w3-get-voice-from-face-cache (description)
  (declare (special w3-face-cache
                    x-font-weight-mappings))
  (let ((font nil)
        (weight nil)
        (size nil)
        (family nil)
        (style nil)
        (face
         (car (rassoc description w3-face-cache ))))
    (cond
     ((and (second face)                ;return color
           (not (emacspeak-w3-rgb-color-p (second face ))))
      (intern (second face )))
     ((vectorp (car face ))
      (setq font  (car face ))          ;font descriptor
      (setq family (car  (font-family font))
            weight
            (or
             (cdr-safe
                   (assq (font-weight font )
                         x-font-weight-mappings))
             weight )
            size (font-size font )
            style (font-style font ))
      (when (numberp style) (setq style nil))
      (intern
       (concat 
        (if family (format "%s" family) "paul")
        (and weight (format "-%s" weight))
        (and style (format "-%s" style))
        ;(and size (format "-%s" size ))
        ))))))

(defadvice w3-add-zone (after emacspeak pre  act)
  "Add property 'personality"
  (declare (special dtk-default-voice ))
  (let ((start (ad-get-arg 0))
        (end (ad-get-arg 1))
        (style (emacspeak-w3-get-voice-from-face-cache
         (ad-get-arg 2 ))))
    (when (and style
               (not (eq style dtk-default-voice )))
      (put-text-property start end
                         'personality   style ))))

;;}}}
;;{{{  Helper functions: deciphering forms

;;; Return data about a possible form field 
(defsubst emacspeak-w3-extract-form-field-information ()
  (let ((data  nil))
        (condition-case nil
            (setq data (w3-zone-data (w3-zone-at (point ))))
          ((error nil)))
    (and (eq 'w3form (car data)) data )))

(defsubst emacspeak-w3-extract-form-field-label (data)
  (declare (special w3-form-labels))
  (let
      ((labels w3-form-labels)
       (key nil))
    (setq key (concat
               (w3-input-element-belongs-to data ) ":" 
               (w3-input-element-id data ) ))_
               (cdr (assoc key  labels ))))

;;}}}
;;{{{  Table of summarizers for form fields

(defvar emacspeak-w3-form-field-describe-table nil
  "Table holding types of form fields
and their associated summarizer functions. ")

(defun emacspeak-w3-define-field-summarizer (type function-name )
  "Associate the name of a function that describes
this type of form field.
Type is a string. "
  (declare (special emacspeak-w3-form-field-describe-table ))
  (setq emacspeak-w3-form-field-describe-table
        (cons (list type function-name )
              emacspeak-w3-form-field-describe-table )))

(defun emacspeak-w3-get-field-summarizer  (type)
  "Retrieve function-name string for this voice"
  (declare (special emacspeak-w3-form-field-describe-table ))
  (or  (cadr  (assoc type emacspeak-w3-form-field-describe-table ))
       nil ))

;;}}}
;;{{{  Associate the form field summarizers

(emacspeak-w3-define-field-summarizer  "TEXT"
                                       'emacspeak-w3-summarize-text-field )

(emacspeak-w3-define-field-summarizer  "OPTION"
                                       'emacspeak-w3-summarize-option-field )

(emacspeak-w3-define-field-summarizer  "CHECKBOX"
                                       'emacspeak-w3-summarize-checkbox-field
                                       )

(emacspeak-w3-define-field-summarizer  "RESET"
                                       'emacspeak-w3-summarize-reset-field)

(emacspeak-w3-define-field-summarizer  "SUBMIT"
                                       'emacspeak-w3-summarize-submit-field)

(emacspeak-w3-define-field-summarizer  "RADIO"
                                       'emacspeak-w3-summarize-radio-field )

(emacspeak-w3-define-field-summarizer  "TEXTAREA"
                                       'emacspeak-w3-summarize-textarea-field
                                       )

(emacspeak-w3-define-field-summarizer  "IMAGE"
                                       'emacspeak-w3-summarize-image-field )

(emacspeak-w3-define-field-summarizer  "INT"
                                       'emacspeak-w3-summarize-int-field )
(emacspeak-w3-define-field-summarizer  "PASSWORD"
                                       'emacspeak-w3-summarize-password-field )

;;}}}
;;{{{  define the summarizer functions:

;;; These functions summarize a form field.
;;; They assume point is on a form field and the type has been validated.
;;; Argument is a list containing form field information

(defun emacspeak-w3-summarize-text-field (data)
  "Summarize a text field given the field data."
  (let ((type (nth 2 data))
        (label (emacspeak-w3-extract-form-field-label data ))
	 (name (nth 3 data))
	 (default (nth 4 data))
	 (value  (nth 5 data))
	 (checked (nth 6 data))
	 (size (nth 7 data))
	 (maxl (nth 8 data))
	 (ident (nth 9 data))
	 (options (nth 10 data)))
    (dtk-speak
     (format "Text  field  %s  %s "
             (or label (if (string= "" name )
                 ""
               (format "called %s" name )))
             (if (string= value "")
                 (if (string= default "") ""  default )
               (format "set to %s" value ))))))


(defun emacspeak-w3-summarize-textarea-field (data)
  "Summarize a text field given the field data."
  (let ((type (nth 2 data))
        (name (nth 3 data))
        (label (emacspeak-w3-extract-form-field-label data ))
        (default (nth 4 data))
        (value  (nth 5 data))
        (checked (nth 6 data))
        (size (nth 7 data))
        (maxl (nth 8 data))
        (ident (nth 9 data))
        (options (nth 10 data)))
    (dtk-speak
     (format "Multiline text input  %s  %s"
             (or label
                 (if (string= "" name )
                 ""
               (format "called %s" name )))
             (cond
              ((not (string= "" value))(format "set to %s" value ))
              ((not (string= default ""))
               (format "with default value %s" default ))
              (t ""))))))

(defun emacspeak-w3-summarize-checkbox-field (data)
  "Summarize a checkbox  field given the field data."
  (let((type (nth 2 data))
	 (name (nth 3 data))
         (label (emacspeak-w3-extract-form-field-label data ))
	 (default (nth 4 data))
	 (value  (nth 5 data))
	 (checked (nth 6 data))
	 (size (nth 7 data))
	 (maxl (nth 8 data))
	 (ident (nth 9 data))
	 (options (nth 10 data)))
    (dtk-speak
     (format "Checkbox   %s  %s"
             (or label name)
             (if checked "is on" "is off ")))))

(defun emacspeak-w3-summarize-option-field (data)
  "Summarize a options   field given the field data."
  (let((type (nth 2 data))
	 (name (nth 3 data))
         (label (emacspeak-w3-extract-form-field-label data ))
	 (default (nth 4 data))
	 (value  (nth 5 data))
	 (checked (nth 6 data))
	 (size (nth 7 data))
	 (maxl (nth 8 data))
	 (ident (nth 9 data))
	 (options (nth 10 data)))
    (dtk-speak
     (format"Choose an option %s  %s"
             (or label
                 (if (string= "" name )
                 ""
               (format "for  %s" name )))
             (if (string=  "" default )
                 ""
               (format "default is %s" default ))))))

;;; to handle brain dead nynex forms
(defun emacspeak-w3-summarize-image-field (data)
  "Summarize a image   field given the field data.
Currently, only the NYNEX server uses this."
  (let ((type (nth 2 data))
       (name (nth 3 data))
       (label (emacspeak-w3-extract-form-field-label data ))
       (default (nth 4 data))
       (value  (nth 5 data))
       (checked (nth 6 data))
       (size (nth 7 data))
       (maxl (nth 8 data))
       (ident (nth 9 data))
       (options (nth 10 data)))
    (dtk-speak
     (substring name 1 ))))

(defun emacspeak-w3-summarize-submit-field (data)
  "Summarize a submit   field given the field data."
  (let  ((type (nth 2 data))
       (name (nth 3 data))
       (label (emacspeak-w3-extract-form-field-label data ))
       (default (nth 4 data))
       (value  (nth 5 data))
       (checked (nth 6 data))
       (size (nth 7 data))
       (maxl (nth 8 data))
       (ident (nth 9 data))
       (options (nth 10 data)))
  (if (or (not default )
          (string= default ""))
      (message "Submit Form")
    (message default ))))

(defun emacspeak-w3-summarize-reset-field (data)
  "Summarize a reset   field given the field data."
  (let  ((type (nth 2 data))
       (name (nth 3 data))
       (default (nth 4 data))
       (value  (nth 5 data))
       (checked (nth 6 data))
       (size (nth 7 data))
       (maxl (nth 8 data))
       (ident (nth 9 data))
       (options (nth 10 data)))
  (if (or (not default ) (string= default ""))
      (message "Reset Form")
    (message default ))))

(defun emacspeak-w3-summarize-radio-field (data)
  "Summarize a radio   field given the field data."
  (let ((type (nth 2 data))
	 (name (nth 3 data))
         xs(label (emacspeak-w3-extract-form-field-label data ))
	 (default (nth 4 data))
	 (value  (nth 5 data))
	 (checked (nth 6 data))
	 (size (nth 7 data))
	 (maxl (nth 8 data))
	 (ident (nth 9 data))
	 (options (nth 10 data)))
    (dtk-speak
     (format "Radio button   %s %s"
             (or label value )
             (if checked 
                 "is pressed"
               "is not pressed")))))
(defun emacspeak-w3-summarize-int-field (data)
  "Summarize an  int field given the field data."
  (let ((type (nth 2 data))
        (label (emacspeak-w3-extract-form-field-label data ))
	 (name (nth 3 data))
	 (default (nth 4 data))
	 (value  (nth 5 data))
	 (checked (nth 6 data))
	 (size (nth 7 data))
	 (maxl (nth 8 data))
	 (ident (nth 9 data))
	 (options (nth 10 data)))
    (dtk-speak
     (format "Text  field  %s  %s "
             (or label (if (string= "" name )
                 ""
               (format "called %s" name )))
             (if (string= value "")
                 (if (string= default "") ""  default )
               (format "set to %s" value ))))))


(defun emacspeak-w3-summarize-password-field (data)
  "Summarize a password field given the field data."
  (let ((type (nth 2 data))
        (label (emacspeak-w3-extract-form-field-label data ))
	 (name (nth 3 data))
	 (default (nth 4 data))
	 (value  (nth 5 data))
	 (checked (nth 6 data))
	 (size (nth 7 data))
	 (maxl (nth 8 data))
	 (ident (nth 9 data))
	 (options (nth 10 data)))
    (dtk-speak
     (format "Text  field  %s  %s "
             (or label (if (string= "" name )
                 ""
               (format "called %s" name )))
             (if (string= value "")
                 (if (string= default "") ""  default )
               (format "set to %s" value ))))))

;;}}}
;;{{{  Summarize a form field

(defun emacspeak-w3-summarize-form-field ()
  "Summarizes field under point if any."
  (let* ((data (emacspeak-w3-extract-form-field-information  ))
         (type (nth 2 data ))
         (summarizer(emacspeak-w3-get-field-summarizer type)))
    (cond
     ((and data summarizer )
    (funcall summarizer data))
     (data
      (message "Please define a summarizer function for %s"  type)))))

;;}}}
;;{{{  Actions to a form field

(defadvice w3-follow-link (around emacspeak pre act)
  "Provide feedback on what you did. "
  (let ((data (emacspeak-w3-extract-form-field-information))
        (obuffer (current-buffer))
        (form-field-p nil)
        (this-zone nil)
        (opoint nil))
    (and       data  (setq form-field-p t 
                        opoint (point)))
    ad-do-it
    (cond
     (form-field-p
      (setq data (w3-zone-data (w3-zone-at opoint)))
      (let ((type (nth 2 data))
            (name (nth 3 data))
            (label (emacspeak-w3-extract-form-field-label data ))
            (value (nth 5 data))
            (checked (nth 6 data)))
        (cond
         ((equal "TEXT" type)
          (message "Text field %s set to %s"
                   (or label name)
                   value ))
         ( (equal "RADIO" type)
           (emacspeak-auditory-icon 'button)
           (message  "%s radio button %s"
                     (if checked "pressed" "released")
                     (or label value )))
         ((equal "CHECKBOX" type)
          (emacspeak-auditory-icon 'button)
          (message "%s  %s"
                   (if checked "checked" "unchecked")
                   (or label name ))))))
    ((eq obuffer (current-buffer))
      (emacspeak-speak-line)))
    ad-return-value))

(defadvice w3-revert-form (after emacspeak pre act)
  "Announce that you cleared the form. "
  (dtk-speak "Cleared the form. "))


(defadvice w3-finish-text-entry (after emacspeak pre act )
  "Announce what the field was set to."
  (when (interactive-p)
    (emacspeak-w3-summarize-textarea-field
     (emacspeak-w3-extract-form-field-information ))))

;;}}}
;;{{{  Moving between links:

(defadvice w3-forward-link (after emacspeak pre act)
  "Produce an auditory icon
If on a form field, summarize it."
  (declare (special emacspeak-lazy-message-time))
  (when (interactive-p)
    ;;; force messages 
    (setq emacspeak-lazy-message-time 0)
    (emacspeak-w3-summarize-form-field)
    (emacspeak-auditory-icon 'large-movement)))

(defadvice w3-back-link (after emacspeak pre act)
  "Produce an auditory icon
If on a form field, than summarize it."
  (declare (special emacspeak-lazy-message-time))
  (when (interactive-p )
    (setq emacspeak-lazy-message-time 0)
    (emacspeak-w3-summarize-form-field)
    (emacspeak-auditory-icon 'large-movement)))

;;}}}
;;{{{  Moving through a document:

(defadvice w3-start-of-document (after emacspeak pre act)
  "Produce an auditory icon.
Also speak the first line. "
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'large-movement)))

(defadvice w3-end-of-document (after emacspeak pre act)
  "Produce an auditory icon.
Also speak the first line. "
  (when (interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'large-movement)))
(defadvice w3-scroll-up (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (let ((start (point )))
      (emacspeak-auditory-icon 'scroll)
      (save-excursion
        (forward-line (window-height))
        (emacspeak-speak-region start (point ))))))

(defun emacspeak-w3-browse-page ()
"Browse a WWW page"
  (interactive)
  (emacspeak-audio-annotate-paragraphs)
  (emacspeak-execute-repeatedly 'forward-paragraph))

(declaim (special url-show-status))
(setq url-show-status nil )
(defvar emacspeak-w3-last-progress-indication 0
  "Caches when we last produced a progress auditory icon")
  

(defadvice url-lazy-message (around emacspeak pre act)
  "Provide pleasant auditory feedback about progress"
  (declare (special emacspeak-w3-last-progress-indication ))
  (let ((now (nth 1 (current-time ))))
  (when (> now
           (+ 3 emacspeak-w3-last-progress-indication ))
    (setq emacspeak-w3-last-progress-indication now )
    (emacspeak-auditory-icon 'progress ))))


(declaim (special w3-mode-map))
(define-key w3-mode-map "." 'emacspeak-w3-browse-page)
;;}}}
;;{{{  Moving between buffers:

(defadvice w3-goto-last-buffer (after emacspeak pre act)
                                "Speak the modeline so I know where I am."
                                (when (interactive-p)
                                  (emacspeak-auditory-icon 'select-object )
                                  (emacspeak-speak-mode-line)))

(defadvice w3-quit (after emacspeak pre act)
  "Speak the mode line of the new buffer."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line )))

;;}}}
;;{{{  setting up voice locking:

(defun emacspeak-w3-use-voice-locking(&optional arg) 
  "Tells w3 to start using voice locking.
This is done by setting the w3 variables so that anchors etc are not marked by
delimiters. We then turn on voice-lock-mode. 
Interactive prefix arg does the opposite. "
  (interactive "P")
  (declare (special w3-delimit-links w3-delimit-emphasis
                    w3-echo-link ))
  (setq w3-echo-link 'text)
  (if arg
      (setq w3-delimit-links  'guess 
            w3-delimit-emphasis  'guess)
    (setq w3-delimit-links nil
          w3-delimit-emphasis nil))
  )
(emacspeak-w3-use-voice-locking )

(defadvice w3-fetch (around  emacspeak  act comp )
  "First produce an auditory icon to indicate retrieval.
After retrieval, 
set  voice-lock-mode to t after displaying the buffer,
and then speak the mode-line. "
  (declare (special dtk-punctuation-mode))
  (emacspeak-auditory-icon 'select-object)
  ad-do-it
  (set (make-local-variable 'voice-lock-mode) t)
  (setq dtk-punctuation-mode "some")
  (modify-syntax-entry 10 " ")
  (emacspeak-auditory-icon 'open-object)
  (emacspeak-speak-mode-line ))

;;}}}
(provide 'emacspeak-w3)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; end: 

;;}}}
