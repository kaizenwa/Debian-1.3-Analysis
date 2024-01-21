;;; $Id: emacspeak-widget.el,v 5.0 1996/11/22 18:04:26 raman Exp $
;;; $Author: raman $ 
;;; Description: Emacspeak extensions to widgets
;;; Keywords:emacspeak, audio interface to emacs customized widgets
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
(eval-when (compile)
(condition-case nil
    (progn (require 'widget)
           (require 'widget-edit)
           (message "Compiling against widget libraries %s %s"
                    (locate-library "widget")
                    (locate-library "widget-edit")))
  (error
   (message  "Widget libraries not found, widget support may not work correctly."))))
(require 'emacspeak-speak)
(require 'emacspeak-sounds)
;;{{{  Introduction

;;; This module implements the necessary extensions to provide talking
;;; widgets.

;;}}}
;;{{{ Helper functions
(define-widget-keywords :emacspeak-help)
(defsubst emacspeak-widget-at (pt)
  (or (get-text-property pt 'button)
      (get-text-property pt 'field)))

(defsubst emacspeak-widget-type (widget) (car widget ))

;;}}}
;;{{{  define summarizer

(defsubst emacspeak-widget-summarize(widget)
  (when widget
    (let ((emacspeak-help (widget-get widget ':emacspeak-help)))
      (if (and emacspeak-help
               (fboundp emacspeak-help))
          (funcall emacspeak-help widget)
        (emacspeak-widget-default-summarize widget))))
  t)

(defun emacspeak-widget-default-summarize (widget)
  "Fall back summarizer for all widgets"
  (let ((emacspeak-lazy-message-time 0)
        (tag (widget-get widget ':tag ))
        (value (widget-value widget )))
    (when (or tag value                                )
      (dtk-speak
       (format "widget %s %s "
               (or tag "")
               (or value ""))))
    (and (eq major-mode 'w3-mode)
         (w3-speak-summarize-form-field))))

;;}}}
;;{{{  widget-voice --as per Per's suggestion

(declaim (special :emacspeak-help))
;;{{{ editable-list

(defun emacspeak-widget-help-editable-list (widget)
  "Summarize a choice item"
  (let ((value (widget-value widget))
        (tag (widget-get widget ':tag))
        (type (emacspeak-widget-type  widget)))
    (dtk-speak
     (format "%s %s   is %s"
             (if (eq type 'editable-list)
                 "editable list"
             (or type ""))
             (or tag  "")
             (or value "")))))

(widget-put (get 'editable-list 'widget-type)
		:emacspeak-help 'emacspeak-widget-help-editable-list)

;;}}}
;;{{{ choice-item

(defun emacspeak-widget-help-choice-item (widget)
  "Summarize a choice item"
  (let ((value (widget-value widget))
        (tag (widget-get widget ':tag))
        (parent-type (emacspeak-widget-type  (widget-get widget ':parent))))
    (dtk-speak
     (format "%s %s   is %s"
             (cond
              ((eq parent-type 'radio-button) "radio button ")
              ((eq parent-type 'menu-choice) "menu choice ")
              ((eq parent-type 'checkbox) " check box ")
              (t parent-type))
             (or tag  "")
             (cond
              ((eq parent-type 'checkbox)
               (if value "checked" "unchecked"))
              ((eq parent-type 'radio-button)
               (if value " pressed " "not pressed "))
              (t (if value " on " " off ")))))))

(widget-put (get 'choice-item 'widget-type)
		:emacspeak-help 'emacspeak-widget-help-choice-item)

;;}}}
;;{{{ checkbox

(defun emacspeak-widget-help-checkbox (widget)
  "Summarize a checkbox"
  (let ((value (widget-value widget))
        (tag (widget-get widget ':tag)))
    (dtk-speak
     (format "%s %s"
             (or tag "")
             (if value "checked" "unchecked")))))

(widget-put (get 'checkbox 'widget-type)
		:emacspeak-help 'emacspeak-widget-help-checkbox)

;;}}}
;;{{{  menu choice 

(defun emacspeak-widget-help-menu-choice  (widget)
  "Summarize a pull down list"
  (let ((help (widget-get widget ':help-echo))
        (tag (widget-get widget ':tag))
        (value (widget-get widget ':value))
        (type (emacspeak-widget-type widget ))
        (emacspeak-speak-messages nil))
    (dtk-speak
     (format "%s %s  %s is %s"
             (if (eq type 'menu-choice)
                     "menu choice"
             (or type ""))
             (or tag "")
             (or help "")
             (cond
              ((stringp value) value)
              ((numberp value) value)
              (t  (if value "on" "off" )))))))
      

    (widget-put (get 'menu-choice 'widget-type)
		:emacspeak-help 'emacspeak-widget-help-menu-choice)

;;}}}
;;{{{ editable field

(defun emacspeak-widget-help-editable-field (widget)
  "Summarize an editable field"
  (let ((format (widget-get widget ':format))
        (value (widget-value widget))
        (scratch-buffer (get-buffer-create " *dtk-scratch-buffer* ")))
    (setq format
          (when format 
            (save-excursion
              (set-buffer scratch-buffer)
              (unwind-proctect 
               (let ((inhibit-read-only t))
                 (erase-buffer)
                 (insert "edit field")
                 (insert format)
                 (put-text-property (point-min) (point-max) 'read-only nil)
                 (goto-char (point-min))
                 (while (search-forward  "%v" nil t)
                   (replace-match  value))
                 (buffer-string ))
               (setq inhibit-read-only nil)))))
    (dtk-speak
     (format "%s"
             (or format value)))))

(widget-put (get 'editable-field 'widget-type)
		:emacspeak-help 'emacspeak-widget-help-editable-field)

;;}}}
;;{{{  link 

(defun emacspeak-widget-help-link (widget)
  "Summarize a link"
  ;;; temporary w3 hack
  (cond
   ((eq major-mode 'w3-mode)
         (w3-speak-summarize-form-field))
   (t (let ((value (widget-get widget ':value)))
    (dtk-speak
     (format "link to %s"
             (or value "")))))))

(widget-put (get 'link 'widget-type)
		:emacspeak-help 'emacspeak-widget-help-link)

;;}}}
;;{{{ push button 

(defun emacspeak-widget-help-push-button (widget)
  "Summarize a push button"
  (let ((type (emacspeak-widget-type widget))
        (value (widget-value widget)))
    (dtk-speak
     (format "%s  %s"
             (cond
              ((eq type 'push-button) "push button")
              ((eq type 'insert-button) " insert button ")
              ((eq type 'delete-button) " delete button ")
               (t (or  type "")))
             (or value "")))))

(widget-put (get 'push-button 'widget-type)
		:emacspeak-help 'emacspeak-widget-help-push-button)

;;}}}
;;{{{ radio-button-choice

(defun emacspeak-widget-help-radio-button-choice (widget)
  "Summarize a choice item"
  (let ((value (widget-value widget))
        (tag (widget-get widget ':tag))
        (type (emacspeak-widget-type  widget)))
    (dtk-speak
     (format "%s %s   is %s"
             (if (eq type 'radio-button-choice )
                 "radio button choice "
                 (or type ""))
             (or tag  "")
             (or value "")))))

(widget-put (get 'radio-button-choice 'widget-type)
		:emacspeak-help 'emacspeak-widget-help-radio-button-choice)

;;}}}

;;}}}
;;{{{  Widget motion

(defadvice widget-forward (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
  (emacspeak-auditory-icon 'large-movement)
  (emacspeak-widget-summarize (emacspeak-widget-at  (point )))))

(defadvice widget-backward (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
(emacspeak-auditory-icon 'large-movement)
  (emacspeak-widget-summarize (emacspeak-widget-at (point)))))

;;}}}
;;{{{  activating widgets:

(defadvice widget-button-press (around emacspeak pre act comp)
  "Provide auditory feedback"
  (let ((widget (emacspeak-widget-at (ad-get-arg 0))))
    (cond
     (widget                            ; First record some state:
      (let ((pos (ad-get-arg 0))
            (old-position (point))
            (old-value (widget-value widget )))
        (when (eq major-mode 'w3-mode)
          (emacspeak-auditory-icon 'button))
        ad-do-it
        (cond
         ((= old-position (point ))     ;did not move
          (emacspeak-auditory-icon 'button)
          (emacspeak-widget-summarize (emacspeak-widget-at pos)))
         (t  (emacspeak-auditory-icon 'large-movement)
             (or (emacspeak-widget-summarize (emacspeak-widget-at (point)))
                 (emacspeak-speak-line))))))
     (t ad-do-it))
    ad-return-value))

;;}}}
;;{{{ voice lock widget buffers:

(defvar emacspeak-widget-field-personality  'paul-animated
  "Personality for edit fields")

(defvar emacspeak-widget-button-personality 'paul-smooth
  "Personality for buttons")

(defvar emacspeak-widget-documentation-personality 'paul-monotone
  "Personality for documentation")

(defadvice widget-specify-field-update (after emacspeak pre act comp)
  "Voiceify the field"
(put-text-property (ad-get-arg 1) (ad-get-arg 2)
                   'personality emacspeak-widget-field-personality))

(defadvice widget-specify-button (after emacspeak pre act  comp)
  "Voiceify the button"
(put-text-property (ad-get-arg 1) (ad-get-arg 2)
                   'personality emacspeak-widget-button-personality))  

(defadvice widget-specify-doc (after emacspeak pre act comp)
"Voiceify the documentation of a widget"
(put-text-property (ad-get-arg 1) (ad-get-arg 2)
                   'personality emacspeak-widget-documentation-personality))

;;}}}
;;{{{  Interactively summarize a widget and its parents.

(defun emacspeak-widget-summarize-widget-under-point (&optional level)
  "Summarize a widget if any under point.
Optional interactive prefix specifies how many levels to go up from current
widget before summarizing."
  (interactive "P")
  (let ((widget (emacspeak-widget-at (point ))))
    (when(and widget  level)
      (loop for i from 1 to level
             do
            (setq widget (widget-get  widget :parent))))
    (cond
     (widget (emacspeak-widget-summarize widget ))
      (t (message "No widget under point")))))

(defun emacspeak-widget-browse-widget-interactively ()
  "Allows you to browse a widget"
  (interactive)
  (let ((level nil )
        (key nil)
        (continue t))
      (emacspeak-widget-summarize-widget-under-point)
    (while  continue
      (setq key (read-event))
      (cond
       ((= key ?q) (setq continue nil)
        (message "exitting widget browser"))
       ((= key ?.) nil)
       ((= key ?u)
        (if (numberp level)
            (incf level)
          (setq level 1)))
       ((= key ?d)
        (if (> level  0)
            (decf level)
          (message "Leaf widget")))
       (t (read-key-sequence "Press any key to continue")))
      (emacspeak-widget-summarize-widget-under-point level))))

;;}}}
(provide  'emacspeak-widget)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; end: 

;;}}}
