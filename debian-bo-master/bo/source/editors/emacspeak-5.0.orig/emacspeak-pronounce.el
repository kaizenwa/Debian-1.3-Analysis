;;; $Id: emacspeak-pronounce.el,v 5.0 1996/11/22 18:04:26 raman Exp $
;;; $Author: raman $ 
;;; Description: Emacspeak pronunciation dictionaries
;;; Keywords:emacspeak, audio interface to emacs customized pronunciation
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
(require 'thingatpt)
(require 'emacspeak-sounds)
;;{{{  Introduction

;;;This module implements user customizable pronunciation dictionaries
;;;for emacspeak. Custom pronunciations can be defined per file, per
;;;directory and/or per major mode. Emacspeak maintains a persistent
;;;user dictionary upon request and loads these in new emacspeak
;;;sessions. This module implements the user interface to the custom
;;;dictionary as well as providing the internal API used by the rest
;;;of emacspeak in using the dictionary.
;;;Algorithm: 

;;;The persistent dictionary is a hash table where the hash keys are
;;;filenames, directory names, or major-mode names. The hash values
;;;are association lists defining the dictionary. Users of this module
;;;can retrieve a dictionary made up of all applicable association
;;;lists for a given file. 

;;}}}
;;{{{  Dictionary structure:

(defvar emacspeak-pronounce-dictionaries (make-hash-table )
"Hash table holding emacspeak's persistent pronunciation dictionaries.
Keys are either filenames, directory names, or major mode names.
Values are alists containing string.pronunciation pairs. ")

(defsubst emacspeak-pronounce-set-dictionary (key pr-alist)
(declare (special emacspeak-pronounce-dictionaries))
(when (stringp key)
(setq key (intern key )))
(setf  (gethash key emacspeak-pronounce-dictionaries) pr-alist ))

(defsubst emacspeak-pronounce-get-dictionary (key)
(declare (special emacspeak-pronounce-dictionaries))
(when (stringp key)
(setq key (intern key )))
(cl-gethash key emacspeak-pronounce-dictionaries))

(defun emacspeak-pronounce-add-dictionary-entry  (key string pronunciation)
  "Add pronunciation pair string.pronunciation to specified key in the
pronunciation dictionary."
  (declare (special emacspeak-pronounce-dictionaries ))
  (let ((dict  (emacspeak-pronounce-get-dictionary key)))
    (cond 
     (dict 
      (setf dict
            (cons (cons string pronunciation) dict))
      (emacspeak-pronounce-set-dictionary key dict ))
     (t (emacspeak-pronounce-set-dictionary key 
                                            (list (cons string pronunciation )))))))

;;}}}
;;{{{  Composing and applying dictionaries:

;;; Composing a dictionary results in the return of a hash table that
;;; contains the applicable string.pronunciation pairs for a given
;;; buffer.
;;; Applying a pronunciation table results in the strings being
;;; globally replaced by the defined pronunciations.
;;; Case is handled similarly to vanila emacs behavior.

(defun emacspeak-pronounce-compose-pronunciation-table  (&optional buffer)
  "Composes a pronunciation table  for buffer --default is current
buffer."
  (setq buffer (or buffer (current-buffer )))
  (let* ((table (make-hash-table))
         (filename (buffer-file-name buffer))
         (directory (and filename 
                         (file-name-directory  filename)))
         (mode  (save-excursion 
                  (set-buffer buffer)
                  major-mode))
         (file-alist (and filename 
                          (emacspeak-pronounce-get-dictionary filename)))
         (dir-alist (and directory
                         (emacspeak-pronounce-get-dictionary directory)))
         (mode-alist (emacspeak-pronounce-get-dictionary mode)))
    (loop for element in mode-alist 
          do 
          (setf (gethash
 (intern (car element))
table)
                (cdr element )))
    (loop for element in dir-alist 
          do 
          (setf (gethash
 (intern (car element))
table)
                (cdr element )))
    (loop for element in file-alist 
          do 
          (setf (gethash
 (intern (car element))
table)
                (cdr element )))
    table))
(defvar emacspeak-pronounce-pronunciation-personality
'paul-surprised
"Personality used when speaking  things that have a pronunciation
applied.")


(defsubst emacspeak-pronounce-apply-pronunciations (pronunciation-table )
  "Applies pronunciations specified in pronunciation table to current
buffer. Modifies text in buffer. "
  (save-match-data 
    (save-excursion 
      (loop for  key  being the hash-keys  of pronunciation-table  
            do 
            (let ((word (symbol-name key))
                  (pronunciation (cl-gethash  key pronunciation-table )))
              (goto-char (point-min))
              (while (search-forward  word nil t)
                (replace-match  pronunciation nil t  )
                (put-text-property 
                 (match-beginning 0)
                 (+ (match-beginning 0) (length pronunciation))
                 'personality 
                 emacspeak-pronounce-pronunciation-personality)))))))

;;}}}
;;{{{  loading, clearing  and saving dictionaries 

(defvar emacspeak-pronounce-dictionaries-file  nil 
"File that holds the persistent emacspeak pronunciation
dictionaries.")

(setq emacspeak-pronounce-dictionaries-file (expand-file-name "~/.emacspeak-dictionary"))

(defun emacspeak-pronounce-save-dictionaries  ()
  "Writes out the persistent emacspeak pronunciation dictionaries."
  (interactive)
  (declare (special emacspeak-pronounce-dictionaries ))
  (let ((filename (read-file-name
                   "Save pronunciation dictionaries to file: "
                   default-directory
                   emacspeak-pronounce-dictionaries-file ))
        (buffer nil ))
    (setq buffer (find-file-noselect filename))
    (save-excursion 
      (set-buffer buffer)
      (erase-buffer)
      (loop for key being the hash-keys  of emacspeak-pronounce-dictionaries
            do
            (insert 
             (format "(emacspeak-pronounce-set-dictionary '%S\n '%S )\n"
                      key 
                      (emacspeak-pronounce-get-dictionary key ))))
(goto-char (point-min))
(while (search-forward ")" nil t)
(replace-match ")\n" nil t))
      (save-buffer))))
(defvar emacspeak-pronounce-dictionaries-loaded nil 
"Indicates if dictionaries already loaded.")

(defun emacspeak-pronounce-load-dictionaries  ()
  "Load pronunciation dictionaries"
  (interactive)
(declare (special emacspeak-pronounce-dictionaries-loaded))
  (let ((filename (read-file-name
                   "Load pronunciation dictionaries from file: "
                   default-directory
                   emacspeak-pronounce-dictionaries-file )))
    (load-file filename)
(setq emacspeak-pronounce-dictionaries-loaded t)))

(defun emacspeak-pronounce-clear-dictionaries ()
"Clear all current pronunciation dictionaries."
(interactive)
(declare (special emacspeak-pronounce-dictionaries ))
(when (yes-or-no-p 
"Do you really want to nuke all currently defined dictionaries?")
(setq emacspeak-pronounce-dictionaries
(make-hash-table ))
(emacspeak-pronounce-refresh-pronunciations)))

;;}}}
;;{{{  Front end to define pronunciations:

(defvar emacspeak-pronounce-pronunciation-keys 
'(("file" . "file")
("directory" . "directory")
("mode" . "mode"))
"Pronunciations can be defined for these kinds of things. ")

(defun emacspeak-pronounce-define-pronunciation ()
  "Interactively define entries in the pronunciation dictionaries.
First loads any persistent dictionaries if not already loaded. "
  (interactive)
  (declare (special emacspeak-pronounce-dictionaries-loaded))
  (let 
      ((key nil)
       (word nil)
       (pronunciation nil)
       (key-type (completing-read  "Define pronunciation that is specific to: "
                                   emacspeak-pronounce-pronunciation-keys nil t )))
    (cond 
     ((string= key-type "file")
      (setq key (buffer-file-name))
      (or key 
          (error "Current buffer is not associated with a file."))
      (setq key (intern key)))
     ((string= key-type "directory")
      (setq key 
            (or
             (condition-case nil 
                 (file-name-directory (buffer-file-name ))
               ((error nil )))
             default-directory))
      (or key (error "No directory associated with current buffer"))
      (setq key (intern key)))
     ((string= key-type "mode")
      (setq key
            major-mode)
      (or key (error "No major mode found for current buffer")))
     (t (error "Cannot define pronunciations with key type %s" key-type)))
    (setq word (read-from-minibuffer
                (format "Define pronunciation in %s for: " key)))
    (setq pronunciation 
          (read-from-minibuffer
           (format "Pronounce %s as: " word)))
    (when (and (not emacspeak-pronounce-dictionaries-loaded)
               (y-or-n-p "Load pre existing  pronunciation dictionaries first"))
      (emacspeak-pronounce-load-dictionaries))
    (emacspeak-pronounce-add-dictionary-entry key word pronunciation)
    (when (y-or-n-p "Refresh pronunciations in this buffer")
      (funcall 'emacspeak-pronounce-refresh-pronunciations))))

;;}}}
;;{{{ Turning dictionaries on and off on a per buffer basis

(defun emacspeak-pronounce-toggle-use-of-dictionaries ()
  "Toggles use of pronunciation dictionaries in current buffer.
Pronunciations can be dfined on a per file, per directory and/or per
mode basis.
Pronunciations are activated on a per buffer basis.
Turning on the use of pronunciation dictionaries results in emacspeak
composing a pronunciation table based on the currently defined
pronunciation dictionaries.
After this, the pronunciations will be applied whenever text in the
buffer is spoken."
  (interactive)
  (declare (special emacspeak-pronounce-pronunciation-table))
  (cond 
   ((not (boundp 'emacspeak-pronounce-pronunciation-table)) ;first time
    (set (make-local-variable 'emacspeak-pronounce-pronunciation-table)
         (emacspeak-pronounce-compose-pronunciation-table))
    (emacspeak-auditory-icon 'on)
    (message
     "Emacspeak pronunciation dictionaries are now active in this buffer"))
   ( emacspeak-pronounce-pronunciation-table ;already on --turn it off
     (setq emacspeak-pronounce-pronunciation-table nil)
     (emacspeak-auditory-icon 'off)
     (message
 "Emacspeak pronunciation dictionaries no longer active in this buffer"))
   (t                                   ;turn it on 
    (setq emacspeak-pronounce-pronunciation-table 
          (emacspeak-pronounce-compose-pronunciation-table))
    (message
 "Emacspeak pronunciations have been re-activated in this buffer")
    (emacspeak-auditory-icon 'on))))

(defun emacspeak-pronounce-refresh-pronunciations ()
  "Refresh pronunciation table for current buffer.
Activates pronunciation dictionaries if not already active."
  (interactive)
  (declare (special emacspeak-pronounce-pronunciation-table))
  (cond 
   ((not (boundp 'emacspeak-pronounce-pronunciation-table)) ;first time
    (set (make-local-variable 'emacspeak-pronounce-pronunciation-table)
         (emacspeak-pronounce-compose-pronunciation-table))
    (emacspeak-auditory-icon 'on)
    (message
     "Refreshed pronunciations for this buffer"))
   ( emacspeak-pronounce-pronunciation-table ;already on --refresh it 
     (setq emacspeak-pronounce-pronunciation-table
           (emacspeak-pronounce-compose-pronunciation-table))
     (emacspeak-auditory-icon 'on)
     (message
      "Refreshed pronunciation for this buffer"))
   (t                                   ;turn it on 
    (setq emacspeak-pronounce-pronunciation-table 
          (emacspeak-pronounce-compose-pronunciation-table))
    (message
     "Refreshed pronunciations for this buffer")
    (emacspeak-auditory-icon 'on))))

;;}}}
;;{{{ top level dispatch routine

(defvar emacspeak-pronounce-help 
"Dictionary:  Clear Define Load Refresh Save Toggle"
"Help message listing emacspeak commands")

(defun emacspeak-pronounce-dispatch ()
  "Provides the user interface front-end to Emacspeak's pronunciation dictionaries."
  (interactive)
  (declare (special emacspeak-message-help))
  (message emacspeak-pronounce-help)
  (let ((event (read-event)))
    (case event 
      (?c (call-interactively 'emacspeak-pronounce-clear-dictionaries))
      (?d (call-interactively 'emacspeak-pronounce-define-pronunciation))
      (?l (call-interactively 'emacspeak-pronounce-load-dictionaries))
      (?r (call-interactively 'emacspeak-pronounce-refresh-pronunciations))
      (?s (call-interactively 'emacspeak-pronounce-save-dictionaries))
      (?t (call-interactively 'emacspeak-pronounce-toggle-use-of-dictionaries))
      (otherwise (message emacspeak-pronounce-help)))
    (emacspeak-auditory-icon 'close-object)))

;;}}}
(provide  'emacspeak-pronounce)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; end: 

;;}}}
