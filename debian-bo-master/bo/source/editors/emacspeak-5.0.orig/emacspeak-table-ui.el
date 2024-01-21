;;; $Id: emacspeak-table-ui.el,v 5.0 1996/11/22 18:04:26 raman Exp $
;;; $Author: raman $ 
;;; Description: Emacspeak table handling module
;;; Keywords:emacspeak, audio interface to emacs tables are structured
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
(require 'emacspeak-table)
(require 'emacspeak-fix-interactive)
(require 'emacspeak-speak)
(require 'emacspeak-sounds)
(require 'thingatpt)
;;{{{  Introduction

;;; User interface to tables

;;}}}
;;{{{ define personalities

(defvar emacspeak-table-column-header-personality 'paul-smooth
  "personality for speaking column headers.")

(defvar emacspeak-table-row-header-personality 'harry
  "Personality for speaking row headers")

;;}}}
;;{{{  emacspeak table mode

(defvar emacspeak-table-keymap (make-sparse-keymap)
  "Keymap for using in table browsing mode")

(progn
  (define-key emacspeak-table-keymap "\t"
    'emacspeak-table-next-column)
  (define-key emacspeak-table-keymap  '[S-tab] 'emacspeak-table-previous-column)
(define-key emacspeak-table-keymap "g" 'emacspeak-table-goto)
(define-key emacspeak-table-keymap '[up] 'emacspeak-table-previous-row)
(define-key emacspeak-table-keymap '[down] 'emacspeak-table-next-row)
(define-key emacspeak-table-keymap '[left] 'emacspeak-table-previous-column)
(define-key emacspeak-table-keymap '[right] 'emacspeak-table-next-column)
(define-key emacspeak-table-keymap "r" 'emacspeak-table-speak-row-header-and-element )
(define-key emacspeak-table-keymap "c" 'emacspeak-table-speak-column-header-and-element)
(define-key emacspeak-table-keymap " " 'emacspeak-table-speak-current-element)
  (define-key emacspeak-table-keymap "b"
    'emacspeak-table-speak-both-headers-and-element)
(define-key emacspeak-table-keymap "." 'emacspeak-table-speak-coordinates)
(define-key emacspeak-table-keymap "a"
  'emacspeak-table-select-automatic-speaking-method)
(define-key emacspeak-table-keymap "s" 'emacspeak-table-search)
(define-key emacspeak-table-keymap "h"
  'emacspeak-table-search-headers)
(define-key emacspeak-table-keymap "T" 'emacspeak-table-goto-top)
(define-key emacspeak-table-keymap "B" 'emacspeak-table-goto-bottom)
(define-key emacspeak-table-keymap "L" 'emacspeak-table-goto-left)
(define-key emacspeak-table-keymap "R" 'emacspeak-table-goto-right)
)

(defun emacspeak-table-mode ()
  "Major mode for browsing tables.
Table mode is designed to allow speech users to browse tabular data
with full contextual feedback while retaining all the power of the
two-dimensional spatial layout of tables.

In table mode, the arrow keys move between cells of the table.
Emacspeak speaks the cell contents in a user-customizable way.  The
visual display is kept in sync with the speech you hear; however
Emacspeak is examining the entire table in order to speak the current
cell content intelligently.

You can interactively specify that emacspeak should speak either the row or column header (or both) while speaking each cell.
You can also move to a specific row or column by searching the cell contents or by searching the row or column headers to locate items of interest.

Here is a short description of the special commands provided in this mode.

The next four commands help you move to the edges of the table:

R               emacspeak-table-goto-right
L               emacspeak-table-goto-left
B               emacspeak-table-goto-bottom
T               emacspeak-table-goto-top

The next two commands let you search the table.
The commands ask you if you want to search rows or columns.
When searching headers remember that row 0 is the column header, and that column 0 is the row header.

h               emacspeak-table-search-headers
s               emacspeak-table-search

The next command lets you specify how cell contents should be spoken.
Specify one of: `b' for both, `c' for column, or `r' for row --table
cells with then be spoken with both (or either)row and column headers,.

a               emacspeak-table-select-automatic-speaking-method

The next set of commands speak the current table cell:

.               emacspeak-table-speak-coordinates
b               emacspeak-table-speak-both-headers-and-element
SPC             emacspeak-table-speak-current-element
c               emacspeak-table-speak-column-header-and-element
r               e macspeak-table-speak-row-header-and-element

The next set of commands navigate the table:

right               emacspeak-table-next-column
left               emacspeak-table-previous-column
down               emacspeak-table-next-row
up               emacspeak-table-previous-row
g               emacspeak-table-goto
S-tab               emacspeak-table-previous-column
TAB               emacspeak-table-next-column"
  (declare (special emacspeak-table-keymap
                    voice-lock-mode))
  (use-local-map emacspeak-table-keymap)
  (setq voice-lock-mode t)
  (setq major-mode 'emacspeak-table-mode)
  (setq mode-name "table")
  (put-text-property (point-min) (point-max)
                     'point-entered 'emacspeak-table-point-motion-hook)
  (not-modified)
  (setq buffer-read-only t)
  (emacspeak-auditory-icon 'alarm)
  (emacspeak-speak-mode-line))

;;}}}
;;{{{  current position

(defsubst emacspeak-table-synchronize-display ()
  "Bring visual display in sync with internal representation"
  (declare (special emacspeak-table positions))
  (let ((row (emacspeak-table-current-row emacspeak-table))
        (column (emacspeak-table-current-column emacspeak-table)))
        (goto-char
         (or 
         (gethash
          (intern
           (format "element:%s:%s" row column))
          positions)
         (point)))))


(defsubst  emacspeak-table-speak-coordinates ()
  "Speak current table coordinates."
  (interactive)
  (declare (special emacspeak-table))
  (and (boundp 'emacspeak-table)
       (message "Row %s Column %s"
                (emacspeak-table-current-row emacspeak-table)
                (emacspeak-table-current-column emacspeak-table))))

(defun emacspeak-table-speak-current-element ()
  "Speak current table element"
  (interactive)
  (declare (special emacspeak-table ))
  (and (boundp 'emacspeak-table)
       (dtk-speak (emacspeak-table-current-element emacspeak-table))))

(defun emacspeak-table-speak-row-header-and-element ()
  "Speak  row header and table element"
  (interactive)
  (declare (special emacspeak-table
                    emacspeak-table-row-header-personality))
  (and (boundp 'emacspeak-table)
       (let ((head (format "%s"
                           (emacspeak-table-row-header-element emacspeak-table
                                                       (emacspeak-table-current-row emacspeak-table )))))
         (put-text-property 0 (length head)
                            'personality
                            emacspeak-table-row-header-personality head)
         (dtk-speak
          (concat head
                  (format " %s"
                  (emacspeak-table-current-element
                   emacspeak-table)))))))

(defun emacspeak-table-speak-column-header-and-element ()
  "Speak  column header and table element"
  (interactive)
  (declare (special emacspeak-table
                    emacspeak-table-column-header-personality))
  (and (boundp 'emacspeak-table)
       (let ((head (format "%s"
                           (emacspeak-table-column-header-element emacspeak-table
                                                                  (emacspeak-table-current-column emacspeak-table )))))
         (put-text-property 0 (length head)
                            'personality
                            emacspeak-table-column-header-personality head)
         (dtk-speak
          (concat head
                  (format " %s"
                  (emacspeak-table-current-element
                   emacspeak-table)))))))

(defun emacspeak-table-speak-both-headers-and-element ()
  "Speak  both row and column header and table element"
  (interactive)
  (declare (special emacspeak-table
                    emacspeak-table-column-header-personality
                    emacspeak-table-row-header-personality))
  (and (boundp 'emacspeak-table)
       (let ((column-head (format "%s"
                                  (emacspeak-table-column-header-element emacspeak-table
                                                                         (emacspeak-table-current-column emacspeak-table ))))
             (row-head (format "%s"
                               (emacspeak-table-row-header-element emacspeak-table
                                                                   (emacspeak-table-current-row emacspeak-table )))))
         (put-text-property 0 (length row-head)
                            'personality
                            emacspeak-table-row-header-personality
                            row-head)
         (put-text-property 0 (length column-head)
                            'personality
                            emacspeak-table-column-header-personality column-head)
         (dtk-speak
          (concat row-head" "  column-head 
                  (format " %s"
                          (emacspeak-table-current-element
                           emacspeak-table)))))))

;;}}}
;;{{{  what to do when point moves

(defun emacspeak-table-point-motion-hook (old new )
  "Bring internal representation in sync with visual display"
  (declare (special emacspeak-table))
  (condition-case nil
      (emacspeak-table-goto-cell emacspeak-table
   (get-text-property new 'row)
   (get-text-property new 'column))
    (error nil ))
  (push-mark old t))

;;}}}
;;{{{  opening a file of table data

(defun emacspeak-table-find-file (filename)
  "Open a file containing table data and display it in table mode.
emacspeak table mode is designed to let you browse tabular data using
all the power of the two-dimensional spatial layout while giving you
sufficient contextual information.  The tables subdirectory of the
emacspeak distribution contains some sample tables --these are the
CalTrain schedules.  Execute command `describe-mode' bound to
\\[describe-mode] in a buffer that is in emacspeak table mode to read
the documentation on the table browser."

  (interactive "FEnter filename containing table data: ")
  (declare (special positions))
  (let ((buffer (get-buffer-create
                 (format  "*%s*"
                          (file-name-nondirectory filename))))
        (table nil)
        (data nil)
        (i 0)
        (j 0)
        (count 0)
        (row-start 1)
        (column-start 1))
    (setq data (find-file-noselect filename))
    (setq table (make-emacspeak-table (read data)))
    (kill-buffer data )
    (save-excursion
      (set-buffer buffer)
      (let ((inhibit-read-only t))
      (erase-buffer)
      (set (make-local-variable 'emacspeak-table) table)
      (set (make-local-variable 'positions) (make-hash-table))
      (setq buffer-file-name filename)
      (setq count (1-  (emacspeak-table-num-columns table)))
      (loop for row across (emacspeak-table-elements table)
            do
            (loop for element across row
                  do 
                  (setf
                   (gethash
                    (intern (format "element:%s:%s" i j ))
                    positions)
                   (point))
                  (insert
                   (format "%s%s"
                           (emacspeak-table-this-element table i j )
                           (if (=  j count)
                               "\n"
                             "\t")))
                  (put-text-property column-start (point)
                                     'column j)
                  (setq column-start (point))
                  (incf j))
            (setq j 0)
            (put-text-property row-start (point) 'row i)
            (setq row-start (point))
            (incf i))
      (emacspeak-table-mode)
      (goto-char (point-min))))
    (switch-to-buffer buffer)))

(emacspeak-fix-interactive-command-if-necessary 'emacspeak-table-find-file)

;;}}}
;;{{{ select default speaking action

(defvar emacspeak-table-select-automatic-speaking-method-prompt
  "Select: b both c column d default r row "
  "Prompt to display when selecting automatic speaking method for
table elements")

(defun emacspeak-table-select-automatic-speaking-method ()
  "Interactively select the kind of automatic speech to produce when
browsing table elements"
  (interactive)
  (declare (special emacspeak-table-speak-element))
  (message emacspeak-table-select-automatic-speaking-method-prompt)
  (let ((key (read-event)))
    (setq emacspeak-table-speak-element 
          (case  key
            (?b 'emacspeak-table-speak-both-headers-and-element)
            (?c 'emacspeak-table-speak-column-header-and-element)
            (?r 'emacspeak-table-speak-row-header-and-element)
            (?d 'emacspeak-table-speak-current-element)
            (?. 'emacspeak-table-speak-coordinates)
            (otherwise (message "Invalid method specified")
                       emacspeak-table-speak-element)))
    (emacspeak-auditory-icon 'button)))

;;}}}
;;{{{ Navigating the table:

(defvar emacspeak-table-speak-element
  'emacspeak-table-speak-current-element
  "Function to call when automatically speaking table elements.")

(defun emacspeak-table-next-row (&optional count)
  "Move to the next row if possible"
  (interactive "p")
  (declare (special emacspeak-table ))
  (setq count (or count 1 ))
  (cond
   ((not (boundp 'emacspeak-table))
    (error "Cannot find table associated with this buffer"))
   (t (emacspeak-table-move-down emacspeak-table count )
      (emacspeak-table-synchronize-display)
      (funcall emacspeak-table-speak-element))))

(defun emacspeak-table-previous-row (&optional count)
  "Move to the previous row if possible"
  (interactive "p")
  (declare (special emacspeak-table ))
  (setq count (or count 1 ))
  (cond
   ((not (boundp 'emacspeak-table))
    (error "Cannot find table associated with this buffer"))
   (t (emacspeak-table-move-up emacspeak-table count )
      (emacspeak-table-synchronize-display)
      (funcall emacspeak-table-speak-element))))

(defun emacspeak-table-next-column (&optional count)
  "Move to the next column if possible"
  (interactive "p")
  (declare (special emacspeak-table ))
  (setq count (or count 1 ))
  (cond
   ((not (boundp 'emacspeak-table))
    (error "Cannot find table associated with this buffer"))
   (t(emacspeak-table-move-right emacspeak-table count )
     (emacspeak-table-synchronize-display)
    (funcall emacspeak-table-speak-element))))

(defun emacspeak-table-previous-column (&optional count)
  "Move to the previous column  if possible"
  (interactive "p")
  (declare (special emacspeak-table ))
  (setq count (or count 1 ))
  (cond
   ((not (boundp 'emacspeak-table))
    (error "Cannot find table associated with this buffer"))
   (t (emacspeak-table-move-left emacspeak-table count )
      (emacspeak-table-synchronize-display)
      (funcall emacspeak-table-speak-element))))

(defun emacspeak-table-goto (row column)
  "Prompt for a table cell coordinates and jump to it."
  (interactive "nRow:\nNColumn:")
  (declare (special emacspeak-table))
  (cond
   ((not (boundp 'emacspeak-table))
    (error "Cannot find table associated with this buffer."))
   (t (emacspeak-table-goto-cell emacspeak-table row column)
      (emacspeak-table-synchronize-display)
  (funcall emacspeak-table-speak-element)
  (emacspeak-auditory-icon 'large-movement))))

(emacspeak-fix-interactive-command-if-necessary 'emacspeak-table-goto)

(defun emacspeak-table-goto-top ()
  "Goes to the top of the current column."
  (interactive)
  (declare (special emacspeak-table))
  (unless (boundp 'emacspeak-table)
    (error "Cannot find table associated with this buffer"))
  (emacspeak-table-goto-cell emacspeak-table
   0 (emacspeak-table-current-column emacspeak-table))
  (emacspeak-table-synchronize-display)
  (funcall emacspeak-table-speak-element)
  (emacspeak-auditory-icon 'large-movement))

(defun emacspeak-table-goto-bottom ()
  "Goes to the bottom of the current column."
  (interactive)
  (declare (special emacspeak-table))
  (unless (boundp 'emacspeak-table)
    (error "Cannot find table associated with this buffer"))
  (emacspeak-table-goto-cell emacspeak-table
                             (1- (emacspeak-table-num-rows emacspeak-table))
                             (emacspeak-table-current-column
                              emacspeak-table))
  (emacspeak-table-synchronize-display)
  (funcall emacspeak-table-speak-element)
  (emacspeak-auditory-icon 'large-movement))

(defun emacspeak-table-goto-left ()
  "Goes to the left of the current row."
  (interactive)
  (declare (special emacspeak-table))
  (unless (boundp 'emacspeak-table)
    (error "Cannot find table associated with this buffer"))
  (emacspeak-table-goto-cell emacspeak-table
   (emacspeak-table-current-row emacspeak-table)
   0)
  (emacspeak-table-synchronize-display)
  (funcall emacspeak-table-speak-element)
  (emacspeak-auditory-icon 'large-movement))

(defun emacspeak-table-goto-right ()
  "Goes to the right of the current row."
  (interactive)
  (declare (special emacspeak-table))
  (unless (boundp 'emacspeak-table)
    (error "Cannot find table associated with this buffer"))
  (emacspeak-table-goto-cell emacspeak-table 
                      (emacspeak-table-current-row emacspeak-table)
                      (1- (emacspeak-table-num-columns
                           emacspeak-table)))
  (emacspeak-table-synchronize-display)
  (funcall emacspeak-table-speak-element)
  (emacspeak-auditory-icon 'large-movement))

;;}}}
;;{{{ searching and finding:

(defun emacspeak-table-search ()
  "Search the table for matching elements.  Interactively prompts for
row or column to search and pattern to look for.    If there is a match, makes
the matching cell current."
  (interactive )
  (declare (special emacspeak-table))
  (unless (boundp 'emacspeak-table)
    (error "Cannot find table associated with this buffer"))
  (message "Search   in: r row c column")
  (let* ((row (emacspeak-table-current-row emacspeak-table))
         (column (emacspeak-table-current-column emacspeak-table))
         (found nil)
         (slice 
          (case (read-event)
            (?r 'row)
            (?c 'column)
            (otherwise (error "Can only search in either row or column"))))
         (pattern (read-string
                   (format "Search in current  %s for: " slice ))))
    (cond
     ((eq slice 'row)
      (setq found (emacspeak-table-find-match-in-row emacspeak-table
                                                     row pattern 'string-match)))
     ((eq slice 'column)
      (setq found (emacspeak-table-find-match-in-column
                   emacspeak-table column pattern 'string-match)))
     (t (error "Invalid search")))
    (cond
     (found
      (cond
       ((eq slice 'row )
        (emacspeak-table-goto-cell emacspeak-table row found))
       ((eq slice 'column)
        (emacspeak-table-goto-cell emacspeak-table found column)))
      (emacspeak-table-synchronize-display)
      (emacspeak-auditory-icon 'search-hit))
     (t (emacspeak-auditory-icon 'search-miss)))
    (funcall emacspeak-table-speak-element)))

(defun emacspeak-table-search-headers ()
  "Search the table row or column headers.  Interactively prompts for
row or column to search and pattern to look for.  If there is a
match, makes the matching row or column current."
  (interactive )
  (declare (special emacspeak-table))
  (unless (boundp 'emacspeak-table)
    (error "Cannot find table associated with this buffer"))
  (message "Search headers : r row c column")
  (let* ((row (emacspeak-table-current-row emacspeak-table))
         (column (emacspeak-table-current-column emacspeak-table))
         (found nil)
         (slice 
          (case (read-event)
            (?r 'row)
            (?c 'column)
            (otherwise (error "Can only search in either row or column"))))
         (pattern (read-string
                   (format "Search %s headers for: " slice ))))
    (cond
     ((eq slice 'row)
      (setq found (emacspeak-table-find-match-in-column
                   emacspeak-table 0 pattern 'string-match)))
     ((eq slice 'column)
      (setq found (emacspeak-table-find-match-in-row
                   emacspeak-table 0 pattern 'string-match)))
     (t (error "Invalid search")))
    (cond
     (found
      (cond
       ((eq slice 'row )
        (emacspeak-table-goto-cell emacspeak-table  found column))
       ((eq slice 'column)
        (emacspeak-table-goto-cell emacspeak-table row found)))
      (emacspeak-table-synchronize-display)
      (emacspeak-auditory-icon 'search-hit))
     (t (emacspeak-auditory-icon 'search-miss)))
    (funcall emacspeak-table-speak-element)))

;;}}}

(provide  'emacspeak-table-ui)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; end: 

;;}}}
