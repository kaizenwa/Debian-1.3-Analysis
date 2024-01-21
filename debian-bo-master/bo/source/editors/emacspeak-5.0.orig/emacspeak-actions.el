;;; $Id: emacspeak-actions.el,v 5.0 1996/11/22 18:04:26 raman Exp $
;;; $Author: raman $ 
;;; Define emacspeak actions for various modes 
;;; Keywords:emacspeak, audio interface to emacs actions 
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
;;{{{  Introduction:

;;; Define mode-specific  actions.
;;; Actions are defined by adding them to hook
;;; emacspeak-<mode-name>-actions-hook 

;;}}}
;;{{{  Define actions for emacs lisp mode 

(defun emacspeak-lisp-blink-matching-paren-when-on-right-paren ()
  "Defines an emacspeak action on all right parens that causes
emacspeak to show the matching paren when the cursor moves across a right paren."
    (save-excursion
      (goto-char (point-min))
      (ems-modify-buffer-safely
          (while (search-forward ")" nil t )
            (put-text-property  (point) (1+ (point))
                                'emacspeak-action
                                'emacspeak-blink-matching-open )))))
        
        

(add-hook 'emacspeak-emacs-lisp-mode-actions-hook
          'emacspeak-lisp-blink-matching-paren-when-on-right-paren )

;;}}}
;;{{{  Define actions for c and c++ modes 

(defun emacspeak-c-speak-semantics-when-on-closing-brace ()
  "Defines an emacspeak action on all right braces  that causes
emacspeak to speak the semantics of the line
 when the cursor moves across a right brace."
  (save-excursion
    (goto-char (point-min))
    (ems-modify-buffer-safely
     (while (search-forward "}" nil t )
       (put-text-property  (point) (1+ (point))
                           'emacspeak-action
                           'emacspeak-c-speak-semantics )))))
        

(add-hook 'emacspeak-c-mode-actions-hook
            'emacspeak-c-speak-semantics-when-on-closing-brace)

;;}}}
(provide  'emacspeak-actions)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; end: 

;;}}}
