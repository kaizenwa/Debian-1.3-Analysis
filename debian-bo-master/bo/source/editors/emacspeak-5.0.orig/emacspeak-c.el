;;; $Id: emacspeak-c.el,v 5.0 1996/11/22 18:04:26 raman Exp $
;;; $Author: raman $ 
;;; DescriptionEmacspeak extensions for C and C++ mode
;;; Keywords:emacspeak, audio interface to emacs C, C++
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
(require 'emacspeak-speak)
(require 'emacspeak-sounds)
(eval-when (compile)
(require 'cc-mode ))
;;{{{ Introduction:

;;; Make some of C and C++ mode more emacspeak friendly
;;; Works with both boring c-mode 
;;; and the excellent cc-mode

;;}}}
;;{{{  emacs 19.30

;;; things got renamed in 19.30.

(when  (string-lessp emacs-version "19.29")
     (defalias 'c-guess-basic-syntax 'c-guess-basic-semantics))

;;}}}
;;{{{  advice things to speak

;;{{{  Electric chars speak

(defadvice c-electric-semi&comma (after emacspeak pre act )
  "Speak the line when a statement is completed."
  (when (interactive-p)
    (cond 
     ((= last-input-char ?,) (dtk-speak " comma "))
     (t (emacspeak-speak-line )))))

(defadvice electric-c-terminator (after emacspeak pre act )
"Speak what was typed. "
(when (interactive-p)
(emacspeak-speak-this-char last-input-char)))

(defadvice c-electric-colon (after emacspeak pre act )
"Speak the character you inserted"
(when (interactive-p)
(emacspeak-speak-this-char last-input-char)))

(defadvice c-electric-pound (after emacspeak pre act )
"Speak the character you inserted"
(when (interactive-p)
(emacspeak-speak-this-char last-input-char)))
(defadvice c-electric-brace (after emacspeak pre act )
"Speak the character you inserted"
(when (interactive-p)
(emacspeak-speak-this-char last-input-char)))

(defadvice electric-c-semi (after emacspeak pre act )
"Speak what was typed. "
(when (interactive-p)
(emacspeak-speak-this-char last-input-char)))


(defadvice electric-c-sharp-sign (after emacspeak pre act )
"Speak what was typed. "
(when (interactive-p)
(emacspeak-speak-this-char last-input-char)))

(defadvice electric-c-brace (after emacspeak pre act )
"Speak what was typed. "
(when (interactive-p)
(emacspeak-speak-this-char last-input-char)))


(defadvice c-electric-delete (before emacspeak pre act )
"Speak char before deleting it."
(when (interactive-p )
    (emacspeak-speak-this-char(preceding-char ))
    (dtk-tone 500 30)
    (dtk-force)))

;;}}}
;;{{{  Moving across logical chunks 

;;; CPP directives: 

(defadvice c-up-conditional (after emacspeak pre act )
"Speak the line moved to."
(when (interactive-p)
  (emacspeak-auditory-icon 'large-movement)
(emacspeak-speak-line )))

(defadvice c-forward-conditional (after emacspeak pre act )
"Speak the line moved to."
(when (interactive-p)
  (emacspeak-auditory-icon 'large-movement)
(emacspeak-speak-line )))

(defadvice c-backward-conditional (after emacspeak pre act )
"Speak the line moved to."
(when (interactive-p)
(emacspeak-auditory-icon 'large-movement)
(emacspeak-speak-line )))

;;; Statements 

(defadvice c-beginning-of-statement (after emacspeak pre act )
"Speak the line moved to."
(when (interactive-p)
(emacspeak-auditory-icon 'large-movement)
(emacspeak-speak-line )))

(defadvice c-end-of-statement (after emacspeak pre act )
"Speak the line moved to."
(when (interactive-p)
  (emacspeak-auditory-icon 'large-movement)
(emacspeak-speak-line )))

(defadvice mark-c-function (after emacspeak pre act )
  "Provide spoken and auditory feedback."
  (when (interactive-p)
(let ((dtk-stop-immediately nil))
    (emacspeak-auditory-icon 'mark-object)
(dtk-speak "Marked function. ")
    (emacspeak-speak-line ))))

;;}}}

;;}}}
;;{{{  extensions  provided by c++ mode

(defadvice c-scope-operator (after emacspeak pre act )
"speak what you inserted."
(when (interactive-p)
(dtk-speak "colon colon")))

;;}}} 
;;{{{  Some more navigation functions I define:

(defun c-previous-statement (count)
  "Move to the previous  C statement. "
  (interactive "P")
  (emacspeak-auditory-icon 'large-movement)
  (let  ((opoint (point))
         (semantics (c-guess-basic-syntax )))
         ;;; skip across a comment
    (cond
     ((or (assoc 'c semantics )
          (assoc 'comment-intro semantics ))
      (while 
          (and (or (assoc 'c semantics )
                   (assoc 'comment-intro semantics ))
               (not (eobp ))
               (= 0 (forward-line -1)))
        (setq semantics (c-guess-basic-syntax )))
      (skip-syntax-backward " ")
      (dtk-speak "Skipping comment")
      (and (sit-for 2)
           (emacspeak-speak-line )))
     (t (setq count (or count 1))
        (c-beginning-of-statement   count )
        (and (save-match-data (looking-at "{"))
             (skip-syntax-backward " "))
        (if (>= (point) opoint)
            (progn (dtk-speak "Cannot move to previous  statement at this level.")
                   (and (sit-for 2)
                        (emacspeak-c-speak-semantics)))
          (emacspeak-speak-line))))))

(defun c-next-statement (count)
  "Move to the next C statement. "
  (interactive "P")
  (emacspeak-auditory-icon 'large-movement)
  (let  ((opoint (point))
         (semantics (c-guess-basic-syntax )))
         ;;; skip across a comment
    (cond
     ((or (assoc 'c semantics )
          (assoc 'comment-intro semantics ))
      (while 
          (and (or (assoc 'c semantics )
                   (assoc 'comment-intro semantics ))
               (not (eobp ))
               (= 0 (forward-line 1)))
        (setq semantics (c-guess-basic-syntax )))
      (skip-syntax-forward " ")
      (dtk-speak "Skipping  comment")
      (and (sit-for 2)
           (emacspeak-speak-line )))
     (t (setq count (or count 1))
        (c-end-of-statement(1+  count ))
        (c-beginning-of-statement  1 )
        (and (save-match-data (looking-at "{"))
             (skip-syntax-backward " "))
        (if (<= (point) opoint)
            (progn (dtk-speak "Cannot move to next statement at this level.")
                   (and (sit-for 2)
                        (emacspeak-c-speak-semantics)))
          (emacspeak-speak-line))))))

;;}}}
;;{{{  C semantics 

(defvar emacspeak-c-semantic-table 
  (list
    '(string                  . " inside multi-line string")
    '(c                       .  "inside a multi-line C style block comment")
    '(defun-open            .    "brace that opens a function definition")
    '(defun-close  .  "brace that closes a function definition")
    '(defun-block-intro . " the first line in a top-level defun")
    '(class-open . " brace that opens a class definition")
    '(class-close . " brace that closes a class definition")
    '(inline-open . " brace that opens an in-class inline method")
    '(inline-close . " brace that closes an in-class inline method")
    '(c++-funcdecl-cont . " the nether region between a C++ function
                           declaration  and the defun opening brace")
    '(knr-argdecl-intro . " first line of a K&R C argument declaration")
    '(knr-argdecl."  subsequent lines in a K&R C argument declaration")
    '(topmost-intro . " the first line in a topmost construct definition")
    '(topmost-intro-cont . " topmost definition continuation lines")
    '(member-init-intro."  first line in a member initialization list")
    '(member-init-cont . " subsequent member initialization list lines")
    '(inher-intro . " first line of a multiple inheritance list")
    '(inher-cont . " subsequent multiple inheritance lines")
    '(block-open . " statement block open brace")
    '(block-close . " brace that closes  ")
    '(brace-list-open  .  "open brace of an enum or static array list")
    '(brace-list-close  .  " brace that closes  an enum or static array list")
    '(brace-list-intro . " first line in an enum or static array list")
    '(brace-list-entry . " subsequent lines in an enum or static array list")
    '(statement . " a C/C++ statement")
    '(statement-cont . " a continuation of a C/C++ statement")
    '(statement-block-intro   . "the first line in a new statement block")
    '(statement-case-intro    . "the first line in a case `block'
 substatement            .  the first line after an if/while/for/do/else")
    '(substatement-open . " the brace that opens a substatement block")
    '(case-label."  a case or default label")
    '(access-label . " C++ private/protected/public access label")
 '(label                   ."  any non-special C/C++ label")
    '(do-while-closure . " the `while' that ends a do/while construct")
    '(else-clause."  the `else' of an if/else construct")
    '(comment-intro . " a line containing only a comment introduction")
    '(arglist-intro . " the first line in an argument list")
    '(arglist-cont . " subsequent argument list lines when no
                           arguments follow on the same line as the
                           the arglist opening paren")
    '(arglist-cont-nonempty   . "subsequent argument list lines when at
                           least one argument follows on the same
                           line as the arglist opening paren")
    '(arglist-close . " the solo close paren of an argument list")
    '(stream-op . " lines continuing a stream operator construct
 inclass                 .  the construct is nested inside a class definition")
    '(cpp-macro . " the start of a cpp macro")
    '(friend . " a C++ friend declaration"))
  "Association list of semantic symbols defined by cc-mode
and their meanings. ")

(defun emacspeak-c-speak-semantics ()
  "Speak the C semantics of this line. "
  (interactive)
  (declare (special emacspeak-c-semantic-table ))
  (let  ((semantics (mapcar 'car
                            (c-guess-basic-syntax )))
         (description ""))
    (setq description
          (mapconcat 
           (function (lambda (sem) 
                       (cdr (assoc  sem emacspeak-c-semantic-table ))))
           semantics 
           " "))
    (cond
     ((or (memq 'block-close semantics )
          (memq 'defun-close semantics )
          (memq 'class-close semantics )
          (memq 'inline-close semantics )
          (memq 'brace-list-close semantics ))
      ;; append the line 
         (setq description
               (concat description
                       ;; that begins this block
                       (let ((start nil ))
                       (save-excursion
                         (end-of-line)
                         (backward-sexp 1)
                         (setq start (point))
                         (beginning-of-line)
                         (skip-syntax-forward " ")
                         (and (= start (point))
                              (forward-line -1)
                              (beginning-of-line))
                         (setq start (point))
                         (end-of-line)
                         (buffer-substring start (point ))))))))
    (dtk-speak description )
description))

;;}}}
;;{{{  indenting commands

(defadvice c-indent-defun (after emacspeak pre act)
  (when (interactive-p)
    (emacspeak-auditory-icon 'fill-object)
    (message "Indented function")))

;;}}}
;;{{{ Additional keybindings:

(add-hook 'c-mode-common-hook 
(function (lambda ()
(define-key c-mode-map "\C-cs" 'emacspeak-c-speak-semantics)
(define-key c-mode-map "\M-n" 'c-next-statement)
(define-key c-mode-map "\M-p" 'c-previous-statement))))

;;}}}

(provide  'emacspeak-c)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; end: 

;;}}}
