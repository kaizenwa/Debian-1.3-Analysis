;;; $Id: emacspeak-speak.el,v 5.0 1996/11/22 18:04:26 raman Exp $
;;; $Author: raman $ 
;;; Description:  Contains the functions for speaking various chunks of text
;;; Keywords: Emacspeak, Speak, Spoken Output
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

;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'backquote)
(require 'thingatpt)
(require 'dtk-speak)
(eval-when (compile) (require 'voice-lock)
           (require 'emacspeak-sounds))

;;}}}
;;{{{  Introduction:

;;; This module defines the core output functions used by emacspeak.
;;; It depends on the dtk-speak module.
;;; It protects other parts of emacspeak
;;; from becoming dependent on the dtk module.

;;}}}
;;{{{  Macros

;;; Save read-only and modification state, perform some actions and
;;; restore state
(defmacro ems-modify-buffer-safely   (&rest body )
  (`
   (unwind-protect 
       (let    ((save-read-only buffer-read-only)
                (buffer-read-only nil )
                (inhibit-read-only t)
                (inhibit-point-motion-hooks t)
                (modification-flag (buffer-modified-p)))
         (unwind-protect
             (,@ body )
           (setq buffer-read-only save-read-only
                 inhibit-read-only nil
                 inhibit-point-motion-hooks nil)
           (set-buffer-modified-p modification-flag )))
     (setq inhibit-read-only nil))))


(defmacro ems-set-personality-temporarily (start end value &rest body)
  "Internal macro used by Emacspeak to set a personality temporarily. 
The previous property is reset after executing body.
At present, we assume that region from start to end has the same personality. "
  (`
   (unwind-protect
       (progn 
         (declare (special voice-lock-mode ))
         (let ((save-voice-lock voice-lock-mode)
               (saved-personality (get-text-property(, start)
                                                    'personality))
               (save-read-only buffer-read-only)
               (inhibit-read-only t)
               (buffer-read-only nil )
               (modification-flag (buffer-modified-p)))
           (unwind-protect 
               (progn
                 (setq voice-lock-mode t )
                 (put-text-property
                  (max (point-min) (, start))
                  (min (point-max) (, end))
                  'personality (, value) )
                 (,@ body))
             (put-text-property
              (max (point-min) (, start))
              (min (point-max)  (, end)) 'personality saved-personality)
             (setq buffer-read-only save-read-only
                   inhibit-read-only nil
                   voice-lock-mode save-voice-lock )
             (set-buffer-modified-p modification-flag ))))
     (setq inhibit-read-only nil))))

;;}}}
;;{{{  Actions

;;; Setting value of property 'emacspeak-action to a list
;;; of the form (before | after function)
;;; function to be executed before or after the unit of text at that
;;; point is spoken.

(defvar emacspeak-action-mode nil
  "If t then any function that is set as the value of property action
is executed when the text unit at that point is spoken.")

(make-variable-buffer-local 'emacspeak-action-mode)

;;; Record in the mode line
(or (assq 'emacspeak-action-mode minor-mode-alist)
    (setq minor-mode-alist
	  (append minor-mode-alist
		  '((emacspeak-action-mode " Action")))))

;;; Return the appropriate action hook variable that defines actions
;;; for this mode.

(defsubst  emacspeak-action-get-action-hook (mode)
  (intern (format "emacspeak-%s-actions-hook" mode )))
  

(defun emacspeak-toggle-action-mode  (&optional prefix)
  "Toggle state of  Emacspeak  action mode.
Interactive prefix arg means toggle  the global default value, and then set the
current local  value to the result. "
  (interactive  "P")
  (declare  (special  emacspeak-action-mode))
  (cond
   (prefix
    (setq-default  emacspeak-action-mode
                   (not  (default-value 'emacspeak-action-mode )))
    (setq emacspeak-action-mode (default-value 'emacspeak-action-mode )))
   (t (make-local-variable'emacspeak-action-mode)
      (setq emacspeak-action-mode 
	    (not emacspeak-action-mode ))))
  (when emacspeak-action-mode
    (require 'emacspeak-actions)
    (let ((action-hook (emacspeak-action-get-action-hook  major-mode
                                                          )))
      (and (boundp action-hook)
           (run-hooks action-hook ))))
  (emacspeak-auditory-icon
   (if emacspeak-action-mode 'on 'off))
  (message "Turned %s Emacspeak Action Mode  %s "
           (if emacspeak-action-mode "on" "off" )
	   (if prefix "" "locally")))

;;; Execute action at point
(defsubst emacspeak-handle-action-at-point ()
  (declare (special emacspeak-action-mode ))
  (let ((action-spec (get-text-property (point) 'emacspeak-action )))
  (when (and emacspeak-action-mode action-spec )
    (condition-case nil
        (funcall  action-spec )
      (error (message "Invalid actionat %s" (point )))))))



;;}}}
;;{{{  line, Word and Character echo 

(defvar emacspeak-line-echo nil 
  "If t, then emacspeak echoes lines as you type.
Do not set this variable by hand;
Use \\[emacspeak-toggle-line-echo]")

(defun emacspeak-toggle-line-echo (&optional prefix)
  "Toggle state of  Emacspeak  line echo. 
Interactive prefix arg means toggle  the global default value, and then set the
current local  value to the result. "
  (interactive  "P")
  (declare  (special  emacspeak-line-echo ))
  (cond
   (prefix
    (setq-default  emacspeak-line-echo
                   (not  (default-value 'emacspeak-line-echo )))
    (setq emacspeak-line-echo (default-value 'emacspeak-line-echo )))
   (t (make-local-variable 'emacspeak-line-echo)
      (setq emacspeak-line-echo 
	    (not emacspeak-line-echo ))))
  (emacspeak-auditory-icon
   (if emacspeak-line-echo 'on 'off))
  (message "Turned %s line echo%s. "
           (if emacspeak-line-echo "on" "off" )
	   (if prefix "" " locally")))

(defvar emacspeak-word-echo t
  "If t, then emacspeak echoes words as you type.
Do not set this variable by hand;
Use \\[emacspeak-toggle-word-echo]")

(defun emacspeak-toggle-word-echo (&optional prefix)
  "Toggle state of  Emacspeak  word echo. 
Interactive prefix arg means toggle  the global default value, and then set the
current local  value to the result. "
  (interactive  "P")
  (declare  (special  emacspeak-word-echo ))
  (cond
   (prefix
    (setq-default  emacspeak-word-echo
                   (not  (default-value 'emacspeak-word-echo )))
    (setq emacspeak-word-echo (default-value 'emacspeak-word-echo )))
   (t (make-local-variable 'emacspeak-word-echo )
      (setq emacspeak-word-echo 
	    (not emacspeak-word-echo ))))
  (emacspeak-auditory-icon
   (if emacspeak-word-echo 'on 'off ))
  (message "Turned %s word echo%s. "
           (if emacspeak-word-echo "on" "off" )
	   (if prefix "" " locally")))
(defvar emacspeak-character-echo t
  "If t, then emacspeak echoes characters  as you type.
Do not set this variable by hand;
Use \\[emacspeak-toggle-character-echo]")

(defun emacspeak-toggle-character-echo (&optional prefix)
  "Toggle state of  Emacspeak  character echo. 
Interactive prefix arg means toggle  the global default value, and then set the
current local  value to the result. "
  (interactive  "P")
  (declare  (special  emacspeak-character-echo ))
  (cond
   (prefix
    (setq-default  emacspeak-character-echo
                   (not  (default-value 'emacspeak-character-echo )))
    (setq emacspeak-character-echo (default-value 'emacspeak-character-echo )))
   (t (make-local-variable 'emacspeak-character-echo)
      (setq emacspeak-character-echo 
	    (not emacspeak-character-echo ))))
  (emacspeak-auditory-icon
   (if emacspeak-character-echo 'on 'off))
  (message "Turned %s character echo%s. "
           (if emacspeak-character-echo "on" "off" )
	   (if prefix "" " locally")))

;;}}}
;;{{{ Showing the point:

(defvar emacspeak-show-point nil 
  "If t, then emacspeak-speak-line indicates position of point by an
aural highlight.  Do not set this variable by hand; Use
\\[emacspeak-toggle-show-point]"
)

(defun emacspeak-toggle-show-point (&optional prefix)
  "Toggle state of  Emacspeak-show-point.
Interactive prefix arg means toggle  the global default value, and then set the
current local  value to the result. "
  (interactive  "P")
  (declare  (special  emacspeak-show-point ))
  (cond
   (prefix
    (setq-default  emacspeak-show-point
                   (not  (default-value 'emacspeak-show-point )))
    (setq emacspeak-show-point (default-value 'emacspeak-show-point )))
   (t (make-local-variable 'emacspeak-show-point)
      (setq emacspeak-show-point 
	    (not emacspeak-show-point ))))
  (emacspeak-auditory-icon
   (if emacspeak-show-point 'on 'off))
  (message "Turned %s show point %s. "
           (if emacspeak-show-point "on" "off" )
	   (if prefix "" " locally")))

;;}}}
;;{{{  indentation:

(defvar emacspeak-audio-indentation nil
  "If non-nil , then speaking a line indicates its indentation.
 Do not set this by hand, use command
emacspeak-toggle-audio-indentation bound to
\\[emacspeak-toggle-audio-indentation]. ")

;;; Indicate indentation.
 ;;; Argument indent   indicates number of columns to indent. 
(defsubst emacspeak-indent (indent)
  (when (> indent 1 )
    (let ((duration (+ 50 (* 20  indent ))))
      (dtk-tone  250 duration)
      (dtk-force))))
    

(defvar emacspeak-audio-indentation-methods
  '(("speak" . "speak")
    ("tone" . "tone"))
  "Possible methods of indicating indentation. ")


(defun emacspeak-toggle-audio-indentation (&optional prefix)
  "Toggle state of  Emacspeak  audio indentation.
Interactive prefix arg means toggle  the global default value, and then set the
current local  value to the result.
 Specifying the method of indentation as `tones'
results in the Dectalk producing a tone whose length is a function of the
line's indentation. Specifying `speak'
results in the number of initial spaces being spoken."
  (interactive  "P")
  (declare  (special  emacspeak-audio-indentation
                      emacspeak-audio-indentation-methods ))
  (cond
   (prefix
    (setq-default  emacspeak-audio-indentation
                   (not  (default-value 'emacspeak-audio-indentation )))
    (setq emacspeak-audio-indentation (default-value 'emacspeak-audio-indentation )))
   (t (make-local-variable'emacspeak-audio-indentation)
      (setq emacspeak-audio-indentation 
	    (not emacspeak-audio-indentation ))))
  (when emacspeak-audio-indentation
    (setq emacspeak-audio-indentation
          (completing-read  "What kind of audio indentation would you like? "
                            emacspeak-audio-indentation-methods  nil t
                            "speak" ))
    (and prefix
         (setq-default emacspeak-audio-indentation
                       emacspeak-audio-indentation )))
  (emacspeak-auditory-icon
   (if emacspeak-audio-indentation 'on 'off))
  (message "Turned %s audio indentation %s "
           (if emacspeak-audio-indentation "on" "off" )
	   (if prefix "" "locally")))

;;}}}
;;{{{  sync emacspeak and dtk:

(defsubst   emacspeak-dtk-sync ()
  "Bring emacspeak and dtk in sync. "
  (declare (special dtk-speaker-process
                    dtk-punctuation-mode dtk-speech-rate dtk-character-scale
                    dtk-capitalize dtk-split-caps ))
  (let ((command (concat 
                  (format "dectalk_set_punctuations %s  \n "
                          dtk-punctuation-mode )
                  (format "dectalk_capitalize %s \n "
                          (if dtk-capitalize 1  0 ))
                  (format "dectalk_split_caps  %s \n "
                          (if dtk-split-caps 1 0 ))
                  (format "dectalk_set_speech_rate %s \n" dtk-speech-rate)
                  (format "dectalk_set_character_scale %s \n "
                          dtk-character-scale)
                  )))
    (process-send-string dtk-speaker-process command )
    command
    )
  )

;;}}}
;;{{{ functions: 

;;{{{  Speak units of text

(defsubst emacspeak-speak-region (start end )
  "Speak region. "
  (interactive "r" )
  (emacspeak-handle-auditory-icon-at-point)
  (emacspeak-handle-action-at-point)
  (dtk-speak (buffer-substring start end )))

(defun emacspeak-speak-line (&optional arg)
  "Speak current line.  With prefix arg, speaks the rest of the line
  from point.  Negative prefix optional arg speaks from start of line
  to point.  Voicifies if voice-lock-mode is on.  Indicates
  indentation with a tone if audio indentation is in use.  Indicates
  position of point with an aural highlight if option
  emacspeak-show-point is turned on --see command emacspeak-show-point
  bound to \\[emacspeak-show-point]."
  (interactive "P")
  (declare (special voice-lock-mode
                    emacspeak-show-point 
                    emacspeak-audio-indentation))
  (when (listp arg) (setq arg (car arg )))
  (save-excursion 
    (let ((start  nil)
          (end nil )
          (inhibit-point-motion-hooks t)
          (line nil)
          (orig (point)) 
          (indent nil)
          (dtk-stop-immediately t))
      (beginning-of-line)
      (emacspeak-handle-auditory-icon-at-point)
      (emacspeak-handle-action-at-point)
      (setq start (point))
      (when (and emacspeak-audio-indentation
                 (null arg ))
        (save-excursion
          (back-to-indentation )
          (setq indent  (current-column ))))
      (end-of-line)
      (setq end (point))
      (goto-char orig) 
      (cond
       ((null arg))
       ((> arg 0) (setq start orig))
       (t (setq end orig)))
      (when (and emacspeak-audio-indentation
                 (string= emacspeak-audio-indentation "tone")
                 (null arg ))
        (setq dtk-stop-immediately nil )
        (emacspeak-indent indent ))
      (if emacspeak-show-point
                  (ems-set-personality-temporarily
                   (point) (1+ (point))
                   'paul-animated 
                   (setq line
                         (buffer-substring  start end )))
                (setq line (buffer-substring start end )))
      (cond
       ((string= ""  (buffer-substring start end))
        (if dtk-stop-immediately (dtk-stop))
        (dtk-tone 200   250))
       (t
        (if (and (string= "speak" emacspeak-audio-indentation )
                 (null arg )
                 indent 
                 (> indent 0))
            (progn
              (setq indent (format "indent %d" indent))
              (put-text-property   0 (length indent)
                                   'personality 'indent-voice  indent )
              (dtk-speak (concat indent line)))
          (dtk-speak line)))))))

(defun emacspeak-speak-word (&optional arg) 
  "Speak current word.
  With prefix arg, speaks the rest of the word from point.
Negative prefix arg speaks from start of word to point."
  (interactive "P")
  (declare (special voice-lock-mode))
  (when (listp arg) (setq arg (car arg )))
  (emacspeak-handle-auditory-icon-at-point)
  (emacspeak-handle-action-at-point)
  (cond
   ((looking-at  "[ \t]+" ) (dtk-say " space "))
   (t (save-excursion
        (let ((orig (point))
              (inhibit-point-motion-hooks t)
              (start nil)
              (end nil))
          (forward-word 1)
          (setq end (point))
          (backward-word 1)
          (setq start (point))
          (cond
           ((null arg ))
           ((> arg 0) (setq start orig))
           ((< arg 0) (setq end orig )))
          (dtk-speak  (buffer-substring  start end )))))))

(defsubst emacspeak-is-alpha-p (c) (= 119 (char-syntax c)))

;;{{{  phonemic table

(defvar emacspeak-char-to-phonetic-table
  '(
    ("1"  . "one")
    ("2" .  "two")
    ("3" .  "three")
    ("4" .  "four")
    ("5" .  "five")
    ("6" .  "six")
    ("7" .  "seven")
    ("8" .  "eight")
    ("9" .  "nine")
    ("0".  "zero")
    ("a" . "alpha" )
    ("b" . "bravo")
    ("c" .  "charlie")
    ("d" . "delta")
    ("e" . "echo")
    ("f" . "foxtrot")
    ("g" . "golf")
    ("h" . "hotel")
    ("i" . "india")
    ("j" . "juliet")
    ("k" . "kilo")
    ("l" . "lima")
    ("m" . "mike")
    ("n" . "november")
    ("o" . "oscar")
    ("p" . "poppa")
    ("q" . "quebec")
    ("r" . "romeo")
    ("s" . "sierra")
    ("t" . "tango")
    ("u" . "unicorn")
    ("v" . "victor")
    ("w" . "whisky")
    ("x" . "xray")
    ("y" . "yankee")
    ("z" . "zulu")
    ("A" . "cap alpha" )
    ("B" . "cap bravo")
    ("C" .  "cap charlie")
    ("D" . "cap delta")
    ("E" . "cap echo")
    ("F" . "cap foxtrot")
    ("G" . "cap golf")
    ("H" . "cap hotel")
    ("I" . "cap india")
    ("J" . "cap juliet")
    ("K" . "cap kilo")
    ("L" . "cap lima")
    ("M" . "cap mike")
    ("N" . "cap november")
    ("O" . "cap oscar")
    ("P" . "cap poppa")
    ("Q" . "cap quebec")
    ("R" . "cap romeo")
    ("S" . "cap sierra")
    ("T" . "cap tango")
    ("U" . "cap unicorn")
    ("V" . "cap victor")
    ("W" . "cap whisky")
    ("X" . "cap xray")
    ("Y" . "cap yankee")
    ("Z" . "cap zulu"))
  "Association list holding the mapping from characters to their phonemic
equivalents. ")


(defun emacspeak-get-phonetic-string (char)
  "Extract and return the phonetic string for this char or its upper case
equivalent.
char is assumed to be one of a--z. "
  (declare (special emacspeak-char-to-phonetic-table))
  (let ((char-string   (char-to-string char )))
    (or   (cdr
           (assoc char-string emacspeak-char-to-phonetic-table ))
          " ")))

;;}}}
(defun emacspeak-speak-char (&optional prefix)
  "Speak char under point
Pronounces character phonetically unless  called with a prefix arg."
  (interactive "P")
  (let ((dtk-stop-immediately t )
        (char  (following-char )))
    (when char
      (emacspeak-handle-auditory-icon-at-point)
      (emacspeak-handle-action-at-point)
      (cond
       ((and (not prefix)
             (emacspeak-is-alpha-p char))
        (dtk-speak (emacspeak-get-phonetic-string char )))
       ((emacspeak-is-alpha-p char) (dtk-letter (char-to-string char )))
       (t (dtk-dispatch
           (dtk-char-to-speech char ))
(dtk-force))))))

(defun emacspeak-speak-this-char (char)
  "Speak this char. "
  (let ((dtk-stop-immediately t ))
    (when char
      (emacspeak-handle-auditory-icon-at-point)
      (emacspeak-handle-action-at-point)
      (cond
       ((emacspeak-is-alpha-p char) (dtk-letter (char-to-string char )))
       (t (dtk-dispatch
           (dtk-char-to-speech char ))
(dtk-force))))))



(defun emacspeak-speak-sentence (&optional arg)
  "Speak current sentence.
With prefix arg, speaks the rest of the sentence  from point.
Negative prefix arg speaks from start of sentence to point."
  (interactive "P" )
  (when (listp arg) (setq arg (car arg )))
  (save-excursion 
    (let ((orig (point))
          (inhibit-point-motion-hooks t)
          (start nil)
          (end nil))
      (forward-sentence 1) 
      (setq end (point))
      (backward-sentence 1)
      (setq start (point))
      (emacspeak-handle-auditory-icon-at-point)
      (emacspeak-handle-action-at-point)
      (cond
       ((null arg ))
       ((> arg 0) (setq start orig))
       ((< arg 0) (setq end orig )))
      (dtk-speak (buffer-substring start end )))))


(defun emacspeak-speak-sexp (&optional arg)
  "Speak current sexp.
 With prefix arg, speaks the rest of the sexp  from point.
Negative prefix arg speaks from start of sexp to point.
If voice-lock-mode is on, then uses the personality. "
  (interactive "P" )
  (when (listp arg) (setq arg (car arg )))
  (save-excursion 
    (let ((orig (point))
          (inhibit-point-motion-hooks t)
          (start nil)
          (end nil))
      (condition-case nil
          (forward-sexp 1)
        (error nil ))
      (setq end (point))
      (condition-case nil 
          (backward-sexp 1)
        (error nil ))
      (setq start (point))
      (emacspeak-handle-auditory-icon-at-point)
      (emacspeak-handle-action-at-point)
      (cond
       ((null arg ))
       ((> arg 0) (setq start orig))
       ((< arg 0) (setq end orig )))
      (dtk-speak (buffer-substring  start end )))))

(defun emacspeak-speak-page (&optional arg)
  "Speak a page. 
  With prefix arg, speaks rest of current page. 
Negative prefix arg will read from start of current page to point.
If voice-lock-mode is on, then it will use any defined personality. "
  (interactive "P")
  (when (listp arg) (setq arg (car arg )))
  (save-excursion 
    (let ((orig (point))
          (inhibit-point-motion-hooks t)
          (start nil)
          (end nil))
      (mark-page)
      (setq start  (point))
      (emacspeak-handle-auditory-icon-at-point)
      (emacspeak-handle-action-at-point)
      (setq end  (mark))
      (cond
       ((null arg ))
       ((> arg 0) (setq start orig))
       ((< arg 0) (setq end orig )))
      (dtk-speak (buffer-substring start end )))))


(defun emacspeak-speak-paragraph(&optional arg)
  "Speak paragraph.
With prefix arg, speaks rest of current paragraph.
Negative prefix arg will read from start of current paragraph to point.
If voice-lock-mode is on, then it will use any defined personality. "
  (interactive "P")
  (when (listp arg) (setq arg (car arg )))
  (save-excursion 
    (let ((orig (point))
          (inhibit-point-motion-hooks t)
          (start nil)
          (end nil))
      (forward-paragraph 1) 
      (setq end (point))
      (backward-paragraph 1)
      (setq start (point))
      (emacspeak-handle-auditory-icon-at-point)
      (emacspeak-handle-action-at-point)
      (cond
       ((null arg ))
       ((> arg 0) (setq start orig))
       ((< arg 0) (setq end orig )))
      (dtk-speak (buffer-substring  start end )))))

;;}}}
;;{{{  Speak buffer objects such as help, completions minibuffer etc 

(defun emacspeak-speak-buffer (&optional arg) 
  "Speak current buffer  contents.
With prefix arg, speaks the rest of the buffer from point.
Negative prefix arg speaks from start of buffer to point. "
  (interactive "P" )
  (when (listp arg) (setq arg (car arg )))
  (let ((start nil )
        (end nil))
    (emacspeak-handle-auditory-icon-at-point)
    (cond
     ((null arg)
      (setq start (point-min)
            end (point-max)))
     ((> arg 0)
      (setq start (point)
            end (point-max)))
     (t (setq start (point-min)
              end (point))))
    (dtk-speak (buffer-substring start end ))))
(defun emacspeak-speak-help(&optional arg)
  "Speak help buffer if one present.
With prefix arg, speaks the rest of the buffer from point.
Negative prefix arg speaks from start of buffer to point."
  (interactive "P")
  (let ((help-buffer (get-buffer "*Help*")))
    (cond
     (help-buffer
      (save-excursion
	(set-buffer help-buffer)
	(emacspeak-speak-buffer arg )))
     (t (dtk-speak "First ask for help" )))))

(defun emacspeak-speak-completions()
  "Speak completions  buffer if one present."
  (interactive )
  (let ((completions-buffer (get-buffer "*Completions*"))
        (start nil)
        (end nil )
        (continue t))
    (cond
     ((and completions-buffer
           (window-live-p (get-buffer-window completions-buffer )))
      (save-window-excursion
        (save-match-data 
          (select-window  (get-buffer-window completions-buffer ))
        (goto-char (point-min))
        (forward-line 3)
        (while continue
          (setq start (point) 
                end (or  (re-search-forward "\\( +\\)\\|\n"  (point-max) t)
                         (point-max )))
          (dtk-speak (buffer-substring start end ) t) ;wait 
          (setq continue  (sit-for 1))
          (if (eobp) (setq continue nil )))) ;end while
      (discard-input)
      (goto-char start )
      (choose-completion )))
    (t (dtk-speak "No completions" )))))

(defun emacspeak-speak-minibuffer(&optional arg) 
  "Speak the minibuffer contents
 With prefix arg, speaks the rest of the buffer from point.
Negative prefix arg speaks from start of buffer to point."
  (interactive "P" )
  (let ((minibuff (window-buffer (minibuffer-window ))))
    (save-excursion
      (set-buffer minibuff)
      (emacspeak-speak-buffer arg))))
(or (string-match "19.30" emacs-version)
    (progn 
(defun next-completion (n)
  "Move to the next item in the completion list.
WIth prefix argument N, move N items (negative N means move backward)."
  (interactive "p")
  (while (and (> n 0) (not (eobp)))
    (let ((prop (get-text-property (point) 'mouse-face))
	  (end (point-max)))
      ;; If in a completion, move to the end of it.
      (if prop
	  (goto-char (next-single-property-change (point) 'mouse-face nil end)))
      ;; Move to start of next one.
      (goto-char (next-single-property-change (point) 'mouse-face nil end)))
    (setq n (1- n)))
  )

(defun previous-completion (n)
  "Move to the previous item in the completion list."
  (interactive "p")
  (setq n (- n ))
  (while (and (< n 0) (not (bobp)))
    (let ((prop (get-text-property (1- (point)) 'mouse-face))
	  (end (point-min)))
      ;; If in a completion, move to the start of it.
      (if prop
	  (goto-char (previous-single-property-change
		      (point) 'mouse-face nil end)))
      ;; Move to end of the previous completion.
      (goto-char (previous-single-property-change (point) 'mouse-face nil end))
      ;; Move to the start of that one.
      (goto-char (previous-single-property-change (point) 'mouse-face nil end)))
    (setq n (1+ n))))


(declaim (special completion-list-mode-map))
(or completion-list-mode-map
    (make-sparse-keymap ))
(define-key completion-list-mode-map '[right] 'next-completion)
(define-key completion-list-mode-map '[left] 'previous-completion)

)) ;; end emacs pre-19.30 specials 



(defun emacspeak-get-current-completion-from-completions  ()
  "Return the completion string under point in the
*Completions* buffer."
  (let (beg end completion)
    (if (and (not (eobp)) (get-text-property (point) 'mouse-face))
	(setq end (point) beg (1+ (point))))
    (if (and (not (bobp)) (get-text-property (1- (point)) 'mouse-face))
	(setq end (1- (point)) beg (point)))
    (if (null beg)
	(error "No current  completion "))
    (setq beg (or 
               (previous-single-property-change beg 'mouse-face)
               (point-min)))
    (setq end (or (next-single-property-change end 'mouse-face) (point-max)))
    (setq completion (buffer-substring beg end))))

;;}}}

;;}}}
;;{{{ mail check

(defvar emacspeak-mail-spool-file
  (concat "/usr/spool/mail/"
          (user-login-name))
  "Name of mail spool file examined by Emacspeak to alert you about
newly arrived mail.")
  
(defsubst emacspeak-get-file-modification-time (filename)
  (or                                (nth 5 (file-attributes filename ))
                                     0))

(defsubst emacspeak-get-file-size (filename)
  (or (nth 7 (file-attributes filename))
      0))

(defvar emacspeak-mail-last-alerted-time 0
  "Records the least  significant 16 digits of the time when
 the user was last alerted to the arrival of new mail.
Alert the user only if mail has arrived since this time in the future.")

(defsubst emacspeak-mail-get-last-mail-arrival-time ()
  (declare (special emacspeak-mail-spool-file))
  (condition-case                                nil

      (nth
       1(emacspeak-get-file-modification-time emacspeak-mail-spool-file))
    (error 0)))
                                     

(defun emacspeak-mail-alert-user ()
  "Alerts user about the arrival of new mail"
  (declare (special emacspeak-mail-last-alerted-time
                    emacspeak-mail-spool-file))
  (let ((mod-time (emacspeak-mail-get-last-mail-arrival-time))
        (size (emacspeak-get-file-size emacspeak-mail-spool-file)))
    (cond
     ((and (> mod-time emacspeak-mail-last-alerted-time)
           (> size 0))
      (emacspeak-auditory-icon 'new-mail)
      (setq emacspeak-mail-last-alerted-time mod-time ))
     (t(setq emacspeak-mail-last-alerted-time mod-time )
      nil))))

(defvar emacspeak-mail-alert t
  "If t, emacspeak will alert you about newly arrived mail
with an auditory icon when
displaying the mode line.
Do not set this variable by hand --use command
emacspeak-toggle-mail-alert bound to \\[emacspeak-toggle-mail-alert].")

(defun emacspeak-toggle-mail-alert (&optional prefix)
  "Toggle state of  Emacspeak  mail alert.
Interactive prefix arg means toggle  the global default value, and then set the
current local  value to the result.
Turning on this option results in Emacspeak producing an auditory icon
indicating the arrival  of new mail when displaying the mode line."
  (interactive  "P")
  (declare  (special  emacspeak-mail-alert))
  (cond
   (prefix
    (setq-default  emacspeak-mail-alert
                   (not  (default-value 'emacspeak-mail-alert )))
    (setq emacspeak-mail-alert (default-value 'emacspeak-mail-alert )))
   (t (make-local-variable'emacspeak-mail-alert)
      (setq emacspeak-mail-alert 
	    (not emacspeak-mail-alert ))))
  (emacspeak-auditory-icon
   (if emacspeak-mail-alert 'on 'off))
  (message "Turned %s mail alert  %s "
           (if emacspeak-mail-alert "on" "off" )
	   (if prefix "" "locally")))

;;}}}
;;{{{  Speak mode line information 

(defun emacspeak-speak-mode-line ()
  "Speak the mode-line. "
  (interactive)
  (declare (special  mode-name major-mode
                     emacspeak-mail-alert))
  (dtk-stop)
  (emacspeak-dtk-sync)
  (force-mode-line-update)
  (let ((dtk-stop-immediately nil )) 
    (when (buffer-modified-p )
      (dtk-tone 700 70))
    (when buffer-read-only
      (dtk-tone 250 50))
    (when  emacspeak-mail-alert
        (and (emacspeak-mail-alert-user)
             (dtk-tone 450 75)))
    (dtk-speak
     (format  "%s  %s"
              (if  (buffer-file-name ) 
                  (file-name-nondirectory  (buffer-file-name ))
                (buffer-name ))
              (if  major-mode major-mode "")))))

(defun emacspeak-speak-minor-mode-line ()
  "Speak the minor mode-information. "
  (interactive)
  (declare (special minor-mode-alist))
  (force-mode-line-update)
  (let ((dtk-stop-immediately nil ))
    (dtk-speak
     (format "Active minor modes:  %s" 
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
              minor-mode-alist " ")))))


(defun emacspeak-speak-line-number ()
"Speak the line number of the current line."
(interactive)
(let ((start (point)))
  (save-excursion
    (goto-char (point-min))
    (message "Line %s"
     (+ 1 (count-lines start (point)))))))
;;}}}
;;{{{  Speak text without moving point

;;; Functions to browse without moving:
(defun emacspeak-read-line-internal(arg)
  "Read a line without moving.
Line to read is specified relative to the current line, prefix args gives the
offset. Default  is to speak the previous line. "
  (save-excursion
    (cond
     ((zerop arg) (emacspeak-speak-line ))
     ((zerop (forward-line arg))
      (emacspeak-speak-line ))
     (t (dtk-speak "Not that many lines in buffer. ")))))

(defun emacspeak-read-previous-line(&optional arg)
  "Read previous line, specified by an offset, without moving.
Default is to read the previous line. "
  (interactive "p")
  (emacspeak-read-line-internal (- (or arg 1 ))))

(defun emacspeak-read-next-line(&optional arg)
  "Read next line, specified by an offset, without moving.
Default is to read the next line. "
  (interactive "p")
  (emacspeak-read-line-internal (or arg 1 )))

(defun emacspeak-read-word-internal(arg)
  "Read a word without moving.
word  to read is specified relative to the current word, prefix args gives the
offset. Default  is to speak the previous word. "
  (save-excursion
    (cond
     ((= arg 0) (emacspeak-speak-word ))
     ((forward-word arg)
      (skip-syntax-forward " ")
      (emacspeak-speak-word 1 ))
     (t (dtk-speak "Not that many words. ")))))

(defun emacspeak-read-previous-word(&optional arg)
  "Read previous word, specified as a prefix arg, without moving.
Default is to read the previous word. "
  (interactive "p")
  (emacspeak-read-word-internal (- (or arg 1 ))))

(defun emacspeak-read-next-word(&optional arg)
  "Read next word, specified as a numeric  arg, without moving.
Default is to read the next word. "
  (interactive "p")
  (emacspeak-read-word-internal  (or arg 1 )))

;;}}}
;;{{{  Speak misc information e.g. time, version, current-kill  etc 

(defun emacspeak-speak-time ()
  "Speak the time. "
  (interactive)
  (dtk-speak (current-time-string )))

(defun emacspeak-speak-version ()
  "Announce version information for running emacspeak. "
  (interactive)
  (declare (special emacspeak-version))
  (dtk-speak
   (format "You are using emacspeak %s "
           emacspeak-version )))

(defun emacspeak-speak-current-kill (count)
  "Speak the current kill entry.
This is the text that will be yanked in by the next \\[yank].
Prefix numeric arg, count, specifies that the text that will be yanked as a
result of a
\\[yank]  followed by count-1 \\[yank-pop]
be spoken.
 The kill number that is spoken says what numeric prefix arg to give
to command yank."
  (interactive "p")
  (let ((voice-lock-mode t)
        (context
         (format "kill %s "
                 (if current-prefix-arg (+ 1 count)  1 ))))
    (put-text-property 0 (length context)
                       'personality 'annotation-voice context )
  (dtk-speak
   (concat
    context 
   (current-kill (if current-prefix-arg count 0)t)))))

(defun emacspeak-zap-dtk () 
  "Send this command to the Dectalk directly. "
  (interactive) 
  (dtk-dispatch 
   (read-from-minibuffer"Enter Dectalk command string: ")))


(defun emacspeak-dial-dtk (number)
  "Prompt for and dial a phone number with the Dectalk."
  (interactive "sEnter phone number to dial:")
  (let ((dtk-stop-immediately nil))
  (dtk-dispatch (format "[:dial %s]" number))
  (sit-for 4)))

(defun emacspeak-dtk-speak-version ()
  "Use this to find out which version of the Dectalk firmware you
have."
  (interactive)
  (dtk-dispatch 
    "this is [:version speak]  "))

;;}}}
;;{{{ speaking marks

;;; Intelligent mark feedback for emacspeak:
;;;

(defun emacspeak-speak-current-mark (count)
  "Speak the line containing the mark.  With no argument, speaks the
line containing the mark--this is where exchange-point-and-mark
\\[exchange-point-and-mark] would jump.  Numeric prefix arg 'n' speaks
line containing mark 'n' where 'n' is one less than the number of
times one has to jump using set-mark-command to get to this marked
position.  The location of the mark is indicated by an aural highlight
achieved by a change in voice personality."
  (interactive "p")
  (unless (mark)
    (error "No marks set in this buffer"))
  (when (and current-prefix-arg 
             (> count (length mark-ring)))
    (error "Not that many marks in this buffer"))
  (let ((voice-lock-mode t)
        (line nil)
        (position nil)
        (context
         (format "mark %s "
                 (if current-prefix-arg count   0 ))))
    (put-text-property 0 (length context)
                       'personality 'annotation-voice context )
    (setq position
          (if current-prefix-arg
              (elt mark-ring(1-  count))
            (mark)))
    (save-excursion
      (goto-char position)
      (ems-set-personality-temporarily
       position (1+ position) 'paul-animated
       (setq line
             (thing-at-point  'line ))))
    (dtk-speak
     (concat context line))))

;;}}}
;;{{{  Execute command repeatedly, browse 

(defun emacspeak-execute-repeatedly (command)
  "Execute command repeatedly. "
  (interactive "CCommand to execute repeatedly:")
  (let ((key "")
        (position (point ))
        (continue t )
        (message (format "Press any key to execute %s again" command)))
    (while continue
      (call-interactively command )
      (cond
       ((= (point) position ) (setq continue nil))
       (t (setq position (point))
          (setq key
                (let ((dtk-stop-immediately nil ))
                  ;(sit-for 2)
                  (read-key-sequence message )))
          (when(and (stringp key)
                    (=  7  (string-to-char key )))
            (dtk-stop)
            (setq continue nil )))))
    (dtk-speak "Exited continuous mode. ")))

(defun emacspeak-speak-continuously ()
  "Speak a buffer continuously.
First prompts using the minibuffer for the kind of action to perform after
speaking each chunk.
E.G. speak a line at a time etc.
Speaking commences at current buffer position.
Pressing  C-g breaks out, leaving point on last chunk that was spoken.
 Any other key continues to speak the buffer. "
  (interactive)
  (let ((command (key-binding
                  (read-key-sequence "Press key sequence to repeat: "))))
    (unless command
      (error "You specified an invalid key sequence. " ))
    (emacspeak-execute-repeatedly command)))

(defvar emacspeak-read-line-by-line-quotient 10
  "Determines behavior of emacspeak-read-line-by-line.")

(defvar emacspeak-read-by-line-by-line-tick 1.0
  "Granularity of time for reading line-by-line. ")

(defun emacspeak-read-line-by-line ()
  "Read line by line until interrupted"
  (interactive)
  (let ((count 0)
        (line-length 0)
        (continue t))
    (while 
        (and continue
             (not (eobp)))
      (setq dtk-last-output "")
      (call-interactively 'next-line)
      (setq line-length (length  (thing-at-point 'line)))
      (setq count 0)
      (when (> line-length 0)
        (while(and (< count
                      (1+ (/ line-length emacspeak-read-line-by-line-quotient)))
                   (setq continue
                         (sit-for
                          emacspeak-read-by-line-by-line-tick 0 nil ))
                   (not (string-match  "done" dtk-last-output))
                   (incf count))))))
  (emacspeak-auditory-icon 'task-done)
  (message "done moving "))

;;}}}
;;{{{  Learn mode 

(defun emacspeak-learn-mode ()
  "Helps you learn the keys. You can press keys and hear what they do.
To leave, press \\[keyboard-quit]. "
  (interactive)
  (let ((continue t )) 
    (while continue 
      (call-interactively 'describe-key-briefly)
      (if (= last-input-event 7) 
          (setq continue nil )))
      (message "Leaving learn mode. ")))

;;}}}
;;{{{ comint

(defvar emacspeak-comint-autospeak t 
  "Says if comint output is automatically spoken.
  Do not set this by hand, use command 
  `emacspeak-toggle-comint-autospeak` bound to 
  \\[emacspeak-toggle-comint-autospeak]")

(defun emacspeak-toggle-comint-autospeak (&optional prefix)
  "Toggle state of  Emacspeak  
  comint autospeak. When turned on, comint output is automatically spoken. 
  Turn this on if you want your shell to speak its results.
  Interactive prefix arg means toggle  the global default value, and then set the
  current local  value to the result. "
  (interactive  "P")
  (declare  (special  emacspeak-comint-autospeak ))
  (cond
   (prefix
    (setq-default  emacspeak-comint-autospeak
                   (not  (default-value 'emacspeak-comint-autospeak )))
    (setq emacspeak-comint-autospeak (default-value 'emacspeak-comint-autospeak )))
   (t (make-local-variable 'emacspeak-comint-autospeak)
      (setq emacspeak-comint-autospeak 
	    (not emacspeak-comint-autospeak ))))
  (emacspeak-auditory-icon
   (if emacspeak-comint-autospeak 'on 'off))
  (message "Turned %s comint autospeak %s "
           (if emacspeak-comint-autospeak "on" "off" )
	   (if prefix "" "locally")))

;;}}}
;;{{{   quiten messages
(defvar emacspeak-speak-messages t
  "If nil, emacspeak will not speak messages as they are echoed to the message
area.
Do not set this variable by hand.
Use command emacspeak-toggle-speak-messages bound to
\\[emacspeak-toggle-speak-messages]. ")

(defun emacspeak-toggle-speak-messages ()
  "Toggle the state of whether emacspeak echoes messages. "
  (interactive)
  (declare (special emacspeak-speak-messages ))
  (and (y-or-n-p
        (format "This will %s  emacs speaking messages. Are you sure"
                 (if emacspeak-speak-messages " stop " " start ")))
  (setq  emacspeak-speak-messages
         (not emacspeak-speak-messages))
  (emacspeak-auditory-icon
   (if emacspeak-speak-messages  'on 'off))
  (dtk-speak
   (format "Turned  speaking of emacs messages %s"
           (if emacspeak-speak-messages  " on" " off")))))
  
;;}}}
;;{{{  Moving across fields:

;;; For the present, we define a field
;;; as a contiguous series of non-blank characters
;;; helper function: speak a field
(defsubst  emacspeak-speak-field (start end )
  (let ((header (or (get-text-property start  'field-name) "")))
  (dtk-speak
       (concat
             (progn (put-text-property 0 (length header )
                                       'personality 'annotation-voice
                                       header )
                    header )
        " "
        (buffer-substring  start end)))))


(defun emacspeak-speak-current-field ()
  "Speak current field.  A field is
  defined currently as a sequence of non-white space characters.  may be made
  mode specific later. "
  (interactive)
  (let ((start nil ))
    (save-excursion
      (skip-syntax-backward "^ ")
      (setq start (point ))
      (skip-syntax-forward "^ ")
      (emacspeak-speak-field start (point )))))

(defun emacspeak-speak-next-field ()
  "Skip across the next contiguous sequence of non-blank characters,
and speak it.
Useful in moving across fields.
Will be improved if it proves useful."
  (interactive)
  (let ((start nil ))
    (skip-syntax-forward "^ ")
    (skip-syntax-forward " ")
    (setq start (point ))
    (save-excursion
      (skip-syntax-forward "^ ")
      (emacspeak-speak-field start (point)))))

(defun emacspeak-speak-previous-field ()
  "Skip backwards across the next contiguous sequence of non-blank characters,
and speak it.
Useful in moving across fields.
Will be improved if it proves useful."
  (interactive)
  (let ((start nil ))
    (skip-syntax-backward " ")
    (setq start (point ))
    (skip-syntax-backward "^ ")
    (emacspeak-speak-field (point ) start)))

(defun emacspeak-speak-current-column ()
  "Speak the current column"
  (interactive)
  (message "Point at column %d" (current-column )))

(defun emacspeak-speak-current-percentage ()
  "Announce the percentage into the current buffer."
  (interactive)
  (let* ((pos (point))
	 (total (buffer-size))
	 (percent (if (> total 50000)
		      ;; Avoid overflow from multiplying by 100!
		      (/ (+ (/ total 200) (1- pos)) (max (/ total 100) 1))
		    (/ (+ (/ total 2) (* 100 (1- pos))) (max total 1)))))
    (message "Point is  %d%% into  the current buffer" percent )))

;;}}}
;;{{{  Speak the last message again:

(defun emacspeak-speak-message-again ()
  "Speak the last message from Emacs once again. "
  (interactive)
  (declare (special emacspeak-last-message ))
  (dtk-speak   emacspeak-last-message ))

(defun emacspeak-announce (announcement)
  "Speak the announcement, if possible.
Otherwise just display a message. "
  (declare (special dtk-speaker-process ))
  (if (and (featurep 'dtk-speak)
           dtk-speaker-process 
           (eq 'run  (process-status dtk-speaker-process )))
      (dtk-speak announcement )
    (message announcement)))

;;}}}
;;{{{  Using emacs's windows usefully:
;;Return current window contents
(defsubst emacspeak-get-window-contents ()
  (let ((start nil))
    (save-excursion
      (move-to-window-line 0)
      (setq start (point))
      (move-to-window-line -1)
      (end-of-line)
      (buffer-substring start (point)))))

(defun emacspeak-speak-window-information ()
  "Speaks information about current windows."
  (interactive)
  (message "Current window has %s lines and %s columns"
           (window-height) (window-width)))

(defun emacspeak-speak-current-window ()
  "Speak contents of current window.
 Speaks entire window irrespective of point."
  (interactive)
    (emacspeak-speak-region (window-start) (window-end )))

(defun emacspeak-speak-other-window (&optional arg)
  "Speak contents of `other' window.
 Speaks entire window irrespective of point.
Semantics  of `other' is the same as for the builtin emacs command
`other-window'. "
  (interactive "nSpeak window")
  (save-window-excursion
    (other-window arg )
    (emacspeak-speak-region (window-start) (window-end ))))

(defun emacspeak-speak-next-window ()
  "Speak the next window"
  (interactive)
  (emacspeak-speak-other-window 1 ))

(defun emacspeak-speak-previous-window ()
  "Speak the previous window"
  (interactive)
  (emacspeak-speak-other-window -1 ))


(defun  emacspeak-owindow-scroll-up ()
  "Scroll up the window that command other-window would move to.
Speak the window contents after scrolling. "
  (interactive)
  (let ((error nil)
        (start
         (save-window-excursion
           (other-window 1)
           (window-start  ))))
    (condition-case nil
        (scroll-other-window  nil)
      (error (setq error t)))
    (if error
        (message "There is no other window. ")
      (save-window-excursion
          (other-window 1)
          (cond
           ((= start (window-start) )
            (message "At bottom of other window. "))
           (t (emacspeak-auditory-icon 'scroll)
              (emacspeak-speak-region (window-start ) (window-end ))))))))
          

(defun  emacspeak-owindow-scroll-down ()
  "Scroll down  the window that command other-window would move to.
Speak the window contents after scrolling. "
  (interactive)
  (let ((error nil)
        (start
         (save-window-excursion
           (other-window 1)
           (window-start )))
        (height (save-window-excursion
                  (other-window 1)
                  (window-height))))
    (condition-case nil
        (scroll-other-window  (- height ))
      (error (setq error t )))
      (if error
          (message "There is no other window. ")
        (save-window-excursion
          (other-window 1)
          (cond
           ((= start (window-start) )
            (message "At top of other window. "))
           (t (emacspeak-auditory-icon 'scroll)
              (emacspeak-speak-region (window-start) (window-end ))))))))

(defun emacspeak-owindow-next-line (count)
  "Move to the next line in the other window and speak it.
Numeric prefix arg can specify number of lines to move. "
  (interactive "p")
  (setq count (or count 1 ))
  (let  ((residue nil )
         (old-buffer (current-buffer )))
    (unwind-protect
        (progn 
          (set-buffer (window-buffer (next-window )))
          (end-of-line)
          (setq residue (forward-line count))
          (cond
           ((> residue 0) (message "At bottom of other window. "))
           (t (set-window-point (get-buffer-window (current-buffer ))
                                (point))
            (emacspeak-speak-line ))))
      (set-buffer old-buffer ))))

(defun emacspeak-owindow-previous-line (count)
  "Move to the next line in the other window and speak it.
Numeric prefix arg can specify number of lines to move. "
  (interactive "p")
  (setq count (or count 1 ))
  (let  ((residue nil )
         (old-buffer (current-buffer )))
    (unwind-protect
        (progn 
          (set-buffer (window-buffer (next-window )))
          (end-of-line)
          (setq residue (forward-line (- count)))
          (cond
           ((> 0 residue) (message "At top of other window. "))
           (t (set-window-point (get-buffer-window (current-buffer ))
                                (point))
            (emacspeak-speak-line ))))
      (set-buffer old-buffer ))))

(defun emacspeak-owindow-speak-line ()
  "Speak the current line in the other window.
"
  (interactive)
(let  ((old-buffer (current-buffer )))
    (unwind-protect
        (progn 
          (set-buffer (window-buffer (next-window )))
          (goto-char (window-point ))
          (emacspeak-speak-line))
      (set-buffer old-buffer ))))  
(defun emacspeak-speak-predefined-window (&optional arg)
  "Speak one of the first 10 windows on the screen.
In general, you'll never have emacs split the screen into more than
two or three.
Argument arg determines the 'other' window to speak.
 Speaks entire window irrespective of point.
Semantics  of `other' is the same as for the builtin emacs command
`other-window'. "
  (interactive "P")
  (let ((window
         (condition-case nil
             (read (format "%c" last-input-event ))
           (error nil ))))
    (or (numberp window)
        (setq window
              (read-minibuffer "Window   between 1 and 9 to speak")))
    (save-window-excursion
    (other-window window )
    (emacspeak-speak-region (window-start) (window-end )))))


;;}}}
;;{{{  Intelligent interactive commands for reading:

;;; Prompt the user if asked to prompt.
;;; Prompt is:
;;; press 'b' for beginning of unit,
;;; 'r' for rest of unit,
;;; any other key for entire unit
;;; returns 1, -1, or nil accordingly.
;;; If prompt is nil, does not prompt: just gets the input

(defun emacspeak-ask-how-to-speak (unit-name prompt)
  (if prompt
      (message 
       (format "Press s to speak start of %s, r for rest of  %s. \
 Any  key for entire %s. "
               unit-name unit-name unit-name )))
  (let ((char (read-char )))
    (cond
     ((= char ?s) -1)
     ((= char ?r) 1)
     (t nil )))
  )

(defun emacspeak-speak-buffer-interactively ()
  "Speak the start of, rest of, or the entire buffer.
's' to speak the start.
'r' to speak the rest.
any other key to speak entire buffer. "
  (interactive) 
  (emacspeak-speak-buffer
   (emacspeak-ask-how-to-speak "buffer" (sit-for 1 0 nil ))))



(defun emacspeak-speak-help-interactively ()
  "Speak the start of, rest of, or the entire help.
's' to speak the start.
'r' to speak the rest.
any other key to speak entire help. "
  (interactive) 
  (emacspeak-speak-help
   (emacspeak-ask-how-to-speak "help" (sit-for 1 0 nil ))))


(defun emacspeak-speak-line-interactively ()
  "Speak the start of, rest of, or the entire line.
's' to speak the start.
'r' to speak the rest.
any other key to speak entire line. "
  (interactive) 
  (emacspeak-speak-line
   (emacspeak-ask-how-to-speak "line" (sit-for 1 0 nil ))))

(defun emacspeak-speak-paragraph-interactively ()
  "Speak the start of, rest of, or the entire paragraph.
's' to speak the start.
'r' to speak the rest.
any other key to speak entire paragraph. "
  (interactive) 
  (emacspeak-speak-paragraph
   (emacspeak-ask-how-to-speak "paragraph" (sit-for 1 0 nil ))))

(defun emacspeak-speak-page-interactively ()
  "Speak the start of, rest of, or the entire page.
's' to speak the start.
'r' to speak the rest.
any other key to speak entire page. "
  (interactive) 
  (emacspeak-speak-page
   (emacspeak-ask-how-to-speak "page" (sit-for 1 0 nil ))))

(defun emacspeak-speak-word-interactively ()
  "Speak the start of, rest of, or the entire word.
's' to speak the start.
'r' to speak the rest.
any other key to speak entire word. "
  (interactive) 
  (emacspeak-speak-word
   (emacspeak-ask-how-to-speak "word" (sit-for 1 0 nil ))))

(defun emacspeak-speak-sexp-interactively ()
  "Speak the start of, rest of, or the entire sexp.
's' to speak the start.
'r' to speak the rest.
any other key to speak entire sexp. "
  (interactive) 
  (emacspeak-speak-sexp
   (emacspeak-ask-how-to-speak "sexp" (sit-for 1 0 nil ))))

;;}}}
;;{{{  emacs' register related commands

;;; Things like view-register are useful.

(defun emacspeak-view-register ()
  "Display the contents of a register, and then speak it. "
  (interactive)
  (call-interactively 'view-register)
  (save-excursion (set-buffer "*Output*")
                  (dtk-speak (buffer-string ))))

;;}}}
;;{{{  emacs rectangles and regions: 

(eval-when (compile) (require 'rect))
;;; These help you listen to columns of text. Useful for tabulated data
(defun emacspeak-speak-rectangle ( start end )
  "Speak a rectangle of text.
Rectangle is delimited by point and mark.
When call from a program,
arguments specify the start and end of the rectangle."
  (interactive  "r")
  (require 'rect) 
  (dtk-speak-list (extract-rectangle start end )))
    

;;; helper function: emacspeak-put-personality
;;; sets property 'personality to personality 
(defsubst emacspeak-put-personality (start end personality )
  (put-text-property start end 'personality personality ))

;;; Compute table of possible voices to use in completing-read 
(defsubst  emacspeak-possible-voices ()
  (declare (special dtk-voice-table ))
  (loop for key being the hash-keys of dtk-voice-table
        collect  (cons
                  (symbol-name key)
                  (symbol-name key))))


(defun emacspeak-voicify-rectangle (start end &optional personality )
  "Voicify the current rectangle.
Prompts for personality  with completion when called interactively.
 When calling from a program,arguments are
start end personality"
  (interactive "r")
  (require 'rect)
  (require 'voice-lock )
  (or voice-lock-mode (setq voice-lock-mode t ))
  (let ((personality-table (emacspeak-possible-voices )))
    (when (interactive-p) 
      (setq personality
            (read
             (completing-read "Use personality: "
                              personality-table nil t ))))
    (operate-on-rectangle
     (function (lambda ( start-seg begextra endextra )
                 (emacspeak-put-personality start-seg  (point) personality )))
     start end  nil)))

(defun emacspeak-voicify-region (start end &optional personality )
  "Voicify the current region.
Prompts for personality  with completion when called interactively.
 When calling from a program,arguments are
start end personality"
  (interactive "r")
  (require 'voice-lock )
  (or voice-lock-mode (setq voice-lock-mode t ))
  (let ((personality-table (emacspeak-possible-voices )))
    (when (interactive-p) 
      (setq personality
            (read
             (completing-read "Use personality: "
                              personality-table nil t ))))
    (put-text-property start end 'personality personality )))

(defun emacspeak-put-text-property-on-rectangle   (start end prop value )
  "Set property to value for each line in the rectangle."
  (require 'rect)
  (operate-on-rectangle
   (function (lambda ( start-seg begextra endextra )
               (put-text-property  start-seg (point)    prop value  )))
   start end  nil ))

;;}}}
;;{{{  Matching delimiters:
;;; A modified blink-matching-open that always displays the matching line
;;; in the minibuffer so emacspeak can speak it.

(defun emacspeak-blink-matching-open ()
  "Display matching delimiter in the minibuffer"
  (interactive)
  (declare (special blink-matching-paren-distance))
  (and (> (point) (1+ (point-min)))
       (not (memq (char-syntax (char-after (- (point) 2))) '(?/ ?\\ )))
       blink-matching-paren
       (let* ((oldpos (point))
              (emacspeak-blink-delay 5)
	      (blinkpos)
	      (mismatch))
	 (save-excursion
	   (save-restriction
	     (if blink-matching-paren-distance
		 (narrow-to-region (max (point-min)
					(- (point) blink-matching-paren-distance))
				   oldpos))
	     (condition-case ()
		 (setq blinkpos (scan-sexps oldpos -1))
	       (error nil)))
	   (and blinkpos (/= (char-syntax (char-after blinkpos))
			     ?\$)
		(setq mismatch
		      (/= (char-after (1- oldpos))
			  (matching-paren (char-after blinkpos)))))
	   (if mismatch (setq blinkpos nil))
	   (if blinkpos
	       (progn
                 (goto-char blinkpos)
                  (message   
                             "Matches %s"
                             ;; Show what precedes the open in its line, if anything.
                             (if (save-excursion
                                   (skip-chars-backward " \t")
                                   (not (bolp)))
                                 (buffer-substring (progn (beginning-of-line) (point))
                                                   (1+ blinkpos))
                               ;; Show what follows the open in its line, if anything.
                               (if (save-excursion
                                     (forward-char 1)
                                     (skip-chars-forward " \t")
                                     (not (eolp)))
                                   (buffer-substring blinkpos
                                                     (progn (end-of-line) (point)))
                                 ;; Otherwise show the previous nonblank line.
                                 (concat
                                  (buffer-substring (progn
                                                      (backward-char 1)
                                                      (skip-chars-backward "\n \t")
                                                      (beginning-of-line)
                                                      (point))
                                                    (progn (end-of-line)
                                                           (skip-chars-backward " \t")
                                                           (point)))
                                  ;; Replace the newline and other whitespace with `...'.
                                  "..."
                                  (buffer-substring blinkpos (1+
                                                              blinkpos)))))))
	     (cond (mismatch
		    (message "Mismatched parentheses"))
		   ((not blink-matching-paren-distance)
		    (message "Unmatched parenthesis")))))
       (sit-for emacspeak-blink-delay))))

(defun  emacspeak-use-customized-blink-paren ()
  "Ask Emacs to use a customized blink-paren function
that speaks the line containing the matching opening paren.
We need to call this in case Emacs
is anal and loads its own builtin blink-paren function
which does not talk. "
  (interactive)
  (fset 'blink-matching-open (symbol-function 'emacspeak-blink-matching-open))
  (and (interactive-p)
  (message "Using customized blink-paren function provided by Emacspeak.")))

(emacspeak-use-customized-blink-paren)

;;}}}
;;{{{  Auxillary functions:
(defsubst emacspeak-kill-buffer-carefully (buffer)
  "Kill buffer BUF if it exists."
  (and buffer
       (get-buffer buffer)
       (buffer-name (get-buffer buffer ))
       (kill-buffer buffer)))

(defsubst emacspeak-overlay-get-text (o)
(save-excursion
(set-buffer (overlay-buffer o ))
(buffer-substring
(overlay-start o)
(overlay-end o ))))


;;}}}
;;{{{  moving across blank lines

(defun emacspeak-skip-blank-lines-forward ()
  "Move forward across blank lines and leave point at the beginning of
the first non-blank line.
This line is then spoken.
Signals end of buffer. "
  (interactive)
  (let ((save-syntax (char-syntax 10))
        (skip 0))
    (unwind-protect
        (progn
          (modify-syntax-entry   10 " ")
          (setq skip (skip-syntax-forward " "))
          (cond
           ((zerop skip)
            (message "Did not move. "))
           ((eobp)
            (message "At end of buffer"))
           (t(emacspeak-auditory-icon 'large-movement )
             (emacspeak-speak-line ))))
      (modify-syntax-entry 10 (format "%c" save-syntax )))))

(defun emacspeak-skip-blank-lines-backward ()
  "Move backward  across blank lines and leave point at the beginning of
the first non-blank line.
This line is then spoken.
Signals beginning  of buffer. "
  (interactive)
  (let ((save-syntax (char-syntax 10))
        (skip 0))
    (unwind-protect
        (progn
          (modify-syntax-entry   10 " ")
          (setq skip (skip-syntax-backward " "))
          (cond
           ((zerop skip)
            (message "Did not move. "))
           ((bobp )
            (message "At start  of buffer"))
           (t (beginning-of-line)
            (emacspeak-auditory-icon 'large-movement )
             (emacspeak-speak-line ))))
      (modify-syntax-entry 10 (format "%c" save-syntax )))))




;;}}}
;;{{{  Apply audio annotations

;;; prompt for auditory icon with completion

(defsubst ems-ask-for-auditory-icon ()
  (declare (special emacspeak-sounds-table))
  (intern
   (completing-read  "Select sound:"
                  (mapcar
                   (lambda (xx)
                     (let ((x (car xx)))
                     (list
                      (format "%s" x)
                      (format "%s" x ))))
                   emacspeak-sounds-table))))

(defun emacspeak-audio-annotate-paragraphs (&optional prefix)
  "Set property auditory-icon at front of all paragraphs.
Interactive prefix arg prompts for sound cue to use"
  (interactive "P")
    (save-excursion
      (goto-char (point-max))
      (ems-modify-buffer-safely
       (let ((sound-cue  (if prefix
                             (ems-ask-for-auditory-icon)
                           'paragraph)))
          (while (not (bobp))
            (backward-paragraph)
            (put-text-property  (point) (1+ (point ))
                                'auditory-icon sound-cue ))))))
        
        

;;}}}
;;{{{  Display properties conveniently

;;; Useful for developping emacspeak:
;;; Display selected properties of interest


(defun emacspeak-show-personality-at-point ()
  "Show value of property personality at point. "
  (interactive )
  (emacspeak-show-property-at-point 'personality))

(defvar emacspeak-property-table
  '(("personality"  . "personality")
    ("auditory-icon" . "auditory-icon")
("action" . "action"))
"Properties emacspeak is interested in")

(defun emacspeak-show-property-at-point (&optional property )
  "Show value of property at point.
If optional arg property is not supplied, read it interactively.
Provides completion based on properties that are of interest.
If no property is set, show a message and exit. "
  (interactive
   (let
       ((properties (text-properties-at  (point))))
   (cond
    ((and properties
          (= 2 (length properties )))
     (list (car properties )))
    (properties 
     (list
      (intern
       (completing-read  "Display property: "
                        emacspeak-property-table ))))
    (t (message "No property set at point. ")
       nil))))
  (declare (special emacspeak-property-table))
  (if property
      (message"%s"
               (get-text-property (point) property ))))

;;}}}
;;{{{ Speaking spaces

(defun emacspeak-speak-spaces-at-point ()
  "Speak the white space at point"
  (interactive)
  (cond
   ((not (= 32 (char-syntax (following-char ))))
    (message "Not on white space"))
   (t
       (let ((orig (point))
             (start (save-excursion
                      (skip-syntax-backward " ")
                      (point)))
             (end (save-excursion
                    (skip-syntax-forward " ")
                    (point))))
         (message "Space %s of %s"
                  (1+ (- orig start)) (- end start ))))))

;;}}}
;;{{{   Switching buffers, killing buffers etc 
(defun emacspeak-switch-to-previous-buffer  ()
  "Switch to most recently used interesting buffer"
  (interactive)
  (switch-to-buffer (other-buffer))
  (emacspeak-speak-mode-line )
  (emacspeak-auditory-icon 'select-object ))

(defun emacspeak-kill-buffer-quietly   ()
  "Kill current buffer without asking for confirmation."
  (interactive)
  (kill-buffer nil )
  (emacspeak-auditory-icon 'close-object)
  (emacspeak-speak-mode-line ))

;;}}}
;;{{{  Generate documentation:
(defsubst emacspeak-list-emacspeak-commands ()
  (let ((commands nil ))
  (mapatoms
       (function
        (lambda (f)
          (when
              (and (fboundp f)
                   (commandp f)
                   (not (string-match "ad-Orig" (symbol-name f)))
                   (not (eq f 'emacspeak))
                   (or (string-match "emacspeak" (symbol-name f))
                       (string-match "dtk" (symbol-name f))))
            (push f commands)))))
  (setq commands
        (sort commands 'string-lessp))
  commands))

(defun emacspeak-generate-documentation (filename)
  "Generates docs for all emacspeak commands.
Prompts for filename in which to save the documentation.
Warning! Contents of file filename will be overwritten."
  (interactive "FEnter filename to save DOC in: ")
  (let ((buffer (find-file-noselect filename)))
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (mapcar 
       (function
        (lambda (f)
          (insert "------------------------------------------------------------\n")
          (insert (format "Command: %s" f))
          (insert (format "\tKey Sequence:%s\n\n"
                          (or (mapconcat
                               'key-description
                               (where-is-internal f) " ")
                              "Not bound to any key")))
          (insert (documentation f))
          (insert "\n\n")))
       (emacspeak-list-emacspeak-commands))
      (save-buffer)))
  (emacspeak-auditory-icon 'task-done))

;;}}}
;;{{{  translate faces to voices
(defun voice-lock-voiceify-faces ()
  (interactive)
  (declare (special voice-lock-mode))
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t )
          (face nil )
          (start (point)))
      (setq voice-lock-mode t)
      (unwind-protect
          (while (not (eobp))
        (setq face (get-text-property (point) 'face ))
        (goto-char
         (or (next-single-property-change (point) 'face )
             (point-max)))
        (put-text-property start  (point)
                           'personality
                           (if (listp face)
                               (car face)
                             face ))
        (setq start (point)))
        (setq inhibit-read-only nil)))))
;;; associate voices to standard faces:
(dtk-define-voice-alias 'bold 'paul-smooth)
(dtk-define-voice-alias 'underline 'ursula)
(dtk-define-voice-alias 'fixed 'paul-monotone)
(dtk-define-voice-alias 'italic 'paul-animated)
(dtk-define-voice-alias 'excerpt 'annotation-voice )


;;}}}
(provide 'emacspeak-speak )
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; end: 


(defun emacspeak-switch-to-completions-window ()
  "Jump to the *Completions* buffer if it is active. "
  (interactive)
  (let ((completions-buffer (get-buffer "*Completions*")))
    (cond
     ((and completions-buffer
           (window-live-p (get-buffer-window completions-buffer )))
          (select-window  (get-buffer-window completions-buffer ))
          (when (interactive-p)
            (unless (get-text-property (point) 'mouse-face)
              (goto-char (next-single-property-change (point)
                                                      'mouse-face )))
               (dtk-speak
           (emacspeak-get-current-completion-from-completions)))
          (emacspeak-auditory-icon 'select-object))
     (t (message "No completions")))))

;;}}}
