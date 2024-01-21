;; @(#) czech.el -- package for effective writing Czech things in Emacs

;; @(#) $Id: czech.el,v 3.4 1997/02/26 20:32:07 pdm Exp $	
;; @(#) $Keywords: i18n, Czech, keyboard $
;; $KnownCompatibility: 19.34, XEmacs 19.14 $

;; This file is *NOT* part of GNU Emacs nor XEmacs.

;; Copyright (C) 1995, 1996, 1997 Milan Zamazal

;; Author:       Milan Zamazal <pdm@fi.muni.cz>
;; Maintainer:   Milan Zamazal <pdm@fi.muni.cz>
;; Remark:       Don't laugh too loudly while reading this file, please.

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, version 2 of the License.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs and/or this package.  If you did not, write to the
;; Free Software Foundation, Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;;; Commentary:

;; This is the initialization file for emacs-czech.  It contains all user
;; variable declarations, autoload declarations, keybindings assignment, menu
;; definition, etc.  It also sets some basic things (keyboard, fonts, ...)
;; according to values of user variables.
;;
;; When you add `(load "czech")' to your `~/.emacs', you will have defined all
;; things necessary to use all functions from emacs-czech package.
;; You can switch on/off Czech keyboard by typing `M-x cz-keyboard-mode'.
;; You can also explore `Czech' menu which should appear on your menubar.
;; You can possibly also find some useful comments in other `czech-*.el' files.
;;
;; You of course need some ISO 8859-2 fonts to display Czech characters.  If
;; you are on Linux console, you can set them (see `kbd' package
;; documentation).  If you are in X Window System, you can find references to
;; ISO 8859-2 X-fonts packages on
;; `http://sizif.mf.uni-lj.si/linux/cee/iso8859-2.html'.


;;; History:

;; So long, so very long...

;;; Code:


;;; *** Basic initializations ***

(defvar cz-verbose-level 9
  "*Verbosity level of `emacs-czech'.
It should be number between `0' and `9'.
`0' means no messages except errors, `9' means print all messages.
`5' or higher guarantees printing of all warnings.")

(defvar cz-windoze (eq system-type 'windows-nt)
  "Indicates, whether it is Windoze NT version of Emacs.")

(defun cz-message (level &rest message)
  "Prints message if it has enough high level.
MESSAGE is given to function `message' just when LEVEL is less or equal
than value of the variable `cz-verbose-level'."
  (if (<= level cz-verbose-level)
      (apply 'message message)))

(if (eq system-type 'ms-dos)
    (error "Sorry, I do not work under M$-DOG."))

(if cz-windoze
    (cz-message
     8
     "Oh, I'm not sure if working properly under that hell system..."))

(defvar cz-xemacs (string= (substring (emacs-version) 0 6) "XEmacs")
  "Indicates, whether we are running in XEmacs.")

(defun cz-safe-eq (a b)
  "If a's value is void, return nil and no error"
  (and (boundp a) (eq (symbol-value a) b)))
(if cz-xemacs
    (cz-message
     8
     "Warning: Maybe Czech support does not work properly under XEmacs yet."))

(defconst cz-version "$Id: czech.el,v 3.4 1997/02/26 20:32:07 pdm Exp $"
  "Latest modification time and version number.")

(defvar cz-load-hook nil
  "Hook run when package has been loaded.")

(defvar cz-enable-keybindings t
  "Whether Czech functions should be assigned to global and/or local keys.")

(defvar cz-evils nil
  "A list of enemies of emacs-czech.
Members of this list can be the following symbols:
 `sun-keyboard' ... typical Sun keyboard which has mysterious key codes
 `bad-sendmail' ... sendmail which makes 7-bit mails from 8-bit MIME")

(if (member 'sun-keyboard cz-evils)
    (cz-message 5 "Sorry, Sun's keyboard is not supported yet."))


;;; *** Internal auxiliary functions ***

(defun cz-test-emacs-version (major minor)
  "True if Emacs version is at least MAJOR.MINOR."
  (or (> emacs-major-version major)
      (and (= emacs-major-version major) (>= emacs-minor-version minor))))

(defun cz-replace-by-car (list element)
  "Replaces element of the list identifying it by its head.
If element with the same head is not found, specified element is added."
  (if list
      (if (equal (car element) (car (car list)))
	  (cons element (cdr list))
	(cons (car list) (cz-replace-by-car (cdr list) element)))
    (setq list (list element))))

(defun cz-add-minor-mode (mode-variable mode-string)
  "Adds minor mode and its modeline string to minor modes alist.
MODE-STRING should start with space according to minor mode conventions."
  (if cz-xemacs
      (add-minor-mode mode-variable mode-string)
    (if (not (assoc mode-variable minor-mode-alist))
	(setq minor-mode-alist
	      (cons (list mode-variable mode-string) minor-mode-alist)))))


;;; *** 8-bit characters ***

(standard-display-european t)
;; It should be possible to use Czech support with external Czech keyboard
(set-input-mode (car (current-input-mode))
		(nth 1 (current-input-mode))
		0)

;; Windoze set uses also 128-159 codes
(if cz-windoze
    (standard-display-8bit 128 159))


;;; *** Syntax ***

(require 'case-table)   ; for XEmacs and Windoze but loaded in iso02-syn too
(cond
 ;; XEmacs
 (cz-xemacs
  ;; I don't understand XEmacs case tables (does it work?) - so it's hacked
  (require 'iso02-syn)
  (add-hook 'first-change-hook
	    (function (lambda ()
			(set-case-table
			 (list (car (standard-case-table)) nil nil nil))))))
 ;; GNU Emacs in Windoze
 (cz-windoze
  (let ((table (standard-case-table)))
    (set-case-syntax-pair ?¡ ?· table)
    (set-case-syntax-pair ?» ?Ë table)
    (set-case-syntax-pair ?œ ?Ô table)
    (set-case-syntax-pair ?… ?È table)
    (set-case-syntax-pair ?Ã ?Ï table)
    (set-case-syntax-pair ?Õ ?Ì table)
    (set-case-syntax-pair ?“ ?Ú table)
    (set-case-syntax-pair ?” ?Û table)
    (set-case-syntax-pair ?ÿ ?¯ table)
    (set-case-syntax-pair ?ä ?ö table)
    (set-case-syntax-pair ?ç ?ù table)
    (set-case-syntax-pair ?⁄ ?˙ table)
    (set-case-syntax-pair ?Ÿ ?˘ table)
    (set-case-syntax-pair ?› ?˝ table)
    (set-case-syntax-pair ?é ?û table)
    ;; Non Czech but ISO 8859-2 characters
    (set-case-syntax-pair ?• ?µ table)
    (set-case-syntax-pair ?£ ?≥ table)
    (set-case-syntax-pair ?º ?æ table)
    (set-case-syntax-pair ?å ?ú table)
    (set-case-syntax-pair ?™ ?∫ table)
    (set-case-syntax-pair ?è ?ü table)
    (set-case-syntax-pair ?Ø ?ø table)
    (set-case-syntax-pair ?¿ ?‡ table)
    (set-case-syntax-pair ?¬ ?‚ table)
    (set-case-syntax-pair ?√ ?„ table)
    (set-case-syntax-pair ?ƒ ?‰ table)
    (set-case-syntax-pair ?≈ ?Â table)
    (set-case-syntax-pair ?∆ ?Ê table)
    (set-case-syntax-pair ?« ?Á table)
    (set-case-syntax-pair ?  ?Í table)
    (set-case-syntax-pair ?À ?Î table)
    (set-case-syntax-pair ?Œ ?Ó table)
    (set-case-syntax-pair ?– ? table)
    (set-case-syntax-pair ?— ?Ò table)
    (set-case-syntax-pair ?‘ ?Ù table)
    (set-case-syntax-pair ?’ ?ı table)
    (set-case-syntax-pair ?÷ ?ˆ table)
    (set-case-syntax-pair ?€ ?˚ table)
    (set-case-syntax-pair ?‹ ?¸ table)
    (set-case-syntax-pair ?ﬁ ?˛ table)
    (set-case-syntax ?ﬂ "w" table)))
 ;; GNU Emacs in UN*X
 (t (require 'iso02-syn)))


;;; *** Fonts ***

(defvar cz-font-default nil
  "Defines the font used as default.
You have to call function `cz-set-fonts' or reload emacs-czech so that this
change would take effect.")

(defvar cz-font-faces-list nil
  "Defines the list of fonts used for various faces.
List is of form ( ( FACE-NAME \"FONT-DEFINITION\" ) ... ).
You must reload emacs-czech so that this change would take effect.")

(defvar cz-font-info-list nil
  "Defines the list of fonts used for info faces.
List is of form ( ( FACE-NAME \"FONT-DEFINITION\" ) ... ).
You have to call function `cz-set-fonts' or reload emacs-czech so that this
change would take effect.")

(autoload 'cz-set-fonts "czech-misc"
  "Set Czech fonts.
See documentation for variables `cz-font-default', `cz-font-faces-list',
and `cz-font-info-list'."
  t)


;;; *** Keyboard ***

;;; Variables involving using of Czech keyboard

(defvar cz-global-mode nil
  "If set non-`nil', Czech keyboard is global, else buffer local.
Default setting is `nil', i.e. buffer local.
You have to reload emacs-czech so that this change would take effect.")

(defvar cz-keyboard-default-on nil
  "If set non-`nil', Czech keyboard mode is switched on globally.
You have to reload emacs-czech so that this change would take effect.")

(defvar cz-minibuffer-nonczech nil
  "If non-`nil', minibuffer does not inherit cz-keyboard from calling buffer.
If `nil', whenever you enter minibuffer, it is set to cz-keyboard-mode just if
calling buffer is in cz-keyboard-mode.  If variable `cz-keyboard-default-on'
or `cz-global-mode' is non-`nil', the value of `cz-minibuffer-nonczech' doesn't
matter.
You have to reload emacs-czech so that this change would take effect.")

(defvar cz-keyboard-nonstandard t
  "If set non-`nil', nonstandard Czech keyboard is used.
If set just `t', default nonstandard keyboard is used.  If you want to use
your own keyboard, set this variable to any value other than `nil' or `t'.
Standard Czech keyboard is probably defined by some official standard.
As much I know it I defined its bindings here.
Nonstandard Czech keyboard is defined by the following variables:
cz-dead-key-1, cz-dead-key-2, cz-keys, cz-keys-dead-1, and cz-keys-dead-2.
You have to reload emacs-czech so that this change would take effect.")

(defvar cz-redefine-alt-digits nil
  "If true, standard numeric argument `M-<digit>' will be redefined.
Instead of numeric argument function this key sequences will produce digits on
Czech keyboard.
You have to reload emacs-czech so that this change would take effect.")

;; Variables defining Czech keyboard (they define non-standard Czech keyboard)

(defvar cz-dead-key-1 ?+
  "Defines the first dead key for Czech keyboard.
It must be character.
It is bind to `?+' default.
You have to reload emacs-czech so that this change would take effect.")

(defvar cz-dead-key-2 ?=
  "Defines the second dead key for Czech keyboard.
It must be character.
It is bind to `?=' key default.
You have to reload emacs-czech so that this change would take effect.")

(defvar cz-keys
  (if cz-windoze
      '((?2 . [236]) (?3 . [154]) (?4 . [232]) (?5 . [248]) (?6 . [158])
	(?7 . [253]) (?8 . [225]) (?9 . [237]) (?0 . [233]) (?\[ . [250])
	(?\] . [249]))
    '((?2 . [236]) (?3 . [185]) (?4 . [232]) (?5 . [248]) (?6 . [190])
      (?7 . [253]) (?8 . [225]) (?9 . [237]) (?0 . [233]) (?\[ . [250])
      (?\] . [249])))
  "Defines keys with Czech characters.
Binding of these keys will be changed in Czech keyboard mode.
Together with any dead key they return original characters bound to them.
It is an association list ( ( KEY . DEFINITION ) ... ( KEY . DEFINITION ) ),
where KEY must be character and definition string or vector.
You have to reload emacs-czech so that this change would take effect.")

(defvar cz-keys-dead-1
  (if cz-windoze
      '((?c . [232]) (?d . [239]) (?e . [236]) (?n . [242]) (?r . [248])
	(?s . [154]) (?t . [157]) (?u . [249]) (?z . [236]) (?C . [200])
	(?D . [207]) (?E . [204]) (?N . [210]) (?R . [216]) (?S . [138])
	(?T . [141]) (?U . [217]) (?Z . [216]))
    '((?c . [232]) (?d . [239]) (?e . [236]) (?n . [242]) (?r . [248])
      (?s . [185]) (?t . [187]) (?u . [249]) (?z . [190]) (?C . [200])
      (?D . [207]) (?E . [204]) (?N . [210]) (?R . [216]) (?S . [169])
      (?T . [171]) (?U . [217]) (?Z . [174])))
  "Key bindings to the first Czech dead key.
It is an association list ( ( KEY . DEFINITION ) ... ( KEY . DEFINITION ) ),
where KEY must be character and definition string or vector.
You have to reload emacs-czech so that this change would take effect.")

(defvar cz-keys-dead-2			; the same in ISO and Windoze
  '((?a . [225]) (?e . [233]) (?i . [237]) (?o . [243]) (?u . [250])
    (?y . [253]) (?A . [193]) (?E . [201]) (?I . [205]) (?O . [211])
    (?U . [218]) (?Y . [221]))
  "Key bindings to the second Czech dead key.
It is an association list ( ( KEY . DEFINITION ) ... ( KEY . DEFINITION ) ),
where KEY must be character and definition string or vector.
You have to reload emacs-czech so that this change would take effect.")

;; Standard Czech keyboard
(defvar cz-standard-keyboard
  (if cz-windoze
      '(
	((?1 . "+") (?2 . [236]) (?3 . [154]) (?4 . [232]) (?5 . [248])
	 (?6 . [158]) (?7 . [253]) (?8 . [225]) (?9 . [237]) (?0 . [233])
	 (?` . "\"") (?~ . ";") (?! . "1") (?@ . "2") (?# . "3")
	 (?$ . "4") (?% . "5") (?^ . "6") (?& . "7") (?* . "8")
	 (?\( . "9") (?\) . "0") (?- . "=") (?_ . "%") (?\[ . [250])
	 (?{ . "/") (?\] . ")") (?} . "(") (?\; . [249]) (?: . "\"")
	 (?' . [167]) (?\" . "!") (?< . "?") (?> . ":") (?/ . "-")
	 (?\? . "_") (?y . "z") (?Y . "Z") (?z . "y") (?Z . "Y"))
	(?+ (?c . [232]) (?d . [239]) (?e . [236]) (?n . [242]) (?r . [248])
	    (?s . [154]) (?t . [157]) (?u . [249]) (?y . [190]) (?C . [200])
	    (?D . [207]) (?E . [204]) (?N . [210]) (?R . [216]) (?S . [138])
	    (?T . [141]) (?U . [217]) (?Y . [174])
	    (?z . "z") (?Z . "Z"))
	(?= (?a . [225]) (?e . [233]) (?i . [237]) (?o . [243]) (?u . [250])
	    (?z . [253]) (?A . [193]) (?E . [201]) (?I . [205]) (?O . [211])
	    (?U . [218]) (?Z . [221])
	    (?y . "y") (?Y . "Y")))
    '(
      ((?1 . "+") (?2 . [236]) (?3 . [185]) (?4 . [232]) (?5 . [248])
       (?6 . [190]) (?7 . [253]) (?8 . [225]) (?9 . [237]) (?0 . [233])
       (?` . "\"") (?~ . ";") (?! . "1") (?@ . "2") (?# . "3")
       (?$ . "4") (?% . "5") (?^ . "6") (?& . "7") (?* . "8")
       (?\( . "9") (?\) . "0") (?- . "=") (?_ . "%") (?\[ . [250])
       (?{ . "/") (?\] . ")") (?} . "(") (?\; . [249]) (?: . "\"")
       (?' . [167]) (?\" . "!") (?< . "?") (?> . ":") (?/ . "-")
       (?\? . "_") (?y . "z") (?Y . "Z") (?z . "y") (?Z . "Y"))
      (?+ (?c . [232]) (?d . [239]) (?e . [236]) (?n . [242]) (?r . [248])
	  (?s . [185]) (?t . [187]) (?u . [249]) (?y . [190]) (?C . [200])
	  (?D . [207]) (?E . [204]) (?N . [210]) (?R . [216]) (?S . [169])
	  (?T . [171]) (?U . [217]) (?Y . [174])
	  (?z . "z") (?Z . "Z"))
      (?= (?a . [225]) (?e . [233]) (?i . [237]) (?o . [243]) (?u . [250])
	  (?z . [253]) (?A . [193]) (?E . [201]) (?I . [205]) (?O . [211])
	  (?U . [218]) (?Z . [221])
	  (?y . "y") (?Y . "Y"))))
  "Definition list of standard Czech keyboard for `cz-set-keyboard'.")

(defvar cz-dead-symbols
  (if cz-windoze
      '((?= . ?\264) (?+ . ?\241))
    '((?= . ?\264) (?+ . ?\267)))
  "*Characters used for displaying dead keys accents.
It is an association list in which each item is
(DEAD-KEY . DISPLAY-CHARACTER).
Both DEAD-KEY and DISPLAY-CHARACTER must be characters.")

;; What about keypad keys?
(defvar cz-redefine-keypad t
  "If non-`nil', the package is allowed to redefine keypad keys.
If you do not allow to do this, you may have problems to write numbers with
Czech keyboard on the numeric block.
You have to reload emacs-czech so that this change would take effect.")


;;; *** ASCII display mode ***

(autoload 'cz-ascii-display-mode "czech-misc"
  "Minor mode for displaying Czech texts in 7-bit ASCII."
  t)


;;; *** Word and region correction ***

(autoload 'cz-correct-word-on "czech-misc"
  "Translates last word(s) typed on non-Czech keyboard.
It translates last COUNT words typed on non-Czech keyboard as they would be
typed on Czech keyboard.  COUNT must be positive.
It does not translate all characters, but most of those defined in the list
`cz-keys'."
  t)
(autoload 'cz-correct-word-off "czech-misc"
  "Translates last word(s) typed on Czech keyboard.
It translates last COUNT words typed on Czech keyboard as they would be
typed on non-Czech keyboard.  COUNT must be positive.
It does not translate all characters, but most of those defined in the list
`cz-keys'."
  t)
(autoload 'cz-correct-word "czech-misc"
  "Translates last word(s) typed on other keyboard.
It translates last COUNT words typed on other keyboard as they would be
typed on current keyboard.  COUNT must be positive."
  t)
(autoload 'cz-correct-word-switch "czech-misc"
  "Translates last word(s) typed on other keyboard.
It is like `cz-correct-word', but also switches keyboard.
It translates last COUNT words typed on other keyboard as they would be
typed on current keyboard.  COUNT must be positive."
  t)
(autoload 'cz-correct-switch "czech-misc"
  "Translates part of text typed on other keyboard.
It switches to the other keyboard mode and translates text from the current
position to the beginning of line and COUNT lines before.
COUNT must be non-negative.
If there is no non-space character at the current line and no prefix argument
was given, then current line is not included into COUNT."
  t)
(autoload 'cz-correct-region "czech-misc"
  "Translates region typed on other keyboard.
An optional argument TYPE says, what translation should be performed:
`czech' means to Czech, `us' means from Czech."
  t)


;;; *** Character selection ***

(autoload 'cz-select-char "czech-misc"
  "Popups buffer for selecting any ISO-8859-2 accent character.
Character can be selected by pressing `RET', `C-c C-c', or middle mouse button
on it.
If an optional prefix argument is given, last selected character is inserted."
  t)


;;; *** Testing for Czech text ***

(defvar cz-is-empty-czech nil
  "*Whether the empty buffer is Czech text.")

(defvar cz-is-buffer-empty 80
  "*How many bytes can have buffer to be considered empty.")

(defvar cz-is-test-size 1000
  "*Maximal count of tested bytes.")

(defvar cz-is-skip-start 0
  "*How many bytes to ignore at the beginning of the buffer.")

(defvar cz-is-czech-min 0.01
  "*The minimal ratio of \"Czech\" characters in the tested part of the text.
The text is considered to be Czech only if this ratio is reached at least.
\"Czech\" characters are those from above half of the character set contained
in the list `cz-is-czech-characters'.")

(defvar cz-is-nonczech-max 0.01
  "*The maximal ratio of \"non-Czech\" characters in the tested part of the text.
The text is considered to be Czech only if this ratio is not exceeded.
\"nonCzech\" characters are those from above half of the character set not
contained in the list `cz-is-czech-characters'.")

(defvar cz-is-czech-characters
  (if cz-windoze
      '(?· ?Ë ?Ô ?È ?Ï ?Ì ?Ú ?Û ?¯ ?ö ?ù ?˙ ?˘ ?˝ ?û
	?¡ ?» ?œ ?… ?Ã ?Õ ?“ ?” ?ÿ ?ä ?ç ?⁄ ?Ÿ ?› ?é)
    '(?· ?Ë ?Ô ?È ?Ï ?Ì ?Ú ?Û ?¯ ?π ?ª ?˙ ?˘ ?˝ ?æ
      ?¡ ?» ?œ ?… ?Ã ?Õ ?“ ?” ?ÿ ?© ?´ ?⁄ ?Ÿ ?› ?Æ))
  "List of characters to be considered beeing \"Czech\".")

(autoload 'cz-is-cz-text "czech-misc"
  "Tries to recognize, whether current buffer contains ISO-8859-2 text.
If the buffer is narrowed, only the narrowed part is consedered.
If optional argument RETURN-RATIO is non-`nil', ratio (floating point number)
is returned instead of boolean value.  Ratio belongs to interval [-1;1] and
equals to zero just when number of Czech and non-Czech characters from the
above half of character set is the same.
If TEST-ALL is non-`nil', test region without considering variables
`cz-is-buffer-empty' and `cz-is-skip-start' (but `cz-is-test-size' is always
considered).
The recognition is absolutely magical and nothing should hard depend on it.
See also variable `cz-is-czech-characters'.")


;;; *** Conversions ***

(defvar cz-use-cstocs-limit 'infinity
  "*The minimal size of the region for using cstocs conversion program.
If the size of the region is greater or equal than the value of this variable,
it will be used an external program for conversion (see variable
`cz-use-cstocs-program').  Otherwise internal recoding functions will be
used.
If the special value `infinity' is set, the external program will never be
used.")

(defvar cz-use-cstocs-program "cstocs"
  "*Program invocation string for external conversion.
It can be set to other value for example if the cstocs program is not in the
default path or if some extra option (like library path) should be given to
that program.
See also variable `cz-use-cstocs-limit'.")

(defvar cz-encoding-files-dir "/usr/local/lib/cstocs"
  "*Directory with definition files of encodings.
These definition files usually have the extension `.enc' and their format is
specified in `cstools' package.")

(defvar cz-convert-accent-file "accent"
  "*Name of the definition file of \"outaccented\" characters.")

(defvar cz-convert-onebymore nil
  "*Whether to convert also the characters with more than one byte equivalent.
This variable involves behaviour of the function `cz-convert-from-to'.
If `nil', characters which should be replaced by more than one byte long string
are replaced by `cz-convert-unknown-char'.
If non-`nil', such characters are replaced by appropriate string.
Opposite direction does not work: more than one-character strings are not
replaced by their single character equivalent, they are replaced like any other
characters.
Warning: Setting this variable to non-`nil' can slow the conversion down.
See also variable `cz-convert-unknown-char'.")

(defvar cz-convert-unknown-char ?.
  "The character used as replacing for characters without one byte equivalent.
If `nil', such characters remain untouched.
See variable `cz-convert-onebymore' for further explanation.
Warning: If you change this variable you possibly should call
`M-x cz-convert-delete-tables' so that created tables would be rebuilt.")

(defvar cz-convert-cache-file nil
  "*The name of the file used for storage of conversion tables.
When the conversion by `cz-convert-from-to' function is asked, an appropriate
conversion table must be created.  This make take some time.  If you want to
eliminate most of this overhead, you can set this variable to some filename.
Emacs will store newly created tables to this file for further usage.
If the value of `cz-convert-cache-file' is `nil', no cache file will be used
and considered.")

(defvar cz-convert-encfiles
  '(("1250" 1) ("1252" 2) ("ascii" 3) ("cork" 4) ("dos" . 5) ("il1" 6)
    ("il2" 7) ("kam" 8) ("koi8" 9) ("mac" 10) ("macce" 11) ("pc2" 12)
    ("t1" 13) ("vga" 14) ("win" 15))
  "*Association list of default *.enc file names.")

(defvar cz-convert-symbol-strings
  '((ascii . "ascii") (il2 . "il2") (kam . "kam") (koi8 . "koi")
    (win . "1250") (dos . "pc2"))
  "*Association list of file names for basic encodings.")

(defvar cz-convert-aux-buffer "*cz*"
  "The name of the buffer used for temporary storage of converted data.")

(defvar cz-convert-verbose-limit 10000
  "*Minimal size of the converted region for verbose conversion.")

(defvar cz-convert-auto-flag nil	; we need this for menu
  "Non-`nil' just when autoconversion is enabled.")

(autoload 'cz-convert-auto-encoding "czech-convert"
  "Sets encoding of the buffer for automatic encoding conversion.
The following encodings are supported: ISO-8859-2, brothers' KamewiËtÌ,
KOI-8, M$-Windoze (\"CP 1250\"), and DO$ (\"CP 852\")."
  t)
(autoload 'cz-convert-auto-toggle "czech-convert"
  "Toggles automatic conversion (globaly).
If an optional argument ARG is positive, set automatic conversion on.
If an optional argument ARG is negative or zero, set automatic conversion
off."
  t)
(autoload 'cz-convert-auto-enable "czech-convert"
  "Enables automatic conversion (globaly)."
  t)
(autoload 'cz-convert-auto-disable "czech-convert"
  "Disables automatic conversion (globaly)."
  t)
(autoload 'cz-convert-auto-set "czech-convert"
    "Noninteractive variation of `cz-convert-auto-encoding'.
If ENCODING is `nil', autoconversion is disabled for the buffer.")
(autoload 'cz-convert-to-csech "czech-convert"
  "Converts region from ISO 8859-2 to 7-bit ASCII (so called \"csech\").
If APPEND-MESSAGE is non-`nil' or prefix argument is given, it appends message
about conversion to end of the converted region.  It converts all 8-bit
characters, but those that are not diacritic will be converted to dots."
  t)
(autoload 'cz-convert-to-iso "czech-convert" nil t)
(autoload 'cz-convert-to-native "czech-convert"
  "Tries to identify encoding of the region and convert it to native encoding.
Known input encoding are: brothers' KameniËtÌ (`kam'), KOI-8 (`koi8'),
CP 1250 (`win'), CP 852 (`dos').
Output encoding is ISO 8859-2; in Windoze it is one of the Micro$oft's
encodings (\"CP 1250\").
If prefix argument is used, an user is asked for input encoding."
  t)
(autoload 'cz-convert-from-to "czech-convert"
  "Converts given region between two encodings.
These encodings must have their definitions files.  These encoding files
usually have the extension `.enc' and their format is specified in `cstools'
package.
Specified input and output encodings must be strings equal to name of
appropriate encoding files without extension.
If called with prefix argument, use the last given encodings.
See also variables `cz-encoding-files-dir', `cz-convert-onebymore', and
`cz-convert-cache-file'."
  t)
(autoload 'cz-convert-undo$ "czech-convert"
  "Removes those silly chars (^M and ^Z) from end of lines in buffer."
  t)
(autoload 'cz-convert-do$ "czech-convert"
  "Adds that silly char (^M) to end of lines of the current buffer."
  t)


;;; *** Sorting ***

(defvar cz-sort nil
  "Whether Czech sorting should be used.
Possible values are the following:
`nil'     - no Czech sorting feature (default)
`t'       - always do Czech sorting
`by-mode' - use sorting according to current value of `cz-keyboard-mode'
Please do not forget that used \"Czech sorting\" is *not* real Czech sorting,
it is only its simpliest variation.")

(defun cz-sort-enable ()
  "Replace standard main sorting function by Czech function.
New function is almost equivalent to the old but considers the value of
`cz-sort' and is able to do some very simple kind of \"Czech sorting\".
See documentation of `cz-sort' for more details."
  (require 'czech-sort))

(autoload 'cz-sort-toggle "czech-sort"
  "Changes value of the variable `cz-sort'.
Change is done by rotating values `nil', `t', and `by-mode'.
If an optional prefix argument is given, rotation is done in opposite
direction."
  t)


;;; *** Mail and news reading ***

(defvar cz-gnus 'no-mime
  "*Whether emacs-czech should attack Gnus.
Possible values are the following:
`nil'     - no connection to Gnus
`no-mime' - enable Czech hooks for messages but do not use MIME (default)
`t'       - enable Czech hooks for messages and use `tm' MIME package")

(defvar cz-message-method-default 'ascii
  "*Default method used for mail/news conversion.
Its values can be the following symbols:
  `ascii' ... convert to 7-bit ASCII (default)
  `mime'  ... convert to MIME
  `plain' ... no conversion
  `nil'   ... the same as `plain'
If the value is `mime' and the value of `cz-gnus' is `no-mime', then this
option acts in the same way as `ascii'.
It can be overriden by variables `cz-message-method-mail' and
`cz-message-method-news'.")

(defvar cz-message-method-mail 'mime
  "*Default method used for mail conversion.
Its values can be the following symbols:
  `ascii' ... convert to 7-bit ASCII
  `mime'  ... convert to MIME (default)
  `plain' ... no conversion
  `nil'   ... use the value of variable `cz-message-method-default'
If the value is `mime' and the value of `cz-gnus' is `no-mime', then this
option acts in the same way as `ascii'.")

(defvar cz-message-method-news nil
  "*Default method used for news conversion.
Its values can be the following symbols:
  `ascii' ... convert to 7-bit ASCII
  `mime'  ... convert to MIME
  `plain' ... no conversion
  `nil'   ... use the value of variable `cz-message-method-default' (default)
If the value is `mime' and the value of `cz-gnus' is `no-mime', then this
option acts in the same way as `ascii'.")

(defvar cz-message-method-people '((mime "pdm@fi.muni.cz"))
  "*List of conversion methods for mail recipients.
This is a list of lists.
Each of sublists begins with one of the following symbols:
 `ascii' mails for listed people are converted to 7-bit ASCII
 `mime'  mails for listed people are converted to 7-bit MIME
 `plain' mails for listed people are not converted or are converted (if
         possible) to 8-bit MIME
Follow regular expressions of respective e-mail addresses.
See also variable `cz-message-method-mail'.")

(defvar cz-message-method-newsgroups nil
  "*List of conversion methods for newsgroups.
This is a list of lists.
Each of sublists begins with one of the following symbols:
 `ascii' listed articles are converted to 7-bit ASCII
 `mime'  listed articles are converted to MIME
 `plain' listed articles are not converted or are converted (if
         possible) to 8-bit MIME
Follow regular expressions of respective newsgroups.
See also variable `cz-message-method-news'.")

(defvar cz-message-use-bbdb nil
  "*If non-`nil', use BBDB for selecting message conversion method.
You can use Insidious Big Brother Database for selecting message conversion
method by specifying field `mailtype'.")

(defvar cz-message-ask-on-send (>= cz-verbose-level 4)
  "*Should I ask for confirmation after performing conversion on message?")

(autoload 'cz-message-set-method "czech-message"
  "Sets final encoding of the current message."
  t)
(autoload 'cz-message-show-method "czech-message"
  "Prints final encoding of the current message.

If an optional argument QUIETLY is non-`nil', no message is printed, only
symbol value is returned."
  t)
(autoload 'cz-message-send "czech-message"
  "Run `cz-message-prepare' and then `message-send'."
  t)
(autoload 'cz-message-send-and-exit "czech-message"
  "Run `cz-message-prepare' and then `message-send-and-exit'."
  t)
(autoload 'cz-message-setup-mime "czech-message")


;;; *** Saving settings ***

(autoload 'cz-save-settings "czech-misc"
  "Saves values of some user variables to `~/.emacs'.
This is saved there before last found `(load \"czech\")' or (if no load found)
before first `(require 'czech)'.  If no of the two variants is found, options
are saved to the end of file and `(load \"czech\")' is inserted.
This function is not much clever and may produce incorrect results in unusual
situations."
  t)


;;; *** Set keybindings ***

;; Global map
(if cz-enable-keybindings
    (progn
      ;; Console dependent part
      (if (string= (getenv "TERM") "linux")
	  (progn
	    (global-set-key "\e[P" 'cz-keyboard-mode)
	    (global-set-key "\e\e[P" 'cz-correct-switch))
	;; XEmacs dependent part
	(if cz-xemacs
	    (progn
	      (global-set-key '(meta pause) 'cz-correct-switch)
	      (global-set-key '(meta break) 'cz-correct-switch)
	      (global-set-key '(control pause) 'cz-select-char)
	      (global-set-key '(control break) 'cz-select-char))
	  (global-set-key [?\e pause] 'cz-correct-switch)
	  (global-set-key [?\e break] 'cz-correct-switch)
	  (global-set-key [M-pause] 'cz-correct-switch)
	  (global-set-key [M-break] 'cz-correct-switch)
	  (global-set-key [C-pause] 'cz-select-char)
	  (global-set-key [C-break] 'cz-select-char)))
      ;; Common part
      (global-set-key [pause] 'cz-keyboard-mode)
      (global-set-key [break] 'cz-keyboard-mode)
      (global-set-key "\C-xz0" 'cz-keyboard-mode-off)
      (global-set-key "\C-xz1" 'cz-keyboard-mode-on)
      (global-set-key "\C-xz7" 'cz-convert-to-csech)
      (global-set-key "\C-xza" 'cz-convert-auto-toggle)
      (global-set-key "\C-xzb" 'cz-convert-auto-encoding)
      (global-set-key "\C-xzc" 'cz-select-char)
      (global-set-key "\C-xzd" 'cz-convert-do$)
      (global-set-key "\C-xzf" 'cz-convert-from-to)
      (global-set-key "\C-xzm" 'cz-menu)
      (global-set-key "\C-xzn" 'cz-convert-to-native)
      (global-set-key "\C-xzr" 'cz-correct-region)
      (global-set-key "\C-xzs" 'cz-sort-toggle)
      (global-set-key "\C-xzt" 'cz-ascii-display-mode)
      (global-set-key "\C-xzu" 'cz-convert-undo$)
      (global-set-key "\C-xzw" 'cz-correct-word-switch)
      (global-set-key "\C-xzz" 'cz-keyboard-mode)))

;; Local maps
(defun cz-message-define-bindings ()
  (define-key message-mode-map "\C-c\C-z" 'cz-message-send-and-exit)
  (define-key message-mode-map "\C-c\C-m" 'cz-message-set-method)
  (define-key message-mode-map "\C-c\C-s" 'cz-message-show-method))
(if cz-enable-keybindings
    (if (featurep 'message)
	(cz-message-define-bindings)
      (add-hook 'message-load-hook 'cz-message-define-bindings)))


;;; *** Menus ***

(defvar cz-enable-menu t
  "If non-`nil', define Czech menus.
You have to reload emacs-czech so that this change would take effect.")

;; We do this by defining very simple minor mode
;; (This is the simpliest way for me to define menu at right place on menubar)
(defvar cz-menu-mode nil
  "Control variable for `cz-menu-mode' (Emacs Czech menus).")
(defun cz-menu (&optional arg)
  "Simple minor mode for displaying Emacs Czech menus."
  (interactive)
  (if (or (null arg)
	  (not (equal cz-menu-mode (> (prefix-numeric-value arg) 0))))
      ;; Mode must be changed
      (progn
	(setq cz-menu-mode (not cz-menu-mode))
	(force-mode-line-update)
	(if cz-xemacs (set-menubar-dirty-flag)))))

(defun cz-menu-on ()
  "Switches Czech menu on."
  (interactive)
  (cz-menu 1))
(defun cz-menu-off ()
  "Switches Czech menu off."
  (interactive)
  (cz-menu -1))

(defvar cz-menu-map (make-sparse-keymap))
(if (not (assoc 'cz-menu-mode minor-mode-map-alist))
    (setq minor-mode-map-alist
	  (cons (cons 'cz-menu-mode cz-menu-map) minor-mode-map-alist)))

(require 'easymenu)
(let ((menu
       (list
	"Czech" ':included 'cz-menu-mode
	["Czech keyboard" cz-keyboard-mode :style toggle
	 :selected cz-keyboard-mode]
	["Correct typed text" cz-correct-region t]
	["Select character" cz-select-char t]
	(list
	 "Sorting"
	 ["Always standard sorting" (setq cz-sort nil) :style radio
	  :selected (not cz-sort)]
	 ["Always Czech sorting" (progn (setq cz-sort t) (cz-sort-enable))
	  :style radio :selected (eq cz-sort t)]
	 ["Sorting by CZ keyboard mode"
	  (progn (setq cz-sort 'by-mode) (cz-sort-enable)) :style radio
	  :selected (eq cz-sort 'by-mode)])
	(append
	 (if cz-xemacs
	      (list "Message" :included (eq major-mode 'message-mode))
	   (list "Message"))
	 (list
	  (list
	   "Set method"
	   ["MIME" (setq cz-message-this-method 'mime) :style radio
	    :selected (cz-safe-eq 'cz-message-this-method 'mime)]
	   ["plain" (setq cz-message-this-method 'plain) :style radio
	    :selected (cz-safe-eq 'cz-message-this-method 'plain)]
	   ["ASCII" (setq cz-message-this-method 'ascii) :style radio
	    :selected (cz-safe-eq 'cz-message-this-method 'ascii)]
	   "----"
	   ["default" (setq cz-message-this-method 'default) :style radio
	    :selected (or (cz-safe-eq 'cz-message-this-method 'default)
			  (cz-safe-eq 'cz-message-this-method nil))]))
	 (list
	  ["Show method" cz-message-show-method t]
	  ["Send and exit..." cz-message-send-and-exit t]))
	"----"
	(if cz-xemacs
	    ["Autorecoding" cz-convert-auto-toggle :style toggle
	     :selected cz-convert-auto-flag]
	  ["autorecoding" cz-convert-auto-toggle :style toggle
	   :selected cz-convert-auto-flag])
	(list
	 "Set document encoding"
	 ["ISO 8859-2" (cz-convert-auto-set "il2") t]
	 ["KameniËtÌ" (cz-convert-auto-set "kam") t]
	 ["KOI8" (cz-convert-auto-set "koi8") t]
	 ["Windoze" (cz-convert-auto-set "win") t]
	 ["DO$" (cz-convert-auto-set "dos") t]
	 "----"
	 ["none" (cz-convert-auto-set nil) t])
	(if cz-windoze
	    ["Convert to Windoze" cz-convert-to-native t]
	  ["Convert to ISO 8859-2" cz-convert-to-native t])
	["Convert to ISO 8859-2" cz-convert-to-native t]
	["Convert to ASCII" cz-convert-to-csech t]
	["Convert from to..." cz-convert-from-to t]
	["ASCII display" cz-ascii-display-mode :style toggle
	 :selected (cz-safe-eq 'cz-ascii-display-mode t)]
	"----"
	["Save settings" cz-save-settings t]
	["Dismiss this menu" cz-menu-off t])))
  (if cz-xemacs
      ;; Define menu in XEmacs
      (if (not (member menu current-menubar))
	  (add-submenu nil menu nil))
    ;; Define menu in Emacs
    (easy-menu-define
     cz-menu: cz-menu-map "Emacs Czech menu."
     menu)
    ;; In Emacs we want to have menu as last as possible in menubar
    (if (not (memq 'cz-menu menu-bar-final-items))
	(let ((list menu-bar-final-items)
	      beg-list)
	  (while (and list (not (eq (car list) 'help-menu)))
	    (setq beg-list (cons (car list) beg-list))
	    (setq list (cdr list)))
	  (setq menu-bar-final-items (append beg-list '(cz-menu) list))))))

;; Switch on/of the menu
(if cz-enable-menu
    (cz-menu-on)
  (cz-menu-off))


;;; *** Patches to Emacs ***

;; The following function in Emacs 19.34 (or older) is buggy
;; Here is the replacement which I received from RMS.

(if (and (not cz-xemacs)
	 (not (cz-test-emacs-version 19 35)))
    (defun isearch-no-upper-case-p (string regexp-flag)
      "Return t if there are no upper case chars in STRING.
If REGEXP-FLAG is non-nil, disregard letters preceded by `\\' (but not `\\\\')
since they have special meaning in a regexp."
      (let (quote-flag (i 0) (len (length string)) found) 
	(while (and (not found) (< i len))
	  (let ((char (aref string i)))
	    (if (and regexp-flag (eq char ?\\))
		(setq quote-flag (not quote-flag))
	      (if (and (not quote-flag) (not (eq char (downcase char))))
		  (setq found t))))
	  (setq i (1+ i)))
	(not found))))


;;; *** Compatibility with previous versions ***

(defun cz-default-bindings ()
  "Compatibility function, now obsolete.
Key bindings are set automatically now."
  (cz-message 4 "`cz-default-bindings' is obsolete. Remove it."))


;;; *** Announce ***

(provide 'czech)


;;; *** Run user hooks ***

(run-hooks 'cz-load-hook)


;;; *** Setup some things ***

;; Fonts
(if (or cz-font-default cz-font-faces-list cz-font-info-list)
    (cz-set-fonts cz-font-default cz-font-faces-list cz-font-info-list))

;; Keyboard
(require 'czech-keyboard)		; we redefine keyboard always
(cz-set-keyboard nil (not cz-redefine-keypad))
(if (not cz-xemacs)
    (define-key isearch-mode-map [pause] 'cz-handle-key-isearch))

;; Sorting
(if cz-sort
    (cz-sort-enable))

;; Gnus
(if (eq cz-gnus t)
    (cz-message-setup-mime))

;;; czech.el ends here

