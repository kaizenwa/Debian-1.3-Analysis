;;; $Id: emacspeak-setup.el,v 5.0 1996/11/22 18:04:26 raman Exp $
;;; $Author: raman $ 
;;; Description:  File for setting up and starting Emacspeak
;;; Keywords: Emacspeak, Setup, Spoken Output
;;{{{  LCD Archive entry: ;;; LCD Archive Entry:;;; emacspeak| T. V. Raman |raman@crl.dec.com ;;; A speech interface to Emacs |;;; $date: $ |;;;  $Revision: 5.0 $ | ;;; Location undetermined;;;;;}}}
;;{{{  Copyright:;;; Copyright (c) 1994, 1995 by Digital Equipment Corporation.;;; All Rights Reserved. ;;;;;; This file is not part of GNU Emacs, but the same permissions apply.;;;;;; GNU Emacs is free software; you can redistribute it and/or modify;;; it under the terms of the GNU General Public License as published by;;; the Free Software Foundation; either version 2, or (at your option);;; any later version.;;;;;; GNU Emacs is distributed in the hope that it will be useful,;;; but WITHOUT ANY WARRANTY; without even the implied warranty of;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the;;; GNU General Public License for more details.;;;;;; You should have received a copy of the GNU General Public License;;; along with GNU Emacs; see the file COPYING.  If not, write to;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.;;}}}

(require 'cl)

(defvar emacspeak-dir
  (expand-file-name "~/emacs/lisp/emacspeak")
  "Directory where emacspeak is installed. ")
(unless (featurep 'emacspeak)
(setq load-path
      (cons emacspeak-dir 
                              load-path )))
(load-library "emacspeak")
(setq dtk-config
      (let ((*dtk-stop-immediately* nil ))
      (function (lambda ()
                  (dtk-set-punctuations "some")
(dtk-split-caps t)
                  (dtk-set-rate 425 )
                  (dtk-set-character-scale 1.3)
                  )))
)
(add-hook 'dtk-startup-hook dtk-config )


(setq emacspeak-config
      (function (lambda ()
                  (emacspeak-toggle-audio-indentation nil)
                  (emacspeak-localize-settings)
                  (setq *dtk-stop-immediately* t )
                  )))
(add-hook 'emacspeak-startup-hook emacspeak-config)
(setq *dtk-program*
      (or (getenv "DTK_PROGRAM")
          "dtk-exp"))
(emacspeak)
(emacspeak-fix-commands-that-use-interactive)
                  ;;; turn on automatic voice locking for modes
 (mapcar
  (function (lambda (hook)
              (add-hook hook
                        (function (lambda ()
                                    (voice-lock-mode 1 ))))))
  (list 'c-mode-hook
 'c++-mode-hook
 'lisp-mode-hook
 'emacs-lisp-mode-hook
 'perl-mode-hook
 'TeX-mode-hook
 'tex-mode-hook
 'text-mode-hook
 'tcl-mode-hook
 'dired-mode-hook))
