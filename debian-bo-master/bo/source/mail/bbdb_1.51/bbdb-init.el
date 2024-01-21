;-*-emacs-lisp-*-

;; bbdb-init.el, perform mailcrypt initialization
;; Copyright (C) 1996 Joe Reinhardt <joe-reinhardt@uiowa.edu>

;;{{{ Licensing
;; This file is intended to be used with GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;}}}

(provide 'bbdb-init)

;; load path
(setq load-path (cons "/usr/lib/emacs/site-lisp/bbdb" load-path))

(autoload 'bbdb         "bbdb-com" "Insidious Big Brother Database" t)
(autoload 'bbdb-name    "bbdb-com" "Insidious Big Brother Database" t)
(autoload 'bbdb-company "bbdb-com" "Insidious Big Brother Database" t)
(autoload 'bbdb-net     "bbdb-com" "Insidious Big Brother Database" t)
(autoload 'bbdb-notes   "bbdb-com" "Insidious Big Brother Database" t)
(autoload 'bbdb-insinuate-vm       "bbdb-vm"    "Hook BBDB into VM")
(autoload 'bbdb-insinuate-rmail    "bbdb-rmail" "Hook BBDB into RMAIL")
(autoload 'bbdb-insinuate-mh       "bbdb-mhe"   "Hook BBDB into MH-E")
(autoload 'bbdb-insinuate-gnus     "bbdb-gnus"  "Hook BBDB into GNUS")
(autoload 'bbdb-insinuate-sendmail "bbdb"       "Hook BBDB into sendmail")

;; for bbdb-print 

(setq bbdb-print-format-files 
      '("/usr/lib/bbdb/bbdb-print" "/usr/lib/bbdb/multicol"))

;; for message mode

(defun bbdb-insinuate-message ()
  "Call this function to hook BBDB into message"
  (define-key message-mode-map "\M-\t" 'bbdb-complete-name))

(add-hook 'message-mode-hook 'bbdb-insinuate-message)

;; per the info pages (rmail and mh-e only) see the info page for vm.
;; Get gnus-bbdb if you want to use gnus.

(add-hook 'rmail-mode-hook 'bbdb-insinuate-rmail)
(add-hook 'mh-folder-mode-hook 'bbdb-insinuate-mh)

;; for sending

(add-hook 'mail-setup-hook 'bbdb-insinuate-sendmail)

;;;end
