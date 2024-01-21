;;; tm-gnus.el --- MIME extension for GNUS

;; Copyright (C) 1993,1994,1995,1996 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; modified by Shuhei KOBAYASHI <shuhei@cmpt01.phys.tohoku.ac.jp>
;; Created: 1993/11/20 (obsolete mol's gnus-mime.el)
;; Version: $Id: tm-gnus.el,v 7.30 1996/12/25 17:20:47 morioka Exp $
;; Keywords: news, MIME, multimedia, encoded-word, multilingual

;; This file is not part of tm (Tools for MIME).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;	Notice that this module is only for GNUS, not for Gnus.

;;; Code:

(require 'gnus)


;;; @ variables
;;;

(defvar tm-gnus/startup-hook nil)


;;; @ set up
;;;

(cond ((fboundp 'gnus-article-prepare)
       ;; for GNUS 3.15 .. 4.*
       (require 'tm-gnus4)
       )
      ((string-match "^GNUS 3" gnus-version)
       ;; for GNUS 3.14.*
       (require 'tm-gnus3)
       (defvar gnus-article-buffer gnus-Article-buffer)
       ))


;;; @ end
;;;

(provide 'tm-gnus)

(run-hooks 'tm-gnus-load-hook)

;;; tm-gnus.el ends here
