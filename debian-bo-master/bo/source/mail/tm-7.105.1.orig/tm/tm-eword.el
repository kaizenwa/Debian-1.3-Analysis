;;;
;;; tm-eword.el --- RFC 1522 based multilingual MIME message header
;;;                 encoder/decoder for GNU Emacs
;;;
;;; Copyright (C) 1995 Free Software Foundation, Inc.
;;; Copyright (C) 1993,1994,1995 MORIOKA Tomohiko
;;;
;;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;; Maintainer: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;; Created: 1993/6/3 (1995/10/3 obsolete tiny-mime.el)
;;; Version:
;;;	$Id: tm-eword.el,v 7.10 1995/12/06 08:20:02 morioka Exp $
;;; Keywords: mail, news, MIME, RFC 1522, multilingual, encoded-word
;;; Commentary: This module is obsoleted. tm-ew-d.el is part of
;;;             tm-view.el, and tm-ew-e.el is part of tm-edit.el.
;;;
;;; This file is part of tm (Tools for MIME).
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with This program.  If not, write to the Free Software
;;; Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; Code:

(require 'tl-822)
(require 'tl-str)
(require 'tm-def)

(autoload 'mime/encode-message-header "tm-ew-e" nil t)
(autoload 'mime/decode-message-header "tm-ew-d" nil t)
(autoload 'mime/encode-field "tm-ew-e" nil t)

(autoload 'mime-eword/decode-region "tm-ew-d" nil t)
(autoload 'mime-eword/encode-string "tm-ew-e")
(autoload 'mime-eword/decode-string "tm-ew-d")

(autoload 'mime/exist-encoded-word-in-subject "tm-ew-e" nil t)


;;; @ end
;;;

(provide 'tm-eword)
