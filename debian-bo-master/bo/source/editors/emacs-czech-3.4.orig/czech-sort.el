;; @(#) czech-sort.el -- pseudo-Czech sorting

;; @(#) $Id: czech-sort.el,v 3.0 1996/09/18 18:47:36 pdm Exp $
;; @(#) $Keywords: i18n, Czech, sorting $
;; $KnownCompatibility: 19.34, XEmacs 19.14 $

;; This file is *NOT* part of GNU Emacs nor XEmacs.

;; Copyright (C) 1996 Milan Zamazal

;; Author:       Milan Zamazal <pdm@fi.muni.cz>
;; Maintainer:   Milan Zamazal <pdm@fi.muni.cz>
;; Requires:     czech.el
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

;; This redefines standard sorting function to implement very simple
;; (i.e. absolutely nonstandard!) Czech sorting.

;;; History:

;; So long, so very long...

;;; Code:


;;; *** Start ***

(require 'czech)

(defconst cz-sort-version "$Id: czech-sort.el,v 3.0 1996/09/18 18:47:36 pdm Exp $"
  "Latest modification time and version number.")

;; We have to load standard sorting package to avoid later replacement of our
;; "replacing" functions
(load "sort")


;;; *** Define the order of characters ***

(defvar cz-sort-order
  [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
   29 30 31 32 129 131 33 34 144 143 133 136 137 147 146 126 35 125 134 245 246
   247 248 249 250 251 252 253 254 130 127 140 145 141 128 142 149 155 157 161
   165 171 173 175 177 181 183 185 191 193 197 205 207 209 215 219 223 231 233
   235 237 241 138 36 139 37 244 132 148 154 156 160 164 170 172 174 176 180
   182 184 190 192 196 204 206 208 214 218 222 230 232 234 236 240 38 135 39
   40 41 Ä Å Ç É Ñ Ö Ü á à â ä ã å ç
   é è ê ë í ì î ï ñ ó ò ô ö õ ú
   ù û ü 74 75 76 77 78 189 79 80 81 217 82 221 83 84 243 85 86 87 88
   89 90 188 91 92 93 216 94 220 95 96 242 97 211 151 98 99 153 187 100 101 159
   167 102 103 169 179 104 163 105 106 195 199 201 107 203 108 213 227 225 109
   229 239 110 111 210 150 112 113 152 186 114 115 158 166 116 117 168 178 118
   162 119 120 194 198 200 121 202 122 212 226 224 123 228 238 124 255]
  "Defines order of characters to be used for Czech sorting.
This is an 256 elements array.
N-th element of this array is the numeric order in the range 0..255 of the
character N.")


;;; *** Czech sorting functions ***

(defun cz-compare-buffer-substrings (buffer1 start1 end1 buffer2 start2 end2)
  "Modified version of `compare-buffer-substrings'.
It tries to use some very simple kind (nonstandard!) of Czech comparision.
It is not full version of `compare-buffer-substrings' but only its relevant
part."
  (let ((buffer (current-buffer))
	s1
	s2)
    (if (not buffer1)
	(setq s1 (buffer-substring-no-properties start1 end1))
      (set-buffer buffer1)
      (setq s1 (buffer-substring-no-properties start1 end1))
      (set-buffer buffer))
    (if (not buffer2)
	(setq s2 (buffer-substring-no-properties start2 end2))
      (set-buffer buffer2)
      (setq s2 (buffer-substring-no-properties start2 end2))
      (set-buffer buffer))
    (if (cz-string< s2 s1) 1 -1)))

(defun cz-string< (s1 s2)
  "Modified version of `string<'.
It tries to use some very simple kind (nonstandard!) of Czech comparision."
  (if (symbolp s1) (setq s1 (symbol-name s1)))
  (if (symbolp s2) (setq s2 (symbol-name s2)))
  (let ((l1 (length s1))
	(l2 (length s2))
	(i 0))
    (while (and (< i l1)
		(< i l2)
		(eq (aref cz-sort-order (aref s1 i))
		    (aref cz-sort-order (aref s2 i))))
      (setq i (1+ i)))
    (and (not (= i l2))
	 (or (= i l1)
	     (< (aref cz-sort-order (aref s1 i))
		(aref cz-sort-order (aref s2 i)))))))


;;; *** Replacement of the standard main sorting function ***

;;; Modified sorting routine for simple Czech sorting

(defun cz-sort-subr (reverse nextrecfun endrecfun startkeyfun endkeyfun)
  ;; Heuristically try to avoid messages if sorting a small amt of text.
  (let ((messages (> (- (point-max) (point-min)) 50000)))
    (save-excursion
      (if messages (message "Finding sort keys..."))
      (let* ((sort-lists (sort-build-lists nextrecfun endrecfun
					   startkeyfun endkeyfun))
	     (old (reverse sort-lists))
	     (case-fold-search sort-fold-case))
	(if (null sort-lists)
	    ()
	  (or reverse (setq sort-lists (nreverse sort-lists)))
	  (if messages (message "Sorting records..."))
	  (setq sort-lists
		(if (fboundp 'sortcar)
		    (sortcar sort-lists
			     (cond ((numberp (car (car sort-lists)))
				    ;; This handles both ints and floats.
				    '<)
				   ((consp (car (car sort-lists)))
				    (function
				     (lambda (a b)
				       (> 0 (cz-compare-buffer-substrings
					     nil (car a) (cdr a)
					     nil (car b) (cdr b))))))
				   (t
				    'cz-string<)))
		  (sort sort-lists
			(cond ((numberp (car (car sort-lists)))
			       'car-less-than-car)
			      ((consp (car (car sort-lists)))
			       (function
				(lambda (a b)
				  (> 0 (cz-compare-buffer-substrings
					nil (car (car a)) (cdr (car a))
					nil (car (car b)) (cdr (car b)))))))
			      (t
			       (function
				(lambda (a b)
				  (cz-string< (car a) (car b)))))))))
	  (if reverse (setq sort-lists (nreverse sort-lists)))
	  (if messages (message "Reordering buffer..."))
	  (sort-reorder-buffer sort-lists old)))
      (if messages (message "Reordering buffer... Done"))))
  nil)

;;; Original sorting routine from Emacs 19.33

(defun cz-sort-subr-orig (reverse nextrecfun endrecfun startkeyfun endkeyfun)
  ;; Heuristically try to avoid messages if sorting a small amt of text.
  (let ((messages (> (- (point-max) (point-min)) 50000)))
    (save-excursion
      (if messages (message "Finding sort keys..."))
      (let* ((sort-lists (sort-build-lists nextrecfun endrecfun
					   startkeyfun endkeyfun))
	     (old (reverse sort-lists))
	     (case-fold-search sort-fold-case))
	(if (null sort-lists)
	    ()
	  (or reverse (setq sort-lists (nreverse sort-lists)))
	  (if messages (message "Sorting records..."))
	  (setq sort-lists
		(if (fboundp 'sortcar)
		    (sortcar sort-lists
			     (cond ((numberp (car (car sort-lists)))
				    ;; This handles both ints and floats.
				    '<)
				   ((consp (car (car sort-lists)))
				    (function
				     (lambda (a b)
				       (> 0 (compare-buffer-substrings
					     nil (car a) (cdr a)
					     nil (car b) (cdr b))))))
				   (t
				    'string<)))
		  (sort sort-lists
			(cond ((numberp (car (car sort-lists)))
			       'car-less-than-car)
			      ((consp (car (car sort-lists)))
			       (function
				(lambda (a b)
				  (> 0 (compare-buffer-substrings
					nil (car (car a)) (cdr (car a))
					nil (car (car b)) (cdr (car b)))))))
			      (t
			       (function
				(lambda (a b)
				  (string< (car a) (car b)))))))))
	  (if reverse (setq sort-lists (nreverse sort-lists)))
	  (if messages (message "Reordering buffer..."))
	  (sort-reorder-buffer sort-lists old)))
      (if messages (message "Reordering buffer... Done"))))
  nil)

;;; Finally, replacement of `sort-subr' - the main sorting function
;;; This comes as last because of possible total crippling of sorting because
;;; of some error while loading this package.

(cz-message 5 "Standard sorting function replaced.")

(defun sort-subr (reverse nextrecfun endrecfun &optional startkeyfun endkeyfun)
  "General text sorting routine to divide buffer into records and sort them.
Arguments are REVERSE NEXTRECFUN ENDRECFUN &optional STARTKEYFUN ENDKEYFUN.

We divide the accessible portion of the buffer into disjoint pieces
called sort records.  A portion of each sort record (perhaps all of
it) is designated as the sort key.  The records are rearranged in the
buffer in order by their sort keys.  The records may or may not be
contiguous.

Usually the records are rearranged in order of ascending sort key.
If REVERSE is non-nil, they are rearranged in order of descending sort key.

The next four arguments are functions to be called to move point
across a sort record.  They will be called many times from within sort-subr.

NEXTRECFUN is called with point at the end of the previous record.
It moves point to the start of the next record.
It should move point to the end of the buffer if there are no more records.
The first record is assumed to start at the position of point when sort-subr
is called.

ENDRECFUN is called with point within the record.
It should move point to the end of the record.

STARTKEYFUN moves from the start of the record to the start of the key.
It may return either a non-nil value to be used as the key, or
else the key is the substring between the values of point after
STARTKEYFUN and ENDKEYFUN are called.  If STARTKEYFUN is nil, the key
starts at the beginning of the record.

ENDKEYFUN moves from the start of the sort key to the end of the sort key.
ENDKEYFUN may be nil if STARTKEYFUN returns a value or if it would be the
same as ENDRECFUN."
  ;; What kind of sorting should be used?
  (if (or (eq cz-sort t)
	  (and cz-keyboard-mode (eq cz-sort 'by-mode)))
      (cz-sort-subr
       reverse nextrecfun endrecfun startkeyfun endkeyfun)
    (cz-sort-subr-orig
     reverse nextrecfun endrecfun startkeyfun endkeyfun)))


;;; *** Miscellaneous ***

(defun cz-sort-disable ()
  "Return original Emacs definition of the main sorting function."
  (interactive)
  (load "sort"))

;;;###autoload
(defun cz-sort-toggle (&optional prefix)
  "Changes value of the variable `cz-sort'.
Change is done by rotating values `nil', `t', and `by-mode'.
If an optional prefix argument is given, rotation is done in opposite
direction."
  (interactive "P")
  (let* ((values-list '((nil . 0) (t . 1) (by-mode . 2)))
	 (current (cdr (assoc cz-sort values-list))))
    (setq current (mod (if prefix (1+ current) (1- current)) 3))
    (setq cz-sort (car (rassoc current values-list)))
    (cond
     ((eq cz-sort nil)
      (cz-message 2 "Never do Czech sorting."))
     ((eq cz-sort t)
      (cz-message 2 "Always do Czech sorting."))
     ((eq cz-sort 'by-mode)
      (cz-message 2 "Do Czech sorting according to cz-keyboard-mode.")))))


;;; *** Announce ***

(provide 'czech-sort)


;;; czech-sort.el ends here

