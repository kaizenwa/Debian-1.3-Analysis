;;; base64.el,v --- Base64 encoding functions
;; Author: wmperry
;; Created: 1995/05/07 14:34:47
;; Version: 1.3
;; Keywords: extensions

;;; LCD Archive Entry:
;;; base64.el|William M. Perry|wmperry@spry.com|
;;; Package for encoding/decoding base64 data|
;;; 1995/05/07 14:34:47|1.3|Location Undetermined
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Base 64 encoding functions
;;; This code was converted to lisp code by me from the C code in
;;; ftp://cs.utk.edu/pub/MIME/b64encode.c
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar base64-encoding
 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
 "The string to use to encode with base 64.")

(defvar base64-max-line-length 64)

;(defun b0 (x) (aref base64-encoding (logand (lsh x -18) 63)))
;(defun b1 (x) (aref base64-encoding (logand (lsh x -12) 63)))
;(defun b2 (x) (aref base64-encoding (logand (lsh x -6) 63)))
;(defun b3 (x) (aref base64-encoding (logand x 63)))

(defmacro b0 (x) (` (aref base64-encoding (logand (lsh (, x) -18) 63))))
(defmacro b1 (x) (` (aref base64-encoding (logand (lsh (, x) -12) 63))))
(defmacro b2 (x) (` (aref base64-encoding (logand (lsh (, x) -6) 63))))
(defmacro b3 (x) (` (aref base64-encoding (logand (, x) 63))))

(defun base64-encode (str)
  "Do base64 encoding on string STR and return the encoded string.
This code was converted to lisp code by me from the C code in
ftp://cs.utk.edu/pub/MIME/b64encode.c.  Returns a string that is
broken into `base64-max-line-length' byte lines."
  (or str (setq str (buffer-string)))
  (let ((x (base64-encode-internal str))
	(y ""))
    (while (> (length x) base64-max-line-length)
      (setq y (concat y (substring x 0 base64-max-line-length) "\n")
	    x (substring x base64-max-line-length nil)))
    (setq y (concat y x))
    y))

(defun base64-encode-internal (str)
  "Do base64 encoding on string STR and return the encoded string.
This code was converted to lisp code by me from the C code in
ftp://cs.utk.edu/pub/MIME/b64encode.c.  Returns the entire string,
not broken up into `base64-max-line-length' byte lines."
  (let (
	(word 0)			; The word to translate
	w1 w2 w3
	)
    (cond
     ((> (length str) 3)
      (concat
       (base64-encode-internal (substring str 0 3))
       (base64-encode-internal (substring str 3 nil))))
     ((= (length str) 3)
      (setq w1 (aref str 0)
	    w2 (aref str 1)
	    w3 (aref str 2)
	    word (logior
		  (lsh (logand w1 255) 16)
		  (lsh (logand w2 255) 8)
		  (logand w3 255)))
      (format "%c%c%c%c" (b0 word) (b1 word) (b2 word) (b3 word)))
     ((= (length str) 2)
      (setq w1 (aref str 0)
	    w2 (aref str 1)
	    word (logior
		  (lsh (logand w1 255) 16)
		  (lsh (logand w2 255) 8)
		  0))
      (format "%c%c%c=" (b0 word) (b1 word) (b2 word)))
     ((= (length str) 1)
      (setq w1 (aref str 0)
	    word (logior
		  (lsh (logand w1 255) 16)
		  0))
      (format "%c%c==" (b0 word) (b1 word)))
     (t ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Base64 decoding functions
;;; This was hacked together by me
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun base64-decode-chunk (chunk)
  (let* ((case-fold-search nil)
	 (number 
	  (+ (lsh (logand 63 (or (string-match (char-to-string (aref chunk 0))
					       base64-encoding) 0)) 18)
	     (lsh (logand 63 (or (string-match (char-to-string (aref chunk 1))
					   base64-encoding) 0)) 12)
	     (lsh (logand 63 (or (string-match (char-to-string (aref chunk 2))
					   base64-encoding) 0)) 6)
	     (logand 63 (or (string-match (char-to-string (aref chunk 3))
					  base64-encoding) 0)))))
    (let ((a (logand (lsh number -16) 255))
	  (b (logand (lsh number -8) 255))
	  (c (logand number 255))
	  (numblanks (if (string-match "==" chunk) 2
		       (if (string-match "=" chunk) 1 0))))
      (cond
       ((= numblanks 0)
	(format "%c%c%c" a b c))
       ((= numblanks 1)
	(concat (char-to-string a) (char-to-string b)
		(if (= 0 c) "" (char-to-string c))))
       ((= numblanks 2)
	(concat (char-to-string a)
		(if (= 0 b) "" (char-to-string c))
		(if (= 0 c) "" (char-to-string c))))))))	 

(defun base64-decode (st &optional nd)
  "Do base64 decoding on string STR and return the original string.
If given buffer positions, destructively decodes that area of the
current buffer."
  (let ((replace-p nil)
	(retval nil))
    (if (stringp st)
	nil
      (setq st (prog1
		   (buffer-substring st (or nd (point-max)))
		 (delete-region st (or nd (point-max))))
	    replace-p t))
    (setq retval
	  (save-excursion
	    (set-buffer (get-buffer-create " *b64decode*"))
	    (erase-buffer)
	    (insert st)
	    (goto-char (point-min))
	    (while (re-search-forward "\r*\n" nil t)
	      (replace-match ""))
	    (goto-char (point-min))
	    (while (not (eobp))
	      (let ((chunk (base64-decode-chunk
			    (buffer-substring (point)
					      (progn
						(forward-char 4)
						(point))))))
		(backward-delete-char 4)
		(insert chunk)))
	    (buffer-string)))
    (if replace-p (insert retval))
    retval))

(provide 'base64)
