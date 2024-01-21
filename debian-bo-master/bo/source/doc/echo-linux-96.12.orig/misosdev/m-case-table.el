;;; m-case-table.el --- code to extend the character set and support case tables.
;;; This code was taken from case-table.
;;; Instead of setting standard syntax table, it sets a local syntax table

(defun describe-buffer-case-table ()
  "Describe the case table of the current buffer."
  (interactive)
  (let ((description (make-char-table 'case-table)))
    (map-char-table
     (function (lambda (key value)
		 (set-char-table-range
		  description key
		  (cond ((null key)
			 "case-invariant")
			((/= key (downcase key))
			 (concat "uppercase, matches "
				 (char-to-string (downcase key))))
			((/= key (upcase key))
			 (concat "lowercase, matches "
				 (char-to-string (upcase key))))
			(t "case-invariant")))))
     (current-case-table))
    (save-excursion
     (with-output-to-temp-buffer "*Help*"
       (set-buffer standard-output)
       (describe-vector description)
       (help-mode)))))

(defun copy-case-table (case-table)
  (let ((copy (copy-sequence case-table)))
    ;; Clear out the extra slots so that they will be
    ;; recomputed from the main (downcase) table.
    (set-char-table-extra-slot copy 0 nil)
    (set-char-table-extra-slot copy 1 nil)
    (set-char-table-extra-slot copy 2 nil)
    copy))
    
(defun set-case-syntax-delims (l r c-table s-table)
  "Make characters L and R a matching pair of non-case-converting delimiters.
This sets the entries for L and R in TABLE, which is a string
that will be used as the downcase part of a case table.
It also modifies `standard-syntax-table' to
indicate left and right delimiters."
  (aset c-table l l)
  (aset c-table r r)
  ;; Clear out the extra slots so that they will be
  ;; recomputed from the main (downcase) table.
  (set-char-table-extra-slot c-table 0 nil)
  (set-char-table-extra-slot c-table 1 nil)
  (set-char-table-extra-slot c-table 2 nil)
  (modify-syntax-entry l (concat "(" (char-to-string r) "  ")
		       s-table)
  (modify-syntax-entry r (concat ")" (char-to-string l) "  ")
		       s-table))

(defun set-case-syntax-pair (uc lc c-table s-table)
  "Make characters UC and LC a pair of inter-case-converting letters.
This sets the entries for characters UC and LC in TABLE, which is a string
that will be used as the downcase part of a case table.
It also modifies `standard-syntax-table' to give them the syntax of
word constituents."
  (aset c-table uc lc)
  (aset c-table lc lc)
  (set-char-table-extra-slot c-table 0 nil)
  (set-char-table-extra-slot c-table 1 nil)
  (set-char-table-extra-slot c-table 2 nil)
  (modify-syntax-entry lc "w   " s-table)
  (modify-syntax-entry uc "w   " s-table))

(defun set-case-syntax (c syntax c-table s-table)
  "Make characters C case-invariant with syntax SYNTAX.
This sets the entries for character C in TABLE, which is a string
that will be used as the downcase part of a case table.
It also modifies `standard-syntax-table'.
SYNTAX should be \" \", \"w\", \".\" or \"_\"."
  (aset c-table c c)
  (set-char-table-extra-slot c-table 0 nil)
  (set-char-table-extra-slot c-table 1 nil)
  (set-char-table-extra-slot c-table 2 nil)
  (modify-syntax-entry c syntax s-table))

(provide 'm-case-table.el)
