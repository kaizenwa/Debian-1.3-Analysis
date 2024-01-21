
;;(autoload 'get-gcl-info  "get-gcl-info" "Get gcl info for ANSI common lisp functions" t)


(defvar gcl-completion-alist nil)


(defun make-gcl-completion-alist()
  (let ((tem all-nodes)
	ans x)
    (while tem
      (setq x (car tem))
      (cond ((and (string-match ";"  x)
		  (not (string-match "; and" x)))
	     (let ((y x)(i 0) j (lim (length x)))
	       (while (< i lim)
		 (setq j (string-match "[;][ ]*" x i))
		 (setq ans (cons (cons (substring x i j) x) ans))
		 (setq i (match-end 0))
		 (or j (setq i lim))))))     
      (setq ans (cons (cons x x) ans))
      (setq tem (cdr tem))
      )
    (setq gcl-completion-alist (nreverse ans))))

;(make-gcl-completion-alist)

(defun get-gcl-info ()
  (interactive)
  (let (a(completion-ignore-case  t))
    (setq a (completing-read "Doc on: " gcl-completion-alist nil t nil nil))
    (setq a (assoc a gcl-completion-alist))
    (Info-goto-node (concat "(gcl.info)" (or (cdr a) (car a))))))

(define-key (current-global-map) "\M-?"  'get-gcl-info)

(defun get-gcl-nodes-from-tags ()
  (let ((tem Info-directory-list) buf y all)
    (save-excursion
      (while tem
	(cond
	 ((file-exists-p (concat (car tem) "gcl.info"))
	  (setq buf (find-file-noselect  (concat (car tem) "gcl.info")))
	  (set-buffer buf)
	  (setq  gcl-completion-alist nil)
	  (setq all nil)
	  (widen)
	  (goto-char (point-min))
	  (setq tem nil)
	  (search-forward "Tag Table:")
	  (search-forward "Node:")
	  (beginning-of-line)
	  (while (looking-at "Node: \\([^\n]*\\)")
	    (setq all
		  (cons (cons (setq y (buffer-substring (match-beginning 1)
							(match-end 1)))
			      y)
			all
			))
	    (forward-line 1)
	    )
	  ))
	(setq tem (cdr tem))))
  all
  ))


;(require 'all-gcl-nodes)
;(setq  gcl-completion-alist(mapcar '(lambda (x) (cons x x)) all-nodes))
;(setq  gcl-completion-alist (get-gcl-nodes-from-tags))
(setq  gcl-completion-alist (get-gcl-nodes-from-tags))

(provide 'get-gcl-info)
