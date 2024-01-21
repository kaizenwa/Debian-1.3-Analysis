(setq gnus-summary-prepare-hook '(m-gnus-set-mode))
(setq gnus-article-prepare-hook '(m-gnus-set-article))
(setq gnus-post-prepare-hook '(m-gnus-set-mode))
(setq gnus-mail-hook '(m-gnus-set-mode))

(defun m-gnus-set-mode ()
  "Set mode according to group name. Default: normal-mode."
  (interactive)
  (raw-mode)
  (setq m-gnus-article-mode-big5 nil)
  (if (or (string-equal (substring gnus-newsgroup-name 0 3) "tw.")
	  (string-equal (substring gnus-newsgroup-name 0 4) "ntu.")
	  (string-equal (substring gnus-newsgroup-name 0 5) "ntnu.")
	  (string-equal (substring gnus-newsgroup-name 0 12) "alt.chinese.")
	  (string-match "tw.bbs." gnus-newsgroup-name)
	  )
      (progn
	(setq m-gnus-article-mode-big5 t)
	(big5-mode))))

(defun m-gnus-set-article ()
  (interactive)
  (if m-gnus-article-mode-big5
    (big5-mode)
   (raw-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; w3
; Default: raw-mode, Big5 if Chinese.
(defun w3-misos ()
  "Set mode of W3 page"
  (raw-mode)
  (if (or (string-match "tw$" url-current-server)
	  (string-match "mtc" url-current-server))
      (big5-mode)))



