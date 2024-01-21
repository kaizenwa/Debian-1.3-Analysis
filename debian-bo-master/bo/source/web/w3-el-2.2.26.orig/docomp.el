(setq load-path (cons (expand-file-name "./") load-path))

(defun hack-dot-emacs ()
  (interactive)
  (let* ((args command-line-args-left)
	 (fname (expand-file-name (nth 0 args)))
	 (dir (nth 1 args)))
    (setq command-line-args-left (cdr (cdr command-line-args-left)))
    (set-buffer (get-buffer-create " *x*"))
    (erase-buffer)
    (if (file-exists-p fname)
	(insert-file-contents fname))
    (goto-char (point-min))
    (if (search-forward ";;; Emacs-w3 configuration options" nil t)
	(message "No changes made.")
      (goto-char (point-max))
      (insert "\n;;; Emacs-w3 configuration options\n")
      (insert "(setq load-path (cons (expand-file-name \""
	      dir "\") load-path))\n")
      (insert "(autoload 'w3-preview-this-buffer \"w3\" \"WWW Previewer\" t)\n")
      (insert "(autoload 'w3-follow-url-at-point \"w3\" \"Find document at pt\" t)\n")
      (insert "(autoload 'w3 \"w3\" \"WWW Browser\" t)\n")
      (insert "(autoload 'w3-open-local \"w3\" \"Open local file for WWW browsing\" t)\n")
      (insert "(autoload 'w3-fetch \"w3\" \"Open remote file for WWW browsing\" t)\n")
      (insert "(autoload 'w3-use-hotlist \"w3\" \"Use shortcuts to view WWW docs\" t)\n")
      (insert "(autoload 'w3-show-hotlist \"w3\" \"Use shortcuts to view WWW docs\" t)\n")
      (insert "(autoload 'w3-follow-link \"w3\" \"Follow a hypertext link.\" t)\n")
      (insert "(autoload 'w3-batch-fetch \"w3\" \"Batch retrieval of URLs\" t)\n")
      (insert "(autoload 'url-get-url-at-point \"url\" \"Find the url under the cursor\" nil)\n")
      (insert "(autoload 'url-file-attributes  \"url\" \"File attributes of a URL\" nil)\n")
      (insert "(autoload 'url-popup-info \"url\" \"Get info on a URL\" t)\n")
      (insert "(autoload 'url-retrieve   \"url\" \"Retrieve a URL\" nil)\n")
      (insert "(autoload 'url-buffer-visiting \"url\" \"Find buffer visiting a URL.\" nil)\n")
      (insert "(autoload 'gopher-dispatch-object \"gopher\" \"Fetch gopher dir\" t)\n")
      (insert ";;; End of Emacs-w3 configuration options\n")
      (write-file fname))))

(defun w3-declare-variables (&rest args)
  (while args
    (eval (list 'defvar (car args) nil ""))
    (setq args (cdr args))))

;; For Emacs 19
(w3-declare-variables 'track-mouse 'menu-bar-help-menu)

;; For Emacs 18
(w3-declare-variables 'mouse-map 'x-button-middle 'x-button-c-middle)

;; For Epoch
(w3-declare-variables 'mouse-middle 'mouse-down 'buffer-style)

;; For macintosh
(w3-declare-variables 'buffers-menu)

;; For OS/2
(w3-declare-variables 'emx-binary-mode)

;; For XEmacs/Lucid
(w3-declare-variables 'current-menubar 'default-menubar 'extent
		      'mode-motion-hook 'mode-popup-menu 'sound-alist
		      'inhibit-help-echo 'default-toolbar
		      'bottom-toolbar-height 'top-toolbar-height
		      'toolbar-buttons-captioned-p
		      'right-toolbar-width 'left-toolbar-width
		      'top-toolbar 'bottom-toolbar 'right-toolbar
		      'left-toolbar 'device-fonts-cache)

;; For MULE
(w3-declare-variables '*noconv* '*autoconv* '*euc-japan* '*internal*
		      'w3-mime-list-for-code-conversion
		      'file-coding-system-for-read 'file-coding-system)

;; For Mailcrypt
(w3-declare-variables 'mc-pgp-path 'mc-pgp-key-begin-line 'mc-ripem-pubkeyfile
		      'mc-default-scheme 'mc-flag)

;; For NNTP
(w3-declare-variables 'nntp-server-buffer 'nntp-server-process 'nntp/connection
		      'gnus-nntp-server 'nntp-server-name 'nntp-version
		      'gnus-default-nntp-server)

;; For ps-print
(w3-declare-variables 'ps-bold-faces 'ps-italic-faces 'ps-print-version)

;; For xpm-button
(w3-declare-variables 'x-library-search-path)

;; For a few intern things
(w3-declare-variables 'w3-blink-style 'w3-wired-style 'tag 'w3-working-buffer
		      'proxy-info)

(w3-declare-variables 'command-line-args-left 'standard-display-table)

(load "bytecomp" t t nil)
;; Emacs 19 byte compiler complains about too much stuff by default.
;; Turn off most of the warnings here.
(setq byte-compile-warnings '(free-vars))

;; Turn off dynamic docstrings and lazy function loading.  This
;; is a new feature of FSF Emacs 19.29, and is incompatible
;; with pre-19.29 versions of FSF Emacs and all version of Lucid
;; Emacs / XEmacs.
(setq byte-compile-dynamic nil
      byte-compile-dynamic-docstrings nil)

(require 'w3-vars)
(require 'url)
(require 'mm)
(load-library "w3-sysdp")
(provide 'ange-ftp)
