;;;-*-emacs-lisp-*-
;;; misos -- Dealing with MS-DOS, Big5 and ISO 8859-1 problems
;;; Georges KO (kott@ccms.ntu.edu.tw)
;;;

;;; Default: Ms-Dos terminal, PC-437, multichar rep., postfix accent
(defvar isoms850 nil "If t, the MS-DOS terminal is supposed to be using 
PC-850 code page, otherwise PC-437 code page.")

(defvar ms850iso nil "If t, MS-DOS characters are interpreted with PC-850 
code page, otherwise with PC-437 code page.")

(defvar isoms850-one2one nil "If t, one byte can be represented by only one
character, otherwise it can be represented by more than one (which gives a
clearer representation, but messes the layout). This is for isoms mode.") 

(defvar ms850iso-one2one nil "If t, one byte can be represented by only one
character, otherwise it can be represented by more than one (which gives a
clearer representation, but messes the layout). This is for msiso mode.")

(defvar prefix-accent nil "With *-one2one nil, if prefix-accent is t then
accents are prefixed, otherwise accents are suffixed.")

(standard-display-8bit 128 255)

;;; Modes declarations
(defvar isoms-mode nil "ISO 8859-1 on MS-DOS mode variable")
(or (assq 'isoms-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(isoms-mode " IsoMs")
				 minor-mode-alist)))
(defvar msms-mode nil "MS-DOS on MS-DOS mode variable")
(or (assq 'msms-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(msms-mode " MsMs")
				 minor-mode-alist)))
(defvar msiso-mode nil "MS-DOS on ISO 8859-1 mode variable")
(or (assq 'msiso-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(msiso-mode " MsIso")
				 minor-mode-alist)))
(defvar big5-mode nil "Big5 mode variable")
(or (assq 'big5-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(big5-mode " Big5")
				 minor-mode-alist)))

;;; Define the modes:
;;; Each mode:
;;; - modifies display table
;;; - modifies syntax table
;;; - modifies keys behavior
;;; - modifies case table

(load "m-case-table")

;;; ISOMS
(defun isoms-mode (&optional arg)
  (interactive "P")
  (make-variable-buffer-local 'isoms-mode)
  (make-variable-buffer-local 'previous-mode)
  (setq isoms-mode (if (null arg)
		       (not isoms-mode)
		     (> (prefix-numeric-value arg) 0)))
  (setq buffer-display-table isoms-display-table)
  (set-syntax-table iso-syntax-table)
  (set-case-table iso-case-table)
  (local-set-key "\C-x8" iso-map)
  (if previous-mode
      (set previous-mode nil))
  (setq previous-mode 'isoms-mode)
  (redraw-display))

(defun isoms-display-table ()
  (interactive)
  (load "m-isoms-d"))
(isoms-display-table)
(load "m-iso-cs")
(load "m-iso-map")

;;; MSMS
(defun msms-mode (&optional arg)
  (interactive "P")
  (make-variable-buffer-local 'msms-mode)
  (make-variable-buffer-local 'previous-mode)
  (setq msms-mode (if (null arg)
		       (not msms-mode)
		     (> (prefix-numeric-value arg) 0)))
  (setq buffer-display-table msms-display-table)
  (set-syntax-table ms-syntax-table)
  (set-case-table ms-case-table)
  (local-set-key "\C-x8" ms-map)
  (if previous-mode
      (set previous-mode nil))
  (setq previous-mode 'msms-mode)
  (redraw-display))

(setq msms-display-table standard-display-table) ; No translation needed

;;; MSISO
(defun msiso-mode (&optional arg)
  (interactive "P")
  (make-variable-buffer-local 'msiso-mode)
  (make-variable-buffer-local 'previous-mode)
  (setq msiso-mode (if (null arg)
		       (not msiso-mode)
		     (> (prefix-numeric-value arg) 0)))
  (setq buffer-display-table msiso-display-table)
  (set-syntax-table ms-syntax-table)
  (set-case-table ms-case-table)
  (local-set-key "\C-x8" ms-map)
  (if previous-mode
      (set previous-mode nil))
  (setq previous-mode 'msiso-mode)
  (redraw-display))

(defun msiso-display-table ()
  (interactive)
  (load "m-msiso-d"))
(msiso-display-table)
(load "m-ms-cs")
(load "m-ms-map")

;;; Big5

(defun big5-mode (&optional arg)
  (interactive "P")
  (make-variable-buffer-local 'big5-mode)
  (make-variable-buffer-local 'previous-mode)
  (setq big5-mode (if (null arg)
		      (not big5-mode)
		    (> (prefix-numeric-value arg) 0)))	
 ; (setq buffer-display-table big5-display-table)
  (setq buffer-display-table standard-display-table)
  (set-syntax-table (standard-syntax-table))
  (local-set-key "\C-x8" big5-map)
  (if previous-mode
      (set previous-mode nil))
  (setq previous-mode 'big5-mode)
  (redraw-display))
(load "m-big5")
;;; raw --> isoms
(defun raw-mode (&optional arg)
  (interactive "P")
  (make-variable-buffer-local 'raw-mode)
  (make-variable-buffer-local 'previous-mode)
  (set-syntax-table iso-syntax-table)
  (set-case-table iso-case-table)
  (local-set-key "\C-x8" iso-map)
  (setq buffer-display-table standard-display-table)
  (if (string= "ms" (getenv "MISOSTERM"))
      (setq buffer-display-table isoms-display-table))
  (if previous-mode
      (set previous-mode nil))
  (setq previous-mode 'raw-mode)
  (redraw-display))

;;; Utilities...
(defun misos-switch ()
  (interactive)
  "Switch order: ISOMS -> MSMS -> MSISO -> Big5 -> (normal)"
  (if isoms-mode
      (msms-mode)
    (if msms-mode
	(msiso-mode)
      (if msiso-mode
	  (big5-mode)
	(if big5-mode
	    (raw-mode)
	  (isoms-mode)))))
  (message (format "%s" previous-mode)))

(setq previous-mode nil)

(global-set-key [f2] 'misos-switch)
(global-set-key [kp-f2] 'misos-switch)


