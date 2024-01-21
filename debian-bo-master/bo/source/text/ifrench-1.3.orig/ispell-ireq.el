;-------La division Robotique de l'Institut de recherche d'Hydro-Qu�bec--------
; 
; Nom     : (require 'ispell-ireq)
; Fonction: Ajustements pour ispell.el � l'IREQ
; Fichiers: ispell-ireq.el
; Notes   : 
; 
; Cr��    :  4 mai 94 ----------- Martin Boyer <mboyer@ireq-robot.hydro.qc.ca>
; Modifi� : 31 d�cembre 94 ----3- Martin Boyer <mboyer@ireq-robot.hydro.qc.ca>
;           Copyright (c) 1994 Hydro-Qu�bec
; 
; Historique: 
;------------------------------------------------------------------------------

(provide 'ispell-ireq)

(autoload (fmakunbound 'ispell-word) "ispell"
  "V�rification de l'orthographe d'un mot (autocharg�)." t)
(autoload (fmakunbound 'ispell-region) "ispell"
  "V�rification de l'orthographe d'une r�gion (autocharg�)." t)
(autoload (fmakunbound 'ispell-buffer) "ispell"
  "V�rification de l'orthographe d'un fichier (autocharg�)." t)
(autoload (fmakunbound 'ispell-complete-word) "ispell"
  "Recherche et compl�tion d'un mot dans le dictionnaire courant (autocharg�)." t)
(autoload 'ispell-change-dictionary "ispell"
  "Changement du dictionnaire courant (autocharg�)." t)

(setq ispell-dictionary-alist		; sk  9-Aug-1991 18:28
  '((nil				; default (english.aff)
     "[A-Za-z]" "[^A-Za-z]" "[-']" nil ("-B") nil)
    ("english"				; make english explicitly selectable
     "[A-Za-z]" "[^A-Za-z]" "[-']" nil ("-B") nil)
    ("francais"				; francais.aff
     "[A-Za-z���-����������-�������]" "[^A-Za-z���-����������-�������]"
     "[-']" t nil)
    ("francais-TeX"			; francais.aff
     "[A-Za-z���-����������-�������\\]" "[^A-Za-z���-����������-�������\\]"
     "[-'^`\"]" t nil "~tex")
    ))

;; Red�finition pour inclure la nouvelle liste de dictionnaires
(if ispell-menu-map
  (let ((dicts (reverse (cons (cons "default" nil) ispell-dictionary-alist)))
	  name)
      (setq ispell-menu-map (make-sparse-keymap "Spell"))
      (while dicts
	(setq name (car (car dicts))
	      dicts (cdr dicts))
	(if (stringp name)
	    (define-key ispell-menu-map (vector (intern name))
	      (cons (concat "Select " (capitalize name))
		    (list 'lambda () '(interactive)
			  (list 'ispell-change-dictionary name))))))
      ;; Why do we need an alias here?
      (defalias 'ispell-menu-map ispell-menu-map)
      ;; Define commands in opposite order you want them to appear in menu.
      (define-key ispell-menu-map [ispell-change-dictionary]
	'("Change Dictionary" . ispell-change-dictionary))
      (define-key ispell-menu-map [ispell-kill-ispell]
	'("Kill Process" . ispell-kill-ispell))
      (define-key ispell-menu-map [ispell-pdict-save]
	'("Save Dictionary" . (lambda () (interactive) (ispell-pdict-save t))))
      (define-key ispell-menu-map [ispell-complete-word]
	'("Complete Word" . ispell-complete-word))
      (define-key ispell-menu-map [ispell-complete-word-interior-frag]
	'("Complete Word Frag" . ispell-complete-word-interior-frag))
      (define-key ispell-menu-map [ispell-continue]
	'("Continue Check" . ispell-continue))
      (define-key ispell-menu-map [ispell-region]
	'("Check Region" . ispell-region))
      (define-key ispell-menu-map [ispell-word]
	'("Check Word" . ispell-word))
      (define-key ispell-menu-map [ispell-buffer]
	'("Check Buffer" . ispell-buffer))
      (define-key ispell-menu-map [ispell-message]
	'("Check Message" . ispell-message))
      (define-key ispell-menu-map [ispell-help]
	'("Help" . (lambda () (interactive) (describe-function 'ispell-help))))
      ))

