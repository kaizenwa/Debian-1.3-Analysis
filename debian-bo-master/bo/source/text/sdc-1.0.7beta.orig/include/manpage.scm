
; If the your man differs from the standard ordering translate it by
; this list

(define man-d-section #f)

(define man-section-alist
'( ("USRCMD"  . ("1" "User Commands"))
   ("SYSCALL" . ("2" "System calls"))
   ("LIBFN"   . ("3" "Library functions"))
   ("SPECIAL" . ("4" "Special files"))
   ("FFMT"    . ("5" "File formats"))
   ("DEMO"    . ("6" "Games and demonstrations"))
   ("MISC"    . ("7" "Miscellaneous"))
   ("SYS"     . ("8" "System maintenance"))
   ("KERNEL"  . ("9" "Kernel functions"))
   ("X"       . ("x" "X11 Window system"))
   ("LOCAL"   . ("l" "Local Documentation"))
))

(define (man-section id)
  (let ((ps (assoc id man-section-alist)))
    (if ps
	(cdr ps)
	(cdar man-section-alist))))

(define (man-set-section id) (set! man-d-section (man-section id)))

; change this to create either a valid URL to the manpages of your site

(define (html-make-man-ref id)
  (let ((paro (string-find-char id #\( ))
	(parc (string-find-char id #\) )))
    (if (and paro parc)
	(list "<A HREF=\"http://www.inf.tu-dresden.de/"
	      "man?arch=decstation&section=" 
	      (substring id (+ paro 1) parc) 
	      "&page=^" (substring id 0 paro)
	      "$\">" id "</A>" )   
	(list "<I>" id "</I>" ))))
