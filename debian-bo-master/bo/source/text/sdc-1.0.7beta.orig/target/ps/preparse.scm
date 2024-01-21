(message 1 "Loading ps")
(set! *load-path* (append *typeset-lib* *load-path*))
; For PostSript we simple redirect to Lout

(set! doc-output "lout")
(set! sgml-opts (cons "-i Lout " sgml-opts))

(load "target/lout/preparse.scm")
(set! compile-function (lout-run-over compile-function))
