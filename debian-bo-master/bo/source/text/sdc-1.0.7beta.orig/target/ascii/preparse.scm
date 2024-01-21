(message 1 "Loading ascii")
(set! *load-path* (append *typeset-lib* *load-path*))
; For ASCII we simple redirect to Lout

(set! doc-output "lout")
(set! sgml-opts (cons "-i Lout " sgml-opts))

(load "target/lout/preparse.scm")

(set! lout-cmd "lout -P" )
(set! lout-postfilter "|sed -e s/\\ *$//")
(set! lout-initial-break " adjust 1.0fx hyphen" )
(set! compile-function (lout-run-over compile-function))
