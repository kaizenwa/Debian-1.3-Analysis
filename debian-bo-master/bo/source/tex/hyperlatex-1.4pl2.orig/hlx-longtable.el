;;
;; Support for `longtable' package in Hyperlatex
;;

;; For Hyperlatex longtable is really the same as tabular,
;; as there is no pagination in HTML

(defun hyperlatex-longtable-hook ()
  
  ;; define the environment `longtable'
  (hyperlatex-define-environment
   ;; the arguments are:
   ;;  (1) name of the environment
   ;;  (2) number of arguments to the environment
   ;;  (3) beginning of environment
   ;;  (4) end of environment
   ;; Note that you have to write a \ as \\ in the expansions!
   "longtable" 0 "\\begin{tabular}" "\\end{tabular}"
   )
  
  ;; define the commands `\endfirsthead' and `\endhead'
  (hyperlatex-define-macro
   ;; the arguments are:
   ;;  (1) name of the command (WITHOUT backslash)
   ;;  (2) number of arguments
   ;;  (3) expansion (write \ as \\)
   "endfirsthead" 0 ""
   )
  (hyperlatex-define-macro
   "endhead" 0 ""
   )
  )

(message "Support for the `longtable' environment loaded.")

