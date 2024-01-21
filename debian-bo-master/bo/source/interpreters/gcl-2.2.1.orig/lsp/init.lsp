
(defun lcs1 (file)
       (compile-file file
                     :c-file t :h-file t :data-file t :ob-file t
                     :system-p t))

(defun make-unix_top ()
       (let ((*features* '(unix)))
            (compile-file "top" :output-file "unix_top"
                          :c-file t :h-file t :data-file t
                          :ob-file nil :fasl-file nil
                          :system-p t)))
(proclaim '(special si::*indent-formatted-output*))
