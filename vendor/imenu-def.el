;; imenu definitions

(require 'imenu)

;; c++
(defvar imenu-c++-generic-expression
      (` 
       (
        ;; General function name regexp
        (nil
         (, 
          (concat
          "^ *" ;; space
          "\\([a-zA-Z0-9 ]+\\) +" ;; return value, static declaration
          "\\([^() |=]*\\)" ;; function name with class name
          " *(.*).*{" ;; parameters
           )) 2)

        ))
     "Imenu generic expression for C++ mode.  See `imenu-generic-expression'.")


(add-hook 'c++-mode-hook 
          (lambda ()
            (setq imenu-generic-expression imenu-c++-generic-expression)))

(provide 'imenu-def)
