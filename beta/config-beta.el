;;; config-beta.el --- Where all the magic begins
;;
;; This is the first thing to get loaded.
;;
;; Provided by Melvin, thanks to all EMACS contributors.
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;;;;; Setup config directory and auto load libraries
;; .emacs.d/config/
(setq beta-config-dir (concat
                  (file-name-directory
                   (or (buffer-file-name) load-file-name))
                  "/")
      
      ;; By default, add beta-config-dir/lib into auto load list
      (defun add-to-load-path (path)
        "Auto-load modules from given path"
        (add-to-list 'load-path (concat beta-config-dir
                                        path)))
      (add-to-load-path "lib-beta")                ;; by default add lib into load path
