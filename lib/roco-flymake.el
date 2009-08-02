;;; roco-kit-flymake.el --- Making the defaults a bit saner
;;
;; Part of the Emacs Roco Kit

(require 'flymake)

;;; Display flymake error message
(defun my-flymake-display-err-minibuf () 
  "Displays the error/warning for the current line in the minibuffer"
  (interactive)
  (let* ((line-no             (flymake-current-line-no))
         (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (count               (length line-err-info-list))
         )
    (while (> count 0)
      (when line-err-info-list
        (let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
               (full-file  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
               (text (flymake-ler-text (nth (1- count) line-err-info-list)))
               (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
          (message "[%s] %s" line text)
          )
        )
      (setq count (1- count)))))

;; Do not launch flymake automatically
;;(add-hook 'find-file-hook 'flymake-find-file-hook)

(defun flymake-get-next-err-only-line-no (err-info-list line-no)
  "Return next line with error."
  (when err-info-list
    (let* ((count  (length err-info-list))
	   (idx    0))

      (while 
           (and (< idx count) 
                (or
                 (>= line-no (flymake-er-get-line (nth idx err-info-list)))
                 (=
                  (flymake-get-line-err-count 
                   (flymake-er-get-line-err-info-list (nth idx err-info-list))
                   "e")
                  0)))
                (setq idx (1+ idx)))
      (if (< idx count)
	  (flymake-er-get-line (nth idx err-info-list))))))



(defun flymake-get-prev-err-only-line-no (err-info-list line-no)
  "Return previous line with error."
  (when err-info-list
    (let* ((count (length err-info-list)))
      (while (and 
              (> count 0) 
              (or
               (<= line-no (flymake-er-get-line (nth (1- count) err-info-list)))
               (=
                (flymake-get-line-err-count 
                 (flymake-er-get-line-err-info-list (nth (1- count) err-info-list))
                 "e")
                0))
               )
	(setq count (1- count)))
      (if (> count 0)
	  (flymake-er-get-line (nth (1- count) err-info-list))))))



(defun flymake-goto-prev-error-only ()
  "Go to previous error (not warn) in err ring."
  (interactive)
  (let ((line-no (flymake-get-prev-err-only-line-no flymake-err-info (flymake-current-line-no))))
    (when (not line-no)
;;    (let* ((count (length err-info-list)))
      (setq line-no (flymake-get-prev-err-only-line-no flymake-err-info (+ (flymake-er-get-line (nth (1- (length flymake-err-info)) flymake-err-info)) 1)))
      (flymake-log 1 "passed beginning of file"))
    (if line-no
	(flymake-goto-line line-no)
      (flymake-log 1 "no errors in current buffer"))))


(defun flymake-goto-next-error-only ()
  "Go to next error (not warn) in err ring."
  (interactive)
  (let ((line-no (flymake-get-next-err-only-line-no flymake-err-info (flymake-current-line-no))))
    (when (not line-no)
      (setq line-no (flymake-get-next-err-only-line-no flymake-err-info 0))
      (flymake-log 1 "passed end of file"))
    (if line-no
	(flymake-goto-line line-no)
      (flymake-log 1 "no errors in current buffer"))))

;; Do NOT warn via GUI, really annoying...
(setq flymake-gui-warnings-enabled nil)

(defun flymake-java-ecj-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'jde-ecj-create-temp-file))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    ;; Change your ecj.jar location here
    (list "java" (list "-jar" "/users/hatu/.emacs.d/jar/ecj.jar" "-Xemacs" "-d" "/dev/null" 
                       "-source" "1.5" "-target" "1.5" "-proceedOnError"
                       "-sourcepath" (car jde-sourcepath) "-classpath" 
                       (jde-global-classpath) temp-file))))
 
(defun flymake-java-ecj-cleanup ()
  "Cleanup after `flymake-java-ecj-init' -- delete temp file and dirs."
;;  (flymake-safe-delete-file flymake-temp-source-file-name)
  (when flymake-temp-source-file-name
    (flymake-safe-delete-directory (file-name-directory flymake-temp-source-file-name))))
 
(defun jde-ecj-create-temp-file (file-name prefix)
  "Create the file FILE-NAME in a unique directory in the temp directory."
  (file-truename (expand-file-name (file-name-nondirectory file-name)
                                   (expand-file-name  (int-to-string (random)) (flymake-get-temp-dir)))))

(push '(".+\\.java$" flymake-java-ecj-init flymake-java-ecj-cleanup) flymake-allowed-file-name-masks)
 
;(push '("\\(.*?\\):\\([0-9]+\\): error: \\(.*?\\)\n" 1 2 nil 2 3 (6 compilation-error-face)) compilation-error-regexp-alist)
 
;(push '("\\(.*?\\):\\([0-9]+\\): warning: \\(.*?\\)\n" 1 2 nil 1 3 (6 compilation-warning-face)) compilation-error-regexp-alist)


(provide 'roco-flymake)

;;; roco-kit-flymake.el ends here
