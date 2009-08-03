(defun my-get-source-directory (path)
  "Please imlement me. Currently returns `path' inchanged."
  path)

(defvar my-anything-c-source-file-search
  '((name . "File Search")
    (init . (lambda ()
              (setq anything-default-directory
                    default-directory)))
    (candidates . (lambda ()
                    (let ((args
                           (format "'%s' \\( -path \\*/.svn \\) -prune -o -iregex '.*%s.*' -print"
                                   (my-get-source-directory anything-default-directory)
                                   anything-pattern)))
                      (start-process-shell-command "file-search-process" nil
                                                   "find" args))))
    (type . file)
    (requires-pattern . 4)
    (delayed))
  "Source for searching matching files recursively.")


(defvar anything-c-source-occur
  '((name . "Occur")
    (init . (lambda ()
              (setq anything-occur-current-buffer
                    (current-buffer))))
    (candidates . (lambda ()
                    (let ((anything-occur-buffer (get-buffer-create "*Anything Occur*")))
                      (with-current-buffer anything-occur-buffer
                        (occur-mode)
                        (erase-buffer)
                        (let ((count (occur-engine anything-pattern
                                                   (list anything-occur-current-buffer) anything-occur-buffer
                                                   list-matching-lines-default-context-lines case-fold-search
                                                   list-matching-lines-buffer-name-face
                                                   nil list-matching-lines-face
                                                   (not (eq occur-excluded-properties t)))))
                          (when (> count 0)
                            (setq next-error-last-buffer anything-occur-buffer)
                            (cdr (split-string (buffer-string) "\n" t))))))))
    (action . (("Goto line" . (lambda (candidate)
                                (with-current-buffer "*Anything Occur*"
                                  (search-forward candidate))
                                (goto-line (string-to-number candidate) anything-occur-current-buffer)))))
    (requires-pattern . 4)
    (volatile)
    (delayed)))


(defun anything-c-define-dummy-source (name func &rest other-attrib)
  `((name . ,name)
    (candidates "dummy")
    ,@other-attrib
    (filtered-candidate-transformer
     . (lambda (candidates source)
         (funcall ',func)))
    (requires-pattern . 1)
    (volatile)
    (category create)))

(defun anything-c-dummy-candidate ()
  ;; `source' is defined in filtered-candidate-transformer
  (list (cons (concat (assoc-default 'name source) 
                      " '" anything-input "'")
              anything-input)))

;; create buffer when not found
(defvar anything-c-source-buffer-not-found
  (anything-c-define-dummy-source
   "Create buffer"
   (lambda () (unless (get-buffer anything-input)
                (anything-c-dummy-candidate)))
   '(type . buffer)))

;; create an ansi-term/shell
(defvar anything-c-source-shell-not-found
  (anything-c-define-dummy-source
   "Create shell"
   #'anything-c-dummy-candidate
   '(action . sw-open-shell)))

(defvar anything-c-source-remoteshell-not-found
  (anything-c-define-dummy-source
   "Create remote shell"
   #'anything-c-dummy-candidate
   '(action . sw-open-remote-shell)))

;; create eshell when not found
(defvar anything-c-source-eshell-not-found
  (anything-c-define-dummy-source
   "Create eshell"
   #'anything-c-dummy-candidate
   '(action . my-eshell-open)))

;; m-x
(defvar anything-c-source-M-x
  (anything-c-define-dummy-source
   "M-x"
   #'anything-c-dummy-candidate
   '(type . command)
'(action . (lambda (c)
             (eval (read c))))))

;; anything source for imenu

(defvar anything-c-imenu-delimiter "/")
(defvar anything-c-cached-imenu-alist nil)
(defvar anything-c-cached-imenu-candidates nil)
(defvar anything-c-cached-imenu-tick nil)
(make-variable-buffer-local 'anything-c-cached-imenu-alist)
(make-variable-buffer-local 'anything-c-cached-imenu-candidates)
(make-variable-buffer-local 'anything-c-cached-imenu-tick)
(setq anything-c-source-imenu
  '((name . "Imenu")
    (init . (lambda ()
              (setq anything-c-imenu-current-buffer
                    (current-buffer))))
    (candidates
     . (lambda ()
         (with-current-buffer anything-c-imenu-current-buffer
           (let ((tick (buffer-modified-tick)))
             (if (eq anything-c-cached-imenu-tick tick)
                 anything-c-cached-imenu-candidates
               (setq anything-c-cached-imenu-tick tick
                     anything-c-cached-imenu-candidates
                     (condition-case nil
                         (mapcan
                          (lambda (entry)
                            (if (listp (cdr entry))
                                (mapcar (lambda (sub)
                                          (concat (car entry) anything-c-imenu-delimiter (car sub)))
                                        (cdr entry))
                              (list (car entry))))
                          (setq anything-c-cached-imenu-alist (imenu--make-index-alist)))
                       (error nil))))))))
    (volatile)
    (requires-pattern . 3)
    (action
     . (lambda (entry)
         (let* ((pair (split-string entry anything-c-imenu-delimiter))
                (first (car pair))
                (second (cadr pair)))
           (imenu
            (if second
                (assoc second (cdr (assoc first anything-c-cached-imenu-alist)))
              (assoc entry anything-c-cached-imenu-alist))))))))


;; (defvar anything-c-imenu-delimiter " / ")
;; (defvar anything-c-imenu-index-filter nil)
;; (defvar anything-c-cached-imenu-alist nil)
;; (defvar anything-c-cached-imenu-candidates nil)
;; (defvar anything-c-cached-imenu-tick nil)
;; (make-variable-buffer-local 'anything-c-imenu-index-filter)
;; (make-variable-buffer-local 'anything-c-cached-imenu-alist)
;; (make-variable-buffer-local 'anything-c-cached-imenu-candidates)
;; (make-variable-buffer-local 'anything-c-cached-imenu-tick)
;; (defun anything-imenu-create-candidates (entry)
;;   (if (listp (cdr entry))
;;       (mapcan (lambda (sub)
;;                 (if (consp (cdr sub))
;;                     (mapcar
;;                      (lambda (subentry)
;;                        (concat (car entry) anything-c-imenu-delimiter subentry))
;;                      (anything-imenu-create-candidates sub))
;;                   (list (concat (car entry) anything-c-imenu-delimiter (car sub)))))
;;               (cdr entry))
;;     (list entry)))
;; (setq anything-c-source-imenu
;;       '((name . "Imenu")
;;         (init . (lambda ()
;;                   (setq anything-c-imenu-current-buffer
;;                         (current-buffer))))
;;         (candidates
;;          . (lambda ()
;;              (with-current-buffer anything-c-imenu-current-buffer
;;                (let ((tick (buffer-modified-tick)))
;;                  (if (eq anything-c-cached-imenu-tick tick)
;;                      anything-c-cached-imenu-candidates
;;                    (setq anything-c-cached-imenu-tick tick
;;                          anything-c-cached-imenu-candidates
;;                          (condition-case nil
;;                              (mapcan
;;                               'anything-imenu-create-candidates
;;                               (setq anything-c-cached-imenu-alist
;;                                     (let ((index (imenu--make-index-alist)))
;;                                       (if anything-c-imenu-index-filter
;;                                           (funcall anything-c-imenu-index-filter index)
;;                                         index))))
;;                            (error nil))))))))
;;         (volatile)
;;         (requires-pattern . 3)
;;         (action
;;          . (lambda (entry)
;; ;;             (message (concat "ABC " entry " " anything-c-imenu-delimiter))
;;              (debug entry)
;; ;;;              (let ((path (split-string entry anything-c-imenu-delimiter))
;; ;;;                    (alist anything-c-cached-imenu-alist))
;; ;;;                (imenu
;; ;;;                 (progn
;; ;;;                   (while path
;; ;;;                     (setq alist (assoc (car path) alist)
;; ;;;                           path (cdr path)))
;; ;;;                   alist)))
;;              ))))


(provide 'anything-sources)
