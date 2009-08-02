;; Provide some extend utilities for easy to use

;;; Easy kill & copy
(defun ex-kill-ring-save (&optional line)
  "This function is a enhancement of `kill-ring-save', which is normal used
to copy a region.  This function will do exactly as `kill-ring-save' if
there is a region selected when it is called. If there is no region, then do
copy lines as `yy' in vim."
  (interactive "P")
  (unless (or line (and mark-active (not (equal (mark) (point)))))
    (setq line 1))
  (if line
      (let ((beg (line-beginning-position))
            (end (line-end-position)))
        (when (>= line 2)
          (setq end (line-end-position line)))
        (when (<= line -2)
          (setq beg (line-beginning-position (+ line 2))))
        (if (and ex-kill-ring-save-include-last-newline
                 (not (= end (point-max))))2
            (setq end (1+ end)))
        (kill-ring-save beg end))
    (call-interactively 'kill-ring-save)))
;; set the following var to t if you like a newline to the end of copied text.
(setq ex-kill-ring-save-include-last-newline nil)

;; A useful copy & paste enhancement by Veldrin@smth
(defun ex-kill-region (&optional line)
  "This function is a enhancement of `kill-region', which is normal used to
kill a region to kill-ring.  This function will do exactly as `kill-region'
if there is a region selected when it is called. If there is no region, then
do kill lines as `dd' in vim."
  (interactive "P")
  (unless (or line (and mark-active (not (equal (mark) (point)))))
    (setq line 1))
  (if line
      (let ((beg (line-beginning-position))
            (end (line-end-position)))
        (when (>= line 2)
          (setq end (line-end-position line)))
        (when (<= line -2)
          (setq beg (line-beginning-position (+ line 2))))
        (if (and ex-kill-region-include-last-newline
                 (not (= end (point-max))))
            (setq end (1+ end)))
        (kill-region beg end))
    (call-interactively 'kill-region)))
;; set the following var to t if you like a newline in the end of killed text.
(setq ex-kill-region-include-last-newline t)

;; automatically indenting yanked text if in programming-modes
(defvar yank-indent-modes '(emacs-lisp-mode
                            c-mode c++-mode
                            tcl-mode sql-mode
                            perl-mode cperl-mode
                            java-mode jde-mode
                            lisp-interaction-mode
                            LaTeX-mode TeX-mode)
  "Modes in which to indent regions that are yanked (or yank-popped)")

(defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))

(defadvice yank (after yank-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (member major-mode yank-indent-modes))
      (let ((transient-mark-mode nil))
    (yank-advised-indent-function (region-beginning) (region-end)))))

(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (member major-mode yank-indent-modes))
    (let ((transient-mark-mode nil))
      (yank-advised-indent-function (region-beginning) (region-end)))))


;;## enable word search, like #,* in VIM
(defun sacha/isearch-yank-current-word ()
  "Pull current word from buffer into search string."
  (interactive)
  (save-excursion
    (skip-syntax-backward "w_")
    (isearch-yank-internal
     (lambda ()
       (skip-syntax-forward "w_")
       (point)))))

;;## enable word search, like #,* in VIM
(define-key isearch-mode-map (kbd "C-x") 'sacha/isearch-yank-current-word)


;; Enable pause key for window dedication
;; Toggle window dedication
;; I hate emacs update my window automatically, my focus was lost
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window 
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

(global-set-key [pause] 'toggle-window-dedicated)



;;; Simulate VIM o & O command
(defun my-open-line-below ()
  (interactive)
  (end-of-line)
  (open-line 1)
  (next-line 1)
  (indent-according-to-mode))

(defun my-open-line-above ()
  (interactive)
  (beginning-of-line)
  (open-line 1)
  
  (indent-according-to-mode))

;;; Simulate VIM o & O command
(global-set-key [?\C-o] 'my-open-line-below)
(global-set-key [?\M-o] 'my-open-line-above)

;; jos for just-one-space
(defun jos ()
  "just one space"
  (interactive)
  (just-one-space))


;; imenu
(require 'imenu)
(require 'imenu-def)
(defun ido-goto-symbol ()
  "Will update the imenu index and then use ido to select a symbol to navigate to"
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
	(symbol-names '()))
    (flet ((addsymbols (symbol-list)
		       (when (listp symbol-list)
			 (dolist (symbol symbol-list)
			   (let ((name nil) (position nil))
			     (cond
			      ((and (listp symbol) (imenu--subalist-p symbol))
			       (addsymbols symbol))
			      ((listp symbol)
			       (setq name (car symbol))
			       (setq position (cdr symbol)))
			      ((stringp symbol)
			       (setq name symbol)
			       (setq position (get-text-property 1 'org-imenu-marker symbol))))
			     (unless (or (null position) (null name))
			       (add-to-list 'symbol-names name)
			       (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
	   (position (cdr (assoc selected-symbol name-and-pos))))
      (if (markerp position)
	  (goto-char position) (goto-char (overlay-start position))))))

(setq imenu-auto-rescan t)
(global-set-key (kbd "M-s") 'ido-goto-symbol)


(autoload 'log4j-mode "log4j-mode" "Major mode for viewing log files." t)
(add-to-list 'auto-mode-alist '("\\.log\\'" . log4j-mode)) ;; log4cxx/log4j log viewer mode

(provide 'ex-util)