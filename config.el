;;; config-stable.el --- Where all the magic begins
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
(setq config-dir (concat
                  (file-name-directory
                   (or (buffer-file-name) load-file-name))
                  "/"))
      
      ;; By default, add config-dir/lib into auto load list
(defun add-to-load-path (path)
  "Auto-load modules from given path"
  (add-to-list 'load-path (concat config-dir
                                  path)))
(add-to-load-path "lib")                ;; by default add lib into load path

;;;;; Basic navigation and editing tools
(require 'ido)                          ;; auto complete for find-file, buffer swtich ...
(ido-mode t)

;; comment out these two if you want it to be safer
(setq backup-inhibited t)               ;disable backup
(setq auto-save-default nil)            ;disable auto save

(put 'dired-find-alternate-file 'disabled nil) ;; enable hotkey 'a' in dired

(global-set-key (kbd "M-m") (lambda ()
                              (interactive)
                              (call-interactively 'man))) ;; man by M-m

;;  show paren mode
(show-paren-mode t)                 ; turn paren-mode on
(setq show-paren-delay 0)           ; how long to wait?
(setq show-paren-style 'expression) ; alternatives are 'parenthesis'

(global-hl-line-mode t)                 ;; highlight current line
(setq-default indent-tabs-mode nil)     ;; insert space instead of tab

(require 'ex-util)                      ;; enabled some extend utilities, check lib-stable/ex-util.el for detail
(global-set-key "\C-w" 'ex-kill-region) ;; cut the current line if nothing selected
(global-set-key "\M-w" 'ex-kill-ring-save) ;; copy the current line if nothing selected

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

(require 'auto-complete-config)         ;; auto complete strings in buffer

;; delete selection mode
(delete-selection-mode t)  ; delete the selection area with a keypress


;;;;; Code editing tools
(add-to-load-path "lib/yasnippet")      ;; yasnippet, auto complete codes, like TextMate in OSX
(require 'yasnippet) 
(yas/initialize)
(yas/load-directory (concat config-dir
                            "lib/snippets/"))


(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)) ;; recognize .h as c++ file, since I do C++ programming a lot
(add-hook 'c-mode-common-hook              
          (lambda() 
            (local-set-key (kbd "<C-f12>") 'ff-find-other-file) ;; C-F12 to switch files between .h & .cpp
            (local-set-key (kbd "<C-f5>") (lambda () 
                                            (interactive)
                                            (compile "make -k"))) ;; C-F5 to compile
            (local-set-key (kbd "<C-M-f5>") (lambda () 
                                            (interactive)
                                            (compile "make -k clean && make -k"))) ;; C-M-F5 to recompile
            (require 'doxymacs)         ;; enable doxygen in C/CPP
            (doxymacs-mode t)
            (doxymacs-font-lock)
            ))

(setq c-basic-offset 4)                 ;; configure C offset

;;; Flymake binding
(require 'roco-flymake)                 ;; Compiling file on the fly
(global-set-key [f2] 'my-flymake-display-err-minibuf)
(global-set-key [f3] 'flymake-goto-next-error-only)
(global-set-key [(shift f3)] 'flymake-goto-prev-error-only)
(global-set-key [f4] 'flymake-goto-next-error)
(global-set-key [(shift f4)] 'flymake-goto-prev-error)

;;; C-F6 to toggle flymake mode
(global-set-key (kbd "<C-f6>") 'flymake-mode)

;;;;; Cosmetic


(defvar font-lock-error-face		'font-lock-error-face
  "Face name to use for things that should stand out.")

(defface font-lock-error-face
  '((((class color) (min-colors 88) (background light)) (:foreground "Red1" :weight bold))
    (((class color) (min-colors 88) (background dark)) (:foreground "red" :weight bold))
    (((class color) (min-colors 16) (background light)) (:foreground "Red1" :weight bold))
    (((class color) (min-colors 16) (background dark)) (:foreground "red" :weight bold))
    (((class color) (min-colors 8)) (:foreground "red"))
    (t (:inverse-video t :weight bold)))
  "*The face used to highlight error message"
  :group 'font-lock-faces)

(add-hook 'c-mode-common-hook
          (lambda ()
            (font-lock-add-keywords nil ;; Highlight FIXME/TODO/... in c-like languages
                                    '(("\\<\\(FIXME\\|WARN\\|TODO\\|BUG\\):" 1 font-lock-error-face t))
                                    )))

(add-hook 'term-mode-hook
          (lambda ()
            (font-lock-add-keywords nil ;; Highlight ERROR in terminal
                                    '(("^.*ERROR.*$" . font-lock-warning-face)))))

(setq default-frame-alist (quote (
                                  (tool-bar-lines . 0) (menu-bar-lines . 0) (right-fringe) (left-fringe)
                                  (scrollbar-lines . 0)
                                  (font . "Monaco:pixelsize=10") (foreground-color . "#F6F3E8")
                                  (background-color . "#101010") (background-mode . dark) (border-color . "black")
                                  (cursor-color . "#6789AB")
                                  )))

(scroll-bar-mode -1)                    ;; don't like scroll bars
(tool-bar-mode -1)                      ;; don't like tool bar modes

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (set-frame-font "Monaco-10")            
            (scroll-bar-mode -1)
            (menu-bar-mode -1)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:inherit highlight :background "grey10"))))
 '(show-paren-match ((t (:foreground "#F6F3E8" :background "#857B6F" :bold t))))
 '(show-paren-match ((t (:background "DarkSlateGrey"))))
 '(highlight-changes ((t (:foreground nil :background "#382f2f"))))
 '(highlight-changes-delete ((t (:foreground nil :background "#916868")))) 
 '(font-lock-comment-face ((t (:foreground "#99968B" :italic t))))
 '(hl-line ((t (:background "grey20"))))
 '(mouse ((t (:background "goldenrod"))))
 )


