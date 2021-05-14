;; Cosmetic
(setq inhibit-startup-message t) ; Disable splash screen
(tool-bar-mode -1)   ; Remove toolbar
; Remove eink for now
(load-theme 'eink t) ; Load color theme
(god-mode)
; Change cusor and modeline color when in god-mode
(defun my-god-mode-color-indicator ()
  (if god-local-mode
      (progn (set-cursor-color "#F8E5AD") (set-face-background 'mode-line "#F8E5AD"))
    (progn (set-cursor-color "#e0dace") (set-face-background 'mode-line "#e0dace"))))


; Add hooks so function is executed on mode change  
(add-hook 'god-mode-enabled-hook #'my-god-mode-color-indicator)
(add-hook 'god-mode-disabled-hook #'my-god-mode-color-indicator)


; Code for making mit-scheme (sicp) work on vanilla emacs with
(set-variable (quote scheme-program-name) "Racket -I sicp")
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . scheme-mode))



; for eval'in a whole buffer using my scheme hack. wip.
;(add-hook 'scheme-mode-hook
;	  (lambda ()
;	    (local-set-key (kbd "C-x C-r") #'steph-eval-scheme-buffer)))
;
;(defun steph-eval-scheme-buffer ()
;  ())



; Define some changes to the eink theme
(setq custom--inhibit-theme-enable nil) ; unsure if needed? read online it was needed.
(custom-theme-set-faces 'eink
			'(show-paren-match ((t :foreground "black" :weight bold :background "#ddddd8")))
			'(mode-line ((t (:height 0.9 :background "#dfd9cb"))))
			'(mode-line-inactive ((t (:height 0.9 :background "#efece6" ))))
			'(fringe ((t :background "#efece7" :foreground "#ffffff")))
					; add more changes here
			);








					; Set the scratch buffer message!
(setq initial-scratch-message "\
;; Welcome to Stephen's EMACS
;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; In god-mode all keys are prefix'd with C-, so x -> C-x.
;; Hitting g is equivalent to M-, so g x -> M-x.
;; SPC breaks the C- prefix. So: x SPC s -> C-x s
;; '.' repeats the previous command.
")

; personal functions w/o hotkeys
; This needs work
;(defun r-parallel (res)
;  (defun inverse (x)
;    (/ 1.0 x))
;  (inverse (apply '+ (mapcar 'inverse res))))


; Line Numbers, except in certain major modes
(require 'display-line-numbers)
(defcustom display-line-numbers-exempt-modes '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode)
  "Major modes on which to disable the linum mode, exempts them from global requirement"
  :group 'display-line-numbers
  :type 'list
  :version "green")

(defun display-line-numbers--turn-on ()
  "turn on line numbers but excempting certain majore modes defined in `display-line-numbers-exempt-modes'"
  (if (and
       (not (member major-mode display-line-numbers-exempt-modes))
       (not (minibufferp)))
      (display-line-numbers-mode)))
(global-display-line-numbers-mode)

;; General Hooks
(add-hook 'emacs-lisp-mode-hook 'rainbow-mode) ; for .emacs edits
(add-hook 'TeX-mode-hook 'flyspell-mode) ; for papers
(add-hook 'LaTeX-mode-hook '(flyspell-mode t)) ; also for papers

;; Package manager (MELPA)
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


(use-package god-mode
  :ensure t)
(use-package rainbow-mode
  :ensure t)
(use-package eink-theme
  :ensure t)

;; key bindings
(global-set-key (kbd "<escape>") #'god-mode-all)
(define-key god-local-mode-map (kbd ".") #'repeat)
(global-set-key (kbd "C-x C-1") #'delete-other-windows)
(global-set-key (kbd "C-x C-2") #'split-window-below)
(global-set-key (kbd "C-x C-3") #'split-window-right)
(global-set-key (kbd "C-x C-0") #'delete-window)
(global-set-key (kbd "C-x C-o") #'other-window)
(global-set-key (kbd "C-x C-u") #'undo)
(global-set-key (kbd "C-x C-b") #'switch-to-buffer)
(global-set-key (kbd "C-c C-q") #'ispell-word) ; for reports!

;; spell checking hunspell
; install hunspell and add to PATH
(setq-default ispell-program-name "C:/hunspell/bin/hunspell.exe")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("021321ae56a45794f43b41de09fb2bfca184e196666b7d7ff59ea97ec2114559" default))
 '(display-time-mode t)
 '(package-selected-packages
   '(evil 2048-game pdf-tools eink-theme rainbow-mode idle-highlight-mode xelb which-key use-package org god-mode flylisp darkroom auctex)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#fffff8" :foreground "#111111" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "outline" :family "Consolas")))))
