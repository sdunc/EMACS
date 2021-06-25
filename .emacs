;; Stephen Duncanson
;; GNU Emacs Configuration File

; enable line numbers when doing any programming
(add-hook 'prog-mode-hook #'display-line-numbers-mode)


(type-break-mode)
;; god-mode
(god-mode)
(global-set-key (kbd "<escape>") #'god-mode-all) ; bind ESC to toggle god-mode
(setq god-exempt-major-modes nil)
(setq god-exempt-predicates nil)
(define-key god-local-mode-map (kbd ".") #'repeat)
(define-key god-local-mode-map (kbd "i") #'god-mode)
(global-set-key (kbd "C-x C-1") #'delete-other-windows)
(global-set-key (kbd "C-x C-2") #'split-window-below)
(global-set-key (kbd "C-x C-3") #'split-window-right)
(global-set-key (kbd "C-x C-0") #'delete-window)
(global-set-key (kbd "C-x C-o") #'other-window)
(global-set-key (kbd "C-x C-b") #'switch-to-buffer)
;(global-set-key (kbd "C-w") #'previous-line)
;(global-set-key (kbd "C-a") #'left-char)
;(global-set-key (kbd "C-s") #'next-line)
;(global-set-key (kbd "C-d") #'right-char)
;(global-set-key (kbd "C-q") #'beginning-of-line)
(global-set-key (kbd "C-z") #'goto-line)
(global-set-key (kbd "C-t") #'query-replace)
(global-set-key (kbd "C-x C-k") #'kill-buffer)
(global-set-key (kbd "C-x C-b") #'buffer-menu-other-window)

(defun steph-god-mode-color-indicator ()
  "Toggles the color of the cursor and modeline to reflect whether god-mode is active."
  (let ((god-color "goldenrod")
	(normal-color "#eeeeee")
	(cursor-color "#eeeeee"))
    (if god-local-mode
	(progn (set-cursor-color god-color) (set-face-background 'mode-line god-color))
      (progn (set-cursor-color cursor-color) (set-face-background 'mode-line normal-color)))))

; Add hooks so function is executed on mode change  
(add-hook 'god-mode-enabled-hook #'steph-god-mode-color-indicator)
(add-hook 'god-mode-disabled-hook #'steph-god-mode-color-indicator)



;; movement changes
(setq scroll-error-top-bottom 'true)
(setq next-line-add-newlines t)

;; Fix python..?
(setq python-shell-interpreter "/usr/bin/python3")
 (defun python-reinstate-current-directory ()
   ;;   "When running Python, add the current directory ('') to the head of sys.path.
   ;; For reasons unexplained, run-python passes arguments to the
   ;; interpreter that explicitly remove '' from sys.path. This means
   ;; that, for example, using `python-send-buffer' in a buffer
   ;; visiting a module's code will fail to find other modules in the
   ;; same directory.
   ;; Adding this function to `inferior-python-mode-hook' reinstates
   ;; the current directory in Python's search path."
   (python-send-string "sys.path[0:0] = ['']"))
(add-hook 'inferior-python-mode-hook 'python-reinstate-current-directory)


;; cosmetic 
(setq inhibit-startup-message t) ; Disable splash screen
(tool-bar-mode -1)   ; Remove toolbar
(setq visible-bell t) ; Instead of ringing the system bell, flash screen
(scroll-bar-mode 0)
(menu-bar-mode 0)
(setq initial-scratch-message "\
;; Welcome to Stephen's EMACS
;; This buffer is for notes you don't want to save, and for Lisp evaluation.
")

;; Package manager (MELPA)
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(professional))
 '(custom-safe-themes
   '("7680e0d0fe93475fcdc514ae4df428245ab30c57114a753701e4fc09a15c949b" default))
 '(package-selected-packages
   '(ergoemacs-status ergoemacs-mode zenburn-theme which-key professional-theme monochrome-theme minimal-theme god-mode eink-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#FFFFDD" :foreground "#000000" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :family "IBM Plex Mono")))))
