;; Stephen Duncanson
;; stephen.duncanson@gmail.com

;; god-mode - keep things emacs-y while keeping wrists happy
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

(defun steph-god-mode-color-indicator ()
  "Toggles the color of the cursor and modeline to reflect whether god-mode is active."
  (let ((god-color "#f2bb4e")
	(normal-color "grey90")
	(cursor-color "grey50"))
    (if god-local-mode
	(progn (set-cursor-color god-color) (set-face-background 'mode-line god-color))
      (progn (set-cursor-color cursor-color) (set-face-background 'mode-line normal-color)))))

; Add hooks so function is executed on mode change  
(add-hook 'god-mode-enabled-hook #'steph-god-mode-color-indicator)
(add-hook 'god-mode-disabled-hook #'steph-god-mode-color-indicator)


;; define startup message in scratch buffer
(setq initial-scratch-message "\
;; Welcome to Stephen's EMACS! 
;; This buffer is for notes you don't want to save, and for Lisp evaluation.
")

;; which-key
(which-key-mode)
(which-key-enable-god-mode-support)

;; movement changes
(setq scroll-error-top-bottom 'true)
(setq next-line-add-newlines t)

;; spellchecking
;setq-default ispell-program-name "C:/hunspell/bin/hunspell.exe")
;; python on macOS, use homebrew install not 2.7
;;(setq python-shell-interpreter "/usr/local/bin/python3")
;; (defun python-reinstate-current-directory ()
;;   "When running Python, add the current directory ('') to the head of sys.path.
;; For reasons unexplained, run-python passes arguments to the
;; interpreter that explicitly remove '' from sys.path. This means
;; that, for example, using `python-send-buffer' in a buffer
;; visiting a module's code will fail to find other modules in the
;; same directory.
;; Adding this function to `inferior-python-mode-hook' reinstates
;; the current directory in Python's search path."
;;   (python-send-string "sys.path[0:0] = ['']"))
;; (add-hook 'inferior-python-mode-hook 'python-reinstate-current-directory)


;; cosmetic 
(setq inhibit-startup-message t) ; Disable splash screen
(tool-bar-mode -1)   ; Remove toolbar
(setq visible-bell t)
(scroll-bar-mode 0)
(menu-bar-mode 0)


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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#000000" "#990000" "#339900" "#666633" "#0000C8" "#9933FF" "#666633" "#000000"])
 '(custom-enabled-themes '(professional))
 '(custom-safe-themes
   '("2fb0ce25e736e293a31fcdea6ef911473d0112230a002a4be66a1739108c9ef2" "7680e0d0fe93475fcdc514ae4df428245ab30c57114a753701e4fc09a15c949b" "716f0a8a9370912d9e6659948c2cb139c164b57ef5fda0f337f0f77d47fe9073" "176a6cbeb68b2a4d174bc3324f223b72989dcc4d66e9ef84cc190983342ec25c" "998975856274957564b0ab8f4219300bca12a0f553d41c1438bbca065f298a29" default))
 '(package-selected-packages
   '(which-key typing pdf-tools clocker professional-theme melancholy-theme faff-theme acme-theme use-package god-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#FFFFDD" :foreground "#000000" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "outline" :family "Consolas")))))









