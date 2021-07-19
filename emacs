;; Stephen Duncanson
;; GNU Emacs Configuration File

(cond ((display-graphic-p)
       ;; Graphical code goes here.
       )
      (t
       ;; Console-specific code
       ))

(cond ((eq system-type 'windows-nt)
       ;; Windows-specific code goes here.
       )
      ((eq system-type 'gnu/linux)
       ;; Linux-specific code goes here. 
       )
      ((eq system-type 'darwin)
       ;; c-h v system-type
       ;; MacOS-specific code goes here.
       ))

; Enable line numbers when doing any programming
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

; Take a typing break every 40 minutes
(setq type-break-interval (* 40 60))
(type-break-mode)

; Start in god-mode and bind esc to toggle it
(god-mode)
(global-set-key (kbd "<escape>") #'god-mode-all)
(define-key god-local-mode-map (kbd ".") #'repeat)
(define-key god-local-mode-map (kbd "i") #'god-mode)


(defun steph-god-mode-color-indicator ()
  "Toggles the color of the cursor and modeline to reflect whether god-mode is active."
  (let ((god-color "goldenrod")
	(normal-color "grey70"))
    (if god-local-mode
	(progn (set-cursor-color god-color) (set-face-background 'mode-line god-color))
      (progn (set-cursor-color normal-color) (set-face-background 'mode-line normal-color)))))

; Add hooks so function is executed on mode change  
(add-hook 'god-mode-enabled-hook #'steph-god-mode-color-indicator)
(add-hook 'god-mode-disabled-hook #'steph-god-mode-color-indicator)

; window control commands
(global-set-key (kbd "C-x C-1") #'delete-other-windows)
(global-set-key (kbd "C-x C-2") #'split-window-below)
(global-set-key (kbd "C-x C-3") #'split-window-right)
(global-set-key (kbd "C-x C-0") #'delete-window)
(global-set-key (kbd "C-x C-o") #'other-window)

; buffer control commands
(global-set-key (kbd "C-x C-b") #'buffer-menu)
(global-set-key (kbd "C-x b") #'switch-to-buffer)

; Define all C- single input event key bindings
; C-q (quoted-insert) Read next input character and insert it.
; C-w (kill-region) Cut the selected region, store in kill ring
; C-e (move-end-of-line) Move cursor to end of line
; C-r (isearch-backward) Search (up) backward
(global-set-key (kbd "C-t") #'query-replace)
; C-y (yank) Yank (paste) last kill into buffer.
; C-u (universal argument) Flag or arg to other commands.
; C-i (indent-for-tab-command) Tab.
; C-o (open-line 1) Enter a blank line below cursor.
; C-p (previous-line) Move up.
; C-a (move-beginning-of-line 1) Move to start of line.
; C-s (isearch-forward) Begin searching forward.
; C-d (delete-char 1) Delete char cursor is on.
; C-f (forward-char) Move forward.
; C-g (keyboard-quit) Get out of trouble.
; C-g while god-mode -> M- Meta prefix
; C-h Display help buffer
(global-set-key (kbd "C-j") #'goto-line)
; C-k (kill-line) Kill from cursor to end of line.
; C-l (recenter-top-bottom) Scroll center->down->up  
(global-set-key (kbd "C-z") #'goto-line)
; C-x Prefix key
; C-c Mode specific prefix key
; C-v (scroll-up-command) Scroll the screen up/down 1 page.
; C-b (backward-char) Move back 1 char.
; C-n (next-line) Move down 1 line.
; C-m (newline) Ret.

; Define all M- single input commands
; In god-mode entering 'g' <-> M-
; M-q (fill-paragraph)
; M-y (yank-pop) Cycle through previous kills in kill ring.
; ...

; Define all C-M- single input commands
; In god-mode entering 'G' <-> C-M-
; ...

; Movement changes
(setq scroll-error-top-bottom 'true)
(setq next-line-add-newlines t)


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
