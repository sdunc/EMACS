;; Stephen Duncanson
;; GNU Emacs Configuration File

(cond ((display-graphic-p)
       ;; Graphical code goes here.
       )
      (t
       ;; Console-specific code
       ))

;; C-h v system-type
(cond ((eq system-type 'windows-nt)
       ;; Windows-specific code goes here.
       )
      ((eq system-type 'gnu/linux)
       ;; Linux-specific code goes here. 
       )
      ((eq system-type 'darwin)
       (setq python-shell-interpreter "/usr/bin/python3")
       ;; MacOS-specific code goes here.
       ))

; Enable line numbers when doing any programming
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

; Take a typing break every 40 minutes
(setq type-break-interval (* 40 60))
(type-break-mode)

; Start in god-mode and bind esc to toggle it.
; I normally have esc bound to caps lock on my set ups. 
(god-mode)
(global-set-key (kbd "<escape>") #'god-mode-all)
(define-key god-local-mode-map (kbd ".") #'repeat)
(define-key god-local-mode-map (kbd "i") #'god-mode)
; Easily switch between buffers staying in god mode.
(setq god-exempt-major-modes nil)
(setq god-exempt-predicates nil)

(defun steph-god-mode-color-indicator ()
  "Toggles the color of the cursor and modeline to reflect whether god-mode is active."
  (let ((god-color "firebrick")
	(normal-color "grey50"))
    (if god-local-mode
	(progn (set-cursor-color god-color) (set-face-background 'mode-line god-color))
      (progn (set-cursor-color normal-color) (set-face-background 'mode-line normal-color)))))

; Add hooks so function is executed on mode change  
(add-hook 'god-mode-enabled-hook #'steph-god-mode-color-indicator)
(add-hook 'god-mode-disabled-hook #'steph-god-mode-color-indicator)

;; Helpful for updating website
(defun insert-current-date () (interactive)
       (insert (shell-command-to-string "echo -n $(date +%m-%d)")))

(defun insert-book-percent (x)
  "Insert percent complete with a book."
  (interactive "nEnter current page: ") 
  (let ((book-length 1069))
    (insert (number-to-string (floor (* 100 (/ x book-length)))))))

; Window control commands
(global-set-key (kbd "C-x C-1") #'delete-other-windows)
(global-set-key (kbd "C-x C-2") #'split-window-below)
(global-set-key (kbd "C-x C-3") #'split-window-right)
(global-set-key (kbd "C-x C-0") #'delete-window)
(global-set-key (kbd "C-x C-o") #'other-window)

; Dired control commands
(global-set-key (kbd "C-x C-]") #'forward-page)
(global-set-key (kbd "C-x C-[") #'backward-page)

; Buffer control commands
;(global-set-key (kbd "C-x C-b") #'buffer-menu)
(global-set-key (kbd "C-x b") #'switch-to-buffer)

; Define all C- single input event key bindings
; C-q (quoted-insert) Read next input character and insert it.
(global-set-key (kbd "C-w") #'backward-kill-word)
(global-set-key (kbd "C-x C-k") #'kill-region)
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

; Import local dir for python path
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

; Cosmetic 
(setq inhibit-startup-message t) ; Disable splash screen
(tool-bar-mode -1)   ; Remove toolbar
(setq visible-bell t) ; Instead of ringing the system bell, flash screen
(scroll-bar-mode 0)
(menu-bar-mode 0)
(setq initial-scratch-message "\
;; Welcome to Emacs
;; This buffer is for notes you don't want to save, and for Lisp evaluation.
")

; Package manager (MELPA)
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
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(beacon-color "#f2777a")
 '(custom-enabled-themes '(sanityinc-tomorrow-eighties))
 '(custom-safe-themes
   '("36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "7680e0d0fe93475fcdc514ae4df428245ab30c57114a753701e4fc09a15c949b" "998975856274957564b0ab8f4219300bca12a0f553d41c1438bbca065f298a29" "5279f5f89566d4049538804c49ad0c04e967295f68c5d15469610464e796fbf6" default))
 '(fci-rule-color "#515151")
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(frame-background-mode 'dark)
 '(package-selected-packages
   '(evil ample-theme color-theme-sanityinc-tomorrow zenburn-theme which-key ssh speed-type professional-theme pdf-tools mwim monochrome-theme minimal-theme god-mode ergoemacs-status ergoemacs-mode eink-theme dictionary datetime acme-theme))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#f2777a")
     (40 . "#f99157")
     (60 . "#ffcc66")
     (80 . "#99cc99")
     (100 . "#66cccc")
     (120 . "#6699cc")
     (140 . "#cc99cc")
     (160 . "#f2777a")
     (180 . "#f99157")
     (200 . "#ffcc66")
     (220 . "#99cc99")
     (240 . "#66cccc")
     (260 . "#6699cc")
     (280 . "#cc99cc")
     (300 . "#f2777a")
     (320 . "#f99157")
     (340 . "#ffcc66")
     (360 . "#99cc99")))
 '(vc-annotate-very-old-color nil)
 '(window-divider-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#2d2d2d" :foreground "#cccccc" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "PfEd" :family "IBM Plex Mono")))))
