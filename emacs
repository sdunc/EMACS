;; Stephen Duncanson
;; GNU Emacs Configuration

(cond ((display-graphic-p)
       ;; Graphical code goes here.
       )
      (t
       ;; Console-specific code
       ))

;; c-h v system-type
(cond ((eq system-type 'windows-nt)
       ;; Windows-specific code goes here.
       )
      ((eq system-type 'gnu/linux)
       ;; Linux-specific code goes here. 
       )
      ((eq system-type 'darwin)
       ;; MacOS-specific code goes here.
       ;;(setq python-shell-interpreter "/usr/local/bin/python3")
       ))

; Enable line numbers when doing any programming
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook  #'electric-pair-mode)
; Take a typing break every 40 minutes
(setq type-break-interval (* 40 60))
(type-break-mode)

; some defaults pilfered from sanemacs
; Line-style cursor similar to other text editors
(setq-default cursor-type 'bar)
; y-or-n-p makes answering questions faster
(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1)                       ; Show closing parens by default
; Line number format
(setq linum-format "%4d ")
; selected text will be deleted if you start typing, like many other editors
(delete-selection-mode 1)

; Start in god-mode and bind esc to toggle it
(god-mode)
(global-set-key (kbd "<escape>") #'god-mode-all)
(define-key god-local-mode-map (kbd ".") #'repeat)
(define-key god-local-mode-map (kbd "i") #'god-mode)
(setq god-exempt-major-modes nil)
(setq god-exempt-predicates nil)
(define-key god-local-mode-map (kbd "[") #'backward-paragraph)
(define-key god-local-mode-map (kbd "]") #'forward-paragraph)



;;; Put Emacs auto-save and backup files to /tmp/ or C:/Temp/
(defconst emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
(setq
   backup-by-copying t                                        ; Avoid symlinks
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t
   auto-save-list-file-prefix emacs-tmp-dir
   auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t))  ; Change autosave dir to tmp
   backup-directory-alist `((".*" . ,emacs-tmp-dir)))

(defun srd-open-line ()
  "opens a line and indents the line below"
  (interactive)
  (open-line 1)
  (forward-line 1)
  (indent-relative)
  (beginning-of-line)
  (backward-char 1))
  
(defun srd-god-mode-color-indicator ()
  "Toggles the color of the cursor and modeline to reflect whether god-mode is active."
  (let ((god-color "#ab4642")
	(normal-color "#585858"))
    (if god-local-mode
	(progn (set-cursor-color god-color) (setq cursor-type 'box))
      (progn (set-cursor-color normal-color) (setq cursor-type 'bar)))))

; Add hooks so function is executed on mode change  
(add-hook 'god-mode-enabled-hook #'srd-god-mode-color-indicator)
(add-hook 'god-mode-disabled-hook #'srd-god-mode-color-indicator)

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
;(global-set-key (kbd "C-t") #'query-replace)
; C-t (transpose-chars)
; C-y (yank) Yank (paste) last kill into buffer.
; C-u (universal argument) Flag or arg to other commands.
; C-i (indent-for-tab-command) Tab.
; C-o (open-line 1) Enter a blank line below cursor.
;(global-set-key (kbd "C-o") #'srd-open-line)
; C-p (previous-line) Move up.
; C-a (move-beginning-of-line 1) Move to start of line.
; C-s (isearch-forward) Begin searching forward.
; C-d (delete-char 1) Delete char cursor is on.
; C-f (forward-char) Move forward.
; C-g (keyboard-quit) Get out of trouble.
; C-g while god-mode -> M- Meta prefix
; C-h Display help buffer, use f1 for this
(global-set-key (kbd "C-h") #'backward-kill-word)
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
;(setq visible-bell t) ; Instead of ringing the system bell, flash screen

(setq ring-bell-function 'ignore)         ; Disable bell sound

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
 '(ansi-color-names-vector
   [("#181818" . "#282828")
    ("#ab4642" . "#dc9656")
    ("#a1b56c" . "#383838")
    ("#f7ca88" . "#383838")
    ("#7cafc2" . "#585858")
    ("#ba8baf" . "#b8b8b8")
    ("#86c1b9" . "#d8d8d8")
    ("#ffffff" . "#ffffff")])
 '(custom-enabled-themes '(base16-tomorrow-night-eighties))
 '(custom-safe-themes
   '("3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "8e51e44e5b079b2862335fcc5ff0f1e761dc595c7ccdb8398094fb8e088b2d50" "542e6fee85eea8e47243a5647358c344111aa9c04510394720a3108803c8ddd1" "c614d2423075491e6b7f38a4b7ea1c68f31764b9b815e35c9741e9490119efc0" "34ed3e2fa4a1cb2ce7400c7f1a6c8f12931d8021435bad841fdc1192bd1cc7da" "8c1dd3d6fdfb2bee6b8f05d13d167f200befe1712d0abfdc47bb6d3b706c3434" "760ce657e710a77bcf6df51d97e51aae2ee7db1fba21bbad07aab0fa0f42f834" "819d24b9aba8fcb446aecfb59f87d1817a6d3eb07de7fdec67743ef32194438b" "cc0dbb53a10215b696d391a90de635ba1699072745bf653b53774706999208e3" "6bdcff29f32f85a2d99f48377d6bfa362768e86189656f63adbf715ac5c1340b" "78c4238956c3000f977300c8a079a3a8a8d4d9fee2e68bad91123b58a4aa8588" "0568a5426239e65aab5e7c48fa1abde81130a87ddf7f942613bf5e13bf79686b" "a6473f7abf949f4a6a1a9cc0dd37ea2e35ba3cea65d3442b98d65c5c5c5cb8d7" default))
 '(package-selected-packages
   '(base16-theme sane-term zenburn-theme use-package riscv-mode rainbow-mode modus-themes minimal-theme magit gruvbox-theme god-mode disable-mouse bongo basic-theme auctex ascii-table))
 '(pdf-view-midnight-colors '("#282828" . "#fbf1c7"))
 '(vc-annotate-background "#ffffff")
 '(vc-annotate-color-map
   '((20 . "#ab4642")
     (50 . "#dc9656")
     (80 . "#f7ca88")
     (110 . "#a1b56c")
     (140 . "#86c1b9")
     (170 . "#7cafc2")
     (200 . "#ba8baf")
     (230 . "#a16046")
     (260 . "#181818")
     (290 . "#282828")
     (320 . "#383838")
     (350 . "#585858")))
 '(vc-annotate-very-old-color "#585858"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
