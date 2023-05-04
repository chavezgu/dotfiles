;; ido is great
(ido-mode t)
(setq ido-enable-flex-matching t)

;; No bars
(menu-bar-mode -1)

;; Check if there's a tool mode
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

;; Shamelessly copied from better-defaults:
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; https://www.emacswiki.org/emacs/SavePlace
(save-place-mode 1)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(savehist-mode 1)
(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      visible-bell t
      load-prefer-newer t
      backup-by-copying t
      frame-inhibit-implied-resize t
      ediff-window-setup-function 'ediff-setup-windows-plain
      custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Finnish keyboard things
(global-set-key (kbd "M-;") 'beginning-of-buffer)
(global-set-key (kbd "M-ö") 'comment-dwim)

(global-set-key (kbd "M-:") 'end-of-buffer)
(global-set-key (kbd "M-ä") 'eval-expression)

(global-set-key (kbd "M--") 'hippie-expand)

;; tango-dark is poor man's version of monokai
(load-theme 'tango-dark)

;; Kill region
;; ido is great
(ido-mode t)
(setq ido-enable-flex-matching t)

;; No bars
(menu-bar-mode -1)

;; Check if there's a tool mode
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

;; Shamelessly copied from better-defaults:
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; https://www.emacswiki.org/emacs/SavePlace
(save-place-mode 1)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(savehist-mode 1)
(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      visible-bell t
      load-prefer-newer t
      backup-by-copying t
      frame-inhibit-implied-resize t
      ediff-window-setup-function 'ediff-setup-windows-plain
      custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Finnish keyboard things
(global-set-key (kbd "M-;") 'beginning-of-buffer)
(global-set-key (kbd "M-ö") 'comment-dwim)

(global-set-key (kbd "M-:") 'end-of-buffer)
;; eval-expression is missing

(global-set-key (kbd "M--") 'hippie-expand)

;; tango-dark is poor man's version of monokai
(load-theme 'tango-dark)

;; Kill region
;; ido is great
(ido-mode t)
(setq ido-enable-flex-matching t)

;; No bars
(menu-bar-mode -1)

;; Check if there's a tool mode
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

;; Shamelessly copied from better-defaults:
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; https://www.emacswiki.org/emacs/SavePlace
(save-place-mode 1)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(savehist-mode 1)
(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      visible-bell t
      load-prefer-newer t
      backup-by-copying t
      frame-inhibit-implied-resize t
      ediff-window-setup-function 'ediff-setup-windows-plain
      custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Finnish keyboard things
(global-set-key (kbd "M-;") 'beginning-of-buffer)
(global-set-key (kbd "M-ö") 'comment-dwim)

(global-set-key (kbd "M-:") 'end-of-buffer)
;; eval-expression is missing

(global-set-key (kbd "M--") 'hippie-expand)

(global-set-key (kbd "C--") 'undo)

;; tango-dark is poor man's version of monokai
(load-theme 'tango-dark)

;; Kill region
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)

;; One letter please
(defalias 'yes-or-no-p 'y-or-n-p)

;; org-mode things
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; Lispy things
(electric-pair-mode 1)

;;; Cascadia Font is now the best font
(custom-set-faces
 '(default ((t (:family "Cascadia Code" :foundry "unknown" :slant normal :weight normal :height 140 :width normal)))))
