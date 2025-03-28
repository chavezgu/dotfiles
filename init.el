;; Requires
(require 'uniquify)
(require 'package)
(require 'ansi-color)

;; Modes
(fido-vertical-mode t)
(save-place-mode 1)
(show-paren-mode 1)
(savehist-mode 1)
(electric-pair-mode 1)
(column-number-mode 1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Autoloads and aliases
(defalias 'yes-or-no-p 'y-or-n-p)
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

;; Define variables
(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      load-prefer-newer t
      backup-by-copying t
      frame-inhibit-implied-resize t
      ido-enable-flex-matching t
      uniquify-buffer-name-style 'forward
      indent-tabs-mode nil
      ediff-window-setup-function 'ediff-setup-windows-plain
      find-file-visit-truename t
      make-backup-files nil
      fill-column 80
      inhibit-startup-message t
      custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Finnish keyboard things
(global-set-key (kbd "M-;") 'beginning-of-buffer)
(global-set-key (kbd "M-ö") 'comment-dwim)
(global-set-key (kbd "M-:") 'end-of-buffer)
(global-set-key (kbd "M-ä") 'eval-expression)
(global-set-key (kbd "M--") 'hippie-expand)
(global-set-key (kbd "C--") 'undo)
(global-set-key (kbd "M-\"") 'mark-word)

;; Better global keybindings
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)
(global-set-key (kbd "M-o") 'other-window)

;; org-mode things
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; Let's have this back:
(custom-set-faces
 '(default ((t (:family "CaskaydiaCove Nerd Font Mono" :foundry "unknown" :slant normal :weight normal :height 160 :width normal)))))	

;; Some weird compilation thing, that should be part of emacs
(defun my-ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'my-ansi-colorize-buffer)

;; Move the current window to the other side
(defun my-display-in-selected-window (buffer window &optional alist)
  (with-selected-window window
    (switch-to-buffer buffer)
    window))

(defun my-switch-windows ()
  (interactive)
  (unless (= 1 (length (window-list)))
    (let* ((other-window (nth 1 (window-list)))
           (other-buffer (window-buffer other-window))
           (current-buffer (current-buffer)))
      (switch-to-buffer other-buffer)
      (my-display-in-selected-window current-buffer other-window)
      (select-window other-window))
    ))

(global-set-key (kbd "M-å") 'my-switch-windows)

;; Tango dark is decent and it's part of emacs
(load-theme 'tango-dark)
;; Melpa stuff
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
