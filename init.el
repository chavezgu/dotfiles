(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

(load-theme 'monokai t)

(setq-default inhibit-startup-screen t)

;; (menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(setq-default indent-tabs-mode nil)
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(when (fboundp 'winner-mode)
  (winner-mode 1))

(require 'uniquify)

(require 'saveplace)
(setq-default save-place t)

;; I copied this from the better defaults
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Evil mode: Vim rocks.
;; (require 'evil)
;; (evil-mode 1)
;; (setq key-chord-two-keys-delay 0.5)
;; (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
;; (key-chord-mode 1)
;; Exit insert mode by pressing j and then j quickly
;; (setq key-chord-two-keys-delay 0.5)

;; Don't use evil with repls
;; (evil-set-initial-state 'eshell-mode 'emacs)
;; (evil-set-initial-state 'shell-mode 'emacs)
;; (evil-set-initial-state 'lisp-interaction-mode 'emacs)
;; (evil-set-initial-state 'dired-mode 'emacs)
;; (evil-set-initial-state 'calculator 'emacs)
;; (evil-set-initial-state 'inferior-emacs-lisp-mode 'emacs)
;; (evil-set-initial-state 'racket-repl-mode 'emacs)

;; GRB: use C-o and M-o to switch windows
(global-set-key "\M-o" 'other-window)
(defun prev-window ()
  (interactive)
  (other-window -1))
(global-set-key "\M-O" 'prev-window)


;; GRB: highlight trailing whitespace
(set-default 'show-trailing-whitespace t)


(column-number-mode t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-mode t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Inconsolata" :foundry "unknown" :slant normal :weight normal :height 120 :width normal)))))

;; Set window layout

(progn
  (split-window-horizontally)
  (split-window-vertically)
  (other-window 1)
  (eshell))


; GRB: open temporary buffers in a dedicated window split
(setq special-display-regexps
        '("^\\*Completions\\*$"
          "^\\*Help\\*$"
          "^\\*grep\\*$"
          "^\\*Apropos\\*$"
          "^\\*elisp macroexpansion\\*$"
          "^\\*local variables\\*$"
          "^\\*Compile-Log\\*$"
          "^\\*Quail Completions\\*$"
          "^\\*Occur\\*$"
          "^\\*frequencies\\*$"
          "^\\*compilation\\*$"
          "^\\*Locate\\*$"
          "^\\*Colors\\*$"
          "^\\*tumme-display-image\\*$"
          "^\\*SLIME Description\\*$"
          "^\\*.* output\\*$"           ; tex compilation buffer
          "^\\*input/output of .*\\*$"
          "^\\*Man .*\\*$"
          "^\\*magit .*\\*$"
          "^\\*TeX Help\\*$"
          "^\\*Shell Command Output\\*$"
          "^\\*Async Shell Command\\*$"
          "^\\*Backtrace\\*$"))

(setq grb-temporary-window (nth 2 (window-list)))
(defun grb-special-display (buffer &optional data)
  (let ((window grb-temporary-window))
    (with-selected-window window
      (switch-to-buffer buffer)
      window)))
(setq special-display-function #'grb-special-display)

;; Some c-mode stuff
(setq c-default-style "linux")

;; Snippets
;; (add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;; Autocomplete defaults
(ac-config-default)

;; Use electric pair
(electric-pair-mode 1)

;; It's magit!
(global-set-key (kbd "C-x g") 'magit-status)


;; Helm!
(require 'helm-config)
(helm-mode 1)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

;; Compile
(global-set-key (kbd "C-c c") 'recompile)
