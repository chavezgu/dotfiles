;;; Try not to install packages!!
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

(setq-default inhibit-startup-screen t)

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(setq-default indent-tabs-mode nil)
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      visible-bell nil
      apropos-do-all t
      mouse-yank-at-point t
      ediff-window-setup-function 'ediff-setup-windows-plain
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
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; GRB: highlight trailing whitespace
(set-default 'show-trailing-whitespace t)

;; Shell to bash
(setq explicit-shell-file-name "/bin/bash")
(setq shell-file-name "/bin/bash")
(setenv "SHELL" shell-file-name)

(column-number-mode t)


(custom-set-variables
 '(org-babel-load-languages (quote ((C . t) (emacs-lisp . t) (sh . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Terminus" :foundry "unknown" :slant normal :weight normal :height 130 :width normal)))))

;; Electric pair
(electric-pair-mode 1)

;;; Ido for slow computers
(ido-mode t)

;; Company configuration
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'company-backends 'company-c-headers)
(add-to-list 'company-backends 'company-gtags)
(global-set-key (kbd "<backtab>") 'company-complete)

;; No semantics or clangs. Just tags.
(setq company-backends (delete 'company-semantic company-backends))
(setq company-backends (delete 'company-clang company-backends))

;;; C settings: 8 tabs, linux style
(defun gcg-c-mode-hook ()
  (setq c-indentation-style "linux"
        c-basic-offset 8
        tab-with 8
        indent-tabs-mode t))
(add-hook 'c-mode-hook 'gcg-c-mode-hook)

;;; C++ settings: 4 spaces, no tabs,
(defun gcg-c++-mode-hook ()
  (setq c-indentation-style "stroustrup"
        c-basic-offset 4
        indent-tabs-mode nil))
(add-hook 'c++-mode-hook 'gcg-c++-mode-hook)

;; Compile and Run,
(global-set-key (kbd "C-<f11>") 'projectile-compile-project)
(global-set-key (kbd "<f5>") 'projectile-run-project)

;; Get to know the major mode name from a buffer
(defun buffer-mode (buffer-or-string)   ;
  "Returns the major mode associated with a buffer."
  (with-current-buffer buffer-or-string
    major-mode))

(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

;; Some org-mode sane values
(require 'org)
(defun my-org-mode-hook()
  (progn
    (turn-on-flyspell)
    (auto-fill-mode 1)))
(add-hook 'org-mode-hook 'my-org-mode-hook)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-agenda-files (list "~/org/todos/inbox.org"
                             "~/org/todos/personal.org"
                             "~/org/todos/lola.org"
                             "~/Documents/work.org"))

(setq org-log-done t)
(setq org-directory "~/org/todos")
(setq org-default-notes-file "~/org/todos/inbox.org")
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-window-setup 'current-window)
(setq org-src-fontify-natively t)

;;  Function for archiving done tasks.
(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'tree))

;; Dired options
(require 'dired-x)
(setq dired-listing-switches "-alh")

;; Set the wanted files.
(add-to-list 'dired-guess-shell-alist-user '("\\.pdf\\'" "mupdf"))
(add-to-list 'dired-guess-shell-alist-user '("\\.azw3\\'" "ebook-viewer"))
(add-to-list 'dired-guess-shell-alist-user '("\\.mobi\\'" "ebook-viewer"))
(add-to-list 'dired-guess-shell-alist-user '("\\.epub\\'" "ebook-viewer"))

;; Sudo in TRAMP
(defun sudo-this ()
  "Use TRAMP to `sudo' the current buffer"
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
             buffer-file-name))))

;; GRB: use C-o and M-o to switch windows
(global-set-key (kbd "M-o") 'other-window)
(defun prev-window ()
  (interactive)
  (other-window -1))
(global-set-key (kbd "M-O") 'prev-window)

;; We follow Steve Yegge advice
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; No more backspace
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)


;; We don't need to send email if we are not inside gnus.
(global-set-key (kbd "C-x m") nil)

(require 'tramp)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
(setq enable-remote-dir-locals t)

;; Enable a proper sudo in eshell
(require 'em-tramp)
(setq eshell-prefer-lisp-functions t)
(setq eshell-prefer-lisp-variables t)
(setq password-cache t) ; enable password caching
(setq password-cache-expiry 600) ; for 10 minutes(time in secs)

;; Same as C-x 1
(global-set-key (kbd "C-x 9") 'delete-other-windows)
(global-set-key (kbd "C-x |") 'split-window-right)
(global-set-key (kbd "C-x -") 'split-window-below)

(defun gcg-display-in-selected-window (buffer window &optional alist)
  (with-selected-window window
    (switch-to-buffer buffer)
    window))

(defun gcg-switch-windows ()
  (interactive)
  (let* ((other-window (nth 1 (window-list)))
         (other-buffer (window-buffer other-window))
         (current-buffer (current-buffer)))
    (switch-to-buffer other-buffer)
    (gcg-display-in-selected-window current-buffer other-window)
    (select-window other-window)))

(global-set-key (kbd "M-\'") 'gcg-switch-windows)

;; Horrible code, but I'm just learning elisp.
(defun gcg-switch-to-shell ()
  (interactive)
  (switch-to-buffer (get-buffer "std")))

(global-set-key (kbd "C-M-`") 'gcg-switch-to-shell)

;; Hooks for info mode
(add-hook 'Info-mode-hook
          (lambda ()
            (local-set-key (kbd "DEL") 'scroll-down-line)
            (local-set-key (kbd "RET") 'scroll-up-line)
            (local-set-key (kbd "o") 'Info-follow-nearest-node)))

;; Always start a shell when we fire up emacs.
(progn
  (split-window-right)
  (other-window 1)
  (shell "std"))

;;; Paredit Mode
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'ielm-mode-hook 'paredit-mode)

;;; IRC
(setq erc-hide-list '("JOIN" "PART" "QUIT"))
