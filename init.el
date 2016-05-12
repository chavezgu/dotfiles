(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

(load-theme 'monokai t)

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

(require 'fill-column-indicator)
(setq fci-rule-column 80)
(setq fci-rule-width  2)
(setq fci-rule-color "darkred")
(add-hook 'after-change-major-mode-hook 'fci-mode)

;; I copied this from the better defaults
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Snippets
;; (add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "<tab>") 'yas-expand)
(define-key yas-minor-mode-map (kbd "TAB") 'yas-expand)


;; Use electric pair
(electric-pair-mode 1)


;; GRB: highlight trailing whitespace
(set-default 'show-trailing-whitespace t)

;; Shell to mskh
(setq explicit-shell-file-name "/bin/mksh")
(setq shell-file-name "/bin/mksh")
(setenv "SHELL" shell-file-name)

;; eshell conf
(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

(column-number-mode t)

;; It's magit!
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-babel-load-languages (quote ((C . t) (emacs-lisp . t))))
 '(shell-pop-shell-type (quote ("shell" "*shell*" (lambda nil (eshell)))))
 '(shell-pop-universal-key "M-`")
 '(shell-pop-windown-position "bottom")
 '(show-paren-mode t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Inconsolata" :foundry "unknown" :slant normal :weight normal :height 140 :width normal)))))

;; Some c-mode stuff
(setq c-default-style "linux")


;; Helm configuration
(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

;; Helm keybidings
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-h a") 'helm-apropos)

;; Fuzzy match for this
(setq helm-buffers-fuzzy-matching t
      helm-M-x-fuzzy-match        t
      helm-recentf-fuzzy-match    t
      helm-apropos-fuzzy-match    t)

;; Enable man pages at that point.
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 )

(require 'helm-gtags)
;; Enable helm-gtags-mode
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-M-i") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
(define-key helm-gtags-mode-map (kbd "M-]") 'helm-gtags-find-rtag)

;; (require 'ggtags)
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
;;               (ggtags-mode 1)
;;               (setq-local eldoc-documentation-function #'ggtags-eldoc-function))))

;; This is what we have to do to avoid mksh stuff
(setq ggtags-executable-directory "/usr/bin/")

;; Company configuration
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'company-backends 'company-c-headers)
(global-set-key (kbd "C-<tab>") 'company-complete)

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
(setq org-agenda-files (list "~/org/todos/personal.org"
                             "~/org/todos/inbox.org"
                             "~/org/todos/lola.org"
                             "~/org/todos/errands.org"))

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

;; Imenu, because it's good.
(global-set-key (kbd "M-i") 'helm-semantic-or-imenu)

;; Set the wanted files.
(add-to-list 'dired-guess-shell-alist-user '("\\.pdf\\'" "mupdf"))
(add-to-list 'dired-guess-shell-alist-user '("\\.azw3\\'" "ebook-viewer"))
(add-to-list 'dired-guess-shell-alist-user '("\\.mobi\\'" "ebook-viewer"))
(add-to-list 'dired-guess-shell-alist-user '("\\.mp4\\'" "mpv"))
(add-to-list 'dired-guess-shell-alist-user '("\\.mkv\\'" "mpv"))
(add-to-list 'dired-guess-shell-alist-user '("\\.png\\'" "feh"))
(add-to-list 'dired-guess-shell-alist-user '("\\.gif\\'" "feh"))
(add-to-list 'dired-guess-shell-alist-user '("\\.jpg\\'" "feh"))

;; Sudo in TRAMP
(defun sudo ()
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

;; Projectile
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(setq projectile-switch-project-action 'helm-projectile)

;; Ace-jump-mode (Vimperator for emacs)
(require 'ace-jump-mode)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "C-c C-SPC") 'ace-jump-mode)

;; BBDB related
(require 'bbdb)
(bbdb-initialize 'gnus)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(bbdb-insinuate-message)

;; We follow Steve Yegge advice
(global-set-key "\C-x\C-m" 'helm-M-x)
(global-set-key "\C-c\C-m" 'helm-M-x)

;; No more backspace
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; Hide the minor modes
(require 'rich-minority)
(rich-minority-mode 1)
(setf rm-blacklist "")

;; Gnus
(add-hook 'gnus-summary-mode-hook
          (lambda ()
            (local-set-key (kbd "DEL") 'gnus-summary-scroll-down)))

;; We don't need to send email if we are not inside gnus.
(global-set-key (kbd "C-x m") nil)

(global-set-key (kbd "C-c C-f") 'jump-char-forward)

;; Because I'm old, I need to highlight the line where my cursor is.
(global-hl-line-mode 1)

;; I want to see the TODO's and FIXME highlighted
(hl-todo-mode)

;; Some semantic stuff that doesn't really work very well with
;; large code bases.

;; (require 'cc-mode)
;; (require 'semantic)
;; (global-semanticdb-minor-mode 1)
;; (global-semantic-idle-scheduler-mode 1)
;; (semantic-mode 1)
;; (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)

(require 'tramp)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
(setq enable-remote-dir-locals t)

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
(defun gcg-switch-to-eshell ()
  (interactive)
  (progn
    (switch-to-buffer "std")
    (insert (format "~/\n"))))

;; Hooks for info mode
(add-hook 'Info-mode-hook
          (lambda ()
            (local-set-key (kbd "DEL") 'scroll-down-line)
            (local-set-key (kbd "RET") 'scroll-up-line)
            (local-set-key (kbd "o") 'Info-follow-nearest-node)))

;; Slime stuff
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))
