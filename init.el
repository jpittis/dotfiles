;; Jake's Emacs Config - 2016-03-08
;; I'm new to Emacs, use at your own risk!

;; Setup package manager and configure repositories.
(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)

(defun ensure-packages-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.
Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
	 nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
	   (package-install package)
	 package)))
   packages))

;; Make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; Activate installed packages.
(package-initialize)

;; Add more packages here!
(ensure-packages-installed 'evil
                           'evil-leader
                           'solarized-theme
                           'flycheck
                           'autopair
						   'go-mode
						   'go-autocomplete
						   'auto-complete)

;; Always turn on evil mode with a leader key <3.
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")

(require 'evil)
(evil-mode t)

;; No menu bars please!
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; Only load solarized in when windowed.
(mapc
 (lambda (hook-name)
   (add-hook hook-name
	     (lambda ()
	       (if window-system
		   (load-theme 'solarized-dark t)))))
 '(after-make-frame-functions
   after-init-hook))

;; Smooth scrolling with a margin.
(setq scroll-conservatively 10)
(setq scroll-margin 5)

;; Flycheck for syntax checking.
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

(setq flycheck-check-syntax-automatically '(save mode-enabled))
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

(global-flycheck-mode t)

;; Don't make backup files.
(setq make-backup-files nil)

;; Better parens!
(require 'autopair)
(autopair-global-mode)
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Don't move the cursor back when leaving insert mode.
(setq evil-move-cursor-back nil)

;; Make escape work!
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

;; Auto indentation.
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Disabe startup screen.
(setq inhibit-startup-message t)

;; Better tabs.
(setq-default tab-width 4)

;; Go language.
(defun my-go-mode-hook ()
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook 'gofmt-before-save)
    (evil-leader/set-key "j" 'godef-jump)
    (evil-leader/set-key "b" 'pop-tag-mark)
	(auto-complete-mode 1))

(add-hook 'go-mode-hook 'my-go-mode-hook)

(with-eval-after-load 'go-mode
   (require 'go-autocomplete))

;; Keybindings and keybound functions.
(evil-leader/set-key
  "e" 'open-emacs-init
  "o" 'open-terminal
  "TAB" 'other-window
  "tb" 'switch-to-previous-buffer
  "tr" 'split-terminal)

(defun open-emacs-init ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun open-terminal ()
  (interactive)
  (shell-command "gnome-terminal"))

(defun split-terminal ()
  (interactive)
  (if (one-window-p)
	  (split-window-right))
  (other-window 1)
  (eshell))

(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
