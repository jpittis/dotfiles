;;;; --------------------------------------------------------
;;;; Jake's emacs config. Use at your own risk, it's evil! ;)
;;;; --------------------------------------------------------

;;; -------------------- Package Management -----------------

;; Use ELPA for package management.
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

;; Given a list of packages, make sure they're installed.
(defun require-packages (packages)
  (mapcar (lambda (package)
            (if (package-installed-p package)
		nil
              (package-install package)
              package))
	  packages))

;; Base packages are the ones I can't use emacs without!
(defvar base-packages
  '(evil evil-leader autopair auto-complete flycheck markdown-mode fzf))

;; Make sure my packages are installed.
(require-packages base-packages)

;;; -------------------- Make Emacs Less Fucky --------------

;; No menu bars please!
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; Disabe startup screen.
(setq inhibit-startup-message t)

;; Smooth scrolling with a margin.
(setq scroll-conservatively 10)
(setq scroll-margin 5)

;; Don't make backup files.
(setq make-backup-files nil)

;; Auto indentation.
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Show your line and column numbers.
(setq line-number-mode t)
(setq column-number-mode t)

;; Full line cursor.
(global-hl-line-mode 1)

;; Better parens!
(require 'autopair)
(autopair-global-mode)
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Start autocomplete.
(ac-config-default)

;;; -------------------- Make Emacs Pretty ------------------

;; A better font.
(set-face-attribute 'default nil :family "Monaco")

;; Start emacs maximized.
(toggle-frame-maximized)

;; Solarized.
(load-theme 'solarized-dark t)

;;; -------------------- Evil -------------------------------

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")

(require 'evil)
(evil-mode t)

;; Don't move the cursor back when leaving insert mode.
(setq evil-move-cursor-back nil)

;; Make escape work!
(defun minibuffer-keyboard-quit ()
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

;; Make G behave mostly like normal.
(define-key evil-normal-state-map (kbd "G") 'end-of-buffer)

;;; -------------------- Keybindings ------------------------

;; Leader keybindings.
(evil-leader/set-key
  "e" 'open-emacs-init
  "o" 'open-terminal
  "TAB" 'other-window
  "tb" 'switch-to-previous-buffer
  "tr" 'split-terminal
  "tp" 'term-paste
  "ff" 'fzf
  "fg" 'fzf-git
  "fd" 'fzf-directory
  "sc" 'slime-compile-and-load-file
  "sp" 'slime-repl-previous-input
  "se" 'slime-eval-last-expression
  "sd" 'slime-eval-defun)

(defun open-emacs-init ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun open-terminal ()
  (interactive)
  (if (equal system-type 'darwin)
      (shell-command "open /Applications/iTerm.app")
    (shell-command "gnome-terminal")))

(defun split-terminal ()
  (interactive)
  (if (one-window-p)
      (split-window-right))
  (other-window 1)
  (ansi-term "bash"))

(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;;; -------------------- Flycheck ---------------------------
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

(setq flycheck-check-syntax-automatically '(save mode-enabled))
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

(global-flycheck-mode t)

;;; -------------------- Lisp -------------------------------

;; Lisp specific packages.
(defvar lisp-packages
  '(slime))

(require-packages lisp-packages)

(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/local/bin/sbcl")

;;; -------------------- Fzf --------------------------------

;; Found this on an fzf issue.
(defadvice fzf/start (after normalize-fzf-mode-line activate)
  "Hide the modeline so FZF will render properly."
  (setq mode-line-format nil))

;; Some hacky code to start fzf from the projects root dir.
(defun in-project-root-dir (path)
  (file-directory-p (concat path ".git")))
  
(defun fzf-git (&optional path)
  (interactive)
  (or path (setq path "./"))
  (if (in-project-root-dir path)
      (progn
      (fzf-directory path) (message path))
    (let* ((current-dir (file-name-as-directory (file-truename path)))
	   (parent-dir (concat current-dir "../")))
      (fzf-git parent-dir))))


