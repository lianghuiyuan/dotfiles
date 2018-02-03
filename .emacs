;;; package --- Summary
;;; Commentary:
;;; Code:

;; === SETUP ===
(require 'package) ;; You might already have this line

(add-to-list 'exec-path "/usr/local/bin")

(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(setq user-full-name "robert zhou")
(setq user-mail-address "robertzhouxh@gmail.com")

;;; Standard package repositories
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("marmalade" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(package-initialize) ;; You might already have this line

;;; Auto install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Install and configure packages through use-package
(setq use-package-always-ensure t)

(eval-when-compile
  (require 'use-package))

;;; My own configurations, which are bundled in my dotfiles.

;;; Revert buffers automatically when underlying files are changed externally
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; tabs do not turn spaces into tab
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq-default left-fringe-width nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(electric-indent-mode 1)
(global-auto-revert-mode t)

;; Convert certain words into symbols. Prime example: lambda becomes λ.
(global-prettify-symbols-mode)

;; This makes my Emacs startup time ~35% faster.
(setq gc-cons-threshold 100000000)
(setq inhibit-splash-screen t)
(setq initial-scratch-message "")

;;On  I use ⌘ as meta and prefer ⌥ to do nothing so I can still insert special characters like easily.
(setq mac-command-modifier 'meta
      mac-option-modifier 'none)

(setq ring-bell-function 'ignore)
(setq make-backup-files nil)

(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

(use-package exec-path-from-shell
             :ensure t
             :config
             (when (memq window-system '(mac ns))
               (exec-path-from-shell-initialize)))
(use-package ido
             :ensure t
             :config
             (progn
               (setq ido-enable-flex-matching t)
               (setq ido-everywhere t)
               (ido-mode 1)))

(use-package ido-vertical-mode
             :ensure t
             :init
             (ido-vertical-mode)
             (setq ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package smex
             :bind (("M-x" . smex)
                    ("M-x" . smex-major-mode-commands))
             :ensure t
             :config (smex-initialize))

(use-package magit
             :ensure t
             :bind ("C-x g" . magit-status))

(use-package git-gutter
             :ensure t
             :init
             (global-git-gutter-mode t)
             :config
             (progn
               (setq git-gutter:window-width 2)
               (setq git-gutter:modified-sign "==")
               (setq git-gutter:added-sign "++")
               (setq git-gutter:deleted-sign "--")
               (set-face-foreground 'git-gutter:added "#daefa3")
               (set-face-foreground 'git-gutter:deleted "#FA8072")
               (set-face-foreground 'git-gutter:modified "#b18cce")
               ))

;; Project management.
(use-package projectile
             :ensure t
             :commands (projectile-find-file projectile-switch-project)
             :diminish projectile-mode
             :init
             ;; projectile Ui
             (use-package helm-projectile
                          :ensure t
                          :bind (("C-x p" . helm-projectile-find-file)
                                 ("C-x j" . helm-projectile-switch-project)))
             :config
             (projectile-global-mode))

(use-package ag
             :ensure t
             :defer t
             :config
             (progn
               (setq ag-highlight-search t)
               (bind-key "n" 'compilation-next-error ag-mode-map)
               (bind-key "p" 'compilation-previous-error ag-mode-map)
               (bind-key "N" 'compilation-next-file ag-mode-map)
               (bind-key "P" 'compilation-previous-file ag-mode-map)))
(use-package helm-ag
             :ensure helm-ag
             :bind ("M-p" . helm-projectile-ag)
             :commands (helm-ag helm-projectile-ag)
             :init (setq helm-ag-insert-at-point 'symbol
                         helm-ag-command-option "--path-to-ignore ~/.agignore"))

(provide 'init)
