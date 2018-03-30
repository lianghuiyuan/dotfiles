;;; init-packages.el
;;; Commentary:
;;; Code:

;; Utilities
(use-package diminish :ensure t)
(use-package exec-path-from-shell
             :ensure t
             :config
             (exec-path-from-shell-initialize)
             (exec-path-from-shell-copy-env "GOPATH"))
(use-package which-key
             :ensure t
             :defer 10
             :config
             (progn
               ;; for emacs 26+
               (which-key-setup-side-window-right)
               (which-key-mode 1)))
(use-package helm-descbinds
             :ensure t
             :init (helm-descbinds-mode))
(use-package json-reformat
             :ensure t
             :defer t
             :bind (("C-x i" . json-reformat-region)))
(use-package rainbow-mode
             :ensure t
             :defer t
             :commands rainbow-mode)
(use-package rainbow-delimiters
             :defer t
             :ensure t
             :init
             (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
(use-package comment-dwim-2
             :defer t
             :ensure t
             :bind ("M-;" . comment-dwim-2))

(use-package multiple-cursors
             :ensure t
             :bind (("C-c C-. *"   . mc/mark-all-dwim)
                    ("C-c C-. C-." . mc/mark-all-like-this-dwim)
                    ("C-c C-. n"   . mc/mark-next-like-this)
                    ("C-c C-. p"   . mc/mark-previous-like-this)
                    ("C-c C-. a"   . mc/mark-all-like-this)
                    ("C-c C-. N"   . mc/mark-next-symbol-like-this)
                    ("C-c C-. P"   . mc/mark-previous-symbol-like-this)
                    ("C-c C-. A"   . mc/mark-all-symbols-like-this)
                    ("C-c C-. f"   . mc/mark-all-like-this-in-defun)
                    ("C-c C-. l"   . mc/edit-lines)
                    ("C-c C-. e"   . mc/edit-ends-of-lines)
                    ("C-M-<mouse-1>" . mc/add-cursor-on-click)))

(use-package hl-line
             :ensure t
             :init     (add-hook 'prog-mode-hook 'hl-line-mode))

(use-package helm
             :ensure t
             :defer t
             :bind
             ("C-x C-f" . helm-find-files)
             ("C-x b" . helm-mini)
             :commands helm-mode
             :init (progn
                     ;; for os-x add the line
                     (setq helm-man-format-switches "%s")
                     (require 'helm-config)
                     (setq helm-split-window-default-side 'other)
                     (setq helm-split-window-in-side-p t))
             :config (progn
                       (setq
                         helm-move-to-line-cycle-in-source nil
                         ;helm-split-window-default-side 'left
                         ;helm-always-two-windows t
                         helm-candidate-number-limit 200
                         helm-M-x-requires-pattern 0
                         helm-google-suggest-use-curl-p t
                         )
                       (helm-autoresize-mode 1)
                       (define-key helm-map (kbd "C-b") 'helm-keyboard-quit)
                       (define-key helm-map (kbd "C-p") 'helm-keyboard-quit)
                       (define-key helm-map (kbd "C-j") 'helm-next-line)
                       (define-key helm-map (kbd "C-k") 'helm-previous-line)
                       ))
(use-package hideshow
             :ensure t
             :bind (("C-c TAB" . hs-toggle-hiding)
                    ("C-\\" . hs-toggle-hiding)
                    ("M-\\" . hs-hide-all)
                    ("M-=" . hs-show-all))
             :init
             (progn
               (defun my/enable-hs-minor-mode ()
                 (interactive)
                 (hs-minor-mode t))
               (add-hook 'prog-mode-hook 'my/enable-hs-minor-mode))
             :config
             (progn
               (defvar hs-special-modes-alist
                 (mapcar 'purecopy
                         '((c-mode "{" "}" "/[*/]" nil nil)
                           (c++-mode "{" "}" "/[*/]" nil nil)
                           (bibtex-mode ("@\\S(*\\(\\s(\\)" 1))
                           (js-mode "{" "}" "/[*/]" nil)
                           (javascript-mode  "{" "}" "/[*/]" nil))))))

(use-package highlight-symbol
             :defer t
             :ensure t
             :diminish ""
             :config
             (setq-default highlight-symbol-idle-delay 1.5))

(use-package flycheck
             :ensure t
             :defer t
             :config
             (global-flycheck-mode)
             ;; disable jshint since we prefer eslint checking
             (setq-default flycheck-disabled-checkers
                           (append flycheck-disabled-checkers
                                   '(javascript-jshint)))

             (setq flycheck-checkers '(javascript-eslint))
             ;; use eslint with web-mode for jsx files
             (flycheck-add-mode 'javascript-eslint 'web-mode)
             (flycheck-add-mode 'javascript-eslint 'js2-mode)
             (flycheck-add-mode 'javascript-eslint 'js-mode)
             ;; To verify just do C-h v flycheck-eslintrc
             (setq flycheck-eslintrc "~/.eslintrc"))

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
             :ensure t
             :defer t
             :init
             (setq helm-ag-base-command "ag --nocolor --nogroup --ignore-case"
                   helm-ag-command-option "--all-text"
                   helm-ag-insert-at-point 'symbol))

(use-package yasnippet
             :ensure t
             :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
             :init
             (progn
               (setq yas-verbosity 3)
               (yas-global-mode 1)))

;(use-package hippie-expand
(use-package hippie-exp-ext
             :ensure t
             :defer t
             :init
             (setq hippie-expand-try-functions-list
                   '(try-complete-file-name-partially
                      try-complete-file-name
                      try-expand-dabbrev
                      try-expand-dabbrev-all-buffers
                      try-expand-dabbrev-from-kill))
             :bind
             ("M-/" . hippie-expand))

(use-package magit
             :ensure t
             :defer t
             :bind (("M-g s" . magit-status)
                    ("M-g l" . magit-log)
                    ("M-g f" . magit-pull)
                    ("M-g p" . magit-push)
                    ("M-g x" . magit-reset-hard))
             :init
             (setq magit-popup-show-common-commands nil)
             (setq magit-log-arguments '("--graph"
                                         "--decorate"
                                         "--color"))
             :config
             (progn
               (defadvice magit-status (around magit-fullscreen activate)
                          (window-configuration-to-register :magit-fullscreen)
                          ad-do-it
                          (delete-other-windows))

               (defun magit-quit-session ()
                 "Restores the previous window configuration and kills the magit buffer"
                 (interactive)
                 (kill-buffer)
                 (jump-to-register :magit-fullscreen))

               (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))

             ;; removes 1.4.0 warning in arguably cleaner way
             (remove-hook 'after-init-hook 'magit-maybe-show-setup-instructions)
             (defadvice magit-blame-mode (after switch-to-emacs-state activate)
                        (if magit-blame-mode
                          (evil-emacs-state 1)
                          (evil-normal-state 1))))

(use-package git-gutter
             :ensure t
             :defer t
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

(use-package projectile
             :defer t
             :ensure t
             :config
             (projectile-global-mode)
             (setq projectile-enable-caching t))

(use-package helm-projectile
             :defer t
             :commands (helm-projectile helm-projectile-switch-project)
             :ensure t)

(use-package company
             :ensure t
             :defer t
             :init
             (global-company-mode)
             :config
             (setq company-idle-delay 0.2)
             (setq company-selection-wrap-around t)
             (define-key company-active-map [tab] 'company-complete)
             (define-key company-active-map (kbd "C-n") 'company-select-next)
             (define-key company-active-map (kbd "C-p") 'company-select-previous))

;; reffer to http://jwintz.me/blog/2014/02/16/helm-dash-makes-you-efficient/
(use-package helm-dash
             :ensure t
             :defines (helm-dash-docsets)
             :functions (esk-helm-dash-install
                          helm-dash-web
                          helm-dash-go
                          helm-dash-installed-docsets)
             :commands (helm-dash-at-point esk-helm-dash-install)
             :preface
             (progn
               (defvar esk-dash-docsets
                 ;'("Bash" "C" "C++" "Go" "Redis" "Ansible" "UnderscoreJS" "JavaScript" "React"))
                 '("Bash" "Go"))

               (defun esk-helm-dash-install (docset-name)
                 (message (format "Installing helm-dash docset '%s'" docset-name))
                 (unless (file-exists-p (concat (concat helm-dash-docsets-path docset-name) ".docset"))
                   (helm-dash-install-docset docset-name)))

               (defun esk-dash-limit (docsets-names)
                 (set (make-local-variable 'helm-dash-docsets) docsets-names))

               (defun helm-dash-bash () (esk-dash-limit '("Bash")))
               (defun helm-dash-go () (esk-dash-limit '("Go" "Redis")))
               (defun helm-dash-yaml () (esk-dash-limit '("Ansible")))
               (defun helm-dash-c () (esk-dash-limit '("c")))
               (defun helm-dash-web () (esk-dash-limit '("UnderscoreJS" "JavaScript" "React")))

               :init
               (progn
                 (setq helm-dash-docsets-path "~/.emacs.d/docsets/")
                 (after sh-script (add-hook 'sh-mode-hook 'helm-dash-bash))
                 (after go-mode (add-hook 'go-mode-hook 'helm-dash-go))
                 ;(after yaml-mode (add-hook 'yaml-mode-hook 'helm-dash-yaml))
                 ;(after c-mode (add-hook 'c-mode-hook 'helm-dash-c))
                 ;(after web-mode (add-hook 'web-mode-hook 'helm-dash-web))
                 )
               :config
               (progn
                 (defun eww-split (url)
                   (interactive)
                   (select-window (split-window-right))
                   (eww url))
                 (setq helm-dash-browser-func 'eww-split)
                 ;(setq helm-dash-browser-func 'eww)
                 (add-hook 'prog-mode-hook
                           (lambda ()
                             (interactive)
                             (setq helm-current-buffer (current-buffer))))
                 (dolist (docset esk-dash-docsets)
                   (esk-helm-dash-install docset))
                 )))

(use-package yaml-mode
             :ensure t
             :mode ("\\.yml$" . yaml-mode)
             :config
             (use-package highlight-indentation
                          ;:load-path "vendor/Highlight-Indentation-for-Emacs"
                          :ensure t
                          :init
                          (setq highlight-indentation-offset 2)
                          :config
                          (add-hook 'yaml-mode-hook 'highlight-indentation-mode)
                          (set-face-background 'highlight-indentation-face "#e3e3d3")
                          (set-face-background 'highlight-indentation-current-column-face "#c3b3b3")
                          ))
(use-package dired+
             :ensure t
             :commands (dired-jump)
             :bind (("C-x C-j" . dired-jump)))

(use-package dired-k
             :ensure t
             :config
             (progn
               (add-hook 'dired-initial-position-hook 'dired-k)))

;; UI
(use-package material-theme
             :ensure t
             :config
             (load-theme 'material 'no-confirm))

(use-package solarized-theme
             :ensure t
             :disabled t
             :init
             (load-theme 'solarized-light 'no-confirm))

(use-package monokai-theme
             :ensure t
             :disabled t
             :init (load-theme 'monokai 'no-confirm))

(load-theme 'monokai t)

;; ======================== mode bar ==============================================
;; Icon package
(use-package all-the-icons :ensure t)
(use-package all-the-icons-dired
             :after all-the-icons
             :ensure t)

;; Requirement of Spaceline
(use-package powerline :ensure t)
(use-package spaceline
             :ensure t
             :after powerline
             :init
             (progn
               ;; slant (requires srbg support)
               (setq powerline-default-separator 'slant)
               ;(setq powerline-default-separator 'wave)
               (setq spaceline-workspace-numbers-unicode t)
               (setq spaceline-separator-dir-left '(right . right))
               (setq spaceline-separator-dir-right '(right . right))
               (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
               (setq powerline-height 20))
             :config
             (require 'spaceline-config)
             (spaceline-toggle-major-mode-on)
             (spaceline-toggle-minor-modes-off)
             (spaceline-toggle-buffer-modified-on)
             (spaceline-spacemacs-theme)
             (spaceline-helm-mode)
             (spaceline-compile)
             (setq spaceline-buffer-encoding-abbrev-p nil
                   spaceline-window-numbers-unicode t
                   spaceline-line-column-p nil
                   ;;spaceline-buffer-id-p nil
                   spaceline-minor-modes-separator nil)
             (powerline-reset))

;; ======================= depreciated ==========================
;;(use-package restclient
;;             :ensure t
;;             :defer t
;;             :mode ("\\.http\\'" . restclient-mode))
;;(use-package anzu
;;             :ensure    anzu
;;             :defer t
;;             :config    (global-anzu-mode t)
;;             :diminish  anzu-mode)
;;(use-package avy
;;             :ensure t
;;             :init
;;             :defer t
;;             :bind ("C-c SPC" . avy-goto-char))

(provide 'init-pkgs)
