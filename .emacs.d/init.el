;;; package --- Summary
;;; Commentary:
;;; Code:

;; === SETUP ===
(require 'package) ;; You might already have this line
(package-initialize)

(defvar vendor-dir (expand-file-name "vendor" user-emacs-directory))
(defvar backup-dir "~/.emacs.d/backups/")
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path vendor-dir)
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(let ((files (directory-files-and-attributes "~/.emacs.d/lisp" t)))
  (dolist (file files)
    (let ((filename (car file))
          (dir (nth 1 file)))
      (when (and dir
                 (not (string-suffix-p "." filename)))
        (add-to-list 'load-path (car file))))))

(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;;; Standard package repositories

;(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
;                         ("melpa" . "http://elpa.emacs-china.org/melpa/")))
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("marmalade" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

;;; Pin some packages to specific repositories.
;(setq package-pinned-packages '((gtags . "marmalade")))

(package-initialize) ;; You might already have this line


;; proxy
;(setq url-proxy-services '(
;        ("no_proxy" . "^\\(localhost\\|10.*\\)")
;        ("http" . "127.0.0.1:8118")
;        ("https" . "127.0.0.1:8118")
;))

;;; auto install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)

(eval-when-compile
  (require 'use-package))

;;; My own configurations, which are bundled in my dotfiles.
(require 'init-bootstrap)
(require 'init-utils)
(require 'init-plantform)
(require 'init-pkgs)
(require 'init-evil)
(require 'init-org)
(require 'init-latex)
(require 'init-languages)
(require 'init-maps)

(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a4c9e536d86666d4494ef7f43c84807162d9bd29b0dfd39bdf2c3d845dcc7b2e" default)))
 '(package-selected-packages
   (quote
    (highlight-indentation crontab-mode material-theme yasnippet yaml-mode wsd-mode which-key web-mode use-package uimage telephone-line systemd swiper spaceline-all-the-icons solarized-theme restclient rainbow-mode rainbow-delimiters racket-mode pdf-tools paredit ox-ioslide org-page org-evil org-bullets nlinum multiple-cursors monokai-theme markdown-mode magit lua-mode linum-off latex-preview-pane js2-mode hippie-exp-ext highlight-symbol helm-projectile helm-descbinds helm-dash helm-ag gotham-theme go-eldoc git-gutter geiser flycheck exec-path-from-shell evil-surround evil-leader evil-indent-textobject dockerfile-mode docker dired-k dired+ company-go comment-dwim-2 color-identifiers-mode bpr avy auctex atom-one-dark-theme anzu ansible all-the-icons-dired ag ac-slime))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t :height 0.9))))
