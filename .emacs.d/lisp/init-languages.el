;;; init-languages.el --- Set up programming languages
;;; Commentary:

;; Basic programming languages

;;; Code:

;; --------------------------------------------------------------------
;; generate the tag files
;; --------------------------------------------------------------------
;; scheme-1
;; (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
;; global、gtags、gtags-cscope三个命令。global是查询，gtags是生成索引文件，gtags-cscope是与cscope一样的界面
;; 查询使用的命令是global和gtags-cscope。前者是命令行界面，后者是与cscope兼容的ncurses界面
;; helm-ggtags
;(if
;    (executable-find "global")
;    (progn
;      (use-package bpr :ensure t)
;      (use-package helm-gtags
;        :diminish helm-gtags-mode
;        :init
;        (add-hook 'dired-mode-hook 'helm-gtags-mode)
;        (add-hook 'eshell-mode-hook 'helm-gtags-mode)
;        (add-hook 'c-mode-hook 'helm-gtags-mode)
;        (add-hook 'c++-mode-hook 'helm-gtags-mode)
;        (add-hook 'asm-mode-hook 'helm-gtags-mode)
;        (add-hook 'go-mode-hook (lambda () (helm-gtags-mode)))
;        (add-hook 'python-mode-hook (lambda () (helm-gtags-mode)))
;        (add-hook 'ruby-mode-hook (lambda () (helm-gtags-mode)))
;        (add-hook 'lua-mode-hook (lambda () (helm-gtags-mode)))
;        (add-hook 'js-mode-hook (lambda () (helm-gtags-mode)))
;        (add-hook 'erlang-mode-hook (lambda () (helm-gtags-mode)))
;        :config
;        ;(custom-set-variables
;        ; '(helm-gtags-prefix-key "C-t")
;        ; '(helm-gtags-suggested-key-mapping t))
;        (setq
;         helm-gtags-ignore-case t
;         helm-gtags-auto-update t
;         helm-gtags-use-input-at-cursor t
;         helm-gtags-pulse-at-cursor t
;         helm-gtags-prefix-key "\C-cg"
;         helm-gtags-suggested-key-mapping t
;         )
;         (define-key helm-gtags-mode-map (kbd "C-]") 'helm-gtags-dwim)
;         (define-key helm-gtags-mode-map (kbd "C-t") 'helm-gtags-pop-stack)
;        ))
;  (message "%s: GNU GLOBAL not found in exec-path. helm-gtags will not be used." 'please check))

;; scheme-2
;; /usr/local/Cellar/global/6.5.5/share/gtags/gtags.el
;; gtags --gtagslabel=pygments --debug
;; or use ensure t

;; scheme-2
;; /usr/local/Cellar/global/6.5.5/share/gtags/gtags.el
;; gtags --gtagslabel=pygments --debug
;; (use-package gtags :ensure t)
(require 'gtags)
(use-package bpr :ensure t)

;; Bind some useful keys in the gtags select buffer that evil overrides.
(add-hook 'gtags-select-mode-hook
          (lambda ()
            (evil-define-key 'normal gtags-select-mode-map (kbd "RET") 'gtags-select-tag)
            (evil-define-key 'normal gtags-select-mode-map (kbd "q") 'kill-buffer-and-window)))

(autoload 'vc-git-root "vc-git")
(defun gtags-reindex ()
  "Kick off gtags reindexing."
  (interactive)
  (let* ((root-path (expand-file-name (vc-git-root (buffer-file-name))))
         (gtags-filename (expand-file-name "GTAGS" root-path)))
    (if (file-exists-p gtags-filename)
      (gtags-index-update root-path)
      (gtags-index-initial root-path))))

(defun gtags-index-initial (path)
  "Generate initial GTAGS files for PATH."
  (let ((bpr-process-directory path))
    (bpr-spawn "gtags")))

(defun gtags-index-update (path)
  "Update GTAGS in PATH."
  (let ((bpr-process-directory path))
    (bpr-spawn "global -uv")))

;-------------------------------------------------------
(use-package dumb-jump
             :ensure nil
             :bind (("M-g o" . dumb-jump-go-other-window)
                    ("M-g j" . dumb-jump-go)
                    ("M-g ." . dumb-jump-back)
                    ("M-g i" . dumb-jump-go-prompt)
                    ("M-g x" . dumb-jump-go-prefer-external)
                    ("M-g z" . dumb-jump-go-prefer-external-other-window))
             :config (setq dumb-jump-selector 'helm) ;; (setq dumb-jump-selector 'ivy)
             )

;;--------------------------------------------------------------
;; sh-mode
;;--------------------------------------------------------------
(use-package sh-script
             :defer t
             :config (setq sh-basic-offset 4))

(use-package eldoc
             :diminish eldoc-mode
             :init  (setq eldoc-idle-delay 0.1))

;;--------------------------------------------------------------
;; cc-mode
;;--------------------------------------------------------------
(use-package cc-mode
  :config
  (progn
    (add-hook 'c-mode-hook (lambda () (c-set-style "bsd")))
    (add-hook 'java-mode-hook (lambda () (c-set-style "bsd")))
    (setq tab-width 4)
    (setq c-basic-offset 4)))

;;---------------------------------------------------------------
;; Erlang
;;---------------------------------------------------------------
(let* ((emacs-version "3.0.1")
       (tools-path
         (concat "/usr/lib/erlang/lib/tools-" emacs-version "/emacs")))
  (when (file-exists-p tools-path)
    (setq load-path (cons tools-path load-path))
    (setq erlang-root-dir "/usr/lib/erlang")
    (setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
    (require 'erlang-start)
    (defvar inferior-erlang-prompt-timeout t)))

;; get erlang man page
(defun get-erl-man ()
  (interactive)
  (let* ((man-path "/usr/lib/erlang/man")
         (man-args (format "-M %s %s" man-path (current-word))))
    (man man-args)))

(defun erlang-insert-binary ()
  "Inserts a binary string into an Erlang buffer and places the
  point between the quotes."
  (interactive)
  (insert "<<\"\">>")
  (backward-char 3)
  )

(eval-after-load "erlang" '(define-key erlang-mode-map (kbd "C-c b") 'erlang-insert-binary))

(add-to-list 'auto-mode-alist '("rebar.config" . erlang-mode)) ;; rebar
(add-to-list 'auto-mode-alist '("rebar.config.script" . erlang-mode)) ;; rebar
(add-to-list 'auto-mode-alist '("app.config" . erlang-mode)) ;; embedded node/riak
(add-to-list 'auto-mode-alist '("\\.src$" . erlang-mode)) ;; User customizations file
(add-to-list 'auto-mode-alist '("\\.erlang$" . erlang-mode)) ;; User customizations file
(add-to-list 'auto-mode-alist '("\\.erl$" . erlang-mode)) ;; User customizations file
(add-to-list 'auto-mode-alist '("\\.hrl" . erlang-mode)) ;; User customizations file

;;;----------------------------------------------------------------------------
;;; lua
;;;----------------------------------------------------------------------------
(use-package lua-mode
             :ensure t
             :defer t
             :mode "\\.lua\\'"
             :init
             (add-hook 'lua-mode-hook
                       (lambda ()
                         (setq lua-indent-level 4)
                         (auto-complete-mode)
                         (hs-minor-mode)
                         (turn-on-font-lock)
                         )
                       )
             )

;; get lua man page
(defun get-lua-man ()
  (interactive)
  (let* ((man-path "/usr/local/Cellar/lua/5.2.4_4/share/man")
         (man-args (format "-M %s %s" man-path (current-word))))
    (man man-args)))

;;----------------------------------------------------------------------------
;; lisp: C-x C-e 执行光标下lisp
;; 或者 l执行整个buffer ==> ,e
;;----------------------------------------------------------------------------
(use-package slime
             :defer t
             :config
             (progn
               (use-package ac-slime :ensure t)
               (setq inferior-lisp-program "sbcl")
               (slime-setup)
               (add-hook 'slime-mode-hook 'set-up-slime-ac)
               (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
               (add-hook 'slime-mode-hook
                         (lambda ()
                           (unless (slime-connected-p)
                             (save-excursion (slime)))))
               (slime-setup '(slime-fancy slime-asdf))
               (slime-setup '(slime-repl slime-fancy slime-banner))
               (setq slime-protocol-version 'ignore
                     slime-net-coding-system 'utf-8-unix
                     ;;slime-complete-symbol*-fancy t
                     slime-complete-symbol-function 'slime-fuzzy-complete-symbol))
             :ensure t)

(use-package color-identifiers-mode
             :ensure t
             :init
             (add-hook 'emacs-lisp-mode-hook 'color-identifiers-mode)
             :diminish color-identifiers-mode)

;;----------------------------------------------------------------------------
;; Golang
;; go get github.com/rogpeppe/godef
;; go get -u github.com/golang/lint/golint
;; go get -u github.com/nsf/gocode
;;----------------------------------------------------------------------------
(use-package company-go
             :ensure t
             :defer t
             :init
             (with-eval-after-load 'company
                                   (add-to-list 'company-backends 'company-go)))

(use-package go-eldoc
             :ensure t
             :defer
             :init
             (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package go-mode
 :config
 (bind-keys :map go-mode-map
  ("C-," . godef-jump)
  ("C-;" . pop-tag-mark)
  )
 (add-hook 'go-mode-hook '(lambda () (setq tab-width 2)))
 (setq gofmt-command "goimports")
 (add-hook 'before-save-hook 'gofmt-before-save))

;;----------------------------------------------------------------------------
;; es6
;;----------------------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
(use-package js2-mode
             :config (setq js2-basic-offset 2))

;; force web-mode’s content type as jsx for .js and .jsx files
(use-package web-mode :ensure t)

(defun my-web-mode-hook ()
  "Hooks for Web mode. Adjust indents"
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(add-hook 'web-mode-hook  'my-web-mode-hook)

(setq web-mode-content-types-alist
  '(("jsx" . "\\.js[x]?\\'")))

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))

;;----------------------------------------------------------------------------
;; Typescript
;; TIDE stands for:
;;   T ypescript
;;   I nteractive
;;   D evelopment
;;   E nvironment
;; http://redgreenrepeat.com/2018/05/04/typescript-in-emacs/
;;----------------------------------------------------------------------------
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;;----------------------------------------------------------------------------
;; other programming languages
;;----------------------------------------------------------------------------
(use-package markdown-mode
             :defer t
             :config
             (progn
               (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
               (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
               (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))
             (add-hook 'markdown-mode-hook
                       (lambda ()
                         (set-fill-column 80)
                         (turn-on-auto-fill)
                         (flyspell-mode)
                         (visual-line-mode t)
                         (writegood-mode t)
                         (flyspell-mode t)))
             (setq markdown-command "pandoc --smart -f markdown -t html")
             ;(setq markdown-css-paths `(,(expand-file-name "markdown.css" vendor-dir)))
             )
(use-package yaml-mode
             :defer t
             :mode ("\\.yml$" . yaml-mode))
(use-package ansible
             :defer t
             :init (add-hook 'yaml-mode-hook '(lambda () (ansible 1))))
;;----------------------------------------------------------------------------
;; jsonnet-mode
;;----------------------------------------------------------------------------
(use-package jsonnet-mode
  :defer t
  :init)

;;----------------------------------------------------------------------------
;; auto insert
;;----------------------------------------------------------------------------
;; https://github.com/alexott/emacs-configs/blob/master/rc/emacs-rc-auto-insert.el
(load "autoinsert")
(auto-insert-mode)
(setq auto-insert t)
(setq auto-insert-query t)
(add-hook 'find-file-hooks 'auto-insert)
;(setq auto-insert-directory "~/.emacs.d/vendor/auto-insert/")
(setq auto-insert-alist
      (append
       '(
         (("\\\\.el\\\\'" . "Emacs Lisp header")
          "Short description: "
          ";;; " (file-name-nondirectory (buffer-file-name)) " --- " str "
;; Copyright (C) " (substring (current-time-string) -4) " by Creasy.L " "
;; Author: Creasy.L"
'(end-of-line 1) " <" (user-login-name) ?@ "lianghuiyuan@126.com>
(defconst "
(substring (file-name-nondirectory (buffer-file-name)) 0 -3)
"-version \\"$Id: "
(file-name-nondirectory (buffer-file-name))
",v 1.1 "
'(require 'time-stamp)
(concat (time-stamp-yyyy/mm/dd) " " (time-stamp-hh:mm:ss))
" matsu Exp matsu $\\")" "
;; Keywords: "
 '(require 'finder)
 ;;'(setq v1 (apply 'vector (mapcar 'car finder-known-keywords)))
 '(setq v1 (mapcar (lambda (x) (list (symbol-name (car x))))
                   finder-known-keywords)
        v2 (mapconcat (lambda (x) (format "%10.0s:  %s" (car x) (cdr x)))
           finder-known-keywords
           "\\n"))
 ((let ((minibuffer-help-form v2))
    (completing-read "Keyword, C-h: " v1 nil t))
    str ", ") & -2 "
;;
;; This program is free software; you can redistribute it and/or modify
(中略)
;;; Commentary:
;; " _ "
;;; Code:
;;; " (file-name-nondirectory (buffer-file-name)) " ends here"))
       auto-insert-alist))

(setq auto-insert-alist
      (append '(
                (("\\.go$" . "golang header")
                 nil
                 "//---------------------------------------------------------------------\n"
                 "// @Copyright © Wise2c Technology Co.,Ltd (http://www.wise2c.com/)\n"
                 "// @Author: Mr.L <lianghy@wise2c.com>\n"
                 "// @Date   Created: " (format-time-string "%Y-%m-%d %H:%M:%S")"\n"
                 "//----------------------------------------------------------------------\n"
                 _
                 ))
              auto-insert-alist))

(setq auto-insert-alist
      (append '(
                (("\\.py$" . "python template")
                 nil
                 "#!/usr/bin/env python\n"
                 "\n"
                 "import sys, os, math\n"
                 "# import numpy as np\n"
                 "# import scipy as sp\n"
                 "# import ROOT\n"
                 "# import pyfits as pf\n"
                 "\n"
                 _
                 )) auto-insert-alist))
(setq auto-insert-alist
      (append '(
                (("\\.sh$" . "shell script template")
                 nil
                 "#!/bin/bash\n"
                 "\n"
                 _
                 )) auto-insert-alist))

(setq auto-insert-alist
      (append '(
                (("\\.erl$" . "erlang header")
                 nil
                 "%%%-------------------------------------------------------------------\n"
                 "%%% @Copyright (c) 2016-2017 keyumall Enterprise, Inc. (http://www.keyumall.com)\n"
                 "%%% @Author: Creasy.L <creasy@keyumall.com>\n"
                 "%%% @Date   Created: " (format-time-string "%Y-%m-%d %H:%M:%S")"\n"
                 "%%%-------------------------------------------------------------------\n"
                 _
                 ))
              auto-insert-alist))

(setq auto-insert-alist
      (append '(
                (("\\.h\\'" . "C/C++ header")
                 nil
                 '(c++-mode)
                 '(setq my:skeleton-author (identity user-full-name))
                 '(setq my:skeleton-mail-address (identity user-mail-address))
                 '(setq my:skeleton-namespace (read-string "Namespace: " ""))
                 '(setq my:skeleton-description (read-string "Short Description: " ""))
                 '(setq my:skeleton-inherit (read-string "Inherits from (space separate for multiple inheritance): " ""))
                 '(setq my:skeleton-inherit-list (split-string my:skeleton-inherit " " t))
                 '(setq my:skeleton-inheritance (cond ((null my:skeleton-inherit-list)
                                                       "")
                                                      (t
                                                        (setq my:skeleton-inheritance-concat "")
                                                        (dolist (element my:skeleton-inherit-list)
                                                          (setq my:skeleton-inheritance-concat
                                                                (concat my:skeleton-inheritance-concat
                                                                        "public " element ", ")))
                                                        (setq my:skeleton-inheritance-concat
                                                              (concat " : "
                                                                      my:skeleton-inheritance-concat))
                                                        (eval (replace-regexp-in-string ", \\'" "" my:skeleton-inheritance-concat)))))
                 '(setq my:skeleton-include (cond ((null my:skeleton-inherit-list)
                                                   "")
                                                  (t
                                                    (setq my:skeleton-include "\n")
                                                    (dolist (element my:skeleton-inherit-list)
                                                      (setq my:skeleton-include
                                                            (concat my:skeleton-include
                                                                    "#include \"" element ".h\"\n")))
                                                    (eval my:skeleton-include))))
                 '(setq my:skeleton-namespace-list (split-string my:skeleton-namespace "::"))
                 '(setq my:skeleton-file-name (file-name-nondirectory (buffer-file-name)))
                 '(setq my:skeleton-class-name (file-name-sans-extension my:skeleton-file-name))
                 '(setq my:skeleton-namespace-class
                        (cond ((string= my:skeleton-namespace "")
                               my:skeleton-class-name)
                              (t
                                (concat my:skeleton-namespace "::" my:skeleton-class-name)
                                )))
                 '(setq my:skeleton-namespace-decl
                        (cond ((string= my:skeleton-namespace "")
                               ""
                               )
                              (t
                                (setq my:skeleton-namespace-decl-pre "")
                                (setq my:skeleton-namespace-decl-post "")
                                (setq my:skeleton-namespace-decl-indent "")
                                (dolist (namespace-element my:skeleton-namespace-list)
                                  (setq my:skeleton-namespace-decl-pre
                                        (concat my:skeleton-namespace-decl-pre
                                                my:skeleton-namespace-decl-indent
                                                "namespace " namespace-element " {\n"))
                                  (setq my:skeleton-namespace-decl-post
                                        (concat "\n"
                                                my:skeleton-namespace-decl-indent
                                                "}"
                                                my:skeleton-namespace-decl-post))
                                  (setq my:skeleton-namespace-decl-indent
                                        (concat my:skeleton-namespace-decl-indent "   "))
                                  )
                                (eval (concat my:skeleton-namespace-decl-pre
                                              my:skeleton-namespace-decl-indent
                                              "class " my:skeleton-class-name ";"
                                              my:skeleton-namespace-decl-post))
                                )))
                 '(random t)
                 '(setq my:skeleton-include-guard
                        (upcase
                          (format "INCLUDE_GUARD_UUID_%04x%04x_%04x_4%03x_%04x_%06x%06x"
                                  (random (expt 16 4))
                                  (random (expt 16 4))
                                  (random (expt 16 4))
                                  (random (expt 16 3))
                                  (+ (random (expt 2 14)) (expt 2 5))
                                  (random (expt 16 6))
                                  (random (expt 16 6)))))
                 "/**" n
                 "* @file   " my:skeleton-file-name > n
                 "* @brief  " my:skeleton-description > n
                 "*" > n
                 "* @date   Created       : " (format-time-string "%Y-%m-%d %H:%M:%S") > n
                 "*         Last Modified :" > n
                 "* @author " my:skeleton-author " <" my:skeleton-mail-address ">" > n
                 "*" > n
                 "*    (C) " (format-time-string "%Y") " " my:skeleton-author > n
                 "*/" > n
                 n
                 "#ifndef " my:skeleton-include-guard n
                 "#define " my:skeleton-include-guard n
                 my:skeleton-include n
                 my:skeleton-namespace-decl n
                 n
                 "class " my:skeleton-namespace-class my:skeleton-inheritance " {" n
                 "public:" > n
                 my:skeleton-class-name "();" n
                 "virtual ~" my:skeleton-class-name "();" n
                 n
                 my:skeleton-class-name "(const " my:skeleton-class-name "& rhs);" n
                 my:skeleton-class-name "& operator=(const " my:skeleton-class-name "& rhs);" n
                 n
                 "protected:" > n
                 n
                 "private:" > n
                 n
                 "ClassDef(" my:skeleton-class-name ",1) // " my:skeleton-description n
                 "};" > n
                 n
                 "#endif // " my:skeleton-include-guard n
                 '(delete-trailing-whitespace)
                 )
                )
              auto-insert-alist))

(provide 'init-languages)
;;; init-languages.el ends here
