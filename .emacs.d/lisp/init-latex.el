;;; 20-tex --- LaTeX
;;; Commentary:
;;; Code:

;; Basic settings
(use-package tex-site
             :ensure auctex
             :mode ("\\.tex\\'" . latex-mode)
             :config
             (setq TeX-auto-save t)
             (setq TeX-parse-self t)
             (setq-default TeX-master nil)
             (add-hook 'LaTeX-mode-hook
                       (lambda ()
                         (rainbow-delimiters-mode)
                         (company-mode)
                         (smartparens-mode)
                         (turn-on-reftex)
                         (setq reftex-plug-into-AUCTeX t)
                         (reftex-isearch-minor-mode)
                         (setq TeX-PDF-mode t)
                         (setq TeX-source-correlate-method 'synctex)
                         (setq TeX-source-correlate-start-server t)))

             ;; Update PDF buffers after successful LaTeX runs
             (add-hook 'TeX-after-TeX-LaTeX-command-finished-hook
                       #'TeX-revert-document-buffer)

             ;; to use pdfview with auctex
             (add-hook 'LaTeX-mode-hook 'pdf-tools-install)

             ;; to use pdfview with auctex
             (setq TeX-view-program-selection '((output-pdf "pdf-tools"))
                   TeX-source-correlate-start-server t)
             (setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view"))))

(use-package reftex
             :ensure t
             :defer t
             :config
             (setq reftex-cite-prompt-optional-args t)); Prompt for empty optional arguments in cite

(use-package pdf-tools
             :ensure t
             :mode ("\\.pdf\\'" . pdf-tools-install)
             :bind ("C-c C-g" . pdf-sync-forward-search)
             :defer t
             :config
             (setq mouse-wheel-follow-mouse t)
             (setq pdf-view-resize-factor 1.10))

(provide 'init-latex)