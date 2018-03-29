;;; init-maps.el -- Provide global key maps

;;; Commentary:
;;; Provide global maps that aren't specific to any mode or package.

;;; Code:
(define-key global-map (kbd "C-x C-q") 'kill-emacs)
(define-key global-map (kbd "C-c u")   'insert-char) ;; "u" for Unicode, get it?
(define-key global-map (kbd "C-c s")   (lambda () (interactive) (ansi-term "zsh")))
(define-key global-map (kbd "s-e")     'eval-buffer)
(define-key global-map (kbd "C-}")     'jcf-split-window)

;(define-key global-map (kbd "C-x 2")   'x-split-window-below)
;(define-key global-map (kbd "C-x 3")   'x-split-window-right)
(global-set-key (kbd "C-x 2") 'vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'hsplit-last-buffer)
(global-set-key [M-left] 'shrink-window-horizontally) ;C+← 横のWindowを狭める
(global-set-key [M-right] 'enlarge-window-horizontally) ;C+→ 横のWindowを広げる
(global-set-key [M-up] 'shrink-window) ;C+← 横のWindowを狭める
(global-set-key [M-down] 'enlarge-window) ;C+→ 横のWindowを広げる

(evil-define-key 'insert global-map (kbd "C-v") 'yank)

(global-set-key (kbd "s-k") 'windmove-up)
(global-set-key (kbd "s-j") 'windmove-down)
(global-set-key (kbd "s-h") 'windmove-left)
(global-set-key (kbd "M-.") 'godef-jump)
(global-set-key (kbd "M-,") 'pop-tag-mark)

(global-set-key (kbd "M-]") 'dumb-jump-go)
(global-set-key (kbd "M-t") 'dumb-jump-back)

(global-set-key (kbd "M-p") 'scroll-other-window-down)
(global-set-key (kbd "M-n") 'scroll-other-window)

(provide 'init-maps)
;;; init-maps.el ends here
