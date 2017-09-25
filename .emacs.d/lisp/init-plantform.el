;;; init-plantform.el
;;; ;;; Commentary:
;;; ;;; Code:


(when (system-is-mac)
  ;; Switch the Cmd and Meta keys
  (setq mac-command-key-is-meta t)
  (setq mac-command-modifier 'meta)
  (setq ns-use-native-fullscreen nil)

  ;; -----------------------------------------------------------------------------  
  ;; setting font for mac system  
  ;; -----------------------------------------------------------------------------  
  ;; Setting English Font   
  (set-face-attribute  
    'default nil :font "Monaco 12")  
  ;; Chinese Font 配制中文字体  
  (dolist (charset '(kana han symbol cjk-misc bopomofo))  
    (set-fontset-font (frame-parameter nil 'font)  
                      charset  
                      (font-spec :family "Kaiti SC" :size 14)))  

  ;; Note: you can chang "Kaiti SC" to "Microsoft YaHei" or other fonts  
  ;; On OSX, I use the pbpaste and pbcopy methods to interact with the system clipboard.
  ;; brew install coreutils
  (if (executable-find "gls")
    (progn
      (setq insert-directory-program "gls")
      (setq dired-listing-switches "-lFaGh1v --group-directories-first"))
    (setq dired-listing-switches "-ahlF"))

  (defun copy-from-osx ()
    "Handle copy/paste intelligently on osx."
    (let ((pbpaste (purecopy "/usr/bin/pbpaste")))
      (if (and (eq system-type 'darwin)
               (file-exists-p pbpaste))
        (let ((tramp-mode nil)
              (default-directory "~"))
          (shell-command-to-string pbpaste)))))

  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "/usr/bin/pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (setq interprogram-cut-function 'paste-to-osx
        interprogram-paste-function 'copy-from-osx)


  ;; Trash.
  (defun move-file-to-trash (file)
    "Use `trash' to move FILE to the system trash.
    When using Homebrew, install it using \"brew install trash\"."
    (call-process (executable-find "trash")
                  nil 0 nil
                  file))
    (setq trash-directory "~/.Trash/emacs")
    (setq delete-by-moving-to-trash t)
    (defun system-move-file-to-trash (file)
      "Use \"trash\" to move FILE to the system trash.
      When using Homebrew, install it using \"brew install trash\"."
      (call-process (executable-find "trash")
                    nil 0 nil
                    file))
      )


(when (system-is-linux)
  (defun yank-to-x-clipboard ()
    (interactive)
    (if (region-active-p)
      (progn
        (shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
        (message "Yanked region to clipboard!")
        (deactivate-mark))
      (message "No region active; can't yank to clipboard!")))
  )


(provide 'init-plantform)