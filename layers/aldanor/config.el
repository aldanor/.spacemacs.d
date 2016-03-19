;; -*- coding: utf-8 -*-

;; Spacemacs-specific configuration.
(setq spacemacs-theme-comment-bg nil
      spacemacs-theme-org-height nil
      theming-modifications
      '((ample
         (font-lock-string-face :foreground "#057f40") ; ample/dark-green
         (js2-function-param :foreground "#baba36")    ; ample/yellow
         (js2-object-property :foreground "#df9522")   ; ample/orange
         )))

;; General settings: line length, line spacing, etc.
(setq-default fill-column 99
              line-spacing 0.1
              sentence-end-double-space t)

;; Set default frame size and position.
(setq default-frame-alist '((top . 0) (left . 0) (width . 110) (height . 64)))

;; Customize frame title format.
(setq frame-title-format
      '("emacs%@" (:eval (system-name)) ": "
        (:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b"))
        " [%*]"))

;; Set default style for C/C++.
(setq c-default-style "bsd"
      c-basic-offset 4)
(c-set-offset 'member-init-intro 0)
(c-set-offset 'innamespace 0)

;; Force default locale to en.UTF-8.
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(set-locale-environment "en.UTF-8")
(prefer-coding-system 'utf-8)

;; Require final newline in all prog-mode buffers.
(add-hook 'prog-mode-hook
          (lambda () (setq-local require-final-newline t)))

;; Automatically save buffers when frame loses focus.
(add-hook 'focus-out-hook
          (lambda () (save-some-buffers t)))

;; Colorize compilation window output.
(ignore-errors
  (require 'ansi-color)
  (add-hook 'compilation-filter-hook
            (lambda ()
              (when (eq major-mode 'compilation-mode)
                (ansi-color-apply-on-region compilation-filter-start (point-max))))))

;; Enable automatic backups, always backup by copying.
(setq make-backup-files t
      backup-directory-alist `(("" . "~/.emacs.d/cache/backups/save"))
      vc-make-backup-files t
      version-control t
      kept-new-versions 10
      kept-old-versions 0
      delete-old-versions t
      backup-by-copying t)

;; Force a backup on each save.
(add-hook 'before-save-hook
          (lambda ()
            (progn
              (when (not buffer-backed-up)
                (let ((backup-directory-alist '(("" . "~/.emacs.d/.cache/backup/session")))
                      (kept-new-versions 3))
                  (backup-buffer)))
              (let ((buffer-backed-up nil))
                (backup-buffer)))))
