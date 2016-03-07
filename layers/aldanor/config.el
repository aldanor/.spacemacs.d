; -*- coding: utf-8 -*-

(setq-default fill-column 99
              line-spacing 0.1
              sentence-end-double-space t)

(setq frame-title-format
      '("emacs%@" (:eval (system-name)) ": "
        (:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b"))
        " [%*]"))

(setq c-default-style "bsd"
      c-basic-offset 4)
(c-set-offset 'member-init-intro 0)
(c-set-offset 'innamespace 0)

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(set-locale-environment "en.UTF-8")
(prefer-coding-system 'utf-8)

(add-hook 'prog-mode-hook
          (lambda () (setq-local require-final-newline t)))

(add-hook 'focus-out-hook
          (lambda () (save-some-buffers t)))

(ignore-errors
  (require 'ansi-color)
  (add-hook 'compilation-filter-hook
            (lambda ()
              (when (eq major-mode 'compilation-mode)
                (ansi-color-apply-on-region compilation-filter-start (point-max))))))

(setq make-backup-files t
      backup-directory-alist `(("" . "~/.emacs.d/cache/backups/save"))
      vc-make-backup-files t
      version-control t
      kept-new-versions 10
      kept-old-versions 0
      delete-old-versions t
      backup-by-copying t)

(defun force-backup-of-buffer ()
  (when (not buffer-backed-up)
    (let ((backup-directory-alist '(("" . "~/.emacs.d/.cache/backup/session")))
          (kept-new-versions 3))
      (backup-buffer)))
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(add-hook 'before-save-hook 'force-backup-of-buffer)
