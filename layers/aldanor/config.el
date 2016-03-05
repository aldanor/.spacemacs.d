(setq-default fill-column 99
              line-spacing 0.1
              sentence-end-double-space t)

(add-hook 'prog-mode-hook
          (lambda () (setq-local require-final-newline t)))

(add-hook 'focus-out-hook
          (lambda () (save-some-buffers t)))

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
