;; Enable backups by default.
(setq make-backup-files t)

;; Set per-save backup directory.
(setq backup-directory-alist `(("" . "~/.emacs.d/cache/backups")))

;; Back up versioned files too.
(setq vc-make-backup-files t)

;; Back up files on each save, keep the most 10 recent versions.
(setq version-control t     ;; Use version numbers for backups.
      kept-new-versions 10  ;; Number of newest versions to keep.
      kept-old-versions 0   ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t)  ;; Copy all files, don't rename them.

;; Location for default and per-save backups.
(setq backup-directory-alist '(("" . "~/.emacs.d/.cache/backup/save")))

;; Enable per-save backups.
(defun force-backup-of-buffer ()
  ;; Make a special backup at the first save of each session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . "~/.emacs.d/.cache/backup/session")))
          (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a per-save backup; the first backup is both per-save and per-session.
  (let ((buffer-backed-up nil))
    (backup-buffer)))
(add-hook 'before-save-hook  'force-backup-of-buffer)
