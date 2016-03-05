;; Set default line width to 99.
(setq-default fill-column 99)

;; Require a trailing newline in all files.
(setq require-final-newline t)

;; Double space at the end of sentences.
(setq-default sentence-end-double-space t)

;; Increase line spacing.
(setq-default line-spacing 0.1)

;; Save when frame loses focus.
(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))
