(defvar aldanor-packages
  '(
    company
    evil
    flycheck
    js2-mode
    neotree
    python
    ))

(defvar aldanor-excluded-packages '())

(when (configuration-layer/layer-usedp 'company)
  (defun aldanor/post-init-company ()
    (add-hook 'evil-normal-state-entry-hook 'company-abort)))

(defun aldanor/post-init-evil ()
  (setq evil-move-cursor-back nil
        evil-want-visual-char-semi-exclusive t
        evil-insert-state-cursor '(bar "#6acb25")))

(defun aldanor/post-init-flycheck ()
  (setq flycheck-idle-change-delay (if flycheck-current-errors 0.25 5.0)
        flycheck-check-syntax-automatically '(mode-enabled save idle-change)))

(defun aldanor/post-init-js2-mode ()
  (add-hook 'js2-mode-hook
            (lambda () (progn
                         (modify-syntax-entry ?_ "w")))))

(defun aldanor/post-init-neotree ()
  (setq neo-theme 'nerd
        neo-hidden-regexp-list '("^\\.\\(git\\|cache\\|tox\\|coverage\\)$"
                                 "^\\.\\(DS_Store\\|python\\-version\\)"
                                 "^\\(htmlcov\\|node_modules\\)$" "\\.elcs$"
                                 "^\\.coverage\\..*" "\\.ipynb.*$" "\\.py[cod]$"
                                 "~$" "^#.*#$" "^\\.#.*$" "^__pycache__$"
                                 "\\.egg\-info$"))

(defun aldanor/post-init-python ()
  (add-hook 'python-mode-hook
            (lambda () (progn
                         (setq-local comment-inline-offset 2
                                     comment-column 4
                                     indent-tabs-mode nil
                                     python-indent-offset 4)
                         (modify-syntax-entry ?_ "w")))))
