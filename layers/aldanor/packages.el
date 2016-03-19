;; -*- coding: utf-8 -*-

(defvar aldanor-packages
  '(
    cc-mode
    company
    deft
    ein
    evil
    flycheck
    jinja2-mode
    js2-mode
    neotree
    org
    python
    recentf
    rust-mode
    speed-type
    theming
    ))

(defvar aldanor-excluded-packages '())

(defun aldanor/set-word-boundaries ()
  (modify-syntax-entry ?_ "w"))

(defun aldanor/post-init-cc-mode ()
  (add-hook 'c++-mode-hook
            (lambda () (progn
                         (aldanor/set-word-boundaries)
                         (set (make-local-variable 'compile-command)
                              (concat "g++ -std=c++11 -Wall " buffer-file-name " && ./a.out"))
                         (setq company-clang-arguments '("-std=c++11")
                               flycheck-clang-language-standard "c++11")))))

(defun aldanor/post-init-company ()
  (add-hook 'evil-normal-state-entry-hook 'company-abort))

(defun aldanor/post-init-deft ()
  (setq deft-directory "~/notes"))

(defun aldanor/post-init-ein ()
  (add-hook 'ein:notebook-multilang-mode-hook 'smartparens-mode))

(defun aldanor/post-init-evil ()
  (setq evil-move-cursor-back nil
        evil-want-visual-char-semi-exclusive t
        evil-insert-state-cursor '(bar "#6acb25"))
  (add-hook 'evil-insert-state-exit-hook
            (lambda () (if (buffer-file-name) (save-buffer))))
  (evil-define-operator evil-destroy (beg end type register yank-handler)
    "Evil operator that destroys text without yanking. "
    (evil-delete beg end type 95 yank-handler))
  (define-key evil-normal-state-map "Q" 'evil-destroy)
  (define-key evil-visual-state-map "Q" 'evil-destroy))

(defun aldanor/init-jinja2-mode ()
  (setq-default sgml-basic-offset 4)
  (add-to-list 'auto-mode-alist '("\\.tmpl\\'" . jinja2-mode)))

(defun aldanor/post-init-flycheck ()
  (setq flycheck-idle-change-delay (if flycheck-current-errors 0.25 5.0)
        flycheck-check-syntax-automatically '(mode-enabled save idle-change))
  (setq-default flycheck-flake8-maximum-line-length 99))

(defun aldanor/post-init-js2-mode ()
  (add-hook 'js2-mode-hook 'aldanor/set-word-boundaries))

(defun aldanor/post-init-neotree ()
  (setq neo-theme 'nerd
        neo-hidden-regexp-list
        '("^\\.\\(git\\|cache\\|tox\\|coverage\\)$"
          "^\\.\\(DS_Store\\|python\\-version\\)"
          "^\\(htmlcov\\|node_modules\\)$" "\\.elcs$"
          "^\\.coverage\\..*" "\\.ipynb.*$" "\\.py[cod]$"
          "~$" "^#.*#$" "^\\.#.*$" "^__pycache__$"
          "\\.egg\-info$")))

(defun aldanor/post-init-org ()
  (setq org-startup-folded nil)
  (add-hook 'org-mode-hook
            (lambda () (progn
                         (spacemacs/toggle-visual-line-navigation-on)))))

(defun aldanor/post-init-python ()
  (add-hook 'python-mode-hook
            (lambda ()
              (progn
                (setq-local indent-tabs-mode nil)
                (setq-local python-indent-offset 4)
                (set (make-local-variable 'comment-inline-offset) 2)
                (set (make-local-variable 'comment-column) 4)
                (aldanor/set-word-boundaries)))))

(defun aldanor/pre-init-recentf ()
  (spacemacs|use-package-add-hook recentf
    :post-config
    (progn
      (add-to-list 'recentf-exclude "/speed-type/"))))

(defun aldanor/post-init-rust-mode ()
  (add-hook 'rust-mode-hook 'aldanor/set-word-boundaries))

(defun aldanor/init-speed-type ()
  (use-package speed-type
    :defer t))
