;; -*- coding: utf-8 -*-

(defvar aldanor-packages
  '(
    cc-mode
    company
    deft
    evil
    flycheck
    jinja2-mode
    js2-mode
    mmm-mode
    neotree
    org
    python
    recentf
    speed-type
    ))

(defvar aldanor-excluded-packages '())

(defun set-word-boundaries ()
  (modify-syntax-entry ?_ "w"))

(defun aldanor/post-init-cc-mode ()
  (add-hook 'c++-mode-hook
            (lambda () (progn
                         (set-word-boundaries)
                         (set (make-local-variable 'compile-command)
                              (concat "g++ -std=c++11 -Wall " buffer-file-name " && ./a.out"))
                         (setq company-clang-arguments '("-std=c++11")
                               flycheck-clang-language-standard "c++11")))))

(defun aldanor/post-init-company ()
  (add-hook 'evil-normal-state-entry-hook 'company-abort))

(defun aldanor/post-init-deft ()
  (setq deft-directory "~/notes"))

(defun aldanor/post-init-evil ()
  (setq evil-move-cursor-back nil
        evil-want-visual-char-semi-exclusive t
        evil-insert-state-cursor '(bar "#6acb25")))

(defun aldanor/init-jinja2-mode ()
  (setq-default sgml-basic-offset 4)
  (add-to-list 'auto-mode-alist '("\\.tmpl\\'" . jinja2-mode)))

(defun aldanor/post-init-flycheck ()
  (setq flycheck-idle-change-delay (if flycheck-current-errors 0.25 5.0)
        flycheck-check-syntax-automatically '(mode-enabled save idle-change))
  (setq-default flycheck-flake8-maximum-line-length 99))

(defun aldanor/post-init-js2-mode ()
  (add-hook 'js2-mode-hook 'set-word-boundaries))

(defun aldanor/pre-init-mmm-mode ()
  (message-box "pre-init")
  (use-package mmm-mode
   :defer t
    ; :commands
    ; mmm-parse-buffer
    ; :post-init
    ; (spacemacs/set-leader-keys-for-major-mode 'jinja2-c++-mode "cs" 'mmm-parse-buffer)
    ; :post-init
    ; (message-box ":post-init")
    :config
    (progn
      (require 'mmm-auto)
      (require 'mmm-compat)
      (require 'mmm-vars)
      (message-box ":post-config")
      (mmm-add-group
       'jinja2
       `((jinja2-variable
          :submode jinja2-mode :face mmm-code-submode-face :front "{{" :back "}}"
          :insert ((?{ jinja2-{{-}} nil @ "{{" @ " " _ " " @ "}}" @)))
         (jinja2-comment
          :submode jinja2-mode :face mmm-comment-submode-face :front "{#" :back "#}"
          :insert ((?# jinja2-comment nil @ "{#" @ " " _ " " @ "#}" @)))
         (jinja2-block
          :submode jinja2-mode :face mmm-code-submode-face :front "{%" :back "%}"
          :insert ((?% jinja2-{%-%} nil @ "{%" @ " " _ " " @ "%}" @)))))
      (add-hook 'mmm-jinja2-class-hook
                (lambda () (setq mmm-buffer-mode-display-name "Jinja2")))
      (define-derived-mode jinja2-c++-mode c++-mode "Jinja2/C++"
        (setq tab-width 4
              indent-tabs-mode nil
              mmm-global-mode 'maybe)
        (mmm-add-mode-ext-class 'jinja2-c++-mode "\\.tmpl\\.cpp\\'" 'jinja2))
      (add-to-list 'auto-mode-alist '("\\.tmpl\\.cpp\\'" . jinja2-c++-mode)))))

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
            (lambda () (progn
                         (set (make-local-variable 'comment-inline-offset) 2)
                         (set (make-local-variable 'comment-column) 4)
                         (setq-local indent-tabs-mode nil
                                     python-indent-offset 4)
                         (set-word-boundaries)))))

(defun aldanor/pre-init-recentf ()
  (spacemacs|use-package-add-hook recentf
    :post-config
    (progn
      (add-to-list 'recentf-exclude "/speed-type/"))))

(defun aldanor/init-speed-type () ())
