(require 'user-keybinds)
(require 'use-package)
(setq use-package-verbose t)

(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

(use-package evil
  :ensure t
  :after which-key
  :init
  (setq evil-want-keybinding nil)
  (evil-mode t)
  (user-kb/evil)
  :config
  (evil-ex-define-cmd "W" 'evil-write-all))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package magit
  :ensure t
  :init
  (user-kb/magit)
  :config)

(use-package ag
  :ensure t)

(use-package all-the-icons
  :ensure t)

(use-package treemacs
  :ensure t
  :after evil
  :config
  (setq treemacs-width 28)
  (treemacs-resize-icons 16)
  (add-hook 'treemacs-mode-hook (lambda ()
                                  (treemacs-display-current-project-exclusively)
                                  (text-scale-decrease 1))))

(use-package treemacs-all-the-icons
  :ensure t
  :after treemacs)

(use-package treemacs-projectile
  :ensure t
  :after treemacs)

(use-package treemacs-evil
  :ensure t
  :after treemacs
  :config
  (user-kb/treemacs))

(use-package doom-themes
    :ensure t
    :config
    (setq doom-themes-enable-bold nil    ; if nil, bold is universally disabled
          doom-themes-enable-italic nil) ; if nil, italics is universally disabled
    (load-theme 'doom-oceanic-next t)

    (setq doom-themes-treemacs-theme "doom-colors")
    (doom-themes-treemacs-config)

    (doom-themes-org-config))

(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-selection-wrap-around t)
  (define-key company-active-map [tab] 'company-complete))

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

(use-package helm
  :ensure t
  :bind
  (("M-x" . helm-M-x))
  :init
  (setq helm-M-x-fuzzy-match t
        helm-mode-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-locate-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-completion-in-region-fuzzy-match t
        helm-candidate-number-list 150
        helm-split-window-in-side-p t
        helm-move-to-line-cycle-in-source t
        helm-echo-input-in-header-line t
        helm-autoresize-max-height 0
        helm-autoresize-min-height 20
        helm-boring-buffer-regexp-list '())
  :config
  (helm-mode t))

(use-package helm-ag
  :ensure t)

(use-package projectile
  :ensure t
  :init
  (setq projectile-require-project-root nil)
  :config
  (projectile-global-mode))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(use-package vterm
  :ensure t)

(use-package whitespace
  :config
  (set-face-attribute 'whitespace-space nil :background nil :foreground "gray30")
  (setq whitespace-style (quote (face spaces tabs newline space-mark tab-mark)))
  (setq whitespace-line-column 320)
  (global-whitespace-mode t))

(use-package window-purpose
  :ensure t
  :config
  (setq purpose-user-mode-purposes
        '((cider-repl-mode . repl)
          (treemacs-mode . files)))
  (setq purpose-user-regexp-purposes
        '((".*repl.*" . repl)))
  (purpose-compile-user-configuration))

(use-package helm-purpose
  :ensure t
  :bind
  ("s-e" . helm-purpose-switch-buffer-with-purpose))

(use-package centaur-tabs
  :ensure t
  :config
  (setq centaur-tabs-set-bar 'over
        centaur-tabs-height 42
        centaur-tabs-set-icons t
        centaur-tabs-style "bar")
  (centaur-tabs-headline-match)
  (centaur-tabs-mode t)
  :bind
  ("s-H" . centaur-tabs-backward)
  ("s-L" . centaur-tabs-forward)
  ("s-t" . centaur-tabs--create-new-tab)
  ("s-w" . centaur-tabs--kill-this-buffer-dont-ask))

(use-package powerline
  :ensure t
  :config
  (setq powerline-default-separator 'wave
        powerline-display-bniluffer-size -1
        powerline-height 25))

(use-package spaceline
  :ensure t
  :config
  (spaceline-spacemacs-theme)
  (spaceline-toggle-minor-modes-off)
  (spaceline-helm-mode))

(use-package spaceline-all-the-icons
  :after spaceline
  (spaceline-all-the-icons-theme))

(use-package org
  :config
  (custom-set-variables '(org-agenda-files (quote ("~/Dropbox/org/diogo.beato.org"))))
  (setq org-src-preserve-indentation nil
        org-edit-src-content-indentation 0
        org-todo-keywords
        '((sequence "TODO" "DOING" "VERIFY" "|" "DONE" "DELEGATED"))))

;;#############  LANGUAGES #######################
;; -------------- CLOJURE -------------------------
(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

(use-package paredit
  :ensure t
  :bind
  (("M-{" . paredit-wrap-curly)
   ("M-<" . paredit-wrap-angled)
   ("M-[" . paredit-wrap-square))
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  ;; enable in the *scratch* buffer
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

(use-package clj-refactor
  :ensure t)

(use-package cider
  :ensure t
  :config
  (setq nrepl-log-messages t)
  (setq cider-prompt-for-symbol nil)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
  (user-kb/cider))

(use-package lsp-mode
  :ensure t
  :hook ((clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp))
  :config
  ;; add paths to your local installation of project mgmt tools, like lein
  (setenv "PATH" (concat
                   "/usr/local/bin" path-separator
                   (getenv "PATH")))
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode
               go-mode
               ))
     (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  (setq lsp-clojure-server-command '("bash" "-c" "clojure-lsp") ;; Optional: In case `clojure-lsp` is not in your PATH
        lsp-enable-indentation nil
        lsp-headerline-breadcrumb-enable nil
        lsp-enable-file-watchers t
        lsp-file-watch-threshold 50000
        lsp-lens-enable t
        ))

;; -------------- GO-LANG -------------------------
(use-package go-mode
  :ensure t
  :bind
  (("M-*" . godef-jump)
   ("M-." . pop-tag-mark))
  :preface
  (defun my-go-mode-hook ()
    (if (not (string-match "go" compile-command))
        (set (make-local-variable 'compile-command)
             "go build -v && go test -v && go vet")))
  ;; :hook ((go-mode . lsp-deferred)
  ;;        (before-save . lsp-format-buffer)
  ;;        (before-save . lsp-organize-imports))
 ;; :init
 ;; (add-hook 'before-save-hook 'gofmt-before-save)
  )

(use-package go-autocomplete
  :ensure t)

;; -------------- General Files Support -------------------------

;; Or using hooks
(use-package grip-mode
  :ensure t
  :hook ((markdown-mode org-mode) . grip-mode)
  :config
  (setq grip-preview-use-webkit t))

(use-package markdown-mode
  :ensure t
  :init (setq markdown-command "grip"))

(use-package yaml-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package docker-compose-mode
  :ensure t)
