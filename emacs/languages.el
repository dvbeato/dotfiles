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
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

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
