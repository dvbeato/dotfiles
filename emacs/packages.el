(require 'use-package)
(setq use-package-verbose t)

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (evil-mode t)
  :config
  (evil-ex-define-cmd "W" 'evil-write-all)
 (evil-set-leader 'normal (kbd "SPC"))
 (evil-define-key 'normal 'global
   (kbd "<leader>ps") 'helm-projectile-switch-project
   (kbd "<leader>po") 'helm-projectile-find-file
   (kbd "<leader>bl") 'helm-buffers-list
   ))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package magit
  :ensure t
  :config
  (evil-define-key 'normal 'magit-mode-map
    (kbd "<leader>gs") 'magit-status
   ))

(use-package ag
  :ensure t)

(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

(use-package all-the-icons
  :ensure t)

(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-selection-wrap-around t)
  (define-key company-active-map [tab] 'company-complete))

(use-package helm
  :ensure t
  :bind
  (("M-x" . helm-M-x)
   ("M-e" . helm-buffers-list))
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
        helm-autoresize-min-height 20)
  :config
  (helm-mode t))

(use-package helm-ag
  :ensure t
  :bind
  (("M-#" . helm-ag-project-root)))

(use-package projectile
  :ensure t
  :init
  (setq projectile-require-project-root nil)
  :config
  (projectile-global-mode))

(use-package helm-projectile
  :ensure t
  :bind
  (("M-O" . helm-projectile-find-file)
   ("M-P" . helm-projectile-switch-project))
  :config
  (helm-projectile-on))

(use-package auto-complete
  :ensure t)

(use-package whitespace
  :config
  (set-face-attribute 'whitespace-space nil :background nil :foreground "gray25")
  (setq whitespace-style (quote (face spaces tabs newline space-mark tab-mark)))
  (setq whitespace-line-column 320)
  (global-whitespace-mode t))

(use-package powerline
  :ensure t
  :config
  (setq powerline-default-separator 'wave
        powerline-display-buffer-size -1
        powerline-height 25))

(use-package diminish
  :ensure t)

(use-package spaceline
  :ensure t
  :config
  (spaceline-spacemacs-theme)
  (spaceline-helm-mode))

(use-package org
  :config
  (custom-set-variables '(org-agenda-files (quote ("~/Dropbox/org/diogo.beato.org"))))
  (setq org-src-preserve-indentation nil
        org-edit-src-content-indentation 0
        org-todo-keywords
        '((sequence "TODO" "DOING" "VERIFY" "|" "DONE" "DELEGATED"))))
