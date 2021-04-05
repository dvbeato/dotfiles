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
  :config

  (evil-ex-define-cmd "W" 'evil-write-all)
  (evil-set-leader 'normal (kbd "SPC"))

  (evil-define-key nil 'global
    (kbd "<leader><SPC>") 'helm-M-x
    (kbd "<leader>ps") 'helm-projectile-switch-project  
    (kbd "<leader>po") 'helm-projectile-find-file 
    (kbd "<leader>pf") 'helm-projectile-rg 
    
    (kbd "<leader>bl") 'helm-buffers-list 
    (kbd "<leader>be") 'eval-buffer)

  (which-key-add-key-based-replacements "<leader>p" "projects") 
  (which-key-add-key-based-replacements "<leader>ps" "switch project")
  (which-key-add-key-based-replacements "<leader>po" "open project file")
  (which-key-add-key-based-replacements "<leader>pf" "find in project")

  (which-key-add-key-based-replacements "<leader>b" "buffers")
  (which-key-add-key-based-replacements "<leader>bl" "buffer list")
  (which-key-add-key-based-replacements "<leader>be" "eval buffer")

  (evil-define-key 'normal 'global
    (kbd "C-k") 'evil-window-up 
    (kbd "C-j") 'evil-window-down 
    (kbd "C-h") 'evil-window-left 
    (kbd "C-l") 'evil-window-right 
    (kbd "C-S-k") 'evil-window-move-very-top 
    (kbd "C-S-j") 'evil-window-move-very-bottom 
    (kbd "C-S-h") 'evil-window-move-far-left 
    (kbd "C-S-l") 'evil-window-move-far-right 
    (kbd "C-S-<up>") 'evil-window-increase-height 
    (kbd "C-S-<down>") 'evil-window-decrease-height 
    (kbd "C-S-<left>") 'evil-window-decrease-width 
    (kbd "C-S-<right>") 'evil-window-increase-width))

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
    (kbd "<leader>gc") 'magit-branch-checkout))

(use-package ag
  :ensure t)

(use-package all-the-icons
  :ensure t)

(use-package treemacs
  :ensure t
  :config
  (evil-define-key 'normal 'magit-mode-map
    (kbd "<leader>1") 'treemacs)
  (setq treemacs-width 28)
  (treemacs-resize-icons 16)
  (add-hook 'treemacs-mode-hook (lambda () (text-scale-decrease 1))))

(use-package treemacs-all-the-icons
  :ensure t
  :after treemacs)

(use-package treemacs-projectile
  :ensure t
  :after treemacs)

(use-package doom-themes
    :ensure t
    :config
    (setq doom-themes-enable-bold nil    ; if nil, bold is universally disabled
          doom-themes-enable-italic nil) ; if nil, italics is universally disabled
    (load-theme 'doom-nord-light t)

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
        helm-autoresize-min-height 20)
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
  (set-face-attribute 'whitespace-space nil :background nil :foreground "gray")
  (setq whitespace-style (quote (face spaces tabs newline space-mark tab-mark)))
  (setq whitespace-line-column 320)
  (global-whitespace-mode t))

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
