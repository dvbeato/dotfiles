(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-auto-revert-mode 1)
(global-hl-line-mode t)
(global-linum-mode t)
(setq column-number-mode t)
(setq size-indication-mode t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq visible-bell t)
(setq ring-bell-function 'ignore)
(setq show-paren-mode t)
(set-default 'truncate-lines t)
(defalias 'yes-or-no-p 'y-or-n-p)
(savehist-mode)

;; No splash screen
(setq inhibit-startup-message t)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
;; (add-to-list 'default-frame-alist '(fullscreen . fullheight))
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;; auto-save and auto-backup
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Reload init file
(defun reload-init-file ()
  (interactive)
  (load-file user-init-file))

(global-set-key (kbd "C-c C-l") 'reload-init-file)

;; encoding settings
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(load-theme 'wombat)


;; PACKAGES
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t))
(package-initialize)

;; update the package metadata is the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

(use-package magit
  :ensure t)

(use-package ag
  :ensure t)

(use-package helm-ag
  :ensure t
  :bind
  (("M-#" . helm-ag-project-root)))

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

(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

(use-package all-the-icons
  :ensure t)

(use-package neotree
  :ensure t
  :bind
  (("M-1" . neotree-toggle))
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(use-package evil
  :ensure t
  :init (evil-mode t)
  :config
;;  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
;;  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
  (evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
  (evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
  (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle))

(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-selection-wrap-around t)
  (define-key company-active-map [tab] 'company-complete))

(use-package markdown-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package docker-compose-mode
  :ensure t)

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

(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(use-package cider
  :ensure t
  :bind
  (("M-b" . cider-find-var)
   ("M-s-<left>" . cider-pop-back))
  :config
  (setq nrepl-log-messages t)
  (setq cider-prompt-for-symbol nil)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

(use-package exec-path-from-shell
  :ensure t)

(use-package whitespace
  :config
  (setq whitespace-style (quote (face spaces tabs newline space-mark tab-mark newline-mark )))
  (setq whitespace-line-column 320))

(use-package powerline
  :ensure t
  :config
  (setq powerline-default-separator 'wave
        powerline-display-buffer-size -1
        powerline-height 20))

(use-package diminish
  :ensure t)

(use-package spaceline
  :ensure t
  :config
  (spaceline-spacemacs-theme)
  (spaceline-helm-mode))


(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (set-face-attribute 'default nil :family "Monaco" :height 140)

  ;; Fancy titlebar for MacOS
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (setq ns-use-proxy-icon  nil)
  (setq frame-title-format nil)

  (use-package doom-themes
    :ensure t
    :config
    (load-theme 'doom-city-lights t)
    (doom-themes-org-config)
    (doom-themes-neotree-config))
  ;; (use-package kaolin-themes
  ;;   :ensure t
  ;;   :config
  ;;   (load-theme 'kaolin-bubblegum t)
  ;;   (kaolin-treemacs-theme))
  )

(when (eq system-type 'gnu/linux)
  ;; do linux settings
  
  (use-package kaolin-themes
    :ensure t
    :config
    (load-theme 'kaolin-aurora t)
    (kaolin-treemacs-theme))
  ;; (use-package gruvbox-theme
  ;;   :ensure t
  ;;   :config
  ;;   (load-theme 'gruvbox-dark-medium t))
  )

(use-package org
  :config
  (custom-set-variables '(org-agenda-files (quote ("~/Dropbox/org/diogo.beato.org"))))
  (setq org-src-preserve-indentation nil
        org-edit-src-content-indentation 0))

;; whitespace settings
(global-whitespace-mode t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("2a9039b093df61e4517302f40ebaf2d3e95215cb2f9684c8c1a446659ee226b9" "e2fd81495089dc09d14a88f29dfdff7645f213e2c03650ac2dd275de52a513de" "d890583c83cb36550c2afb38b891e41992da3b55fecd92e0bb458fb047d65fb3" "75d3dde259ce79660bac8e9e237b55674b910b470f313cdf4b019230d01a982a" "2757944f20f5f3a2961f33220f7328acc94c88ef6964ad4a565edc5034972a53" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "9399db70f2d5af9c6e82d4f5879b2354b28bc7b5e00cc8c9d568e5db598255c4" "6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "100e7c5956d7bb3fd0eebff57fde6de8f3b9fafa056a2519f169f85199cc1c96" default)))
 '(org-agenda-files (quote ("~/Dropbox/org/diogo.beato.org")))
 '(package-selected-packages
   (quote
    (spaceline gruvbox-theme telephone-line doom-themes use-package rainbow-delimiters powerline-evil paredit neotree markdown-mode magit kaolin-themes helm-projectile helm-ag exec-path-from-shell docker-compose-mode company cider all-the-icons ag)))
 '(whitespace-display-mappings
   (quote
    ((space-mark 32
                 [46]
                 [46])
     (space-mark 160
                 [164]
                 [95])
     (tab-mark 9
               [187 9]
               [92 9])))))
(set-face-attribute 'whitespace-space nil :background nil :foreground "gray20")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(let ((nufile "~/.emacs.d/nu.el"))
  (when (file-exists-p nufile)
    (load nufile)))
