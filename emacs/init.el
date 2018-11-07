(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (set-face-attribute 'default nil :family "Monaco" :height 160))

(when (eq system-type 'gnu/linux)
  ;; do linux settings
  )

;; Interface settings
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(global-auto-revert-mode 1)
(global-hl-line-mode t)
(global-linum-mode t)
(setq column-number-mode t)
(setq size-indication-mode t)
(setq indent-tabs-mode nil)
(setq tab-width 2)
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

;;(use-package flatland-theme
;;  :ensure t
;;  :config (load-theme 'flatland t))

;;(use-package dracula-theme
;;  :init t)

(use-package kaolin-themes
  :ensure t
  :config
  (load-theme 'kaolin-bubblegum t)
  (kaolin-treemacs-theme))

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
  :config
  (helm-mode t))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t))

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
  :ensure t)

(use-package powerline-evil
  :ensure t
  :config
  (powerline-evil-vim-color-theme))

;; whitespace settings
(global-whitespace-mode t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("e893b3d424a9b8b19fb8ab8612158c5b12b9071ea09bade71ba60f43c69355e6" "f4260b30a578a781b4c0858a4a0a6071778aaf69aed4ce2872346cbb28693c1a" default)))
 '(package-selected-packages
   (quote
    (powerline-evil powerline kaolin-themes use-package rainbow-delimiters paredit neotree markdown-mode magit helm-projectile helm-ag exec-path-from-shell evil company cider all-the-icons ag)))
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
