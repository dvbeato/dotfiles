;(setq mac-command-modifier 'meta)
;(setq mac-option-modifier 'super)
(set-face-attribute 'default nil :family "Monaco" :height 140)

;; Fancy titlebar for MacOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

(use-package doom-themes
    :ensure t
    :config
    (load-theme 'doom-tomorrow-night t)
    (setq doom-themes-treemacs-theme "doom-colors")
    (doom-themes-treemacs-config)
    (doom-themes-org-config))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))
