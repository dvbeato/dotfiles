(global-auto-revert-mode 1)
(global-hl-line-mode t)
;;(global-linum-mode t)
;;(global-whitespace-mode t)
(setq column-number-mode t)
(setq size-indication-mode t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq visible-bell t)
(setq ring-bell-function 'ignore)
(setq show-paren-mode t)
(setq line-spacing 4)
(set-default 'truncate-lines t)
(defalias 'yes-or-no-p 'y-or-n-p)
(savehist-mode)
(global-set-key (kbd "<escape><escape>") 'keyboard-escape-quit)

(setq tab-bar-mode t)
(setq tab-bar-show 1)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)


