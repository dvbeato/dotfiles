;(setq mac-command-modifier 'meta)
;(setq mac-option-modifier 'super)
(set-face-attribute 'default nil :family "Monaco" :height 150)

;; Fancy titlebar for MacOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)


