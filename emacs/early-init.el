;; ――――――――――――――――――――――― package setup ―――――――――――――――――――――――――――
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

(defun blessed/load-if-exists (file-path)
 (let ((file file-path))
  (when (file-exists-p file)
    (load file))))

;; ――――――――――――――――――――――― OS setup ―――――――――――――――――――――――――――
(when (eq system-type 'darwin)
  (blessed/load-if-exists "~/.emacs.d/osx.el"))

(when (eq system-type 'gnu/linux)
  (blessed/load-if-exists "~/.emacs.d/linux.el"))

;; ――――――――――――――――――――――― early UI setup ―――――――――――――――――――――――――――
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . fullheight))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; No splash screen
(setq inhibit-startup-message t)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
;; (add-to-list 'default-frame-alist '(fullscreen . fullheight))
