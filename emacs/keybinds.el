(provide 'user-keybinds)

(defun user-kb/evil ()
  (evil-set-leader 'normal (kbd "SPC"))

  (evil-define-key nil 'global
    (kbd "<leader><SPC>") 'helm-M-x
    (kbd "<leader>ps") 'helm-projectile-switch-project  
    (kbd "<leader>po") 'helm-projectile-find-file 
    (kbd "<leader>pf") 'helm-projectile-rg 
    
    (kbd "<leader>bl") 'helm-mini 
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
    (kbd "C-S-<right>") 'evil-window-increase-width)
  )

(defun user-kb/magit ()
  (evil-define-key 'normal 'magit-mode-map
    (kbd "<leader>gs") 'magit-status
    (kbd "<leader>gc") 'magit-branch-checkout)
  )

(defun user-kb/treemacs ()
  (evil-set-leader 'treemacs (kbd "SPC"))
  (evil-define-key 'normal 'global (kbd "<leader>1") 'treemacs-add-and-display-current-project)
  (evil-define-key 'treemacs treemacs-mode-map
    (kbd "C-k") 'evil-window-up 
    (kbd "C-j") 'evil-window-down 
    (kbd "C-h") 'evil-window-left 
    (kbd "C-l") 'evil-window-right
    (kbd "<leader>1") 'treemacs
    (kbd "<leader>nd") 'treemacs-create-dir
    (kbd "<leader>nf") 'treemacs-create-file)
  )

(defun user-kb/cider ()
  (evil-define-key 'normal 'clojure-mode-map
    (kbd "gb")          'cider-pop-back
    (kbd "fr")          'cider-find-resource
    (kbd "<leader>cj")  'cider-jack-in
    (kbd "<leader>ceb") 'cider-eval-buffer
    (kbd "<leader>ces") 'cider-eval-last-sexp
    (kbd "<leader>ctn") 'cider-test-run-ns-tests
    (kbd "<leader>ctr") 'cider-test-run-test))
