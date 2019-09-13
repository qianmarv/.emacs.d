;;; init-edit.el --- Use various mode to enhance edit and more -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Rebind comment
(global-set-key (kbd "C-c C-l") 'comment-line)

;;-------------------------------------------
;; smartparens - config with use-package
;;-------------------------------------------
(use-package smartparens
  :ensure t
  :init
  (progn
    (setq sp-show-pair-deplay 0.2
          sp-show-pair-from-inside t
          sp-cancel-autoskip-on-backward-movement nil
          sp-highlight-pair-overlay nil
          sp-highlight-wrap-overlay nil
          sp-highlight-wrap-tag-verlay nil)
    
    (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode))
  :config
  (progn
    (require 'smartparens-config)
    (show-smartparens-global-mode t)))

;;-------------------------------------------
;; ace-jump-mode - config with use-package
;;-------------------------------------------
(use-package ace-jump-mode
  :ensure t
  :commands ace-jump-mode
  :init
  (bind-key "C-." 'ace-jump-mode))

(use-package undo-tree
  :ensure t
  :defer t
  :init
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t))


(provide 'init-edit)
;;; init-edit.el ends here
