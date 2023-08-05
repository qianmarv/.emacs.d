;; Default Theme
(load-theme 'monokai t)


;; Highligh Line
(global-hl-line-mode 1)

;;----------------------------------------------------------------------------
;; Powerline
;; TODO move to init-ui.el or somewhere else
;;----------------------------------------------------------------------------
(use-package smart-mode-line
  :ensure t)

(use-package spaceline
  :ensure t
  :config
  (spaceline-spacemacs-theme))

(provide 'init-ui)
