;;; init-ivy.el --- Use projectile for better project management -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package projectile
  :ensure t
  :config
  (progn
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)    
    (projectile-mode 1)))


(use-package counsel-projectile
  :ensure t
  :config
  (progn
    (counsel-projectile-mode)))

(provide 'init-projectile)
;;; init-projectile.el ends here
