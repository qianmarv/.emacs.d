;;; init-ivy.el --- Use ivy for minibuffer completion and more -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;-------------------------------------------
;; ivy, counsel and swiper
;; https://github.com/abo-abo/swiper
;;-------------------------------------------
(use-package ivy)

(use-package counsel
  :ensure ivy
  :config
  (progn
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    (setq ivy-count-format "(%d/%d) ")
    (global-set-key "\C-s" 'swiper)    
    (counsel-mode 1)))

(provide 'init-ivy)
;;; init-ivy.el ends here
