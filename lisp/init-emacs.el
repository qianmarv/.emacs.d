;;; init-emacs.el --- Use better default settings for emacs and more -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;-------------------------------------------
;; Personal Information
;;-------------------------------------------
(setq user-full-name "Marvin Qian")
(setq user-mail-address "qianmarv@gmail.com")


;;-------------------------------------------
;; Default Settings
;;-------------------------------------------
(setq backup-inhibited t)

(tool-bar-mode -1)

(menu-bar-mode -1)

(toggle-scroll-bar -1)

(setq-default indent-tabs-mode nil)

(setq-default tab-width 4)

;; Default Theme
(load-theme 'solarized-dark t)

;;-------------------------------------------
;; emacs-which-key
;; https://github.com/justbur/emacs-which-key
;;-------------------------------------------
(use-package which-key
  :ensure t
  :init
  :config
  (progn
    (which-key-setup-side-window-bottom)
    (which-key-setup-minibuffer)    
    (which-key-mode)))


;;-------------------------------------------
;; winum - config with use-package
;;-------------------------------------------
(use-package winum
  :ensure t
  :init
  (progn
    (setq window-numbering-scope           'global
          window-reverse-frame-list         nil
          winum-auto-assign-0-to-minibuffer nil
          winum-auto-setup-mode-line        t
          winnum-format                    " %s"
          winum-ignored-buffers            '(" *which-key*")
          winum-keymap                     (let ((map (make-sparse-keymap)))
			                                 (define-key map (kbd "C-`") 'winum-select-window-by-number)
			                                 (define-key map (kbd "C-²") 'winum-select-window-by-number)
			                                 (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
			                                 (define-key map (kbd "M-1") 'winum-select-window-1)
			                                 (define-key map (kbd "M-2") 'winum-select-window-2)
			                                 (define-key map (kbd "M-3") 'winum-select-window-3)
			                                 (define-key map (kbd "M-4") 'winum-select-window-4)
			                                 (define-key map (kbd "M-5") 'winum-select-window-5)
			                                 (define-key map (kbd "M-6") 'winum-select-window-6)
			                                 (define-key map (kbd "M-7") 'winum-select-window-7)
			                                 (define-key map (kbd "M-8") 'winum-select-window-8)
			                                 map))
    (winum-mode)))
;; :config
;; (progn
;;   (winum-mode)      
;;   (set-face-attribute 'winum-face nil :weight 'bold))


(provide 'init-emacs)
;;; init-emacs.el ends here


