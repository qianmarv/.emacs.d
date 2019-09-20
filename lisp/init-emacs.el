;;; init-emacs.el --- Use better default settings for emacs and more -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;----------------------------------------------------------------------------
;; Personal Information
;;----------------------------------------------------------------------------
(setq user-full-name "Marvin Qian")
(setq user-mail-address "qianmarv@gmail.com")


;;----------------------------------------------------------------------------
;; Better Default Settings
;;----------------------------------------------------------------------------
(setq backup-inhibited t)

(tool-bar-mode -1)

(menu-bar-mode -1)

(toggle-scroll-bar -1)

(delete-selection-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)

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

;;----------------------------------------------------------------------------
;; emacs-which-key
;; https://github.com/justbur/emacs-which-key
;;----------------------------------------------------------------------------
(use-package which-key
  :ensure t
  :init
  :config
  (progn
    (which-key-setup-side-window-bottom)
    (which-key-setup-minibuffer)    
    (which-key-mode)))

;;----------------------------------------------------------------------------
;; Recentf
;; Reference to Purcell's init-recentf
;;----------------------------------------------------------------------------
(add-hook 'after-init-hook 'recentf-mode)
(setq-default
 recentf-max-saved-items 100
 recentf-exclude '("/tmp/" "/ssh:"))

;;----------------------------------------------------------------------------
;; Dired-Mode
;; Refer to http://book.emacs-china.org/#orgheadline9
;;----------------------------------------------------------------------------
(put 'dired-find-alternate-file 'disabled nil)
;; 主动加载 Dired Mode
;; (require 'dired)
;; (defined-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)

;; 延迟加载
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

;;----------------------------------------------------------------------------
;; Quick Jump Through Panels
;; winum - config with use-package
;;----------------------------------------------------------------------------
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
    (setq winum-auto-setup-mode-line nil)
    (winum-mode)))
;; :config
;; (progn
;;   (winum-mode)      
;;   (set-face-attribute 'winum-face nil :weight 'bold))


(provide 'init-emacs)
;;; init-emacs.el ends here
