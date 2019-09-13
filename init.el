;;-------------------------------------------
;; Package Related Settings
;;-------------------------------------------
(setq package-check-signature nil)

(require 'package)
(setq package-archives
      '(("melpa-cn" . "https://elpa.emacs-china.org/melpa/")
		("org-cn"   . "https://elpa.emacs-china.org/org/")
		("gnu-cn"   . "https://elpa.emacs-china.org/gnu/")))

;; Alternative: Don't Delete Below - Used for Non-China Mirrors
;;(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
;;                         ("marmalade" . "https://marmalade-repo.org/packages/")
;;                         ("melpa" . "https://melpa.org/packages/")))

(setq package-enable-at-startup nil)

(package-initialize)

;; Customizing config
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Init Required Packages
(defvar base-packages '(
			          solarized-theme
 					  use-package ))

(defun my/install-missing-packages(package-list)
  (dolist (p package-list)
 	(unless (package-installed-p p)
 	  (package-refresh-contents)
      (package-install p))
 	(add-to-list 'package-selected-packages p)))

(my/install-missing-packages base-packages)


;;-------------------------------------------
;; Personal Information
;;-------------------------------------------
(setq user-full-name "Marvin Qian")
(setq user-mail-address "qianmarv@gmail.com")


;; Default theme
(load-theme 'solarized-dark t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)

;; Garbage Collection
(setq gc-cons-threshold (* 64 1024 1024))
(setq gc-cons-percentag 0.5)

;; Rebind comment
(global-set-key (kbd "C-c C-l") 'comment-line)

;; disable emacs backup file
;; Or (setq make-backup-files nil)

(setq backup-inhibited t)

(setq custom-file (concat user-emacs-directory "custom.el"))


;;-------------------------------------------
;; smartparens - config with use-package
;;-------------------------------------------
(use-package smartparens-config
  :ensure smartparens
  :config
  (progn
    (show-smartparens-global-mode t)
    (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)))

(use-package projectile
  :ensure t
  :config
  (progn
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)    
    (projectile-mode 1)))
;;-------------------------------------------
;; ace-jump-mode - config with use-package
;;-------------------------------------------
(use-package ace-jump-mode
  :ensure t
  :commands ace-jump-mode
  :init
  (bind-key "C-." 'ace-jump-mode))

;; (use-package better-defaults
;;   :ensure t
;;   :init)

;; (use-package ivy
;;   :ensure t
;;   :config
;;   (progn
;;     (ivy-mode t)))

(use-package counsel
  :ensure ivy
  :config
  (progn
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    (setq ivy-count-format "(%d/%d) ")
    (global-set-key "\C-s" 'swiper)    
    (counsel-mode 1)))

;; (use-package swiper
;;   :ensure ivy
;;   :config
;;   (progn
;;     ))

(use-package counsel-projectile
  :ensure t
  :config
  (progn
    (counsel-projectile-mode)))

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


(when (file-exists-p custom-file)
  (load custom-file))
