;; init-dashboard.el --- Customized Dashboard  -*- lexical-binding: t -*-

;;; Commentary:
;;  https://github.com/emacs-dashboard/emacs-dashboard

;;; Code:

(use-package all-the-icons
  :ensure t
  :init)

(use-package dashboard
  :ensure t
  :config
  (progn
    (setq dashboard-banner-logo-title "Stay Hungry, Stay Foolish!")
    (setq dashboard-items '((recents  . 10)
                            (bookmarks . 5)
                            ;; (registers . 5)
                            ;; (agenda . 5)
                            (projects . 5)))
    (setq initial-buffer-choice (lambda ()
                                  (toggle-scroll-bar -1)
                                  (get-buffer "*dashboard*")))
    (dashboard-setup-startup-hook)))

(provide 'init-dashboard)
