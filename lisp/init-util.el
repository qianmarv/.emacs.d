;;; init-util.el --- Utilities -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;-------------------------------------------
;; util, counsel and swiper
;; https://github.com/abo-abo/swiper
;;-------------------------------------------

(defun my/is-wsl()
  (not (string-equal (shell-command-to-string "uname -a|grep Microsoft") "")))

(defun my/is-win()
  (string-equal system-type "windows-nt"))

(defun my/is-linux()
  (string-equal system-type "gnu/linux"))

(provide 'init-util)
;;; init-util.el ends here
