;; init-elpa.el --- Settings and helpers for package.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'package)

;;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility
(let ((versioned-package-dir
       (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                         user-emacs-directory)))
  (setq package-user-dir versioned-package-dir))

(setq package-check-signature nil)

(require 'package)

(setq package-archives '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

;; Alternative: Don't Delete Below - Used for Non-China Mirrors
;; (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
;; ("marmalade" . "https://marmalade-repo.org/packages/")
;; ("melpa" . "https://melpa.org/packages/")))

(setq package-enable-at-startup nil)

(package-initialize)

;; Init Required Packages
(defvar base-packages '(
			solarized-theme
                        monokai-theme
 			use-package ))

(defun my/install-missing-packages(package-list)
  (dolist (p package-list)
    (unless (package-installed-p p)
      (package-refresh-contents)
      (package-install p))
    (add-to-list 'package-selected-packages p)))

(my/install-missing-packages base-packages)

(provide 'init-elpa)
;;; init-elpa.el ends here
