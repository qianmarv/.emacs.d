;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;; References
;; https://github.com/purcell/emacs.d
;; https://github.com/syl20bnr/spacemacs

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
;; Garbage Collection
(setq gc-cons-threshold (* 128 1024 1024))
(setq gc-cons-percentag 0.5)

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'init-elpa)      ;; Machinery for installing required packages

(require 'init-dashboard)

(require 'init-ivy)

(require 'init-edit)

(require 'init-emacs)

(require 'init-projectile)

(require 'init-org)

(require 'init-chinese)

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))
