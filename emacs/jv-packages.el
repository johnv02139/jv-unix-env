;; -*- Mode: Emacs-Lisp -*-

;; jv-packages
;;
;; Emacs packages I want to bring in

(if (not (boundp 'download-jv-packages-p))
  (defvar download-jv-packages-p t))

(defvar my-packages '(typescript-mode js2-mode clojure-mode markdown-mode))
;; omit cider, for development

(require 'package)
(setq package-archives
      '(
	;; ("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

;; If there is a problem installing, can try the following:
;; (package-refresh-contents)
;; But this takes a very long time, so definitely don't want
;; to do it upon normal load.

(require 'shell)
(require 'ediff)
(require 'compile)
(require 'font-lock)

(when download-jv-packages-p
  (dolist (p my-packages)
    (unless (package-installed-p p)
      (package-install p))))
