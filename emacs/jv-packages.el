;; -*- Mode: Emacs-Lisp -*-

;; jv-packages
;;
;; Emacs packages I want to bring in

(if (not (boundp 'download-jv-packages-p))
  (defvar download-jv-packages-p t))

(defvar my-packages '(clojure-mode cider markdown-mode))

(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(package-initialize)

(require 'ediff)
(require 'cl)
(require 'compile)
(require 'font-lock)

(when download-jv-packages-p
  (dolist (p my-packages)
    (unless (package-installed-p p)
      (package-install p))))
