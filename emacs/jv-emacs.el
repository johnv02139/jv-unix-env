;; -*- Mode: Emacs-Lisp -*-

;; jv-emacs
;;
;; Preferences common to FSF and X emacs

;; jv-customizations-p... setting that to nil will give you essentially the
;; same thing as running "emacs -q".  It will bind those few variables listed
;; beneath it, but won't load any other files.
(if (not (boundp 'jv-customizations-p))
  (defvar jv-customizations-p t))

;; homedir... useful knowledge to have around
(defvar homedir (getenv "HOME"))

(defun mswin-emacs-p ()
  (or
   (string-match "windows" (emacs-version))
   (string-match "mingw" (emacs-version))
   (string-match "nt[4-9]\\\.[0-9]" (emacs-version))))

(defun post-24-emacs-p ()
  (>= emacs-major-version 24))

(defun load-if-exists (f)
  (when (file-exists-p (format "%s/%s.el" personal-emacs-directory f))
    (load f)))

(cond
 (jv-customizations-p
  (load-if-exists "jv-before")
  ;; Load the file which does the somewhat weird custom-set-variables.
  (load "csetvars")
  (when (post-24-emacs-p)
    (load "jv-packages"))
  (load "jv-functions")
  (when (require 'cider nil 'noerror)
    (load "jv-cider"))
  (load "jv-setup")
  (load "gnu-bindings")
  (when window-system
    (load "yw-fsf"))
  (load-if-exists "jv-after")))
