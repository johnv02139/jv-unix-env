;; -*- Mode: Emacs-Lisp -*-

;; This is an example .emacs file.  Copy it to ~/.emacs.d/init.el (or .emacs)
;; and edit it to the config home you want to use.

;; The all-caps string below is meant to allow easy substitution via sed.

;; where emacs/jv-emacs (et al) can be found:
(defvar jv-env-home "JVCFGHME")
(defvar personal-emacs-directory (format "%s/emacs" jv-env-home))

(add-to-list 'load-path personal-emacs-directory)

;; These can be customized to have emacs resize the window to the
;; specified dimensions upon load (assuming, of course, running in
;; windowed mode).  These don't need to be specified.
(defvar jv-emacs-columns 120)
(defvar jv-emacs-rows 71)

;; When first installing on a new machine, it can be simpler to
;; set this to nil, make sure everything else is ok, and then
;; try to download additional emacs packages.
(defvar download-jv-packages-p nil)

(load "jv-emacs")

(defvar jv-cider-directory "JVCIDER")
(load "jv-cider")
