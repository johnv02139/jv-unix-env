;; -*- Mode: Emacs-Lisp -*-

;; csetvars.el - a separate file because the whole notion is a bit
;; different from normal Emacs Lisp, but also for the text of the
;; scratch buffer, which is "ugly" to see in the .el file.

(custom-set-variables
 '(inhibit-startup-screen t)
 '(initial-scratch-message ";; -*- Mode: Lisp-Interaction -*-\n\n\n")
 '(package-selected-packages
   '(mustache-mode markdown-mode json-mode clojure-mode fsharp-mode))
 '(safe-local-variable-values
   (quote ((Syntax . COMMON-LISP)
           (Package . CL-EDI)
           (Base . 10)
           (indent-tabs-mode . NIL)))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
