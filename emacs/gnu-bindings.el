;; -*- Mode: Emacs-Lisp -*-

;; gnu-bindings
;;
;; This file contains key bindings for GNU Emacs.  It is named to differentiate it from
;; bindings for XEmacs, which tended towards a different syntax.
;;
;; I no longer use XEmacs, but I still like this filename.

;;; Bindings I use on a regular basis

(define-key esc-map "g" 'goto-line)
(define-key esc-map "k" 'bury-buffer)
(define-key esc-map "o" 'other-window)
(define-key esc-map "r" 'toggle-read-only)
(define-key esc-map "s" 'replace-string)
(define-key esc-map "z" 'move-line-to-other-buffer)

(define-key global-map [(control x) (control F)] 'find-file-literally)
(define-key global-map [(control x) (control f)] 'find-file)

(define-key global-map [(control x) (control x)] 'clear-buffer)
(define-key global-map (kbd "C-M-S-w") 'clear-buffer)

(define-key global-map (kbd "C-M-w") 'delete-region)
(define-key global-map (kbd "C-S-w") 'delete-region)

(define-key global-map (kbd "M-S-w") 'kill-ring-save)
(define-key global-map (kbd "M-w") 'kill-ring-save)
(define-key global-map (kbd "C-w") 'kill-region)
(define-key global-map (kbd "M-n") 'recenter)
;; (define-key global-map (kbd "M-n") 'kill-this-buffer)
;; (define-key diff-mode-map (kbd "M-n") 'kill-this-buffer)

(defun find-next-file ()
  (interactive)
  (dired-next-line 1)
  (dired-find-file))

;; (define-key dired-mode-map (kbd "M-n") 'find-next-file)

(define-key global-map (kbd "<kp-subtract>") 'clipboard-kill-region)
(define-key global-map (kbd "<kp-add>") 'clipboard-kill-ring-save)
(define-key global-map (kbd "<kp-enter>") 'clipboard-yank)

;;; Key bindings I use rarely or never, but should use more

(define-key global-map [f2] 'split-window-vertically)
(define-key global-map [f3] 'delete-other-windows)
(define-key global-map [f6] 'kill-buffer)
(define-key global-map [f7] 'move-line-to-other-buffer)

(define-key global-map [f2] 'clipboard-kill-ring-save)
(define-key global-map [f3] 'clipboard-yank)
(define-key global-map [f4] 'move-line-to-other-buffer)
(define-key global-map [f5] 'recenter)
(define-key global-map [f6] 'split-window-vertically)
(define-key global-map [f7] 'delete-other-windows)
(define-key global-map [f8] 'bury-buffer)
(define-key global-map [f9] 'other-frame)
(define-key global-map [f10] 'other-window)
(define-key global-map [f11] 'retop)
(define-key global-map [f12] 'clear-buffer)
(define-key global-map [M-f12] 'clear-buffer)

(define-key global-map [kp-4] 'previous-error)
(define-key global-map [kp-5] 'recenter)
(define-key global-map [kp-6] 'next-error)

;;; Key bindings I use all the time, but which may be default

(define-key global-map [home] 'beginning-of-buffer)
(define-key global-map [end] 'end-of-buffer)

;; This is what the "home" key appears to turn into when running Emacs 21.4.1 within putty
(define-key global-map (kbd "M-[ 1 ~") 'beginning-of-buffer)

(define-key global-map "\C-h" 'help)
(define-key text-mode-map "\M-s" 'replace-string)

(define-key esc-map "j" 'switch-to-buffer)
(define-key esc-map "\C-l" 'previous-buffer)

;;; Key bindings which seem like good ideas, but I don't use much yet

(define-key global-map (kbd "C-x j") 'jv-what-line)

(define-key global-map [(control x) (control j)] 'move-buffer-to-other-buffer)
(define-key global-map [(control x) (control m)] 'switch-to-buffer-other-window)

(define-key global-map (kbd "<kp-begin>") 'recenter)
(define-key global-map (kbd "<kp-divide>") 'retop)

(define-key global-map [(control c) (control w)] 'clear-buffer)
(define-key global-map [(control x) (control l)] 'retop)
(define-key global-map [(control c) (control s)] 'shell)
(define-key global-map [(control c) (control x)(control k)] 'new-shell)

;;; Key bindings which may not be such a good idea

(define-key esc-map "\C-g" 'clojure-grep)

;; on starbuck, at least while running in no-window, the <end> key is seen as
;; <select>, so map that too.
(define-key global-map [select] 'end-of-buffer)

(define-key minibuffer-local-filename-completion-map " " 'minibuffer-complete-word)
(define-key minibuffer-local-must-match-filename-map " " 'minibuffer-complete-word)

(define-key global-map "\C-c," 'shell)
(define-key global-map "\C-c\C-j" 'switch-to-gdb-buffer)

;; override
(define-key global-map "\M-w" 'all-copy)
(define-key global-map "\M-`" 'other-frame)

;; default
(define-key global-map "\M-w" 'kill-ring-save)

;; On Macs, alt-tab is not caught by the OS ("change windows" is command-tab),
;; and alt-tab has an annoying binding.  On Windows or Linux, I assume it is
;; caught by the OS, so these bindings won't matter at all.
(define-key global-map (kbd "C-M-i") 'jv-do-nothing)
(define-key text-mode-map (kbd "C-M-i") 'jv-do-nothing)

(define-key global-map (kbd "M-G") 'jv-grep)
