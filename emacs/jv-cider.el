;; -*- Mode: Emacs-Lisp -*-

;; jv-cider
;;

(require 'cider)

(setq nrepl-repl-buffer-name-template "*repl-cider%s*")

;; cider-repl-wrap-history
(setq cider-repl-history-size 5000)
(setq cider-repl-history-file
      (format "%s/.emacs.d/repl-history.txt" homedir))
(setq cider-repl-scroll-on-output nil)

;; copy of original function, with different name
(defun cider--switch-to-repl-buffer-orig (repl-buffer &optional set-namespace)
  "Select the REPL-BUFFER, when possible in an existing window.

Hint: You can use `display-buffer-reuse-frames' and
`special-display-buffer-names' to customize the frame in which
the buffer should appear.

When SET-NAMESPACE is t, sets the namespace in the REPL buffer to
that of the namespace in the Clojure source buffer."
  (cider-ensure-connected)
  (let ((buffer (current-buffer)))
    (delete-other-windows)
    (split-window-below)
    (other-window 1)
    ;; first we switch to the REPL buffer
    (if cider-repl-display-in-current-window
        (pop-to-buffer-same-window repl-buffer)
      (pop-to-buffer repl-buffer))
    ;; then if necessary we update its namespace
    (when set-namespace
      (cider-repl-set-ns (with-current-buffer buffer (cider-current-ns))))
    ;; (cider-remember-clojure-buffer buffer)
    (goto-char (point-max))))

;; TODO: see special-display-buffer-names, maybe it provides a simpler way to customize
;; override
(defun cider--switch-to-repl-buffer (repl-buffer &optional set-namespace)
  "Select the REPL-BUFFER, in the top window, with the current buffer in the bottom."
  (cider-ensure-connected)
  (let ((buffer (current-buffer)))
    (delete-other-windows)
    (split-window-below)
    (other-window 1)
    (cider--switch-to-repl-buffer-orig repl-buffer set-namespace)))

(defun jv-cider-mode ()
  (message "invoking cider mode hook"))

(add-hook 'cider-mode-hook 'jv-cider-mode)

(defun jv-clojure-mode ()
  (eldoc-mode))

(add-hook 'clojure-mode-hook 'jv-clojure-mode)

;; (require 'cider-history)
(setq cider-history-show-preview t)
; (setq cider-history-quit-action 'quit-window)
; (setq cider-history-resize-window '(8 . 4))
; (setq cider-history-resize-window nil)
; (setq cider-history-resize-window t)

(defun clear-repl-buffer ()
  (interactive)
  (cider-history-clear-preview)
  (clear-buffer))

(defun cider-repl-kill-all-input ()
  "Kill all text and overlays from the prompt to point."
  (interactive)
  (cider-history-clear-preview)
  (cider-repl-kill-input))
  
(define-key cider-repl-mode-map (kbd "C-c C-u") #'cider-repl-kill-all-input)


(define-key cider-mode-map      (kbd "C-c ,") 'shell)
(define-key cider-repl-mode-map (kbd "C-c ,") 'shell)

(define-key cider-mode-map "\M-s" 'replace-string)
(define-key cider-repl-mode-map "\M-s" 'replace-string)

(define-key cider-mode-map (kbd "C-c n") #'cider-repl-set-ns)
(define-key cider-repl-mode-map (kbd "C-c n") #'cider-repl-set-ns)

(define-key global-map (kbd "C-c ,") 'shell)
(define-key global-map [(control c) (control z)] 'cider-switch-to-repl-buffer)


(defun repl-prompt-abbreviated (namespace)
  "Return a prompt string that abbreviates NAMESPACE."
  (let* ((last-elt (car (last (split-string namespace "\\."))))
         (words (split-string last-elt "[_-]"))
         (prompt "> "))
    (dolist (elt (reverse words) prompt)
      (setq prompt (concat (substring elt 0 1) prompt)))))

(setq cider-repl-prompt-function 'repl-prompt-abbreviated)
