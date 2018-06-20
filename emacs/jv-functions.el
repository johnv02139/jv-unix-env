;; -*- Mode: Emacs-Lisp -*-

;;; jv-functions.el

;; Pretty simple  Elisp functions written by jv for various usage.  Functions do
;; not change the environment until they are used.

(defun fsf-emacs-p ()
  (string-match "GNU Emacs" (emacs-version)))

(defun all-of (list)
  (dolist (elt list)
    (princ elt)
    (terpri)))

(defun show-load-order ()
  (dolist (lh (reverse load-history))
    (princ (car lh))
    (terpri)))

(defun load-path ()
  (dolist (lp load-path)
    (princ lp)
    (terpri)))

(defun jv-colors ()
  (set-face-foreground 'font-lock-keyword-face "blue")
  (set-face-foreground 'font-lock-string-face "forestgreen")
  (set-face-foreground 'font-lock-comment-face "purple"))


;;; Overrides

(defun better-previous-matching-input-from-input (n)
  "Search backwards through input history for match for current input.
\(Previous history elements are earlier commands.)
With prefix argument N, search for Nth previous match.
If N is negative, search forwards for the -Nth following match."
  (interactive "p")
  ;; (goto-char (point-max))
  (unless (memq last-command '(better-previous-matching-input-from-input
                               comint-previous-matching-input-from-input
                               comint-next-matching-input-from-input))
    ;; Starting a new search
    (setq comint-matching-input-from-input-string
          (buffer-substring
           (or (marker-position comint-accum-marker)
               (process-mark (get-buffer-process (current-buffer))))
           (point))
          comint-input-ring-index nil))
  (comint-previous-matching-input
   (concat "^" (regexp-quote comint-matching-input-from-input-string))
   n))


;;; Grep

(require 'grep)

(defun find-git-dir ()
  (file-name-as-directory
   (expand-file-name
    default-directory)))

(defun git-grep-expand-template (regexp files)
  (if (and files (> (length files) 0))
      (grep-expand-template "/usr/local/bin/git --no-pager grep -n -e <R> -- <F>"
                            regexp files)
      (grep-expand-template "/usr/local/bin/git --no-pager grep -n -e <R>" regexp)))

;; Derived from `vc-git-grep', derived from `lgrep'.
(defun jv-grep (regexp)
  "Run git grep, searching for REGEXP in default directory."
  (interactive
   (progn
     (grep-compute-defaults)
     (list (grep-read-regexp))))
  (when (and (stringp regexp) (> (length regexp) 0))
    (let ((dir (find-git-dir)))
      (setq command (git-grep-expand-template regexp nil))
      (add-to-history 'grep-history command)
      (let ((default-directory dir)
            (compilation-environment (cons "PAGER=" compilation-environment)))
        ;; Setting process-setup-function makes exit-message-function work
        ;; even when async processes aren't supported.
        (compilation-start command 'grep-mode))
      (when (eq next-error-last-buffer (current-buffer))
        (setq default-directory dir)))))

(defun jv-grep-files (regexp files)
  "Run git grep, searching for REGEXP in FILES in directory DIR."
  (interactive
   (progn
     (grep-compute-defaults)
     (let* ((regexp (grep-read-regexp))
            (files (grep-read-files regexp)))
       (list regexp files))))
  (when (and (stringp regexp) (> (length regexp) 0))
    (let ((command regexp)
          (dir (find-git-dir)))
      (setq command (git-grep-expand-template regexp files))
      (add-to-history 'grep-history command)
      (when command
        (let ((default-directory dir)
              (compilation-environment (cons "PAGER=" compilation-environment)))
          ;; Setting process-setup-function makes exit-message-function work
          ;; even when async processes aren't supported.
          (compilation-start command 'grep-mode))
        (if (eq next-error-last-buffer (current-buffer))
            (setq default-directory dir))))))


;;; Begin mode functions

(defun jv-shell-script-mode ()
  (sh-set-shell "/bin/sh")
  (auto-fill-mode 0)
  (setq sh-indentation 2)
  (setq sh-basic-offset 2))

(defun jv-comint-mode ()
  (setq comint-scroll-show-maximum-output nil)
  ;; (load "next-history-element")
  (define-key shell-mode-map (kbd "M-p") 'better-previous-matching-input-from-input)
  (define-key comint-mode-map "\C-a" 'comint-bol)
  (define-key comint-mode-map "\C-j" 'comint-send-input)
  (define-key comint-mode-map "\M-p"
    'better-previous-matching-input-from-input)
  (define-key comint-mode-map "\M-s" 'replace-string)
  (define-key comint-mode-map "\M-\C-l" 'retop))

(defun jv-dired-mode ()
  (define-key dired-mode-map "c" 'dired-byte-compile-file)
  (define-key dired-mode-map "h" 'dired-find-file-literally)
  (define-key dired-mode-map "\M-k" 'bury-buffer))

(defun jv-java-mode ()
  (c-set-style "java")
  (font-lock-mode 1)
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 2)
  ;; See comment in jv-setup.el about why these are different
  (setq tab-width 2)
  (define-key java-mode-map (kbd "M-G") 'jv-grep)
  (c-set-offset 'substatement-open 0))

(defun jv-clojure-mode ()
  (eldoc-mode 1)
  ;; just re-establishing root binding
  (define-key clojure-mode-map "\C-c," 'shell))

(defun jv-ediff-mode ()
  (set-face-background 'ediff-current-diff-A "green3")
  (set-face-foreground 'ediff-current-diff-A "black")
  (set-face-background 'ediff-current-diff-B "blue")
  (set-face-foreground 'ediff-current-diff-B "white")
  (set-face-background 'ediff-fine-diff-A "green3")
  (set-face-foreground 'ediff-fine-diff-A "white")
  (set-face-background 'ediff-fine-diff-B "skyblue")
  (set-face-foreground 'ediff-fine-diff-B "black"))

(defun jv-diff-mode ()
  (define-key diff-mode-map (kbd "M-k") 'bury-buffer))

(defun jv-elisp-mode ()
  (font-lock-mode 1)
  (setq-default tab-width 2))

(defun jv-lisp-mode ()
  (set (make-local-variable lisp-indent-function)
       'common-lisp-indent-function)
  (font-lock-mode 1)
  (setq indent-tabs-mode nil)
  (setq-default tab-width 2)
  (setq tab-width 2))

;; This has an apparent inconsistency which might look like a bug, but it isn't.
;; This is a hook to be run when an *inferior*-lisp is started, but it's
;; changing the mode map for plain old lisp.  That's because running an inferior
;; lisp changes the lisp-mode-map, and so this function is just reverting one
;; binding back to what it was before the inferior lisp was started.

(defun jv-inferior-lisp-mode ()
  (define-key lisp-mode-map [(control c) (control k)] 'shell))

(defun jv-c-mode ()
  (setq c-default-style "linux")
  (font-lock-mode 1)
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 2)
  (setq tab-width 2)
  (define-key c-mode-map (read "[?\\M-G]") 'grep-c-sandbox))

(defun jv-c++-mode ()
  (font-lock-mode 1)
  (define-key c++-mode-map [(control up)] 'c-beginning-of-defun)
  (define-key c++-mode-map [(control down)] 'c-end-of-defun)
  (define-key c++-mode-map (read "[?\\M-G]") 'grep-c-sandbox))



;;; begin interactive functions

(defun jv-do-nothing ()
  (interactive))

(defun clear-buffer ()
  "Delete everything in the buffer"
  (interactive)
  (let ((inhibit-read-only t))
    (delete-region 1 (point-max))))

(defun retop ()
  (interactive)
  (recenter 0))

(defun jvnl ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;; note, this does not notice if the tab is inside quotes
    (while (search-forward "\t" nil t)
      (replace-match "    " nil t))
    (goto-char (point-min))
    (while (re-search-forward "[ ]+$" nil t)
      (replace-match "" nil nil))))

(defun copy-and-remove ()
  (interactive)
  (let ((current-block
         (buffer-substring (mark) (point))))
    (save-excursion
      (while (search-forward current-block nil t)
        (replace-match "" nil t)))))

(defun move-line-to-other-buffer ()
  (interactive)
  (let ((kill-whole-line t))
    (beginning-of-line)
    (kill-line))
  (other-window -1)
  (yank)
  (other-window 1))

(defun other-window-counterclockwise (&optional whole-exp)
  "Other window, in the other direction"
  (interactive)
  (other-window -1))

(defun previous-buffer ()
  (interactive)
  (switch-to-buffer nil))

(defun swap-windows ()
  (interactive)
  (let ((me (current-buffer)))
    (other-window 1)
    (let ((him (current-buffer)))
      (switch-to-buffer me)
      (other-window -1)
      (switch-to-buffer him)
      (other-window 1))))

(defun sort-lines-reverse ()
  (interactive)
  (sort-lines t (mark) (point)))

(defun dired-byte-compile-file ()
  "In dired, byte-compile the file named on this line."
  (interactive)
  (byte-compile-file (dired-get-filename)))

(defun dired-find-file-literally ()
  (interactive)
  (find-file-literally (dired-get-filename)))

(defun json-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))

;; new-shell - doesn't work if shell mode hasn't started yet.
;; Should check that, and just call (shell) if not.

(defvar shell-names (list "incoming" "mods" "tools"))

(defun unused-comint-buffer-name (basename)
  (if (null (get-buffer (concat "*" basename "*")))
      basename
    (unused-comint-buffer-name
     (let ((hyphen-position (string-match "-" basename))
           (existing-suffix 0))
       (if hyphen-position
           (setq existing-suffix
                 ;; for earlier versions of emacs, use string-to-int
                 (string-to-number
                  (substring basename
                             (+ 1 hyphen-position)))))
       (if (= 0 existing-suffix)
           (concat basename "-1")
         (concat (substring basename 0 (+ 1 hyphen-position))
                 (int-to-string (+ 1 existing-suffix))))))))

(defun new-shell ()
  (interactive)
  (let* ((prog (or (and (boundp 'explicit-shell-file-name)
                        (symbol-value 'explicit-shell-file-name))
                   (getenv "ESHELL")
                   (getenv "SHELL")
                   "/bin/sh"))
         (name (file-name-nondirectory prog))
         (startfile (concat "~/.emacs_" name))
         (xargs-name (intern-soft (concat "explicit-" name "-args")))
         (default-name (unused-comint-buffer-name "shell"))
         (default-values (cons default-name shell-names))
         (supplied-name (read-from-minibuffer
                         (format "Create shell buffer with name (default %s): " default-name)
                         nil ;; initial-contents
                         nil ;; keymap
                         nil ;; read
                         'shell-names ;; hist
                         default-values ;; default-value
                         ))
         (buffer-name (if (> (length supplied-name) 0) supplied-name default-name))
         shell-buffer)
    (setq shell-names (delete buffer-name shell-names))
    (save-excursion
      (set-buffer (apply 'make-comint buffer-name prog
                         (if (file-exists-p startfile) startfile)
                         (if (and xargs-name (boundp xargs-name))
                             (symbol-value xargs-name)
                             '("-i"))))
      (setq shell-buffer (current-buffer))
      (shell-mode))
    (pop-to-buffer shell-buffer)))


(defvar diff-buff-1 nil)
(defvar diff-buff-2 nil)
(defvar orig-diff-buff nil)

(defun do-diffs ()
  (interactive)
  (setq orig-diff-buff (current-buffer))
  (let* ((text (thing-at-point 'line t))
         (texts (cddr (split-string text))))
    (setq diff-buff-1 (find-file (car texts)))
    (setq diff-buff-2 (find-file (cadr texts)))
    (ediff-buffers diff-buff-1 diff-buff-2)))

(defun no-diffs ()
  (interactive)
  (kill-buffer diff-buff-1)
  (kill-buffer diff-buff-2)
  (switch-to-buffer orig-diff-buff)
  (delete-other-windows)
  (goto-char (point-max))
  (comint-previous-matching-input "^" 1))
