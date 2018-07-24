;; -*- Mode: Emacs-Lisp -*- coding: utf-8 -*-

;;; dirbrowse.el - browse files in a dired

;; Copyright (c) 2018 John Valente <johnv02139@yahoo.com>

;; This file is not part of GNU Emacs.


(defvar buffer-for-dired-browse nil)
(defvar current-browse-buffer nil)

;; I assume it would be bad form to begin the function name with "dired"
;; when it's not actually part of the dired package
(defun open-for-browse-dired ()
  (dired-find-file)
  (setq current-browse-buffer (current-buffer))
  (read-only-mode))

(defun kill-browse-dired-hook ()
  (when (and buffer-for-dired-browse
             (eq buffer-for-dired-browse
                 (current-buffer)))
    (remove-hook 'kill-buffer-hook 'kill-browse-dired-hook)
    (setq buffer-for-dired-browse nil)
    (when current-browse-buffer
      ;; maybe customize whether or not we kill the browsed buffer?
      (kill-buffer current-browse-buffer)
      (setq current-browse-buffer nil))))

(defun start-browse ()
  (interactive)
  (cond
   ((eq major-mode 'dired-mode)
    (setq buffer-for-dired-browse (current-buffer))
    (add-hook 'kill-buffer-hook 'kill-browse-dired-hook)
    (open-for-browse-dired))
   (t
    (setq buffer-for-dired-browse nil)
    (message "Error: can only browse in a dired buffer!"))))

(defun end-browse ()
  (interactive)
  (kill-browse-dired-hook))

(defun browse-next (&optional arg)
  (interactive)
  (or arg (setq arg 1))
  (if (or (null buffer-for-dired-browse)
          (null current-browse-buffer))
      (message "Error: browse not started")
    (progn
      (kill-buffer current-browse-buffer)
      (set-buffer buffer-for-dired-browse)
      (forward-line arg)
      (open-for-browse-dired))))

(defun browse-previous (&optional arg)
  (interactive)
  (or arg (setq arg 1))
  (browse-next (- arg)))

(add-hook 'dired-mode-hook
          '(lambda ()
             (define-key dired-mode-map "B" 'start-browse)
             (define-key dired-mode-map "b" 'start-browse)))

(add-hook 'diff-mode-hook
          '(lambda ()
             (define-key diff-mode-map (kbd "M-k") 'bury-buffer)
             (define-key diff-mode-shared-map "n" 'browse-next)
             (define-key diff-mode-map "n" 'browse-next)
             (define-key diff-mode-shared-map "p" 'browse-previous)
             (define-key diff-mode-map "p" 'browse-previous)
             (define-key diff-mode-map (kbd "M-u") 'browse-previous)
             (define-key diff-mode-map (kbd "M-d") 'browse-next)))

(provide 'dirbrowse)

;;; dirbrowse.el ends here
