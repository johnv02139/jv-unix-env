;;; jv-setup.el

;; Stuff to customize my emacs environment, aside from a few basic things set up
;; in jv-emacs.el.  Uses functions defined in jv-functions.el, so load that first.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; editor settings

;; don't backup files; I never use 'em
(setq make-backup-files nil)
(setq backup-inhibited t)

;; if any somehow do pop up, delete them periodically
(setq delete-auto-save-files t)
(setq auto-save-default t
      auto-save-interval 0
      auto-save-timeout 120)                ; Once every 2 minutes.

(setq require-final-newline nil)  ;; absolutely necessary
(setq version-control 'never)
(setq minibuffer-max-depth nil)
(setq pop-up-windows t)

;; never split window vertically; always split it horizontally
(setq split-height-threshold 50)
(setq split-width-threshold nil)

;; wide monitors
(setq default-fill-column 110)

;; Try to never have two buffers on same file.
(setq find-file-compare-truenames t)

;; don't bother me about GNU at startup
(setq inhibit-startup-message t)

;; don't beep at me
(setq visible-bell t)

;; normally 3; what about ediff?
(setq window-min-height 3)

;; don't complete into object files
(setq completion-ignored-extensions
      '(".class" ".obj" ".exe" ".dll" ".lib"))

;; C mode

;; In theory, "c-basic-offset" and "tab-width" should be the same value.
;; For me, I want 2 spaces of indentation, and no tabs, ever.  If I ever
;; were to open up a source file that had tabs, I suppose I'd still want
;; to see 2 spaces of indentation, so I'd want "tab-width" to be 2.  But
;; more likely, it would be a file with a *mixture* of tabs and spaces,
;; and I think the most likely right value would be 4.
(setq-default indent-tabs-mode nil
              c-basic-offset 2
              tab-width 4
              c-default-style "linux")

;; show line number in modeline
(line-number-mode t)

;; show time in modeline
(display-time)

;; the fake menu bar inside a terminal is pointless
(when (not window-system)
  (menu-bar-mode -1))

(put 'eval-expression 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled t)  ;; the most annoying thing in the world


;;;; Shell Mode - Synch Emacs State with Prompt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode stuff

(add-hook 'sh-mode-hook            'jv-shell-script-mode)
;;; Add consider-prompt-in-output to the output filter functions for shell-mode.
(defvar consider-prompt-in-shell-mode-p t)
(add-hook 'comint-mode-hook        'jv-comint-mode)
;(add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m)

;; If this is nil, it means the actual shell process does not echo the input,
;; and therefore comint will do it.  Comint believes whatever you tell it.
;; If this gets set to nil, and your shell DOES actually echo back your input,
;; you'll end up seeing the thing you typed twice.  Setting this to t ensures
;; that annoyance doesn't happen.
(unless (post-24-emacs-p)
  (setq comint-process-echoes t))

;; turn on font-lock-mode and set some colors whenever a file is opened
(add-hook
 'find-file-hooks
 '(lambda ()
    (font-lock-mode t)))

(add-hook 'text-mode-hook
   '(lambda () (auto-fill-mode 0)))

(add-hook 'dired-mode-hook         'jv-dired-mode)
(add-hook 'java-mode-hook          'jv-java-mode)
(add-hook 'ediff-mode-hook         'jv-ediff-mode)
(add-hook 'diff-mode-hook          'jv-diff-mode)
(add-hook 'emacs-lisp-mode-hook    'jv-elisp-mode)
(add-hook 'lisp-mode-hook          'jv-lisp-mode)
(add-hook 'inferior-lisp-mode-hook 'jv-inferior-lisp-mode)
(add-hook 'c-mode-hook             'jv-c-mode)
(add-hook 'c++-mode-hook           'jv-c-mode)
(add-hook 'c++-mode-hook           'jv-c++-mode)
(add-hook 'js-mode-hook            'jv-js-mode)
(add-hook 'js2-mode-hook           'jv-js-mode)
(add-hook 'typescript-mode-hook    'jv-typescript-mode)
(add-hook 'html-mode-hook          'jv-html-mode)
(add-hook 'css-mode-hook           'jv-css-mode)

(add-to-list 'auto-mode-alist '("\\.c$"     . c-mode))
(add-to-list 'auto-mode-alist '("\\.h$"     . c-mode))
(add-to-list 'auto-mode-alist '("\\.i$"     . c-mode))
(add-to-list 'auto-mode-alist '("\\.cpp$"   . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc$"    . c++-mode))
(add-to-list 'auto-mode-alist '("\\.C$"     . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hxx$"   . c++-mode))
(add-to-list 'auto-mode-alist '("\\.m$"     . objc-mode))

(add-to-list 'auto-mode-alist '("\\.js$"    . js2-mode))
(add-to-list 'auto-mode-alist '("\\.ts$"    . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.tsx$"   . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$"   . html-mode))
(add-to-list 'auto-mode-alist '("\\.aspx$"  . html-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml$" . html-mode))

(when (require 'cloure-mode nil 'noerror)
  (add-hook 'clojure-mode-hook 'jv-clojure-mode)
  (add-to-list 'auto-mode-alist '("\\.clj$"  . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.edn$"  . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.cljx$" . clojure-mode)))

(when (require 'markdown-mode nil 'noerror)
  (add-to-list 'auto-mode-alist '("\\.md?$" . markdown-mode)))

(defvar scriptsdir (format "%s/jv-scripts" jv-env-home))
(add-to-list 'exec-path scriptsdir)

(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  )

(when (post-24-emacs-p)
  (server-start))
