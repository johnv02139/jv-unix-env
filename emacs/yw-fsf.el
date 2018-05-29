;; -*- Mode: Emacs-Lisp -*-

;;;; yw-fsf.el: yw = "yes-windows", the opposite of "nw", as in "emacs -nw".
;;;; In other words, this is stuff that we use when FSF Emacs is run in a
;;;; windowed mode.  This concerns things like fonts and setting the size
;;; of the window.

;; By default, we like fontification.
(font-lock-mode 1)

(tool-bar-mode 0)

(if (not (boundp 'jv-emacs-columns))
  (defvar jv-emacs-columns 90))

(if (not (boundp 'jv-emacs-rows))
  (defvar jv-emacs-rows 41))

;; On MSWin GNU Emacs 23.1.1, at least, set-frame-size no longer appears to
;; work.  (More specifically, it appears to work briefly, and then be overruled
;; by something.)  This is recommended by the EmacsWiki.  If I go back and run
;; with an earlier version of Emacs, this may or may not work.  In that case,
;; the code will need to be bifurcated.

(add-to-list 'default-frame-alist (cons 'height jv-emacs-rows))
(add-to-list 'default-frame-alist (cons 'width jv-emacs-columns))


;; Mouse stuff

;; Windows: two-button-mouse
(setq w32-num-mouse-buttons 2)
;; use right button as "middle button"
(setq w32-swap-mouse-buttons t)
;; use left+right (within 125 ms of each other) as "right button"
(setq w32-mouse-button-tolerance '125)

(setq select-active-regions nil)
(setq mouse-drag-copy-region t)
(global-set-key [mouse-2] 'mouse-yank-at-click)


;; known colors according to GNU Emacs 22.1.1 (mac-apple-darwin)
;; black, red, green, blue, magenta, cyan, yellow, white

;; 'ultra-bold 'extra-bold 'bold 'semi-bold 'normal 'semi-light 'light 'extra-light 'ultra-light
;; 'italic 'oblique 'normal 'reverse-italic 'reverse-oblique

;; These faces are defined in the order they appear in "Faces for Font Lock"
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Faces-for-Font-Lock.html

;; Among other things, font-lock-warning-face applies to the close quote in Java mode.
(set-face-attribute 'font-lock-warning-face              nil :foreground "red")
(set-face-attribute 'font-lock-function-name-face        nil :foreground "blue"
                                                             :weight     'bold)
(set-face-attribute 'font-lock-variable-name-face        nil :foreground "goldenrod")
(set-face-attribute 'font-lock-keyword-face              nil :foreground "purple"
                                                             :weight     'bold)
(set-face-attribute 'font-lock-comment-face              nil :foreground "red"
                                                             :slant      'italic)
(set-face-attribute 'font-lock-comment-delimiter-face    nil :foreground "red")
(set-face-attribute 'font-lock-type-face                 nil :foreground "darkgreen")
(set-face-attribute 'font-lock-constant-face             nil :foreground "goldenrod")
(set-face-attribute 'font-lock-builtin-face              nil :foreground "purple")
;; preprocessor-face includes java methods in clojure
(set-face-attribute 'font-lock-preprocessor-face         nil :foreground "blue")
(set-face-attribute 'font-lock-string-face               nil :foreground "#556b2f")
(set-face-attribute 'font-lock-doc-face                  nil :foreground "#556b2f")
(set-face-attribute 'font-lock-negation-char-face        nil :foreground "red")
(set-face-attribute 'font-lock-regexp-grouping-backslash nil :foreground "red")
(set-face-attribute 'font-lock-regexp-grouping-construct nil :foreground "red")

;;; `Highlight-prompt-face'  is used to highlight parts of the
;;; prompt that deserve attention.

(copy-face 'default 'highlight-prompt-face)
(set-face-foreground 'highlight-prompt-face "violetred")
