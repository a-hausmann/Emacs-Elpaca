;; -*- lexical-binding: t -*-
;; File name:     early-init.el
;; Created:       2023-07-13
;; Last modified: Thu Jul 13, 2023 1:23:26
;; Purpose:       For repository "Emacs-Elpaca".
;; References:    https://github.com/progfolio/.emacs.d
;;

;; Installation of Elpaca, ref: https://github.com/progfolio/elpaca#installer
;; Disable package.el in the early-init file.
(setq package-enable-at-startup nil)

(setq inhibit-default-init nil)

;; (setq native-comp-async-report-warnings-errors nil)  ;; only in Emacs 29.0+

;; Debugging: ref: https://github.com/progfolio/.emacs.d#debugging
;; Running this form will launch the debugger after loading a package. 
;; This is useful for finding out when a dependency is requiring a package 
;; (perhaps earlier than you want). Use by tangling this block and 
;; launching Emacs with emacs --debug-init.
(unless (string-empty-p file)
  (eval-after-load file
    '(debug)))

;; file-name-handler-alist Ref: https://github.com/progfolio/.emacs.d#file-name-handler-alist
;; Skipping a bunch of regular expression searching in the file-name-handler-alist should improve start time.
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Garbage Collection, ref: https://github.com/progfolio/.emacs.d#garbage-collection
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1)

(defun +gc-after-focus-change ()
  "Run GC when frame loses focus."
  (run-with-idle-timer
   5 nil
   (lambda () (unless (frame-focus-state) (garbage-collect)))))

(defun +reset-init-values ()
  (run-with-idle-timer
   1 nil
   (lambda ()
     (setq file-name-handler-alist default-file-name-handler-alist
           gc-cons-percentage 0.1
           gc-cons-threshold 100000000)
     (message "gc-cons-threshold & file-name-handler-alist restored")
     (when (boundp 'after-focus-change-function)
       (add-function :after after-focus-change-function #'+gc-after-focus-change)))))

(with-eval-after-load 'elpaca
  (add-hook 'elpaca-after-init-hook '+reset-init-values))

;; UI, ref: https://github.com/progfolio/.emacs.d#ui
;; Implicitly resizing the Emacs frame adds to init time. Fonts larger than the 
;; system default can cause frame resizing, which adds to startup time.
(setq frame-inhibit-implied-resize t)

;; Set default and backup fonts
(push '(font . "Source Code Pro") default-frame-alist)
(set-face-font 'default "Source Code Pro")
(set-face-font 'variable-pitch "DejaVu Sans")
(copy-face 'default 'fixed-pitch)

;; Ignore X resources.
(advice-add #'x-apply-session-resources :override #'ignore)

;; Silence bells.
(setq ring-bell-function #'ignore
      inhibit-startup-screen t)


(provide 'early-init)
;;; early-init.el ends here
