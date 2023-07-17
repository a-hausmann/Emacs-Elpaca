;; -*- lexical-binding: t -*-
;; File name:     ee-requires.el
;; Created:       2023-07-15
;; Last modified: Sat Jul 15, 2023 18:22:32
;; Purpose:       This is the main package loader/configurator for Emacs-Elpaca
;;

;; Set the size of the frame
(when window-system
  (if (string-equal system-type "windows-nt")
      (progn
        (add-to-list 'default-frame-alist '(height . 40))
        (add-to-list 'default-frame-alist '(width . 160)))
    (progn
      (add-to-list 'default-frame-alist '(height . 38))
      (add-to-list 'default-frame-alist '(width . 140))))
  (blink-cursor-mode 0))

;; Set garbage collection hook
(add-hook 'focus-out-hook #'garbage-collect)

