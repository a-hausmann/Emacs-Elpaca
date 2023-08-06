;; -*- lexical-binding: t -*-
;; File name:     ee-editing.el
;; Created:       2023-07-30
;; Last modified: Sun Jul 30, 2023 15:13:49
;; Purpose:       Configure packages used in straight editing (not programming languages)
;;

;; Configure WS-Butler (trims trailing whitespace ONLY on changed lines.)
(use-package ws-butler
  :elpaca t
  :defer 2
  :delight
  :hook ((sql-mode . ws-butler-mode)
         (sh-mode . ws-butler-mode)
         (emacs-lisp-mode . ws-butler-mode)))
;; Allow Elpaca to process queues up to this point
(elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


;; Configure EditorConfig, ref: https://github.com/editorconfig/editorconfig-emacs#readme
(use-package editorconfig
  :elpaca t
  :delight
  :config
  (editorconfig-mode 1)
  (setq editorconfig-trim-whitespaces-mode 'ws-butler-mode))
;; Allow Elpaca to process queues up to this point
(elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


;; Configure Expand-region
(use-package expand-region
  :elpaca t
  :delight
  :bind ("C-=" . er/expand-region))
;; Allow Elpaca to process queues up to this point
(elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


;; Configure Drag-stuff
(use-package drag-stuff
  :elpaca t
  :delight
  :bind ("M-<f3>" . drag-stuff-mode)
  :config
  (drag-stuff-define-keys))
;; Allow Elpaca to process queues up to this point
(elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


;; Configure Oragami, folding package plus personal functions.
;; Step 1, define function to set set folding mode to triple braces.
(defun ah--set-origami-fold-style-braces ()
  "Set origami fold-style to triple braces."
  (interactive)
  (if (bound-and-true-p display-line-numbers-mode)
      (message "Already displaying line numbers")
    (display-line-numbers-mode))
  (setq-local origami-fold-style 'triple-braces)
  (origami-mode)
  (origami-close-all-nodes (current-buffer)))
;; Step 2, define a "wrapper" function.
(defun aeh/origami-toggle-node ()
  (interactive)
  (save-excursion ;; leave point where it is
    (goto-char (point-at-eol))             ;; then go to the end of line
    (origami-toggle-node (current-buffer) (point))))                 ;; and try to fold
;; Step 3, install Origami.
(use-package origami
  :elpaca t
  :defer 1
  :delight)
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local origami-fold-style 'triple-braces)
            (origami-mode)
            (origami-close-all-nodes (current-buffer))))
;; Allow Elpaca to process queues up to this point
(elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword

