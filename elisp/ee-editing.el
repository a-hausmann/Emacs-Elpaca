;; -*- lexical-binding: t -*-
;; File name:     ee-editing.el
;; Created:       2023-07-30
;; Last modified: Fri Jun 07, 2024 16:12:12
;; Purpose:       Configure packages used in straight editing (not programming languages)
;;

;; 05/26/2024: add treesitter.
;; (elpaca '(treesit-auto :source "MELPA" :recipe (:package "treesit-auto" :fetcher github :repo "renzmann/treesit-auto" :files ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))) :description "Automatically use tree-sitter enhanced major modes" :date (14445 17280) :url "https://github.com/renzmann/treesit-auto.git"))
;; (use-package treesit-auto
;;     :custom
;;   (treesit-auto-install 'prompt)
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode))

;; Configure WS-Butler (trims trailing whitespace ONLY on changed lines.)
(use-package ws-butler
  :elpaca t
  :defer 2
  :delight
  :hook ((sql-mode . ws-butler-mode)
         (sh-mode . ws-butler-mode)
         (emacs-lisp-mode . ws-butler-mode)))
;; Allow Elpaca to process queues up to this point
;; (elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


;; Configure EditorConfig, ref: https://github.com/editorconfig/editorconfig-emacs#readme
(use-package editorconfig
  :elpaca t
  :delight
  :config
  (editorconfig-mode 1)
  (setq editorconfig-trim-whitespaces-mode 'ws-butler-mode))
;; Allow Elpaca to process queues up to this point
;; (elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


;; Configure Expand-region
(use-package expand-region
  :elpaca t
  :delight
  :bind ("C-=" . er/expand-region))
;; Allow Elpaca to process queues up to this point
;; (elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


;; Configure Drag-stuff
(use-package drag-stuff
  :elpaca t
  :delight
  :bind ("M-<f3>" . drag-stuff-mode)
  :config
  (drag-stuff-define-keys))
;; Allow Elpaca to process queues up to this point
;; (elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


;; Configure Highlight-thing; highlights all occurances of the "thing" under point.
;; Generally have found only the "word" under point useful, and not ALL the time, so
;; provide toggle in "C-<f3>" and set to "word" for thing.
(use-package highlight-thing
  :delight
  :bind ("C-<f3>" . highlight-thing-mode)
  :config
  (setq highlight-thing-what-thing 'word
        highlight-thing-case-sensitive-p nil))
;; Allow Elpaca to process queues up to this point
;; (elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


;; Configure Oragami, folding package plus personal functions.
;; Step 1, define function to set set folding mode to triple braces.
(defun ah--set-origami-fold-style-braces ()
  "Set origami fold-style to triple braces.

Additionally, display line numbers if not already doing so, enable origami,
and close all nodes.

This is designed to be used in a prog-mode-hook."
  (interactive)
  (if (bound-and-true-p display-line-numbers-mode)
      (message "Already displaying line numbers")
    (display-line-numbers-mode))
  (setq-local origami-fold-style 'triple-braces)
  (origami-mode 1)
  (origami-close-all-nodes (current-buffer))
  ;; 10/07/2023: keep this here; for some reason, loading during startup does NOT
  ;; define TAB key correctly, and even if it might, toggling evil-insert-state
  ;; fixes the key definition in the buffer.
  (general-define-key
   :states 'normal
   :keymaps 'origami-mode-map
   "TAB" 'aeh/origami-toggle-node)
  (evil-insert-state)
  (evil-normal-state)
  (message "ah--set-origami-fold-style-braces completed!"))
;; Step 2, define a "wrapper" function.
(defun aeh/origami-toggle-node ()
  (interactive)
  (save-excursion ;; leave point where it is
    (goto-char (point-at-eol))             ;; then go to the end of line
    (origami-toggle-node (current-buffer) (point))))                 ;; and try to fold
;; Step 3, install Origami.
;; 08/27/2023: stop deferring to see if the bindings work without resetting the mode.
(use-package origami
  :elpaca t
  :demand
  ;; :config
  :delight)
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local origami-fold-style 'triple-braces)
            (origami-mode)
            (origami-close-all-nodes (current-buffer))))
;; Allow Elpaca to process queues up to this point
;; (elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


;; Configure rainbow-mode, useful for showing color of codes.
(use-package rainbow-mode
  :defer
  :delight
  :hook 
  (prog-mode . rainbow-mode)
  (org-mode . rainbow-mode))
;; Allow Elpaca to process queues up to this point
;; (elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


;; 02/10/2024: Adding keycast-mode
;; Ref: https://github.com/tarsius/keycast
(use-package keycast
    :elpaca t
    :delight
    ;; :custom
    ;; (customize-set-variable keycast-mode-line-remove-tail-elements nil)
    :config
    (keycast-header-line-mode))

;; Configure Smartparens
;; Ref: https://ebzzry.com/en/emacs-pairs/
(use-package smartparens
  :init 'smartparens-config
  :config (progn (show-smartparens-global-mode t))
  (sp-with-modes sp-lisp-modes
    ;; disable ', it's the quote character!
    (sp-local-pair "'" nil :actions nil))
  )
;; Allow Elpaca to process queues up to this point
;; (elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword
(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)


;; Configure Undo-fu
(use-package undo-fu
  :after evil
  :defer 1
  :bind ((:map evil-normal-state-map ("u" . undo-fu-only-undo))
         (:map evil-normal-state-map ("C-r" . undo-fu-only-redo)))
  ;; :config
  ;; (message "Loaded Undo-fu.")
  ;; (global-undo-tree-mode -1)
  )
(use-package undo-fu-session
  :after evil
  :defer 1
  :config
  (progn
    (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
    (if (file-directory-p "~/.emacs.d/undo-fu-session")
        (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-fu-session")))
      (progn
        (dired-create-directory "~/.emacs.d/undo-fu-session")
        (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-fu-session")))))
    (global-undo-fu-session-mode 1)))


;; Configure Yasnippet
(use-package yasnippet
  :ensure t
  ;; :diminish 'yas-minor-mode
  :delight 'yas-minor-mode
  :hook ((prog-mode . yas-minor-mode)
         (text-mode . yas-minor-mode)
         )
  :config
  (unless (boundp 'warning-suppress-types)
    (setq warning-suppress-types nil))
  (push '(yasnippet backquote-change) warning-suppress-types)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (setq yas-indent-line 'fixed)
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :delight
  :after yasnippet)


(message "Loaded ee-editing.el")

(provide 'ee-editing)

;;; End of ee-editing.el
