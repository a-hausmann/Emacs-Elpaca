;;; Setups for editing -*- lexical-binding: t -*-
;; File name:     ee-editing.el
;; Created:       2023-07-30
;; Last modified: Wed Apr 08, 2026 9:42:47
;; Purpose:       Configure packages used in straight editing (not programming languages)
;;

;;; 05/26/2024: add treesitter.
;; (elpaca '(treesit-auto :source "MELPA" :recipe (:package "treesit-auto" :fetcher github :repo "renzmann/treesit-auto" :files ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))) :description "Automatically use tree-sitter enhanced major modes" :date (14445 17280) :url "https://github.com/renzmann/treesit-auto.git"))
;; (use-package treesit-auto
;;     :custom
;;   (treesit-auto-install 'prompt)
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode))

;;; Configure WS-Butler (trims trailing whitespace ONLY on changed lines.)
(use-package ws-butler
  :ensure t
  :defer 2
  :delight
  :hook ((sql-mode . ws-butler-mode)
         (sh-mode . ws-butler-mode)
         (emacs-lisp-mode . ws-butler-mode)))


;;; Configure EditorConfig, ref: https://github.com/editorconfig/editorconfig-emacs#readme
(use-package editorconfig
  :ensure t
  :delight
  :config
  (editorconfig-mode 1)
  (setq editorconfig-trim-whitespaces-mode 'ws-butler-mode))


;;; Configure Expand-region
(use-package expand-region
  :ensure t
  :delight
  :bind ("C-=" . er/expand-region))


;;; Configure Drag-stuff
(use-package drag-stuff
  :ensure t
  :delight
  :bind ("M-<f3>" . drag-stuff-mode)
  :config
  (drag-stuff-define-keys))


;;; Configure Highlight-thing; highlights all occurances of the "thing" under point.
;; Generally have found only the "word" under point useful, and not ALL the time, so
;; provide toggle in "C-<f3>" and set to "word" for thing.
(use-package highlight-thing
  :delight
  :bind ("C-<f3>" . highlight-thing-mode)
  :config
  (setq highlight-thing-what-thing 'word
        highlight-thing-case-sensitive-p nil))


;;; Configure Oragami, folding package plus personal functions.
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
  :ensure t
  :demand
  ;; :config
  :delight)
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local origami-fold-style 'triple-braces)
            (origami-mode)
            (origami-close-all-nodes (current-buffer))))

;;; 06/29/2025: Add hook to enable hide/show minor mode for prog-mode buffers.
;; Hide/Show mode prefix key chord is "C-c @"
;; Hook doesn't seem to be working. Shoot.
(add-hook 'prog-mode-hook 'hs-minor-mode)


;;; Configure rainbow-mode, useful for showing color of codes.
(use-package rainbow-mode
  :defer
  :ensure nil
  :delight
  :hook 
  (prog-mode . rainbow-mode)
  (org-mode . rainbow-mode))


;;; 02/10/2024: Adding keycast-mode
;; Ref: https://github.com/tarsius/keycast
(use-package keycast
    :ensure t
    :delight
    ;; :custom
    ;; (customize-set-variable keycast-mode-line-remove-tail-elements nil)
    :config
    (keycast-header-line-mode))

;;; Configure Electric Pair mode (just turn it on globally)
;; Found that electric-pair-mode works just as well if not better. Will need to figure out the
(electric-pair-mode 1)

;;; Configure Undo-fu
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


;;; Configure Yasnippet
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


;;; 04/08/2026: Adding Embrace (like surround)
;; As I'm moving away from Evil, will lose evil-surround and need replacement
;; NOTE: this package relies on expand-region.
(use-package embrace
    :ensure t
    :bind
    ("C-{" . embrace-commander)
    :init
    (add-hook 'org-mode-hook 'embrace-org-mode-hook))


;; 12/28/2025: Added Linux program "nuspell" (spellchecker) and "jinx", Emacs wrapper for nuspell.
;; Ref: https://github.com/minad/jinx
;; Doesn't work, getting compile error on jinx-mod.0 file. Oddly, if I try command twice it works,
;; but leaves ALL words as misspelled (until I add them to dictionary?) I'm turning this off for now.
;; (use-package jinx
;;   :ensure t
;;   :hook (emacs-startup . global-jinx-mode)
;;   :bind (("M-$" . jinx-correct)
;;          ("C-M-$" . jinx-languages)))


(message "Loaded ee-editing.el")

(provide 'ee-editing)

;;; End of ee-editing.el

