;; -*- lexical-binding: t -*-
;; File name:     ee-evil.el
;; Created:       2023-07-22
;; Last modified: Thu Oct 05, 2023 15:19:32
;; Purpose:       Configure Evil mode and accompanying packages
;;

;; Originally had issues with evil mode which were solved by loading evil LAST.
;; Note that all Evil mode must come last in the configuration as other packages
;; need to load first else their config could override that of Evil.
;; 08/12/2023: Still seems necessary for evil to be loaded last.
;; Ref: https://github.com/howardabrams/dot-files/blob/master/emacs-evil.org
;; Ref: https://github.com/aaronbieber/dotfiles/blob/master/configs/emacs.d/lisp/init-evil.el

(setq evil-want-keybinding nil)
(use-package evil
  :elpaca t
  :demand
  :delight
  :config
  (evil-mode 1)

  ;; Do NOT have to use evil in every mode, so let's make a list where evil is not used.
  (dolist (mode '(ag-mode
                  flycheck-error-list-mode
                  paradox-menu-mode
                  git-rebase-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  ;; Start in insert mode for small buffers
  ;; 2018-10-16: This was Howard's idea (I think), and it is *BAD*. Better to start in normal mode for most files
  ;; including org files...and text files (added 2019-05-28).
  (dolist (mode '(org-mode sql-mode lisp-mode text-mode))
    (add-to-list 'evil-normal-state-modes mode))

  (evil-add-hjkl-bindings eww-mode-map 'emacs
    (kbd "/")       'evil-search-forward
    (kbd "n")       'evil-search-next
    (kbd "N")       'evil-search-previous
    (kbd "C-f")     'evil-scroll-down
    (kbd "C-b")     'evil-scroll-up
    (kbd "C-w C-w") 'ace-window)

  :bind (:map evil-normal-state-map
              ;; Don't need 'q' to start recording a macro...
              ;; I'm more familiar with Emacs' way of doing things.
              ("q" . nil)
              ;; Why is 'f' line-bound?
              ;; Wanna rebind f to avy?
              ;; ("f" . iy-go-to-char)
              ;; ("F" . iy-go-to-char-backward)
              ;; How about avy to 't'?
              ;; ("t" . avy-goto-char-timer)
              ;; ("T" . avy-goto-word-timer)
              ;; Shame that meta keys don't work, so let's
              ;; use the 'z' prefix:
              ;; ("z," . ha/xref-pop-marker-stack)
              ("z." . find-tag)))
;; Allow Elpaca to process queues up to this point
;; (elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


;; Configure evil-surround
(use-package evil-surround
  :elpaca t
  :delight
  :after evil
  :config
  (global-evil-surround-mode))
;; Allow Elpaca to process queues up to this point
;; (elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


;; Configure evil-commentary
(use-package evil-commentary
  :elpaca t
  :delight
  :after evil)
(add-hook 'prog-mode-hook 'evil-commentary-mode)
;; Allow Elpaca to process queues up to this point
;; (elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


;; Configure evil-matchit
(use-package evil-matchit
  :elpaca t
  :delight
  :after evil
  :config
  (global-evil-matchit-mode 1))
;; Allow Elpaca to process queues up to this point
;; (elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


;; Configure evil-exchange
(use-package evil-exchange
  :elpaca t
  :delight
  :after evil)
;; Allow Elpaca to process queues up to this point
;; (elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword
;; (evil-exchange-cx-install)

;; Configure evil-collection, which supersedes evil-magit
(use-package evil-collection
  :elpaca t
  :after evil
  :demand
  :delight
  :config (evil-collection-init))
;; Allow Elpaca to process queues up to this point
(elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


(provide 'ee-evil)

;; End of ee-evil.el
