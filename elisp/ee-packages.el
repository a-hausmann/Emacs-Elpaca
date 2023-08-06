;; -*- lexical-binding: t -*-
;; File name:     ee-packages.el
;; Created:       2023-07-15
;; Last modified: Sun Jul 30, 2023 15:00:30
;; Purpose:       This is the main package loader/configurator for Emacs-Elpaca
;;

;; Configure delight
(use-package delight
  :elpaca t)
;; Allow Elpaca to process queues up to this point
(elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword

;; Configure which-key
(use-package which-key
  :elpaca t
  :init (which-key-mode)
  ;; :diminish (which-key-mode)
  :delight
  :config
  (setq which-key-idle-delay 0.5))
;; Allow Elpaca to process queues up to this point
(elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


;; Elisp mode
(use-package emacs-lisp-mode
  :elpaca nil
  :commands emacs-lisp-mode
  :delight emacs-lisp-mode "Emacs Lisp"
  :config (delight 'lisp-interaction-mode "Lisp Interaction"))
;; 2021-02-21: Package ielm is a repl for emacs lisp, so ONLY load when commanded in.
(use-package ielm
  :elpaca nil
  :commands ielm
  :hook (ielm-mode . (lambda () (setq-local scroll-margin 0))))
(use-package lisp-mode
  :elpaca nil
  :hook (emacs-lisp-mode . lisp-mode)
  :delight lisp-mode "Lisp")
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
;; Allow Elpaca to process queues up to this point
(elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


;; All-the-icons
(use-package all-the-icons
  :elpaca t
  :delight)
;; Allow Elpaca to process queues up to this point
(elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


;; Amx is the newer alternative to smex (aka smart M-x). 
(use-package amx
  :elpaca t
  :commands (amx amx-major-mode-commands execute-extended-command)
  :delight
  :config
  (amx-mode t)   ; always in amx-mode
  (global-set-key (kbd "M-x") 'amx)
  (global-set-key (kbd "M-X") 'amx-major-mode-commands)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c M-x") 'execute-extended-command))
;; (setq-default amx-save-file (no-littering-expand-var-file-name ".amx-items"))
;; Allow Elpaca to process queues up to this point
(elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


;; Configure Avy
(use-package avy
  :elpaca t
  :commands avy-goto-char-timer
  :delight
  :bind
  ("C-x C-a l" . avy-copy-line)
  ("C-x C-a r" . avy-copy-region)
  ("C-x C-a m" . avy-move-line)
  ("C-x C-a M-r" . avy-move-region)
  ("C-x C-a w" . avy-goto-word-1)
  ("C-x C-a ;" . avy-goto-char)
  ("C-x C-a '" . avy-goto-char-2)
  ("C-x C-a t" . avy-goto-char-timer)
  ("C-x C-t" . avy-goto-char-timer)
  :config
  (setq avy-timeout-seconds 0.5))
;; Allow Elpaca to process queues up to this point
(elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


;; Configure Aggressive-indent, works well with Emacs-lisp, not that well with other languages (Python?)
(use-package aggressive-indent
  :elpaca t
  :delight
  :hook (emacs-lisp-mode . aggressive-indent-mode))
;; Allow Elpaca to process queues up to this point
(elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


;; allow asynchronous processing wherever possible…pretty nice.
(use-package async
  :elpaca t
  :demand
  :delight
  :config
     (dired-async-mode 1))
;; Allow Elpaca to process queues up to this point
(elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


;; Configure beacon
(use-package beacon
  :elpaca t
  :demand
  :delight
  :config
  (beacon-mode 1))
;; Allow Elpaca to process queues up to this point
(elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


;; Configure column-enforce-mode
(use-package column-enforce-mode
  :elpaca t
  :delight
  :hook (prog-mode . column-enforce-mode)
  :config (setq column-enforce-comments nil))
;; Allow Elpaca to process queues up to this point
(elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


;; Configure Magit.
(use-package magit
    :elpaca t
    :delight
    :commands (magit-status)
    :bind ("C-x g g" . magit-status)
    ("C-x g b" . magit-blame)
    ("C-x g c" . magit-branch-checkout)
    ("C-x g l" . magit-log-buffer-file)
    )
(setq magit-push-always-verify nil)
(setq git-commit-summary-max-length 50)
(use-package magit-gitflow
  :elpaca t
  :after magit
  :delight
  :hook (magit-mode . turn-on-magit-gitflow))
;; Allow Elpaca to process queues up to this point
(elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


;; Load scripts to set up completion, both auto-complete and completing read.
(load "ee-auto-complete")
(load "ee-completion")


;; Configure wgrep/Ripgrep. 07/29/2023: Use ripgrep only at home for now.
(use-package wgrep
  :elpaca nil
  :defer 1
  :delight)

(when (string-equal ee-system-type "linux")
  (use-package rg
    :elpaca t
    :after wgrep
    ;; :delight
    :init (rg-enable-default-bindings)))
;; Allow Elpaca to process queues up to this point
(elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


;; Configure command-log-mode
(use-package command-log-mode
  :elpaca t
  :commands (command-log-mode)
  :bind ("C-c o" . clm/toggle-command-log-buffer))
;; Allow Elpaca to process queues up to this point
(elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


;; Configure editing stuff
(load "ee-editing")




;; Keep this at end.
(if (string-equal ee-system-type "windows-nt")
    (progn
      (find-file "c:/_work/org/todo.org")
      ;; 2020-04-27: Created acts.org, load with todo.org
      (find-file "c:/_work/org/acts.org")
      ;; 2020-09-09: Add back in jira.org file.
      (find-file "c:/_work/org/jira.org")
      ;; 2020-02-28: add x12-mode autoload plus file extensions
      (progn
        (autoload 'x12-mode "x12-mode" "" t)
        ;; Add more file extensions as required
        (add-to-list 'auto-mode-alist '("\\.x12\\'" . x12-mode))))
  (progn
    (find-file "~/Documents/AA/zoom-meetings-info.txt")
    (find-file "~/Documents/Health/BP-tracking.txt")))
(load "ee-useful")

;; End ee-packages.el
