;; -*- lexical-binding: t -*-
;; File name:     ee-packages.el
;; Created:       2023-07-15
;; Last modified: Wed Jul 29, 2026 17:51:51
;; Purpose:       This is the main package loader/configurator for Emacs-Elpaca
;;

;; Configure delight
(use-package delight
    :ensure t
    :demand)


;; Configure which-key
(use-package which-key
  :ensure t
  :init (which-key-mode)
  ;; :diminish (which-key-mode)
  :delight
  :config
  (setq which-key-idle-delay 0.5))


;; Configure command-log-mode
(use-package command-log-mode
  :ensure t
  :delight
  :commands (command-log-mode)
  :bind ("C-c o" . clm/toggle-command-log-buffer))


;; Configure editing stuff
(require 'ee-editing)


;; Load scripts to set up completion, both auto-complete and completing read.
;; 02/14/2026: trying Corfu instead of Company. Cannot get Elpaca to load it correctly. Shoot!
(require 'config-corfu)

;; 06/01/2026: Lisp error (void-function company-mode), so killed the require of Company.
;; The only other thing referencing "company-mode" is in ee-general.el, which is not called.
;; Weird!
;; (require 'ee-auto-complete)    ; aka Company

(require 'ee-completion)


(use-package flycheck
    :ensure t
    :config
    (add-hook 'after-init-hook #'global-flycheck-mode))


;; Elisp mode
(use-package emacs-lisp-mode
  :ensure nil
  :commands emacs-lisp-mode
  :delight emacs-lisp-mode "Emacs Lisp"
  :config (delight 'lisp-interaction-mode "Lisp Interaction"))
;; 2021-02-21: Package ielm is a repl for emacs lisp, so ONLY load when commanded in.
(use-package ielm
  :ensure nil
  :delight
  :commands ielm
  :hook (ielm-mode . (lambda () (setq-local scroll-margin 0))))
(use-package lisp-mode
  :ensure nil
  :hook (emacs-lisp-mode . lisp-mode)
  :delight lisp-mode "Lisp")
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)


;; All-the-icons
(use-package all-the-icons
  :ensure t
  :delight)


;; Amx is the newer alternative to smex (aka smart M-x). 
(use-package amx
  :ensure t
  :commands (amx amx-major-mode-commands execute-extended-command)
  :delight
  :init 
  (amx-mode t)   ; always in amx-mode
  :config
  (global-set-key (kbd "M-x") 'amx)
  (global-set-key (kbd "M-X") 'amx-major-mode-commands)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c M-x") 'execute-extended-command))
;; (setq-default amx-save-file (no-littering-expand-var-file-name ".amx-items"))


;; Configure Avy; 06/24/2026: changed bindings and added more stuff. Thanks, Karthinks!
;; Ref: https://karthinks.com/software/avy-can-do-anything/
(use-package avy
    :ensure t
    :commands avy-goto-char-timer
    :delight
    :bind
    ("C-x C-a l" . avy-copy-line)
    ("C-x C-a r" . avy-copy-region)
    ("C-x C-a M-l" . avy-move-line)
    ("C-x C-a M-r" . avy-move-region)
    ("C-x C-a t" . avy-goto-char-timer)
    ("C-x C-t" . avy-goto-char-timer)
    ("M-s j" . avy-goto-char-timer)
    :config
      (setf (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
            (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line
            (alist-get ?t avy-dispatch-alist) 'avy-action-teleport
            (alist-get ?t avy-dispatch-alist) 'avy-action-teleport-whole-line)
    (setq avy-timeout-seconds 0.5))

(defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

(defun avy-action-teleport-whole-line (pt)
  (avy-action-kill-whole-line pt)
  (save-excursion (yank)) t)

;; Configure Aggressive-indent, works well with Emacs-lisp, not that well with other languages (Python?)
(use-package aggressive-indent
  :ensure t
  :delight
  :hook (emacs-lisp-mode . aggressive-indent-mode))


;; allow asynchronous processing wherever possible…pretty nice.
(use-package async
  :ensure t
  :demand
  :delight
  :config
     (dired-async-mode 1))
;; Allow Elpaca to process queues up to this point
;; (elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


;; Configure beacon
(use-package beacon
  :ensure t
  :demand
  :delight
  :config
  (beacon-mode 1))
;; Allow Elpaca to process queues up to this point
;; (elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


;; Configure column-enforce-mode
(use-package column-enforce-mode
  :ensure t
  :delight
  :hook (prog-mode . column-enforce-mode)
  :config (setq column-enforce-comments nil))
;; Allow Elpaca to process queues up to this point
;; (elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


;; Load the dired configuration file. So much going on there, it deserves its own config file.
(require 'ee-dired)


;; Configure Garbage Collector Magic Hack
(use-package gcmh
  :delight
  :defer 1
  :config
  (gcmh-mode 1))


;; Configure Helpful help commands
;; 05/23/2026: helpful is NOT loading, comment for now.
;; 06/01/2026: it is now!
(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))


;; Configure HTMLize
(use-package htmlize
  :defer 2
  :commands aeh-html-stuff-mode
  :delight)

;; 2020-09-07: adding custom package; 2020-09-08: make non-Windows (not work)
;; (cond ((not (string-equal system-type "windows-nt"))
;;        (load "aeh-html-stuff")
;;        ;; (require 'aeh-html-stuff)
;;        (add-hook 'html-mode-hook 'aeh-html-stuff-mode)))
;; 07/23/2026: Do this better! If the file exists, load it plus hook, else don't even try.
(when (file-readable-p (expand-file-name "elisp/aeh-html-stuff.el" user-emacs-directory))
  (require 'aeh-html-stuff)
  (add-hook 'html-mode-hook 'aeh-html-stuff-mode))


;; Configure Minions
(use-package minions
  :defer 1
  :delight
  :config (minions-mode 1))


;; Configure wgrep/Ripgrep.
(use-package wgrep
  :ensure nil
  :defer 1
  :delight)

;; Configure Ripgrep Emacs interface.
(use-package rg
    :ensure t
    :config
    (rg-enable-menu)   ; THIS enables the transient menu; forget the other options.
    ;; 03/26/2024: added from Prot video.
    (setq rg-group-result t
          rg-hide-command t
          rg-show-columns nil
          rg-show-header t
          rg-custom-type-aliases nil
          rg-default-alias-fallback "all"
          rg-show-columns t)

    :bind (("M-s R" . rg)               ; global binding for getting right to rg
           ("M-s M" . rg-menu)
           :map aeh-html-stuff-mode-map
           ("C-c C-c s" . rg-menu)))    ; "C-c s" was already used, so redefine for this map.


;; 04/26/2026: add `key-chord' package, ref: https://github.com/emacsorphanage/key-chord
;; Allow definition of non-modifier-key based chords, like ";;" or "hj".
;; Keys must be typed quickly
;; Command `key-chord-describe' lists currently defined key chords
;; 
(use-package key-chord
    :ensure t
    :demand
    ;; :init (key-chord-mode 1)
    :custom
    (customize-set-variable key-chord-two-keys-delay 0.1)
    (customize-set-variable key-chord-one-key-delay 0.1)
    (customize-set-variable key-chord-one-key-min-delay 0.0)
    :config
    (key-chord-mode 1)
    ;; Define global key chords
    ;; (key-chord-define-global "''" "`'\C-b")
    ;; Define key chords to specific keymaps
    (key-chord-define prog-mode-map ";," 'indent-for-comment)
    (key-chord-define prog-mode-map ";;" 'comment-line)
    (key-chord-define prog-mode-map "JJ" 'reindent-then-newline-and-indent)
    (key-chord-define lisp-mode-map "``" "`'\C-b")  ; Emacs Lisp `quote'
    )


;; 04/27/2026: Configure `hideshow' mode, native to Emacs.
;; This will perform some rudimentary code folding without markers.
(use-package hideshow
    :ensure nil
    :config
  (defun hs-cycle (&optional level)
    (interactive "p")
    (let (message-log-max
          (inhibit-message t))
      (if (= level 1)
          (pcase last-command
            ('hs-cycle
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))
            ('hs-cycle-children
             ;; TODO: Fix this case. `hs-show-block' needs to be
             ;; called twice to open all folds of the parent
             ;; block.
             (save-excursion (hs-show-block))
             (hs-show-block)
             (setq this-command 'hs-cycle-subtree))
            ('hs-cycle-subtree
             (hs-hide-block))
            (_
             (if (not (hs-already-hidden-p))
                 (hs-hide-block)
                 (hs-hide-level 1)
                 (setq this-command 'hs-cycle-children))))
          (hs-hide-level level)
          (setq this-command 'hs-hide-level))))

  (defun hs-global-cycle ()
    (interactive)
    (pcase last-command
      ('hs-global-cycle
       (save-excursion (hs-show-all))
       (setq this-command 'hs-global-show))
      (_ (hs-hide-all))))

  (defun hs-cycle-key-bindings ()
    (keymap-set hs-minor-mode-map "C-c C-<tab>" 'hs-cycle)
    (keymap-set hs-minor-mode-map "C-c C-M-<tab>" 'hs-global-cycle))

  (add-hook 'hs-minor-mode-hook 'hs-cycle-key-bindings))
;; End of hideshow.


;; 07/26/2026: The `use-package' is NOT working so try another way. Will add the
;; local git repo for this package to the load path. This works, but NOTE that
;; I will ALWAYS have to hard code the path until I can get Elpaca to use Gitlab.
(let ((my-menus "~/git/my-menus/"))
  (when (file-readable-p my-menus)
    (add-to-list 'load-path my-menus)
    (use-package my-menus
        ;; :ensure (:host gitlab :repo "a-hausmann/my-menus")
        :ensure nil)))


;; 07/29/2026: For some reason I never added dumb-jump to my home config (I have at work).
;; Ref: https://github.com/jacktasia/dumb-jump  Use recommended `use-package' example.
(use-package dumb-jump
    :ensure t
    :custom
    (dumb-jump-prefer-searcher 'rg)
    (xref-show-definitions-function #'consult-xref)
    :config
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))


;; Configure org mode
(load "ee-org.el")


;; Load my "useful" functions.
(load "ee-useful")

;; End ee-packages.el
