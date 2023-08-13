;; -*- lexical-binding: t -*-
;; File name:     ee-completion.el
;; Created:       2023-07-22
;; Last modified: Sat Aug 12, 2023 21:08:28
;; Purpose:       Configure all completing-read framework.
;;                As of initial writing, this is: Consult, Vertigo, 
;;                Orderless, Marginalia, and Embark. Also use
;;                savehist, which persists history over restarts.
;;

;; Additional packages for completions and enrichments
;; Consult provides functionality similary to Counsel.
;; Ref: https://github.com/minad/consult
;; Also: https://config.daviwil.com/emacs and search for "Consult Commands"
;; Copy of David's code for projectile functions.
(defun aeh/get-project-root ()
  (when (fboundp 'projectile-project-root)
    (projectile-project-root)))

;; Configure Consult
(use-package consult
  :elpaca t
  :demand t
  :bind (
         ("C-s" . isearch-forward)                 ;; Still useful, consult has no better solution.
         ("C-c C-r" . isearch-backward)            ;; Still useful, consult has no better solution.
         ("C-c C-s" . consult-isearch-forward)     ;; works in mini-buffer ONLY!
         ("C-S-s" . consult-line)
         ("C-M-l" . consult-imenu)
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-M-'" . consult-register-store)        ;; dwim register: store, append, prepend, optionally delete (prefix arg)
         ("M-'" . consult-register-load)           ;; dwim register: insert, jump, or restore (window config)
         ("C-M-#" . consult-register)
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("C-x c r" . consult-recent-file)
         ("C-x c g" . consult-goto-line)           ;; goto specified line
         ("C-x c m" . consult-mark)                ;; jump to marker in the mark-ring
         ("C-x c f" . consult-find)
         ("C-x c g" . consult-grep)
         ("C-x c G" . consult-git-grep)
         ("C-x c l" . consult-line)                  ;; required by consult-line to detect isearch
         :map minibuffer-local-map ("C-r" . consult-history))
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  ;; Enable automatic preview at point in the *Completions* buffer.
  ;; This is relevant when you use the default completion UI,
  ;; and not necessary for Selectrum, Vertico etc.
  ;; :hook (completion-list-mode . consult-preview-at-point-mode)  
  :custom
  (consult-project-root-function #'aeh/get-project-root)
  (completion-in-region-function #'consult-completion-in-region)
  :config
  ;; (consult-preview-mode)
  (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  ;; 07/22/2023: a recent upgrade to Consult resulted in several commands breaking.
  ;; Now, think the below commented line is the culprit, as latest documentation on
  ;; Consult site shows no need to use kbd()--and the error was about key definition.
  ;; using the below setup allows this Emacs-Elpaca to work correctly (so far).
  (setq consult-narrow-key "<") ;; (kbd "C-+")
  (consult-customize
   consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   ;; :preview-key (kbd "M-."))
   :preview-key "M-.")
)
;; Allow Elpaca to process queues up to this point
;; (elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


;; Ref: https://github.com/gagbo/consult-lsp
(use-package consult-lsp
  :elpaca t
  :after lsp-mode
  ;; :config
  ;; (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)
  ;; By all that's holy (https://github.com/jwiegley/use-package#key-binding) this should work but has not.
  ;; :bind (:map lsp-mode-map
  ;; However, merely binding globally instead of to lsp-mode-map does work, so will not need to put in config.
  :bind ([remap xref-find-apropos] . consult-lsp-symbols)
)
;; Allow Elpaca to process queues up to this point
;; (elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


;; Ref: https://github.com/minad/vertico
;; Also, https://config.daviwil.com/emacs and find "vertico"
;; David's custom function to backward kill in minibuffer
(defun dw/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent
folder, otherwise delete a word"
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
      (backward-kill-word arg)))

;; Enable vertico
(use-package vertico
  :elpaca t
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  :custom-face
  (vertico-current ((t (:background "#3a3f5a"))))
  :bind (:map vertico-map
         ("C-n" . vertico-next)
         ("C-p" . vertico-previous)
         ("C-g" . vertico-exit)
         :map minibuffer-local-map
         ("<C-backspace>" . dw/minibuffer-backward-kill)))
;; Allow Elpaca to process queues up to this point
;; (elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


;; 2022-08-04: Changes in vertico invalidated the below. Documentation for Projectile
;; indicates it will automatically use the default completion system, in my case, Vertico.
;; Testing showed that I don't need to set this variable at all.
;; (setq projectile-completion-system 'vertico)

;; Use the `orderless' completion style.
;; Enable `partial-completion' for files to allow path expansion.
;; You may prefer to use `initials' instead of `partial-completion'.
(use-package orderless
  :elpaca t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))
;; Allow Elpaca to process queues up to this point
;; (elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


;; Persist history over Emacs restarts. Vertico sorts by history position.
;; Became part of Emacs with version 22, so no external package.
;; (use-package savehist
;;   :elpaca nil
;;   :init
;;   (savehist-mode))
;; ;; Allow Elpaca to process queues up to this point
;; (elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword
(savehist-mode 1)

;; Marginalia provides similar functionality as ivy-rich--which we LOVE!
;; Ref: https://config.daviwil.com/emacs search for Marginalia

(use-package marginalia
  :elpaca t
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))


;; Embark is hard to describe, but provides ways to ACT upon completion items.
;; Ref: https://config.daviwil.com/emacs search for Embark

(use-package embark
  :elpaca t
  :bind (("C-S-a" . embark-act)
         :map minibuffer-local-map
         ("C-d" . embark-act))
  :config
  ;; Show Embark actions via which-key, this from David 
  ;; (setq embark-action-indicator
  ;;       (lambda (map)
  ;;         (which-key--show-keymap "Embark" map nil nil 'no-paging)
  ;;         #'which-key--hide-popup-ignore-command)
  ;;       embark-become-indicator embark-action-indicator)
;; Ref: https://github.com/oantolin/embark/ and search for "which-key"  
  (setq embark-action-indicator
      (lambda (map _target)
        (which-key--show-keymap "Embark" map nil nil 'no-paging)
        #'which-key--hide-popup-ignore-command)
      embark-become-indicator embark-action-indicator)
)
;; Allow Elpaca to process queues up to this point
;; (elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword

;; End of ee-completion.el
