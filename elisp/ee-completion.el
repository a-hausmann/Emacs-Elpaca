;; -*- lexical-binding: t -*-
;; File name:     ee-completion.el
;; Created:       2023-07-22
;; Last modified: Sun Apr 12, 2026 17:59:06
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
;; 04/11/2026: David's site doesn't exist anymore, and I don't think I need this code as I don't use projectile.
;; (defun aeh/get-project-root ()
;;   (when (fboundp 'projectile-project-root)
;;     (projectile-project-root)))


;; Configure Consult
(use-package consult
  :ensure t
  :demand t
  :bind (
         ("C-s" . isearch-forward)                 ; Still useful, consult has no better solution.
         ("C-c C-r" . isearch-backward)            ; Still useful, consult has no better solution.
         ("C-c C-s" . consult-isearch-forward)     ; works in mini-buffer ONLY!
         ("C-c r" . consult-recent-file)           ; my original binding, doesn't work in mhtml-mode.
         ("M-s r" . consult-recent-file)           ; new, better binding, looks to work everywhere.
         ("C-S-s" . consult-line)                  ; my original binding, a little hard to type.
         ("M-s l" . consult-line)                  ; new, better binding, looks to work everywhere.
         ("M-s i" . consult-imenu)
         ("C-x c i" . consult-imenu)               ; second binding.
         ("C-x b" . consult-buffer)                ; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ; orig. switch-to-buffer-other-frame
         ("C-M-'" . consult-register-store)        ; dwim register: store, append, prepend, delete (prefix arg)
         ("M-'" . consult-register-load)           ; dwim register: insert, jump, or restore (window config)
         ("M-s g" . consult-register)              ; preview & narrow register list.
         ("M-y" . consult-yank-pop)                ; orig. yank-pop
         ("C-x c F" . consult-focus-lines)         ; focus (narrow) text
         ("M-s f" . consult-focus-lines)           ; focus (narrow) text
         ("C-x c f" . consult-find)
         ("C-x c g" . consult-grep)
         ("C-x c G" . consult-git-grep)
         ("C-x c R" . consult-ripgrep)
         ("M-g M-g" . consult-goto-line)           ; with preview, replacing ONE old binding for `goto-line'
         ("M-g m" . consult-mark)                  ; jump to marker in the mark-ring
         ("M-g M" . consult-global-mark)           ; jump to marker in the global mark-ring
         ("M-s x" . consult-xref)                  ; added 04/11/2026
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
  :custom
  ;; (consult-project-root-function #'aeh/get-project-root)  ; I don't think I need this anymore.
  (completion-in-region-function #'consult-completion-in-region)
  :config
  ;; (consult-preview-mode)
  (setq consult-preview-key 'any)
  (setq consult-narrow-key "<")
  (consult-customize
   consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key "M-.")
)


;; Ref: https://github.com/gagbo/consult-lsp
(use-package consult-lsp
  :ensure t
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
  :ensure t
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
         ("<C-backspace>" . dw/minibuffer-backward-kill)
         ;; "C-g" STILL doesn't work here, only "s-g" for same command defined globally works.
         ("C-g" . minibuffer-keyboard-quit)))


;; 2022-08-04: Changes in vertico invalidated the below. Documentation for Projectile
;; indicates it will automatically use the default completion system, in my case, Vertico.
;; Testing showed that I don't need to set this variable at all.
;; (setq projectile-completion-system 'vertico)

;; Use the `orderless' completion style.
;; Enable `partial-completion' for files to allow path expansion.
;; You may prefer to use `initials' instead of `partial-completion'.
;; 02/05/2026: `completion-styles' can have multiple values, so taking a clue
;; from https://robbmann.io/posts/006_emacs_2_python/, but including orderless
;; as well as `flex' which will give fuzzy matching, plus the original values.
(use-package orderless
  :ensure t
  :init
  ;; 02/05/2026: Original was only orderless; worked fine. Added all from reference
  ;; which gave me ODD results. Removing "flex" initially to see if that helps. AND,
  ;; that seems to have helped. Searched for "Schlafly" file but didn't find after
  ;; typing "schla", which should have been enough. Got it finally with "schlaf".
  ;; I think it best to leave out the "flex", which I didn't see in documentation
  ;; but was described in reference as something to give "fuzzy" results.
  ;; (setq completion-styles '(orderless)
  ;; (setq completion-styles '(flex basic orderless partial-completion emacs22)
  ;; 02/06/2026: removed "basic", "emacs22", "flex"
  (setq completion-styles '(orderless partial-completion)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))


;; Persist history over Emacs restarts. Vertico sorts by history position.
;; Became part of Emacs with version 22, so no external package.
(savehist-mode 1)                          ; ALWAYS turn on.
(setq history-length 300)                  ; also see `amx-history-length' 
(setq savehist-save-minibuffer-history t)  ; Default
(setq savehist-additional-variables
      '(kill-ring                          ; clipboard
        register-alist                     ; macros
        mark-ring global-mark-ring         ; marks
        search-ring regexp-search-ring))   ; searches


;; Marginalia provides similar functionality as ivy-rich--which we LOVE!
;; Ref: https://config.daviwil.com/emacs search for Marginalia
;; Do NOT use the =:custom= setting for =marginalia-annotators= as it messes up everything.
(use-package marginalia
    :ensure t
    ;; :after vertico
    ;; :custom
    ;; (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
    :bind (:map minibuffer-local-map
                ("M-a" . marginalia-cycle))
    :init
    (marginalia-mode))


;; Embark is hard to describe, but provides ways to ACT upon completion items.
;; Ref: https://config.daviwil.com/emacs search for Embark

;; Ref: https://github.com/oantolin/embark?tab=readme-ov-file#quick-start
;; Recommended binding "C-." is already used by "evil-repeat-pop" in state: "evil-normal-state-map"
;; I don't EVER use that, so TRYING to use "evil-define-key" to redefine to nil and then
;; reuse binding for embark.  SHIT, that didn't work.

;; (evil-define-key 'normal global (kbd "C-.") nil)

(use-package embark
  :ensure t
  :bind (("s-." . embark-act)
         ("s-," . embark-dwim)
         ("C-h B" . embark-bindings)
         :map minibuffer-local-map
         ("s-." . embark-act))
  :init (setq prefix-help-command #'embark-prefix-help-command)
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



;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t) ; only need to install it, embark loads it after consult if found



(message "Loaded ee-completion.el")

(provide 'ee-completion)

;; End of ee-completion.el
