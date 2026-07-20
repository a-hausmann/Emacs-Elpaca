;; -*- coding: utf-8; lexical-binding: t -*-
;; File name:     config-corfu.el
;; Created:       2026-02-14
;; Last modified: Mon Jul 20, 2026 10:24:47
;; Purpose:       Corfu configuration.
;;

;; Ref: https://github.com/minad/corfu?tab=readme-ov-file#configuration
;; 07/20/2026: Ref: https://protesilaos.com/codelog/2026-07-19-emacs-completion-at-point-functions/
(use-package corfu
    ;; :ensure (:host github :repo "minad/corfu")
    :ensure t
    ;; Optional customizations
    :custom
    (corfu-cycle t) ;; Enable cycling for `corfu-next/previous'
    (corfu-preselect 'prompt)
    ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
    ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
    ;; (corfu-preview-current nil)    ;; Disable current candidate preview
    ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
    ;; (corfu-on-exact-match 'insert) ;; Configure handling of exact matches

    ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
    ;; :hook ((prog-mode . corfu-mode)
    ;;        (shell-mode . corfu-mode)
    ;;        (eshell-mode . corfu-mode))

    ;; Seems to work best without additional bindings. Use standard "C-n" and "C-p"
    ;; to run through selection list rather than arrows. If NO TEXT CONVERSION is
    ;; needed (i.e., capitalization, etc.) then selected text just needs SPC or
    ;; punctuation to insert. Do NOT use TAB or RET. 
    ;; :bind

    :init

    ;; Recommended: Enable Corfu globally.  Recommended since many modes provide
    ;; Capfs and Dabbrev can be used globally (M-/).  See also the customization
    ;; variable `global-corfu-modes' to exclude certain modes.
    (global-corfu-mode)
    ;; Enable optional extension modes:
    (corfu-history-mode)
    (corfu-popupinfo-mode)

    :config
    (setq corfu-popupinfo-delay '(1.25 . 0.5))
    (corfu-popupinfo-mode 1)  ; show documentation after `corfu-popupinfo-delay'
    (keymap-set corfu-map "<tab>" #'corfu-complete)
    ;; Sort by input history; no need to modify `corfu-sort-function'
    (with-eval-after-load 'savehist
      (corfu-history-mode 1)
      (add-to-list 'savehist-additional-variables 'corfu-history))
    )

;; Enable auto completion, configure delay, trigger and quitting
(setq corfu-auto t
      corfu-auto-prefix 2    ; popup after typing n characters.
      corfu-auto-delay 0.2
      corfu-auto-trigger "." ; Custom trigger characters
      corfu-quit-no-match 'separator)
;; Enable Corfu in all minibuffers as long as no completion UI is active.
(setq global-corfu-minibuffer
      (lambda ()
        (not (or (bound-and-true-p mct--active)
                 (bound-and-true-p vertico--input)
                 (eq (current-local-map) read-passwd-map)))))

;; (defun orderless-fast-dispatch (word index total)
;;   (and (= index 0) (= total 1) (length< word 4)
;;        (cons 'orderless-literal-prefix word)))
;; (orderless-define-completion-style orderless-fast
;;                                    (orderless-style-dispatchers '(orderless-fast-dispatch))
;;                                    (orderless-matching-styles '(orderless-literal orderless-regexp)))
;; (add-hook 'corfu-mode-hook
;;           (lambda ()
;;             (setq-local completion-styles '(orderless-fast basic)
;;                         completion-category-overrides nil
;;                         completion-category-defaults nil)))

;; A few more useful configurations...
(use-package emacs
  :ensure nil
  :custom
  ;; TAB cycle if there are only few candidates
  (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  ;; 06/25/2026: commented as back on 29.2 for time being.
  ;; (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))


;; Do NOT use dabbrev with corfu configuration binding, but use the rest.
;; I already set "M-/" to hippie-expand, and "C-M-/" is dabbrev-completion.
;; Use Dabbrev with Corfu!
(use-package dabbrev
  :ensure nil
  ;; Swap M-/ and C-M-/
  ;; :bind (("M-/" . dabbrev-completion)
  ;;        ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'authinfo-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))


;; 06/25/2026: Looks like I need to install Cape to get actually completion at point popups.
;; FINALLY, it works like it does at work. I probably installed the Cape package there already.
;; Add extensions
;; 07/20/2026: Ref: https://protesilaos.com/codelog/2026-07-19-emacs-completion-at-point-functions/
(use-package cape
  :after corfu
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
  :config

  ;; This is for the global value.
  (setq completion-at-point-functions '(cape-dabbrev cape-file))

  (defun prot/cape-super-set-local (capfs &optional individual-capfs)
    "Set `completion-at-point-functions' to current value plus CAPFS.
Treat CAPFS and the default value as a super CAPF.  Then append the
INDIVIDUAL-CAPFS to the list."
    (let* ((all-for-super (append completion-at-point-functions capfs))
           (all-minus-global (delq t all-for-super))
           (cape-super (apply #'cape-capf-super all-minus-global)))
      (setq-local completion-at-point-functions (append (list cape-super) individual-capfs (list t)))))

  (defun prot/cape-prog-setup ()
    "Set up Cape for programming."
    (prot/cape-super-set-local '(cape-dabbrev) '(cape-file)))

  (add-hook 'prog-mode-hook #'prot/cape-prog-setup)

  (defun prot/cape-text-setup ()
    "Set up Cape for prose."
    (prot/cape-super-set-local '(cape-dict cape-dabbrev cape-emoji) '(cape-file)))

  (add-hook 'text-mode-hook #'prot/cape-text-setup)
)


(message "config-corfu completed.")

(provide 'config-corfu)

;; End of config-corfu.el
