;; -*- lexical-binding: t -*-
;; File name:     ee-final.el
;; Created:       2023-07-30
;; Last modified: Wed Jun 17, 2026 22:24:29
;; Purpose:       Perform things/functions which need to be done last.
;;


(defalias 'list-buffers 'ibuffer) ; make ibuffer default

;; Remap some keys

;; Need to unset global "C-y" from an evil binding so I can reuse it.
;; (keymap-global-unset "C-y")       ; Unset the Evil keybinding from evil-scroll-line-up
;; For SOME reason, the "keymap-global-unset" will NOT work so using the old form.
(global-unset-key (kbd "C-y"))
(keymap-global-set "C-y" 'yank)

;; FIXME: Redo these with latest function, "keymap-global-set"
(keymap-global-set "C-y" #'yank)   ; need to do this here else `yank' not available in query-replace.
(keymap-global-set "C-c c" #'calendar)
(keymap-global-set "C-M-+" #'text-scale-adjust)

(keymap-global-set "C-c C" #'capitalize-word)
(keymap-global-set "C-c U" #'upcase-word)
(keymap-global-set "C-c D" #'downcase-word)
(keymap-global-set "M-c" #'capitalize-dwim)
(keymap-global-set "M-u" #'upcase-dwim)
(keymap-global-set "M-l" #'downcase-dwim)
(keymap-global-set "C-x C-y" #'clipboard-yank)

;; 03/09/2026: Unset "C-j" ("electric-newline-and-maybe-indent") which I don't use; reuse as prefix key.
;; (keymap-global-unset "C-j")
(keymap-global-set "M-o" 'other-window)
;; `eval-print-last-sexp' is too valuable to lose and `electric-pairs' steals the default binding.
(keymap-set lisp-mode-map "M-j" 'eval-print-last-sexp)


;; Dired mappings, set after evil
;; 06/02/2026: These are already set in "ee-dired.el", no need to set again.
;; (eval-after-load 'evil
;;   '(progn
;;      (evil-set-initial-state 'dired-mode 'normal)
;;      (general-define-key
;;       :states 'normal
;;       :keymaps 'dired-mode-map
;;       "(" 'dired-hide-details-mode
;;       "j" 'dired-next-line
;;       "k" 'dired-previous-line
;;       "h" 'dired-up-directory
;;       "H" 'dired-hide-dotfiles-mode
;;       "l" 'dired-find-alternate-file
;;       "o" 'dired-find-file-other-window
;;       "s" 'dired-sort-toggle-or-edit
;;       "v" 'dired-toggle-marks
;;       "m" 'dired-mark
;;       "u" 'dired-unmark
;;       "U" 'dired-unmark-all-marks
;;       "c" 'dired-create-directory
;;       "q" 'kill-this-buffer
;;       "g" 'revert-buffer
;;       "M-s" 'avy-goto-char-timer
;;       "W" 'evil-forward-WORD-begin
;;       "B" 'evil-backward-WORD-begin
;;       "E" 'evil-forward-WORD-end
;;       ")" 'dired-git-info-mode
;;       "n" 'dired-next-line
;;       "p" 'dired-previous-line
;;       "SPC" nil
;; )
;;      ;; (define-key dired-mode-map "SPC" nil)
;; ))

;; Dired mappings, set after evil
;; 06/02/2026: Don't really use origami anymore, and don't want to do the "C-d" here either.
;; (eval-after-load 'evil
;;   (progn
;;     (general-define-key
;;      :states 'normal
;;      :keymaps '(prog-mode-map origami-mode-map)
;;      "TAB" 'aeh/origami-toggle-node)
;;     (general-define-key
;;      :states 'normal
;;      "za" 'origami-forward-toggle-node
;;      "zR" 'origami-close-all-nodes
;;      "zM" 'origami-open-all-nodes
;;      "zr" 'origami-close-node-recursively
;;      "zm" 'origami-open-node-recursively
;;      "zo" 'origami-show-node
;;      "zc" 'origami-close-node
;;      "zh" 'origami-forward-fole
;;      "zk" 'origami-previous-fold)
;;     (general-define-key
;;      :states '(insert replace)
;;      "C-d" nil)
;;     ))

;; 02/10/2024: Delight the minor modes which show up in Org docs...somehow.
(defun delight-additional-modes ()
  "Need to execute `delight' on additional modes even after all the
`use-package' statements. Need to put this into a function which can
be evaluated on the `elpaca-after-init-hook'."
  (interactive)
  (progn
    ;; (delight '((abbrev-mode nil t)
    ;;            (evil-collection-unimpaired-mode nil t)
    ;;            (evil-commentary-mode nil t)
    ;;            (org-indent-mode nil t)
    ;;            (subword-mode nil t)
    ;;            (visual-line-mode nil t)
    ;;            (yas-minor-mode nil t)
    ;;            (yas-global-mode nil t)
    ;;            (outline-minor-mode nil t)
    ;;            (eldoc-mode nil t)))
    (delight 'abbrev-mode nil t)
    (delight 'evil-collection-unimpaired-mode nil t)
    (delight 'evil-commentary-mode nil t)
    ;; 
    ;; (delight 'org-indent-mode nil t) 
    (delight 'subword-mode nil t)
    (delight 'visual-line-mode nil t)
    (delight 'yas-minor-mode nil t)
    (delight 'yas-global-mode nil t)
    (delight 'outline-minor-mode nil t)
    (delight 'eldoc-mode nil t)
    ))

(add-hook 'elpaca-after-init-hook 'delight-additional-modes)


;; (eval-after-load 'delight
;;   '(progn
;;     (delight '((abbrev-mode nil t)
;;                (evil-collection-unimpaired-mode nil t)
;;                (evil-commentary-mode nil t)
;;                (org-indent-mode nil t)
;;                (subword-mode nil t)
;;                (visual-line-mode nil t)
;;                (yas-minor-mode nil t)
;;                (yas-global-mode nil t)
;;                (outline-minor-mode nil t)
;;                (eldoc-mode nil t)
;;                ))))


;; 04/21/2024: undefine some Evil bindings to replace with Emacs.
;; (general-unbind 'insert
;;     "C-k"    ;; Evil: evil-insert-digraph
;;   )
;; (general-def 'insert
;;     "C-k" 'evil-delete-line) ;; Use evil-delete-line as kill-line is remapped to sp-kill-hybrid-sexp.

;; (aeh/command-of-the-day)

;; (smartparens-mode 1) ; 07/14/2025: added after noticing playing with SP had turned it off somehow.

(provide 'ee-final)

;; End of ee-final.el
