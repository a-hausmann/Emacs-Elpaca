;; -*- lexical-binding: t -*-
;; File name:     ee-final.el
;; Created:       2023-07-30
;; Last modified: Sat Apr 11, 2026 23:29:06
;; Purpose:       Perform things/functions which need to be done last.
;;

;; Allow Elpaca to process queues up to this point
(elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword

(defalias 'list-buffers 'ibuffer) ; make ibuffer default

;; Remap some keys

;; Global mappings
;; 10/20/2023: Found I CAN use "opt/start" as Super key in Emacs.  This is GREAT!
;; 03/06/2026: After an OS upgrade, the "opt/start" (aka Super) is TAKEN OVER by KDE Plasma.
;; 01/17/2024: Sure, but it doesn't work all the time, find another.

;; Need to unset global "C-y" from an evil binding so I can reuse it.
;; (keymap-global-unset "C-y")       ; Unset the Evil keybinding from evil-scroll-line-up
;; For SOME reason, the "keymap-global-unset" will NOT work so using the old form.
;; (global-unset-key (kbd "C-y"))
;; (keymap-global-set "C-y" 'yank)

;; FIXME: Redo these with latest function, "keymap-global-set"
(keymap-global-set "C-y" #'yank)   ; need to do this here else `yank' not available in query-replace.
(keymap-global-set "C-c c" #'calendar)
(keymap-global-set "C-M-+" #'text-scale-adjust)

;; 04/11/2026: `bookmark-bmenu-list' is already in "C-x r" map.
;; (keymap-global-set "C-c l" #'bookmark-bmenu-list)


;; FIXME: Redo these with latest function, "keymap-global-set"
(general-def
  "C-c C" 'capitalize-word
  "C-c U" 'upcase-word
  "C-c D" 'downcase-word
  "M-c" 'capitalize-dwim
  "M-u" 'upcase-dwim
  "M-l" 'downcase-dwim
  "C-x C-y" 'clipboard-yank)


;; 03/09/2026: Unset "C-j" ("electric-newline-and-maybe-indent") which I don't use; reuse as prefix key.
;; (keymap-global-unset "C-j")
(keymap-global-set "M-o" 'other-window)
;; `eval-print-last-sexp' is too valuable to lose and `electric-pairs' steals the default binding.
(keymap-set lisp-mode-map "M-j" 'eval-print-last-sexp)
(keymap-global-set "s-j d" 'duplicate-dwim)


;; Dired mappings, set after evil
(eval-after-load 'evil
  '(progn
     (evil-set-initial-state 'dired-mode 'normal)
     (general-define-key
      :states 'normal
      :keymaps 'dired-mode-map
      "(" 'dired-hide-details-mode
      "j" 'dired-next-line
      "k" 'dired-previous-line
      "h" 'dired-up-directory
      "H" 'dired-hide-dotfiles-mode
      "l" 'dired-find-alternate-file
      "o" 'dired-find-file-other-window
      "s" 'dired-sort-toggle-or-edit
      "v" 'dired-toggle-marks
      "m" 'dired-mark
      "u" 'dired-unmark
      "U" 'dired-unmark-all-marks
      "c" 'dired-create-directory
      "q" 'kill-this-buffer
      "gg" 'revert-buffer
      "M-s" 'avy-goto-char-timer
      "W" 'evil-forward-WORD-begin
      "B" 'evil-backward-WORD-begin
      "E" 'evil-forward-WORD-end
      ")" 'dired-git-info-mode
      "n" 'dired-next-line
      "p" 'dired-previous-line
      "SPC" nil
)
     ;; (define-key dired-mode-map "SPC" nil)
))

;; Dired mappings, set after evil
(eval-after-load 'evil
  (progn
    (general-define-key
     :states 'normal
     :keymaps '(prog-mode-map origami-mode-map)
     "TAB" 'aeh/origami-toggle-node)
    (general-define-key
     :states 'normal
     "za" 'origami-forward-toggle-node
     "zR" 'origami-close-all-nodes
     "zM" 'origami-open-all-nodes
     "zr" 'origami-close-node-recursively
     "zm" 'origami-open-node-recursively
     "zo" 'origami-show-node
     "zc" 'origami-close-node
     "zh" 'origami-forward-fole
     "zk" 'origami-previous-fold)
    (general-define-key
     :states '(insert replace)
     "C-d" nil)
    ))

;; 02/10/2024: Delight the minor modes which show up in Org docs...somehow.
(delight '((abbrev-mode nil t)
           (evil-collection-unimpaired-mode nil t)
           (evil-commentary-mode nil t)
           (org-indent-mode nil t)
           (subword-mode nil t)
           (visual-line-mode nil t)
           (yas-minor-mode nil t)
           )) 


;; 10/05/2023: have tried too many things; NOTHING loads these TXT files
;; and have TAB mapped to toggle origami folding, despite the file-local-variable
;; I ALWAYS have to kill the buffer and reload from disk, or eval the sexp,
;; "(ah--set-origami-fold-style-braces)", which is the file-local-variable.
(setq aeh-start-files '(
                        "~/Documents/org/Premier-League-2024-watched.org"
                        "~/Documents/Health/Weight-tracker.org"
                        "~/Documents/AA/zoom-meetings-info.txt"
                        "~/Documents/Health/BP-tracking.txt"
                        "~/Documents/Health/UO-tracking.txt"
                        ;; "~/git/Emacs-Elpaca/elisp/my-modeline.el"
                        ))
(mapcar 'find-file aeh-start-files)

;; 04/21/2024: undefine some Evil bindings to replace with Emacs.
(general-unbind 'insert
    "C-k"    ;; Evil: evil-insert-digraph
  )
(general-def 'insert
    "C-k" 'evil-delete-line) ;; Use evil-delete-line as kill-line is remapped to sp-kill-hybrid-sexp.

(aeh/command-of-the-day)

;; (smartparens-mode 1) ; 07/14/2025: added after noticing playing with SP had turned it off somehow.

(provide 'ee-final)

;; End of ee-final.el
