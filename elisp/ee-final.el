;; -*- lexical-binding: t -*-
;; File name:     ee-final.el
;; Created:       2023-07-30
;; Last modified: Sun Jul 30, 2023 17:15:08
;; Purpose:       Perform things/functions which need to be done last.
;;


(defalias 'list-buffers 'ibuffer) ; make ibuffer default

;; Remap some keys

;; Global mappings
(general-define-key
 "C-c c" 'calendar
 "C-c r l" 'bookmark-bmenu-list)

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

(eval-after-load 'origami
  '(progn
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
      "zk" 'origami-previous-fold)))
