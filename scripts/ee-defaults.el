;; -*- lexical-binding: t -*-
;; File name:     ee-defaults.el
;; Created:       2023-07-22
;; Last modified: Tue Apr 07, 2026 21:01:45
;; Purpose:       Set default values.
;;

;; Mostly taken from my original myDotemacs configuration.

;; Set user variables
(setq user-full-name "Arnold Hausmann")
(if (string-equal system-type "windows-nt")
    (setq user-mail-address "Arnold.Hausmann@trinity-health.org")
  (setq user-mail-address "aehjr1@gmail.com"))

;; "Better" defaults, ref: https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org
(setq-default
 ;; ad-redefinition-action 'accept                   ; Silence warnings for redefinition
 auto-window-vscroll nil                          ; Lighten vertical scroll  (in NEW)
 confirm-kill-emacs 'yes-or-no-p                  ; Confirm before exiting Emacs
 cursor-in-non-selected-windows nil               ; Hide the cursor in inactive windows (in NEW)
 delete-by-moving-to-trash t                      ; Delete files to trash (in NEW)
 ;; display-time-default-load-average nil            ; Don't display load average  (in NEW)
 display-time-format "%H:%M"                      ; Format the time string
 fill-column 80                                   ; Set width for automatic line breaks  (in NEW)
 garbage-collection-messages t                    ; set to non-nil to see GC messages.
 help-window-select t                             ; Focus new help windows when opened
 indent-tabs-mode nil                             ; Stop using tabs to indent (in NEW)
 inhibit-startup-screen t                         ; Disable start-up screen
 mouse-yank-at-point t                            ; Yank at point rather than pointer
 ;; ns-use-srgb-colorspace nil                       ; Don't use sRGB colors
 ;; recenter-positions '(5 top bottom)               ; Set re-centering positions
 scroll-conservatively most-positive-fixnum       ; Always scroll by one line  (in NEW)
 scroll-margin 0                                  ; Add a margin when scrolling vertically  (in NEW)
 sentence-end-double-space nil                    ; End a sentence after a dot and a space (in NEW)
 show-trailing-whitespace nil                     ; Display trailing whitespaces
 ;; split-height-threshold nil                       ; Disable vertical window splitting
 ;; split-width-threshold nil                        ; Disable horizontal window splitting
 tab-width 4                                      ; Set width for tabs (in NEW)
 ;; uniquify-buffer-name-style 'forward              ; Uniquify buffer names
 window-combination-resize t                      ; Resize windows proportionally
 x-stretch-cursor t                               ; Stretch cursor to the glyph width (in NEW)
 ;; Seen to need both of these. The "visual" shows relative visual lines where a fold is treated as one line.
 display-line-numbers-type 'relative              ; Display relative line#, works with folding. (in NEW)
 display-line-numbers 'visual                     ; Display relative line# based on VISUAL lines. (in NEW)

 ;; 2019-08-30: Found in Oleh Krehel's init.el.
 recentf-max-saved-items 100                      ; abo-abo sets to 600, but I'm cautious. (in NEW)
 )

(delete-selection-mode 1)                         ; Replace region when inserting text
(display-time-mode 1)                             ; Enable time in the mode-line
(fringe-mode 0)                                   ; Disable fringes
(fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n  (in NEW)
(global-subword-mode 1)                           ; Iterate through CamelCase words
(menu-bar-mode 1)                                 ; Enable the menu bar
(tool-bar-mode -1)                                ; Disable the tool bar
(scroll-bar-mode -1)                              ; Disable to scroll bar
;; DO NOT USE option "banish" as this conflicts with Windows, at least Windows 10, as if you drag
;; the frame to the corner it demands to take half the screen and will not enable pulling out to
;; to middle of screen.  Most disconcerting!
(mouse-avoidance-mode 'animate)                   ; Avoid collision of mouse with point KEEP ON ANIMATE
(put 'downcase-region 'disabled nil)              ; Enable downcase-region
(put 'upcase-region 'disabled nil)                ; Enable upcase-region
(setq dired-dwim-target t)                        ; Allow direct to dwim target of move, copy commands (in NEW)
(setq make-backup-files nil)                      ; Disable backup files (in NEW)
(setq auto-save-default nil)                      ; Disable auto-save funtionality (in NEW)
;; (put 'narrow-to-region 'disabled nil)             ; I don't think I need this line
(setq inhibit-startup-message t)                  ; Using dashboard
(setq ring-bell-function 'ignore)                 ; Disable bell (in NEW)
(show-paren-mode 1)                               ; Show matching parentheses (in NEW)
(global-visual-line-mode 1)                       ; I like this, so set globally.
(recentf-mode 1)                                  ; Turn on recent files
(global-auto-revert-mode 1)                       ; 06/23/2024: turn this on to automatically revert buffers when file changed.


;; 10/20/2023: Configure settings/bindings for kill-ring & clipboard
;; Now, C-w/C-y will kill-ring-save & yank from kill-ring,
;; and s-w/s-y will save/yank from system clipboard. No 3-finger chords required.
(setq kill-ring-max 100)                             ; Keep up to 100 entries in kill-ring
(setq save-interprogram-paste-before-kill 4096)      ; Save up to 4K of clipboard to kill-ring
(setq select-enable-clipboard nil )                  ; Keep kill-ring and system clipboard separate
(setq x-select-enable-clipboard-manager t)           ; Emacs will transfer clipboard contents to system clipboard
;; 03/06/2026: Updated OS and it has taken over the Super key.
;; 03/18/2026: GOT IT BACK. Open System Settings -> Shortcuts, change bindings under KWin.
(keymap-global-set "s-w" 'clipboard-kill-ring-save)
(keymap-global-set "s-y" 'clipboard-yank)

;; For SOME reason, using the "keymap-global-X" commands don't work well in the Evil environment.
;; Going back to the OLD WAY of doing things, which unfortunately means using General.
;; (keymap-global-unset (kbd "C-y"))      ; Unset the Evil keybinding from evil-scroll-line-up
;; (keymap-global-unset "C-y")            ; Unset the Evil keybinding from evil-scroll-line-up
;; (keymap-global-set "C-y" 'yank)        ; Redefine to standard Emacs yank.
(global-unset-key (kbd "C-y"))                       ; Unset the Evil keybinding from evil-scroll-line-up
(general-define-key                                  ; Redefine to standard Emacs yank.
 :states '(insert normal)
 "C-y" 'yank)

;; Set UTF-8 encoding
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


;; Will now use C-1…10 and M-1…10 however I see fit, they can now be reassigned.
;; Ref: http://pragmaticemacs.com/emacs/use-your-digits-and-a-personal-key-map-for-super-shortcuts/
;; 02/13/2026: Have decided to NOT do this anymore. I don't have these mapped to anything anyway,
;; and I can use these keys as numeric arguments later.
;; (dotimes (n 10)
;;   (global-unset-key (kbd (format "C-%d" n)))
;;   (global-unset-key (kbd (format "M-%d" n))))


;; Hooks
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(when window-system (add-hook 'prog-mode-hook 'hl-line-mode))


;; Set default browsers.
;; Since cannot get Brave working in Windows, use EWW instead
(setq aeh-default-browser "brave-browser")
(if (string-equal system-type "windows-nt")
    (setq browse-url-browser-function 'eww-browse-url)
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program aeh-default-browser))

;; 12/28/2025: added customizable options.
;; Ref: https://codeberg.org/ashton314/emacs-bedrock/src/branch/main/init.el
;; Move thru windows with SHIFT-arrow keys. Changed to `control', both conflict
;; with Org, so use M-S arrow keys instead.
;; 03/18/2026: since updating KWin bindings, can use Super key here.
(keymap-global-set "s-<left>" 'windmove-left)
(keymap-global-set "s-<right>" 'windmove-right)
(keymap-global-set "s-<up>" 'windmove-up)
(keymap-global-set "s-<down>" 'windmove-down)
(keymap-global-set "C-s-<left>" 'windmove-swap-states-left)
(keymap-global-set "C-s-<right>" 'windmove-swap-states-right)
(keymap-global-set "C-s-<up>" 'windmove-swap-states-up)
(keymap-global-set "C-s-<down>" 'windmove-swap-states-down)

;; (keymap-global-set "C-c <left>" 'winner-undo)  ; not a command?
;; (keymap-global-set "C-c <right>" 'winner-redo) ; not a command?
(setopt windmove-wrap-around t)
;; (setq windmove-default-keybindings #'(meta shift))
;; (setq windmove-swap-states-default-keybindings #'(control meta))


;; minibuffer completion settings
(setopt enable-recursive-minibuffers t)                ; Use the minibuffer when in the minibuffer
(setopt completion-cycle-threshold 1)                  ; TAB cycles candidates
(setopt completions-detailed t)                        ; Show annotations
(setopt tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent
(setopt completion-auto-help 'always)
(setopt completions-max-height 20)
(setopt completions-format 'one-column)                ; Show completions in single column
(setopt completions-group t)                           ; Group completions (if possible)
(setopt completion-auto-select t)                      ; First TAB opens *Completions* window and selects it

;; Interface enhancements
(setopt switch-to-buffer-obey-display-actions t)       ; Make switching buffers more consistent

;; Prose-friendly behavior. This also makes my HTML align.
(when (>= emacs-major-version 30)       ; compat test
  (add-hook 'text-mode-hook 'visual-wrap-prefix-mode))

;; 01/13/2026: add code to use hippie-expand over dabbrev "M-/" command
;; NOTE: the format for "remap" is different between global-key-set
;; and keymap-global-set commands.
;; (global-set-key [remap dabbrev-expand] 'hippie-expand)  ; original
(keymap-global-set "<remap> <dabbrev-expand>" 'hippie-expand)

;; 02/13/2026: Isearch improvements. Show current/total matches, and more.
(setq isearch-lazy-count t)
(setq isearch-allow-motion t)
(setq lazy-count-prefix-format "(%s/%s) ")
(setq lazy-count-suffix-format nil)

;; Make regular Isearch interpret a space like Consult, IOW
;; a non-greedy regular expression.
(setq search-whitespace-regexp ".*?")
;; (setq search-whitespace-regexp "[ 	]+")  ;; original value.

;; Ref: https://www.youtube.com/watch?v=1-UIzYPn38s
;; Ref: https://protesilaos.com/emacs/dotemacs#h:50f8b1e4-b14e-453f-a37e-1c0e495ab80f
;; Add more entries as I come across them.
(setq display-buffer-alist
      '(
        ;; Anatomy of an entry
        ;; (BUFFER-NAME-REGEX
        ;;  LIST-OF-DISPLAY-FUNCTIONS
        ;;  &optional PARAMETERS)
        ("\\*Occur\\*"  ; regex
         ;; list of display functions
         (display-buffer-reuse-mode-window
          display-buffer-below-selected)
         ;; parameter(s)
         (window-height . fit-window-to-buffer) ; size window to output
         (dedicated . t)                        ; dedicate window to output
         (body-function . select-window))       ; make active window
        ))


;; 03/28/2026: Ediff settings, ref: https://www.youtube.com/watch?v=pSvsAutseO0
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; 12/28/2025: END customizable options.


;; 04/06/2026: Use repeat-mode and more. Ref: https://www.youtube.com/watch?v=AG_OB3CiPnI
(repeat-mode 1)
(use-package text-mode
    :ensure nil
    :hook (text-mode . display-fill-column-indicator-mode))

(use-package emacs-lisp-mode
    :ensure nil
    :hook (emacs-lisp-mode . flycheck-mode))

;; 04/07/2026: Some keybinding changes, `keyboard-quit' issues and `suspend-frame';
(keymap-global-unset "C-z") ; getting rid of the `suspend-frame' (when evil-mode disabled)
;; Had 2 versions of this function to replace `keyboard-quit' but neither worked well.
;; Leave in but commented as another solution presented itself.
;; (defun smart-keyboard-quit ()
;;   "Smarter version of the built-in `keyboard-quit'."
;;   (interactive)
;;   (if (active-minibuffer-window)
;;       (minibuffer-keyboard-quit)
;;       (keyboard-quit)))

;; (defun smart-keyboard-quit ()
;;   "Smarter version of the built-in `keyboard-quit'."
;;   (interactive)
;;   (if (active-minibuffer-window)
;;       (if (minibufferp)
;;           (minibuffer-keyboard-quit)
;;           (abort-recursive-edit))
;;       (keyboard-quit)))

;; And the better solution to quitting when the minibuffer is active
(keymap-global-set "s-g" #'minibuffer-keyboard-quit)





(message "Loaded ee-defaults.el")
(provide 'ee-defaults)

;;; ee-defaults.el ends here
