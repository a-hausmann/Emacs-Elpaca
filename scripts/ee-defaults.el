;; -*- lexical-binding: t -*-
;; File name:     ee-defaults.el
;; Created:       2023-07-22
;; Last modified: Sun Jun 02, 2024 14:53:45
;; Purpose:       Set default values.
;;

;; Mostly taken from my original myDotemacs configuration.

;; Set user variables
(setq user-full-name "Arnold Hausmann")
(if (string-equal system-type "windows-nt")
    (setq user-mail-address "Arnold.Hausmann@trinity-health.org")
  (setq user-mail-address "ArnoldH@comcast.net"))

;; "Better" defaults, ref: https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org
(setq-default
 ;; ad-redefinition-action 'accept                   ; Silence warnings for redefinition
 auto-window-vscroll nil                          ; Lighten vertical scroll
 confirm-kill-emacs 'yes-or-no-p                  ; Confirm before exiting Emacs
 cursor-in-non-selected-windows nil               ; Hide the cursor in inactive windows
 delete-by-moving-to-trash t                      ; Delete files to trash
 ;; display-time-default-load-average nil            ; Don't display load average
 display-time-format "%H:%M"                      ; Format the time string
 fill-column 80                                   ; Set width for automatic line breaks
 garbage-collection-messages t                    ; set to non-nil to see GC messages.
 help-window-select t                             ; Focus new help windows when opened
 indent-tabs-mode nil                             ; Stop using tabs to indent
 inhibit-startup-screen t                         ; Disable start-up screen
 mouse-yank-at-point t                            ; Yank at point rather than pointer
 ;; ns-use-srgb-colorspace nil                       ; Don't use sRGB colors
 ;; recenter-positions '(5 top bottom)               ; Set re-centering positions
 scroll-conservatively most-positive-fixnum       ; Always scroll by one line
 scroll-margin 2                                  ; Add a margin when scrolling vertically
 sentence-end-double-space nil                    ; End a sentence after a dot and a space
 show-trailing-whitespace nil                     ; Display trailing whitespaces
 ;; split-height-threshold nil                       ; Disable vertical window splitting
 ;; split-width-threshold nil                        ; Disable horizontal window splitting
 tab-width 4                                      ; Set width for tabs
 ;; uniquify-buffer-name-style 'forward              ; Uniquify buffer names
 window-combination-resize t                      ; Resize windows proportionally
 x-stretch-cursor t                               ; Stretch cursor to the glyph width
 display-line-numbers-type 'relative              ; Display relative line#, works with folding.
 ;; 2019-08-30: Found in Oleh Krehel's init.el.
 recentf-max-saved-items 100                      ; abo-abo sets to 600, but I'm cautious.
 )

(delete-selection-mode 1)                         ; Replace region when inserting text
(display-time-mode 1)                             ; Enable time in the mode-line
(fringe-mode 0)                                   ; Disable fringes
(fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n
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
(set-default-coding-systems 'utf-8)               ; Default to utf-8 encoding
(setq dired-dwim-target t)                        ; Allow direct to dwim target of move, copy commands
(setq make-backup-files nil)                      ; Disable backup files
(setq auto-save-default nil)                      ; Disable auto-save funtionality
;; (put 'narrow-to-region 'disabled nil)             ; I don't think I need this line
(setq inhibit-startup-message t)                  ; Using dashboard
(setq ring-bell-function 'ignore)                 ; Disable bell
(show-paren-mode 1)                               ; Show matching parentheses
(global-visual-line-mode 1)                       ; I like this, so set globally.
(recentf-mode 1)                                  ; Turn on recent files


;; 10/20/2023: Configure settings/bindings for kill-ring & clipboard
;; Now, C-w/C-y will kill-ring-save & yank from kill-ring,
;; and s-w/s-y will save/yank from system clipboard. No 3-finger chords required.
(setq kill-ring-max 100)                             ; Keep up to 100 entries in kill-ring
(setq save-interprogram-paste-before-kill 4096)      ; Save up to 4K of clipboard to kill-ring
(setq select-enable-clipboard nil )                  ; Keep kill-ring and system clipboard separate
(setq x-select-enable-clipboard-manager t)           ; Emacs will transfer clipboard contents to system clipboard
(general-define-key "s-w" 'clipboard-kill-ring-save) ; Copy region to kill-ring and clipboard
(general-define-key "s-y" 'clipboard-yank)           ; New mapping for yanking from clipboard
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
(dotimes (n 10)
  (global-unset-key (kbd (format "C-%d" n)))
  (global-unset-key (kbd (format "M-%d" n))))


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


(message "Loaded ee-defaults.el")
(provide 'ee-defaults)

;;; ee-defaults.el ends here
