;; -*- lexical-binding: t;  -*-

;; File name:     ee-bindings.el
;; Created:       2026-01-13
;; Last modified: Tue Jun 09, 2026 20:28:27
;; Purpose:       Replacing general.el, all non-use-package bindings to go here
;;
;; I find that which-key is still displaying incorrect text in some cases, and
;; these seem to be directly related to the General "menu" items that I created
;; so long ago when first using Emacs (to recreate the menu structure of Spacemacs.)
;; This is one of my drivers to stop using General. I'm going to comment out the
;; "menu" code one by one to get through this.
;; 03/29/2026: I found out that General is creating `keymaps' for these prefix keys
;; behind the scenes and setting the which-key names as what I created in General.
;; So, I will need to create my OWN keymaps and bindings...as done below.

;; So I FINALLY found/refound the correct syntax to create a key binding
;; syntax using the new "keymap-...-set" functions which will allow a
;; customized text string for which-key to use (instead of the function name.)
;; The prefix "C-c z", "C-c x" and "C-c C-x" appear to be unused ("C-c C-z")
;; is already used to execute function "run-lisp".

;; Lambda / anonymous command example.
;; (keymap-global-set "C-c t" '("Toggle truncate" . (lambda () (interactive) (toggle-truncate-lines))))

;; NOTE: to show the sub-menus with title, MUST USE the backtick instead of quote,
;; and prefix the keymap name with comma.


;;; Code for keymaps and bindings.

;;;; keymaps for `C-c z' prefix

(defvar-keymap my-s-z-s-prefix-map
  :doc "Prefix map for `s-z' s"
  "a" '("Ansi-term" . ansi-term)
  "e" '("Eshell" . eshell)
  "t" '("Term" . term))

(defvar-keymap my-s-z-prefix-map
  :doc "Prefix map for `s-z'"
  "n" '("New buffer". aeh/new-untitled-buffer)
  "p" '("Politics" . aeh-set-politics-directory)
  "s" `("Shells" . ,my-s-z-s-prefix-map)
  "t" '("Resize text" . text-scale-adjust)
  )

(keymap-global-set "s-z" `("Menu" . ,my-s-z-prefix-map))


;;;; keymaps for `C-;' prefix

(defvar-keymap my-c-sc-prefix-map
  :doc "Prefix map for `C-;'"
  "d" '("Insert Date" . insert-date-time))

(keymap-global-set "C-;" `("Menu" . ,my-c-sc-prefix-map))


;;; Alternate bindings for `insert-date-time'

(keymap-global-set "C-c M-i" 'insert-date-time)
(keymap-global-set "s-c i" 'insert-date-time)
(keymap-global-set "s-i" 'insert-date-time)



;;; Stuff I haven't yet created a prefix keymap.

(keymap-global-set "C-c C-x F" '("Copy full name to kill ring" . aeh/copy-full-file-name-to-kill-ring))
(keymap-global-set "C-c C-x f" '("Copy fname to kill ring" . aeh/copy-fname-to-kill-ring))

(keymap-global-set "s-t" '("Toggle hs at point" . hs-toggle-hiding))

;;; code from original "ee-general.el" module, changed to use new binding functions.

(keymap-global-set "C-<tab>" #'aeh/switch-to-previous-buffer)

;; 11/07/2025: adding describe-char mapping using global function.
(keymap-global-set "C-x c c" #'describe-char)

;; 05/03/2026: THIS will allow me to have point in column 1 and delete leading spaces.
;; The command works either on a single line or region.
(keymap-global-set "C-c C-w" #'delete-whitespace-rectangle)


;; 06/01/2026: Bind the clipboard commands, try getting native `yank' to work.
(keymap-global-set "s-w" #'clipboard-kill-ring-save)
(keymap-global-set "C-x M-w" #'clipboard-kill-ring-save)
(keymap-global-set "C-x C-y" #'clipboard-yank)

;; 06/01/2026: worked until removing all `elpaca-wait' Now trying to unset evil bindings before evil loads.
(with-eval-after-load 'evil
  (keymap-unset evil-motion-state-map "C-y" t)   ; This WORKS! 
  (keymap-unset evil-insert-state-map "C-y" t)   ; This WORKS! 
  )
(keymap-global-set "C-y" #'yank)               ; With the above, I may NOT need this but doesn't hurt.

(keymap-global-set "s-d" #'duplicate-dwim)

;;; ee-bindings.el ends here

;;; Local Variables:
;;; eval: (keymap-local-set "C-c i" #'consult-outline)
;;; eval: (setq-local outline-regexp ";;;")
;;; End:
