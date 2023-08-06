;; -*- lexical-binding: t -*-
;; File name:     ee-general.el
;; Created:       2023-08-05
;; Last modified: Sat Aug 05, 2023 16:30:18
;; Purpose:       Configure the basic General bindings for menu structures to 
;;                reduce Hydra usage. Hydra should be used for PERSISTANT menus only.
;;

;; Older structure plus menu from original configuration.
;; ;; Prefix keybindings
;; (general-create-definer aeh-leader-def
;;   :prefix "SPC")
;; (general-create-definer aeh-local-leader-def
;;   :prefix "C-;")
;; ;; Global keybindings
;; (aeh-leader-def
;;   :keymaps '(normal visual emacs)
;;   ";" '(frog-jump-buffer :which-key "Frog jump buffer")
;;   "TAB" '(aeh/switch-to-previous-buffer :which-key "prev-buffer")
;;   "b" '(aeh/hydra-buffers/body :which-key "buffers")
;;   "c" '(aeh/hydra-consult/body :which-key "consult")
;;   "f" '(aeh/hydra-files/body :which-key "files")
;;   "m" '(aeh/hydra-modes/body :which-key "modes")
;;   "t" '(aeh/hydra-toggles/body :which-key "toggles")
;;   "y" '(aeh/hydra-yasnippet/body :which-key "snippets")
;;   "w" '(aeh/hydra-windows/body :which-key "windows"))
;; (aeh-local-leader-def
;;   :keymaps 'insert
;;   ";" '(frog-jump-buffer :which-key "Frog jump buffer")
;;   "TAB" '(aeh/switch-to-previous-buffer :which-key "prev-buffer")
;;   ;; "c" '(aeh/hydra-counsel/body :which-key "counsel")
;;   "d" '(aeh/hydra-insert-date-menu/body :which-key "dates")
;;   "i" '(aeh/hydra-insert-stuff-menu/body :which-key "insert stuff"))

;; Ref: https://github.com/progfolio/.emacs.d#general-key-bindings
;; The global definer allows me to use a leader key in most states. 
;; Modified to use "non-normal-prefix" which matches my original setup.
(general-def
 :keymaps 'override
 :states '(insert normal hybrid motion visual operator emacs)
 :prefix-map '+prefix-map
 :prefix "SPC"
 :non-normal-prefix "C-;")

;; Ref: https://github.com/progfolio/.emacs.d#general-key-bindings
(general-create-definer ee-definer
  :wk-full-keys nil                     ; which-key, bound keys do NOT correspond to full sequence
  :keymaps '+prefix-map)

;; General stuff I like to keep with just SPC as leader.
(ee-definer
 ";" '(frog-jump-buffer :wk "Frog jump buffer")
 "TAB" '(aeh/switch-to-previous-buffer :wk "prev-buffer")
 )


;; Define main menus for normal/non-normal
;; Define Files Menu.
(ee-definer
 "f" '(:ignore t :wk "Files")
 "f b j" '(bookmark-jump :wk "Bookmark jump")
 "f b l" '(bookmark-bmenu-list :wk "Bookmark list")
 "f b s" '(bookmark-set :wk "Bookmark set")
 "f b d" '(bookmark-delete :wk "Bookmark delete")
 "f c" '(copy-file :wk "Copy file")
 "f r" '(consult-recent-file :wk "Consult recent file")
 "f d" '(aeh/delete-current-buffer-file :wk "Delete buffer/file")
 "f r" '(aeh/rename-current-buffer-file :wk "Rename buffer/file")
 "f y" '(aeh/copy-file-name-to-clipboard :wk "File name to clipboard")
 "f E" '(aeh/edit-file-as-root :wk "Edit file as root")
 "f D" '(aeh/set-buffer-to-dos-format :wk "Convert to DOS")
 "f U" '(aeh/set-buffer-to-unix-format :wk "Convert to UNIX")
 )





;; Ref: https://github.com/progfolio/.emacs.d#general-key-bindings
;; 08/05/2023: don't really like any of these, and "zoom" doesn't work
;; (ee-definer
;;   "!"   'shell-command
;;   ":"   'eval-expression
;;   "."   'repeat
;;   "z"   '((lambda (local) (interactive "p")
;;             (unless repeat-mode (repeat-mode))
;;             (let ((local current-prefix-arg)
;;                   (current-prefix-arg nil))
;;               (call-interactively (if local #'text-scale-adjust #'global-text-scale-adjust))))
;;           :which-key "zoom"))

;; Ref: https://github.com/progfolio/.emacs.d#general-key-bindings
;; We define a global-leader definer to access major-mode specific bindings:
;; 08/05/2023: Not sure what this is supposed to do, but "SPC m" isn't defined when testing.
;; (general-create-definer ee-leader
;;   :keymaps 'override
;;   :states '(insert normal hybrid motion visual operator)
;;   :prefix "SPC m"
;;   :non-normal-prefix "S-SPC m"
;;   "" '( :ignore t
;;         :which-key
;;         (lambda (arg)
;;           (cons (cadr (split-string (car arg) " "))
;;                 (replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))))
