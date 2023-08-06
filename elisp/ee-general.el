;; -*- lexical-binding: t -*-
;; File name:     ee-general.el
;; Created:       2023-08-05
;; Last modified: Sun Aug 06, 2023 17:29:09
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
;; Define Buffers Menu.
(ee-definer
 "b" '(:ignore t :wk "Buffers")
 "b b" '(consult-buffer :wk "consult-buffer")
 "b i" '(ibuffer :wk "Ibuffer")
 "b k" '(kill-this-buffer :wk "kill buffer")
 "b e" '(erase-buffer :wk "erase buffer")
 "b E" '((let ((inhibit-read-only t)) (erase-buffer)) :wk "erase buffer force")
 "b r" '(rename-buffer :wk "Rename buffer")
 "b R" '(rename-uniquely :wk "Rename buffer unique")
 "b g" '(revert-buffer :wk "revert buffer") 
 "b M" '(aeh/delete-carrage-returns :wk "Delete ^M")
 "b C" '(ms/cleanup-buffer :wk "Cleanup buffer")
 )

;; Define Consult Menu.
(ee-definer
 "c" '(:ignore t :wk "Consult")
 "c b" '(consult-bookmark :wk "Bookmarks")
 "c c" '(consult-mode-command :wk "Mode")
 "c C" '(consult-minor-mode-menu :wk "Minor mode")
 "c i" '(consult-imenu :wk "Imenu")
 "c I" '(consult-project-imenu :wk "Project Imenu")
 "c m" '(consult-mark :wk "Marks")
 "c M" '(consult-global-mark :wk "Global marks")
 "c r" '(consult-recent-file :wk "recent files")
 "c o" '(consult-outline :wk "Outline")
 "c t" '(consult-theme :wk "Themes")
 "c y" '(consult-yank-from-kill-ring :wk "Yank from kill ring")
)

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

;; Define Modes Menu.
(ee-definer
 "m" '(:ignore t :wk "Modes")
 "m o" '(orgtbl-mode :wk "Org Table mode")
 "m p" '(prog-mode :wk "Prog mode")
 "m s" '(sql-mode :wk "SQL mode")
 "m t" '(text-mode :wk "Text mode")
)

;; Define Toggles Menu.
(ee-definer
 "t" '(:ignore t :wk "Toggles")
 "t a" '(aggressive-indent-mode :wk "Aggressive indent")
 "t b" '(page-break-lines-mode :wk "Page break lines")
 "t c" '(column-enforce-mode :wk "Col. enforce")
 "t C" '(company-mode :wk "Company mode")
 "t D" '(drag-stuff-mode :wk "Drag stuff") 
 "t e" '(toggle-debug-on-error :wk "Debug on error")
 "t g" '(toggle-debug-on-quit :wk "Debug on quit")
 "t h" '(highlight-thing-mode :wk "Highlight thing")
 "t l" '(display-line-numbers-mode :wk "Line numbers")       ; display type set to "visual" in "Better-defaults"
 "t p" '(smartparens-mode :wk "SmartParens")
 "t r" '(read-only-mode :wk "Read-only")
 "t f" '(auto-fill-mode :wk "Auto fill")
 "t s" '(flycheck-mode :wk "Flycheck")
 "t S" '(flyspell-mode :wk "Flyspell")
 "t t" '(toggle-truncate-lines :wk "Truncate Lines")
 "t w" '(whitespace-mode :wk "White space")
 "t W" '(toggle-word-wrap :wk "Word wrap")
 "t z" '(origami-mode :wk "Origami")
)

;; Define Windows Menu.
(ee-definer
 "w" '(:ignore t :wk "Windows")
 "w b" '(balance-windows :wk "Balance windows")
 "w h" '(enlarge-window-horizontally :wk "Grow horizontal")
 "w j" '(enlarge-window :wk "Grow vertical")
 "w k" '(shrink-window :wk "Shrink vertical")
 "w l" '(shrink-window-horizontally :wk "Shrink horizontal")
 "w t" '(text-scale-adjust :wk "Text adjust scale")
)

;; Define Yasnippets Menu.
(ee-definer
 "y" '(:ignore t :wk "Yasnippets")
 "y d" '(yas-describe-tables :wk "Describe tables")
 "y i" '(yas-insert-snippet :wk "Insert snippet")
 "y l" '(yas-load-directory :wk "Load directory")
 "y n" '(yas-new-snippet :wk "New snippet")
 "y r" '(yas-reload-all :wk "Reload all")
 "y v" '(yas-visit-snippet-file :wk "Visit snippet")
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
