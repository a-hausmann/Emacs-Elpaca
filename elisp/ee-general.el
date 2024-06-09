;; -*- lexical-binding: t -*-
;; File name:     ee-general.el
;; Created:       2023-08-05
;; Last modified: Wed May 29, 2024 20:00:19
;; Purpose:       Configure the basic General bindings for menu structures to 
;;                reduce Hydra usage. Hydra should be used for PERSISTANT menus only.
;;

;; Ref: https://github.com/progfolio/.emacs.d#general-key-bindings
;; The global definer allows me to use a leader key in most states. 
;; Modified to use "non-normal-prefix" which matches my original setup.
(general-def
 :keymaps 'override
 ;; :states '(insert normal hybrid motion visual operator emacs)
 :states '(normal hybrid motion visual operator emacs)
 :prefix-map '+prefix-map
 :prefix "SPC")
 ;; :non-normal-prefix "C-;")
(general-def
 :keymaps 'override
 ;; :states '(insert normal hybrid motion visual operator emacs)
 :states 'insert
 :prefix-map '+iprefix-map
 :prefix "C-;")

;; Ref: https://github.com/progfolio/.emacs.d#general-key-bindings
(general-create-definer ee-definer
  :wk-full-keys nil                     ; which-key, bound keys do NOT correspond to full sequence
  :keymaps '+prefix-map)

(general-create-definer ee-inserter
  :wk-full-keys nil                     ; which-key, bound keys do NOT correspond to full sequence
  :keymaps '+iprefix-map)

;; General stuff I like to keep with just SPC as leader.
(ee-definer
 ";" '(frog-jump-buffer :wk "Frog Jump Buffer")
 "TAB" '(aeh/switch-to-previous-buffer :wk "Prev Buffer")
 "s" '(aeh-set-politics-directory :wk "Politics"))

;; Define menu for insert state
(ee-inserter
 "i" '(:ignore t :wk "Insert Stuff")
 "TAB" '(aeh/switch-to-previous-buffer :wk "Prev Buffer")
 "d" '(aeh/hydra-insert-date-menu/body :wk "Insert Dates")
 "i b" '(aeh-emacs-flower-box :wk "Flowerbox")
 "i D" '(aeh/insert-default-directory :wk "Insert Default Directory")
 "i f" '(aeh/insert-fname :wk "Insert File Name")
 "i F" '(aeh/insert-full-file-name :wk "Insert Full File Name")
 "i i" '(aeh/hydra-insert-stuff-menu/body :wk "Insert Stuff")
 "i p" '(aeh/insert-file-name-directory :wk "Insert Path")
)

;; Define main menus for normal/non-normal
;; Define Buffers Menu.
(ee-definer
 "b" '(:ignore t :wk "Buffers")
 "b b" '(consult-buffer :wk "Consult Buffer")
 "b C" '(ms/cleanup-buffer :wk "Cleanup Buffer")
 "b d" '(aeh/delete-current-buffer-file :wk "Delete Current Buffer/File")
 "b e" '(erase-buffer :wk "Erase Buffer")
 "b E" '((let ((inhibit-read-only t)) (erase-buffer)) :wk "Erase Buffer Force")
 "b f" '(aeh/rename-current-buffer-file :wk "Rename Buffer/File") 
 "b g" '(revert-buffer :wk "Revert Buffer") 
 "b i" '(ibuffer-jump :wk "Ibuffer")
 "b k" '(kill-this-buffer :wk "Kill Buffer")
 "b M" '(aeh/delete-carrage-returns :wk "Delete ^M")
 "b r" '(rename-buffer :wk "Rename Buffer")
 "b R" '(rename-uniquely :wk "Rename Buffer unique")
 "b q" '(keyboard-quit :wk "Quit")
 )

;; Define Consult Menu.
(ee-definer
    "c" '(:ignore t :wk "Consult")
  "c b" '(consult-bookmark :wk "Bookmarks")
  "c c" '(consult-mode-command :wk "Mode")
  "c C" '(consult-minor-mode-menu :wk "Minor Mode")
  "c i" '(consult-imenu :wk "Imenu")
  "c I" '(consult-project-imenu :wk "Project Imenu")
  "c m" '(consult-mark :wk "Marks")
  "c M" '(consult-global-mark :wk "Global marks")
  "c r" '(consult-recent-file :wk "Recent Files")
  "c o" '(consult-outline :wk "Outline")
  "c t" '(consult-theme :wk "Themes")
  "c y" '(consult-yank-from-kill-ring :wk "Yank From Kill Ring")
  "c q" '(keyboard-quit :wk "Quit")
  )

;; Define Eval Menu.
(ee-definer
    "e" '(:ignore t :wk "Evaluate")
  "e A" '(beginning-of-defun :wk "Defun Begin")
  "e E" '(end-of-defun :wk "Defun End")
  "e M" '(mark-defun :wk "Defun Mark")
  "e d" '(eval-defun :wk "Defun")
  "e e" '(eval-expression :wk "Expression")
  "e l" '(eval-last-sexp :wk "Last Sexp")
  "e p" '(pp-eval-expression :wk "Pretty Print")
  "e r" '(eval-region :wk "Region")
  "e q" '(keyboard-quit :wk "Quit"))

;; Define Files Menu.
(ee-definer
 "f" '(:ignore t :wk "Files")
 "f b" '(:ignore t :wk "Bookmarks")
 "f b d" '(bookmark-delete :wk "Bookmark Delete")
 "f b j" '(bookmark-jump :wk "Bookmark Jump")
 "f b l" '(bookmark-bmenu-list :wk "Bookmark List")
 "f b s" '(bookmark-set :wk "Bookmark Set")
 "f c" '(:ignore t :wk "Copy Stuff")
 "f c c" '(copy-file :wk "Copy File")
 "f c f" '(aeh/copy-fname-to-kill-ring :wk "Copy File Name")
 "f c F" '(aeh/copy-full-file-name-to-kill-ring :wk "Copy Full File Name")
 "f d" '(aeh/delete-current-buffer-file :wk "Delete Buffer/File")
 "f D" '(aeh/set-buffer-to-dos-format :wk "Convert To DOS")
 "f E" '(aeh/edit-file-as-root :wk "Edit File As Root")
 "f r" '(consult-recent-file :wk "Consult Recent File")
 "f R" '(aeh/rename-current-buffer-file :wk "Rename Buffer/File")
 "f U" '(aeh/set-buffer-to-unix-format :wk "Convert To UNIX")
 "f y" '(aeh/copy-file-name-to-clipboard :wk "File Name To Clipboard")
 "f q" '(keyboard-quit :wk "Quit")
 )

;; Define Modes Menu.
(ee-definer
 "m" '(:ignore t :wk "Modes")
 "m o" '(orgtbl-mode :wk "Org Table Mode")
 "m p" '(prog-mode :wk "Prog Mode")
 "m s" '(sql-mode :wk "SQL Mode")
 "m t" '(text-mode :wk "Text Mode")
 "m q" '(keyboard-quit :wk "Quit")
)

;; Define Toggles Menu.
(ee-definer
 "t" '(:ignore t :wk "Toggles")
 "t a" '(aggressive-indent-mode :wk "Aggressive Indent")
 "t b" '(page-break-lines-mode :wk "Page Break Lines")
 "t c" '(column-enforce-mode :wk "Col. Enforce")
 "t C" '(company-mode :wk "Company Mode")
 "t D" '(drag-stuff-mode :wk "Drag Stuff") 
 "t e" '(toggle-debug-on-error :wk "Debug On Error")
 "t f" '(auto-fill-mode :wk "Auto Fill")
 "t g" '(toggle-debug-on-quit :wk "Debug On Quit")
 "t h" '(highlight-thing-mode :wk "Highlight Thing")
 "t l" '(display-line-numbers-mode :wk "Line Numbers")       ; display type set to "visual" in "Better-defaults"
 "t p" '(smartparens-mode :wk "SmartParens")
 "t r" '(read-only-mode :wk "Read Only")
 "t s" '(flycheck-mode :wk "Flycheck")
 "t S" '(flyspell-mode :wk "Flyspell")
 "t t" '(toggle-truncate-lines :wk "Truncate Lines")
 "t w" '(whitespace-mode :wk "White Space")
 "t W" '(toggle-word-wrap :wk "Word Wrap")
 "t z" '(origami-mode :wk "Origami")
 "t q" '(keyboard-quit :wk "Quit")
)

;; Define Windows Menu.
(ee-definer
 "w" '(:ignore t :wk "Windows")
 "w b" '(balance-windows :wk "Balance Windows")
 "w h" '(enlarge-window-horizontally :wk "Grow Horizontal")
 "w j" '(enlarge-window :wk "Grow Vertical")
 "w k" '(shrink-window :wk "Shrink Vertical")
 "w l" '(shrink-window-horizontally :wk "Shrink Horizontal")
 "w t" '(text-scale-adjust :wk "Text Adjust Scale")
 "w q" '(keyboard-quit :wk "Quit")
)

;; Define Yasnippets Menu.
(ee-definer
 "y" '(:ignore t :wk "Yasnippets")
 "y d" '(yas-describe-tables :wk "Describe Tables")
 "y i" '(yas-insert-snippet :wk "Insert Snippet")
 "y l" '(yas-load-directory :wk "Load Directory")
 "y n" '(yas-new-snippet :wk "New Snippet")
 "y r" '(yas-reload-all :wk "Reload All")
 "y v" '(yas-visit-snippet-file :wk "Visit Snippet")
 "y q" '(keyboard-quit :wk "Quit")
)
