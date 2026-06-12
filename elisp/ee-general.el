;; -*- lexical-binding: t -*-
;; File name:     ee-general.el
;; Created:       2023-08-05
;; Last modified: Fri May 15, 2026 19:44:24
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
;; 07/21/2025 Frog Jump Buffer doesn't work, and I don't need the "TAB" either, so wipe the whole thing
;; and just use new def for "Politics".
;; (ee-definer
;;  ";" '(frog-jump-buffer :wk "Frog Jump Buffer")
;;  "TAB" '(aeh/switch-to-previous-buffer :wk "Prev Buffer")
;;  "p" '(aeh-set-politics-directory :wk "Politics"))

;; 07/21/2025: add key def for "Politics" that doesn't need main menu.
;; (general-def
;;     "C-c z p" '(aeh-set-politics-directory :wk "Politics"))

;; Define menu for insert state
(ee-inserter
 "TAB" '(aeh/switch-to-previous-buffer :wk "Prev Buffer")
 ;; 02/01/2026: Completed module "insert-date-time.el" and now using that. 
 ;; "d" '(aeh/hydra-insert-date-menu/body :wk "Insert Dates")
 "d" '(insert-date-time :wk "Insert Dates")
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
 "B" '(:ignore t :wk "Buffers")
 "B b" '(consult-buffer :wk "Consult Buffer")
 "B C" '(ms/cleanup-buffer :wk "Cleanup Buffer")
 "B d" '(aeh/delete-current-buffer-file :wk "Delete Current Buffer/File")
 "B e" '(erase-buffer :wk "Erase Buffer")
 "B E" '((let ((inhibit-read-only t)) (erase-buffer)) :wk "Erase Buffer Force")
 "B f" '(aeh/rename-current-buffer-file :wk "Rename Buffer/File") 
 "B g" '(revert-buffer :wk "Revert Buffer") 
 "B i" '(ibuffer-jump :wk "Ibuffer")
 "B k" '(kill-this-buffer :wk "Kill Buffer")
 "B M" '(aeh/delete-carrage-returns :wk "Delete ^M")
 "B r" '(rename-buffer :wk "Rename Buffer")
 "B R" '(rename-uniquely :wk "Rename Buffer unique")
 "B q" '(keyboard-quit :wk "Quit")
 )

;; Define Consult Menu.
;; 09/27/2025: Removing from SPC menu as most commands available from "C-x c" menu.
;; (ee-definer
;;     "c" '(:ignore t :wk "Consult")
;;   "c b" '(consult-bookmark :wk "Bookmarks")
;;   "c c" '(consult-mode-command :wk "Mode")
;;   "c C" '(consult-minor-mode-menu :wk "Minor Mode")
;;   "c i" '(consult-imenu :wk "Imenu")
;;   ;; "c I" '(consult-project-imenu :wk "Project Imenu")  ; 09/27/2025: invalid function now.
;;   "c m" '(consult-mark :wk "Marks")
;;   "c M" '(consult-global-mark :wk "Global marks")
;;   "c r" '(consult-recent-file :wk "Recent Files")
;;   "c o" '(consult-outline :wk "Outline")
;;   "c t" '(consult-theme :wk "Themes")
;;   "c y" '(consult-yank-from-kill-ring :wk "Yank From Kill Ring")
;;   "c q" '(keyboard-quit :wk "Quit")
;;   )

;; Define Eval Menu.
;; 07/05/2024: 
(ee-definer
    "E" '(:ignore t :wk "Evaluate")
  "E A" '(beginning-of-defun :wk "Defun Begin")
  "E E" '(end-of-defun :wk "Defun End")
  "E M" '(mark-defun :wk "Defun Mark")
  "E d" '(eval-defun :wk "Defun")
  "E e" '(eval-expression :wk "Expression")
  "E l" '(eval-last-sexp :wk "Last Sexp")
  "E p" '(pp-eval-expression :wk "Pretty Print")
  "E r" '(eval-region :wk "Region")
  "E q" '(keyboard-quit :wk "Quit"))

;; Define Files Menu.
(ee-definer
 "F" '(:ignore t :wk "Files")
 "F b" '(:ignore t :wk "Bookmarks")
 "F b d" '(bookmark-delete :wk "Bookmark Delete")
 "F b j" '(bookmark-jump :wk "Bookmark Jump")
 "F b l" '(bookmark-bmenu-list :wk "Bookmark List")
 "F b s" '(bookmark-set :wk "Bookmark Set")
 "F c" '(:ignore t :wk "Copy Stuff")
 "F c c" '(copy-file :wk "Copy File")
 "F c f" '(aeh/copy-fname-to-kill-ring :wk "Copy File Name")
 "F c F" '(aeh/copy-full-file-name-to-kill-ring :wk "Copy Full File Name")
 "F d" '(aeh/delete-current-buffer-file :wk "Delete Buffer/File")
 "F D" '(aeh/set-buffer-to-dos-format :wk "Convert To DOS")
 "F E" '(aeh/edit-file-as-root :wk "Edit File As Root")
 "F r" '(consult-recent-file :wk "Consult Recent File")
 "F R" '(aeh/rename-current-buffer-file :wk "Rename Buffer/File")
 "F U" '(aeh/set-buffer-to-unix-format :wk "Convert To UNIX")
 "F y" '(aeh/copy-file-name-to-clipboard :wk "File Name To Clipboard")
 "F q" '(keyboard-quit :wk "Quit")
 )

;; Define Modes Menu.
(ee-definer
 "M" '(:ignore t :wk "Modes")
 "M o" '(orgtbl-mode :wk "Org Table Mode")
 "M p" '(prog-mode :wk "Prog Mode")
 "M s" '(sql-mode :wk "SQL Mode")
 "M t" '(text-mode :wk "Text Mode")
 "M q" '(keyboard-quit :wk "Quit")
)

;; Define Toggles Menu.
(ee-definer
 "T" '(:ignore t :wk "Toggles")
 "T a" '(aggressive-indent-mode :wk "Aggressive Indent")
 "T b" '(page-break-lines-mode :wk "Page Break Lines")
 "T c" '(column-enforce-mode :wk "Col. Enforce")
 "T C" '(company-mode :wk "Company Mode")
 "T D" '(drag-stuff-mode :wk "Drag Stuff") 
 "T e" '(toggle-debug-on-error :wk "Debug On Error")
 "T f" '(auto-fill-mode :wk "Auto Fill")
 "T g" '(toggle-debug-on-quit :wk "Debug On Quit")
 "T h" '(highlight-thing-mode :wk "Highlight Thing")
 "T l" '(display-line-numbers-mode :wk "Line Numbers")       ; display type set to "visual" in "Better-defaults"
 ;; "t p" '(smartparens-mode :wk "SmartParens")
 "T r" '(read-only-mode :wk "Read Only")
 "T s" '(flycheck-mode :wk "Flycheck")
 "T S" '(flyspell-mode :wk "Flyspell")
 "T t" '(toggle-truncate-lines :wk "Truncate Lines")
 "T w" '(whitespace-mode :wk "White Space")
 "T W" '(toggle-word-wrap :wk "Word Wrap")
 "T z" '(origami-mode :wk "Origami")
 "T q" '(keyboard-quit :wk "Quit")
)

;; Define Windows Menu.
(ee-definer
 "W" '(:ignore t :wk "Windows")
 "W b" '(balance-windows :wk "Balance Windows")
 "W h" '(enlarge-window-horizontally :wk "Grow Horizontal")
 "W j" '(enlarge-window :wk "Grow Vertical")
 "W k" '(shrink-window :wk "Shrink Vertical")
 "W l" '(shrink-window-horizontally :wk "Shrink Horizontal")
 "W t" '(text-scale-adjust :wk "Text Adjust Scale")
 "W q" '(keyboard-quit :wk "Quit")
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

(general-def
 "C-<tab>" 'aeh/switch-to-previous-buffer)


;; 05/08/2025:
;; (general-def
;;     "C-c z s" '(:ignore t :wk "Shells")
;;   "C-c z s e" '(eshell :wk "Eshell")
;;   "C-c z s q" '(keyboard-quit :wk "Quit")
;;   "C-c z s t" '(term :wk "Term")
;;   "C-c z s a" '(ansi-term :wk "Ansi-term"))

;; 11/07/2025: adding describe-char mapping using global function.
(keymap-global-set "C-x c c" 'describe-char)
