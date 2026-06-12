;; -*- lexical-binding: t -*-
;; File name:     ee-org.el
;; Created:       2023-10-03
;; Last modified: Mon Jun 01, 2026 23:24:55
;; Purpose:       This is to configure Org mode.
;;

;; Key to org starting collapsed is org-startup-folded, never set before, doing it here.
(setq org-startup-folded t)
(setq inhibit-compacting-font-caches t)

(use-package org
    :ensure nil
    :delight
    :bind (:map org-mode-map
          ("C-c C-x l" . org-toggle-link-display))
    :config
    (custom-set-faces
     '(org-level-1 ((t (:inherit outline-1 :height 1.3))))
     '(org-level-2 ((t (:inherit outline-2 :height 1.2))))
     '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
     '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
     '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))
    ;; Other settings here:
    (setq org-auto-align-tags t
          org-tags-column -77
          org-fold-catch-invisible-edits 'smart
          org-special-ctrl-a/e nil
          org-insert-heading-respect-content nil
          org-hide-emphasis-markers t
          org-pretty-entities t
          org-agenda-tags-column 0)
    ;; Use "C-x 8 RET", enter string "arrow down left" to get set of symbols
    ;; (setq org-ellipsis "▼▼▼") 
    ;; (setq org-ellipsis " ⏎")  ; "RETURN SYMBOL"
    ;; (setq org-ellipsis " ↵")  ; "DOWNWARDS ARROW WITH CORNER LEFTWARDS" 
    (setq org-ellipsis " ⮨")      ; "BLACK CURVED DOWNWARDS AND LEFTWARDS ARROW"
    ;; (setq org-ellipsis " ⧨")  ; "DOWN-POINTING TRIANGLE WITH LEFT HALF BLACK"
    )


;; 04/28/2026: adding `org-modern', ref: https://github.com/minad/org-modern
;; I like this MUCH better than org-bullets.
;; 05/11/2026: Tweaked after getting error on original hook, needed `:after' to work correctly.
;; New code all in `:hook' and `:custom' settings.
(use-package org-modern
    :ensure t
    :after org
    :hook
    ;; Enable org-modern in org-mode buffers
    (org-mode . org-modern-mode)
    ;; optionally enable in org-agenda
    (org-agenda-finalize . org-modern-agenda)
    :custom
    ;; general visual tweaks
    (org-modern-star '("◉" "○" "✸" "✿"))       ;; heading bullets (level 1..4)
    ;; (org-modern-list '((?* . "•") (?- . "—"))) ;; list bullets (original)
    (org-modern-list '((?* . "✱") (?- . "—"))) ;; list bullets (don't think this changes anything)
    (org-modern-hide-stars t)                  ;; hide leading stars
    (org-modern-table-vertical 1) ;; table border style
    (org-modern-keyword nil)      ;; keywords styling (nil = default)
    (org-modern-block-name nil)   ;; block name style
    (org-modern-label nil)        ;; label style
    )


;; 04/22/2026: Customize `org-latex-hyperref-template' to add "colorlinks" & "urlcolor"
(customize-set-value
 'org-latex-hyperref-template
 "\\hypersetup{\n pdfauthor={%a},\n pdftitle={%t},\n pdfkeywords={%k},\n pdfsubject={%d},\n pdfcreator={%c}, \n pdflang={%L}, \n colorlinks=true, \n urlcolor=blue}\n")


(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sl" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("slt" . "src emacs-lisp :tangle"))
(add-to-list 'org-structure-template-alist '("sltf" . "src emacs-lisp :tangle FILE"))


;; 2019-06-08: After loading ONE of MANY themes, the "fontify-natively" non-nil started
;; throwing code between source markers into horrid light colors regardless of theme used.
;; Setting the variable to nil gets rid of that tendency.
(setq org-src-fontify-natively nil)
(setq org-src-tab-acts-natively t)
(setq org-confirm-babel-evaluate nil)
(setq org-export-with-smart-quotes t)
(setq org-src-window-setup 'current-window)                   ; Allows for "C-c '" to narrow to code being edited.

(add-hook 'org-mode-hook
            #'(lambda ()
               (visual-line-mode 1)
               (org-indent-mode 1)))
(keymap-set org-mode-map "C-c '" 'org-edit-src-code)

;; Ref: http://doc.norang.ca/org-mode.html#TasksAndStates
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "ONGOING" "|" "DONE(d@/!)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("ONGOING" :forground "yellow" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))
;; More from http://pragmaticemacs.com/emacs/org-mode-basics-vii-a-todo-list-with-schedules-and-deadlines/
;;warn me of any deadlines in next 7 days
(setq org-deadline-warning-days 7)
;;show me tasks scheduled or due in next fortnight
(setq org-agenda-span (quote fortnight))
;;don't show tasks as scheduled if they are already shown as a deadline
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
;;normal todo list - not sure I want to ignore deadlines/schedules
;; (setq org-agenda-todo-ignore-deadlines (quote all))
;; (setq org-agenda-todo-ignore-scheduled (quote all))
;;sort tasks in order of when they are due and then by priority
(setq org-agenda-sorting-strategy
      (quote
       ((agenda deadline-up priority-down)
        (todo priority-down category-keep)
        (tags priority-down category-keep)
        (search category-keep))))

;; End ee-org.el
