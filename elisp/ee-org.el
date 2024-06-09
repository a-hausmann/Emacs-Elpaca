;; -*- lexical-binding: t -*-
;; File name:     ee-org.el
;; Created:       2023-10-03
;; Last modified: Fri May 24, 2024 11:48:41
;; Purpose:       This is to configure Org mode.
;;

;; Key to org starting collapsed is org-startup-folded, never set before, doing it here.
(setq org-startup-folded t)
(setq inhibit-compacting-font-caches t)

(use-package org
    :elpaca nil
    :delight)

;; 05/24/2024: is org-modern a possible replacement for org-bullets?
;; Ref: https://github.com/minad/org-modern
;; Getting an error when attempting to toggle org-modern-mode, tried a lot, couldn't get to work.
;; Debugger entered--Lisp error: (void-function internal--without-restriction)
;;   internal--without-restriction(#f(compiled-function () #<bytecode 0x17c4b12d9671>))
;;   org-modern-mode(toggle)
;;   funcall-interactively(org-modern-mode toggle)
;;   call-interactively(org-modern-mode record nil)
;;   command-execute(org-modern-mode record)
;;   execute-extended-command(nil "org-modern-mode" "org modern")
;;   funcall-interactively(execute-extended-command nil "org-modern-mode" "org modern")
;;   call-interactively(execute-extended-command nil nil)
;;   command-execute(execute-extended-command)
;; Debugger entered--Lisp error: (void-function internal--without-restriction)
;;   internal--without-restriction(#f(compiled-function () #<bytecode 0x15758da0dc11>))
;;   org-modern-mode()
;;   org-modern--on()
;;   global-org-modern-mode(toggle)
;;   funcall-interactively(global-org-modern-mode toggle)
;;   call-interactively(global-org-modern-mode record nil)
;;   command-execute(global-org-modern-mode record)
;;   execute-extended-command(nil "global-org-modern-mode" "globa org")
;;   funcall-interactively(execute-extended-command nil "global-org-modern-mode" "globa org")
;;   call-interactively(execute-extended-command nil nil)
;;   command-execute(execute-extended-command)
;; (use-package org-modern
;;     :elpaca t
;;     :defer
;;     :delight
;;     :config
;;     (add-hook 'org-mode-hook 'org-modern-mode)
;;     )

(use-package org-bullets
  :elpaca t
  :defer
  :delight
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  ;; make available "org-bullet-face" such that I can control the font size individually
  (setq org-bullets-face-name (quote org-bullet-face))
  (setq org-bullets-bullet-list '("✙" "♱" "♰" "☥" "✞" "✟" "✝" "†" "✠" "✚" "✜" "✛" "✢" "✣" "✤" "✥"))
  (setq org-ellipsis "▼▼▼")
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.3))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.2))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))
  )

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

;; 2019-06-08: After loading ONE of MANY themes, the "fontify-natively" non-nil started
;; throwing code between source markers into horrid light colors regardless of theme used.
;; Setting the variable to nil gets rid of that tendency.
(setq org-src-fontify-natively nil)
(setq org-src-tab-acts-natively t)
(setq org-confirm-babel-evaluate nil)
(setq org-export-with-smart-quotes t)
(setq org-src-window-setup 'current-window)                   ; Allows for "C-c '" to narrow to code being edited.
;; 2019-01-07: Updated to Org 9.2, this method now invalid, using yas-snippet instead.
(add-hook 'org-mode-hook
            #'(lambda ()
               (visual-line-mode 1)
               (org-indent-mode 1)))
;; (global-set-key (kbd "C-c '") 'org-edit-src-code)
(general-def org-mode-map
  "C-c '" 'org-edit-src-code)
;; 2021-02-24: Adding SECOND org-ellipsis set after requiring org-tempo to see if this has effect.
(setq org-ellipsis "▼▼▼")


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
