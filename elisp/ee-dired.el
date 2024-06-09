;; -*- lexical-binding: t -*-
;; File name:     ee-dired.el
;; Created:       2023-08-12
;; Last modified: Sat Feb 10, 2024 12:38:56
;; Purpose:       Configure dired and associated packages.
;;


;; Configure dired and more.
(use-package all-the-icons-dired
  :elpaca t
  :delight)
;; Allow Elpaca to process queues up to this point
;; (elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword

;; set the directory listing switches string.
(if (string-equal system-type "windows-nt")
    (setq my/dired-string "-alG")
    (setq my/dired-string "-alG --group-directories-first"))

(use-package dired
  :elpaca nil
  :delight
  :after evil
  :config
  (setq dired-listing-switches my/dired-string)
  (evil-set-initial-state 'dired-mode 'normal)       ; Note: evil loads first.
  :bind ("C-c d" . dired-jump)
  :hook ((dired-mode . all-the-icons-dired-mode)
         (dired-mode . hl-line-mode))
)
;; Allow Elpaca to process queues up to this point
;; (elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword

(use-package dired-collapse
  :after dired
  :hook (dired-mode . dired-collapse-mode))

(use-package dired-git-info
  :after dired)

(use-package dired-single
  :after dired)

(use-package dired-narrow
  :after dired)

(use-package dired-subtree
  :after dired
)

(use-package dired-hide-dotfiles
  :after dired)

(use-package peep-dired
  :after dired)
;; Allow Elpaca to process queues up to this point
;; (elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword

(general-def
 :states 'normal
 :keymaps 'dired-mode-map
 "(" 'dired-hide-details-mode
 ")" 'dired-git-info-mode
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
 "W" 'evil-forward-WORD-begin
 "B" 'evil-backward-WORD-begin
 "E" 'evil-forward-WORD-end
 "" 'dired-git-info-mode
 "n" 'dired-next-line
 "p" 'dired-previous-line
 "P" 'peep-dired
 "C-c C-n" 'dired-narrow
 "C-c f r" 'dired-narrow-fuzzy
 "C-c f r" 'dired-narrow-regexp
 "<tab>" 'dired-subtree-toggle
 "<backtab>" 'dired-subtree-cycle
 "SPC" nil)


;; Configure treemacs and more
(if (string-equal system-type "windows-nt")
    (setq python-string "python")
    (setq python-string "python3"))
(use-package treemacs
  :elpaca t
  :after evil
  :defer t
  :commands (treemacs)
  :bind (("M-<f6>" . treemacs))
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  :delight
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (setq treemacs-collapse-dirs              (if (executable-find python-string) 3 0)
        treemacs-deferred-git-apply-delay   0.5
        treemacs-display-in-side-window     t
        treemacs-file-event-delay           5000
        treemacs-file-follow-delay          0.2
        treemacs-follow-after-init          t
        treemacs-follow-recenter-distance   0.1
        treemacs-goto-tag-strategy          'refetch-index
        treemacs-indentation                2
        treemacs-indentation-string         " "
        treemacs-is-never-other-window      nil
        treemacs-no-png-images              nil
        treemacs-project-follow-cleanup     nil
        treemacs-persist-file               (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
        treemacs-recenter-after-file-follow nil
        treemacs-recenter-after-tag-follow  nil
        treemacs-show-hidden-files          t
        treemacs-silent-filewatch           nil
        treemacs-silent-refresh             nil
        treemacs-sorting                    'alphabetic-desc
        treemacs-space-between-root-nodes   t
        treemacs-tag-follow-cleanup         t
        treemacs-tag-follow-delay           1.5
        treemacs-width                      35)

  (progn
    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;; (treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find python-string))))
      (`(t . t)
       (treemacs-git-mode 'extended))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  )

(use-package treemacs-evil
  ;; :after (treemacs evil)
  :after treemacs
  :elpaca t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :elpaca t)

;; 2019-10-16: added package
(use-package treemacs-icons-dired
  :after (treemacs dired)
  :elpaca t
  :config (treemacs-icons-dired-mode))
;; Allow Elpaca to process queues up to this point
;; (elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


(message "Loaded ee-dired.el")

(provide 'ee-dired)

;;; End of ee-dired.el
