;; -*- lexical-binding: t -*-
;; File name:     ee-dired.el
;; Created:       2023-08-12
;; Last modified: Thu Aug 06, 2026 9:56:50
;; Purpose:       Configure dired and associated packages.
;;


;; Configure dired and more.
(use-package all-the-icons-dired
  :ensure t
  :delight)
;; Allow Elpaca to process queues up to this point
;; (elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword

;; set the directory listing switches string.
(if (string-equal system-type "windows-nt")
    (setq my/dired-string "-alG")
    (setq my/dired-string "-alG --group-directories-first"))

;; 08/06/2026: some of the bindings to Evil functions do NOT work, likely because
;; I'm using `keymap-set' instead of `evil-define-key'. I will experiment with using
;; that function here. Another option is to set Evil to use Emacs mode in dired buffers.
(use-package dired
    :ensure nil
    :delight
    :after evil
    :config
    (setq dired-listing-switches my/dired-string)
    (evil-set-initial-state 'dired-mode 'normal)       ; Note: evil loads first.
    (setq global-auto-revert-non-file-buffers t)       ; 06/23/2024, revert dired list when files change
    (setq dired-kill-when-opening-new-dired-buffer t)
    (keymap-set dired-mode-map "(" #'dired-hide-details-mode)
    (keymap-set dired-mode-map ")" #'dired-git-info-mode)
    (keymap-set dired-mode-map "j" #'dired-next-line)
    (keymap-set dired-mode-map "n" #'dired-next-line)
    (keymap-set dired-mode-map "k" #'dired-previous-line)
    (keymap-set dired-mode-map "p" #'dired-previous-line)
    (keymap-set dired-mode-map "H" #'dired-hide-dotfiles-mode)  ; actual is `dired-do-hard-link'
    (keymap-set dired-mode-map "o" #'dired-find-file-other-window)
    (keymap-set dired-mode-map "s" #'dired-sort-toggle-or-edit)
    (keymap-set dired-mode-map "m" #'dired-mark)
    (keymap-set dired-mode-map "u" #'dired-unmark)
    (keymap-set dired-mode-map "U" #'dired-unmark-all-marks)
    (keymap-set dired-mode-map "q" #'kill-this-buffer)
    (keymap-set dired-mode-map "g" #'revert-buffer)
    ;; (keymap-set dired-mode-map "W" #'evil-forward-WORD-begin)   ; actual is `browse-url-of-dired-file'
    (evil-define-key
        'normal
        'dired-mode-map (kbd "W") #'evil-forward-WORD-begin) ; THIS WORKS
    ;; (keymap-set dired-mode-map "B" #'evil-backward-WORD-begin)  ; actual is `dired-do-byte-compile'
    (evil-define-key
        'normal
        'dired-mode-map (kbd "B") #'evil-backward-WORD-begin) ; THIS WORKS
    (keymap-set dired-mode-map "E" #'evil-forward-WORD-end)     ; THIS WORKS!
    (keymap-set dired-mode-map "C-c C-p" #'peep-dired)
    (keymap-set dired-mode-map "C-c C-n" #'dired-narrow)
    (keymap-set dired-mode-map "C-c f r" #'dired-narrow-fuzzy)
    (keymap-set dired-mode-map "C-c f r" #'dired-narrow-regexp)
    (keymap-set dired-mode-map "<tab>" #'dired-subtree-toggle)
    (keymap-set dired-mode-map "<backtab>" #'dired-subtree-cycle)
    (keymap-set dired-mode-map "SPC" nil)
    :bind ("C-c d" . dired-jump)
    ;; :hook ((dired-mode . all-the-icons-dired-mode)
    :hook ((dired-mode . nerd-icons-dired-mode)
           (dired-mode . hl-line-mode))
    )

;; Allow Elpaca to process queues up to this point
;; (elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword

(use-package dired-collapse
  :ensure t
  :after dired
  :hook (dired-mode . dired-collapse-mode))

(use-package dired-git-info
  :ensure t
  :after dired)

;; (use-package dired-single
;;   :ensure t
;;   :after dired)

(use-package dired-narrow
  :ensure t
  :after dired)

(use-package dired-subtree
  :ensure t
  :after dired)

(use-package dired-hide-dotfiles
  :ensure t
  :after dired)

(use-package peep-dired
  :ensure t
  :after dired)
;; Allow Elpaca to process queues up to this point
;; (elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


;; Configure treemacs and more
(if (string-equal system-type "windows-nt")
    (setq python-string "python")
    (setq python-string "python3"))
(use-package treemacs
  :ensure t
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
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

;; 2019-10-16: added package
(use-package treemacs-icons-dired
  :after (treemacs dired)
  :ensure t
  :config (treemacs-icons-dired-mode))
;; Allow Elpaca to process queues up to this point
;; (elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


(message "Loaded ee-dired.el")

(provide 'ee-dired)

;;; End of ee-dired.el
