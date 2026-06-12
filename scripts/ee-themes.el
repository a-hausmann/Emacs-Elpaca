;; -*- lexical-binding: t -*-
;; File name:     ee-themes.el
;; Created:       2023-07-21
;; Last modified: Tue Jun 02, 2026 20:56:25
;; Purpose:       Configure themes for Emacs-Elpaca
;;

;; Ref: https://github.com/protesilaos/modus-themes/

;; The Modus themes are INCLUDED in Emacs 28.1 and above.
;; For versions below, will NEED to ensure latest Modus versions installed.
;; Will NOT CHECK for Emacs version.

(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      modus-themes-region nil
      modus-themes-syntax '(alt-syntax)
      modus-themes-fringes 'subtle
      modus-themes-mode-line '(accented borderless padded)
      modus-themes-tabs-accented t
      modus-themes-paren-match '(bold intense)
      modus-themes-prompts '(bold intense)
      modus-themes-disable-other-themes t
      ;; modus-themes-completions 'opinionated
      modus-themes-completions '((matches . (extrabold))
                                 (selection . (semibold accented))
                                 (popup . (accented intense)))
      modus-themes-org-blocks 'tinted-background)
(setq modus-themes-headings
      '((1 . (rainbow overline background 1.3))
        (2 . (rainbow background 1.2))
        (3 . (rainbow bold 1.1))
        (t . (semilight 1.1))))
;; Important!
(setq modus-themes-scale-headings t)
(setq modus-themes-org-blocks 'gray-background)

(keymap-global-set "<f6>" #'modus-themes-select)
(keymap-global-set "<f7>" #'modus-themes-toggle)

;; Load the theme of your choice:
(load-theme 'modus-vivendi t nil) ;; OR (load-theme 'modus-vivendi)

(message "Loaded ee-themes.el")

(provide 'ee-themes)

;;; ee-themes.el ends here
