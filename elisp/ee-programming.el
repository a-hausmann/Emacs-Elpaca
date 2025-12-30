;; -*- lexical-binding: t -*-
;; File name:     ee-programming.el
;; Created:       2025-05-30
;; Last modified: Mon Sep 01, 2025 17:57:14
;; Purpose:       Configure packages used in programming languages.
;;
;; ################################################################################################
;; NOTE!
;; I created this module as I looked up some pages on using Guile Scheme in Emacs and saw that the
;; ParEdit and Geiser packages were recommended.  HOWEVER, I've found out that SmartParens can do
;; basically EVERYTHING that ParEdit can do, and I'm already using it.  I can merely expand the
;; keybindings for SmartParens and get the slurping/barfing that I wanted to begin with. Looks like
;; I really don't have to learn something new.
;; ################################################################################################


;; ################################################################################################
;; Guile Scheme Setup
;; ################################################################################################

;; 05/30/2025: ref: https://www.gnu.org/software/guile/manual/html_node/Using-Guile-in-Emacs.html

;; Paredit: ref: https://gitlab.com/buildfunthings/emacs-config/blob/master/loader.org
(use-package paredit
    :ensure t
    ;; :diminish paredit-mode
    :delight
    :config
    (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
    (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
    (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
    ;; Add hooks to turn off smartparens-global-mode
    ;; 09/01/2025: stopped using smartparens in favor of native electricpairs
    ;; (add-hook 'emacs-lisp-mode-hook       #'smartparens-global-mode)
    ;; (add-hook 'eval-expression-minibuffer-setup-hook #'smartparens-global-mode)
    ;; (add-hook 'ielm-mode-hook             #'smartparens-global-mode)
    ;; (add-hook 'lisp-mode-hook             #'smartparens-global-mode)
    ;; (add-hook 'lisp-interaction-mode-hook #'smartparens-global-mode)
    ;; (add-hook 'scheme-mode-hook           #'smartparens-global-mode)
    ;; :bind (("C-c d" . paredit-forward-down)) ; don't need this binding which was developed for Mac.
    )

;; Ensure paredit is used EVERYWHERE!
(use-package paredit-everywhere
  :ensure t
  ;; :diminish paredit-everywhere-mode
  :delight
  :config
  (add-hook 'lisp-mode-hook #'paredit-everywhere-mode))

;; Geiser: ref: https://github.com/emacsmirror/geiser
;; Documentation ref: https://www.nongnu.org/geiser/
(use-package geiser-guile
  :ensure t
  :delight
  :config
  (add-hook 'lisp-mode-hook #'geiser-mode))


(message "Loaded ee-programming.el")

(provide 'ee-programming)

;;; End of ee-programming.el
