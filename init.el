;; -*- lexical-binding: t -*-
;; File name:     init.el
;; Created:       2023-07-13
;; Last modified: Sun Aug 27, 2023 17:40:41
;; Purpose:       For repository "Emacs-Elpaca".
;;

;; This function displays how long Emacs took to start.
(add-hook 'elpaca-after-init-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract (current-time) before-init-time)))
                     gcs-done)))
;; Create a profiler report after init.
(profiler-start 'cpu+mem)
(add-hook 'elpaca-after-init-hook (lambda () (profiler-stop) (profiler-report)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set load path for scripts (more constant config) and elisp (changing config)
(add-to-list 'load-path "~/.emacs.d/scripts")
(add-to-list 'load-path "~/.emacs.d/elisp")
(require 'elpaca-config)                ; The Elpaca Package Manager

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Follow symlinks for version controlled files
(setq vc-follow-symlinks t)
;; Add following to prevent "cl is deprecated" messages.
;; Ref: https://github.com/kiwanami/emacs-epc/issues/35
(setq byte-compile-warnings '(cl-functions))
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
;; 2021-02-21: add setup for showing backtrace on errors.
(setq debug-on-error t)

;; Ensure all is set to UTF-8
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
;; set the default encoding system
(prefer-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; Make `load' prefer the newest version of a file.
(setq load-prefer-newer t)

(if (string-equal system-type "windows-nt")
    (setq ee-system-type "windows-nt")
  (setq ee-system-type "linux"))

(if (string-equal system-type "windows-nt")
    (add-to-list 'load-path "c:/Users/frst6889/.emacs.d/elisp")
  (add-to-list 'load-path "~/.emacs.d/elisp"))


;; general.el provides a more convenient method for binding keys in emacs (for both evil and non-evil users).
;; Ref: https://github.com/noctuid/general.el#about
;; Load general before the remaining packages so they can make use of the ~:general~ keyword in their declarations.
(use-package general
  :elpaca t
  :demand t
  :config
  (general-override-mode)
  (general-auto-unbind-keys))
;; Configure Hydra
(use-package hydra
  :elpaca t
  :demand)
;; Allow Elpaca to process queues up to this point
(elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


;; Load the General configuration file. This defines the menu structures only.
(load "ee-general")

;; Set the size of the frame
(when window-system
  (if (string-equal system-type "windows-nt")
      (progn
        (add-to-list 'default-frame-alist '(height . 40))
        (add-to-list 'default-frame-alist '(width . 160)))
    (progn
      (add-to-list 'default-frame-alist '(height . 38))
      (add-to-list 'default-frame-alist '(width . 140))))
  (blink-cursor-mode 0))


;; Set garbage collection hook, Emacs should feel snappier
(add-hook 'focus-out-hook #'garbage-collect)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set customization file
;; Add any configuration which relies on after-init-hook, emacs-startup-hook, 
;; etc to elpaca-after-init-hook so it runs after Elpaca has activated all 
;; queued packages. This includes loading of saved customizations. e.g.
(setq-default custom-file (expand-file-name ".custom.el" user-emacs-directory))
(add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))

;; Enable some features
;; Ref: https://github.com/skangas/dot-emacs/blob/master/init.el
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'help-fns-edit-variable 'disabled nil)
(put 'list-threads 'disabled nil)
(put 'list-timers 'disabled nil)

;; Load fonts
(require 'ee-fonts)

;; Load defaults
(require 'ee-defaults)


;; Load themes
(require 'ee-themes)


;; Load Evil and complementary packages
(load "ee-evil")


;; main package loader/config file.
(load "ee-packages")


;; Using Prot's modeline, ref: https://git.sr.ht/~protesilaos/dotfiles/tree/master/item/emacs/.emacs.d
(require 'prot-modeline)

;; Load final stuff; key bindings and more (if needed)
(load "ee-final")

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values '((eval ah--set-origami-fold-style-braces))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
