;; -*- lexical-binding: t -*-
;; File name:     init.el
;; Created:       2023-07-13
;; Last modified: Sat Aug 12, 2023 22:00:07
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

;; Elpaca elisp package manager; ref: https://github.com/progfolio/elpaca
;; Installation of Elpaca, ref: https://github.com/progfolio/elpaca#installer
(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; This package gets the correct environment variables so elpaca can use the ssh protocol.
(elpaca-queue
 (elpaca keychain-environment
   (keychain-refresh-environment)))

;; use-package
;; Elpaca macro like use-package
(defmacro use-feature (name &rest args)
  "Like `use-package' but accounting for asynchronous installation.
  NAME and ARGS are in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :elpaca nil
     ,@args))

(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

;; Allow Elpaca to process queues up to this point
(elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword

(if debug-on-error
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; General settings before loading more.
;; 07/21/2023: don't need this with Elpaca.
;; (let ((default-directory  "~/.emacs.d/elpa/"))
;;   (normal-top-level-add-subdirs-to-load-path))
;; (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

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
(setq-local custom-file (expand-file-name "customs.el" user-emacs-directory))
(add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))


;; Load fonts
(load "ee-fonts")

;; Load themes
(load "ee-themes")


;; Load defaults
(load "ee-defaults")


;; Load Evil and complementary packages
(load "ee-evil")


;; main package loader/config file.
(load "ee-packages")


;; Using Prot's modeline, ref: https://git.sr.ht/~protesilaos/dotfiles/tree/master/item/emacs/.emacs.d
;; (require 'prot-modeline)
(load "prot-modeline")

;; Load final stuff; key bindings and more (if needed)
(load "ee-final")

;;; init.el ends here
