;; -*- lexical-binding: t -*-
;; File name:     init.el
;; Created:       2023-07-13
;; Last modified: Thu Jul 23, 2026 12:02:48
;; Purpose:       For repository "Emacs-Elpaca".
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Follow symlinks for version controlled files
(setq vc-follow-symlinks t)


;; Set load path for scripts (more constant config) and elisp (changing config)
;; 07/23/2026: Use variables and `expand-file-name' to make generic path.
;; (add-to-list 'load-path "~/.emacs.d/scripts")
(add-to-list 'load-path (expand-file-name "scripts" user-emacs-directory))
;; (add-to-list 'load-path "~/.emacs.d/elisp")
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

;; 05/10/2025: Add local Linux directories to the $PATH Emacs sets which doesn't include them.
(setq my:path-prepends
      (concat
       "/home/arnold/bin" path-separator
       "/home/arnold/.local/bin" path-separator))

(setenv "PATH"
        (concat my:path-prepends (getenv "PATH")))
(message "PATH: %s" (getenv "PATH"))
;; *****************************************************************************
;; 03/29/2025: updated flatpack version of Emacs to 30.1, and immediately get
;; Elpaca errors: Run "elpaca-update" on some packages, then "elpaca-update-all".
;; Found that Elpaca itself didn't update corrected, fingered that out, then
;; worked on changing all ":elpaca x" attributes to ":ensure x", which got rid
;; of most of them. But found that NOW Elpaca demands the below code to be added
;; to init.el before ANY calls to other Elpaca functions/macros. I assume that
;; includes the initial "require" commands as well.
;; Warning (elpaca): Init installer version does not match /home/arnold/.emacs.d/elpaca/builds/elpaca/doc/installer.el.
;; *****************************************************************************

;; *****************************************************************************
;; New code starts here
;; *****************************************************************************
;; (defvar elpaca-installer-version 0.12)

;; 05/18/2026: Create Elpaca lock file, now need to set customized variable `elpaca-lock-file'.
;; Ref: https://github.com/progfolio/elpaca/blob/master/doc/manual.md#lock-files
;; Variable `elpaca-menu-functions' lists `elpaca-menu-lock-file' FIRST so lock file will
;; be used for packages specified in the file.
(setopt elpaca-lock-file (expand-file-name "elisp/elpaca-lock-file-list.el" user-emacs-directory))

(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
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
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
;; *****************************************************************************
;; New code ends here
;; *****************************************************************************

;; Enable use-package :ensure support for Elpaca.
(elpaca elpaca-use-package
  (elpaca-use-package-mode))
  
(add-hook 'after-init-hook #'elpaca-process-queues)

(with-eval-after-load 'elpaca
  (add-hook 'elpaca-after-init-hook '+reset-init-values))

(message "Completed Elpaca, continuing with init.el")

(setq use-package-always-ensure t) ; 05/25/2026: added to try to get rid of errors.

;; Follow symlinks for version controlled files
(setq vc-follow-symlinks t)
;; Add following to prevent "cl is deprecated" messages.
;; Ref: https://github.com/kiwanami/emacs-epc/issues/35
(setq byte-compile-warnings '(cl-functions))

;; 2021-02-21: add setup for showing backtrace on errors.
(setq debug-on-error t)

(setq load-prefer-newer t)

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


;; 06/02/2026: I MAY have completed getting rid of all usage of General! Hurray!!!
;; (use-package general
;;   :ensure t
;;   :demand t
;;   :config
;;   (general-override-mode)
;;   (general-auto-unbind-keys))
;; (message "Configured General")
;; 06/01/2026: got rid of all `elpaca-wait' commands but now getting `general-def' errors.
;; (elpaca-wait)

(if (string-equal system-type "windows-nt")
    (progn
      (add-to-list 'default-frame-alist '(height . 40))
      (add-to-list 'default-frame-alist '(width . 160)))
    (progn
      (add-to-list 'default-frame-alist '(height . 38))
      (add-to-list 'default-frame-alist '(width . 124))))
(message "Set frame height/width for %s" system-type)
(blink-cursor-mode 0)


;; Set garbage collection hook, Emacs should feel snappier
(add-hook 'focus-out-hook #'garbage-collect)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set customization file
;; Add any configuration which relies on after-init-hook, emacs-startup-hook, 
;; etc to elpaca-after-init-hook so it runs after Elpaca has activated all 
;; queued packages. This includes loading of saved customizations. e.g.
(setq-default custom-file (expand-file-name ".custom.el" user-emacs-directory))
(message "custom-file value: %s" custom-file)
(defun aeh/load-custom ()
  (interactive)
  (when (file-exists-p custom-file)
    (load custom-file 'noerror)
    (message "custom-file loaded!")))
  (when (file-exists-p custom-file)
    (aeh/load-custom))

;; (elpaca-process-queues)
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


;; 2026-05-23: some of these files have local variables set, which seem to kill off
;; execution of the files (the "ask" isn't available). Enable `all' of them before
;; doing the loads, and then set back to "ask"
;; Was HOPING this would enable resetting the global "C-y" binding to `yank'
;; from the evil binding in module "ee-final.el", but that isn't happening.
;; OTOH, it's not hurting anything that I can see presently.
(setopt enable-local-variables :all)    ;; Enable here, disable at end.

;; Load fonts
(require 'ee-fonts)


;; Load defaults
(require 'ee-defaults)


;; Load themes
(require 'ee-themes)


;; Load Evil and complementary packages
(require 'ee-evil)


;; main package loader/config file.
(load "ee-packages")


;; Using Prot's modeline, ref: https://git.sr.ht/~protesilaos/dotfiles/tree/master/item/emacs/.emacs.d
;; (require 'prot-modeline)
;; (load "aeh-myownmodeline")
(require 'my-modeline)


;; TODO: add functionality of remaining hydra into "insert-date-time.el".
;; 10/01/2023: Load hydras
;; (load "ee-hydra")


;; 10/06/2023: Load abbreviations config.
(load "ee-abbrevs")


;; 05/30/2025: Load new module for programming (e.g. paredit.)
;; ################################################################################################
;; NOTE!
;; I created this module as I looked up some pages on using Guile Scheme in Emacs and saw that the
;; ParEdit and Geiser packages were recommended.  HOWEVER, I've found out that SmartParens can do
;; basically EVERYTHING that ParEdit can do, and I'm already using it.  I can merely expand the
;; keybindings for SmartParens and get the slurping/barfing that I wanted to begin with. Looks like
;; I really don't have to learn something new.
;; ################################################################################################
;; (require 'ee-programming)

;; 04/05/2026: trying out `puni' instead. Ref: https://github.com/AmaiKinono/puni
;; Use puni-mode only for certain major modes.
(use-package puni
  :ensure t
  :defer t
  :bind (:map puni-mode-map
              ("C-c C-<right>" . puni-slurp-forward)
              ("C-c C-<left>" . puni-slurp-backward)
              ("C-c C-M-<right>" . puni-barf-forward)
              ("C-c C-M-<left>" . puni-barf-backward))
  :hook ((emacs-lisp-mode lisp-mode). puni-mode))


;; 03/29/2026? Load new module for NEW key bindings & keymaps.
(load "ee-bindings")

(setq enable-local-variables t)    ;; Enable here, disable at end.


;; 02/01/2026: Completed "insert-date-time.el" with transient.el. Using that instead of Hydra.
;; TODO: Add to package the remaining functions from "ee-hydras.el"
;; NOTE: CAN employ `use-package' to load but CANNOT defer as package doesn't have autoloads.
(use-package insert-date-time
    :ensure nil)


;; Finally, open some files
(setq aeh-start-files '("~/Documents/AA/zoom-meetings-info.org"
                        "~/Documents/Health/Weight-tracker.org"
                        "~/Documents/org/FIFA-World-Cup--2026.org"
                        ;; "~/Documents/org/Premier-League-tracking.org"
                        "~/Documents/Health/BP-tracking.md"
                        "~/Documents/Health/UO-tracking.md"))
(defun aeh--load-start-files ()
  (mapcar 'find-file aeh-start-files))

(add-hook 'elpaca-after-init-hook 'aeh--load-start-files)


;; Load final stuff; key bindings and more (if needed)
(require 'ee-final)

;; 02/01/2026: Completed "insert-date-time.el" with transient.el. Using that instead of Hydra.
(require 'insert-date-time)

;;; init.el ends here
