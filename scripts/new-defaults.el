;; -*- coding: utf-8; lexical-binding: t -*-
;; File name:     new-defaults.el
;; Created:       2025-12-04
;; Last modified: Tue Dec 09, 2025 22:57:47
;; Purpose:       New version of my defaults, heavy influence from
;;                https://github.com/jamescherti/minimal-emacs.d in the init.el file.
;;                I like easier-to-read grouping of settings with clear explanation.
;;
;; NOTE:          Use M-x occur (M-s o) to generate a "menu" of section headers marked by ";;;".

;;; Set user variables
(setq user-full-name "Arnold Hausmann")
(if (string-equal system-type "windows-nt")
    (setq user-mail-address "Arnold.Hausmann@trinity-health.org")
  (setq user-mail-address "aehjr1@gmail.com"))

;; Much of the following copies from ee-defaults.el but reformats in the "minimal-emacs" style.

;; Ask the user whether to terminate asynchronous compilations on exit.
;; This prevents native compilation from leaving temporary files in /tmp.
(setq native-comp-async-query-on-exit t)

;; Allow for shorter responses: "y" for yes and "n" for no.
(setq read-answer-short t)
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
    (advice-add 'yes-or-no-p :override #'y-or-n-p))

;;; Undo/redo
(setq undo-limit (* 13 160000)
      undo-strong-limit (* 13 240000)
      undo-outer-limit (* 13 24000000))


;;; Minibuffer
;; Allow nested minibuffers
(setq enable-recursive-minibuffers t)

;; Keep the cursor out of the read-only portions of the.minibuffer
(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)


;;; User Interface
;; By default, Emacs "updates" its ui more often than it needs to
(setq which-func-update-delay 1.0)
(setq idle-update-delay which-func-update-delay)  ;; Obsolete in >= 30.1

(defalias #'view-hello-file #'ignore)  ; Never show the hello file

;; No beeping or blinking
(setq visible-bell nil)
(setq ring-bell-function #'ignore)

;;; Show-paren

(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)
(show-paren-mode 1)


;;; Miscellaneous.
(setq custom-buffer-done-kill t)

(setq whitespace-line-column nil)  ; Use the value of `fill-column'. Emacs version???

;; Can be activated with `display-line-numbers-mode'
(setq-default display-line-numbers-width 3)
(setq-default display-line-numbers-widen t)
(setq-default display-line-numbers-type 'relative)
(setq-default display-line-numbers 'visual)

(setq truncate-string-ellipsis "…")

;; Disable truncation of printed s-expressions in the message buffer
(setq eval-expression-print-length nil
      eval-expression-print-level nil)

;; Position underlines at the descent line instead of the baseline.
(setq x-underline-at-descent-line t)

(setq remote-file-name-inhibit-cache 50)

;; Automatically rescan the buffer for Imenu entries when `imenu' is invoked
;; This ensures the index reflects recent edits.
(setq imenu-auto-rescan t)

;; Prevent truncation of long function names in `imenu' listings
(setq imenu-max-item-length 160)

;; Disable auto-adding a new line at the bottom when scrolling.
(setq next-line-add-newlines nil)

;; This setting forces Emacs to save bookmarks immediately after each change.
;; Benefit: you never lose bookmarks if Emacs crashes.
(setq bookmark-save-flag 1)

;; Remove duplicates from the kill ring to reduce clutter
(setq kill-do-not-save-duplicates t)


;;; Files.
;; Delete by moving to trash in interactive mode
(setq delete-by-moving-to-trash (not noninteractive))
(setq remote-file-name-inhibit-delete-by-moving-to-trash t)

;; Ignoring this is acceptable since it will redirect to the buffer regardless.
(setq find-file-suppress-same-file-warnings t)

;; Resolve symlinks so that operations are conducted from the file's directory
(setq find-file-visit-truename t
      vc-follow-symlinks t)

;; Prefer vertical splits over horizontal ones
;; Means has to have at least 170 characters before allowing a split into left & right sides,
;; oddly enough called a horizontal split. Setting the height nil means there is NO
;; required number of lines before splitting into top and bottom, called a vertical split.
(setq split-width-threshold 170
      split-height-threshold nil)


;;; Buffers.
(setq uniquify-buffer-name-style 'forward)

;; comint (general command interpreter in a window)
(setq ansi-color-for-comint-mode t
      comint-prompt-read-only t
      comint-buffer-maximum-size 4096)

;; Compilation
(setq compilation-ask-about-save nil
      compilation-always-kill t
      compilation-scroll-output 'first-error)

;; Skip confirmation prompts when creating a new file or buffer
(setq confirm-nonexistent-file-or-buffer nil)


;;; Backup files.
;; Avoid backups or lockfiles to prevent creating world-readable copies of files
(setq create-lockfiles nil)
(setq make-backup-files nil)


;;; Version Control.
(setq vc-git-print-log-follow t)
(setq vc-make-backup-files nil)  ; Do not backup version controlled files
(setq vc-git-diff-switches '("--histogram"))  ; Faster algorithm for diffing.


;;; Auto-save.
;; Enable auto-save to safeguard against crashes or data loss. The
;; `recover-file' or `recover-session' functions can be used to restore
;; auto-saved data.
(setq auto-save-default nil)
(setq auto-save-no-message t)

;; Do not auto-disable auto-save after deleting large chunks of text.
(setq auto-save-include-big-deletions t)

;; Where to save the auto-save files.
(setq auto-save-list-file-prefix
      (expand-file-name "autosave/" user-emacs-directory))

;; Offer to kill auto-save file when killing original buffer.
(setq kill-buffer-delete-auto-save-files t)


;;; Auto revert.
;; Auto-revert in Emacs is a feature that automatically updates the contents of
;; a buffer to reflect changes made to the underlying file.
(setq revert-without-query (list ".")  ; Do not prompt
      auto-revert-stop-on-user-input nil
      auto-revert-verbose t)

;; 06/23/2024: turn this on to automatically revert buffers when file changed.
;; Don't know if I actually need this single line with the above set. Docs say
;; merely turning on the global mode may not automatically revert files.
;; (global-auto-revert-mode 1)

;; Revert other buffers (e.g, Dired)
;; Use this option with care since it could lead to excessive auto-reverts.
(setq global-auto-revert-non-file-buffers t)
(setq global-auto-revert-ignore-modes '(Buffer-menu-mode))  ; Resolve issue #29


;;; recentf
;; `recentf' is an that maintains a list of recently accessed files.
(setq recentf-max-saved-items 300) ; default is 20
(setq recentf-max-menu-items 15)
(setq recentf-auto-cleanup 'mode)
(setq recentf-exclude nil)


;;; saveplace
;; Enables Emacs to remember the last location within a file upon reopening.
;; 12/08/2025: Cannot see variables, is this package? Comment out for now.
;; (setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
;; (setq save-place-limit 600)


;;; savehist
;; `savehist-mode' is an Emacs feature that preserves the minibuffer history
;; between sessions.
(setq history-length 300)                  ; also see `amx-history-length' 
(setq savehist-save-minibuffer-history t)  ; Default
(setq savehist-additional-variables
      '(kill-ring                          ; clipboard
        register-alist                     ; macros
        mark-ring global-mark-ring         ; marks
        search-ring regexp-search-ring))   ; searches


;;; Frames and windows
;; Do not resize windows pixelwise, as this can cause crashes in some
;; cases when resizing too many windows at once or rapidly.
(setq window-resize-pixelwise nil)

(setq resize-mini-windows 'grow-only)

;; The native border "uses" a pixel of the fringe on the rightmost
;; splits, whereas `window-divider-mode' does not.
(setq window-divider-default-bottom-width 1
      window-divider-default-places t
      window-divider-default-right-width 1)


;;; Fontification
;; Disable fontification during user input to reduce lag in large buffers.
;; Also helps marginally with scrolling performance.
(setq redisplay-skip-fontification-on-input t)


;;; Scrolling
;; Enables faster scrolling. This may result in brief periods of inaccurate
;; syntax highlighting, which should quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Move point to top/bottom of buffer before signaling a scrolling error.
(setq scroll-error-top-bottom t)

;; Keep screen position if scroll command moved it vertically out of the window.
(setq scroll-preserve-screen-position t)

;; Emacs recenters the window when the cursor moves past `scroll-conservatively'
;; lines beyond the window edge. A value over 101 disables recentering; the
;; default (0) is too eager. Here it is set to 20 for a balanced behavior.
(setq scroll-conservatively 20)

;; 1. Preventing automatic adjustments to `window-vscroll' for long lines.
;; 2. Resolving the issue of random half-screen jumps during scrolling.
(setq auto-window-vscroll nil)

;; Number of lines of margin at the top and bottom of a window.
(setq scroll-margin 0)

;; Number of lines of continuity when scrolling by screenfuls.
(setq next-screen-context-lines 0)

;; Horizontal scrolling
(setq hscroll-margin 2
      hscroll-step 1)


;;; Mouse
;; If non-nil, mouse yank commands yank at point instead of at click.
;; This also allows yanking text into an isearch without moving the
;; mouse cursor to the echo area.
(setq mouse-yank-at-point nil)


;;; Cursor
;; The blinking cursor is distracting and interferes with cursor settings in
;; some minor modes that try to change it buffer-locally (e.g., Treemacs).
(when (bound-and-true-p blink-cursor-mode)
  (blink-cursor-mode -1))

;; When typing closing paren, briefly jump to matching paren even if off screen.
(setq blink-matching-paren jump-offscreen)

;; Do not extend the cursor to fit wide characters
(setq x-stretch-cursor nil)

;; Reduce rendering/line scan work by not rendering cursors or regions in
;; non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; `nil': Do not extend the cursor to fit wide characters
;; `t': DO stretch the cursor to fit wide characters/glyphs. I like this.
(setq x-stretch-cursor t)

;; Reduce rendering/line scan work by not rendering cursors or regions in
;; non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)


;;; Text editing, indent, font, and formatting
;; Avoid automatic frame resizing when adjusting settings. Don't see it but let's try it.
(setq global-text-scale-adjust-resizes-frames nil)

;; A longer delay can be annoying as it causes a noticeable pause after each
;; deletion, disrupting the flow of editing.
(setq delete-pair-blink-delay 0.03)

(setq-default left-fringe-width  8)
(setq-default right-fringe-width 8)

;; Disable visual indicators in the fringe for buffer boundaries and empty lines
(setq-default indicate-buffer-boundaries nil)
(setq-default indicate-empty-lines nil)

;; Continue wrapped lines at whitespace rather than breaking in the
;; middle of a word.
(setq-default word-wrap t)

;; Disable wrapping by default due to its performance cost.
;; Don't set this to a non-nil value when visual-line-mode is
;; turned on, as it could produce confusing results.
;; and I DO set visual-line-mode, so this will be nil.
(setq-default truncate-lines nil)

;; If enabled and `truncate-lines' is disabled, soft wrapping will not occur
;; when the window is narrower than `truncate-partial-width-windows' characters.
(setq truncate-partial-width-windows nil)

;; Configure automatic indentation to be triggered exclusively by newline and
;; DEL (backspace) characters.
(setq-default electric-indent-chars '(?\n ?\^?))

;; Prefer spaces over tabs. Spaces offer a more consistent default compared to
;; 8-space tabs. This setting can be adjusted on a per-mode basis as needed.
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Enable indentation and completion using the TAB key
(setq tab-always-indent 'complete)
(setq tab-first-completion 'word-or-paren-or-punct)

;; Perf: Reduce command completion overhead.
;; 12/09/2025: current value is `transient-command-completion-not-suffix-only-p' ?
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; Enable multi-line commenting which ensures that `comment-indent-new-line'
;; properly continues comments onto new lines.
(setq comment-multi-line t)

;; Ensures that empty lines within the commented region are also commented out.
;; This prevents unintended visual gaps and maintains a consistent appearance.
(setq comment-empty-lines t)

;; We often split terminals and editor windows or place them side-by-side,
;; making use of the additional horizontal space.
(setq-default fill-column 80)

;; Disable the obsolete practice of end-of-line spacing from the typewriter era.
(setq sentence-end-double-space nil)

;; According to the POSIX, a line is defined as "a sequence of zero or more
;; non-newline characters followed by a terminating newline".
(setq require-final-newline t)

;; Eliminate delay before highlighting search matches
(setq lazy-highlight-initial-delay 0)


;;; Modeline
;; Makes Emacs omit the load average information from the mode line.
(setq display-time-default-load-average nil)


;;; Filetype
;; Do not notify the user each time Python tries to guess the indentation offset
(setq python-indent-guess-indent-offset-verbose nil)

(setq sh-indent-after-continuation 'always)


;;; Dired and ls-lisp
(setq dired-free-space nil                 ; Do not try to display free space
      dired-dwim-target t                  ; Propose a target for intelligent moving/copying
      dired-deletion-confirmer 'y-or-n-p
      ;; Cannot find the below in documentation or web.
      ;; dired-filter-verbose nil 
      dired-recursive-deletes 'top         ; ask for each top-level, delete sub-directories without asking
      dired-recursive-copies 'always       ; copy recursively without asking.
      dired-vc-rename-file t               ; Use `vc-rename-file' to perform rename if under VC control.
      dired-create-destination-dirs 'ask
      ;; Suppress Dired buffer kill prompt for deleted dirs
      dired-clean-confirm-killing-deleted-buffers nil)

;; This is a higher-level predicate that wraps `dired-directory-changed-p'
;; with additional logic. This `dired-buffer-stale-p' predicate handles remote
;; files, wdired, unreadable dirs, and delegates to dired-directory-changed-p
;; for modification checks.
(setq auto-revert-remote-files nil)         ; remote files are not reverted in Auto Revert modes.
;; Revert buffer if `dired-buffer-stale-p' returns non-nil.
(setq dired-auto-revert-buffer 'dired-buffer-stale-p)

;; dired-omit-mode: these variables only "available" when dired-omit-mode is enabled.
;; The first eliminates the "Omitted N lines" when mode activated.
;; The second is the regex describing lines to hide, in this case only the current directory (".").
;; Original value: "\\`[.]?#\\|\\`[.][.]?\\'"
(setq dired-omit-verbose nil
      dired-omit-files (concat "\\`[.]\\'"))

;; Useful ONLY in Windows where Emacs has to fake an "ls" listing with "ls-lisp.el".
;; The nil for verbosity will not show UID, GID or links count.
;; The "dirs-first t" lists directories first instead of filename order.
;; The "ignore-case t" will sort files while ignoring case.
(setq ls-lisp-verbosity nil)
(setq ls-lisp-dirs-first t)
(setq ls-lisp-ignore-case t)


;;; Ediff
;; Configure Ediff to use a single frame and split windows horizontally
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)


;;; Help
;; Enhance `apropos' and related functions to perform more extensive searches
(setq apropos-do-all t)


;;; Eglot
;; A setting of nil or 0 means Eglot will not block the UI at all, allowing
;; Emacs to remain fully responsive, although LSP features will only become
;; available once the connection is established in the background.
(setq eglot-sync-connect 0)

(setq eglot-autoshutdown t)  ; Shut down server after killing last managed buffer

;; Activate Eglot in cross-referenced non-project files
(setq eglot-extend-to-xref t)

;; Eglot optimization, triggered ONLY when Emacs run with "--init-debug"
(defvar my-emacs-debug (bound-and-true-p init-file-debug)
  "Non-nil to enable debug.")

(if my-emacs-debug
    (setq eglot-events-buffer-config '(:size 2000000 :format full))
    ;; This reduces log clutter to improves performance.
    (setq jsonrpc-event-hook nil)
    ;; Reduce memory usage and avoid cluttering *EGLOT events* buffer
    (setq eglot-events-buffer-size 0)  ; Deprecated
    (setq eglot-events-buffer-config '(:size 0 :format short)))

(setq eglot-report-progress my-emacs-debug)  ; Prevent minibuffer spam


;;; Flymake
(setq flymake-show-diagnostics-at-end-of-line nil)

;; Disable wrapping around when navigating Flymake errors.
(setq flymake-wrap-around nil)


;;; hl-line-mode
;; Restrict `hl-line-mode' highlighting to the current window, reducing visual
;; clutter and slightly improving `hl-line-mode' performance.
(setq hl-line-sticky-flag nil)
(setq global-hl-line-sticky-flag nil)


;; HAVE SKIPPED OVER sections for icomplete, flyspell, and ispell as I don't use them.
;; Lines 486 through 507.


;;; ibuffer
;; Set up how the ibuffer listing is created. I don't understand this one.
(setq ibuffer-formats
      '((mark modified read-only locked
              " " (name 55 55 :left :elide)
              " " (size 8 -1 :right)
              " " (mode 18 18 :left :elide) " " filename-and-process)
        (mark " " (name 16 -1) " " filename)))


;; Skipping the "xref" section as I will continue to use Consult with `consult-xref'.


;;; abbrev
;; One CAN define/save abbreviations on the fly, although I prefer using my module.
;; This section governs how those "on-the-fly" abbreviations are saved.
;; Ensure `abbrev_defs` is stored in the correct location when
;; `user-emacs-directory` is modified, as it defaults to ~/.emacs.d/abbrev_defs
;; regardless of the change.
(setq abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory))
(setq save-abbrevs 'silently)


;; START LINE 532, "dabbrev"
