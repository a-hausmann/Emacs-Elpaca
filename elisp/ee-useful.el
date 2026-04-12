;;; module ee-useful.el
;; -*- lexical-binding: t -*-
;; File name:     ee-useful.el
;; Created:       2023-07-30
;; Last modified: Mon Mar 30, 2026 19:39:59
;; Purpose:       Some useful but minor functions.
;;

;;; Automatically update Last modified value.

(defun aeh/set-last-modified-ts ()
  "Set new timestamp for \"Last modified: \" tag, or if in org-mode,
  the \"#+date: \" tag.  Function searches for string from point-min forward;
  when found, it deletes from point (at end of search string) to point-at-eol,
  then inserts current time in specified format. "
  (interactive)
  (if (equal major-mode 'org-mode)
      (save-excursion
        (goto-char (point-min))
        (when (search-forward "#+date: " nil t)
          ;; It appears I do NOT need a lambda here, just execute two functions on when()
          (delete-region (point) (point-at-eol))
          (let ((current-prefix-arg '(16)))
            (call-interactively 'org-time-stamp))))
    (save-excursion
      (goto-char (point-min))
      (when (search-forward "Last modified: " nil t)
        (delete-region (point) (point-at-eol))
        (insert (format-time-string "%a %b %d, %Y %-H:%M:%S"))))))
(add-hook 'before-save-hook
          (lambda () (aeh/set-last-modified-ts)))


;;; Clean up buffers

;; This stuff is stolen code from Magnar Sveen, from his What The Emacs blog (http://whattheemacsd.com). 
;; It is pretty self explanatory.
(defun ms/cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
  Does not indent buffer, because it is used for a before-save-hook, and that
  might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(defun ms/cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
  Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (ms/cleanup-buffer-safe)
  (indent-region (point-min) (point-max)))
;; (global-set-key (kbd "C-c n") 'ms/cleanup-buffer)


;;; Switch to previous buffer

(defun aeh/switch-to-previous-buffer ()
  "Switch to previously open buffer.
  Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; (general-def "C-c C-;" 'aeh/switch-to-previous-buffer)
(keymap-global-set "C-c C-;" 'aeh/switch-to-previous-buffer)
(keymap-global-set "C-<tab>" 'aeh/switch-to-previous-buffer)

;;; Kill other buffer and window

(defun aeh/kill-other-buffer-and-window ()
  "Kill the `other' buffer and window; useful to kill ibuffer and/or *Occur*
buffers/windows."
  (interactive)
  (ace-window 1)
  (aeh/kill-current-buffer)
  (delete-window))

;; (general-def "C-c C-k" 'aeh/kill-other-buffer-and-window)
(keymap-global-set "C-c C-k" '("Kill other buffer/window" . aeh/kill-other-buffer-and-window))

;;; Delete current buffer & file.

(defun aeh/delete-current-buffer-file ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))
;; (general-def "s-x C-f C-d" 'aeh/delete-current-buffer-file)
(keymap-global-set "s-x k" '("Kill current file/buffer" . aeh/delete-current-buffer-file))

;;; Rename current buffer & file.

(defun aeh/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

;;; Directory & file name code

;; Insert directory name of current file at point
(defun aeh/insert-default-directory ()
  "Insert the directory name of current file at point"
  (interactive)
  (insert default-directory))

;; Insert file name of current file at point
(defun aeh/insert-fname ()
  "Insert the value of current file name at point"
  (interactive)
  (insert (s-replace-regexp "^.*/" "" buffer-file-name)))

;; Insert FULL file name (of buffer) at point
(defun aeh/insert-full-file-name ()
  "Insert the FULL path & file name of current file at point"
  (interactive)
  (insert buffer-file-name))

;; Copy current file name to kill ring
(defun aeh/copy-fname-to-kill-ring ()
  "Copy the current file name to the kill ring."
  (interactive)
  (kill-new (s-replace-regexp "^.*/" "" buffer-file-name)))

;; Copy FULL file name & path (of buffer) to kill ring
(defun aeh/copy-full-file-name-to-kill-ring ()
  "Copy the FULL path & file name to the kill ring."
  (interactive)
  (kill-new buffer-file-name))

(defun aeh/insert-file-name-directory ()
  "Insert the file-name-directory of the current buffer-file-name"
  (interactive)
  (insert (file-name-directory (buffer-file-name))))



;;; Edit file as root, one of Bailey Ling's functions.

(defun aeh/edit-file-as-root (file)
  "Edits a file as root."
  (interactive "f")
  (find-file-other-window (concat "/sudo:root@localhost:" file)))


;;; After you split a window, the focus remains in the original window. 

;; Uncle Dave disliked this so much he wrote two functions to fix the problem.
(defun aeh/split-right-and-follow ()
  "Split window to the right and switch to new window."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(keymap-global-set "C-x 2" '("Split Right" . aeh/split-right-and-follow))

(defun aeh/split-below-and-follow ()
  "Split window to below and switch to new window."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(keymap-global-set "C-x 3" '("Split Below" . aeh/split-below-and-follow))

;;; We always want to kill the current buffer instead of having to ask.

(defun aeh/kill-current-buffer ()
  "Kills the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
(keymap-global-set "C-x k" '("Kill Current Buffer" . aeh/kill-current-buffer))

;;; Set up preferred JSON indentation with a hook.

(defun aeh/json-mode-preferred-indent ()
  (interactive)
  (setq-local js-indent-level 2))
(add-hook 'json-mode-hook 'aeh/json-mode-preferred-indent)
(general-def
    :maps 'js-json-mode-map
    "C-c j" '("Set JSON indent 2" . aeh/json-mode-preferred-indent))
;; (keymap-set js-json-mode-map "C-c j" 'aeh/json-mode-preferred-indent)

;;; Create a new, untitled buffer.

(defun aeh/new-untitled-buffer ()
  "Create new buffer named `*Untitled*'"
  (interactive)
  (switch-to-buffer (generate-new-buffer "*Untitled*"))
  (text-mode))
;; (global-set-key (kbd "C-c n") 'aeh/new-untitled-buffer)
;; 03/26/2026: Not sure why I EVER changed this to normal-state only, but making it global again.
;; (general-def
;;   :states 'normal
;;   "C-c n" '("Create *Untitled*" . aeh/new-untitled-buffer))
(keymap-global-set "C-c n" '("Create *Untitled*" . aeh/new-untitled-buffer))


;;; Toggle narrowing, use this ALL the time.

(defun aeh/narrow-dwim ()
  "Toggle narrowing."
  (interactive)
  (cond ((region-active-p)
          ;; If region is highlighted, narrow to that
          (call-interactively #'narrow-to-region)
          (deactivate-mark t))
    ((buffer-narrowed-p)
      ;; Otherwise widen if narrowed
      (widen))
    ((derived-mode-p 'org-mode)
      (call-interactively #'org-narrow-to-subtree))
    (t
      (message "Do not know what to narrow to.")
      (call-interactively #'narrow-to-defun))))
;; (global-set-key (kbd "C-x n w") 'aeh/narrow-dwim)
(general-def
  :states 'normal
  "C-x n w" 'aeh/narrow-dwim)


;;; Some oddball stuff.

;; Can be useful in debugging, log begin/end positions of region
(defun aeh/ff ()
  "Display positions at begin and end of a region."
  (interactive)
  (message "begin at %s; end at %s" (region-beginning) (region-end)))
(defalias 'ff 'aeh/ff)

;; Used at Trinity to "prettify" Rules.
(defun aeh/make-pretty (p-from p-thru)
  "Prettify Rule code by moving all and/or conjunctions to a new line"
  (interactive "r")
  (save-match-data
    (save-excursion
      (save-restriction
        (let ((change-count 0))
          (goto-char p-from)
          (while (re-search-forward "\\( and \\| or \\)" p-thru t )
            (setq change-count (+ change-count 1))
            (replace-match "
\\1" nil nil))
          (message (format "Made %d changes." change-count)))))))

(defun aeh/prettify-rule-dwim ()
  "The dwim will allow for prettifying by either region or full buffer."
  (interactive)
  (cond ((region-active-p)
          (aeh/make-pretty (region-beginning) (region-end)))
    (t (aeh/make-pretty (point-min) (point-max)))))



;;; DWIM to strip carriage returns from region/buffer

(defun aeh/delete-carriage-return-dwim ()
  "The dwim will delete carriage return by either region or full buffer."
  (interactive)
  (cond ((region-active-p)
          (aeh/strip-ctl-m (region-beginning) (region-end)))
    (t (aeh/strip-ctl-m (point-min) (point-max)))))

(defun aeh/strip-ctl-m (p-from p-thru)
  "Replace carriage returns (^M) with nil"
  (interactive)
  (save-match-data
    (save-excursion
      (save-restriction
        (let ((remove-count 0))
          (goto-char p-from)
          (while (re-search-forward (concat (char-to-string 13) "$") p-thru t)
            (setq remove-count (+ remove-count 1))
            (replace-match "" nil nil))
          (message (format "%d ^M removed from buffer." remove-count)))))))


;;; Sane tabs or spaces.

;; Ref: https://dougie.io/emacs/indentation/#using-tabs-or-spaces-in-different-files
;; Set the defaults, NO tabs
(setq-default indent-tabs-mode nil)

;; Create a variable for our preferred tab width
(setq custom-tab-width 2)

;; Two callable functions for enabling/disabling tabs in Emacs
(defun aeh/disable-tabs ()
  "Custom function to disable tabs"
  (interactive)
  (setq indent-tabs-mode nil))
(defun aeh/enable-tabs  ()
  "Custom function to enable tabs"
  (interactive)
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq indent-tabs-mode t)
  (setq tab-width custom-tab-width))

;; Hooks to Disable Tabs
(add-hook 'lisp-mode-hook 'aeh/disable-tabs)
(add-hook 'emacs-lisp-mode-hook 'aeh/disable-tabs)
(add-hook 'sql-mode-hook 'aeh/disable-tabs)


;;; Convert space to underline DWIM

;; FIXME: this should be able to be done in ONE function.
(defun aeh-space-to-underline-dwim ()
  "The dwim will replace space with underline in either region or full buffer."
  (interactive)
  (cond ((region-active-p)
          (aeh-replace-space-with-underline (region-beginning) (region-end))
          )
    (t
      (aeh-replace-space-with-underline (point-min) (point-max)))))
(keymap-global-set "C-c _" 'aeh-space-to-underline-dwim)

(defun aeh-replace-space-with-underline (p-from p-thru)
  "Replace spaces with underlines"
  (interactive)
  (save-match-data
    (save-excursion
      (save-restriction
        (let ((remove-count 0))
          (goto-char p-from)
          (while (search-forward " " p-thru t)
            (replace-match "_" nil nil)))))))


;;; Delete quotes DWIM

;; FIXME: this should be able to be done in ONE function.
(defun aeh-delete-quotes-dwim ()
  "The dwim will delete quotes in either region or current line."
  (interactive)
  (cond ((region-active-p)
          (aeh-delete-quotes-line-or-region (region-beginning) (region-end))
          )
    (t
      (aeh-delete-quotes-line-or-region (point-at-bol) (point-at-eol)))))

(keymap-set text-mode-map "C-c '" 'aeh-delete-quotes-dwim)

(defun aeh-delete-quotes-line-or-region (p-from p-thru)
  (interactive)
  (save-match-data
    (save-excursion
      (save-restriction
        (let ((remove-count 0))
          (goto-char p-from)
          (while (search-forward-regexp "['\"]" p-thru t)
            (replace-match "" nil nil)))))))


;;; Delete word at point, needs work, commented

;; Needs work still, cannot figure out how to configure delimiter to not space.
;; (cl-defun aeh-delete-word-at-point (&optional (ARG 1) (CHAR ?\s))
;;   "Uses zap-to-char to delete the (big) word at point.
;; Words are defined with space as default, and the space will also be
;; deleted. Option ARG is the count of words to be deleted, and option CHAR allows
;; for a character other than space as delimiter."
;;   (interactive "P")
;;   (save-excursion
;;     (let ((count (or ARG 1))
;;           (delimiter CHAR))
;;       ;; (message "Count: %d, delimiter: >%s<" count delimiter)
;;       (backward-word 1)
;;       (zap-to-char count delimiter))))
;; (keymap-global-set "C-c C-w" 'aeh-delete-word-at-point)


;;; Code for working with marks.

;; https://www.masteringemacs.org/article/fixing-mark-commands-transient-mark-mode
(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))
(keymap-global-set "C-`" 'push-mark-no-activate)


(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
(keymap-global-set "C-M-`" 'jump-to-mark)


(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))
;; 07/21/2025: Changing mapping from redefining C-x C-x as new function to
;; mapping new function to new key chord while retaining the old key chord/function.
;; (define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)
(define-key global-map (kbd "C-x M-x") 'exchange-point-and-mark-no-activate)


;;; Command of the day documentation.

;; 05/24/2024
;; https://github.com/minad/org-modern/blob/main/example.org
;; Taken from the well-structured Emacs config by @oantolin.
;; Take a look at https://github.com/oantolin/emacs-config!
(defun aeh/command-of-the-day ()
  "Show the documentation for a random command."
  (interactive)
  (let ((commands))
    (mapatoms (lambda (s)
                (when (commandp s) (push s commands))))
    (describe-function
     (nth (random (length commands)) commands))))
(keymap-global-set "<f12>" 'aeh/command-of-the-day)


;;; Create a flowerbox for documentation.

(defun aeh/emacs-flower-box ()
  (interactive)
  (insert-char #x3B 80)
  (newline)
  (insert ";; \n")
  (insert-char #x3B 80)
  (previous-line)
  (end-of-line))


;;; Customize Ibuffer functionality

;; 06/07/2024: adding function to open ibuffer without splitting windows, new buffer name, "q" to quit.
(defun aeh/open-and-goto-ibuffer ()
  "Open an ibuffer window and make it the active window."
  (interactive)
  (list-buffers nil "*Buffers*" nil nil))
(defalias 'list-buffers 'aeh/open-and-goto-ibuffer)


;;; Copy calendar dates to kill ring as formatted date.

;; 03/06/2025: Found some useful code to allow me to copy ("M-w") calendar dates
;; to kill ring as a specifically formatted string; this allows yank into buffer.
;; Ref: https://emacs.stackexchange.com/questions/41978/how-to-retrieve-the-date-under-the-cursor-in-emacs-calendar-as-the-format-day-mo/41987
(defcustom my-calendar-copy-as-kill-format "%Y-%m-%d"
  "Format string for formatting calendar dates with `format-time-string'."
  :type 'string
  :group 'calendar)

(defun my-calendar-copy-as-kill ()
  "Copy date at point as kill if region is not active.
Delegate to `kill-ring-save' otherwise."
  (interactive)
  (if (use-region-p)
      (call-interactively #'kill-ring-save)
    (let ((date (calendar-cursor-to-date)))
      (when date
        (setq date (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date)))
        (kill-new (format-time-string my-calendar-copy-as-kill-format date))))))

(defun my-calendar-mode-hook-fun ()
  "Let \[kill-ring-save] copy the date at point if region is not active."
  (local-set-key [remap kill-ring-save] #'my-calendar-copy-as-kill))

(add-hook 'calendar-mode-hook #'my-calendar-mode-hook-fun)


;;; Length of word at point or region

(defun length-of-word-at-point ()
  "Return the length of the word at point. (TY, Duck.ai)"
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (start (car bounds))
         (end (cdr bounds)))
    (if bounds
        (message "Length of word: %d" (- end start))
      (message "No word at point."))))
(keymap-global-set "C-s-=" 'length-of-word-at-point)

;; 04/24/2025: Ref: https://www.emacswiki.org/emacs/misc-cmds.el
(defun my/region-length ()
  "Return the length of the region as message."
  (interactive)
  (let ((len  (abs (- (mark) (point)))))
    (message "Length: %s" len)))
(keymap-global-set "C-M-s-=" 'my/region-length)


;;; Mark whole word (at point?)

;; 04/24/2025: Ref: https://www.emacswiki.org/emacs/misc-cmds.el
(defun my/mark-whole-word (&optional arg allow-extend)
  "Like `mark-word', but selects whole words and skips over whitespace.
If you use a negative prefix arg then select words backward.
Otherwise select them forward. Repetition works like expand-region,
but only in one direction, slurping the next word.

If cursor starts in the middle of word then select that whole word.

If there is whitespace between the initial cursor position and the
first word (in the selection direction), it is skipped (not selected).

If the command is repeated or the mark is active, select the next NUM
words, where NUM is the numeric prefix argument.  (Negative NUM
selects backward.)"
  (interactive "P\np")
  (let ((num  (prefix-numeric-value arg)))
    (unless (eq 'mark-whole-word last-command)
      (if (natnump num)
          (skip-syntax-forward "\\s-")
          (skip-syntax-backward "\\s-")))
    (unless (or (eq 'mark-whole-word last-command)
                (if (natnump num) (looking-at "\\b") (looking-back "\\b")))
      (if (natnump num)
          (if (fboundp 'left-word)    ; Emacs 24+
              (left-word)
              (backward-word 1))
          (if (fboundp 'right-word)
              (right-word)
              (forward-word 1))))
    (mark-word arg allow-extend)))

(keymap-global-set "C-+" 'my/mark-whole-word)


;;; Prot's custom functions for window configs & inserting pairs (replacement for electric-pair?)

;; 03/26/2026: Ref: https://protesilaos.com/codelog/2020-08-03-emacs-custom-functions-galore/
;; Maximise window + kill buffer (and close window)
(use-package emacs
  :ensure nil
  :config
  (defvar prot/window-configuration nil
    "Current window configuration.
Intended for use by `prot/window-monocle'.")

  (define-minor-mode prot/window-single-toggle
    "Toggle between multiple windows and single window.
This is the equivalent of maximising a window.  Tiling window
managers such as DWM, BSPWM refer to this state as 'monocle'."
    :lighter " [M]"
    :global nil
    (if (one-window-p)
        (when prot/window-configuration
          (set-window-configuration prot/window-configuration))
      (setq prot/window-configuration (current-window-configuration))
      (delete-other-windows)))

  (defun prot/kill-buffer-current (&optional arg)
    "Kill current buffer or abort recursion when in minibuffer."
    (interactive "P")
    (if (minibufferp)
        (abort-recursive-edit)
      (kill-buffer (current-buffer)))
    (when (and arg
               (not (one-window-p)))
      (delete-window)))

  ;; Insert pairs.
    (defconst prot/insert-pair-alist
    '(("' Single quote" . (39 39))           ; ' '
      ("\" Double quotes" . (34 34))         ; " "
      ("` Elisp quote" . (96 39))            ; ` '
      ("‘ Single apostrophe" . (8216 8217))  ; ‘ ’
      ("“ Double apostrophes" . (8220 8221)) ; “ ”
      ("( Parentheses" . (40 41))            ; ( )
      ("{ Curly brackets" . (123 125))       ; { }
      ("[ Square brackets" . (91 93))        ; [ ]
      ("< Angled brackets" . (60 62))        ; < >
      ;; ("« Εισαγωγικά Gr quote" . (171 187))) ; « »
      )
    "Alist of pairs for use with `prot/insert-pair-completion'.")

  (defun prot/insert-pair-completion (&optional arg)
    "Insert pair from `prot/insert-pair-alist'."
    (interactive "P")
    (let* ((data prot/insert-pair-alist)
           (chars (mapcar #'car data))
           (choice (completing-read "Select character: " chars nil t))
           (left (cadr (assoc choice data)))
           (right (caddr (assoc choice data))))
      (insert-pair arg left right)))

  :bind (("s-m" . prot/window-single-toggle)
         ("s-k" . prot/kill-buffer-current)
         ("s-i C-p" . prot/insert-pair-completion))
  )  ; End, use-package emacs




;; ee-useful.el ends here.
