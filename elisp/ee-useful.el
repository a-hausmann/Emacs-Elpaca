;; -*- lexical-binding: t -*-
;; File name:     ee-useful.el
;; Created:       2023-07-30
;; Last modified: Sat Aug 05, 2023 17:26:47
;; Purpose:       Some useful but minor functions.
;;

;; Automatically update Last modified value.
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


;; Clean up buffers
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


;; Switch to previous buffer
(defun aeh/switch-to-previous-buffer ()
  "Switch to previously open buffer.
  Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))


;; Delete current buffer & file.
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


;; Rename current buffer & file.
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


;; Edit file as root, one of Bailey Ling's functions.
(defun aeh/edit-file-as-root (file)
  "Edits a file as root."
  (interactive "f")
  (find-file-other-window (concat "/sudo:root@localhost:" file)))


;; After you split a window, the focus remains in the original window. 
;; Uncle Dave disliked this so much he wrote two functions to fix the problem.
(defun aeh/split-and-follow-horizontally ()
  "Split window to below and switch to new window."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun aeh/split-and-follow-vertically ()
  "Split window to the right and switch to new window."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(general-def
  "C-x 2" 'aeh/split-and-follow-horizontally
  "C-x 3" 'aeh/split-and-follow-vertically)


;; We always want to kill the current buffer instead of having to ask.
(defun aeh/kill-current-buffer ()
  "Kills the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
(general-def
  "C-x k" 'aeh/kill-current-buffer)

;; Set up preferred JSON indentation with a hook.
(defun aeh/json-mode-preferred-indent ()
  (setq-local js-indent-level 2))
(add-hook 'json-mode-hook 'aeh/js2-mode-preferred-indent)


;; Create a new, untitled buffer.
(defun aeh/new-untitled-buffer ()
  "Create new buffer named \"untitled\""
  (interactive)
  (switch-to-buffer (generate-new-buffer "untitled"))
  (text-mode))
;; (global-set-key (kbd "C-c n") 'aeh/new-untitled-buffer)
(general-def
  :states 'normal
  "C-c n" 'aeh/new-untitled-buffer)


;; Can be useful in debugging, log begin/end positions of region
(defun aeh/ff ()
  "Display positions at begin and end of a region."
  (interactive)
  (message "begin at %s; end at %s" (region-beginning) (region-end)))
(defalias 'ff 'aeh/ff)


;; Toggle narrowing, use this ALL the time.
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


;; DWIM to strip carriage returns from region/buffer
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
