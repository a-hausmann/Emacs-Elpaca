;; -*- lexical-binding: t; eval: (keymap-local-set "C-c i" #'consult-outline); outline-regexp: ";;;"; -*-
;; File name:     my-modeline.el
;; Created:       Sat Jul 23, 2022 17:04:51
;; Last modified: Mon Mar 30, 2026 17:53:22
;; Purpose:       This is to create my own, personalized modeline.
;;

;; 12/22/2025: Copied from old myDotemacs repository, renamed to current file.
;; NOW REQUIRES EMACS 30.1+ TO WORK CORRECTLY, WITH RIGHT-ALIGNMENT!!!

;; 02/22/2026: Need to look into Smart Mode Line, https://github.com/Malabarba/smart-mode-line/
;; This package can (MAYBE) shorten buffer file names so it doesn't push other things off the mode line.

;;; Current info:
;; Ref: https://github.com/protesilaos/dotfiles/tree/master/emacs/.emacs.d
;; Ref: https://www.youtube.com/watch?v=Qf_DLPIA9Cs  (Prot's write a custom mode line)
;; Ref: https://github.com/protesilaos/dotfiles/blob/master/emacs/.emacs.d/prot-lisp/prot-modeline.el
;; Ref: https://github.com/protesilaos/dotfiles/blob/master/emacs/.emacs.d/prot-emacs-modules/prot-emacs-modeline.el

;;; OLD references & info:
;; Set active window & inactive window modelines to different colors with fringe
;; Ref: https://www.saltycrane.com/blog/2007/10/emacs-mode-line-color-custimization/
;; see http://www.gnu.org/software/emacs/manual/html_node/emacs/Standard-Faces.html
;; (set-face-background 'modeline          "#4466aa")
;; (set-face-background 'modeline-inactive "#99aaff")
;; (set-face-background 'fringe "#809088")

;; I've likes Prot's modeline for a long time, and he now has his emacs config in 
;; pure emacs-lisp, not a literate file. So I will use bits and chunks of that.
;; Ref (main): https://git.sr.ht/~protesilaos/dotfiles/tree/master/item/emacs/.emacs.d/
;; Ref: https://git.sr.ht/~protesilaos/dotfiles/tree/master/item/emacs/.emacs.d/prot-emacs-modules/prot-emacs-modeline.el


;;; Actual code

(setq-default my-modeline-position (list "(%l:%c)"))    ; Emacs <28
(setq-default mode-line-compact 'long)                  ; Emacs 28: only long modelines will be compacted
(setq-default mode-line-right-align-edge 'right-margin) ; Emacs 30


;;;; Common helper functions

(defun prot-common-window-narrow-p ()
  "Return non-nil if window is narrow.
Check if the `window-width' is less than `split-width-threshold'."
  (and (numberp split-width-threshold)
       (< (window-total-width) split-width-threshold)))

;; Removed condition of "(not (one-window-p :no-minibuffer))" so all windows affected.
(defun prot-modeline--string-truncate-p (str)
  "Return non-nil if STR should be truncated."
  (cond
   ((or (not (stringp str))
        (string-empty-p str)
        (string-blank-p str))
    nil)
   ((and (prot-common-window-narrow-p)
         (> (length str) prot-modeline-string-truncate-length)))))

(defun prot-modeline-string-cut-middle (str)
  "Return truncated STR, if appropriate, else return STR.
Cut off the middle of STR by counting half of
`prot-modeline-string-truncate-length' both from its beginning
and end."
  (interactive "s\Input string: ")
  (let ((half (floor prot-modeline-string-truncate-length 2)))
    (if (prot-modeline--string-truncate-p str)
        (concat (substring str 0 half) "..." (substring str (- half)))
      str)))

(defun prot-modeline--first-char (str)
  "Return first character from STR."
  (substring str 0 1))

(defun prot-modeline-string-abbreviate (str)
  "Abbreviate STR individual hyphen or underscore separated words.
Also see `prot-modeline-string-abbreviate-but-last'."
  (if (prot-modeline--string-truncate-p str)
      (mapconcat #'prot-modeline--first-char (split-string str "[_-]") "-")
    str))

(defun prot-modeline-string-abbreviate-but-last (str nthlast)
  "Abbreviate STR, keeping NTHLAST words intact.
Also see `prot-modeline-string-abbreviate'."
  (if (prot-modeline--string-truncate-p str)
      (let* ((all-strings (split-string str "[_-]"))
             (nbutlast-strings (nbutlast (copy-sequence all-strings) nthlast))
             (last-strings (nreverse (ntake nthlast (nreverse (copy-sequence all-strings)))))
             (first-component (mapconcat #'prot-modeline--first-char nbutlast-strings "-"))
             (last-component (mapconcat #'identity last-strings "-")))
        (if (string-empty-p first-component)
            last-component
          (concat first-component "-" last-component)))
    str))


;;;; Keyboard macro indicator

(defvar-local prot-modeline-kbd-macro
    '(:eval
      (when (and (mode-line-window-selected-p) defining-kbd-macro)
        (propertize " KMacro " 'face 'prot-modeline-indicator-blue-bg)))
  "Mode line construct displaying `mode-line-defining-kbd-macro'.
Specific to the current window's mode line.")

;;;; Narrow indicator

(defvar-local prot-modeline-narrow
    '(:eval
      (when (and (mode-line-window-selected-p)
                 (buffer-narrowed-p)
                 (not (derived-mode-p 'Info-mode 'help-mode 'special-mode 'message-mode)))
        (propertize " Narrow " 'face 'prot-modeline-indicator-cyan-bg)))
  "Mode line construct to report the narrowed state of the current buffer.")

;;;; Buffer name and modified status

;; String length after which truncation is done.
(defvar prot-modeline-string-truncate-length 60)

(defun prot-modeline-buffer-identification-face ()
  "Return appropriate face or face list for `prot-modeline-buffer-identification'."
  (let ((file (buffer-file-name)))
    (cond
     ((and (mode-line-window-selected-p)
           file
           (buffer-modified-p))
      '(italic mode-line-buffer-id))
     ((and file (buffer-modified-p))
      'italic)
     ((mode-line-window-selected-p)
      'mode-line-buffer-id))))

(defun prot-modeline--buffer-name ()
  "Return `buffer-name', truncating it if necessary.
See `prot-modeline-string-cut-middle'."
  (when-let* ((name (buffer-name)))
    (prot-modeline-string-cut-middle name)))

(defun prot-modeline-buffer-name ()
  "Return buffer name, with read-only indicator if relevant."
  (let ((name (prot-modeline--buffer-name)))
    (if buffer-read-only
        (format "%s %s" (char-to-string #xE0A2) name)
      name)))

(defun prot-modeline-buffer-name-help-echo ()
  "Return `help-echo' value for `prot-modeline-buffer-identification'."
  (concat
   (propertize (buffer-name) 'face 'mode-line-buffer-id)
   "\n"
   (propertize
    (or (buffer-file-name)
        (format "No underlying file.\nDirectory is: %s" default-directory))
    'face 'font-lock-doc-face)))

(defvar-local prot-modeline-buffer-identification
    '(:eval
      (propertize (prot-modeline-buffer-name)
                  'face (prot-modeline-buffer-identification-face)
                  'mouse-face 'mode-line-highlight
                  'help-echo (prot-modeline-buffer-name-help-echo)))
  "Mode line construct for identifying the buffer being displayed.
Propertize the current buffer with the `mode-line-buffer-id'
face.  Let other buffers have no face.")

;;;; misc-info
(defvar-local my-modeline-misc-info
    '(:eval
      (when (mode-line-window-selected-p)
        mode-line-misc-info))
  "Mode line construct displaying `mode-line-misc-info'.
Specific to the current window's mode line.")


;;; MUST SET ALL CUSTOM VARIABLES AS `risky-local-variable'
(dolist (construct '(prot-modeline-kbd-macro
                     prot-modeline-narrow
                     prot-modeline-buffer-identification
                     my-modeline-misc-info))
  (put construct 'risky-local-variable t))


;; According to Prot, this should be a `setq-default' 
(setq-default mode-line-format
              '("%e"
                mode-line-front-space               ; shows: leading space.
                mode-line-mule-info                 ; shows: multi-lingual environment info
                prot-modeline-kbd-macro
                prot-modeline-narrow
                mode-line-modified                  ; shows: writeable, modified
                mode-line-remote                    ; shows: if local/remote
                mode-line-frame-identification
                prot-modeline-buffer-identification ; shows: buffer-name, possibly abbreviated.
                " | "
                my-modeline-position                ; shows: line:column in parentheses
                " "
                mode-line-modes                     ; shows: all modes, good with `Minions'
                (vc-mode vc-mode)                   ; shows: version control method & branch/hash
                mode-line-format-right-align        ; Emacs 30+
                mode-line-misc-info
                mode-line-end-spaces
                ))

;; This is the default value of `mode-line-format':
;; ("%e"
;;  mode-line-front-space
;;  (:propertize
;;   (""
;;    mode-line-mule-info            ; multi-lingual environment info
;;    mode-line-client               ; identifies emacsclient frames
;;    mode-line-modified             ; buffer modification info
;;    mode-line-remote               ; indicates remote buffer (tramp)
;;    mode-line-window-dedicated)    ; describes the current window (?)
;;   display (min-width (6.0)))
;;  mode-line-frame-identification   ; describes the current frame
;;  mode-line-buffer-identification  ; displays buffer name or other info depending on major mode
;;  "   "
;;  mode-line-position               ; position in buffer: percentage, size, line number, column number.
;;  (project-mode-line               ; Emacs30+: current project name & menu
;;   project-mode-line-format)       ; see `project-mode-line'
;;  (vc-mode vc-mode)                ; displays version control system (Git) and branch name
;;  "  "
;;  mode-line-modes                  ; displays major & minor modes
;;  mode-line-misc-info              ; displays misc. info, default value is `global-mode-string'
;;  mode-line-end-spaces)


;; (setopt mode-line-right-align-edge 'window)
(setq-default mode-line-right-align-edge 'window)


;; Minor modes that are also shown directly in the mode line.
(setq-default minions-prominent-modes
              (list 'defining-kbd-macro
                    'beacon-mode
                    'flymake-mode))

(message "Loaded my-modeline.el")

;; End of my-modeline.el

(provide 'my-modeline)
