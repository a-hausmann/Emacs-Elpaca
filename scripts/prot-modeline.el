;;; prot-modeline.el --- Code for my custom mode line -*- lexical-binding: t -*-

;; Copyright (C) 2023  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(defgroup prot-modeline nil
  "Custom modeline that is stylistically close to the default."
  :group 'mode-line)

(defgroup prot-modeline-faces nil
  "Faces for my custom modeline."
  :group 'prot-modeline)

(defcustom prot-modeline-string-truncate-length 9
  "String length after which truncation should be done in small windows."
  :type 'natnum)

;;;; Faces

(defface prot-modeline-indicator-red
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#880000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#ff9f9f")
    (t :foreground "red"))
  "Face for modeline indicators (e.g. see my `notmuch-indicator')."
  :group 'prot-modeline-faces)

(defface prot-modeline-indicator-red-bg
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :background "#aa1111" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#ff9090" :foreground "black")
    (t :background "red" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'prot-modeline-faces)

(defface prot-modeline-indicator-green
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#005f00")
    (((class color) (min-colors 88) (background dark))
     :foreground "#73fa7f")
    (t :foreground "green"))
  "Face for modeline indicators (e.g. see my `notmuch-indicator')."
  :group 'prot-modeline-faces)

(defface prot-modeline-indicator-green-bg
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :background "#207b20" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#77d077" :foreground "black")
    (t :background "green" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'prot-modeline-faces)

(defface prot-modeline-indicator-yellow
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#6f4000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#f0c526")
    (t :foreground "yellow"))
  "Face for modeline indicators (e.g. see my `notmuch-indicator')."
  :group 'prot-modeline-faces)

(defface prot-modeline-indicator-yellow-bg
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :background "#805000" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#ffc800" :foreground "black")
    (t :background "yellow" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'prot-modeline-faces)

(defface prot-modeline-indicator-blue
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#00228a")
    (((class color) (min-colors 88) (background dark))
     :foreground "#88bfff")
    (t :foreground "blue"))
  "Face for modeline indicators (e.g. see my `notmuch-indicator')."
  :group 'prot-modeline-faces)

(defface prot-modeline-indicator-blue-bg
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :background "#0000aa" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#77aaff" :foreground "black")
    (t :background "blue" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'prot-modeline-faces)

(defface prot-modeline-indicator-magenta
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#6a1aaf")
    (((class color) (min-colors 88) (background dark))
     :foreground "#e0a0ff")
    (t :foreground "magenta"))
  "Face for modeline indicators (e.g. see my `notmuch-indicator')."
  :group 'prot-modeline-faces)

(defface prot-modeline-indicator-magenta-bg
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :background "#6f0f9f" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#e3a2ff" :foreground "black")
    (t :background "magenta" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'prot-modeline-faces)

(defface prot-modeline-indicator-cyan
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#004060")
    (((class color) (min-colors 88) (background dark))
     :foreground "#30bbd0")
    (t :foreground "cyan"))
  "Face for modeline indicators (e.g. see my `notmuch-indicator')."
  :group 'prot-modeline-faces)

(defface prot-modeline-indicator-cyan-bg
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :background "#006080" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#40c0e0" :foreground "black")
    (t :background "cyan" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'prot-modeline-faces)

;;;; Common helper functions

(defun prot-modeline--string-truncate-p (str)
  "Return non-nil if STR should be truncated."
  (and (< (window-total-width) split-width-threshold)
       (> (length str) prot-modeline-string-truncate-length)
       (not (one-window-p :no-minibuffer))))

(defun prot-modeline-string-truncate (str)
  "Return truncated STR, if appropriate, else return STR.
Truncation is done up to `prot-modeline-string-truncate-length'."
  (if (prot-modeline--string-truncate-p str)
      (concat (substring str 0 prot-modeline-string-truncate-length) "...")
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
  "Mode line construct to report the multilingual environment.")

;;;; Input method

(defvar-local prot-modeline-input-method
    '(:eval
      (when current-input-method-title
        (propertize (format " %s" current-input-method-title)
                    'mouse-face 'mode-line-highlight)))
  "Mode line construct to report the multilingual environment.")

;;;; Buffer status

;; TODO 2023-07-05: What else is there beside remote files?  If
;; nothing, this must be renamed accordingly.
(defvar-local prot-modeline-buffer-status
    '(:eval
      (when (file-remote-p default-directory)
        (propertize "@" 'mouse-face 'mode-line-highlight)))
  "Mode line construct for showing remote file name.")

;;;; Buffer name and modified status

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
See `prot-modeline-string-truncate'."
  (when-let ((name (buffer-name)))
    (prot-modeline-string-truncate name)))

(defun prot-modeline-buffer-name ()
  "Return buffer name, with read-only indicator if relevant."
  (let ((name (prot-modeline--buffer-name)))
    (if buffer-read-only
        (format "%s %s" (char-to-string #xE0A2) name)
      name)))

(defun prot-modeline-buffer-name-help-echo ()
  "Return `help-echo' value for `prot-modeline-buffer-identification'."
  (concat
   (or (buffer-file-name)
       (format "No underlying file.\nDirectory is: %s" default-directory))))

(defvar-local prot-modeline-buffer-identification
    '(:eval
      (propertize (prot-modeline-buffer-name)
                  'face (prot-modeline-buffer-identification-face)
                  'mouse-face 'mode-line-highlight
                  'help-echo (prot-modeline-buffer-name-help-echo)))
  "Mode line construct for identifying the buffer being displayed.
Propertize the current buffer with the `mode-line-buffer-id'
face.  Let other buffers have no face.")

;;;; Major mode

(defun prot-modeline-major-mode-indicator ()
  "Return appropriate propertized mode line indicator for the major mode."
  (let ((indicator (cond
                    ((derived-mode-p 'text-mode) "§")
                    ((derived-mode-p 'prog-mode) "λ")
                    ((derived-mode-p 'comint-mode) ">_")
                    (t "◦"))))
    (propertize indicator 'face 'shadow)))

(defun prot-modeline-major-mode-name ()
  "Return capitalized `major-mode' without the -mode suffix."
  (capitalize (string-replace "-mode" "" (symbol-name major-mode))))

(defun prot-modeline-major-mode-help-echo ()
  "Return `help-echo' value for `prot-modeline-major-mode'."
  (format "Symbol: `%s'.  Derived from: `%s'"
          major-mode (get major-mode 'derived-mode-parent)))

(defvar-local prot-modeline-major-mode
    (list
     (propertize "%[" 'face 'prot-modeline-indicator-red)
     '(:eval
       (concat
        (prot-modeline-major-mode-indicator)
        " "
        (propertize
         (prot-modeline-string-truncate
          (prot-modeline-major-mode-name))
         'mouse-face 'mode-line-highlight
         'help-echo (prot-modeline-major-mode-help-echo))))
     (propertize "%]" 'face 'prot-modeline-indicator-red))
  "Mode line construct for displaying major modes.")

(defvar-local prot-modeline-process
    (list '("" mode-line-process))
  "Mode line construct for the running process indicator.")

;;;; Git branch and diffstat

(defun prot-modeline-diffstat (file)
  "Return shortened Git diff numstat for FILE."
  (when-let* ((output (shell-command-to-string (format "git diff --numstat %s" file)))
              (stats (split-string output "[\s\t]" :omit-nulls "[\s\f\t\n\r\v]+"))
              (added (nth 0 stats))
              (deleted (nth 1 stats)))
    (cond
     ((and (equal added "0") (equal deleted "0"))
      "")
     ((and (not (equal added "0")) (equal deleted "0"))
      (propertize (format "+%s" added) 'face 'shadow))
     ((and (equal added "0") (not (equal deleted "0")))
      (propertize (format "-%s" deleted) 'face 'shadow))
     (t
      (propertize (format "+%s -%s" added deleted) 'face 'shadow)))))

(declare-function vc-git-working-revision "vc-git" (file))

(defun prot-modeline--vc-text (file branch &optional face)
  "Prepare text for Git controlled FILE, given BRANCH.
With optional FACE, use it to propertize the BRANCH."
  (concat
   (propertize (char-to-string #xE0A0) 'face 'shadow)
   " "
   (propertize (capitalize branch)
               'face face
               'mouse-face 'highlight
               'help-echo (vc-git-working-revision file))
   " "
   (prot-modeline-diffstat file)))

(defun prot-modeline--vc-details (file branch &optional face)
  "Return Git BRANCH details for FILE, truncating it if necessary.
The string is truncated if the width of the window is smaller
than `split-width-threshold'."
  (let ((text (prot-modeline--vc-text file branch face)))
    (prot-modeline-string-truncate text)))

(defvar-local prot-modeline-vc-branch
    '(:eval
      (when-let* (((mode-line-window-selected-p))
                  (file (buffer-file-name))
                  ((vc-git-registered file))
                  (branches (vc-git-branches))
                  (branch (car branches))
                  (state (vc-state file 'Git))
                  (face (pcase state
                          ('added 'vc-locally-added-state)
                          ('edited 'vc-edited-state)
                          ('removed 'vc-removed-state)
                          ('missing 'vc-missing-state)
                          ('conflict 'vc-conflict-state)
                          ('locked 'vc-locked-state)
                          (_ 'vc-up-to-date-state))))
        (prot-modeline--vc-details file branch face)))
  "Mode line construct to return propertized VC branch.")

;;;; Flymake errors, warnings, notes

(declare-function flymake--severity "flymake" (type))
(declare-function flymake-diagnostic-type "flymake" (diag))

;; Based on `flymake--mode-line-counter'.
(defun prot-modeline-flymake-counter (type)
  "Compute number of diagnostics in buffer with TYPE's severity.
TYPE is usually keyword `:error', `:warning' or `:note'."
  (let ((count 0))
    (dolist (d (flymake-diagnostics))
      (when (= (flymake--severity type)
               (flymake--severity (flymake-diagnostic-type d)))
        (cl-incf count)))
    (when (cl-plusp count)
      (number-to-string count))))

(defmacro prot-modeline-flymake-type (type indicator &optional face)
  "Return function that handles Flymake TYPE with stylistic INDICATOR and FACE."
  `(defun ,(intern (format "prot-modeline-flymake-%s" type)) ()
     (when-let ((count (prot-modeline-flymake-counter
                        ,(intern (format ":%s" type)))))
       (concat
        (propertize ,indicator 'face 'shadow)
        (propertize count 'face ',(or face type))))))

(prot-modeline-flymake-type error "☣")
(prot-modeline-flymake-type warning "!")
(prot-modeline-flymake-type note "·" success)

;; (defvar prot-modeline-flymake-keymap
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map [mode-line down-mouse-1] 'flymake-show-buffer-diagnostics)
;;     map)
;;   "Keymap to display on Flymake indicator.")

(defvar-local prot-modeline-flymake
    `(:eval
      (when (and (bound-and-true-p flymake-mode)
                 (mode-line-window-selected-p))
        (list
         ;; FIXME 2023-07-03: Clicking on the text with this buffer
         ;; and a single warning present, the diagnostics take up the
         ;; entire frame.  Why?
         ;;
         ;; (propertize "Lint: "
         ;;             'mouse-face 'highlight
         ;;             'local-map prot-modeline-flymake-keymap
         ;;             'help-echo "Click to show diagnostics")

         ;; See the calls to the macro `prot-modeline-flymake-type'
         '(:eval (prot-modeline-flymake-error))
         '(:eval (prot-modeline-flymake-warning))
         '(:eval (prot-modeline-flymake-note)))))
  "Mode line construct displaying `flymake-mode-line-format'.
Specific to the current window's mode line.")

;;;; Right side alignment

(defun prot-modeline--right-align-rest ()
  "Return string if everything after `prot-modeline-align-right'."
  (format-mode-line
   `(""
     ,@(cdr (memq 'prot-modeline-align-right mode-line-format)))))

(defun prot-modeline--right-align-width ()
  "Return pixel width of `prot-modeline--right-align-rest'."
  (string-pixel-width (prot-modeline--right-align-rest)))

(defun prot-modeline--box-p ()
  "Return non-nil if the `mode-line' has a box attribute."
  (and (face-attribute 'mode-line :box)
       (null (eq (face-attribute 'mode-line :box) 'unspecified))))

;; NOTE 2023-07-13: I could also do what I am doing in
;; `fontaine--family-list-variable-pitch' and check if the family is a
;; member of those, but I don't need that as I always inherit
;; `variable-pitch' in my themes instead of hardcoding the family.
(defun prot-modeline--variable-pitch-p ()
  "Return non-nil if the `mode-line' inherits `variable-pitch'."
  (when-let* ((mode-line-inherit (face-attribute 'mode-line :inherit))
              ((string-match-p "variable-pitch" (symbol-name mode-line-inherit)))
              (family-face (face-attribute mode-line-inherit :inherit))
              (variable-pitch
               (if (listp family-face)
                   (memq 'variable-pitch family-face)
                 (eq 'variable-pitch family-face))))
    variable-pitch))

;; I just came up with this experimentally, but I am not sure if it is
;; the best approach.
(defun prot-modeline--magic-number ()
  "Return constant for use in `prot-modeline-align-right'."
  (let ((height (face-attribute 'mode-line :height nil 'default))
        (m-width (string-pixel-width (propertize "m" 'face 'mode-line))))
    (round height (* m-width (* height m-width 0.001)))))

(defvar-local prot-modeline-align-right
    '(:eval
      (propertize
       " "
       'display
       (let ((box-p (prot-modeline--box-p))
             (variable-pitch-p (prot-modeline--variable-pitch-p))
             (magic-number (prot-modeline--magic-number)))
         `(space
           :align-to
           (- right
              right-fringe
              right-margin
              ,(ceiling
                (prot-modeline--right-align-width)
                (string-pixel-width (propertize "m" 'face 'mode-line)))
              ,(cond
                ;; FIXME 2023-07-13: These hardcoded numbers are
                ;; probably wrong in some case.  I am still testing.
                ((and variable-pitch-p box-p)
                 (* magic-number 0.5))
                ((and (not variable-pitch-p) box-p)
                 (* magic-number 0.25))
                ((and variable-pitch-p (not box-p))
                 (* magic-number 0.05))
                ;; No box, no variable pitch, but I am keeping it as
                ;; the fallback for the time being.
                (t (- (* magic-number 0.1)))))))))
  "Mode line construct to align following elements to the right.
Read Info node `(elisp) Pixel Specification'.")

;;;; Miscellaneous

(defvar-local prot-modeline-misc-info
    '(:eval
      (when (mode-line-window-selected-p)
        mode-line-misc-info))
  "Mode line construct displaying `mode-line-misc-info'.
Specific to the current window's mode line.")

;;;; Risky local variables

;; NOTE 2023-04-28: The `risky-local-variable' is critical, as those
;; variables will not work without it.
(dolist (construct '(prot-modeline-kbd-macro
                     prot-modeline-narrow
                     prot-modeline-input-method
                     prot-modeline-buffer-status
                     prot-modeline-buffer-identification
                     prot-modeline-major-mode
                     prot-modeline-process
                     prot-modeline-vc-branch
                     prot-modeline-flymake
                     prot-modeline-align-right
                     prot-modeline-misc-info))
  (put construct 'risky-local-variable t))


(message "Loaded prot-modeline.el")

(provide 'prot-modeline)

;;; prot-modeline.el ends here
