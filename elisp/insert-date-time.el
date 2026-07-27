;; -*- coding: utf-8; lexical-binding: t -*-
;; File name:     insert-date-time.el
;; Created:       2026-01-08
;; Last modified: Mon Jul 27, 2026 10:09:04
;; Purpose:       Package to insert the current date/time in various formats.

;; Got tons of helpful ideas from this Github: https://github.com/xenodium/time-zones

;; The idea behind this package is to have a set of pre-defined formats for date
;; and time which are selectable, and insert the current date/time into the current
;; buffer using the desired format. This replaces my initial brute-force means of
;; having a separate function for each format and using Hydra or which-key to select
;; the desired format.

;; I knew that I would need to use a list or alist as the "container" for the
;; various different formats, holding both the format string and the description
;; used to select the format. What I didn't know was HOW to select the format.
;; The "time-zones" package helped with that as it shows in function
;; "time-zones-select-timezone" that "completing-read" can be used to select
;; from the list of format descriptions, then "map-elt" is used to get the
;; format string from the list using the selected description from the completing-read.
;; There are still a lot of details to work out.

;; 01/31/2026: See file "~/Documents/Emacs/xah-insert-date.el" for interesting ideas.
;; See alternate transient manual:
;; Ref: https://github.com/Trevoke/alternate-transient-docs/blob/main/docs/transient-alternate.org

(require 'transient)


(defcustom insert-date-time-usa "%m/%d/%Y"
  "MM/DD/YYYY"
  :group 'insert-date-time
  :tag "USA"
  :type 'string)

(defcustom insert-date-time-iso "%Y-%m-%d"
  "YYYY-MM-DD"
  :group 'insert-date-time
  :tag "ISO"
  :type 'string)

(defcustom insert-date-time-file "%Y%m%d"
  "YYYYMMDD"
  :group 'insert-date-time
  :tag "ISO (file)"
  :type 'string)

(defcustom insert-date-time-julian "%j"
  "JJJ"
  :group 'insert-date-time
  :tag "Julian"
  :type 'string)

(defcustom insert-date-time-year-julian "%Y%j"
  "YYYYJJJ"
  :group 'insert-date-time
  :tag "Year + Julian"
  :type 'string)

(defcustom insert-date-time-iso-time
  "%Y-%m-%d %H:%M:%S"
  "YYYY-MM-DD HH24:MI:SS"
  :group 'insert-date-time
  :tag "YYYY-MM-DD HH24:MI:SS"
  :type 'string)

(defcustom insert-date-time-abb-string
  "%a %b %d, %Y"
  "Day Mon Date, Year"
  :group 'insert-date-time
  :tag "Day Mon Date, Year"
  :type 'string)

(defcustom insert-date-time-abb-string-time
  "%a %b %d, %Y %H:%M:%S"
  "Day Mon Date, Year HH24:MI:SS"
  :group 'insert-date-time
  :tag "Day Mon Date, Year HH24:MI:SS"
  :type 'string)

(defcustom insert-date-time-string
  "%A, %B %d, %Y"
  "Day, Month Date, Year"
  :group 'insert-date-time
  :tag "Day, Month Date, Year"
  :type 'string)

(defcustom insert-date-time-string-time
  "%A, %B %d, %Y %I:%M:%S %p"
  "Day, Month Date, Year HH:MI:SS PM"
  :group 'insert-date-time
  :tag "Day, Month Date, Year HH:MI:SS PM"
  :type 'string)

(defcustom insert-date-time-month-year
  "%B, %Y"
  "Month, Year"
  :group 'insert-date-time
  :tag "Month, Year"
  :type 'string)
 
(defcustom insert-date-time-time-pm
  "%I:%M %p"
  "HH:MI PM"
  :group 'insert-date-time
  :tag "HH:MI PM"
  :type 'string)

(defcustom insert-date-time-time-24
  "%H:%M"
  "HH24:MI"
  :group 'insert-date-time
  :tag "HH24:MI"
  :type 'string)

(defcustom insert-date-time-time-seconds-pm
  "%I:%M:%S %p"
  "HH:MI:SS PM"
  :group 'insert-date-time
  :tag "HH:MI:SS PM"
  :type 'string)

(defcustom insert-date-time-time-seconds-24
  "%H:%M:%S"
  "HH24:MI:SS"
  :group 'insert-date-time
  :tag "HH24:MI:SS"
  :type 'string)


;; Insert paired characters into buffer.
(defconst insert-paired-char-alist
  '(("' Single quote" . (39 39))             ; ' '
    ("\" Double quotes" . (34 34))           ; " "
    ("` Elisp quote" . (96 39))              ; ` '
    ("‘ Smart single quotes" . (8216 8217))  ; ‘ ’
    ("“ Smart double quotes" . (8220 8221))  ; “ ”
    ("( Parentheses" . (40 41))              ; ( )
    ("{ Curly brackets" . (123 125))         ; { }
    ("[ Square brackets" . (91 93))          ; [ ]
    ("< Angled brackets" . (60 62))          ; < >
    )
  "Alist of pairs for use with `insert-paired-characters'.")

(defun insert-paired-characters (&optional arg)
  "Insert pair from `insert-paired-char-alist'."
  (interactive "P")
  (let* ((data insert-paired-char-alist)
         (chars (mapcar #'car data))
         (choice (completing-read "Select pairs: " chars nil t))
         (left (cadr (assoc choice data)))
         (right (caddr (assoc choice data))))
    (insert-pair arg left right)))





;; Use transient.el to define base menu for selecting format to use.

(transient-define-prefix insert-date-time ()
  "Insert `now' as formatted string. Options are shown via a transient menu.
Because this needs to be used in any buffer, there are several keybindings.
The main binding is `C-; d' and alternates are `C-c M-i' and `s-c i'."
  ["Quit" ("q" "Quit menu" keyboard-quit)]
  ;; The date and date/time to be in columns, left and right respectively
  [["Insert date:" :pad-keys t  ; left side
  ("s" "MM/DD/YYYY"
       (lambda () (interactive)
         (insert (format-time-string insert-date-time-usa))))
  ("d" "YYYY-MM-DD"
       (lambda () (interactive)
         (insert (format-time-string insert-date-time-iso))))
  ("D" "YYYYMMDD"
       (lambda () (interactive)
         (insert (format-time-string insert-date-time-file))))
  ("j" "JJJ"
       (lambda () (interactive)
         (insert (format-time-string insert-date-time-julian))))
  ("J" "YYYYJJJ"
       (lambda () (interactive)
         (insert (format-time-string insert-date-time-year-julian))))
  ("a" "Dy Mon Date, Year"
       (lambda () (interactive)
         (insert (format-time-string insert-date-time-abb-string))))
  ("f" "Day Month Date, Year"
       (lambda () (interactive)
         (insert (format-time-string insert-date-time-string))))
  ("M" "Month, Year"
       (lambda () (interactive)
         (insert (format-time-string insert-date-time-month-year))))]
  ["Insert date & time" :pad-keys t  ; middle
  ("t" "YYYY-MM-DD HH24:MI:SS"
       (lambda () (interactive)
         (insert (format-time-string insert-date-time-iso-time))))
  ("A" "Dy Mon Date, Year HH24:MI:SS"
       (lambda () (interactive)
         (insert (format-time-string insert-date-time-abb-string-time))))
  ("F" "Day Month Date, Year HH24:MI:SS"
       (lambda () (interactive)
         (insert (format-time-string insert-date-time-string-time))))]
  ["Insert time only" :pad-keys t  ; right side
  ("T m" "HH:MI PM"
       (lambda () (interactive)
         (insert (format-time-string insert-date-time-time-pm))))
  ("T M" "HH24:MI"
       (lambda () (interactive)
         (insert (format-time-string insert-date-time-time-24))))
  ("T s" "HH:MI:SS PM"
       (lambda () (interactive)
         (insert (format-time-string insert-date-time-time-seconds-pm))))
  ("T S" "HH24:MI:SS"
       (lambda () (interactive)
         (insert (format-time-string insert-date-time-time-seconds-24))))]]
  ;; Other things will be in a group at the bottom.
  ;; TODO: add functions from "ee-hydras.el" to the below; may need to go right.
  [["Insert other things" :pad-keys t   ; left side
  ("%" "Full Name" (lambda () (interactive) (insert user-full-name)))
  ("@" "Email" (lambda () (interactive) (insert user-mail-address)))
  ("^" "Login Name" (lambda () (interactive) (insert user-login-name)))
  ("p" "Paired characters" insert-paired-characters)]
  ["Insert path/filename" :pad-keys t   ; right side
  ("N b" "Base filename"
       (lambda () (interactive)
         (insert (file-name-nondirectory (buffer-file-name)))))
  ("N f" "Full filename (and path)"
       (lambda () (interactive)
         (insert (expand-file-name (buffer-file-name)))))
  ("N p" "Path only"
       (lambda () (interactive)
         (insert (file-name-directory (buffer-file-name)))))
  ]]
  )




(provide 'insert-date-time)

;;; insert-date-time.el ends here
