;; -*- coding: utf-8; lexical-binding: t -*-
;; File name:     insert-date-time.el
;; Created:       2026-01-08
;; Last modified: Thu Apr 09, 2026 23:17:30
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
  "%Y-%m-%d %-H:%M:%S"
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
  "%a %b %d, %Y %-H:%M:%S"
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
  "%A, %B %d, %Y %-H:%M:%S %p"
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
 
;; Use transient.el to define base menu for selecting format to use.

(transient-define-prefix insert-date-time ()
  "Insert `now' as formatted string. Options are shown via a transient menu.
Because this needs to be used in any buffer, there are several keybindings.
The main binding is `C-; d' and alternates are `C-c M-i' and `s-c i'."
  ["Insert date:"
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
  ("t" "YYYY-MM-DD HH24:MI:SS"
       (lambda () (interactive)
         (insert (format-time-string insert-date-time-iso-time))))
  ("a" "Day Mon Date, Year"
       (lambda () (interactive)
         (insert (format-time-string insert-date-time-abb-string))))
  ("A" "Day Mon Date, Year HH24:MI:SS"
       (lambda () (interactive)
         (insert (format-time-string insert-date-time-abb-string-time))))
  ("f" "Day Month Date, Year"
       (lambda () (interactive)
         (insert (format-time-string insert-date-time-string))))
  ("F" "Day Month Date, Year HH24:MI:SS"
       (lambda () (interactive)
         (insert (format-time-string insert-date-time-string-time))))
  ("M" "Month, Year"
       (lambda () (interactive)
         (insert (format-time-string insert-date-time-month-year))))])




(provide 'insert-date-time)

;;; insert-date-time.el ends here
