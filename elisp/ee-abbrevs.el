;; -*- lexical-binding: t -*-
;; File name:     ee-abbrevs.el
;; Created:       2023-10-06
;; Last modified: Sat Oct 07, 2023 14:21:03
;; Purpose:       Contains my abbreviations and configuration.
;;

(setq save-abbrevs nil)      ;; Do NOT save abbrevs when quiting emacs
(delight 'abbrev-mode)

(clear-abbrev-table global-abbrev-table)

(define-abbrev-table 'global-abbrev-table
  '(
    ;; Days
    ("Mon" "Monday")
    ("Tue" "Tuesday")
    ("Wed" "Wednesday")
    ("Thu" "Thursday")
    ("Fri" "Friday")
    ("Sat" "Saturday")
    ("Sun" "Sunday")

    ;; Months, don't need all, just the longer ones.
    ("Jan" "January")
    ("Feb" "February")
    ("Aug" "August")
    ("Sep" "September")
    ("Oct" "October")
    ("Nov" "November")
    ("Dec" "December")

    ;; programing
    ("subdir" "subdirectory" )
    ("-*-" "-*- mode:  -*-")

    ;; common words, phrases
    ("enc" "Encounter")
    ("dl" "Data Loader")
    ("ARV" "Applied Rules Viewer")
    ("Lo" "Loyola")
    ))

(set-default 'abbrev-mode t)
