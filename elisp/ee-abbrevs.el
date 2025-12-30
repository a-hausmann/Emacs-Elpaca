;; -*- lexical-binding: t -*-
;; File name:     ee-abbrevs.el
;; Created:       2023-10-06
;; Last modified: Tue Nov 25, 2025 16:48:36
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
    ("BAO" "Broken Arrow")
    ("BAOK" "Broken Arrow, OK")
    ("aehj" "aehjr1@gmail.com")
    ("FUR" "furosimide")
    ))

(set-default 'abbrev-mode t)
