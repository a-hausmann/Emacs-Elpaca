;; -*- lexical-binding: t -*-
;; File name:     ee-abbrevs.el
;; Created:       2023-10-06
;; Last modified: Thu Jul 16, 2026 11:38:55
;; Purpose:       Contains my abbreviations and configuration.
;;

(setq save-abbrevs nil)      ;; Do NOT save abbrevs when quiting emacs
;; (delight 'abbrev-mode)    ;; WHY does this fail?

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
    ("mshit" "Microsoft")

    ;; common words, phrases
    ("BAO" "Broken Arrow")
    ("BAOK" "Broken Arrow, OK")
    ("aehj" "aehjr1@gmail.com")
    ("FUR" "furosimide")
    ("SFI" "State Farm")
    ("FR" "Flood Rescue")
    ("ACS" "Air Comfort Solutions")
    ))

(set-default 'abbrev-mode t)
