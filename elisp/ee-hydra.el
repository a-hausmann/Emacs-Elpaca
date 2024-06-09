;; -*- lexical-binding: t -*-
;; File name:     ee-hydra.el
;; Created:       2023-07-30
;; Last modified: Wed May 29, 2024 19:55:01
;; Purpose:       Configure Hydra and create hydra menus.
;;

;; Configure Hydra
;; (use-package hydra
;;   :elpaca t
;;   :demand)
;; Allow Elpaca to process queues up to this point
;; (elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword

;; aeh/hydra-insert-date-menu
(defconst aeh/date-simple "%m/%d/%Y" "Simple format: MM/DD/YYYY")
(defconst aeh/date-format "%Y-%m-%d" "Simple date as YYYY-MM-DD")
(defconst aeh/date-file "%Y%m%d" "Simple format: YYYYMMDD")
(defconst aeh/date-time-format "%Y-%m-%d %-H:%M:%S" "Simple Date with Time: YYYY-MM-DD HH24:MI:SS")
(defconst aeh/day-format "%a %b %d, %Y" "English date as: Day Mon Date, Year")
(defconst aeh/day-time-format "%a %b %d, %Y %-H:%M:%S" "English Date Time as: Day Mon Date, Year HH24:MI:SS")
(defconst aeh/full-day-format "%A, %B %d, %Y" "English date as: Day, Month Date, Year")
(defconst aeh/full-day-date-format "%A, %Y-%m-%d" "English date as: Day, YYYY-MM-DD")
(defconst aeh/full-day-time-format "%A, %B %d, %Y %-H:%M:%S %p" "English Date Time as: Day, Month Date, Year HH:MI:SS PM")

(defhydra aeh/hydra-insert-date-menu (:color blue)
  "
_q_: quit
_s_: MM/DD/YYYY
_d_: YYYY-MM-DD
_f_: YYYYMMDD
_t_: YYYY-MM-DD HH24:MI:SS
_D_: DD Mon Date, Year
_T_: DD Mon Date, Year HH24:MI:SS
_A_: Day, YYYY-MM-DD
_e_: Day, Month Day, Year
_E_: Day, Month Day, Year HH:MI:SS PM
 "
  ("q" nil)
  ("s" (insert (format-time-string aeh/date-simple)))
  ("d" (insert (format-time-string aeh/date-format)))
  ("f" (insert (format-time-string aeh/date-file)))
  ("t" (insert (format-time-string aeh/date-time-format)))
  ("D" (insert (format-time-string aeh/day-format)))
  ("T" (insert (format-time-string aeh/day-time-format)))
  ("A" (insert (format-time-string aeh/full-day-date-format)))
  ("e" (insert (format-time-string aeh/full-day-format)))
  ("E" (insert (format-time-string aeh/full-day-time-format))))

(defhydra aeh/hydra-insert-stuff-menu (:color blue)
  "This hydra inserts so-called random stuff."
  ("f" (insert (file-name-nondirectory (buffer-file-name))) "base filename")
  ("F" (insert (expand-file-name (buffer-file-name))) "full filename")
  ("p" (insert (file-name-directory (buffer-file-name))) "path"))

(provide 'ee-hydra)

;; End of ee-hydra.el
