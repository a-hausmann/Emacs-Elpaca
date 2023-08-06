;; -*- lexical-binding: t -*-
;; File name:     ee-hydra.el
;; Created:       2023-07-30
;; Last modified: Sun Jul 30, 2023 16:02:49
;; Purpose:       Configure Hydra and create hydra menus.
;;

;; Configure Hydra
(use-package hydra
  :elpaca t)
;; Allow Elpaca to process queues up to this point
(elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword

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

;; This sets more "natural" keys "+", and "-" to zoom in/out, and "0" to reset
(defhydra aeh/hydra-zoom (:hint nil)
  "zoom"
  ("+" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ("0" (text-scale-set 0) "reset" :color blue)
  ("q" nil "quit"))

;; Changing General setup to "newzoom."
(defhydra aeh/hydra-newzoom (:exit nil)
  ("+" text-scale-increase "Zoom in")
  ("-" text-scale-decrease "Zoom out")
  ("0" (text-scale-set 0) "Reset" :color blue)
  ("q" nil "Quit"))

;; Rockin' the buffers menu!
(defhydra aeh/hydra-buffers (:hint nil :exit t)
  "
   buffers:   _b_ → buffers              _i_ → ibuffer                 _k_ → kill buffer
              _p_ → prev buffer          _e_ → erase buffer            _E_ → erase buffer (force)
              _n_ → new buffer           _A_ → save buffer AS file     _r_ → rename buffer
              _R_ → rename uniquely      _g_ → revert buffer           _G_ → GOTO buffer
              _M_ → delete ^M            _C_ → cleanup buffer
"
  ;; ("b" #'counsel-ibuffer)
  ("b" #'consult-buffer)
  ("i" #'ibuffer)
  ("k" #'kill-this-buffer)
  ("p" #'aeh/switch-to-previous-buffer)
  ("e" #'erase-buffer)
  ("E" (let ((inhibit-read-only t)) (erase-buffer)))
  ("n" (switch-to-buffer (generate-new-buffer "untitled")))
  ("A" #'write-file)
  ("r" #'rename-buffer)
  ("R" #'rename-uniquely)
  ("g" #'revert-buffer)
  ("G" #'aeh/hydra-buffer-goto/body)
  ("M" #'aeh/delete-carrage-returns)
  ("C" #'ms/cleanup-buffer))

;; Hydra for files
(defhydra aeh/hydra-files (:hint nil :exit t)
  "
  files:
  _f_ → find files      _D_ → delete    _y_ → copy filename   _E_ → edit as root
  _r_ → recent files    _R_ → rename    _c_ → copy file       _C_ → convert        _b_ → bookmarks
  "
  ;; _z_ → fzf
  ("D" aeh/delete-current-buffer-file)	;; OK
  ("R" aeh/rename-current-buffer-file)	;; OK
  ;; ("f" #'counsel-find-file)		;; OK
  ("f" #'find-file)		;; OK
  ;; ("r" #'counsel-recentf)		;; OK
  ("r" #'consult-recent-file)		;; OK
  ("y" aeh/copy-file-name-to-clipboard)	;; OK
  ("E" aeh/edit-file-as-root)		;; OK
  ("c" copy-file)			;; OK
  ("C" aeh/hydra-files-convert/body)	;; OK
  ("b" aeh/hydra-bookmarks/body)        ;; OK
  ;; ("z" #'counsel-fzf))                 ;; OK
)

;; Hydra for toggles
(defvar aeh/hydras/toggles/vdiff nil)
(defhydra aeh/hydra-toggles (:hint nil :exit t)
  "
   toggle:  _z_ → origami-mode        _c_ → column-enforce
            _a_ → aggressive indent   _s_ → flycheck       _r_ → read only      _t_ → truncate lines   _e_ → debug on error
            _f_ → auto-fill           _S_ → flyspell       _C_ → completion     _W_ → word wrap        _g_ → debug on quit
            _w_ → whitespace          _E_ → electric-pairs _l_ → linum-relative _b_ → page break       _d_ → ediff/vdiff
            _h_ → highlight-thing     _D_ → drag-stuff
"
  ("z" origami-mode)
  ("c" column-enforce-mode)
  ("a" aggressive-indent-mode)
  ("b" page-break-lines-mode)
  ("C" company-mode)
  ("t" toggle-truncate-lines)
  ("e" toggle-debug-on-error)
  ("E" electric-pair-mode)
  ("h" highlight-thing-mode)
  ("l" display-line-numbers-mode)       ; display type set to "visual" in "Better-defaults"
  ("g" toggle-debug-on-quit)
  ("s" flycheck-mode)
  ("S" flyspell-mode)
  ("w" whitespace-mode)
  ("W" toggle-word-wrap)
  ("r" read-only-mode)
  ("f" auto-fill-mode)
  ("D" drag-stuff-mode)
  ;; Needs work on the "/pairs/toggle" code, copied from Bailey Ling's code base.
  ;; This switches between smartparens and electric-pairs; at this point, I'm not sure what is installed.
  ;; ("p" /pairs/toggle)
  ("d" (progn
         (if aeh/hydras/toggles/vdiff
	         (progn
	           (/bindings/vdiff/turn-off)
	           (message "using ediff"))
           (/vcs/setup-vdiff)
           (/bindings/vdiff/turn-on)
           (message "using vdiff"))
         (setq aeh/hydras/toggles/vdiff (not aeh/hydras/toggles/vdiff)))))

;; 2018-11-19: create yasnippet hydra, set blue overall to quit upon command
(defhydra aeh/hydra-yasnippet (:color blue :hint nil)
  "
Snippets^
---------------------------------------------------------
_q_ uit
_i_ insert snippet
_n_ new snippet
_l_ load directory
_r_ reload all
_v_ visit snippet
_d_ describe table
"
  ("q" nil :exit t)
  ("i" yas-insert-snippet)
  ("n" yas-new-snippet)
  ("l" yas-load-directory)
  ("r" yas-reload-all)
  ("v" yas-visit-snippet-file)
  ("d" yas-describe-tables))


;; Add hydra for Consult commands. copy from aeh/hydra-counsel.
(defhydra aeh/hydra-consult (:color blue :hint nil)
  "
Consult^
---------------------------------------------------------
_q_ uit
_a_ apropos
_b_ bookmarks
_i_ imenu
_I_ project imenu
_m_ mark ring
_M_ global mark ring
_r_ recent files
_o_ outline headings
_O_ multi occur
_T_ load theme
_c_ mode command
_C_ minor mode menu
"
  ("q" nil :exit t)
  ("a" consult-apropos)
  ("b" consult-bookmark)
  ("i" consult-imenu)
  ("I" consult-project-imenu)
  ("m" consult-mark)
  ("M" consult-global-mark)
  ("r" consult-recent-file)
  ("o" consult-outline)
  ("O" consult-multi-occur)
  ("T" consult-theme)
  ("c" consult-mode-command)
  ("C" consult-minor-mode-menu)
  )

;; 2019-06-19: Add "Window" commands (check for frame commands)
;; Ref: https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org#hydra
(defhydra aeh/hydra-windows (:color pink)
  "
^
^Windows^           ^Window^            ^Zoom^
^-------^-----------^------^------------^----^--------------
_q_ quit            _b_ balance         _-_ out
^^                  _i_ heighten        _+_ in
^^                  _j_ narrow          _=_ reset
^^                  _k_ lower           ^^
^^                  _l_ widen           ^^
^^                  ^^                  ^^
"
  ("q" nil)
  ("b" balance-windows)
  ("i" enlarge-window)
  ("j" shrink-window-horizontally)
  ("k" shrink-window)
  ("l" enlarge-window-horizontally)
  ("-" text-scale-decrease)
  ("+" text-scale-increase)
  ("=" (text-scale-increase 0)))

;; Add mode menu
(defhydra aeh/hydra-modes (:exit t)
  "Modes"
  ("o" #'orgtbl-mode "Org Table mode")
  ("p" #'prog-mode "Prog mode")
  ("s" #'sql-mode "SQL mode")
  ("t" #'text-mode "Text mode"))



(provide 'ee-hydra)

;; End of ee-hydra.el
