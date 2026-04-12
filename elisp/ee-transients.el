;; ee-transients.el  -*- coding: utf-8; lexical-binding: t -*-
;; Created:       2026-02-22
;; Last modified: Sun Feb 22, 2026 14:03:02
;; Ref:           https://github.com/positron-solutions/transient-showcase
;;

;; Evaluate next sexp to open documentation in browser.
(browse-url-xdg-open "https://github.com/positron-solutions/transient-showcase")
;; How to do layout of transient groups.
(browse-url-xdg-open "https://github.com/positron-solutions/transient-showcase?tab=readme-ov-file#layouts")

;; 02/22/2026: creating transient menu for practice.
;; Nice to play with, but "text-scale-adjust" will do all things so don't need.
;; The ":transient" property when True will retain the menu after function execution.
;; When nil, the menu is exited.
;; (transient-define-prefix my/text-resizer-tmenu ()
;;   ["Resize Text"
;;   ("+" "Increase" text-scale-increase :transient t)
;;   ("-" "Decrease" text-scale-decrease :transient t)
;;   ("0" "Reset" text-scale-adjust :transient nil)
;;   ])

(transient-define-prefix my/transient-dwim ()
  )



;; ee-transients.el ends here
