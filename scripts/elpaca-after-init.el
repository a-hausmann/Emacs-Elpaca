;; -*- lexical-binding: t -*-
;; File name:     elpaca-after-init.el
;; Created:       2023-10-04
;; Last modified: Thu Oct 05, 2023 13:15:59
;; Purpose:       Contains all the elpaca-after-init-hooks etc.
;;

;; This function displays how long Emacs took to start.
(add-hook 'elpaca-after-init-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract (current-time) before-init-time)))
                     gcs-done)))
;; Create a profiler report after init.
(profiler-start 'cpu+mem)
(add-hook 'elpaca-after-init-hook (lambda () (profiler-stop) (profiler-report)))


(provide 'elpaca-after-init)
