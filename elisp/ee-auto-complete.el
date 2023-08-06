;; -*- lexical-binding: t -*-
;; File name:     ee-auto-complete.el
;; Created:       2023-07-22
;; Last modified: Sat Jul 22, 2023 15:37:06
;; Purpose:       Configure Company and companion packages.
;;

;; --------------------------------------------------------------------------------
;; I think that Company is acting a bit better now, and on 10/30/2018, I added
;; some code from Oleh Krehel https://oremacs.com/2017/12/27/company-numbers/
;; to show numbers on the popup, and be able to use them to select text.
;; This works like a charm. Oleh's blog article is from December 2017, and he
;; states his git log shows he's been using this setup for three years without
;; any issues.  Grand!

;; 2019-09-05: After setting Counsel to defer, needed to wrap this entire code
;; with an "with-eval-after-load" function.
;; --------------------------------------------------------------------------------
;; Basic setting
(with-eval-after-load 'company
  (setq company-show-numbers t)
  ;; Oleh's function:
  (defun ora-company-number ()
    "Forward to `company-complete-number'.

  Unless the number is potentially part of the candidate.
  In that case, insert the number."
    (interactive)
    (let* ((k (this-command-keys))
           (re (concat "^" company-prefix k)))
      (if (cl-find-if (lambda (s) (string-match re s))
                      company-candidates)
          (self-insert-command 1)
          (company-complete-number (string-to-number k)))))

  ;; Add some bindings
  (let ((map company-active-map))
    (mapc
     (lambda (x)
       (define-key map (format "%d" x) 'ora-company-number))
     (number-sequence 0 9))
    (define-key map " " (lambda ()
                          (interactive)
                          (company-abort)
                          (self-insert-command 1)))
    ;; This line UNBINDS RET key from closing the popup
    (define-key map (kbd "<return>") nil)))

;; --------------------------------------------------------------------------------
;; Start Company configuration.
;; --------------------------------------------------------------------------------
(use-package company
    :elpaca t
    :commands company-complete-common
    :delight
    :hook ((emacs-lisp-mode . company-mode)
           (shell-mode . company-mode))
    :config
    (setq company-idle-delay .5)  ; half-second delay
    (global-set-key (kbd "C-M-.") 'company-complete-common)
    (setq company-minimum-prefix-length 3)   ; three letters needed for completion
    (setq company-dabbrev-ignore-case t)
    (setq company-dabbrev-downcase nil)      ; return candidates AS IS.
    (with-eval-after-load 'company
      (define-key company-active-map (kbd "M-n") nil)
      (define-key company-active-map (kbd "M-p") nil)
      (define-key company-active-map (kbd "C-n") #'company-select-next)
      (define-key company-active-map (kbd "C-p") #'company-select-previous)))
;; Allow Elpaca to process queues up to this point
(elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword

(add-hook 'sql-mode-hook
          #'(lambda ()
              (setq-default company-minimum-prefix-length 4)
              (setq-default company-dabbrev-code-ignore-case t)
              (setq-default completion-ignore-case t)))

(defun shell-mode-company-init ()
  (setq-local company-backends '((company-shell
                                  company-shell-env
                                  company-etags
                                  company-dabbrev-code))))

(use-package company-shell
  :elpaca t
  :after company
  :delight
  :config (add-hook 'shell-mode-hook 'shell-mode-company-init))
;; Allow Elpaca to process queues up to this point
(elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


(use-package company-statistics
  :elpaca t
  :after company
  :delight
  :config (company-statistics-mode))
;; Allow Elpaca to process queues up to this point
(elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


(use-package company-web
  :elpaca t
  :after company
  :delight)
;; Allow Elpaca to process queues up to this point
(elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword

;; Not sure what I was thinking here and no notes on it.
;; (use-package company-try-hard
;;     :ensure t
;;     :after company
;;     :bind
;;     ;; Change from C-<tab> to "C-." and "C-M-."
;;     (("C-." . company-try-hard)
;;      ("C-M-." . company-try-hard)
;;      :map company-active-map
;;      ("C-." . company-try-hard)
;;      ("C-M-." . company-try-hard)))

(use-package company-quickhelp
  :elpaca t
  :after company
  :delight
  :config
  (company-quickhelp-mode))
;; Allow Elpaca to process queues up to this point
(elpaca-wait)  ;; ALWAYS run elpaca-wait AFTER installing a package using a use-package keyword


(setq company-backends
      '((company-files          ; files & directory
         company-keywords       ; keywords
         company-capf)          ; completion-at-point-functions
        (company-abbrev company-dabbrev)))


;; End of aeh-auto-complete.el
