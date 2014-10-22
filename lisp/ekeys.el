;;; -*- Mode: Emacs-Lisp -*-
;;;;---------------------------------------------------------------------------
;; ekeys.el configuration file
;;
;;; Global key bindings and aliases for emacs
;;;;---------------------------------------------------------------------------

(defmacro bind-key (key function)
  `(progn
     (global-set-key (read-kbd-macro ,key) ,function)))

(defun bind-keys (bindings)
  (if (eq (cdr bindings) nil)
      t
      (progn
        (let ((key (car bindings))
              (function (cadr bindings)))
          (bind-key key function)
          (bind-keys (cddr bindings))))))

(bind-keys
'(
  ;; Function keys:
  ;; f1 - help
  ;; f2 - 2-column
  ;; f3 - define kbd macro
  "<f8>"    compile
  "<f11>" visual-line-mode ; toggles
  "<f12>" toggle-truncate-lines

  ;; for moving to `M-x compile' and `M-x grep' matches
  "C-c n"   next-error
  "C-c p"   previous-error

   ;; additional shortcuts buffers and windows
  "C-<tab>"   bury-buffer ; cycle through buffers
  "C-x I"   insert-buffer
  "C-c w"   count-words-region
  "M-/"     hippie-expand

  ;; org-mode
  "C-c l"   org-store-link
  "C-c a"   org-agenda
  "C-c b"   org-iswitchb
  "C-c c"   org-capture  

  ;; Start shell or switch to it if it's active.
  "C-x m"   shell
  ;; git, magit, vc
  "C-x g"   magit-status

  ;; bindings for functions defined in lisp/efuncs.el
   "C-c r"   rename-file-and-buffer
   "C-c \\"  the-the
   "C-c i"   insert-date-string
))

;; Don't use alt-q to fill when we are in visual-line-mode make temporary
;; function, run hook function in text mode and after it is run once remove the
;; function
(add-hook 'text-mode-hook
  (defun dws-fix-text-mode ()
    (define-key text-mode-map "\M-q" 'maybe-fill-paragraph)
    ; remove hook not really necessary, but why not clean up after it runs?
    (remove-hook 'text-mode-hook 'dws-fix-text-mode)))

;; Define eval line in ESS as shift-enter rather than C-enter. because C-enter
;; conflicts with the rectangle selection tool in CUA-mode.
(define-key ess-mode-map [(control return)] nil)
(define-key ess-mode-map [(shift return)] 'ess-eval-region-or-line-and-step)
