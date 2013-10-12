;;; -*- Mode: Emacs-Lisp -*-
;;;;---------------------------------------------------------------------------
;; ekeys.el configuration file
;; author: Dylan Schwilk
;; version: 1.3
;; date: 2012-11-14
;;
;;; my global key bindings and aliases for emacs
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
   "C-x w"    what-line
   "M-g"      goto-line
   "s-n"   next-logical-line
   "s-p"   previous-logical-line
 ;;  "<f5>"     copy-region-as-kill ; Copy  also S-ins
 ;;  "<f6>"     kill-region         ; Cut also S-del
 ;;  "<f7>"     yank                ; yank also C-ins
  ;; prefer backward-kill-word over Backspace
;  "C-w"     backward-kill-word
;  "C-x C-k" kill-region
;  "C-c C-k" kill-region

  ;; for moving to `M-x compile' and `M-x grep' matches
  "C-c n"   next-error
  "C-c p"   previous-error

  ;; last call-kbd-macro-rebind to a single keystroke
  "C-c A"   auto-fill-mode

   ;; additional shortcuts buffers and windows
  "C-x B"   bury-buffer
  "C-x E"   apply-macro-to-region-lines
  "C-x I"   insert-buffer
  "C-c s"   swap-windows  ; defined in efuncs.el

  "C-c g"   goto-line
  "C-c G"   goto-char

  "C-c k"   delete-region
  "C-c c"   comment-region
  "C-c u"   uncomment-region
  "C-c w"   count-words-region
  "M-/"     hippie-expand
  "\C-cr"   replace-garbage-chars ;; replaces MS-windows \222, etc

  ;; org-mode
  "C-c l"   org-store-link
  "C-c a"   org-agenda
  "C-c b"   org-iswitchb
  "C-c c"   org-capture  

  ;; Programming and ess
  "<f8>"     compile
  ;"C-c C-r"  eval-region   ;; same as in ess ; should not be needed
  ;"C-c C-b"  eval-buffer   ;; same as in ess

  ;; blogging
;  "C-c b s" weblogger-start-entry

  ;; chop chop!
;  "M-n"     chop-move-down
;  "M-p"     chop-move-up

  ;; bindings for functions defined in lisp/ekeys.el
  "C-x C-c" dont-kill-emacs
  "C-c \\"  the-the

  ;; Browse url
;  [(shift button3)] browse-url-at-mouse
  "C-c b"   browse-url
))

;; kill ring menu
(when (require 'browse-kill-ring nil 'noerror)
  (browse-kill-ring-default-keybindings))

(global-set-key "\C-cy" '(lambda ()
   (interactive)
   (popup-menu 'yank-menu)))

;; quick function keys 
;; f1 - help
;; f2 - 2-column
;; f3 - define kbd macro

(global-set-key [f12]      'toggle-truncate-lines)

;; (eval-when-compile
;;   (require 'python-mode))
;; (define-key py-mode-map "\C-c\C-v" 'py-pychecker-run)
;; (define-key py-mode-map "\C-c\C-l" 'py-execute-import-or-reload)


;; add occur to isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

;; disable iconify
;; (when window-system
;;   (global-unset-key (kbd "C-z")))

;; disable sendmail
(global-unset-key (kbd "C-x m"))

;; aliases
(defalias 'qrr 'query-replace-regexp)
(defalias 'rr 'replace-regexp)

;; ?
org-completion-use-ido

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode specific bindings
;; Don't fill when we are in visual-line-mode
(add-hook 'text-mode-hook
  (defun cjm-fix-text-mode ()
    (define-key text-mode-map "\M-q" 'maybe-fill-paragraph)
    (remove-hook 'text-mode-hook 'cjm-fix-text-mode)))

;; ESS ctl-return to start
(add-hook 'ess-mode-hook
  (lambda()
    (local-set-key [(shift return)] 'my-ess-eval)))
