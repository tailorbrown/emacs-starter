;;; -*- Mode: Emacs-Lisp -*-
;;;;---------------------------------------------------------------------------
;; modes.el configuration file for student starter kit
;; author: Dylan Schwilk
;; version: 1.0
;; date: 2013-10-19
;;
;;;;---------------------------------------------------------------------------

;; Version control: load VC hooks
(require 'vc-hooks)
(setq vc-colorized-diffs t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code for various document modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Setup text mode
;;(add-hook 'text-mode-hook '(lambda() (auto-fill-mode 1)))
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'text-mode-hook '(lambda() (setq visual-wrap-column 79)))
(add-hook 'text-mode-hook '(lambda() (setq fill-column 79)))
(add-hook 'text-mode-hook 'flyspell-mode)

;; Setup table editing using table.el
(require 'table)
(add-hook 'text-mode-hook 'table-recognize)

;;;----------------------------------------------------------------------------
;; Setup R mode and and define the shift-enter function
(require 'ess-site)
(setq ess-ask-for-ess-directory nil)
(setq ess-local-process-name "R")
(setq ansi-color-for-comint-mode 'filter)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)
(setq ess-nuke-trailing-whitespace-p t)
(ess-toggle-underscore nil)
(setq ess-nuke-trailing-whitespace-p t)
;; turn off aligning single '#' to col 40!
(setq ess-fancy-comments nil)

(autoload 'R-mode "R mode" "mode for interacting with R" t)
 (setq auto-mode-alist
       (append '(("\\.[rR]$" . R-mode)
                 ("\\.[rR]history" . R-mode)) auto-mode-alist))

;; End setup R-mode -----------------------------------------------------------

;; ----------------------------------------------------------------------------
;; Setup Latex mode

;;(load "auctex.el" nil t t)  ;; not needed when using auctex from ELPA

;(define-key LaTeX-mode-map "\C-cw" 'latex-word-count))
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-visual-line-mode)  ;; visual line wrapping
(add-hook 'LaTeX-mode-hook #'(lambda() (setq TeX-fold-mode 1)))    ;; turn on folding
(add-hook 'LaTeX-mode-hook #'(lambda() (setq TeX-newline-function 'reindent-then-newline-and-indent) ))
(add-hook 'LaTeX-mode-hook #'(lambda() (setq LaTeX-item-indent 2)))
(add-hook 'LaTeX-mode-hook #'(lambda() (setq TeX-brace-indent-level 2)))
(add-hook 'LaTeX-mode-hook #'(lambda() (setq font-latex-match-textual-keywords (quote ("citet" "citep" "citeauthor" "citeyear")))))
(font-lock-add-keywords
 'latex-mode
 '(("\\\\clearpage" 0 font-lock-keyword-face prepend)
  ("\\\\label" 0 font-lock-keyword-face prepend)))

(add-hook 'LaTeX-mode-hook '(lambda() (setq ispell-check-comments nil)))
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
(setq TeX-show-compilation nil) ;; turn off compilation buffer
;; turn on reftex
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-cite-format 'natbib)
(setq reftex-enable-partial-scans t)
(setq reftex-save-parse-info t)
(setq reftex-use-multiple-selection-buffers t)
(setq reftex-plug-into-AUCTeX t)


;;;----------------------------------------------------------------------------
;; Setup Shell mode

;; Actually display colors when programs output colored text.  Without
;; this command, emacs prints the actual control characters.
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(add-hook 'shell-mode-hook
	  (function (lambda ()
		      (setq comint-scroll-to-bottom-on-input t)
		      (setq comint-scroll-to-bottom-on-output t)
		      (setq comint-scroll-show-maximum-output t)
		      (compilation-shell-minor-mode)
		      (rename-buffer "shell" t))))

;; End setup shell-mode -------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End document modes setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
