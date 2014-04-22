;;; -*- Mode: Emacs-Lisp -*-
;;;;---------------------------------------------------------------------------
;; modes.el configuration file
;; author: Dylan Schwilk
;;
;; Provides hooks and customizations for various modes (text, LaTeX and bibtex,
;; html, C, C++, python, ess (R and julia).
;;
;; All org-mode customizations are in ~/.emacs.d/lisp/org-mode-setup.el
;;;;---------------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Version control: load VC hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'vc-hooks)
(setq vc-colorized-diffs t)

;; magit installed via MELPA
;; nothing needed. See http://magit.github.io/magit/magit.html

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code for various document modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Setup text mode
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'text-mode-hook '(lambda() (setq visual-wrap-column 79)))
;; and for when we turn off visual-line-mode:
(add-hook 'text-mode-hook '(lambda() (setq fill-column 79)))
(add-hook 'text-mode-hook 'flyspell-mode)

;; Setup table editing using table.el
(require 'table)
(add-hook 'text-mode-hook 'table-recognize)

;; Markdown mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;;;---------------------------------------------------------------------------
;;; Setup psgml-mode ;;  
;; ---------------------------------------------------------------------------
;; XML/SGML - PSGML
;;
;;   First, we'll just set up PSGML. Then we'll create a derived mode for
;;   (X)HTML using mmm-mode for fancy (X)HTML/CSS/Javascript/PHP coding

(autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t )
(autoload 'xml-mode "psgml" "Major mode to edit XML files." t)

(setq auto-mode-alist
  (append
  (list
    '("\\.sgm$" . sgml-mode)
    '("\\.sgml$" . sgml-mode)
    '("\\.xml$" . xml-mode)
    )
  auto-mode-alist))
;; Enable editing help with mouse-3 in all sgml files

(defun go-bind-markup-menu-to-mouse3 ()
        (define-key sgml-mode-map [(down-mouse-3)] 'sgml-tags-menu))
(add-hook 'sgml-mode-hook 'go-bind-markup-menu-to-mouse3)

(setq sgml-indent-step 2)
(setq sgml-indent-data t)
(setq sgml-warn-about-undefined-entities nil)
(setq sgml-warn-about-undefined-elements nil)
(add-to-list 'auto-mode-alist '("\\.xsd$"    . xml-mode))
;; End setup psgml-mode ------------------------------------------------------

;; Use tidy.el to provide support for tidy
(autoload 'tidy-buffer "tidy" "Run Tidy HTML parser on current buffer" t)
(autoload 'tidy-parse-config-file "tidy" "Parse the `tidy-config-file'" t)
(autoload 'tidy-save-settings "tidy" "Save settings to `tidy-config-file'" t)
(autoload 'tidy-build-menu  "tidy" "Install an options menu for HTML Tidy." t)
(add-hook 'sgml-html-mode-hook #'(lambda () (tidy-build-menu sgml-html-mode-map)))
(add-hook 'xml-html-mode-hook #'(lambda () (tidy-build-menu xml-html-mode-map)))

;; ----------------------------------------------------------------------------
;; Setup Common Lisp mode
(condition-case err
    (require 'cl)
  (error (message "Unable to load Common Lisp package.")))
;; End setup lisp-mode --------------------------------------------------------

;;;----------------------------------------------------------------------------
;; python mode

;; nothing to do should work

;; End setup python-mode-------------------------------------------------------

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

;; (load "auctex.el" nil t t)  ;; not needed when using auctex from ELPA
;; (require 'latex) ;; not needed when using auctex from ELPA

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
