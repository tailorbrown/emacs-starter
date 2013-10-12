;;; -*- Mode: Emacs-Lisp -*-
;;;;---------------------------------------------------------------------------
;; modes.el configuration file
;; author: Dylan Schwilk
;; version: 2.2
;; date: 2012-12-03
;;
;;;;---------------------------------------------------------------------------

;; setup SVN hooks
;; (require 'psvn)  ;; removed 2013-06-20
;; Start the svn interface with M-x svn-status

;; Version control: load VC hooks
(require 'vc-hooks)
(setq vc-colorized-diffs t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code for various document modes
;;;;;;;;;;;;;;;;;;;;;
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

;; Setup RST mode
;(require 'rst)
;   (add-hook 'text-mode-hook 'rst-text-mode-bindings)

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

;;;---------------------------------------------------------------------------
;; Setup HTMLhelper mode
(setq html-helper-do-write-file-hooks t)
(setq html-helper-address-string 
  "<a href=\"http://www.schwilk.org/\">Dylan Schwilk &lt;dylan@schwilk.org&gt;</a>")
(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
(setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))
;; End setup HTMLhelper-mode --------------------------------------------------

;; ----------------------------------------------------------------------------
;; Setup Common Lisp mode
(condition-case err
    (require 'cl)
  (error (message "Unable to load Common Lisp package.")))
;; End setup lisp-mode --------------------------------------------------------


;;;----------------------------------------------------------------------------
;; Setup C, C++ mode
(autoload 'c++-mode  "cc-mode" "C++ Editing Mode" t)
(autoload 'c-mode    "cc-mode" "C Editing Mode" t)
(autoload 'c-mode-common-hook "cc-mode" "C Mode Hooks" t)
(autoload 'c-add-style "cc-mode" "Add coding style" t)

;; Associate extensions with c modes
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

;; Create my own c/cpp coding style
;; No space before { and function sig indents 4 if argument overflow
(setq dws-c-style
      '((c-auto-newline                 . nil)
        (c-basic-offset                 . 4)
        (c-comment-only-line-offset     . 0)
        (c-echo-syntactic-information-p . nil)
        (c-hungry-delete-key            . t)
        (c-tab-always-indent            . t)
        (c-toggle-hungry-state          . t)
        (c-hanging-braces-alist         . ((substatement-open after)
                                          (brace-list-open)))
        (c-offsets-alist                . ((arglist-close . c-lineup-arglist)
                                           (case-label . 4)
                                           (substatement-open . 0)
                                           (block-open . 0) ; no space before {
                                           (knr-argdecl-intro . -)))
        (c-hanging-colons-alist         . ((member-init-intro before)
                                           (inher-intro)
                                           (case-label after)
                                           (label after)
                                           (access-label after)))
        (c-cleanup-list                 . (scope-operator
                                           empty-defun-braces
                                           defun-close-semi))))
        
;; Construct a hook to be called when entering C mode
(defun lconfig-c-mode ()
  (progn (define-key c-mode-base-map "\C-m" 'newline-and-indent)
         (define-key c-mode-base-map [f4] 'speedbar-get-focus)
         (define-key c-mode-base-map [f5] 'next-error)
         (define-key c-mode-base-map [f6] 'run-program)
         (define-key c-mode-base-map [f8] 'compile)
         (define-key c-mode-base-map [f9] 'insert-breakpoint)
         (define-key c-mode-base-map [f10] 'step-over)
         (define-key c-mode-base-map [f11] 'step-into)
         (c-add-style "Dylan's Coding Style" dws-c-style t)))
(add-hook 'c-mode-common-hook 'lconfig-c-mode)
;; End setup c mode -----------------------------------------------------------


;;;----------------------------------------------------------------------------
;; Setup python mode - 
;; not needed in GNU Emacs 22 and up. Replaced by
;; python.el which is loaded automatically
;; End setup python mode ------------------------------------------------------


;;;----------------------------------------------------------------------------
;; Setup flymake for pylint -- uses my custom epylint script in ~/scripts
    (when (load "flymake" t)
      (defun flymake-pylint-init ()
        (let* ((temp-file (flymake-init-create-temp-buffer-copy
                           'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
          (list "epylint" (list local-file))))
    
      (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))
;; End setup flymake ----------------------------------------------------------


;;;----------------------------------------------------------------------------
;; Setup julia mode for ESS
(require 'ess-site) 
(setq inferior-julia-program-name "/opt/julia/usr/bin/julia-release-basic")

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

;; Function to create window and start R
(defun my-ess-start-R ()
    (interactive)
    (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
      (progn
          (delete-other-windows)
          (setq w1 (selected-window))
          (setq w1name (buffer-name))
          (setq w2 (split-window w1 nil t))
          (R)
          (set-window-buffer w2 "*R*")
          (set-window-buffer w1 w1name))))

(defun my-ess-eval ()
    (interactive)
    (my-ess-start-R)
    (if (and transient-mark-mode mark-active)
      (call-interactively 'ess-eval-region)
      (call-interactively 'ess-eval-line-and-step)))

;; ;; Allow C-up anddown to scroll through history. alt-p also works
;; (add-hook 'inferior-ess-mode-hook
;;   #'(lambda()
;;      (local-set-key [C-up] 'comint-previous-input)
;;      (local-set-key [C-down] 'comint-next-input)))

(autoload 'R-mode "R mode" "mode for interacting with R" t)
 (setq auto-mode-alist
       (append '(("\\.[rR]$" . R-mode) 
                 ("\\.[rr]history" . R-mode)) auto-mode-alist))

;; End setup R-mode -----------------------------------------------------------

;; ----------------------------------------------------------------------------
;; Setup Latex mode

;;(load "auctex.el" nil t t)  ;; not needed when using auctex from ELPA
;; (require 'latex) ;; not needed when using auctex from ELPA

;(define-key LaTeX-mode-map "\C-cw" 'latex-word-count))
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;;(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
;;(add-hook 'LaTeX-mode-hook #'(lambda() (setq fill-column 79)))
(add-hook 'LaTeX-mode-hook 'turn-on-visual-line-mode)  ;; visual line wrapping
(add-hook 'LaTeX-mode-hook #'(lambda() (setq TeX-fold-mode 1)))    ;; turn on folding
;; note: can most of these just be made normal setq calls?
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable synctex correlation
;;(setq TeX-source-correlate-method 'synctex)
;; Enable synctex generation. Even though the command shows
;; as "latex" pdflatex is actually called
;;(custom-set-variables '(LaTeX-command "latex -synctex=1") )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use Okular as the pdf viewer. Build okular 
;; command, so that Okular jumps to the current line 
;; in the viewer.

;; this is not working as of 2013-07-25.  docview works ok, but C-c C-v complains about a missing dvi file
;; (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode) 

;; (defun pdf-with-okular ()
;;  (add-to-list 'TeX-output-view-style
;;  (quote ("^pdf$" "." "okular %o %(outpage)"))))

;; (add-hook 'LaTeX-mode-hook 'pdf-with-okular t)

;; (setq TeX-view-program-selection
;;  '((output-pdf "PDF Viewer")))
;; (setq TeX-view-program-list
;;      '(("Okular" "okular --unique %o#src:%n%(dir)./%b"))); %o: TeX-output-extension; %n: TeX-current-line; %b: TeX-current-file-name-master-relative


;; turn on reftex
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-cite-format 'natbib)
(setq reftex-enable-partial-scans t)
(setq reftex-save-parse-info t)
(setq reftex-use-multiple-selection-buffers t)
(setq reftex-plug-into-AUCTeX t)

;; LaTeX environments to recognize  ;; removed for now not working 
;;(add-hook 'LaTeX-mode-hook (add-to-list 'LaTeX-indent-environment-list '("tikzpicture")))
;;(add-to-list 'LaTeX-verbatim-environments "lstlisting")

;; ;; Ispell ignore \citep  
;; (eval-after-load "ispell"
;;    '(let ((list (car ispell-tex-skip-alists)))
;;       (add-to-list 'list '("\\\\cite[tp]" ispell-tex-arg-end))
;;       (setcar ispell-tex-skip-alists list)))

;; Add keywords
 

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


;; setup remote file access mode for tramp ------------------------------------
(setq tramp-default-method "ssh")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End document modes setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
