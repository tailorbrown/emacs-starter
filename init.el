;;;;---------------------------------------------------------------------------
;; init.el emacs configuration file
;; author: Dylan W. Schwilk
;;
;; packages supported:
;;   font-lock, auctex, reftex, ess, org-mode
;;
;; Supports modes for: text, LaTeX and bibtex, C, C++, python, html
;;
;; this .emacs file loads several other customization files:
;;        - ~/.emacs.d/lisp/efunc.el  -   custom functions
;;        - ~/.emacs.d/lisp/mode.el   -   modes supported
;;        - ~/.emacs./lisp/ekeys     -   key bindings
;;        - ~/.emacs.d/lisp/theme.el  -   modeline and color theme
;;
;; And color themese are in ~/.emacs.d/themes
;;;;---------------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General setup and package managment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; turn on Common Lisp support
(require 'cl-lib)

;; Identification
(defun user-mail-address() "john.doe@ttu.edu")
(setq user-full-name "John Doe")

;; add the elisp directories under ~/emacs to my load path
(defvar home-dir (expand-file-name "~/"))
(defvar emacs-root (concat home-dir ".emacs.d/"))
(cl-labels ((add-path (p)
           (add-to-list 'load-path (concat emacs-root p))))
  (add-path "lisp")              ; my personal elisp code
  (add-path "contrib")           ; elisp code from other people
)
;; add path for emacs24 style themes
(add-to-list 'custom-theme-load-path (concat emacs-root "themes"))

;; ELPA Package Management
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
))

;; Package setup, taken from
;; https://github.com/zane/dotemacs/blob/master/zane-packages.el#L62
(setq required-packages
      '(auctex
        dynamic-fonts
        ess
        htmlize
        list-utils
        magit
        markdown-mode
        pandoc-mode
        ))

(package-initialize)

;; install missing packages
;; see http://technical-dresese.blogspot.com/2012/12/elpa-and-initialization.html
(let ((not-installed (remove-if 'package-installed-p required-packages)))
  (if not-installed
      (if (y-or-n-p (format "there are %d packages to be installed. install them? "
                            (length not-installed)))
          (progn (package-refresh-contents)
                 (dolist (package not-installed)
                   (package-install package))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Options ON/OFF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq inhibit-startup-message t)           ; Disable the startup splash screen
(setq-default visible-bell t)              ; no beeps, flash on errors
(menu-bar-mode 1)                          ; arg >= 1 enable the menu bar
(tool-bar-mode -1)
(show-paren-mode 1)                        ; Turn on parentheses matching
;; Turn off blinking cursor
;;(if (fboundp 'blink-cursor-mode) (blink-cursor-mode 0))
(setq zmacs-regions t)
(setq inhibit-ge t)
(setq-default indent-tabs-mode nil)        ; uses spaces rather than tabs
(setq default-tab-width 4);
(setq delete-key-deletes-forward t)
(setq mouse-yank-at-point t)
(line-number-mode t)
(column-number-mode t)
(setq-default fill-column 79)
(defalias 'yes-or-no-p 'y-or-n-p) ;; get rid of yes-or-no questions - y or n is enough

;; Windows-style cut-copy-paste
(cua-mode t)
(setq x-select-enable-clipboard t) ;; cut-paste
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;;window splitting: prefer vertical
(setq split-height-threshold 80)
(setq split-width-threshold 80)

;; Printing setup
(setq ps-n-up-printing 2)
(setq ps-print-header nil)

;; we speak utf-8 here
(set-language-environment "utf-8")
(prefer-coding-system 'utf-8)

(setq sentence-end-double-space nil)
(setq sentence-end "[.?!][]\"')]*\\($\\|\t\\| \\)[ \t\n]*")

;; disable advanced features? Bah.
(put 'downcase-region  'disabled nil)
(put 'set-goal-column  'disabled nil)
(put 'upcase-region    'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; display various non-editing buffers in their own frames
(setq special-display-buffer-names
      (nconc '("*Backtrace*" "*VC-log*" "*compilation*" "*grep*")
             special-display-buffer-names))
(add-to-list 'special-display-frame-alist '(tool-bar-lines . 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load all external files for keybindings, modes, color themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mapcar
 'load-library
 '( "modes"        ; various modes configurations
   "efuncs"        ; a bunch of utilities functions
   "ekeys"         ; my key bindings and some aliases
;;   "org-mode-setup"; Optional, requires customization
   "theme" ))      ; all the visual stuff goes there

;; Do customize stuff last to override anything reset
(setq custom-file (concat emacs-root "custom.el"))
(load custom-file)

;; Add final message so using C-h l I can see if .emacs failed
(message ".emacs loaded successfully!.")
