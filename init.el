;;;;---------------------------------------------------------------------------
;; init.el emacs configuration file
;; author: Dylan W. Schwilk
;; version: 2.2
;; date: 2013-10-07
;;
;; packages supported:
;;   bs (buffer management), cc-mode, font-lock, func-menu, html-mode,
;;   speedbar, auctex, reftex, vc (version control)
;;
;; Supports modes for: text, RestructuredText, LaTeX and bibtex, C,
;; C++, python, html
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
;; General setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; turn on Common Lisp support
(require 'cl)

;; Identification
(defun user-mail-address() "dylan@schwilk.org")
(setq user-full-name "Dylan W. Schwilk")

;; EasyPG for gpg. built-in
(require 'epa-file)
(epa-file-enable)
;; stop EasyPG from asking for the recipient
(setq epa-file-encrypt-to "dylan@schwilk.org")

;; add the elisp directories under ~/emacs to my load path
(defvar home-dir (expand-file-name "~/"))
(defvar emacs-root (concat home-dir ".emacs.d/"))
(labels ((add-path (p)
           (add-to-list 'load-path (concat emacs-root p))))
  (add-path "lisp")              ; my personal elisp code
  (add-path "contrib")           ; elisp code from other people
)

;; ELPA
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
))

;; Package setup, taken from
;; https://github.com/zane/dotemacs/blob/master/zane-packages.el#L62
(setq schwilk-packages
      '(auctex
        dynamic-fonts
        ess
        org-plus-contrib
        rainbow-mode
        ))

(package-initialize)

;;; install missing packages 
;; see http://technical-dresese.blogspot.com/2012/12/elpa-and-initialization.html
(let ((not-installed (remove-if 'package-installed-p schwilk-packages)))
  (if not-installed
      (if (y-or-n-p (format "there are %d packages to be installed. install them? "
                            (length not-installed)))
          (progn (package-refresh-contents)
                 (dolist (package not-installed)
                   (package-install package))))))


;; add path for emacs24 style themes
(add-to-list 'custom-theme-load-path (concat emacs-root "themes"))

; add ESS from git for julia support change according to location
;;(add-to-list 'load-path "/opt/ESS/lisp")
;; not needed, I am using a symlink to opt/julia/contrib/julia-mode.el in ./contrib

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Options ON/OFF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq inhibit-startup-message t)                        ;; Disable the startup splash screen
(setq-default visible-bell t)                           ;; no beeps, flash on errors
(menu-bar-mode 1)                                        ;; arg >= 1 enable the menu bar.
(tool-bar-mode -1)
(show-paren-mode 1)                                     ;; Turn on parentheses matching
;;(if (fboundp 'blink-cursor-mode) (blink-cursor-mode 0)) ;; Turn off blinking cursor
(setq zmacs-regions t)
(setq inhibit-ge t)
(setq-default indent-tabs-mode nil)                    ;; uses spaces rather than tabs
(setq default-tab-width 4); 
(setq delete-key-deletes-forward t)
(delete-selection-mode t)                               ;; Typed text replaces a selection
(setq mouse-yank-at-point t)                          ;; commented out this does what I want:
(line-number-mode t)
(column-number-mode t)
(setq mark-diary-entries-in-calendar t)
(setq x-select-enable-clipboard t)                      ;; cut-paste
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
(setq-default fill-column 79)
(defalias 'yes-or-no-p 'y-or-n-p) ;; get rid of yes-or-no questions - y or n is enough


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


;; Setup save options (auto and backup) -- still buggy need new Replace func
(setq backup-directory-alist `((".*" . "~/.saves")))
(setq auto-save-file-name-transforms
          `((".*" , "~/.saves" t)))

;; delete backup files > week old
(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files "~/.saves" t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth (file-attributes file))))
                  week))
      (message "%s" file)
      (delete-file file))))

;;(setq auto-save-timeout 2000)
;;(setq make-backup-files t)
;;(setq make-backup-files nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load all external files for keybindings, modes, color themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mapcar
 'load-library
 '( "modes"        ; various modes configurations
   "org-mode-setup"; org-mode specific
   "efuncs"        ; a bunch of utilities functions
   "ekeys"         ; my key bindings and some aliases
   "theme" ))      ; all the visual stuff goes there


;; Do customize stuff last to override anything reset
(setq custom-file (concat emacs-root "custom.el"))
(load custom-file)

;; Start in insert mode.  Needed?
(put 'overwrite-mode 'disabled nil)

;; Add final message so using C-h l I can see if .emacs failed
(message ".emacs loaded successfully!.")
