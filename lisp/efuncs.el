;;; -*- Mode: Emacs-Lisp -*-
;;;;---------------------------------------------------------------------------
;; efuncs.el configuration file
;; author: Dylan Schwilk
;; version: 2.2
;; date: 2012-11-14
;;
;;; my utility functions
;;;;---------------------------------------------------------------------------


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface customizations, menus, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Setup speedbar, an additional frame for viewing source files
(autoload 'speedbar-frame-mode "speedbar" "Popup a speedbar frame" t)
(autoload 'speedbar-get-focus "speedbar" "Jump to speedbar frame" t)
(autoload 'speedbar-toggle-etags "speedbar" "Add argument to etags command" t)
(setq speedbar-frame-plist '(minibuffer nil
                             border-width 0
                             internal-border-width 0
                             menu-bar-lines 0
                             modeline t
                             name "SpeedBar"
                             width 24
                             unsplittable t))
(condition-case err
    (progn (speedbar-toggle-etags "-C"))
  (error (message "Unable to load Speedbar package.")))
;; end setup speedbar

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check if in visual line mode before hard wrapping
(defun maybe-fill-paragraph (&optional justify region)
  "Fill paragraph at or after point (see `fill-paragraph').

Does nothing if `visual-line-mode' is on."
  (interactive (progn
    	 (barf-if-buffer-read-only)
    	 (list (if current-prefix-arg 'full) t)))
  (or visual-line-mode
      (fill-paragraph justify region)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set visual line mode wrap 
(defvar visual-wrap-column nil)

(defun set-visual-wrap-column (new-wrap-column &optional buffer)
  "Force visual line wrap at NEW-WRAP-COLUMN in BUFFER (defaults
to current buffer) by setting the right-hand margin on every
window that displays BUFFER.  A value of NIL or 0 for
NEW-WRAP-COLUMN disables this behavior."
  (interactive (list (read-number "New visual wrap column, 0 to disable: " (or visual-wrap-column fill-column 0))))
  (if (and (numberp new-wrap-column)
           (zerop new-wrap-column))
    (setq new-wrap-column nil))
  (with-current-buffer (or buffer (current-buffer))
    (visual-line-mode t)
    (set (make-local-variable 'visual-wrap-column) new-wrap-column)
    (add-hook 'window-configuration-change-hook 'update-visual-wrap-column nil t)
    (let ((windows (get-buffer-window-list)))
      (while windows
        (when (window-live-p (car windows))
          (with-selected-window (car windows)
            (update-visual-wrap-column)))
        (setq windows (cdr windows))))))

(defun update-visual-wrap-column ()
  (if (not visual-wrap-column)
    (set-window-margins nil nil)
    (let* ((current-margins (window-margins))
           (right-margin (or (cdr current-margins) 0))
           (current-width (window-width))
           (current-available (+ current-width right-margin)))
      (if (<= current-available visual-wrap-column)
        (set-window-margins nil (car current-margins))
        (set-window-margins nil (car current-margins)
                            (- current-available visual-wrap-column))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Do visual line movement
;; (defun next-visual-line (&optional arg try-vscroll)
;;  (interactive "^p\np")
;;  (let ((line-move-visual t))
;;    (with-no-warnings
;;      (next-line arg try-vscroll))))

;; (defun previous-visual-line (&optional arg try-vscroll)
;;  (interactive "^p\np")
;;  (let ((line-move-visual t))
;;    (with-no-warnings
;;      (previous-line arg try-vscroll))))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Don't kill emacs with C-x C-c
(defun dont-kill-emacs ()
  (interactive)
  (error (substitute-command-keys "To exit emacs: \\[kill-emacs]")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My general custom functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;----------------------------------------------------------------------------
;;; open gtd.org
(defun gtd ()
   (interactive)
   (find-file "~/org/gtd.org")
)


;;;----------------------------------------------------------------------------
;;; Replace windows garbage chars
(defun replace-garbage-chars ()
"Replace goofy MS and other garbage characters with latin1 equivalents."
(interactive)
(save-excursion                         ;save the current point
  (replace-string "—" "--" nil (point-min) (point-max)) ; multi-byte
  (replace-string "" "`" nil (point-min) (point-max))
  (replace-string "" "'" nil (point-min) (point-max))
  (replace-string "" "``" nil (point-min) (point-max))
  (replace-string "" "''" nil (point-min) (point-max))
  (replace-string "" "--" nil (point-min) (point-max))
))

;;;----------------------------------------------------------------------------
;;; Instert date
(defun insert-date-string ()
  "Insert a nicely formated date string."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))
;; C-c i calls insert-date-string
(global-set-key (kbd "C-c i") 'insert-date-string)
;; end insert-date-string



;;;----------------------------------------------------------------------------
;; Indent region, clean up
(defun fix-and-indent ()
  "Clean up the code"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;;;----------------------------------------------------------------------------
;; tidy-region
(global-set-key "\C-xt" 'tidy-region)
(setq shell-command-default-error-buffer "tidy-errors") ; define error buffer
(defun tidy-region ()
  "Run Tidy HTML parser on current region."
  (interactive)
  (let ((start (mark))
        (end (point))
        (command "/usr/bin/tidy -config ~/emacs/contrib/tidy-config-file -asxhtml"))
        (shell-command-on-region start end command t t
             shell-command-default-error-buffer)))


;;;----------------------------------------------------------------------------
;; Latex word counting
(defun latex-word-count ()
    (interactive)
    (let* ((this-file (buffer-file-name))
           (word-count
            (with-output-to-string
              (with-current-buffer standard-output
                (call-process "texcount" nil t nil "-brief" this-file)))))
      (string-match "\n$" word-count)
     (message (replace-match "" nil nil word-count))))

;;;----------------------------------------------------------------------------
;; word-count-region
;;; First version; has bugs!
(defun count-words-region (beginning end)
  "Print number of words in the region.
Words are defined as at least one word-constituent
character followed by at least one character that
is not a word-constituent.  The buffer's syntax
table determines which characters these are."
  (interactive "r")
  (message "Counting words in region ... ")

;;; 1. Set up appropriate conditions.
  (save-excursion
    (goto-char beginning)
    (let ((count 0))

;;; 2. Run the while loop.
      (while (< (point) end)
        (re-search-forward "\\w+\\W*")
        (setq count (1+ count)))

;;; 3. Send a message to the user.
      (cond ((zerop count)
             (message
              "The region does NOT have any words."))
            ((= 1 count)
             (message
              "The region has 1 word."))
            (t
             (message
              "The region has %d words." count))))))



;; Functions for filling and unfilling text.
;; For composing in emacs then pasting into a word processor,
;; this un-fills all the paragraphs (i.e. turns each paragraph
;; into one very long line) and removes any blank lines that
;; previously separated paragraphs.
;;
(defun wp-munge () "un-fill paragraphs and remove blank lines" (interactive)
  (let ((save-fill-column fill-column))
    (set-fill-column 1000000)
    (mark-whole-buffer)
    (fill-individual-paragraphs (point-min) (point-max))
    (delete-matching-lines "^$")
    (set-fill-column save-fill-column)
))

(defun wp-unmunge () "fill paragraphs and separate them with blank lines"
  (interactive)
    (mark-whole-buffer)
    (replace-regexp "$" "\n")
    ; (replace-regexp "\(^.*\)$" "groogle\n");"\1\n\n")
    (fill-individual-paragraphs (point-min) (point-max))
)

(defun fill () "fill paragraphs" (interactive)
    (mark-whole-buffer)
    (fill-individual-paragraphs (point-min) (point-max)))

(defun unfill () "un-fill paragraphs" (interactive)
  (let ((save-fill-column fill-column))
    (set-fill-column 1000000)
    (mark-whole-buffer)
    (fill-individual-paragraphs (point-min) (point-max))
    (set-fill-column save-fill-column)
))


(defun make-file-executable-if-script ()
  "Make file executable according to umask if not already executable.
If file already has any execute bits set at all, do not change existing
file modes."
  (and (save-excursion
         (save-restriction
           (widen)
           (goto-char (point-min))
           (save-match-data
             (looking-at "^#!"))))
       (let* ((current-mode (file-modes (buffer-file-name)))
              (add-mode (logand ?\111 (default-file-modes))))
         (or (/= (logand ?\111 current-mode) 0)
             (zerop add-mode)
             (set-file-modes (buffer-file-name)
                             (logior current-mode add-mode))))))


(defun byte-compile-emacs-config ()
  "Byte compile the current file, when saved, if the file is part of
my Emacs Lisp configuration."
  (let* ((current-file (buffer-file-name))
         (config-dir (concat emacs-root "lisp"))
         (string-length (length config-dir)))
    (if (and (eq (compare-strings config-dir 0 string-length
                         current-file 0 string-length) t)
             (string-match "\\.el\\'" current-file))
        (byte-compile-file current-file)
        nil)))


(defun swap-windows ()
 "If you have 2 windows, it swaps them." 
 (interactive)
 (cond ((not (= (count-windows) 2))
        (message "You need exactly 2 windows to do this."))
       (t
        (let* ((w1 (first (window-list)))
               (w2 (second (window-list)))
               (b1 (window-buffer w1))
               (b2 (window-buffer w2))
               (s1 (window-start w1))
               (s2 (window-start w2)))
          (set-window-buffer w1 b2)
          (set-window-buffer w2 b1)
          (set-window-start w1 s2)
          (set-window-start w2 s1)))))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
          (progn
            (rename-file name new-name 1)
            (rename-buffer new-name)
            (set-visited-file-name new-name)
            (set-buffer-modified-p nil))))))

(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
          (filename (buffer-file-name))
          (dir
             (if (string-match dir "\\(?:/\\|\\\\)$")
                       (substring dir 0 -1) dir))
          (newname (concat dir "/" name)))

    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn
        (copy-file filename newname 1)
        (delete-file filename)
        (set-visited-file-name newname)
        (set-buffer-modified-p nil)
        t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find duplicate words
(defun the-the ()
  "Search forward for a duplicated word."
  (interactive)
  (message "Searching for  duplicated words ...")
  (push-mark)
  ;; This regexp is not perfect
  ;; but is fairly good over all:
  (if (re-search-forward
       "\\b\\([^@ \n\t]+\\)[ \n\t]+\\1\\b" nil 'move)
      (message "Found duplicated word.")
      (message "End of buffer")))

(defun fix-amazon-url ()
  "Minimizes the Amazon URL under the point.  You can paste an Amazon
URL out of your browser, put the cursor in it somewhere, and invoke
this method to convert it."
  (interactive)
  (and (search-backward "http://www.amazon.com" (point-at-bol) t)
       (search-forward-regexp
		".+/\\([A-Z0-9]\\{10\\}\\)/[^[:space:]\"]+" (point-at-eol) t)
       (replace-match
		(concat "http://www.amazon.com/o/asin/"
				(match-string 1)
				(match-string 3)))))

(defun fix-google-search-url ()
  "Minimizes a Google search URL under the point."
  (interactive)
  (and (search-backward-regexp "http://www\\.google\\.[a-z]\\{2,3\\}/search" (point-at-bol) t)
       (search-forward-regexp
        ".+[&?]\\(q=[a-zA-Z0-9%+]+\\)\\(&.+\\)*" (point-at-eol) t)
       (replace-match
        (concat "http://www.google.com/search?"
                (match-string 1)))))

(defun compile-adjust-variable ()
  (unless (file-exists-p "Makefile")
    (set (make-local-variable 'compile-command)
         (let ((file (file-name-nondirectory buffer-file-name)))
           (concat "gcc -O2 -Wall -o " (file-name-sans-extension file)
                   " " file)))))

(defmacro define-hash-region (name hash-type)
  `(defun ,name (start end)
     (interactive "r")
     (save-excursion
       (let ((s (,hash-type (buffer-substring start end))))
         (delete-region start end)
         (insert s)))))

(define-hash-region sha1-region sha1)
(define-hash-region md5-region md5)

(defvar screenshot-file (concat home-dir "Screenshot.png")
  "Filename for screenshots.")

(defun screenshot-take ()
  "Take a screenshot of a single window."
  (interactive)
  (shell-command (concat "import -silent " screenshot-file)))

(defun screenshot-take-all ()
  "Take a screenshot of the whole screen."
  (interactive)
  (shell-command (concat "import -window root -silent " screenshot-file))) ;; uses ImageMagick import command

(defun screenshot-display ()
  "Display a previously taken screenshot."
  (interactive)
  (shell-command (concat "firefox " screenshot-file)))  ;; display in firefox

;; ;; (defun screenshot-upload ()
;; ;;   "Upload a screenshot to my web server."
;; ;;   (interactive)
;; ;;   (shell-command (concat "scp "
;; ;;                          screenshot-file
;; ;;                          " mkuze@schwilk.org:schwilk.org/files")))
