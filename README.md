schwilk-emacs-starter
=====================

A starter .emacs.d directory for my students with support for R programming (through ESS), LaTeX editing (auctex), org-mode (http://orgmode.org/) and git (using magit, https://github.com/magit/magit). Note that the org-mode setup is just the org-mode defaults.  I have a customized org-mode setup in org-mode-setup.el, but this file is not loaded unless you uncomment a line in init.el.  If you want to do that, you should read through [org-mode-setup.el](file:lisp/org-mode-setup.el) and read some comments and make a few personalizations where noted.

Installing
----------

### Via git
1. Make your .emacs.d directory if it does not exist
2. clone into it: git clone `https://github.com/dschwilk/schwilk-emacs-starter.git ~/.emacs.d`

### Via a zipfile
1. use the "Download zip" link at [https://github.com/dschwilk/schwilk-emacs-starter](https://github.com/dschwilk/schwilk-emacs-starter)
2. extract the zip file to your .emacs.d directory

Personalizing
-------------
Modify the placeholders for email address and name in init.el:

    ;; Identification
    (defun user-mail-address() "john.doe@ttu.edu")
    (setq user-full-name "John Doe")

Using
-----

### For coding in R and running R interactively

R mode is automatically entered when you edit a file with the extension *.r or *.R. To start the R interpreter highlight a region or put your cursor on
a line of code and hit [shift-enter]. Do the same to send more code to the interpreter. See emacs speaks statistics for more details: [http://ess.r-project.org/](http://ess.r-project.org/)

Note that my version binds shift-enter rather than shift-control to starting the R interpreter and sending a line to the interpreter.

### Some departures from emacs defaults

Things to note when you are reading documentation or surfing the web for help. This mode differs from the emacs defaults in these ways

* shift-enter for ess
* [cua-mode](http://ess.r-project.org/) Windows style cut-copy-paste (`C-x, C-c, C-v`) enabled
* Uses my dark theme (schwilk-theme) and a custom mode-line (that line of information at the bottom of the frame just above the minibuffer). My setup also uses dynamic-fonts to select the font. Currently it chooses monospaced fonts according to this priority list:

    "Inconsolata" "Consolas" "Ubuntu Mono" "Source Code Pro" "Envy Code R" "Droid Sans Mono Pro" "Droid Sans Mono" "DejaVu Sans Mono"
    
  You can change this in lisp/theme.el. Install one of these fonts, if needed. I prefer [Inconsolata](http://www.levien.com/type/myfonts/inconsolata.html)

### Additional functions

Some keybindings (shortcuts to a few functions in efuncs.el):

* "C-c r"   rename-file-and-buffer: Rename your current buffer and the file with which it is associated
* "C-c \\"  the-the: find duplicated words in your text (spellcheckers usually miss these)
* "C-c i"   insert-date-string: insert a quick date such as 2013-10-22

