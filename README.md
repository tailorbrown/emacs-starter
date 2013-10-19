schwilk-emacs-starter
=====================

A starter .emacs.d directory for my students with support for ESS, auctex, org-mode

Installing
----------

### Via git
    - Make your .emacs.d directory if it does not exist.
    - clone into it:  
      git clone https://github.com/dschwilk/schwilk-emacs-starter.git ~/.emacs.d

### Via a zipfile
   - use the "Download zip" link at https://github.com/dschwilk/schwilk-emacs-starter
   - extract the zip file to your .emacs.d directory


Personalizing
-------------

Modify the placeholders for email address and name in init.el:


> ;; Identification
> (defun user-mail-address() "john.doe@ttu.edu")
> (setq user-full-name "John Doe")


Using
-----

### For coding in R and running R interactively

    R mode is automatical entered when you edit a file with the extension *.r
    or *.R. To start the R interpreter highlight a region or put your cursor on
    a line of code and hit [shift-enter]. Do the same to send more code to the
    interpreter. See emacs speaks statistics for more details:
    http://ess.r-project.org/

   Note that my version binds shift-enter rather than shift-control.

### Some departures from emacs defaults
    - shift-enter for ess
    - cua-mode: http://ess.r-project.org/
      Windows style cut-copy-paste (C-x, C-c, C-v) enabled.
    - Uses my dark theme (schwilk-theme) and a custom mode-line. Also uses
      dynamic-fonts to select the font. Currently it chooses monospaced fonts
      according to this priority list:

    > "Inconsolata" "Consolas" "Ubuntu Mono" "Source Code Pro" "Envy Code R" "Droid Sans
    >  Mono Pro" "Droid Sans Mono" "DejaVu Sans Mono"

   You can change this in lisp/theme.el. Install one of these fonts, if needed.
   I prefer Inconsolata: http://www.levien.com/type/myfonts/inconsolata.html
    