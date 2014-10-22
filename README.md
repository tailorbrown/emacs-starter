schwilk-emacs-starter
=====================

A starter .emacs.d directory for my students with support for R programming (through ESS), Markdown editing ([markdown-mode][markdown-mode] and [pandoc-mode][pandoc-mode]), LaTeX editing (auctex), [org-mode][org-mode] and git (using [magit][magit]). Note that the org-mode setup is just the org-mode defaults. I have a customized org-mode setup in org-mode-setup.el, but this file is not loaded unless you uncomment a line in init.el. If you want to do that, you should read through [org-mode-setup.el](file:lisp/org-mode-setup.el) and read some comments and make a few personalizations where noted.

Requirements
------------

This configuration requires Emacs 24. The init.el file will use the Emacs package manager to automatically install any missing required packages.

Installing
----------

### Via git
1. Make your ~/.emacs.d directory if it does not exist
2. clone into it: git clone `https://github.com/schwilklab/emacs-starter.git ~/.emacs.d`

### Via a zipfile
1. use the "Download zip" link at

https://github.com/schwilklab/emacs-starter

2. extract the zip file to your .emacs.d directory

Personalizing
-------------
Modify the placeholders for email address and name in init.el:

    ;; Identification
    (defun user-mail-address() "john.doe@ttu.edu")
    (setq user-full-name "John Doe")

Using
-----

The first time you start emacs with this new configuration, do so as a regular emacs session (not daemon). Emacs will ask you if it is ok to install some packages from MELPA (see list of required packages in init.el). Choose "y" so that emacs will use the package manager to install these packages upon which this configuration depends.

When emacs starts, it may show a gray background theme rather than the dark "schwilk" theme. To load my color theme, go to "options -> Customize Emacs -> Custom themes" and choose "schwilk".  Then choose "save" to allow this theme to load automatically from now on.

### For coding in R and running R interactively

R mode is automatically entered when you edit a file with the extension *.r or *.R. To start the R interpreter highlight a region or put your cursor on
a line of code and hit [shift-enter]. Do the same to send more code to the interpreter. See [emacs speaks statistics][ess] for more details.

### Using git from Emacs

[magit][magit] provides a great interface for git. To bring up the git interface, type "C-x g" when in a window visiting any buffer in a git repository.  See the magit documentation for more detail.

### Some departures from emacs defaults

Things to note when you are reading documentation or surfing the web for help. This mode differs from the emacs defaults in these ways

* shift-enter for ess
* [cua-mode][cua-mode] Windows style cut-copy-paste (`C-x, C-c, C-v`) enabled
* Uses my dark theme (schwilk-theme) and a custom mode-line (that line of information at the bottom of the frame just above the minibuffer, what is called a "status bar" in other applications). My setup also uses dynamic-fonts to select the font. Currently it chooses monospaced fonts according to this priority list:

    "Inconsolata" "Consolas" "Ubuntu Mono" "Source Code Pro" "Envy Code R" "Droid Sans Mono Pro" "Droid Sans Mono" "DejaVu Sans Mono"
    
Emacs will look for fonts in this order so make sure that at least one is installed. You can change this in lisp/theme.el. I prefer [Inconsolata][inconsolata].

* See lisp/ekeys.el for all keybindings.
### Additional functions

Some keybindings (shortcuts to a few functions in efuncs.el):

* "C-c r"   rename-file-and-buffer: Rename your current buffer and the file with which it is associated
* "C-c \\"  the-the: find duplicated words in your text (spellcheckers usually miss these)
* "C-c i"   insert-date-string: insert a quick date such as 2013-10-22


[cua-mode]:http://www.emacswiki.org/CuaMode
[ess]:http://ess.r-project.org/
[inconsolata]:http://www.levien.com/type/myfonts/inconsolata.html
[magit]:https://github.com/magit/magit
[markdown-mode]:http://jblevins.org/projects/markdown-mode/
[org-mode]:http://orgmode.org/
[pandoc-mode]:http://joostkremers.github.io/pandoc-mode/
