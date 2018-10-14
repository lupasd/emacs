;;; bs.el --- menu for selecting and displaying buffers

;; Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003 Free Software Foundation, Inc.
;; Author: Olaf Sylvester <olaf@geekware.de>
;; Maintainer: Olaf Sylvester <olaf@geekware.de>
;; Keywords: convenience

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Version: 1.17
;; X-URL: http://www.geekware.de/software/emacs
;;
;; The bs-package contains a main function bs-show for poping up a
;; buffer in a way similar to `list-buffers' and `electric-buffer-list':
;; The new buffer offers a Buffer Selection Menu for manipulating
;; the buffer list and buffers.
;; 
;; -----------------------------------------------------------------------
;; | MR Buffer          Size  Mode          File                         |
;; | -- ------          ----  ----          ----                         |
;; |.   bs.el           14690  Emacs-Lisp    /home/sun/sylvester/el/bs.e$|
;; |  % executable.el    9429  Emacs-Lisp    /usr/share/emacs/19.34/lisp$|
;; |  % vc.el          104893  Emacs-Lisp    /usr/share/emacs/19.34/lisp$|
;; |  % test_vc.el        486  Emacs-Lisp    /home/sun/sylvester/el/test$|
;; |  % vc-hooks.el     43605  Emacs-Lisp    /usr/share/emacs/19.34/lisp$|
;; -----------------------------------------------------------------------

;;; Quick Installation und Customization:

;; Use
;;   M-x bs-show
;; for buffer selection or optional bind a key to main function `bs-show'
;;   (global-set-key "\C-x\C-b" 'bs-show)    ;; or another key
;;
;; For customization use
;; M-x bs-customize


;;; More Commentary:

;; bs-show will generate a new buffer named *buffer-selection*, which shows
;; all buffers or a subset of them, and has possibilities for deleting,
;; saving and selecting buffers. For more details see docstring of
;; function `bs-mode'. A current configuration describes which buffers appear
;; in *buffer-selection*. See docstring of variable `bs-configurations' for
;; more details.
;; 
;; The package bs combines the advantages of the Emacs functions
;; `list-buffers' and `electric-buffer-list'.
;; 
;; Additioal features for Buffer Selection Menu:
;;  - configurable list of buffers (show only files etc.).
;;  - comfortable way to change displayed subset of all buffers.
;;  - show sorted list of buffers.
;;  - cyclic navigation:
;;     - goes to top of buffer list if you are on last line and press down.
;;     - goes to end of buffer list if you are on first line and press up.
;;  - Offer an alternative buffer list by prefix key C-u.

;;; Cycling through buffers

;; This package offers two functions for buffer cycling. If you want to cycle
;; through buffer list you can use `bs-cycle-next' or `bs-cycle-previous'.
;; Bind these function to a key like
;;   (global-set-key [(f9)]   'bs-cycle-previous)
;;   (global-set-key [(f10)]  'bs-cycle-next)
;; 
;; Both functions use a special subset of all buffers for cycling to avoid
;; to go through internal buffers like *Messages*.
;; 
;; Cycling through buffers ignores sorting because sorting destroys
;; the logical buffer list. If buffer list is sorted by size you
;; won't be able to cycle to the smallest buffer.
 
;;; Customization:

;; There is a customization group called `bs' in group `convenience'.
;; Start customization by M-x bs-customize
;;  
;; Buffer list
;; -----------
;; You can define your own configurations by extending variable
;; `bs-configurations' (see docstring for details).
;; 
;; `bs-default-configuration' contains the name of default configuration.
;; The default value is "files" which means to show only files.
;;  
;; If you always want to see all buffers, customize variable
;; `bs-default-configuration' in customization group `bs'.
;; 
;; Configure sorting
;; -----------------
;; You can define functions for sorting the buffer list.
;; When selecting buffers, you can step through available sorting
;; methods with key 'S'.
;; To define a new way of sorting, customize variable `bs-sort-functions'.
;; 
;; There are four basic functions for sorting:
;;   by buffer name, by mode, by size, or by filename
;; 
;; Configure buffer cycling
;; ------------------------
;; When cycling through buffer list the functions for cycling will use
;; the current configuration of bs to calculate the buffer list.
;; If you want to use a different configuration for cycling you have to set
;; the variable `bs-cycle-configuration-name'. You can customize this variable.
;;
;; For example: If you use the configuration called "files-and-scratch" you
;; can cycle through all file buffers and *scratch* although your current
;; configuration perhaps is "files" which ignores buffer *scratch*.

;;; History:

;;; Code:

;; ----------------------------------------------------------------------
;; Globals for customization
;; ----------------------------------------------------------------------

(defgroup bs nil
  "Buffer Selection: Maintaining buffers by buffer menu."
  :group 'convenience)

(defgroup bs-appearence nil
  "Buffer Selection appearence: Appearence of bs buffer menu."
  :group 'bs)

(defcustom bs-attributes-list
  '((""       1   1 left  bs--get-marked-string)
    ("M"      1   1 left  bs--get-modified-string)
    ("R"      2   2 left  bs--get-readonly-string)
    ("Buffer" bs--get-name-length 10 left  bs--get-name)
    (""       1   1 left  " ")
    ("Size"   8   8 right bs--get-size-string)
    (""       1   1 left  " ")
    ("Mode"   12 12 right bs--get-mode-name)
    (""       2   2 left  "  ")
    ("File"   12 12 left  bs--get-file-name)
    (""       2   2 left  "  "))
  "*List specifying the layout of a Buffer Selection Menu buffer.
Each entry specifies a column and is a list of the form of:
(HEADER MINIMUM-LENGTH MAXIMUM-LENGTH ALIGNMENT FUN-OR-STRING)
HEADER         : string for header for first line or a function
  which calculates column title.
MINIMUM-LENGTH : minimum width of column (number or name of function).
  The function must return a positive integer.
MAXIMUM-LENGTH : maximum width of column (number or name of function)
                 (currently ignored)
ALIGNMENT      : alignment of column: (`left' `right' `middle')
FUN-OR-STRING  : Name of a function for calculating the value or
a string for a constant value.
The function gets as parameter the buffer we have started
buffer selection and the list of all buffers to show.  The function must
return a string representing the columns value."
  :group 'bs-appearence
  :type '(repeat sexp))

(defvar bs--running-in-xemacs (string-match "XEmacs" (emacs-version))
  "Non-nil when running under XEmacs.")


(defun bs--make-header-match-string ()
  "Return a regexp matching the first line of a Buffer Selection Menu buffer."
  (let ((res "^\\(")
	(ele  bs-attributes-list))
    (while ele
      (setq res (concat res (car (car ele)) " *"))
      (setq ele (cdr ele)))
    (concat res "$\\)")))

;;; Font-Lock-Settings
(defvar bs-mode-font-lock-keywords
  (list ;; header in font-lock-type-face
        (list (bs--make-header-match-string)
	      '(1 font-lock-type-face append) '(1 'bold append))
	;; Buffername embedded by *
	(list "^\\(.*\\*.*\\*.*\\)$"
	      1 (if bs--running-in-xemacs
		    ;; problem in XEmacs with font-lock-constant-face
		    (if (facep 'font-lock-constant-face)
			'font-lock-constant-face
		      'font-lock-comment-face)
		  'font-lock-constant-face))
	;; Dired-Buffers
	'("^..\\(.*Dired by .*\\)$" 1 font-lock-function-name-face)
	;; the star for modified buffers
	'("^.\\(\\*\\) +[^\\*]"     1 font-lock-comment-face))
  "Default font lock expressions for Buffer Selection Menu.")

(defcustom bs-max-window-height 20
  "*Maximal window height of Buffer Selection Menu."
  :group 'bs-appearence
  :type 'integer)

(defvar bs-dont-show-regexp nil
  "Regular expression specifying which buffers not to show.
A buffer whose name matches this regular expression will not be
included in the buffer list.")

(defvar bs-must-show-regexp nil
  "Regular expression for specifying buffers which must be shown.
A buffer whose name matches this regular expression will be
included in the buffer list.
Note that this variable is temporary: if the configuration is changed
it is reset to nil.  Use `bs-must-always-show-regexp' to specify buffers
that must always be shown regardless of the configuration.")

(defcustom bs-must-always-show-regexp nil
  "*Regular expression for specifying buffers to show always.
A buffer whose name matches this regular expression will
be shown regardless of current configuration of Buffer Selection Menu."
  :group 'bs
  :type '(choice (const :tag "Nothing at all" nil) regexp))

(defvar bs-dont-show-function nil
  "Function for specifying buffers not to show.
The function gets one argument - the buffer to test.  The function must
return a value different from nil to ignore the buffer in
Buffer Selection Menu.")

(defvar bs-must-show-function nil
  "Function for specifying buffers which must be shown.
The function gets one argument - the buffer to test.")

(defvar bs-buffer-sort-function nil
  "Sort function to sort the buffers that appear in Buffer Selection Menu.
The functions gets two arguments - the buffers to compare.")

(defcustom bs-maximal-buffer-name-column 45
  "*Maximum column width for buffer names.
The column for buffer names has dynamic width.  The width depends on
maximal and minimal length of names of buffers to show.  The maximal
width is bounded by `bs-maximal-buffer-name-column'.
See also `bs-minimal-buffer-name-column'."
  :group 'bs-appearence
  :type 'integer)

(defcustom bs-minimal-buffer-name-column 15
  "*Minimum column width for buffer names.
The column for buffer names has dynamic width.  The width depends on
maximal and minimal length of names of buffers to show.  The minimal
width is bounded by `bs-minimal-buffer-name-column'.
See also `bs-maximal-buffer-name-column'."
  :group 'bs-appearence
  :type 'integer)

(defconst bs-header-lines-length 2
  "Number of lines for headers in Buffer Selection Menu.")

(defcustom bs-configurations
  '(("all" nil nil nil nil nil)
    ("files" nil nil nil bs-visits-non-file bs-sort-buffer-interns-are-last)
    ("files-and-scratch" "^\\*scratch\\*" nil nil bs-visits-non-file
     bs-sort-buffer-interns-are-last)
    ("all-intern-last" nil nil nil nil bs-sort-buffer-interns-are-last))
  "*List of all configurations you can use in the Buffer Selection Menu.
A configuration describes which buffers appear in Buffer Selection Menu
and describes the order of buffers.  A configuration is a list with
six elements.  The first element is a string and describes the configuration.
The following five elements represent the values for Buffer Selection Menu
configurations variables `bs-dont-show-regexp', `bs-dont-show-function',
`bs-must-show-regexp', `bs-must-show-function' and `bs-buffer-sort-function'.
By setting these variables you define a configuration."
  :group 'bs-appearence
  :type '(repeat sexp))

(defcustom bs-default-configuration "files"
  "*Name of default configuration used by in the Buffer Selection Menu.
\\<bs-mode-map>
Will be changed using key \\[bs-select-next-configuration].
Must be a string used in `bs-configurations' for naming a configuration."
  :group 'bs
  :type 'string)

(defcustom bs-alternative-configuration "all"
  "*Name of configuration used when calling `bs-show' with \
\\[universal-argument] as prefix key.
Must be a string used in `bs-configurations' for naming a configuration."
  :group 'bs
  :type  'string)

(defvar bs-current-configuration bs-default-configuration
  "Name of current configuration.
Must be a string found in `bs-configurations' for naming a configuration.")

(defcustom bs-cycle-configuration-name nil
  "*Name of configuration used when cycling through the buffer list.
A value of nil means to use current configuration `bs-default-configuration'.
Must be a string used in `bs-configurations' for naming a configuration."
  :group 'bs
  :type '(choice (const :tag "like current configuration" nil)
		 string))

(defcustom bs-string-show-always "+"
  "*String added in column 1 indicating a buffer will always be shown."
  :group 'bs-appearence
  :type 'string)

(defcustom bs-string-show-never "-"
  "*String added in column 1 indicating a buffer will never be shown."
  :group 'bs-appearence
  :type 'string)

(defcustom bs-string-current "."
  "*String added in column 1 indicating the current buffer."
  :group 'bs-appearence
  :type 'string)

(defcustom bs-string-current-marked "#"
  "*String added in column 1 indicating the current buffer when it is marked."
  :group 'bs-appearence
  :type 'string)

(defcustom bs-string-marked ">"
  "*String added in column 1 indicating a marked buffer."
  :group 'bs-appearence
  :type 'string)

(defcustom bs-string-show-normally  " "
  "*String added in column 1 indicating a unmarked buffer."
  :group 'bs-appearence
  :type 'string)

(defvar bs--name-entry-length 20
  "Maximum length of all displayed buffer names.
Used internally, only.")

;; ----------------------------------------------------------------------
;; Intern globals
;; ----------------------------------------------------------------------

(defvar bs-buffer-show-mark nil
  "Flag for the current mode for showing this buffer.
A value of nil means buffer will be shown depending on the current on
current configuration.
A value of `never' means to never show the buffer.
A value of `always' means to show buffer regardless of the configuration.")

(make-variable-buffer-local 'bs-buffer-show-mark)

;; Make face named region (for XEmacs)
(unless (facep 'region)
  (make-face 'region)
  (set-face-background 'region "gray75"))


(defun bs--sort-by-name (b1 b2)
  "Compare buffers B1 and B2 by buffer name."
  (string< (buffer-name b1)
	   (buffer-name b2)))

(defun bs--sort-by-filename (b1 b2)
  "Compare buffers B1 and B2 by file name."
  (string< (or (buffer-file-name b1) "")
	   (or (buffer-file-name b2) "")))

(defun bs--sort-by-mode (b1 b2)
  "Compare buffers B1 and B2 by mode name."
  (save-excursion
    (string< (progn (set-buffer b1) (format "%s" mode-name))
	     (progn (set-buffer b2) (format "%s" mode-name)))))

(defun bs--sort-by-size (b1 b2)
  "Compare buffers B1 and B2 by buffer size."
  (save-excursion
    (< (progn (set-buffer b1) (buffer-size))
       (progn (set-buffer b2) (buffer-size)))))

(defcustom bs-sort-functions
  '(("by name"     bs--sort-by-name     "Buffer" region)
    ("by size"     bs--sort-by-size     "Size"   region)
    ("by mode"     bs--sort-by-mode     "Mode"   region)
    ("by filename" bs--sort-by-filename "File"   region)
    ("by nothing"  nil                  nil      nil))
  "*List of all possible sorting aspects for Buffer Selection Menu.
You can add a new entry with a call to `bs-define-sort-function'.
Each element is a list of four elements (NAME FUNCTION REGEXP-FOR-SORTING FACE)
NAME specifies the sort order defined by function FUNCTION.
FUNCTION nil means don't sort the buffer list.  Otherwise the functions
must have two parameters - the buffers to compare.
REGEXP-FOR-SORTING is a regular expression which describes the
column title to highlight.
FACE is a face used to fontify the sorted column title.  A value of nil means
don't highlight."
  :group 'bs
  :type '(repeat sexp))

(defun bs-define-sort-function (name fun &optional regexp-for-sorting face)
  "Define a new function for buffer sorting in Buffer Selection Menu.
NAME specifies the sort order defined by function FUN.
A value of nil for FUN means don't sort the buffer list.  Otherwise the
functions must have two parameters - the buffers to compare.
REGEXP-FOR-SORTING is a regular expression which describes the
column title to highlight.
FACE is a face used to fontify the sorted column title.  A value of nil means
don't highlight.
The new sort aspect will be inserted into list `bs-sort-functions'."
  (let ((tupel (assoc name bs-sort-functions)))
    (if tupel
	(setcdr tupel (list fun regexp-for-sorting face))
      (setq bs-sort-functions
	    (cons (list name fun regexp-for-sorting face)
		  bs-sort-functions)))))

(defvar bs--current-sort-function nil
  "Description of the current function for sorting the buffer list.
This is an element of `bs-sort-functions'.")

(defcustom bs-default-sort-name "by nothing"
  "*Name of default sort behavior.
Must be \"by nothing\" or a string used in `bs-sort-functions' for
naming a sort behavior.  Default is \"by nothing\" which means no sorting."
  :group 'bs
  :type  'string
  :set (lambda (var-name value)
	 (set var-name value)
	 (setq bs--current-sort-function
	       (assoc value bs-sort-functions))))

(defvar bs--buffer-coming-from nil
  "The buffer in which the user started the current Buffer Selection Menu.")

(defvar bs--show-all nil
  "Flag whether showing all buffers regardless of current configuration.
Non nil means to show all buffers.  Otherwise show buffers
defined by current configuration `bs-current-configuration'.")

(defvar bs--window-config-coming-from nil
  "Window configuration before starting Buffer Selection Menu.")

(defvar bs--intern-show-never "^ \\|\\*buffer-selection\\*"
  "Regular expression specifying which buffers never to show.
A buffer whose name matches this regular expression will never be
included in the buffer list.")

(defvar bs-current-list nil
  "List of buffers shown in Buffer Selection Menu.
Used internally, only.")

(defvar bs--marked-buffers nil
  "Currently marked buffers in Buffer Selection Menu.")

(defvar bs-mode-map ()
  "Keymap of `bs-mode'.")

(if bs-mode-map
    ()
  (setq bs-mode-map (make-sparse-keymap))
  (define-key bs-mode-map " "       'bs-select)
  (define-key bs-mode-map "f"       'bs-select)
  (define-key bs-mode-map "v"       'bs-view)
  (define-key bs-mode-map "!"       'bs-select-in-one-window)
  (define-key bs-mode-map [mouse-2] 'bs-mouse-select) ;; for GNU EMACS
  (define-key bs-mode-map [button2] 'bs-mouse-select) ;; for XEmacs
  (define-key bs-mode-map "F"       'bs-select-other-frame)

  (let ((key ?1))
    (while (<= key ?9)
      (define-key bs-mode-map (char-to-string key) 'digit-argument)
      (setq key (1+ key))))

  (define-key bs-mode-map "-"       'negative-argument)
  (define-key bs-mode-map "\e-"     'negative-argument)

  (define-key bs-mode-map "o"       'bs-select-other-window)
  (define-key bs-mode-map "\C-o"    'bs-tmp-select-other-window)
  ;; for GNU EMACS
  (define-key bs-mode-map [mouse-3] 'bs-mouse-select-other-frame)
  ;; for XEmacs
  (define-key bs-mode-map [button3] 'bs-mouse-select-other-frame)
  (define-key bs-mode-map [up]      'bs-up)
  (define-key bs-mode-map "n"       'bs-down)
  (define-key bs-mode-map "p"       'bs-up)
  (define-key bs-mode-map [down]    'bs-down)
  (define-key bs-mode-map "\C-m"    'bs-select)
  (define-key bs-mode-map "b"       'bs-bury-buffer)
  (define-key bs-mode-map "s"       'bs-save)
  (define-key bs-mode-map "S"       'bs-show-sorted)
  (define-key bs-mode-map "a"       'bs-toggle-show-all)
  (define-key bs-mode-map "d"       'bs-delete)
  (define-key bs-mode-map "\C-d"    'bs-delete-backward)
  (define-key bs-mode-map "k"       'bs-delete)
  (define-key bs-mode-map "g"       'bs-refresh)
  (define-key bs-mode-map "C"       'bs-set-configuration-and-refresh)
  (define-key bs-mode-map "c"       'bs-select-next-configuration)
  (define-key bs-mode-map "q"       'bs-kill)
  ;; (define-key bs-mode-map "z"       'bs-kill)
  (define-key bs-mode-map "\C-c\C-c" 'bs-kill)
  (define-key bs-mode-map "\C-g"    'bs-abort)
  (define-key bs-mode-map "\C-]"    'bs-abort)
  (define-key bs-mode-map "%"       'bs-toggle-readonly)
  (define-key bs-mode-map "~"       'bs-clear-modified)
  (define-key bs-mode-map "M"       'bs-toggle-current-to-show)
  (define-key bs-mode-map "+"       'bs-set-current-buffer-to-show-always)
  ;;(define-key bs-mode-map "-"       'bs-set-current-buffer-to-show-never)
  (define-key bs-mode-map "t"       'bs-visit-tags-table)
  (define-key bs-mode-map "m"       'bs-mark-current)
  (define-key bs-mode-map "u"       'bs-unmark-current)
  (define-key bs-mode-map ">"       'scroll-right)
  (define-key bs-mode-map "<"       'scroll-left)
  (define-key bs-mode-map "\e\e"    nil)
  (define-key bs-mode-map "\e\e\e"  'bs-kill)
  (define-key bs-mode-map [escape escape escape] 'bs-kill)
  (define-key bs-mode-map "?"       'bs-help))

;; ----------------------------------------------------------------------
;; Functions
;; ----------------------------------------------------------------------

(defun bs-buffer-list (&optional list sort-description)
  "Return a list of buffers to be shown.
LIST is a list of buffers to test for appearence in Buffer Selection Menu.
The result list depends on the global variables `bs-dont-show-regexp',
`bs-must-show-regexp', `bs-dont-show-function', `bs-must-show-function'
and `bs-buffer-sort-function'.
If SORT-DESCRIPTION isn't nil the list will be sorted by
a special function.  SORT-DESCRIPTION is an element of `bs-sort-functions'."
  (setq sort-description (or sort-description bs--current-sort-function)
	list (or list (buffer-list)))
  (let ((result nil))
    (while list
      (let* ((buffername (buffer-name (car list)))
	     (int-show-never (string-match bs--intern-show-never buffername))
	     (ext-show-never (and bs-dont-show-regexp
				     (string-match bs-dont-show-regexp
						   buffername)))
	     (extern-must-show (or (and bs-must-always-show-regexp
					(string-match bs-must-always-show-regexp
						      buffername))
				   (and bs-must-show-regexp
					(string-match bs-must-show-regexp
						      buffername))))
	     (extern-show-never-from-fun (and bs-dont-show-function
					      (funcall bs-dont-show-function
						       (car list))))
	     (extern-must-show-from-fun (and bs-must-show-function
					     (funcall bs-must-show-function
						      (car list))))
	     (show-flag (save-excursion
			  (set-buffer (car list))
			  bs-buffer-show-mark)))
	(if (or (eq show-flag 'always)
		(and (or bs--show-all (not (eq show-flag 'never)))
		     (not int-show-never)
		     (or bs--show-all
			 extern-must-show
			 extern-must-show-from-fun
			 (and (not ext-show-never)
			      (not extern-show-never-from-fun)))))
	    (setq result (cons (car list)
			       result)))
	(setq list (cdr list))))
    (setq result (reverse result))
    ;; The current buffer which was the start point of bs should be an element
    ;; of result list, so that we can leave with space and be back in the
    ;; buffer we started bs-show.
    (if (and bs--buffer-coming-from
	     (buffer-live-p bs--buffer-coming-from)
	     (not (memq bs--buffer-coming-from result)))
	(setq result (cons bs--buffer-coming-from result)))
    ;; sorting
    (if (and sort-description
	     (nth 1 sort-description))
	(setq result (sort result (nth 1 sort-description)))
      ;; else standard sorting
      (bs-buffer-sort result))))

(defun bs-buffer-sort (buffer-list)
  "Sort buffers in BUFFER-LIST according to `bs-buffer-sort-function'."
  (if bs-buffer-sort-function
      (sort buffer-list bs-buffer-sort-function)
    buffer-list))

;; XEmacs Support
(unless (fboundp 'line-end-position)
  (defun line-end-position ()
    "Return the point at the end of the current line."
    (save-excursion
      (end-of-line)
      (point))))

(defun bs--redisplay (&optional keep-line-p sort-description)
  "Redisplay whole Buffer Selection Menu.
If KEEP-LINE-P is non nil the point will stay on current line.
SORT-DESCRIPTION is an element of `bs-sort-functions'"
  (let ((line (1+ (count-lines 1 (point)))))
    (bs-show-in-buffer (bs-buffer-list nil sort-description))
    (if keep-line-p
	(goto-line line))
    (beginning-of-line)))

(defun bs--goto-current-buffer ()
  "Goto line which represents the current buffer;
actually the line which begins with character in `bs-string-current' or
`bs-string-current-marked'."
  (let (point
	(regexp (concat "^"
			(regexp-quote bs-string-current)
			"\\|^"
			(regexp-quote bs-string-current-marked))))
    (save-excursion
      (goto-char (point-min))
      (if (search-forward-regexp regexp nil t)
	  (setq point (- (point) 1))))
    (if point
	(goto-char point))))

(defun bs--current-config-message ()
  "Return a string describing the current `bs-mode' configuration."
  (if bs--show-all
      "Show all buffers."
    (format "Show buffer by configuration %S"
	    bs-current-configuration)))

(defun bs-mode ()
  "Major mode for editing a subset of Emacs' buffers.
\\<bs-mode-map>
Aside from two header lines each line describes one buffer.
Move to a line representing the buffer you want to edit and select
buffer by \\[bs-select] or SPC. Abort buffer list with \\[bs-kill].
There are many key commands similar to `Buffer-menu-mode' for
manipulating the buffer list and buffers.
For faster navigation each digit key is a digit argument.

\\[bs-select] or SPACE -- select current line's buffer and other marked buffers.
\\[bs-toggle-show-all] 	-- toggle between all buffers and a special subset.
\\[bs-select-other-window]	-- select current line's buffer in other window.
\\[bs-tmp-select-other-window]	-- make another window display that buffer and
	   remain in Buffer Selection Menu.
\\[bs-mouse-select]	-- select current line's buffer and other marked buffers.
\\[bs-save]	-- save current line's buffer immediatly.
\\[bs-delete]	-- kill current line's buffer immediatly.
\\[bs-toggle-readonly]	-- toggle read-only status of current line's buffer.
\\[bs-clear-modified]	-- clear modified-flag on that buffer.
\\[bs-mark-current]	-- mark current line's buffer to be displayed.
\\[bs-unmark-current]	-- unmark current line's buffer to be displayed.
\\[bs-show-sorted]	-- display buffer list sorted by next sort aspect.
\\[bs-set-configuration-and-refresh]	-- ask user for a configuration and \
apply selected configuration.
\\[bs-select-next-configuration]	-- select and apply next \
available Buffer Selection Menu configuration.
\\[bs-kill]	-- leave Buffer Selection Menu without a selection.
\\[bs-toggle-current-to-show]	-- toggle status of appearence .
\\[bs-set-current-buffer-to-show-always]	-- mark current line's buffer \
to show always.
\\[bs-visit-tags-table]	-- call `visit-tags-table' on current line'w buffer.
\\[bs-help]	-- display this help text."
  (interactive)
  (kill-all-local-variables)
  (use-local-map bs-mode-map)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'font-lock-verbose)
  (setq major-mode 'bs-mode
	mode-name "Buffer-Selection-Menu"
	buffer-read-only t
	truncate-lines t
	font-lock-defaults '(bs-mode-font-lock-keywords t)
	font-lock-verbose nil)
  (run-hooks 'bs-mode-hook))

(defun bs-kill ()
  "Let buffer disappear and reset window-configuration."
  (interactive)
  (bury-buffer (current-buffer))
  (set-window-configuration bs--window-config-coming-from))

(defun bs-abort ()
  "Ding and leave Buffer Selection Menu without a selection."
 (interactive)
  (ding)
  (bs-kill))

(defun bs-set-configuration-and-refresh ()
  "Ask user for a configuration and apply selected configuration.
Refresh whole Buffer Selection Menu."
  (interactive)
  (let ((starting-buffer (bs--current-buffer t)))
    (call-interactively 'bs-set-configuration)
    (setq bs-default-configuration bs-current-configuration)
    (bs--redisplay t)
    (bs-goto-buffer starting-buffer)))

(defun bs-refresh ()
  "Refresh whole Buffer Selection Menu."
  (interactive)
  (bs--redisplay t))

(defun bs--window-for-buffer (buffer-name)
  "Return a window showing a buffer with name BUFFER-NAME.
Take only windows of current frame into account.
Return nil if there is no such buffer."
  (let ((window nil))
    (walk-windows (lambda (wind)
		    (if (string= (buffer-name (window-buffer wind))
				 buffer-name)
			(setq window wind))))
    window))

(defun bs--set-window-height ()
  "Change the height of the selected window to suit the current buffer list."
  (unless (one-window-p t)
    (shrink-window (- (window-height (selected-window))
		      ;; window-height in xemacs includes mode-line
		      (+ (if bs--running-in-xemacs 3 1)
			 bs-header-lines-length
			 (min (length bs-current-list)
			      bs-max-window-height))))))

(defun bs--current-buffer (&optional no-error)
  "Return buffer on current line.
Raise an error if not an a buffer line and NO-ERROR is nil."
  (beginning-of-line)
  (let ((line (+ (- bs-header-lines-length)
		 (count-lines 1 (point)))))
    (if (< line 0)
	(unless no-error
	  (error "You are on a header row")))
    (nth line bs-current-list)))

(defun bs--update-current-line ()
  "Update the entry on current line for Buffer Selection Menu."
  (let ((buffer (bs--current-buffer))
	(inhibit-read-only t))
    (beginning-of-line)
    (delete-region (point) (line-end-position))
    (bs--insert-one-entry buffer)
    (beginning-of-line)))

(defun bs-view ()
  "View current line's buffer in View mode.
Leave Buffer Selection Menu."
  (interactive)
  (view-buffer (bs--current-buffer)))

(defun bs-select ()
  "Select current line's buffer and other marked buffers.
If there are no marked buffers the window configuration before starting
Buffer Selectin Menu will be restored.
If there are marked buffers each marked buffer and the current line's buffer
will be selected in a window.
Leave Buffer Selection Menu."
  (interactive)
  (let ((buffer (bs--current-buffer)))
    (bury-buffer (current-buffer))
    (set-window-configuration bs--window-config-coming-from)
    (switch-to-buffer buffer)
    (if bs--marked-buffers
	;; Some marked buffers for selection
	(let* ((all (delq buffer bs--marked-buffers))
	       (height (/ (1- (frame-height)) (1+ (length all)))))
	  (delete-other-windows)
	  (switch-to-buffer buffer)
	  (while all
	    (split-window nil height)
	    (other-window 1)
	    (switch-to-buffer (car all))
	    (setq all (cdr all)))
	  ;; goto window we have started bs.
	  (other-window 1)))))

(defun bs-select-other-window ()
  "Select current line's buffer by `switch-to-buffer-other-window'.
The window configuration before starting Buffer Selectin Menu will be restored
unless there is no other window.  In this case a new window will be created.
Leave Buffer Selection Menu."
  (interactive)
  (let ((buffer (bs--current-buffer)))
    (bury-buffer (current-buffer))
    (set-window-configuration bs--window-config-coming-from)
    (switch-to-buffer-other-window buffer)))
 
(defun bs-tmp-select-other-window ()
  "Make the other window select this line's buffer.
The current window remains selected."
  (interactive)
  (let ((buffer (bs--current-buffer)))
    (display-buffer buffer t)))
 
(defun bs-select-other-frame ()
  "Select current line's buffer in new created frame.
Leave Buffer Selection Menu."
  (interactive)
  (let ((buffer (bs--current-buffer)))
    (bury-buffer (current-buffer))
    (set-window-configuration bs--window-config-coming-from)
    (switch-to-buffer-other-frame buffer)))

(defun bs-mouse-select-other-frame (event)
  "Select selected line's buffer in new created frame.
Leave Buffer Selection Menu.
EVENT: a mouse click EVENT."
  (interactive "e")
  (mouse-set-point event)
  (bs-select-other-frame))

(defun bs-mouse-select (event)
  "Select buffer on mouse click EVENT.
Select buffer by `bs-select'."
  (interactive "e")
  (mouse-set-point event)
  (bs-select))

(defun bs-select-in-one-window ()
  "Select current line's buffer in one window and delete other windows.
Leave Buffer Selection Menu."
  (interactive)
  (bs-select)
  (delete-other-windows))

(defun bs-bury-buffer ()
  "Bury buffer on current line."
  (interactive)
  (bury-buffer (bs--current-buffer))
  (bs--redisplay t))

(defun bs-save ()
  "Save buffer on current line."
  (interactive)
  (let ((buffer (bs--current-buffer)))
    (save-excursion
      (set-buffer buffer)
      (save-buffer))
    (bs--update-current-line)))

(defun bs-visit-tags-table ()
  "Visit the tags table in the buffer on this line.
See `visit-tags-table'."
  (interactive)
  (let ((file (buffer-file-name (bs--current-buffer))))
    (if file
	(visit-tags-table file)
      (error "Specified buffer has no file"))))

(defun bs-toggle-current-to-show ()
  "Toggle status of showing flag for buffer in current line."
  (interactive)
  (let ((buffer (bs--current-buffer))
	res)
    (save-excursion
      (set-buffer buffer)
      (setq res (cond ((null bs-buffer-show-mark)
		       'never)
		      ((eq bs-buffer-show-mark 'never)
		       'always)
		      (t nil)))
      (setq bs-buffer-show-mark res))
    (bs--update-current-line)
    (bs--set-window-height)
    (bs--show-config-message res)))

(defun bs-set-current-buffer-to-show-always (&optional not-to-show-p)
  "Toggle status of buffer on line to `always shown'.
NOT-TO-SHOW-P: prefix argument.
With no prefix argument the buffer on current line is marked to show
always.  Otherwise it is marked to show never."
  (interactive "P")
  (if not-to-show-p
      (bs-set-current-buffer-to-show-never)
    (bs--set-toggle-to-show (bs--current-buffer) 'always)))

(defun bs-set-current-buffer-to-show-never ()
  "Toggle status of buffer on line to `never shown'."
  (interactive)
  (bs--set-toggle-to-show (bs--current-buffer) 'never))

(defun bs--set-toggle-to-show (buffer what)
  "Set value `bs-buffer-show-mark' of buffer BUFFER to WHAT.
Redisplay current line and display a message describing
the status of buffer on current line."
  (save-excursion
    (set-buffer buffer)
    (setq bs-buffer-show-mark what))
  (bs--update-current-line)
  (bs--set-window-height)
  (bs--show-config-message what))

(defun bs-mark-current (count)
  "Mark buffers.
COUNT is the number of buffers to mark.
Move cursor vertically down COUNT lines."
  (interactive "p")
  (let ((dir (if (> count 0) 1 -1))
	(count (abs count)))
    (while (> count 0)
      (let ((buffer (bs--current-buffer)))
	(if buffer
	    (setq bs--marked-buffers (cons buffer bs--marked-buffers)))
	(bs--update-current-line)
	(bs-down dir))
      (setq count (1- count)))))

(defun bs-unmark-current (count)
  "Unmark buffers.
COUNT is the number of buffers to unmark.
Move cursor vertically down COUNT lines."
  (interactive "p")
  (let ((dir (if (> count 0) 1 -1))
	(count (abs count)))
    (while (> count 0)
      (let ((buffer (bs--current-buffer)))
	(if buffer
	    (setq bs--marked-buffers (delq buffer bs--marked-buffers)))
	(bs--update-current-line)
	(bs-down dir))
      (setq count (1- count)))))

(defun bs--show-config-message (what)
  "Show message indicating the new showing status WHAT.
WHAT is a value of nil, `never', or `always'."
  (bs-message-without-log (cond ((null what)
				 "Buffer will be shown normally.")
				((eq what 'never)
				 "Mark buffer to never be shown.")
				(t "Mark buffer to show always."))))

(defun bs-delete ()
  "Kill buffer on current line."
  (interactive)
  (let ((current (bs--current-buffer))
	(inhibit-read-only t))
    (setq bs-current-list (delq current bs-current-list))
    (kill-buffer current)
    (beginning-of-line)
    (delete-region (point) (save-excursion
			     (end-of-line)
			     (if (eobp) (point) (1+ (point)))))
    (if (eobp)
	(progn
	  (backward-delete-char 1)
	  (beginning-of-line)
	  (recenter -1)))
    (bs--set-window-height)))

(defun bs-delete-backward ()
  "Like `bs-delete' but go to buffer in front of current."
  (interactive)
  (let ((on-last-line-p (save-excursion (end-of-line) (eobp))))
    (bs-delete)
    (unless on-last-line-p
	(bs-up 1))))

(defun bs-show-sorted ()
  "Show buffer list sorted by buffer name."
  (interactive)
  (setq bs--current-sort-function
	(bs-next-config-aux (car bs--current-sort-function)
			    bs-sort-functions))
  (bs--redisplay)
  (bs--goto-current-buffer)
  (bs-message-without-log "Sorted %s" (car bs--current-sort-function)))

(defun bs-apply-sort-faces (&optional sort-description)
  "Set text properties for the sort described by SORT-DESCRIPTION.
SORT-DESCRIPTION is an element of `bs-sort-functions'.
Default is `bs--current-sort-function'."
  (let ((sort-description (or sort-description
			      bs--current-sort-function)))
    (save-excursion
      (goto-char (point-min))
      (if (and window-system
	       (nth 2 sort-description)
	       (search-forward-regexp (nth 2 sort-description) nil t))
	  (let ((inhibit-read-only t))
	    (put-text-property (match-beginning 0)
			       (match-end 0)
			       'face
			       (or (nth 3 sort-description)
				   'region)))))))

(defun bs-toggle-show-all ()
  "Toggle show all buffers / show buffers with current configuration."
  (interactive)
  (setq bs--show-all (not bs--show-all))
  (bs--redisplay)
  (bs--goto-current-buffer)
  (bs-message-without-log "%s" (bs--current-config-message)))

(defun bs-toggle-readonly ()
  "Toggle read-only status for buffer on current line.
Uses Function `vc-toggle-read-only'."
  (interactive)
  (let ((buffer (bs--current-buffer)))
    (save-excursion
      (set-buffer buffer)
      (vc-toggle-read-only))
    (bs--update-current-line)))

(defun bs-clear-modified ()
  "Set modified flag for buffer on current line to nil."
  (interactive)
  (let ((buffer (bs--current-buffer)))
    (save-excursion
      (set-buffer buffer)
      (set-buffer-modified-p nil)))
  (bs--update-current-line))

(defun bs--nth-wrapper (count fun &rest args)
  "Call COUNT times function FUN with arguments ARGS."
  (setq count (or count 1))
  (while (> count 0)
    (apply fun args)
    (setq count (1- count))))

(defun bs-up (arg)
  "Move cursor vertically up ARG lines in Buffer Selection Menu."
  (interactive "p")
  (if (and arg (numberp arg) (< arg 0))
      (bs--nth-wrapper (- arg) 'bs--down)
    (bs--nth-wrapper arg 'bs--up)))

(defun bs--up ()
  "Move cursor vertically up one line.
If on top of buffer list go to last line."
  (interactive "p")
  (previous-line 1)
  (if (<= (count-lines 1 (point)) (1- bs-header-lines-length))
      (progn
	(goto-char (point-max))
	(beginning-of-line)
	(recenter -1))
    (beginning-of-line)))

(defun bs-down (arg)
  "Move cursor vertically down ARG lines in Buffer Selection Menu."
  (interactive "p")
  (if (and arg (numberp arg) (< arg 0))
      (bs--nth-wrapper (- arg) 'bs--up)
    (bs--nth-wrapper arg 'bs--down)))

(defun bs--down ()
  "Move cursor vertically down one line.
If at end of buffer list go to first line."
  (let ((last (line-end-position)))
    (if (eq last (point-max))
	(goto-line (1+ bs-header-lines-length))
      (next-line 1))))

(defun bs-visits-non-file (buffer)
  "Return t or nil whether BUFFER visits no file.
A value of t means BUFFER belongs to no file.
A value of nil means BUFFER belongs to a file."
  (not (buffer-file-name buffer)))

(defun bs-sort-buffer-interns-are-last (b1 b2)
  "Function for sorting intern buffers B1 and B2 at the end of all buffers."
  (string-match "^\\*" (buffer-name b2)))

;; ----------------------------------------------------------------------
;; Configurations:
;; ----------------------------------------------------------------------

(defun bs-config-clear ()
  "*Reset all variables which specify a configuration.
These variables are `bs-dont-show-regexp', `bs-must-show-regexp',
`bs-dont-show-function', `bs-must-show-function' and
`bs-buffer-sort-function'."
  (setq bs-dont-show-regexp nil
	bs-must-show-regexp nil
	bs-dont-show-function nil
	bs-must-show-function nil
	bs-buffer-sort-function nil))

(defun bs-config--only-files ()
  "Define a configuration for showing only buffers visiting a file."
  (bs-config-clear)
  (setq ;; I want to see *-buffers at the end
        bs-buffer-sort-function 'bs-sort-buffer-interns-are-last
	;; Don't show files who don't belong to a file
	bs-dont-show-function 'bs-visits-non-file))

(defun bs-config--files-and-scratch ()
  "Define a configuration for showing buffer *scratch* and file buffers."
  (bs-config-clear)
  (setq ;; I want to see *-buffers at the end
        bs-buffer-sort-function 'bs-sort-buffer-interns-are-last
	;; Don't show files who don't belong to a file
	bs-dont-show-function 'bs-visits-non-file
	;; Show *scratch* buffer.
	bs-must-show-regexp "^\\*scratch\\*"))

(defun bs-config--all ()
  "Define a configuration for showing all buffers.
Reset all according variables by `bs-config-clear'."
  (bs-config-clear))

(defun bs-config--all-intern-last ()
  "Define a configuration for showing all buffers.
Intern buffers appear at end of all buffers."
  (bs-config-clear)
  ;; I want to see *-buffers at the end
  (setq bs-buffer-sort-function 'bs-sort-buffer-interns-are-last))

(defun bs-set-configuration (name)
  "Set configuration to the one saved under string NAME in `bs-configurations'.
When called interactively ask user for a configuration and apply selected
configuration."
  (interactive (list (completing-read "Use configuration: "
				      bs-configurations
				      nil
				      t)))
  (let ((list (assoc name bs-configurations)))
    (if list
	(if (listp list)
	    (setq bs-current-configuration name
		  bs-must-show-regexp     (nth 1 list)
		  bs-must-show-function   (nth 2 list)
		  bs-dont-show-regexp     (nth 3 list)
		  bs-dont-show-function   (nth 4 list)
		  bs-buffer-sort-function (nth 5 list))
	  ;; for backward compability
	  (funcall (cdr list)))
      ;; else
      (ding)
      (bs-message-without-log "No bs-configuration named %S." name))))

(defun bs-help ()
  "Help for `bs-show'."
  (interactive)
  (describe-function 'bs-mode))

(defun bs-next-config-aux (start-name list)
  "Get the next assoc after START-NAME in list LIST.
Will return the first if START-NAME is at end."
  (let ((assocs list)
	(length (length list))
	pos)
    (while (and assocs (not pos))
      (if (string= (car (car assocs)) start-name)
	  (setq pos (- length (length assocs))))
      (setq assocs (cdr assocs)))
    (setq pos (1+ pos))
    (if (eq pos length)
	(car list)
      (nth pos list))))

(defun bs-next-config (name)
  "Return next configuration with respect to configuration with name NAME."
  (bs-next-config-aux name bs-configurations))

(defun bs-select-next-configuration (&optional start-name)
  "Apply next configuration START-NAME and refresh buffer list.
If START-NAME is nil the current configuration `bs-current-configuration'
will be used."
  (interactive)
  (let ((starting-buffer (bs--current-buffer t))
	(config (bs-next-config (or start-name bs-current-configuration))))
    (bs-set-configuration (car config))
    (setq bs-default-configuration bs-current-configuration)
    (bs--redisplay t)
    (bs--set-window-height)
    (bs-goto-buffer starting-buffer)
    (bs-message-without-log "Selected config: %s" (car config))))

(defun bs-goto-buffer (buffer)
  "Goto line for buffer BUFFER."
  (let ((buffers bs-current-list))
    (while (and buffers (not (eq (car buffers) buffer)))
      (setq buffers (cdr buffers)))
    (if buffers ;; Found it
	(goto-line (+ 1 bs-header-lines-length
		      (- (length bs-current-list)
			 (length buffers)))))))

(defun bs-show-in-buffer (list)
  "Display buffer list LIST in buffer *buffer-selection*.
Select buffer *buffer-selection* and display buffers according to current
configuration `bs-current-configuration'.  Set window height, fontify buffer
and move point to current buffer."
  (setq bs-current-list list)
  (switch-to-buffer (get-buffer-create "*buffer-selection*"))
  (bs-mode)
  (let* ((inhibit-read-only t)
	 (map-fun (lambda (entry)
		    (length (buffer-name entry))))
	 (max-length-of-names (apply 'max
				     (cons 0 (mapcar map-fun list))))
	 (name-entry-length (min bs-maximal-buffer-name-column
				 (max bs-minimal-buffer-name-column
				      max-length-of-names))))
    (erase-buffer)
    (setq bs--name-entry-length name-entry-length)
    (bs--show-header)
    (while list
      (bs--insert-one-entry (car list))
      (insert "\n")
      (setq list (cdr list)))
    (delete-backward-char 1)
    (bs--set-window-height)
    (bs--goto-current-buffer)
    (font-lock-fontify-buffer)
    (bs-apply-sort-faces)))

(defun bs-next-buffer (&optional buffer-list sorting-p)
  "Return next buffer and buffer list for buffer cycling in BUFFER-LIST.
Ignore sorting when SORTING-P is nil.
If BUFFER-LIST is nil the result of `bs-buffer-list' will be used as
buffer list.  The result is a cons of normally the second element of
BUFFER-LIST and the buffer list used for buffer cycling."
  (let* ((bs--current-sort-function (if sorting-p
					bs--current-sort-function))
	 (bs-buffer-list (or buffer-list (bs-buffer-list))))
    (cons (or (car (cdr bs-buffer-list))
	      (car bs-buffer-list)
	      (current-buffer))
	  bs-buffer-list)))

(defun bs-previous-buffer (&optional buffer-list sorting-p)
  "Return previous buffer and buffer list for buffer cycling in BUFFER-LIST.
Ignore sorting when SORTING-P is nil.
If BUFFER-LIST is nil the result of `bs-buffer-list' will be used as
buffer list.  The result is a cons of last element of BUFFER-LIST and the
buffer list used for buffer cycling."
  (let* ((bs--current-sort-function (if sorting-p
					bs--current-sort-function))
	 (bs-buffer-list (or buffer-list (bs-buffer-list))))
    (cons (or (car (last bs-buffer-list))
	      (current-buffer))
	  bs-buffer-list)))

(defun bs-message-without-log (&rest args)
  "Like `message' but don't log it on the message log.
All arguments ARGS are transfered to function `message'."
  (let ((message-log-max nil))
    (apply 'message args)))

(defvar bs--cycle-list nil
  "Currentyl buffer list used for cycling.")

;;;###autoload
(defun bs-cycle-next ()
  "Select next buffer defined by buffer cycling.
The buffers taking part in buffer cycling are defined
by buffer configuration `bs-cycle-configuration-name'."
  (interactive)
  (let ((bs--buffer-coming-from (current-buffer))
	(bs-dont-show-regexp   bs-dont-show-regexp)
	(bs-must-show-regexp   bs-must-show-regexp)
	(bs-dont-show-function bs-dont-show-function)
	(bs-must-show-function bs-must-show-function)
	(bs--show-all          bs--show-all))
    (if bs-cycle-configuration-name
	(bs-set-configuration bs-cycle-configuration-name))
    (let ((bs-buffer-sort-function nil)
	  (bs--current-sort-function nil))
      (let* ((tupel (bs-next-buffer (if (or (eq last-command
						'bs-cycle-next)
					    (eq last-command
						'bs-cycle-previous))
					bs--cycle-list)))
	     (next (car tupel))
	     (cycle-list (cdr tupel)))
	(setq bs--cycle-list (append (cdr cycle-list)
				     (list (car cycle-list))))
	(bury-buffer)
	(switch-to-buffer next)
	(bs-message-without-log "Next buffers: %s"
				(or (cdr bs--cycle-list)
				    "this buffer"))))))


;;;###autoload
(defun bs-cycle-previous ()
  "Select previous buffer defined by buffer cycling.
The buffers taking part in buffer cycling are defined
by buffer configuration `bs-cycle-configuration-name'."
  (interactive)
  (let ((bs--buffer-coming-from (current-buffer))
	(bs-dont-show-regexp   bs-dont-show-regexp)
	(bs-must-show-regexp   bs-must-show-regexp)
	(bs-dont-show-function bs-dont-show-function)
	(bs-must-show-function bs-must-show-function)
	(bs--show-all          bs--show-all))
    (if bs-cycle-configuration-name
	(bs-set-configuration bs-cycle-configuration-name))
    (let ((bs-buffer-sort-function nil)
	  (bs--current-sort-function nil))
      (let* ((tupel (bs-previous-buffer (if (or (eq last-command
						    'bs-cycle-next)
						(eq last-command
						    'bs-cycle-previous))
					    bs--cycle-list)))
	     (prev-buffer (car tupel))
	     (cycle-list (cdr tupel)))
	(setq bs--cycle-list (append (last cycle-list)
				     (reverse (cdr (reverse cycle-list)))))
	(switch-to-buffer prev-buffer)
	(bs-message-without-log "Previous buffers: %s"
				(or (reverse (cdr bs--cycle-list))
				    "this buffer"))))))
  
(defun bs--get-value (fun &optional args)
  "Apply function FUN with arguments ARGS.
Return result of evaluation.  Will return FUN if FUN is a number
or a string."
  (cond ((numberp fun)
	 fun)
	((stringp fun)
	 fun)
	(t (apply fun args))))
	
(defun bs--get-marked-string (start-buffer all-buffers)
  "Return a string which describes whether current buffer is marked.
START-BUFFER is the buffer where we started buffer selection.
ALL-BUFFERS is the list of buffer appearing in Buffer Selection Menu.
The result string is one of `bs-string-current', `bs-string-current-marked',
`bs-string-marked', `bs-string-show-normally', `bs-string-show-never', or
`bs-string-show-always'."
  (cond ;; current buffer is the buffer we started buffer selection.
        ((eq (current-buffer) start-buffer)
	 (if (memq (current-buffer) bs--marked-buffers)
	     bs-string-current-marked ; buffer is marked
	   bs-string-current))
	;; current buffer is marked
	((memq (current-buffer) bs--marked-buffers)
	 bs-string-marked)
	;; current buffer hasn't a special mark.
	((null bs-buffer-show-mark)
	 bs-string-show-normally)
	;; current buffer has a mark not to show itself.
	((eq bs-buffer-show-mark 'never)
	 bs-string-show-never)
	;; otherwise current buffer is marked to show always.
	(t
	 bs-string-show-always)))

(defun bs--get-modified-string (start-buffer all-buffers)
  "Return a string which describes whether current buffer is modified.
START-BUFFER is the buffer where we started buffer selection.
ALL-BUFFERS is the list of buffer appearing in Buffer Selection Menu."
  (if (buffer-modified-p) "*" " "))

(defun bs--get-readonly-string (start-buffer all-buffers)
  "Return a string which describes whether current buffer is read only.
START-BUFFER is the buffer where we started buffer selection.
ALL-BUFFERS is the list of buffer appearing in Buffer Selection Menu."
  (if buffer-read-only "%" " "))

(defun bs--get-size-string (start-buffer all-buffers)
  "Return a string which describes the size of current buffer.
START-BUFFER is the buffer where we started buffer selection.
ALL-BUFFERS is the list of buffer appearing in Buffer Selection Menu."
  (int-to-string (buffer-size)))

(defun bs--get-name (start-buffer all-buffers)
  "Return name of current buffer for Buffer Selection Menu.
The name of current buffer gets additional text properties
for mouse highlighting.
START-BUFFER is the buffer where we started buffer selection.
ALL-BUFFERS is the list of buffer appearing in Buffer Selection Menu."
  (let ((name (copy-sequence (buffer-name))))
    (put-text-property 0 (length name) 'mouse-face 'highlight name)
    (if (< (length name) bs--name-entry-length)
	(concat name
		(make-string (- bs--name-entry-length (length name)) ? ))
      name)))
		

(defun bs--get-mode-name (start-buffer all-buffers)
  "Return the name of mode of current buffer for Buffer Selection Menu.
START-BUFFER is the buffer where we started buffer selection.
ALL-BUFFERS is the list of buffer appearing in Buffer Selection Menu."
  mode-name)

(defun bs--get-file-name (start-buffer all-buffers)
  "Return string for column 'File' in Buffer Selection Menu.
This is the variable `buffer-file-name' of current buffer.
If current mode is `dired-mode' or shell-mode it returns the
default directory.
START-BUFFER is the buffer where we started buffer selection.
ALL-BUFFERS is the list of buffer appearing in Buffer Selection Menu."
  (let ((string (copy-sequence (if (member major-mode
					   '(shell-mode dired-mode))
				   default-directory
				 (or buffer-file-name "")))))
    (put-text-property 0 (length string) 'mouse-face 'highlight string)
    string))


(defun bs--insert-one-entry (buffer)
  "Generate one entry for buffer BUFFER in Buffer Selection Menu.
It goes over all columns described in `bs-attributes-list'
and evaluates corresponding string.  Inserts string in current buffer;
normally *buffer-selection*."
  (let ((string "")
	(columns bs-attributes-list)
	(to-much 0)
        (apply-args (append (list bs--buffer-coming-from bs-current-list))))
    (save-excursion
      (while columns
	(set-buffer buffer)
	(let ((min   (bs--get-value (nth 1 (car columns))))
	      ;;(max   (bs--get-value (nth 2 (car columns)))) refered no more
	      (align (nth 3 (car columns)))
	      (fun   (nth 4 (car columns)))
	      (val   nil)
	      new-string)
	  (setq val (bs--get-value fun apply-args))
	  (setq new-string (bs--format-aux val align (- min to-much)))
	  (setq string (concat string new-string))
	  (if (> (length new-string) min)
	      (setq to-much (- (length new-string) min)))
	  ) ; let
	(setq columns (cdr columns))))
    (insert string)
    string))

(defun bs--format-aux (string align len)
  "Generate a string with STRING with alignment ALIGN and length LEN.
ALIGN is one of the symbols `left', `middle', or `right'."
  (let ((length (length string)))
    (if (>= length len)
	string
      (if (eq 'right align)
	  (concat (make-string (- len length) ? ) string)
	(concat string (make-string (- len length) ? ))))))
		  
(defun bs--show-header ()
  "Insert header for Buffer Selection Menu in current buffer."
  (mapcar '(lambda (string)
	     (insert string "\n"))
	  (bs--create-header)))

(defun bs--get-name-length ()
  "Return value of `bs--name-entry-length'."
  bs--name-entry-length)

(defun bs--create-header ()
  "Return all header lines used in Buffer Selection Menu as a list of strings."
  (list (mapconcat (lambda (column)
		     (bs--format-aux (bs--get-value (car column))
				     (nth 3 column) ; align
				     (bs--get-value (nth 1 column))))
		   bs-attributes-list
		   "")
	(mapconcat (lambda (column)
		     (let ((length (length (bs--get-value (car column)))))
		       (bs--format-aux (make-string length ?-)
				       (nth 3 column) ; align
				       (bs--get-value (nth 1 column)))))
		   bs-attributes-list
		   "")))

(defun bs--show-with-configuration (name &optional arg)
  "Display buffer list of configuration with NAME name.
Set configuration NAME and determine window for Buffer Selection Menu.
Unless current buffer is buffer *buffer-selection* we have to save
the buffer we started Buffer Selection Menu and the current window
configuration to restore buffer and window configuration after a
selection.  If there is already a window displaying *buffer-selection*
select this window for Buffer Selection Menu.  Otherwise open a new
window.
The optional argument ARG is the prefix argument when calling a function
for buffer selection."
  (bs-set-configuration name)
  (let ((bs--show-all (or bs--show-all arg)))
  (unless (string= "*buffer-selection*" (buffer-name))
      ;; Only when not in buffer *buffer-selection*
      ;; we have to set the buffer we started the command
      (progn
	(setq bs--buffer-coming-from (current-buffer))
	(setq bs--window-config-coming-from (current-window-configuration))))
  (let ((liste (bs-buffer-list))
	(active-window (bs--window-for-buffer "*buffer-selection*")))
    (if active-window
	(select-window active-window)
      (if (> (window-height (selected-window)) 7)
	  (progn
	    (split-window-vertically)
	    (other-window 1))))
    (bs-show-in-buffer liste)
    (bs-message-without-log "%s" (bs--current-config-message)))))

(defun bs--configuration-name-for-prefix-arg (prefix-arg)
  "Convert prefix argument PREFIX-ARG to a name of a buffer configuration.
If PREFIX-ARG is nil return `bs-default-configuration'.
If PREFIX-ARG is an integer return PREFIX-ARG element of `bs-configurations'.
Otherwise return `bs-alternative-configuration'."
  (cond ;; usually activation
        ((null prefix-arg)
	 bs-default-configuration)
	;; call with integer as prefix argument
	((integerp prefix-arg)
	 (if (and (< 0 prefix-arg) (<= prefix-arg (length bs-configurations)))
	     (car (nth (1- prefix-arg) bs-configurations))
	   bs-default-configuration))
	;; call by prefix argument C-u
	(t bs-alternative-configuration)))
  
;; ----------------------------------------------------------------------
;; Main function bs-customize and bs-show
;; ----------------------------------------------------------------------

;;;###autoload
(defun bs-customize ()
  "Customization of group bs for Buffer Selection Menu."
  (interactive)
  (customize-group "bs"))

;;;###autoload
(defun bs-show (arg)
  "Make a menu of buffers so you can manipulate buffer list or buffers itself.
\\<bs-mode-map>
There are many key commands similar to `Buffer-menu-mode' for
manipulating buffer list and buffers itself.
User can move with [up] or [down], select a buffer
by \\[bs-select] or [SPC]\n
Type \\[bs-kill] to leave Buffer Selection Menu without a selection.
Type \\[bs-help] after invocation to get help on commands available.
With prefix argument ARG show a different buffer list.  Function
`bs--configuration-name-for-prefix-arg' determine accordingly
name of buffer configuration."
  (interactive "P")
  (setq bs--marked-buffers nil)
  (bs--show-with-configuration (bs--configuration-name-for-prefix-arg arg)))

;;; Now provide feature bs
(provide 'bs)

;;; bs.el ends here;;
;; Craig McPheeters
;; cmcpheeters@aw.sgi.com
;;
;; March, 1999.
;;
;; ctags_p4.el
;; 
;; ----------------------------------------------------------------------

(provide 'ctags_p4)

;;
;; Declare variables
;;

(defvar ctag-vi-compatability t 
  "*Were the tags generated to be compatable with VI?  VI has a limit of 30
characters for each tag.")

(defvar ctag-case-fold-search nil
  "*Should tag searches be case insensitive?")

(defvar ctag-file-names nil
   "List of tag files to search")

(defvar ctag-last-file nil
   "List of tag files remaining to search")

(defvar ctag-last-point nil
  "Point of the last successful search")

(defvar ctag-last nil
  "Last tag pattern searched for")

(defvar ctag-apropos-last nil
  "Last pattern searched for by a tag-apropos")

(defvar ctag-method-last nil
  "Last pattern searched for by a tag-methods")

(defvar ctag-class-last nil
  "Last pattern searched for by a tag-class")

(defvar ctag-ag-oneliner-last nil
  "Last pattern searched for by a ctag-ag-oneliner")

(defvar ctag-recurse nil
  "If non-nil, enter a recursive whenever a tag search completes successfully.")

(defvar ctag-next-form nil
  "Form to eval for the ctag-next-search command")

(defvar ctag-visit-file-name nil
  "File name to visit.  Set by the ctag-visit-file command.")

(defvar ctag-visited-files nil
  "List of files already visited by this instance of the ctag-visit-file cmd")

(defvar ctag-mtags-reuse-window t
  "If you tag from a buffer named 'mtags.mt' reuse the window")

(defvar ctag-tag-buffer nil)

;;
;; Utility functions
;;

(defun ctag-visit-tag-file-buffer (name) "\
Select the named tag buffer.  If the tag file has not been read in then read
the tag file.  If the file has been changed, then offer to re-read it."
  (let ((cur-dir default-directory) buf)
	(cond (name)
		  (ctag-file-names (setq name (car ctag-file-names)))
		  (t (call-interactively 'ctag-file-add) 
			 (setq name (car ctag-file-names))))
	(set-buffer 
	 (or (find-buffer-visiting name)
		 (progn
		   (message "Reading tags file %s..." name)
		   (setq buf (find-file-noselect name))
		   buf)))
	(setq case-fold-search ctag-case-fold-search)
	(or (verify-visited-file-modtime (find-buffer-visiting name))
		(cond ((yes-or-no-p 
				(concat "Tag file " name" has changed, read new contents? "))
			   (revert-buffer t t))))
	(setq default-directory cur-dir)
	(message "")))

(defun skip-white-space () "\
Skip forward in buffer over the white-space"
  (while (looking-at "\\s-")
    (forward-char 1)))

(defun skip-to-white-space () "\
Skip forward in buffer to the first white-space"
  (while (not (looking-at "\\s-"))
    (forward-char 1)))

(defun string-in-list (list str) "\
Given the LIST, return t if the STRING is in it"
  (let (success)
	(while list
	  (if (string-equal (car list) str)
		  (progn 
			(setq list nil)
			(setq success t))
		(setq list (cdr list))))
	success))

(defun ctag-get-tag () "\
Return the tag string of the current line.  Assumes the tag table is the 
current buffer"
  (beginning-of-line)
  (buffer-substring (point)
					(progn (skip-to-white-space) (point))))

(defun ctag-get-filename () "\
Return the file name of the tag on the line point is at.
Assumes the tag table is the current buffer"
  (beginning-of-line)
  (skip-to-white-space)
  (skip-white-space)
  (buffer-substring (point)
					(progn (skip-to-white-space) (point))))

(defun ctag-get-line () "\
Get the line number from the current tag line."
  (let (pat)
	(beginning-of-line)
	(skip-to-white-space)
	(skip-white-space)
	(skip-to-white-space)
	(skip-white-space)
	(setq pat (buffer-substring (point) 
								(progn (end-of-line) (point))))
	pat))

(defun ctag-default-tag (cpp) "\
Return a default tag string based upon the text surrounding point in the 
current buffer"
  (let ((valid-chars "\\sw\\|\\s_")
		start end)
	(if cpp
		(setq valid-chars (concat valid-chars "\\|:\\|~")))
	(save-excursion
	  (while (looking-at valid-chars) (forward-char))
	  (if (re-search-backward valid-chars nil t)
		  (progn
			;; Don't accept Cpp tags of form   'word:' only 'word::'
			(if (looking-at ":")
				(progn
				  (forward-char -1)
				  (if (looking-at ":")
					  (forward-char 1))))
			(forward-char 1)
			(setq end (point))
			(forward-char -1)
			(while (looking-at valid-chars) (forward-char -1))
			(forward-char 1)
			(setq start (point))
			(buffer-substring start end)
			)
		nil))))

(defun ctag-query-tag (prompt tag) "\
Read a tag to search for after querying the user"
  (let ((default tag))
	(setq tag (read-string
			   (if default
				   (format "%s(default %s) " prompt default)
				 prompt)))
	(if (string-equal tag "")
		default
	  tag)))

(defun ctag-query-file (prompt) "\
Read a file name to search for after querying the user"
  (let (default name (pat "[a-zA-Z0-9._]"))
	(save-excursion
	  (re-search-backward pat nil t)
	  (while (looking-at pat) (forward-char))
	  (setq default 
			(buffer-substring 
			 (point)
			 (progn
			   (if (> (point) 1) (backward-char 1))
			   (while 
				   (and (> (point) 1)
						(looking-at pat))
				 (backward-char 1))
			   (forward-char 1)
			   (point)))))
	(if (and (> (length default) 2)
			 (string-equal (substring default -2 nil) ".o"))
		(setq default (concat (substring default 0 -2) ".c")))
	(if (and (> (length default) 4)
			 (string-equal (substring default -4 nil) ".obj"))
		(setq default (concat (substring default 0 -4) ".c")))
	(if (eq window-system 'w32)
		(setq default (concat "\\" default))
	  (setq default (concat "/" default)))
	
	(setq name (read-string prompt default))
	(if (string-equal name "")
		default
	  name)))

(defun ctag-truncate-tag (tag) "\
If ctag-vi-compatibility, truncate tags to 30 characters"
  (if ctag-vi-compatability
	  (if (> (length tag) 30)
		  (substring tag 0 30)
		tag)
	tag))

(defun ctag-file-add (file) "\
Add a tag file to the start of the list of tag files to search."
  (interactive (list (read-file-name "Visit ctag table: (default tags) "
									 default-directory
									 (concat default-directory "tags")
									 t)))
  (setq file (expand-file-name file))
  (if (file-directory-p file)
      (setq file (concat file "tags")))
  (if (file-readable-p file)
	  (setq ctag-file-names (append (list file) ctag-file-names))
	(message "Can't read the tag file: %s" file)))

(defun ctag-file-remove () "\
Remove the last tag file to be added."
  (interactive)
  (if (listp ctag-file-names)
	  (setq ctag-file-names (cdr ctag-file-names)))
  (ctag-show-files))

(defun ctag-show-files () "\
Display the tag file names searched"
  (interactive)
  (message "%s" ctag-file-names))


;;
;;
;; Main tag functions
;;
;;

(defun ctag (tag) "\
Find TAG, starting at the beginning of the tag file list.  If tagname
is nil, the expression in the buffer at or around point is used as the 
tagname"
  (interactive (list (ctag-query-tag "Find tag: " (ctag-default-tag t))))
  (ctag-main tag))

(defun ctag-at-point () "\
Find the tag under point"
  (interactive)
  (ctag-main (ctag-default-tag t)))

(defun ctag-main (tag) "\
Search for the tag given"
  (if (not ctag-file-names)
	  (call-interactively 'ctag-file-add))
  (if (and tag ctag-file-names)
	  (progn
		(setq tag (ctag-truncate-tag tag))
		(setq ctag-last-file ctag-file-names)
		(setq ctag-last-point 1)
		(setq ctag-last tag)
		(setq ctag-next-form '(ctag-find-next-tag 1))
		(setq ctag-recurse t)
		(setq ctag-tag-buffer (current-buffer))
		(ctag-find-next-tag 1))
	(message "Empty tagname or tagfile list")))

(defun ctag-find-next-tag (n) "\
Find the N'th next occurence of a tag found by ctag.  
Handle recursion if needed"
  (interactive "p")
  (let 
	  ((window-config (ctag-window-list))
	   (found-one nil))
	(if ctag-recurse
		(progn
		  (save-window-excursion
			(setq found-one (ctag-find-next n))
			(if found-one
				(recursive-edit)))
		  (if found-one
			  (ctag-window-restore window-config)))
	  (setq found-one (ctag-find-next n)))
	(if (not found-one)
		(message "Not found, tagname: %s" ctag-last))))

(defun ctag-find-next (n) "\
Find the N'th next occurence of a tag found by ctag.
Return nil if not found, otherwise do the search"
  (interactive "p");; The arg is an integer.
  (if (and
	   ctag-last-file
	   (> n 0))
	  (progn
		(ctag-visit-tag-file-buffer (car ctag-last-file))
		(goto-char ctag-last-point)
		(if (re-search-forward (concat "^" ctag-last) nil t)
			(progn
			  (setq ctag-last-point (point))
			  (setq n (1- n))
			  (if (= n 0)
				  (ctag-do-search)
				(ctag-find-next n)))
		  (progn
			(setq ctag-last-file (cdr ctag-last-file))
			(setq ctag-last-point 1)
			(ctag-find-next n))))))


;;
;; Support routines to ctag
;;

(defun ctag-do-search () "\
Assume point is on a tag line.  Extract the file and line and then go there.
If the search is successful, enter a recursive edit if ctag-recurse is non-nil"
  (let ((filename (ctag-get-filename))
		(linepat (ctag-get-line))
		(case case-fold-search)
		found-point linenum found-it)
	(setq linenum (string-to-int linepat))
	(if (and ctag-mtags-reuse-window
			 ctag-tag-buffer
			 (posix-string-match ".mt$" (buffer-name ctag-tag-buffer)))
		(find-file (substitute-in-file-name filename))
	  (find-file-other-window (substitute-in-file-name filename)))
	(widen)
	(goto-char (point-min))
	(if (= linenum 0)
		(progn
		  (setq case-fold-search nil)
		  (setq found-it (re-search-forward linepat nil t))
		  (setq case-fold-search case))
	  (progn
		(goto-line linenum)
		(setq found-it t)))
	(setq found-point (point))
	(if found-it
		(beginning-of-line))
	found-it))

(defun ctag-next-search () "\
Do another tag search, just like the last one"
  (interactive)
  (if ctag-next-form
	  (progn
		(if current-prefix-arg
			(setq ctag-recurse t)
		  (setq ctag-recurse nil))
		(eval ctag-next-form))
	(message "No next to search for.")))

(defun ctag-apropos (string) "\
Display list of all tags in tag tables that REGEXP match.
By default it only prints out the tag itself, given an argument it will
display the whole tag entry.  The whole entry includes the filename and line
number or pattern used in the tag search"
  (interactive (list
				(let ((pattern (read-from-minibuffer 
								(concat (if current-prefix-arg
											"Full listing tag "
										  "Tag ")
										"apropos (regexp): ")
								ctag-apropos-last)))
				  (setq ctag-apropos-last pattern))))
  (let ((file-list ctag-file-names)
		(match 0)
		end)
	(save-window-excursion
	  (with-output-to-temp-buffer "*Tags List*"
		(princ "Tags matching regexp ")
		(prin1 string)
		(terpri)
		(while file-list
		  (princ (format "------  In file: %s\n" (car file-list)))
		  (ctag-visit-tag-file-buffer (car file-list))
		  (goto-char 1)
		  (while (re-search-forward string nil t)
			(save-excursion
			  (beginning-of-line)
			  (skip-to-white-space)
			  (setq end (point))
			  (beginning-of-line)
			  (if (re-search-forward string end t)
				  (progn
					(setq match (1+ match))
					(beginning-of-line)
					(princ (buffer-substring 
							(point)
							(progn
							  (if current-prefix-arg
								  (progn
									(end-of-line)
									(point))
								end))))
					(terpri))))
			(forward-line 1))
		  (setq file-list (cdr file-list))))

	  ;; Decide whether or not to display the tag buffer.  Don't if there were 
	  ;; no matches found, otherwise do.
	  (if (> match 0)
		  (progn
			(display-buffer "*Tags List*")
			(message "Found %d matching tag%s" match (if (> match 1) "s" ""))
			(recursive-edit))
		(message "No tags match that pattern: %s" string)))))

(defun ctag-method-apropos (string) "\
Display a list of all C++ methods in tag tables that contain the PATTERN"
  (interactive 
   (list
	(let ((pattern (read-from-minibuffer "Method contains pattern: "
										 (ctag-default-tag nil))))
				  (setq ctag-method-last pattern))))
  (let ((last-ctag-apropos ctag-apropos-last))
	(ctag-apropos (concat "::.*" string))
	(setq ctag-apropos-last last-ctag-apropos)))

(defun ctag-method-list (name) "\
Display a list of all C++ methods in tag tables that belong to CLASS"
  (interactive 
   (list
	(let ((pattern (read-from-minibuffer "Class name : "
										 (ctag-default-tag nil))))
				  (setq ctag-class-last pattern))))
  (let ((last-ctag-apropos ctag-apropos-last))
	(ctag-apropos (concat name "::"))
	(setq ctag-apropos-last last-ctag-apropos)))

(defun ctag-visit-file (name) "\
Visit the files in the tag file which match NAME.  The next occurence of a 
file matching name can be found with the ctag-next-search command"
  (interactive (list (ctag-query-file "Visit tag file: ")))
  (setq ctag-last-file ctag-file-names)
  (setq ctag-last-point 1)
  (setq ctag-visit-file-name name)
  (setq ctag-visited-files nil)
  (setq ctag-next-form '(ctag-visit-next-file 1))
  (setq ctag-recurse t)
  (ctag-visit-next-file 1))

(defun ctag-visit-next-file (n) "\
Find the N'th next occurence of a tag file matching the previously set name.
Use ctag-visit-file to set the tag file name to match"
  (interactive "p")
  (let (start end file found)
	(while (and 
			(not found) 
			ctag-last-file 
			(> n 0))
	  (ctag-visit-tag-file-buffer (car ctag-last-file))
	  (goto-char ctag-last-point)
	  ;; Search for a match
	  (if (search-forward ctag-visit-file-name nil t)
		  (save-excursion
			(end-of-line)
			(setq ctag-last-point (point))
			(beginning-of-line)
			(skip-to-white-space)
			(save-excursion
			  (skip-white-space)
			  (skip-to-white-space)
			  (setq end (point)))
			;; Restrict this search to just the filename portion of tag
			(if (search-forward ctag-visit-file-name end t)
				(progn
				  ;; Got a file match
				  (setq file (ctag-get-filename))
				  (if (not (string-in-list ctag-visited-files file))
					  (progn
						;; First time we've visited this file
						(setq found t)
						(setq ctag-visited-files 
							  (append ctag-visited-files
									  (list file)))
						;; We may recurse here...
						(ctag-visit-file-doit file))))))
		(progn
		  ;; No match in this file, try the next file in list
		  (setq ctag-last-file (cdr ctag-last-file))
		  (setq ctag-last-point 1))))
	(if (not found)
		(message "File not found: %s" ctag-visit-file-name))))

(defun ctag-visit-file-doit (name) "\
Visit the file found in a ctag-visit-next-file call"
  (if ctag-recurse
	  (save-window-excursion
		(find-file-other-window (substitute-in-file-name name))
		(message "Visiting file: %s" name)
		(recursive-edit))
	(progn
	  (find-file-other-window (substitute-in-file-name name))
	  (message "Visiting file: %s" name))))

(defun ctag-window-list () "\
Returns a list of Lisp window objects and the point at the top of the window
for all Emacs windows"
  (let* ((first-window (selected-window))
		 (top-char (save-excursion
					 (move-to-window-line 0)
					 (point)))
		 (windows (cons 
				   (cons first-window 
						 (cons top-char 
							   (cons (point) nil))) nil))
		 (current-cons windows)
		 (w (next-window first-window nil)))
    (while (not (eq w first-window))
	  (select-window w)
      (setq current-cons 
			(setcdr current-cons
					(cons 
					 (cons w
						   (cons (save-excursion
								   (move-to-window-line 0)
								   (point))
								 (cons (point) nil))) nil)))
      (setq w (next-window w nil)))
	(select-window first-window)
    windows))

(defun ctag-window-restore (windows) "\
Restore the positioning of the windows from the window LIST
Used in conjunction with ctag-window-list"
  (let* ((this (car windows))
		 (rest (cdr windows))
		 (top-char nil)
		 (window-from-list nil)
		 (first-window (selected-window))
		 the-point)
	(while this
	  (setq window-from-list (car this))
	  (setq top-char (car (cdr this)))
	  (setq the-point (car (cdr (cdr this))))
	  (select-window window-from-list)
	  (set-window-start nil top-char)
	  (goto-char the-point)
	  (setq this (car rest))
	  (setq rest (cdr rest)))
	(select-window first-window)))

;;
;; Perforce commands
;;

(defun p4-run-command (fresh command)
  (let (p4buf min)
	(setq p4buf (get-buffer-create "*Perforce*"))
	(pop-to-buffer p4buf)
	(if fresh
		(erase-buffer)
	  (goto-char (point-max)))
	(setq min (point))
	(insert "% p4 ")
	(mapcar '(lambda (a) (insert a)(insert " ")) command)
	(insert "\n---\n\n")
	(apply 'call-process "p4" nil t nil command)
	(goto-char 0)
	(set-buffer-modified-p nil)
	min))
  
(defun p4-command (show &rest command) "\
Runs the COMMAND in the buffer *Perforce*"
  (let (curbuf p4buf min)
	(if show
		(progn
		  (setq curbuf (current-buffer))
		  (setq min (p4-run-command t command))
		  (pop-to-buffer curbuf))
	  (save-window-excursion
		(setq curbuf (current-buffer))
		(setq min (p4-run-command t command))
		(pop-to-buffer curbuf)))
	min))

(defun p4-get-line (start wholeLine pattern)
  (let (curbuf newbuf)
	(save-window-excursion
	  (setq curbuf (current-buffer))
	  (setq newbuf (get-buffer-create "*Perforce*"))
	  (pop-to-buffer newbuf)
	  (if start (goto-char 0))
	  (if (search-forward pattern nil t)
		  (buffer-substring (if wholeLine
								(progn
								  (beginning-of-line)
								  (point))
							  (point))
							(progn (end-of-line) (point)))
		nil))))

(defun ctag-p4-is-file-locked () "\
Display information on the file associated with the current buffer"
  (interactive)
  (let ((file-name (buffer-file-name))
		person lock action msg)
	(p4-command nil "fstat" file-name)
	(setq msg (concat " (" file-name ")"))
	(if (p4-get-line t nil "... clientFile ")
		(progn
		  (if (setq lock (p4-get-line t nil "... ourLock "))
			  (setq msg (concat " File is locked by " (user-login-name) ". " 
								msg))
			(if (setq lock (p4-get-line t nil "... otherLock "))
				(setq msg (concat " File is locked by " lock ". " msg))
			  (setq msg (concat " File is not locked. " msg))))
		  (if (setq action (p4-get-line t nil "... action "))
			  (setq msg (concat "Open for " action ". " msg))))
	  (setq msg (concat "File is not in the repo. " msg)))
	(message msg)))

(defun ctag-p4-diff (version) "\
Display differences between current buffer and its repo file."
  (interactive "sVersion (#n, @change, @client, @label): ")
  (let ((file-name (buffer-file-name))
		(window-config (ctag-window-list)))
	(save-window-excursion
	  (if version
		  (setq file-name (concat file-name version)))
	  (p4-command t "diff" "-f" file-name)
	  (recursive-edit))
	(ctag-window-restore window-config)))

(defun ctag-p4-get (version) "\
Get a specific version of the current file."
  (interactive "sVersion (#n, @change, @client, @label): ")
  (let (file-name buf-name buf
		(window-config (ctag-window-list)))
	(save-window-excursion
	  (if version
		  (progn
			(setq file-name (concat (buffer-file-name) version))
			(setq buf-name (concat (buffer-name) version))
			(setq buf (get-buffer-create buf-name))
			(pop-to-buffer buf)
			(erase-buffer)
			(goto-char (point-max))
			(call-process "p4" nil t nil "print" file-name)
			(goto-char 0)
			(set-buffer-modified-p nil)
			(recursive-edit))))
	(ctag-window-restore window-config)))

(defun ctag-p4-filelog () "\
Get the RCS log from the repo file associated with buffer."
  (interactive)
  (let ((file-name (buffer-file-name))
		(window-config (ctag-window-list)))
	(save-window-excursion
	  (p4-command t "filelog" "-i" "-l" file-name)
	  (recursive-edit))
	(ctag-window-restore window-config)))

(defun ctag-p4-edit () "\
Mark the file for edit."
  (interactive)
  (let ((file-name (buffer-file-name))
		(window-config (ctag-window-list))
		line)
	(save-window-excursion
	  (p4-command t "edit" file-name)
	  (if (setq line (p4-get-line t t "- opened for edit"))
		  (progn
			(setq buffer-read-only nil)
			(message line))
		(recursive-edit)))
	(ctag-window-restore window-config)))

(defun ctag-p4-add () "\
Add the current file to the repo."
  (interactive)
  (let ((file-name (buffer-file-name))
		(window-config (ctag-window-list))
		line)
	(save-window-excursion
	  (p4-command t "add" file-name)
	  (if (setq line (p4-get-line t t "- opened for add"))
		  (message line)
		(recursive-edit)))
	(ctag-window-restore window-config)))

(defun ctag-p4-revert () "\
Revert the current file to the repo version."
  (interactive)
  (let ((file-name (buffer-file-name))
		(window-config (ctag-window-list))
		line)
	(if (yes-or-no-p (concat "Revert " file-name ": "))
		(save-window-excursion
		  (p4-command t "revert" file-name)
		  (if (setq line (p4-get-line t t "reverted"))
			  (progn
				(find-file-read-only file-name)
				(message line))
			(recursive-edit)))
	  (ctag-window-restore window-config))))

(defun ctag-p4-lock () "\
Lock the current file."
  (interactive)
  (let ((file-name (buffer-file-name))
		(window-config (ctag-window-list))
		line)
	(save-window-excursion
	  (p4-command t "lock" file-name)
	  (if (setq line (p4-get-line t t "- locking"))
		  (message line)
		(recursive-edit)))
	(ctag-window-restore window-config)))

(defun ctag-p4-unlock () "\
Lock the current file."
  (interactive)
  (let ((file-name (buffer-file-name))
		(window-config (ctag-window-list))
		line)
	(save-window-excursion
	  (p4-command t "unlock" file-name)
	  (if (setq line (p4-get-line t t "- unlocking"))
		  (message line)
		(recursive-edit)))
	(ctag-window-restore window-config)))

(defun ctag-p4-opened () "\
Return the list of opened files."
  (interactive)
  (let ((window-config (ctag-window-list)))
	(save-window-excursion
	  (p4-command t "opened")
	  (recursive-edit))
	(ctag-window-restore window-config)))


;;
;; Wrappers to a couple of useful commands
;;

(defun ctag-softwhere (name) "\
Call the alias 'softwhere' program with the name"
  (interactive (list (read-string "Softwhere on which string: "
									(ctag-default-tag nil))))
  (let (curbuf newbuf)
	(save-window-excursion
	  (setq curbuf (current-buffer))
	  (setq newbuf (get-buffer-create "*Softwhere*"))
	  (pop-to-buffer newbuf)
	  (erase-buffer)
	  (message "Executing: softwhere %s" name)
	  (shell-command (concat
					  "softwhere "
					  name)
					 t)
	  (pop-to-buffer curbuf)
	  (recursive-edit))))

(defun ctag-ag-oneliner (string) "\
Find all occurences of the STRING in the AG one-liners file"
  (interactive (list
				(let ((pattern (read-from-minibuffer 
								"AG oneliner search for: "
								ctag-ag-oneliner-last)))
				  (setq ctag-ag-oneliner-last pattern))))
  (let ((location) newbuf)
	(save-window-excursion
	  (setq newbuf (get-buffer-create "*AG oneliners*"))
	  (if (getenv "AG-ONELINER")
		  (setq location (getenv "AG-ONELINER"))
		(setq location (concat (getenv "TAGSRC") "/AGv2.7/man/oneliner")))
	  (pop-to-buffer newbuf)
	  (erase-buffer)
	  (insert "Oneliner file: " location "\n\n")
	  (insert "AG oneliner search for: " ctag-ag-oneliner-last "\n\n")
	  (message (concat "Searching for: " ctag-ag-oneliner-last))
	  (call-process-region (point-min)
						   (point-max)
						   "/bin/sh" nil newbuf t
						   "-c" (concat "grep" " -i"
										" " ctag-ag-oneliner-last
										" " location))
	  (message "")
	  (goto-char 0)
	  (recursive-edit))))


;; Make a keymap for the tag stuff

(setq ctag-map (make-sparse-keymap))
(define-key	ctag-map 	"." 	'ctag)
(define-key	ctag-map 	"?" 	'ctag-apropos)
(define-key	ctag-map 	":" 	'ctag-method-apropos)
(define-key ctag-map    "+"     'ctag-file-add)
(define-key ctag-map    "-"     'ctag-file-remove)
(define-key ctag-map    "="     'ctag-show-files)
(define-key ctag-map    "m"     'ctag-method-list)
(define-key ctag-map    "v"     'ctag-visit-file)
(define-key ctag-map 	"w"		'ctag-softwhere)
(define-key ctag-map    "A"     'ctag-ag-oneliner)
(define-key ctag-map    "\^a"   'ctag-ag-oneliner)
(define-key ctag-map    "\^n"   'ctag-next-search)
(define-key ctag-map    "\^v"   'ctag-visit-file)
(define-key	ctag-map 	"\^w" 	'ctag-softwhere)


;; Perforce commands

(define-key ctag-map    "a"     'ctag-p4-add)
(define-key ctag-map    "d"     'ctag-p4-diff)
(define-key ctag-map    "e"     'ctag-p4-edit)
(define-key ctag-map    "f"     'ctag-p4-filelog)
(define-key ctag-map    "g"     'ctag-p4-get)
(define-key ctag-map    "i"     'ctag-p4-is-file-locked)
(define-key ctag-map    "l"     'ctag-p4-lock)
(define-key ctag-map    "o"     'ctag-p4-opened)
(define-key ctag-map    "r"     'ctag-p4-revert)
(define-key ctag-map    "u"     'ctag-p4-unlock)


;;
;; The old ctag package used to keep the default directory at the directory
;; you started emacs from.  This supported the local-work space approach
;; that we were using them.  When you have a source tree to work with, you
;; want the default directory to be where each file is.  However, you don't
;; want to worry about having to 'cd' back to the home to compile.  To get
;; around this, I override the compile command to reset the default directory
;; to a variable which is maintained by a local version of 'cd'  This will
;; allow you to tag around the repo, check a file out, edit it, write it
;; in place and then compile and have the compile pick it up.
;;
;; Craig, Mar 1999.
;;

(setq ctag-default-directory default-directory)
(defun cd (dir)
  (interactive "DChange directory: ")
  (setq default-directory dir)
  (setq ctag-default-directory dir))

(defun ctag-compile ()
  (interactive)
  (let ((old-dir default-directory))
	(setq default-directory ctag-default-directory)
	(call-interactively 'compile)
	(setq default-directory old-dir)))

(substitute-key-definition 'compile 'ctag-compile global-map)
(substitute-key-definition 'compile 'ctag-compile ctl-x-map)

(message "Loaded ctags_p4...")


;; Do this to put the map where you want it:
;;(define-key global-map 	"\^t" ctag-map)
;;; ibs.el --- windows like buffer selection mode by C-TAB.

;; Copyright (C) 2000, 2001, 2002, 2003 Olaf Sylvester

;; Author: Olaf Sylvester <olaf@geekware.de>
;; Maintainer: Olaf Sylvester <olaf@geekware.de>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Commentary:
;; This Emacs package provides a minor mode for buffer cycling;
;; more exact: to switch by key C-TAB between Emacs buffers like
;; MS-Windows IDEs.
;; C-TAB starts buffer cycling.  Every other key terminates cycling
;; and sets the current buffer at the top of Emacs buffer list.
;; The buffer we started buffer cycling won't be buried !!!

;; You can configure the keys for cycling.
;; Therefore set global `ibs-cycling-key' before loading ibs.el.

;; You can define which buffers will be used for buffer cycling.
;; Set global `ibs-cycle-buffer-function' to a function which
;; returns a buffer list. The default is buffer-list, which returns
;; all buffers in recently order.
;; If package bs is loaded the cycling list of this package
;; will be used.

;;; History:
;; 28.02.2001
;; Version 0.12
;; - delete references to ibs-other-meta-char 
;; - solved problems with meta key sequences
;; 
;; 22.12.2000
;; Version 0.11
;; - problems with generic-character-list (XEMACS 21.x)
;; - no more occurrence of eval-when
;; 
;; 17.12.2000
;; First Version 0.1
;;; Code:

;;; Global variables for customization.
(defvar ibs-cycling-key "<C-tab>"
  "Key sequence for buffer cycling.")

(defvar ibs-cycle-buffer-function nil
  "Function to calculate buffers for cycling.
When nil use `buffer-list'.
The function needs no arguments and must return a list of buffers.")

(defvar ibs-timeout 4
  "Seconds of inactivity for deactivating cycling mode.")

(defvar ibs-mode-hook nil
  "Function(s) to call after invoking mode ibs.")

(defvar ibs-mode-end-hook nil
  "Function(s) to call after terminating mode ibs.")

(defvar ibs-buffer-list nil
  "Current buffer list for cycling.")

(defvar ibs-start-buffer nil
  "Buffer we started cycling.")

;;; Define ibs-mode keymap.

(defvar ibs-mode-map nil
  "Keymap for function `ibs-mode'.
Derived from `isearch-mode-map'.")
;;(setq ibs-mode-map nil)

(or ibs-mode-map
    (let* ((i 0)
	   (map (make-keymap)))
      ;; Make characters stop cycling.
      (if (fboundp 'generic-character-list)
	  (let ((l (generic-character-list))
		(table (nth 1 map)))
	    (while l
	      (set-char-table-default table
				      (car l)
				      'ibs-select-buffer-and-quit)
	      (setq l (cdr l)))))
      ;; Make function keys, etc, stop cycling.
      (define-key map [t] 'ibs-select-buffer-and-quit)
      ;; Control chars, by default, end ibs mode transparently.
      ;; We need these explicit definitions because, in a dense
      ;; keymap, the binding for t does not affect characters.
      ;; We use a dense keymap to save space.
      (while (< i ?\ )
	(define-key map
	            (make-string 1 i)
		    'ibs-select-buffer-and-quit)
	(setq i (1+ i)))

      ;; Single-byte printing chars stop cycling.
      (setq i ?\ )
      (while (< i 256)
	(define-key map (vector i) 'ibs-select-buffer-and-quit)
	(setq i (1+ i)))

      ;; Several non-printing chars change the searching behavior.
      (define-key map "\C-g" 'ibs-abort)
      ;; This assumes \e is the meta-prefix-char.
      (or (= ?\e meta-prefix-char)
	  (error "Inconsistency in ibs.el"))
      
      (define-key map
	          (read-kbd-macro ibs-cycling-key)
		  'ibs-next-buffer)

      (let ((meta-map (make-sparse-keymap)))
	(define-key map (char-to-string meta-prefix-char) meta-map)
	(define-key map [escape] meta-map))
      (define-key map (vector meta-prefix-char t) 'ibs-other-meta-char)

      ;; Pass frame events transparently so they won't exit
      ;; the search. In particular, if we have more than one
      ;; display open, then a switch-frame might be generated
      ;; by someone typing at another keyboard.
      (define-key map [switch-frame]       nil)
      (define-key map [delete-frame]       nil)
      (define-key map [iconify-frame]      nil)
      (define-key map [make-frame-visible] nil)
      
      (setq ibs-mode-map map)))

(defun ibs-other-meta-char ()
  "Exit the search normally and reread this key sequence.
But only if `search-exit-option' is non-nil, the default.
If it is the symbol `edit', the search string is edited in the minibuffer
and the meta character is unread so that it applies to editing the string."
  (interactive)
  (let* ((key (this-command-keys))
	 (main-event (aref key 0))
	 (keylist (listify-key-sequence key)))
    (cond ((and (= (length key) 1)
		(let ((lookup (lookup-key function-key-map key)))
		  (not (or (null lookup) (integerp lookup)
			   (keymapp lookup)))))
	   ;; Handle a function key that translates into something else.
	   ;; If the key has a global definition too,
	   ;; exit and unread the key itself, so its global definition runs.
	   ;; Otherwise, unread the translation,
	   ;; so that the translated key takes effect within isearch.
	   (cancel-kbd-macro-events)
	   (if (lookup-key global-map key)
	       (progn 
		 (ibs-done)
		 (apply 'ibs-unread keylist))
	     (apply 'ibs-unread
		    (listify-key-sequence (lookup-key function-key-map key)))))
	  (
	   ;; Handle an undefined shifted control character
	   ;; by downshifting it if that makes it defined.
	   ;; (As read-key-sequence would normally do,
	   ;; if we didn't have a default definition.)
	   (let ((mods (event-modifiers main-event)))
	     (and (integerp main-event)
		  (memq 'shift mods)
		  (memq 'control mods)
		  (lookup-key ibs-mode-map
			      (let ((copy (copy-sequence key)))
				(aset copy 0
				      (- main-event (- ?\C-\S-a ?\C-a)))
				copy)
			      nil)))
	   (setcar keylist (- main-event (- ?\C-\S-a ?\C-a)))
	   (cancel-kbd-macro-events)
	   (apply 'ibs-unread keylist))
	  (t
	   (let (window)
	     (cancel-kbd-macro-events)
	     (apply 'ibs-unread keylist)
	     ;; Properly handle scroll-bar and mode-line clicks
	     ;; for which a dummy prefix event was generated as (aref key 0).
	     (and (> (length key) 1)
		  (symbolp (aref key 0))
		  (listp (aref key 1))
		  (not (numberp (posn-point (event-start (aref key 1)))))
		  ;; Convert the event back into its raw form,
		  ;; with the dummy prefix implicit in the mouse event,
		  ;; so it will get split up once again.
		  (progn (setq unread-command-events
			       (cdr unread-command-events))
			 (setq main-event (car unread-command-events))
			 (setcar (cdr (event-start main-event))
				 (car (nth 1 (event-start main-event))))))
	     ;; If we got a mouse click, maybe it was read with the buffer
	     ;; it was clicked on.  If so, that buffer, not the current one,
	     ;; is in ibs mode.  So end the search in that buffer.
	     (if (and (listp main-event)
		      (setq window (posn-window (event-start main-event)))
		      (windowp window)
		      (or (> (minibuffer-depth) 0)
			  (not (window-minibuffer-p window))))
		 (save-excursion
		   (set-buffer (window-buffer window))
		   (ibs-done))
	       (ibs-done)))))))

;; The value of input-method-function when ibs is invoked.
(defvar ibs-input-method-function nil)

;; A flag to tell if input-method-function is locally bound when
;; ibs is invoked.
(defvar ibs-input-method-local-p nil)

;; Register minor mode
(or (assq 'ibs-mode minor-mode-alist)
    (nconc minor-mode-alist
	   (list '(ibs-mode ibs-mode))))

(defvar ibs-mode nil)

(define-key global-map (read-kbd-macro ibs-cycling-key) 'ibs-select)

(defun ibs-cancel ()
  "Terminate cycling and signal quit."
  (interactive)
  (ibs-done)
  (signal 'quit nil))

(defun ibs-abort ()
  "Terminate cycling and reselect starting buffer."
  (interactive)
  (ibs-done)
  (switch-to-buffer ibs-start-buffer t))

(defun ibs-select ()
  "Do buffer selection."
  (interactive)
  (setq ibs-start-buffer (current-buffer))
  (setq ibs-buffer-list (mapcar
			 'identity
			 (funcall (or ibs-cycle-buffer-function
				      (function buffer-list)))))
  (if (not (memq (current-buffer) ibs-buffer-list))
      (setq ibs-buffer-list (cons (current-buffer) ibs-buffer-list)))
  (setq ibs-buffer-list (ibs-step-right ibs-buffer-list))
  (ibs-mode)
  (ibs-next-buffer)
  )


(defun ibs-cancel-after-timeout ()
  "Wait `ibs-timeout' seconds for terminating cycling."
  (when (sit-for ibs-timeout)
    (ibs-done t)
    (message "")))

(defun ibs-mode ()
  "Start ibs minor mode.  
Called by `ibs-select', etc.
\\{ibs-mode-map}"
  ;; Initialize global vars.
  (setq ibs-input-method-function
	input-method-function)
  (setq ibs-input-method-local-p
	(local-variable-p 'input-method-function))

  ;; We must bypass input method while reading key.
  ;; When a user type printable character, appropriate input
  ;; method is turned on in minibuffer to read multibyte
  ;; charactes.
  (or ibs-input-method-local-p
      (make-local-variable 'input-method-function))
  (setq input-method-function nil)
  (setq	ibs-mode " I-BS")
  (force-mode-line-update)
  (setq overriding-terminal-local-map ibs-mode-map)
  (run-hooks 'ibs-mode-hook)
  (add-hook 'mouse-leave-buffer-hook 'ibs-done)
  t)

(defun ibs-done (&optional select-buffer-p)
  "Terminate ibs normally."
  (remove-hook 'mouse-leave-buffer-hook 'ibs-done)
  (setq overriding-terminal-local-map nil)
  (setq ibs-mode nil)
  (if ibs-input-method-local-p
      (setq input-method-function ibs-input-method-function)
    (kill-local-variable 'input-method-function))
  (if select-buffer-p
      (switch-to-buffer (car (last ibs-buffer-list))))
  (force-mode-line-update)
  (run-hooks 'ibs-mode-end-hook)
  t)

(defun ibs-select-buffer-and-quit ()
  "Exit the cycling normally and reread this key sequence."
  (interactive)
  (let* ((key (this-command-keys))
	 (main-event (aref key 0))
	 (keylist (listify-key-sequence key)))
    (cond ((and (= (length key) 1)
		(let ((lookup (lookup-key function-key-map key)))
		  (not (or (null lookup) (integerp lookup)
			   (keymapp lookup)))))
	   ;; Handle a function key that translates into something
	   ;; else. If the key has a global definition too,
	   ;; exit and unread the key itself, so its global
	   ;; definition runs. Otherwise, unread the translation,
	   ;; so that the translated key takes effect within ibs.
	   (cancel-kbd-macro-events)
	   (if (lookup-key global-map key)
	       (progn
		 (ibs-done t)
		 (apply 'ibs-unread keylist))
	     (apply 'ibs-unread
		    (listify-key-sequence
		     (lookup-key function-key-map
				 key)))))
	  (
	   ;; Handle an undefined shifted control character
	   ;; by downshifting it if that makes it defined.
	   ;; (As read-key-sequence would normally do,
	   ;; if we didn't have a default definition.)
	   (let ((mods (event-modifiers main-event)))
	     (and (integerp main-event)
		  (memq 'shift mods)
		  (memq 'control mods)
		  (lookup-key ibs-mode-map
			      (let ((copy (copy-sequence key)))
				(aset copy 0
				      (- main-event (- ?\C-\S-a
						       ?\C-a)))
				copy)
			      nil)))
	   (setcar keylist (- main-event (- ?\C-\S-a ?\C-a)))
	   (cancel-kbd-macro-events)
	   (apply 'ibs-unread keylist))

	  (t
	   (let (window)
	     (cancel-kbd-macro-events)
	     (apply 'ibs-unread keylist)
	     ;; Properly handle scroll-bar and mode-line clicks
	     ;; for which a dummy prefix event was generated as (aref key 0).
	     (and (> (length key) 1)
		  (symbolp (aref key 0))
		  (listp (aref key 1))
		  (not (numberp (posn-point (event-start (aref key 1)))))
		  ;; Convert the event back into its raw form,
		  ;; with the dummy prefix implicit in the mouse event,
		  ;; so it will get split up once again.
		  (progn (setq unread-command-events
			       (cdr unread-command-events))
			 (setq main-event (car unread-command-events))
			 (setcar (cdr (event-start main-event))
				 (car (nth 1 (event-start main-event))))))
	     ;; If we got a mouse click, maybe it was read with the buffer
	     ;; it was clicked on.  If so, that buffer, not the current one,
	     ;; is in ibs mode.  So end the search in that buffer.
	     (if (and (listp main-event)
		      (setq window (posn-window (event-start main-event)))
		      (windowp window)
		      (or (> (minibuffer-depth) 0)
			  (not (window-minibuffer-p window))))
		 (save-excursion
		   (set-buffer (window-buffer window))
		   (ibs-done t)
		   )
	       (ibs-done t)
	       )))	  
	  )))

(defun ibs-unread (&rest char-or-events)
  "Unread all input events in CHAR-OR-EVENTS."
  (mapcar 'store-kbd-macro-event char-or-events)
  (setq unread-command-events
	(append char-or-events unread-command-events)))

(defun ibs-next-buffer ()
  "Switch to next buffer."
  (interactive)
  (let ((buff (car ibs-buffer-list)))
    (switch-to-buffer buff t)
    (ibs-mode)
    (setq ibs-buffer-list (ibs-step-right ibs-buffer-list))
    (message "%S" (mapcar (function buffer-name)
			  ibs-buffer-list))
    (ibs-cancel-after-timeout)
    ))

(defun ibs-step-right (alist)
  "Return ALIST rotated right."
  (append (cdr alist)
	  (list (car alist))))

(if (featurep 'bs)
    (progn
      (defun bs-cycling-list ()
	"Return buffer list for buffer cycling.
The buffers taking part in buffer cycling are defined
by buffer configuration `bs-cycle-configuration-name'."
	(interactive)
	(let ((bs--buffer-coming-from (current-buffer))
	      (bs-dont-show-regexp   bs-dont-show-regexp)
	      (bs-must-show-regexp   bs-must-show-regexp)
	      (bs-dont-show-function bs-dont-show-function)
	      (bs-must-show-function bs-must-show-function)
	      (bs--show-all          bs--show-all))
	  (if bs-cycle-configuration-name
	      (bs-set-configuration bs-cycle-configuration-name))
	  (let ((bs-buffer-sort-function nil)
		(bs--current-sort-function nil))
	    (let* ((tupel (bs-next-buffer)))
	      (cdr tupel)))))
      
      (setq ibs-cycle-buffer-function
	    (or ibs-cycle-buffer-function 'bs-cycling-list))))

(provide 'ibs)

;;; ibs.el ends here
;;; p4.el --- Simple Perforce-Emacs Integration
;;
;; $Id: p4.el,v 1.66 2004/05/18 14:52:11 rvgnu Exp $

;;; Commentary:
;;
;;    Applied the GNU G.P.L. to this file - rv 3/27/1997

;;    Programs for  Emacs <-> Perforce Integration.
;;    Copyright (C) 1996, 1997	Eric Promislow
;;    Copyright (C) 1997-2004	Rajesh Vaidheeswarran
;;
;;    This program is free software; you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation; either version 2 of the License, or
;;    (at your option) any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with this program; if not, write to the Free Software
;;    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;    If you have any problems to report, or suggestions, please send them
;;    to p4el-bugs@lists.sourceforge.net

;; LCD Archive Entry:
;; p4|Rajesh Vaidheeswarran|rv@NoSpAm.lOsEtHiS.dsmit.com|
;; P4 SCM Integration into Emacs/XEmacs|
;; 2004/06/11|10.5|not_assigned_yet|

;; WARNING:
;; --------
;;
;;    % p4 edit foo.c
;;    ... make changes to foo.c in emacs
;;    % p4 submit
;;     ... keep the writable copy of foo.c in emacs.  Start making changes
;;     to it.  Discover that you can't save it.	 If you do M-x:p4-edit,
;;     you'll lose your changes.  You need to do a 'p4 edit' at the
;;     command-line.
;;

;; NOTES:
;; ------
;;
;; It is best if you take this file and byte compile it. To do that, you
;; need to do the following:
;;
;; % emacs -batch -f batch-byte-compile /full/path/to/file/p4.el
;;
;; This creates a binary file p4.elc in the path. Add the path to your
;; load-path variable in .emacs like this:
;;
;; (setq load-path (cons "/full/path/to/file" load-path))
;;
;; Then add the library like this:
;;
;; (load-library "p4")
;;

;;; Code:

(defvar p4-emacs-version "10.5" "The Current P4-Emacs Integration Revision.")

;; Find out what type of emacs we are running in. We will be using this
;; quite a few times in this program.
(eval-and-compile
  (defvar p4-running-emacs nil
    "If the current Emacs is not XEmacs, then, this is non-nil.")
  (defvar p4-running-xemacs nil
    "If the current Emacs is XEmacs/Lucid, then, this is non-nil.")
  (if (string-match "XEmacs\\|Lucid" emacs-version)
      (setq p4-running-xemacs t)
    (setq p4-running-emacs t)))

;; Pick up a couple of missing function defs
(if p4-running-xemacs
    (eval-when-compile
      (require 'timer)
      (require 'dired)))

(defvar p4-emacs-maintainer
  "p4.el maintainers <p4el-bugs@lists.sourceforge.net>"
  "The maintainer(s) of the emacs-p4 integration. Used for bug reports.")

(defvar p4-web-page "http://p4el.sourceforge.net/" "The home of p4.el.")

;; For flavors of Emacs which don't define `defgroup' and `defcustom'.
(eval-when-compile
  (if (not (fboundp 'defgroup))
      (defmacro defgroup (sym memb doc &rest args)
	"Null macro for defgroup in all versions of Emacs that don't define
defgroup"
	t))
  (if (not (fboundp 'defcustom))
      (defmacro defcustom (sym val doc &rest args)
	"Macro to alias defcustom to defvar in all versions of Emacs that
don't define defcustom"
	`(defvar ,sym ,val ,doc))))

(defgroup p4 nil "Perforce VC System." :group 'tools)

;; This can be set to wherever 'p4' lies using p4-set-p4-executable
(eval-and-compile
  (defun p4-windows-os ()
    (memq system-type '(ms-dos windows-nt)))

  (defcustom p4-executable
    (let ((lst (append
		exec-path
		(list "/usr/local/bin/p4"
		      (concat (getenv "HOME") "/bin/p4")
		      "p4")))
	  (p4-progname (if (p4-windows-os) "p4.exe" "p4"))
	  p4ex)
      (while (and lst (not p4ex))
	(let ((tmp (concat (file-name-as-directory (car lst))
			   p4-progname)))
	  (if (and (file-executable-p tmp)
		   (not (file-directory-p tmp)))
	      (setq p4ex tmp))
	  (setq lst (cdr lst))))
      p4ex)
    "This is the p4 executable.
To set this, use the function  `p4-set-p4-executable' or `customize'"
    :type 'string
    :group 'p4))

;; This is a string with default arguments to pass to "p4 diff",
;; "p4 diff2", "p4 describe", etc.
(defcustom p4-default-diff-options "-du"
  "Type of p4 diff output to be displayed. \(regular or context or
unified.\)"
  :type 'string
  :group 'p4)

(defcustom p4-default-depot-completion-prefix "//depot/"
  "Prefix to be used for completion prompt when prompting user for a depot
file."
  :type 'string
  :group 'p4)

;; Set this variable to nil to turn off colorized diff buffers.
(defcustom p4-colorized-diffs t
  "Set this to nil to disable colorized diffs."
  :type 'boolean
  :group 'p4)

;; Set whether P4CONFIG should be used exclusively for VC checking
(defcustom p4-use-p4config-exclusively nil
  "Whether P4 mode should use P4CONFIG exclusively to check whether a file
is under P4 version control. If set to nil, `p4-check-mode' is always
called; otherwise, it checks to see if the file named by P4CONFIG exists in
this or a parent directory, and if so, only then runs p4-check-mode.

This provides for a much faster `p4-find-file-hook'."
  :type 'boolean
  :group 'p4)

;; Auto-refresh?
(defcustom p4-auto-refresh t
  "Set this to automatically refresh p4 submitted files in buffers."
  :type 'boolean
  :group 'p4)

;; Check for empty diffs at submit time
(defcustom p4-check-empty-diffs t
  "Set this to check for files with empty diffs before submitting."
  :type 'boolean
  :group 'p4)

(defcustom p4-verbose t
  "When set, p4 will pop up the output buffer with the result of the
command."
  :type 'boolean
  :group 'p4)

;; Follow Symlinks?
(defcustom p4-follow-symlinks nil
  "When set, p4 will call `file-truename' on all opened files."
  :type 'boolean
  :group 'p4)

(defcustom p4-mode-hook nil
  "Hook run by `p4-mode'."
  :type 'sexp
  :group 'p4)

(eval-and-compile
  (defvar p4-output-buffer-name "*P4 Output*" "P4 Output Buffer."))

;; Set this variable in .emacs if you want p4-set-client-name to complete
;; your client name for you.
(defvar p4-my-clients nil
  "This variable holds the alist of p4 clients that the function
`p4-set-client-name' can complete on.

Set this variable *only* if you don't want P4 to complete on all the clients
in the P4 server.

This is a alist, and should be set using the function
`p4-set-my-clients'. For example, in your .emacs:

\(load-library \"p4\"\)
\(p4-set-my-clients \'(client1 client2 client3)\)")

;; Set this variable in .emacs if you want to alter the completion
;; behavior of p4-set-client-name.

(defcustom p4-strict-complete t
  "Set this variable in .emacs \(or using `customize'\) if you want to alter
the completion behavior of `p4-set-client-name'.
"
  :type 'boolean
  :group 'p4)

(if (not (getenv "P4PORT"))
    (setenv "P4PORT" "perforce:1666"))

(defvar p4-notify-list (getenv "P4NOTIFY") "The P4 Notify List.")

(defcustom p4-sendmail-program (if (boundp 'sendmail-program)
				   sendmail-program
				 nil)
  "The sendmail program. To set this use `customize'."
  :type 'string
  :group 'p4)

(defcustom p4-user-email (if (boundp 'user-mail-address)
			     user-mail-address nil)
  "The e-mail address of the current user. This is used with the
notification system, and must be set if notification should take place. To
set this, use `customize'."
  :type 'string
  :group 'p4)

(defcustom p4-notify nil
  "If this is t then the users in the notification list set by
`p4-set-notify-list' will get a notification of any P4 change submitted from
within emacs."
  :type 'boolean
  :group 'p4)

;; This can be set with p4-toggle-vc-mode
(defcustom p4-do-find-file t
  "If non-nil, the `p4-find-file-hook' will run when opening files."
  :type 'boolean
  :group 'p4)

;; Now add a hook to find-file-hooks
(add-hook 'find-file-hooks 'p4-find-file-hook)
;; .. and one to kill-buffer-hook
(add-hook 'kill-buffer-hook 'p4-kill-buffer-hook)

;; Tell Emacs about this new kind of minor mode
(defvar p4-mode nil "Is this file under p4?")
(make-variable-buffer-local 'p4-mode)
(put 'p4-mode 'permanent-local t)

(defvar p4-offline-mode nil "Is this file under p4 but handled in offline mode?")
(make-variable-buffer-local 'p4-offline-mode)
(put 'p4-offline-mode 'permanent-local t)

(defvar p4-minor-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-x\C-q" 'p4-toggle-read-only)
    map)
  "Keymap for p4 minor mode")
(fset 'p4-minor-map p4-minor-map)
(or (assoc 'p4-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(p4-mode p4-mode)
				 minor-mode-alist)))
(or (assoc 'p4-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons '(p4-mode . p4-minor-map) minor-mode-map-alist)))
(or (assoc 'p4-offline-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(p4-offline-mode p4-offline-mode)
				 minor-mode-alist)))
(or (assoc 'p4-offline-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons '(p4-offline-mode . p4-minor-map) minor-mode-map-alist)))

(defvar p4-async-minor-mode nil
  "The minor mode for editing p4 asynchronous command buffers.")
(make-variable-buffer-local 'p4-async-minor-mode)
(defvar p4-async-minor-map (make-sparse-keymap) "Keymap for p4 async minor mode")
(fset 'p4-async-minor-map p4-async-minor-map)

(or (assoc 'p4-async-minor-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(p4-async-minor-mode " P4") minor-mode-alist)))

(or (assoc 'p4-async-minor-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons '(p4-async-minor-mode . p4-async-minor-map) minor-mode-map-alist)))

(defvar p4-current-command nil)
(make-variable-buffer-local 'p4-current-command)
(put 'p4-current-command 'permanent-local t)
(set-default 'p4-current-command nil)

(defvar p4-current-args nil)
(make-variable-buffer-local 'p4-current-args)
(put 'p4-current-args 'permanent-local t)
(set-default 'p4-current-args nil)

;; To check if the current buffer's modeline and menu need to be altered
(defvar p4-vc-check nil)
(make-variable-buffer-local 'p4-vc-check)
(put 'p4-vc-check 'permanent-local t)
(set-default 'p4-vc-check nil)

(defvar p4-set-client-hooks nil
  "List of functions to be called after a p4 client is changed.
The buffer's local variables (if any) will have been processed before the
functions are called.")

(if p4-running-emacs (require 'timer))

(defvar p4-timer nil "Timer object that will be set to cleanup the caches
periodically.")

(defcustom p4-cleanup-time 600 "seconds after which `p4-cache-cleanup' will
check for dirty caches."
  :type 'integer
  :group 'p4)

(defcustom p4-cleanup-cache t "`p4-cache-cleanup' will cleanup the
branches/clients/dirs/labels caches once in a while if this is non-nil."
  :type 'boolean
  :group 'p4)

(defvar p4-all-buffer-files nil "An associated list of all buffers and
their files under p4 version control. This is to enable autorefreshing of
p4 submitted files being visited by the buffer.")

(defvar p4-file-refresh-timer nil "Timer object that will be set to refresh
the files in Emacs buffers that have been modified by a `p4-submit'.")

(defcustom p4-file-refresh-timer-time 60 "seconds after which
`p4-file-refresh' will check for modified files in Emacs buffers. Set this
variable to 0 to disable periodic refreshing."
  :type 'integer
  :group 'p4)

(defvar p4-async-command-hook nil
  "This hook is run after an async buffer has been set up by
`p4-async-process-command'")

(defvar p4-window-config-stack nil
  "Stack of saved window configurations.")

(defcustom p4-window-config-stack-size 20 "Maximum stack size
for saved window configurations."
  :type 'integer
  :group 'p4)

(defcustom p4-exec-arg-len-max 20000 "Maximum total length of all
arguments to p4 commands."
  :type 'integer
  :group 'p4)

(defvar p4-basic-map
  (let ((map (make-sparse-keymap)))
    (cond (p4-running-xemacs
	   (define-key map [button2] 'p4-buffer-mouse-clicked)
	   (define-key map [button3] 'p4-buffer-mouse-clicked-3))
	  (p4-running-emacs
	   (define-key map [mouse-2] 'p4-buffer-mouse-clicked)
	   (define-key map [mouse-3] 'p4-buffer-mouse-clicked-3)))
    (define-key map [return] 'p4-buffer-commands)
    (define-key map "\r" 'p4-buffer-commands)
    (define-key map "q"	 'p4-quit-current-buffer)
    (define-key map "k"	 'p4-scroll-down-1-line)
    (define-key map "j"	 'p4-scroll-up-1-line)
    (define-key map "b"	 'p4-scroll-down-1-window)
    (define-key map [backspace] 'p4-scroll-down-1-window)
    (define-key map " "	 'p4-scroll-up-1-window)
    (define-key map "<"	 'p4-top-of-buffer)
    (define-key map ">"	 'p4-bottom-of-buffer)
    (define-key map "="	 'p4-delete-other-windows)
    map))

(defun p4-make-derived-map (base-map)
  (let (map)
    (cond (p4-running-xemacs
	   (setq map (make-sparse-keymap))
	   (set-keymap-parents map (list base-map)))
	  (p4-running-emacs
	   (setq map (cons 'keymap base-map))))
    map))

(defvar p4-filelog-map
  (let ((map (p4-make-derived-map p4-basic-map)))
    (define-key map "d"	 'p4-diff2)
    (define-key map "f"	 'p4-find-file-other-window)
    (define-key map "s"	 'p4-filelog-short-format)
    (define-key map "l"	 'p4-filelog-long-format)
    (define-key map "k"	 'p4-scroll-down-1-line-other-w)
    (define-key map "j"	 'p4-scroll-up-1-line-other-w)
    (define-key map "b"	 'p4-scroll-down-1-window-other-w)
    (define-key map [backspace] 'p4-scroll-down-1-window-other-w)
    (define-key map " "	 'p4-scroll-up-1-window-other-w)
    (define-key map "<"	 'p4-top-of-buffer-other-w)
    (define-key map ">"	 'p4-bottom-of-buffer-other-w)
    (define-key map "="	 'p4-delete-other-windows)
    (define-key map "n"	 'p4-goto-next-change)
    (define-key map "p"	 'p4-goto-prev-change)
    (define-key map "N" (lookup-key map "p"))
    map)
  "The key map to use for selecting filelog properties.")

(defvar p4-opened-map
  (let ((map (p4-make-derived-map p4-basic-map)))
    (define-key map "n"	 'p4-next-depot-file)
    (define-key map "p"	 'p4-prev-depot-file)
    (define-key map "N" (lookup-key map "p"))
    map)
  "The key map to use for selecting opened files.")

(defvar p4-diff-map
  (let ((map (p4-make-derived-map p4-basic-map)))
    (define-key map "n"	 'p4-goto-next-diff)
    (define-key map "p"	 'p4-goto-prev-diff)
    (define-key map "N" (lookup-key map "p"))
    (define-key map "d"	 'p4-next-depot-diff)
    (define-key map "u"	 'p4-prev-depot-diff)
    map))

(defvar p4-print-rev-map
  (let ((map (p4-make-derived-map p4-basic-map)))
    (define-key map "n"	 'p4-next-change-rev-line)
    (define-key map "p"	 'p4-prev-change-rev-line)
    (define-key map "N" (lookup-key map "p"))
    (define-key map "l"	 'p4-toggle-line-wrap)
    map)
  "The key map to use for browsing print-revs buffers.")

;;; All functions start here.

;; A generic function that we use to execute p4 commands
(eval-and-compile
  (defun p4-exec-p4 (output-buffer args &optional clear-output-buffer)
    "Internal function called by various p4 commands.
Executes p4 in the current buffer's current directory
with output to a dedicated output buffer.
If successful, adds the P4 menu to the current buffer.
Does auto re-highlight management (whatever that is)."
    (save-excursion
      (if (eq major-mode 'dired-mode)
	  (let ((dir (dired-current-directory)))
	    (set-buffer output-buffer)
	    (setq default-directory dir)))
      (if clear-output-buffer
	  (progn
	    (set-buffer output-buffer)
	    (delete-region (point-min) (point-max))))
      (let ((result
	     ;; XXX - call-process has changed from using
	     ;; p4-null-device to nil as its second argument
	     ;; in emacs version 21.1.1?? - rv 1/25/2002
	     (apply 'call-process (p4-check-p4-executable) nil
		    output-buffer
		    nil			; update display?
		    "-d" default-directory  ;override "PWD" env var
		    args)))
	(p4-menu-add)
	(if (and p4-running-emacs
		 (boundp 'hilit-auto-rehighlight))
	    (setq hilit-auto-rehighlight nil))
	result)))
  (defun p4-call-p4-here (&rest args)
    "Internal function called by various p4 commands.
Executes p4 in the current buffer (generally a temp)."
    (apply 'call-process (p4-check-p4-executable) nil
	   t
	   nil			; update display?
	   "-d" default-directory  ;override "PWD" env var
	   args)))

(defun p4-push-window-config ()
  "Push the current window configuration on the `p4-window-config-stack'
stack."
  (interactive)
  (setq p4-window-config-stack
	(cons (current-window-configuration)
	      p4-window-config-stack))
  (while (> (length p4-window-config-stack) p4-window-config-stack-size)
    (setq p4-window-config-stack
	  (reverse (cdr (reverse p4-window-config-stack))))))

(defun p4-pop-window-config (num)
  "Pop `num' elements from the `p4-window-config-stack' stack and use
the last popped element to restore the window configuration."
  (interactive "p")
  (while (> num 0)
    (if (eq p4-window-config-stack nil)
	(error "window config stack empty"))
    (set-window-configuration (car p4-window-config-stack))
    (setq p4-window-config-stack (cdr p4-window-config-stack))
    (setq num (1- num)))
  (message "window config popped (stack size %d)"
	   (length p4-window-config-stack)))


;; The menu definition is in the XEmacs format. Emacs parses and converts
;; this definition to its own menu creation commands.

(defalias 'p4-toggle-vc-mode-off 'p4-toggle-vc-mode)
(defalias 'p4-toggle-vc-mode-on 'p4-toggle-vc-mode)

(eval-and-compile
  (defvar p4-menu-def
    '(["Specify Arguments..." universal-argument t]
      ["--" nil nil]
      ["Add Current to P4" p4-add
       (and (p4-buffer-file-name) (not p4-mode))]
      ["Check out/Edit"    p4-edit
       (and (p4-buffer-file-name-2) (or (not p4-mode) buffer-read-only))]
      ["Re-open"	       p4-reopen
       (and (p4-buffer-file-name-2) (or (not p4-mode) (not buffer-read-only)))]
      ["Revert File"  p4-revert
       (and (p4-buffer-file-name-2) (or (not p4-mode) (not buffer-read-only)))]
      ["Delete File from Depot"  p4-delete
       (and (p4-buffer-file-name-2) (or (not p4-mode) buffer-read-only))]
      ["Rename Depot File" p4-rename
       (and (p4-buffer-file-name-2) (or (not p4-mode) buffer-read-only))]
      ["Submit Changes"  p4-submit t]
      ["--" nil nil]
      ["Sync/Get Files from Depot" p4-get t]
      ["--" nil nil]
      ["Show Opened Files"	p4-opened t]
      ["Filelog" p4-filelog (p4-buffer-file-name-2)]
      ["Changes" p4-changes t]
      ["Describe Change" p4-describe t]
      ["--" nil nil]
      ["Diff 2 Versions" p4-diff2 (p4-buffer-file-name-2)]
      ["Diff Current" p4-diff t]
      ["Diff All Opened Files" p4-diff-all-opened t]
      ["Diff Current with Ediff"   p4-ediff
       (and (p4-buffer-file-name) (not buffer-read-only) p4-mode)]
      ["Diff 2 Versions with Ediff"   p4-ediff2 (p4-buffer-file-name-2)]
      ["--" nil nil]
      ["Schedule Integrations" p4-integ t]
      ["Resolve Conflicts" p4-resolve t]
      ["--" nil nil]
      ["Print" p4-print (p4-buffer-file-name-2)]
      ["Print with Revision History" p4-blame
       (p4-buffer-file-name-2)]
      ["Find File using Depot Spec" p4-depot-find-file
       p4-do-find-file]
      ["--" nil nil]
      ["Edit a Branch Specification" p4-branch t]
      ["Edit a Label Specification" p4-label t]
      ["Edit a Client Specification" p4-client t]
      ["Edit a User Specification" p4-user t]
      ["--" nil nil]
      ["Show Version" p4-emacs-version t]
      ["Disable P4 VC Check"  p4-toggle-vc-mode-off
       p4-do-find-file]
      ["Enable P4 VC Check"	 p4-toggle-vc-mode-on
       (not p4-do-find-file)]
      ["--" nil nil]
      ["Set P4 Config"  p4-set-client-config p4-do-find-file]
      ["Get Current P4 Config"  p4-get-client-config
       p4-do-find-file]
      ["--" nil nil]
      ["Set P4 Client"  p4-set-client-name p4-do-find-file]
      ["Get Current P4 Client"  p4-get-client-name
       p4-do-find-file]
      ["--" nil nil]
      ["Set P4 Server/Port"	 p4-set-p4-port p4-do-find-file]
      ["Get Current P4 Server/Port"	 p4-get-p4-port
       p4-do-find-file]
      ["--" nil nil]
      ["Set P4 Notification List"  p4-set-notify-list
       p4-mode]
      ["Get P4 Notification List"  p4-get-notify-list p4-notify]
      ["--" nil nil]
      ["Describe Key Bindings"  p4-describe-bindings t]
      ["Check for later versions of p4.el" p4-browse-web-page t]
      ["--" nil nil]
      ["Report Bug in p4.el"  p4-bug-report t])
    "The P4 menu definition")

  (cond (p4-running-xemacs
	 ;; Menu Support for XEmacs
	 (require 'easymenu)
	 (defun p4-mode-menu (modestr)
	   (cons modestr p4-menu-def)))

	(p4-running-emacs
	 ;; Menu support for Emacs
	 (or (lookup-key global-map [menu-bar])
	     (define-key global-map [menu-bar] (make-sparse-keymap "menu-bar")))
	 (defvar menu-bar-p4-menu (make-sparse-keymap "P4"))
	 (setq menu-bar-final-items (cons 'p4-menu menu-bar-final-items))
	 (define-key global-map [menu-bar p4-menu]
	   (cons "P4" menu-bar-p4-menu))
	 (let ((m (reverse p4-menu-def))
	       (separator-number 0))
	   (while m
	     (let ((menu-text (elt (car m) 0))
		   (menu-action (elt (car m) 1))
		   (menu-pred (elt (car m) 2)))
	       (if menu-action
		   (progn
		     (define-key menu-bar-p4-menu (vector menu-action)
		       (cons menu-text menu-action))
		     (put menu-action 'menu-enable menu-pred))
		 (define-key menu-bar-p4-menu
		   (vector (make-symbol
			    (concat "separator-"
				    (int-to-string separator-number))))
		   '("--"))
		 (setq separator-number (1+ separator-number))))
	     (setq m (cdr m))))))

  (defun p4-depot-output (command &optional args)
    "Executes p4 command inside a buffer.
Returns the buffer."
    (let ((buffer (generate-new-buffer p4-output-buffer-name)))
      (p4-exec-p4 buffer (cons command args) t)
      buffer))

  (defun p4-check-p4-executable ()
    "Check if the `p4-executable' is nil, and if so, prompt the user for a
valid `p4-executable'."
    (interactive)
    (if (not p4-executable)
	(call-interactively 'p4-set-p4-executable)
      p4-executable))

  (defun p4-menu-add ()
    "To add the P4 menu bar button for files that are already not in
the P4 depot or in the current client view.."
    (interactive)
    (cond (p4-running-xemacs
	   (if (not (boundp 'p4-mode))
	       (setq p4-mode nil))
	   (easy-menu-add (p4-mode-menu "P4"))))
    t)

  (defun p4-help-text (cmd text)
    (if cmd
	(let ((buf (generate-new-buffer p4-output-buffer-name))
	      (help-text ""))
	  (if (= (p4-exec-p4 buf (list "help" cmd) t) 0)
	      (setq help-text (save-excursion
				(set-buffer buf)
				(buffer-string))))
	  (kill-buffer buf)
	  (concat text help-text))
      text))

  ;; To set the path to the p4 executable
  (defun p4-set-p4-executable (p4-exe-name)
    "Set the path to the correct P4 Executable.

To set this as a part of the .emacs, add the following to your .emacs:

\(load-library \"p4\"\)
\(p4-set-p4-executable \"/my/path/to/p4\"\)

Argument P4-EXE-NAME The new value of the p4 executable, with full path."
    (interactive "fFull path to your P4 executable: " )
    (setq p4-executable p4-exe-name)
    p4-executable))

;; The kill-buffer hook for p4.
(defun p4-kill-buffer-hook ()
  "To Remove a file and its associated buffer from our global list of P4
controlled files."
  (if p4-vc-check
      (p4-refresh-refresh-list (p4-buffer-file-name)
			       (buffer-name))))

(defmacro defp4cmd (fkn &rest all-args)
  (let ((args (car all-args))
	(help-cmd (cadr all-args))
	(help-txt (eval (cadr (cdr all-args))))
	(body (cdr (cddr all-args))))
    `(defalias ',fkn
       ,(append (list 'lambda args
		      (p4-help-text help-cmd help-txt))
		body))))

(defun p4-noinput-buffer-action (cmd
				 do-revert
				 show-output
				 &optional arguments preserve-buffer)
  "Internal function called by various p4 commands."
  (save-excursion
    (save-excursion
      (if (not preserve-buffer)
	  (progn
	    (get-buffer-create p4-output-buffer-name);; We do these two lines
	    (kill-buffer p4-output-buffer-name)))    ;; to ensure no duplicates
      (p4-exec-p4 (get-buffer-create p4-output-buffer-name)
		  (append (list cmd) arguments)
		  t))
    (p4-partial-cache-cleanup cmd)
    (if show-output
	(if (and
	     (eq show-output 's)
	     (= (save-excursion
		  (set-buffer p4-output-buffer-name)
		  (count-lines (point-min) (point-max)))
		1)
	     (not (save-excursion
		    (set-buffer p4-output-buffer-name)
		    (goto-char (point-min))
		    (looking-at "==== "))))
	    (save-excursion
	      (set-buffer p4-output-buffer-name)
	      (message (buffer-substring (point-min)
					 (save-excursion
					   (goto-char (point-min))
					   (end-of-line)
					   (point)))))
	  (p4-push-window-config)
	  (if (not (one-window-p))
	      (delete-other-windows))
	  (display-buffer p4-output-buffer-name t))))
  (if (and do-revert (p4-buffer-file-name))
      (revert-buffer t t)))

;; The p4 edit command
(defp4cmd p4-edit (show-output)
  "edit" "To open the current depot file for edit, type \\[p4-edit].\n"
  (interactive (list p4-verbose))
  (let ((args (p4-buffer-file-name))
	refresh-after)
    (if (or current-prefix-arg (not args))
	(progn
	  (setq args (if (p4-buffer-file-name-2)
			 (p4-buffer-file-name-2)
		       ""))
	  (setq args (p4-make-list-from-string
		      (p4-read-arg-string "p4 edit: " (cons args 0))))
	  (setq refresh-after t))
      (setq args (list args)))
    (p4-noinput-buffer-action "edit" t (and show-output 's) args)
    (if refresh-after
	(p4-refresh-files-in-buffers)))
  (p4-check-mode)
  (p4-update-opened-list))

;; The p4 reopen command
(defp4cmd p4-reopen (show-output)
  "reopen"
  "To change the type or changelist number of an opened file, type \\[p4-reopen].

Argument SHOW-OUTPUT displays the *P4 Output* buffer on executing the
command if t.\n"

  (interactive (list p4-verbose))
  (let ((args (if (p4-buffer-file-name-2)
		  (p4-buffer-file-name-2)
		"")))
    (setq args (p4-make-list-from-string
		(p4-read-arg-string "p4 reopen: " (cons args 0))))
    (p4-noinput-buffer-action "reopen" t (and show-output 's) args))
  (p4-check-mode)
  (p4-update-opened-list))

;; The p4 revert command
(defp4cmd p4-revert (show-output)
  "revert" "To revert all change in the current file, type \\[p4-revert].\n"
  (interactive (list p4-verbose))
  (let ((args (p4-buffer-file-name))
	refresh-after)
    (if (or current-prefix-arg (not args))
	(progn
	  (setq args (if (p4-buffer-file-name-2)
			 (p4-buffer-file-name-2)
		       ""))
	  (setq args (p4-make-list-from-string
		      (p4-read-arg-string "p4 revert: " args)))
	  (setq refresh-after t))
      (setq args (list args)))
    (if (yes-or-no-p "Really revert changes? ")
	(progn
	  (p4-noinput-buffer-action "revert" t (and show-output 's) args)
	  (if refresh-after
	      (progn
		(p4-refresh-files-in-buffers)
		(p4-check-mode-all-buffers))
	    (p4-check-mode))
	  (p4-update-opened-list)))))

;; The p4 lock command
(defp4cmd p4-lock ()
  "lock" "To lock an opened file against changelist submission, type \\[p4-lock].\n"
  (interactive)
  (let ((args (list (p4-buffer-file-name-2))))
    (if (or current-prefix-arg (not (p4-buffer-file-name-2)))
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 lock: "
					(p4-buffer-file-name-2)))))
    (p4-noinput-buffer-action "lock" t 's args)
    (p4-update-opened-list)))

;; The p4 unlock command
(defp4cmd p4-unlock ()
  "unlock" "To release a locked file but leave open, type \\[p4-unlock].\n"
  (interactive)
  (let ((args (list (p4-buffer-file-name-2))))
    (if (or current-prefix-arg (not (p4-buffer-file-name-2)))
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 unlock: "
					(p4-buffer-file-name-2)))))
    (p4-noinput-buffer-action "unlock" t 's args)
    (p4-update-opened-list)))

;; The p4 diff command
(defp4cmd p4-diff ()
  "diff" "To diff the current file and topmost depot version, type \\[p4-diff].\n"
  (interactive)
  (let ((args (p4-make-list-from-string p4-default-diff-options)))
    (if (p4-buffer-file-name-2)
	(setq args (append args
			   (list (p4-buffer-file-name-2)))))
    (if current-prefix-arg
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 diff: " p4-default-diff-options))))
    (p4-noinput-buffer-action "diff" nil 's args)
    (p4-activate-diff-buffer "*P4 diff*")))

(defun p4-diff-all-opened ()
  (interactive)
  (p4-noinput-buffer-action "diff" nil 's
			    (p4-make-list-from-string p4-default-diff-options))
  (p4-activate-diff-buffer "*P4 diff*"))


(defun p4-get-file-rev (default-name rev)
  (if (string-match "^\\([0-9]+\\|none\\|head\\|have\\)$" rev)
      (setq rev (concat "#" rev)))
  (cond ((string-match "^[#@]" rev)
	 (concat default-name rev))
	((string= "" rev)
	 default-name)
	(t
	 rev)))

;; The p4 diff2 command
(defp4cmd p4-diff2 (version1 version2)
  "diff2" "Display diff of two depot files.

When visiting a depot file, type \\[p4-diff2] and enter the versions.\n"
  (interactive
   (let ((rev (get-char-property (point) 'rev)))
     (if (and (not rev) (p4-buffer-file-name-2))
	 (let ((rev-num 0))
	   (setq rev (p4-is-vc nil (p4-buffer-file-name-2)))
	   (if rev
	       (setq rev-num (string-to-number rev)))
	   (if (> rev-num 1)
	       (setq rev (number-to-string (1- rev-num)))
	     (setq rev nil))))
     (list (p4-read-arg-string "First Depot File or Version# to diff: " rev)
	   (p4-read-arg-string "Second Depot File or Version# to diff: "))))
  (let (diff-version1
	diff-version2
	(diff-options (p4-make-list-from-string p4-default-diff-options)))
    (if current-prefix-arg
	(setq diff-options (p4-make-list-from-string
			    (p4-read-arg-string "Optional Args: "
						p4-default-diff-options))))
    ;; try to find out if this is a revision number, or a depot file
    (setq diff-version1 (p4-get-file-rev (p4-buffer-file-name-2) version1))
    (setq diff-version2 (p4-get-file-rev (p4-buffer-file-name-2) version2))

    (p4-noinput-buffer-action "diff2" nil t
			      (append diff-options
				      (list diff-version1
					    diff-version2)))
    (p4-activate-diff-buffer "*P4 diff2*")))

(defp4cmd p4-diff-head ()
  "diff-head" "Display diff of file against the head revision in depot.

When visiting a depot file, type \\[p4-diff-head].\n"

  (interactive)
  (let (head-revision
	(diff-options (p4-make-list-from-string p4-default-diff-options)))
    (if current-prefix-arg
	(setq diff-options (p4-make-list-from-string
			    (p4-read-arg-string "Optional Args: "
						p4-default-diff-options))))
    (setq head-revision (p4-get-file-rev (p4-buffer-file-name-2) "head"))

    (p4-noinput-buffer-action "diff" nil t
			      (append diff-options
				      (list head-revision)))
    (p4-activate-diff-buffer "*P4 diff vs. head*")))


;; p4-ediff for all those who diff using ediff

(defun p4-ediff ()
  "Use ediff to compare file with its original client version."
  (interactive)
  (require 'ediff)
  (if current-prefix-arg
      (call-interactively 'p4-ediff2)
    (progn
      (p4-noinput-buffer-action "print" nil nil
				(list "-q"
				      (concat (p4-buffer-file-name) "#have")))
      (let ((local (current-buffer))
	    (depot (get-buffer-create p4-output-buffer-name)))
	(ediff-buffers local
		       depot
		       `((lambda ()
			   (make-local-variable 'ediff-cleanup-hook)
			   (setq ediff-cleanup-hook
				 (cons (lambda ()
					 (kill-buffer ,depot)
					 (p4-menu-add))
				       ediff-cleanup-hook)))))))))

(defp4cmd p4-ediff2 (version1 version2)
  "ediff2" "Use ediff to display two versions of a depot file.

When visiting a depot file, type \\[p4-ediff2] and enter the versions.\n"
  (interactive
   (let ((rev (get-char-property (point) 'rev)))
     (if (and (not rev) (p4-buffer-file-name-2))
	 (let ((rev-num 0))
	   (setq rev (p4-is-vc nil (p4-buffer-file-name-2)))
	   (if rev
	       (setq rev-num (string-to-number rev)))
	   (if (> rev-num 1)
	       (setq rev (number-to-string (1- rev-num)))
	     (setq rev nil))))
     (list (p4-read-arg-string "First Depot File or Version# to diff: " rev)
	   (p4-read-arg-string "Second Depot File or Version# to diff: "))))
  (let* ((file-name (p4-buffer-file-name-2))
         (basename (file-name-nondirectory file-name))
         (bufname1 (concat "*P4 ediff " basename "#" version1  "*"))
         (bufname2 (concat "*P4 ediff " basename "#" version2  "*"))
         (diff-version1 (p4-get-file-rev file-name version1))
         (diff-version2 (p4-get-file-rev file-name version2)))
    (p4-noinput-buffer-action "print" nil nil (list "-q" diff-version1))
    (set-buffer p4-output-buffer-name)
    (rename-buffer bufname1 t)
    (p4-noinput-buffer-action "print" nil nil (list "-q" diff-version2))
    (set-buffer p4-output-buffer-name)
    (rename-buffer bufname2 t)
    (let ((buffer-version-1 (get-buffer-create bufname1))
          (buffer-version-2 (get-buffer-create bufname2)))
      (ediff-buffers buffer-version-1
                     buffer-version-2
                     `((lambda ()
                         (make-local-variable 'ediff-cleanup-hook)
                         (setq ediff-cleanup-hook
                               (cons (lambda ()
                                       (kill-buffer ,buffer-version-1)
                                       (kill-buffer ,buffer-version-2)
                                       (p4-menu-add))
                                     ediff-cleanup-hook))))))))
;; The p4 add command
(defp4cmd p4-add ()
  "add" "To add the current file to the depot, type \\[p4-add].\n"
  (interactive)
  (let ((args (p4-buffer-file-name))
	refresh-after)
    (if (or current-prefix-arg (not args))
	(progn
	  (setq args (if (p4-buffer-file-name-2)
			 (p4-buffer-file-name-2)
		       ""))
	  (setq args (p4-make-list-from-string
		      (p4-read-arg-string "p4 add: " (cons args 0))))
	  (setq refresh-after t))
      (setq args (list args)))
    (p4-noinput-buffer-action "add" nil 's args)
    (if refresh-after
	(p4-check-mode-all-buffers)
      (p4-check-mode)))
  (p4-update-opened-list))


;; The p4 delete command
(defp4cmd p4-delete ()
  "delete" "To delete the current file from the depot, type \\[p4-delete].\n"
  (interactive)
  (let ((args (p4-buffer-file-name)))
    (if (or current-prefix-arg (not args))
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 delete: "
					(p4-buffer-file-name-2))))
      (setq args (list args)))
    (if (yes-or-no-p "Really delete from depot? ")
	(p4-noinput-buffer-action "delete" nil 's args)))
  (p4-check-mode)
  (p4-update-opened-list))

;; The p4 filelog command
(defp4cmd p4-filelog ()
  "filelog"
  "To view a history of the change made to the current file, type \\[p4-filelog].\n"
  (interactive)
  (let ((file-name (p4-buffer-file-name-2)))
    (if (or current-prefix-arg (not file-name))
	(setq file-name (p4-make-list-from-string
			 (p4-read-arg-string "p4 filelog: " file-name)))
      (setq file-name (list file-name)))
    (p4-file-change-log "filelog" file-name)))

(defun p4-set-extent-properties (start end prop-list)
  (cond (p4-running-xemacs
	 (let ((ext (make-extent start end)))
	   (while prop-list
	     (set-extent-property ext (caar prop-list) (cdar prop-list))
	     (setq prop-list (cdr prop-list)))))
	(p4-running-emacs
	 (let ((ext (make-overlay start end)))
	   (while prop-list
	     (overlay-put ext (caar prop-list) (cdar prop-list))
	     (setq prop-list (cdr prop-list)))))))

(defun p4-create-active-link (start end prop-list)
  (p4-set-extent-properties start end
			    (append (list (cons 'face 'bold)
					  (cons 'mouse-face 'highlight))
				    prop-list)))

(defun p4-move-buffer-point-to-top (buf-name)
  (if (get-buffer-window buf-name)
      (save-selected-window
	(select-window (get-buffer-window buf-name))
	(goto-char (point-min)))))

(defun p4-file-change-log (cmd file-list-spec)
  (let ((p4-filelog-buffer
	 (concat "*P4 " cmd ": "
		 (p4-list-to-string file-list-spec) "*")))
    (p4-noinput-buffer-action cmd nil t (cons "-l" file-list-spec))
    (p4-activate-file-change-log-buffer p4-filelog-buffer)))

(defun p4-activate-file-change-log-buffer (bufname)
  (let (p4-cur-rev p4-cur-change p4-cur-action
	p4-cur-user p4-cur-client)
    (p4-activate-print-buffer bufname nil)
    (set-buffer bufname)
    (setq buffer-read-only nil)
    (goto-char (point-min))
    (while (re-search-forward (concat
			       "^\\(\\.\\.\\. #\\([0-9]+\\) \\)?[Cc]hange "
			       "\\([0-9]+\\) \\([a-z]+\\)?.*on.*by "
			       "\\([^ @]+\\)@\\([^ \n]+\\).*\n"
			       "\\(\\(\\([ \t].*\\)?\n\\)*\\)") nil t)
      (let ((rev-match 2)
	    (ch-match 3)
	    (act-match 4)
	    (user-match 5)
	    (cl-match 6)
	    (desc-match 7))
	(setq p4-cur-rev (match-string rev-match))
	(setq p4-cur-change (match-string ch-match))
	(setq p4-cur-action (match-string act-match))
	(setq p4-cur-user (match-string user-match))
	(setq p4-cur-client (match-string cl-match))

	(if (match-beginning rev-match)
	    (p4-create-active-link (match-beginning rev-match)
				   (match-end rev-match)
				   (list (cons 'rev p4-cur-rev))))
	(p4-create-active-link (match-beginning ch-match)
			       (match-end ch-match)
			       (list (cons 'change p4-cur-change)))
	(if (match-beginning act-match)
	    (p4-create-active-link (match-beginning act-match)
				   (match-end act-match)
				   (list (cons 'action p4-cur-action)
					 (cons 'rev p4-cur-rev))))
	(p4-create-active-link (match-beginning user-match)
			       (match-end user-match)
			       (list (cons 'user p4-cur-user)))
	(p4-create-active-link (match-beginning cl-match)
			       (match-end cl-match)
			       (list (cons 'client p4-cur-client)))
	(p4-set-extent-properties (match-beginning desc-match)
				  (match-end desc-match)
				  (list (cons 'invisible t)
					(cons 'isearch-open-invisible t)))))
    (p4-find-change-numbers bufname (point-min) (point-max))
    (use-local-map p4-filelog-map)
    (setq buffer-invisibility-spec (list))
    (setq buffer-read-only t)
    (p4-move-buffer-point-to-top bufname)))

;; Scan specified region for references to change numbers
;; and make the change numbers clickable.
(defun p4-find-change-numbers (buffer start end)
  (save-excursion
    (set-buffer buffer)
    (goto-char start)
    (while (re-search-forward "\\(changes?\\|submit\\|p4\\)[:#]?[ \t\n]+" end t)
      (while (looking-at
	      (concat "\\([#@]\\|number\\|no\\.\\|\\)[ \t\n]*"
		      "\\([0-9]+\\)[-, \t\n]*"
		      "\\(and/or\\|and\\|&\\|or\\|\\)[ \t\n]*"))
	(let ((ch-start (match-beginning 2))
	      (ch-end (match-end 2))
	      (ch-str (match-string 2))
	      (next (match-end 0)))
	  (set-text-properties 0 (length ch-str) nil ch-str)
	  (p4-create-active-link ch-start ch-end (list (cons 'change ch-str)))
	  (goto-char next))))))

;; The p4 files command
(defp4cmd p4-files ()
  "files" "List files in the depot. Type, \\[p4-files].\n"
  (interactive)
  (let ((args (p4-buffer-file-name-2)))
    (if (or current-prefix-arg (not args))
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 files: " (p4-buffer-file-name-2))))
      (setq args (list args)))
    (p4-noinput-buffer-action "files" nil t args)
    (save-excursion
      (set-buffer p4-output-buffer-name)
      (p4-find-change-numbers p4-output-buffer-name (point-min) (point-max)))
    (p4-make-depot-list-buffer
     (concat "*P4 Files: (" (p4-current-client) ") " (car args) "*"))))


(defvar p4-server-version-cache nil)

(defun p4-get-server-version ()
  "To get the version number of the p4 server."
  (let ((p4-port (p4-current-server-port))
	ser-ver pmin)
    (setq ser-ver (cdr (assoc p4-port p4-server-version-cache)))
    (if (not ser-ver)
	(save-excursion
	  (get-buffer-create p4-output-buffer-name)
	  (set-buffer p4-output-buffer-name)
	  (goto-char (point-max))
	  (setq pmin (point))
	  (if (zerop (p4-call-p4-here "info"))
	      (progn
		(goto-char pmin)
		(re-search-forward
		 "^Server version: .*\/.*\/\\(\\([0-9]+\\)\.[0-9]+\\)\/.*(.*)$")
		(setq ser-ver (string-to-number (match-string 2)))
		(setq p4-server-version-cache (cons (cons p4-port ser-ver)
						    p4-server-version-cache))
		(delete-region pmin (point-max))))))
    ser-ver))

(defun p4-get-client-root (client-name)
  "To get the current value of Client's root type \\[p4-get-client-root].
   This can be used by any other macro that requires this value."
  (let (p4-client-root pmin)
    (save-excursion
      (get-buffer-create p4-output-buffer-name)
      (set-buffer p4-output-buffer-name)
      (goto-char (point-max))
      (setq pmin (point))
      (if (zerop (p4-call-p4-here "client" "-o" client-name))
	  (progn
	    (goto-char pmin)
	    (re-search-forward "^Root:[ \t]+\\(.*\\)$")
	    (setq p4-client-root (p4-canonize-client-root (match-string 1)))
	    (delete-region pmin (point-max)))))
    p4-client-root))

(defun p4-canonize-client-root (p4-client-root)
  "Canonizes client root"
  (let ((len (length p4-client-root)))
    ;; For Windows, since the client root may be terminated with
    ;; a \ as in c:\ or drive:\foo\bar\, we need to strip the
    ;; trailing \ .
    (if (and (p4-windows-os)
	     (> len 1)
	     (equal (substring p4-client-root (1- len) len) "\\"))
	(setq p4-client-root (substring p4-client-root 0 (1- len))))
    p4-client-root))

(defun p4-map-depot-files (file-list)
  "Map a list of files in the depot on the current client.
Return a list of pairs, where each pair consists of a depot
name and a client name."
  (let (file-map)
    (while file-list
      (let (sub-list (arg-len 0) elt)
	(while (and file-list (< arg-len p4-exec-arg-len-max))
	  (setq elt (car file-list))
	  (setq file-list (cdr file-list))
	  (setq sub-list (cons elt sub-list))
	  (setq arg-len (+ arg-len (length elt) 1)))
	(setq file-map (append file-map
			       (p4-map-depot-files-int sub-list)))))
    file-map))

(defun p4-map-depot-files-int (file-list)
  (let* ((current-client (p4-current-client))
	 (client-root (p4-get-client-root current-client))
	 (re-current-client (regexp-quote current-client))
	 (re-client-root (regexp-quote client-root))
	 files pmin)
    (save-excursion
      (get-buffer-create p4-output-buffer-name)
      (set-buffer p4-output-buffer-name)
      (goto-char (point-max))
      (setq pmin (point))
      (insert "\n")
      (apply 'p4-call-p4-here "where" file-list)
      (goto-char pmin)
      (if (< (p4-get-server-version) 98)
	  (while (re-search-forward
		  (concat "^\\([^\n]+\\) //" re-current-client
			  "\\(.*\\)$") nil t)
	    (setq files (cons
			 (cons
			  (match-string 1)
			  (concat client-root (match-string 2)))
			 files)))
	(while (re-search-forward
		(concat "^\\([^\n]+\\) //" re-current-client
			"\\([^\n]+\\) \\(" re-client-root ".*\\)$") nil t)
	  (setq files (cons
		       (cons
			(match-string 1) (match-string 3)) files))))
      (delete-region pmin (point-max)))
    files))

(defun p4-make-face (face-name fg bg)
  "Creates a new face if it does not already exist."
  (let ((face (facep face-name)))
    (cond
     ((null face)
      (make-face face-name)
      (if (not (null bg))
	  (set-face-background face-name bg) t)
      (if (not (null fg))
	  (set-face-foreground face-name fg) t)))))

(p4-make-face 'p4-depot-unmapped-face "grey30" nil)
(p4-make-face 'p4-depot-deleted-face "red" nil)
(p4-make-face 'p4-depot-added-face "blue" nil)
(p4-make-face 'p4-depot-branch-op-face "blue4" nil)

(defun p4-make-depot-list-buffer (bufname &optional print-buffer)
  "Take the p4-output-buffer-name buffer, rename it to bufname, and
make all depot file names active, so that clicking them opens
the corresponding client file."
  (let (args files depot-regexp)
    (set-buffer p4-output-buffer-name)
    (goto-char (point-min))
    (setq depot-regexp
	  (if print-buffer
	      "\\(^\\)\\(//[^/@# ][^/@#]*/[^@#]+\\)#[0-9]+ - "
	    "^\\(\\.\\.\\. [^/\n]*\\|==== \\)?\\(//[^/@# ][^/@#]*/[^#\n]*\\)"))
    (while (re-search-forward depot-regexp nil t)
      (setq args (cons (match-string 2) args)))
    (setq files (p4-map-depot-files args))
    (get-buffer-create bufname);; We do these two lines
    (kill-buffer bufname);; to ensure no duplicates
    (set-buffer p4-output-buffer-name)
    (rename-buffer bufname t)
    (goto-char (point-min))
    (while (re-search-forward depot-regexp nil t)
      (let ((p4-client-file (cdr (assoc (match-string 2) files)))
	    (p4-depot-file (match-string 2))
	    (start (match-beginning 2))
	    (end (match-end 2))
	    (branching-op-p (and (match-string 1)
				 (string-match "\\.\\.\\. \\.\\.\\..*"
					       (match-string 1))))
	    prop-list)
	(if (and p4-client-file
		 (file-readable-p p4-client-file))
	    (setq prop-list (list (cons 'link-client-name
					p4-client-file)))
	  (setq prop-list (list (cons 'link-depot-name
				      p4-depot-file))))
	;; some kind of operation related to branching/integration
	(if branching-op-p
	    (setq prop-list (append (list
				     (cons 'history-for p4-depot-file)
				     (cons 'face
					   'p4-depot-branch-op-face))
				    prop-list)))
	(cond
	 ((not p4-client-file)
	  (p4-set-extent-properties
	   start end
	   (append (list (cons 'face 'p4-depot-unmapped-face))
		   prop-list)))
	 ((save-excursion
	    (goto-char end)
	    (looking-at ".* deleted?[ \n]"))
	  (p4-set-extent-properties
	   start end
	   (append (list (cons 'face 'p4-depot-deleted-face))
		   prop-list)))
	 ((save-excursion
	    (goto-char end)
	    (looking-at ".* \\(add\\|branch\\)\\(ed\\)?[ \n]"))
	  (p4-create-active-link
	   start end
	   (append (list (cons 'face 'p4-depot-added-face))
		   prop-list)))
	 (t
	  (p4-create-active-link start end prop-list)))))
    (use-local-map p4-opened-map)
    (setq buffer-read-only t)
    (p4-move-buffer-point-to-top bufname)))

;; The p4 print command
(defp4cmd p4-print ()
  "print" "To print a depot file to a buffer, type \\[p4-print].\n"
  (interactive)
  (let ((arg-string (p4-buffer-file-name-2))
	(rev (get-char-property (point) 'rev))
	(change (get-char-property (point) 'change)))
    (cond (rev
	   (setq arg-string (concat arg-string "#" rev)))
	  (change
	   (setq arg-string (concat arg-string "@" change))))
    (if (or current-prefix-arg (not arg-string))
	(setq arg-string (p4-make-list-from-string
			  (p4-read-arg-string "p4 print: " arg-string)))
      (setq arg-string (list arg-string)))
    (p4-noinput-buffer-action "print" nil t arg-string)
    (p4-activate-print-buffer "*P4 print*" t)))

;; Insert text in a buffer, but make sure that the inserted text doesn't
;; inherit any properties from surrounding text. This is needed for xemacs
;; because the insert function makes the inserted text inherit properties.
(defun p4-insert-no-properties (str)
  (let ((start (point))
	end)
    (insert str)
    (setq end (point))
    (set-text-properties start end nil)))

(defun p4-font-lock-buffer (buf-name)
  (save-excursion
    (let (file-name (first-line ""))
      (set-buffer buf-name)
      (goto-char (point-min))
      (if (looking-at "^//[^#@]+/\\([^/#@]+\\)")
	  (progn
	    (setq file-name (match-string 1))
	    (forward-line 1)
	    (setq first-line (buffer-substring (point-min) (point)))
	    (delete-region (point-min) (point))))
      (setq buffer-file-name file-name)
      (set-auto-mode)
      (setq buffer-file-name nil)
      (condition-case nil
	  (font-lock-fontify-buffer)
	(error nil))
      (fundamental-mode)
      (if (and p4-running-emacs
	       (boundp 'hilit-auto-rehighlight))
	  (setq hilit-auto-rehighlight nil))
      (goto-char (point-min))
      (p4-insert-no-properties first-line))))

(defun p4-activate-print-buffer (buffer-name print-buffer)
  (if print-buffer
      (p4-font-lock-buffer p4-output-buffer-name))
  (p4-make-depot-list-buffer buffer-name print-buffer)
  (let ((depot-regexp
	 (if print-buffer
	     "^\\(//[^/@# ][^/@#]*/\\)[^@#]+#[0-9]+ - "
	   "^\\(//[^/@# ][^/@#]*/\\)")))
    (save-excursion
      (set-buffer buffer-name)
      (setq buffer-read-only nil)
      (goto-char (point-min))
      (while (re-search-forward depot-regexp nil t)
	(let ((link-client-name (get-char-property (match-end 1)
						   'link-client-name))
	      (link-depot-name (get-char-property (match-end 1)
						  'link-depot-name))
	      (start (match-beginning 1))
	      (end (point-max)))
	  (save-excursion
	    (if (re-search-forward depot-regexp nil t)
		(setq end (match-beginning 1))))
	  (if link-client-name
	      (p4-set-extent-properties start end
					(list (cons 'block-client-name
						    link-client-name))))
	  (if link-depot-name
	      (p4-set-extent-properties start end
					(list (cons 'block-depot-name
						    link-depot-name))))))
      (setq buffer-read-only t))))

(defconst p4-blame-change-regex
  (concat "^\\.\\.\\. #"     "\\([0-9]+\\)"   ;; revision
	  "\\s-+change\\s-+" "\\([0-9]+\\)"   ;; change
	  "\\s-+"            "\\([^ \t]+\\)"  ;; type
	  "\\s-+on\\s-+"     "\\([^ \t]+\\)"  ;; date	   
	  "\\s-+by\\s-+"     "\\([^ \t]+\\)"  ;; author
	  "@"))

(defconst p4-blame-branch-regex
  "^\\.\\.\\. \\.\\.\\. branch from \\(//[^#]*\\)#")

(defconst p4-blame-revision-regex
  (concat "^\\([0-9]+\\),?"
	  "\\([0-9]*\\)"
	  "\\([acd]\\)"
	  "\\([0-9]+\\),?"
	  "\\([0-9]*\\)"))

(defconst p4-blame-index-regex
  (concat " *\\([0-9]+\\)"               ;; change
	  " *\\([0-9]+\\)"               ;; revision
	  " *\\([0-9]+/[0-9]+/[0-9]+\\)" ;; date
	  "\\s-+\\([^:]*\\)"             ;; author
	  ":"))          

(defconst P4-REV  0)
(defconst P4-DATE 1)
(defconst P4-AUTH 2)
(defconst P4-FILE 3)

(defun p4-blame ()
  "To Print a depot file with revision history to a buffer,
type \\[p4-blame]"
  (interactive)
  (let ((arg-string (p4-buffer-file-name-2))
	(rev (get-char-property (point) 'rev))
	(change (get-char-property (point) 'change)))
    (cond (rev
	   (setq arg-string (concat arg-string "#" rev)))
	  (change
	   (setq arg-string (concat arg-string "@" change))))
    (if (or current-prefix-arg (not arg-string))
	(setq arg-string (p4-read-arg-string "p4 print-revs: " arg-string)))
    (p4-blame-int arg-string)))

(defalias 'p4-print-with-rev-history 'p4-blame)

(defun p4-blame-int (file-spec)
  (get-buffer-create p4-output-buffer-name);; We do these two lines
  (kill-buffer p4-output-buffer-name)      ;; to ensure no duplicates
  (let ((file-name file-spec)
	(buffer (get-buffer-create p4-output-buffer-name))
	head-name  ;; file spec of the head revision for this blame assignment
	branch-p   ;; have we tracked into a branch?
	cur-file   ;; file name of the current branch during blame assignment
	change ch-alist fullname head-rev headseen)

    ;; we asked for blame constrained by a change number
    (if (string-match "\\(.*\\)@\\([0-9]+\\)" file-spec)
	(progn
	  (setq file-name (match-string 1 file-spec))
	  (setq change (string-to-int (match-string 2 file-spec)))))

    ;; we asked for blame constrained by a revision
    (if (string-match "\\(.*\\)#\\([0-9]+\\)" file-spec)
	(progn
	  (setq file-name (match-string 1 file-spec))
	  (setq head-rev (string-to-int (match-string 2 file-spec)))))

    ;; make sure the filespec is unambiguous
    (p4-exec-p4 buffer (list "files" file-name) t)
    (save-excursion
      (set-buffer buffer)
      (if (> (count-lines (point-min) (point-max)) 1)
	  (error "File pattern maps to more than one file.")))

    ;; get the file change history:
    (p4-exec-p4 buffer (list "filelog" "-i" file-spec) t)
    (setq fullname (p4-read-depot-output buffer)
	  cur-file  fullname
	  head-name fullname)

    ;; parse the history:
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (while (< (point) (point-max))

	;; record the current file name (and the head file name,
	;; if we have not yet seen one):
	(if (looking-at "^\\(//.*\\)$")
	    (setq cur-file (match-string 1)))

	;; a non-branch change:
	(if (looking-at p4-blame-change-regex)
	    (let ((rev (string-to-int (match-string 1)))
		  (ch (string-to-int (match-string 2)))
		  (op (match-string 3))
		  (date (match-string 4))
		  (author (match-string 5)))
	      (cond
	       ;; after the change constraint, OR
	       ;; after the revision constraint _for this file_
	       ;;   [remember, branches complicate this]:
	       ((or (and change   (< change ch))
		    (and head-rev (< head-rev rev)
			 (string= head-name cur-file))) nil)
	       
	       ;; file has been deleted, can't assign blame:
	       ((string= op "delete") 
		(if (not headseen) (goto-char (point-max))))

	       ;; OK, we actually want to look at this one:
	       (t
		(setq ch-alist
		      (cons
		       (cons ch (list rev date author cur-file)) ch-alist))
		(if (not head-rev) (setq head-rev rev))
		(setq headseen t)) ))

	  ;; not if we have entered a branch (this used to be used, isn't
	  ;; right now - maybe again later:
	  (if (and headseen (looking-at p4-blame-branch-regex))
	      (setq branch-p t)) )
	(forward-line)))
    
    (if (< (length ch-alist) 1)
	(error "Head revision not available"))
  
    (let ((base-ch (int-to-string (caar ch-alist)))
	  (ch-buffer (get-buffer-create "p4-ch-buf"))
	  (tmp-alst (copy-alist ch-alist)))
      (p4-exec-p4 ch-buffer
		  (list "print" "-q" (concat cur-file "@" base-ch)) t)
      (save-excursion
	(set-buffer ch-buffer)
	(goto-char (point-min))
	(while (re-search-forward ".*\n" nil t)
	  (replace-match (concat base-ch "\n"))))
      (while (> (length tmp-alst) 1)
	(let ((ch-1 (car (car  tmp-alst)))
	      (ch-2 (car (cadr tmp-alst)))
	      (file1 (nth P4-FILE (cdr (car  tmp-alst))))
	      (file2 (nth P4-FILE (cdr (cadr tmp-alst))))
	      ins-string)
	  (setq ins-string (format "%d\n" ch-2))
	  (p4-exec-p4 buffer (list "diff2"
				   (format "%s@%d" file1 ch-1)
				   (format "%s@%d" file2 ch-2)) t)
	  (save-excursion
	    (set-buffer buffer)
	    (goto-char (point-max))
	    (while (re-search-backward p4-blame-revision-regex nil t)
	      (let ((la (string-to-int (match-string 1)))
		    (lb (string-to-int (match-string 2)))
		    (op (match-string 3))
		    (ra (string-to-int (match-string 4)))
		    (rb (string-to-int (match-string 5))))
		(if (= lb 0)
		    (setq lb la))
		(if (= rb 0)
		    (setq rb ra))
		(cond ((string= op "a")
		       (setq la (1+ la)))
		      ((string= op "d")
		       (setq ra (1+ ra))))
		(save-excursion
		  (set-buffer ch-buffer)
		  (goto-line la)
		  (let ((beg (point)))
		    (forward-line (1+ (- lb la)))
		    (delete-region beg (point)))
		  (while (<= ra rb)
		    (insert ins-string)
		    (setq ra (1+ ra)))))))
	  (setq tmp-alst (cdr tmp-alst))))
      (p4-noinput-buffer-action "print" nil t
				(list (format "%s#%d" fullname head-rev))
				t)
      (p4-font-lock-buffer p4-output-buffer-name)
      (let (line cnum (old-cnum 0) change-data
	    xth-rev xth-date xth-auth xth-file)
	(save-excursion
	  (set-buffer buffer)
	  (goto-line 2)
	  (move-to-column 0)
	  (p4-insert-no-properties "Change  Rev       Date  Author\n")
	  (while (setq line (p4-read-depot-output ch-buffer))
	    (setq cnum (string-to-int line))
	    (if (= cnum old-cnum)
		(p4-insert-no-properties (format "%29s : " ""))

	      ;; extract the change data from our alist: remember,
	      ;; `eq' works for integers so we can use assq here:
	      (setq change-data (cdr (assq cnum ch-alist))
		    xth-rev     (nth P4-REV  change-data)
		    xth-date    (nth P4-DATE change-data)
		    xth-auth    (nth P4-AUTH change-data)
		    xth-file    (nth P4-FILE change-data))
	      
	      (p4-insert-no-properties
	       (format "%6d %4d %10s %7s: " cnum xth-rev xth-date xth-auth))
	      (move-to-column 0)
	      (if (looking-at p4-blame-index-regex)
		  (let ((nth-cnum (match-string 1))
			(nth-revn (match-string 2))
			(nth-user (match-string 4)))
		    (p4-create-active-link (match-beginning 1)
					   (match-end 1)
					   (list (cons 'change nth-cnum)))
		    ;; revision needs to be linked to a file now that we
		    ;; follow integrations (branches):
		    (p4-create-active-link (match-beginning 2)
					   (match-end 2)
					   (list (cons 'rev  nth-revn)
						 (cons 'link-depot-name xth-file)))
		    (p4-create-active-link (match-beginning 4)
					   (match-end 4)
					   (list (cons 'user nth-user)))
		    ;; truncate the user name:
		    (let ((start (+ (match-beginning 4) 7))
			  (end (match-end 4)))
		      (if (> end start)
			  (delete-region start end))))))
	    (setq old-cnum cnum)
	    (forward-line))))

      (kill-buffer ch-buffer))
    (let ((buffer-name (concat "*P4 print-revs " file-name "*")))
      (p4-activate-print-buffer buffer-name nil)
      (save-excursion
	(set-buffer buffer-name)
	(setq truncate-lines t)
	(use-local-map p4-print-rev-map)))))

;; The p4 refresh command
(defp4cmd p4-refresh ()
  "sync" "Refresh the contents of an unopened file. \\[p4-refresh].

This is equivalent to \"sync -f\"
"
  (interactive)
  (let ((args (p4-buffer-file-name)))
    (if (or current-prefix-arg (not args))
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 refresh: ")))
      (setq args (list args)))
    (p4-noinput-buffer-action "refresh" nil t args)
    (p4-refresh-files-in-buffers)
    (p4-make-depot-list-buffer
     (concat "*P4 Refresh: (" (p4-current-client) ") " (car args) "*"))))

;; The p4 get/sync command
(defp4cmd p4-sync ()
  "sync"
  "To synchronise the local view with the depot, type \\[p4-get].\n"
  (interactive)
  (p4-get))

(defp4cmd p4-get ()
  "sync"
  "To synchronise the local view with the depot, type \\[p4-get].\n"
  (interactive)
  (let (args)
    (if current-prefix-arg
	(setq args (p4-make-list-from-string (p4-read-arg-string "p4 get: "))))
    (p4-noinput-buffer-action "get" nil t args)
    (p4-refresh-files-in-buffers)
    (p4-make-depot-list-buffer
     (concat "*P4 Get: (" (p4-current-client) ") " (car args) "*"))))

;; The p4 have command
(defp4cmd p4-have ()
  "have" "To list revisions last gotten, type \\[p4-have].\n"
  (interactive)
  (let ((args (list "...")))
    (if current-prefix-arg
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 have: " (p4-buffer-file-name-2)))))
    (p4-noinput-buffer-action "have" nil t args)
    (p4-make-depot-list-buffer
     (concat "*P4 Have: (" (p4-current-client) ") " (car args) "*"))))

;; The p4 changes command
(defp4cmd p4-changes ()
  "changes" "To list changes, type \\[p4-changes].\n"
  (interactive)
  (let ((arg-list (list "-m" "200" "...")))
    (if current-prefix-arg
	(setq arg-list (p4-make-list-from-string
			(p4-read-arg-string "p4 changes: " "-m 200"))))
    (p4-file-change-log "changes" arg-list)))

;; The p4 help command
(defp4cmd p4-help (arg)
  "help" "To print help message, type \\[p4-help].

Argument ARG command for which help is needed.
"
  (interactive (list (p4-make-list-from-string
		      (p4-read-arg-string "Help on which command: "
					  nil "help"))))
  (p4-noinput-buffer-action "help" nil t arg)
  (p4-make-basic-buffer "*P4 help*"))

(defun p4-make-basic-buffer (buf-name &optional map)
  "rename `p4-output-buffer-name' to buf-name \(which will be killed first if
it already exists\), set its local map to map, if specified, or
`p4-basic-map' otherwise. Makes the buffer read only."
  (get-buffer-create buf-name)
  (kill-buffer buf-name)
  (set-buffer p4-output-buffer-name)
  (goto-char (point-min))
  (rename-buffer buf-name t)
  (use-local-map (if (keymapp map) map p4-basic-map))
  (setq buffer-read-only t)
  (p4-move-buffer-point-to-top buf-name))

;; The p4 info command
(defp4cmd p4-info ()
  "info" "To print out client/server information, type \\[p4-info].\n"
  (interactive)
  (p4-noinput-buffer-action "info" nil t)
  (p4-make-basic-buffer "*P4 info*"))

;; The p4 integrate command
(defp4cmd p4-integ ()
  "integ" "To schedule integrations between branches, type \\[p4-integ].\n"
  (interactive)
  (let ((args (p4-make-list-from-string
	       (p4-read-arg-string "p4 integ: " "-b "))))
    (p4-noinput-buffer-action "integ" nil t args)
    (p4-make-depot-list-buffer "*P4 integ*")))

(defp4cmd p4-resolve ()
  "resolve"
  "To merge open files with other revisions or files, type \\[p4-resolve].\n"
  (interactive)
  (let (buffer args (buf-name "*p4 resolve*"))
    (if current-prefix-arg
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 resolve: " nil))))
    (setq buffer (get-buffer buf-name))
    (if (and (buffer-live-p buffer)
	     (not (comint-check-proc buffer)))
	(save-excursion
	  (let ((cur-dir default-directory))
	    (set-buffer buffer)
	    (cd cur-dir)
	    (goto-char (point-max))
	    (insert "\n--------\n\n"))))
    (setq args (cons "resolve" args))
    (setq buffer (apply 'make-comint "p4 resolve" p4-executable nil "-d" default-directory args))
    (set-buffer buffer)
    (comint-mode)
    (display-buffer buffer)
    (select-window (get-buffer-window buffer))
    (goto-char (point-max))))

(defp4cmd p4-rename ()
  "rename" "To rename a file in the depot, type \\[p4-rename].

This command will execute the integrate/delete commands automatically.
"
  (interactive)
  (let (from-file to-file)
    (setq from-file (p4-read-arg-string "rename from: " (p4-buffer-file-name-2)))
    (setq to-file (p4-read-arg-string "rename to: " (p4-buffer-file-name-2)))
    (p4-noinput-buffer-action "integ" nil t (list from-file to-file))
    (p4-exec-p4 (get-buffer-create p4-output-buffer-name)
		(list "delete" from-file)
		nil)))

(defun p4-scroll-down-1-line ()
  "Scroll down one line"
  (interactive)
  (scroll-down 1))

(defun p4-scroll-up-1-line ()
  "Scroll up one line"
  (interactive)
  (scroll-up 1))

(defun p4-scroll-down-1-window ()
  "Scroll down one window"
  (interactive)
  (scroll-down
   (- (window-height) next-screen-context-lines)))

(defun p4-scroll-up-1-window ()
  "Scroll up one window"
  (interactive)
  (scroll-up
   (- (window-height) next-screen-context-lines)))

(defun p4-top-of-buffer ()
  "Top of buffer"
  (interactive)
  (goto-char (point-min)))

(defun p4-bottom-of-buffer ()
  "Bottom of buffer"
  (interactive)
  (goto-char (point-max)))

(defun p4-delete-other-windows ()
  "Make buffer full height"
  (interactive)
  (delete-other-windows))

(defun p4-goto-next-diff ()
  "Next diff"
  (interactive)
  (goto-char (window-start))
  (if (= (point) (point-max))
      (error "At bottom"))
  (forward-line 1)
  (re-search-forward "^====" nil "")
  (beginning-of-line)
  (set-window-start (selected-window) (point)))

(defun p4-goto-prev-diff ()
  "Previous diff"
  (interactive)
  (if (= (point) (point-min))
      (error "At top"))
  (goto-char (window-start))
  (re-search-backward "^====" nil "")
  (set-window-start (selected-window) (point)))

(defun p4-next-depot-file ()
  "Next file"
  (interactive)
  (goto-char (window-start))
  (if (= (point) (point-max))
      (error "At bottom"))
  (forward-line 1)
  (re-search-forward "^//[^/@# ][^/@#]*/[^@#]+#[0-9]+ - " nil "")
  (beginning-of-line)
  (set-window-start (selected-window) (point)))

(defun p4-prev-depot-file ()
  "Previous file"
  (interactive)
  (if (= (point) (point-min))
      (error "At top"))
  (goto-char (window-start))
  (re-search-backward "^//[^/@# ][^/@#]*/[^@#]+#[0-9]+ - " nil "")
  (set-window-start (selected-window) (point)))


(defun p4-next-depot-diff ()
  "Next diff"
  (interactive)
  (goto-char (window-start))
  (if (= (point) (point-max))
      (error "At bottom"))
  (forward-line 1)
  (re-search-forward "^\\(@@\\|\\*\\*\\* \\|[0-9]+[,acd]\\)" nil "")
  (beginning-of-line)
  (set-window-start (selected-window) (point)))

(defun p4-prev-depot-diff ()
  "Previous diff"
  (interactive)
  (if (= (point) (point-min))
      (error "At top"))
  (goto-char (window-start))
  (re-search-backward "^\\(@@\\|\\*\\*\\* \\|[0-9]+[,acd]\\)" nil "")
  (set-window-start (selected-window) (point)))

(defun p4-moveto-print-rev-column (old-column)
  (let ((colon (save-excursion
		 (move-to-column 0)
		 (if (looking-at "[^:\n]*:")
		     (progn
		       (goto-char (match-end 0))
		       (current-column))
		   0))))
    (move-to-column old-column)
    (if (and (< (current-column) colon)
	     (re-search-forward "[^ ][ :]" nil t))
	(goto-char (match-beginning 0)))))

(defun p4-next-change-rev-line ()
  "Next change/revision line"
  (interactive)
  (let ((c (current-column)))
    (move-to-column 1)
    (re-search-forward "^ *[0-9]+ +[0-9]+[^:]+:" nil "")
    (p4-moveto-print-rev-column c)))

(defun p4-prev-change-rev-line ()
  "Previous change/revision line"
  (interactive)
  (let ((c (current-column)))
    (forward-line -1)
    (move-to-column 32)
    (re-search-backward "^ *[0-9]+ +[0-9]+[^:]*:" nil "")
    (p4-moveto-print-rev-column c)))

(defun p4-toggle-line-wrap ()
  "Toggle line wrap mode"
  (interactive)
  (setq truncate-lines (not truncate-lines))
  (save-window-excursion
    (recenter)))

(defun p4-quit-current-buffer (pnt)
  "Quit a buffer"
  (interactive "d")
  (if (not (one-window-p))
      (delete-window)
    (bury-buffer)))

(defun p4-buffer-mouse-clicked (event)
  "Function to translate the mouse clicks in a P4 filelog buffer to
character events"
  (interactive "e")
  (let (win pnt)
    (cond (p4-running-xemacs
	   (setq win (event-window event))
	   (setq pnt (event-point event)))
	  (p4-running-emacs
	   (setq win (posn-window (event-end event)))
	   (setq pnt (posn-point (event-start event)))))
    (select-window win)
    (goto-char pnt)
    (p4-buffer-commands pnt)))

(defun p4-buffer-mouse-clicked-3 (event)
  "Function to translate the mouse clicks in a P4 filelog buffer to
character events"
  (interactive "e")
  (let (win pnt)
    (cond (p4-running-xemacs
	   (setq win (event-window event))
	   (setq pnt (event-point event)))
	  (p4-running-emacs
	   (setq win (posn-window (event-end event)))
	   (setq pnt (posn-point (event-start event)))))
    (select-window win)
    (goto-char pnt)
    (let ((link-name (or (get-char-property pnt 'link-client-name)
			 (get-char-property pnt 'link-depot-name)))
	  (rev (get-char-property pnt 'rev)))
      (cond (link-name
	     (p4-diff))
	    (rev
	     (p4-diff2 rev "#head"))
	    (t
	     (error "No file to diff!"))))))

(defun p4-buffer-commands (pnt)
  "Function to get a given property and do the appropriate command on it"
  (interactive "d")
  (let ((rev (get-char-property pnt 'rev))
	(change (get-char-property pnt 'change))
	(action (get-char-property pnt 'action))
	(user (get-char-property pnt 'user))
	(group (get-char-property pnt 'group))
	(client (get-char-property pnt 'client))
	(label (get-char-property pnt 'label))
	(branch (get-char-property pnt 'branch))
	(filename (p4-buffer-file-name-2)))
    (cond ((and (not action) rev)
	   (let ((fn1 (concat filename "#" rev)))
	     (p4-noinput-buffer-action "print" nil t (list fn1))
	     (p4-activate-print-buffer "*P4 print*" t)))
	  (action
	   (let* ((rev2 (int-to-string (1- (string-to-int rev))))
		  (fn1 (concat filename "#" rev))
		  (fn2 (concat filename "#" rev2)))
	     (if (> (string-to-int rev2) 0)
		 (progn
		   (p4-noinput-buffer-action
		    "diff2" nil t
		    (append (p4-make-list-from-string
			     p4-default-diff-options)
			    (list fn2 fn1)))
		   (p4-activate-diff-buffer "*P4 diff*"))
	       (error "There is no earlier revision to diff."))))
	  (change (p4-describe-internal
		   (append (p4-make-list-from-string p4-default-diff-options)
			   (list change))))
	  (user (p4-async-process-command "user" nil
					  (concat
					   "*P4 User: " user "*")
					  "user" (list user)))
	  (client (p4-async-process-command
		   "client" "Description:\n\t"
		   (concat "*P4 Client: " client "*") "client" (list client)))
	  (label (p4-label (list label)))
	  (branch (p4-branch (list branch)))

	  ;; Check if a "filename link" or an active "diff buffer area" was
	  ;; selected.
	  (t
	   (let ((link-client-name (get-char-property pnt 'link-client-name))
		 (link-depot-name (get-char-property pnt 'link-depot-name))
		 (block-client-name (get-char-property pnt 'block-client-name))
		 (block-depot-name (get-char-property pnt 'block-depot-name))
		 (p4-history-for (get-char-property pnt 'history-for))
		 (first-line (get-char-property pnt 'first-line))
		 (start (get-char-property pnt 'start)))
	     (cond
	      (p4-history-for
	       (p4-file-change-log "filelog" (list p4-history-for)))
	      ((or link-client-name link-depot-name)
	       (p4-find-file-or-print-other-window
		link-client-name link-depot-name))
	      ((or block-client-name block-depot-name)
	       (if first-line
		   (let ((c (max 0 (- pnt
				      (save-excursion
					(goto-char pnt)
					(beginning-of-line)
					(point))
				      1)))
			 (r first-line))
		     (save-excursion
		       (goto-char start)
		       (while (re-search-forward "^[ +>].*\n" pnt t)
			 (setq r (1+ r))))
		     (p4-find-file-or-print-other-window
		      block-client-name block-depot-name)
		     (goto-line r)
		     (if (not block-client-name)
			 (forward-line 1))
		     (beginning-of-line)
		     (goto-char (+ (point) c)))
		 (p4-find-file-or-print-other-window
		  block-client-name block-depot-name)))
	      (t
	       (error "There is no file at that cursor location!"))))))))

(defun p4-find-file-or-print-other-window (client-name depot-name)
  (if client-name
      (find-file-other-window client-name)
    (p4-noinput-buffer-action "print" nil t
			      (list depot-name))
    (p4-activate-print-buffer depot-name t)
    (other-window 1)))

(defun p4-find-file-other-window ()
  "Open/print file"
  (interactive)
  (let ((link-client-name (get-char-property (point) 'link-client-name))
	(link-depot-name (get-char-property (point) 'link-depot-name))
	(block-client-name (get-char-property (point) 'block-client-name))
	(block-depot-name (get-char-property (point) 'block-depot-name)))
    (cond ((or link-client-name link-depot-name)
	   (p4-find-file-or-print-other-window
	    link-client-name link-depot-name)
	   (other-window 1))
	  ((or block-client-name block-depot-name)
	   (p4-find-file-or-print-other-window
	    block-client-name block-depot-name)
	   (other-window 1)))))

(defun p4-filelog-short-format ()
  "Short format"
  (interactive)
  (setq buffer-invisibility-spec t)
  (redraw-display))

(defun p4-filelog-long-format ()
  "Long format"
  (interactive)
  (setq buffer-invisibility-spec (list))
  (redraw-display))

(defun p4-scroll-down-1-line-other-w ()
  "Scroll other window down one line"
  (interactive)
  (scroll-other-window -1))

(defun p4-scroll-up-1-line-other-w ()
  "Scroll other window up one line"
  (interactive)
  (scroll-other-window 1))

(defun p4-scroll-down-1-window-other-w ()
  "Scroll other window down one window"
  (interactive)
  (scroll-other-window
   (- next-screen-context-lines (window-height))))

(defun p4-scroll-up-1-window-other-w ()
  "Scroll other window up one window"
  (interactive)
  (scroll-other-window
   (- (window-height) next-screen-context-lines)))

(defun p4-top-of-buffer-other-w ()
  "Top of buffer, other window"
  (interactive)
  (other-window 1)
  (goto-char (point-min))
  (other-window -1))

(defun p4-bottom-of-buffer-other-w ()
  "Bottom of buffer, other window"
  (interactive)
  (other-window 1)
  (goto-char (point-max))
  (other-window -1))

(defun p4-goto-next-change ()
  "Next change"
  (interactive)
  (let ((c (current-column)))
    (forward-line 1)
    (while (get-char-property (point) 'invisible)
      (forward-line 1))
    (move-to-column c)))

(defun p4-goto-prev-change ()
  "Previous change"
  (interactive)
  (let ((c (current-column)))
    (forward-line -1)
    (while (get-char-property (point) 'invisible)
      (forward-line -1))
    (move-to-column c)))


;; Activate special handling for a buffer generated with a diff-like command
(p4-make-face 'p4-diff-file-face nil "gray90")
(p4-make-face 'p4-diff-head-face nil "gray95")
(p4-make-face 'p4-diff-ins-face "blue" nil)
(p4-make-face 'p4-diff-del-face "red" nil)
(p4-make-face 'p4-diff-change-face "dark green" nil)

(defun p4-buffer-set-face-property (regexp face-property)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (let ((start (match-beginning 0))
	    (end (match-end 0)))
	(p4-set-extent-properties start end
				  (list (cons 'face face-property)))))))

(defun p4-activate-diff-buffer (buffer-name)
  (p4-make-depot-list-buffer buffer-name)
  (save-excursion
    (set-buffer buffer-name)
    (setq buffer-read-only nil)
    (if p4-colorized-diffs
	(progn
	  (p4-buffer-set-face-property "^=.*\n" 'p4-diff-file-face)
	  (p4-buffer-set-face-property "^[@*].*" 'p4-diff-head-face)
	  (p4-buffer-set-face-property "^\\([+>].*\n\\)+" 'p4-diff-ins-face)
	  (p4-buffer-set-face-property "^\\([-<].*\n\\)+" 'p4-diff-del-face)
	  (p4-buffer-set-face-property "^\\(!.*\n\\)+" 'p4-diff-change-face)))

    (goto-char (point-min))
    (while (re-search-forward "^\\(==== //\\).*\n"
			      nil t)
      (let* ((link-client-name (get-char-property (match-end 1) 'link-client-name))
	     (link-depot-name (get-char-property (match-end 1) 'link-depot-name))
	     (start (match-beginning 0))
	     (end (save-excursion
		    (if (re-search-forward "^==== " nil t)
			(match-beginning 0)
		      (point-max)))))
	(if link-client-name
	    (p4-set-extent-properties start end
				      (list (cons 'block-client-name
						  link-client-name))))
	(if link-depot-name
	    (p4-set-extent-properties start end
				      (list (cons 'block-depot-name
						  link-depot-name))))))

    (goto-char (point-min))
    (while (re-search-forward
	    (concat "^[@0-9].*\\([cad+]\\)\\([0-9]*\\).*\n"
		    "\\(\\(\n\\|[^@0-9\n].*\n\\)*\\)") nil t)
      (let ((first-line (string-to-int (match-string 2)))
	    (start (match-beginning 3))
	    (end (match-end 3)))
	(p4-set-extent-properties start end
				  (list (cons 'first-line first-line)
					(cons 'start start)))))

    (goto-char (point-min))
    (let ((stop
	   (if (re-search-forward "^\\(\\.\\.\\.\\|====\\)" nil t)
	       (match-beginning 0)
	     (point-max))))
      (p4-find-change-numbers buffer-name (point-min) stop))

    (goto-char (point-min))
    (if (looking-at "^Change [0-9]+ by \\([^ @]+\\)@\\([^ \n]+\\)")
	(let ((user-match 1)
	      (cl-match 2)
	      cur-user cur-client)
	  (setq cur-user (match-string user-match))
	  (setq cur-client (match-string cl-match))
	  (p4-create-active-link (match-beginning user-match)
				 (match-end user-match)
				 (list (cons 'user cur-user)))
	  (p4-create-active-link (match-beginning cl-match)
				 (match-end cl-match)
				 (list (cons 'client cur-client)))))

    (use-local-map p4-diff-map)
    (setq buffer-read-only t)))


;; The p4 describe command
(defp4cmd p4-describe ()
  "describe" "To get a description for a change number, type \\[p4-describe].\n"
  (interactive)
  (let ((arg-string (p4-make-list-from-string
		     (read-string "p4 describe: "
				  (concat p4-default-diff-options " ")))))
    (p4-describe-internal arg-string)))

;; Internal version of the p4 describe command
(defun p4-describe-internal (arg-string)
  (p4-noinput-buffer-action
   "describe" nil t arg-string)
  (p4-activate-diff-buffer
   (concat "*P4 describe: " (p4-list-to-string arg-string) "*")))

;; The p4 opened command
(defp4cmd p4-opened ()
  "opened"
  "To display list of files opened for pending change, type \\[p4-opened].\n"
  (interactive)
  (let (args)
    (if current-prefix-arg
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 opened: "
					(p4-buffer-file-name-2)))))
    (p4-opened-internal args)))

(defun p4-opened-internal (args)
  (let ((p4-client (p4-current-client)))
    (p4-noinput-buffer-action "opened" nil t args)
    (p4-make-depot-list-buffer (concat "*Opened Files: " p4-client "*"))))

(defun p4-update-opened-list ()
  (if (get-buffer-window (concat "*Opened Files: " (p4-current-client) "*"))
      (progn
	(setq current-prefix-arg nil)
	(p4-opened-internal nil))))

(defun p4-regexp-create-links (buffer-name regexp property)
  (save-excursion
    (set-buffer buffer-name)
    (setq buffer-read-only nil)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (let ((start (match-beginning 1))
	    (end (match-end 1))
	    (str (match-string 1)))
	(p4-create-active-link start end (list (cons property str)))))
    (setq buffer-read-only t)))

;; The p4 users command
(defp4cmd p4-users ()
  "users" "To display list of known users, type \\[p4-users].\n"
  (interactive)
  (let (args)
    (if current-prefix-arg
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 users: " nil "user"))))
    (p4-noinput-buffer-action "users" nil t args))
  (p4-make-basic-buffer "*P4 users*")
  (p4-regexp-create-links "*P4 users*" "^\\([^ ]+\\).*\n" 'user))

(defp4cmd p4-groups ()
  "groups" "To display list of known groups, type \\[p4-groups].\n"
  (interactive)
  (let (args)
    (if current-prefix-arg
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 groups: " nil "group"))))
    (p4-noinput-buffer-action "groups" nil t args))
  (p4-make-basic-buffer "*P4 groups*")
  (p4-regexp-create-links "*P4 groups*" "^\\(.*\\)\n" 'group))

;; The p4 jobs command
(defp4cmd p4-jobs ()
  "jobs" "To display list of jobs, type \\[p4-jobs].\n"
  (interactive)
  (let (args)
    (if current-prefix-arg
	(setq args (p4-make-list-from-string (p4-read-arg-string "p4 jobs: "))))
    (p4-noinput-buffer-action "jobs" nil t args))
  (p4-make-basic-buffer "*P4 jobs*"))

;; The p4 fix command
(defp4cmd p4-fix ()
  "fix" "To mark jobs as being fixed by a changelist number, type \\[p4-fix].\n"
  (interactive)
  (let ((args (p4-make-list-from-string (p4-read-arg-string "p4 fix: "
							    nil "job"))))
    (p4-noinput-buffer-action "fix" nil t args)))

;; The p4 fixes command
(defp4cmd p4-fixes ()
  "fixes" "To list what changelists fix what jobs, type \\[p4-fixes].\n"
  (interactive)
  (let (args)
    (if current-prefix-arg
	(setq args (p4-make-list-from-string (p4-read-arg-string "p4 fixes: "))))
    (p4-noinput-buffer-action "fixes" nil t args)
    (p4-make-basic-buffer "*P4 fixes*")))

;; The p4 where command
(defp4cmd p4-where ()
  "where"
  "To show how local file names map into depot names, type \\[p4-where].\n"
  (interactive)
  (let (args)
    (if current-prefix-arg
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 where: "
					(p4-buffer-file-name-2)))))
    (p4-noinput-buffer-action "where" nil 's args)))


(defun p4-async-process-command (p4-this-command &optional
						 p4-regexp
						 p4-this-buffer
						 p4-out-command
						 p4-in-args
						 p4-out-args)
  "Internal function to call an asynchronous process with a local buffer,
instead of calling an external client editor to run within emacs.

Arguments:
P4-THIS-COMMAND is the command that called this internal function.

P4-REGEXP is the optional regular expression to search for to set the cursor
on.

P4-THIS-BUFFER is the optional buffer to create. (Default is *P4 <command>*).

P4-OUT-COMMAND is the optional command that will be used as the command to
be called when `p4-async-call-process' is called.

P4-IN-ARGS is the optional argument passed that will be used as the list of
arguments to the P4-THIS-COMMAND.

P4-OUT-ARGS is the optional argument passed that will be used as the list of
arguments to P4-OUT-COMMAND."
  (let ((dir default-directory))
    (if p4-this-buffer
	(set-buffer (get-buffer-create p4-this-buffer))
      (set-buffer (get-buffer-create (concat "*P4 " p4-this-command "*"))))
    (setq p4-current-command p4-this-command)
    (cd dir))
  (if (zerop (apply 'call-process-region (point-min) (point-max)
		    (p4-check-p4-executable) t t nil
		    "-d" default-directory
		    p4-current-command "-o"
		    p4-in-args))
      (progn
	(goto-char (point-min))
	(insert (concat "# Created using " (p4-emacs-version) ".\n"
			"# Type C-c C-c to submit changes and exit buffer.\n"
			"# Type C-x k to kill current changes.\n"
			"#\n"))
	(if p4-regexp (re-search-forward p4-regexp))
	(indented-text-mode)
	(setq p4-async-minor-mode t)
	(setq fill-column 79)
	(p4-push-window-config)
	(switch-to-buffer-other-window (current-buffer))
	(if p4-out-command
	    (setq p4-current-command p4-out-command))
	(setq p4-current-args p4-out-args)
	(setq buffer-offer-save t)

	(define-key p4-async-minor-map "\C-c\C-c" 'p4-async-call-process)
	(run-hooks 'p4-async-command-hook)
	(set-buffer-modified-p nil)
	(message "C-c C-c to finish editing and exit buffer."))
    (error "%s %s -o failed to complete successfully."
	   (p4-check-p4-executable) p4-current-command)))

(defun p4-async-call-process ()
  "Internal function called by `p4-async-process-command' to process the
buffer after editing is done using the minor mode key mapped to `C-c C-c'."
  (interactive)
  (message "p4 %s ..." p4-current-command)
  (let ((max (point-max)) msg
	(current-command p4-current-command)
	(current-args p4-current-args))
    (goto-char max)
    (if (zerop (apply 'call-process-region (point-min)
		      max (p4-check-p4-executable)
		      nil '(t t) nil
		      "-d" default-directory
		      current-command "-i"
		      current-args))
	(progn
	  (goto-char max)
	  (setq msg (buffer-substring max (point-max)))
	  (delete-region max (point-max))
	  (save-excursion
	    (set-buffer (get-buffer-create p4-output-buffer-name))
	    (delete-region (point-min) (point-max))
	    (insert msg))
	  (kill-buffer nil)
	  (display-buffer p4-output-buffer-name)
	  (p4-partial-cache-cleanup current-command)
	  (message "p4 %s done." current-command)
	  (if (equal current-command "submit")
	      (progn
		(p4-refresh-files-in-buffers)
		(p4-check-mode-all-buffers)
		(if p4-notify
		    (p4-notify p4-notify-list)))))
      (error "%s %s -i failed to complete successfully."
	     (p4-check-p4-executable)
	     current-command))))

(defun p4-cmd-line-flags (args)
  (memq t (mapcar (lambda (x) (not (not (string-match "^-" x))))
		  args)))

;; The p4 change command
(defp4cmd p4-change ()
  "change" "To edit the change specification, type \\[p4-change].\n"
  (interactive)
  (let (args
	(change-buf-name "*P4 New Change*"))
    (if (buffer-live-p (get-buffer change-buf-name))
	(switch-to-buffer-other-window (get-buffer change-buf-name))
      (if current-prefix-arg
	  (setq args (p4-make-list-from-string
		      (p4-read-arg-string "p4 change: " nil))))
      (if (p4-cmd-line-flags args)
	  (p4-noinput-buffer-action "change" nil t args)
	(p4-async-process-command "change" "Description:\n\t"
				  change-buf-name nil args)))))

;; The p4 client command
(defp4cmd p4-client ()
  "client" "To edit a client specification, type \\[p4-client].\n"
  (interactive)
  (let (args
	(client-buf-name "*P4 client*"))
    (if (buffer-live-p (get-buffer client-buf-name))
	(switch-to-buffer-other-window (get-buffer client-buf-name))
      (if current-prefix-arg
	  (setq args (p4-make-list-from-string
		      (p4-read-arg-string "p4 client: " nil "client"))))
      (if (p4-cmd-line-flags args)
	  (p4-noinput-buffer-action "client" nil t args)
	(p4-async-process-command "client" "\\(Description\\|View\\):\n\t"
				  client-buf-name nil args)))))

(defp4cmd p4-clients ()
  "clients" "To list all clients, type \\[p4-clients].\n"
  (interactive)
  (p4-noinput-buffer-action "clients" nil t nil)
  (p4-make-basic-buffer "*P4 clients*")
  (p4-regexp-create-links "*P4 clients*" "^Client \\([^ ]+\\).*\n" 'client))

(defp4cmd p4-branch (args)
  "branch" "Edit a P4-BRANCH specification using \\[p4-branch]."
  (interactive (list
		(p4-make-list-from-string
		 (p4-read-arg-string "p4 branch: " nil "branch"))))
  (if (or (null args) (equal args (list "")))
      (error "Branch must be specified!")
    (if (p4-cmd-line-flags args)
	(p4-noinput-buffer-action "branch" nil t args)
      (p4-async-process-command "branch" "Description:\n\t"
				(concat "*P4 Branch: "
					(car (reverse args)) "*")
				"branch" args))))

(defp4cmd p4-branches ()
  "branches" "To list all branches, type \\[p4-branches].\n"
  (interactive)
  (p4-noinput-buffer-action "branches" nil t nil)
  (p4-make-basic-buffer "*P4 branches*")
  (p4-regexp-create-links "*P4 branches*" "^Branch \\([^ ]+\\).*\n" 'branch))

(defp4cmd p4-label (args)
  "label" "Edit a P4-label specification using \\[p4-label].\n"
  (interactive (list
		(p4-make-list-from-string
		 (p4-read-arg-string "p4 label: " nil "label"))))
  (if (or (null args) (equal args (list "")))
      (error "label must be specified!")
    (if (p4-cmd-line-flags args)
	(p4-noinput-buffer-action "label" nil t args)
      (p4-async-process-command "label" "Description:\n\t"
				(concat "*P4 label: "
					(car (reverse args)) "*")
				"label" args))))

(defp4cmd p4-labels ()
  "labels" "To display list of defined labels, type \\[p4-labels].\n"
  (interactive)
  (p4-noinput-buffer-action "labels" nil t nil)
  (p4-make-basic-buffer "*P4 labels*")
  (p4-regexp-create-links "*P4 labels*" "^Label \\([^ ]+\\).*\n" 'label))

;; The p4 labelsync command
(defp4cmd p4-labelsync ()
  "labelsync"
  "To synchronize a label with the current client contents, type \\[p4-labelsync].\n"
  (interactive)
  (let ((args (p4-make-list-from-string
	       (p4-read-arg-string "p4 labelsync: "))))
    (p4-noinput-buffer-action "labelsync" nil t args))
  (p4-make-depot-list-buffer "*P4 labelsync*"))

(defun p4-filter-out (pred lst)
  (let (res)
    (while lst
      (if (not (funcall pred (car lst)))
	  (setq res (cons (car lst) res)))
      (setq lst (cdr lst)))
    (reverse res)))

;; The p4 submit command
(defp4cmd p4-submit (&optional arg)
  "submit" "To submit a pending change to the depot, type \\[p4-submit].\n"
  (interactive "P")
  (let (args
	(submit-buf-name "*P4 Submit*")
	(change-list (if (integerp arg) arg)))
    (if (buffer-live-p (get-buffer submit-buf-name))
	(switch-to-buffer-other-window (get-buffer submit-buf-name))
      (if change-list
	  (setq args (list "-c" (int-to-string change-list)))
	(if current-prefix-arg
	    (setq args (p4-make-list-from-string
			(p4-read-arg-string "p4 submit: " nil)))))
      (setq args (p4-filter-out (lambda (x) (string= x "-c")) args))
      (p4-save-opened-files)
      (if (or (not (and p4-check-empty-diffs (p4-empty-diff-p)))
	      (progn
		(ding t)
		(yes-or-no-p
		 "File with empty diff opened for edit. Submit anyway? ")))
	  (p4-async-process-command "change" "Description:\n\t"
				    submit-buf-name "submit" args)))))

;; The p4 user command
(defp4cmd p4-user ()
  "user" "To create or edit a user specification, type \\[p4-user].\n"
  (interactive)
  (let (args)
    (if current-prefix-arg
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 user: " nil "user"))))
    (if (p4-cmd-line-flags args)
	(p4-noinput-buffer-action "user" nil t args)
      (p4-async-process-command "user" nil nil nil args))))

;; The p4 group command
(defp4cmd p4-group ()
  "group" "To create or edit a group specification, type \\[p4-group].\n"
  (interactive)
  (let (args)
    (if current-prefix-arg
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 group: " nil "group"))))
    (if (p4-cmd-line-flags args)
	(p4-noinput-buffer-action "group" nil t args)
      (p4-async-process-command "group" nil nil nil args))))

;; The p4 job command
(defp4cmd p4-job ()
  "job" "To create or edit a job, type \\[p4-job].\n"
  (interactive)
  (let (args)
    (if current-prefix-arg
	(setq args (p4-make-list-from-string
		    (p4-read-arg-string "p4 job: " nil "job"))))
    (if (p4-cmd-line-flags args)
	(p4-noinput-buffer-action "job" nil t args)
      (p4-async-process-command "job" "Description:\n\t" nil nil args))))

;; The p4 jobspec command
(defp4cmd p4-jobspec ()
  "jobspec" "To edit the job template, type \\[p4-jobspec].\n"
  (interactive)
  (p4-async-process-command "jobspec"))

;; A function to set the current P4 client name
(defun p4-set-client-name (p4-new-client-name)
  "To set the current value of P4CLIENT, type \\[p4-set-client-name].

This will change the current client from the previous client to the new
given value.

Setting this value to nil would disable P4 Version Checking.

`p4-set-client-name' will complete any client names set using the function
`p4-set-my-clients'. The strictness of completion will depend on the
variable `p4-strict-complete' (default is t).

Argument P4-NEW-CLIENT-NAME The new client to set to. The default value is
the current client."
  (interactive (list
		(completing-read "Change Client to: "
				 (if p4-my-clients
				     p4-my-clients
				   'p4-clients-completion)
				 nil p4-strict-complete (p4-current-client))
		))
  (if (or (null p4-new-client-name) (equal p4-new-client-name "nil"))
      (progn
	(setenv "P4CLIENT" nil)
	(if (not (getenv "P4CONFIG"))
	    (message
	     "P4 Version check disabled. Set a valid client name to enable."
	     )))
    (setenv "P4CLIENT" p4-new-client-name)
    (message "P4CLIENT changed to %s" p4-new-client-name)
    (run-hooks 'p4-set-client-hooks)))

(defun p4-get-client-config ()
  "To get the current value of the environment variable P4CONFIG,
type \\[p4-get-client-config].

This will be the current configuration that is in use for access through
Emacs P4."

  (interactive)
  (message "P4CONFIG is %s" (getenv "P4CONFIG")))

(defun p4-set-client-config (p4config)
  "To set the P4CONFIG variable, for use with the current versions of the p4
client.

P4CONFIG is a more flexible mechanism wherein p4 will find the current
client automatically by checking the config file found at the root of a
directory \(recursing all the way to the top\).

In this scenario, a P4CLIENT variable need not be explicitly set.
"
  (interactive "sP4 Config: ")
  (if (or (null p4config) (equal p4config ""))
      (message "P4CONFIG not changed.")
    (setenv "P4CONFIG" p4config)
    (message "P4CONFIG changed to %s" p4config)))

(defun p4-set-my-clients (client-list)
  "To set the client completion list used by `p4-set-client-name', use
this function in your .emacs (or any lisp interaction buffer).

This will change the current client list from the previous list to the new
given value.

Setting this value to nil would disable client completion by
`p4-set-client-name'.

The strictness of completion will depend on the variable
`p4-strict-complete' (default is t).

Argument CLIENT-LIST is the 'list' of clients.

To set your clients using your .emacs, use the following:

\(load-library \"p4\"\)
\(p4-set-my-clients \'(client1 client2 client3)\)"
  (setq p4-my-clients nil)
  (let (p4-tmp-client-var)
    (while client-list
      (setq p4-tmp-client-var (format "%s" (car client-list)))
      (setq client-list (cdr client-list))
      (setq p4-my-clients (append p4-my-clients
				  (list (list p4-tmp-client-var)))))))

;; A function to get the current P4PORT
(defun p4-get-p4-port ()
  "To get the current value of the environment variable P4PORT, type \
\\[p4-get-p4-port].

This will be the current server/port that is in use for access through Emacs
P4."
  (interactive)
  (let ((port (p4-current-server-port)))
    (message "P4PORT is [local: %s], [global: %s]" port (getenv "P4PORT"))
    port))

;; A function to set the current P4PORT
(defun p4-set-p4-port (p4-new-p4-port)
  "To set the current value of P4PORT, type \\[p4-set-p4-port].

This will change the current server from the previous server to the new
given value.

Argument P4-NEW-P4-PORT The new server:port to set to. The default value is
the current value of P4PORT."
  (interactive (list (let
			 ((symbol (read-string "Change server:port to: "
					       (getenv "P4PORT"))))
		       (if (equal symbol "")
			   (getenv "P4PORT")
			 symbol))))
  (if (or (null p4-new-p4-port) (equal p4-new-p4-port "nil"))
      (progn
	(setenv "P4PORT" nil)
	(if (not (getenv "P4CONFIG"))
	    (message
	     "P4 Version check disabled. Set a valid server:port to enable.")))
    (setenv "P4PORT" p4-new-p4-port)
    (message "P4PORT changed to %s" p4-new-p4-port)))

;; The find-file hook for p4.
(defun p4-find-file-hook ()
  "To check while loading the file, if it is a P4 version controlled file."
  (if (or (getenv "P4CONFIG") (getenv "P4CLIENT"))
      (p4-detect-p4)))

(defun p4-refresh-refresh-list (buffile bufname)
  "Refresh the list of files to be refreshed."
  (setq p4-all-buffer-files (delete (list buffile bufname)
				    p4-all-buffer-files))
  (if (not p4-all-buffer-files)
      (progn
	(if (and p4-running-emacs (timerp p4-file-refresh-timer))
	    (cancel-timer p4-file-refresh-timer))
	(if (and p4-running-xemacs p4-file-refresh-timer)
	    (disable-timeout p4-file-refresh-timer))
	(setq p4-file-refresh-timer nil))))

;; Set keymap. We use the C-x p Keymap for all perforce commands
(defvar p4-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" 'p4-add)
    (define-key map "b" 'p4-bug-report)
    (define-key map "B" 'p4-branch)
    (define-key map "c" 'p4-client)
    (define-key map "C" 'p4-changes)
    (define-key map "d" 'p4-diff2)
    (define-key map "D" 'p4-describe)
    (define-key map "e" 'p4-edit)
    (define-key map "E" 'p4-reopen)
    (define-key map "\C-f" 'p4-depot-find-file)
    (define-key map "f" 'p4-filelog)
    (define-key map "F" 'p4-files)
    (define-key map "g" 'p4-get-client-name)
    (define-key map "G" 'p4-get)
    (define-key map "h" 'p4-help)
    (define-key map "H" 'p4-have)
    (define-key map "i" 'p4-info)
    (define-key map "I" 'p4-integ)
    (define-key map "j" 'p4-job)
    (define-key map "J" 'p4-jobs)
    (define-key map "l" 'p4-label)
    (define-key map "L" 'p4-labels)
    (define-key map "\C-l" 'p4-labelsync)
    (define-key map "m" 'p4-rename)
    (define-key map "n" 'p4-notify)
    (define-key map "o" 'p4-opened)
    (define-key map "p" 'p4-print)
    (define-key map "P" 'p4-set-p4-port)
    (define-key map "q" 'p4-pop-window-config)
    (define-key map "r" 'p4-revert)
    (define-key map "R" 'p4-refresh)
    (define-key map "\C-r" 'p4-resolve)
    (define-key map "s" 'p4-set-client-name)
    (define-key map "S" 'p4-submit)
    (define-key map "t" 'p4-toggle-vc-mode)
    (define-key map "u" 'p4-user)
    (define-key map "U" 'p4-users)
    (define-key map "v" 'p4-emacs-version)
    (define-key map "V" 'p4-blame)
    (define-key map "w" 'p4-where)
    (define-key map "x" 'p4-delete)
    (define-key map "X" 'p4-fix)
    (define-key map "=" 'p4-diff)
    (define-key map "-" 'p4-ediff)
    (define-key map "?" 'p4-describe-bindings)
    map)
  "The Prefix for P4 Library Commands.")

(if (not (keymapp (lookup-key global-map "\C-xp")))
    (define-key global-map "\C-xp" p4-prefix-map))

;; For users interested in notifying a change, a notification list can be
;; set up using this function.
(defun p4-set-notify-list (p4-new-notify-list &optional p4-supress-stat)
  "To set the current value of P4NOTIFY, type \\[p4-set-notify-list].

This will change the current notify list from the existing list to the new
given value.

An empty string will disable notification.

Argument P4-NEW-NOTIFY-LIST is new value of the notification list.
Optional argument P4-SUPRESS-STAT when t will suppress display of the status
message. "

  (interactive (list (let
			 ((symbol (read-string
				   "Change Notification List to: "
				   p4-notify-list)))
		       (if (equal symbol "")
			   nil
			 symbol))))
  (let ((p4-old-notify-list p4-notify-list))
    (setenv "P4NOTIFY" p4-new-notify-list)
    (setq p4-notify-list p4-new-notify-list)
    (setq p4-notify (not (null p4-new-notify-list)))
    (if (not p4-supress-stat)
	(message "Notification list changed from '%s' to '%s'"
		 p4-old-notify-list p4-notify-list))))

;; To get the current notification list.
(defun p4-get-notify-list ()
  "To get the current value of the environment variable P4NOTIFY,
type \\[p4-get-notify-list].

   This will be the current notification list that is in use for mailing
   change notifications through Emacs P4."

  (interactive)
  (message "P4NOTIFY is %s" p4-notify-list))

(defun p4-notify (users)
  "To notify a list of users of a change submission manually, type
\\[p4-notify].

To do auto-notification, set the notification list with `p4-set-notify-list'
and on each submission, the users in the list will be notified of the
change.

Since this uses the sendmail program, it is mandatory to set the correct
path to the sendmail program in the variable `p4-sendmail-program'.

Also, it is mandatory to set the user's email address in the variable
`p4-user-email'.

Argument USERS The users to notify to. The default value is the notification
list."
  (interactive (list (let
			 ((symbol (read-string "Notify whom? "
					       p4-notify-list)))
		       (if (equal symbol "")
			   nil
			 symbol))))
  (p4-set-notify-list users t)
  (if (and p4-sendmail-program p4-user-email)
      (p4-do-notify)
    (message "Please set p4-sendmail-program and p4-user-email variables.")))

(defun p4-do-notify ()
  "This is the internal notification function called by `p4-notify'."
  (save-excursion
    (if (and p4-notify-list (not (equal p4-notify-list "")))
	(save-excursion
	  (set-buffer (get-buffer-create p4-output-buffer-name))
	  (goto-char (point-min))
	  (if (re-search-forward "[0-9]+.*submitted" (point-max) t)
	      (let (p4-matched-change)
		(setq p4-matched-change (substring (match-string 0) 0 -10))
		(set-buffer (get-buffer-create "*P4 Notify*"))
		(delete-region (point-min) (point-max))
		(call-process-region (point-min) (point-max)
				     (p4-check-p4-executable)
				     t t nil
				     "-d" default-directory
				     "describe" "-s"
				     p4-matched-change)
		(switch-to-buffer "*P4 Notify*")
		(goto-char (point-min))
		(let (p4-chg-desc)
		  (if (re-search-forward "^Change.*$" (point-max) t)
		      (setq p4-chg-desc (match-string 0))
		    (setq p4-chg-desc (concat
				       "Notification of Change "
				       p4-matched-change)))
		  (goto-char (point-min))
		  (insert
		   "From: " p4-user-email "\n"
		   "To: P4 Notification Recipients:;\n"
		   "Subject: " p4-chg-desc "\n")
		  (call-process-region (point-min) (point-max)
				       p4-sendmail-program t t nil
				       "-odi" "-oi" p4-notify-list)

		  (kill-buffer nil)))
	    (save-excursion
	      (set-buffer (get-buffer-create p4-output-buffer-name))
	      (goto-char (point-max))
	      (insert "\np4-do-notify: No Change Submissions found."))))
      (save-excursion
	(set-buffer (get-buffer-create p4-output-buffer-name))
	(goto-char (point-max))
	(insert "\np4-do-notify: Notification list not set.")))))

;; Function to return the current version.
(defun p4-emacs-version ()
  "Return the current Emacs-P4 Integration version."
  (interactive)
  (message (concat (cond (p4-running-xemacs "X")) "Emacs-P4 Integration v%s")
	   p4-emacs-version))

(defun p4-find-p4-config-file ()
  (let ((p4config (getenv "P4CONFIG"))
	(p4-cfg-dir (cond ((p4-buffer-file-name)
			   (file-name-directory
			    (file-truename (p4-buffer-file-name))))
			  (t (file-truename default-directory)))))
    (if (not p4config)
	nil
      (let (found at-root)
	(while (not (or found at-root))
	  (let ((parent-dir (file-name-directory
			     (directory-file-name
			      p4-cfg-dir))))
	    (if (file-exists-p (concat p4-cfg-dir p4config))
		(setq found (concat p4-cfg-dir p4config)))
	    (setq at-root (string-equal parent-dir p4-cfg-dir))
	    (setq p4-cfg-dir parent-dir)))
	found))))

(defun p4-detect-p4 ()
  (if (or (not p4-use-p4config-exclusively)
	  (p4-find-p4-config-file))
      (p4-check-mode)))

(defun p4-get-add-branch-files (&optional name-list)
  (let ((output-buffer (p4-depot-output "opened" name-list))
	files depot-map)
    (save-excursion
      (set-buffer output-buffer)
      (goto-char (point-min))
      (while (re-search-forward "^\\(//[^/@#]+/[^#\n]*\\)#[0-9]+ - add " nil t)
	(setq files (cons (cons (match-string 1) "Add")
			  files)))
      (goto-char (point-min))
      (while (re-search-forward "^\\(//[^/@#]+/[^#\n]*\\)#[0-9]+ - branch " nil t)
	(setq files (cons (cons (match-string 1) "Branch")
			  files))))
    (kill-buffer output-buffer)
    (setq depot-map (p4-map-depot-files (mapcar 'car files)))
    (mapcar (lambda (x) (cons (cdr (assoc (car x) depot-map))
			      (cdr x))) files)))

(defun p4-get-have-files (file-list)
  (let ((output-buffer (p4-depot-output "have" file-list))
	line files depot-map elt)
    (while (setq line (p4-read-depot-output output-buffer))
      (if (string-match "^\\(//[^/@#]+/[^#\n]*\\)#\\([0-9]+\\) - " line)
	  (setq files (cons (cons (match-string 1 line)
				  (match-string 2 line))
			    files))))
    (kill-buffer output-buffer)
    (setq depot-map (p4-map-depot-files (mapcar 'car files)))
    (setq files (mapcar (lambda (x) (cons (cdr (assoc (car x) depot-map))
					  (cdr x))) files))
    (while file-list
      (setq elt (car file-list))
      (setq file-list (cdr file-list))
      (if (not (assoc elt files))
	  (setq files (cons (cons elt nil) files))))
    files))

;; A function to check if the file being opened is version controlled by p4.
(defun p4-is-vc (&optional file-mode-cache filename)
  "If a file is controlled by P4 then return version else return nil."
  (if (not filename)
      (setq filename (p4-buffer-file-name)))
  (let (version done)
    (let ((el (assoc filename file-mode-cache)))
      (setq done el)
      (setq version (cdr el)))
    (if (and (not done) filename)
	(let ((output-buffer (p4-depot-output "have" (list filename)))
	      line)
	  (setq line (p4-read-depot-output output-buffer))
	  (kill-buffer output-buffer)
	  (if (string-match "^//[^/@#]+/[^#\n]*#\\([0-9]+\\) - " line)
	      (setq version (match-string 1 line)))
	  (setq done version)))
    (if (and (not done) (not file-mode-cache))
	(progn
	  (setq file-mode-cache
		(p4-get-add-branch-files (and filename (list filename))))
	  (setq version (cdr (assoc filename file-mode-cache)))))
    version))

(defun p4-check-mode (&optional file-mode-cache)
  "Check to see whether we should export the menu map to this buffer.

Turning on P4 mode calls the hooks in the variable `p4-mode-hook' with
no args."
  (setq p4-mode nil)
  (if p4-do-find-file
      (progn
	(setq p4-vc-check (p4-is-vc file-mode-cache))
	(if p4-vc-check
	    (progn
	      (p4-menu-add)
	      (setq p4-mode (concat " P4:" p4-vc-check))))
	(p4-force-mode-line-update)
	(let ((buffile (p4-buffer-file-name))
	      (bufname (buffer-name)))
	  (if (and p4-vc-check (not (member (list buffile bufname)
					    p4-all-buffer-files)))
	      (add-to-list 'p4-all-buffer-files (list buffile bufname))))
	(if (and (not p4-file-refresh-timer) (not (= p4-file-refresh-timer-time 0)))
	    (setq p4-file-refresh-timer
		  (cond (p4-running-emacs
			 (run-at-time nil p4-file-refresh-timer-time
				      'p4-refresh-files-in-buffers))
			(p4-running-xemacs
			 (add-timeout p4-file-refresh-timer-time
				      'p4-refresh-files-in-buffers nil
				      p4-file-refresh-timer-time)))))
	;; run hooks
	(and p4-vc-check (run-hooks 'p4-mode-hook))
	p4-vc-check)))

(defun p4-refresh-files-in-buffers (&optional arg)
  "Check to see if all the files that are under P4 version control are
actually up-to-date, if in buffers, or need refreshing."
  (interactive)
  (let ((p4-all-my-files p4-all-buffer-files) buffile bufname thiselt)
    (while p4-all-my-files
      (setq thiselt (car p4-all-my-files))
      (setq p4-all-my-files (cdr p4-all-my-files))
      (setq buffile (car thiselt))
      (setq bufname (cadr thiselt))
      (if (buffer-live-p (get-buffer bufname))
	  (save-excursion
	    (let ((buf (get-buffer bufname)))
	      (set-buffer buf)
	      (if p4-auto-refresh
		  (if (not (buffer-modified-p buf))
		      (if (not (verify-visited-file-modtime buf))
			  (if (file-readable-p buffile)
			      (revert-buffer t t)
			    (p4-check-mode))))
		(if (file-readable-p buffile)
		    (find-file-noselect buffile t)
		  (p4-check-mode)))
	      (setq buffer-read-only (not (file-writable-p
					   (p4-buffer-file-name))))))
	(p4-refresh-refresh-list buffile bufname)))))

(defun p4-check-mode-all-buffers ()
  "Call p4-check-mode for all buffers under P4 version control"
  (let ((p4-all-my-files p4-all-buffer-files) buffile bufname thiselt
	file-mode-cache)
    (if (and p4-all-my-files p4-do-find-file)
	(setq file-mode-cache
	      (append (p4-get-add-branch-files)
		      (p4-get-have-files (mapcar 'car p4-all-my-files)))))
    (while p4-all-my-files
      (setq thiselt (car p4-all-my-files))
      (setq p4-all-my-files (cdr p4-all-my-files))
      (setq buffile (car thiselt))
      (setq bufname (cadr thiselt))
      (if (buffer-live-p (get-buffer bufname))
	  (save-excursion
	    (set-buffer (get-buffer bufname))
	    (p4-check-mode file-mode-cache))
	(p4-refresh-refresh-list buffile bufname)))))

;; Force mode line updation for different Emacs versions
(defun p4-force-mode-line-update ()
  "To Force the mode line update for different flavors of Emacs."
  (cond (p4-running-xemacs
	 (redraw-modeline))
	(p4-running-emacs
	 (force-mode-line-update))))

;; In case, the P4 server is not available, or when operating off-line, the
;; p4-find-file-hook becomes a pain... this functions toggles the use of the
;; hook when opening files.

(defun p4-toggle-vc-mode ()
  "In case, the P4 server is not available, or when working off-line, toggle
the VC check on/off when opening files."
  (interactive)
  (setq p4-do-find-file (not p4-do-find-file))
  (message (concat "P4 mode check " (if p4-do-find-file
					"enabled."
				      "disabled."))))

;; Wrap C-x C-q to allow p4-edit/revert and also to ensure that
;; we don't stomp on vc-toggle-read-only.

(defun p4-toggle-read-only (&optional arg)
  "If p4-mode is non-nil, \\[p4-toggle-read-only] toggles between `p4-edit'
and `p4-revert'. If ARG is non-nil, p4-offline-mode will be enabled for this
buffer before the toggling takes place. In p4-offline-mode, toggle between
making the file writable and write protected."
  (interactive "P")
  (if (and arg p4-mode)
      (setq p4-mode nil
	    p4-offline-mode t))
  (cond
   (p4-mode
    (if buffer-read-only
	(p4-edit p4-verbose)
      (p4-revert p4-verbose)))
   (p4-offline-mode
    (toggle-read-only)
    (if buffer-file-name
	(let ((mode (file-modes buffer-file-name)))
	  (if buffer-read-only
	      (setq mode (logand mode (lognot 128)))
	    (setq mode (logior mode 128)))
	  (set-file-modes buffer-file-name mode))))))

(defun p4-browse-web-page ()
  "Browse the p4.el web page."
  (interactive)
  (require 'browse-url)
  (browse-url p4-web-page))

(defun p4-bug-report ()
  (interactive)
  (if (string-match " 19\\." (emacs-version))
      ;; unfortunately GNU Emacs 19.x doesn't have compose-mail
      (mail nil p4-emacs-maintainer (concat "BUG REPORT: "
					    (p4-emacs-version)))
    (compose-mail p4-emacs-maintainer (concat "BUG REPORT: "
					      (p4-emacs-version))))
  (goto-char (point-min))
  (re-search-forward (concat "^" (regexp-quote mail-header-separator) "\n"))
  ;; Insert warnings for novice users.
  (insert
   "This bug report will be sent to the P4-Emacs Integration Maintainer,\n"
   p4-emacs-maintainer "\n\n")
  (insert (concat (emacs-version) "\n\n"))
  (insert "A brief description of the problem and how to reproduce it:\n")
  (save-excursion
    (let ((message-buf (get-buffer
			(cond (p4-running-xemacs " *Message-Log*")
			      (p4-running-emacs "*Messages*")))))
      (if message-buf
	  (let (beg-pos
		(end-pos (point-max)))
	    (save-excursion
	      (set-buffer message-buf)
	      (goto-char end-pos)
	      (forward-line -10)
	      (setq beg-pos (point)))
	    (insert "\n\nRecent messages:\n")
	    (insert-buffer-substring message-buf beg-pos end-pos))))))

(defun p4-describe-bindings ()
  "A function to list the key bindings for the p4 prefix map"
  (interactive)
  (save-excursion
    (p4-push-window-config)
    (let ((map (make-sparse-keymap))
	  (p4-bindings-buffer "*P4 key bindings*"))
      (get-buffer-create p4-bindings-buffer)
      (cond
       (p4-running-xemacs
	(set-buffer p4-bindings-buffer)
	(delete-region (point-min) (point-max))
	(insert "Key Bindings for P4 Mode\n------------------------\n")
	(describe-bindings-internal p4-prefix-map))
       (p4-running-emacs
	(kill-buffer p4-bindings-buffer)
	(describe-bindings "\C-xp")
	(set-buffer "*Help*")
	(rename-buffer p4-bindings-buffer)))
      (define-key map "q" 'p4-quit-current-buffer)
      (use-local-map map)
      (display-buffer p4-bindings-buffer))))

;; Break up a string into a list of words
;; (p4-make-list-from-string "ab c de  f") -> ("ab" "c" "de" "f")
(defun p4-make-list-from-string (str)
  (let (lst)
    (while (or (string-match "^ *\"\\([^\"]*\\)\"" str)
	       (string-match "^ *\'\\([^\']*\\)\'" str)
	       (string-match "^ *\\([^ ]+\\)" str))
      (setq lst (append lst (list (match-string 1 str))))
      (setq str (substring str (match-end 0))))
    lst))

(defun p4-list-to-string (lst)
  (mapconcat (lambda (x) x) lst " "))

;; Return the file name associated with a buffer. If the real buffer file
;; name doesn't exist, try special filename tags set in some of the p4
;; buffers.
(defun p4-buffer-file-name-2 ()
  (cond ((p4-buffer-file-name))
	((get-char-property (point) 'link-client-name))
	((get-char-property (point) 'link-depot-name))
	((get-char-property (point) 'block-client-name))
	((get-char-property (point) 'block-depot-name))
	((if (and (fboundp 'dired-get-filename)
		  (dired-get-filename nil t))
	     (p4-follow-link-name (dired-get-filename nil t))))))

(defun p4-buffer-file-name ()
  (cond (buffer-file-name
	 (p4-follow-link-name buffer-file-name))
	(t nil)))

(defun p4-follow-link-name (name)
  (if p4-follow-symlinks
      (file-truename name)
    name))

(defvar p4-depot-filespec-history nil
  "History for p4-depot filespecs.")

(defvar p4-depot-completion-cache nil
  "Cache for `p4-depot-completion'.
It is a list of lists whose car is a filespec and
cdr is the list of anwers")

(defvar p4-branches-history nil
  "History for p4 clients.")

(defvar p4-branches-completion-cache nil
  "Cache for `p4-depot-completion'.
It is a list of lists whose car is a client and
cdr is the list of answers??")

(defvar p4-clients-history nil
  "History for p4 clients.")

(defvar p4-clients-completion-cache nil
  "Cache for `p4-depot-completion'.
It is a list of lists whose car is a client and
cdr is the list of answers??")

(defvar p4-jobs-completion-cache nil
  "Cache for `p4-depot-completion'.
It is a list of lists whose car is a job and
cdr is the list of answers??")

(defvar p4-labels-history nil
  "History for p4 clients.")

(defvar p4-labels-completion-cache nil
  "Cache for `p4-depot-completion'.
It is a list of lists whose car is a label and
cdr is the list of answers??")

(defvar p4-users-completion-cache nil
  "Cache for `p4-depot-completion'.
It is a list of lists whose car is a user and
cdr is the list of answers??")

(defvar p4-groups-completion-cache nil
  "Cache for `p4-depot-completion'.
It is a list of lists whose car is a group and
cdr is the list of answers??")

(defvar p4-arg-string-history nil
  "History for p4 command arguments")

(defun p4-depot-completion-search (filespec cmd)
  "Look into `p4-depot-completion-cache' for filespec.
Filespec is the candidate for completion, so the
exact file specification is \"filespec*\".

If found in cache, return a list whose car is FILESPEC and cdr is the list
of matches.
If not found in cache, return nil.
So the 'no match' answer is different from 'not in cache'."
  (let ((l (cond
	    ((equal cmd "branches") p4-branches-completion-cache)
	    ((equal cmd "clients") p4-clients-completion-cache)
	    ((equal cmd "dirs") p4-depot-completion-cache)
	    ((equal cmd "jobs") p4-jobs-completion-cache)
	    ((equal cmd "labels") p4-labels-completion-cache)
	    ((equal cmd "users") p4-users-completion-cache)
	    ((equal cmd "groups") p4-groups-completion-cache)))
	dir list)

    (if (and p4-cleanup-cache (not p4-timer))
	(setq p4-timer (cond (p4-running-emacs
			      (run-at-time p4-cleanup-time nil
					   'p4-cache-cleanup))
			     (p4-running-xemacs
			      (add-timeout p4-cleanup-time 'p4-cache-cleanup
					   nil nil)))))
    (while l
      (if (string-match (concat "^" (car (car l)) "[^/]*$") filespec)
	  (progn
	    ;; filespec is included in cache
	    (if (string= (car (car l)) filespec)
		(setq list (cdr (car l)))
	      (setq dir (cdr (car l)))
	      (while dir
		(if (string-match (concat "^" filespec) (car dir))
		    (setq list (cons (car dir) list)))
		(setq dir (cdr dir))))
	    (setq l nil
		  list (cons filespec list))))
      (setq l (cdr l)))
    list))

(defun p4-cache-cleanup (&optional arg)
  "Cleanup all the completion caches."
  (message "Cleaning up the p4 caches ...")
  (setq p4-branches-completion-cache nil)
  (setq p4-clients-completion-cache nil)
  (setq p4-depot-completion-cache nil)
  (setq p4-jobs-completion-cache nil)
  (setq p4-labels-completion-cache nil)
  (setq p4-users-completion-cache nil)
  (setq p4-groups-completion-cache nil)
  (if (and p4-running-emacs (timerp p4-timer)) (cancel-timer p4-timer))
  (if (and p4-running-xemacs p4-timer) (disable-timeout p4-timer))
  (setq p4-timer nil)
  (message "Cleaning up the p4 caches ... done."))

(defun p4-partial-cache-cleanup (type)
  "Cleanup a specific completion cache."
  (cond ((string= type "branch")
	 (setq p4-branches-completion-cache nil))
	((string= type "client")
	 (setq p4-clients-completion-cache nil))
	((or (string= type "submit") (string= type "change"))
	 (setq p4-depot-completion-cache nil))
	((string= type "job")
	 (setq p4-jobs-completion-cache nil))
	((string= type "label")
	 (setq p4-labels-completion-cache nil))
	((string= type "user")
	 (setq p4-users-completion-cache nil))
	((string= type "group")
	 (setq p4-groups-completion-cache nil))))

(defun p4-read-depot-output (buffer &optional regexp)
  "Reads first line of BUFFER and returns it.
Read lines are deleted from buffer.

If optional REGEXP is passed in, return the substring of the first line that
matched the REGEXP."

  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (forward-line)

    (let ((line (buffer-substring (point-min) (point))))
      (if (string= line "")
	  nil
	(delete-region (point-min) (point))
	(if (and regexp (string-match regexp line))
	    (setq line (substring line (match-beginning 1) (match-end 1))))

	;; remove trailing newline
	(if (equal (substring line (1- (length line)) (length line)) "\n")
	    (substring line 0 (1- (length line)))
	  line)))))

(defun p4-completion-helper (filespec cmd var regexp)
  (let (output-buffer line list)
    (message "Making %s completion list..." cmd)
    (setq output-buffer (p4-depot-output cmd))
    (while (setq line (p4-read-depot-output
		       output-buffer regexp))
      (if line
	  (setq list (cons line list))))
    (kill-buffer output-buffer)
    (set var
	 (cons (cons filespec list) (eval var)))
    list))

(defun p4-depot-completion-build (filespec cmd)
  "Ask Perforce for a list of files and directories beginning with FILESPEC."
  (let (output-buffer line list)
    (cond
     ((equal cmd "branches")
      (setq list (p4-completion-helper
		  filespec cmd 'p4-branches-completion-cache
		  "^Branch \\([^ \n]*\\) [0-9][0-9][0-9][0-9]/.*$")))
     ((equal cmd "clients")
      (setq list (p4-completion-helper
		  filespec cmd 'p4-clients-completion-cache
		  "^Client \\([^ \n]*\\) [0-9][0-9][0-9][0-9]/.*$")))

     ((equal cmd "dirs")
      (message "Making p4 completion list...")
      (setq output-buffer (p4-depot-output cmd
					   (list (concat filespec "*"))))
      (while (setq line (p4-read-depot-output output-buffer))
	(if (not (string-match "no such file" line))
	    (setq list (cons (concat line "/") list))))
      (kill-buffer output-buffer)
      (setq output-buffer (p4-depot-output "files"
					   (list (concat filespec "*"))))
      (while (setq line (p4-read-depot-output output-buffer))
	(if (string-match "^\\(.+\\)#[0-9]+ - " line)
	    (setq list (cons (match-string 1 line) list))))
      (kill-buffer output-buffer)
      (setq p4-depot-completion-cache
	    (cons (cons filespec list) p4-depot-completion-cache)))

     ((equal cmd "jobs")
      (setq list (p4-completion-helper
		  filespec cmd 'p4-jobs-completion-cache
		  "\\([^ \n]*\\) on [0-9][0-9][0-9][0-9]/.*$")))
     ((equal cmd "labels")
      (setq list (p4-completion-helper
		  filespec cmd 'p4-labels-completion-cache
		  "^Label \\([^ \n]*\\) [0-9][0-9][0-9][0-9]/.*$")))
     ((equal cmd "users")
      (setq list (p4-completion-helper
		  filespec cmd 'p4-users-completion-cache
		  "^\\([^ ]+\\).*$")))
     ((equal cmd "groups")
      (setq list (p4-completion-helper
		  filespec cmd 'p4-groups-completion-cache
		  "^\\(.*\\)$"))))
    (message nil)
    (cons filespec list)))

(defun p4-completion-builder (type)
  `(lambda (string predicate action)
     ,(concat "Completion function for Perforce " type ".

Using the mouse in completion buffer on a client will select it
and exit, unlike standard selection. This is because
`choose-completion-string' (in simple.el) has a special code for
file name selection.")
     (let (list)
       ,(if (string= type "dirs")
	    ;; when testing for an exact match, remove trailing /
	    `(if (and (eq action 'lambda)
		      (eq (aref string (1- (length string))) ?/))
		 (setq string (substring string 0 (1- (length string))))))

       ;; First, look in cache
       (setq list (p4-depot-completion-search string ,type))

       ;; If not found in cache, build list.
       (if (not list)
	   (setq list (p4-depot-completion-build string ,type)))

       (cond
	;; try completion
	((null action)
	 (try-completion string (mapcar 'list (cdr list)) predicate))
	;; all completions
	((eq action t)
	 (let ((lst
		(all-completions string (mapcar 'list (cdr list)) predicate)))
	   ,(if (string= type "dirs")
		`(setq lst (mapcar (lambda (s)
				     (if (string-match ".*/\\(.+\\)" s)
					 (match-string 1 s)
				       s))
				   lst)))
	   lst))
	;; Test for an exact match
	(t
	 (and (>= (length list) 2)
	      (member (car list) (cdr list))))))))

(defalias 'p4-branches-completion (p4-completion-builder "branches"))
(defalias 'p4-clients-completion (p4-completion-builder "clients"))
(defalias 'p4-depot-completion (p4-completion-builder "dirs"))
(defalias 'p4-jobs-completion (p4-completion-builder "jobs"))
(defalias 'p4-labels-completion (p4-completion-builder "labels"))
(defalias 'p4-users-completion (p4-completion-builder "users"))
(defalias 'p4-groups-completion (p4-completion-builder "groups"))


(defun p4-read-arg-string (prompt &optional initial type)
  (let ((minibuffer-local-completion-map
	 (copy-keymap minibuffer-local-completion-map)))
    (define-key minibuffer-local-completion-map " " 'self-insert-command)
    (completing-read prompt
		     (cond ((not type)
			    'p4-arg-string-completion)
			   ((string= type "branch")
			    'p4-branch-string-completion)
			   ((string= type "client")
			    'p4-client-string-completion)
			   ((string= type "label")
			    'p4-label-string-completion)
			   ((string= type "job")
			    'p4-job-string-completion)
			   ((string= type "user")
			    'p4-user-string-completion)
			   ((string= type "group")
			    'p4-group-string-completion))
		     nil nil
		     initial 'p4-arg-string-history)))

(defun p4-arg-string-completion (string predicate action)
  (let ((first-part "") completion)
    (if (string-match "^\\(.* +\\)\\(.*\\)" string)
	(progn
	  (setq first-part (match-string 1 string))
	  (setq string (match-string 2 string))))
    (cond ((string-match "-b +$" first-part)
	   (setq completion (p4-branches-completion string predicate action)))
	  ((string-match "-t +$" first-part)
	   (setq completion (p4-list-completion
			     string (list "text " "xtext " "binary "
					  "xbinary " "symlink ")
			     predicate action)))
	  ((string-match "-j +$" first-part)
	   (setq completion (p4-jobs-completion string predicate action)))
	  ((string-match "-l +$" first-part)
	   (setq completion (p4-labels-completion string predicate action)))
	  ((string-match "\\(.*status=\\)\\(.*\\)" string)
	   (setq first-part (concat first-part (match-string 1 string)))
	   (setq string (match-string 2 string))
	   (setq completion (p4-list-completion
			     string (list "open " "closed " "suspended ")
			     predicate action)))
	  ((or (string-match "\\(.*@.+,\\)\\(.*\\)" string)
	       (string-match "\\(.*@\\)\\(.*\\)" string))
	   (setq first-part (concat first-part (match-string 1 string)))
	   (setq string (match-string 2 string))
	   (setq completion (p4-labels-completion string predicate action)))
	  ((string-match "\\(.*#\\)\\(.*\\)" string)
	   (setq first-part (concat first-part (match-string 1 string)))
	   (setq string (match-string 2 string))
	   (setq completion (p4-list-completion
			     string (list "none" "head" "have")
			     predicate action)))
	  ((string-match "^//" string)
	   (setq completion (p4-depot-completion string predicate action)))
	  ((string-match "\\(^-\\)\\(.*\\)" string)
	   (setq first-part (concat first-part (match-string 1 string)))
	   (setq string (match-string 2 string))
	   (setq completion (p4-list-completion
			     string (list "a " "af " "am " "as " "at " "ay "
					  "b " "c " "d " "dc " "dn "
					  "ds " "du " "e " "f " "i " "j "
					  "l " "m " "n " "q " "r " "s " "sa "
					  "sd " "se " "sr " "t " "v ")
			     predicate action)))
	  (t
	   (setq completion (p4-file-name-completion string
						     predicate action))))
    (cond ((null action) ;; try-completion
	   (if (stringp completion)
	       (concat first-part completion)
	     completion))
	  ((eq action t) ;; all-completions
	   completion)
	  (t             ;; exact match
	   completion))))

(defun p4-list-completion (string lst predicate action)
  (let ((collection (mapcar 'list lst)))
    (cond ((not action)
	   (try-completion string collection predicate))
	  ((eq action t)
	   (all-completions string collection predicate))
	  (t
	   (eq (try-completion string collection predicate) t)))))

(defun p4-file-name-completion (string predicate action)
  (if (string-match "//\\(.*\\)" string)
      (setq string (concat "/" (match-string 1 string))))
  (setq string (substitute-in-file-name string))
  (setq string (p4-follow-link-name (expand-file-name string)))
  (let ((dir-path "") completion)
    (if (string-match "^\\(.*[/\\]\\)\\(.*\\)" string)
	(progn
	  (setq dir-path (match-string 1 string))
	  (setq string (match-string 2 string))))
    (cond ((not action)
	   (setq completion (file-name-completion string dir-path))
	   (if (stringp completion)
	       (concat dir-path completion)
	     completion))
	  ((eq action t)
	   (file-name-all-completions string dir-path))
	  (t
	   (eq (file-name-completion string dir-path) t)))))

(defun p4-string-completion-builder (completion-function)
  `(lambda (string predicate action)
     (let ((first-part "") completion)
       (if (string-match "^\\(.* +\\)\\(.*\\)" string)
	   (progn
	     (setq first-part (match-string 1 string))
	     (setq string (match-string 2 string))))
       (cond ((string-match "^-" string)
	      (setq completion nil))
	     (t
	      (setq completion
		    (,completion-function string predicate action))))
       (cond ((null action);; try-completion
	      (if (stringp completion)
		  (concat first-part completion)
		completion))
	     ((eq action t);; all-completions
	      completion)
	     (t;; exact match
	      completion)))))

(defalias 'p4-branch-string-completion (p4-string-completion-builder
					'p4-branches-completion))

(defalias 'p4-client-string-completion (p4-string-completion-builder
					'p4-clients-completion))

(defalias 'p4-job-string-completion (p4-string-completion-builder
				     'p4-jobs-completion))

(defalias 'p4-label-string-completion (p4-string-completion-builder
				       'p4-labels-completion))

(defalias 'p4-user-string-completion (p4-string-completion-builder
				      'p4-users-completion))

(defalias 'p4-group-string-completion (p4-string-completion-builder
				      'p4-groups-completion))

(defun p4-depot-find-file (file)
  (interactive (list (completing-read "Enter filespec: "
				      'p4-depot-completion
				      nil nil
				      p4-default-depot-completion-prefix
				      'p4-depot-filespec-history)))
  (let ((lfile (cdar (p4-map-depot-files (list file)))))
    (if lfile
	(find-file lfile)
      (if (get-file-buffer file)
	  (switch-to-buffer-other-window file)
	(get-buffer-create file)
	(set-buffer file)
	(p4-noinput-buffer-action "print" nil t (list file))
	(p4-activate-print-buffer file t)))))


;; A function to get the current P4 client name
(defun p4-get-client-name ()
  "To get the current value of the environment variable P4CLIENT,
type \\[p4-get-client-name].

This will be the current client that is in use for access through
Emacs P4."
  (interactive)
  (let ((client (p4-current-client)))
    (message "P4CLIENT [local: %s], [global: %s]" client (getenv "P4CLIENT"))
    client))

(defun p4-get-config-info (file-name token)
  (let ((output-buffer (generate-new-buffer p4-output-buffer-name))
	(data (getenv token)))
    (save-excursion
      (set-buffer output-buffer)
      (goto-char (point-min))
      (insert-file-contents file-name)
      (goto-char (point-min))
      (if (re-search-forward (concat "^" (regexp-quote token) "=\\(.*\\)")
			     nil t)
	  (setq data (match-string 1))))
    (kill-buffer output-buffer)
    data))

(defun p4-current-client ()
  "Get the current local client, or the global client, if that."
  (let ((p4-config-file (p4-find-p4-config-file))
	cur-client pmin)
    (if (not p4-config-file)
	(setq cur-client (getenv "P4CLIENT"))
      (setq cur-client (p4-get-config-info p4-config-file "P4CLIENT")))
    (if (not cur-client)
	(save-excursion
	  (get-buffer-create p4-output-buffer-name)
	  (set-buffer p4-output-buffer-name)
	  (goto-char (point-max))
	  (setq pmin (point))
	  (if (zerop (p4-call-p4-here "info"))
	      (progn
		(goto-char pmin)
		(if (re-search-forward "^Client name:[ \t]+\\(.*\\)$" nil t)
		    (setq cur-client (match-string 1)))
		(delete-region pmin (point-max))))))
    cur-client))

(defun p4-current-server-port ()
  "Get the current local server:port address, or the global server:port, if
that."
  (let ((p4-config-file (p4-find-p4-config-file)))
    (if (not p4-config-file)
	(getenv "P4PORT")
      (p4-get-config-info p4-config-file "P4PORT"))))

(defun p4-save-opened-files ()
  (get-buffer-create p4-output-buffer-name);; We do these two lines
  (kill-buffer p4-output-buffer-name)      ;; to ensure no duplicates
  (let ((output-buffer (p4-depot-output "opened"))
	opened)
    (save-excursion
      (set-buffer output-buffer)
      (goto-char (point-min))
      (while (re-search-forward "^\\(.*\\)#[0-9]+ - " nil t)
	(setq opened (cons (match-string 1) opened))))
    (kill-buffer output-buffer)
    (setq opened (mapcar 'cdr (p4-map-depot-files opened)))
    (save-window-excursion
      (map-y-or-n-p
       (function
	(lambda (buffer)
	  (and (buffer-modified-p buffer)
	       (not (buffer-base-buffer buffer))
	       (buffer-file-name buffer)
	       (member (buffer-file-name buffer) opened)
	       (format "Save file %s? "
		       (buffer-file-name buffer)))))
       (function
	(lambda (buffer)
	  (set-buffer buffer)
	  (save-buffer)))
       (buffer-list)
       '("buffer" "buffers" "save")))))

(defun p4-empty-diff-p ()
  "Return t if there exists a file opened for edit with an empty diff"
  (let ((buffer (get-buffer-create "p4-edp-buf"))
	opened empty-diff)
    (p4-exec-p4 buffer (list "opened") t)
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (while (re-search-forward "^\\(.*\\)#[0-9]* - edit.*" nil t)
	(setq opened (cons (match-string 1) opened))))
    (if opened
	(progn
	  (p4-exec-p4 buffer (list "diff") t)
	  (save-excursion
	    (set-buffer buffer)
	    (goto-char (point-max))
	    (insert "====\n")
	    (goto-char (point-min))
	    (while (re-search-forward "^==== \\([^#\n]+\\)#.*\n====" nil t)
	      (if (member (match-string 1) opened)
		  (progn
		    (setq empty-diff t)
		    (goto-char (point-max))))))))
    (kill-buffer buffer)
    empty-diff))

;; this next chunk is not currently used, but my plan is to
;; reintroduce it as configurable bury-or-kill-on-q behaviour:

;; (defcustom p4-blame-2ary-disp-method 'default
;;   "Method to use when displaying p4-blame secondary buffers
;;    (currently change and rev buffers)

;;    new-frame   --  pop a new frame for the buffer
;;    new-window  --  create a new window for the buffer
;;    default     --  just do what `display-buffer' would do

;;    Any other value is equivalent to default."
;;   :type '(radio (const default) (const  new-frame) (const new-window))
;;   :group 'p4)

(defun p4-blame-kill-blame ()
  "Don\'t ask any questions, just kill the current buffer"
  (interactive)
  (set-buffer-modified-p nil)
  (kill-buffer (current-buffer)))

(defun p4-blame-secondary-buffer-cleanup ()
  "Attempt to clean up a` p4-blame' secondary buffer neatly, deleting
windows or frames when we think that\'s necessary"
  (let* ((this-buffer (current-buffer))
	 (this-window (get-buffer-window this-buffer t)))
    (cond
     ;; in new-frame mode, delete the frame
     ((eq p4-blame-2ary-disp-method 'new-frame)
      (if (one-window-p 'ignore-minibuffer 'just-this-frame)
	  (delete-frame (window-frame this-window))
	(delete-window this-window)) t)
     ;; in new-window mode, just zap the window,
     ;; provided it is not the only one:
     ((eq p4-blame-2ary-disp-method 'new-window)
      (if (not (one-window-p 'ignore-minibuffer 'just-this-frame))
	  (delete-window this-window)) t)
     ;; any other mode, nothing special need be done
     (t
      t))))

(provide 'p4)

;;; p4.el ends here
;;; python-mode.el --- Major mode for editing Python programs

;; Copyright (C) 1992,1993,1994  Tim Peters

;; Author: 1995-1998 Barry A. Warsaw
;;         1992-1994 Tim Peters
;; Maintainer: python-mode@python.org
;; Created:    Feb 1992
;; Keywords:   python languages oop

(defconst py-version "3.105"
  "`python-mode' version number.")

;; This software is provided as-is, without express or implied
;; warranty.  Permission to use, copy, modify, distribute or sell this
;; software, without fee, for any purpose and by any individual or
;; organization, is hereby granted, provided that the above copyright
;; notice and this paragraph appear in all copies.

;;; Commentary:

;; This is a major mode for editing Python programs.  It was developed
;; by Tim Peters after an original idea by Michael A. Guravage.  Tim
;; subsequently left the net; in 1995, Barry Warsaw inherited the mode
;; and is the current maintainer.  Tim's now back but disavows all
;; responsibility for the mode.  Smart Tim :-)

;; This version of python-mode.el is no longer compatible with Emacs
;; 18.  I am striving to maintain compatibility with the X/Emacs 19
;; lineage but as time goes on that becomes more and more difficult.
;; I current recommend that you upgrade to the latest stable released
;; version of your favorite branch: Emacs 20.3 or better, or XEmacs
;; 20.4 or better (XEmacs 21.0 is in beta testing as of this writing
;; 27-Oct-1998 appears to work fine with this version of
;; python-mode.el).  Even Windows users should be using at least
;; NTEmacs 20.3, and XEmacs 21.0 will work very nicely on Windows when
;; it is released.

;; FOR MORE INFORMATION:

;; For more information on installing python-mode.el, especially with
;; respect to compatibility information, please see
;;
;;     http://www.python.org/emacs/python-mode/
;;
;; This site also contains links to other packages that you might find 
;; useful, such as pdb interfaces, OO-Browser links, etc.

;; BUG REPORTING:

;; To submit bug reports, use C-c C-b.  Please include a complete, but
;; concise code sample and a recipe for reproducing the bug.  Send
;; suggestions and other comments to python-mode@python.org.

;; When in a Python mode buffer, do a C-h m for more help.  It's
;; doubtful that a texinfo manual would be very useful, but if you
;; want to contribute one, I'll certainly accept it!

;; TO DO LIST:

;; - Better integration with pdb.py and gud-mode for debugging.
;; - Rewrite according to GNU Emacs Lisp standards.
;; - have py-execute-region on indented code act as if the region is
;;   left justified.  Avoids syntax errors.
;; - add a py-goto-block-down, bound to C-c C-d

;;; Code:

(require 'comint)
(require 'custom)
(eval-when-compile
  (require 'cl)
  (if (not (and (condition-case nil
		    (require 'custom)
		  (error nil))
		;; Stock Emacs 19.34 has a broken/old Custom library
		;; that does more harm than good.  Fortunately, it is
		;; missing defcustom
		(fboundp 'defcustom)))
      (error "STOP! STOP! STOP! STOP!

The Custom library was not found or is out of date.  A more current
version is required.  Please download and install the latest version
of the Custom library from:

    <http://www.dina.kvl.dk/~abraham/custom/>

See the Python Mode home page for details:

    <http://www.python.org/emacs/python-mode>
")))



;; user definable variables
;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

(defgroup python nil
  "Support for the Python programming language, <http://www.python.org/>"
  :group 'languages
  :prefix "py-")

(defcustom py-python-command "python"
  "*Shell command used to start Python interpreter."
  :type 'string
  :group 'python)

(defcustom py-jpython-command "jpython"
  "*Shell command used to start the JPython interpreter."
  :type 'string
  :group 'python
  :tag "JPython Command")

(defcustom py-default-interpreter 'cpython
  "*Which Python interpreter is used by default.
The value for this variable can be either `cpython' or `jpython'.

When the value is `cpython', the variables `py-python-command' and
`py-python-command-args' are consulted to determine the interpreter
and arguments to use.

When the value is `jpython', the variables `py-jpython-command' and
`py-jpython-command-args' are consulted to determine the interpreter
and arguments to use.

Note that this variable is consulted only the first time that a Python
mode buffer is visited during an Emacs session.  After that, use
\\[py-toggle-shells] to change the interpreter shell."
  :type '(choice (const :tag "Python (a.k.a. CPython)" cpython)
		 (const :tag "JPython" jpython))
  :group 'python)

(defcustom py-python-command-args '("-i")
  "*List of string arguments to be used when starting a Python shell."
  :type '(repeat string)
  :group 'python)

(defcustom py-jpython-command-args '("-i")
  "*List of string arguments to be used when starting a JPython shell."
  :type '(repeat string)
  :group 'python
  :tag "JPython Command Args")

(defcustom py-indent-offset 4
  "*Amount of offset per level of indentation.
`\\[py-guess-indent-offset]' can usually guess a good value when
you're editing someone else's Python code."
  :type 'integer
  :group 'python)

(defcustom py-smart-indentation t
  "*Should `python-mode' try to automagically set some indentation variables?
When this variable is non-nil, two things happen when a buffer is set
to `python-mode':

    1. `py-indent-offset' is guessed from existing code in the buffer.
       Only guessed values between 2 and 8 are considered.  If a valid
       guess can't be made (perhaps because you are visiting a new
       file), then the value in `py-indent-offset' is used.

    2. `indent-tabs-mode' is turned off if `py-indent-offset' does not
       equal `tab-width' (`indent-tabs-mode' is never turned on by
       Python mode).  This means that for newly written code, tabs are
       only inserted in indentation if one tab is one indentation
       level, otherwise only spaces are used.

Note that both these settings occur *after* `python-mode-hook' is run,
so if you want to defeat the automagic configuration, you must also
set `py-smart-indentation' to nil in your `python-mode-hook'."
  :type 'boolean
  :group 'python)

(defcustom py-align-multiline-strings-p t
  "*Flag describing how multi-line triple quoted strings are aligned.
When this flag is non-nil, continuation lines are lined up under the
preceding line's indentation.  When this flag is nil, continuation
lines are aligned to column zero."
  :type '(choice (const :tag "Align under preceding line" t)
		 (const :tag "Align to column zero" nil))
  :group 'python)

(defcustom py-block-comment-prefix "##"
  "*String used by \\[comment-region] to comment out a block of code.
This should follow the convention for non-indenting comment lines so
that the indentation commands won't get confused (i.e., the string
should be of the form `#x...' where `x' is not a blank or a tab, and
`...' is arbitrary).  However, this string should not end in whitespace."
  :type 'string
  :group 'python)

(defcustom py-honor-comment-indentation t
  "*Controls how comment lines influence subsequent indentation.

When nil, all comment lines are skipped for indentation purposes, and
if possible, a faster algorithm is used (i.e. X/Emacs 19 and beyond).

When t, lines that begin with a single `#' are a hint to subsequent
line indentation.  If the previous line is such a comment line (as
opposed to one that starts with `py-block-comment-prefix'), then its
indentation is used as a hint for this line's indentation.  Lines that
begin with `py-block-comment-prefix' are ignored for indentation
purposes.

When not nil or t, comment lines that begin with a `#' are used as
indentation hints, unless the comment character is in column zero."
  :type '(choice
	  (const :tag "Skip all comment lines (fast)" nil)
	  (const :tag "Single # `sets' indentation for next line" t)
	  (const :tag "Single # `sets' indentation except at column zero"
		 other)
	  )
  :group 'python)

(defcustom py-temp-directory
  (let ((ok '(lambda (x)
	       (and x
		    (setq x (expand-file-name x)) ; always true
		    (file-directory-p x)
		    (file-writable-p x)
		    x))))
    (or (funcall ok (getenv "TMPDIR"))
	(funcall ok "/usr/tmp")
	(funcall ok "/tmp")
	(funcall ok  ".")
	(error
	 "Couldn't find a usable temp directory -- set `py-temp-directory'")))
  "*Directory used for temp files created by a *Python* process.
By default, the first directory from this list that exists and that you
can write into:  the value (if any) of the environment variable TMPDIR,
/usr/tmp, /tmp, or the current directory."
  :type 'string
  :group 'python)

(defcustom py-beep-if-tab-change t
  "*Ring the bell if `tab-width' is changed.
If a comment of the form

  \t# vi:set tabsize=<number>:

is found before the first code line when the file is entered, and the
current value of (the general Emacs variable) `tab-width' does not
equal <number>, `tab-width' is set to <number>, a message saying so is
displayed in the echo area, and if `py-beep-if-tab-change' is non-nil
the Emacs bell is also rung as a warning."
  :type 'boolean
  :group 'python)

(defcustom py-jump-on-exception t
  "*Jump to innermost exception frame in *Python Output* buffer.
When this variable is non-nil and an exception occurs when running
Python code synchronously in a subprocess, jump immediately to the
source code of the innermost traceback frame."
  :type 'boolean
  :group 'python)

(defcustom py-ask-about-save t
  "If not nil, ask about which buffers to save before executing some code.
Otherwise, all modified buffers are saved without asking."
  :type 'boolean
  :group 'python)

(defcustom py-backspace-function 'backward-delete-char-untabify
  "*Function called by `py-electric-backspace' when deleting backwards."
  :type 'function
  :group 'python)

(defcustom py-delete-function 'delete-char
  "*Function called by `py-electric-delete' when deleting forwards."
  :type 'function
  :group 'python)

(defcustom py-imenu-show-method-args-p nil 
  "*Controls echoing of arguments of functions & methods in the Imenu buffer.
When non-nil, arguments are printed."
  :type 'boolean
  :group 'python)
(make-variable-buffer-local 'py-indent-offset)

;; Not customizable
(defvar py-master-file nil
  "If non-nil, execute the named file instead of the buffer's file.
The intent is to allow you to set this variable in the file's local
variable section, e.g.:

    # Local Variables:
    # py-master-file: \"master.py\"
    # End:

so that typing \\[py-execute-buffer] in that buffer executes the named
master file instead of the buffer's file.  If the file name has a
relative path, the value of variable `default-directory' for the
buffer is prepended to come up with a file name.")
(make-variable-buffer-local 'py-master-file)



;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; NO USER DEFINABLE VARIABLES BEYOND THIS POINT

(defconst py-emacs-features
  (let (features)
   ;; NTEmacs 19.34.6 has a broken make-temp-name; it always returns
   ;; the same string.
   (let ((tmp1 (make-temp-name ""))
	 (tmp2 (make-temp-name "")))
     (if (string-equal tmp1 tmp2)
	 (push 'broken-temp-names features)))
   ;; return the features
   features)
  "A list of features extant in the Emacs you are using.
There are many flavors of Emacs out there, with different levels of
support for features needed by `python-mode'.")

(defvar python-font-lock-keywords
  (let ((kw1 (mapconcat 'identity
			'("and"      "assert"   "break"   "class"
			  "continue" "def"      "del"     "elif"
			  "else"     "except"   "exec"    "for"
			  "from"     "global"   "if"      "import"
			  "in"       "is"       "lambda"  "not"
			  "or"       "pass"     "print"   "raise"
			  "return"   "while"
			  )
			"\\|"))
	(kw2 (mapconcat 'identity
			'("else:" "except:" "finally:" "try:")
			"\\|"))
	)
    (list
     ;; keywords
     (cons (concat "\\b\\(" kw1 "\\)\\b[ \n\t(]") 1)
     ;; block introducing keywords with immediately following colons.
     ;; Yes "except" is in both lists.
     (cons (concat "\\b\\(" kw2 "\\)[ \n\t(]") 1)
     ;; classes
     '("\\bclass[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"
       1 font-lock-type-face)
     ;; functions
     '("\\bdef[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"
       1 font-lock-function-name-face)
     ))
  "Additional expressions to highlight in Python mode.")
(put 'python-mode 'font-lock-defaults '(python-font-lock-keywords))

;; have to bind py-file-queue before installing the kill-emacs-hook
(defvar py-file-queue nil
  "Queue of Python temp files awaiting execution.
Currently-active file is at the head of the list.")


;; Constants

(defconst py-stringlit-re
  (concat
   ;; These fail if backslash-quote ends the string (not worth
   ;; fixing?).  They precede the short versions so that the first two
   ;; quotes don't look like an empty short string.
   ;;
   ;; (maybe raw), long single quoted triple quoted strings (SQTQ),
   ;; with potential embedded single quotes
   "[rR]?'''[^']*\\(\\('[^']\\|''[^']\\)[^']*\\)*'''"
   "\\|"
   ;; (maybe raw), long double quoted triple quoted strings (DQTQ),
   ;; with potential embedded double quotes
   "[rR]?\"\"\"[^\"]*\\(\\(\"[^\"]\\|\"\"[^\"]\\)[^\"]*\\)*\"\"\""
   "\\|"
   "[rR]?'\\([^'\n\\]\\|\\\\.\\)*'"	; single-quoted
   "\\|"				; or
   "[rR]?\"\\([^\"\n\\]\\|\\\\.\\)*\""	; double-quoted
   )
  "Regular expression matching a Python string literal.")

(defconst py-continued-re
  ;; This is tricky because a trailing backslash does not mean
  ;; continuation if it's in a comment
  (concat
   "\\(" "[^#'\"\n\\]" "\\|" py-stringlit-re "\\)*"
   "\\\\$")
  "Regular expression matching Python backslash continuation lines.")
  
(defconst py-blank-or-comment-re "[ \t]*\\($\\|#\\)"
  "Regular expression matching a blank or comment line.")

(defconst py-outdent-re
  (concat "\\(" (mapconcat 'identity
			   '("else:"
			     "except\\(\\s +.*\\)?:"
			     "finally:"
			     "elif\\s +.*:")
			   "\\|")
	  "\\)")
  "Regular expression matching statements to be dedented one level.")
  
(defconst py-block-closing-keywords-re
  "\\(return\\|raise\\|break\\|continue\\|pass\\)"
  "Regular expression matching keywords which typically close a block.")

(defconst py-no-outdent-re
  (concat
   "\\("
   (mapconcat 'identity
	      (list "try:"
		    "except\\(\\s +.*\\)?:"
		    "while\\s +.*:"
		    "for\\s +.*:"
		    "if\\s +.*:"
		    "elif\\s +.*:"
		    (concat py-block-closing-keywords-re "[ \t\n]")
		    )
	      "\\|")
	  "\\)")
  "Regular expression matching lines not to dedent after.")

(defconst py-defun-start-re
  "^\\([ \t]*\\)def[ \t]+\\([a-zA-Z_0-9]+\\)\\|\\(^[a-zA-Z_0-9]+\\)[ \t]*="
  ;; If you change this, you probably have to change py-current-defun
  ;; as well.  This is only used by py-current-defun to find the name
  ;; for add-log.el.
  "Regular expression matching a function, method, or variable assignment.")

(defconst py-class-start-re "^class[ \t]*\\([a-zA-Z_0-9]+\\)"
  ;; If you change this, you probably have to change py-current-defun
  ;; as well.  This is only used by py-current-defun to find the name
  ;; for add-log.el.
  "Regular expression for finding a class name.")

(defconst py-traceback-line-re
  "[ \t]+File \"\\([^\"]+\\)\", line \\([0-9]+\\)"
  "Regular expression that describes tracebacks.")



;; Major mode boilerplate

;; define a mode-specific abbrev table for those who use such things
(defvar python-mode-abbrev-table nil
  "Abbrev table in use in `python-mode' buffers.")
(define-abbrev-table 'python-mode-abbrev-table nil)

(defvar python-mode-hook nil
  "*Hook called by `python-mode'.")

;; In previous version of python-mode.el, the hook was incorrectly
;; called py-mode-hook, and was not defvar'd.  Deprecate its use.
(and (fboundp 'make-obsolete-variable)
     (make-obsolete-variable 'py-mode-hook 'python-mode-hook))

(defvar py-mode-map ()
  "Keymap used in `python-mode' buffers.")
(if py-mode-map
    nil
  (setq py-mode-map (make-sparse-keymap))
  ;; electric keys
  (define-key py-mode-map ":" 'py-electric-colon)
  ;; indentation level modifiers
  (define-key py-mode-map "\C-c\C-l"  'py-shift-region-left)
  (define-key py-mode-map "\C-c\C-r"  'py-shift-region-right)
  (define-key py-mode-map "\C-c<"     'py-shift-region-left)
  (define-key py-mode-map "\C-c>"     'py-shift-region-right)
  ;; subprocess commands
  (define-key py-mode-map "\C-c\C-c"  'py-execute-buffer)
  (define-key py-mode-map "\C-c\C-m"  'py-execute-import-or-reload)
  (define-key py-mode-map "\C-c\C-s"  'py-execute-string)
  (define-key py-mode-map "\C-c|"     'py-execute-region)
  (define-key py-mode-map "\e\C-x"    'py-execute-def-or-class)
  (define-key py-mode-map "\C-c!"     'py-shell)
  (define-key py-mode-map "\C-c\C-t"  'py-toggle-shells)
  ;; Caution!  Enter here at your own risk.  We are trying to support
  ;; several behaviors and it gets disgusting. :-( This logic ripped
  ;; largely from CC Mode.
  ;;
  ;; In XEmacs 19, Emacs 19, and Emacs 20, we use this to bind
  ;; backwards deletion behavior to DEL, which both Delete and
  ;; Backspace get translated to.  There's no way to separate this
  ;; behavior in a clean way, so deal with it!  Besides, it's been
  ;; this way since the dawn of time.
  (if (not (boundp 'delete-key-deletes-forward))
      (define-key py-mode-map "\177" 'py-electric-backspace)
    ;; However, XEmacs 20 actually achieved enlightenment.  It is
    ;; possible to sanely define both backward and forward deletion
    ;; behavior under X separately (TTYs are forever beyond hope, but
    ;; who cares?  XEmacs 20 does the right thing with these too).
    (define-key py-mode-map [delete]    'py-electric-delete)
    (define-key py-mode-map [backspace] 'py-electric-backspace))
  ;; Separate M-BS from C-M-h.  The former should remain
  ;; backward-kill-word.
  (define-key py-mode-map [(control meta h)] 'py-mark-def-or-class)
  (define-key py-mode-map "\C-c\C-k"  'py-mark-block)
  ;; Miscellaneous
  (define-key py-mode-map "\C-c:"     'py-guess-indent-offset)
  (define-key py-mode-map "\C-c\t"    'py-indent-region)
  (define-key py-mode-map "\C-c\C-n"  'py-next-statement)
  (define-key py-mode-map "\C-c\C-p"  'py-previous-statement)
  (define-key py-mode-map "\C-c\C-u"  'py-goto-block-up)
  (define-key py-mode-map "\C-c#"     'py-comment-region)
  (define-key py-mode-map "\C-c?"     'py-describe-mode)
  (define-key py-mode-map "\C-c\C-hm" 'py-describe-mode)
  (define-key py-mode-map "\e\C-a"    'py-beginning-of-def-or-class)
  (define-key py-mode-map "\e\C-e"    'py-end-of-def-or-class)
  (define-key py-mode-map "\C-c-"     'py-up-exception)
  (define-key py-mode-map "\C-c="     'py-down-exception)
  ;; stuff that is `standard' but doesn't interface well with
  ;; python-mode, which forces us to rebind to special commands
  (define-key py-mode-map "\C-xnd"    'py-narrow-to-defun)
  ;; information
  (define-key py-mode-map "\C-c\C-b" 'py-submit-bug-report)
  (define-key py-mode-map "\C-c\C-v" 'py-version)
  ;; shadow global bindings for newline-and-indent w/ the py- version.
  ;; BAW - this is extremely bad form, but I'm not going to change it
  ;; for now.
  (mapcar #'(lambda (key)
	      (define-key py-mode-map key 'py-newline-and-indent))
	  (where-is-internal 'newline-and-indent))
  ;; Force RET to be py-newline-and-indent even if it didn't get
  ;; mapped by the above code.  motivation: Emacs' default binding for
  ;; RET is `newline' and C-j is `newline-and-indent'.  Most Pythoneers
  ;; expect RET to do a `py-newline-and-indent' and any Emacsers who
  ;; dislike this are probably knowledgeable enough to do a rebind.
  ;; However, we do *not* change C-j since many Emacsers have already
  ;; swapped RET and C-j and they don't want C-j bound to `newline' to 
  ;; change.
  (define-key py-mode-map "\C-m" 'py-newline-and-indent)
  )

(defvar py-mode-output-map nil
  "Keymap used in *Python Output* buffers.")
(if py-mode-output-map
    nil
  (setq py-mode-output-map (make-sparse-keymap))
  (define-key py-mode-output-map [button2]  'py-mouseto-exception)
  (define-key py-mode-output-map "\C-c\C-c" 'py-goto-exception)
  ;; TBD: Disable all self-inserting keys.  This is bogus, we should
  ;; really implement this as *Python Output* buffer being read-only
  (mapcar #' (lambda (key)
	       (define-key py-mode-output-map key
		 #'(lambda () (interactive) (beep))))
	     (where-is-internal 'self-insert-command))
  )

(defvar py-shell-map nil
  "Keymap used in *Python* shell buffers.")
(if py-shell-map
    nil
  (setq py-shell-map (copy-keymap comint-mode-map))
  (define-key py-shell-map [tab]   'tab-to-tab-stop)
  (define-key py-shell-map "\C-c-" 'py-up-exception)
  (define-key py-shell-map "\C-c=" 'py-down-exception)
  )

(defvar py-mode-syntax-table nil
  "Syntax table used in `python-mode' buffers.")
(if py-mode-syntax-table
    nil
  (setq py-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\( "()" py-mode-syntax-table)
  (modify-syntax-entry ?\) ")(" py-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]" py-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" py-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}" py-mode-syntax-table)
  (modify-syntax-entry ?\} "){" py-mode-syntax-table)
  ;; Add operator symbols misassigned in the std table
  (modify-syntax-entry ?\$ "."  py-mode-syntax-table)
  (modify-syntax-entry ?\% "."  py-mode-syntax-table)
  (modify-syntax-entry ?\& "."  py-mode-syntax-table)
  (modify-syntax-entry ?\* "."  py-mode-syntax-table)
  (modify-syntax-entry ?\+ "."  py-mode-syntax-table)
  (modify-syntax-entry ?\- "."  py-mode-syntax-table)
  (modify-syntax-entry ?\/ "."  py-mode-syntax-table)
  (modify-syntax-entry ?\< "."  py-mode-syntax-table)
  (modify-syntax-entry ?\= "."  py-mode-syntax-table)
  (modify-syntax-entry ?\> "."  py-mode-syntax-table)
  (modify-syntax-entry ?\| "."  py-mode-syntax-table)
  ;; For historical reasons, underscore is word class instead of
  ;; symbol class.  GNU conventions say it should be symbol class, but
  ;; there's a natural conflict between what major mode authors want
  ;; and what users expect from `forward-word' and `backward-word'.
  ;; Guido and I have hashed this out and have decided to keep
  ;; underscore in word class.  If you're tempted to change it, try
  ;; binding M-f and M-b to py-forward-into-nomenclature and
  ;; py-backward-into-nomenclature instead.  This doesn't help in all
  ;; situations where you'd want the different behavior
  ;; (e.g. backward-kill-word).
  (modify-syntax-entry ?\_ "w"  py-mode-syntax-table)
  ;; Both single quote and double quote are string delimiters
  (modify-syntax-entry ?\' "\"" py-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" py-mode-syntax-table)
  ;; backquote is open and close paren
  (modify-syntax-entry ?\` "$"  py-mode-syntax-table)
  ;; comment delimiters
  (modify-syntax-entry ?\# "<"  py-mode-syntax-table)
  (modify-syntax-entry ?\n ">"  py-mode-syntax-table)
  )



;; Utilities

(defmacro py-safe (&rest body)
  "Safely execute BODY, return nil if an error occurred."
  (` (condition-case nil
	 (progn (,@ body))
       (error nil))))

(defsubst py-keep-region-active ()
  "Keep the region active in XEmacs."
  ;; Ignore byte-compiler warnings you might see.  Also note that
  ;; FSF's Emacs 19 does it differently; its policy doesn't require us
  ;; to take explicit action.
  (and (boundp 'zmacs-region-stays)
       (setq zmacs-region-stays t)))

(defsubst py-point (position)
  "Returns the value of point at certain commonly referenced POSITIONs.
POSITION can be one of the following symbols:

  bol  -- beginning of line
  eol  -- end of line
  bod  -- beginning of def or class
  eod  -- end of def or class
  bob  -- beginning of buffer
  eob  -- end of buffer
  boi  -- back to indentation
  bos  -- beginning of statement

This function does not modify point or mark."
  (let ((here (point)))
    (cond
     ((eq position 'bol) (beginning-of-line))
     ((eq position 'eol) (end-of-line))
     ((eq position 'bod) (py-beginning-of-def-or-class))
     ((eq position 'eod) (py-end-of-def-or-class))
     ;; Kind of funny, I know, but useful for py-up-exception.
     ((eq position 'bob) (beginning-of-buffer))
     ((eq position 'eob) (end-of-buffer))
     ((eq position 'boi) (back-to-indentation))
     ((eq position 'bos) (py-goto-initial-line))
     (t (error "Unknown buffer position requested: %s" position))
     )
    (prog1
	(point)
      (goto-char here))))

(defsubst py-highlight-line (from to file line)
  (cond
   ((fboundp 'make-extent)
    ;; XEmacs
    (let ((e (make-extent from to)))
      (set-extent-property e 'mouse-face 'highlight)
      (set-extent-property e 'py-exc-info (cons file line))
      (set-extent-property e 'keymap py-mode-output-map)))
   (t
    ;; Emacs -- Please port this!
    )
   ))

(defun py-in-literal (&optional lim)
  "Return non-nil if point is in a Python literal (a comment or string).
Optional argument LIM indicates the beginning of the containing form,
i.e. the limit on how far back to scan."
  ;; This is the version used for non-XEmacs, which has a nicer
  ;; interface.
  ;;
  ;; WARNING: Watch out for infinite recursion.
  (let* ((lim (or lim (py-point 'bod)))
	 (state (parse-partial-sexp lim (point))))
    (cond
     ((nth 3 state) 'string)
     ((nth 4 state) 'comment)
     (t nil))))

;; XEmacs has a built-in function that should make this much quicker.
;; In this case, lim is ignored
(defun py-fast-in-literal (&optional lim)
  "Fast version of `py-in-literal', used only by XEmacs.
Optional LIM is ignored."
  ;; don't have to worry about context == 'block-comment
  (buffer-syntactic-context))

(if (fboundp 'buffer-syntactic-context)
    (defalias 'py-in-literal 'py-fast-in-literal))



;; Menu definitions, only relevent if you have the easymenu.el package
;; (standard in the latest Emacs 19 and XEmacs 19 distributions).
(defvar py-menu nil
  "Menu for Python Mode.
This menu will get created automatically if you have the `easymenu'
package.  Note that the latest X/Emacs releases contain this package.")

(and (py-safe (require 'easymenu) t)
     (easy-menu-define
      py-menu py-mode-map "Python Mode menu"
      '("Python"
	["Comment Out Region"   py-comment-region  (mark)]
	["Uncomment Region"     (py-comment-region (point) (mark) '(4)) (mark)]
	"-"
	["Mark current block"   py-mark-block t]
	["Mark current def"     py-mark-def-or-class t]
	["Mark current class"   (py-mark-def-or-class t) t]
	"-"
	["Shift region left"    py-shift-region-left (mark)]
	["Shift region right"   py-shift-region-right (mark)]
	"-"
	["Import/reload file"   py-execute-import-or-reload t]
	["Execute buffer"       py-execute-buffer t]
	["Execute region"       py-execute-region (mark)]
	["Execute def or class" py-execute-def-or-class (mark)]
	["Execute string"       py-execute-string t]
	["Start interpreter..." py-shell t]
	"-"
	["Go to start of block" py-goto-block-up t]
	["Go to start of class" (py-beginning-of-def-or-class t) t]
	["Move to end of class" (py-end-of-def-or-class t) t]
	["Move to start of def" py-beginning-of-def-or-class t]
	["Move to end of def"   py-end-of-def-or-class t]
	"-"
	["Describe mode"        py-describe-mode t]
	)))



;; Imenu definitions
(defvar py-imenu-class-regexp
  (concat				; <<classes>>
   "\\("				;
   "^[ \t]*"				; newline and maybe whitespace
   "\\(class[ \t]+[a-zA-Z0-9_]+\\)"	; class name
					; possibly multiple superclasses
   "\\([ \t]*\\((\\([a-zA-Z0-9_,. \t\n]\\)*)\\)?\\)"
   "[ \t]*:"				; and the final :
   "\\)"				; >>classes<<
   )
  "Regexp for Python classes for use with the Imenu package."
  )

(defvar py-imenu-method-regexp
  (concat                               ; <<methods and functions>>
   "\\("                                ; 
   "^[ \t]*"                            ; new line and maybe whitespace
   "\\(def[ \t]+"                       ; function definitions start with def
   "\\([a-zA-Z0-9_]+\\)"                ;   name is here
					;   function arguments...
;;   "[ \t]*(\\([-+/a-zA-Z0-9_=,\* \t\n.()\"'#]*\\))"
   "[ \t]*(\\([^:#]*\\))"
   "\\)"                                ; end of def
   "[ \t]*:"                            ; and then the :
   "\\)"                                ; >>methods and functions<<
   )
  "Regexp for Python methods/functions for use with the Imenu package."
  )

(defvar py-imenu-method-no-arg-parens '(2 8)
  "Indices into groups of the Python regexp for use with Imenu.

Using these values will result in smaller Imenu lists, as arguments to
functions are not listed.

See the variable `py-imenu-show-method-args-p' for more
information.")

(defvar py-imenu-method-arg-parens '(2 7)
  "Indices into groups of the Python regexp for use with imenu.
Using these values will result in large Imenu lists, as arguments to
functions are listed.

See the variable `py-imenu-show-method-args-p' for more
information.")

;; Note that in this format, this variable can still be used with the
;; imenu--generic-function. Otherwise, there is no real reason to have
;; it.
(defvar py-imenu-generic-expression
  (cons
   (concat 
    py-imenu-class-regexp
    "\\|"				; or...
    py-imenu-method-regexp
    )
   py-imenu-method-no-arg-parens)
  "Generic Python expression which may be used directly with Imenu.
Used by setting the variable `imenu-generic-expression' to this value.
Also, see the function \\[py-imenu-create-index] for a better
alternative for finding the index.")

;; These next two variables are used when searching for the Python
;; class/definitions. Just saving some time in accessing the
;; generic-python-expression, really.
(defvar py-imenu-generic-regexp nil)
(defvar py-imenu-generic-parens nil)


(defun py-imenu-create-index-function ()
  "Python interface function for the Imenu package.
Finds all Python classes and functions/methods. Calls function
\\[py-imenu-create-index-engine].  See that function for the details
of how this works."
  (setq py-imenu-generic-regexp (car py-imenu-generic-expression)
	py-imenu-generic-parens (if py-imenu-show-method-args-p
				    py-imenu-method-arg-parens
				  py-imenu-method-no-arg-parens))
  (goto-char (point-min))
  ;; Warning: When the buffer has no classes or functions, this will
  ;; return nil, which seems proper according to the Imenu API, but
  ;; causes an error in the XEmacs port of Imenu.  Sigh.
  (py-imenu-create-index-engine nil))

(defun py-imenu-create-index-engine (&optional start-indent)
  "Function for finding Imenu definitions in Python.

Finds all definitions (classes, methods, or functions) in a Python
file for the Imenu package.

Returns a possibly nested alist of the form

	(INDEX-NAME . INDEX-POSITION)

The second element of the alist may be an alist, producing a nested
list as in

	(INDEX-NAME . INDEX-ALIST)

This function should not be called directly, as it calls itself
recursively and requires some setup.  Rather this is the engine for
the function \\[py-imenu-create-index-function].

It works recursively by looking for all definitions at the current
indention level.  When it finds one, it adds it to the alist.  If it
finds a definition at a greater indentation level, it removes the
previous definition from the alist. In its place it adds all
definitions found at the next indentation level.  When it finds a
definition that is less indented then the current level, it returns
the alist it has created thus far.

The optional argument START-INDENT indicates the starting indentation
at which to continue looking for Python classes, methods, or
functions.  If this is not supplied, the function uses the indentation
of the first definition found."
  (let (index-alist
	sub-method-alist
	looking-p
	def-name prev-name
	cur-indent def-pos
	(class-paren (first  py-imenu-generic-parens)) 
	(def-paren   (second py-imenu-generic-parens)))
    (setq looking-p
	  (re-search-forward py-imenu-generic-regexp (point-max) t))
    (while looking-p
      (save-excursion
	;; used to set def-name to this value but generic-extract-name
	;; is new to imenu-1.14. this way it still works with
	;; imenu-1.11
	;;(imenu--generic-extract-name py-imenu-generic-parens))
	(let ((cur-paren (if (match-beginning class-paren)
			     class-paren def-paren)))
	  (setq def-name
		(buffer-substring-no-properties (match-beginning cur-paren)
						(match-end cur-paren))))
	(save-match-data
	  (py-beginning-of-def-or-class 'either))
	(beginning-of-line)
	(setq cur-indent (current-indentation)))
      ;; HACK: want to go to the next correct definition location.  We
      ;; explicitly list them here but it would be better to have them
      ;; in a list.
      (setq def-pos
	    (or (match-beginning class-paren)
		(match-beginning def-paren)))
      ;; if we don't have a starting indent level, take this one
      (or start-indent
	  (setq start-indent cur-indent))
      ;; if we don't have class name yet, take this one
      (or prev-name
	  (setq prev-name def-name))
      ;; what level is the next definition on?  must be same, deeper
      ;; or shallower indentation
      (cond
       ;; at the same indent level, add it to the list...
       ((= start-indent cur-indent)
	(push (cons def-name def-pos) index-alist))
       ;; deeper indented expression, recurse
       ((< start-indent cur-indent)
	;; the point is currently on the expression we're supposed to
	;; start on, so go back to the last expression. The recursive
	;; call will find this place again and add it to the correct
	;; list
	(re-search-backward py-imenu-generic-regexp (point-min) 'move)
	(setq sub-method-alist (py-imenu-create-index-engine cur-indent))
	(if sub-method-alist
	    ;; we put the last element on the index-alist on the start
	    ;; of the submethod alist so the user can still get to it.
	    (let ((save-elmt (pop index-alist)))
	      (push (cons prev-name
			  (cons save-elmt sub-method-alist))
		    index-alist))))
       ;; found less indented expression, we're done.
       (t 
	(setq looking-p nil)
	(re-search-backward py-imenu-generic-regexp (point-min) t)))
      ;; end-cond
      (setq prev-name def-name)
      (and looking-p
	   (setq looking-p
		 (re-search-forward py-imenu-generic-regexp
				    (point-max) 'move))))
    (nreverse index-alist)))


;;;###autoload
(defun python-mode ()
  "Major mode for editing Python files.
To submit a problem report, enter `\\[py-submit-bug-report]' from a
`python-mode' buffer.  Do `\\[py-describe-mode]' for detailed
documentation.  To see what version of `python-mode' you are running,
enter `\\[py-version]'.

This mode knows about Python indentation, tokens, comments and
continuation lines.  Paragraphs are separated by blank lines only.

COMMANDS
\\{py-mode-map}
VARIABLES

py-indent-offset\t\tindentation increment
py-block-comment-prefix\t\tcomment string used by `comment-region'
py-python-command\t\tshell command to invoke Python interpreter
py-temp-directory\t\tdirectory used for temp files (if needed)
py-beep-if-tab-change\t\tring the bell if `tab-width' is changed"
  (interactive)
  ;; set up local variables
  (kill-all-local-variables)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'require-final-newline)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-indent-function)
  (make-local-variable 'indent-region-function)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'add-log-current-defun-function)
  ;;
  (set-syntax-table py-mode-syntax-table)
  (setq major-mode              'python-mode
	mode-name               "Python"
	local-abbrev-table      python-mode-abbrev-table
	font-lock-defaults      '(python-font-lock-keywords)
	paragraph-separate      "^[ \t]*$"
	paragraph-start         "^[ \t]*$"
	require-final-newline   t
	comment-start           "# "
	comment-end             ""
	comment-start-skip      "# *"
	comment-column          40
	comment-indent-function 'py-comment-indent-function
	indent-region-function  'py-indent-region
	indent-line-function    'py-indent-line
	;; tell add-log.el how to find the current function/method/variable
	add-log-current-defun-function 'py-current-defun
	)
  (use-local-map py-mode-map)
  ;; add the menu
  (if py-menu
      (easy-menu-add py-menu))
  ;; Emacs 19 requires this
  (if (boundp 'comment-multi-line)
      (setq comment-multi-line nil))
  ;; Install Imenu if available
  (when (py-safe (require 'imenu))
    (setq imenu-create-index-function #'py-imenu-create-index-function)
    (setq imenu-generic-expression py-imenu-generic-expression)
    (if (fboundp 'imenu-add-to-menubar)
	(imenu-add-to-menubar (format "%s-%s" "IM" mode-name)))
    )
  ;; Run the mode hook.  Note that py-mode-hook is deprecated.
  (if python-mode-hook
      (run-hooks 'python-mode-hook)
    (run-hooks 'py-mode-hook))
  ;; Now do the automagical guessing
  (if py-smart-indentation
    (let ((offset py-indent-offset))
      ;; It's okay if this fails to guess a good value
      (if (and (py-safe (py-guess-indent-offset))
	       (<= py-indent-offset 8)
	       (>= py-indent-offset 2))
	  (setq offset py-indent-offset))
      (setq py-indent-offset offset)
      ;; Only turn indent-tabs-mode off if tab-width !=
      ;; py-indent-offset.  Never turn it on, because the user must
      ;; have explicitly turned it off.
      (if (/= tab-width py-indent-offset)
	  (setq indent-tabs-mode nil))
      ))
  ;; Set the default shell if not already set
  (when (null py-which-shell)
    (py-toggle-shells py-default-interpreter))
  )


;; electric characters
(defun py-outdent-p ()
  "Returns non-nil if the current line should dedent one level."
  (save-excursion
    (and (progn (back-to-indentation)
		(looking-at py-outdent-re))
	 ;; short circuit infloop on illegal construct
	 (not (bobp))
	 (progn (forward-line -1)
		(py-goto-initial-line)
		(back-to-indentation)
		(while (or (looking-at py-blank-or-comment-re)
			   (bobp))
		  (backward-to-indentation 1))
		(not (looking-at py-no-outdent-re)))
	 )))
      
(defun py-electric-colon (arg)
  "Insert a colon.
In certain cases the line is dedented appropriately.  If a numeric
argument ARG is provided, that many colons are inserted
non-electrically.  Electric behavior is inhibited inside a string or
comment."
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  ;; are we in a string or comment?
  (if (save-excursion
	(let ((pps (parse-partial-sexp (save-excursion
					 (py-beginning-of-def-or-class)
					 (point))
				       (point))))
	  (not (or (nth 3 pps) (nth 4 pps)))))
      (save-excursion
	(let ((here (point))
	      (outdent 0)
	      (indent (py-compute-indentation t)))
	  (if (and (not arg)
		   (py-outdent-p)
		   (= indent (save-excursion
			       (py-next-statement -1)
			       (py-compute-indentation t)))
		   )
	      (setq outdent py-indent-offset))
	  ;; Don't indent, only dedent.  This assumes that any lines
	  ;; that are already dedented relative to
	  ;; py-compute-indentation were put there on purpose.  It's
	  ;; highly annoying to have `:' indent for you.  Use TAB, C-c
	  ;; C-l or C-c C-r to adjust.  TBD: Is there a better way to
	  ;; determine this???
	  (if (< (current-indentation) indent) nil
	    (goto-char here)
	    (beginning-of-line)
	    (delete-horizontal-space)
	    (indent-to (- indent outdent))
	    )))))


;; Python subprocess utilities and filters
(defun py-execute-file (proc filename)
  "Send to Python interpreter process PROC \"execfile('FILENAME')\".
Make that process's buffer visible and force display.  Also make
comint believe the user typed this string so that
`kill-output-from-shell' does The Right Thing."
  (let ((curbuf (current-buffer))
	(procbuf (process-buffer proc))
;	(comint-scroll-to-bottom-on-output t)
	(msg (format "## working on region in file %s...\n" filename))
	(cmd (format "execfile(r'%s')\n" filename)))
    (unwind-protect
	(save-excursion
	  (set-buffer procbuf)
	  (goto-char (point-max))
	  (move-marker (process-mark proc) (point))
	  (funcall (process-filter proc) proc msg))
      (set-buffer curbuf))
    (process-send-string proc cmd)))

(defun py-comint-output-filter-function (string)
  "Watch output for Python prompt and exec next file waiting in queue.
This function is appropriate for `comint-output-filter-functions'."
  ;; TBD: this should probably use split-string
  (when (and (or (string-equal string ">>> ")
		 (and (>= (length string) 5)
		      (string-equal (substring string -5) "\n>>> ")))
	     py-file-queue)
    (py-safe (delete-file (car py-file-queue)))
    (setq py-file-queue (cdr py-file-queue))
    (if py-file-queue
	(let ((pyproc (get-buffer-process (current-buffer))))
	  (py-execute-file pyproc (car py-file-queue))))
    ))

(defun py-postprocess-output-buffer (buf)
  "Highlight exceptions found in BUF.
If an exception occurred return t, otherwise return nil.  BUF must exist."
  (let (line file bol err-p)
    (save-excursion
      (set-buffer buf)
      (beginning-of-buffer)
      (while (re-search-forward py-traceback-line-re nil t)
	(setq file (match-string 1)
	      line (string-to-int (match-string 2))
	      bol (py-point 'bol))
	(py-highlight-line bol (py-point 'eol) file line)))
    (when (and py-jump-on-exception line)
      (beep)
      (py-jump-to-exception file line)
      (setq err-p t))
    err-p))



;;; Subprocess commands

;; only used when (memq 'broken-temp-names py-emacs-features)
(defvar py-serial-number 0)
(defvar py-exception-buffer nil)
(defconst py-output-buffer "*Python Output*")
(make-variable-buffer-local 'py-output-buffer)

;; for toggling between CPython and JPython
(defvar py-which-shell nil)
(defvar py-which-args  py-python-command-args)
(defvar py-which-bufname "Python")
(make-variable-buffer-local 'py-which-shell)
(make-variable-buffer-local 'py-which-args)
(make-variable-buffer-local 'py-which-bufname)

(defun py-toggle-shells (arg)
  "Toggles between the CPython and JPython shells.

With positive argument ARG (interactively \\[universal-argument]),
uses the CPython shell, with negative ARG uses the JPython shell, and
with a zero argument, toggles the shell.

Programmatically, ARG can also be one of the symbols `cpython' or
`jpython', equivalent to positive arg and negative arg respectively."
  (interactive "P")
  ;; default is to toggle
  (if (null arg)
      (setq arg 0))
  ;; preprocess arg
  (cond
   ((equal arg 0)
    ;; toggle
    (if (string-equal py-which-bufname "Python")
	(setq arg -1)
      (setq arg 1)))
   ((equal arg 'cpython) (setq arg 1))
   ((equal arg 'jpython) (setq arg -1)))
  (let (msg)
    (cond
     ((< 0 arg)
      ;; set to CPython
      (setq py-which-shell py-python-command
	    py-which-args py-python-command-args
	    py-which-bufname "Python"
	    msg "CPython"
	    mode-name "Python"))
     ((> 0 arg)
      (setq py-which-shell py-jpython-command
	    py-which-args py-jpython-command-args
	    py-which-bufname "JPython"
	    msg "JPython"
	    mode-name "JPython"))
     )
    (message "Using the %s shell" msg)
    (setq py-output-buffer (format "*%s Output*" py-which-bufname))))

;;;###autoload
(defun py-shell (&optional argprompt)
  "Start an interactive Python interpreter in another window.
This is like Shell mode, except that Python is running in the window
instead of a shell.  See the `Interactive Shell' and `Shell Mode'
sections of the Emacs manual for details, especially for the key
bindings active in the `*Python*' buffer.

With optional \\[universal-argument], the user is prompted for the
flags to pass to the Python interpreter.  This has no effect when this
command is used to switch to an existing process, only when a new
process is started.  If you use this, you will probably want to ensure
that the current arguments are retained (they will be included in the
prompt).  This argument is ignored when this function is called
programmatically, or when running in Emacs 19.34 or older.

Note: You can toggle between using the CPython interpreter and the
JPython interpreter by hitting \\[py-toggle-shells].  This toggles
buffer local variables which control whether all your subshell
interactions happen to the `*JPython*' or `*Python*' buffers (the
latter is the name used for the CPython buffer).

Warning: Don't use an interactive Python if you change sys.ps1 or
sys.ps2 from their default values, or if you're running code that
prints `>>> ' or `... ' at the start of a line.  `python-mode' can't
distinguish your output from Python's output, and assumes that `>>> '
at the start of a line is a prompt from Python.  Similarly, the Emacs
Shell mode code assumes that both `>>> ' and `... ' at the start of a
line are Python prompts.  Bad things can happen if you fool either
mode.

Warning:  If you do any editing *in* the process buffer *while* the
buffer is accepting output from Python, do NOT attempt to `undo' the
changes.  Some of the output (nowhere near the parts you changed!) may
be lost if you do.  This appears to be an Emacs bug, an unfortunate
interaction between undo and process filters; the same problem exists in
non-Python process buffers using the default (Emacs-supplied) process
filter."
  (interactive "P")
  ;; Set the default shell if not already set
  (when (null py-which-shell)
    (py-toggle-shells py-default-interpreter))
  (let ((args py-which-args))
    (when (and argprompt
	       (interactive-p)
	       (fboundp 'split-string))
      ;; TBD: Perhaps force "-i" in the final list?
      (setq args (split-string
		  (read-string (concat py-which-bufname
				       " arguments: ")
			       (concat
				(mapconcat 'identity py-which-args " ") " ")
			       ))))
    (switch-to-buffer-other-window
     (apply 'make-comint py-which-bufname py-which-shell nil args))
    (make-local-variable 'comint-prompt-regexp)
    (setq comint-prompt-regexp "^>>> \\|^[.][.][.] \\|^(pdb) ")
    (add-hook 'comint-output-filter-functions
	      'py-comint-output-filter-function)
    (set-syntax-table py-mode-syntax-table)
    (use-local-map py-shell-map)
    ))

(defun py-clear-queue ()
  "Clear the queue of temporary files waiting to execute."
  (interactive)
  (let ((n (length py-file-queue)))
    (mapcar 'delete-file py-file-queue)
    (setq py-file-queue nil)
    (message "%d pending files de-queued." n)))


(defun py-execute-region (start end &optional async)
  "Execute the region in a Python interpreter.

The region is first copied into a temporary file (in the directory
`py-temp-directory').  If there is no Python interpreter shell
running, this file is executed synchronously using
`shell-command-on-region'.  If the program is long running, use
\\[universal-argument] to run the command asynchronously in its own
buffer.

When this function is used programmatically, arguments START and END
specify the region to execute, and optional third argument ASYNC, if
non-nil, specifies to run the command asynchronously in its own
buffer.

If the Python interpreter shell is running, the region is execfile()'d
in that shell.  If you try to execute regions too quickly,
`python-mode' will queue them up and execute them one at a time when
it sees a `>>> ' prompt from Python.  Each time this happens, the
process buffer is popped into a window (if it's not already in some
window) so you can see it, and a comment of the form

    \t## working on region in file <name>...

is inserted at the end.  See also the command `py-clear-queue'."
  (interactive "r\nP")
  (or (< start end)
      (error "Region is empty"))
  (let* ((proc (get-process py-which-bufname))
	 (temp (if (memq 'broken-temp-names py-emacs-features)
		   (let
		       ((sn py-serial-number)
			(pid (and (fboundp 'emacs-pid) (emacs-pid))))
		     (setq py-serial-number (1+ py-serial-number))
		     (if pid
			 (format "python-%d-%d" sn pid)
		       (format "python-%d" sn)))
		 (make-temp-name "python-")))
	 (file (expand-file-name temp py-temp-directory)))
    (write-region start end file nil 'nomsg)
    (cond
     ;; always run the code in its own asynchronous subprocess
     (async
      (let* ((buf (generate-new-buffer-name py-output-buffer))
	     ;; TBD: a horrible hack, but why create new Custom variables?
	     (arg (if (string-equal py-which-bufname "Python")
		      "-u" "")))
	(start-process py-which-bufname buf py-which-shell arg file)
	(pop-to-buffer buf)
	(py-postprocess-output-buffer buf)
	))
     ;; if the Python interpreter shell is running, queue it up for
     ;; execution there.
     (proc
      ;; use the existing python shell
      (if (not py-file-queue)
	  (py-execute-file proc file)
	(message "File %s queued for execution" file))
      (setq py-file-queue (append py-file-queue (list file)))
      (setq py-exception-buffer (cons file (current-buffer))))
     (t
      ;; TBD: a horrible hack, buy why create new Custom variables?
      (let ((cmd (concat py-which-shell
			 (if (string-equal py-which-bufname "JPython")
			     " -" ""))))
	;; otherwise either run it synchronously in a subprocess
	(shell-command-on-region start end cmd py-output-buffer)
	;; shell-command-on-region kills the output buffer if it never
	;; existed and there's no output from the command
	(if (not (get-buffer py-output-buffer))
	    (message "No output.")
	  (setq py-exception-buffer (current-buffer))
	  (let ((err-p (py-postprocess-output-buffer py-output-buffer)))
	    (pop-to-buffer py-output-buffer)
	    (if err-p
		(pop-to-buffer py-exception-buffer)))
	  )))
     )))


;; Code execution commands
(defun py-execute-buffer (&optional async)
  "Send the contents of the buffer to a Python interpreter.
If the file local variable `py-master-file' is non-nil, execute the
named file instead of the buffer's file.

If there is a *Python* process buffer it is used.  If a clipping
restriction is in effect, only the accessible portion of the buffer is
sent.  A trailing newline will be supplied if needed.

See the `\\[py-execute-region]' docs for an account of some
subtleties, including the use of the optional ASYNC argument."
  (interactive "P")
  (if py-master-file
      (let* ((filename (expand-file-name py-master-file))
	     (buffer (or (get-file-buffer filename)
			 (find-file-noselect filename))))
	(set-buffer buffer)))
  (py-execute-region (point-min) (point-max) async))

(defun py-execute-import-or-reload (&optional async)
  "Import the current buffer's file in a Python interpreter.

If the file has already been imported, then do reload instead to get
the latest version.

If the file's name does not end in \".py\", then do execfile instead.

If the current buffer is not visiting a file, do `py-execute-buffer'
instead.

If the file local variable `py-master-file' is non-nil, import or
reload the named file instead of the buffer's file.  The file may be
saved based on the value of `py-execute-import-or-reload-save-p'.

See the `\\[py-execute-region]' docs for an account of some
subtleties, including the use of the optional ASYNC argument.

This may be preferable to `\\[py-execute-buffer]' because:

 - Definitions stay in their module rather than appearing at top
   level, where they would clutter the global namespace and not affect
   uses of qualified names (MODULE.NAME).

 - The Python debugger gets line number information about the functions."
  (interactive "P")
  ;; Check file local variable py-master-file
  (if py-master-file
      (let* ((filename (expand-file-name py-master-file))
             (buffer (or (get-file-buffer filename)
                         (find-file-noselect filename))))
        (set-buffer buffer)))
  (let ((file (buffer-file-name (current-buffer))))
    (if file
        (progn
	  ;; Maybe save some buffers
	  (save-some-buffers (not py-ask-about-save) nil)
          (py-execute-string
           (if (string-match "\\.py$" file)
               (let ((f (file-name-sans-extension
			 (file-name-nondirectory file))))
                 (format "if globals().has_key('%s'):\n    reload(%s)\nelse:\n    import %s\n"
                         f f f))
             (format "execfile(r'%s')\n" file))
           async))
      ;; else
      (py-execute-buffer async))))


(defun py-execute-def-or-class (&optional async)
  "Send the current function or class definition to a Python interpreter.

If there is a *Python* process buffer it is used.

See the `\\[py-execute-region]' docs for an account of some
subtleties, including the use of the optional ASYNC argument."
  (interactive "P")
  (save-excursion
    (py-mark-def-or-class)
    ;; mark is before point
    (py-execute-region (mark) (point) async)))


(defun py-execute-string (string &optional async)
  "Send the argument STRING to a Python interpreter.

If there is a *Python* process buffer it is used.

See the `\\[py-execute-region]' docs for an account of some
subtleties, including the use of the optional ASYNC argument."
  (interactive "sExecute Python command: ")
  (save-excursion
    (set-buffer (get-buffer-create
                 (generate-new-buffer-name " *Python Command*")))
    (insert string)
    (py-execute-region (point-min) (point-max) async)))



(defun py-jump-to-exception (file line)
  "Jump to the Python code in FILE at LINE."
  (let ((buffer (cond ((string-equal file "<stdin>")
		       (if (consp py-exception-buffer)
			   (cdr py-exception-buffer)
			 py-exception-buffer))
		      ((and (consp py-exception-buffer)
			    (string-equal file (car py-exception-buffer)))
		       (cdr py-exception-buffer))
		      ((py-safe (find-file-noselect file)))
		      ;; could not figure out what file the exception
		      ;; is pointing to, so prompt for it
		      (t (find-file (read-file-name "Exception file: "
						    nil
						    file t))))))
    (pop-to-buffer buffer)
    ;; Force Python mode
    (if (not (eq major-mode 'python-mode))
	(python-mode))
    (goto-line line)
    (message "Jumping to exception in file %s on line %d" file line)))

(defun py-mouseto-exception (event)
  "Jump to the code which caused the Python exception at EVENT.
EVENT is usually a mouse click."
  (interactive "e")
  (cond
   ((fboundp 'event-point)
    ;; XEmacs
    (let* ((point (event-point event))
	   (buffer (event-buffer event))
	   (e (and point buffer (extent-at point buffer 'py-exc-info)))
	   (info (and e (extent-property e 'py-exc-info))))
      (message "Event point: %d, info: %s" point info)
      (and info
	   (py-jump-to-exception (car info) (cdr info)))
      ))
   ;; Emacs -- Please port this!
   ))

(defun py-goto-exception ()
  "Go to the line indicated by the traceback."
  (interactive)
  (let (file line)
    (save-excursion
      (beginning-of-line)
      (if (looking-at py-traceback-line-re)
	  (setq file (match-string 1)
		line (string-to-int (match-string 2)))))
    (if (not file)
	(error "Not on a traceback line"))
    (py-jump-to-exception file line)))

(defun py-find-next-exception (start buffer searchdir errwhere)
  "Find the next Python exception and jump to the code that caused it.
START is the buffer position in BUFFER from which to begin searching
for an exception.  SEARCHDIR is a function, either
`re-search-backward' or `re-search-forward' indicating the direction
to search.  ERRWHERE is used in an error message if the limit (top or
bottom) of the trackback stack is encountered."
  (let (file line)
    (save-excursion
      (set-buffer buffer)
      (goto-char (py-point start))
      (if (funcall searchdir py-traceback-line-re nil t)
	  (setq file (match-string 1)
		line (string-to-int (match-string 2)))))
    (if (and file line)
	(py-jump-to-exception file line)
      (error "%s of traceback" errwhere))))

(defun py-down-exception (&optional bottom)
  "Go to the next line down in the traceback.
With \\[univeral-argument] (programmatically, optional argument
BOTTOM), jump to the bottom (innermost) exception in the exception
stack."
  (interactive "P")
  (let* ((proc (get-process "Python"))
	 (buffer (if proc "*Python*" py-output-buffer)))
    (if bottom
	(py-find-next-exception 'eob buffer 're-search-backward "Bottom")
      (py-find-next-exception 'eol buffer 're-search-forward "Bottom"))))

(defun py-up-exception (&optional top)
  "Go to the previous line up in the traceback.
With \\[universal-argument] (programmatically, optional argument TOP)
jump to the top (outermost) exception in the exception stack."
  (interactive "P")
  (let* ((proc (get-process "Python"))
	 (buffer (if proc "*Python*" py-output-buffer)))
    (if top
	(py-find-next-exception 'bob buffer 're-search-forward "Top")
      (py-find-next-exception 'bol buffer 're-search-backward "Top"))))


;; Electric deletion
(defun py-electric-backspace (arg)
  "Delete preceding character or levels of indentation.
Deletion is performed by calling the function in `py-backspace-function'
with a single argument (the number of characters to delete).

If point is at the leftmost column, delete the preceding newline.

Otherwise, if point is at the leftmost non-whitespace character of a
line that is neither a continuation line nor a non-indenting comment
line, or if point is at the end of a blank line, this command reduces
the indentation to match that of the line that opened the current
block of code.  The line that opened the block is displayed in the
echo area to help you keep track of where you are.  With
\\[universal-argument] dedents that many blocks (but not past column
zero).

Otherwise the preceding character is deleted, converting a tab to
spaces if needed so that only a single column position is deleted.
\\[universal-argument] specifies how many characters to delete;
default is 1.

When used programmatically, argument ARG specifies the number of
blocks to dedent, or the number of characters to delete, as indicated
above."
  (interactive "*p")
  (if (or (/= (current-indentation) (current-column))
	  (bolp)
	  (py-continuation-line-p)
;	  (not py-honor-comment-indentation)
;	  (looking-at "#[^ \t\n]")	; non-indenting #
	  )
      (funcall py-backspace-function arg)
    ;; else indent the same as the colon line that opened the block
    ;; force non-blank so py-goto-block-up doesn't ignore it
    (insert-char ?* 1)
    (backward-char)
    (let ((base-indent 0)		; indentation of base line
	  (base-text "")		; and text of base line
	  (base-found-p nil))
      (save-excursion
	(while (< 0 arg)
	  (condition-case nil		; in case no enclosing block
	      (progn
		(py-goto-block-up 'no-mark)
		(setq base-indent (current-indentation)
		      base-text   (py-suck-up-leading-text)
		      base-found-p t))
	    (error nil))
	  (setq arg (1- arg))))
      (delete-char 1)			; toss the dummy character
      (delete-horizontal-space)
      (indent-to base-indent)
      (if base-found-p
	  (message "Closes block: %s" base-text)))))


(defun py-electric-delete (arg)
  "Delete preceding or following character or levels of whitespace.

The behavior of this function depends on the variable
`delete-key-deletes-forward'.  If this variable is nil (or does not
exist, as in older Emacsen and non-XEmacs versions), then this
function behaves identically to \\[c-electric-backspace].

If `delete-key-deletes-forward' is non-nil and is supported in your
Emacs, then deletion occurs in the forward direction, by calling the
function in `py-delete-function'.

\\[universal-argument] (programmatically, argument ARG) specifies the
number of characters to delete (default is 1)."
  (interactive "*p")
  (if (or (and (fboundp 'delete-forward-p) ;XEmacs 21
	       (delete-forward-p))
	  (and (boundp 'delete-key-deletes-forward) ;XEmacs 20
	       delete-key-deletes-forward))
      (funcall py-delete-function arg)
    (py-electric-backspace arg)))

;; required for pending-del and delsel modes
(put 'py-electric-backspace 'delete-selection 'supersede) ;delsel
(put 'py-electric-backspace 'pending-delete   'supersede) ;pending-del
(put 'py-electric-delete    'delete-selection 'supersede) ;delsel
(put 'py-electric-delete    'pending-delete   'supersede) ;pending-del



(defun py-indent-line (&optional arg)
  "Fix the indentation of the current line according to Python rules.
With \\[universal-argument] (programmatically, the optional argument
ARG non-nil), ignore dedenting rules for block closing statements
(e.g. return, raise, break, continue, pass)

This function is normally bound to `indent-line-function' so
\\[indent-for-tab-command] will call it."
  (interactive "P")
  (let* ((ci (current-indentation))
	 (move-to-indentation-p (<= (current-column) ci))
	 (need (py-compute-indentation (not arg))))
    ;; see if we need to dedent
    (if (py-outdent-p)
	(setq need (- need py-indent-offset)))
    (if (/= ci need)
	(save-excursion
	  (beginning-of-line)
	  (delete-horizontal-space)
	  (indent-to need)))
    (if move-to-indentation-p (back-to-indentation))))

(defun py-newline-and-indent ()
  "Strives to act like the Emacs `newline-and-indent'.
This is just `strives to' because correct indentation can't be computed
from scratch for Python code.  In general, deletes the whitespace before
point, inserts a newline, and takes an educated guess as to how you want
the new line indented."
  (interactive)
  (let ((ci (current-indentation)))
    (if (< ci (current-column))		; if point beyond indentation
	(newline-and-indent)
      ;; else try to act like newline-and-indent "normally" acts
      (beginning-of-line)
      (insert-char ?\n 1)
      (move-to-column ci))))

(defun py-compute-indentation (honor-block-close-p)
  "Compute Python indentation.
When HONOR-BLOCK-CLOSE-P is non-nil, statements such as `return',
`raise', `break', `continue', and `pass' force one level of
dedenting."
  (save-excursion
    (beginning-of-line)
    (let* ((bod (py-point 'bod))
	   (pps (parse-partial-sexp bod (point)))
	   (boipps (parse-partial-sexp bod (py-point 'boi)))
	   placeholder)
      (cond
       ;; are we inside a multi-line string or comment?
       ((or (and (nth 3 pps) (nth 3 boipps))
	    (and (nth 4 pps) (nth 4 boipps)))
	(save-excursion
	  (if (not py-align-multiline-strings-p) 0
	    ;; skip back over blank & non-indenting comment lines
	    ;; note: will skip a blank or non-indenting comment line
	    ;; that happens to be a continuation line too
	    (re-search-backward "^[ \t]*\\([^ \t\n#]\\|#[ \t\n]\\)" nil 'move)
	    (back-to-indentation)
	    (current-column))))
       ;; are we on a continuation line?
       ((py-continuation-line-p)
	(let ((startpos (point))
	      (open-bracket-pos (py-nesting-level))
	      endpos searching found state)
	  (if open-bracket-pos
	      (progn
		;; align with first item in list; else a normal
		;; indent beyond the line with the open bracket
		(goto-char (1+ open-bracket-pos)) ; just beyond bracket
		;; is the first list item on the same line?
		(skip-chars-forward " \t")
		(if (null (memq (following-char) '(?\n ?# ?\\)))
					; yes, so line up with it
		    (current-column)
		  ;; first list item on another line, or doesn't exist yet
		  (forward-line 1)
		  (while (and (< (point) startpos)
			      (looking-at "[ \t]*[#\n\\\\]")) ; skip noise
		    (forward-line 1))
		  (if (and (< (point) startpos)
			   (/= startpos
			       (save-excursion
				 (goto-char (1+ open-bracket-pos))
				 (forward-comment (point-max))
				 (point))))
		      ;; again mimic the first list item
		      (current-indentation)
		    ;; else they're about to enter the first item
		    (goto-char open-bracket-pos)
		    (setq placeholder (point))
		    (py-goto-initial-line)
		    (py-goto-beginning-of-tqs
		     (save-excursion (nth 3 (parse-partial-sexp
					     placeholder (point)))))
		    (+ (current-indentation) py-indent-offset))))

	    ;; else on backslash continuation line
	    (forward-line -1)
	    (if (py-continuation-line-p) ; on at least 3rd line in block
		(current-indentation)	; so just continue the pattern
	      ;; else started on 2nd line in block, so indent more.
	      ;; if base line is an assignment with a start on a RHS,
	      ;; indent to 2 beyond the leftmost "="; else skip first
	      ;; chunk of non-whitespace characters on base line, + 1 more
	      ;; column
	      (end-of-line)
	      (setq endpos (point)  searching t)
	      (back-to-indentation)
	      (setq startpos (point))
	      ;; look at all "=" from left to right, stopping at first
	      ;; one not nested in a list or string
	      (while searching
		(skip-chars-forward "^=" endpos)
		(if (= (point) endpos)
		    (setq searching nil)
		  (forward-char 1)
		  (setq state (parse-partial-sexp startpos (point)))
		  (if (and (zerop (car state)) ; not in a bracket
			   (null (nth 3 state))) ; & not in a string
		      (progn
			(setq searching nil) ; done searching in any case
			(setq found
			      (not (or
				    (eq (following-char) ?=)
				    (memq (char-after (- (point) 2))
					  '(?< ?> ?!)))))))))
	      (if (or (not found)	; not an assignment
		      (looking-at "[ \t]*\\\\")) ; <=><spaces><backslash>
		  (progn
		    (goto-char startpos)
		    (skip-chars-forward "^ \t\n")))
	      (1+ (current-column))))))

       ;; not on a continuation line
       ((bobp) (current-indentation))

       ;; Dfn: "Indenting comment line".  A line containing only a
       ;; comment, but which is treated like a statement for
       ;; indentation calculation purposes.  Such lines are only
       ;; treated specially by the mode; they are not treated
       ;; specially by the Python interpreter.

       ;; The rules for indenting comment lines are a line where:
       ;;   - the first non-whitespace character is `#', and
       ;;   - the character following the `#' is whitespace, and
       ;;   - the line is dedented with respect to (i.e. to the left
       ;;     of) the indentation of the preceding non-blank line.

       ;; The first non-blank line following an indenting comment
       ;; line is given the same amount of indentation as the
       ;; indenting comment line.

       ;; All other comment-only lines are ignored for indentation
       ;; purposes.

       ;; Are we looking at a comment-only line which is *not* an
       ;; indenting comment line?  If so, we assume that it's been
       ;; placed at the desired indentation, so leave it alone.
       ;; Indenting comment lines are aligned as statements down
       ;; below.
       ((and (looking-at "[ \t]*#[^ \t\n]")
	     ;; NOTE: this test will not be performed in older Emacsen
	     (fboundp 'forward-comment)
	     (<= (current-indentation)
		 (save-excursion
		   (forward-comment (- (point-max)))
		   (current-indentation))))
	(current-indentation))

       ;; else indentation based on that of the statement that
       ;; precedes us; use the first line of that statement to
       ;; establish the base, in case the user forced a non-std
       ;; indentation for the continuation lines (if any)
       (t
	;; skip back over blank & non-indenting comment lines note:
	;; will skip a blank or non-indenting comment line that
	;; happens to be a continuation line too.  use fast Emacs 19
	;; function if it's there.
	(if (and (eq py-honor-comment-indentation nil)
		 (fboundp 'forward-comment))
	    (forward-comment (- (point-max)))
	  (let ((prefix-re (concat py-block-comment-prefix "[ \t]*"))
		done)
	    (while (not done)
	      (re-search-backward "^[ \t]*\\([^ \t\n#]\\|#\\)" nil 'move)
	      (setq done (or (bobp)
			     (and (eq py-honor-comment-indentation t)
				  (save-excursion
				    (back-to-indentation)
				    (not (looking-at prefix-re))
				    ))
			     (and (not (eq py-honor-comment-indentation t))
				  (save-excursion
				    (back-to-indentation)
				    (not (zerop (current-column)))))
			     ))
	      )))
	;; if we landed inside a string, go to the beginning of that
	;; string. this handles triple quoted, multi-line spanning
	;; strings.
	(py-goto-beginning-of-tqs (nth 3 (parse-partial-sexp bod (point))))
	;; now skip backward over continued lines
	(setq placeholder (point))
	(py-goto-initial-line)
	;; we may *now* have landed in a TQS, so find the beginning of
	;; this string.
	(py-goto-beginning-of-tqs
	 (save-excursion (nth 3 (parse-partial-sexp
				 placeholder (point)))))
	(+ (current-indentation)
	   (if (py-statement-opens-block-p)
	       py-indent-offset
	     (if (and honor-block-close-p (py-statement-closes-block-p))
		 (- py-indent-offset)
	       0)))
	)))))

(defun py-guess-indent-offset (&optional global)
  "Guess a good value for, and change, `py-indent-offset'.

By default, make a buffer-local copy of `py-indent-offset' with the
new value, so that other Python buffers are not affected.  With
\\[universal-argument] (programmatically, optional argument GLOBAL),
change the global value of `py-indent-offset'.  This affects all
Python buffers (that don't have their own buffer-local copy), both
those currently existing and those created later in the Emacs session.

Some people use a different value for `py-indent-offset' than you use.
There's no excuse for such foolishness, but sometimes you have to deal
with their ugly code anyway.  This function examines the file and sets
`py-indent-offset' to what it thinks it was when they created the
mess.

Specifically, it searches forward from the statement containing point,
looking for a line that opens a block of code.  `py-indent-offset' is
set to the difference in indentation between that line and the Python
statement following it.  If the search doesn't succeed going forward,
it's tried again going backward."
  (interactive "P")			; raw prefix arg
  (let (new-value
	(start (point))
	(restart (point))
	(found nil)
	colon-indent)
    (py-goto-initial-line)
    (while (not (or found (eobp)))
      (when (and (re-search-forward ":[ \t]*\\($\\|[#\\]\\)" nil 'move)
		 (not (py-in-literal restart)))
	(setq restart (point))
	(py-goto-initial-line)
	(if (py-statement-opens-block-p)
	    (setq found t)
	  (goto-char restart))))
    (unless found
      (goto-char start)
      (py-goto-initial-line)
      (while (not (or found (bobp)))
	(setq found (and
		     (re-search-backward ":[ \t]*\\($\\|[#\\]\\)" nil 'move)
		     (or (py-goto-initial-line) t) ; always true -- side effect
		     (py-statement-opens-block-p)))))
    (setq colon-indent (current-indentation)
	  found (and found (zerop (py-next-statement 1)))
	  new-value (- (current-indentation) colon-indent))
    (goto-char start)
    (if (not found)
	(error "Sorry, couldn't guess a value for py-indent-offset")
      (funcall (if global 'kill-local-variable 'make-local-variable)
	       'py-indent-offset)
      (setq py-indent-offset new-value)
      (or noninteractive
	  (message "%s value of py-indent-offset set to %d"
		   (if global "Global" "Local")
		   py-indent-offset)))
    ))

(defun py-comment-indent-function ()
  "Python version of `comment-indent-function'."
  ;; This is required when filladapt is turned off.  Without it, when
  ;; filladapt is not used, comments which start in column zero
  ;; cascade one character to the right
  (save-excursion
    (beginning-of-line)
    (let ((eol (py-point 'eol)))
      (and comment-start-skip
	   (re-search-forward comment-start-skip eol t)
	   (setq eol (match-beginning 0)))
      (goto-char eol)
      (skip-chars-backward " \t")
      (max comment-column (+ (current-column) (if (bolp) 0 1)))
      )))

(defun py-narrow-to-defun (&optional class)
  "Make text outside current defun invisible.
The defun visible is the one that contains point or follows point.
Optional CLASS is passed directly to `py-beginning-of-def-or-class'."
  (interactive "P")
  (save-excursion
    (widen)
    (py-end-of-def-or-class class)
    (let ((end (point)))
      (py-beginning-of-def-or-class class)
      (narrow-to-region (point) end))))


(defun py-shift-region (start end count)
  "Indent lines from START to END by COUNT spaces."
  (save-excursion
    (goto-char end)
    (beginning-of-line)
    (setq end (point))
    (goto-char start)
    (beginning-of-line)
    (setq start (point))
    (indent-rigidly start end count)))

(defun py-shift-region-left (start end &optional count)
  "Shift region of Python code to the left.
The lines from the line containing the start of the current region up
to (but not including) the line containing the end of the region are
shifted to the left, by `py-indent-offset' columns.

If a prefix argument is given, the region is instead shifted by that
many columns.  With no active region, dedent only the current line.
You cannot dedent the region if any line is already at column zero."
  (interactive
   (let ((p (point))
	 (m (mark))
	 (arg current-prefix-arg))
     (if m
	 (list (min p m) (max p m) arg)
       (list p (save-excursion (forward-line 1) (point)) arg))))
  ;; if any line is at column zero, don't shift the region
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (back-to-indentation)
      (if (and (zerop (current-column))
	       (not (looking-at "\\s *$")))
	  (error "Region is at left edge"))
      (forward-line 1)))
  (py-shift-region start end (- (prefix-numeric-value
				 (or count py-indent-offset))))
  (py-keep-region-active))

(defun py-shift-region-right (start end &optional count)
  "Shift region of Python code to the right.
The lines from the line containing the start of the current region up
to (but not including) the line containing the end of the region are
shifted to the right, by `py-indent-offset' columns.

If a prefix argument is given, the region is instead shifted by that
many columns.  With no active region, indent only the current line."
  (interactive
   (let ((p (point))
	 (m (mark))
	 (arg current-prefix-arg))
     (if m
	 (list (min p m) (max p m) arg)
       (list p (save-excursion (forward-line 1) (point)) arg))))
  (py-shift-region start end (prefix-numeric-value
			      (or count py-indent-offset)))
  (py-keep-region-active))

(defun py-indent-region (start end &optional indent-offset)
  "Reindent a region of Python code.

The lines from the line containing the start of the current region up
to (but not including) the line containing the end of the region are
reindented.  If the first line of the region has a non-whitespace
character in the first column, the first line is left alone and the
rest of the region is reindented with respect to it.  Else the entire
region is reindented with respect to the (closest code or indenting
comment) statement immediately preceding the region.

This is useful when code blocks are moved or yanked, when enclosing
control structures are introduced or removed, or to reformat code
using a new value for the indentation offset.

If a numeric prefix argument is given, it will be used as the value of
the indentation offset.  Else the value of `py-indent-offset' will be
used.

Warning: The region must be consistently indented before this function
is called!  This function does not compute proper indentation from
scratch (that's impossible in Python), it merely adjusts the existing
indentation to be correct in context.

Warning: This function really has no idea what to do with
non-indenting comment lines, and shifts them as if they were indenting
comment lines.  Fixing this appears to require telepathy.

Special cases: whitespace is deleted from blank lines; continuation
lines are shifted by the same amount their initial line was shifted,
in order to preserve their relative indentation with respect to their
initial line; and comment lines beginning in column 1 are ignored."
  (interactive "*r\nP")			; region; raw prefix arg
  (save-excursion
    (goto-char end)   (beginning-of-line) (setq end (point-marker))
    (goto-char start) (beginning-of-line)
    (let ((py-indent-offset (prefix-numeric-value
			     (or indent-offset py-indent-offset)))
	  (indents '(-1))		; stack of active indent levels
	  (target-column 0)		; column to which to indent
	  (base-shifted-by 0)		; amount last base line was shifted
	  (indent-base (if (looking-at "[ \t\n]")
			   (py-compute-indentation t)
			 0))
	  ci)
      (while (< (point) end)
	(setq ci (current-indentation))
	;; figure out appropriate target column
	(cond
	 ((or (eq (following-char) ?#)	; comment in column 1
	      (looking-at "[ \t]*$"))	; entirely blank
	  (setq target-column 0))
	 ((py-continuation-line-p)	; shift relative to base line
	  (setq target-column (+ ci base-shifted-by)))
	 (t				; new base line
	  (if (> ci (car indents))	; going deeper; push it
	      (setq indents (cons ci indents))
	    ;; else we should have seen this indent before
	    (setq indents (memq ci indents)) ; pop deeper indents
	    (if (null indents)
		(error "Bad indentation in region, at line %d"
		       (save-restriction
			 (widen)
			 (1+ (count-lines 1 (point)))))))
	  (setq target-column (+ indent-base
				 (* py-indent-offset
				    (- (length indents) 2))))
	  (setq base-shifted-by (- target-column ci))))
	;; shift as needed
	(if (/= ci target-column)
	    (progn
	      (delete-horizontal-space)
	      (indent-to target-column)))
	(forward-line 1))))
  (set-marker end nil))

(defun py-comment-region (beg end &optional arg)
  "Like `comment-region' but uses double hash (`#') comment starter."
  (interactive "r\nP")
  (let ((comment-start py-block-comment-prefix))
    (comment-region beg end arg)))


;; Functions for moving point
(defun py-previous-statement (count)
  "Go to the start of the COUNTth preceding Python statement.
By default, goes to the previous statement.  If there is no such
statement, goes to the first statement.  Return count of statements
left to move.  `Statements' do not include blank, comment, or
continuation lines."
  (interactive "p")			; numeric prefix arg
  (if (< count 0) (py-next-statement (- count))
    (py-goto-initial-line)
    (let (start)
      (while (and
	      (setq start (point))	; always true -- side effect
	      (> count 0)
	      (zerop (forward-line -1))
	      (py-goto-statement-at-or-above))
	(setq count (1- count)))
      (if (> count 0) (goto-char start)))
    count))

(defun py-next-statement (count)
  "Go to the start of next Python statement.
If the statement at point is the i'th Python statement, goes to the
start of statement i+COUNT.  If there is no such statement, goes to the
last statement.  Returns count of statements left to move.  `Statements'
do not include blank, comment, or continuation lines."
  (interactive "p")			; numeric prefix arg
  (if (< count 0) (py-previous-statement (- count))
    (beginning-of-line)
    (let (start)
      (while (and
	      (setq start (point))	; always true -- side effect
	      (> count 0)
	      (py-goto-statement-below))
	(setq count (1- count)))
      (if (> count 0) (goto-char start)))
    count))

(defun py-goto-block-up (&optional nomark)
  "Move up to start of current block.
Go to the statement that starts the smallest enclosing block; roughly
speaking, this will be the closest preceding statement that ends with a
colon and is indented less than the statement you started on.  If
successful, also sets the mark to the starting point.

`\\[py-mark-block]' can be used afterward to mark the whole code
block, if desired.

If called from a program, the mark will not be set if optional argument
NOMARK is not nil."
  (interactive)
  (let ((start (point))
	(found nil)
	initial-indent)
    (py-goto-initial-line)
    ;; if on blank or non-indenting comment line, use the preceding stmt
    (if (looking-at "[ \t]*\\($\\|#[^ \t\n]\\)")
	(progn
	  (py-goto-statement-at-or-above)
	  (setq found (py-statement-opens-block-p))))
    ;; search back for colon line indented less
    (setq initial-indent (current-indentation))
    (if (zerop initial-indent)
	;; force fast exit
	(goto-char (point-min)))
    (while (not (or found (bobp)))
      (setq found
	    (and
	     (re-search-backward ":[ \t]*\\($\\|[#\\]\\)" nil 'move)
	     (or (py-goto-initial-line) t) ; always true -- side effect
	     (< (current-indentation) initial-indent)
	     (py-statement-opens-block-p))))
    (if found
	(progn
	  (or nomark (push-mark start))
	  (back-to-indentation))
      (goto-char start)
      (error "Enclosing block not found"))))

(defun py-beginning-of-def-or-class (&optional class count)
  "Move point to start of `def' or `class'.

Searches back for the closest preceding `def'.  If you supply a prefix
arg, looks for a `class' instead.  The docs below assume the `def'
case; just substitute `class' for `def' for the other case.
Programmatically, if CLASS is `either', then moves to either `class'
or `def'.

When second optional argument is given programmatically, move to the
COUNTth start of `def'.

If point is in a `def' statement already, and after the `d', simply
moves point to the start of the statement.

Otherwise (i.e. when point is not in a `def' statement, or at or
before the `d' of a `def' statement), searches for the closest
preceding `def' statement, and leaves point at its start.  If no such
statement can be found, leaves point at the start of the buffer.

Returns t iff a `def' statement is found by these rules.

Note that doing this command repeatedly will take you closer to the
start of the buffer each time.

To mark the current `def', see `\\[py-mark-def-or-class]'."
  (interactive "P")			; raw prefix arg
  (setq count (or count 1))
  (let ((at-or-before-p (<= (current-column) (current-indentation)))
	(start-of-line (goto-char (py-point 'bol)))
	(start-of-stmt (goto-char (py-point 'bos)))
	(start-re (cond ((eq class 'either) "^[ \t]*\\(class\\|def\\)\\>")
			(class "^[ \t]*class\\>")
			(t "^[ \t]*def\\>")))
	)
    ;; searching backward
    (if (and (< 0 count)
	     (or (/= start-of-stmt start-of-line)
		 (not at-or-before-p)))
	(end-of-line))
    ;; search forward
    (if (and (> 0 count)
	     (zerop (current-column))
	     (looking-at start-re))
	(end-of-line))
    (if (re-search-backward start-re nil 'move count)
	(goto-char (match-beginning 0)))))

;; Backwards compatibility
(defalias 'beginning-of-python-def-or-class 'py-beginning-of-def-or-class)

(defun py-end-of-def-or-class (&optional class count)
  "Move point beyond end of `def' or `class' body.

By default, looks for an appropriate `def'.  If you supply a prefix
arg, looks for a `class' instead.  The docs below assume the `def'
case; just substitute `class' for `def' for the other case.
Programmatically, if CLASS is `either', then moves to either `class'
or `def'.

When second optional argument is given programmatically, move to the
COUNTth end of `def'.

If point is in a `def' statement already, this is the `def' we use.

Else, if the `def' found by `\\[py-beginning-of-def-or-class]'
contains the statement you started on, that's the `def' we use.

Otherwise, we search forward for the closest following `def', and use that.

If a `def' can be found by these rules, point is moved to the start of
the line immediately following the `def' block, and the position of the
start of the `def' is returned.

Else point is moved to the end of the buffer, and nil is returned.

Note that doing this command repeatedly will take you closer to the
end of the buffer each time.

To mark the current `def', see `\\[py-mark-def-or-class]'."
  (interactive "P")			; raw prefix arg
  (if (and count (/= count 1))
      (py-beginning-of-def-or-class (- 1 count)))
  (let ((start (progn (py-goto-initial-line) (point)))
	(which (cond ((eq class 'either) "\\(class\\|def\\)")
		     (class "class")
		     (t "def")))
	(state 'not-found))
    ;; move point to start of appropriate def/class
    (if (looking-at (concat "[ \t]*" which "\\>")) ; already on one
	(setq state 'at-beginning)
      ;; else see if py-beginning-of-def-or-class hits container
      (if (and (py-beginning-of-def-or-class class)
	       (progn (py-goto-beyond-block)
		      (> (point) start)))
	  (setq state 'at-end)
	;; else search forward
	(goto-char start)
	(if (re-search-forward (concat "^[ \t]*" which "\\>") nil 'move)
	    (progn (setq state 'at-beginning)
		   (beginning-of-line)))))
    (cond
     ((eq state 'at-beginning) (py-goto-beyond-block) t)
     ((eq state 'at-end) t)
     ((eq state 'not-found) nil)
     (t (error "Internal error in `py-end-of-def-or-class'")))))

;; Backwards compabitility
(defalias 'end-of-python-def-or-class 'py-end-of-def-or-class)


;; Functions for marking regions
(defun py-mark-block (&optional extend just-move)
  "Mark following block of lines.  With prefix arg, mark structure.
Easier to use than explain.  It sets the region to an `interesting'
block of succeeding lines.  If point is on a blank line, it goes down to
the next non-blank line.  That will be the start of the region.  The end
of the region depends on the kind of line at the start:

 - If a comment, the region will include all succeeding comment lines up
   to (but not including) the next non-comment line (if any).

 - Else if a prefix arg is given, and the line begins one of these
   structures:

     if elif else try except finally for while def class

   the region will be set to the body of the structure, including
   following blocks that `belong' to it, but excluding trailing blank
   and comment lines.  E.g., if on a `try' statement, the `try' block
   and all (if any) of the following `except' and `finally' blocks
   that belong to the `try' structure will be in the region.  Ditto
   for if/elif/else, for/else and while/else structures, and (a bit
   degenerate, since they're always one-block structures) def and
   class blocks.

 - Else if no prefix argument is given, and the line begins a Python
   block (see list above), and the block is not a `one-liner' (i.e.,
   the statement ends with a colon, not with code), the region will
   include all succeeding lines up to (but not including) the next
   code statement (if any) that's indented no more than the starting
   line, except that trailing blank and comment lines are excluded.
   E.g., if the starting line begins a multi-statement `def'
   structure, the region will be set to the full function definition,
   but without any trailing `noise' lines.

 - Else the region will include all succeeding lines up to (but not
   including) the next blank line, or code or indenting-comment line
   indented strictly less than the starting line.  Trailing indenting
   comment lines are included in this case, but not trailing blank
   lines.

A msg identifying the location of the mark is displayed in the echo
area; or do `\\[exchange-point-and-mark]' to flip down to the end.

If called from a program, optional argument EXTEND plays the role of
the prefix arg, and if optional argument JUST-MOVE is not nil, just
moves to the end of the block (& does not set mark or display a msg)."
  (interactive "P")			; raw prefix arg
  (py-goto-initial-line)
  ;; skip over blank lines
  (while (and
	  (looking-at "[ \t]*$")	; while blank line
	  (not (eobp)))			; & somewhere to go
    (forward-line 1))
  (if (eobp)
      (error "Hit end of buffer without finding a non-blank stmt"))
  (let ((initial-pos (point))
	(initial-indent (current-indentation))
	last-pos			; position of last stmt in region
	(followers
	 '((if elif else) (elif elif else) (else)
	   (try except finally) (except except) (finally)
	   (for else) (while else)
	   (def) (class) ) )
	first-symbol next-symbol)

    (cond
     ;; if comment line, suck up the following comment lines
     ((looking-at "[ \t]*#")
      (re-search-forward "^[ \t]*[^ \t#]" nil 'move) ; look for non-comment
      (re-search-backward "^[ \t]*#")	; and back to last comment in block
      (setq last-pos (point)))

     ;; else if line is a block line and EXTEND given, suck up
     ;; the whole structure
     ((and extend
	   (setq first-symbol (py-suck-up-first-keyword) )
	   (assq first-symbol followers))
      (while (and
	      (or (py-goto-beyond-block) t) ; side effect
	      (forward-line -1)		; side effect
	      (setq last-pos (point))	; side effect
	      (py-goto-statement-below)
	      (= (current-indentation) initial-indent)
	      (setq next-symbol (py-suck-up-first-keyword))
	      (memq next-symbol (cdr (assq first-symbol followers))))
	(setq first-symbol next-symbol)))

     ;; else if line *opens* a block, search for next stmt indented <=
     ((py-statement-opens-block-p)
      (while (and
	      (setq last-pos (point))	; always true -- side effect
	      (py-goto-statement-below)
	      (> (current-indentation) initial-indent))
	nil))

     ;; else plain code line; stop at next blank line, or stmt or
     ;; indenting comment line indented <
     (t
      (while (and
	      (setq last-pos (point))	; always true -- side effect
	      (or (py-goto-beyond-final-line) t)
	      (not (looking-at "[ \t]*$")) ; stop at blank line
	      (or
	       (>= (current-indentation) initial-indent)
	       (looking-at "[ \t]*#[^ \t\n]"))) ; ignore non-indenting #
	nil)))

    ;; skip to end of last stmt
    (goto-char last-pos)
    (py-goto-beyond-final-line)

    ;; set mark & display
    (if just-move
	()				; just return
      (push-mark (point) 'no-msg)
      (forward-line -1)
      (message "Mark set after: %s" (py-suck-up-leading-text))
      (goto-char initial-pos))))

(defun py-mark-def-or-class (&optional class)
  "Set region to body of def (or class, with prefix arg) enclosing point.
Pushes the current mark, then point, on the mark ring (all language
modes do this, but although it's handy it's never documented ...).

In most Emacs language modes, this function bears at least a
hallucinogenic resemblance to `\\[py-end-of-def-or-class]' and
`\\[py-beginning-of-def-or-class]'.

And in earlier versions of Python mode, all 3 were tightly connected.
Turned out that was more confusing than useful: the `goto start' and
`goto end' commands are usually used to search through a file, and
people expect them to act a lot like `search backward' and `search
forward' string-search commands.  But because Python `def' and `class'
can nest to arbitrary levels, finding the smallest def containing
point cannot be done via a simple backward search: the def containing
point may not be the closest preceding def, or even the closest
preceding def that's indented less.  The fancy algorithm required is
appropriate for the usual uses of this `mark' command, but not for the
`goto' variations.

So the def marked by this command may not be the one either of the
`goto' commands find: If point is on a blank or non-indenting comment
line, moves back to start of the closest preceding code statement or
indenting comment line.  If this is a `def' statement, that's the def
we use.  Else searches for the smallest enclosing `def' block and uses
that.  Else signals an error.

When an enclosing def is found: The mark is left immediately beyond
the last line of the def block.  Point is left at the start of the
def, except that: if the def is preceded by a number of comment lines
followed by (at most) one optional blank line, point is left at the
start of the comments; else if the def is preceded by a blank line,
point is left at its start.

The intent is to mark the containing def/class and its associated
documentation, to make moving and duplicating functions and classes
pleasant."
  (interactive "P")			; raw prefix arg
  (let ((start (point))
	(which (cond ((eq class 'either) "\\(class\\|def\\)")
		     (class "class")
		     (t "def"))))
    (push-mark start)
    (if (not (py-go-up-tree-to-keyword which))
	(progn (goto-char start)
	       (error "Enclosing %s not found"
		      (if (eq class 'either)
			  "def or class"
			which)))
      ;; else enclosing def/class found
      (setq start (point))
      (py-goto-beyond-block)
      (push-mark (point))
      (goto-char start)
      (if (zerop (forward-line -1))	; if there is a preceding line
	  (progn
	    (if (looking-at "[ \t]*$")	; it's blank
		(setq start (point))	; so reset start point
	      (goto-char start))	; else try again
	    (if (zerop (forward-line -1))
		(if (looking-at "[ \t]*#") ; a comment
		    ;; look back for non-comment line
		    ;; tricky: note that the regexp matches a blank
		    ;; line, cuz \n is in the 2nd character class
		    (and
		     (re-search-backward "^[ \t]*[^ \t#]" nil 'move)
		     (forward-line 1))
		  ;; no comment, so go back
		  (goto-char start)))))))
  (exchange-point-and-mark)
  (py-keep-region-active))

;; ripped from cc-mode
(defun py-forward-into-nomenclature (&optional arg)
  "Move forward to end of a nomenclature section or word.
With \\[universal-argument] (programmatically, optional argument ARG), 
do it that many times.

A `nomenclature' is a fancy way of saying AWordWithMixedCaseNotUnderscores."
  (interactive "p")
  (let ((case-fold-search nil))
    (if (> arg 0)
	(re-search-forward
	 "\\(\\W\\|[_]\\)*\\([A-Z]*[a-z0-9]*\\)"
	 (point-max) t arg)
      (while (and (< arg 0)
		  (re-search-backward
		   "\\(\\W\\|[a-z0-9]\\)[A-Z]+\\|\\(\\W\\|[_]\\)\\w+"
		   (point-min) 0))
	(forward-char 1)
	(setq arg (1+ arg)))))
  (py-keep-region-active))

(defun py-backward-into-nomenclature (&optional arg)
  "Move backward to beginning of a nomenclature section or word.
With optional ARG, move that many times.  If ARG is negative, move
forward.

A `nomenclature' is a fancy way of saying AWordWithMixedCaseNotUnderscores."
  (interactive "p")
  (py-forward-into-nomenclature (- arg))
  (py-keep-region-active))



;; Documentation functions

;; dump the long form of the mode blurb; does the usual doc escapes,
;; plus lines of the form ^[vc]:name$ to suck variable & command docs
;; out of the right places, along with the keys they're on & current
;; values
(defun py-dump-help-string (str)
  (with-output-to-temp-buffer "*Help*"
    (let ((locals (buffer-local-variables))
	  funckind funcname func funcdoc
	  (start 0) mstart end
	  keys )
      (while (string-match "^%\\([vc]\\):\\(.+\\)\n" str start)
	(setq mstart (match-beginning 0)  end (match-end 0)
	      funckind (substring str (match-beginning 1) (match-end 1))
	      funcname (substring str (match-beginning 2) (match-end 2))
	      func (intern funcname))
	(princ (substitute-command-keys (substring str start mstart)))
	(cond
	 ((equal funckind "c")		; command
	  (setq funcdoc (documentation func)
		keys (concat
		      "Key(s): "
		      (mapconcat 'key-description
				 (where-is-internal func py-mode-map)
				 ", "))))
	 ((equal funckind "v")		; variable
	  (setq funcdoc (documentation-property func 'variable-documentation)
		keys (if (assq func locals)
			 (concat
			  "Local/Global values: "
			  (prin1-to-string (symbol-value func))
			  " / "
			  (prin1-to-string (default-value func)))
		       (concat
			"Value: "
			(prin1-to-string (symbol-value func))))))
	 (t				; unexpected
	  (error "Error in py-dump-help-string, tag `%s'" funckind)))
	(princ (format "\n-> %s:\t%s\t%s\n\n"
		       (if (equal funckind "c") "Command" "Variable")
		       funcname keys))
	(princ funcdoc)
	(terpri)
	(setq start end))
      (princ (substitute-command-keys (substring str start))))
    (print-help-return-message)))

(defun py-describe-mode ()
  "Dump long form of Python-mode docs."
  (interactive)
  (py-dump-help-string "Major mode for editing Python files.
Knows about Python indentation, tokens, comments and continuation lines.
Paragraphs are separated by blank lines only.

Major sections below begin with the string `@'; specific function and
variable docs begin with `->'.

@EXECUTING PYTHON CODE

\\[py-execute-import-or-reload]\timports or reloads the file in the Python interpreter
\\[py-execute-buffer]\tsends the entire buffer to the Python interpreter
\\[py-execute-region]\tsends the current region
\\[py-execute-def-or-class]\tsends the current function or class definition
\\[py-execute-string]\tsends an arbitrary string
\\[py-shell]\tstarts a Python interpreter window; this will be used by
\tsubsequent Python execution commands
%c:py-execute-import-or-reload
%c:py-execute-buffer
%c:py-execute-region
%c:py-execute-def-or-class
%c:py-execute-string
%c:py-shell

@VARIABLES

py-indent-offset\tindentation increment
py-block-comment-prefix\tcomment string used by comment-region

py-python-command\tshell command to invoke Python interpreter
py-temp-directory\tdirectory used for temp files (if needed)

py-beep-if-tab-change\tring the bell if tab-width is changed
%v:py-indent-offset
%v:py-block-comment-prefix
%v:py-python-command
%v:py-temp-directory
%v:py-beep-if-tab-change

@KINDS OF LINES

Each physical line in the file is either a `continuation line' (the
preceding line ends with a backslash that's not part of a comment, or
the paren/bracket/brace nesting level at the start of the line is
non-zero, or both) or an `initial line' (everything else).

An initial line is in turn a `blank line' (contains nothing except
possibly blanks or tabs), a `comment line' (leftmost non-blank
character is `#'), or a `code line' (everything else).

Comment Lines

Although all comment lines are treated alike by Python, Python mode
recognizes two kinds that act differently with respect to indentation.

An `indenting comment line' is a comment line with a blank, tab or
nothing after the initial `#'.  The indentation commands (see below)
treat these exactly as if they were code lines: a line following an
indenting comment line will be indented like the comment line.  All
other comment lines (those with a non-whitespace character immediately
following the initial `#') are `non-indenting comment lines', and
their indentation is ignored by the indentation commands.

Indenting comment lines are by far the usual case, and should be used
whenever possible.  Non-indenting comment lines are useful in cases
like these:

\ta = b   # a very wordy single-line comment that ends up being
\t        #... continued onto another line

\tif a == b:
##\t\tprint 'panic!' # old code we've `commented out'
\t\treturn a

Since the `#...' and `##' comment lines have a non-whitespace
character following the initial `#', Python mode ignores them when
computing the proper indentation for the next line.

Continuation Lines and Statements

The Python-mode commands generally work on statements instead of on
individual lines, where a `statement' is a comment or blank line, or a
code line and all of its following continuation lines (if any)
considered as a single logical unit.  The commands in this mode
generally (when it makes sense) automatically move to the start of the
statement containing point, even if point happens to be in the middle
of some continuation line.


@INDENTATION

Primarily for entering new code:
\t\\[indent-for-tab-command]\t indent line appropriately
\t\\[py-newline-and-indent]\t insert newline, then indent
\t\\[py-electric-backspace]\t reduce indentation, or delete single character

Primarily for reindenting existing code:
\t\\[py-guess-indent-offset]\t guess py-indent-offset from file content; change locally
\t\\[universal-argument] \\[py-guess-indent-offset]\t ditto, but change globally

\t\\[py-indent-region]\t reindent region to match its context
\t\\[py-shift-region-left]\t shift region left by py-indent-offset
\t\\[py-shift-region-right]\t shift region right by py-indent-offset

Unlike most programming languages, Python uses indentation, and only
indentation, to specify block structure.  Hence the indentation supplied
automatically by Python-mode is just an educated guess:  only you know
the block structure you intend, so only you can supply correct
indentation.

The \\[indent-for-tab-command] and \\[py-newline-and-indent] keys try to suggest plausible indentation, based on
the indentation of preceding statements.  E.g., assuming
py-indent-offset is 4, after you enter
\tif a > 0: \\[py-newline-and-indent]
the cursor will be moved to the position of the `_' (_ is not a
character in the file, it's just used here to indicate the location of
the cursor):
\tif a > 0:
\t    _
If you then enter `c = d' \\[py-newline-and-indent], the cursor will move
to
\tif a > 0:
\t    c = d
\t    _
Python-mode cannot know whether that's what you intended, or whether
\tif a > 0:
\t    c = d
\t_
was your intent.  In general, Python-mode either reproduces the
indentation of the (closest code or indenting-comment) preceding
statement, or adds an extra py-indent-offset blanks if the preceding
statement has `:' as its last significant (non-whitespace and non-
comment) character.  If the suggested indentation is too much, use
\\[py-electric-backspace] to reduce it.

Continuation lines are given extra indentation.  If you don't like the
suggested indentation, change it to something you do like, and Python-
mode will strive to indent later lines of the statement in the same way.

If a line is a continuation line by virtue of being in an unclosed
paren/bracket/brace structure (`list', for short), the suggested
indentation depends on whether the current line contains the first item
in the list.  If it does, it's indented py-indent-offset columns beyond
the indentation of the line containing the open bracket.  If you don't
like that, change it by hand.  The remaining items in the list will mimic
whatever indentation you give to the first item.

If a line is a continuation line because the line preceding it ends with
a backslash, the third and following lines of the statement inherit their
indentation from the line preceding them.  The indentation of the second
line in the statement depends on the form of the first (base) line:  if
the base line is an assignment statement with anything more interesting
than the backslash following the leftmost assigning `=', the second line
is indented two columns beyond that `='.  Else it's indented to two
columns beyond the leftmost solid chunk of non-whitespace characters on
the base line.

Warning:  indent-region should not normally be used!  It calls \\[indent-for-tab-command]
repeatedly, and as explained above, \\[indent-for-tab-command] can't guess the block
structure you intend.
%c:indent-for-tab-command
%c:py-newline-and-indent
%c:py-electric-backspace


The next function may be handy when editing code you didn't write:
%c:py-guess-indent-offset


The remaining `indent' functions apply to a region of Python code.  They
assume the block structure (equals indentation, in Python) of the region
is correct, and alter the indentation in various ways while preserving
the block structure:
%c:py-indent-region
%c:py-shift-region-left
%c:py-shift-region-right

@MARKING & MANIPULATING REGIONS OF CODE

\\[py-mark-block]\t mark block of lines
\\[py-mark-def-or-class]\t mark smallest enclosing def
\\[universal-argument] \\[py-mark-def-or-class]\t mark smallest enclosing class
\\[comment-region]\t comment out region of code
\\[universal-argument] \\[comment-region]\t uncomment region of code
%c:py-mark-block
%c:py-mark-def-or-class
%c:comment-region

@MOVING POINT

\\[py-previous-statement]\t move to statement preceding point
\\[py-next-statement]\t move to statement following point
\\[py-goto-block-up]\t move up to start of current block
\\[py-beginning-of-def-or-class]\t move to start of def
\\[universal-argument] \\[py-beginning-of-def-or-class]\t move to start of class
\\[py-end-of-def-or-class]\t move to end of def
\\[universal-argument] \\[py-end-of-def-or-class]\t move to end of class

The first two move to one statement beyond the statement that contains
point.  A numeric prefix argument tells them to move that many
statements instead.  Blank lines, comment lines, and continuation lines
do not count as `statements' for these commands.  So, e.g., you can go
to the first code statement in a file by entering
\t\\[beginning-of-buffer]\t to move to the top of the file
\t\\[py-next-statement]\t to skip over initial comments and blank lines
Or do `\\[py-previous-statement]' with a huge prefix argument.
%c:py-previous-statement
%c:py-next-statement
%c:py-goto-block-up
%c:py-beginning-of-def-or-class
%c:py-end-of-def-or-class

@LITTLE-KNOWN EMACS COMMANDS PARTICULARLY USEFUL IN PYTHON MODE

`\\[indent-new-comment-line]' is handy for entering a multi-line comment.

`\\[set-selective-display]' with a `small' prefix arg is ideally suited for viewing the
overall class and def structure of a module.

`\\[back-to-indentation]' moves point to a line's first non-blank character.

`\\[indent-relative]' is handy for creating odd indentation.

@OTHER EMACS HINTS

If you don't like the default value of a variable, change its value to
whatever you do like by putting a `setq' line in your .emacs file.
E.g., to set the indentation increment to 4, put this line in your
.emacs:
\t(setq  py-indent-offset  4)
To see the value of a variable, do `\\[describe-variable]' and enter the variable
name at the prompt.

When entering a key sequence like `C-c C-n', it is not necessary to
release the CONTROL key after doing the `C-c' part -- it suffices to
press the CONTROL key, press and release `c' (while still holding down
CONTROL), press and release `n' (while still holding down CONTROL), &
then release CONTROL.

Entering Python mode calls with no arguments the value of the variable
`python-mode-hook', if that value exists and is not nil; for backward
compatibility it also tries `py-mode-hook'; see the `Hooks' section of
the Elisp manual for details.

Obscure:  When python-mode is first loaded, it looks for all bindings
to newline-and-indent in the global keymap, and shadows them with
local bindings to py-newline-and-indent."))


;; Helper functions
(defvar py-parse-state-re
  (concat
   "^[ \t]*\\(if\\|elif\\|else\\|while\\|def\\|class\\)\\>"
   "\\|"
   "^[^ #\t\n]"))

(defun py-parse-state ()
  "Return the parse state at point (see `parse-partial-sexp' docs)."
  (save-excursion
    (let ((here (point))
	  pps done)
      (while (not done)
	;; back up to the first preceding line (if any; else start of
	;; buffer) that begins with a popular Python keyword, or a
	;; non- whitespace and non-comment character.  These are good
	;; places to start parsing to see whether where we started is
	;; at a non-zero nesting level.  It may be slow for people who
	;; write huge code blocks or huge lists ... tough beans.
	(re-search-backward py-parse-state-re nil 'move)
	(beginning-of-line)
	;; In XEmacs, we have a much better way to test for whether
	;; we're in a triple-quoted string or not.  Emacs does not
	;; have this built-in function, which is its loss because
	;; without scanning from the beginning of the buffer, there's
	;; no accurate way to determine this otherwise.
	(if (not (fboundp 'buffer-syntactic-context))
	    ;; Emacs
	    (progn
	      (save-excursion (setq pps (parse-partial-sexp (point) here)))
	      ;; make sure we don't land inside a triple-quoted string
	      (setq done (or (not (nth 3 pps))
			     (bobp)))
	      ;; Just go ahead and short circuit the test back to the
	      ;; beginning of the buffer.  This will be slow, but not
	      ;; nearly as slow as looping through many
	      ;; re-search-backwards.
	      (if (not done)
		  (goto-char (point-min))))
	  ;; XEmacs
	  (setq done (or (not (buffer-syntactic-context))
			 (bobp)))
	  (when done
	    (setq pps (parse-partial-sexp (point) here)))
	  ))
      pps)))

(defun py-nesting-level ()
  "Return the buffer position of the last unclosed enclosing list.
If nesting level is zero, return nil."
  (let ((status (py-parse-state)))
    (if (zerop (car status))
	nil				; not in a nest
      (car (cdr status)))))		; char# of open bracket

(defun py-backslash-continuation-line-p ()
  "Return t iff preceding line ends with backslash that is not in a comment."
  (save-excursion
    (beginning-of-line)
    (and
     ;; use a cheap test first to avoid the regexp if possible
     ;; use 'eq' because char-after may return nil
     (eq (char-after (- (point) 2)) ?\\ )
     ;; make sure; since eq test passed, there is a preceding line
     (forward-line -1)			; always true -- side effect
     (looking-at py-continued-re))))

(defun py-continuation-line-p ()
  "Return t iff current line is a continuation line."
  (save-excursion
    (beginning-of-line)
    (or (py-backslash-continuation-line-p)
	(py-nesting-level))))

(defun py-goto-beginning-of-tqs (delim)
  "Go to the beginning of the triple quoted string we find ourselves in.
DELIM is the TQS string delimiter character we're searching backwards
for."
  (let ((skip (and delim (make-string 1 delim))))
    (when skip
      (save-excursion
	(py-safe (search-backward skip))
	(if (and (eq (char-before) delim)
		 (eq (char-before (1- (point))) delim))
	    (setq skip (make-string 3 delim))))
      ;; we're looking at a triple-quoted string
      (py-safe (search-backward skip)))))

(defun py-goto-initial-line ()
  "Go to the initial line of the current statement.
Usually this is the line we're on, but if we're on the 2nd or
following lines of a continuation block, we need to go up to the first
line of the block."
  ;; Tricky: We want to avoid quadratic-time behavior for long
  ;; continued blocks, whether of the backslash or open-bracket
  ;; varieties, or a mix of the two.  The following manages to do that
  ;; in the usual cases.
  ;;
  ;; Also, if we're sitting inside a triple quoted string, this will
  ;; drop us at the line that begins the string.
  (let (open-bracket-pos)
    (while (py-continuation-line-p)
      (beginning-of-line)
      (if (py-backslash-continuation-line-p)
	  (while (py-backslash-continuation-line-p)
	    (forward-line -1))
	;; else zip out of nested brackets/braces/parens
	(while (setq open-bracket-pos (py-nesting-level))
	  (goto-char open-bracket-pos)))))
  (beginning-of-line))

(defun py-goto-beyond-final-line ()
  "Go to the point just beyond the fine line of the current statement.
Usually this is the start of the next line, but if this is a
multi-line statement we need to skip over the continuation lines."
  ;; Tricky: Again we need to be clever to avoid quadratic time
  ;; behavior.
  ;;
  ;; XXX: Not quite the right solution, but deals with multi-line doc
  ;; strings
  (if (looking-at (concat "[ \t]*\\(" py-stringlit-re "\\)"))
      (goto-char (match-end 0)))
  ;;
  (forward-line 1)
  (let (state)
    (while (and (py-continuation-line-p)
		(not (eobp)))
      ;; skip over the backslash flavor
      (while (and (py-backslash-continuation-line-p)
		  (not (eobp)))
	(forward-line 1))
      ;; if in nest, zip to the end of the nest
      (setq state (py-parse-state))
      (if (and (not (zerop (car state)))
	       (not (eobp)))
	  (progn
	    (parse-partial-sexp (point) (point-max) 0 nil state)
	    (forward-line 1))))))

(defun py-statement-opens-block-p ()
  "Return t iff the current statement opens a block.
I.e., iff it ends with a colon that is not in a comment.  Point should 
be at the start of a statement."
  (save-excursion
    (let ((start (point))
	  (finish (progn (py-goto-beyond-final-line) (1- (point))))
	  (searching t)
	  (answer nil)
	  state)
      (goto-char start)
      (while searching
	;; look for a colon with nothing after it except whitespace, and
	;; maybe a comment
	(if (re-search-forward ":\\([ \t]\\|\\\\\n\\)*\\(#.*\\)?$"
			       finish t)
	    (if (eq (point) finish)	; note: no `else' clause; just
					; keep searching if we're not at
					; the end yet
		;; sure looks like it opens a block -- but it might
		;; be in a comment
		(progn
		  (setq searching nil)	; search is done either way
		  (setq state (parse-partial-sexp start
						  (match-beginning 0)))
		  (setq answer (not (nth 4 state)))))
	  ;; search failed: couldn't find another interesting colon
	  (setq searching nil)))
      answer)))

(defun py-statement-closes-block-p ()
  "Return t iff the current statement closes a block.
I.e., if the line starts with `return', `raise', `break', `continue',
and `pass'.  This doesn't catch embedded statements."
  (let ((here (point)))
    (py-goto-initial-line)
    (back-to-indentation)
    (prog1
	(looking-at (concat py-block-closing-keywords-re "\\>"))
      (goto-char here))))

(defun py-goto-beyond-block ()
  "Go to point just beyond the final line of block begun by the current line.
This is the same as where `py-goto-beyond-final-line' goes unless
we're on colon line, in which case we go to the end of the block.
Assumes point is at the beginning of the line."
  (if (py-statement-opens-block-p)
      (py-mark-block nil 'just-move)
    (py-goto-beyond-final-line)))

(defun py-goto-statement-at-or-above ()
  "Go to the start of the first statement at or preceding point.
Return t if there is such a statement, otherwise nil.  `Statement'
does not include blank lines, comments, or continuation lines."
  (py-goto-initial-line)
  (if (looking-at py-blank-or-comment-re)
      ;; skip back over blank & comment lines
      ;; note:  will skip a blank or comment line that happens to be
      ;; a continuation line too
      (if (re-search-backward "^[ \t]*[^ \t#\n]" nil t)
	  (progn (py-goto-initial-line) t)
	nil)
    t))

(defun py-goto-statement-below ()
  "Go to start of the first statement following the statement containing point.
Return t if there is such a statement, otherwise nil.  `Statement'
does not include blank lines, comments, or continuation lines."
  (beginning-of-line)
  (let ((start (point)))
    (py-goto-beyond-final-line)
    (while (and
	    (looking-at py-blank-or-comment-re)
	    (not (eobp)))
      (forward-line 1))
    (if (eobp)
	(progn (goto-char start) nil)
      t)))

(defun py-go-up-tree-to-keyword (key)
  "Go to begining of statement starting with KEY, at or preceding point.

KEY is a regular expression describing a Python keyword.  Skip blank
lines and non-indenting comments.  If the statement found starts with
KEY, then stop, otherwise go back to first enclosing block starting
with KEY.  If successful, leave point at the start of the KEY line and 
return t.  Otherwise, leav point at an undefined place and return nil."
  ;; skip blanks and non-indenting #
  (py-goto-initial-line)
  (while (and
	  (looking-at "[ \t]*\\($\\|#[^ \t\n]\\)")
	  (zerop (forward-line -1)))	; go back
    nil)
  (py-goto-initial-line)
  (let* ((re (concat "[ \t]*" key "\\b"))
	 (case-fold-search nil)		; let* so looking-at sees this
	 (found (looking-at re))
	 (dead nil))
    (while (not (or found dead))
      (condition-case nil		; in case no enclosing block
	  (py-goto-block-up 'no-mark)
	(error (setq dead t)))
      (or dead (setq found (looking-at re))))
    (beginning-of-line)
    found))

(defun py-suck-up-leading-text ()
  "Return string in buffer from start of indentation to end of line.
Prefix with \"...\" if leading whitespace was skipped."
  (save-excursion
    (back-to-indentation)
    (concat
     (if (bolp) "" "...")
     (buffer-substring (point) (progn (end-of-line) (point))))))

(defun py-suck-up-first-keyword ()
  "Return first keyword on the line as a Lisp symbol.
`Keyword' is defined (essentially) as the regular expression
([a-z]+).  Returns nil if none was found."
  (let ((case-fold-search nil))
    (if (looking-at "[ \t]*\\([a-z]+\\)\\b")
	(intern (buffer-substring (match-beginning 1) (match-end 1)))
      nil)))

(defun py-current-defun ()
  "Python value for `add-log-current-defun-function'.
This tells add-log.el how to find the current function/method/variable."
  (save-excursion
    (if (re-search-backward py-defun-start-re nil t)
	(or (match-string 3)
	    (let ((method (match-string 2)))
	      (if (and (not (zerop (length (match-string 1))))
		       (re-search-backward py-class-start-re nil t))
		  (concat (match-string 1) "." method)
		method)))
      nil)))


(defconst py-help-address "python-mode@python.org"
  "Address accepting submission of bug reports.")

(defun py-version ()
  "Echo the current version of `python-mode' in the minibuffer."
  (interactive)
  (message "Using `python-mode' version %s" py-version)
  (py-keep-region-active))

;; only works under Emacs 19
;(eval-when-compile
;  (require 'reporter))

(defun py-submit-bug-report (enhancement-p)
  "Submit via mail a bug report on `python-mode'.
With \\[universal-argument] (programmatically, argument ENHANCEMENT-P
non-nil) just submit an enhancement request."
  (interactive
   (list (not (y-or-n-p
	       "Is this a bug report (hit `n' to send other comments)? "))))
  (let ((reporter-prompt-for-summary-p (if enhancement-p
					   "(Very) brief summary: "
					 t)))
    (require 'reporter)
    (reporter-submit-bug-report
     py-help-address			;address
     (concat "python-mode " py-version)	;pkgname
     ;; varlist
     (if enhancement-p nil
       '(py-python-command
	 py-indent-offset
	 py-block-comment-prefix
	 py-temp-directory
	 py-beep-if-tab-change))
     nil				;pre-hooks
     nil				;post-hooks
     "Dear Barry,")			;salutation
    (if enhancement-p nil
      (set-mark (point))
      (insert 
"Please replace this text with a sufficiently large code sample\n\
and an exact recipe so that I can reproduce your problem.  Failure\n\
to do so may mean a greater delay in fixing your bug.\n\n")
      (exchange-point-and-mark)
      (py-keep-region-active))))


(defun py-kill-emacs-hook ()
  "Delete files in `py-file-queue'.
These are Python temporary files awaiting execution."
  (mapcar #'(lambda (filename)
	      (py-safe (delete-file filename)))
	  py-file-queue))

;; arrange to kill temp files when Emacs exists
(add-hook 'kill-emacs-hook 'py-kill-emacs-hook)



(provide 'python-mode)
;;; python-mode.el ends here
;;; yads-mode.el --- yads-mode, major mode for editing YADS files.

(defvar yads-font-lock-keywords
  (eval-when-compile
    (list
     ;; Keywords
     (concat "\\<\\\\\\("
			 "class\\|"
			 "page\\|"
			 "struct\\|"
			 "class\\|"
			 "page\\|"
			 "struct\\|"
			 "union\\|"
			 "enum\\|"
			 "fn\\|"
			 "var\\|"
			 "def\\|"
			 "namespace\\|"
			 "p1\\|"
			 "p2\\|"
			 "p3\\|"
			 "p4\\|"
			 "p\\|"
			 "code\\|"
			 "verbatim\\|"
			 "table\\|"
			 "end\\|"
			 "super\\|"
			 "bug\\|"
			 "warning\\|"
			 "seealso\\|"
			 "return\\|"
			 "param\\|"
			 "arg\\|"
			 "li\\|"
			 "author\\|"
			 "anchor\\|"
			 "deprecated\\|"
			 "virtual\\|"
			 "example\\|"
			 "insertfile\\|"
			 "defaultpage\\|"
			 "url\\|"
			 "ref\\|"
			 "rem\\|"
			 "q\\|"
			 "b\\|"
			 "c\\|"
			 "i\\|"
			 "new"
			 "\\)\\>")))
  "Default expressions to highlight in YADS modes.")

(defun yads-mode ()
   "Mode for editing Yadsfile's."
   (interactive)
   (setq fill-column 75)
   (setq indent-tabs-mode nil)
   (setq paragraph-start "^[ 	\\.
]\\|^$\\|^<")
   (setq paragraph-separate paragraph-start)

   (set (make-local-variable 'font-lock-defaults)
		'(yads-font-lock-keywords nil nil ((?_ . "w")
										   (?\\ . "w"))))

   (setq mode-name "yads")
   (setq major-mode 'yads-mode)

   (run-hooks 'yads-mode-hook)
   (font-lock-mode)
   )

(provide 'yads-mode)
