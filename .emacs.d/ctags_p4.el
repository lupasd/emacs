;;
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
