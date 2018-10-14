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
