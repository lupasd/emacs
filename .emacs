; load modes by default

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'icomplete)
;;(icomplete-mode 1)

(load "~/.emacs.d/ctags_p4.el")
(load "~/.emacs.d/yads-mode.el")
; (load "~/.emacs.d/python-mode.el")
; (load "carbon-font.el")
(load "~/.emacs.d/ibs.el")
(load "~/.emacs.d/bm-1.37.el")
(load "~/.emacs.d/gud.el")
(load "~/.emacs.d/gud-lldb.el")


(require 'cc-mode)
(require 'semantic)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)

(semantic-mode 1)

;; (defun my-cedet-hook ()
;;   (local-set-key [(control return)] 'semantic-ia-complete-symbol)
;;   (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
;;   (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
;;   (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle))
;; (add-hook 'c-mode-common-hook 'my-cedet-hook)

;;(when
;;    (load
;;     (expand-file-name "~/.emacs.d/package.el")))

(require 'package)
(add-to-list 'package-archives
'("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
'("melpa" . "http://melpa.org/packages/") t)

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(global-flycheck-mode)

(package-install 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; Use compilation database first, clang_complete as fallback.
(setq-default irony-cdb-compilation-databases '(irony-cdb-libclang
                                                irony-cdb-clang-complete))

(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(eval-after-load 'company
  '(add-to-list 'company-backends '(company-irony-c-headers company-irony)))

(add-hook 'after-init-hook 'global-company-mode)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-tooltip ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
 '(company-tooltip-selection ((t (:background "steelblue" :foreground "white"))))
 '(magit-blame-heading ((t (:background "gray75" :foreground "black")))))

 ; global settings
(setq-default backup-inhibited 'T)
(setq-default auto-fill-hook 'do-autofill)
(setq-default compile-command "build mainline -d -d" )
(setq-default compilation-read-command 'nil)
(setq-default tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
(setq-default case-fold-search 't)
(setq-default isearch-case-fold-search 't)
(define-key lisp-mode-map [return] 'newline-and-indent)
(define-key emacs-lisp-mode-map [return] 'newline-and-indent)
(setq-default devenvcmd "c:\\bin\\devenvcmd.exe" )

(defun my-newline ()
 (interactive)
  (newline)
  (indent-relative-maybe))
(setq auto-mode-alist
      (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.mm$" . objc-mode) auto-mode-alist))
(setq interpreter-mode-alist
      (cons '("python" . python-mode)
            interpreter-mode-alist))

; tabs.  Goddammit.
(setq-default indent-tabs-mode 'nil)
(setq-default default-tab-width 4)
(setq tab-stop-list
      '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 
	  88 92 96 100 104 108 112 116 120))
; Python needs real 8-space tabs
(add-hook 'python-mode-hook '(lambda ()
			       (setq tab-width 8)
			       (setq py-indent-offset 4)))
; ^x^t toggles the buffer between 4 and 8
;; (global-set-key "\C-c\C-t" '(lambda () (interactive)
;; 				 (if (= tab-width 4)
;; 				     (setq tab-width 8)
;; 				   (setq tab-width 4))
;; 				 (redraw-display)))

(global-set-key (kbd "C-x g") 'magit-status)

(defun tasso-make-frame-command ()
  (interactive)
  (let (('frame (make-frame)))
    (select-frame frame)
    (set-background-color "black")
    (set-foreground-color "white")
    (set-cursor-color "yellow")))

(defun indent-buffer ()
  (interactive)
  (indent-region 1 (point-max) 'nil))
(defun untabify-buffer ()
  (interactive)
  (untabify 1 (point-max)))

(add-hook 'c-mode-common-hook
  (lambda() 
    (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

(defun open-file-in-visual-studio ()
  "Opens the file visited by the buffer in Visual Studio, and go to the current line."
  (interactive)
  (if buffer-file-name
      (progn
        (call-process
         devenvcmd
         nil 0 nil
         "File.OpenFile"
         (replace-regexp-in-string "/" "\\" buffer-file-name t t))
        (call-process
         devenvcmd
         nil 0 nil
         "Edit.GoTo"
         (number-to-string (line-number-at-pos (point))))
        )
    (message "Buffer does not correspond to a file."))
)

(defun breakpoint-in-visual-studio ()
  "Opens the file visited by the buffer in Visual Studio and set the breakpoint at curent line."
  (interactive)
  (if buffer-file-name
      (progn
        (open-file-in-visual-studio)
        (call-process
         devenvcmd
         nil 0 nil
         "Debug.ToggleBreakpoint"
         "")
        )
    (message "Buffer does not correspond to a file."))
)

(defun start-debug-in-visual-studio ()
  "Launches the debugger in Visual Studio."
  (interactive)
  (call-process
   devenvcmd
   nil 0 nil
   "Debug.Start"
   "")
)

(defun stop-debug-in-visual-studio ()
  "Stops the debugger in Visual Studio."
  (interactive)
  (call-process
   devenvcmd
   nil 0 nil
   "Debug.StopDebugging"
   "")
)

(ido-mode t)
;;(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
(setq ido-max-prospects 0)

(global-set-key '[f12] 'open-file-in-visual-studio)
(global-set-key '[f9] 'breakpoint-in-visual-studio)
(global-set-key '[f5] 'start-debug-in-visual-studio)
(global-set-key '[S-f5] 'stop-debug-in-visual-studio)

(global-set-key "\C-cc" 'compile)
(global-set-key "\C-cf" 'font-lock-fontify-buffer)
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-cr" 'replace-string)
(global-set-key "\C-cq" 'query-replace)
(global-set-key "\C-ctt" 'ctag-file-add)
(global-set-key "\C-cta" 'ctag-apropos)
(global-set-key "\C-ctv" 'ctag-visit-file)
(global-set-key "\M-." 'ctag)
(global-set-key "\M-," 'ctag-find-next)
(global-set-key "\M-c" 'exit-recursive-edit)
(global-set-key "\M-p" 'cua-scroll-down)
(global-set-key "\M-n" 'cua-scroll-up)
(global-set-key "\C-z" 'undo)
(global-set-key "\C-xx" 'executable-interpret)
(global-set-key "\C-xf" 'other-frame)

;;   M$ Visual Studio key setup.
(global-set-key (kbd "<M-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)


;; Semantic ia
(global-set-key "\C-c;" 'semantic-ia-complete-symbol)
(global-set-key "\C-c'" 'semantic-complete-analyze-inline)

(global-set-key "\C-ci" 'clang-format-region)
(global-set-key "\C-cb" 'clang-format-buffer)

; turn off the stupid toolbar
(tool-bar-mode -1)

; set the frame colours
(setq default-frame-alist '((top . 1)
                            (left . 1)
                            (width . 80)
                            (height . 69)
                            (foreground-color . "gray90")
                            (background-color . "gray10")
                            (cursor-color . "white")))

(setq initial-frame-alist '((top . 1)
                            (left . 1)
                            (width . 80)
                            (height . 69)))

(if (string= window-system "mac")
   (add-to-list 'default-frame-alist '(font . "-*-*-medium-r-normal--10-*-*-*-*-*-fontset-osaka")))

; (if (string= window-system "mac")
;   (add-to-list 'default-frame-alist '(font . "-apple-monaco-medium-r-normal--10*")))

;; 
; set font decoration level
(setq font-lock-maximum-decoration
      '((c-mode . 3) (c++-mode . 3)))

; cc-mode settings
; (define-key c-mode-map [return] 'newline-and-indent)
; (defun my-c-mode-common-hook ()
;  (c-set-style "stroustrup"))
; (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
; (add-hook 'c-mode-common-hook 'turn-on-font-lock)
; (add-hook 'c-mode-common-hook 'font-lock-mode)

; mode hooks
(add-hook 'c-mode-common-hook
	  (function (lambda ()
		      (define-key c-mode-base-map "\C-m" 'newline-and-indent)
		      (c-set-style "stroustrup")
		      (setq-default c-basic-offset 4)
		      (font-lock-mode)
		      (turn-on-font-lock))))

(add-hook 'perl-mode-hook 
	  (function (lambda ()
		      (define-key perl-mode-map [return] 'newline-and-indent))))

(add-hook 'text-mode-hook
	  (function (lambda ()
		      (auto-fill-mode 1 )
		      (define-key text-mode-map (quote [return])
			'my-newline))))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(autoload 'qml-mode "qml-mode" "Editing Qt Declarative." t)
(add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-offsets-alist (quote ((inextern-lang . 0))))
 '(compilation-read-command t t)
 '(cua-mode t nil (cua-base))
 '(package-selected-packages
   (quote
    (company-irony-c-headers company-irony clang-format exec-path-from-shell flycheck-irony flycheck irony company realgud qml-mode magit)))
 '(show-trailing-whitespace nil)
 '(tool-bar-mode nil))


;; load tags for studio
(defun tags-studio ()
  (interactive)
      (progn
        (setenv "TAGSRC" "/Users/dan/Source/studio/mainline/studio")
        (setenv "LPROJECT" "/Users/dan/build/mainline/Debug")
        (ctag-file-add "/Users/dan/build/mainline/Debug/mtags_l")))

;; load tags for studio
(defun tags-master ()
  (interactive)
      (progn
        (setenv "TAGSRC" "/Users/dan/Source/alias/master")
        (setenv "LPROJECT" "/Users/dan/build/master/Debug")
        (ctag-file-add "/Users/dan/build/master/Debug/mtags_l")))

;; load tags for granite
(defun tags-ui ()
  (interactive)
      (progn
        (setenv "TAGSRC" "/Users/dan/Source/alias/studio")
        (setenv "LPROJECT" "/Users/dan/build/ui/Debug")
        (ctag-file-add "/Users/dan/build/ui/Debug/mtags_l")))

;; load tags for minialias
(defun tags-cobra ()
  (interactive)
      (progn
        (setenv "TAGSRC" "/Users/dan/Source/cobra/Alias")
        (setenv "LPROJECT" "/Users/dan/build/cobra/Debug")
        (ctag-file-add "/Users/dan/build/cobra/Debug/mtags_l")))

(defun p4-load ()
  (interactive)
  (progn
    (load "~/.emacs.d/p4.el")
    (p4-set-p4-executable "/usr/local/bin/p4")))
