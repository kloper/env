(setq load-path (append 
		 '("~/emacs")
		 load-path))

(setq Info-additional-directory-list 
      '("~/emacs/info"))

(when (eq system-type 'windows-nt)
  (add-to-list 'Info-additional-directory-list 
	'("c:/cygwin/usr/info"
          "c:/cygwin/usr/share/info"
          "d:/emacs-24.1/info")))

;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(package-initialize)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

;;;;;;;;;;;;;;;;;;;;;
;; Terminal
;;;;;;;;;;;;;;;;;;;;;

(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] '(lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] '(lambda ()
                              (interactive)
                              (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t))

;;;;;;;;;;;;;;;;;;;;;
;; color theme
;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'custom-theme-load-path "~/emacs/themes")

(if (not  window-system)
    (load-theme 'kloper-nw t)    
    (load-theme 'kloper t))

;;;;;;;;;;;;;;;;;;;;;
;; general settings
;;;;;;;;;;;;;;;;;;;;;

;; for using my .emacs on sites where there is default.el in system path
(setq inhibit-default-init t)
;; no startup message
(setq inhibit-startup-message t)
;; allow local variables in files
(setq enable-local-eval t)
;; turn all symlinks to real file names
(setq find-file-visit-truename t)

;; show lines and column numbers
(line-number-mode 1)
(column-number-mode 1)
;; who needs a toolbar in emacs?
(tool-bar-mode 0)
;; scrolling
(setq scroll-bar-mode 'right)
(toggle-scroll-bar 1)
(setq scroll-step 1)
(mouse-wheel-mode 1)
;; font lock
(setq font-lock-maximum-decoration t)
(global-font-lock-mode 1)

;; autosave and backup files
(setq auto-save-interval 90)
(setq auto-save-timeout 30)
(setq backup-directory-alist 
      (append '(("." . ".backup"))))
(setq backup-by-copying 1) ;; this one avoids problems with cons hard links
(setq version-control t)
(setq kept-new-versions 5)
(setq kept-old-versions 5)
(setq delete-old-versions t)

;; control buffer menu size
(setq mouse-buffer-menu-maxlen 60)
(setq mouse-buffer-menu-mode-mult 100)

;; insert spaces instead of tabs
(setq indent-tabs-mode nil)

;; dont display continuation lines (with arrow at fringe)
(setq truncate-lines nil)

;; enable narrow-to-region (C-x n n)
(put 'narrow-to-region 'disabled nil)


;; Frame parameters
(setq default-frame-alist
      (append  '((vertical-scroll-bars . t)
		 (font . "-outline-Anonymous Pro-normal-r-normal-normal-15-97-96-96-c-*-iso10646-1")
		 (height . 46))
	       default-frame-alist))
(add-hook 'after-make-frame-hook (function 
				  (lambda(frame) 
				    (frame-update-face-colors frame))))

; file extension -> mode
(setq auto-mode-alist
      (append '(("Makefile"  . makefile-mode)
		("makefile" . makefile-mode)
		("\\.mak$"  . makefile-mode)
		("\\.C$"    . c++-mode)
		("\\.H$"    . c++-mode)
		("\\.sh$"    . sh-mode)
		("\\.java$"  . jde-mode)
		("\\.pl$"    . cperl-mode)
		("\\.pm$"    . cperl-mode)
		("\\.py$"    . python-mode)                
		("[Cc]onstruct$" . cperl-mode)
		("[Cc]onscript$" . cperl-mode)
		("\\.[xX][Mm][lL]$" . nxml-mode)
                ("\\.wrl$" . iv-mode)
		) auto-mode-alist))

(setq interpreter-mode-alist 
      (append '(("perl" . cperl-mode))
	      interpreter-mode-alist))
	      
;; key bindings
(global-set-key [ret] 'newline-and-indent)
(global-set-key [f2] 'compile)
(global-set-key [f3] 'goto-line)
(global-set-key [f4] 'what-line)
(global-set-key [f5] 'font-lock-mode)
(global-set-key [f6] 'kmacro-end-and-call-macro)
(global-set-key [f7] 'toggle-read-only)
(global-set-key [S-next] 'next-error)
(global-set-key [S-prior] 'previous-error)
(global-set-key [C-up] 'scroll-down)
(global-set-key "\e[a" 'scroll-down)
(global-set-key [C-down] 'scroll-up)
(global-set-key "\e[b" 'scroll-up)
(global-set-key [C-left] 'beginning-of-line)
(global-set-key "\e[d" 'beginning-of-line)
(global-set-key [C-right] 'end-of-line)
(global-set-key "\e[c" 'end-of-line)
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)
(global-set-key [S-up] 'previous-line)
(global-set-key [S-down] 'next-line)
(global-set-key [S-right] 'forward-word)
(global-set-key [S-left] 'backward-word)
(global-set-key [S-tab] 'dabbrev-expand)
(global-set-key [backtab] 'dabbrev-expand)
(global-set-key [C-S-down-mouse-1] 'mouse-set-font)
(global-set-key [C-down-mouse-1] 'mouse-buffer-menu)
;; (global-set-key [wheel-up] 'scroll-down)
;; (global-set-key [wheel-down] 'scroll-up)

;;;;;;;;;;;;;;;;;;;;
;; fix shell on windows
;;;;;;;;;;;;;;;;;;;;
(setq w32-get-true-file-attributes nil)

(when (eq system-type 'windows-nt)
  (defun set-shell-cmdproxy()
    (interactive)
    (setq shell-file-name "cmdproxy")
    (setq explicit-shell-file-name "cmdproxy")
    (setenv "SHELL" explicit-shell-file-name)
    (setq w32-quote-process-args t)
                                        ;  (setq shell-command-switch "/c")
    )

  (set-shell-cmdproxy)
)

;;;;;;;;;;;;;;;;;;;;
;;; mail
;;;;;;;;;;;;;;;;;;;;

(setq user-mail-address "Dimitry Kloper <dimka@users.sf.net>")

;;;;;;;;;;;;;;;;;;;;
;; column-marker
;;;;;;;;;;;;;;;;;;;;
(require 'column-marker)     

;;;;;;;;;;;;;;;;;;;;
;;; c-mode & c++-mode
;;;;;;;;;;;;;;;;;;;;

(c-add-style "kloper"
	     '("ellemtel"
	       (c-offsets-alist
	        . ((namespace-open . 0)
		   (namespace-close . 0)
		   (innamespace . 0)
		   (arglist-cont-nonempty c-lineup-argcont 
					  c-lineup-arglist 
					  +)
		   (arglist-close . 0)
		   (statement-cont c-lineup-string-cont
				   c-lineup-cascaded-calls 
				   c-lineup-math)
		   ))))

(add-hook 'c-mode-hook (function (lambda() (c-set-style "kloper") 
				   (setq ps-line-number t)
                                   (column-marker-1 79))))
(add-hook 'c++-mode-hook (function (lambda() (c-set-style "kloper")
				     (setq ps-line-number t) 
                                     (column-marker-1 79))))
(add-hook 'python-mode-hook (function (lambda() (column-marker-1 79))))
(add-hook 'tex-mode-hook (function (lambda() (column-marker-1 79))))

;;;;;;;;;;;;;;;;;;;;
;; python
;;;;;;;;;;;;;;;;;;;;

(require 'python-mode)
(setq py-electric-colon-active t)
(add-hook 'python-mode-hook
          (function (lambda()
                      (local-set-key [tab] 'py-indent-line-outmost)
                      (local-set-key [C-tab] 'py-indent-line)
                      (add-hook 'write-contents-functions
                                (lambda()
                                  (save-excursion
                                    (delete-trailing-whitespace)))))))

;;;;;;;;;;;;;;;;;;;;
;; flyspell
;;;;;;;;;;;;;;;;;;;;

(add-hook 'python-mode-hook
          (function (lambda()
                      (flyspell-prog-mode))))

(add-hook 'c-mode-hook
          (function (lambda()
                      (flyspell-prog-mode))))

(add-hook 'c++-mode-hook
          (function (lambda()
                      (flyspell-prog-mode))))


;;;;;;;;;;;;;;;;;;;;
;; perl
;;;;;;;;;;;;;;;;;;;;

(require 'cperl-mode)
(setq cperl-invalid-face nil)
(setq cperl-break-one-line-blocks-when-indent nil)
(setq cperl-indent-level 4)
(setq cperl-lineup-step 1)
(setq cperl-indent-parens-as-block t)

;;;;;;;;;;;;;;;;;;;;
;; flycheck
;;;;;;;;;;;;;;;;;;;;

(require 'flycheck)
(setq flycheck-checker-error-threshold nil)

(when (eq system-type 'cygwin)
  (flycheck-define-checker python-pylint-custom
    "A Python syntax and style checker using Pylint.
     This syntax checker requires Pylint 1.0 or newer.
     See URL `http://www.pylint.org/'."
    ;; -r n disables the scoring report
    :command ("~/emacs/pylint.py" "-r" "n"
              "--msg-template"
              "{path}:{line}:{column}:{C}:{msg_id}:{msg} ({symbol})"
              (config-file "--rcfile" flycheck-pylintrc)
              ;; Need `source-inplace' for relative imports (e.g. `from .foo
              ;; import bar'), see
              ;; https://github.com/flycheck/flycheck/issues/280
              source-inplace)
    :error-filter
    (lambda (errors)
      (flycheck-sanitize-errors (flycheck-increment-error-columns errors)))
    :error-patterns
    ((error line-start (file-name) ":" line ":" column ":"
            (or "E" "F") ":"
            (id (one-or-more (not (any ":")))) ":"
            (message) line-end)
     (warning line-start (file-name) ":" line ":" column ":"
              (or "W" "R") ":"
              (id (one-or-more (not (any ":")))) ":"
              (message) line-end)
     (info line-start (file-name) ":" line ":" column ":"
           "C:" (id (one-or-more (not (any ":")))) ":"
           (message) line-end))
    :modes python-mode)

  (add-hook 'python-mode-hook
            (function (lambda()
                        (flycheck-select-checker 'python-pylint-custom)
                        (flycheck-mode)))))

(unless (eq system-type 'cygwin)
    (add-hook 'python-mode-hook
            (function (lambda()
                        (flycheck-select-checker 'python-pylint)
                        (flycheck-mode)))))

;;;;;;;;;;;;;;;;;;;;
;; auto insert
;;;;;;;;;;;;;;;;;;;;
(require 'auto-insert-tkld)
(setq auto-insert-full-name "Dimitry Kloper")

(setq auto-insert-path
       '("./emacs-insert"
	 "../emacs-insert"
	 "../../emacs-insert"
	 "../../../emacs-insert"
	 "~/emacs/insert"))

(setq auto-insert-alist (append
			 '(("\\.java$" . "Java")
			   ("\\.tcl$" . "Tcl")
			   ("\\.cpp$" . "C++")
			   ("\\.p[lm]$" . "CPerl")
			   ("\\.py$" . "Python")
			   ("[Cc]onstruct$" . "CPerl")
			   ("[Cc]onscript$" . "CPerl"))
			 auto-insert-alist))

(setq auto-insert-type-alist (append
			      '(("Java" . "java-insert.java")
				("Tcl" . "tcl-insert.tcl")
                                ("Python" . "py-insert.py")
				("CPerl" . "pl-insert.pl"))
			      auto-insert-type-alist))


;;;;;;;;;;;;;;;;;;;;
;; ido/iswitchb ( completioon )
;;;;;;;;;;;;;;;;;;;;

(require 'ido)
(setq ido-show-dot-for-dired t)
(setq ido-auto-merge-work-directories-length -1)
(ido-mode t)

;;;;;;;;;;;;;;;;;;;;;;
;; make better buffer names
;;;;;;;;;;;;;;;;;;;;;;
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

;;;;;;;;;;;;;;;;;;;;
;; VRML mode
;;;;;;;;;;;;;;;;;;;;

(load-library "iv-mode")

;;;;;;;;;;;;;;;;;;;;
;; ibuffer
;;;;;;;;;;;;;;;;;;;;

(setq ibuffer-saved-filters 
      (quote 
       (("gnus" ((or (mode . message-mode) 
                     (mode . mail-mode) 
                     (mode . gnus-group-mode) 
                     (mode . gnus-summary-mode) 
                     (mode . gnus-article-mode)))) 
        ("programming" ((or (mode . emacs-lisp-mode) 
                            (mode . cperl-mode) 
                            (mode . c-mode) 
                            (mode . java-mode) 
                            (mode . idl-mode) 
                            (mode . lisp-mode)))))))

(setq ibuffer-saved-filter-groups 
      (quote 
       (("scooter" 
         ("openvrml" (filename . "/scooter/ext/openvrml/")) 
         ("dgscons" (filename . "/dgscons/")) 
         ("scooter" (filename . "/scooter/"))))))

(setq ibuffer-formats
      '((mark modified read-only " "
              (name 28 28 :left :elide)
              " "
              (size 9 -1 :right)
              " "
              (mode 6 16 :left :elide)
              " "
              filename-and-process)))

(eval-after-load "ibuf-ext"
  '(define-ibuffer-filter filename
       "Toggle current view to buffers with file or directory name matching QUALIFIER."
     (:description "filename"
                   :reader (read-from-minibuffer 
                            "Filter by file/directory name (regexp): "))
     (ibuffer-awhen (or (buffer-local-value 'buffer-file-name buf)
                        (buffer-local-value 'dired-directory buf))
                    (string-match qualifier it))))

;;;;;;;;;;;;;;;;;;;;
;; custom
;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("ac0df460b05de49aafa7a4595999cd926a250fcbc18b19a0d7923fe0b693aacd" "d347797c547ca95a11a2fa34ca1a825b5c4c80bfbb30e9b4fd34977f405fd746" "d24e10524bb50385f7631400950ba488fa45560afcadd21e6e03c2f5d0fad194" "fe6330ecf168de137bb5eddbf9faae1ec123787b5489c14fa5fa627de1d9f82b" "3dd173744ae0990dd72094caef06c0b9176b3d98f0ee5d822d6a7e487c88d548" "246a51f19b632c27d7071877ea99805d4f8131b0ff7acb8a607d4fd1c101e163" "f5e56ac232ff858afb08294fc3a519652ce8a165272e3c65165c42d6fe0262a0" default)))
 '(directory-free-space-program nil)
 '(indent-tabs-mode nil)
 '(py-underscore-word-syntax-p nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
