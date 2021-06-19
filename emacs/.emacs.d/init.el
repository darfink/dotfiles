; Fix parens highlight overlap visual mode (majs)
; Fix all jumps relative to cursor
; Add avy for swapping words?
; Truncate modeline file paths (modeline-buffer-path-function)
; Use web-mode for '%'?
; Use H/L for 'arg' navigation?
; Eval keybinds (eval-region)
; Organize custom functions
; Config multiple cursors
; Config indentation
; Add narrowing keybinds
; Implement window swap keybind?
; Implement tree view (treemacs, lsp-treemacs, lookup all-the-icons)
; Leader keybinds
; <company> previous wrap around
; <company> fix ctrl-f (company--show-inline-p)
; lsp-ui

;; ----------------------------------------------------------------------------------
;; Package manager
;; ----------------------------------------------------------------------------------

;; Setup package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(package-initialize)

;; Prevent boilerplate
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Setup support for Git repositories
(use-package quelpa
	:init
	(setq quelpa-checkout-melpa-p nil))
(use-package quelpa-use-package
	:after quelpa
	:config
	(quelpa-use-package-activate-advice))

;; ----------------------------------------------------------------------------------
;; Emacs
;; ----------------------------------------------------------------------------------

(use-package emacs
	:ensure nil
	:init
	;; Prefer y/n keypress instead of typing yes/no
	(defalias 'yes-or-no-p 'y-or-n-p)
	(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t))
        backup-directory-alist `(("." . "~/.emacs.d/backups"))
				completion-ignore-case t
				completion-styles '(basic flex)
	      custom-file (concat user-emacs-directory "custom.el")
				custom-safe-themes t
				debug-on-error nil
				delete-old-versions -1
				frame-resize-pixelwise t
        gc-cons-threshold 100000000
        inhibit-startup-screen t
	      read-process-output-max (* 1024 1024)
        select-enable-clipboard nil ; Do not use the system clipboard when yanking by default
        sentence-end-double-space nil
				truncate-partial-width-windows nil
        use-dialog-box nil) ; Disable GUI dialogs
	(setq-default buffer-file-coding-system 'utf-8
	              line-spacing 4
								tab-width 2
								truncate-lines t
								require-final-newline nil
								show-trailing-whitespace t)
	;; Right option as meta modifier on macOS
	(when (eq system-type 'darwin)
		(define-key key-translation-map (kbd "M-1") (kbd "©"))
		(define-key key-translation-map (kbd "M-2") (kbd "@"))
		(define-key key-translation-map (kbd "M-3") (kbd "£"))
		(define-key key-translation-map (kbd "M-4") (kbd "$"))
		(define-key key-translation-map (kbd "M-5") (kbd "∞"))
		(define-key key-translation-map (kbd "M-6") (kbd "§"))
		(define-key key-translation-map (kbd "M-7") (kbd "|"))
		(define-key key-translation-map (kbd "M-8") (kbd "["))
		(define-key key-translation-map (kbd "M-9") (kbd "]"))
		(define-key key-translation-map (kbd "M-0") (kbd "≈"))
		(define-key key-translation-map (kbd "M-(") (kbd "{"))
		(define-key key-translation-map (kbd "M-)") (kbd "}"))
		(define-key key-translation-map (kbd "M-/") (kbd "\\")))
	:config
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-keyboard-coding-system 'utf-8)
	(set-selection-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
	(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (scroll-bar-mode -1) ; Disable native scroll bars
  (blink-cursor-mode 0) ; Disable blinking cursor
  (tool-bar-mode -1) ; Remove the tool bar
	(load custom-file 'noerror))

;; ----------------------------------------------------------------------------------
;; Built-in packages
;; ----------------------------------------------------------------------------------

(use-package display-line-numbers
	:ensure nil
	:init
	(setq display-line-numbers-type 'relative)
	:hook ((prog-mode conf-mode) . display-line-numbers-mode))

(use-package elec-pair
	:ensure nil
	:config
  (electric-pair-mode 1))

(use-package hl-line
	:ensure nil
	:config
	(global-hl-line-mode))

(use-package recentf
	:ensure nil
	:config
	(recentf-mode)
	(setq recentf-max-menu-items 150
	      recentf-max-saved-items 150
	      recentf-exclude '("^/var/folders\\.*"
													"[/\\]\\.emacs.d/elpa"
													"COMMIT_EDITMSG\\'")))

(use-package saveplace
	:ensure nil
	:config
  (save-place-mode 1))

(use-package term
	:ensure nil
	:init
  ;; On windows, use eshell otherwise ansi-term
  (if (eq system-type 'windows-nt)
			(defalias 'os-term 'eshell)
		(defalias 'os-term 'ansi-term))
	;; Don't ask which shell, use the default
	(defadvice ansi-term (before force-bash)
		(interactive (list (getenv "SHELL"))))
	(ad-activate 'ansi-term)
	;; Delete the terminal buffer upon exit
	(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
		(if (memq (process-status proc) '(signal exit))
				(let ((buffer (process-buffer proc)))
					ad-do-it
					(kill-buffer buffer))
			ad-do-it))
	(ad-activate 'term-sentinel)
	:hook
	(term-mode . (lambda() (setq show-trailing-whitespace nil))))

(use-package undo-tree
	:ensure nil
	:init
  ;; Preserve undo history between sessions
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(use-package vc
	:ensure nil
	:init
	;; Follow symlinks without prompt
  (setq vc-follow-symlinks t
        vc-make-backup-files t))

(use-package winner
	:ensure nil
	:config
	(winner-mode 1))

(use-package xref
	:ensure nil
	:config
	;; Follow symlinks without prompt
  (setq xref-prompt-for-identifier nil))

;; ----------------------------------------------------------------------------------
;; Editor theming
;; ----------------------------------------------------------------------------------

;; Highlight delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight numbers
(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package treemacs
	:config
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
	(treemacs-follow-mode -1))
(use-package treemacs-evil :after (treemacs evil))

;; Fancy modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; Sweet themes
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t ; if nil, bold is universally disabled
        doom-themes-enable-italic t ; if nil, italics is universally disabled
				doom-themes-treemacs-theme "doom-colors")
  (load-theme 'doom-solarized-light t)
  (doom-themes-visual-bell-config) ; Enable flashing mode-line on errors
	(doom-themes-treemacs-config) ; Stylize treemacs
  (doom-themes-org-config)) ; Correct org-mode fontification.

;; ----------------------------------------------------------------------------------
;; Text completion
;; ----------------------------------------------------------------------------------

(use-package company
  :init
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.3
        company-selection-wrap-around t
	      company-backends '(company-capf company-files company-dabbrev)
	      company-frontends '(company-box-frontend company-echo-metadata-frontend company-preview-frontend))
  :config
  (define-key company-active-map (kbd "C-w") nil)
  (define-key company-active-map (kbd "C-n") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-u") 'company-previous-page)
  (define-key company-active-map (kbd "C-d") 'company-next-page)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (global-company-mode)
	:hook
	(coompany-mode . (lambda() (setq show-trailing-whitespace nil))))

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode))

;; ----------------------------------------------------------------------------------
;; Modal editing
;; ----------------------------------------------------------------------------------

;; Evil mode
(use-package evil
  :init
	(setq evil-magic nil
	      evil-want-keybinding nil
	      evil-want-minibuffer t
        evil-want-C-u-delete t
        evil-want-C-u-scroll t
        evil-want-Y-yank-to-eol t
        evil-want-change-word-to-end nil)
  ;(setq evil-normal-state-cursor '(box "DarkGoldenrod2")
  ;      evil-insert-state-cursor '((bar . 2) "SkyBlue2")
  ;      evil-emacs-state-cursor '(box "chartreuse3")
  ;      evil-replace-state-cursor '((hbar . 2) "chocolate")
  ;      evil-visual-state-cursor '((hbar . 2) "gray")
  ;      evil-motion-state-cursor '(box "plum3"))
	(defmacro evil-define-and-bind-text-object (key start-regex end-regex)
		(let ((inner-name (make-symbol "inner-name"))
					(outer-name (make-symbol "outer-name")))
			`(progn
				 (evil-define-text-object ,inner-name (count &optional beg end type)
					 (evil-select-paren ,start-regex ,end-regex beg end type count nil))
				 (evil-define-text-object ,outer-name (count &optional beg end type)
					 (evil-select-paren ,start-regex ,end-regex beg end type count t))
				 (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
				 (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
	;; Custom keybinds
  (evil-define-key '(normal visual) 'global
		(kbd "+") 'evil-first-non-blank
		(kbd "?") 'evil-last-non-blank
    (kbd "-") 'evil-ex-search-forward
    (kbd "_") 'evil-ex-search-backward
    (kbd "RET") 'evil-ex-nohighlight)
  (evil-define-key 'normal 'global
	  (kbd "g,") 'goto-last-change
	  (kbd "g;") 'goto-last-change-reverse
    (kbd "C-M-j") 'evil-mc-make-cursor-move-next-line
    (kbd "C-M-k") 'evil-mc-make-cursor-move-prev-line)
  (evil-define-key 'insert 'global
    (kbd "C-a") 'evil-beginning-of-line
    (kbd "C-e") 'end-of-line
    (kbd "C-k") 'kill-line
    (kbd "C-SPC") 'company-complete)
	;; Minibuffer
	(evil-define-key 'insert minibuffer-local-map
		(kbd "C-v") 'scroll-up-command
    (kbd "C-p") 'previous-line
    (kbd "C-n") 'next-line
		(kbd "<escape>") 'abort-recursive-edit)
	;; Terminal
  (evil-set-initial-state 'term-mode 'emacs)
	;; Package menu
  (evil-set-initial-state 'package-menu-mode 'normal)
	(evil-define-key 'normal package-menu-mode-map
	 	"i" 'package-menu-mark-install
    "U" 'package-menu-mark-upgrades
    "d" 'package-menu-mark-delete
    "gr" 'package-menu-refresh
    "u" 'package-menu-mark-unmark
    "x" 'package-menu-execute
    "q" 'quit-window)
	(evil-define-key 'normal profiler-report-mode-map
		"h" 'profiler-report-collapse-entry
		"l" 'profiler-report-expand-entry)
	;; Text objects
	(evil-define-and-bind-text-object "-" "-" "-")
	:hook
	(evil-list-view-mode . (lambda() (setq show-trailing-whitespace nil))))

;; Mode integration
(use-package evil-collection
	:after evil
	:init
	(setq evil-collection-company-use-tng nil
	      evil-collection-term-sync-state-and-mode-p nil
	      evil-collection-setup-debugger-keys nil)
	:config
	(evil-collection-init '(flycheck dired profiler xref)))

;; Multiple cursors
(use-package evil-mc
  :after evil
  :init
  ;; Remove default keybindings
  (defvar evil-mc-key-map (make-sparse-keymap))
  ;; Fix cursor alignment on macOS/windows
  (setq evil-mc-enable-bar-cursor nil)
  :config
  (global-evil-mc-mode 1)
  ;; Visual selections are not compatible with matchit
  (add-to-list 'evil-mc-incompatible-minor-modes 'evil-matchit-mode)
  ;; Remove multiple cursors using escape
  (evil-define-key 'normal evil-mc-key-map (kbd "<escape>") 'evil-mc-undo-all-cursors)
  ;; Fix 0-register after mc-mode (https://github.com/gabesoft/evil-mc/issues/70)
  (add-hook 'evil-mc-after-cursors-deleted
	    (lambda ()
	      (setq evil-was-yanked-without-register t)))
  ;; Enable registers in mc-mode (https://github.com/gabesoft/evil-mc/issues/83)
  (setq evil-mc-cursor-variables
	(mapcar
	 (lambda (s)
	   (remove 'register-alist
		   (remove 'evil-markers-alist
			   (remove evil-was-yanked-without-register s))))
	 evil-mc-cursor-variables)))

;; Improved matching
(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1))

;; Exchange operator
(use-package evil-exchange
  :init
  (advice-add 'evil-exchange-install :override
							(lambda ()
								(evil-global-set-key 'normal "go" 'browse-url-at-point)
								(evil-global-set-key 'normal "gx" 'evil-exchange)
								(evil-global-set-key 'normal "gX" 'evil-exchange-cancel)
								(evil-global-set-key 'visual "X" 'evil-exchange)))
  :config
  (evil-exchange-install)
  (defface atom/evil-exchange-highlight-face
    '((t (:inherit region :background "sienna1")))
    "Face for evil exchange"
    :group 'evil-exchange)
  (setq evil-exchange-highlight-face 'atom/evil-exchange-highlight-face))

;; Increment/decrement operation
(use-package evil-numbers
	:quelpa (evil-numbers :fetcher github :repo "dieggsy/evil-numbers")
  :after evil
  :config
  (evil-global-set-key 'normal (kbd "C-A") 'evil-numbers/inc-at-pt)
  (evil-global-set-key 'normal (kbd "C-X") 'evil-numbers/dec-at-pt))

;; Search visual selection
(use-package evil-visualstar
  :after evil
  :config
  (global-evil-visualstar-mode))

;; Surround operators
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1)
  (evil-define-key 'normal evil-surround-mode-map "S" 'evil-surround-edit))

;; Comment operators
(use-package evil-commentary
	:config
	(evil-commentary-mode))

;; Indentation text object
(use-package evil-indent-plus
  :after evil
  :config
  (evil-indent-plus-default-bindings))

;; Argument text object
(use-package evil-args
  :after evil
  :config
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))

;; Highlight operations
(use-package evil-goggles
  :after evil
  :init
  (setq evil-goggles-duration 0.2
        evil-goggles-enable-change nil
        evil-goggles-enable-delete nil
        evil-goggles-enable-record-macro nil)
  :config
  (evil-goggles-mode))

;; Clever-F navigation
(use-package evil-snipe
  :after evil
  :init
  (setq evil-snipe-scope 'line
        evil-snipe-spillover-scope 'buffer)
  :config
  (evil-snipe-override-mode 1)
  :custom-face
  (evil-snipe-matches-face ((t (:inherit region :background "light blue")))))

;; Jump navigation
(use-package avy
  :after evil
	:init
  (setq avy-orders-alist '((avy-goto-char-timer . avy-order-closest))))

;; Leader prefix
(use-package evil-leader
  :after evil
  :config
  (global-evil-leader-mode t)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "TAB" 'mode-line-other-buffer
		":" 'execute-extended-command
    "0" 'treemacs-select-window
    "1" 'winum-select-window-1
    "2" 'winum-select-window-2
    "3" 'winum-select-window-3
    "4" 'winum-select-window-4
    "5" 'winum-select-window-5
    "6" 'winum-select-window-6
    "7" 'winum-select-window-7
    "8" 'winum-select-window-8
    "9" 'winum-select-window-9
		"b b" 'switch-to-buffer
    "b d" 'kill-this-buffer
    "b k" 'kill-buffer
		"b E" 'set-buffer-file-coding-system
		"e c" 'flycheck-clear-errors
		"e e" 'flycheck-explain-error-at-point
		"e h" 'flycheck-describe-checker
		"e l" 'flycheck-list-errors
		"e s" 'flycheck-select-checker
		"e S" 'flycheck-set-checker-executable
    "e n" 'next-error
    "e p" 'previous-error
    "f D" 'atom/delete-this-file-and-buffer
    "f e c" 'atom/open-init
    "f e l" 'find-library
		"f f" 'find-file
		"f M-f" 'hexl-find-file
		"f r" 'atom/recentf-open-files
		"f M-r" 'treemacs-find-file
		"f R" 'revert-buffer
    "f s" 'save-buffer
    "f S" 'save-some-buffers
		"f y" (lambda () (interactive) (gui-set-selection 'CLIPBOARD (message (buffer-file-name))))
		"h d b" 'describe-bindings
		"h d c" 'describe-char
		"h d f" 'describe-function
		"h d F" 'describe-face
		"h d k" 'describe-key
		"h d K" 'describe-keymap
		"h d m" 'describe-mode
		"h d M" 'describe-minor-mode
		"h d p" 'describe-package
		"h d s" 'describe-symbol
		"h d S" 'describe-syntax
		"h d t" 'describe-theme
		"h d v" 'describe-variable
		"h p k" 'profiler-stop
		"h p r" 'profiler-report
		"h p s" 'profiler-start
    "SPC" 'evil-avy-goto-char-timer
    "j c" 'evil-avy-goto-char
    "j j" 'evil-avy-goto-char-2
    "j l" 'evil-avy-goto-line
    "j w" 'evil-avy-goto-word-or-subword-1
		"p b" 'projectile-switch-to-buffer
		"p d" 'projectile-find-dir
		"p f" 'projectile-find-file
		"p k" 'projectile-kill-buffers
		"p o" 'projectile-multi-occur
		"p r" 'projectile-recentf
		"p t" 'treemacs-display-current-project-exclusively
		"P d" 'package-delete
		"P l" 'package-list-packages
		"P r" 'package-refresh-contents
		"P t" 'try
    "s c" 'evil-ex-nohighlight
		"t h l" 'global-hl-line-mode
		"t h s" 'font-lock-mode
		"t s" 'flycheck-mode
    "t l" 'toggle-truncate-lines
    "t n" 'display-line-numbers-mode
    "t w" 'whitespace-mode
		"T l" 'load-theme
    "w TAB" 'ace-select-window
    "w u" 'winner-undo
    "w U" 'winner-redo
		"w b" 'atom/switch-to-minibuffer
    "w r" 'rotate-frame-clockwise
    "w R" 'rotate-frame-anticlockwise
    "w d" 'delete-window
    "w D" 'ace-delete-window
    "w f" 'follow-mode
    "w F" 'make-frame
    "w o" 'other-frame
    "w w" 'ace-window
    "w W" 'other-window
    "w h" 'evil-window-left
    "w H" 'evil-window-move-far-left
    "w j" 'evil-window-down
    "w J" 'evil-window-move-very-bottom
    "w k" 'evil-window-up
    "w K" 'evil-window-move-very-top
    "w l" 'evil-window-right
    "w L" 'evil-window-move-far-right
    "w s" (lambda () (interactive) (split-window-below) (windmove-down))
    "w S" 'split-window-below
    "w v" (lambda () (interactive) (split-window-right) (windmove-right))
    "w V" 'split-window-right
		"w <" 'evil-window-decrease-width
		"w >" 'evil-window-increase-width
    "w +" 'evil-window-increase-height
    "w -" 'evil-window-decrease-height
    "w =" 'balance-windows)
	;; Enable evil late to fix leader key in meta buffers
  (evil-mode 1))

;; ----------------------------------------------------------------------------------
;; Window management
;; ----------------------------------------------------------------------------------

;; Window navigation
(use-package ace-window)

;; Window numbering
(use-package winum
  :config
  (winum-mode))

;; Window rotation
(use-package transpose-frame)

;; Discrete window popups
(use-package shackle
  :custom
  (shackle-rules
	 '((help-mode :align below :size 0.2 :select t)
	   (flycheck-error-list-mode :align below :size 0.2)
		 (evil-list-view-mode :align below :size 0.4)))
  :config
  (shackle-mode 1))

;; ----------------------------------------------------------------------------------
;; Minibuffer completion
;; ----------------------------------------------------------------------------------

(use-package fuz
	:quelpa (fuz :fetcher github :repo "rustify-emacs/fuz.el")
	:config
	(unless (require 'fuz-core nil t)
		(fuz-build-and-load-dymod)))

(use-package projectile
	:init
	(setq projectile-completion-system 'default)
	:config
	(projectile-mode +1))

(use-package selectrum
	:config
	(selectrum-mode +1))

;; (use-package selectrum-prescient
;; 	:after selectrum
;; 	:init
;; 	(setq prescient-filter-method '(literal fuzzy))
;; 	:config
;; 	(selectrum-prescient-mode)
;; 	(prescient-persist-mode +1))

(defun atom/recentf-open-files ()
  "Use `completing-read' to open a recent file."
  (interactive)
  (let ((files (mapcar 'abbreviate-file-name recentf-list)))
    (find-file (completing-read "Find recent file: " files nil t))))

(defun atom/eval-region ()
  (interactive)
  (eval-region (region-beginning) (region-end))
  (evil-normal-state))

(defun atom/evil-avy-make-cursor-timer (&optional arg)
  "Read one or many consecutive chars and add a cursor at the first on
The window scope is determined by `avy-all-windows' (ARG negates it)."
  (interactive "P")
  (let ((avy-all-windows (if arg (not avy-all-windows) avy-all-windows)))
    (avy-with avy-goto-char-timer
      (setq avy--old-cands (avy--read-candidates))
      (setq avy-action '(lambda (pos)
			  (evil-mc-run-cursors-before)
			  (evil-mc-make-cursor-at-pos pos)))
      (avy-process avy--old-cands))))

(defun atom/open-init ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun atom/delete-this-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (if (yes-or-no-p "Delete the current file? ")
			(let ((filename (buffer-file-name)))
	(when filename
	  (if (vc-backend filename)
				(vc-delete-file filename)
			(progn
	      (delete-file filename)
	      (message "Deleted file %s" filename)
	      (kill-buffer)))))))

(defun atom/switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
	(if (active-minibuffer-window)
			(select-window (active-minibuffer-window))
		(error "Minibuffer is not active")))

;; ----------------------------------------------------------------------------------
;; Major modes
;; ----------------------------------------------------------------------------------

(use-package rust-mode)

(use-package csharp-mode)

(use-package web-mode
	:mode (("\\.hbs\\'" . web-mode))
	:init
  (setq web-mode-auto-close-style 2
	      web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2))

;; ----------------------------------------------------------------------------------
;; Miscellaneous
;; ----------------------------------------------------------------------------------

;; Ensure $PATH is configured
(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :init
  ;; Load the shell non-interactively
  (setq exec-path-from-shell-arguments nil)
  :config
  (exec-path-from-shell-initialize))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; Language server
(use-package lsp-mode
	:init
  (setq lsp-auto-guess-root t)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-semantic-highlighting t)
  (setq lsp-enable-snippet nil)
  (setq lsp-prefer-capf t)
  :config
	(setq lsp-rust-server 'rust-analyzer)
  (add-hook 'prog-mode-hook #'lsp-deferred)
	(advice-add #'lsp--auto-configure :override
							#'(lambda () (lsp-flycheck-enable))))

;; Error management
(use-package flycheck
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :config
  (global-flycheck-mode))

;; Shortcut guide
(use-package which-key
  :config
  (which-key-mode))

;; Trial packages
(use-package try)

(defun selectrum-fuz--refine (pattern candidates)
  "Filter and sort CANDIDATES according to PATTERN."
	(let* ((candidates-by-score (make-hash-table :test #'equal))
				 (candidates-filtered (cl-delete-if-not
															 (lambda (candidate)
																 (puthash candidate (fuz-calc-score-skim pattern candidate) candidates-by-score))
															 (copy-sequence candidates))))
		(cl-stable-sort candidates-filtered #'> :key (lambda (candidate) (gethash candidate candidates-by-score)))))

(defun selectrum-fuz--highlight (pattern candidates)
	"Highlight characters in CANDIDATES matching the PATTERN."
	(mapcar
	 (lambda (candidate)
		 (progn
			 (dolist (pos (fuz-find-indices-skim pattern candidate))
				 (setq candidate (copy-sequence candidate))
				 (put-text-property pos (1+ pos) 'face 'selectrum-primary-highlight candidate))
			 candidate))
	 candidates))

(setq selectrum-refine-candidates-function #'selectrum-fuz--refine)
(setq selectrum-highlight-candidates-function #'selectrum-fuz--highlight)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
