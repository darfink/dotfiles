; Fix evil-search repeat with 'n'
; Fix parens highlight overlap visual mode (majs)
; Fix all jumps relative to cursor
; Fix evil-commentary
; Add avy for swapping words?
; Truncate modeline file paths (modeline-buffer-path-function)
; Use web-mode for '%'?
; Use H/L for 'arg' navigation?
; Eval keybinds (eval-region)
; Organize custom functions
; Config multiple cursors
; Config indentation
; Config surround keybindings
; Add narrowing keybinds
; Implement window swap keybind?
; Implement tree view (treemacs, lsp-treemacs, lookup all-the-icons)
; Leader keybinds
; <company> previous wrap around
; <company> fix ctrl-f (company--show-inline-p)
; lsp-ui
; Investigate helm or ivy

; (setq debug-on-error t)
(setq delete-old-versions -1)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
(setq vc-follow-symlinks t) ; Follow symlinks without prompt
(setq inhibit-startup-screen t)
(setq coding-system-for-write 'utf-8)
(setq sentence-end-double-space nil)
(setq vc-make-backup-files t)
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))
(setq xref-prompt-for-identifier nil)
(setq undo-tree-auto-save-history t) ; Preserve undo history between sessions
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
(setq use-dialog-box nil) ; Disable GUI dialogs
(setq-default show-trailing-whitespace t)
(setq-default tab-width 2)

;; Do not use the system clipboard when yanking by default
(setq select-enable-clipboard nil)

;; Prevent custom variables from polluting config
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; Option as meta modifier on macOS
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'nil)
  (setq mac-right-option-modifier 'meta)
  (setq mac-command-modifier 'super))

;; Prefer y/n keypress instead of typing yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

(global-hl-line-mode) ; Highlight the active line
(blink-cursor-mode 0) ; Disabling blinking cursor
(tool-bar-mode -1) ; Remove the tool bar
(save-place-mode 1) ; Continue from before
(scroll-bar-mode -1) ; Disable native scroll bars
(recentf-mode) ; Record recent files

;; ----------------------------------------------------------------------------------
;; Terminal
;; ----------------------------------------------------------------------------------

;; On windows, use eshell otherwise ansi-term
(if (eq system-type 'windows-nt)
  (defalias 'os-term 'eshell)
  (defalias 'os-term 'ansi-term))

;; Don't ask which shell, use the default
(defvar my-term-shell (getenv "SHELL"))
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

;; Delete the terminal buffer upon exit
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

;; Disable whitespace highlighting
(add-hook 'term-mode-hook (lambda() (setq show-trailing-whitespace nil)))

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
;; Editor theming
;; ----------------------------------------------------------------------------------

; Highlight delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Fancy modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; Sweet themes
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; ----------------------------------------------------------------------------------
;; Text completion
;; ----------------------------------------------------------------------------------

;; Use case-insensitive fuzzy completion
(setq completion-ignore-case t)
(setq completion-styles '(basic flex))

(use-package company
  :init
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.0)
  (setq company-selection-wrap-around t)
	(setq company-backends '(company-capf company-files company-dabbrev))
	(setq company-frontends '(company-box-frontend company-echo-metadata-frontend company-preview-frontend))
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
  (global-company-mode))

(use-package company-box
  :after company
  :init
  :hook (company-mode . company-box-mode))

;; ----------------------------------------------------------------------------------
;; Modal editing
;; ----------------------------------------------------------------------------------

;; Evil mode
(use-package evil
  :init
	(setq evil-magic nil)
	(setq evil-want-minibuffer t)
  (setq evil-want-C-u-delete t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-want-change-word-to-end nil)
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
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-select-search-module 'evil-search-module 'evil-search)
	;; Custom keybinds
	(evil-global-set-key 'normal "g," 'goto-last-change)
	(evil-global-set-key 'normal "g;" 'goto-last-change-reverse)
  (evil-global-set-key 'normal (kbd "+") 'evil-first-non-blank)
  (evil-global-set-key 'normal (kbd "g+") 'evil-last-non-blank)
  (evil-global-set-key 'normal (kbd "-") 'evil-ex-search-forward)
  (evil-global-set-key 'normal (kbd "_") 'evil-ex-search-backward)
  (evil-global-set-key 'normal (kbd "C-ª") 'evil-mc-make-cursor-move-prev-line)
  (evil-global-set-key 'normal (kbd "C-√") 'evil-mc-make-cursor-move-next-line)
  (evil-global-set-key 'normal (kbd "RET") 'evil-ex-nohighlight)
  (evil-global-set-key 'insert (kbd "C-a") 'evil-beginning-of-line)
  (evil-global-set-key 'insert (kbd "C-e") 'end-of-line)
  (evil-global-set-key 'insert (kbd "C-k") 'kill-line)
  (evil-global-set-key 'insert (kbd "C-SPC") 'company-complete)
	(evil-define-key 'insert minibuffer-local-map (kbd "<escape>") 'abort-recursive-edit)
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
	;; Ivy minibuffer
  (evil-define-key 'insert ivy-minibuffer-map
  	[escape] 'abort-recursive-edit
  	[backspace] 'ivy-backward-delete-char
  	(kbd "C-r") 'ivy-reverse-i-search
  	(kbd "C-n") 'ivy-next-line
  	(kbd "C-p") 'ivy-previous-line)
	;; Text objects
	(evil-define-and-bind-text-object "-" "-" "-"))

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
  (setq evil-goggles-duration 0.2)
  (setq evil-goggles-enable-change nil)
  (setq evil-goggles-enable-delete nil)
  (setq evil-goggles-enable-record-macro nil)
  :config
  (evil-goggles-mode))

;; Clever-F navigation
(use-package evil-snipe
  :after evil
  :init
  (setq evil-snipe-scope 'line)
  (setq evil-snipe-spillover-scope 'buffer)
  :config
  (evil-snipe-override-mode 1)
  :custom-face
  (evil-snipe-matches-face ((t (:inherit region :background "light blue")))))

;; Jump navigation
(use-package avy
  :after evil
  :config
  (add-to-list 'avy-orders-alist '(avy-goto-char-timer . avy-order-closest)))

(defun atom/evil-avy-make-cursor-timer (&optional arg)
  "Read one or many consecutive chars and add a cursor at the first one.
The window scope is determined by `avy-all-windows' (ARG negates it)."
  (interactive "P")
  (let ((avy-all-windows (if arg
                             (not avy-all-windows)
                           avy-all-windows)))
    (avy-with avy-goto-char-timer
      (setq avy--old-cands (avy--read-candidates))
      (setq avy-action '(lambda (pos)
			  (evil-mc-run-cursors-before)
			  (evil-mc-make-cursor-at-pos pos)))
      (avy-process avy--old-cands))))

(defun atom/open-init ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun atom/switch-to-recent-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

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

;; Leader prefix
(use-package evil-leader
  :after evil
  :config
  (global-evil-leader-mode t)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "TAB" 'atom/switch-to-recent-buffer
		":" 'execute-extended-command
    "0" 'winum-select-window-0
    "1" 'winum-select-window-1
    "2" 'winum-select-window-2
    "3" 'winum-select-window-3
    "4" 'winum-select-window-4
    "5" 'winum-select-window-5
    "6" 'winum-select-window-6
    "7" 'winum-select-window-7
    "8" 'winum-select-window-8
    "9" 'winum-select-window-9
		"b b" 'ivy-switch-buffer
    "b d" 'kill-this-buffer
    "e n" 'next-error
    "e p" 'previous-error
    "f \S-d" 'atom/delete-this-file-and-buffer
    "f e c" 'atom/open-init
    "f e l" 'find-library
    "f s" 'save-buffer
    "f S" 'save-some-buffers
    "SPC" 'evil-avy-goto-char-timer
    "j c" 'evil-avy-goto-char
    "j j" 'evil-avy-goto-char-2
    "j l" 'evil-avy-goto-line
    "j w" 'evil-avy-goto-word-or-subword-1
		"P d" 'package-delete
		"P l" 'package-list-packages
		"P r" 'package-refresh-contents
		"P t" 'try
    "s c" 'evil-ex-nohighlight
		"t h l" 'global-hl-line-mode
		"t h s" 'font-lock-mode
    "t l" 'toggle-truncate-lines
    "t w" 'whitespace-mode
    "w TAB" 'ace-select-window
    "w u" 'winner-undo
    "w U" 'winner-redo
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
    "w s" 'split-window-below
    "w v" 'split-window-right
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
  (shackle-rules '((help-mode :align below :size 0.2 :select t)))
  :config
  (shackle-mode 1))

;; ----------------------------------------------------------------------------------
;; Generic completion
;; ----------------------------------------------------------------------------------

(use-package ivy
	:config
	(ivy-mode 1))

(use-package all-the-icons-ivy
	:after ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

(use-package ivy-prescient
	:after ivy
	:init
	(setq prescient-filter-method '(literal fuzzy))
	:config
	(ivy-prescient-mode)
	(prescient-persist-mode +1))

(defun spacemacs/eval-region ()
  (interactive)
  (eval-region (region-beginning) (region-end))
  (evil-normal-state))

;; ----------------------------------------------------------------------------------
;; Major modes
;; ----------------------------------------------------------------------------------

(use-package rust-mode)

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

;; Language server
(use-package lsp-mode
	:init
  (setq lsp-prefer-capf t)
  :config
  (add-hook 'prog-mode-hook #'lsp-deferred)
	(advice-add #'lsp--auto-configure :override
							#'(lambda () (lsp-flycheck-enable))))

;; Error management
(use-package flycheck
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :config
  (global-flycheck-mode))

;; Relative line numbers
(use-package linum-relative
  :hook ((prog-mode conf-mode) . linum-relative-mode))

;; Pair completion
(use-package smartparens
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode t)
  :diminish smartparens-mode
  :hook (prog-mode . smartparens-mode))

;; Shortcut guide
(use-package which-key
  :config
  (which-key-mode))

;; Trial packages
(use-package try)
