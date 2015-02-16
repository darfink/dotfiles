;; No fringes or scrollbars
(set-fringe-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; No menu bar in terminal
(when (not (display-graphic-p))
  (menu-bar-mode -1))

;; Trust all themes
(setq custom-safe-themes t)

;; Start scratch in text mode (usefull to get a faster Emacs load time
;; because it avoids autoloads of elisp modes)
(setq initial-major-mode 'text-mode)

;; Disable the splash screen
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)

;; Empty scratch buffer
(setq initial-scratch-message nil)

;; Don't know what this does
(setq redisplay-dont-pause t)

;; Enable global line and column numbers
(column-number-mode t)
(global-linum-mode 1)

;; Enable relative line numbers
(require 'linum-relative)
(setq linum-relative-current-symbol '"")

;; Use shorthand [y/n] syntax
(defalias 'yes-or-no-p 'y-or-n-p)

;; Prefer case-insensitive
(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; Enable smart mode line
(sml/setup)
(sml/apply-theme 'respectful)

;; Enable powerline
(require 'powerline)
(powerline-default-theme)
(setq powerline-default-separator 'contour)

;; Disable blinking cursor
(blink-cursor-mode -1)

;; Show matching delimiter
(show-paren-mode t)

;; Smooth scrolling
(setq scroll-margin 5)
(setq scroll-conservatively 9999)
(setq scroll-step 1)

;; Prevent mode-line cluttering
(require 'diminish)
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))

(provide 'init-interface)
