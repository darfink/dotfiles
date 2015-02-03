;; Disable the tool bar first to speed up initialization
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; User information
(setq user-full-name "Elliott Linder")
(setq user-mail-address "elliott.darfink@gmail.com")

;; Add the config directory to path
(add-to-list 'load-path (expand-file-name "configs" user-emacs-directory))

;; Load all configs
(require 'init-packages)
(require 'init-interface)
(require 'init-persistence)
(require 'init-colors)
(require 'init-evil)

;; Enable smart paranthesis
(require 'smartparens-config)
(smartparens-global-mode t)

;; Enable the right option modifier on OS X
(when (eq system-type 'darwin)
  (setq mac-right-option-modifier 'none))
