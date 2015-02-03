;; No fringes or scrollbars
(set-fringe-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Start scratch in text mode (usefull to get a faster Emacs load time
;; because it avoids autoloads of elisp modes)
(setq initial-major-mode 'text-mode)

;; Disable the splash screen
(setq inhibit-startup-message t)

;; Enable global line numbers
(global-linum-mode 1)

;; Make them relative
(require 'linum-relative)
(setq linum-relative-current-symbol '"")

(provide 'init-interface)
