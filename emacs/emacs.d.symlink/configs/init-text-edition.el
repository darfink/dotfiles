;; Prefer two spaces for indentation
(setq-default c-basic-offset 2 c-default-style "bsd")
(setq-default tab-width 2 indent-tabs-mode nil)
(setq-default highlight-tabs t)

;; Disable word wrapping
(setq-default truncate-lines 1)

;; Enable smart delimiters
(require 'smartparens-config)
(smartparens-global-mode t)

(provide 'init-text-edition)
