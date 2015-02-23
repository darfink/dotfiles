;; Enable page-up functionality
(setq evil-want-C-u-scroll t)

;; Enable colorful cursors
(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("#258BD2" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))

;; Enable dem' evil-mode
(require 'evil)
(evil-mode 1)

;; Improve our '%' operator
(require 'evil-matchit)
(global-evil-matchit-mode 1)

;; Enable the surruound operator
(require 'evil-surround)
(global-evil-surround-mode 1)

;; Enable persistent highlighting
(require 'highlight)
(require 'evil-search-highlight-persist)
(global-evil-search-highlight-persist t)

;; Do not use the system clipboard unless explicit
(setq x-select-enable-clipboard nil)
(setq x-select-enable-primary nil)

;; ---------------------------------
;; Custom key bindings
;; ---------------------------------

;; Make 'RET' remove any search highlights
(define-key evil-normal-state-map (kbd "RET") 'evil-search-highlight-persist-remove-all)

(provide 'init-evil)
