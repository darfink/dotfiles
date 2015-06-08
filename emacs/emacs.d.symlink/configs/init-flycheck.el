;; Enable flycheck
(require 'flycheck)
(global-flycheck-mode t)

;; Display errors immediately
(setq eldoc-idle-delay 0.1
      flycheck-display-errors-delay 0.2)

(after 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(provide 'init-flycheck)