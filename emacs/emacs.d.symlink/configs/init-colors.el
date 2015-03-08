;; Make emacs look good in terminal
(color-theme-approximate-on)

;; Setup Solarized theme
(load-theme 'solarized t)

(set-frame-parameter nil 'background-mode 'light)
(when (not (display-graphic-p))
  (set-terminal-parameter nil 'background-mode 'dark))
;; Call enable-theme to pick up the change to 'background-mode.
(enable-theme 'solarized)

;; Highlight everything with colors
(global-color-identifiers-mode)
(diminish 'color-identifiers-mode)

;; Enable rainbow delimiters for programming
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Highlight the current line
(global-hl-line-mode)

(provide 'init-colors)
