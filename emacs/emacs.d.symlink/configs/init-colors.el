;; Make emacs look good in terminal
(color-theme-approximate-on)

;; Setup Solarized theme
(load-theme 'solarized-light t)
(load-theme 'solarized-dark t)

(defun mb/pick-color-theme (frame)
  (select-frame frame)
  (if (window-system frame)
    (progn
      (disable-theme 'solarized-dark)
      (enable-theme 'solarized-light))
    (progn
      (disable-theme 'solarized-light)
      (enable-theme 'solarized-dark))))
(add-hook 'after-make-frame-functions 'mb/pick-color-theme)

;; For when started with emacs or emacs -nw rather than emacs --daemon
(if window-system
  (enable-theme 'solarized-light)
  (enable-theme 'solarized-dark))

;; Highlight everything with colors
(global-color-identifiers-mode)
(diminish 'color-identifiers-mode)

;; Enable rainbow delimiters for programming
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Highlight the current line
(global-hl-line-mode)

(provide 'init-colors)
