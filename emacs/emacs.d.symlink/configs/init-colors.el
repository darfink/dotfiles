;; Make emacs look good in terminal
(color-theme-approximate-on)

;; Setup Solarized theme
(load-theme 'solarized t)

;; Use dark in terminal, and light in GUI
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (set-frame-parameter frame
                                 'background-mode
                                 (if (display-graphic-p frame) 'light 'dark))
            (enable-theme 'solarized)))

;; Highlight everything with colors
(global-color-identifiers-mode)
(diminish 'color-identifiers-mode)

;; Enable rainbow delimiters for programming
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Highlight the current line
(global-hl-line-mode)

(provide 'init-colors)
