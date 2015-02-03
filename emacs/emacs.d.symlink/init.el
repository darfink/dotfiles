;; packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

;; Disable the tool bar
(tool-bar-mode -1)

;; Enable line numbers
(global-linum-mode 1)

;; Enable color theme approximation
(color-theme-approximate-on)

;; Relative line numbers
(require 'linum-relative)

;; Enable smart paranthesis
(require 'smartparens-config)
(smartparens-global-mode t)

;; Do not use the system clipboard
(setq x-select-enable-clipboard nil)
(setq x-select-enable-primary nil)

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

;; Setup Solarized theme
(load-theme 'solarized-light t)
(load-theme 'solarized-dark t)

(defun mb/pick-color-theme (frame)
  (select-frame frame)
  (if (window-system frame)
    (progn
      (disable-theme 'solarized-dark) ; in case it was active
      (enable-theme 'solarized-light))
    (progn
      (disable-theme 'solarized-light) ; in case it was active
      (enable-theme 'solarized-dark))))
(add-hook 'after-make-frame-functions 'mb/pick-color-theme)

;; For when started with emacs or emacs -nw rather than emacs --daemon
(if window-system
  (enable-theme 'solarized-light)
  (enable-theme 'solarized-dark))
