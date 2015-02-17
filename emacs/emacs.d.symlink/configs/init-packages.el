; list the packages you want
(setq package-list
  '(evil
    diminish
    saveplace
    savehist
    recentf
    smart-mode-line
    flycheck
    powerline
    exec-path-from-shell
    color-identifiers-mode
    linum-relative
    rainbow-delimiters
    color-theme-solarized
    smartparens))

;; Add additional package sources
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

;; Initialize packages
(package-initialize)

;; Fetch list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; Install missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(provide 'init-packages)
