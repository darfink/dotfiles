;; -*- mode: emacs-lisp -*-

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
    '(
      markdown
      lua
      syntax-checking
      auto-completion
      javascript
      colors
      csharp
      rust
      html
      php
      osx)
   dotspacemacs-additional-packages
    '(
      editorconfig
      (hlsl-mode :location (recipe :fetcher github :repo "darfink/hlsl-mode")))
   dotspacemacs-excluded-packages '()
   dotspacemacs-frozen-packages '()
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '(recents projects)
   dotspacemacs-themes '(solarized-light solarized-dark)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Operator Mono"
                               :size 12
                               :weight normal
                               :slant normal
                               :width normal
                               :powerline-scale 1.4)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-line-numbers 'relative
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-command-key ":"
   dotspacemacs-use-ido nil
   dotspacemacs-enable-paste-micro-state t
   dotspacemacs-guide-key-delay 0.4
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil)
  ;; Some paths are specific to the shell (zshrc)
  (setq exec-path-from-shell-check-startup-files nil)
  (setq-default truncate-lines t))

(defun dotspacemacs/user-config ()
  ;; Always prefer two spaces for indentation
  (setq-default omnisharp-server-executable-path "/Users/atomen/Projects/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe")
  (setq-default require-final-newline t)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  (setq-default standard-indent 2)
  (setq-default c-basic-offset 2)
  (setq-default js2-strict-trailing-comma-warning nil)
  (setq js-indent-level 2)
  (setq sgml-basic-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq-default require-final-newline nil)

  ;; Make underscore part of the motion in Rust mode
  (add-hook 'rust-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

  ;; Don't show hidden files in Neotree
  (setq-default neo-show-hidden-files nil)

  ;; Disable GUI dialogs
  (setq use-dialog-box nil)

  ;; Custom keybindings for the terminal
  (spacemacs/set-leader-keys "p$" 'projectile-run-term-in-root)
  (spacemacs/set-leader-keys "ot" 'os-term)

  ;; Enable persisent undos (see: https://github.com/syl20bnr/spacemacs/issues/774#)
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist
        `(("." . ,(concat spacemacs-cache-directory "undo"))))
  (unless (file-exists-p (concat spacemacs-cache-directory "undo"))
    (make-directory (concat spacemacs-cache-directory "undo")))

  ;; Don't bother with Vim mode in the terminal
  (evil-set-initial-state 'term-mode 'emacs)

  ;; Enable the cache for projectile
  (setq projectile-enable-caching t)

  ;; Follow symlinks without prompt
  (setq vc-follow-symlinks t)

  ;; Hide specific modes
  (with-eval-after-load 'omnisharp
    (diminish 'omnisharp-mode))

  ;; Prevent visual selection from overriding clipboard
  (fset 'evil-visual-update-x-selection 'ignore)

  ;; Do not use the system clipboard unless explicit
  ;; Override the default x-select-text function because it doesn't
  ;; respect x-select-enable-clipboard on OS X.
  (defun x-set-selection (type data) (ns-set-pasteboard data))
  (defun x-get-selection (&optional type data) (ns-get-pasteboard))
  (setq interprogram-cut-function nil)
  (setq interprogram-paste-function nil)

  ;; Disable natural swiping
  (global-set-key [swipe-left] 'ignore)
  (global-set-key [swipe-right] 'ignore)

  ;; Highlight active token
  (spacemacs/toggle-automatic-symbol-highlight-on)

  ;; Add additional file support for web-mode
  (add-to-list 'auto-mode-alist '("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.twig$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.blade\\.php$" . web-mode))
)

;; On windows, use eshell otherwise ansi-term
(if (eq system-type 'windows-nt)
  (defalias 'os-term 'eshell)
  (defalias 'os-term 'ansi-term))

;; Don't ask which shell, use the default
(defvar my-term-shell (getenv "SHELL"))
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

;; Delete the terminal buffer upon exit
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

;; Extension method to projectile
(defun projectile-run-term-in-root ()
  "Invoke `term` in the project's root."
  (interactive)
  (projectile-with-default-dir (projectile-project-root)
    (call-interactively 'os-term)))

;; Add terminal keybindings to Helm
(with-eval-after-load 'helm
  (define-key helm-map (kbd "C-u") '(lambda () (interactive) (kill-line 0)))
  (define-key helm-map (kbd "C-w") 'backward-kill-word))

;; OS X specific settings
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta)
  (setq mac-right-option-modifier 'nil)
  (setq mac-command-modifier 'super))

;; Setup flycheck and C# indentation
(defun my-csharp-mode-hook()
  (set-process-query-on-exit-flag (get-process "Omni-Server") nil)
  (flycheck-mode)
  (setq c-basic-offset 2))
(add-hook 'csharp-mode-hook 'my-csharp-mode-hook)

;; Scale fonts
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-case-fold-search nil t)
 '(ahs-default-range (quote ahs-range-whole-buffer) t)
 '(ahs-idle-interval 0.25 t)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil t)
 '(evil-want-Y-yank-to-eol nil)
 '(evil-want-change-word-to-end nil)
 '(js2-basic-offset 2)
 '(js2-mode-show-parse-errors nil)
 '(js2-strict-missing-semi-warning t)
 '(js2-strict-trailing-comma-warning t)
 '(package-selected-packages
   (quote
    (powerline spinner shut-up skewer-mode simple-httpd json-snatcher json-reformat parent-mode haml-mode pos-tip pkg-info epl flx anzu goto-chg highlight web-completion-data dash-functional tern rust-mode bind-map bind-key auto-complete popup yasnippet undo-tree js2-mode hydra f s async dash company iedit smartparens evil helm helm-core multiple-cursors avy markdown-mode csharp-mode flycheck php-mode helm-projectile helm-make projectile ws-butler winum which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toml-mode toc-org tagedit spaceline solarized-theme slim-mode scss-mode sass-mode reveal-in-osx-finder restart-emacs request rainbow-mode rainbow-identifiers rainbow-delimiters racer pug-mode popwin phpunit phpcbf php-extras php-auto-yasnippets persp-mode pcre2el pbcopy paradox osx-trash osx-dictionary org-plus-contrib org-bullets open-junk-file omnisharp neotree move-text mmm-mode markdown-toc lua-mode lorem-ipsum livid-mode linum-relative link-hint less-css-mode launchctl json-mode js2-refactor js-doc indent-guide hungry-delete hlsl-mode hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-themes helm-swoop helm-mode-manager helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag google-translate golden-ratio gh-md fuzzy flycheck-rust flycheck-pos-tip flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu emmet-mode editorconfig dumb-jump drupal-mode diminish company-web company-tern company-statistics column-enforce-mode color-identifiers-mode coffee-mode clean-aindent-mode cargo bracketed-paste auto-yasnippet auto-highlight-symbol aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(paradox-github-token t)
 '(ring-bell-function (quote ignore)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
