;; User information
(setq user-full-name "Elliott Linder")
(setq user-mail-address "elliott.darfink@gmail.com")

;; "after" macro definition
(if (fboundp 'with-eval-after-load)
    (defmacro after (feature &rest body)
      "After FEATURE is loaded, evaluate BODY."
      (declare (indent defun))
      `(with-eval-after-load ,feature ,@body))
  (defmacro after (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
        '(progn ,@body))))

;; Add the config directory to path
(add-to-list 'load-path (expand-file-name "configs" user-emacs-directory))

;; Load all configs
(require 'init-packages)
(require 'init-interface)
(require 'init-persistence)
(require 'init-colors)
(require 'init-evil)
(require 'init-text-edition)
(require 'init-i18n)
(require 'init-flycheck)
(require 'init-web-mode)

;; Enable the right option modifier on OS X
(when (eq system-type 'darwin)
  (setq mac-right-option-modifier 'none)
  (exec-path-from-shell-initialize))
