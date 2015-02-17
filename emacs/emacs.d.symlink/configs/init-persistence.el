;; Use a directory specifically for backup and saves
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Remember last edit positions
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)

;; Minibuffer history
(setq savehist-file "~/.emacs.d/savehist"
  savehist-additional-variables '(search ring regexp-search-ring)
  savehist-autosave-interval 60)
(setq-default history-length 1000)
(savehist-mode +1)

;; Enable bookmark functionality
(setq bookmark-default-file "~/.emacs.d/bookmarks"
  bookmark-save-flag 1) ;; save after every change

;; Save recently used files
(setq recentf-save-file "~/.emacs.d/recentf"
  recentf-max-saved-items 1000
  recentf-max-menu-items 500)
(recentf-mode +1)
(run-with-timer 1800 1800 'recentf-save-list)

(provide 'init-persistence)
