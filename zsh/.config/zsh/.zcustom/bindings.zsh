# Alt-s: execute 'git status'
bindkey -s '\es' '^Ugws^M'

# Alt-l: execute 'ls'
bindkey -s '\el' '^Uls^M'

# Alt-.: insert last argument of previous command
bindkey '\e.' insert-last-word

# Ctrl-v: execute 'vim'
bindkey -s '^v' '^Avim ^M'

# Ctrl-r: search history
bindkey '^R' history-incremental-search-backward

# Ctrl-Alt-e: expand aliases
bindkey '\e^E' expand-aliases

# Skim key bindings
bindkey '\et' fzf-file-widget      # Alt-t - insert descending file
bindkey '\eT' fzf-file-all-widget  # Alt-T - insert descending file (including hidden)
bindkey '\ej' fzf-jump-widget      # Alt-j - change directory using autojump
bindkey '\eJ' fzf-jump-file-widget # Alt-J - insert file using autojump
bindkey '\ec' fzf-cd-widget        # Alt-c - change to descending directory
bindkey '\eC' fzf-cd-all-widget    # Alt-C - change to descending directory (including hidden)
bindkey '^R' fzf-history-widget    # Ctrl-R - search history interactively

# Remove arguments using M-delete
bindkey '^[^?' shell-backward-kill-word

# Vim inspired tab-menu navigation
zmodload zsh/complist
bindkey -M menuselect '^I' vi-forward-char
bindkey -M menuselect '^[[Z' vi-backward-char
bindkey -M menuselect '^f' accept-and-infer-next-history
bindkey -M menuselect '^h' vi-backward-char
bindkey -M menuselect '^j' vi-down-line-or-history
bindkey -M menuselect '^k' vi-up-line-or-history
bindkey -M menuselect '^l' vi-forward-char
