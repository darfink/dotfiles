# Alt+s: execute 'git status'
bindkey -s '\es' '^Ugws^M'

# Alt+l: execute 'ls'
bindkey -s '\el' '^Uls^M'

# Alt+.: insert last argument of previous command
bindkey '\e.' insert-last-word

# Ctrl+r: search history
bindkey '^R' history-incremental-search-backward

# Ctrl+Alt+e: expand aliases
zle -N expand-aliases
bindkey '\e^E' expand-aliases
