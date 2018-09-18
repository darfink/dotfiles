# Alt+s: execut 'git status'
bindkey -s '\es' '^Ugws^M'

# Alt+l: execute 'ls'
bindkey -s '\el' '^Uls^M'

# Alt+.: insert last argument of previous command
bindkey '\e.' insert-last-word

# Mimic the bash behavior
bindkey '^R' history-incremental-search-backward
