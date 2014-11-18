# Ctrl+space: print Git/Svn status or list files with ls
vcs-status() {
  zle kill-whole-line
  if [ git rev-parse --is-inside-work-tree > /dev/null ^&1 ]; then
    zle -U 'git status --short'
  elif [ -d .svn ]; then
    zle -U 'svn status'
  else
    zle -U 'ls'
  fi

  zle accept-line
}

zle -N vcs-status
bindkey '^ ' vcs-status

# Alt+l: execute 'ls'
bindkey -s '\el' '^Uls^M'

# Alt+.: insert last argument of previous command
bindkey '\e.' insert-last-word
