# Easier navigation
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ~='cd ~'
alias -- -='cd -'
alias www='cd /var/www'

# Shortcuts
alias dl='cd ~/Downloads'
alias dt='cd ~/Desktop'
alias pj='cd ~/Projects'
alias h='history'

# Safer GNU tools
alias chown='chown --preserve-root'
alias chmod='chmod --preserve-root'
alias chgrp='chgrp --preserve-root'

# Override Prezto 'interactive' settings
alias ln='nocorrect ln'
alias cp='nocorrect cp'
alias mv='nocorrect mv'
alias rm='nocorrect rm'

# Utility aliases
alias dateutc='date -u +"%Y-%m-%dT%H:%M:%SZ"'
alias stats='sort | uniq -c | sort -r'
alias map='xargs -n1'
alias week='date +%V'

# Additional git
alias git='noglob git'
alias gbl='git branch -vv'
alias gff='git pull --ff-only'
alias gmf='git merge --ff-only'
alias gpr='git push origin HEAD:refs/for/master'
alias gpR='git push-remotes'

# Easier copy-paste methods
alias c='tr -d "\n" | pbcopy'
alias p='pbpaste'
alias preview="fzf --preview 'if isutf8 {}; then bat --color always {}; else file -b {}; fi'"

if is-command bat; then; alias cat='bat'; fi
if is-command grunt; then; alias grunt='grunt --stack'; fi
if is-command gopass; then; alias pass='gopass'; fi
if is-command jq; then; alias jql='jq -C . | less'; fi

if is-command prettier; then
  alias prettier='prettier --single-quote --trailing-comma all'
fi

if is-command tree; then
  alias treel='tree -C | less'
  alias tre='fd | tree'
fi

if is-command patool; then
  alias pac='patool --verbose create'
  alias pae='patool extract'
  alias pal='patool list'
fi

if is-command exa; then
  unalias ll lm lk lc lu

  # Different “exa” aliases
  alias ls='exa --group-directories-first --git'
  alias l='ls -a1'             # Lists in one column, hidden files.
  alias ll='ls -l'             # Lists in long format.
  alias lr='ls -lR'            # Lists directory contents recursively.
  alias lt='ls -T'             # Lists directory contents in a tree view.
  alias la='ls -a'             # Lists all files (including hidden).
  alias lal='ls -la'           # Lists all files in long format (including hidden).
  alias lx='ls -l -sextension' # Lists sorted by extension.
  alias lsdot='ls -ld .?*'     # Lists only hidden files.
  alias lc='colorls --sd'      # Lists using 'colorls'.
fi

###############################################################################
# OS specific
###############################################################################

case "$OS" in
'osx')
  # PlistBuddy alias, because sometimes `defaults` just doesn’t cut it
  alias plbuddy='/usr/libexec/PlistBuddy'

  # Remove duplicates in "Open With" menu
  alias fixow='/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user'

  # Enable/disable Spotlight
  alias spoton='sudo mdutil -a -i on'
  alias spotoff='sudo mdutil -a -i off'

  # List dynamic libraries
  alias ldd='otool -L'

  if is-command gawk; then
    alias awk='gawk'
  fi

  if is-command brew; then
    alias pmi='brew install'
    alias pmx='brew uninstall'
    alias pmu='brew update'
    alias pmU='brew upgrade'
    alias pmc='brew cleanup'
    alias pms='brew search'
    alias pmS='brew edit'
  fi
  ;;
'linux')
  alias open = 'xdg-open'

  if is-command apt-get; then
    alias pmi='sudo apt-get install'
    alias pmx='sudo apt-get remove'
    alias pmu='sudo apt-get update'
    alias pmU='sudo apt-get install --only-upgrade'
    alias pmc='sudo apt-get clean'
    alias pms='apt-cache search'
    alias pmS='apt-cache show'
  fi
  ;;
'cygwin')
  ;;
esac
