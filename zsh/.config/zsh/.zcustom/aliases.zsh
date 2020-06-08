# Easier navigation
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ~='cd ~'
alias -- -='cd -'

# Shortcuts
alias mkt='cd $(mktemp -d)'
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
alias publicip='echo $(curl -s "https://api6.ipify.org")'
alias privateip='ipconfig getifaddr en0'

# Additional git
alias git='noglob git'
alias gbl='git branch -vv'
alias gff='git pull --ff-only'
alias gmf='git merge --ff-only'
alias gpr='git push origin HEAD:refs/for/master'
alias gpR='git push-remotes'

# Easier copy-paste methods
alias c='pbcopy'
alias ci='pngcopy'
alias copy='pbcopy'
alias copy-img='pngcopy'
alias copy-trim='tr -d "\n" | pbcopy'

alias p='pbpaste'
alias pi='pngpaste'
alias paste='pbpaste'
alias paste-img='pngpaste'
alias paste-trim='pbpaste | tr -d "\n"'

# Open code with vim
alias -s {c,cpp,cs,css,java,js,ts,html,py,zsh,rs,xml,md,zprofile,zshrc,yml,lua}=vim

if is-command bat; then; alias cat='bat'; fi
if is-command gopass; then; alias pass='gopass'; fi
if is-command grunt; then; alias grunt='grunt --stack'; fi
if is-command jq; then; aliasp jql jq -C .; fi
if is-command rg; then; aliasp rgl rg -n --color=always; fi
if is-command tokei; then; alias loc='tokei'; fi
if is-command uni; then; alias ucs='uni search'; fi

if is-command prettier; then
  alias prettier='prettier --single-quote --trailing-comma all'
fi

if is-command tree; then
  aliasp treel tree -C
  alias tre='fd | treel'
fi

if is-command patool; then
  alias pac='patool --verbose create'
  alias pae='patool extract'
  alias pal='patool list'
fi

if is-command lsd; then
  unalias ll lm lk lc lu

  # Different “lsd” aliases
  alias ls='lsd --group-dirs=first'
  alias l='ls -a1'             # Lists in one column, hidden files.
  alias ll='ls -l'             # Lists in long format.
  alias lr='ls -lR'            # Lists directory contents recursively.
  alias lt='ls --tree'         # Lists directory contents in a tree view.
  alias lta='lt -a'            # Lists directory contents in a tree view (including hidden).
  alias la='ls -a'             # Lists all files (including hidden).
  alias lal='ls -la'           # Lists all files in long format (including hidden).
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
    alias pma='brew tap'
    alias pmi='brew install'
    alias pml='brew list'
    alias pmx='brew uninstall'
    alias pmu='brew update'
    alias pmU='brew upgrade'
    alias pmc='brew cleanup'
    alias pms='brew search'
    alias pmS='brew edit'
  fi
  ;;
'linux')
  alias open='xdg-open'

  if is-command apt-get; then
    alias pma='sudo add-apt-repository'
    alias pmi='sudo apt-get install'
    alias pml='sudo apt-get list --installed'
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
