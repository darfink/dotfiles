# Easier navigation
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ~='cd ~'
alias -- -='cd -'
alias www='cd /var/www'

# Shortcuts
alias db='cd ~/Dropbox'
alias dl='cd ~/Downloads'
alias dt='cd ~/Desktop'
alias pj='cd ~/Projects'
alias h='history'

# Different “ls” aliases
alias l='ls -lF'
alias la='ls -lAF'
alias lr='ls -tRF'
alias lt='ls -ltF'
alias lsd='ls -lF | grep --color=never "^d"'
alias ldot='ls -ld .*'
alias lS='ls -1FSsh'
alias lart='ls -1Fcart'
alias lrt='ls -1Fcrt'

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
alias gitjk='history 10 | tac | gitjk_cmd'
alias gpR='git push-all'
alias map='xargs -n1'

if is-command grunt; then
  alias grunt='grunt --stack'
fi

if is-command tree; then
  alias treel='tree -C | less'
fi

if is-command hub; then
  alias git='noglob hub'
elif is-command git; then
  alias git='noglob git'
fi

if is-command lwp-request; then
  # One of @janmoesen’s ProTip™s
  for method in GET HEAD POST PUT DELETE TRACE OPTIONS; do
    alias "$method"="lwp-request -m '$method'"
  done
fi

if is-command patool; then
  alias pac='patool --verbose create'
  alias pae='patool extract'
  alias pal='patool list'
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

  # Disable Spotlight
  alias spotoff='sudo mdutil -a -i off'

  # Enable Spotlight
  alias spoton='sudo mdutil -a -i on'

  if is-command gawk; then
    alias awk='gawk'
  fi
  ;;
'linux')
  alias open='o'
  ;;
'cygwin')
  # Make all platforms uniformed
  alias open='o'
  ;;
esac

# Easier copy-paste methods
alias c='tr -d "\n" | pbcopy'
alias p='pbpaste'
