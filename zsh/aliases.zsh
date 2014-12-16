# Easier navigation
alias ...='../..'
alias ....='../../..'
alias .....='../../../..'
alias ~='cd ~'
alias -- -='cd -'
alias www='cd /var/www'

# Shortcuts
alias db='cd ~/Dropbox'
alias dl='cd ~/Downloads'
alias dt='cd ~/Desktop'
alias p='cd ~/Projects'
alias h='history'

# Different “ls” aliases
alias l='ls -lF'
alias la='ls -lAF'
alias lr='ls -tRF'
alias lt='ls -ltF'
alias ll='ls -l'
alias lsd='ls -lF | grep --color=never "^d"'
alias ldot='ls -ld .*'
alias lS='ls -1FSsh'
alias lart='ls -1Fcart'
alias lrt='ls -1Fcrt'

# Safer GNU tools
alias chown='chown --preserve-root'
alias chmod='chmod --preserve-root'
alias chgrp='chgrp --preserve-root'
alias mv='mv -i'
alias cp='cp -i'
alias rm='rm -i'

# Utility aliases
alias stats='sort | uniq -c | sort -r'
alias gitjk='history 10 | tac | gitjk_cmd'
alias map='xargs -n1'

# Lists the ten most used commands
alias history-stat="history 0 | awk '{print \$2}' | sort | uniq -c | sort -nr | head"

if is-command grunt; then
  alias grunt='grunt --stack'
fi

if is-command hub; then
  alias git='hub'
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
  if is-command xclip; then
    alias pbpaste='xclip -selection clipboard -o'
    alias pbcopy='xclip -selection clipboard'
  elif is-command xsel; then
    alias pbpaste='xsel --clipboard --output'
    alias pbcopy='xsel --clipboard --input'
  fi

  alias open='xdg-open'
  ;;
'cygwin')
  # Make all platforms uniformed
  alias pbpaste='putclip'
  alias pbcopy='getclip'
  alias open='cygstart'
  ;;
esac

# Easier copy-paste methods
alias c='tr -d "\n" | pbcopy'
alias p='pbpaste'