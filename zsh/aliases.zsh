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
alias la='ls -laF'
alias lsd='ls -lF | grep --color=never "^d"'
alias l='ls -lF'

# Safer GNU tools
alias chown='chown --preserve-root'
alias chmod='chmod --preserve-root'
alias chgrp='chgrp --preserve-root'
alias mv='mv -i'

# Utility aliases
alias stats='sort | uniq -c | sort -r'
alias map='xargs -n1'

# Lists the ten most used commands
alias history-stat="history 0 | awk '{print \$2}' | sort | uniq -c | sort -n -r | head"

if is-command grunt; then
  alias grunt='grunt --stack'
fi

# One of @janmoesen’s ProTip™s
for method in GET HEAD POST PUT DELETE TRACE OPTIONS; do
  alias "$method"="lwp-request -m '$method'"
done

###############################################################################
# OS specific
###############################################################################

case "$OS" in
'osx')
  # PlistBuddy alias, because sometimes `defaults` just doesn’t cut it
  alias plistbuddy='/usr/libexec/PlistBuddy'

  # Remove duplicates in "Open With" menu
  alias fixow='/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user'

  # Disable Spotlight
  alias spotoff='sudo mdutil -a -i off'

  # Enable Spotlight
  alias spoton='sudo mdutil -a -i on'
  ;;
'linux')
  # Make the same functions available as OS X
  alias pbpaste='xclip -selection clipboard -o'
  alias pbcopy='xclip -selection clipboard'
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