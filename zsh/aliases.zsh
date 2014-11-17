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
alias g='git'
alias h='history'

# Different “ls” aliases
alias la='ls -laF'
alias lsd='ls -lF | grep --color=never "^d"'
alias l='ls -lF'

# Safer GNU utilities
alias chown='chown --preserve-root'
alias chmod='chmod --preserve-root'
alias chgrp='chgrp --preserve-root'

if [ is-command grunt ]; then
        alias grunt='grunt --stack'
fi

# One of @janmoesen’s ProTip™s
for method in (GET HEAD POST PUT DELETE TRACE OPTIONS); do
        alias "$method"="lwp-request -m '$method'"
done

if [ $OS = 'osx' ]
        # PlistBuddy alias, because sometimes `defaults` just doesn’t cut it
        alias plistbuddy='/usr/libexec/PlistBuddy'

        # Remove duplicates in "Open With" menu
        alias fixopenwith='/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user'

        # Disable Spotlight
        alias spotoff='sudo mdutil -a -i off'

        # Enable Spotlight
        alias spoton='sudo mdutil -a -i on'

        # Change working directory to the top-most Finder window location
        cdf() {
                cd "$(osascript -e 'tell app "Finder" to POSIX path of (insertion location as alias)')"
        }
else
        # Ubuntu does not have easy C/P functions
        alias pbpaste='xclip -selection clipboard -o'
        alias pbcopy='xclip -selection clipboard'
end
