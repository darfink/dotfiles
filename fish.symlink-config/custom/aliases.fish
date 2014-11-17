# Easier navigation
alias ...="../.."
alias ....="../../.."
alias .....="../../../.."
alias ~="cd ~" # `cd` is probably faster to type though
alias -- -="cd -"
alias www="cd /var/www"

# Shortcuts
alias db="cd ~/Dropbox"
alias dl="cd ~/Downloads"
alias dt="cd ~/Desktop"
alias p="cd ~/Projects"
alias g="git"
alias h="history"

# Different 'ls' aliases
alias la="ls -laF"
alias lsd="ls -lF | grep --color=never '^d'"
alias l="ls -lF"

# Safer GNU utilities
alias chown="chown --preserve-root"
alias chmod="chmod --preserve-root"
alias chgrp="chgrp --preserve-root"

if type grunt > /dev/null
	alias grunt="grunt --stack"
end

if type npm > /dev/null
	alias npme="npm --registry http://registry.npmjs.eu/"
end

# One of @janmoesen’s ProTip™s
for method in GET HEAD POST PUT DELETE TRACE OPTIONS
	alias "$method"="lwp-request -m '$method'"
end

if [ $OS = 'osx' ]
	# PlistBuddy alias, because sometimes `defaults` just doesn’t cut it
	alias plistbuddy="/usr/libexec/PlistBuddy"

	# Remove duplicates in "Open With"
	alias fixopenwith="/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user"

	# Disable Spotlight
	alias spotoff="sudo mdutil -a -i off"

	# Enable Spotlight
	alias spoton="sudo mdutil -a -i on"

	# Change working directory to the top-most Finder window location
	function cdf
		cd (osascript -e 'tell app "Finder" to POSIX path of (insertion location as alias)')
	end
else
	# Ubuntu does not have easy C/P functions
	alias pbpaste='xclip -selection clipboard -o'
	alias pbcopy='xclip -selection clipboard'
end
