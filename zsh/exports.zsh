# Set oh-my-zsh directory
export ZSH="$DOTFILES/zsh/oh-my-zsh"

# Enable 256 colors support
export TERM='xterm-256color'

# Make Vim the default editor
export EDITOR='vim'
export GIT_EDITOR="$EDITOR -f"
export VISUAL="$EDITOR"

# Default to Unicode locale
export LC_ALL='en_GB.UTF-8'
export LANG='en_GB.UTF-8'
export TZ='Europe/Stockholm'

# Less colors for Man Pages
export LESS_TERMCAP_mb=$'\E[0;103m' # begin blinking
export LESS_TERMCAP_md=$'\E[0;93m'  # begin bold
export LESS_TERMCAP_me=$'\E[0m'     # end mode
export LESS_TERMCAP_se=$'\E[0m'     # end standout-mode
export LESS_TERMCAP_so=$(tput bold; tput setaf 8; tput setab 3) # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'     # end underline
export LESS_TERMCAP_us=$'\E[04;32m' # begin underline
export LESS_TERMCAP_mr=$(tput rev)
export LESS_TERMCAP_mh=$(tput dim)
export LESS_TERMCAP_ZN=$(tput ssubm)
export LESS_TERMCAP_ZV=$(tput rsubm)
export LESS_TERMCAP_ZO=$(tput ssupm)
export LESS_TERMCAP_ZW=$(tput rsupm)

# Don’t clear the screen after quitting a manual page
export MANPAGER='less -FRX'
export PAGER='less'

# Color grep output
export GREP_OPTIONS='--color=auto --exclude-dir=.svn --exclude-dir=.git --binary-files=without-match'

# Use human friendly size format (e.g 1024MB)
export BLOCK_SIZE='human-readable'

# Use 'ag' instead of 'find'
export FZF_DEFAULT_COMMAND='ag -l -g ""'

if [ $OS = 'osx' ]; then
        # We want to use the GNU man pages
        export MANPATH="$(brew --prefix coreutils)/libexec/gnuman:$MANPATH"

        # Always install to “/Applications” with brew cask
        export HOMEBREW_CASK_OPTS='--appdir=/Applications'
fi

