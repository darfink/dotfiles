# Set dotfiles directory
set --export DOTFILES "$HOME/.dotfiles"

# Make Vim the default editor
set --export EDITOR "vim"
set --export GIT_EDITOR "$EDITOR -f"
set --export VISUAL "$EDITOR"

# Default to Unicode locale
set --export LC_ALL en_US.UTF-8
set --export LANG en_US.UTF-8
set --export TZ 'Europe/Stockholm'

# Less colors for Man-pages
# See: http://stealthefish.com/development/2014/04/26/Better-bash-man-pages.html
set -gx LESS_TERMCAP_mb \e'[01;31m'	  # begin blinking
set -gx LESS_TERMCAP_md \e'[01;38;5;74m'  # begin bold
set -gx LESS_TERMCAP_me \e'[0m' 	  # end mode
set -gx LESS_TERMCAP_se \e'[0m' 	  # end standout-mode
set -gx LESS_TERMCAP_so \e'[38;5;246m'	  # begin standout-mode - info box
set -gx LESS_TERMCAP_ue \e'[0m' 	  # end underline
set -gx LESS_TERMCAP_us \e'[04;38;5;146m' # begin underline

# Don’t clear the screen after quitting a manual page
set --export MANPAGER 'less -FRX'
set --export PAGER 'less'

# Color grep output
set --export GREP_OPTIONS '--color=auto --exclude-dir=.svn --exclude-dir=.git --binary-files=without-match'

if [ $OS = 'Darwin' ]
	# Path to the GNU utilities
	set corePath (brew --prefix coreutils)

	# We want to use the GNU utility applications by default
	set --export PATH $corePath/libexec/gnubin $PATH
	set --export MANPATH $corePath/libexec/gnuman $MANPATH

	# Add Homebrew directories to path for easy access
	set --export PATH /usr/local/bin /usr/local/sbin $PATH

	# Always install to /Applications with brew cask
	set --export HOMEBREW_CASK_OPTS "--appdir=/Applications"
end

# Add binary folders to path
set --export PATH $DOTFILES/bin $PATH
set --export PATH $HOME/bin $PATH

# Use 'ag' instead of 'find'
set --export FZF_DEFAULT_COMMAND 'ag -l -g ""'