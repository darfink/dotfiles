###############################################################################
# Vital functions
###############################################################################

# Returns the operating system
os() {
  uname="$(uname -s)"

  if [[ "$uname" == "Darwin" ]]; then
    echo "osx"
  elif [[ "$(expr substr "$uname" 1 5)" == "Linux" ]]; then
    echo "linux"
  elif [[ "$(expr substr "$uname" 1 10)" == "MINGW32_NT" ]]; then
    echo "cygwin"
  else
    exit 1
  fi
}

# Adds a path if it exists and is valid
add-path() {
  if [ -d "$1" ] && [[ ":$PATH:" != *":$1:"* ]]; then
    PATH="$1:$PATH"
  fi
}

# Returns successfully if a command exists
is-command() {
  if (( $+commands[$1] )); then
    return 0
  else
    return 1
  fi
}

###############################################################################
# Paths and initial setup
###############################################################################

# Ensure path arrays do not contain duplicates.
typeset -gU cdpath fpath mailpath path

# All of our executables
add-path "$HOME/.bin"

# Cache the current OS
export OS="$(os)"

if [ "$OS" = 'osx' ]; then
  # Add Homebrew directories
  add-path "/usr/local/sbin"
  add-path "/usr/local/bin"
  add-path "/usr/local/opt/gpg-agent/bin"

  if is-command brew; then
    export HOMEBREW_NO_AUTO_UPDATE=1

    # Add some brew specific paths
    add-path "/usr/local/opt/coreutils/libexec/gnubin"
    add-path "/usr/local/opt/ruby/bin"
  fi

  if test -e "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/"; then
    export DYLD_LIBRARY_PATH="/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/"
  fi
fi

if is-command ruby && is-command gem; then
  # Add local gem installations to path
  add-path "$(ruby -rubygems -e 'puts Gem.user_dir')/bin"
fi

if is-command python; then
  add-path "$(python -m site --user-base)/bin"
fi

if is-command python3; then
  add-path "$(python3 -m site --user-base)/bin"
fi

if is-command go; then
  export GOPATH="$HOME/.go"
  add-path "$GOPATH/bin"
fi

if is-command dotnet; then
  add-path "$HOME/.dotnet/tools"
fi

if [ -n "$CARGO_HOME" ]; then
  add-path "$CARGO_HOME/bin"
else
  add-path "$HOME/.cargo/bin"
fi

if is-command rustup && is-command racer; then
  local toolchain="${$(rustup show | rg default | head -1)%% *}"
  export RUST_SRC_PATH="$HOME/.rustup/toolchains/$toolchain/lib/rustlib/src/rust/src"
fi

if is-command rg; then
  export RIPGREP_CONFIG_PATH="$HOME/.ripgreprc"
fi

if is-command fzf; then
  export FZF_DEFAULT_OPTS="--bind 'ctrl-k:kill-line,alt-enter:select-all+accept,ctrl-y:execute-silent(echo -n \$(pwd)/{} | pbcopy),ctrl-v:page-down,alt-v:page-up'"

  if is-command fd; then
    export FZF_DEFAULT_COMMAND="fd --type f"
  fi
fi

###############################################################################
# Exports
###############################################################################

# Make the home configuration directory available
[[ -z $XDG_CONFIG_HOME ]] && export XDG_CONFIG_HOME="$HOME/.config"

# Make the emacs info directory available
[[ -z $INFOPATH ]] && export INFOPATH='/usr/share/info'

# Make Vim the default editor
export EDITOR='vim'
export GIT_EDITOR="$EDITOR -f"
export VISUAL="$EDITOR"

# Disable virtualenv prefix
export VIRTUAL_ENV_DISABLE_PROMPT=1

# Use human friendly size format (e.g 1024MB)
export BLOCK_SIZE='human-readable'

if [ $OS = 'osx' ]; then
  if is-command gpg-agent; then
    # Use terminal prompt for GPG in TTY
    export PINENTRY_USER_DATA="USE_CURSES=1"
    export GPG_TTY=$(tty)
  fi

  if is-command brew; then
    # We want to use the GNU man & info pages
    export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:$MANPATH"
    export INFOPATH="/usr/local/share/info:$INFOPATH"
  fi

  # Always install to system-wide locations with brew cask
  export HOMEBREW_CASK_OPTS='--appdir=/Applications --qlplugindir=/Library/QuickLook'
fi

if [ -s "$HOME/perl5" ]; then
  # Add local cpan installations to path
  eval "$(perl -I "$HOME/perl5/lib/perl5" -Mlocal::lib)"

  # ... and their man page information
  export MANPATH="$HOME/perl5/man:$MANPATH"
fi

###############################################################################
# Exports - Localization
###############################################################################

# Avoid this superseding other settings
unset LC_ALL

# Always UTF-8, and prefer Swedish settings but never the Swedish language

# US influenced settings
export LC_MESSAGES='en_US.UTF-8'    # Always US English language
export LC_NUMERIC='en_US.UTF-8'     # Use dots instead of commas
export LC_TIME='en_US.UTF-8'	    # Have to learn ISO-8601

# SE influenced settings
export LC_ADDRESS='sv_SE.UTF-8'     # Address in Swedish order
export LC_COLLATE='sv_SE.UTF-8'     # Sort order ('ö' is after 'z')
export LC_CTYPE='sv_SE.UTF-8'	    # Support Swedish characters
export LC_MEASUREMENT='sv_SE.UTF-8' # Prefer the metric system
export LC_MONETARY='sv_SE.UTF-8'    # SEK is the currency we use
export LC_NAME='sv_SE.UTF-8'	    # Present names with style
export LC_PAPER='sv_SE.UTF-8'	    # Print documents in A4 format
export LC_TELEPHONE='sv_SE.UTF-8'   # Just like yellow pages

# Fallback - just in case
export LANG='en_US.UTF-8'

if [ -z "${TZ}" ]; then
  # Where I reside for the time being
  export TZ='Europe/Stockholm'
fi

###############################################################################
# Exports - Less
###############################################################################

# Mouse scrolling is disabled in favor of not using screen clearing if the
# content that is being viewed fits in the current terminal screen.
export LESS='-F -g -i -M -R -w -X -z-4'

# Set the Less input preprocessor
if is-command src-hilite-lesspipe.sh; then
  export LESSOPEN='| src-hilite-lesspipe.sh %s'
elif is-command lesspipe.sh; then
  export LESSOPEN='| lesspipe.sh %s 2>&-'
fi

# Prefer UTF-8 encoding
export LESSCHARSET='utf-8'

# Less colors for Man pages
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

################################################################################
# Temporary files
################################################################################

if [[ ! -d "$TMPDIR" ]]; then
  export TMPDIR="/tmp/$USER"
  mkdir -p -m 700 "$TMPDIR"
fi

TMPPREFIX="${TMPDIR%/}/zsh"
if [[ ! -d "$TMPPREFIX" ]]; then
  mkdir -p "$TMPPREFIX"
fi
