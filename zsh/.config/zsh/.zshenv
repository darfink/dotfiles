# Ensure path arrays do not contain duplicates.
typeset -gU cdpath fpath mailpath path manpath infopath

###############################################################################
# Vital functions
###############################################################################

# Adds a path if it exists and is valid
add-path() {
  [ -d "$1" ] && path=("$1" $path)
}

# Returns successfully if a command exists
is-command() {
  (( $+commands[$1] ))
}

###############################################################################
# Exports - Generic
###############################################################################

case "$OSTYPE" in
  darwin*) export OS="macos";;
  linux*) export OS="linux";;
esac

# Avoid cluttering home
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-"$HOME/.config"}"

# Avoid files cluttering home
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"

# Use vim as the default editor
export EDITOR='vim'
export GIT_EDITOR="$EDITOR -f"
export VISUAL="$EDITOR"

# Use human friendly size format (e.g 1024MB)
export BLOCK_SIZE='human-readable'

# Ensure emacs info directory is accessible
infopath+='/usr/share/info'

###############################################################################
# Exports - Applications
###############################################################################

# Python - disable virtualenv prefix
(is-command virtualenv) && export VIRTUAL_ENV_DISABLE_PROMPT=1
(is-command rg) && export RIPGREP_CONFIG_PATH="$HOME/.ripgreprc"
(is-command go) && export GOPATH="$HOME/.go"

if is-command sk; then
  export SKIM_DEFAULT_OPTIONS="--bind 'ctrl-k:kill-line,alt-enter:select-all+accept,ctrl-y:execute-silent[echo -n \$(pwd)/{} | pbcopy],ctrl-v:page-down,alt-v:page-up'"
  export SKIM_DEFAULT_COMMAND="fd --type f"
fi

if is-command fzf; then
  export FZF_DEFAULT_OPTS="--bind 'ctrl-k:kill-line,alt-enter:select-all+accept,ctrl-y:execute-silent(echo -n \$(pwd)/{} | pbcopy),ctrl-v:page-down,alt-v:page-up'"
  export FZF_DEFAULT_COMMAND="fd --type f"
fi

if [[ $OS = "macos" ]]; then
  export BROWSER='open'

  if is-command gpg-agent; then
    # Use terminal prompt for GPG in TTY
    export PINENTRY_USER_DATA="USE_CURSES=1"
    export GPG_TTY=$(tty)
  fi

  if is-command brew; then
    infopath+="/usr/local/share/info"

    # Install to system-wide locations when using brew cask
    export HOMEBREW_CASK_OPTS='--appdir=/Applications --qlplugindir=/Library/QuickLook'
    export HOMEBREW_NO_AUTO_UPDATE=1
  fi
fi

###############################################################################
# Exports - Localization
###############################################################################

# Always UTF-8, and prefer Swedish settings but never the Swedish language
unset LC_ALL

# US influenced settings
export LC_MESSAGES='en_US.UTF-8'    # Always US English language
export LC_NUMERIC='en_US.UTF-8'     # Use dots instead of commas
export LC_TIME='en_US.UTF-8'	      # Have to learn ISO-8601

# SE influenced settings
export LC_ADDRESS='sv_SE.UTF-8'     # Address in Swedish order
export LC_COLLATE='sv_SE.UTF-8'     # Sort order ('ö' is after 'z')
export LC_CTYPE='sv_SE.UTF-8'	      # Support Swedish characters
export LC_MEASUREMENT='sv_SE.UTF-8' # Prefer the metric system
export LC_MONETARY='sv_SE.UTF-8'    # SEK is the currency we use
export LC_NAME='sv_SE.UTF-8'	      # Present names in style
export LC_PAPER='sv_SE.UTF-8'	      # Print documents in A4 format
export LC_TELEPHONE='sv_SE.UTF-8'   # Just like yellow pages

# Fallback - just in case
export LANG='en_US.UTF-8'

# Home sweet home
export TZ="${TZ:-"Europe/Stockholm"}"

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

###############################################################################
# Exports - PATH
###############################################################################

add-path "/usr/local/sbin"
add-path "/usr/local/bin"

if [[ $OS = 'macos' ]] && [[ $- == *i* ]]; then
  # Use GNU tools for interactive shells
  manpath+="/usr/local/opt/coreutils/libexec/gnuman"
  add-path "/usr/local/opt/coreutils/libexec/gnubin"
fi

if [[ $OS = "linux" ]]; then
  add-path "/home/linuxbrew/.linuxbrew/bin"
fi

# Cargo executables
add-path "${CARGO_HOME:-"$HOME/.cargo"}/bin"

# .NET executables
add-path "$HOME/.dotnet/tools"

# Ruby executables
if is-command ruby; then
  add-path "$(ruby -e 'puts Gem.user_dir')/bin"
  add-path "/usr/local/opt/ruby/bin"
fi

# Python executables
(is-command python2) && add-path "$(python2 -m site --user-base)/bin"
(is-command python3) && add-path "$(python3 -m site --user-base)/bin"

# Go executables
(is-command go) && add-path "$GOPATH/bin"

# Node version manager executables
export NVM_DIR="$HOME/.config/nvm"
#[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"

# Custom executables
add-path "$HOME/.bin"

# GCP SDK
add-path "$HOME/Tools/google-cloud-sdk/bin"

# Pulumi
add-path "$HOME/.pulumi/bin"
