###############################################################################
# Oh-my-ZSH
###############################################################################

# Use my custom theme
ZSH_THEME='darfink'

# Update oh-my-zsh manually
DISABLE_AUTO_UPDATE='true'

# Enable auto-completion waiting dots
COMPLETION_WAITING_DOTS='true'

# Setup zsh plugins
plugins=(
  autojump
  dirhistory
  vi-mode
  sudo
  git
  git-extras
)

if [ $OS = 'osx' ]; then
  plugins+=(brew osx)
elif [ $OS = 'linux' ]; then
  plugins+=(debian command-not-found)
fi

# This must be last to work properly
plugins+=(zsh-syntax-highlighting)

# Load oh-my-zsh configuration
source "$ZSH/oh-my-zsh.sh"

###############################################################################
# Miscellaneous
###############################################################################

# Make ZSH completion color the same as ls
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# Make zsh know about hosts already accessed by SSH
zstyle -e ':completion:*:(ssh|scp|sftp|rsh|rsync):hosts' hosts 'reply=(${=${${(f)"$(cat {/etc/ssh_,~/.ssh/known_}hosts(|2)(N) /dev/null)"}%%[# ]*}//,/ })'

# Enable ctrl+s in Vim by disabling flow control and sending of start/stop characters
stty -ixon -ixoff

if [ -e "$HOME/.dir_colors" ]; then
        # Make the 'ls' command pretty (solarized dark theme by default)
        eval $(dircolors -c "$HOME/.dir_colors" | sed 's/>&\/dev\/null$//')
fi

