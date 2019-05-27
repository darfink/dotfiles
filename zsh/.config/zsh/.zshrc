# Expose custom prompts for prezto
fpath=("${ZDOTDIR:-$HOME}/.zcustom/contrib" "${fpath[@]}")

# Source Prezto
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

for file in ${ZDOTDIR:-$HOME}/.zcustom/{bindings,functions,aliases,fzf}.zsh; do
  [ -s "$file" ] && source "$file"
done

# Setup auto completions
compdef _gnu_generic \
  df cat tr nl curl mv file head paste tail fzf \
  touch wc shred htop make whois recode compare tac \
  emacsclient emacs rustc fd lsd alacritty

# Load the NodeJS version manager (NVM)
[ -s "$HOME/.nvm/nvm.sh" ] && source "$HOME/.nvm/nvm.sh"

# Load the local zshrc if provided
[ -s "$HOME/Cloud/Configs/zshrc" ] && source "$HOME/Cloud/Configs/zshrc"

# Enable ctrl+s in Vim by disabling flow control and sending of start/stop characters
setopt NO_FLOW_CONTROL

# Enable instant menu completion
setopt menu_complete

# Enable curly brackets (used with git)
unsetopt BRACE_CCL
