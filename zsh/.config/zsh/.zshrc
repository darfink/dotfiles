# Expose custom prompts for prezto
fpath=("$ZDOTDIR/.zcustom/contrib" "${fpath[@]}")

# Source Prezto
[[ -s "$ZDOTDIR/.zprezto/init.zsh" ]] && source "$ZDOTDIR/.zprezto/init.zsh"

# Source all custom configurations
for file in $ZDOTDIR/.zcustom/{functions,aliases,widgets,bindings}.zsh; do
  [ -s "$file" ] && source "$file"
done

# Source asdf
[ -s "$HOME/.asdf/asdf.sh" ] && source "$HOME/.asdf/asdf.sh"

# Setup auto completions
compdef _gnu_generic \
  df cat tr nl curl mv file head paste tail fzf \
  touch wc shred htop make whois recode compare tac \
  emacsclient emacs rustc fd lsd alacritty

# Load the local zshrc if provided
[ -s "$HOME/Cloud/Configs/zshrc" ] && source "$HOME/Cloud/Configs/zshrc"

# Enable ctrl+s in Vim by disabling flow control and sending of start/stop characters
setopt NO_FLOW_CONTROL

# Enable instant menu completion
setopt menu_complete

# Enable curly brackets (used with git)
unsetopt BRACE_CCL
