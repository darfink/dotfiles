# Key bindings
# ------------
if [[ $- == *i* ]]; then

# Ensure precmds are run after cd
__redraw_prompt() {
  local precmd
  for precmd in $precmd_functions; do
    $precmd
  done
  zle reset-prompt
}

__fsel() {
  setopt localoptions pipefail 2> /dev/null
  files=("$(eval "$@" | FZF_DEFAULT_OPTS="--height ${FZF_HEIGHT:-40%} --reverse $FZF_DEFAULT_OPTS" fzf -m --ansi)")
  local ret=$?
  if [[ -n $files ]]; then
    LBUFFER+=$(echo -n "${(fq)files}")
  fi
  zle redisplay
  typeset -f zle-line-init >/dev/null && zle zle-line-init
  return $ret
}

__dsel() {
  setopt localoptions pipefail 2> /dev/null
  local directory=("$(eval "$@" | FZF_DEFAULT_OPTS="--height ${FZF_HEIGHT:-40%} --reverse $FZF_DEFAULT_OPTS" fzf +m --ansi --query=${(Q)LBUFFER})")
  local ret=$?

  if [[ -z "$directory" ]]; then
    zle redisplay
    return 0
  fi

  cd "$directory"
  local ret=$?
  __redraw_prompt
  typeset -f zle-line-init >/dev/null && zle zle-line-init
  return $ret
}

# ALT-t - Paste the selected file path(s) into the command line
fzf-file-widget() { __fsel "fd --color=always" }
zle     -N   fzf-file-widget
bindkey '\et' fzf-file-widget

# ALT-T - Paste the selected file path(s) into the command line (including hidden)
fzf-file-all-widget() { __fsel "fd --color=always -HI" }
zle     -N    fzf-file-all-widget
bindkey '\eT' fzf-file-all-widget

# ALT-J - Select a file using fasd
fzf-jump-file-widget() { __fsel "fasd -lR | \${commands[lscolors]:-cat}" }
zle     -N    fzf-jump-file-widget
bindkey '\eJ' fzf-jump-file-widget

# ALT-c - cd into the selected directory
fzf-cd-widget() { __dsel "fd --color=always --type=d" }
zle     -N    fzf-cd-widget
bindkey '\ec' fzf-cd-widget

# ALT-C - cd into the selected directory (including hidden)
fzf-cd-all-widget() { __dsel "fd --color=always --type=d -HI" }
zle     -N    fzf-cd-all-widget
bindkey '\eC' fzf-cd-all-widget

# ALT-j - Change directory with autojump
fzf-jump-widget() { __dsel "fasd -ldR | \${commands[lscolors]:-cat}" }
zle     -N    fzf-jump-widget
bindkey '\ej' fzf-jump-widget

# CTRL-R - Paste the selected command from history into the command line
fzf-history-widget() {
  local selected num
  setopt localoptions noglobsubst noposixbuiltins pipefail 2> /dev/null
  selected=($(fc -rl 1 |
    FZF_DEFAULT_OPTS="--height ${FZF_HEIGHT:-40%} $FZF_DEFAULT_OPTS -n2..,.. --tiebreak=index --bind=ctrl-r:toggle-sort --expect=ctrl-x --reverse --query=${(qqq)LBUFFER} +m" fzf))
  local ret=$?
  if [ -n "$selected" ]; then
    local accept=0
    if [[ $selected[1] = ctrl-x ]]; then
      accept=1
      shift selected
    fi
    num=$selected[1]
    if [ -n "$num" ]; then
      zle vi-fetch-history -n $num
      [[ $accept = 1 ]] && zle accept-line
    fi
  fi
  zle redisplay
  typeset -f zle-line-init >/dev/null && zle zle-line-init
  return $ret
}

zle     -N   fzf-history-widget
bindkey '^R' fzf-history-widget

fi
