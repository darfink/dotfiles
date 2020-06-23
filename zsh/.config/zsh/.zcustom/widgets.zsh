###############################################################################
# Utilities
###############################################################################

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
  local cword="${LBUFFER/* /}${RBUFFER/ */}"
  files=("$(eval "$@" | SKIM_DEFAULT_OPTIONS="--height ${SKIM_HEIGHT:-40%} --reverse $SKIM_DEFAULT_OPTIONS" sk --tiebreak=score,index -m --ansi --query ${cword:-""})")
  local ret=$?
  if [[ -n $files ]]; then
    if [[ -n $cword ]]; then
      LBUFFER=$(echo -n "${LBUFFER:0:-${#cword}}")
    fi

    LBUFFER+=$(echo -n "${(fq)files}")
  fi
  zle redisplay
  typeset -f zle-line-init >/dev/null && zle zle-line-init
  return $ret
}

__dsel() {
  setopt localoptions pipefail 2> /dev/null
  local cword="${LBUFFER/* /}${RBUFFER/ */}"
  local directory=("$(eval "$@" | SKIM_DEFAULT_OPTIONS="--height ${SKIM_HEIGHT:-40%} --reverse $SKIM_DEFAULT_OPTIONS" sk --tiebreak=score,index --no-multi --ansi --query=${cword:-""})")
  local ret=$?

  if [[ -z "$directory" ]]; then
    zle redisplay
    return 0
  fi

  if [[ -n $cword ]]; then
    LBUFFER=$(echo -n "${LBUFFER:0:-${#cword}}")
  fi

  cd "$directory"
  local ret=$?
  __redraw_prompt
  typeset -f zle-line-init >/dev/null && zle zle-line-init
  return $ret
}

###############################################################################
# Widgets
###############################################################################

# Paste the selected file path(s) into the command line
fzf-file-widget() { __fsel "fd --color=always" }
zle -N fzf-file-widget

# Paste the selected file path(s) into the command line (including hidden)
fzf-file-all-widget() { __fsel "fd --color=always -HI" }
zle -N fzf-file-all-widget

# Select a file using fasd
fzf-jump-file-widget() { __fsel "fasd -lfR | \${commands[lscolors]:-cat}" }
zle -N fzf-jump-file-widget

# Change to the selected directory
fzf-cd-widget() { __dsel "fd --color=always --type=d" }
zle -N fzf-cd-widget

# Change to the selected directory (including hidden)
fzf-cd-all-widget() { __dsel "fd --color=always --type=d -HI" }
zle -N fzf-cd-all-widget

# Change directory with autojump
fzf-jump-widget() { __dsel "fasd -ldR | \${commands[lscolors]:-cat}" }
zle -N fzf-jump-widget

# Paste the selected command from history into the command line
fzf-history-widget() {
  local selected num
  setopt localoptions noglobsubst noposixbuiltins pipefail 2> /dev/null
  selected=($(fc -rl 1 |
    SKIM_DEFAULT_OPTIONS="--height ${SKIM_HEIGHT:-40%} $SKIM_DEFAULT_OPTIONS -n2..,.. --tiebreak=index --bind=ctrl-r:toggle-sort --expect=ctrl-x --reverse --query=${(qqq)LBUFFER} --no-multi" sk))
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
zle -N fzf-history-widget

# Remove whole arguments
shell-backward-kill-word() {
	emulate -L zsh
	local words word spaces
	words=(${(z)LBUFFER})
	word=$words[-1]
	if [[ -n ${word[2,-2]//[^\/]} ]]; then
		word=${word##*/?}o
	fi
	spaces=-1
	while [[ $LBUFFER[$spaces] == " " ]]; do
			(( spaces-- ))
	done
	LBUFFER=$LBUFFER[0,$((-$#word + $spaces))]
}
zle -N shell-backward-kill-word

# Expands an alias recursively
expand-aliases() {
  unset 'functions[_expand-aliases]'
  functions[_expand-aliases]=$BUFFER
  (($+functions[_expand-aliases])) &&
    BUFFER=${functions[_expand-aliases]#$'\t'} &&
    CURSOR=$#BUFFER
}
zle -N expand-aliases
