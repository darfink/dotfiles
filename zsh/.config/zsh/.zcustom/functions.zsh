# This is only for the lulz
compute() {
  while true; do head -n 100 /dev/urandom; sleep 0.1; done | hexdump -C | grep "ca fe"
}

if is-command fzf; then
  fzv() {
    file=$(fzf --preview 'bat --color "always" {}' $@)
    if (( $? == 0)); then vim "$file"; fi
  }
fi

# Count code lines in some directory.
# $ loc py js css
# # => Lines of code for .py: 3781
# # => Lines of code for .js: 3354
# # => Lines of code for .css: 2970
# # => Total lines of code: 10105
loc() {
  local total=0
  local lines

  for ext in $@; do
    local firstletter=$(echo $ext | cut -c1-1)

    if [[ firstletter != "." ]]; then
      ext=".$ext"
    fi

    lines=$(find-exec "*$ext" cat | wc -l)
    lines=${lines// /}

    total=$(($total + $lines))
    echo "Lines of code for $FG[blue]$ext$FG[none]: $FG[green]$lines$FG[none]"
  done

  echo "$FG[blue]Total$FG[none] lines of code: $FG[green]$total$FG[none]"
}

# Create a directory and change into it
mkd() {
  mkdir -p "$@" && cd "$_"
}

# Show how much RAM application uses.
# $ ram safari
# # => safari uses 154.69 MBs of RAM
ram() {
  local sum=0
  local app="$1"

  if [ -z "$app" ]; then
    echo 'Usage: `ram <pattern>`'
    return 1
  fi

  for i in $(ps aux | grep -i "$app" | grep -v "grep" | awk '{print $6}'); do
    sum=$(($i + $sum))
  done

  sum=$(echo "scale=2; $sum / 1024.0" | bc)

  if [[ $sum != "0" ]]; then
    echo "$FG[blue]${app}$FG[none] uses $FG[green]${sum}$FG[none] MBs of RAM"
  else
    echo "There are no processes with pattern '$FG[blue]${app}$FG[none]' running"
  fi
}

# Converts a text to an image
text2img() {
  echo -n "$@" | convert -size 400x -background black -gravity center -border 30 -bordercolor "#000" -pointsize 50 -font Arial -fill red caption:@- png:- | impbcopy -
}

# Creates a new tmux session
tnew() {
  tmux new-session -As "$(basename "$PWD")"
}

# Removes silent audio
trimaudio() {
  ext="${1##*.}"
  sox "$1" "temp.$ext" silence 1 0.1 0.1% reverse silence 1 0.1 0.1% reverse
  mv "temp.$ext" "$1"
}

# Macro for pager aliases
aliasp() {
  local name="$1"
  shift
  eval ""$name"() {
    $@ \$@ | $PAGER -F
  }"
  compdef $name=$1
}

# Expands an alias recursively
expand-aliases() {
  unset 'functions[_expand-aliases]'
  functions[_expand-aliases]=$BUFFER
  (($+functions[_expand-aliases])) &&
    BUFFER=${functions[_expand-aliases]#$'\t'} &&
    CURSOR=$#BUFFER
}
