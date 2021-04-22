# This is only for the lulz
compute() {
  while true; do head -n 100 /dev/urandom; sleep 0.1; done | hexdump -C | grep "ca fe"
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


# Browse Chrome history
function ch() {
  local cols sep
  cols=$(( COLUMNS / 3 ))
  sep='{::}'

  cp -f ~/Library/Application\ Support/Google/Chrome/Default/History /tmp/h

  sqlite3 -separator $sep /tmp/h \
    "select substr(title, 1, $cols), url
     from urls order by last_visit_time desc" |
  awk -F $sep '{printf "%-'$cols's  \x1b[36m%s\x1b[m\n", $1, $2}' |
  sk --ansi --multi | sed 's#.*\(https*://\)#\1#' | xargs open
}
