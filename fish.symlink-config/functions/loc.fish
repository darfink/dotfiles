function loc -d "Prints lines of code for extensions"
  set -l total 0

  for ext in $argv
    set -l firstLetter (echo $ext | cut -c1-1)

    if [ firstLetter != "." ]
      set ext ".$ext"
    end

    set -l lines (find-exec "*$ext" cat | wc -l)
    set lines ${lines// /}

    set total ($total + $lines)

    echo "Lines of code for ${fg[blue]}$ext${reset_color}: ${fg[green]}$lines${reset_color}"
  end

  echo "${fg[blue]}Total${reset_color} lines of code: ${fg[green]}$total${reset_color}"
end