function ram -d "Displays memory usage of a process" -a process
  set -l sum 0

  if [ -z $process ]
    echo "Usage: `ram <process>`"
  else
    for i in (ps aux | grep -i "$process" | grep -v "grep" | awk '{print $6}')
      set sum ($i + $sum)
    end

    set sum (echo "scale=2; $sum / 1024.0" | bc)

    if [ $sum != 0 ]
      echo "${fg[blue]}$process${reset_color} uses ${fg[green]}${sum}${reset_color} MBs of RAM."
    else
      echo "There are no processes with pattern '${fg[blue]}${process}${reset_color}' running."
    end
  end
end