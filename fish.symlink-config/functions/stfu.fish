function stfu -d "Disable the system sound"
	if [ $OS = 'Darwin' ]
		osascript -e 'set volume output muted true'
	else
		amixer -q -D pulse sset Master mute
	end
end
