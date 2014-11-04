function pumpitup -d "Enable the system sound"
	if [ $OS = 'Darwin' ]
		osascript -e 'set volume output muted false'
	else
		amixer -q -D pulse sset Master unmute
	end
end
