function httpdump -d "View HTTP activities"
	sudo tcpdump -i en1 -n -s 0 -w - | grep -a -o -E "Host\: .*|GET \/.*" $argv
end
