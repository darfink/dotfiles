function sniff -d "Monitor HTTP traffic"
	sudo ngrep -d 'en1' -t '^(GET|POST) ' 'tcp and port 80' $argv
end
