function publicip -d "Print the public IP"
	set ip (curl ipecho.net/plain ^ /dev/null)
	echo -n $ip

	# Print a newline if not piping
	if test -t 1
		echo ''
	end
end
