function o -d "Opens a directory or file using the default application"
	if [ (count $argv) = 0 ]
		open .
	else
		open $argv
	end
end
