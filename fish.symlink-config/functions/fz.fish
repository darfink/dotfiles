function fz -d "Print the size of a file or directory"
	set arg '-sbh'

	if [ (count $argv) > 0 ]
		du $arg -- $argv
	else
		du $arg .[^.]* *;
	end
end
