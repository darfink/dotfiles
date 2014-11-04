function mkd -d "Create a directory and cd into it" -a path
	if [ (count $argv) != 1 ]
		echo 'Usage: `mkd <path>`'
		return 1
	end

	mkdir -p $path; and cd $path
end
