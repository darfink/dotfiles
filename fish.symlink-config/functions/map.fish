function map -d "Pipe output as argument"
	# Intuitive map function
	# For example, to list all directories that contain a certain file:
	# find . -name .gitattributes | map dirname
	xargs -n1 $argv;
end
