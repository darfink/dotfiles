function e -d "Opens a directory or file using the default editor"
	if [ (count $argv) = 0 ]
		$EDITOR .
	else
		$EDITOR $argv
	end
end
