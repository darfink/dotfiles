function extract -d "Extract (almost) any file" -a file
	if [ (count $argv) != 1 ]
		echo 'Usage: `extract <file>`'
		return 1
	end

	if [ -f "$file" ]
		switch $file
			case '*.tar*' '*.tbz2' '*.tgz'
				tar -xvf $file
			case '*.bz2'
				bunzip2 $file
			case '*.dmg'
				hdiutil mount $file
			case '*.gz'
				gunzip $file
			case '*.zip'
				unzip $file
			case '*.7z' '*.msi' '*.deb' '*.arj' '*.cab' '*.iso'
				7za x $file
			case '*.pax'
				cat $file | pax -r
			case '*.pax.Z'
				uncompress $file --stdout | pax -r
			case '*.Z'
				uncompress $file
			case '*'
				echo "'$file' cannot be extracted via extract()"
		end
	else
		echo "'$file' is not a valid file"
	end
end
