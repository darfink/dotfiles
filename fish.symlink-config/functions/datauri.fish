function datauri -d "Create a data URI from a file" -a file
	if [ (count $argv) != 1 ]
		echo 'Usage: `datauri <file>`'
		return 1
	end

	set mimeType (file -b --mime-type $file)

	switch $mimeType
		case 'text/*'
			set mimeType "$mimeType;charset=utf-8"
	end

	echo "data:$mimeType;base64,"(openssl base64 -in $file | tr -d '\n')
end
