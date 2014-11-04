function upload -d "Upload a local file" -a file
	if [ (count $argv) != 1 ]
		echo 'Usage: `upload <file>`'
		return 1
	end

	curl --upload-file "$file" https://transfer.sh/(basename "$file")
end
