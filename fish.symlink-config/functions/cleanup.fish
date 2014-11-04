function cleanup -d "Remove all .DS_Store files recursively"
	find . -type f -name '*.DS_Store' -ls -delete
end
