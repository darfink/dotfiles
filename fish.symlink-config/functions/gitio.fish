function gitio -d "Shorten a GitHub URL using git.io" -a slug url
	if [ (count $argv) != 2 ]
		echo 'Usage: `gitio <slug> <url>`'
		return 1
	end

	curl -i http://git.io/ -F "url=$url" -F "code=$slug"
end
