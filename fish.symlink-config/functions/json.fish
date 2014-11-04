function json -d "Pretty-print JSON data"
	if [ (count $argv) != 0 ]
		echo $argv | python -mjson.tool | pygmentize -l javascript
	else
		cat | python -mjson.tool | pygmentize -l javascript
	end
end
