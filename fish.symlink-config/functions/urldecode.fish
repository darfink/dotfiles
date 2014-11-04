function urldecode -d "Decode URL entities"
	if [ (count $argv) != 0 ]
		python -c "import sys, urllib as ul; print ul.unquote(sys.argv[1]).decode('utf8');" $argv
	else
		cat | python -c "import sys, urllib as ul; print(ul.unquote(sys.stdin.read()).decode('utf8')),;"
	end
end
