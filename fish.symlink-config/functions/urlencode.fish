function urlencode -d "Encode URL entities"
	if [ (count $argv) != 0 ]
		python -c "import sys, urllib as ul; print ul.quote_plus(sys.argv[1]);" "$argv"
	else
		cat | python -c "import sys, urllib as ul; print ul.quote_plus(sys.stdin.read());"
	end
end
