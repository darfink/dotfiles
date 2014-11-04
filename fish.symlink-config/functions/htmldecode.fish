function htmldecode -d "Decode HTML entities"
	if [ (count $argv) != 0 ]
		echo $argv | recode html..utf8
	else
		cat | recode html..utf8
	end
end
