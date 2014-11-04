function htmlencode -d "Encode HTML entities"
	if [ (count $argv) != 0 ]
		echo $argv | recode utf8..html
	else
		cat | recode utf8..html
	end
end
