# Enable the !! bash functionality to re-execute the last command as root
function sudo -d "Execute as administrator"
	if test "$argv" = !!
	eval command sudo $history[1]
    else
	command sudo $argv
    end
end
