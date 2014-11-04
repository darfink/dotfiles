function emptytrash
	if [ $OS = 'Darwin' ]
		sudo rm -rfv /Volumes/*/.Trashes
		sudo rm -rfv ~/.Trash
		sudo rm -rfv /private/var/log/asl/*.asl
	else
		rm -rf ~/.local/share/Trash/*
	end
end
