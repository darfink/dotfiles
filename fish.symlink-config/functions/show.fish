function show -d "Enable display of hidden files"
	if [ $OS = 'Darwin' ]
		defaults write com.apple.finder AppleShowAllFiles -bool true; and killall Finder
	else
		gsettings set org.gnome.nautilus.preferences show-hidden-files true
	end
end
