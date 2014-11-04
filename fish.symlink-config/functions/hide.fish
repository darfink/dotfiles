function hide -d "Disable display of hidden files"
	if [ $OS = 'Darwin' ]
		defaults write com.apple.finder AppleShowAllFiles -bool false; and killall Finder
	else
		gsettings set org.gnome.nautilus.preferences show-hidden-files false
	end
end
