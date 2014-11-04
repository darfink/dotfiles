function showdesktop -d "Show desktop icons"
	if [ $OS = 'Darwin' ]
		defaults write com.apple.finder CreateDesktop -bool true; and killall Finder
	else
		gsettings set org.gnome.desktop.background show-desktop-icons true
	end
end
