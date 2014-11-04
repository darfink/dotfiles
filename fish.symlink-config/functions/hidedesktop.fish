function hidedesktop -d "Hide desktop icons"
	if [ $OS = 'Darwin' ]
		defaults write com.apple.finder CreateDesktop -bool false; and killall Finder
	else
		gsettings set org.gnome.desktop.background show-desktop-icons false
	end
end
