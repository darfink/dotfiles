function update -d "Update the entire system"
	if [ $OS = 'Darwin' ]
		# Install OS X updates
		sudo softwareupdate -i -a

		# Update the Homebrew installation
		brew update
		brew upgrade
		brew cleanup
	else
		# Install Ubuntu updates
		sudo apt-get update
		sudo apt-get upgrade
		sudo apt-get dist-upgrade
	end

	# Update node packages
	npm install npm -g
	npm update -g

	# Update ruby gems
	sudo gem update --system
	gem update
end
