#
# Application installer (via brew-cask)
#

apps=(
	# We want X11 support
	"xquartz"

	# Common utilities
	"the-unarchiver"
	"asepsis"
	"iterm2"
	"alfred"
	"sizeup-x11"
	"seil"

	# Things you cannot live without
	"virtualbox"
	"popcorn-time"
	"dropbox"
	"spotify"
	"deluge"
	"skype"
	"flux"
	"vlc"

	# Install some browsers
	"google-chrome"
	"firefox"

	# Image software
	"imagealpha"
	"imageoptim"

	# Install quicklook viewers
	"qlcolorcode"
	"qlstephen"
	"qlmarkdown"
	"quicklook-json"
	"qlprettypatch"
	"quicklook-csv"
	"betterzipql"
	"webp-quicklook"
	"suspicious-package"
)

# Fonts
fonts=(
	"font-m-plus"
	"font-clear-sans"
	"font-roboto"
)

# Specify the location of the apps
appdir="/Applications"

# Wait for brew cask support
wget "https://github.com/newmarcel/KeepingYouAwake/releases/download/1.1/KeepingYouAwake-1.1.zip"

main() {
	echo "Installing cask..."
	brew tap caskroom/cask
	brew install brew-cask

	# Tap alternative versions
	brew tap caskroom/versions
	brew tap casidiablo/custom
	brew tap caskroom/fonts

	echo "Installing apps..."
	brew cask install --appdir=$appdir ${apps[@]}

	echo "Installing fonts..."
	brew cask install ${fonts[@]}

	# Link with alfred (it must have been opened once for linking)
	open "$appdir/Alfred.app"
	brew cask alfred link

	# Remove any temporary files
	brew cask cleanup
}

main "$@"
exit 0
