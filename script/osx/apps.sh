#
# Application installer (via brew-cask)
#

set -e

apps=(
	# We want X11 support
	"xquartz"

	# Common utilities
	"the-unarchiver"
	"asepsis"
	"iterm2"
	"alfred"
	"sizeup"
	"seil"
	"dash"

	# Things you cannot live without
	"virtualbox"
	"dropbox"
	"utorrent"
	"caffeine"
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

main() {
	echo "Installing cask..."

	brew tap caskroom/cask
	brew install brew-cask

	# Tap alternative versions
	brew tap caskroom/versions

	# Tap the fonts
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
