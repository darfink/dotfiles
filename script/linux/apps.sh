#
# Application installer (via apt-get)
#

set -e

echo "Installing apps..."

apps=(
	# Things that you cannot live without
	"google-chrome-stable"
	"dropbox"
	"deluge"
	"skype"
	"xpdf"
	"vlc"

	# Best application for email notifications
	"unity-mail"

	# Makes everything customizable
	"unity-tweak-tool"

	# Miscellaneous packages
	"virtualbox"
	"lynx-cur"

	# Make it sexy and swell!
	"numix-icon-theme-utouch"
	"numix-wallpaper-notd"
)

# Install apps
sudo apt-get install -y ${binaries[@]}

exit 0
