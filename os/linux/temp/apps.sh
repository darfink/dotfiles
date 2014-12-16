#
# Application installer (via apt-get)
#

echo "Installing apps..."

apps=(
	# Things that you cannot live without
	"google-chrome-stable"
	"spotify-client"
	"popcorn-time"
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
	"numix-gtk-theme"

	# Quicklook for Nautilus
	"gloobus-preview"
	"gloobus-sushi"
)

# Install apps
sudo apt-get install -y ${binaries[@]}
exit 0
