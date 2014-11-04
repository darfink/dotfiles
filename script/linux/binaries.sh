#
# Binary installer
#

set -e

echo "Installing binaries..."

binaries=(
	# Essential packages to build runtimes
	"build-essential"
	"libssl-dev"
	"libxss1"
	"libappindicator1"
	"libindicator7"

	# Quickly access the terminal
	"nautilus-open-terminal"

	# Generate lorem ipsum right from the prompt
	"libtext-lorem-perl"

	# Tools used for our development
	"silversearcher-ag"
	"vim-gnome"
	"recode"
	"sshfs"
	"tree"
	"curl"
	"git"
	"pv"

	# Image optimization software
	"jpegoptim"
	"optipng"

	# NodeJS is always essential
	"nodejs"
	"npm"

	# Bash is old-school
	"autojump"
	"fish"

	# Handle different compression algorithms
	"p7zip-full"
	"xz-utils"
	"zopfli"
)

# Install the binaries
sudo apt-get install -y ${binaries[@]}

# Make nodejs accessible as 'node'
sudo ln -sf /usr/bin/nodejs /usr/bin/node

exit 0
