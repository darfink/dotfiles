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
	"dnsmasq"
	"recode"
	"sshfs"
	"xclip"
	"tree"
	"curl"
	"make"
	"git"
	"par"
	"pv"

	# Image optimization software
	"jpegoptim"
	"optipng"

	# NodeJS is always essential
	"nodejs"
	"npm"

	# Bash is old-school
	"autojump"
	"dconf-cli"
	"fish"

	# Handle different compression algorithms
	"p7zip-full"
	"xz-utils"
	"zopfli"
)

main() {
	# Install binaries
	sudo apt-get install -y ${binaries[@]}

	# Make nodejs accessible as 'node'
	sudo ln -sf /usr/bin/nodejs /usr/bin/node

	if [ is-installed dnsmasq ]; then
		# Make all *.dev requests go to local host
		sudo mkdir -p /etc/NetworkManager/dnsmasq.d
		echo 'address=/dev/127.0.0.1' | sudo tee /etc/NetworkManager/dnsmasq.d/dev-tld > /dev/null
	fi

	if [ ! -d "$HOME/.fzf" ]; then
		git clone https://github.com/junegunn/fzf.git ~/.fzf
		~/.fzf/install
	fi
}

main "$@"
exit 0
