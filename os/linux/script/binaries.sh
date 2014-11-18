#
# Binary installer
#

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
	"libgnome-keyring-dev"

	# Tools used for our development
	"silversearcher-ag"
	"exuberant-ctags"
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

	"gnome-shell-extensions-gpaste"
	"ubuntu-restricted-extras"

	# Image optimization software
	"jpegoptim"
	"optipng"

	# NodeJS is always essential
	"nodejs"
	"npm"

	# Bash is old-school
	"autojump"
	"zsh"

	# Handle different compression algorithms
	"p7zip-full"
	"unrar"
	"xz-utils"
	"zopfli"
)

main() {
	# Install binaries
	sudo apt-get install -qy ${binaries[@]}

	if [ is-installed nodejs ]; then
		# Make nodejs accessible as 'node'
		sudo ln -sf /usr/bin/nodejs /usr/bin/node
	fi

	if [ is-installed dnsmasq ]; then
		# Make all *.dev requests go to local host
		sudo mkdir -p /etc/NetworkManager/dnsmasq.d
		echo 'address=/dev/127.0.0.1' | sudo tee /etc/NetworkManager/dnsmasq.d/dev-tld > /dev/null
	fi

	if [ is-installed libgnome-keyring-dev ]; then
		local keydir='/usr/share/doc/git/contrib/credential/gnome-keyring'

		# Install the Gnome keyring and make it easily accessible
		sudo make --directory=$keydir
		sudo ln -sf $keydir/git-credential-gnome-keyring /usr/bin/gnome-keyring
	fi

	if [ ! -d "$HOME/.fzf" ]; then
		git clone https://github.com/junegunn/fzf.git ~/.fzf
		~/.fzf/install
	fi
}

main "$@"
exit 0
