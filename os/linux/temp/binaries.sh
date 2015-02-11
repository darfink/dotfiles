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
	"tmux"
	"sshfs"
	"xclip"
	"tree"
	"curl"
	"make"
	"htop"
	"git"
	"par"
	"pv"
	"plutil"
	"openssh-server"
	"source-highlight"
	"shellcheck"
  "imagemagick"

  # Encoding detector
  "libuchardet-dev"
  "cmatrix"

	"gnome-shell-extensions-gpaste"
	"ubuntu-restricted-extras"
	"moreutils"
	# Image optimization software
	"jpegoptim"
	"optipng"

	# Bash is old-school
	"autojump"
	"zsh"

  # Exfat support
  "exfat-utils"
  "exfat-fuse"

	# Handle different compression algorithms
	"p7zip-full"
	"rar"
	"unrar"
	"xz-utils"
	"zopfli"
)

main() {
	# Install binaries
	sudo apt-get install -qy "${binaries[@]}"

	if is-installed dnsmasq; then
		# Make all *.dev requests go to local host
		sudo mkdir -p /etc/NetworkManager/dnsmasq.d
		echo 'address=/dev/127.0.0.1' | sudo tee /etc/NetworkManager/dnsmasq.d/dev-tld > /dev/null
	fi

	if is-installed libgnome-keyring-dev; then
		local keydir='/usr/share/doc/git/contrib/credential/gnome-keyring'

		# Install the Gnome keyring and make it git default
		sudo make --directory=$keydir
		git config -f ~/.gitconfig.local credential.helper "$keydir/git-credential-gnome-keyring"
	fi

	if [ ! -d "$HOME/.fzf" ]; then
		git clone https://github.com/junegunn/fzf.git ~/.fzf
		~/.fzf/install
	fi
}

main "$@"
exit 0
