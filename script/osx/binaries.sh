#
# Binary installer
#

echo "Installing binaries..."

binaries=(
	# Install GNU core utilities (those that come with OS X are outdated)
	"coreutils"

	# Install some other useful utilities
	"moreutils"
	"binutils"
	"diffutils"
	"wget --enable-iri"
	"wdiff --with-gettext"
	"autojump"
	"screen"
	"fish"
	"gzip"

	# Install GNU `find`, `locate`, `updatedb`, `xargs` etc
	"findutils --default-names"
	"gnu-indent --default-names"
	"gnu-which --default-names"
	"gnu-tar --default-names"
	"gnu-sed --default-names"
	"gnutls --default-names"
	"grep --default-names"
	"ed --default-names"

	# Image optimization software
	"jpegoptim"
	"optipng"

	# Enable some useful scripting
	"lua --completion"
	"python"
	"ruby"
	"node"

	# Awesomest editor (Vim)
	"cscope"
	"luajit"
	"macvim --with-cscope --with-python --with-luajit --override-system-vim"

	# Tools for development
	"bfg"
	"git"
	"recode"
	"dex2jar"
	"fcrackzip"
	"foremost"
	"dnsmasq"
	"nmap"
	"pngcheck"
	"sqlmap"
	"xz"

	# Commonly used binaries
	"the_silver_searcher"
	"fzf"
	"ssh-copy-id"
	"lynx"
	"p7zip"
	"pigz"
	"pv"
	"xpdf"
	"rename"
	"tree"
	"webkit2png"
	"zopfli"
	"par"

	# OS X fuse systems
	"ext4fuse"
	"sshfs"
)

main() {
	# Install binaries
	brew install ${binaries[@]}

	# Overwrite system python
	brew link --overwrite python

	# Install the CLI lorem ipsum generator
	sudo cpan install Text::Lorem

	if [ is-installed dnsmasq ]; then
		# Make *.dev requests reply with 127.0.0.1
		echo 'address=/.dev/127.0.0.1' > $(brew --prefix)/etc/dnsmasq.conf
		echo 'listen-address=127.0.0.1' >> $(brew --prefix)/etc/dnsmasq.conf

		# Load dnsmasq automatically at startup
		sudo cp $(brew --prefix dnsmasq)/homebrew.mxcl.dnsmasq.plist /Library/LaunchDaemons
		sudo launchctl load -w /Library/LaunchDaemons/homebrew.mxcl.dnsmasq.plist

		# Use our local host for *.dev DNS queries
		sudo mkdir -p /etc/resolver 
		echo 'nameserver 127.0.0.1' | sudo tee /etc/resolver/dev > /dev/null
	fi
}

main "$@"
exit 0
