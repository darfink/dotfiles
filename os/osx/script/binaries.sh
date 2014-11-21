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
	"wget --with-iri"
	"wdiff --with-gettext"
	"autojump"
	"screen"
	"tmux"
	"reattach-to-user-namespace --wrap-pbcopy-and-pbpaste"
	"duti"
	"zsh"
	"gzip"
	"lesspipe"

	# Install GNU `find`, `locate`, `updatedb`, `xargs` etc
	"findutils --with-default-names"
	"gnu-indent --with-default-names"
	"gnu-which --with-default-names"
	"gnu-tar --with-default-names"
	"gnu-sed --with-default-names"
	"gnutls --with-default-names"
	"grep --with-default-names"
	"ed --with-default-names"

	# Image optimization software
	"jpegoptim"
	"optipng"

	# Enable some useful scripting
	"lua --with-completion"
	"python"
	"ruby"

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

	# MAMP setup
	"mysql"
	"httpd24 --with-brewed-openssl"
	"php56 --with-homebrew-openssl --homebrew-apxs --with-apache"
	"php56-opcache"

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
  # Enable additional binaries
  brew tap homebrew/dupes
  brew tap homebrew/apache
  brew tap homebrew/php

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

  if [ is-installed mysql ]; then
    # Use the default MySQL configuration
    cp -f "$(brew --prefix mysql)/support-files/my-default.cnf" "$(brew --prefix mysql)/my.cnf"

    # Load MySQL at system startup
    sudo ln -sf "$(brew --prefix mysql)/homebrew.mxcl.mysql.plist" /Library/LaunchDaemons/
    sudo launchctl load -w /Library/LaunchDeamons/homebrew.mxcl.mysql.plist
  fi

  if [ is-installed php56 ]; then
    # Fix a permission problem with pearl and pecl
    touch "$(brew --prefix php56)/lib/php/.lock" && chmod 0644 "$(brew --prefix php56)/lib/php/.lock"
  fi

  if [ is-installed httpd24 ]; then
    # Disable the default httpd startup process (the one that's bundled with OS X)
    sudo launchctl unload -w /System/Library/LaunchDaemons/org.apache.httpd.plist 2>/dev/null

    # Enable the virtual hosts configuration
    sed -i 's,#\(.*httpd-vhosts.conf\),\1,g' "$(brew --prefix)/etc/apache2/2.4/httpd.conf"
    cp -f ext/httpd-vhosts.conf "$(brew --prefix)/etc/apache2/2.4/extra/"

    # Setup the HTTP port forwarding (80 -> 8080)
    sudo cp -f "os/$OS/ext/co.echo.httpdfwd.plist" /Library/LaunchDaemons/
    sudo launchctl load -w /Library/LaunchDaemons/co.echo.httpdfwd.plist

    if [ is-installed php56 ]; then
      cat >> $(brew --prefix)/etc/apache2/2.4/httpd.conf <<EOF
      # Send PHP extensions to mod_php
      AddHandler php5-script .php
      AddType text/html .php
      DirectoryIndex index.php index.html
      EOF
    fi

    # Load Apache at system startup
    sudo ln -sf "$(brew --prefix httpd24)/homebrew.mxcl.httpd24.plist" /Library/LaunchDaemons/
    sudo launchctl load -w /Library/LaunchDaemons/homebrew.mxcl.httpd24.plist
  fi
}

main "$@"
exit 0
