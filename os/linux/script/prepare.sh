# Disable any interrupting user prompts
export DEBIAN_FRONTEND=noninteractive

# Enable the partner & multiverse repositories
sudo sed -i "/^# deb .*partner/ s/^# //" /etc/apt/sources.list
sudo sed -i "/^# deb .*multiverse/ s/^# //" /etc/apt/sources.list

# Add the Google Chrome repository key
wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | sudo apt-key add -

# Add the Dropbox & Spotify repository keys
sudo apt-key adv --keyserver pgp.mit.edu --recv-keys 5044912E
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 94558F59

repositories=(
	"deb http://linux.dropbox.com/ubuntu precise main"
	"deb http://dl.google.com/linux/chrome/deb/ stable main"
	"deb http://repository.spotify.com stable non-free"
	"ppa:gloobus-dev/gloobus-preview"
	"ppa:webupd8team/popcorntime"
	"ppa:webupd8team/gnome3"
	"ppa:deluge-team/ppa"
	"ppa:mitya57/ppa"
	"ppa:numix/ppa"
	"ppa:pi-rho/dev"
)

# Add application repositories
sudo apt-add-repository -y ${repositories[@]}

# Update apt-get cache
sudo apt-get update
sudo apt-get -y upgrade

# Run each program
source "$dir/binaries.sh"
source "$dir/apps.sh"
source "$dir/defaults.sh"

# Remove outdated versions from the cellar
sudo apt-get -y autoremove
sudo apt-get -y autoclean
