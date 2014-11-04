#!/usr/bin/env bash

# Enable the partners repository
sudo sed -i "/^# deb .*partner/ s/^# //" /etc/apt/sources.list

# Add the Google Chrome repository key
wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | sudo apt-key add -

# Add the Dropbox repository key
sudo apt-key adv --keyserver pgp.mit.edu --recv-keys 5044912E

repositories=(
	"deb http://linux.dropbox.com/ubuntu precise main"
	"deb http://dl.google.com/linux/chrome/deb/ stable main"
	"ppa:deluge-team/ppa"
	"ppa:fish-shell/release-2"
	"ppa:mitya57/ppa"
	"ppa:numix/ppa"
)

# Add application repositories
sudo apt-add-repository -y ${repositories[@]}

# Update apt-get cache
sudo apt-get update
sudo apt-get -y upgrade

# Run each program
bash "$dir/binaries.sh"
bash "$dir/apps.sh"
bash "$dir/defaults.sh"

# Remove outdated versions from the cellar
sudo apt-get -y autoremove
sudo apt-get -y autoclean
