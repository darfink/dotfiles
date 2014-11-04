#!/usr/bin/env bash

set -e

# Ask for password up-front
sudo -v

# Check for command line tools
xcode-select -p
if [[ $? -ne 0 ]]; then
  echo "Installing command line tools..."
  xcode-select --install
fi

# Check for XCode installation
xcodebuild -version
if [[ $? -ne 0 ]]; then
    # TODO: find a way to install Xcode.app automatically
    # See: http://stackoverflow.com/a/18244349

    # Accept Xcode license
    sudo xcodebuild -license
fi

# Update all OSX packages
sudo softwareupdate -i -a

# Check for Homebrew
if test ! $(which brew); then
  echo "Installing homebrew..."
  ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

# Update homebrew
brew update
brew upgrade

# Run each program
bash "$dir/binaries.sh"
bash "$dir/apps.sh"
bash "$dir/defaults.sh"

# Remove outdated versions from the cellar
brew cleanup
brew prune
