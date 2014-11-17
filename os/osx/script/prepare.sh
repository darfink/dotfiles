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

  echo "Creating 'brew' user group"
  sudo dseditgroup -o create brew
  sudo chgrp -R brew /usr/local
  sudo chmod -R g+w /usr/local
  sudo chgrp -R brew /Library/Caches/Homebrew
  sudo chmod -R g+w /Library/Caches/Homebrew
fi

# Check if the user is part of the 'brew' group
if [ ! dseditgroup -o checkmember -m $USER brew ]; then
  echo "Adding $USER to user group 'brew'"
  sudo dseditgroup -o edit -a $USER -t user brew
fi

# Update homebrew
brew update
brew upgrade

# Run each program
source "$dir/binaries.sh"
source "$dir/apps.sh"
source "$dir/defaults.sh"

# Remove outdated versions from the cellar
brew cleanup
brew prune
