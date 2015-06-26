#!/usr/bin/env bash

directory=''

info() {
  printf "  [ \033[00;34m..\033[0m ] %s\n" "$1"
}

setting() {
  printf "  [ \033[00;32m⚙\033[0m ] %s\n" "$1"
}

user() {
  printf "\r  [ \033[0;33m?\033[0m ] %s " "$1"
}

fail() {
  printf "\r\033[2K  [\033[0;31mFAIL\033[0m] %s\n" "$1"
}

ask() {
  user "$1"
  printf "[y/N] "
  read -r -n 1 -s response
  printf "\n"

  if [[ $response =~ ^([yY][eE][sS]|[yY])$ ]]; then
    return 0
  else
    return 1
  fi
}

is-command() {
  hash "$1" 2> /dev/null
}

setup_defaults() {
  if ! ask "configure defaults?"; then
    return 1
  fi

  info '[System settings]'
  if ask 'disable sound effects on boot?'; then
    sudo nvram SystemAudioVolume=0
  fi

  if sudo spctl --status | grep 'enabled' > /dev/null && ask 'disable OS X gate keeper?'; then
    sudo spctl --master-disable
    sudo defaults write /var/db/SystemPolicy-prefs.plist enabled -string no
    defaults write com.apple.LaunchServices LSQuarantine -bool false
  fi

  setting 'revealing IP address, hostname, OS version, etc when clicking the clock in the login window'
  sudo defaults write /Library/Preferences/com.apple.loginwindow AdminHostInfo HostName

  setting 'restarting automatically if computer freezes'
  sudo systemsetup -setrestartfreeze on

  if ask 'set default login text?'; then
    sudo defaults write /Library/Preferences/com.apple.loginwindow LoginwindowText -string "I'm sorry Dave, I'm afraid I can't do that"
  fi

  info '[User experience]'

  setting 'disabling system-wide resume upon login'
  defaults write com.apple.systempreferences NSQuitAlwaysKeepsWindows -bool false

  setting 'disabling automatic termination of inactive apps'
  defaults write NSGlobalDomain NSDisableAutomaticTermination -bool true

  setting 'setting scrollbars behavior to automatic'
  defaults write NSGlobalDomain AppleShowScrollBars -string "Automatic"

  setting 'automatically quit the print app once the print jobs complete'
  defaults write com.apple.print.PrintingPrefs "Quit When Finished" -bool true

  setting 'saving to disk (instead of iCloud) by default'
  defaults write NSGlobalDomain NSDocumentSaveNewDocumentsToCloud -bool false

  setting 'disabling "application crashed" dialog'
  defaults write com.apple.CrashReporter DialogType none

  setting 'setting the update frequency to once every week'
  defaults write com.apple.SoftwareUpdate ScheduleFrequency -int 7

  setting 'disabling smart quotes and dashes'
  defaults write NSGlobalDomain NSAutomaticQuoteSubstitutionEnabled -bool false
  defaults write NSGlobalDomain NSAutomaticDashSubstitutionEnabled -bool false

  info '[User interface]'

  setting 'setting highlight color to #cc99cc'
  defaults write NSGlobalDomain AppleHighlightColor -string "0.600000 0.800000 0.600000"

  setting 'setting sidebar icon size to medium'
  defaults write NSGlobalDomain NSTableViewDefaultSizeMode -int 2

  setting 'expanding the save panel by default'
  defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true
  defaults write NSGlobalDomain PMPrintingExpandedStateForPrint -bool true
  defaults write NSGlobalDomain PMPrintingExpandedStateForPrint2 -bool true

  setting 'showing battery life percentage'
  defaults write com.apple.menuextra.battery ShowPercent -string "YES"

  setting 'displaying ASCII control characters using caret notation in standard text views'
  defaults write NSGlobalDomain NSTextShowsControlCharacters -bool true

  setting 'setting help viewer windows to non-floating mode'
  defaults write com.apple.helpviewer DevMode -bool true

  setting 'adding hotkey to quickly switch to Dark Mode (ctrl+opt+⌘+t)'
  sudo defaults write /Library/Preferences/.GlobalPreferences.plist _HIEnableThemeSwitchHotKey -bool true

  info '[Input/output]'

  setting 'increasing sound quality for Bluetooth headphones/headsets'
  defaults write com.apple.BluetoothAudioAgent "Apple Bitpool Min (editable)" -int 40

  setting 'enabling full keyboard access for all controls (e.g. enable Tab in modal dialogs)'
  defaults write NSGlobalDomain AppleKeyboardUIMode -int 3

  setting 'disabling press-and-hold for keys in favor of key repeat'
  defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false

  setting 'setting a blazingly fast keyboard repeat rate'
  defaults write NSGlobalDomain KeyRepeat -int 2

  if ask 'enable tap-to-click?'; then
    defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking -bool true
    defaults -currentHost write NSGlobalDomain com.apple.mouse.tapBehavior -int 1
    defaults write NSGlobalDomain com.apple.mouse.tapBehavior -int 1
  fi

  if ask 'map bottom right corner to right-click? (Trackpad)'; then
    defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadCornerSecondaryClick -int 2
    defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadRightClick -bool true
    defaults -currentHost write NSGlobalDomain com.apple.trackpad.trackpadCornerClickBehavior -int 1
    defaults -currentHost write NSGlobalDomain com.apple.trackpad.enableSecondaryClick -bool true
  fi

  if ask 'disable "natural" (Lion-style) scrolling?'; then
    defaults write NSGlobalDomain com.apple.swipescrolldirection -bool false
  fi

  setting 'enabling scroll gesture with the Ctrl (^) modifier key to zoom'
  defaults write com.apple.universalaccess closeViewScrollWheelToggle -bool true
  defaults write com.apple.universalaccess HIDScrollZoomModifierMask -int 262144

  info '[Localization]'

  setting 'setting language and text format to English'
  defaults write NSGlobalDomain AppleLanguages -array "en" "sv"
  defaults write NSGlobalDomain AppleLocale -string "en_SE@currency=SEK"

  setting 'using the metric system instead of the imperial'
  defaults write NSGlobalDomain AppleMeasurementUnits -string "Centimeters"
  defaults write NSGlobalDomain AppleMetricUnits -bool true

  setting 'using dot notation as decimal separator'
  defaults write NSGlobalDomain AppleICUNumberSymbols -dict 0 "."

  setting 'using swedish date & time format with English language'
  defaults write NSGlobalDomain AppleICUDateFormatStrings -dict 1 "y-MM-dd" 2 "d MMM y" 3 "d MMMM y"
  defaults write com.apple.menuextra.clock DateFormat -string "EEE HH:mm"

  setting 'setting the timezone to Swedish (Stockholm)'
  sudo systemsetup -settimezone "Europe/Stockholm" > /dev/null
  sudo systemsetup -setnetworktimeserver "time.euro.apple.com" > /dev/null
  sudo systemsetup -setusingnetworktime on > /dev/null

  info '[Screen]'

  setting 'requiring password immediately (5s) after sleep or screen saver begins'
  defaults write com.apple.screensaver askForPassword -int 1
  defaults write com.apple.screensaver askForPasswordDelay -int 5

  setting 'saving screenshots to ~/Screenshots'
  defaults write com.apple.screencapture location -string "$HOME/Screenshots"
  mkdir -p ~/Screenshots

  setting 'saving screenshots in PNG format (other options: BMP, GIF, JPG, PDF, TIFF)'
  defaults write com.apple.screencapture type -string "png"

  setting 'enabling subpixel font rendering on non-Apple LCDs'
  defaults write NSGlobalDomain AppleFontSmoothing -int 2

  setting 'showing displays in menu bar'
  defaults write com.apple.airplay showInMenuBarIfPresent -bool false

  setting 'enabling HiDPI display modes (requires restart)'
  sudo defaults write /Library/Preferences/com.apple.windowserver DisplayResolutionEnabled -bool true

  info '[Power management]'

  setting 'disabling the sudden motion sensor (not useful for SSDs)'
  sudo pmset -a sms 0

  setting 'removing the sleep image file to save disk space'
  sudo chflags nouchg /Private/var/vm/sleepimage
  sudo rm /Private/var/vm/sleepimage
  sudo touch /Private/var/vm/sleepimage
  sudo chflags uchg /Private/var/vm/sleepimage

  setting 'speeding up wake from sleep from 24 hours to an hour'
  # http://www.cultofmac.com/221392/quick-hack-speeds-up-retina-macbooks-wake-from-sleep-os-x-tips/
  sudo pmset -a standbydelay 86400

  setting 'disabling hibernation (only use sleep mode)'
  sudo pmset -a hibernatemode 0

  setting 'setting (battery) display sleep to 5 minutes and (disk) sleep to 10 minutes'
  sudo pmset -b sleep 10
  sudo pmset -b displaysleep 5
  sudo pmset -b disksleep 10

  setting 'setting (charger) display sleep to 15 minutes and disabling disk sleep'
  sudo pmset -c sleep 0
  sudo pmset -c displaysleep 15
  sudo pmset -c disksleep 0

  setting 'setting keyboard illumination to turn off when computer is not used for 5 minutes'
  defaults write com.apple.BezelServices kDimTime -int 300

  info '[Dock]'

  setting 'enabling highlight hover effect for the grid view of a stack'
  defaults write com.apple.dock mouse-over-hilite-stack -bool true

  setting 'changing minimize/maximize window effect'
  defaults write com.apple.dock mineffect -string "scale"

  setting 'minimizing windows into their own application icon'
  defaults write com.apple.dock mineffect -string "scale"

  setting 'setting the icon size of items to 56 pixels'
  defaults write com.apple.dock tilesize -int 56

  setting 'enabling spring loading for all items'
  defaults write com.apple.dock enable-spring-load-actions-on-all-items -bool true

  setting 'showing indicator lights for open applications'
  defaults write com.apple.dock show-process-indicators -bool true

  setting 'disabling animations when opening applications'
  defaults write com.apple.dock launchanim -bool false

  setting 'enabling auto-hide and removing the display delay'
  defaults write com.apple.dock autohide -bool true
  defaults write com.apple.dock autohide-delay -float 0
  defaults write com.apple.dock autohide-time-modifier -float 0

  setting 'making icons of hidden applications translucent'
  defaults write com.apple.dock showhidden -bool true

  if ask 'setup default dock apps?'; then
    dockutil --remove all

    apps=(
      "/Applications/Google Chrome.app"
      "/Applications/Messages.app"
      "/Applications/Skype.app"
      "/Applications/Dash.app"
      "/Applications/Mail.app"
      "/Applications/Spotify.app"
      "/Applications/Xcode.app"
      "/Applications/Popcorn-Time.app"
      "/Applications/MacVim.app"
      "/Applications/1Password 5.app"
      "/Applications/iTerm.app"
    )

    for app in "${apps[@]}"; do
      if [ -d "$app" ]; then
        dockutil --add "$app"
      else
        fail "could not add $app to dock (does not exist)"
      fi
    done

    # ... the default folders
    dockutil --add "/Applications" --view list --display folder --sort name
    dockutil --add "$HOME/Downloads" --view grid --display stack --sort dateadded
  fi

  if ask 'setup hot corners?'; then
    info '[Hot corners]'
    # Possible values:
    #  0: no-op
    #  2: Mission Control
    #  3: Show application windows
    #  4: Desktop
    #  5: Start screen saver
    #  6: Disable screen saver
    #  7: Dashboard
    # 10: Put display to sleep
    # 11: Launchpad
    # 12: Notification Center

    setting 'top left screen corner → Mission Control'
    defaults write com.apple.dock wvous-tl-corner -int 2
    defaults write com.apple.dock wvous-tl-modifier -int 0

    setting 'top right screen corner → Mission Control'
    defaults write com.apple.dock wvous-tr-corner -int 4
    defaults write com.apple.dock wvous-tr-modifier -int 0

    setting 'bottom left screen corner → Mission Control'
    defaults write com.apple.dock wvous-bl-corner -int 5
    defaults write com.apple.dock wvous-bl-modifier -int 0
  fi

  info '[Mission control]'

  setting 'speeding up animations'
  defaults write com.apple.dock expose-animation-duration -float 0.1

  setting 'grouping windows by application'
  defaults write com.apple.dock expose-group-by-app -bool true

  setting 'disabling most-recently-used (MRU) ordering of spaces'
  defaults write com.apple.dock mru-spaces -bool false

  info '[Spotlight]'

  setting 'hiding the spotlight tray icon'
  sudo chmod 600 /System/Library/CoreServices/Search.bundle/Contents/MacOS/Search

  setting 'disabling indexing for mounted volumes'
  sudo defaults write /.Spotlight-V100/VolumeConfiguration Exclusions -array "/Volumes"

  setting 'disabling display of disclaimer text'
  defaults write com.apple.Spotlight useCount -int 3
  defaults write com.apple.Spotlight showedFTE -bool YES

  setting 'disabling shortcuts in favor of Alfred'
  defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 64 "{ enabled = 0; value = { parameters = ( 32, 49, 1048576); type = standard; }; }"
  defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 65 "{ enabled = 0; value = { parameters = ( 32, 49, 1048576); type = standard; }; }"

  setting 'ensuring indexing is enabled for the main volume'
  sudo mdutil -i on / > /dev/null

  info '[Finder]'

  setting 'disabling window animations and Get Info animations'
  defaults write com.apple.finder DisableAllAnimations -bool true

  setting 'setting home (~) as the default finder location'
  defaults write com.apple.finder NewWindowTarget -string "PfHm"
  defaults write com.apple.finder NewWindowTargetPath -string "file://${HOME}/"

  setting 'showing icons for hard drives server and removable media on the desktop'
  defaults write com.apple.finder ShowExternalHardDrivesOnDesktop -bool true

  setting 'showing all filename extensions'
  defaults write NSGlobalDomain AppleShowAllExtensions -bool true

  setting 'showing status and path bar'
  defaults write com.apple.finder ShowStatusBar -bool true
  defaults write com.apple.finder ShowPathbar -bool true

  setting 'preferring to group by kind'
  defaults write com.apple.finder FXPreferredGroupBy -string "Kind"

  setting 'allowing text selection in Quick Look/Preview'
  defaults write com.apple.finder QLEnableTextSelection -bool true

  setting 'displaying full POSIX path in the window title'
  defaults write com.apple.finder _FXShowPosixPathInTitle -bool true

  setting 'setting the default search scope to the current folder'
  defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"

  setting 'disabling warning when changing a file extension'
  defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false

  setting 'using list view in all windows by default'
  defaults write com.apple.finder FXPreferredViewStyle "Nlsv"

  setting 'expanding "General", "Open with" and "Sharing & Permissions" info panes'
  defaults write com.apple.finder FXInfoPanesExpanded -dict General -bool true OpenWith -bool true Privileges -bool true

  setting 'disabling the warning when emptying the Trash'
  defaults write com.apple.finder WarnOnEmptyTrash -bool false

  setting 'disabling the creation of .DS_Store files on network volumes'
  defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true

  setting 'disabling disk image verification'
  defaults write com.apple.frameworks.diskimages skip-verify -bool true
  defaults write com.apple.frameworks.diskimages skip-verify-locked -bool true
  defaults write com.apple.frameworks.diskimages skip-verify-remote -bool true

  setting 'showing item info near icons on the desktop and in other icon views'
  /usr/libexec/PlistBuddy -c "Set :DesktopViewSettings:IconViewSettings:showItemInfo true" ~/Library/Preferences/com.apple.finder.plist
  /usr/libexec/PlistBuddy -c "Set :FK_StandardViewSettings:IconViewSettings:showItemInfo true" ~/Library/Preferences/com.apple.finder.plist
  /usr/libexec/PlistBuddy -c "Set :StandardViewSettings:IconViewSettings:showItemInfo true" ~/Library/Preferences/com.apple.finder.plist

  setting 'enabling snap-to-grid for icons on the desktop and in other icon views'
  /usr/libexec/PlistBuddy -c "Set :DesktopViewSettings:IconViewSettings:arrangeBy grid" ~/Library/Preferences/com.apple.finder.plist
  /usr/libexec/PlistBuddy -c "Set :FK_StandardViewSettings:IconViewSettings:arrangeBy grid" ~/Library/Preferences/com.apple.finder.plist
  /usr/libexec/PlistBuddy -c "Set :StandardViewSettings:IconViewSettings:arrangeBy grid" ~/Library/Preferences/com.apple.finder.plist

  setting 'setting the icon size and grid spacing to 64 pixels'
  /usr/libexec/PlistBuddy -c "Set :DesktopViewSettings:IconViewSettings:gridSpacing 64" ~/Library/Preferences/com.apple.finder.plist
  /usr/libexec/PlistBuddy -c "Set :DesktopViewSettings:IconViewSettings:iconSize 64" ~/Library/Preferences/com.apple.finder.plist

  if ask 'setup Finder toolbar?'; then
    # Remove all currently assigned toolbar items
    /usr/libexec/PlistBuddy -c 'Delete "NSToolbar Configuration Browser:TB Item Identifiers"' ~/Library/Preferences/com.apple.finder.plist
    /usr/libexec/PlistBuddy -c 'Add "NSToolbar Configuration Browser:TB Item Identifiers" array' ~/Library/Preferences/com.apple.finder.plist

    # Add each item to the toolbar
    /usr/libexec/PlistBuddy -c 'Add "NSToolbar Configuration Browser:TB Item Identifiers:0" string "com.apple.finder.BACK"' ~/Library/Preferences/com.apple.finder.plist
    /usr/libexec/PlistBuddy -c 'Add "NSToolbar Configuration Browser:TB Item Identifiers:1" string "com.apple.finder.ARNG"' ~/Library/Preferences/com.apple.finder.plist
    /usr/libexec/PlistBuddy -c 'Add "NSToolbar Configuration Browser:TB Item Identifiers:2" string "com.apple.finder.ACTN"' ~/Library/Preferences/com.apple.finder.plist
    /usr/libexec/PlistBuddy -c 'Add "NSToolbar Configuration Browser:TB Item Identifiers:3" string "NSToolbarSpaceItem"' ~/Library/Preferences/com.apple.finder.plist
    /usr/libexec/PlistBuddy -c 'Add "NSToolbar Configuration Browser:TB Item Identifiers:4" string "com.apple.finder.SWCH"' ~/Library/Preferences/com.apple.finder.plist
    /usr/libexec/PlistBuddy -c 'Add "NSToolbar Configuration Browser:TB Item Identifiers:5" string "com.apple.finder.SRCH"' ~/Library/Preferences/com.apple.finder.plist
    /usr/libexec/PlistBuddy -c 'Add "NSToolbar Configuration Browser:TB Item Identifiers:6" string "NSToolbarFlexibleSpaceItem"' ~/Library/Preferences/com.apple.finder.plist
    /usr/libexec/PlistBuddy -c 'Add "NSToolbar Configuration Browser:TB Item Identifiers:7" string "com.apple.finder.loc "' ~/Library/Preferences/com.apple.finder.plist
    /usr/libexec/PlistBuddy -c 'Add "NSToolbar Configuration Browser:TB Item Identifiers:8" string "com.apple.finder.CNCT"' ~/Library/Preferences/com.apple.finder.plist
    /usr/libexec/PlistBuddy -c 'Add "NSToolbar Configuration Browser:TB Item Identifiers:9" string "com.apple.finder.TRSH"' ~/Library/Preferences/com.apple.finder.plist

    # Remove the custom defined items
    /usr/libexec/PlistBuddy -c 'Delete "NSToolbar Configuration Browser:TB Item Plists"' ~/Library/Preferences/com.apple.finder.plist
    /usr/libexec/PlistBuddy -c 'Add "NSToolbar Configuration Browser:TB Item Plists" dict' ~/Library/Preferences/com.apple.finder.plist

    # Add the terminal opener item
    /usr/libexec/PlistBuddy -c 'Add "NSToolbar Configuration Browser:TB Item Plists:7" dict' ~/Library/Preferences/com.apple.finder.plist
    /usr/libexec/PlistBuddy -c "Add 'NSToolbar Configuration Browser:TB Item Plists:7:_CFURLString' string 'file://$directory/ext/open-terminal.app'" ~/Library/Preferences/com.apple.finder.plist
    /usr/libexec/PlistBuddy -c 'Add "NSToolbar Configuration Browser:TB Item Plists:7:_CFURLStringType" integer 15' ~/Library/Preferences/com.apple.finder.plist
  fi

  setting 'making the home library folder visible'
  chflags nohidden ~/Library

  if ask 'enable spring loading for directories?'; then
    defaults write NSGlobalDomain com.apple.springing.enabled -bool true
    defaults write NSGlobalDomain com.apple.springing.delay -float 0.5
  fi

  setting 'enabling AirDrop over Ethernet and on unsupported Macs running Lion'
  defaults write com.apple.NetworkBrowser BrowseAllInterfaces -bool true
}

setup_file_assoc() {
  if ! is-command duti || ! ask "configure file associations?"; then
    return 1
  fi

  info 'running duti (file associator)'
  duti "ext/extensions.duti"
}

setup_quicklook() {
  if ! ask "install quicklooks?"; then
    return 1
  fi

  qls=(
    "betterzipql"
    "qlimagesize"
    "ipaql"
    "qlcolorcode"
    "qlmarkdown"
    "qlprettypatch"
    "qlstephen"
    "quicklook-csv"
    "quicklook-json"
    "provisionql"
    "webpquicklook"
    "suspicious-package"
  )

  for ql in "${qls[@]}"; do
    if ! brew cask list "$ql" &>-; then
      sudo brew cask install "$ql" --qlplugindir=/Library/QuickLook
    fi
  done
}

setup_apps() {
  if ! ask "install apps?"; then
    return 1
  fi

  apps=(
    "1password"
    "alfred"
    "asepsis"
    "bettertouchtool"
    "cheatsheet"
    "dash"
    "dropbox"
    "firefox"
    "flux"
    "google-chrome"
    "iterm2"
    "keepingyouawake"
    "popcorn-time"
    "seil"
    "skype"
    "spotify"
    "the-unarchiver"
    "transmission"
    "virtualbox"
    "vlc"
    "xcode"
    "xquartz"
  )

  for app in "${apps[@]}"; do
    if ! brew cask list "$app" &>-; then
      brew cask install "$app" --appdir=/Applications
    fi
  done
}

setup_binaries() {
  if ! ask "install binaries?"; then
    return 1
  fi

  binaries=(
    # Install GNU core utilities (those that come with OS X are outdated)
    "coreutils"
    "diffutils"
    "binutils"

    # Install some other useful utilities
    "moreutils --without-parallel"
    "uchardet"
    "dockutil"
    "gifify"
    "fasd"
    "htop-osx"
    "tmux"
    "icdiff"
    "reattach-to-user-namespace --with-wrap-pbcopy-and-pbpaste"
    "duti"
    "zsh"
    "bash"
    "gzip"
    "cmatrix"
    "shellcheck"
    "editorconfig"
    "ascii"
    "jq"

    # Install GNU `find`, `locate`, `updatedb`, `xargs` etc
    "findutils --with-default-names"
    "gnu-indent --with-default-names"
    "gnu-which --with-default-names"
    "gnu-tar --with-default-names"
    "gnu-sed --with-default-names"
    "gnutls"
    "grep --with-default-names"
    "ed --with-default-names"
    "wdiff --with-gettext"
    "wget --with-iri"
    "parallel"
    "screen"
    "watch"
    "gawk"

    # OS X outdated tools
    "git"
    "file-formula"
    "openssh --with-brewed-openssl"
    "rsync"
    "gpatch"
    "nano"
    "less"
    "source-highlight"
    "lesspipe"

    # Image optimization software
    "gifsicle"
    "jpegoptim"
    "optipng"

    # Enable some useful scripting
    "python --with-brewed-openssl"
    "ruby"

    # Awesomest editor (Vim)
    "cscope"
    "luajit"
    "vim --with-cscope --with-python --with-luajit --override-system-vim"
    "emacs-mac --with-dbus --with-xml2 --with-spacemacs-icon"

    # Tools for development
    "mackup"
    "bfg"
    "node"
    "sassc"
    "hub"
    "recode"
    "dex2jar"
    "fcrackzip"
    "foremost"
    "dnsmasq"
    "nmap"
    "pngcheck"
    "sqlmap"
    "xz"
    "rar"

    # Commonly used binaries
    "the_silver_searcher"
    "fzf"
    "ssh-copy-id"
    "p7zip"
    "pigz"
    "pv"
    "rename"
    "tree"
    "webkit2png"
    "zopfli"
    "par"
  )

  for binary in "${binaries[@]}"; do
    local name=($binary)

    if [ -z "$(brew ls --versions "${name[0]}")" ]; then
      brew install "${name[@]}"
    fi
  done

  brew linkapps
}

setup_cask() {
  if is-command brew-cask; then
    return 0
  elif ask "install brew cask? (required for apps & ql)"; then
    brew install caskroom/cask/brew-cask
  else
    return 1
  fi
}

setup_taps() {
  taps=(
    "homebrew/dupes"
    "homebrew/binary"
    "railwaycat/emacsmacport"
    "casidiablo/custom"
    "darfink/custom"
  )

  for tap in "${taps[@]}"; do
    if ! brew tap | grep -Fq "$tap"; then
      info "tapping $tap"
      brew tap "$tap" > /dev/null
    fi
  done
}

setup_brew() {
  if is-command brew; then
    return 0
  elif ask "install brew? (required for apps, binaries & ql)"; then
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
  else
    return 1
  fi
}

setup_fonts() {
  if ! ask "install powerline fonts?"; then
    return 1
  fi

  fonts/install.sh > /dev/null
  if [ $? -ne 0 ]; then
    fail 'could not install powerline fonts'
    info 'try to run fonts/install.sh manually'
    return 1
  else
    info 'powerline fonts installed'
  fi
}

setup_sshkey() {
  if [ ! -e "$HOME/.ssh/id_rsa" ]; then
    user 'generating SSH key; please input your email:'
    read email
    ssh-keygen -f ~/.ssh/id_rsa -t rsa -C "$email" -N ""
  else
    return 0
  fi
}

install_prerequisites() {
  if ! xcode-select -p >&-; then
    info 'dependency: installing CLT (command-line-tools)'
    info 'choose "Install"; then run "install.sh" again'
    sleep 5
    xcode-select --install 2> /dev/null
    return 1
  fi
}

prompt_directory() {
  while true; do
    user 'specify installation directory (press [Enter] for ~/.dotfiles):'
    read -r directory

    if [ -z "$directory" ]; then
      directory="$HOME/.dotfiles"
    fi

    if [ -e "$directory" ]; then
      if ask "directory already exists; delete it?"; then
        info "removing $directory"
        rm -rf "$directory"
      else
        # Ask for another directory
        continue
      fi
    fi

    # Attempt to create the directory
    mkdir -p "$directory" 2> /dev/null && break
    fail "could not create directory: $directory"
    return 1
  done

  # Ensure it is an absolute path
  directory="$(python -c 'import os, sys; print os.path.realpath(sys.argv[1])' "$directory")"
}

info '------ DOTFILES ------'

# Ask for the administrator password upfront
sudo -v

# Keep-alive: update existing `sudo` time stamp until `install.sh` has finished
while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

install_prerequisites || exit 1
prompt_directory || exit 1

# Move to our dotfiles directory
cd "$directory" || exit 1

if ! git clone --recursive https://github.com/darfink/dotfiles.git "$directory"; then
  fail 'could not clone dotfiles repository'
  exit 1
fi

# We need to ensure these paths are available
export PATH="$directory/bin:$PATH"

setup_fonts
python -m tools.sshkey
python -m tools.symlinker

if setup_brew; then
  setup_taps

  info 'ensuring brew is up-to-date'
  brew update > /dev/null
  brew upgrade

  setup_binaries

  if setup_cask; then
    setup_apps
    setup_quicklook
  fi

  info 'removing old and unused versions'
  brew cleanup
  brew prune
fi

# Last but not least!
setup_file_assoc
setup_defaults
