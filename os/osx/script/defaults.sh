# Some things taken from here
# https://github.com/mathiasbynens/dotfiles/blob/master/.osx

# We use PlistBuddy a lot
alias plistc="/usr/libexec/PlistBuddy -c"

###############################################################################
# General UI/UX
###############################################################################

echo ""
echo "Disabling sound effects on boot"
sudo nvram SystemAudioVolume=0

echo ""
echo "Setting highlight color to #CC99CC"
defaults write NSGlobalDomain AppleHighlightColor -string "0.600000 0.800000 0.600000"

echo ""
echo "Setting sidebar icon size to medium"
defaults write NSGlobalDomain NSTableViewDefaultSizeMode -int 2

echo ""
echo "Always showing scrollbars"
defaults write NSGlobalDomain AppleShowScrollBars -string "Always"

echo ""
echo "Hiding the Time Machine, Volume, User, and Bluetooth icons"
for domain in ~/Library/Preferences/ByHost/com.apple.systemuiserver.*; do
  defaults write "${domain}" dontAutoLoad -array \
    "/System/Library/CoreServices/Menu Extras/TimeMachine.menu" \
    "/System/Library/CoreServices/Menu Extras/Volume.menu" \
    "/System/Library/CoreServices/Menu Extras/User.menu"
done
defaults write com.apple.systemuiserver menuExtras -array \
  "/System/Library/CoreServices/Menu Extras/Bluetooth.menu" \
  "/System/Library/CoreServices/Menu Extras/AirPort.menu" \
  "/System/Library/CoreServices/Menu Extras/Battery.menu" \
  "/System/Library/CoreServices/Menu Extras/Clock.menu"

echo ""
echo "Disabling OS X Gate Keeper"
echo "(You'll be able to install any app you want from here on, not just Mac App Store apps)"
sudo spctl --master-disable
sudo defaults write /var/db/SystemPolicy-prefs.plist enabled -string no
defaults write com.apple.LaunchServices LSQuarantine -bool false

echo ""
echo "Increasing the window resize speed for Cocoa applications"
defaults write NSGlobalDomain NSWindowResizeTime -float 0.001

echo ""
echo "Expanding the save panel by default"
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint -bool true
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint2 -bool true

echo ""
echo "Automatically quit printer app once the print jobs complete"
defaults write com.apple.print.PrintingPrefs "Quit When Finished" -bool true

# Try e.g. `cd /tmp; unidecode "\x{0000}" > cc.txt; open -e cc.txt`
echo ""
echo "Displaying ASCII control characters using caret notation in standard text views"
defaults write NSGlobalDomain NSTextShowsControlCharacters -bool true

echo ""
echo "Disabling system-wide resume"
defaults write com.apple.systempreferences NSQuitAlwaysKeepsWindows -bool false

echo ""
echo "Disabling automatic termination of inactive apps"
defaults write NSGlobalDomain NSDisableAutomaticTermination -bool true

echo ""
echo "Saving to disk (not to iCloud) by default"
defaults write NSGlobalDomain NSDocumentSaveNewDocumentsToCloud -bool false

echo ""
echo "Reveal IP address, hostname, OS version, etc. when clicking the clock in the login window"
sudo defaults write /Library/Preferences/com.apple.loginwindow AdminHostInfo HostName

echo ""
echo "Showing battery life percentage"
defaults write com.apple.menuextra.battery ShowPercent -string "YES"

echo ""
echo "Changing the default wallpaper"
sqlite3 '~/Library/Application Support/Dock/desktoppicture.db' "UPDATE data SET value = '$DOTFILES/ext/wallpaper.jpg'"

echo ""
echo "Disabling scheduled updates (do it manually instead)"
defaults write com.apple.SoftwareUpdate ScheduleFrequency -int 0

echo ""
echo "Disabling smart quotes and dashes as they're annoying when typing code"
defaults write NSGlobalDomain NSAutomaticQuoteSubstitutionEnabled -bool false
defaults write NSGlobalDomain NSAutomaticDashSubstitutionEnabled -bool false

echo ""
echo "Setting Help Viewer windows to non-floating mode"
defaults write com.apple.helpviewer DevMode -bool true

echo ""
echo "Restarting automatically if the computer freezes"
sudo systemsetup -setrestartfreeze on

###############################################################################
# Trackpad, mouse, keyboard, Bluetooth accessories, and input
###############################################################################

echo ""
echo "Increasing sound quality for Bluetooth headphones/headsets"
defaults write com.apple.BluetoothAudioAgent "Apple Bitpool Min (editable)" -int 40

echo ""
echo "Enabling full keyboard access for all controls (e.g. enable Tab in modal dialogs)"
defaults write NSGlobalDomain AppleKeyboardUIMode -int 3

echo ""
echo "Disabling press-and-hold for keys in favor of a key repeat"
defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false

echo ""
echo "Setting a blazingly fast keyboard repeat rate (ain't nobody got time fo special chars while coding!)"
defaults write NSGlobalDomain KeyRepeat -int 0

echo ""
echo "Disabling auto-correct"
defaults write NSGlobalDomain NSAutomaticSpellingCorrectionEnabled -bool false

echo ""
echo "Setting trackpad & mouse speed to a reasonable number"
defaults write -g com.apple.trackpad.scaling 1
defaults write -g com.apple.mouse.scaling 1.5

echo ""
echo "Enabling tap to click (Trackpad) for this user and for the login screen"
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking -bool true
defaults -currentHost write NSGlobalDomain com.apple.mouse.tapBehavior -int 1
defaults write NSGlobalDomain com.apple.mouse.tapBehavior -int 1

echo ""
echo "Enabling map bottom right corner (Trackpad) to right-click"
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadCornerSecondaryClick -int 2
defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadRightClick -bool true
defaults -currentHost write NSGlobalDomain com.apple.trackpad.trackpadCornerClickBehavior -int 1
defaults -currentHost write NSGlobalDomain com.apple.trackpad.enableSecondaryClick -bool true

echo ""
echo "Disabling “natural” (Lion-style) scrolling"
defaults write NSGlobalDomain com.apple.swipescrolldirection -bool false

echo ""
echo "Enabling scroll gesture with the Ctrl (^) modifier key to zoom"
defaults write com.apple.universalaccess closeViewScrollWheelToggle -bool true
defaults write com.apple.universalaccess HIDScrollZoomModifierMask -int 262144

echo ""
echo "Follow the keyboard focus while zoomed in"
defaults write com.apple.universalaccess closeViewZoomFollowsFocus -bool true

echo ""
echo "Set language and text formats"
defaults write NSGlobalDomain AppleLanguages -array "en" "sv"
defaults write NSGlobalDomain AppleLocale -string "en_GB@currency=SEK"
defaults write NSGlobalDomain AppleMeasurementUnits -string "Centimeters"
defaults write NSGlobalDomain AppleMetricUnits -bool true

echo ""
echo "Set the timezone to swedish time"
sudo systemsetup -settimezone "Europe/Stockholm" > /dev/null

###############################################################################
# Screen
###############################################################################

echo ""
echo "Setting login window text"
sudo defaults write /Library/Preferences/com.apple.loginwindow LoginwindowText -string "I'm sorry Dave, I'm afraid I can't do that"

echo ""
echo "Requiring password immediately (5s) after sleep or screen saver begins"
defaults write com.apple.screensaver askForPassword -int 1
defaults write com.apple.screensaver askForPasswordDelay -int 5

echo ""
echo "Saving screenshots to ~/Screenshots"
defaults write com.apple.screencapture location -string "${HOME}/Screenshots"

# Create the directory if it doesn't exist
mkdir -p ~/Screenshots

echo ""
echo "Saving screenshots in PNG format (other options: BMP, GIF, JPG, PDF, TIFF)"
defaults write com.apple.screencapture type -string "png"

echo ""
echo "Enabling subpixel font rendering on non-Apple LCDs"
defaults write NSGlobalDomain AppleFontSmoothing -int 2

echo ""
echo "Showing displays in menu bar"
defaults write com.apple.airplay showInMenuBarIfPresent -bool false

echo ""
echo "Enabling HiDPI display modes (requires restart)"
sudo defaults write /Library/Preferences/com.apple.windowserver DisplayResolutionEnabled -bool true

###############################################################################
# Power Management
###############################################################################

echo ""
echo "Disabling the sudden motion sensor as it's not useful for SSDs"
sudo pmset -a sms 0

echo ""
Echo "Removing the sleep image file to save disk space"
sudo rm /Private/var/vm/sleepimage
sudo touch /Private/var/vm/sleepimage
sudo chflags uchg /Private/var/vm/sleepimage

echo ""
echo "Speeding up wake from sleep from 24 hours to an hour"
# http://www.cultofmac.com/221392/quick-hack-speeds-up-retina-macbooks-wake-from-sleep-os-x-tips/
sudo pmset -a standbydelay 86400

echo ""
echo "Disabling hibernation (only use sleep mode)"
sudo pmset -a hibernatemode 0

echo ""
echo "Turn off keyboard illumination when computer is not used for 5 minutes"
defaults write com.apple.BezelServices kDimTime -int 300

###############################################################################
# Dock & Mission Control
###############################################################################

echo ""
echo "Enabling highlight hover effect for the grid view of a stack (Dock)"
defaults write com.apple.dock mouse-over-hilite-stack -bool true

echo ""
echo "Changing minimize/maximize window effect"
defaults write com.apple.dock mineffect -string "scale"

echo ""
echo "Minimize windows into their application’s icon"
defaults write com.apple.dock minimize-to-application -bool true

echo ""
echo "Setting the icon size of Dock items to 36 pixels for optimal size/screen-realestate"
defaults write com.apple.dock tilesize -int 36

echo ""
echo "Enabling spring loading for all Dock items"
defaults write com.apple.dock enable-spring-load-actions-on-all-items -bool true

echo ""
echo "Showing indicator lights for open applications in the Dock"
defaults write com.apple.dock show-process-indicators -bool true

echo ""
echo "Don’t animate opening applications from the Dock"
defaults write com.apple.dock launchanim -bool false

echo ""
echo "Speeding up Mission Control animations and grouping windows by application"
defaults write com.apple.dock expose-animation-duration -float 0.1
defaults write com.apple.dock expose-group-by-app -bool true

echo ""
echo "Setting Dock to auto-hide and removing the auto-hiding delay"
defaults write com.apple.dock autohide -bool true
defaults write com.apple.dock autohide-delay -float 0
defaults write com.apple.dock autohide-time-modifier -float 0

echo ""
echo "Making Dock icons of hidden applications translucent"
defaults write com.apple.dock showhidden -bool true

echo ""
echo "Disabling Dashboard"
defaults write com.apple.dashboard mcx-disabled -bool true

echo ""
echo "Don’t show Dashboard as a Space"
defaults write com.apple.dock dashboard-in-overlay -bool true

echo ""
echo "Don’t automatically rearrange Spaces based on most recent use"
defaults write com.apple.dock mru-spaces -bool false

echo ""
echo "Reset Launchpad, but keep the desktop wallpaper intact"
find "${HOME}/Library/Application Support/Dock" -name "*-*.db" -maxdepth 1 -delete

echo ""
echo "Setting up dock items"
dockutil --remove all

dockapps=(
	"Google Chrome"
	"Messages"
	"Skype"
	"Mail"
	"Spotify"
	"Xcode"
	"Popcorn Time"
	"MacVim"
	"iTerm"
)

dockutil --add "/Applications/${dockapps[@]}.app"

# Add a separator between the applications and folders
defaults write com.apple.dock persistent-apps -array-add '{"tile-type"="spacer-tile";}'

dockutil --add '/Applications' --view list --display folder --sort name
dockutil --add '~/Dropbox' --view grid --display folder --sort name
dockutil --add '~/Downloads' --view grid --display stack --sort dateadded

# Hot corners
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

echo ""
echo "Top left screen corner → Mission Control"
defaults write com.apple.dock wvous-tl-corner -int 2
defaults write com.apple.dock wvous-tl-modifier -int 0

echo ""
echo "Top right screen corner → Desktop"
defaults write com.apple.dock wvous-tr-corner -int 4
defaults write com.apple.dock wvous-tr-modifier -int 0

echo ""
echo "Bottom left screen corner → Start screen saver"
defaults write com.apple.dock wvous-bl-corner -int 5
defaults write com.apple.dock wvous-bl-modifier -int 0

###############################################################################
# Spotlight
###############################################################################

echo ""
echo "Disabling Spotlight indexing for mounted volumes"
sudo defaults write /.Spotlight-V100/VolumeConfiguration Exclusions -array "/Volumes"

echo ""
echo "Disabling display of disclaimer text"
defaults write com.apple.Spotlight useCount -int 3
defaults write com.apple.Spotlight showedFTE -bool YES

echo ""
echo "Disabling Spotlight shortcuts in favor of Alfred"
defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 64 "{ enabled = 0; value = { parameters = ( 32, 49, 1048576); type = standard; }; }"
defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 65 "{ enabled = 0; value = { parameters = ( 32, 49, 1048576); type = standard; }; }"

echo ""
echo "Hiding Spotlight tray icon"
sudo chmod 600 /System/Library/CoreServices/Search.bundle/Contents/MacOS/Search

# Make sure indexing is enabled for the main volume
sudo mdutil -i on / > /dev/null

###############################################################################
# Finder
###############################################################################

echo ""
echo "Disabling window animations and Get Info animations"
defaults write com.apple.finder DisableAllAnimations -bool true

echo ""
echo "Setting home as the default finder location"
defaults write com.apple.finder NewWindowTarget -string "PfHm"
defaults write com.apple.finder NewWindowTargetPath -string "file://${HOME}/"

echo ""
echo "Showing icons for hard drives, servers, and removable media on the desktop"
defaults write com.apple.finder ShowExternalHardDrivesOnDesktop -bool true

echo ""
echo "Showing all filename extensions in Finder by default"
defaults write NSGlobalDomain AppleShowAllExtensions -bool true

echo ""
echo "Showing status and path bar in Finder by default"
defaults write com.apple.finder ShowStatusBar -bool true
defaults write com.apple.finder ShowPathbar -bool true

echo ""
echo "Preferring to group by kind in Finder by default"
defaults write com.apple.finder 'FXPreferredGroupBy' -string 'Kind'

echo ""
echo "Allowing text selection in Quick Look/Preview in Finder by default"
defaults write com.apple.finder QLEnableTextSelection -bool true

echo ""
echo "Displaying full POSIX path as Finder window title"
defaults write com.apple.finder _FXShowPosixPathInTitle -bool true

echo ""
echo "When performing a search, search the current folder by default"
defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"

echo ""
echo "Disabling the warning when changing a file extension"
defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false

echo ""
echo "Using list view in all Finder windows by default"
defaults write com.apple.finder FXPreferredViewStyle "Nlsv"

echo ""
echo "Expanding the following File Info panes:"
echo "“General”, “Open with”, and “Sharing & Permissions”"
defaults write com.apple.finder FXInfoPanesExpanded -dict \
	General -bool true \
	OpenWith -bool true \
	Privileges -bool true

echo ""
echo "Disabling the warning before emptying the Trash"
defaults write com.apple.finder WarnOnEmptyTrash -bool false

echo ""
echo "Avoiding the creation of .DS_Store files on network volumes"
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true

echo ""
echo "Disabling disk image verification"
defaults write com.apple.frameworks.diskimages skip-verify -bool true
defaults write com.apple.frameworks.diskimages skip-verify-locked -bool true
defaults write com.apple.frameworks.diskimages skip-verify-remote -bool true

echo ""
echo "Showing item info near icons on the desktop and in other icon views"
plistc "Set :DesktopViewSettings:IconViewSettings:showItemInfo true" ~/Library/Preferences/com.apple.finder.plist
plistc "Set :FK_StandardViewSettings:IconViewSettings:showItemInfo true" ~/Library/Preferences/com.apple.finder.plist
plistc "Set :StandardViewSettings:IconViewSettings:showItemInfo true" ~/Library/Preferences/com.apple.finder.plist

echo ""
echo "Enabling snap-to-grid for icons on the desktop and in other icon views"
plistc "Set :DesktopViewSettings:IconViewSettings:arrangeBy grid" ~/Library/Preferences/com.apple.finder.plist
plistc "Set :FK_StandardViewSettings:IconViewSettings:arrangeBy grid" ~/Library/Preferences/com.apple.finder.plist
plistc "Set :StandardViewSettings:IconViewSettings:arrangeBy grid" ~/Library/Preferences/com.apple.finder.plist

echo ""
echo "Setting icon size and grid spacing to 64 pixels"
plistc "Set :DesktopViewSettings:IconViewSettings:gridSpacing 64" ~/Library/Preferences/com.apple.finder.plist
plistc "Set :DesktopViewSettings:IconViewSettings:iconSize 64" ~/Library/Preferences/com.apple.finder.plist

echo ""
echo "Configuring Finder toolbar"
plistc 'Delete "NSToolbar Configuration Browser:TB Item Identifiers"' ~/Library/Preferences/com.apple.finder.plist
plistc 'Add "NSToolbar Configuration Browser:TB Item Identifiers" array' ~/Library/Preferences/com.apple.finder.plist

finderbuttons=(
	"com.apple.finder.BACK"
	"com.apple.finder.PATH"
	"com.apple.finder.ARNG"
	"com.apple.finder.ACTN"
	"com.apple.finder.SWCH"
	"NSToolbarSpaceItem"
	"NSToolbarFlexibleSpaceItem"
	"com.apple.finder.INFO"
	"com.apple.finder.CNCT"
	"com.apple.finder.EJCT"
	"com.apple.finder.TRSH"
)

for i in "${!finderbuttons[@]}"; do
	plistc "Add 'NSToolbar Configuration Browser:TB Item Identifiers:$i' string '${finderbuttons[$i]}'" ~/Library/Preferences/com.apple.finder.plist
done

echo ""
echo "Showing the home library folder by default"
chflags nohidden ~/Library

echo ""
echo "Enabling spring loading for directories"
defaults write NSGlobalDomain com.apple.springing.enabled -bool true
defaults write NSGlobalDomain com.apple.springing.delay -float 0.5

echo ""
echo "Enabling AirDrop over Ethernet and on unsupported Macs running Lion"
defaults write com.apple.NetworkBrowser BrowseAllInterfaces -bool true

###############################################################################
# Safari & WebKit
###############################################################################

echo ""
echo "Hiding Safari's bookmarks bar by default"
defaults write com.apple.Safari ShowFavoritesBar -bool false

echo ""
echo "Hiding Safari's sidebar in Top Sites"
defaults write com.apple.Safari ShowSidebarInTopSites -bool false

echo ""
echo "Disabling Safari's thumbnail cache for History and Top Sites"
defaults write com.apple.Safari DebugSnapshotsUpdatePolicy -int 2

echo ""
echo "Prevent Safari from opening ‘safe’ files automatically after downloading"
defaults write com.apple.Safari AutoOpenSafeDownloads -bool false

echo ""
echo "Enabling Safari's debug menu"
defaults write com.apple.Safari IncludeInternalDebugMenu -bool true

echo ""
echo "Making Safari's search banners default to Contains instead of Starts With"
defaults write com.apple.Safari FindOnPageMatchesWordStartsOnly -bool false

echo ""
echo "Removing useless icons from Safari's bookmarks bar"
defaults write com.apple.Safari ProxiesInBookmarksBar "()"

echo ""
echo "Allow hitting the Backspace key to go to the previous page in history"
defaults write com.apple.Safari com.apple.Safari.ContentPageGroupIdentifier.WebKit2BackspaceKeyNavigationEnabled -bool true

echo ""
echo "Enabling the Develop menu and the Web Inspector in Safari"
defaults write com.apple.Safari IncludeDevelopMenu -bool true
defaults write com.apple.Safari WebKitDeveloperExtrasEnabledPreferenceKey -bool true
defaults write com.apple.Safari "com.apple.Safari.ContentPageGroupIdentifier.WebKit2DeveloperExtrasEnabled" -bool true

echo ""
echo "Adding a context menu item for showing the Web Inspector in web views"
defaults write NSGlobalDomain WebKitDeveloperExtras -bool true

echo ""
echo "Disabling search queries being sent to Apple"
defaults write com.apple.Safari UniversalSearchEnabled -bool false
defaults write com.apple.Safari SuppressSearchSuggestions -bool true

###############################################################################
# Mail
###############################################################################

echo ""
echo "Setting email addresses to copy as 'foo@example.com' instead of 'Foo Bar <foo@example.com>' in Mail.app"
defaults write com.apple.mail AddressesIncludeNameOnPasteboard -bool false

echo ""
echo "Adding ⌘ + Enter keyboard shortcut to send an email in Mail.app"
defaults write com.apple.mail NSUserKeyEquivalents -dict-add "Send" -string "@\\U21a9"

echo ""
echo "Displaying emails in threaded mode, sorted by date (oldest at the top)"
defaults write com.apple.mail DraftsViewerAttributes -dict-add "DisplayInThreadedMode" -string "yes"
defaults write com.apple.mail DraftsViewerAttributes -dict-add "SortedDescending" -string "yes"
defaults write com.apple.mail DraftsViewerAttributes -dict-add "SortOrder" -string "received-date"

echo ""
echo "Disabling inline attachments (just show the icons)"
defaults write com.apple.mail DisableInlineAttachmentViewing -bool true

echo ""
echo "Disabling automatic spell checking"
defaults write com.apple.mail SpellCheckingBehavior -string "NoSpellCheckingEnabled"

###############################################################################
# Terminal & iTerm 2
###############################################################################

echo ""
echo "Enabling UTF-8 ONLY in Terminal.app and setting the Pro theme by default"
defaults write com.apple.terminal StringEncodings -array 4
defaults write com.apple.Terminal "Default Window Settings" -string "Pro"
defaults write com.apple.Terminal "Startup Window Settings" -string "Pro"

echo ""
echo "Don’t display the annoying prompt when quitting iTerm"
defaults write com.googlecode.iterm2 PromptOnQuit -bool false

echo ""
echo "Installing the Solarized Dark theme for iTerm"
open "os/$OS/ext/Solarized Dark.itermcolors"

###############################################################################
# Time Machine
###############################################################################

echo ""
echo "Preventing Time Machine from prompting to use new hard drives as backup volume"
defaults write com.apple.TimeMachine DoNotOfferNewDisksForBackup -bool true

echo ""
echo "Disabling local Time Machine backups"
hash tmutil &> /dev/null && sudo tmutil disablelocal

###############################################################################
# Activity Monitor
###############################################################################

echo ""
echo "Showing the main window when launching Activity Monitor"
defaults write com.apple.ActivityMonitor OpenMainWindow -bool true

echo ""
echo "Visualize CPU usage in the Activity Monitor Dock icon"
defaults write com.apple.ActivityMonitor IconType -int 5

echo ""
echo "Showing all processes in Activity Monitor"
defaults write com.apple.ActivityMonitor ShowCategory -int 0

echo ""
echo "Sorting Activity Monitor results by CPU usage"
defaults write com.apple.ActivityMonitor SortColumn -string "CPUUsage"
defaults write com.apple.ActivityMonitor SortDirection -int 0

###############################################################################
# Disk Utility
###############################################################################

echo ""
echo "Enabling the debug menu in Disk Utility"
defaults write com.apple.DiskUtility DUDebugMenuEnabled -bool true
defaults write com.apple.DiskUtility advanced-image-options -bool true

###############################################################################
# Messages
###############################################################################

echo ""
echo "Disabling automatic emoji substitution (i.e. use plain text smileys)"
defaults write com.apple.messageshelper.MessageController SOInputLineSettings -dict-add "automaticEmojiSubstitutionEnablediMessage" -bool false

echo ""
echo "Disabling smart quotes as it's annoying for messages that contain code"
defaults write com.apple.messageshelper.MessageController SOInputLineSettings -dict-add "automaticQuoteSubstitutionEnabled" -bool false

echo ""
echo "Disabling continuous spell checking"
defaults write com.apple.messageshelper.MessageController SOInputLineSettings -dict-add "continuousSpellCheckingEnabled" -bool false

###############################################################################
# Alfred
###############################################################################

echo ""
echo "Binding ⌘ + Space for Alfred"
defaults write com.runningwithcrayons.Alfred-Preferences hotkey.default -dict key -int 49 mod -int 1048576 string Space

echo ""
echo "Setting Alfred synchronization folder to ~/Dropbox"
defaults write com.runningwithcrayons.Alfred-Preferences syncfolder -string "~/Dropbox/"

echo ""
echo "Changing default Alfred theme to light-large"
defaults write com.runningwithcrayons.Alfred-Preferences appearance.theme -string "alfred.theme.lightlarge"

echo ""
echo "Enabling blur with Alfred's background"
defaults write com.runningwithcrayons.Alfred-2 experimentalBlur -int 5

###############################################################################
# SizeUp
###############################################################################

echo ""
echo "Start SizeUp at login"
defaults write com.irradiatedsoftware.SizeUp StartAtLogin -bool true

echo ""
echo "Don’t show the preferences window on next start"
defaults write com.irradiatedsoftware.SizeUp ShowPrefsOnNextStart -bool false

echo ""
echo "Allow X11 windows to be modified"
defaults write com.irradiatedsoftware.SizeUp X11SupportEnabled -bool true

###############################################################################
# Seil
###############################################################################

echo ""
echo "Mapping caps lock to escape"
defaults write org.pqrs.Seil sysctl -dict \
  enable_capslock -bool true \
  keycode_capslock -int 53

###############################################################################
# Google Chrome
###############################################################################

echo ""
echo "Allowing installation of user scripts via GitHub Gist"
defaults write com.google.Chrome ExtensionInstallSources -array "https://gist.githubusercontent.com/"

echo ""
echo "Using the system-native print preview dialog"
defaults write com.google.Chrome DisablePrintPreview -bool true

echo ""
echo "Setting Chrome to the default web browser"
open -a "Google Chrome" --args --make-default-browser

###############################################################################
# Personal Additions
###############################################################################

if [ is-installed zsh ]; then
	shell="$(brew --prefix)/bin/zsh"

	# Append zsh to the shell list if not already there
	[ sudo grep -Fxq "$shell" /etc/shells ] || echo "$shell" | sudo tee -a /etc/shells > /dev/null

	echo ""
	echo "Changing the default shell to zsh"
	chsh -s "$shell"
fi
