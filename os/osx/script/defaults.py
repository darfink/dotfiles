# -*- coding: utf-8 -*-

from invoke import task, Collection, run

from ..utils import info

#TODO: iTerm, Alfred, add iTerm shortcut to Finder, normalize task names
defaults = {
  'System': {
    'disabling sound effects on boot': [
      'sudo nvram SystemAudioVolume=0',
    ],
    'disabling OS X Gate Keeper': [
      'sudo spctl --master-disable',
      'sudo defaults write /var/db/SystemPolicy-prefs.plist enabled -string no',
      'defaults write com.apple.LaunchServices LSQuarantine -bool false',
    ],
    'revealing IP address, hostname, OS version, etc when clicking the clock in the login window': [
      'sudo defaults write /Library/Preferences/com.apple.loginwindow AdminHostInfo HostName',
    ],
    'disabling the guest user account': [
      'sudo defaults write /Library/Preferences/SystemConfiguration/com.apple.smb.server AllowGuestAccess -bool NO',
      'sudo defaults write /Library/Preferences/com.apple.AppleFileServer guestAccess -bool NO',
      'sudo defaults write /Library/Preferences/com.apple.loginwindow GuestEnabled -bool NO',
    ],
    'restarting automatically if the computer freezes': [
      'sudo systemsetup -setrestartfreeze on',
    ],
    'setting login window text': [
      'sudo defaults write /Library/Preferences/com.apple.loginwindow LoginwindowText -string "I\'m sorry Dave, I\'m afraid I can\'t do that"',
    ],
  },
  'User Experience': {
    'disabling system-wide resume upon login': [
      'defaults write com.apple.systempreferences NSQuitAlwaysKeepsWindows -bool false',
    ],
    'disabling automatic termination of inactive apps': [
      'defaults write NSGlobalDomain NSDisableAutomaticTermination -bool true',
    ],
    'setting scrollbars behavior to automatic': [
      'defaults write NSGlobalDomain AppleShowScrollBars -string "Automatic"',
    ],
    'automatically quit the print app once the print jobs complete': [
      'defaults write com.apple.print.PrintingPrefs "Quit When Finished" -bool true',
    ],
    'saving to disk (instead of iCloud) by default': [
      'defaults write NSGlobalDomain NSDocumentSaveNewDocumentsToCloud -bool false',
    ],
    'disabling the "application crashed" dialog': [
      'defaults write com.apple.CrashReporter DialogType none',
    ],
    'setting the update frequency to once every week': [
      'defaults write com.apple.SoftwareUpdate ScheduleFrequency -int 7',
    ],
    'disabling smart quotes and dashes as they are annoying when typing code': [
      'defaults write NSGlobalDomain NSAutomaticQuoteSubstitutionEnabled -bool false',
      'defaults write NSGlobalDomain NSAutomaticDashSubstitutionEnabled -bool false',
    ],
  },
  'User Interface': {
    'setting highlight color to #CC99CC': [
      'defaults write NSGlobalDomain AppleHighlightColor -string "0.600000 0.800000 0.600000"',
    ],
    'setting sidebar icon size to medium': [
      'defaults write NSGlobalDomain NSTableViewDefaultSizeMode -int 2',
    ],
    'expanding the save panel by default': [
      'defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true',
      'defaults write NSGlobalDomain PMPrintingExpandedStateForPrint -bool true',
      'defaults write NSGlobalDomain PMPrintingExpandedStateForPrint2 -bool true',
    ],
    'showing battery life percentage': [
      'defaults write com.apple.menuextra.battery ShowPercent -string "YES"',
    ],
    #TODO: Find some badass picture
    #'changing the default wallpaper': [
    #  'sqlite3 "$HOME/Library/Application Support/Dock/desktoppicture.db" "UPDATE data SET value = \'$DOTFILES/ext/wallpaper.jpg\'"',
    #],
    'displaying ASCII control characters using caret notation in standard text views': [
      'defaults write NSGlobalDomain NSTextShowsControlCharacters -bool true',
    ],
    'setting help viewer windows to non-floating mode': [
      'defaults write com.apple.helpviewer DevMode -bool true',
    ],
    'adding hotkey to quickly switch to Dark Mode (ctrl+opt+⌘+t)': [
      'sudo defaults write /Library/Preferences/.GlobalPreferences.plist _HIEnableThemeSwitchHotKey -bool true',
    ]
  },
  'Input/Output Settings': {
    'increasing sound quality for Bluetooth headphones/headsets': [
      'defaults write com.apple.BluetoothAudioAgent "Apple Bitpool Min (editable)" -int 40',
    ],
    'enabling full keyboard access for all controls (e.g. enable Tab in modal dialogs)': [
      'defaults write NSGlobalDomain AppleKeyboardUIMode -int 3',
    ],
    'disabling press-and-hold for keys in favor of key repeat': [
      'defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false',
    ],
    'setting a blazingly fast keyboard repeat rate': [
      'defaults write NSGlobalDomain KeyRepeat -int 2',
    ],
    'setting trackpad & mouse speed to a reasonable number': [
      'defaults write -g com.apple.trackpad.scaling 0.875',
      'defaults write -g com.apple.mouse.scaling 1.5',
    ],
    'enabling tap to click (Trackpad) for this user and for the login screen': [
      'defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking -bool true',
      'defaults -currentHost write NSGlobalDomain com.apple.mouse.tapBehavior -int 1',
      'defaults write NSGlobalDomain com.apple.mouse.tapBehavior -int 1',
    ],
    'enabling map bottom right corner (Trackpad) to right-click': [
      'defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadCornerSecondaryClick -int 2',
      'defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadRightClick -bool true',
      'defaults -currentHost write NSGlobalDomain com.apple.trackpad.trackpadCornerClickBehavior -int 1',
      'defaults -currentHost write NSGlobalDomain com.apple.trackpad.enableSecondaryClick -bool true',
    ],
    'Disabling “natural” (Lion-style) scrolling': [
      'defaults write NSGlobalDomain com.apple.swipescrolldirection -bool false',
    ],
    'enabling scroll gesture with the Ctrl (^) modifier key to zoom': [
      'defaults write com.apple.universalaccess closeViewScrollWheelToggle -bool true',
      'defaults write com.apple.universalaccess HIDScrollZoomModifierMask -int 262144',
    ],
    'following the keyboard focus while zoomed in': [
      'defaults write com.apple.universalaccess closeViewZoomFollowsFocus -bool true',
    ],
  },
  'Localization': {
    'setting language and text format to English': [
      'defaults write NSGlobalDomain AppleLanguages -array "en" "sv"',
      'defaults write NSGlobalDomain AppleLocale -string "en_SE@currency=SEK"',
    ],
    'using the metric system instead of the imperial': [
      'defaults write NSGlobalDomain AppleMeasurementUnits -string "Centimeters"',
      'defaults write NSGlobalDomain AppleMetricUnits -bool true',
    ],
    'using dot notation as decimal separator': [
      'defaults write NSGlobalDomain AppleICUNumberSymbols -dict 0 "."',
    ],
    'using swedish date & time format with English language': [
      'defaults write NSGlobalDomain AppleICUDateFormatStrings -dict 1 "y-MM-dd" 2 "d MMM y" 3 "d MMMM y"',
      'defaults write com.apple.menuextra.clock DateFormat -string "EEE HH:mm"',
    ],
    'setting the timezone to Swedish (Stockholm)': [
      'sudo systemsetup -settimezone "Europe/Stockholm" > /dev/null',
      'sudo systemsetup -setnetworktimeserver "time.euro.apple.com" > /dev/null',
      'sudo systemsetup -setusingnetworktime on > /dev/null',
    ],
  },
  'Screen': {
    'requiring password immediately (5s) after sleep or screen saver begins': [
      'defaults write com.apple.screensaver askForPassword -int 1',
      'defaults write com.apple.screensaver askForPasswordDelay -int 5',
    ],
    'saving screenshots to ~/Screenshots': [
      'defaults write com.apple.screencapture location -string "$HOME/Screenshots"',
      # Create the directory if it doesn't exist
      'mkdir -p ~/Screenshots',
    ],
    'saving screenshots in PNG format (other options: BMP, GIF, JPG, PDF, TIFF)': [
      'defaults write com.apple.screencapture type -string "png"',
    ],
    'enabling subpixel font rendering on non-Apple LCDs': [
      'defaults write NSGlobalDomain AppleFontSmoothing -int 2',
    ],
    'showing displays in menu bar': [
      'defaults write com.apple.airplay showInMenuBarIfPresent -bool false',
    ],
    'enabling HiDPI display modes (requires restart)': [
      'sudo defaults write /Library/Preferences/com.apple.windowserver DisplayResolutionEnabled -bool true',
    ]
  },
  'Power Management': {
    'disabling the sudden motion sensor (not useful for SSDs)': [
      'sudo pmset -a sms 0',
    ],
    'removing the sleep image file to save disk space': [
      'sudo chflags nouchg /Private/var/vm/sleepimage',
      'sudo rm /Private/var/vm/sleepimage',
      'sudo touch /Private/var/vm/sleepimage',
      'sudo chflags uchg /Private/var/vm/sleepimage',
    ],
    'speeding up wake from sleep from 24 hours to an hour': [
      # http://www.cultofmac.com/221392/quick-hack-speeds-up-retina-macbooks-wake-from-sleep-os-x-tips/
      'sudo pmset -a standbydelay 86400',
    ],
    'disabling hibernation (only use sleep mode)': [
      'sudo pmset -a hibernatemode 0',
    ],
    'setting (battery) display sleep to 5 minutes and (disk) sleep to 10 minutes': [
      'sudo pmset -b displaysleep 5',
      'sudo pmset -b disksleep 10',
      'sudo pmset -b sleep 10',
    ],
    'setting (charger) display sleep to 15 minutes and disabling disk sleep': [
      'sudo pmset -c displaysleep 15',
      'sudo pmset -c disksleep 0',
      'sudo pmset -c sleep 0',
    ],
    'setting keyboard illumination to turn off when computer is not used for 5 minutes': [
      'defaults write com.apple.BezelServices kDimTime -int 300',
    ],
  },
  'Dock': {
    'enabling highlight hover effect for the grid view of a stack': [
      'defaults write com.apple.dock mouse-over-hilite-stack -bool true',
    ],
    'changing minimize/maximize window effect': [
      'defaults write com.apple.dock mineffect -string "scale"',
    ],
    'minimizing windows into their own application icon': [
      'defaults write com.apple.dock mineffect -string "scale"',
    ],
    'setting the icon size of items to 56 pixels': [
      'defaults write com.apple.dock tilesize -int 56',
    ],
    'enabling spring loading for all items': [
      'defaults write com.apple.dock enable-spring-load-actions-on-all-items -bool true',
    ],
    'showing indicator lights for open applications': [
      'defaults write com.apple.dock show-process-indicators -bool true',
    ],
    'disabling animations when opening applications': [
      'defaults write com.apple.dock launchanim -bool false',
    ],
    'enabling auto-hide and removing the display delay': [
      'defaults write com.apple.dock autohide -bool true',
      'defaults write com.apple.dock autohide-delay -float 0',
      'defaults write com.apple.dock autohide-time-modifier -float 0',
    ],
    'making icons of hidden applications translucent': [
      'defaults write com.apple.dock showhidden -bool true',
    ],
    'setting up default items': [
      'dockutil --remove all',

      # The default applications
      'dockutil --add "/Applications/Google Chrome.app"',
      'dockutil --add "/Applications/Messages.app"',
      'dockutil --add "/Applications/Skype.app"',
      'dockutil --add "/Applications/Dash.app"',
      'dockutil --add "/Applications/Mail.app"',
      'dockutil --add "/Applications/Spotify.app"',
      'dockutil --add "/Applications/Xcode.app"',
      'dockutil --add "/Applications/Popcorn-Time.app"',
      'dockutil --add "/Applications/MacVim.app"',
      'dockutil --add "/Applications/iTerm.app"',

      # ... and some default folders
      'dockutil --add "/Applications" --view list --display folder --sort name',
      'dockutil --add "~/Downloads" --view grid --display stack --sort dateadded',
    ],
  },
  'Hot Corners': {
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
    'top left screen corner → Mission Control': [
      'defaults write com.apple.dock wvous-tl-corner -int 2',
      'defaults write com.apple.dock wvous-tl-modifier -int 0',
    ],
    'top right screen corner → Mission Control': [
      'defaults write com.apple.dock wvous-tr-corner -int 4',
      'defaults write com.apple.dock wvous-tr-modifier -int 0',
    ],
    'bottom left screen corner → Mission Control': [
      'defaults write com.apple.dock wvous-bl-corner -int 5',
      'defaults write com.apple.dock wvous-bl-modifier -int 0',
    ],
  },
  'Mission Control': {
    'speeding up animations': [
      'defaults write com.apple.dock expose-animation-duration -float 0.1',
    ],
    'grouping windows by application': [
      'defaults write com.apple.dock expose-group-by-app -bool true',
    ],
    'disabling hotkeys (makes F9-F12 usable)': [
      # Application window
      'defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 33 "{ enabled = 0; }"',
      'defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 35 "{ enabled = 0; }"',
      # Show desktop
      'defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 36 "{ enabled = 0; }"',
      'defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 37 "{ enabled = 0; }"',
      # Mission control
      'defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 32 "{ enabled = 0; }"',
      'defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 34 "{ enabled = 0; }"',
      # Dashboard
      'defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 62 "{ enabled = 0; }"',
      'defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 63 "{ enabled = 0; }"',
    ],
    'disabling most-recently-used (MRU) ordering of spaces': [
      'defaults write com.apple.dock mru-spaces -bool false',
    ]
  },
  'Spotlight': {
    'hiding the spotlight tray icon': [
      'sudo chmod 600 /System/Library/CoreServices/Search.bundle/Contents/MacOS/Search',
    ],
    'disabling indexing for mounted volumes': [
      'sudo defaults write /.Spotlight-V100/VolumeConfiguration Exclusions -array "/Volumes"',
    ],
    'disabling display of disclaimer text': [
      'defaults write com.apple.Spotlight useCount -int 3',
      'defaults write com.apple.Spotlight showedFTE -bool YES',
    ],
    'disabling shortcuts in favor of Alfred': [
      'defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 64 "{ enabled = 0; value = { parameters = ( 32, 49, 1048576); type = standard; }; }"',
      'defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 65 "{ enabled = 0; value = { parameters = ( 32, 49, 1048576); type = standard; }; }"',
    ],
    'ensuring indexing is enabled for the main volume': [
      'sudo mdutil -i on / > /dev/null',
    ]
  },
  'Finder': {
    'disabling window animations and Get Info animations': [
      'defaults write com.apple.finder DisableAllAnimations -bool true',
    ],
    'setting home (~) as the default finder location': [
      'defaults write com.apple.finder NewWindowTarget -string "PfHm"',
      'defaults write com.apple.finder NewWindowTargetPath -string "file://${HOME}/"',
    ],
    'showing icons for hard drives server and removable media on the desktop': [
      'defaults write com.apple.finder ShowExternalHardDrivesOnDesktop -bool true',
    ],
    'showing all filename extensions': [
      'defaults write NSGlobalDomain AppleShowAllExtensions -bool true',
    ],
    'showing status and path bar': [
      'defaults write com.apple.finder ShowStatusBar -bool true',
      'defaults write com.apple.finder ShowPathbar -bool true',
    ],
    'preferring to group by kind': [
      'defaults write com.apple.finder FXPreferredGroupBy -string "Kind"',
    ],
    'allowing text selection in Quick Look/Preview': [
      'defaults write com.apple.finder QLEnableTextSelection -bool true',
    ],
    'displaying full POSIX path in the window title': [
      'defaults write com.apple.finder _FXShowPosixPathInTitle -bool true',
    ],
    'setting the default search scope to the current folder': [
      'defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"',
    ],
    'disabling warning when changing a file extension': [
      'defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false',
    ],
    'using list view in all windows by default': [
      'defaults write com.apple.finder FXPreferredViewStyle "Nlsv"',
    ],
    'expanding "General", "Open with" and "Sharing & Permissions" info panes': [
      'defaults write com.apple.finder FXInfoPanesExpanded -dict General -bool true OpenWith -bool true Privileges -bool true',
    ],
    'disabling the warning when emptying the Trash': [
      'defaults write com.apple.finder WarnOnEmptyTrash -bool false',
    ],
    'disabling the creation of .DS_Store files on network volumes': [
      'defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true',
    ],
    'disabling disk image verification': [
      'defaults write com.apple.frameworks.diskimages skip-verify -bool true',
      'defaults write com.apple.frameworks.diskimages skip-verify-locked -bool true',
      'defaults write com.apple.frameworks.diskimages skip-verify-remote -bool true',
    ],
    'showing item info near icons on the desktop and in other icon views': [
      '/usr/libexec/PlistBuddy -c "Set :DesktopViewSettings:IconViewSettings:showItemInfo true" ~/Library/Preferences/com.apple.finder.plist',
      '/usr/libexec/PlistBuddy -c "Set :FK_StandardViewSettings:IconViewSettings:showItemInfo true" ~/Library/Preferences/com.apple.finder.plist',
      '/usr/libexec/PlistBuddy -c "Set :StandardViewSettings:IconViewSettings:showItemInfo true" ~/Library/Preferences/com.apple.finder.plist',
    ],
    'enabling snap-to-grid for icons on the desktop and in other icon views': [
      '/usr/libexec/PlistBuddy -c "Set :DesktopViewSettings:IconViewSettings:arrangeBy grid" ~/Library/Preferences/com.apple.finder.plist',
      '/usr/libexec/PlistBuddy -c "Set :FK_StandardViewSettings:IconViewSettings:arrangeBy grid" ~/Library/Preferences/com.apple.finder.plist',
      '/usr/libexec/PlistBuddy -c "Set :StandardViewSettings:IconViewSettings:arrangeBy grid" ~/Library/Preferences/com.apple.finder.plist',
    ],
    'setting the icon size and grid spacing to 64 pixels': [
      '/usr/libexec/PlistBuddy -c "Set :DesktopViewSettings:IconViewSettings:gridSpacing 64" ~/Library/Preferences/com.apple.finder.plist',
      '/usr/libexec/PlistBuddy -c "Set :DesktopViewSettings:IconViewSettings:iconSize 64" ~/Library/Preferences/com.apple.finder.plist',
    ],
    'setting up the toolbar with default items': [
      # Remove all currently assigned toolbar items
      '/usr/libexec/PlistBuddy -c \'Delete "NSToolbar Configuration Browser:TB Item Identifiers"\' ~/Library/Preferences/com.apple.finder.plist',
      '/usr/libexec/PlistBuddy -c \'Add "NSToolbar Configuration Browser:TB Item Identifiers" array\' ~/Library/Preferences/com.apple.finder.plist',

      # Add each item to the toolbar
      '/usr/libexec/PlistBuddy -c \'Add "NSToolbar Configuration Browser:TB Item Identifiers:0" string "com.apple.finder.BACK"\' ~/Library/Preferences/com.apple.finder.plist',
      '/usr/libexec/PlistBuddy -c \'Add "NSToolbar Configuration Browser:TB Item Identifiers:1" string "com.apple.finder.ARNG"\' ~/Library/Preferences/com.apple.finder.plist',
      '/usr/libexec/PlistBuddy -c \'Add "NSToolbar Configuration Browser:TB Item Identifiers:2" string "com.apple.finder.ACTN"\' ~/Library/Preferences/com.apple.finder.plist',
      '/usr/libexec/PlistBuddy -c \'Add "NSToolbar Configuration Browser:TB Item Identifiers:3" string "NSToolbarSpaceItem"\' ~/Library/Preferences/com.apple.finder.plist',
      '/usr/libexec/PlistBuddy -c \'Add "NSToolbar Configuration Browser:TB Item Identifiers:4" string "com.apple.finder.SWCH"\' ~/Library/Preferences/com.apple.finder.plist',
      '/usr/libexec/PlistBuddy -c \'Add "NSToolbar Configuration Browser:TB Item Identifiers:5" string "com.apple.finder.SRCH"\' ~/Library/Preferences/com.apple.finder.plist',
      '/usr/libexec/PlistBuddy -c \'Add "NSToolbar Configuration Browser:TB Item Identifiers:6" string "NSToolbarFlexibleSpaceItem"\' ~/Library/Preferences/com.apple.finder.plist',
      '/usr/libexec/PlistBuddy -c \'Add "NSToolbar Configuration Browser:TB Item Identifiers:7" string "com.apple.finder.loc "\' ~/Library/Preferences/com.apple.finder.plist',
      '/usr/libexec/PlistBuddy -c \'Add "NSToolbar Configuration Browser:TB Item Identifiers:8" string "com.apple.finder.CNCT"\' ~/Library/Preferences/com.apple.finder.plist',
      '/usr/libexec/PlistBuddy -c \'Add "NSToolbar Configuration Browser:TB Item Identifiers:9" string "com.apple.finder.TRSH"\' ~/Library/Preferences/com.apple.finder.plist',

      # Remove the custom defined items
      '/usr/libexec/PlistBuddy -c \'Delete "NSToolbar Configuration Browser:TB Item Plists"\' ~/Library/Preferences/com.apple.finder.plist',
      '/usr/libexec/PlistBuddy -c \'Add "NSToolbar Configuration Browser:TB Item Plists" dict\' ~/Library/Preferences/com.apple.finder.plist',

      # Add the terminal opener item
      '/usr/libexec/PlistBuddy -c \'Add "NSToolbar Configuration Browser:TB Item Plists:7" dict\' ~/Library/Preferences/com.apple.finder.plist',
      '/usr/libexec/PlistBuddy -c "Add \'NSToolbar Configuration Browser:TB Item Plists:7:_CFURLString\' string \'file://$DOTFILES/os/osx/ext/open-terminal.app\'" ~/Library/Preferences/com.apple.finder.plist',
      '/usr/libexec/PlistBuddy -c \'Add "NSToolbar Configuration Browser:TB Item Plists:7:_CFURLStringType" integer 15\' ~/Library/Preferences/com.apple.finder.plist',
    ],
    'making the home library folder visible': [
      'chflags nohidden ~/Library',
    ],
    'enabling spring loading for directories': [
      'defaults write NSGlobalDomain com.apple.springing.enabled -bool true',
      'defaults write NSGlobalDomain com.apple.springing.delay -float 0.5',
    ],
    'enabling AirDrop over Ethernet and on unsupported Macs running Lion': [
      'defaults write com.apple.NetworkBrowser BrowseAllInterfaces -bool true',
    ],
  },
  'Safari': {
    'disabling thumbnail cache for History and Top Sites': [
      'defaults write com.apple.Safari DebugSnapshotsUpdatePolicy -int 2',
    ],
    'disabling automatic opening of "safe" files': [
      'defaults write com.apple.Safari AutoOpenSafeDownloads -bool false',
    ],
    'enabling the debug menu': [
      'defaults write com.apple.Safari IncludeInternalDebugMenu -bool true',
    ],
    'making search banners default to Contains instead of Starts With': [
      'defaults write com.apple.Safari FindOnPageMatchesWordStartsOnly -bool false',
    ],
    'removing useless icons from the bookmarks bar': [
      'defaults write com.apple.Safari ProxiesInBookmarksBar "()"',
    ],
    'enabling Backspace hotkey to go to the previous page in history': [
      'defaults write com.apple.Safari com.apple.Safari.ContentPageGroupIdentifier.WebKit2BackspaceKeyNavigationEnabled -bool true',
    ],
    'enabling the Develop menu and the Web Inspector': [
      'defaults write com.apple.Safari IncludeDevelopMenu -bool true',
      'defaults write com.apple.Safari WebKitDeveloperExtrasEnabledPreferenceKey -bool true',
      'defaults write com.apple.Safari "com.apple.Safari.ContentPageGroupIdentifier.WebKit2DeveloperExtrasEnabled" -bool true',
    ],
    'disabling search queries being sent to Apple': [
      'defaults write com.apple.Safari UniversalSearchEnabled -bool false',
      'defaults write com.apple.Safari SuppressSearchSuggestions -bool true',
    ]
  },
  'Webkit': {
    'adding a context menu item for showing the Web Inspector in web views': [
      'defaults write NSGlobalDomain WebKitDeveloperExtras -bool true',
    ]
  },
  'Mail': {
    'setting email addresses to copy as "foo@example.com" instead of "Foo Bar <foo@example.com>"': [
      'defaults write com.apple.mail AddressesIncludeNameOnPasteboard -bool false',
    ],
    'adding ⌘ + Enter keyboard shortcut to send an email': [
      'defaults write com.apple.mail NSUserKeyEquivalents -dict-add "Send" -string "@\\U21a9"',
    ],
    'displaying emails in threaded mode, sorted by date (oldest at the top)': [
      'defaults write com.apple.mail DraftsViewerAttributes -dict-add "DisplayInThreadedMode" -string "yes"',
      'defaults write com.apple.mail DraftsViewerAttributes -dict-add "SortedDescending" -string "yes"',
      'defaults write com.apple.mail DraftsViewerAttributes -dict-add "SortOrder" -string "received-date"',
    ],
    'disabling inline attachments (just show the icons)': [
      'defaults write com.apple.mail DisableInlineAttachmentViewing -bool true',
    ]
  },
  'Terminal': {
    'enabling UTF-8 so it is always used': [
      'defaults write com.apple.terminal StringEncodings -array 4',
    ],
    'settings the Pro terminal theme as default': [
      'defaults write com.apple.Terminal "Default Window Settings" -string "Pro"',
      'defaults write com.apple.Terminal "Startup Window Settings" -string "Pro"',
    ]
  },
  'Time Machine': {
    'disabling prompts to use new hard drives as backup volume': [
      'defaults write com.apple.TimeMachine DoNotOfferNewDisksForBackup -bool true',
    ],
    'disabling local time machine backups': [
      'hash tmutil &> /dev/null && sudo tmutil disablelocal',
    ]
  },
  'Disk Utility': {
    'enabling the debug menu': [
      'defaults write com.apple.DiskUtility DUDebugMenuEnabled -bool true',
      'defaults write com.apple.DiskUtility advanced-image-options -bool true',
    ],
  },
  'TextEdit': {
    'using plain text mode for new documents': [
      'defaults write com.apple.TextEdit RichText -int 0',
    ],
    'always opening and saving files as UTF-8': [
      'defaults write com.apple.TextEdit PlainTextEncoding -int 4',
      'defaults write com.apple.TextEdit PlainTextEncodingForWrite -int 4',
    ]
  },
  'Messages': {
    'disabling automatic emoji substitution': [
      'defaults write com.apple.messageshelper.MessageController SOInputLineSettings -dict-add "automaticEmojiSubstitutionEnablediMessage" -bool false',
    ],
    'disabling smart quotes as it makes it impossible to write code': [
      'defaults write com.apple.messageshelper.MessageController SOInputLineSettings -dict-add "automaticQuoteSubstitutionEnabled" -bool false',
    ],
    'disabling continuous spell checking': [
      'defaults write com.apple.messageshelper.MessageController SOInputLineSettings -dict-add "continuousSpellCheckingEnabled" -bool false',
    ]
  },
  'XQuartz': {
    'enabling X11 clipboard synchronization': [
      'defaults write org.macosforge.xquartz.X11 sync_clipboard_to_pasteboard -bool true',
      'defaults write org.macosforge.xquartz.X11 sync_pasteboard -bool true',
      'defaults write org.macosforge.xquartz.X11 sync_pasteboard_to_clipboard -bool true',
      'defaults write org.macosforge.xquartz.X11 sync_pasteboard_to_primary -bool true',
      'defaults write org.macosforge.xquartz.X11 sync_primary_on_select -bool false',
    ],
    'setting zsh to the default X11 shell': [
      'defaults write org.macosforge.xquartz.X11 login_shell -string "$(brew --prefix)/bin/zsh"',
    ]
  },
  'iTerm': {
    'disabling prompt when quitting iTerm': [
      'defaults write com.googlecode.iterm2 PromptOnQuit -bool false'
    ],
    'hiding excessive UI elements for sleeker experience': [
      'defaults write com.googlecode.iterm2 HideActivityIndicator -bool true',
      'defaults write com.googlecode.iterm2 HideMenuBarInFullscreen -bool true',
      'defaults write com.googlecode.iterm2 HideTab -bool true',
      'defaults write com.googlecode.iterm2 UseBorder -bool false',
      'defaults write com.googlecode.iterm2 WindowStyle -int 0',
      'defaults write com.googlecode.iterm2 ShowPaneTitles -bool false',
    ],
    'disabling Lion style full screen mode': [
      'defaults write com.googlecode.iterm2 UseLionStyleFullscreen -bool false'
    ]
  },
  'Skype': {
    'disabling login greeting, welcome tour and dialpad': [
      'defaults write com.skype.skype SKDisableWelcomeTour -bool true',
      'defaults write com.skype.skype SKShowWelcomeOnLogin -bool false',
      'defaults write com.skype.skype ShowDialpadOnLogin -bool false',
    ],
    'setting default country code to Swedish': [
      'defaults write com.skype.skype SKDefaultPSTNCountryCode -string "se"',
    ],
    'disabling recipient notification when writing': [
      'defaults write com.skype.skype SKChatReportMeTyping -bool false',
    ],
    'hiding the skype dialog during calls': [
      'defaults write com.skype.skype CallsMonitorEnabled -bool false',
    ],
    'enabling developer extras': [
      'defaults write com.skype.skype DisableWebKitDeveloperExtras -bool false',
      'defaults write com.skype.skype IncludeDebugMenu -bool true',
    ],
  },
  'Seil': {
    'mapping caps lock to escape': [
      'defaults -currentHost write -g "com.apple.keyboard.modifiermapping.1452-566-0" -array "<dict><key>HIDKeyboardModifierMappingDst</key><integer>-1</integer><key>HIDKeyboardModifierMappingSrc</key><integer>0</integer></dict>"',
      'defaults -currentHost write -g "com.apple.keyboard.modifiermapping.1452-544-0" -array "<dict><key>HIDKeyboardModifierMappingDst</key><integer>-1</integer><key>HIDKeyboardModifierMappingSrc</key><integer>0</integer></dict>"',
      'defaults write org.pqrs.Seil sysctl -dict enable_capslock -bool true keycode_capslock -int 53',
    ]
  },
  'Google Chrome': {
    'allowing installation of user scripts via GitHub Gist': [
      'defaults write com.google.Chrome ExtensionInstallSources -array "https://gist.githubusercontent.com/"'
    ],
    'using the system-native print preview dialog': [
      'defaults write com.google.Chrome DisablePrintPreview -bool true'
    ],
    'setting Chrome to the default web browser': [
      'open -a "Google Chrome" --args --make-default-browser'
    ]
  },
  'Transmission': {
    'using "~/Downloads/Incomplete" to store incomplete downloads': [
      'defaults write org.m0k.transmission UseIncompleteDownloadFolder -bool true',
      'mkdir -p ~/Downloads/Incomplete',
      'defaults write org.m0k.transmission IncompleteDownloadFolder -string "$HOME/Downloads/Incomplete"',
    ],
    'disabling prompts for confirmation before downloading': [
      'defaults write org.m0k.transmission DownloadAsk -bool false',
    ],
    'trashing original torrent files': [
      'defaults write org.m0k.transmission DeleteOriginalTorrent -bool true',
    ],
    'hiding the donation message': [
      'defaults write org.m0k.transmission WarningDonate -bool false',
    ],
    'hiding the legal disclaimer': [
      'defaults write org.m0k.transmission WarningLegal -bool false',
    ],
    'disabling quit and download prompt': [
      'defaults write org.m0k.transmission CheckRemoveDownloading -bool true',
      'defaults write org.m0k.transmission CheckQuit -bool false',
    ],
  },
}

ns = Collection()

for category, actions in defaults.iteritems():
  def context(category, actions):
    def default():
      info('============ {} ============'.format(category))
      for action, tweaks in actions.iteritems():
        info('• {}'.format(action))
        for tweak in tweaks:
          run(tweak)
    return default
  ns.add_task(task(context(category, actions), name=category))
