# Some things taken from here:
# https://github.com/ko91h/dotfiles/blob/master/ubuntu/gsettings

garray() {
        # Prints a gsettings array (e.g ['foo', 'bar'])
        echo "[$(printf "'%s', " "$@" | cut -d ',' -f 1-$#)]"
}

###############################################################################
# System
###############################################################################

echo ""
echo "Speeding up GRUB boot from 10 to 1 second(s)"
sudo sed -i 's/GRUB_TIMEOUT=10/GRUB_TIMEOUT=1/g' /etc/default/grub
sudo update-grub

echo ""
echo "Disabling update notifier"
gsettings set com.ubuntu.update-notifier regular-auto-launch-interval 0

###############################################################################
# Unity & Desktop
###############################################################################

echo ""
echo "Disabling user switching"
gsettings set org.gnome.desktop.lockdown disable-user-switching true
gsettings set org.gnome.desktop.screensaver user-switch-enabled false

echo ""
echo "Disabling actions for auto mounted media"
gsettings set org.gnome.desktop.media-handling automount-open false
gsettings set org.gnome.desktop.media-handling autorun-never true

echo ""
echo "Allowing desktop accelerators to be changed"
gsettings set org.gnome.desktop.interface can-change-accels true

echo ""
echo "Showing clock in 24 hour format"
gsettings set org.gnome.desktop.interface clock-format 24h
gsettings set org.gnome.desktop.interface clock-show-date true

echo ""
echo "Showing date and week in system calendar"
gsettings set org.gnome.shell.clock show-date true
gsettings set org.gnome.shell.calendar show-weekdate true

echo ""
echo "Hiding Bluetooth icon in menu bar"
gsettings set com.canonical.indicator.bluetooth visible false

echo ""
echo "Showing battery usage in percent (%)"
gsettings set com.canonical.indicator.power show-percentage true

echo ""
echo "Enabling cursor blinking appearance"
gsettings set org.gnome.desktop.interface cursor-blink true

echo ""
echo "Hiding language settings in menu bar"
gsettings set org.gnome.desktop.interface show-input-method-menu false
gsettings set org.gnome.desktop.interface show-unicode-menu false

echo ""
echo "Setting desktop theme to Numix"
gsettings set org.gnome.desktop.interface gtk-theme Numix
gsettings set org.gnome.desktop.interface gtk-color-scheme 'selected_bg_color:#729fcf;selected_fg_color:#f9f9f9;'
gsettings set org.gnome.desktop.interface icon-theme Numix-uTouch
gsettings set org.gnome.desktop.wm.preferences theme Numix

unityapps=(
        'application://nautilus.desktop'
        'application://google-chrome.desktop'
        'application://spotify.desktop'
        'application://skype.desktop'
        'application://vlc.desktop'
        'application://gnome-terminal.desktop'
        'application://gnome-control-center.desktop'
        'application://gnome-system-monitor.desktop'
        'unity://running-apps'
        'unity://expo-icon'
        'unity://devices'
)

echo ""
echo "Setting up Unity launcher applications"
gsettings set com.canonical.Unity.Launcher favorites "$(garray "${unityapps[@]}")"

echo ""
echo "Disabling remote content search for Unity Dash"
gsettings set com.canonical.Unity.Lenses remote-content-search none

echo ""
echo "Setting Spotify to the preferred media player"
gsettings set com.canonical.indicator.sound interested-media-players "$(garray spotify.desktop)"
gsettings set com.canonical.indicator.sound preferred-media-players "$(garray spotify.desktop)"

###############################################################################
# Window Management
###############################################################################

echo ""
echo "Enabling visual terminal bell"
gsettings set org.gnome.desktop.wm.preferences audible-bell false
gsettings set org.gnome.desktop.wm.preferences visual-bell true
gsettings set org.gnome.desktop.wm.preferences visual-bell-type frame-flash

echo ""
echo "Setting right click on title bar to collapse/open window"
gsettings set org.gnome.desktop.wm.preferences action-right-click-titlebar toggle-shade

echo ""
echo "Setting middle click on title bar to put window in background"
gsettings set org.gnome.desktop.wm.preferences action-middle-click-titlebar lower

###############################################################################
# Touch pad, mouse, keyboard, Bluetooth accessories, and input
###############################################################################

echo ""
echo "Enabling tap-to-click, two-finger scroll and horizontal scroll for the touch pad"
gsettings set org.gnome.settings-daemon.peripherals.touchpad tap-to-click true
gsettings set org.gnome.settings-daemon.peripherals.touchpad scroll-method two-finger-scrolling
gsettings set org.gnome.settings-daemon.peripherals.touchpad horiz-scroll-enabled true

echo ""
echo "Disabling touch pad natural scrolling"
gsettings set org.gnome.settings-daemon.peripherals.touchpad natural-scroll false

echo ""
echo "Swapping caps lock with escape"
gsettings set org.gnome.desktop.input-sources xkb-options "$(garray caps:swapescape)"

###############################################################################
# Screen
###############################################################################

echo ""
echo "Setting screensaver locking timeout to 5 minutes"
gsettings set org.gnome.desktop.screensaver lock-enabled true
gsettings set org.gnome.desktop.screensaver idle-activation-enabled true
gsettings set org.gnome.desktop.session idle-delay 300

echo ""
echo "Saving screenshots to ~/Screenshots"
gsettings set org.gnome.gnome-screenshot auto-save-directory "file:///home/$USER/Screenshots/"

# Create the directory if it doesn't exist
mkdir -p ~/Screenshots

echo ""
echo "Setting wallpapers to stretch by default"
gsettings set org.gnome.desktop.background picture-options stretched

echo ""
echo "Changing the default wallpaper"
gsettings set org.gnome.desktop.background picture-uri "file://$DOTFILES/ext/wallpaper.jpg"

###############################################################################
# Nautilus
###############################################################################

echo ""
echo "Displaying delete option in Nautilus drop down menu"
gsettings set org.gnome.nautilus.preferences enable-delete true

echo ""
echo "Setting double-click action to open folders"
gsettings set org.gnome.nautilus.preferences click-policy double

echo ""
echo "Setting date format to ISO standard (e.g 2012-06-12 17:34:22)"
gsettings set org.gnome.nautilus.preferences date-format iso

echo ""
echo "Displaying files and folders in list view by default"
gsettings set org.gnome.nautilus.preferences default-folder-viewer list-view

echo ""
echo "Displaying all columns same width and setting increased zoom level for compact view"
gsettings set org.gnome.nautilus.compact-view all-columns-have-same-width true
gsettings set org.gnome.nautilus.compact-view default-zoom-level larger

echo ""
echo "Disabling captions and setting increased zoom level for icon view"
gsettings set org.gnome.nautilus.icon-view captions "$(garray none none none)"
gsettings set org.gnome.nautilus.icon-view default-zoom-level large

listcolumns=(
        'name'
        'size'
        'type'
        'date_modified'
        'permissions'
)

echo ""
echo "Setting list view columns to; name, size, type, date modified, permissions"
gsettings set org.gnome.nautilus.list-view default-column-order "$(garray "${listcolumns[@]}")"
gsettings set org.gnome.nautilus.list-view default-visible-columns "$(garray "${listcolumns[@]}")"
gsettings set org.gnome.nautilus.list-view default-zoom-level smaller

echo ""
echo "Enabling status bar and side bar (with “places” displayed by default)"
gsettings set org.gnome.nautilus.window-state side-pane-view places
gsettings set org.gnome.nautilus.window-state start-with-status-bar true
gsettings set org.gnome.nautilus.window-state start-with-sidebar true

###############################################################################
# GNOME Terminal
###############################################################################

echo ""
echo "Changing the terminal theme to Solarized Dark"
source "os/$OS/ext/gnome-terminal-colors-solarized/set_dark.sh"

###############################################################################
# GEdit
###############################################################################

echo ""
echo "Disabling automatic gedit backups"
gsettings set org.gnome.gedit.preferences.editor create-backup-copy false

echo ""
echo "Displaying line numbers"
gsettings set org.gnome.gedit.preferences.editor display-line-numbers true

echo ""
echo "Using 2 spaces for indentation by default"
gsettings set org.gnome.gedit.preferences.editor insert-spaces true
gsettings set org.gnome.gedit.preferences.editor tabs-size 2

echo ""
echo "Highlighting matching brackets"
gsettings set org.gnome.gedit.preferences.editor bracket-matching true
gsettings set org.gnome.gedit.preferences.editor right-margin-position 120

echo ""
echo "Showing status bar and tabs"
gsettings set org.gnome.gedit.preferences.ui statusbar-visible true
gsettings set org.gnome.gedit.preferences.ui notebook-show-tabs-mode always

###############################################################################
# GPaste
###############################################################################

echo ""
echo "Enabling system clipboard integration with GPaste"
gsettings set org.gnome.GPaste primary-to-history true
gsettings set org.gnome.GPaste synchronize-clipboards true

echo ""
echo "Setting history size to 100 and display count to 19"
gsettings set org.gnome.GPaste max-history-size 100
gsettings set org.gnome.GPaste max-displayed-history-size 19

###############################################################################
# Personal Additions
###############################################################################

# Setup the default file type associations
sh "os/$OS/ext/extensions.sh"

if [ is-installed zsh ]; then
        shell="$(which zsh)"

        # Append zsh to the shell list if not already there
        [ sudo grep -Fxq "$shell" /etc/shells ] || echo "$shell" | sudo tee -a /etc/shells > /dev/null

        echo ""
        echo "Changing the default shell to zsh"
        chsh -s "$shell"
fi

