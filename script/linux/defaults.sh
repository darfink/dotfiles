# Some things taken from here:
# http://google.com

echo ""
echo "Speeding up GRUB boot from 10 to 1 second(s)"
sudo sed -i 's/GRUB_TIMEOUT=10/GRUB_TIMEOUT=1/g' /etc/default/grub
sudo update-grub

echo ""
echo "Swapping caps lock and escape button functionality"
dconf write /org/gnome/desktop/input-sources/xkb-options "['caps:swapescape']"

echo ""
echo "Saving screenshots to ~/Screenshots"
gsettings set org.gnome.gnome-screenshot auto-save-directory "file:///home/$USER/Screenshots/"

# Create the directory if it doesn't exist
mkdir -p ~/Screenshots

echo ""
echo "Changing the default wallpaper"
gsettings set org.gnome.desktop.background picture-uri file://$DOTFILES/resource/wallpaper.jpg

echo ""
echo "Changing the terminal theme to Solarized Dark"
source "extra/install.sh" -s dark -p "Default"

if [ is-installed fish ]; then
        fish="$(which fish)"

        # Append fish to the shell list if not already there
        [ sudo grep -Fq "fish" /etc/shells ] || echo "$fish" | sudo tee -a /etc/shells > /dev/null

        echo ""
        echo "Changing default shell to fish"
        chsh -s "$fish"
fi