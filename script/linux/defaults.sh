# Some things taken from here:
# http://google.com

echo ""
echo "Speeding up GRUB boot from 10 to 1 second(s)"
sudo sed -i 's/GRUB_TIMEOUT=10/GRUB_TIMEOUT=1/g' /etc/default/grub
sudo update-grub

echo ""
echo "Saving screenshots to ~/Screenshots"
gsettings set org.gnome.gnome-screenshot auto-save-directory "file:///home/$USER/Screenshots/"

echo ""
echo "Changing the terminal theme to Solarized Dark"
bash "$dir/extra/install.sh" -s dark -p "Default"

if dpkg-query -Wf'${db:Status-abbrev}' 'fish' 2>/dev/null | grep -q '^i'; then
        fish="/usr/bin/fish"

        # Append fish to the shell list if not already there
        [ sudo grep -Fq "fish" /etc/shells ] || echo "$fish" | sudo tee -a /etc/shells > /dev/null

        echo ""
        echo "Changing default shell to fish"
        chsh -s "$fish"
fi