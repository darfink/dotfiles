# Adds a path if it doesn't exist and is valid
add_path() {
  if [ -d "$1" ] && [[ ":$PATH:" != *":$1:"* ]]; then
    PATH="${PATH:+"$PATH:"}$1"
  fi
}

if [ $OS = 'osx' ]; then
        # We want to use the GNU utility applications by default
        add_path "$(brew --prefix coreutils)/libexec/gnubin"

        # Add Homebrew directories
        add_path /usr/local/sbin
        add_path /usr/local/bin
fi

# Add binary folders to path
add_path "$DOTFILES/os/$OS/bin"
add_path "$HOME/bin"