#!/usr/bin/env sh

info() {
  printf "  [ \033[00;34m..\033[0m ] $1"
}

user() {
  printf "\r  [ \033[0;33m?\033[0m ] $1 "
}

fail() {
  printf "\r\033[2K  [\033[0;31mFAIL\033[0m] $1\n"
}

is_command() {
  hash "$1" 2> /dev/null
}

install_prerequisites() {
  # TODO: Check for git here!!!

  if ! is_command easy_install; then
    # OS X is bundled with this, so it never reaches here
    info 'dependency: installing easy_install'
    sudo apt-get install -y python-setuptools || exit 1
  fi

  if ! is_command invoke; then
    info 'dependency: installing invoke'
    sudo easy_install invoke || exit 1
  fi
}

prompt_directory() {
  local directory

  while true; do
    user 'specify installation directory (press [Enter] for ~/.dotfiles):'
    read directory

    if [ -z "$directory" ]; then
      directory="$HOME/.dotfiles"
    fi

    # Attempt to create the directory
    mkdir -p "$directory" 2> /dev/null && break
    fail "could not create directory: $directory"
  done

  # Ensure it is an absolute path
  echo "$(readlink -f "$directory")"
}

if [ ! -f "$HOME/.dotlock" ]; then
  # Ask for the administrator password upfront
  sudo -v

  # Keep-alive: update existing `sudo` time stamp until `install.sh` has finished
  while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

  info 'installing dotfiles for the first time'

  if ! install_prerequisites; then
    fail 'could not install prerequisites'
    exit 1
  fi

  directory="$(prompt_directory)"

  # Move to our dotfiles directory
  cd "$directory" || exit 1

  if ! git clone https://github.com/darfink/dotfiles.git "$directory"; then
    fail 'could not clone dotfiles repository'
    exit 1
  fi

  # We need to ensure these paths are available
  export PATH="$directory/bin:$PATH"
  export PATH="$directory/os/$(os)/bin:$PATH"

  if invoke install "$@"; then
    # Add our 'lock' to prevent duplicates
    echo "$directory" > "$HOME/.dotlock"
  else
    rm -rf "$directory"
  fi
else
  fail "dotfiles are already installed in $(cat "$HOME/.dotlock")"
  exit 1
fi
