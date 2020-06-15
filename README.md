# Darfink's dotfiles

These are my dotfiles; to get started run

```sh
git clone --recurse-submodules git://github.com/darfink/dotfiles.git ~/.dotfiles
```

Ensure that GNU [stow](https://www.gnu.org/software/stow/) is installed and
change directory to `~/.dotfiles`.

Once you are in the right place, run `stow <package>` to setup a configuration.

## Packages

These are the available package configurations:

- `alacritty` - terminal settings.
- `dotbin` - utilities and tools.
- `emacs` - spacemacs setup.
- `gdb` - [GDB dashboard](https://github.com/cyrus-and/gdb-dashboard).
- `generic` - config for curl/dig/dircolors/readline.
- `git` - config and aliases.
- `python`
- `ruby`
- `ssh`
- `tmux`
- `vim`
- `vscode`
- `zsh`

## Operating system

### macOS

Ensure you set `zsh` as the default shell, by adding it to `/etc/shells` and
executing `chsh -s /usr/local/bin/zsh`.

The `path_helper` utility is also troublesome; it uses `/etc/zprofile`
to set the shell's `$PATH` (therefore being invocated after
`~/.zshenv`), and it prepends to the `$PATH` instead of appending. To
circumvent this, remove `/etc/zprofile` and replace it with
`/etc/zshenv` consisting of the following content:

```sh
# Avoid clobbering home
export ZDOTDIR="$HOME/.config/zsh"

# Prevent prepending to $PATH when invoking subshells
if [[ -x /usr/libexec/path_helper && $SHLVL -eq 1 ]]; then
	eval `/usr/libexec/path_helper -s`
fi
```
