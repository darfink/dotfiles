# -*- coding: utf-8 -*-

import os

from invoke import task

from .. import core
from ..osx import brew, taps
from .base import homebrew

@task(homebrew)
def rtun():
  """- reattach to user namespace"""
  brew.install('reattach-to-user-namespace', flags=['--wrap-pbcopy-and-pbpaste'])

@task(homebrew)
def ag():
  brew.install('the_silver_searcher')

@task(homebrew)
def autojump():
  brew.install('autojump')

@task(pre=[homebrew, taps.dupes, taps.binary])
def compression():
  # These makes patool awesome
  brew.install('gzip')
  brew.install('xz')
  brew.install('rar')
  brew.install('zopfli')
  brew.install('p7zip')
  brew.install('pigz')

@task(homebrew)
def cscope():
  brew.install('cscope')

@task(homebrew)
def ctags():
  brew.install('ctags')

@task(homebrew)
def dnsmasq():
  if brew.installed('dnsmasq'):
    return
  brew.install('dnsmasq')

  # Make *.dev requests reply with 127.0.0.1
  run('echo "address=/.dev/127.0.0.1" > "$(brew --prefix)/etc/dnsmasq.conf"')
  run('echo "listen-address=127.0.0.1" >> "$(brew --prefix)/etc/dnsmasq.conf"')

  if 'homebrew.mxcl.dnsmasq' not in run('launchctl list', hide=True).stdout:
    # Load dnsmasq automatically at startup
    run('sudo cp "$(brew --prefix dnsmasq)/homebrew.mxcl.dnsmasq.plist" /Library/LaunchDaemons')
    run('sudo launchctl load -w /Library/LaunchDaemons/homebrew.mxcl.dnsmasq.plist')

  # Use our local host for *.dev DNS queries
  run('sudo mkdir -p /etc/resolver')
  run('echo "nameserver 127.0.0.1" | sudo tee /etc/resolver/dev', hide=True)

@task(homebrew)
def dockutil():
  brew.install('dockutil')

@task(homebrew)
def duti():
  brew.install('duti')

@task(homebrew)
def fzf():
  brew.install('fzf')

@task(homebrew)
def gifify():
  brew.install('gifify')

@task(homebrew)
def gitutils():
  brew.install('bfg')
  brew.install('hub')

@task(pre=[homebrew, taps.dupes, rtun])
def gnutools():
  # Install GNU core utilities (those that come with OS X are outdated)
  brew.install('coreutils')
  brew.install('moreutils')
  brew.install('diffutils')
  brew.install('binutils')

  # Install GNU `find`, `locate`, `updatedb`, `xargs` etc
  brew.install('findutils', flags=['--with-default-names'])
  brew.install('gnu-indent', flags=['--with-default-names'])
  brew.install('gnu-which', flags=['--with-default-names'])
  brew.install('gnu-tar', flags=['--with-default-names'])
  brew.install('gnu-sed', flags=['--with-default-names'])
  brew.install('gnutls', flags=['--with-default-names'])
  brew.install('grep', flags=['--with-default-names'])
  brew.install('ed', flags=['--with-default-names'])
  brew.install('wdiff', flags=['--with-gettext'])
  brew.install('wget', flags=['--with-iri'])
  brew.install('screen')
  brew.install('watch')
  brew.install('gawk')

@task(homebrew)
def htop():
  brew.install('htop-osx')

@task(homebrew)
def imagetools():
  """- installs image optimization software"""
  brew.install('gifsicle')
  brew.install('jpegoptim')
  brew.install('optipng')

@task(homebrew)
def lua():
  brew.install('lua', flags=['--with-completion'])
  brew.install('luajit')

@task(homebrew)
def lynx():
  brew.install('lynx')

@task(homebrew)
def osxfuse():
  """- adds fuse support (sshfs, ext{2-4}, ntfs)"""
  if not brew.installed('osxfuse'):
    brew.install('osxfuse')
    run('sudo cp -Rxf "$(brew --prefix osxfuse)/Library/Filesystems/osxfusefs.fs" /Library/Filesystems/')
    run('sudo chmod +s /Library/Filesystems/osxfusefs.fs/Support/load_osxfusefs')
  brew.install('ext2fuse')
  brew.install('ext4fuse')
  brew.install('ntfs-3g')
  brew.install('sshfs')

@task(homebrew)
def pv():
  brew.install('pv')

@task(homebrew)
def tree():
  brew.install('tree')

@task(pre=[homebrew, rtun])
def tmux():
  brew.install('tmux')

@task(homebrew)
def python():
  brew.install('python', flags=['--with-brewed-openssl'])

@task(homebrew)
def recode():
  brew.install('recode')

@task(homebrew)
def ruby():
  brew.install('ruby')

@task(pre=[homebrew, core.symlinks, taps.dupes])
def shiny():
  """- updates OS X outdated tools"""
  if not brew.installed('git'):
    brew.install('git')
    run('git config -f ~/.gitconfig.local credential.helper osxkeychain')
  brew.install('file-formula')
  brew.install('openssh', flags=['--with-brewed-openssl'])
  brew.install('rsync')
  brew.install('gpatch')
  brew.install('nano')
  brew.install('less')
  brew.install('source-highlight')
  brew.install('lesspipe')

@task(homebrew, name='ssh-copy-id')
def ssh_copy_id():
  brew.install('ssh-copy-id')

@task(pre=[homebrew, taps.apache, taps.dupes, taps.php, dnsmasq])
def xamp():
  """- setup for Apache, MariaDB & PHP
       "mysql"
        "httpd24 --with-brewed-openssl"
        "php56 --with-homebrew-openssl --homebrew-apxs --with-apache"
        "php56-opcache"
  """
  pass

@task(homebrew)
def webkit2png():
  brew.install('webkit2png')

@task(pre=[homebrew, core.symlinks, autojump])
def zsh():
  if brew.installed('zsh'):
    return
  brew.install('zsh')
  path = run('echo "$(brew --prefix)/bin/zsh"', hide=True).stdout

  # Append zsh to the shell list if not already there, and then change to it
  run('[ grep -Fxq "{0}" /etc/shells ] || echo "{0}" | sudo tee -a /etc/shells > /dev/null'.format(path))
  run('chsh -s "{}"'.format(path))
