# -*- coding: utf-8 -*-

import os

from invoke import task

from .. import core
from ..osx import base, brew, taps

@task(base.homebrew)
def rtun():
  """- reattach to user namespace"""
  brew.install('reattach-to-user-namespace', flags=['--wrap-pbcopy-and-pbpaste'])

@task(base.homebrew)
def ag():
  brew.install('the_silver_searcher')

@task(base.homebrew)
def autojump():
  brew.install('autojump')

@task(pre=[base.homebrew, taps.dupes, taps.binary])
def compression():
  """- essential compression tools"""
  brew.install('gzip')
  brew.install('xz')
  brew.install('rar')
  brew.install('zopfli')
  brew.install('p7zip')
  brew.install('pigz')

@task(base.homebrew)
def cscope():
  brew.install('cscope')

@task(base.homebrew)
def ctags():
  brew.install('ctags')

@task(base.homebrew)
def dnsmasq():
  if brew.installed('dnsmasq'):
    return
  brew.install('dnsmasq')

  # Make *.dev requests reply with 127.0.0.1
  run('echo "address=/.dev/127.0.0.1" > "$(brew --prefix)/etc/dnsmasq.conf"')
  run('echo "listen-address=127.0.0.1" >> "$(brew --prefix)/etc/dnsmasq.conf"')

  if 'homebrew.mxcl.dnsmasq' not in run('launchctl list', hide=True).stdout:
    # Load dnsmasq automatically at startup
    run('sudo cp "$(brew --prefix dnsmasq)/homebrew.mxcl.dnsmasq.plist" /Library/LaunchDaemons/')
    run('sudo launchctl load -w /Library/LaunchDaemons/homebrew.mxcl.dnsmasq.plist')

  # Use our local host for *.dev DNS queries
  run('sudo mkdir -p /etc/resolver')
  run('echo "nameserver 127.0.0.1" | sudo tee /etc/resolver/dev', hide=True)

@task(base.homebrew)
def dockutil():
  brew.install('dockutil')

@task(base.homebrew)
def duti():
  brew.install('duti')

@task(base.homebrew)
def fzf():
  brew.install('fzf')

@task(base.homebrew)
def gifify():
  brew.install('gifify')

@task(base.homebrew)
def gitutils():
  brew.install('bfg')
  brew.install('hub')

@task(pre=[base.homebrew, taps.dupes, rtun])
def gnutools():
  """- entire GNU tools collection"""
  # Install GNU core utilities (those that come with OS X are outdated)
  brew.install('coreutils')
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
  brew.install('parallel')
  brew.install('screen')
  brew.install('watch')
  brew.install('gawk')

@task(base.homebrew)
def htop():
  brew.install('htop-osx')

@task(base.homebrew)
def icdiff():
  brew.install('icdiff')

@task(base.homebrew)
def imagetools():
  """- image optimization software"""
  brew.install('gifsicle')
  brew.install('jpegoptim')
  brew.install('optipng')

@task(base.homebrew)
def lua():
  brew.install('lua', flags=['--with-completion'])
  brew.install('luajit')

@task(base.homebrew)
def lynx():
  brew.install('lynx')

@task(base.homebrew)
def moreutils():
  brew.install('moreutils', flags=['--without-parallel'])

@task(base.homebrew)
def osxfuse():
  """- fuse support (sshfs, ext{2-4}, ntfs)"""
  if not brew.installed('osxfuse'):
    brew.install('osxfuse')
    run('sudo cp -Rxf "$(brew --prefix osxfuse)/Library/Filesystems/osxfusefs.fs" /Library/Filesystems/')
    run('sudo chmod +s /Library/Filesystems/osxfusefs.fs/Support/load_osxfusefs')
  brew.install('ext2fuse')
  brew.install('ext4fuse')
  brew.install('ntfs-3g')
  brew.install('sshfs')

@task(base.homebrew)
def pv():
  brew.install('pv')

@task(base.homebrew)
def tree():
  brew.install('tree')

@task(pre=[base.homebrew, rtun])
def tmux():
  brew.install('tmux')

@task(base.homebrew)
def python():
  brew.install('python', flags=['--with-brewed-openssl'])

@task(base.homebrew)
def recode():
  brew.install('recode')

@task(base.homebrew)
def ruby():
  brew.install('ruby')

@task(pre=[base.homebrew, core.symlinks, taps.dupes])
def shiny():
  """- replacements for OS X outdated tools"""
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

@task(base.homebrew, name='ssh-copy-id')
def ssh_copy_id():
  brew.install('ssh-copy-id')

@task(name='ssh-server')
def ssh_server():
  if not run('sudo systemsetup -getremotelogin', hide=True, warn=False).ok:
    # OSX is bundled with a SSH server
    run('sudo systemsetup -setremotelogin on')

@task(pre=[base.homebrew, taps.apache, taps.dupes, taps.php, dnsmasq])
def xamp():
  if not brew.installed('mysql'):
    brew.install('mysql')

    prefix = run('brew --prefix mysql', hide=True).stdout.strip()
    run('cp -v "{0}/support-files/my-default.cnf" "{0}/my.cnf"'.format(prefix))

    if 'homebrew.mxcl.mysql.plist' not in run('launchctl list', hide=True).stdout:
      # Load MySQL automatically at startup
      run('sudo cp "{}/homebrew.mxcl.mysql.plist" /Library/LaunchDaemons/'.format(prefix))
      run('sudo launchctl load -w /Library/LaunchDaemons/homebrew.mxcl.mysql.plist')

  if not brew.installed('httpd24'):
    brew.install('httpd24', flags=['--with-brewed-openssl'])

    # Enable the virtual hosts configuration
    run('sed -i "s,#\(.*httpd-vhosts.conf\),\1,g" "$(brew --prefix)/etc/apache2/2.4/httpd.conf"')
    run('ln -sf ext/httpd-vhosts.conf "$(brew --prefix)/etc/apache2/2.4/extra/"')


  """- setup for Apache, MariaDB & PHP
        "httpd24 --with-brewed-openssl"
        "php56 --with-homebrew-openssl --homebrew-apxs --with-apache"
        "php56-opcache"
  """
  pass

@task(base.homebrew)
def webkit2png():
  brew.install('webkit2png')

@task(pre=[base.homebrew, core.symlinks, autojump])
def zsh():
  if brew.installed('zsh'):
    return
  brew.install('zsh')
  path = run('echo "$(brew --prefix)/bin/zsh"', hide=True).stdout

  # Append zsh to the shell list if not already there, and then change to it
  run('[ grep -Fxq "{0}" /etc/shells ] || echo "{0}" | sudo tee -a /etc/shells > /dev/null'.format(path))
  run('chsh -s "{}"'.format(path))
