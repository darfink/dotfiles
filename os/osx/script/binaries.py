# -*- coding: utf-8 -*-

import os

from invoke import task, run

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
def fasd():
  brew.install('fasd')

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

#TODO: This doesn't wuuurk
#@task(base.homebrew)
#def ctags():
#  brew.install('ctags-exuberant')

@task(base.homebrew)
def dnsmasq():
  if brew.installed('dnsmasq'):
    return
  brew.install('dnsmasq')

  # Make *.dev requests reply with 127.0.0.1
  run('echo "address=/.dev/127.0.0.1" > "$(brew --prefix)/etc/dnsmasq.conf"')
  run('echo "listen-address=127.0.0.1" >> "$(brew --prefix)/etc/dnsmasq.conf"')

  brew.load(run('echo "$(brew --prefix dnsmasq)/homebrew.mxcl.dnsmasq.plist"', hide=True).stdout.strip())

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
  brew.install('gnu-tar', flags=['--with-default-names'])
  brew.install('gnu-indent', flags=['--with-default-names'])
  brew.install('gnu-which', flags=['--with-default-names'])
  brew.install('gnu-sed', flags=['--with-default-names'])
  brew.install('gnutls')
  brew.install('grep', flags=['--with-default-names'])
  brew.install('ed', flags=['--default-names'])
  brew.install('wdiff', flags=['--with-gettext'])
  brew.install('wget', flags=['--with-iri'])
  brew.install('parallel')
  brew.install('screen')
  brew.install('watch')
  brew.install('gawk')

@task(base.homebrew)
def htop():
  if not brew.installed('htop-osx'):
    brew.install('htop-osx')
    run('sudo chown root:wheel "$(brew --prefix htop)/bin/htop"')
    run('sudo chmod u+s "$(brew --prefix htop)/bin/htop"')

@task(base.homebrew)
def icdiff():
  brew.install('icdiff')

@task(base.homebrew)
def imagetools():
  brew.install('imagemagick')

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
def rtorrent():
  brew.install('rtorrent', flags=['--with-xmlrpc-c'])

@task(base.homebrew)
def ruby():
  brew.install('ruby')

@task(base.homebrew)
def sassc():
  brew.install('sassc')

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
  if not run('sudo systemsetup -getremotelogin | grep On', hide=True, warn=True).ok:
    # OSX is bundled with a SSH server
    run('sudo systemsetup -setremotelogin on')
    run('sudo sed -i -E "s/^#?(PasswordAuthentication|ChallengeResponseAuthentication).*$/\1 no/" /etc/sshd_config')

@task(pre=[base.homebrew, taps.apache, taps.dupes, taps.php, dnsmasq])
def xamp():
  if not brew.installed('mysql'):
    brew.install('mysql')
    prefix = run('brew --prefix mysql', hide=True).stdout.strip()

    run('cp -v "{0}/support-files/my-default.cnf" "{0}/my.cnf"'.format(prefix))
    brew.load('{}/homebrew.mxcl.mysql.plist'.format(prefix))

  if not brew.installed('httpd24'):
    # Unload the default Apache if running
    run('sudo launchctl unload -w /System/Library/LaunchDaemons/org.apache.httpd.plist', hide=True, warn=True)

    brew.install('httpd24', flags=['--with-brewed-openssl'])

    # Setup apache port forwarding (port 80 -> 8080)
    run('sudo cp -f os/osx/ext/apache /etc/pf.anchors/')
    run('sudo cp -f os/osx/ext/com.apple.pfctl.plist /System/Library/LaunchDaemons/')
    run('printf "%s\\n" /rdr-anchor/a \'rdr-anchor "apache"\' . w q | sudo ex -s /etc/pf.conf')
    run('echo \'load anchor "apache" from "/etc/pf.anchors/apache"\' | sudo tee -a /etc/pf.conf')

    config = run('echo "$(brew --prefix)/etc/apache2/2.4/httpd.conf"').stdout.strip()

    # Enable frequently used/required mods (PHP is enabled automatically by brew)
    run('sed -i "s,#\(.*vhost_alias_module.*\),\1,g" "{}"'.format(config))
    run('sed -i "s,#\(.*httpd-vhosts.conf\),\1,g" "{}"'.format(config))
    run('sed -i "s,#\(.*rewrite_module.*\),\1,g" "{}"'.format(config))

    # Enable the virtual hosts configuration
    run('cp -f ext/httpd-vhosts.conf "$(brew --prefix)/etc/apache2/2.4/extra/"')
    run('sed -i s#%PREFIX%#$(brew --prefix)# "$(brew --prefix)/etc/apache2/2.4/extra/httpd-vhosts.conf"')

    # Give apache full access to the 'www' directory
    run('sudo chown -R daemon:daemon "$(brew --prefix)/var/www/*"')

  if not brew.installed('php56'):
    brew.install('php56', flags=['--without-snmp', '--with-homebrew-openssl', '--homebrew-apxs', '--with-apache'])
    brew.install('phpsh')

    with open(run('echo "$(brew --prefix)/etc/apache2/2.4/httpd.conf"', hide=True).stdout.strip(), 'a') as config:
      config.write(
        'AddHandler php5-script .php'
        'AddType text/html .php'
        'DirectoryIndex index.php index.html')

  # Load apache after all modules have been properly setup
  brew.load(run('echo "$(brew --prefix httpd24)/homebrew.mxcl.httpd24.plist"').stdout.strip())

@task(base.homebrew)
def webkit2png():
  brew.install('webkit2png')

@task(pre=[base.homebrew, core.symlinks, autojump])
def zsh():
  if brew.installed('zsh'):
    return
  brew.install('zsh')
  path = run('echo "$(brew --prefix)/bin/zsh"', hide=True).stdout.strip()

  # Append zsh to the shell list if not already there, and then change to it
  run('[ grep -Fxq "{0}" /etc/shells ] || echo "{0}" | sudo tee -a /etc/shells > /dev/null'.format(path))
  run('chsh -s "{}"'.format(path))
