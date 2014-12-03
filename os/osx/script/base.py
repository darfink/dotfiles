# -*- coding: utf-8 -*-

import os

from invoke import task

@task
def computername(name):
  run('sudo scutil --set ComputerName "{}"'.format(name))
  run('sudo scutil --set HostName "{}"'.format(name))
  run('sudo scutil --set LocalHostName "{}"'.format(name))
  run('sudo defaults write /Library/Preferences/SystemConfiguration/com.apple.smb.server NetBIOSName -string "{}"'.format(name))

#@task(pre=[binaries.duti])
def extensions():
  run(os.path.expandvars('duti "$OSDIR/ext/extensions.duti"'))

@task
def xcode_clt():
  if run('xcode-select -p', warn=False, hide=True).failed:
    info('installing xcode command line tools')
    run('xcode-select --install')

@task(xcode_clt)
def homebrew():
  if not command_exists('brew'):
    info('installing Homebrew')
    run('ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"')

  user = os.environ['USER']

  try:
    run('sudo dseditgroup -o checkmember -m "{}" admin'.format(user), hide=True)
  except invoke.exceptions.Failure:
    info('adding {} to “admin” user group'.format(user))
    run('sudo dseditgroup -o edit -a "{}" -t user admin'.format(user))
  run('brew update')

@task(homebrew)
def cask():
  if not binary_installed('cask'):
    info('installing Caskroom')
    brew.install('caskroom/cask/brew-cask')
  run('brew upgrade brew-cask')
