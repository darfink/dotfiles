# -*- coding: utf-8 -*-

import os

from invoke import task, run

from ..osx import brew
from ..utils import command_exists, info

@task
def computername(name):
  run('sudo scutil --set ComputerName "{}"'.format(name))
  run('sudo scutil --set HostName "{}"'.format(name))
  run('sudo scutil --set LocalHostName "{}"'.format(name))
  run('sudo defaults write /Library/Preferences/SystemConfiguration/com.apple.smb.server NetBIOSName -string "{}"'.format(name))

#@task(pre=[binaries.duti])
def extensions():
  run(os.path.expandvars('duti "os/osx/ext/extensions.duti"'))

@task
def xcode_clt():
  if run('xcode-select -p', warn=True, hide=True).failed:
    info('installing xcode command line tools')
    run('xcode-select --install')

class Homebrew:
  def __init__(self):
    if not command_exists('brew'):
      self.install()
    self.check_group()
    self.check_path()

    run('brew update')

  def install(self):
    info('installing Homebrew')
    run('ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"')

  def check_group(self):
    user = os.environ['USER']
    try:
      run('dseditgroup -o checkmember -m "{}" admin'.format(user), hide=True)
    except invoke.exceptions.Failure:
      info('adding {} to “admin” user group'.format(user))
      run('sudo dseditgroup -o edit -a "{}" -t user admin'.format(user))

  def check_path(self):
    # We need to ensure that the brew path is first
    brewdir = run('brew --prefix', hide=True).stdout.strip()
    path = os.environ['PATH'].split(os.pathsep)

    if brewdir in path:
      path.remove(brewdir)

    path.insert(0, brewdir)
    os.environ['PATH'] = os.pathsep.join(path)

@task(xcode_clt)
def homebrew():
  Homebrew()

@task(homebrew)
def cask():
  if not brew.installed('brew-cask'):
    info('installing Caskroom')
    brew.install('caskroom/cask/brew-cask')
  #TODO: Troubleshoot why this fails
  #run('brew upgrade brew-cask')

@task(cask)
def xquartz():
  brew.install('xquartz', cask=True)
