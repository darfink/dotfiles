import os

from invoke import task
form .utils import *

@task
def xcode():
  def is_installed():
    return run('xcodebuild -version', warn=False, hide=True).ok

  if is_installed():
    return

  info('installing Xcode')
  run('curl https://developer.apple.com/downloads/download.action?path=Developer_Tools/xcode_6.1/56841_xcode_6.1.dmg -#fO /tmp/xcode.dmg')
  run('sudo hdiutil attach /tmp/xcode.dmg')

  while True:
    user('press [Enter] when the installation is finished')
    raw_input()

    if is_installed():
      break
    fail('could not detect Xcode')
  run('sudo hdiutil detach /Volumes/xcode')
  run('sudo xcodebuild -license')

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
    brew_install('caskroom/cask/brew-cask')

