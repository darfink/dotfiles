# -*- coding: utf-8 -*-

import os

from invoke import run

def install(names, cask=False, flags=[]):
  if isinstance(names, basestring):
    names = [names]

  packages = installed(cask=cask)
  names = [n for n in names if n not in packages]

  if len(names) > 0:
    if cask:
      flags.append('--appdir=/Applications')

    run(u" ".join([
      'brew',
      'cask' if cask else '',
      'install',
      u" ".join(names),
      u" ".join(flags),
    ]))
    return True
  return False

def installed(name=None, cask=False):
  command = 'brew {} list'.format('cask' if cask else '')
  names = run(command, hide=True).stdout.strip().split("\n")
  return (name in names) if name else names

def tap(repo):
  if not tapped(repo):
    run('brew tap "{}"'.format(repo))

def tapped(tap=None):
  taps = run('brew tap', hide=True).stdout.strip().split("\n")
  return (tap in taps) if tap else taps

#TODO: Find a better place for this
def load(config):
  if os.path.basename(config) not in run('launchctl list', hide=True).stdout:
    run('sudo cp -f "{}" /Library/LaunchDaemons/'.format(config))
    run('sudo chown root /Library/LaunchDaemons/{}'.format(os.path.basename(config)))
    run('sudo launchctl load -w "/Library/LaunchDaemons/{}"'.format(os.path.basename(config)))
