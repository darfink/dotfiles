# -*- coding: utf-8 -*-

import os

from invoke import run

def install(names, cask=False, flags=[]):
  if isinstance(names, basestring):
    names = [names]

  installed = installed(cask=cask)
  names = [n for n in names if n not in installed]

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
