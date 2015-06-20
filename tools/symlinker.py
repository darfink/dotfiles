# -*- coding: utf-8 -*-

import os
import glob
import shutil

from .utils import (
  enum,
  info,
  user,
)

Action = enum(Unknown=0, Skip=1, Overwrite=2, Backup=3)

class Symlinker:
  def __init__(self, directory='$HOME', action=Action.Unknown):
    """Symlink dotfiles"""
    self.directory = os.path.expandvars(directory)
    self.action = action

    # Iterate over all names containing 'symlink'
    for symlink in glob.iglob('*/*.symlink*'):
      target = self.parse_target(symlink)
      self.connect(symlink, target)

  def parse_target(self, symlink):
    """Parse path containing symlink info"""
    target = '{}/.'.format(self.directory)
    parts = symlink.split('-')

    if len(parts) > 1:
      # The symlink is within a directory. Parts example:
      #  [0] = name (e.g config)
      #  [1] = directory (e.g ssh)
      target += parts[1] + '/'

      # Create the directory (e.g .ssh) if it doesn't exist
      if not os.path.exists(target):
        info('creating .{} in {}'.format(parts[1], self.directory))
        os.mkdir(target)

    # Combine the directory with the name (without symlink extension)
    return (target + os.path.splitext(os.path.basename(parts[0]))[0])

  def connect(self, source, target):
    """Connect a source path to a target path"""
    reltarget = os.path.relpath(target, self.directory)
    abssource = os.path.abspath(source)

    islink = os.path.islink(target)
    action = self.action

    if islink or os.path.exists(target):
      if action == Action.Unknown:
        if islink and os.readlink(target) == abssource:
          action = Action.Skip
        else:
          action = self.prompt_path(reltarget)

      if action == Action.Overwrite:
        self.remove_path(target)
        info('removed {}'.format(reltarget))
      elif action == Action.Backup:
        shutil.move(target, target + '.backup')
        info('moved {0} to {0}.backup'.format(reltarget))
      elif action == Action.Skip:
        info('skipped {}'.format(source))

    if action != Action.Skip:
      os.symlink(abssource, target)
      info('linked {} to {}'.format(source, reltarget))

  def prompt_path(self, path):
    """Prompt the user about a path action"""
    while True:
      user('path already exists: {}, what do you want to do? [s/S/o/O/b/B/?]'.format(path))
      option = raw_input()

      try:
        action = {
          's': Action.Skip,
          'o': Action.Overwrite,
          'b': Action.Backup,
        }[option.lower()]

        if option.isupper():
          # Save this action for future prompts
          self.action = action
        return action
      except KeyError:
        info('s - skip this file')
        info('S - skip this and remaining files')
        info('o - overwrite this file')
        info('O - overwrite this and remaining files')
        info('b - backup this file')
        info('B - backup this and remaining files')
        info('? - print help')

  def remove_path(self, target):
    """Remove a file, symlink or directory"""
    if os.path.islink(target):
      os.unlink(target)
    elif os.path.isdir(target):
      shutil.rmtree(target)
    else:
      os.remove(target)

Symlinker()
