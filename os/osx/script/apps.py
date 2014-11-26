import os
import re

from invoke import task, Collection
from .util import *

class App:
  def __init__(self,
    display_name,
    cask_name=None,
    task_name=None,
    tap=None,
  ):
    self.name = display_name
    self.cask_name = cask_name or self.normalize(self.name)
    self.task_name = task_name or self.normalize(self.name)
    self.tap = tap

  def install(self):
    if brew_installed(self.cask_name, cask=True):
      return
    info('installing {0} app'.format(self.name))

    if self.tap:
      brew_tap(self.tap)
    brew_install(self.cask_name, cask=True)

  def normalize(self, string):
    return re.sub(r'[^a-z0-9 ]', '', string.lower()).replace(' ', '-')

base_apps = [
  App('Alfred'),
  App('Asepsis'),
  App('Dash'),
  App('Deluge'),
  App('Dropbox'),
  App('Firefox'),
  App('f.lux'),
  App('Google Chrome'),
  App('ImageAlpha'),
  App('ImageOptim'),
  App('iTerm2', task_name='iterm'),
  App('KeepingYouAwake'),
  App('Popcorn Time', tap='casidiablo/custom'),
  App('Seil'),
  App('SizeUp', cask_name='sizeup-x11', tap='caskroom/versions')
  App('Skype'),
  App('Spotify'),
  App('The Unarchiver'),
  App('VirtualBox'),
  App('VLC'),
  App('XQuartz'),

  App('Quicklook: Archive',        cask_name='betterzipql')
  App('Quicklook: Syntax',         cask_name='qlcolorcode')
  App('Quicklook: Markdown',       cask_name='qlmarkdown')
  App('Quicklook: Patch',          cask_name='qlprettypatch')
  App('Quicklook: Extension-less', cask_name='qlstephen')
  App('Quicklook: CSV',            cask_name='quicklook-csv')
  App('Quicklook: JSON',           cask_name='quicklook-json')
  App('Quicklook: WebP',           cask_name='webp-quicklook')
  App('Quicklook: Package',        cask_name='suspicious-package')
]

apps = Collection('apps')

for app in base_apps:
  apps.add_task(task(app.install, pre=[cask]), app.task_name)
