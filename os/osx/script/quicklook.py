# -*- coding: utf-8 -*-

import os

from invoke import task

from ..osx import base, brew

@task(base.cask)
def archive():
  brew.install('betterzipql', cask=True)

@task(base.cask)
def syntax():
  brew.install('qlcolorcode', cask=True)

@task(base.cask)
def markdown():
  brew.install('qlmarkdown', cask=True)

@task(base.cask)
def patch():
  brew.install('qlprettypatch', cask=True)

@task(base.cask, name='extless')
def extensionless():
  brew.install('qlstephen', cask=True)

@task(base.cask)
def csv():
  brew.install('quicklook-csv', cask=True)

@task(base.cask)
def json():
  brew.install('quicklook-json', cask=True)

@task(base.cask)
def webp():
  brew.install('webp-quicklook', cask=True)

@task(base.cask)
def package():
  brew.install('suspicious-package', cask=True)
