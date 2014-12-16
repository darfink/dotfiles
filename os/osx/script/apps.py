# -*- coding: utf-8 -*-

import os
import re
import sys

from invoke import task, Collection

from ..osx import (
  base,
  brew,
  binaries,
  taps,
)

@task(pre=[base.cask, taps.cask_xcode])
def xcode():
  if not brew.installed('xcode', cask=True):
    brew.install('xcode', cask=True)
    run('sudo xcodebuild -license')

@task(base.cask)
def alfred():
  brew.install('alfred', cask=True)

@task(base.cask)
def asepsis():
  brew.install('asepsis', cask=True)

@task(base.cask)
def dash():
  brew.install('dash', cask=True)

@task(base.cask)
def deluge():
  brew.install('deluge', cask=True)

@task(base.cask)
def dropbox():
  brew.install('dropbox', cask=True)

@task(base.cask)
def firefox():
  brew.install('firefox', cask=True)

@task(base.cask)
def flux():
  brew.install('flux', cask=True)

@task(base.cask, name='google-chrome')
def google_chrome():
  brew.install('google-chrome', cask=True)

@task(base.cask)
def iterm():
  brew.install('iterm2', cask=True)

@task(base.cask)
def keepingyouawake():
  brew.install('keepingyouawake', cask=True)

@task(pre=[base.cask, taps.cask_popcorn], name='popcorn-time')
def popcorn_time():
  brew.install('popcorn-time', cask=True)

@task(base.cask)
def seil():
  brew.install('seil', cask=True)

@task(pre=[base.cask, base.xquartz, taps.cask_versions])
def sizeup():
  brew.install('sizeup-x11', cask=True)

@task(base.cask)
def skype():
  brew.install('skype', cask=True)

@task(base.cask)
def spotify():
  brew.install('spotify', cask=True)

@task(base.cask, name='the-unarchiver')
def the_unarchiver():
  brew.install('the-unarchiver', cask=True)

@task(pre=[base.homebrew, xcode, binaries.lua, binaries.python, binaries.cscope])
def vim():
  brew.install('macvim', flags=['--with-cscope', '--with-python', '--with-luajit', '--override-system-vim'])
  brew.install('shellcheck')
  brew.install('par')

@task(base.cask)
def virtualbox():
  brew.install('virtualbox', cask=True)

@task(base.cask)
def vlc():
  brew.install('vlc', cask=True)

@task(base.homebrew)
def xpdf():
  brew.install('xpdf')


