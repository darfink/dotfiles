# -*- coding: utf-8 -*-

import os

from invoke import task

from .base import homebrew, cask
from ..osx import brew

@task(homebrew)
def dupes():
  brew.tap('homebrew/dupes')

@task(homebrew)
def binary():
  brew.tap('homebrew/binary')

@task(homebrew)
def apache():
  brew.tap('homebrew/apache')

@task(homebrew)
def php():
  brew.tap('homebrew/php')

@task(cask)
def cask_popcorn():
  # The brew foundation doesn't host “troublesome” apps
  brew.tap('casidiablo/custom')

@task(cask)
def cask_versions():
  brew.tap('caskroom/versions')

@task(cask)
def cask_xcode():
  brew.tap('darfink/custom')
