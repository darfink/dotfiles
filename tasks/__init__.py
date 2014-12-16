# -*- coding: utf-8 -*-

import os
import sys

from invoke import task, Collection

from .utils import merge_collection

from .core import (
  fonts,
  sshkey,
  symlinks,
)

if sys.platform.startswith('darwin'):
    from . import osx as platform
elif sys.platform.startswith('linux'):
    from . import linux as platform
else:
    raise ImportError('your OS is not supported')

@task(pre=[core.submodule_init, core.submodules])
def bootstrap():
  """- bootstrap for the dotfiles installation"""
  print '======================================================'
  print 'Welcome to my dotfiles installation'
  print '======================================================'

  sshkey()
  symlinks()
  fonts()

ns = Collection.from_module(sys.modules[__name__])
merge_collection(ns, Collection.from_module(platform))
