import os

from invoke import task
from provisioning.core import *

@task(pre=[submodule_init, submodules], default=True)
def bootstrap(target, skip_submodules=False):
  assert os.path.isdir(target), 'target directory is not valid'

  print '======================================================'
  print 'Welcome to my dotfiles installation'
  print '======================================================'

  sshkey()
  dotfiles(target)
  fonts()
  vimplugins()
