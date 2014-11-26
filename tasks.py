import os

from invoke import task, Collection
from provisioning.core import *

@task(pre=[submodule_init, submodules], default=True)
def bootstrap():
  """Bootstrap the entire dotfiles installation"""
  assert os.path.isdir(target), 'target directory is not valid'

  print '======================================================'
  print 'Welcome to my dotfiles installation'
  print '======================================================'

  sshkey()
  symlinks()
  fonts()
  vimplugins()

ns = Collection()
ns.add_task(bootstrap)
ns.add_task(symlinks)
ns.add_task(vimplugins)
ns.add_task(sshkey)
ns.add_task(fonts)
