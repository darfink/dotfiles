# -*- coding: utf-8 -*-

import os

from invoke import task, run

from .symlinker import Symlinker
from .utils import (
  command_exists,
  fail,
  info,
  user,
)

@task
def submodule_init():
  run('git submodule update --init --recursive')

@task(submodule_init)
def submodules():
  info('downloading dotfiles submodules... please wait')
  run('git submodule update --recursive')
  run('git clean -df')

@task
def symlinks():
  Symlinker()

@task
def sshkey():
  """- generates an RSA key if none exists"""
  if os.path.exists(os.path.expandhome('~/.ssh/id_rsa.pub')):
    return

  while True:
    user('generating SSH key; please input your email:')
    email = raw_input()

    if is_valid_email(email):
      run('ssh-keygen -t rsa -C "{}"'.format(email))
      break
    fail('invalid email address')

@task(pre=[submodule_init, submodules])
def fonts():
  """- installs Powerline fonts (locally)"""
  info('installing patched fonts for Powerline')
  run('fonts/install.sh')

# TODO: VIM requirement here
@task(pre=[symlinks])
def vimplugins():
  info('installing Vim plugins')
  run('vim +PlugInstall +qall 2&>/dev/null')

@task(symlinks)
def nvm():
  path = os.path.expandhome('~/.nvm')

  def nvm_setup():
    run('source "{}/nvm.sh"'.format(path))

  if not os.path.exists(path):
    info('installing node version manager (nvm)')
    run('git clone https://github.com/creationix/nvm.git "{}"'.format(path))
    run('(cd "{}" && git checkout $(git describe --abbrev=0 --tags))'.format(path))

    nvm_setup()

    run('nvm install')
    run('nvm alias default "$(cat ~/.nvmrc)"')

  if 'NVM_DIR' not in os.environ:
    nvm_setup()

