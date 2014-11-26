import os

from glob import iglob
from invoke import task, run
from .utils import *

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
  Action = enum(Unknown=0, Skip=1, Overwrite=2, Backup=4, All=8)

  def link_file(source, target, action):
    islink = os.path.islink(target)
    reltarget = '~/' + os.path.relpath(target, os.path.expandvars('$HOME'))

    if islink or os.path.exists(target):
      if action == Action.Unknown:
        if islink and os.readlink(target) == source:
          action = Action.Skip
        else:
          user('file already exists: ~/{0}, what do you want to do? [s/S/o/O/b/B/?]'.format(reltarget))
          option = sys.stdin.read(1)

          action = {
            's': Action.Skip,
            'S': Action.Skip & Action.All,
            'o': Action.Overwrite,
            'O': Action.Overwrite & Action.All,
            'b': Action.Backup,
            'B': Action.Backup & Action.All
          }[option]

      if action == Action.Overwrite:
        remove_file(target)
        info('removed {0}'.format(reltarget))
      elif action == Action.Backup:
        shutil.move(target, target + '.backup')
        info('moved {0} to {0}.backup'.format(reltarget))

      if action == Action.Skip:
        info('skipping {0}'.format(source))

    if action != Action.Skip:
      os.symlink(source, target)
      info('linked {0} to {1}'.format(source, reltarget))

    if (action & Action.All) == 0:
      return Action.Unknown
    return action

  action = Action.None

  for sym in iglob('*/*.symlink*'):
    target = os.path.expandvars('$HOME/.')
    result = sym.split('-')

    if len(result) > 1:
      target += result[1] + '/'
      if not os.path.exists(target):
        info('creating .{0} in home'.format(result[1]))
        os.mkdir(target)

    target += os.path.splitext(result[0])[0]
    action = link_file(source, target, action)

@task
def sshkey():
  if not os.path.exists(os.path.expandhome('~/.ssh/id_rsa.pub')):
    while True:
      user('generating SSH key; please input your email:')
      email = raw_input()

      if is_valid_email(email):
        run('ssh-keygen -t rsa -C "{}"'.format(email))
        break
      fail('invalid email address')

@task(pre=[submodule_init, submodules])
def fonts():
  info('installing patched fonts for Powerline')
  run('fonts/install.sh')

@task(pre=[symlinks, binaries.vim])
def vimplugins():
  info('installing Vim plugins')
  run('vim +PlugInstall +qall 2&> /dev/null')

@task(symlinks):
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

