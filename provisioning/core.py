import os

from glob import iglob
from invoke import task, run
from .utils import *

@task
def submodule_init(skip_submodules=False):
  if not skip_submodules:
    run('git submodule update --init --recursive')

@task
def submodules(skip_submodules=False):
  if skip_submodules:
    return

  info('downloading dotfiles submodules... please wait')

  run('git submodule update --recursive')
  run('git clean -df')

@task
def dotfiles(directory):
  Action = enum(None=0, Skip=1, Overwrite=2, Backup=4, All=8)

  def link_file(source, target, action):
    islink = os.path.islink(target)
    reltarget = '~/' + os.path.relpath(target, os.path.expandvars('$HOME'))

    if islink || os.path.exists(target):
      if action == Action.None:
        if islink && os.readlink(target) == source:
          action = Action.Skip
        else:
          user('file already exists: ~/{0}, what do you want to do? [s/S/o/O/b/B]'.format(reltarget))
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

      if action == Action.Skip
        info('skipping {0}'.format(source))

    if action != Action.Skip:
      os.symlink(source, target)
      info('linked {0} to {1}'.format(source, reltarget))

    if (action & Action.All) == 0:
      return Action.None
    return action

  action = Action.None

  for sym in iglob('{0}/*/*.symlink*'.format(directory)):
    target = os.path.expandvars('$HOME/.')
    result = sym.split('-')

    if len(result) > 1:
      target += result[1] + '/'
      if not os.path.exists(target):
        info('creating '.{0}' in home'.format(result[1]))
        os.mkdir(target)

    target += os.path.splitext(result[0])[0]
    action = link_file(source, target, action)

@task
def sshkey():
  if not os.path.exists(os.path.expandvars('$HOME/.ssh/id_rsa.pub')):
    while True:
      user('generating SSH key; please input your email:')
      email = raw_input()

      if is_valid_email(email):
        run('ssh-keygen -t rsa -C "{0}"'.format(email))
        break

      fail('invalid email address')

@task(pre=[submodule_init, submodules])
def fonts():
  info('installing patched fonts for Powerline')
  run('fonts/install.sh')

@task
def vimplugins():
  info('installing Vim plugins')
  run('vim +PlugInstall +qall 2&> /dev/null')

