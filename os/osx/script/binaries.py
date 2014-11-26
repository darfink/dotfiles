
@task(homebrew)
def mamp():
  """Setup for Apache, MariaDB & PHP"""
  pass

@task(pre=[homebrew, symlinks])
def git():
  binary_install('git')
  run('git config -f ~/.gitconfig.local credential.helper osxkeychain')
