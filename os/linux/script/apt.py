def update(repo=None):
  if not repo:
    run('sudo apt-get update')
  else:
    run('sudo apt-get update -o Dir::Etc::sourcelist="sources.list.d/{}" -o Dir::Etc::sourceparts="-" -o APT::Get::List-Cleanup="0"'.format(repo))

def add_repo(repo):
  if not installed(repo):
    run('')
    update(repo)

def installed(name=None):
  names = run('dpkg --get-selections', hide=True).stdout.strip().split("\n")
  return (name in names) if name else names
