import os

@task
def computername(name):
  run('sudo scutil --set ComputerName "{}"'.format(name))
  run('sudo scutil --set HostName "{}"'.format(name))
  run('sudo scutil --set LocalHostName "{}"'.format(name))
  run('sudo defaults write /Library/Preferences/SystemConfiguration/com.apple.smb.server NetBIOSName -string "{}"'.format(name))

@task(pre=[binaries.vim, apps.vlc])
def extensions():
  run(os.path.expandvars('duti "$OSDIR/ext/extensions.duti"'))
