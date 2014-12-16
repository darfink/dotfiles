from invoke import task

from ..linux import apt

@task
def multiverse():
  run('sudo sed -i "/^# deb .*multiverse/ s/^# //" /etc/apt/sources.list')
  apt.update()

@task
def deluge():
  apt.add_repo('ppa:deluge-team/ppa')

@task
def popcorn():
  apt.add_repo('ppa:webupd8team/popcorntime')
