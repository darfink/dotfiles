# -*- coding: utf-8 -*-

import os

from .utils import user

def sshkey():
  """- generates an RSA key if none exists"""
  if os.path.exists(os.path.expanduser('~/.ssh/id_rsa.pub')):
    return

  while True:
    user('generating SSH key; please input your email:')
    email = raw_input()

    if is_valid_email(email):
      run('ssh-keygen -f ~/.ssh/id_rsa -t rsa -C "{}" -N ""'.format(email))
      break
    fail('invalid email address')

sshkey()
