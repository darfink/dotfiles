import os
import re
import shutil

def info(message):
  print '  [ \033[00;34m..\033[0m ] ', message

def user(message):
  print '\r  [ \033[0;33m?\033[0m ] ', message, ' '

def fail(message):
  print '\r\033[2K  [\033[0;31mFAIL\033[0m] ', message, '\n'

def enum(**enums):
  return type('Enum', (), enums)

def remove_file(target):
  """Remove a file, symlink or directory"""
  if os.path.islink(target):
    os.unlink(target)
  elif os.path.isdir(target):
    shutil.rmtree(target)
  else:
    os.remove(target)

def is_valid_email(text):
  return re.match(r'[^@]+@[^@]+\.[^@]+', text)
