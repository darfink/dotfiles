# -*- coding: utf-8 -*-

import os
import re
import shutil

from invoke import Collection

def info(message):
  print "  [ \033[00;34m..\033[0m ] ", message

def user(message):
  print "\r  [ \033[0;33m?\033[0m ] {}".format(message),

def fail(message):
  print "\r\033[2K  [\033[0;31mFAIL\033[0m] {}".format(message)

def enum(**enums):
  return type('Enum', (), enums)

def is_valid_email(text):
  return re.match(r'[^@]+@[^@]+\.[^@]+', text)

def command_exists(name):
  return run('hash "{}"'.format(name), hide=True, warn=False).ok

def merge_collection(target, source):
  for name in source.task_names:
    target.add_task(source[name])
