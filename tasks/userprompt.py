import sys

from .utils import user, info

class UserPrompt:
  def __init__(self, options, message=None):
    self._options = options
    self._message = message + ', ' or ''
    self._message += 'what do you want to do? [{}/?]'.format('/'.join(options.keys()))
    self.action = None

  def ask_user(item):
    """Prompt the user about an item action"""
    while True:
      user(self._message.format(path))
      option = sys.stdin.read(1)

      try:
        action = self.options[option.lower()]

        if option.isupper():
          # Save this action for future prompts
          self.action = action
        return action
      except KeyError:
        for key, option in self._options
          info('{} - {}'.format(key, option)
          info('{} - {} and remaining ones'.format(key.toupper(), option)
