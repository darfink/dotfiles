# -*- coding: utf-8 -*-

import os

from invoke import task

defaults = {
  'google-chrome': {
    'allowing installation of user scripts via GitHub Gist': [
      'defaults write com.google.Chrome ExtensionInstallSources -array "https://gist.githubusercontent.com/"'
    ],
    'using the system-native print preview dialog': [
      'defaults write com.google.Chrome DisablePrintPreview -bool true'
    ],
    'setting Chrome to the default web browser': [
      'open -a "Google Chrome" --args --make-default-browser'
    ]
  }
}
