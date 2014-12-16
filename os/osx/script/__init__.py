# -*- coding: utf-8 -*-

from invoke import task, Collection

from ..osx import (
  apps,
  binaries,
  quicklook,
)

ns = Collection()

for module in (apps, binaries, defaults, quicklook):
  def setup():
    collection = Collection.from_module(module)
    executor = Executor(collection)

    for task in collection.task_names:
      if prompt.option or prompt.ask_user(task):
        executor.execute(task)
  ns.add_task(task(setup, name=module.__name__.split('.')[-1]))
