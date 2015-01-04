# -*- coding: utf-8 -*-

from invoke import task, Collection
from invoke.executor import Executor
from invoke.context import Context

from ..osx import (
  apps,
  binaries,
  quicklook,
  defaults,
)

ns = Collection()
context = Context()

for module in (apps, binaries, quicklook, defaults):
  def context(module):
    def setup():
      collection = Collection.from_module(module)
      executor = Executor(collection, context)

      for task in collection.task_names:
        executor.execute(task)
    return setup
  ns.add_task(task(context(module), name=module.__name__.split('.')[-1]))
