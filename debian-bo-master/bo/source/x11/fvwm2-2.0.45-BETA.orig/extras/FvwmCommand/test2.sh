#!/bin/sh
. FvwmCommand.f
if [ "$1" = 'b' ] ; then
  Module FvwmButtons;
elif [ "$1" = 'kb' ] ; then
  KillModule FvwmButtons;
elif [ "$1" = 'c' ] ; then
  shift;
  AddModuleConfig '*FvwmConsoleKey' $1 "'"$2"'" ;
fi

