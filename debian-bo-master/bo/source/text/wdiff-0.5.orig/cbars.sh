#!/bin/sh
# Produce change bars from two revisions of a document.
# Copyright (C) 1992 Free Software Foundation, Inc.
# Francois Pinard <pinard@iro.umontreal.ca>, 1992.

usage="$0 OLD_FILE NEW_FILE"

if [ $# -ne 2 ]; then
  echo "$usage"; exit
fi

@bindir@/wdiff -1n $1 $2 | sed -e 's/^/  /;/{+/s/^ /|/;s/{+//g;s/+}//g'
