#!/bin/sh
exec xgettext --keyword=_ --keyword=__ --keyword=_M --keyword=__M -d new *.c
