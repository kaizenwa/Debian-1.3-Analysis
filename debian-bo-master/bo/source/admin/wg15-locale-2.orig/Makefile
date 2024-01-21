# Makefile for installing locale data source files.

# Copyright (C) 1996 Free Software Foundation, Inc.
# This file is part of the GNU C Library.

# The GNU C Library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public License
# as published by the Free Software Foundation; either version 2 of
# the License, or (at your option) any later version.

# The GNU C Library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Library General Public License for more details.

# You should have received a copy of the GNU Library General Public
# License along with the GNU C Library; see the file COPYING.LIB.  If
# not, write to the Free Software Foundation, Inc., 675 Mass Ave,
# Cambridge, MA 02139, USA.

subdir := localedata

all: # Make this the default target; it will be defined in Rules.

# List with all available character set descriptions.
charmaps := $(wildcard charmaps/*)

# List with all available character set descriptions.
locales := $(wildcard locales/*)


# Get $(i18ndir) defined.
include ../Makeconfig

# Files to install.
install-others := $(addprefix $(i18ndir)/, $(charmaps) $(locales))

include ../Rules

# Install the message object files as libc.po in the language directory.
$(i18ndir)/charmaps/%: charmaps/%; $(do-install)
$(i18ndir)/locales/%: locales/%; $(do-install)
