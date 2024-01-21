@echo off
rem Configure batch file for `Wget' utility
rem Copyright (C) 1995, 1996, 1997 Free Software Foundation, Inc.

rem This program is free software; you can redistribute it and/or modify
rem it under the terms of the GNU General Public License as published by
rem the Free Software Foundation; either version 2 of the License, or
rem (at your option) any later version.

rem This program is distributed in the hope that it will be useful,
rem but WITHOUT ANY WARRANTY; without even the implied warranty of
rem MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
rem GNU General Public License for more details.

rem You should have received a copy of the GNU General Public License
rem along with this program; if not, write to the Free Software
rem Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

cls
copy windows\config.h.ms config.h > nul
copy windows\Makefile.ms src\Makefile > nul
cd src
echo Type NMAKE to start compiling.
echo If it doesn't work, try executing MSDEV\BIN\VCVARS32.BAT first,
echo and then NMAKE.

