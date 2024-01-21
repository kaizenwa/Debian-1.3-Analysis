/*
    Wn: A Server for the HTTP
    File: wndex/err.h
    Version 1.17.0
    
    Copyright (C) 1996  <by John Franks>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 1, or (at your option)
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#define ERRMSG1	"\tLine too long for index.cache: partial line is:\n %s\n"
#define ERRMSG2 "\tCan't open %s -- skipping it\n\n"
#define ERRMSG3	"\tError: Indexfile= after File=\n"
#define ERRMSG4	"\tError: \"Text=\" with no Indexfile\n"
#define ERRMSG5 "\tWarning: unknown directive %s\n"
#define ERRMSG6 "\tBad index file path %s.  Ignoring subdirectory\n"
#define ERRMSG7 "\tBad cache file path %s.  Ignoring subdirectory\n"
#define ERRMSG8 "\tWarning: %s ignored; it must be in first record\n"
#define ERRMSG9 "\tWarning:  Unable to open mime type file %s, using defaults.\n"
#define ERRMSG10 "\tNot enough memory\n"
#define ERRMSG11 "\tCorrupt mime type line in file %s\n"
#define ERRMSG12 "\tMime type file too large.\n"
#define ERRMSG13 "\tBad cache file path %s\n"
#define ERRMSG14 "\tWarning: can't open %s. Using file name as title.\n\n"
#define ERRMSG15 "\tCan't find title in %s. Using file name as title.\n\n"
#define ERRMSG16 "\tCan't rename %s -- not rewriting it\n\n"
#define ERRMSG17 "\tTitle too long: %s\n\n"
#define ERRMSG18 "\tIgnoring field with empty value; field: %s\n\tLine: %s\n\n"
#define ERRMSG19 "\tWarning: unknown file attribute: %s\n"
#define ERRMSG20 "\tWarning: unknown directory attribute: %s\n"
#define ERRMSG21 "\tToo many files in index\n"
#define ERRMSG22 "\tCan't open directory %s\n"
#define ERRMSG23 "\tTitle= not allowed as directory attribute\n"
#define ERRMSG24 "\tCan't stat file %s\n"
#define ERRMSG25 "\tCan't rename %s to %s\n"
#define ERRMSG26 "\tCan't open %s -- quitting\n\n"
#define ERRMSG27 "\tWarning: I don't understand the line: %s\n\n"

#define MSG1	"Wrote cache file %s\n"
#define MSG2	"Writing index.html file file %s\n"

