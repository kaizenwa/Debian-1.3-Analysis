/*
    Wn: A Server for the HTTP
    File: wn/csearch.c
    Version 1.14.0
    
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
#define	MAXMATCHES	(20)
#define MAXEXCEEDED "<li> <b>Maximum</b> number of matches per file exceeded.\n"
#define REGMSG1 "Please Enter a Search Term"

#define REGRES1	"<hr>\nThese are the matches for the regular expression \
<b>`%s'</b>.\n"
#define REGMISS	"Sorry, there appear to be no items containing a match for the regular expression <b>`%s'</b>.\n"
#define REGRES2	"You may repeat your search with a new regular expression.\n\
Searches are not case sensitive.\n"
#define REGRES3	"<form action=\"search=%s\">\nSearch term:\n<input name=\"query\">\n"
#define REGRES4 "<input type=\"submit\" value=\"Execute Search\">\n</form>\n"
#define REGRES5	"<form>\nSearch term:\n<input name=\"query\">\n"

