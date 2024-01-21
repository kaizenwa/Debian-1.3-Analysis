/*
    Wn: A Server for the HTTP
    File: wn/parse.h
    Version 1.07
    
    Copyright (C) 1995  <by John Franks>

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


#define NULLTOKEN		(0)
#define END			(1)
#define ELSE			(2)
#define ENDIF			(3)
#define FIELD			(4)
#define IF_TRUE			(5)
#define IF_FALSE		(6)
#define IF_ERR			(7)
#define INCLUDE			(8)
#define QUERY			(9)
#define REDIRECT		(10)
#define START			(11)
#define SECTION			(12)
#define SEARCH_ON		(13)
#define SEARCH_OFF		(14)
#define TITLE			(15)
#define MATCH_ACCEPT		(16)
#define MATCH_UA		(17)
#define MATCH_REFERRER		(18)
#define MATCH_IP		(19)
#define MATCH_HOST		(20)
#define MATCH_COOKIE		(21)
#define MATCH_QUERY		(22)
#define MATCH_PARAM_FIELD	(23)
#define MATCH_PARAM_VALUE	(24)
#define MATCH_REQUEST		(25)
#define MATCH_HOST_HEAD		(26)
#define MATCH_FIELD		(27)
#define ENVIRON			(28)
#define COMMENT			(29)


typedef struct Parsedata {
	unsigned	show;  /* see comment below */
	char		*currfile;
	int		currline;
} Parsedata;

/* 
 * The Parsedata field show has four significant bits.  The first SHOW_MSK
 * when set indicates that the parsed if clauses indicate to serve the
 * current text.  The third, SECT_MSK, when set indicates that the 
 * file is being served by virtue of a <!-- section ... directive.
 * The second, IN_SECT_MSK, indicates that the current line is inside
 * the start/end pair for the section and hence viewable.  In general
 * when show == SHOWIT the current line should be displayed.  The
 * fourth bit of show is ERR_MSK.  It is set if an error occurs, e.g.
 * inability to open an accessfile.
 */

#define SHOW_MSK	(1)
#define IN_SECT_MSK	(2)
#define SECT_MSK	(4)
#define SHOW_IT		(SHOW_MSK + IN_SECT_MSK)
#define ERR_MSK		(8)

#define PARSE_ERRMSG1	"Parse error: 'else' with no 'if'"
#define PARSE_ERRMSG2	"Parse error: 'endif' with no 'if'"
#define PARSE_ERRMSG3	"Parse error: 'start' inside 'if' construct or \
multiple starts with no 'end'"
#define PARSE_ERRMSG4	"Parse error: 'end' inside 'if' construct or \
multiple ends with no 'start'"
#define PARSE_ERRMSG5	"Parse line not understood"
#define PARSE_ERRMSG6	"Parse error: Bad if accessfile format"
#define PARSE_ERRMSG7	"Parse error: Bad if - regexp format"
#define PARSE_ERRMSG8	"Conditional text file line overflow"
#define PARSE_ERRMSG9	"Can't open Conditional text file"
#define PARSE_ERRMSG10	"Redirect attempted after text sent"
#define PARSE_ERRMSG11	"Missing quote mark in redirect URL"

