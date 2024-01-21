/*
    Wn: A Server for the HTTP
    File: wn/cgi.h
    Version 1.16.0
    
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
#define CGI_BYTECHUNK	(128*1024)

typedef struct CGI_data {
char	serv_protocol[TINYLEN],
	servsoft[TINYLEN],
	dataroot[TINYLEN + SMALLLEN],
	dirpath[TINYLEN + SMALLLEN],
	query[MIDLEN + SMALLLEN],
	pathinfo[MIDLEN + SMALLLEN],
	tpath[MIDLEN + SMALLLEN],
	scrname[MIDLEN + SMALLLEN],
	http_accept[ACCEPTLEN + TINYLEN],
	http_lang[ACCEPTLEN/4 + TINYLEN],
	http_charset[ACCEPTLEN/4 + TINYLEN],
	http_cookie[ACCEPTLEN + TINYLEN],
	http_referrer[MIDLEN + TINYLEN],
	http_ua[TINYLEN + SMALLLEN],
	http_from[TINYLEN + SMALLLEN],
	http_myhost[TINYLEN + SMALLLEN],
	authtype[TINYLEN],
	lochost[SMALLLEN],
	lport[TINYLEN],
	scheme[TINYLEN],
	rhost[MAXHOSTNAMELEN + TINYLEN],
	raddr[2*TINYLEN],
	rident[SMALLLEN + TINYLEN],
	ruser[SMALLLEN + TINYLEN],
	method[SMALLLEN],
	range[MIDLEN],
	postfile[SMALLLEN + TINYLEN],
	content[SMALLLEN + TINYLEN],
	length[TINYLEN];
} CGI_data;



