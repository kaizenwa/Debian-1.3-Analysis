/*
    Wn: A Server for the HTTP
    File: wn/content.h
    Version 1.14.0
    
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


#define MAXMIME		(100)

static char	*list[MAXMIME][2] = {
		{ "html", "text/html"},
		{ "htm", "text/html"},
		{ "cgi", "text/html"},
		{ "txt", "text/plain"},
		{ "ps", "application/postscript"},
		{ "eps", "application/postscript"},
		{ "xbm", "image/x-xbitmap"},
		{ "tex", "text/plain"},
		{ "xbm", "image/x-xbitmap"},
		{ "exe", "application/octet-stream"},
		{ "ai", "application/postscript"},
		{ "bin", "application/octet-stream"},
		{ "class", "application/octet-stream"},
		{ "tar", "application/octet-stream"},
		{ "ms", "application/octet-stream"},
		{ "dvi", "application/x-dvi"},
		{ "eps", "application/postscript"},
		{ "ps", "application/postscript"},
		{ "pdf", "application/pdf"},
		{ "hqx", "application/mac-binhex40"},
		{ "avi", "video/x-msvideo"},
		{ "gif", "image/gif"},
		{ "ief", "image/ief"},
		{ "xbm", "image/x-xbitmap"},
		{ "jpeg", "image/jpeg"},
		{ "jpg", "image/jpeg"},
		{ "jpe", "image/jpeg"},
		{ "mov", "video/quicktime"},
		{ "movie", "video/x-sgi-movie"},
		{ "mpe", "video/mpeg"},
		{ "mpeg", "video/mpeg"},
		{ "mpg", "video/mpeg"},
		{ "qt", "video/quicktime"},
		{ "xbm", "image/x-xbitmap"},
		{ "xwd", "image/x-xwindowdump"},
		{ "au", "audio/basic"},
		{ "snd", "audio/basic"},
		{ "aif", "audio/x-aiff"},
		{ "aifc", "audio/x-aiff"},
		{ "aiff", "audio/x-aiff"},
		{ "wav", "audio/x-wav"},
		{ NULL, NULL }
};

#ifdef WNDEX
static char	*enclist[10][2] = {
		{ "gz", "x-gzip"},
		{ "z", "x-compress"},
		{ NULL, NULL }
};
#endif
