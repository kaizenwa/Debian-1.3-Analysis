/*
    Wn: A Server for the HTTP
    File: wn/common.h
    Version 1.15.7
    
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

#ifndef TRUE
#define TRUE	(1)
#endif

#ifndef FALSE
#define FALSE	(0)
#endif

#define STANDARD_PORT	(80)

#define	BIGLEN		(4096)
#define	CACHELINE_LEN	(2*BIGLEN)
#define	MIDLEN		(2048)
#define	RANGELEN	(1024)
#define	SMALLLEN	(256)
#define	TINYLEN		(32)
#define INBUFFSIZE	(2048)
#define OUT_BUFFSIZE	(128)

/* Bits in the Request attributes */
#define WN_DYNAMIC	(1)
#define WN_NONDYNAMIC	(2)
#define	WN_INCLUDE	(4)
#define	WN_WRAPPED	(8)
#define	WN_SWRAPPED	(16)
#define	WN_FILTERED	(32)
#define	WN_NOSEARCH	(64)
#define	WN_PARSE	(128)
#define	WN_NOPARSE	(256)
#define	WN_CGI		(512)
#define WN_ISMAP	(1024)
#define WN_NOCACHE	(2048)
#define WN_UNBUFFERED	(4096)



