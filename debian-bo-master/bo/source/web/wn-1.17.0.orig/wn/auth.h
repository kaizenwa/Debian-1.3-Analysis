/*
    WN: A Server for the HTTP
    File: wn/auth.h
    Version 1.15.7
    
    Copyright (C) 1995, 1996  <by John Franks>

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

#define AUTH_GRANTED	(0)
#define AUTH_DENIED	(1)
#define AUTH_EXPIRED	(2)

#define AUTHERR3	"Badly formed user info string"
#define AUTHERR4	"Can't open passwd file"
#define AUTHERR5	"Can't init dbm file"
#define AUTHERR6	"Can't open group file"
#define AUTHERR7	"No password file listed on command line"
#define AUTHERR8	"DBM code for authorization not installed"
#define AUTHERR9	"Incorrect or unknown authorization type"
#define AUTHERR10	"No AUTHORIZATION header"
#define AUTHERR11	"Authorization module failed"
#define AUTHERR12	"Improper usage: bad options"
#define AUTHERR13	"Authorization denied"
#define AUTHERR14	"Authorization expired"

#define AUTHERR16	"Authentication timed out"

#define AUTHERR_GENERIC	"Authorization module error: error number"
