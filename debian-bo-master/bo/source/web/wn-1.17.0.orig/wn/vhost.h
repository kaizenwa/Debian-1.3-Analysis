/*
    Wn: A Server for the HTTP
    File: wn/vhost.h
    Version 1.16.0
    
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

#ifdef USE_VIRTUAL_HOSTS
char *vhostlist[][3] =
{
	{ "localhost", "127.0.0.1", ROOT_DIR },
	{ "abc.def.com" , "123.123.121.1", "/usr/local/data1" },
	{ "xxx.def.com" , "123.123.123.1", "/usr/local/data2" },
	{ "www.def.com", "123.123.123.2", "/usr/local/data3" },
	{ NULL, NULL, NULL }
};
#endif



/*
 * To implement multiple hostname/rootdir pairs  you must edit this 
 * file and enter the host names, IP addresses, and root directories.
 * The format of an entry is 
 *       { "<hostname>", "<ip_address>", "<rootdir>" },
 */



