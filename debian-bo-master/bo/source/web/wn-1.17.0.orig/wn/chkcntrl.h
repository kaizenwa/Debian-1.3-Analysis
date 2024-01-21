/*
    Wn: A Server for the HTTP
    File: wn/chkcntrl.h
    Version 1.09

    Copyright (C) 1994  <by John Franks>

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

#define	WN_OPT_U	(serv_perm & WN_COMP_UID) 
#define	WN_OPT_T	(serv_perm & (WN_TRUSTED_UID + WN_TRUSTED_GID))

#define is_trusted( ip)	 \
	( ((serv_perm & WN_TRUSTED_UID) && (ip->cache_uid == cache_id)) || \
	((serv_perm & WN_TRUSTED_GID) && (ip->cache_gid == cache_id)) )

#define is_atrusted( ip)	 \
	( ((serv_perm & WN_ATRUSTED_UID) && (ip->cache_uid == acache_id)) || \
	((serv_perm & WN_ATRUSTED_GID) && (ip->cache_gid == acache_id)) )




