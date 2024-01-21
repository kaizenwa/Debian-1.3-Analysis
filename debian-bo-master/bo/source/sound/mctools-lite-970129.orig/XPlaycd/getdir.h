/* Copyright (C) 1994 - 1996 
            Olav Woelfelschneider (wosch@rbg.informatik.th-darmstadt.de)

     This program is free software; you can redistribute it and/or modify
     it under the terms of the GNU General Public License as published by
     the Free Software Foundation; either version 2, or (at your option)
     any later version.

     This program is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
     GNU General Public License for more details.

     You should have received a copy of the GNU General Public License
     along with this program; if not, write to the Free Software
     Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#ifndef _GETDIR_H_
#define _GETDIR_H_

int read_dir_in_buffer(unsigned char *root,	/* root path */
		       unsigned char *path,	/* path relative to root */
		       unsigned char ***filep,
		       unsigned char ***dirp,
		       unsigned char ***namep,
		       int *files, int *dirs, int *current);

extern void free_dir_buffer(unsigned char ***files, int count);

extern int make_room_for_more(size_t count, size_t *size, size_t *inc,
			      unsigned char ***buf);

extern unsigned char *read_media_name(unsigned char *path);

#endif
