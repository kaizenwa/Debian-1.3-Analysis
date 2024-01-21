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

#ifndef _XMEDIT_H_
#define _XMEDIT_H_

extern void create_edit_window(McApp *app, unsigned char *path,
			       unsigned char *root, int count,
			       unsigned char **dirs, int numdirs);
extern int is_being_edited(unsigned char *path);
extern int is_editing(void);
extern void update_editor_dirs(unsigned char **dirs, int numdirs);

#endif
