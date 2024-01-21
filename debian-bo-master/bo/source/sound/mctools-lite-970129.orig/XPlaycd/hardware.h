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

#ifdef linux
#include <linux/cdrom.h>
#else
#include <sundev/srreg.h>
#endif /* linux */

struct McApp;
extern int cd_fd;
extern int cd_status(void);
extern void cd_volume(int left, int right);
extern int play_cd(int start, int pos, int end);
extern int replay_cd(int end);
extern void pr_trackinfo(struct McApp *appl, int track);
extern int checkmount(void);
extern int eject_cd(void);
extern int load_cd(void);
extern void stop_cd(void);
extern void pause_cd(void);







