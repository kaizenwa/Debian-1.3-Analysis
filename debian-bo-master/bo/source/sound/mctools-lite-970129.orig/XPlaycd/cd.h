/* Copyright (C) 1994 - 1996
            Olav Woelfelschneider (wosch@rbg.informatik.th-darmstadt.de)

     This file contains code taken from the
     WorkBone CD-Player, Copyright (C) 1993 Thomas McWilliams

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

#include <sys/time.h>

#define ID_START DIG_RIGHT_ARROW
#define ID_STOP  DIG_LEFT_ARROW


enum {
  BTN_EXIT,
  BTN_EJECT,
  BTN_REWIND,
  BTN_PLAYSTOP,
  BTN_FF,
  BTN_PAUSE,
  BTN_SHUFFLE,
  BTN_REPEAT,
  BTN_PREV,
  BTN_NEXT,
  MSG_TIME,
  MSG_TRACK,
  MSG_STATUS,
  BMP_LOGO,
  BMP_VOLUME,
  BMP_CLOCK,
  SLD_VOLUME,

  ORD_SONGS,
  MSG_TITLE,

  NUM_GADGETS,

  REQ_POLL,
};

enum {
  MAIN_NONE,
  MAIN_EDIT,
  MAIN_REREAD,
  MAIN_WRITE,
  MAIN_PRINT,
  MAIN_DEBUG,
  MAIN_EXIT,
  MAIN_TRACK,
};

enum {
  CLK_TRACK,
  CLK_TRACKREMAIN,
  CLK_ELAPSED,
  CLK_REMAIN,

  NUM_CLKMODES,
};

extern void cd_init(void);
extern int cd_action(int request, int down);
extern void cd_update(int cds);
extern void cd_cleanup(void);
extern void start_cd(void);
extern int free_songs(void);
extern void new_cd(int reread);
extern void update_db(int reread);

struct McGadget;
struct McApp;
struct McWindow;
extern struct McGadget *Gad[NUM_GADGETS];
extern struct McApp *app;
extern struct McWindow *mainWindow;
extern int WindowVisible;
extern int NeedToPoll;
extern int cue;
extern struct cdinfo thiscd, *cd;
extern int cd_fd;
extern int cur_track;
extern char *mixer_name;
extern char *cdrom_name;
extern char *scsi_name;
extern char *myname;
extern int cur_cdmode;
extern long udiff(struct timeval *old, struct timeval *new);
extern unsigned char *currentFile;

extern int emptypoll, poll_out, active_search;

