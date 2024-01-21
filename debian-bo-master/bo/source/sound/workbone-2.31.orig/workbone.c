/*   WorkBone CD Rom Player Software

   Copyright (c) 1994  Thomas McWilliams

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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include <signal.h>
#include <sys/utsname.h>
#include <unistd.h>
#include <termios.h>
#include <mntent.h>
#include <getopt.h>
#include "struct.h"
#include "workbone.h"

void control_panel (void);
void rl_ttyset (int Reset);
void help (void);
static inline void playtime (void);
static inline int kvers (void);
static inline void setgraf (void);
static inline void cls (void);

char *cur_trackname;		/* Take a guess */
int cur_index = 0;		/* Current index mark */
int cur_frame;			/* Current frame number */
struct play *playlist = NULL;
struct cdinfo thiscd, *cd = &thiscd;
int cur_track = -1;		/* Current track number, starting at 1 */
char *cur_artist;		/* Name of current CD's artist */
char cur_avoid;			/* Avoid flag */
char cur_contd;			/* Continued flag */
char *cur_cdname;		/* Album name */
int cur_nsections;		/* Number of sections currently defined */
int exit_on_eject = 0;

int cur_balance = 10, info_modified;
int cur_track, cur_pos_abs, cur_pos_rel, cur_tracklen, cur_cdlen, cur_cdmode,
  cur_ntracks, cur_lasttrack, cur_firsttrack, cur_listno;
char tmptime[100];
char *tottime;
static int kern = 0;
static int usegraf = TRUE;
static char gon[100];
static char gof[100];

const char *wb_hdr[] =
{
  "\nWorkBone version 2.3 Copyright 1994 (c) Thomas McWilliams",
  "Free Software under GNU General Public License. NO WARRANTY.\n",
  NULL};

const char *gpl_terms[] =
{
  "This program is free software; you can redistribute it and/or modify",
  "it under the terms of the GNU General Public License as published by",
  "the Free Software Foundation; either version 2 of the License, or",
  "(at your option) any later version.",
  " ",
  "This program is distributed in the hope that it will be useful,",
  "but WITHOUT ANY WARRANTY; without even the implied warranty of",
  "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the",
  "GNU General Public License for more details.",
  " ",
  "You should have received a copy of the GNU General Public License",
  "along with this program; if not, write to the Free Software",
  "Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.",
  " ",
  NULL};

int
main (int argc, char *argv[])
{

  int sss, sel_stat, dly;
  int fastin = FALSE;
  int scmd = 0, tmppos = 0;
  int save_track = 1;
  fd_set rset;
  struct timeval mydelay;
  struct mntent *mnt;
  char copt;
  FILE *fp;
  thiscd.trk = NULL;
  thiscd.lists = NULL;
  mydelay.tv_sec = 0;
  mydelay.tv_usec = 500000;	/* initial delay 1/2 sec */
  tottime = tmptime;
  kern = kvers ();
  /* get options */
  for (;;)
    {
      if ((copt = getopt (argc, argv, "ahqtvV")) == EOF)
	break;
      switch (copt)
	{
	case 'a':
	  usegraf = FALSE;
	  break;
	case 'q':
	  fastin = TRUE;
	  break;
	case 't':
	  show_terms (wb_hdr);
	  show_terms (gpl_terms);
	  exit (0);
	default:
	  printf ("\nWorkBone version 2.3 Copyright 1994 (c) Thomas McWilliams\n");
	  printf ("Free Software under GNU General Public License. NO WARRANTY.\n\n");
	  printf ("Usage:  workbone [ -v | -h | -q | -a ]\n");
	  printf ("           -v : version\n");
	  printf ("           -h : help\n");
	  printf ("           -q : fast start (no wait for init)\n");
	  printf ("           -a : use 7-bit ascii for display\n");
	  printf ("           -t : show license and no warranty\n\n");
	  printf ("Type 'workbone' to start program.\n\n");
	  printf ("Engage the NUM LOCK on your keypad. From inside WorkBone\n");
	  printf ("pressing DEL on numeric keypad will display a help menu.\n\n");
	  exit (0);
	}
    }
  /* set graphics */
  setgraf ();
  /* check if drive is mounted (from Mark Buckaway's cdplayer code) */
  if ((fp = setmntent (MOUNTED, "r")) == NULL)
    {
      fprintf (stderr, "Couldn't open %s: %s\n", MOUNTED, strerror (errno));
      exit (1);
    }
  while ((mnt = getmntent (fp)) != NULL)
    {
      if (strcmp (mnt->mnt_type, "iso9660") == 0)
	{
	  fputs ("CDROM already mounted. Operation aborted.\n", stderr);
	  endmntent (fp);
	  exit (1);
	}
    }
  endmntent (fp);

  /*  display control panel template */
  control_panel ();

  /* delay while CD drive initializes itself */
  if (!fastin)
    for (dly = 6; dly > -1; dly--)
      {
	printf (MTAB3 "wait ... initializing %d\r", dly);
	fflush (stdout);
	sleep (1);
      }
  printf (MTAB3 "                         \r");
  fflush (stdout);
  sss = cd_status ();
  signal (SIGINT, SIG_IGN);
  rl_ttyset (0);
  if (sss == 0 || sss == 4)
    goto done;
  cur_track = 1;

  do
    {
/*
   use select() to update status 
 */

      FD_ZERO (&rset);
      FD_SET (STDIN_FILENO, &rset);
      sel_stat = select (4, &rset, NULL, NULL, &mydelay);
      if (sel_stat == 0)
	{
	  mydelay.tv_sec = WBS_DELAY;
	  mydelay.tv_usec = WBU_DELAY;
	}
      sss = cd_status ();
      if (sss == 0 || sss == 4 || cur_cdmode == CDEJECT)
	{
	  scmd = '0';
	  goto done;
	}
      if (cur_cdmode < 1)
	save_track = 1;
      /* if key was pressed, parse it and do function */
      if (FD_ISSET (STDIN_FILENO, &rset))
	{
	  read (0, &scmd, 1);
	  switch (scmd & 255)
	    {
	    case '.':
	    case '?':
	      help ();
	      break;
	    case '1':
	      if (cur_cdmode == CDPLAY)
		{
		  tmppos = cur_pos_rel - 15;
		  play_cd (cur_track, tmppos > 0 ? tmppos : 0, cur_ntracks + 1);
		  mydelay.tv_sec = 0;
		}
	      break;
	    case '3':
	      if (cur_cdmode == CDPLAY)
		{
		  tmppos = cur_pos_rel + 15;
		  if (tmppos < thiscd.trk[cur_track - 1].length)
		    {
		      play_cd (cur_track, tmppos, cur_ntracks + 1);
		      mydelay.tv_sec = 0;
		    }
		}
	      break;
	    case '2':
	      stop_cd ();
	      eject_cd ();
	      break;
	    case '4':
	      cur_track--;
	      if (cur_track < 1)
		cur_track = cur_ntracks;
	      play_cd (cur_track, 0, cur_ntracks + 1);
	      mydelay.tv_sec = 0;
	      break;
	    case '5':
	      if (cur_cdmode == CDPLAY)
		play_cd (cur_track, 0, cur_ntracks + 1);
	      mydelay.tv_sec = 0;
	      break;
	    case '6':
	      if (cur_track == cur_ntracks)
		cur_track = 0;
	      play_cd (cur_track + 1, 0, cur_ntracks + 1);
	      mydelay.tv_sec = 0;
	      break;
	    case '7':
	      printf (MTAB3 "stop                         \r");
	      save_track = cur_track;
	      stop_cd ();
	      break;
	    case '8':
	      if (cur_cdmode == CDPLAY || cur_cdmode == CDPAUZ)
		{
		  pause_cd ();
		}
	      break;
	    case '9':
	      if (cur_cdmode == CDSTOP || cur_cdmode == CDNULL)
		{
		  play_cd (save_track, 0, cur_ntracks + 1);
		  mydelay.tv_sec = 0;
		}
	      break;
	    case 0x1b:
	      read (0, &scmd, 1);
	      read (0, &scmd, 1);
	      printf (MTAB3 "Turn ON Num Lock!                    %c\r", 7);
	      fflush (stdout);
	      sleep (1);
	      printf (MTAB3 "                        \r");
	      fflush (stdout);
	      break;
	    default:
	      break;
	    }
	}
      /* update display of which track is playing */
      switch (cur_cdmode)
	{
	case 0:		/* CDNULL */
	  cur_track = save_track = 1;
	  printf (MTAB3 "stopped                    \r");
	  break;
	case 1:		/* CDPLAY */
	  playtime ();
	  printf (MTAB3 "playing #%d%s       \r", cur_track, tottime);
	  break;
	case 3:		/* CDPAUZ */
	  printf (MTAB3 "pause   #%d  \r", cur_track);
	  break;
	case 4:		/* CDSTOP */
	  printf (MTAB3 "stopped #%d \r", save_track);
	  break;
	case 5:		/* CDEJECT */
	  goto done;
	default:
	  printf (MTAB3 "cur_cdmode %d       \r", cur_cdmode);
	}
      fflush (stdout);

    }
  while (scmd != '0');
done:
  if (thiscd.trk != NULL)
    free (thiscd.trk);
  printf ("\n");
  rl_ttyset (1);
  signal (SIGINT, SIG_DFL);
  return (0);
}

/* takes terminal in and out of raw mode */
void
rl_ttyset (int Reset)
{
  static struct termios old;
  struct termios new;

  if (Reset == 0)
    {
      (void) tcgetattr (0, &old);
      new = old;
      new.c_lflag &= ~(ECHO | ICANON);
      new.c_iflag &= ~(ISTRIP | INPCK);
      (void) tcsetattr (0, TCSANOW, &new);
    }
  else
    (void) tcsetattr (0, TCSANOW, &old);
}


/* Copy into a malloced string. */
void
strmcpy (char **t, const char *s)
{
  if (*t != NULL)
    free (*t);

  *t = malloc (strlen (s) + 1);
  if (*t == NULL)
    {
      perror ("strmcpy");
      exit (1);
    }

  (void) strcpy (*t, s);
}

/* display keypad template on screen */
void
control_panel ()
{
  if (usegraf)
    {
      printf ("%s\n", gon);
      printf (ROWT);
      printf (ROW0);
      printf (ROW1);
      printf (ROW0);
      printf (ROW2);
      printf (ROW0);
      printf (ROW3);
      printf (ROW0);
      printf (ROW4);
      printf (ROW0);
      printf (ROWB, gof);
    }
  else
    {
      printf ("\n");
      printf (AROWT);
      printf (AROW0);
      printf (AROW1);
      printf (AROW0);
      printf (AROW2);
      printf (AROW0);
      printf (AROW3);
      printf (AROW0);
      printf (AROW4);
      printf (AROW0);
      printf (AROWB);
    }
}

/* ansi codes to clear screen */
static void
cls (void)
{
  if (usegraf)
    {
      printf ("\033[2J\033[H");
    }
  else
    {
      int count = 60;
      while (count--)
	printf ("\n");
    }
}

/* print help screen */
void
help ()
{
  const char *glst[] =
  {
    "\376\376", "\tstop                       ",
    "||", "\tpause/resume",
    "|\020", "\tplay",
    "\036\036", "\tre-start current selection",
    "|\021", "\tprevious selection",
    "\020|", "\tnext selection",
    "\021\021", "\tgo backward 15 seconds",
    "\020\020", "\tgo foreward 15 seconds",
    "..", "\tabort workbone",
    "quit", "\texit workbone (music continues)",
    "?", "\tdisplay help screen", NULL};

  const char *alst[] =
  {
    "[]", "\tstop                       ",
    "||", "\tpause/resume",
    "=>", "\tplay",
    "^^", "\tre-start current selection",
    "< ", "\tprevious selection",
    "> ", "\tnext selection",
    "<<", "\tgo backward 15 seconds",
    ">>", "\tgo foreward 15 seconds",
    "..", "\tabort workbone",
    "quit", "\texit workbone (music continues)",
    "?", "\tdisplay help screen", NULL};

  int i = 0;
  cls ();
  while (glst[i] != NULL)
    {
      printf ("\t\t\t%s%s%s%s\n", gon, (usegraf ? glst[i] : alst[i]),
	      gof, (usegraf ? glst[i + 1] : alst[i + 1]));
      i += 2;
    }
  control_panel ();
}

inline void
playtime (void)
{
  static int mymin, emin;
  static int mysec, esec;
  int tmp = 0;

  if (cur_pos_rel > 0 && (tmp = cur_pos_rel % 60) == mysec)
    return;
  mysec = tmp;
  mymin = cur_pos_rel / 60;
  esec = cur_pos_abs % 60;
  emin = cur_pos_abs / 60;
  sprintf (tmptime, "%s %02d:%02d  %02d:%02d", cur_track > 9 ? " " : "  ",
	   mymin, mysec, emin, esec);
  return;
}

inline int
kvers (void)
{
  struct utsname ubf;
  uname (&ubf);
  if (ubf.release[0] == '1' && ubf.release[2] == '0')
    return 0;
  else
    return 1;
}

static inline void
setgraf (void)
{
  if (usegraf)
    {
      strcpy (gon, (kern == 0 ? OGON : GON));
      strcpy (gof, (kern == 0 ? OGOF : GOF));
    }
  else
    {
      gon[0] = 0;
      gof[0] = 0;
    }
}

void
show_terms (const char **p)
{
  int i = 0;

  while (p[i] != NULL)
    {
      puts (p[i]);
      i++;
    }
}
