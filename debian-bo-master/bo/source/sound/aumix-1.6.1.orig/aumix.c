/* aumix.c: adjust /dev/mixer   copyright (c) 1993, 1996 the authors

   AUTHORS:

   - Savio Lam (versions 0.1 and 0.2)
   - TimeCop (version 1.0)
   - Trevor Johnson <trevor@jpj.net> (versions 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 
   1.6.1)
   - gpm code based on rmev.c by Alessandro Rubini <alessandro.rubini@linux.it>
   - Kenn Humborg <kenn@wombat.ie> (version 1.4)
   - Dan Fandrich <dan@fch.wimsey.bc.ca> (version 1.4)
   - Jeff Spirko <spirko@topquark.physics.lehigh.edu> (version 1.5)

   HISTORY:

   1993-10-23 - version 0.1

   1993-11-28 - version 0.2
   - doesn't assume existence of any device (volume, bass, etc)
   - some minor bug fixes
   - some bug fixes to work correctly with SB Pro
   - should now work correctly when multiple copies are run simutaneously

   1993-11-28 - version 0.2

   1996-08-23 - version 1.0
   - added command-line interface

   1996-09-02 - version 1.1
   - fewer colors and no border to simplify full-screen interface
   - knobs are highlighted as you move to them, then brighten and
   leave a trail behind when you adjust them
   - horizontal knobs drawn with same characters as vertical ones
   - vertical controls spaced more closely so they fit in 80 columns
   - use of "<" and ">" for cursor motion, and "q" to quit program
   - use of tab, "+" and "-" keys shown on help line
   - added "h" option for command line

   1996-09-07 - version 1.2
   - two banks of horizontal controls, no special treatment of
   volume, bass, and treble (layout idea borrowed from CAM), so
   labels don't have to be jammed together
   - record/play indicators now show the letters "R" and "P"
   rather than just a red or green square, so they should be
   more self-explanatory and more monochrome-friendly
   - added use of "[" and "]" keys to set controls to 0% or 100%

   1996-09-13 - version 1.3
   - added gpm mouse support by incorporating code from rmev.c by 
   Alessandro Rubini (packaged with gpm) 
   - added Quit button for use with mouse
   - changed color of controls from white to cyan/green so the gpm 
   cursor would be more visible
   - added keys "Q" and ctrl-D to quit from program
   - removed use of two different graphic characters to show controls in use 
   or not in use

   1996-12-02 - version 1.4
   - added -b, -t, -s, and -m options (K.H. and D.F.)
   - moved printf statements after the ioctl call, since this makes more
   sense if the ioctl fails (K.H.) 
   - changed layout to allow for balance controls, and added them (T.J.) 
   - eliminated "level" array:  levels are now read by ioctl so changes by other
   processes system-wide will be properly reflected (T.J.)
   - conditional compilation:  GPM now optional, and interactive mode can be
   disabled entirely (T.J.)

   1996-12-06 - version 1.5
   - "q" options for command line to print settings without changing (J.S.)
   - command-line section modularized for compactness (T.J.) 
   - widened controls in interactive mode for greater precision in setting and
   reading (T.J.)
   - balance controls get redrawn after levels adjusted (T.J.)
   - added command-line options for imix, PCM2, reclev, igain, ogain, line1, 
   line2 and line3 (T.J.) 

   1996-12-15 - version 1.6
   - ability to save settings to a file and load them in again
   - option to use a restricted character set in interactive mode, when IBM
   graphic characters are not available
   - support for second mixer
   - changed meaning of "<" and ">" keys (now same as Tab or Enter)
   - "," and "." function synonymously with "<" and ">" so users need not shift 
   when using US keyboard
   - prettified device names removed, to save space and to allow for future
   changes to the sound driver
   - added labels for banks of level and balance controls
   - fixed bug with record/play indicators
   - record/play information updated more often
   - record/play information gets printed on query

   1996-12-17 - version 1.6.1
   - fixed problem with conditional compilation

   LICENSE:

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 2 of the License, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License along
   with this program; if not, write to the Free Software Foundation, Inc.,
   59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
   ************************************************************************** */

#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/soundcard.h>
#ifdef GPM
#include <gpm.h>
#ifndef NCURSES
#define NCURSES			/* How can you have a mouse if you don't have ncurses? */
#endif
#endif
#ifdef NCURSES
#include <ncurses.h>
#endif

#ifdef NCURSES
#define TITLE_COLOR 1
#define TITLE_COLOR2 2
#define HANDLE_COLOR 3
#define TRACK_COLOR 4
#define REC_LED_COLOR 5
#define PLAY_LED_COLOR 6
#define ACTIVE_COLOR 7
#define INACTIVE_COLOR 8
#define INCREMENT 4
#define XOFFSET 9
#define YOFFSET 2
#else				/* these are defined in ncurses.h */
#define TRUE 1
#define FALSE 0
#endif

void RefreshAllSettings(void);
void InitMixer(int minor);
void Usage(void);
void SetShowNoninter(int dev);
void LoadSettings(void);
void SaveSettings(void);
#ifdef GPM
int MouseHandler(Gpm_Event * event, void *data);
#endif
#ifdef NCURSES
void InitColors(void);
void DrawHandles(int dev);
void Inter(void);
void AdjustLevel(int dev, int incr, int setlevel);
void SwitchRP(int dev);
void InitScreen(void);
void AdjustBalance(int dev, int incr, int setlevel);
void DrawLevelBalMode(int dev, int mode);
void RedrawBalance(int dev);
void EraseLevel(int dev);
#endif

#ifdef NCURSES
WINDOW *w_panel;
#endif
FILE *setfile;
char setfile_name[256];
short setfile_opened = FALSE, setfile_write_perm = FALSE, setfile_read_perm = FALSE;
int devmask = 0, recmask = 0, recsrc = 0, stereodevs = 0, mixer_fd;
int opened = FALSE, interactive = FALSE;
int handle_char = 178, track_char = 193;
char *devname[SOUND_MIXER_NRDEVICES] = SOUND_DEVICE_LABELS;
char *optarg;

int main(int argc, char *const argv[], const char *optstring)
{
#ifdef GPM
    Gpm_Connect conn;
#endif
    int optn, i;
    InitMixer(0);
    if (argc <= 1) {
	interactive = TRUE;
    }
/* get command-line options */
    while (1) {
/*
   from the BSD 4.3 getopt(3) man page:
   It is also possible to handle digits as option letters.  This allows
   getopt() to be used with programs that expect a number (``-3'') as an
   option.  This practice is wrong, and should not be used in any current
   development.  It is provided for backward compatibility only.

   >:-)
 */
	optn = getopt(argc, argv, "dhLqSI:v:b:t:s:w:p:l:m:c:x:W:r:i:o:1:2:3:");
	if (optn == -1)
	    break;
	switch (optn) {
	case 'q':
	    optarg = "qvbtswplmcxWrio123";
	    for (i = 0; i < SOUND_MIXER_NRDEVICES; i++) {
		if ((1 << i) & devmask)
		    SetShowNoninter(i);
	    }
	    break;
/* to do: change over to use index() */
	case 'v':
	    SetShowNoninter(SOUND_MIXER_VOLUME);
	    break;
	case 'b':
	    SetShowNoninter(SOUND_MIXER_BASS);
	    break;
	case 't':
	    SetShowNoninter(SOUND_MIXER_TREBLE);
	    break;
	case 's':
	    SetShowNoninter(SOUND_MIXER_SYNTH);
	    break;
	case 'w':
	    SetShowNoninter(SOUND_MIXER_PCM);
	    break;
	case 'p':
	    SetShowNoninter(SOUND_MIXER_SPEAKER);
	    break;
	case 'l':
	    SetShowNoninter(SOUND_MIXER_LINE);
	    break;
	case 'm':
	    SetShowNoninter(SOUND_MIXER_MIC);
	    break;
	case 'c':
	    SetShowNoninter(SOUND_MIXER_CD);
	    break;
	case 'x':
	    SetShowNoninter(SOUND_MIXER_IMIX);
	    break;
	case 'W':
	    SetShowNoninter(SOUND_MIXER_ALTPCM);
	    break;
	case 'r':
	    SetShowNoninter(SOUND_MIXER_RECLEV);
	    break;
	case 'i':
	    SetShowNoninter(SOUND_MIXER_IGAIN);
	    break;
	case 'o':
	    SetShowNoninter(SOUND_MIXER_OGAIN);
	    break;
	case '1':
	    SetShowNoninter(SOUND_MIXER_LINE1);
	    break;
	case '2':
	    SetShowNoninter(SOUND_MIXER_LINE2);
	    break;
	case '3':
	    SetShowNoninter(SOUND_MIXER_LINE3);
	    break;
	case 'd':
	    InitMixer(1);
	    break;
	case 'S':		/* save to file */
	    SaveSettings();
	    break;
	case 'L':		/* load from file */
	    LoadSettings();
	    break;
	case 'I':
	    interactive = TRUE;
	    if (*optarg == 'd') {
/* user said we can't use IBM character set */
		handle_char = 'O';
		track_char = '=';
	    }
	    break;
	default:
	    Usage();
	}
    }
    if (interactive == FALSE)
	exit(0);
#ifdef NCURSES
    initscr();
    leaveok(stdscr, TRUE);
    keypad(stdscr, TRUE);
    cbreak();
    noecho();
    if (!has_colors())
	fprintf(stderr, "aumix: warning: no color capability detected.\n");
    InitColors();
    InitScreen();
#endif
#ifdef GPM
    conn.eventMask = ~0;
    conn.defaultMask = GPM_MOVE | GPM_HARD;
    conn.maxMod = ~0;
    conn.minMod = 0;
    if (Gpm_Open(&conn, 0) == -1) {
	attrset(COLOR_PAIR(TITLE_COLOR) | A_BOLD | A_ALTCHARSET);
	move(0, 35);
	addstr("mouse off");
	refresh();
    }
    gpm_handler = MouseHandler;
#endif
#ifdef NCURSES
    Inter();
#endif
#ifdef GPM
    Gpm_Close();
#endif
    close(mixer_fd);
#ifdef NCURSES
    clear();
    refresh();
    endwin();
    if (interactive) {
	if (setfile_opened)
	    fclose(setfile);
    }
#endif
    return 0;
}

void LoadSettings(void)
{
/* read settings from file */
    char tmpstring[80], recplay;
    int tmp, i, j = TRUE, left, right;
    if (getenv("HOME")) {
	sprintf(setfile_name, "%s/.aumixrc", (char *) getenv("HOME"));
    } else {
#ifdef NCURSES
	if (interactive) {
	    clear();
	    refresh();
	    endwin();
	}
#endif
	printf("aumix: problem opening ~/.aumixrc: $HOME not set\n");
	exit(1);
    }
    setfile = fopen(setfile_name, "r");
    if (!setfile) {
#ifdef NCURSES
	if (interactive) {
	    clear();
	    refresh();
	    endwin();
	}
#endif
	perror("aumix: error opening .aumixrc");
	exit(1);
    }
    for (j = TRUE; (!(j == EOF) && !(j == 0)); j = fscanf(setfile, "%5c:%3u:%3u:%1c\n", tmpstring, &left, &right, &recplay)) {
	for (i = 0; i < SOUND_MIXER_NRDEVICES; i++) {
/* cycle through device names, looking for matches */
	    if (!strncmp(tmpstring, devname[i], 5)) {
		left = (left > 100) ? 100 : left;
		left = (left < 0) ? 0 : left;
		right = (right > 100) ? 100 : right;
		right = (right < 0) ? 0 : right;
		tmp = left + (right << 8);
		if (ioctl(mixer_fd, MIXER_WRITE(i), &tmp) == -1) {
		    perror("MIXER_WRITE");
		    exit(-1);
		} else {
		    if (!interactive)
			printf("%s set to %i, %i", devname[i], left, right);
		    if ((1 << i) & recmask) {
			recsrc = (recplay == 'R') ? recsrc | (1 << i) : recsrc &
			    ~(1 << i);
			if (ioctl(mixer_fd, SOUND_MIXER_WRITE_RECSRC, &recsrc) == -1) {
			    perror("SOUND_MIXER_WRITE_RECSRC");
			    exit(-1);
			} else {
			    if (!interactive)
				printf(", %c", recplay);
			}
		    }
		    if (!interactive)
			printf("\n");
		}
	    }
	}
    }
    fclose(setfile);
#ifdef NCURSES
    if (interactive)
	RefreshAllSettings();
#endif
}

void SaveSettings(void)
{
/* save to file */
    int tmp, i;
    if (getenv("HOME")) {
	sprintf(setfile_name, "%s/.aumixrc", (char *) getenv("HOME"));
    } else {
#ifdef NCURSES
	if (interactive) {
	    clear();
	    refresh();
	    endwin();
	}
#endif
	printf("aumix: problem opening ~/.aumixrc: $HOME not set\n");
	exit(1);
    }
    setfile = fopen(setfile_name, "w");
    if (!setfile) {
#ifdef NCURSES
	if (interactive) {
	    clear();
	    refresh();
	    endwin();
	}
#endif
	perror("aumix: error opening .aumixrc");
	exit(1);
    }
    if (ioctl(mixer_fd, SOUND_MIXER_READ_RECSRC, &recsrc) == -1) {
	perror("SOUND_MIXER_READ_RECSRC");
#ifdef NCURSES
	if (interactive) {
	    clear();
	    refresh();
	    endwin();
	}
#endif
	fclose(setfile);
	exit(-1);
    }
    for (i = 0; i < SOUND_MIXER_NRDEVICES; i++) {
	if ((1 << i) & devmask) {
	    if (ioctl(mixer_fd, MIXER_READ(i), &tmp) == -1) {
		perror("MIXER_READ");
		exit(-1);
	    }
	    fprintf(setfile, "%s:%i:%i:%c\n", devname[i], (tmp & 0xFF), ((tmp >> 8) & 0xFF), ((1 << i) & recsrc ? 'R' : 'P'));
	}
    }
    fclose(setfile);
}

void InitMixer(int minor)
{
    if (minor) {
	if ((mixer_fd = open("/dev/mixer1", O_RDWR)) < 0) {
	    fprintf(stderr, "Error opening /dev/mixer1.\n");
	    exit(1);
	}
    } else {
	if ((mixer_fd = open("/dev/mixer", O_RDWR)) < 0) {
	    fprintf(stderr, "Error opening /dev/mixer.\n");
	    exit(1);
	}
    }
    if (ioctl(mixer_fd, SOUND_MIXER_READ_DEVMASK, &devmask) == -1) {
	perror("SOUND_MIXER_READ_DEVMASK");
	exit(-1);
    }
    if (ioctl(mixer_fd, SOUND_MIXER_READ_RECMASK, &recmask) == -1) {
	perror("SOUND_MIXER_READ_RECMASK");
	exit(-1);
    }
    if (ioctl(mixer_fd, SOUND_MIXER_READ_RECSRC, &recsrc) == -1) {
	perror("SOUND_MIXER_READ_RECSRC");
	exit(-1);
    }
    if (ioctl(mixer_fd, SOUND_MIXER_READ_STEREODEVS, &stereodevs) == -1) {
	perror("SOUND_MIXER_READ_STEREODEVS");
	exit(-1);
    }
    if (!devmask) {
	fprintf(stderr, "No device found.\n");
	exit(-1);
    }
}

void SetShowNoninter(int dev)
{
/* change or display settings from the command line */
    char *devstr;
    int tmp;
    if ((*optarg == 'q') || (*optarg == ' ')) {
	devstr = devname[dev];
	if (ioctl(mixer_fd, MIXER_READ(dev), &tmp) == -1) {
	    perror("MIXER_READ");
	    exit(-1);
	}
	printf("%s %i, %i", devname[dev], (tmp & 0xFF), ((tmp >> 8) & 0xFF));
	if ((1 << (dev)) & recmask) {
	    if (ioctl(mixer_fd, SOUND_MIXER_READ_RECSRC, &recsrc) == -1) {
		perror("SOUND_MIXER_READ_RECSRC");
		exit(-1);
	    }
	    printf(", %c", ((1 << dev) & recsrc ? 'R' : 'P'));
	}
	printf("\n");
	return;
    } else {
	tmp = atoi(optarg);
	tmp = (tmp > 100) ? 100 : tmp;
	tmp = (tmp < 0) ? 0 : 257 * tmp;
	if (ioctl(mixer_fd, MIXER_WRITE(dev), &tmp) == -1) {
	    perror("MIXER_WRITE");
	    exit(-1);
	}
	return;
    }
}

#ifdef NCURSES
void InitScreen()
{
    int i;
    move(0, 0);
    attrset(COLOR_PAIR(TITLE_COLOR) | A_BOLD | A_ALTCHARSET);
    printw("  uit  oad  ave         "
	   "                                            aumix 1.6.1 ");
    move(0, 1);
    attrset(COLOR_PAIR(TITLE_COLOR2) | A_BOLD | A_ALTCHARSET);
    printw("Q");
    move(0, 6);
    printw("L");
    move(0, 11);
    printw("S");
    move(LINES - 1, 0);
    addstr("Page Arrows ");
    move(LINES - 1, 19);
    addstr("Tab Enter < > ");
    move(LINES - 1, 43);
    addstr("+ - [ ] Arrows ");
    move(LINES - 1, 65);
    addstr("Space ");
    attrset(COLOR_PAIR(TITLE_COLOR) | A_BOLD | A_ALTCHARSET);
    move(LINES - 1, 12);
    addstr("Device");
    move(LINES - 1, 33);
    addstr("Level/Bal");
    move(LINES - 1, 58);
    addstr("Adjust");
    move(LINES - 1, 71);
    addstr("Rec/Play");
    refresh();
    w_panel = newwin(LINES - 2, COLS, 1, 0);
    wattrset(w_panel, COLOR_PAIR(INACTIVE_COLOR) | A_DIM | A_ALTCHARSET);
    wclear(w_panel);
    wmove(w_panel, YOFFSET - 1, XOFFSET + 1);
    waddstr(w_panel, "0         Level        100       L        Balance         R");
    RefreshAllSettings();
    for (i = 0; i < SOUND_MIXER_NRDEVICES; i++) {
	if ((1 << i) & devmask) {
	    wattrset(w_panel, COLOR_PAIR(INACTIVE_COLOR) | A_BOLD | A_ALTCHARSET);
	} else
	    wattrset(w_panel, COLOR_PAIR(INACTIVE_COLOR) | A_DIM | A_ALTCHARSET);
	/* draw control labels */
	wmove(w_panel, YOFFSET + i, XOFFSET + 28);
	waddstr(w_panel, devname[i]);
    }
    wrefresh(w_panel);
}

void RefreshAllSettings(void)
{
    int i;
    for (i = 0; i < SOUND_MIXER_NRDEVICES; i++) {
	if ((1 << i) & devmask) {
	    EraseLevel(i);
	    RedrawBalance(i);
	    wattrset(w_panel, COLOR_PAIR(HANDLE_COLOR) | A_BOLD | A_ALTCHARSET);
	    DrawHandles(i);
	    /* print record/play indicators */
	    if ((1 << (i)) & recmask) {
		wattrset(w_panel, COLOR_PAIR(((1 << i) & recsrc ? REC_LED_COLOR
			     : PLAY_LED_COLOR)) | A_BOLD | A_ALTCHARSET);
		wmove(w_panel, YOFFSET + i, XOFFSET + 0);
		waddch(w_panel, ((1 << i) & recsrc ? 'R' : 'P'));
	    }
	}
    }
    wrefresh(w_panel);
}

void InitColors(void)
{
    start_color();
    init_pair(TITLE_COLOR, COLOR_CYAN, COLOR_BLUE);
    init_pair(TITLE_COLOR2, COLOR_RED, COLOR_BLUE);
    init_pair(HANDLE_COLOR, COLOR_CYAN, COLOR_BLUE);
    init_pair(TRACK_COLOR, COLOR_BLUE, COLOR_BLACK);
    init_pair(REC_LED_COLOR, COLOR_RED, COLOR_BLACK);
    init_pair(PLAY_LED_COLOR, COLOR_GREEN, COLOR_BLACK);
    init_pair(ACTIVE_COLOR, COLOR_YELLOW, COLOR_RED);
    init_pair(INACTIVE_COLOR, COLOR_WHITE, COLOR_BLACK);
}

void DrawHandles(int dev)
{
    int temp;
    if (ioctl(mixer_fd, MIXER_READ(dev), &temp) == -1) {
	perror("MIXER_READ");
	endwin();
	exit(-1);
    }
    EraseLevel(dev);
    wattrset(w_panel, COLOR_PAIR(HANDLE_COLOR) | A_BOLD | A_ALTCHARSET);
    wmove(w_panel, YOFFSET + dev, XOFFSET + 1 + ((temp & 0x7F) + 2) / 4);
    waddch(w_panel, handle_char);
}

void Inter(void)
{
    int key, incr, dir, dev, levelbalmode;
/* find first existing device */
    for (dev = 0; !(devmask & (1 << dev)); dev++);
/* highlight device label */
    wattrset(w_panel, COLOR_PAIR(ACTIVE_COLOR) | A_BOLD | A_ALTCHARSET);
    wmove(w_panel, YOFFSET + dev, XOFFSET + 28);
    waddstr(w_panel, devname[dev]);
    DrawHandles(dev);
    DrawLevelBalMode(dev, 0);
    wrefresh(w_panel);
    levelbalmode = 0;
#ifdef GPM
    while ((key = Gpm_Getch()) != EOF) {
#else
    while (1) {
	key = getch();
#endif
	incr = 0;
	dir = 0;
	switch (key) {
	case '.':		/* no need to shift */
	case ',':
	case '<':
	case '>':
	case 9:		/* tab */
	case 10:		/* enter */
	    levelbalmode = !levelbalmode;
	    wattrset(w_panel, COLOR_PAIR(INACTIVE_COLOR) | A_BOLD | A_ALTCHARSET);
	    wmove(w_panel, YOFFSET + dev, XOFFSET + 27);
	    waddstr(w_panel, "       ");
	    wattrset(w_panel, COLOR_PAIR(ACTIVE_COLOR) | A_BOLD | A_ALTCHARSET);
	    wmove(w_panel, YOFFSET + dev, XOFFSET + 28);
	    waddstr(w_panel, devname[dev]);
	    DrawLevelBalMode(dev, levelbalmode);
	    break;
	case 27:		/* Esc */
	case 4:		/* Ctrl-D */
	case 'q':
	case 'Q':
	    return;
	    dir = -1;
	case KEY_UP:
	case KEY_PPAGE:	/* PgUp */
	    dir = -1;
	case KEY_DOWN:
	case KEY_NPAGE:	/* PgDn */
	    if (!dir)		/* if not PgUp or < pressed */
		dir = 1;
	    /* un-highlight current label */
	    DrawHandles(dev);
	    wattrset(w_panel, COLOR_PAIR(INACTIVE_COLOR) | A_BOLD | A_ALTCHARSET);
	    wmove(w_panel, YOFFSET + dev, XOFFSET + 27);
	    waddstr(w_panel, "       ");
	    wmove(w_panel, YOFFSET + dev, XOFFSET + 28);
	    waddstr(w_panel, devname[dev]);
/* switch to next existing device */
	    do {
		if (dir == 1) {
		    dev++;
		    if (dev > SOUND_MIXER_NRDEVICES - 1)
			dev = 0;
		} else {
		    dev--;
		    if (dev < 0)
			dev = SOUND_MIXER_NRDEVICES - 1;
		}
	    }
	    while (!((1 << dev) & devmask));

	    /* highlight new device label */
	    wattrset(w_panel, COLOR_PAIR(ACTIVE_COLOR) | A_BOLD | A_ALTCHARSET);
	    wmove(w_panel, YOFFSET + dev, XOFFSET + 28);
	    waddstr(w_panel, devname[dev]);
	    DrawHandles(dev);
	    DrawLevelBalMode(dev, levelbalmode);
	    break;
	case ' ':
	    SwitchRP(dev);
	    break;
	case '[':
	    if ((levelbalmode) && ((1 << dev) & stereodevs))
		AdjustBalance(dev, 0, 0);
	    else
		AdjustLevel(dev, 0, 0);
	    break;
	case ']':
	    if ((levelbalmode) && ((1 << dev) & stereodevs))
		AdjustBalance(dev, 0, 100);
	    else
		AdjustLevel(dev, 0, 100);
	    break;
	case '+':
	case KEY_RIGHT:
	    incr = INCREMENT;
	case '-':
	case KEY_LEFT:
	    if (!incr)		/* '+'/up/right not pressed */
		incr = -INCREMENT;
	    if ((levelbalmode) && ((1 << dev) & stereodevs))
		AdjustBalance(dev, incr, -1);
	    else
		AdjustLevel(dev, incr, -1);
	    break;
	case 'L':
	case 'l':
	    LoadSettings();
	    break;
	case 'S':
	case 's':
	    SaveSettings();
	    break;
	}
	wrefresh(w_panel);
    }
}

void DrawLevelBalMode(int dev, int mode)
/* arrow to show whether keyboard commands will adjust level or balance */
{
    wattrset(w_panel, COLOR_PAIR(ACTIVE_COLOR) | A_BOLD | A_ALTCHARSET);
    if ((mode) && ((1 << dev) & stereodevs)) {
	wmove(w_panel, YOFFSET + dev, XOFFSET + 33);
	waddch(w_panel, '>');
    } else {
	wmove(w_panel, YOFFSET + dev, XOFFSET + 27);
	waddch(w_panel, '<');
    }
}

void AdjustLevel(int dev, int incr, int setlevel)
/*
 *  dev: device to adjust
 *  incr: signed percentage to increase (decrease) level, ignored unless
 *      setlevel = -1
 *  setlevel: level to set directly, or -1 to increment
 */
{
    int temp;
    if ((1 << dev) & devmask) {
	if (dev > SOUND_MIXER_NRDEVICES - 1) {
	    dev = SOUND_MIXER_NRDEVICES - 1;
	} else {
	    dev = (dev < 0) ? 0 : dev;
	}
	if (ioctl(mixer_fd, MIXER_READ(dev), &temp) == -1) {
	    perror("MIXER_READ");
	    endwin();
	    exit(-1);
	}
	if ((1 << dev) & stereodevs) {
	    /* Take the average of left and right settings.
	       This messes up the balance setting. */
	    temp = ((temp & 0x7F) + ((temp >> 8) & 0x7F)) / 2;
	} else {
	    temp = (temp & 0x7F);
	}
	temp += incr;
	temp = (setlevel > -1) ? setlevel : temp;
	temp = (temp > 100) ? 100 : temp;
	temp = (temp < 0) ? 0 : temp;
	temp |= (temp << 8);
	if (ioctl(mixer_fd, MIXER_WRITE(dev), &temp) == -1) {
	    perror("MIXER_WRITE");
	    endwin();
	    exit(-1);
	}
	EraseLevel(dev);
	/* Draw handle at new position */
	wattrset(w_panel, COLOR_PAIR(HANDLE_COLOR) | A_BOLD | A_ALTCHARSET);
	wmove(w_panel, YOFFSET + dev, XOFFSET + 1 + ((temp & 0x7F) + 2) / 4);
	waddch(w_panel, handle_char);
	RedrawBalance(dev);
	wrefresh(w_panel);
    }
}

void EraseLevel(int dev)
{
/* redraw level track */
    int i;
    wattrset(w_panel, COLOR_PAIR(TRACK_COLOR) | A_BOLD | A_ALTCHARSET);
    for (i = 0; i < 26; i++) {
	wmove(w_panel, YOFFSET + dev, XOFFSET + 1 + i);
	waddch(w_panel, track_char);
    }
}

void AdjustBalance(int dev, int incr, int setabs)
/*
   *  dev: device to adjust
   *  incr: signed percentage to change balance
   *  setabs: absolute balance setting, or -1 to increment only
   *  Balance settings go from 0 to 100.
   *  At setting of 0, right amplitude is 0.
 */
{
    int balset, max, left, right, temp, left_orig, right_orig;
    if (((1 << dev) & devmask) & (dev > SOUND_MIXER_NRDEVICES - 1))
	dev = SOUND_MIXER_NRDEVICES - 1;
    if (ioctl(mixer_fd, MIXER_READ(dev), &temp) == -1) {
	perror("MIXER_READ");
	endwin();
	exit(-1);
    }
    left = temp & 0x7F;
    right = (temp >> 8) & 0x7F;
    left_orig = left;
    right_orig = right;
    max = (left > right) ? left : right;
    if (max) {
	balset = (left > right) ? 50 * right / max : 100 - (50 * left / max);
    } else {
	balset = 50;
    }
    balset = (setabs == -1) ? balset + incr : setabs;
    balset = (balset > 100) ? 100 : balset;
    balset = (balset < 0) ? 0 : balset;
    if (balset > 49) {
	left = max * (100 - balset) / 50;
	right = max;
    } else {
	right = max * balset / 50;
	left = max;
    }

/* hack to keep controls from getting stuck */
    if (right_orig == right && left_orig == left && setabs == -1) {
	if (incr > 0 && balset < 50) {
	    left--;
	    right++;
	    left = (left < 0) ? 0 : left;
	    right = (right > 100) ? 100 : right;
	}
	if (incr < 0 && balset > 50) {
	    left++;
	    right--;
	    left = (left > 100) ? 100 : left;
	    right = (right < 0) ? 0 : right;
	}
    }
    temp = (right << 8) | left;
    if (ioctl(mixer_fd, MIXER_WRITE(dev), &temp) == -1) {
	perror("MIXER_WRITE");
	endwin();
	exit(-1);
    }
    /* draw handle at new position */
    RedrawBalance(dev);
}

void RedrawBalance(int dev)
/* redraw balance track */
{
    int left, right, max, temp, balset;
    if ((1 << dev) & stereodevs) {
	wattrset(w_panel, COLOR_PAIR(TRACK_COLOR) | A_BOLD | A_ALTCHARSET);
	for (temp = 0; temp < 26; temp++) {
	    wmove(w_panel, YOFFSET + dev, XOFFSET + 34 + temp);
	    waddch(w_panel, track_char);
	}
	if (ioctl(mixer_fd, MIXER_READ(dev), &temp) == -1) {
	    perror("MIXER_READ");
	    endwin();
	    exit(-1);
	}
	left = temp & 0x7F;
	right = (temp >> 8) & 0x7F;
	max = (left > right) ? left : right;
	if (temp) {
	    balset = (left > right) ? 50 * right / max : 100 - (50 * left / max);
	} else {
	    balset = 50;
	}
	wattrset(w_panel, COLOR_PAIR(HANDLE_COLOR) | A_BOLD | A_ALTCHARSET);
	wmove(w_panel, YOFFSET + dev, XOFFSET + 34 + balset / 4);
	waddch(w_panel, handle_char);
	wrefresh(w_panel);
    }
}

void SwitchRP(int dev)
{
    /* change record/play and print indicators */
    if ((1 << dev) & recmask) {
	if (ioctl(mixer_fd, SOUND_MIXER_READ_RECSRC, &recsrc) == -1) {
	    perror("SOUND_MIXER_READ_RECSRC");
	    exit(-1);
	}
	if (recsrc & (1 << dev))
	    recsrc &= ~(1 << dev);	/* turn off recording */
	else
	    recsrc |= (1 << dev);	/* turn on recording */
	if (ioctl(mixer_fd, SOUND_MIXER_WRITE_RECSRC, &recsrc) == -1) {
	    perror("SOUND_MIXER_WRITE_RECSRC");
	    endwin();
	    exit(-1);
	}
	if (ioctl(mixer_fd, SOUND_MIXER_READ_RECSRC, &recsrc) == -1) {
	    perror("SOUND_MIXER_READ_RECSRC");
	    exit(-1);
	}
	wattrset(w_panel, COLOR_PAIR(((1 << dev) & recsrc
	     ? REC_LED_COLOR : PLAY_LED_COLOR)) | A_BOLD | A_ALTCHARSET);
	wmove(w_panel, YOFFSET + dev, XOFFSET + 0);
	waddch(w_panel, ((1 << dev) & recsrc ? 'R' : 'P'));
	wrefresh(w_panel);
    }
}
#endif				/* end of ncurses stuff */

#ifdef GPM
int MouseHandler(Gpm_Event * event, void *data)
{
    int dev, temp;
/* quit */
    if ((event->y == 1) & (event->x == 2) && (event->type & GPM_DOWN)) {
	Gpm_Close();
	close(mixer_fd);
	clear();
	refresh();
	endwin();
	exit(0);
    }
/* load from file */ if ((event->y == 1) & (event->x == 7) && (event->type & GPM_DOWN)) {
	LoadSettings();
	return 0;
    }
/* save to file */
    if ((event->y == 1) & (event->x == 12) && (event->type & GPM_DOWN)) {
	SaveSettings();
	return 0;
    }
    dev = event->y - 2 - YOFFSET;
    temp = event->x - 2 - XOFFSET;
    if ((temp > 32) & (temp < 59)) {
	temp -= 33;
	temp *= 4;
	if (((event->type & GPM_DOWN) || (event->type & GPM_DRAG)) && ((1 << dev) & stereodevs)) {
	    AdjustBalance(dev, 0, temp);
	    return 0;
	}
	return 0;
    }
    if ((event->type & GPM_DOWN) && (temp == -1)) {
	SwitchRP(dev);
	return 0;
    }
    if (temp < 26) {
	temp *= 4;
	if ((event->type & GPM_DOWN) || (event->type & GPM_DRAG)) {
	    AdjustLevel(dev, 0, temp);
	}
    }
    return 0;
}
#endif				/* end of GPM stuff */

void Usage(void)
{
    printf("aumix 1.6.1 usage: aumix [[ -<device option>[ ]<level>|q[uery],\n");
    printf("[ -<device option>[ ]<level>|q[uery], [...]][-d] [-h] [-I{d|c}] [-L] [-q] [-S]\n\n");
    printf("device options:\n");
    printf("  v: main Volume           x: miX monitor\n");
    printf("  b: Bass                  W: PCM2\n");
    printf("  t: Treble                r: Record\n");
    printf("  s: Synth                 i: Input gain\n");
    printf("  w: PCM                   o: Output gain\n");
    printf("  p: PC speaker            1: line1\n");
    printf("  l: Line                  2: line2\n");
    printf("  m: Mic                   3: line3\n");
    printf("  c: CD\n\n");
    printf("other options:\n");
    printf("   d:  Dual mixers; adjust secondary\n");
    printf("   h:  this Helpful message\n");
    printf("  Id:  start in Interactive mode; Don't use IBM graphic characters\n");
    printf("  Ic:  start in Interactive mode after doing non-interactive functions\n");
    printf("   L:  Load settings from ~/.aumixrc\n");
    printf("   q:  Query all devices and print their settings\n");
    printf("   S:  Save settings to ~/.aumixrc\n");
    exit(1);
}
