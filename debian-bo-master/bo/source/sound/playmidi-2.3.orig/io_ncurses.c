/************************************************************************
   io_ncurses.c  -- shows midi events using ncurses or printf

   Copyright (C) 1994-1996 Nathan I. Laredo

   This program is modifiable/redistributable under the terms
   of the GNU General Public Licence.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
   Send your comments and all your spare pocket change to
   laredo@gnu.ai.mit.edu (Nathan Laredo) or to PSC 1, BOX 709, 2401
   Kelly Drive, Lackland AFB, TX 78236-5128, USA.
 *************************************************************************/
#include "playmidi.h"
#include <ncurses/curses.h>
#include <sys/time.h>
#include <unistd.h>

char *metatype[7] =
{"Text", "Copyright Notice", "Sequence/Track name",
 "Instrument Name", "Lyric", "Marker", "Cue Point"};

char *sharps[12] =		/* for a sharp key */
{"C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "B#", "B"};
char *flats[12] =		/* for a flat key */
{"C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Gb", "A", "Bb", "B"};
char *majflat[15] =		/* name of major key with 'x' flats */
{"C", "F", "Bb", "Eb", "Ab", "Db", "Gb", "Cb", "Fb", "Bbb", "Ebb",
 "Abb", "Gbb", "Cbb", "Fbb"};	/* only first 8 defined by file format */
char *majsharp[15] =		/* name of major key with 'x' sharps */
{"C", "G", "D", "A", "E", "B", "F#", "C#", "G#", "D#", "A#",
 "E#", "B#", "F##", "C##"};	/* only first 8 defined by file format */
char *minflat[15] =		/* name of minor key with 'x' flats */
{"A", "D", "G", "C", "F", "Bb", "Eb", "Ab", "Db", "Gb", "Cb",
 "Fb", "Bbb", "Ebb", "Abb"};	/* only first 8 defined by file format */
char *minsharp[15] =		/* name of minor key with 'x' sharps */
{"A", "E", "B", "F#", "C#", "G#", "D#", "A#", "E#", "B#", "F##",
 "C##", "G##", "D##", "A##"};	/* only first 8 defined by file format */

char *drumset[11] =
{
 "Standard Kit", "Room Kit", "Power Kit", "Electronic Kit", "TR-808 Kit",
    "Jazz Kit", "Brush Kit", "Orchestra Kit", "Sound FX Kit",
    "Downloaded Program", "MT-32 Kit"
};

char *drum3ch[11] =
{
    "STD", "RM.", "PWR", "ELE", "808",
    "JAZ", "BRU", "ORC", "SFX", "PRG", "M32"
};

#define SET(x) (x == 8 ? 1 : x >= 16 && x <= 23 ? 2 : x == 24 ? 3 : \
		x == 25  ? 4 : x == 32 ? 5 : x >= 40 && x <= 47 ? 6 : \
		x == 48 ? 7 : x == 56 ? 8 : x >= 96 && x <= 111 ? 9 : \
		x == 127 ? 10 : 0)

extern int graphics, verbose, perc;
extern int format, ntrks, division;
extern unsigned long int ticks;
extern char *filename, *gmvoice[256];
extern float skew;
extern void seq_reset();
extern struct timeval start_time;

struct timeval now_time, want_time;
char textbuf[1024], **nn;
int i, ytxt;

void close_show(error)
int error;
{
    if (graphics) {
	attrset(A_NORMAL);
	refresh();
	endwin();
    }
    exit(error);
}

#define CHN		(cmd & 0xf)
#define NOTE		((int)data[0])
#define VEL		((int)data[1])
#define OCTAVE		(NOTE / 12)
#define OCHAR		(0x30 + OCTAVE)
#define XPOS		(4 * CHN)
#define YPOS		(LINES - NOTE % (LINES - 2))
#define NNAME		nn[NOTE % 12]

int cdeltat(t1, t2)
struct timeval *t1;
struct timeval *t2;
{
    int d1, d2;

    d1 = t1->tv_sec - t2->tv_sec;
    if((d2 = t1->tv_usec - t2->tv_usec) < 0)
	(d2 += 1000000, d1 -= 1);
    d2 /= 10000;
    return (d2 + d1 * 100);
}
int updatestatus()
{
    int ch, d1, d2;

    want_time.tv_sec = start_time.tv_sec + (ticks / 100);
    want_time.tv_usec = start_time.tv_usec + (ticks % 100) * 10000;
    if (want_time.tv_usec > 1000000)
	(want_time.tv_usec -= 1000000, want_time.tv_sec++);

    do {
	attrset(A_BOLD);
	if ((ch = getch()) != ERR)
	    switch (ch) {
	    case KEY_RIGHT:
		if ((skew -= 0.01) < 0.25)
		    skew = 0.25;
		if (graphics)
		    mvprintw(9, 64, "skew=%0.2f", skew);
		break;
	    case KEY_LEFT:
		if ((skew += 0.01) > 4)
		    skew = 4.0;
		if (graphics)
		    mvprintw(9, 64, "skew=%0.2f", skew);
		break;
	    case KEY_PPAGE:
	    case KEY_UP:
		seq_reset();
		return (ch == KEY_UP ? 0 : -1);
		break;
	    case 18:
	    case 12:
		wrefresh(curscr);
		break;
	    case 'q':
	    case 'Q':
	    case 3:
		close_show(0);
		break;
	    default:
		return 1;	/* skip to next song */
		break;
	    }
	gettimeofday(&now_time, NULL);
	d1 = now_time.tv_sec - start_time.tv_sec;
	d2 = now_time.tv_usec - start_time.tv_usec;
	if (d2 < 0)
	    (d2 += 1000000, d1 -= 1);
	mvprintw(0, 66, "%02d:%02d.%d", d1 / 60, d1 % 60, d2 / 100000);
	refresh();
	d1 = cdeltat(&want_time, &now_time);
	if (d1 > 15)
	    usleep(100000);
    } while (d1 > 10);
    return NO_EXIT;
}

void showevent(cmd, data, length)
int cmd;
unsigned char *data;
int length;
{
    if (cmd < 8 && cmd > 0) {
	if (verbose) {
	    printf("%s: ", metatype[cmd - 1]);
	    for (i = 0; i < length; i++)
		putchar(data[i]);
	    putchar('\n');
	} else {
	    attrset(A_BOLD | COLOR_PAIR(cmd));
	    strncpy(textbuf, data, length < COLS - 66 ? length : COLS - 66);
	    if (length < 1024)
		textbuf[length] = 0;
	    mvaddstr(ytxt, 64, textbuf);
	    if ((++ytxt) > LINES - 2)
		ytxt = 10;
	    move(ytxt, 64);
	    clrtoeol();
	}
    } else if (cmd == key_signature) {
	if (graphics || verbose)
	    nn = ((NOTE & 0x80) ? flats : sharps);
	if (verbose) {
	    if (VEL)	/* major key */
		printf("Key: %s major\n", (!(NOTE & 0x80) ?
			majsharp[NOTE] : majflat[256-NOTE]));
	    else	/* minor key */
		printf("Key: %s minor\n", (!(NOTE & 0x80) ?
			minsharp[NOTE] : minflat[256-NOTE]));
	}
    } else
	switch (cmd & 0xf0) {
	case MIDI_KEY_PRESSURE:
	    if (verbose > 4)
		printf("Chn %d Key Pressure %s%c=%d\n",
		       1 + (cmd & 0xf), NNAME, OCHAR, VEL);
	    break;
	case MIDI_NOTEON:
	    if (graphics)
		if (VEL) {
		    attrset(A_BOLD | COLOR_PAIR((CHN % 6 + 1)));
		    if (!ISPERC(CHN) || NOTE > 127 ||
			gmvoice[NOTE + 128] == NULL)
			mvprintw(YPOS, XPOS, "%s%c", NNAME, OCHAR);
		    else
			mvprintw(YPOS, XPOS, "%c%c%c",
				 gmvoice[NOTE + 128][0],
				 gmvoice[NOTE + 128][1],
				 gmvoice[NOTE + 128][2]);
		} else
		    mvaddstr(YPOS, XPOS, "   ");
	    else if (verbose > 5)
		printf("Chn %d Note On %s%c=%d\n",
		       1 + (cmd & 0xf), NNAME, OCHAR, VEL);
	    break;
	case MIDI_NOTEOFF:
	    if (graphics)
		mvaddstr(YPOS, XPOS, "   ");
	    else if (verbose > 5)
		printf("Chn %d Note Off %s%c=%d\n",
		       1 + (cmd & 0xf), NNAME, OCHAR, VEL);
	    break;
	case MIDI_CTL_CHANGE:
	    if (verbose > 5)
		printf("Chn %d Ctl Change %d=%d\n",
		       1 + (cmd & 0xf), NOTE, VEL);
	    break;
	case MIDI_CHN_PRESSURE:
	    if (verbose > 5)
		printf("Chn %d Pressure=%d\n",
		       1 + (cmd & 0xf), NOTE);
	    break;
	case MIDI_PITCH_BEND:
	    {
		register val = (VEL << 7) | NOTE;

		if (graphics) {
		    attrset(A_BOLD);
		    if (val > 0x2000)
			mvaddch(1, 4 * CHN + 2, '^');
		    else if (val < 0x2000)
			mvaddch(1, 4 * CHN + 2, 'v');
		    else
			mvaddch(1, 4 * CHN + 2, ' ');
		} else if (verbose > 4)
		    printf("Chn %d Bender=0x%04x\n",
			   1 + CHN, val);
	    }
	    break;
	case MIDI_PGM_CHANGE:
	    if (graphics) {
		attrset(COLOR_PAIR((CHN % 6 + 1)));
		if (!ISPERC(CHN))
		    mvaddnstr(0, 4 * CHN, gmvoice[NOTE], 3);
		else
		    mvaddstr(0, 4 * CHN, drum3ch[SET(NOTE)]);
	    } else if (verbose > 3)
		printf("Chn %d Program=%s %d\n",
		       1 + CHN, (ISPERC(CHN) ? drumset[SET(NOTE)]
				 : gmvoice[NOTE]), NOTE + 1);
	    break;
	case 0xf0:
	case 0xf7:
	    if (verbose > 2) {
		printf("Sysex(%2x): ", cmd);
		for (i = 0; i < length; i++)
		    printf("%02x", data[i]);
		putchar('\n');
	    }
	    break;
	default:
	    break;
	}
}

void init_show()
{
    char *tmp;

    nn = flats;
    ytxt = 10;
    if (graphics) {
	clear();
	attrset(A_NORMAL);
	mvprintw(0, 0, "ch1 ch2 ch3 ch4 ch5 ch6 ch7 ch8 ch9 "
		 "c10 c11 c12 c13 c14 c15 c16");
	mvprintw(2, 64, RELEASE);
	mvprintw(3, 70, "by");
	mvprintw(4, 64, "Nathan Laredo");
	tmp = strrchr(filename, '/');
	strncpy(textbuf, (tmp == NULL ? filename : tmp + 1), 14);
	mvaddstr(6, 64, textbuf);
	mvprintw(7, 64, "%d track%c", ntrks, ntrks > 1 ? 's' : ' ');
	refresh();
    } else if (verbose) {
	printf("** Now Playing \"%s\"\n", filename);
	printf("** Format: %d, Tracks: %d, Division: %d\n", format,
	       ntrks, division);
    }
}

void setup_show(argc, argv)
int argc;
char **argv;
{
    if (graphics) {
	initscr();
	start_color();
	verbose = 0;
	init_pair(1, COLOR_RED, COLOR_BLACK);
	init_pair(2, COLOR_GREEN, COLOR_BLACK);
	init_pair(3, COLOR_YELLOW, COLOR_BLACK);
	init_pair(4, COLOR_BLUE, COLOR_BLACK);
	init_pair(5, COLOR_MAGENTA, COLOR_BLACK);
	init_pair(6, COLOR_CYAN, COLOR_BLACK);
	init_pair(7, COLOR_WHITE, COLOR_BLACK);
	raw();
	noecho();
	nodelay(stdscr, TRUE);
	keypad(stdscr, TRUE);
	attrset(A_NORMAL);
    }
}
