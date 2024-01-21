/*================================================================
 * AWE MIDI player ncurses interface
 *================================================================*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdarg.h>

#include <ncurses/curses.h>

#include "util.h"
#include "channel.h"
#include "controls.h"
#include "midievent.h"

static void ctl_update(MidiInfo *mp);
static void ctl_help_mode(void);
static void ctl_total_time(int tt);
static void ctl_master_volume(int mv);
static void ctl_file_name(char *name);
static void ctl_current_time(int ct);
static void ctl_note(int ch, int note, int vel);
static void ctl_program(int ch, int val);
static void ctl_bank(int ch, int val);
static void ctl_volume(int channel, int val);
static void ctl_expression(int channel, int val);
static void ctl_panning(int channel, int val);
static void ctl_sustain(int channel, int val);
static void ctl_wheel(int channel, int val);
static void ctl_pitch_sense(int channel, int val);
static void ctl_change(int type, int ch, int value);
static void ctl_reset(MidiInfo *mp);
static void ctl_chorus(int mode);
static void ctl_reverb(int mode);
static int ctl_open(int using_stdin, int using_stdout);
static void ctl_close(void);
static int ctl_read(int *valp);
static int cmsg(int type, int verbosity_level, char *fmt, ...);
void dumb_pass_playing_list(int number_of_files, char *list_of_files[]);

char *inst_name(int val);
char *drum_name(int val);


/* export the interface functions */

#define ctl ncurses_control_mode

ControlMode ctl= 
{
  "ncurses interface", 'n',
  TRUE, /* need_args */
  TRUE, /* need_sync */
  TRUE, /* fancy_prev */
  0,0,0,0, /* verbose, trace, open, repeat */
  ctl_open, dumb_pass_playing_list, ctl_close, ctl_read, cmsg,
  ctl_update, ctl_reset, ctl_file_name, ctl_total_time, ctl_current_time, 
  ctl_note, ctl_program, ctl_bank, ctl_volume, 
  ctl_expression, ctl_panning, ctl_sustain, ctl_wheel, ctl_pitch_sense,
  ctl_chorus, ctl_reverb, ctl_change, ctl_master_volume,
};


/*----------------------------------------------------------------*/

static int ctl_helpmode=0;

static int msglen;
static WINDOW *dftwin = NULL, *msgwin = NULL, *trcwin = NULL, *helpwin = NULL;
static int bar_len;
static int multi_part = 1;
static int cur_chorus, cur_reverb;

#define FLAG_NOTE_OFF	1
#define FLAG_NOTE_ON	2

#define FLAG_BANK	1
#define FLAG_PROG	2
#define FLAG_PAN	4
#define FLAG_SUST	8
#define FLAG_WHEEL	16

static unsigned char cvel[MAX_MIDI_CHANNELS];
static unsigned char cnote[MAX_MIDI_CHANNELS];
static int ctotal[MAX_MIDI_CHANNELS];
static int clen[MAX_MIDI_CHANNELS];
static char v_flags[MAX_MIDI_CHANNELS];

static int cprog[MAX_MIDI_CHANNELS];
static int cpan[MAX_MIDI_CHANNELS];
static int cbend[MAX_MIDI_CHANNELS];
static int csust[MAX_MIDI_CHANNELS];
static int c_flags[MAX_MIDI_CHANNELS];


/*----------------------------------------------------------------*/

static void ctl_help_mode(void)
{
	if (msglen <= 0) return;
	if (ctl_helpmode) {
		ctl_helpmode = 0;
		delwin(helpwin);
		touchwin(msgwin);
		wmove(dftwin, 0, COLS-1);
		wrefresh(msgwin);
		helpwin = NULL;
	} else {
		helpwin = newwin(3, COLS, 2, 0);
		if (helpwin == NULL)
			return;
		ctl_helpmode = 1;
		werase(helpwin); 
		waddstr(helpwin, 
			"V/Up=Louder    b/Left=Skip back      "
			"n/Next=Next    r/Home=Restart");
		wmove(helpwin, 1, 0);
		waddstr(helpwin, 
			"v/Down=Softer  f/Right=Skip forward  "
			"p/Prev=Previous  q/End=Quit");
		wmove(helpwin, 2, 0);
		waddstr(helpwin, 
			"C=chorus mode  R=reverb mode");
		wmove(dftwin, 0, COLS-1);
		wrefresh(helpwin);
	}
}


static void ctl_total_time(int secs)
{
	secs /= 100;
	wmove(dftwin, 1, 40 + 8);
	wattron(dftwin, A_BOLD);
	wprintw(dftwin, "%02d:%02d", secs / 60, secs % 60);
	wattroff(dftwin, A_BOLD);
	wmove(dftwin, 0, COLS-1);
	wrefresh(dftwin);
}

static void ctl_master_volume(int mv)
{
	wmove(dftwin, 1, 56 + 8);
	wattron(dftwin, A_BOLD);
	wprintw(dftwin, "%03d %%", mv);
	wattroff(dftwin, A_BOLD);
	wmove(dftwin, 0, COLS-1);
	wrefresh(dftwin);
}

static void ctl_file_name(char *name)
{
	char local[32+1];
	sprintf(local, "%-32.32s", name);
	wmove(dftwin, 1, 9);
	wattron(dftwin, A_BOLD);
	waddstr(dftwin, local);
	wattroff(dftwin, A_BOLD);
	wmove(dftwin, 0, COLS-1);
	wrefresh(dftwin);
}

static void ctl_chorus(int mode)
{
	cur_chorus = mode;
	wmove(dftwin, 1, 73);
	wattron(dftwin, A_BOLD);
	wprintw(dftwin, "C%d/", mode);
	wattroff(dftwin, A_BOLD);
	wmove(dftwin, 0, COLS-1);
	wrefresh(dftwin);
}

static void ctl_reverb(int mode)
{
	cur_reverb = mode;
	wmove(dftwin, 1, 76);
	wattron(dftwin, A_BOLD);
	wprintw(dftwin, "R%d", mode);
	wattroff(dftwin, A_BOLD);
	wmove(dftwin, 0, COLS-1);
	wrefresh(dftwin);
}

/*----------------------------------------------------------------*/

static void ctl_current_time(int secs)
{
	static int lastsec = 0;
	secs /= 100;
	if (secs != lastsec) {
		wmove(dftwin, 4,6);
		wmove(dftwin, 1, 40);
		wattron(dftwin, A_BOLD);
		wprintw(dftwin, "%02d:%02d", secs / 60, secs % 60);
		wattroff(dftwin, A_BOLD);
		wmove(dftwin, 0, COLS-1);
		wrefresh(dftwin);
	}
}

/*----------------------------------------------------------------*/

#define DELTA_VEL	32

static void update_note(int ch)
{
	int i, xl, len, color, pat;
	xl = (ch >= 16) ? COLS/2 : 0;
	len = bar_len * ctotal[ch] / 127;
	if (clen[ch] < len) {
		wmove(trcwin, ch % 16 + 1, xl + 3+8+4 + clen[ch]);
		if (channels[ch].isdrum)
			color = 1;
		else
			color = 2 + ch%2;
		wattron(trcwin, A_REVERSE|COLOR_PAIR(color));
		if (channels[ch].isdrum)
			pat = ACS_CKBOARD;
		else
			/*pat = '_';*/
			pat = ACS_S9;
		for (i = clen[ch]; i < len; i++)
			waddch(trcwin, pat);
		wattroff(trcwin, A_REVERSE|A_COLOR);
	} else if (clen[ch] > len) {
		wmove(trcwin, ch % 16 + 1, xl + 3+8+4 + len);
		for (i = len; i < clen[ch]; i++)
			waddch(trcwin, ' ');
	}
	clen[ch] = len;
}

static void update_program(int ch)
{
	int xl;
	xl = (ch >= 16) ? COLS/2 : 0;
	wmove(trcwin, ch % 16 + 1, xl + 3);
	if (channels[ch].isdrum) {
		wattron(trcwin, A_BOLD);
		wprintw(trcwin, "%-7s", drum_name(cprog[ch]));
		wattroff(trcwin, A_BOLD);
	}
	else
		wprintw(trcwin, "%-7s", inst_name(cprog[ch]));
	wmove(dftwin, 0, COLS-1);
}

static void update_pan(int ch)
{
	int xl;
	xl = (ch >= 16) ? COLS/2 : 0;
	wmove(trcwin, ch % 16 + 1, xl + 3 + 8);
	waddch(trcwin, cpan[ch]);
}

static void update_sustain(int ch)
{
	int xl;
	xl = (ch >= 16) ? COLS/2 : 0;
	wmove(trcwin, ch % 16 + 1, xl + 3 + 8 + 1);
	waddch(trcwin, csust[ch]);
}


static void update_wheel(int ch)
{
	int xl;
	xl = (ch >= 16) ? COLS/2 : 0;
	wmove(trcwin, ch % 16 + 1, xl + 3 + 8 + 2);
	waddch(trcwin, cbend[ch]);
}

static void ctl_update(MidiInfo *mp)
{
	int i, do_refresh = 0;
	if (!(ctl.playing_mode & PLAY_TRACE))
		return;
	for (i = 0; i < MAX_MIDI_CHANNELS; i++) {
		if (v_flags[i]) {
			if (v_flags[i] == FLAG_NOTE_OFF) {
				ctotal[i] -= DELTA_VEL;
				if (ctotal[i] <= 0) {
					ctotal[i] = 0;
					v_flags[i] = 0;
				}
			} else
				v_flags[i] = 0;
			update_note(i);
			do_refresh = 1;
		}
		if (c_flags[i]) {
			if (c_flags[i] & FLAG_PAN)
				update_pan(i);
			if (c_flags[i] & FLAG_PROG)
				update_program(i);
			if (c_flags[i] & FLAG_SUST)
				update_sustain(i);
			if (c_flags[i] & FLAG_WHEEL)
				update_wheel(i);
			c_flags[i] = 0;
			do_refresh = 1;
		}
	}
	if (do_refresh) {
		wmove(dftwin, 0, COLS-1);
		wrefresh(trcwin);
		wrefresh(dftwin);
	}
}

/*----------------------------------------------------------------*/

static void ctl_note(int ch, int note, int vel)
{
	if (!(ctl.playing_mode & PLAY_TRACE))
		return;

	if (vel == 0) {
		if (note == cnote[ch]) {
			v_flags[ch] = FLAG_NOTE_OFF;
			cvel[ch] = 0;
		}
	} else if (vel >= cvel[ch]) {
		cvel[ch] = vel;
		cnote[ch] = note;
		ctotal[ch] = vel * channels[ch].volume *
			channels[ch].expression / (127*127);
		v_flags[ch] = FLAG_NOTE_ON;
	}
}

static void ctl_volume(int ch, int val)
{
	if (!(ctl.playing_mode & PLAY_TRACE)) 
		return;
	ctl_note(ch, cnote[ch], cvel[ch]);
}

static void ctl_expression(int ch, int val)
{
	if (!(ctl.playing_mode & PLAY_TRACE))
		return;
	ctl_note(ch, cnote[ch], cvel[ch]);
}

static void ctl_program(int ch, int val)
{
	if (!(ctl.playing_mode & PLAY_TRACE))
		return;
	if (cprog[ch] == val)
		return;
	cprog[ch] = val;
	c_flags[ch] |= FLAG_PROG;
}

static void ctl_panning(int ch, int val)
{
	int pat;
	if (!(ctl.playing_mode & PLAY_TRACE))
		return;

	if (val < 0)
		pat = ' ';
	else if (val < 5)
		pat = 'L';
	else if (val <= 60)
		pat = 'l';
	else if (val <= 67)
		pat = ' ';
	else if (val <= 122)
		pat = 'r';
	else
		pat = 'R';
	if (pat == cpan[ch]) return;

	cpan[ch] = pat;
	c_flags[ch] |= FLAG_PAN;
}

static void ctl_sustain(int ch, int val)
{
	int pat;
	if (!(ctl.playing_mode & PLAY_TRACE)) 
		return;
	if (val == 127)
		pat = 'S';
	else
		pat = ' ';
	if (pat == csust[ch])
		return;
	csust[ch] = pat;
	c_flags[ch] |= FLAG_SUST;
}

static void ctl_wheel(int ch, int val)
{
	int pat;
	if (!(ctl.playing_mode & PLAY_TRACE)) 
		return;
	if (val > 0x2000) pat = '+';
	else if (val < 0x2000) pat = '-';
	else pat = ' ';

	if (pat == cbend[ch])
		return;

	cbend[ch] = pat;
	c_flags[ch] |= FLAG_WHEEL;
}

static void ctl_bank(int ch, int val) {}
static void ctl_pitch_sense(int ch, int val) {}
static void ctl_change(int type, int ch, int value) {}

static void ctl_reset(MidiInfo *mp)
{
	int i, imax;
	if (!(ctl.playing_mode & PLAY_TRACE)) 
		return;

	/*
	if (mp->multi_part != multi_part) {
		multi_part = mp->multi_part;
	}
	*/
	wmove(trcwin, 0, 0);
	for (i = 0; i < COLS-1; i++)
		waddch(trcwin, ' ');
	wmove(trcwin, 0, 0);
	waddstr(trcwin, "Ch Program psb");
	imax = 16;
	if (multi_part) {
		wmove(trcwin, 0, COLS/2);
		waddstr(trcwin, "Ch Program psb");
		bar_len = COLS/2 - (3+8+5);
		imax = 32;
	} else
		bar_len = COLS - (3+8+5);
			
	for (i = 0; i < imax; i++) {
		wmove(trcwin, i % 16 + 1, (i / 16) * (COLS/2));
		wprintw(trcwin, "%02d", i);
		cvel[i] = 0;
		ctotal[i] = 0;
		clen[i] = bar_len + 1;
		update_note(i);
		v_flags[i] = 0;
		ctl_program(i, channels[i].preset);
		ctl_panning(i, channels[i].pan);
		ctl_sustain(i, channels[i].sustain);
		ctl_wheel(i, channels[i].pitchbend);
	}
	wmove(dftwin, 0, COLS-1);
	wrefresh(trcwin);
}

/***********************************************************************/

static int ctl_open(int using_stdin, int using_stdout)
{
	int i;
	SCREEN *dftscr, *oldscr;

	if (using_stdin) {
		fflush(stdout);
		setvbuf(stdout, 0, _IOFBF, BUFSIZ);
		dftscr = newterm(0, stdout, stdout);
	} else {
		dftscr = newterm(0, stdout, stdin);
	}
	if (! dftscr)
		return -1;
	oldscr = set_term(dftscr);
	/*initscr();*/
	raw();
	cbreak();
	noecho();
	nonl();
	nodelay(stdscr, 1);
	scrollok(stdscr, 0);
	idlok(stdscr, 1);
	keypad(stdscr, TRUE);
	start_color();
	ctl.opened = 1;

	dftwin = newwin(2, 0, 0, 0);
	if (dftwin == NULL || LINES < 4) {
		fprintf(stderr, "Too small windo size!!\n");
		exit(1);
	}
	scrollok(dftwin, 0);
	werase(dftwin);

	init_pair(1, COLOR_RED, COLOR_BLACK);
	init_pair(2, COLOR_GREEN, COLOR_BLACK);
	init_pair(3, COLOR_BLUE, COLOR_BLACK);

	wmove(dftwin, 0, 0);
	waddstr(dftwin, "AWE MIDI Player / Copyright (c) 1996,97 Takashi Iwai");
	wmove(dftwin, 0, COLS-20);
	waddstr(dftwin, "Press [h] for help");
	wmove(dftwin, 1, 0);
	waddstr(dftwin, "Playing:");
	wmove(dftwin, 1, 40);
	waddstr(dftwin, "00:00 / ");
	wmove(dftwin, 1, 48);
	wattron(dftwin, A_BOLD);
	waddstr(dftwin, "00:00");
	wattroff(dftwin, A_BOLD);
	wmove(dftwin, 1, 56);
	waddstr(dftwin, "Volume:");
	wrefresh(dftwin);

	msglen = 0;
	if ((ctl.playing_mode & PLAY_TRACE) && LINES >= 17 + 3) {
		msglen = LINES - 17 - 2;
		msgwin = newwin(msglen, COLS, 2, 0);
		werase(msgwin);
		scrollok(msgwin, 1);
		trcwin = newwin(17, COLS, 2 + msglen, 0);
		scrollok(trcwin, 0);
		wrefresh(msgwin);
		wrefresh(trcwin);
		for (i = 0; i < MAX_MIDI_CHANNELS; i++) {
			cprog[i] = -1;
		}
	} else {
		ctl.playing_mode &= ~PLAY_TRACE;
		msglen = LINES - 2;
		msgwin = newwin(msglen, COLS, 2, 0);
		werase(msgwin);
		scrollok(msgwin, 1);
	}

	return 0;
}

static void ctl_close(void)
{
	if (ctl.opened) {
		endwin();
		ctl.opened=0;
	}
}

static int ctl_read(int *valp)
{
	int c;
	while ((c = getch()) != ERR) {
		switch(c) {
		case 'h':
		case '?':
		case KEY_F(1):
			ctl_help_mode();
			return RC_NONE;
	  
		case 'V':
		case KEY_UP:
			*valp = glinfo.master_volume + 10;
			if (*valp <= 200)
				return RC_CHANGE_VOLUME;
			else
				return RC_NONE;
		case 'v':
		case KEY_DOWN:
			*valp = glinfo.master_volume - 10;
			if (*valp >= 0)
				return RC_CHANGE_VOLUME;
			else
				return RC_NONE;
		case 'q':
		case KEY_END:
			return RC_QUIT;
		case 'n':
		case KEY_NPAGE:
			return RC_NEXT;
		case 'p':
		case KEY_PPAGE:
			return RC_REALLY_PREVIOUS;
		case 'r':
		case KEY_HOME:
			return RC_RESTART;

		case 'f':
		case KEY_RIGHT:
			*valp=1;
			return RC_FORWARD;
		case 'b':
		case KEY_LEFT:
			*valp=1;
			return RC_BACK;
		case 'C':
			*valp = (cur_chorus + 1) % 8;
			ctl_chorus(*valp);
			return RC_CHANGE_CHORUS;
		case 'R':
			*valp = (cur_reverb + 1) % 8;
			ctl_reverb(*valp);
			return RC_CHANGE_REVERB;
		case ' ':
			return *valp ? RC_CONTINUE : RC_PAUSE;
		}
	}
	return RC_NONE;
}

static int cmsg(int type, int verbosity_level, char *fmt, ...)
{
	va_list ap;
	if ((type==CMSG_TEXT || type==CMSG_INFO || type==CMSG_WARNING) &&
	    ctl.verbosity<verbosity_level)
		return 0;
	va_start(ap, fmt);
	if (!ctl.opened || msgwin == NULL) {
		vfprintf(stderr, fmt, ap);
		fprintf(stderr, "\n");
	} else {
		vwprintw(msgwin, fmt, ap);
		wprintw(msgwin, "\n");
		wrefresh(msgwin);
		wmove(dftwin, 0, 0);
		if (type == CMSG_ERROR)
			sleep(2);
	}

	va_end(ap);
	return 0;
}
