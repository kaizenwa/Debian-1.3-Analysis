/*****************************************************************************/
/*									     */
/*									     */
/*	Xsok version 1.00 -- module loadsave.c				     */
/*									     */
/*	Functions for highscores and loading/saving games.		     */
/*	Written by Michael Bischoff (mbi@mo.math.nat.tu-bs.de)		     */
/*	November-1994							     */
/*	see COPYRIGHT.xsok for Copyright details			     */
/*									     */
/*									     */
/*****************************************************************************/
#ifndef _POSIX_SOURCE
#define _POSIX_SOURCE
#endif
#include <limits.h>
#include <unistd.h>
#include <time.h>
#include "xsok.h"
#include "version.h"

/* code to switch between real and effective user id */
/* we must use the real user id when saving files */

void switch_uid(int to_real) {
#ifdef _POSIX_SAVED_IDS
    static int uid_state = -1; /* -1 = unknown, 1 = real, 0 = effective */
    static uid_t real_uid, effective_uid;
    if (uid_state < 0) {
	real_uid = getuid();
	effective_uid = geteuid();
	uid_state = 0;
    }
    if (to_real != uid_state && real_uid != effective_uid) {
	setuid(to_real ? real_uid : effective_uid);
	uid_state = to_real;
    }
#endif
}

void setlangdir(void) {
    const char *s;
    char p[100];
    if ((s = getenv("LANG"))) {
	sprintf(p, "%s/%s", xsokdir, s);
	if (!access(p, F_OK)) {		/* langdir does exist */
	    langdir = s;
	    return;
	}
    }
    langdir = "";
}

const char *savedir = NULL;

#define NARGS	16	/* score, pushes, moves, time */
#define BUFSIZE	256	/* at least length of longest shortname + 1 */

#define MAGIC1	0x741b	/* magic of xsok version 1 */
#define MAGICS	0x741c	/* magic of xsok version 1 simple file */
#define MAGICH	0x741d	/* magic of xsok version 1 highscore file */


static void read_err(const char *msg) {
    if (gamegraphic) {
	show_message("%s %s", TXT_LOAD_ERR_BASIC, msg);
	cmd_LeaveSok();				/* make it better! */
    }
    fprintf(stderr, "%s %s\n", TXT_LOAD_ERR_BASIC, msg);
    exit(EXIT_FAILURE);
}

static void portable_to_internal(long *args, unsigned char *p, int num) {
    do {
	int j;
	*args = 0;
	for (j = 0; j < 4; ++j)
	    *args += (long)p[3-j] << (j << 3);
	++args;
	p += 4;
    } while (--num);
}

static void internal_to_portable(unsigned char *p, long *args, int num) {
    do {
	int j;
	memset(p, (*args < 0 ? -1 : 0), 4);
	for (j = 0; j < 4; ++j)
	    p[3-j] = (unsigned char)(*args >> (j << 3));
	++args;
	p += 4;
    } while (--num);
}

int highscore[300];

void cmd_ReadHighscores(void) {
    FILE *fp;
    char filename[MAXSAVEFILELEN];
    int i;

    memset(highscore, 0, sizeof(highscore));
    for (i = 100; i < 300; ++i)
	    highscore[i] = 0x7fffffff;

    sprintf(filename, "%s/%s.score", savedir, game.type);
    if ((fp = fopen(filename, "rb"))) {
	long p[300];
	unsigned char s[1200];
	fread(s, 4, 300, fp);
	portable_to_internal(p, s, 300);	
	for (i = 0; i < 300; ++i)
	    highscore[i] = p[i];
	fclose(fp);
    }
    highscore[0] = MAGICH;
}

void WriteHighscores(void) {
    FILE *fp;
    char filename[MAXSAVEFILELEN];
    sprintf(filename, "%s/%s.score", savedir, game.type);
    if ((fp = fopen(filename, "wb"))) {
	int i;
	long p[300];
	unsigned char s[1200];
	for (i = 0; i < 300; ++i)
	    p[i] = highscore[i];
	internal_to_portable(s, p, 300);
	fwrite(s, 4, 300, fp);
	fclose(fp);
    }
}

void load_game(const char *filename) {
    FILE *fp;
    long args[NARGS];
    int i, remgraphic = gamegraphic;
    unsigned char p[NARGS * 4];

    if (gamegraphic)	/* interactive loading */
	graphics_control(Disable);
    OrgLevel();
    if (!(fp = fopen(filename, "rb")))
	read_err(TXT_LOAD_ERR_OPEN);

    if (fread(p, 4, 1, fp) != 1)
	read_err(TXT_LOAD_ERR_HEADER);
    portable_to_internal(args, p, 1);
    if (args[0] == MAGICS) {
	fseek(fp, 0L, SEEK_END);
	game.stored_moves = ftell(fp) - 4;
	game.n_moves = game.bookmark = game.stored_moves;
	game.macroStart = -1;
	fseek(fp, 4L, SEEK_SET);
	goto readmoves;
    }

    if (args[0] != MAGIC1)
	read_err(TXT_LOAD_ERR_BADMAGIC);
    if (fread(p, 4, NARGS-1, fp) != NARGS-1)
	read_err(TXT_LOAD_ERR_HEADER);
    portable_to_internal(args+1, p, NARGS-1);

    game.n_moves = args[3];
    if (args[7] == -1) {	/* loading a game from the alpha version */
	fseek(fp, 32, SEEK_SET);
	game.stored_moves = args[3];
	game.bookmark = game.n_moves;
	game.macroStart = -1;
    } else {
	game.stored_moves = args[7];
	game.macroStart = args[8];
	game.macroEnd = args[9];
	game.macro_x = args[10];
	game.macro_y = args[11];
	game.bookmark = args[12];
    }
    /* skip game type and level */
    fseek(fp, 8L, SEEK_CUR);

    /* skip user name */
    i = getc(fp);
    fseek(fp, (long)i, SEEK_CUR);

 readmoves:
    if (game.stored_moves > numalloc) {
	numalloc = 256 + (game.stored_moves & ~255);
	movetab = realloc_(movetab, numalloc);
    }
    if (fread(movetab, 1, game.stored_moves, fp) != game.stored_moves)
	read_err(TXT_LOAD_ERR_MOVES);
    fclose(fp);

    {   int rem;
	rem = game.n_moves;
	game.n_moves = 0;
	jumpto_movenr(rem);
    }

    if (remgraphic)
	graphics_control(EnableAndRedraw);
    cmd_ShowScore();
}


void save_game(const char *ext) {
    FILE *fp;
    char filename[MAXSAVEFILELEN];
    long args[NARGS];
    int i;
    unsigned char p[4 * NARGS];

    compute_score();
    sprintf(filename, "%s/%s.%02d.%s", savedir, game.type, game.level, ext);
    remove(filename);	/* kill any old one first! */
    if (!(fp = fopen(filename, "wb"))) {
	show_message("%s %s", TXT_SAVE_ERR_BASIC, TXT_SAVE_ERR_OPEN);
	goto werr2;
    }
    args[0] = MAGIC1;
    args[1] = game.score;
    args[2] = game.n_pushes;
    args[3] = game.n_moves;
    args[4] = time(NULL);
    args[5] = game.finished;
    args[6] = game.level;
    args[7] = game.stored_moves;
    args[8] = game.macroStart;
    args[9] = game.macroEnd;
    args[10] = game.macro_x;
    args[11] = game.macro_y;
    args[12] = game.bookmark;
    args[13] = args[14] = args[15] = -1;

    internal_to_portable(p, args, NARGS);
    if (fwrite(p, 4, NARGS, fp) != NARGS) {
    werr3:
	show_message("%s %s", TXT_SAVE_ERR_BASIC, TXT_SAVE_ERR_HEADER);
	goto werr;
    }
    strncpy(filename, game.type, 8);
    if (fwrite(filename, 1, 8, fp) != 8)
	goto werr3;

    i = strlen(username);
    putc(i, fp);
    if (fwrite(username, 1, i, fp) != i)
	goto werr3;
    if (fwrite(movetab, 1, game.stored_moves, fp) != game.stored_moves)
	goto werr3;

    fclose(fp);
    show_message(TXT_SAVE_OK);
    play_sound("ok");	/* found no sound file for this. maybe later */
    return;

werr:
    fclose(fp);
werr2:
    play_sound("cannotsave");
}

void link_game(const char *old, const char *ext) {
    char file1[MAXSAVEFILELEN], file2[MAXSAVEFILELEN];

    sprintf(file1, "%s/%s.%02d.%s", savedir, game.type, game.level, old);
    sprintf(file2, "%s/%s.%02d.%s", savedir, game.type, game.level, ext);
    remove(file2);	/* kill any old one first! */
    if (link(file1, file2))
	show_message(TXT_SAVE_ERR_LINK);
}
