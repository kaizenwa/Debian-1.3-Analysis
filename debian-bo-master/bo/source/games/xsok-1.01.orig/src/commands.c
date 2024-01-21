/*****************************************************************************/
/*									     */
/*									     */
/*	Xsok version 1.00 -- module commands.c				     */
/*									     */
/*	Most of the entries for commands assignable to keys		     */
/*	Written by Michael Bischoff (mbi@mo.math.nat.tu-bs.de)		     */
/*	November-1994							     */
/*	see COPYRIGHT.xsok for Copyright details			     */
/*									     */
/*									     */
/*****************************************************************************/
#ifndef _POSIX_SOURCE
#define _POSIX_SOURCE
#endif
#include <sys/types.h>
#include <unistd.h>
#include "xsok.h"
#include "version.h"

void cmd_Up(void) {
    playermove(0);
}
void cmd_Left(void) {
    playermove(1);
}
void cmd_Down(void) {
    playermove(2);
}
void cmd_Right(void) {
    playermove(3);
}

void cmd_Repeat(void) {
    if (game.n_moves)
	playermove(movetab[game.n_moves-1]);
}

void cmd_ShowAuthor(void) {
    if (*levelauthor)
	show_message("%s", levelauthor);
    else
	show_message("%s", TXT_NOAUTHOR);
}

void cmd_LevelInfo(void) {
    if (*levelcomment) {
	if (highscore[game.level])
	    show_message("%s (Best Score: %d)", levelcomment, highscore[game.level]);
	else
	    show_message("%s (Unsolved level)", levelcomment);
    } else
	cmd_ShowBestScore();
}

void cmd_NextUnsolved(void) {
    while (game.level < maxlevel) {
	if (!highscore[++game.level]) {
	    NewLevel(game.level);
	    cmd_LevelInfo();
	    return;
	}
    }
}

void cmd_NextLevel(void) {
    if (numeric_arg)
	if (numeric_arg <= maxlevel)
	    game.level = numeric_arg - 1;	/* & fall through */
    if (game.level < maxlevel) {
	if (game.stored_moves && !game.finished) {
	    play_sound("giveup");
	}
	NewLevel(game.level+1);
	cmd_LevelInfo();
    }
}

void cmd_PrevLevel(void) {
    if (game.level > 1) {
	if (game.stored_moves && !game.finished) {
	    play_sound("giveup");
	}
	NewLevel(game.level-1);
	cmd_LevelInfo();
    }
}

void rq_LeaveSok(void) {
    request_confirm(cmd_LeaveSok, TXT_QUIT_CONFIRM);
}

/* unused, since this can be undone */
void rq_RestartGame(void) {
    request_confirm(cmd_RestartGame, TXT_RESTART_CONFIRM);
}

void rq_PrevLevel(void) {
    request_confirm(cmd_PrevLevel, TXT_PREV_CONFIRM);
}
void rq_NextLevel(void) {
    request_confirm(cmd_NextLevel, TXT_NEXT_CONFIRM);
}
void rq_NextUnsolved(void) {
    request_confirm(cmd_NextUnsolved, TXT_UNSOLVED_CONFIRM);
}


void cmd_DropBookmark(void) {
    game.bookmark = game.n_moves;	/* easy, isn't it? */
    show_message(TXT_BOOKMARK_SET);
}

void jumpto_movenr(int move_ptr) {
    int remgraphic = gamegraphic;
    if (move_ptr == game.n_moves)
	return;
    /* assert(move_ptr <= game.stored_moves); */
    if (remgraphic)		/* graphic was on */
	graphics_control(Disable);

    if (move_ptr < game.n_moves)	/* must reset first */
	OrgLevel();
    while (move_ptr > game.n_moves) {
	/* printf("doing move %d of %d (%d)\n", game.n_moves, move_ptr, movetab[game.n_moves]); */
	int xx;
	xx = game.n_moves;
	playermove(movetab[game.n_moves]);
	if (xx == game.n_moves)
	    fatal("Shit, same old bug again!\n");
    }
    if (remgraphic)
	graphics_control(EnableAndRedraw);
}

void cmd_RestartGame(void) {
    if (numeric_arg <= game.stored_moves)
	jumpto_movenr(numeric_arg);
    else 
	jumpto_movenr(game.stored_moves);
    cmd_LevelInfo();
}

void cmd_GotoBookmark(void) {
    jumpto_movenr(game.bookmark);
    cmd_ShowScore();
}

void cmd_SaveGame(void) {
    game.finished = compute_score();
    save_game("sv");
}
void cmd_ShowVersion(void) {
    show_message(TXT_VERSION, VERSION);
}


void cmd_ShowScore(void) {
    compute_score();
    show_message(TXT_SCORE, game.n_moves, game.n_pushes, game.score,
		 obj[game.y][game.x]->power - obj[game.y][game.x]->weight);
}
void cmd_ShowBestScore(void) {
    if (highscore[game.level])
	show_message(TXT_BEST, highscore[game.level+100],
		     highscore[game.level+200], highscore[game.level]);
    else
	show_message(TXT_UNSOLVED);
}
void cmd_ReplayGame(void) {
    if (game.n_moves) {
	int rem = game.n_moves;
	cmd_RestartGame();
	sync_and_wait();
	sync_and_wait();
	do {
	    cmd_RedoMove();
	    sync_and_wait();
	    sync_and_wait();
	} while (game.n_moves < rem);
	cmd_ShowScore();
    }
}

void cmd_LoadGame(void) {
    char filename[MAXSAVEFILELEN];
    static const char **p, *extensions[] = { "sv", "bs", "mp", "mm", "sav", "sol", NULL };
    for (p = extensions; *p; ++p) {
	sprintf(filename, "%s/%s.%02d.%s", savedir, game.type, game.level, *p);
	if (!access(filename, R_OK)) {
	    load_game(filename);
	    return;
	}
    }
    show_message(TXT_NOLOAD);
}
void cmd_StartMacro(void) {
    game.macroStart = game.n_moves;
    game.macroEnd = game.n_moves;
    game.macro_x = game.x;
    game.macro_y = game.y;
    show_message(TXT_STARTMACRO);
}
void cmd_EndMacro(void) {
    if (game.macroStart >= 0) {
	game.macroEnd = game.n_moves;
	show_message(TXT_ENDMACRO, game.n_moves-game.macroStart);
    }
}
void cmd_PlayMacro(void) {
    /* show_message("Stard %d, End %d, at (%d,%d)", game.macroStart, game.macroEnd,
       game.macro_x, game.macro_y); */
    if (game.macroStart >= 0) {
	mouse_x = game.macro_x;
	mouse_y = game.macro_y;
	cmd_MouseMove();	/* sets the before_move variable */
	if (game.x == game.macro_x && game.y == game.macro_y) {
	    int i, base;
	    base = game.n_moves;
	    for (i = game.macroStart; i < game.macroEnd;) {
		playermove(movetab[i++]);
		if (game.n_moves != base + i - game.macroStart)
		    break;	/* invalid move */
	    }
	} else
	    show_message(TXT_MACRO_BADPOS);
    }
}

void cmd_UndoMove(void) {
    if (game.n_moves) {
	int num = numeric_arg;
	if (!num)
	    num = 1;
	if (game.n_moves >= num)
	    jumpto_movenr(game.n_moves-num);
	else
	    jumpto_movenr(0);
	show_message(TXT_UNDO);
	if (game.n_moves < game.macroStart || game.n_moves < game.macroEnd)
	    game.macroStart = -1;
    } else if (game.stored_moves) {
	jumpto_movenr(game.stored_moves);
	show_message(TXT_UNDO);
    } else
	show_message(TXT_NOUNDO);
}
void cmd_RedoMove(void) {
    if (game.n_moves < game.stored_moves) {
	int num = numeric_arg;
	if (!num)
	    num = 1;
	if (game.n_moves + num <= game.stored_moves)
	    jumpto_movenr(game.n_moves+num);
	else
	    jumpto_movenr(game.stored_moves);
	show_message(TXT_REDO);
    } else
	show_message(TXT_NOREDO);
}
