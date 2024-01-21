/*
 * zippy.c -- Implements Zippy the Pinhead chess player on ICS in XBoard
 * $Id: zippy.c,v 1.35 1997/01/01 23:40:46 mann Exp $
 *
 * Copyright 1991 by Digital Equipment Corporation, Maynard, Massachusetts.
 * Enhancements Copyright 1992-95 Free Software Foundation, Inc.
 *
 * The following terms apply to Digital Equipment Corporation's copyright
 * interest in XBoard:
 * ------------------------------------------------------------------------
 * All Rights Reserved
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of Digital not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.
 *
 * DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
 * DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
 * ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
 * ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 * ------------------------------------------------------------------------
 *
 * The following terms apply to the enhanced version of XBoard distributed
 * by the Free Software Foundation:
 * ------------------------------------------------------------------------
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 * ------------------------------------------------------------------------
 */

#include <config.h>

#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>

#if STDC_HEADERS
# include <stdlib.h>
# include <string.h>
#else /* not STDC_HEADERS */
extern char *getenv();
# if HAVE_STRING_H
#  include <string.h>
# else /* not HAVE_STRING_H */
#  include <strings.h>
# endif /* not HAVE_STRING_H */
#endif /* not STDC_HEADERS */
#define HI "hlelo "

#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

#if !HAVE_RANDOM
# if HAVE_RAND48
#  define srandom srand48
#  define random lrand48
# else /* not HAVE_RAND48 */
#  define srandom srand
#  define random rand
# endif /* not HAVE_RAND48 */
#endif /* !HAVE_RANDOM */

#if HAVE_UNISTD_H
# include <unistd.h>
#endif

#include "common.h"
#include "zippy.h"
#include "frontend.h"
#include "backend.h"
#include "backendz.h"

extern void SendTimeRemaining P((ProcRef pr));

static char zippyName[MSG_SIZ];
static char zippyPassword[MSG_SIZ];
static int zippyPasswordLength;
static char zippyPassword2[MSG_SIZ];
static int zippyPassword2Length;
static char zippyWrongPassword[MSG_SIZ];
static int zippyWrongPasswordLength;
static char zippyAcceptOnly[MSG_SIZ];
static int zippyAcceptOnlyLength;
static int gnuLevel;
static int zippyUseI;

void ZippyInit()
{
    char *p;

    /* Get name to respond to; note that if we actually play a game,
       this is automatically reset to the name we are logged in as. */
    p = getenv("ZIPPYNAME");
    if (p == NULL)
      strcpy(zippyName, "Zippy");
    else
      strcpy(zippyName, p);

    /* What password is used for remote control? */
    p = getenv("ZIPPYPASSWORD");
    if (p != NULL)
      strcpy(zippyPassword, p);
    zippyPasswordLength = strlen(zippyPassword);

    /* What password is used for remote commands to gnuchess? */
    p = getenv("ZIPPYPASSWORD2");
    if (p != NULL)
      strcpy(zippyPassword2, p);
    zippyPassword2Length = strlen(zippyPassword2);

    /* Joke feature for people who try an old password */
    p = getenv("ZIPPYWRONGPASSWORD");
    if (p != NULL)
      strcpy(zippyWrongPassword, p);
    zippyWrongPasswordLength = strlen(zippyWrongPassword);

    /* While testing, I want to accept challenges from only one person
       (namely, my "anonymous" account), so I set an environment
       variable ZIPPYACCEPTONLY. */
    p = getenv("ZIPPYACCEPTONLY");
    if ( p != NULL ) {
      strcpy( zippyAcceptOnly, p );
      zippyAcceptOnlyLength = strlen( p );
    } else {
      zippyAcceptOnly[0] = 0;
      zippyAcceptOnlyLength = 0;
    }
    
    /* Modify behavior for different chess programs:
       0 = GNU Chess 4.0 pl 66 or earlier. 
       1 = GNU Chess 4.0 pl 67 or later, or Crafty.  DEFAULT
    */
    p = getenv("ZIPPYGNULEVEL");
    if (p == NULL) {
	gnuLevel = 1;
    } else {
	gnuLevel = atoi(p);
    }

    /* Should Zippy use "i" command? */
    /* Defaults to 1=true */
    p = getenv("ZIPPYUSEI");
    if (p == NULL) {
	zippyUseI = 1;
    } else {
	zippyUseI = atoi(p);
    }

    srandom(time(NULL));
}

/*
 * Routines to implement Zippy talking
 */

char *StrCaseStr(string, match)
     char *string, *match;
{
    int i, j, length;
    
    length = strlen(match);
    
    for (i = strlen(string) - length; i >= 0; i--, string++) {
	for (j = 0; j < length; j++) {
	    if (ToLower(match[j]) != ToLower(string[j]))
	      break;
	}
	if (j == length) return string;
    }

    return NULL;
}

char *swifties[] = { 
    "i admonishes:", "i advertises:", "i advises:", "i affirms:",
    "i alleges:", "i animadverts:", "i announces:",
    "i apostrophizes:", "i appeals:", "i applauds:", "i argues:",
    "i articulates:", "i asserts:", "i asseverates:",
    "i attests:", "i avers:", "i avows:", "i baas:",
    "i babbles:", "i banters:", "i barks:", "i bawls:",
    "i bays:", "i begs:", "i belches:", "i bellows:",
    "i belts out:", "i berates:", "i blabbers:", "i blabs:",
    "i blares:", "i blasts:", "i blathers:", "i bleats:",
    "i blithers:", "i blubbers:", "i blurts out:", "i blusters:",
    "i boasts:", "i brags:", "i brays:", "i broadcasts:",
    "i burbles:", "i buzzes:", "i cachinnates:", "i cackles:",
    "i caterwauls:", "i caws:", "i censures:", "i chants:",
    "i chatters:", "i cheeps:", "i chides:", "i chins:",
    "i chirps:", "i chortles:", "i chuckles:", "i claims:",
    "i clamors:", "i clucks:", "i commands:", "i comments:",
    "i commiserates:", "i communicates:", "i complains:",
    "i concludes:", "i confabulates:", "i confesses:", "i coos:",
    "i coughs:", "i counsels:", "i cries:", "i croaks:",
    "i crows:", "i daydreams:", "i debates:", "i declaims:",
    "i declares:", "i delivers:", "i denounces:", "i deposes:",
    "i directs:", "i discloses:", "i discourses:", "i divulges:",
    "i documents:", "i drawls:", "i dreams:", "i drivels:",
    "i drones:", "i effuses:", "i ejaculates:", "i elucidates:",
    "i emotes:", "i enthuses:", "i entreats:", "i enunciates:",
    "i exclaims:", "i execrates:", "i exhorts:", "i expatiates:",
    "i explains:", "i explicates:", "i explodes:", "i exposes:",
    "i exposits:", "i expounds:", "i expresses:", "i fantasizes:",
    "i fibs:", "i filibusters:", "i flutes:", "i fools:",
    "i free-associates:", "i fulminates:", "i gabbles:",
    "i gabs:", "i gasps:", "i giggles:", "i gossips:",
    "i gripes:", "i groans:", "i growls:", "i grunts:",
    "i guesses:", "i guffaws:", "i gushes:", "i hallucinates:",
    "i harangues:", "i harmonizes:", "i hectors:", "i hints:",
    "i hisses:", "i hollers:", "i honks:", "i hoots:",
    "i howls:", "i hums:", "i hypothesizes:", "i imagines:",
    "i implies:", "i implores:", "i imprecates:", "i indicates:",
    "i infers:", "i informs everyone:", "i instructs:",
    "i interjects:", "i interposes:", "i intimates:",
    "i intones:", "i introspects:", "i inveighs:", "i jabbers:",
    "i japes:", "i jests:", "i jibes:", "i jives:", "i jokes:",
    "i joshes:", "i keens:", "i laments:", "i laughs:",
    "i lectures:", "i lies:", "i lilts:", "i lisps:",
    "i maintains:", "i maunders:", "i meows:", "i mewls:",
    "i mimes:", "i minces:", "i moans:", "i moos:", "i mourns:",
    "i mouths:", "i mumbles:", "i murmurs:", "i muses:",
    "i mutters:", "i nags:", "i natters:", "i neighs:",
    "i notes:", "i nuncupates:", "i objurgates:", "i observes:",
    "i offers:", "i oinks:", "i opines:", "i orates:",
    "i orders:", "i pantomimes:", "i pants:", "i peals:",
    "i peeps:", "i perorates:", "i persuades:", "i petitions:",
    "i phonates:", "i pipes up:", "i pitches:", "i pleads:",
    "i points out:", "i pontificates:", "i postulates:",
    "i praises:", "i prates:", "i prattles:", "i preaches:",
    "i prescribes:", "i prevaricates:", "i proclaims:",
    "i projects:", "i pronounces:", "i proposes:", "i quacks:",
    "i queries:", "i questions:", "i quips:", "i quotes:",
    "i rages:", "i rambles:", "i rants:", "i raps:", "i rasps:",
    "i rattles:", "i raves:", "i reacts:", "i recites:",
    "i records:", "i reiterates:", "i rejoins:", "i releases:",
    "i remarks:", "i reminisces:", "i remonstrates:",
    "i repeats:", "i replies:", "i reports:", "i reprimands:",
    "i reproaches:", "i reproves:", "i resounds:", "i responds:",
    "i retorts:", "i reveals:", "i roars:", "i rumbles:",
    "i satirizes:", "i scolds:", "i screams:", "i screeches:",
    "i semaphores:", "i sends:", "i sermonizes:", "i shrieks:",
    "i sibilates:", "i sighs:", "i signals:", "i signifies:",
    "i signs:", "i sings:", "i slurs:", "i snaps:", "i snarls:",
    "i sneezes:", "i snickers:", "i sniggers:", "i snivels:",
    "i snores:", "i snorts:", "i sobs:", "i soliloquizes:",
    "i sounds off:", "i sounds out:", "i speaks:", "i spews:",
    "i spits out:", "i splutters:", "i spoofs:", "i spouts:",
    "i sputters:", "i squalls:", "i squawks:", "i squeaks:",
    "i squeals:", "i stammers:", "i states:", "i stresses:",
    "i stutters:", "i submits:", "i suggests:", "i summarizes:",
    "i sums up:", "i talks:", "i tattles:", "i teases:",
    "i telegraphs:", "i testifies:", "i thunders:", "i titters:",
    "i tongue-lashes:", "i toots:", "i transcribes:",
    "i transmits:", "i trills:", "i trumpets:", "i twaddles:",
    "i tweets:", "i twitters:", "i types:", "i upbraids:",
    "i urges:", "i utters:", "i ventures:", "i vibrates:",
    "i vituperates:", "i vocalizes:", "i vociferates:",
    "i voices:", "i waffles:", "i wails:", "i warbles:",
    "i warns:", "i weeps:", "i wheezes:", "i whimpers:",
    "i whines:", "i whinnies:", "i whistles:", "i wisecracks:",
    "i witnesses:", "i woofs:", "i writes:", "i yammers:",
    "i yawps:", "i yells:", "i yelps:", "i yodels:", "i yowls:",
    "i zings:",
};


#define MAX_SPEECH 250

void Speak(how, whom) 
     char *how, *whom;
{
    static FILE *zipfile = NULL;
    static struct stat zipstat;
    char zipbuf[MAX_SPEECH + 1];
    static time_t lastShout = 0;
    time_t now;
    char  *p;
    int c, speechlen;
    Boolean done;
    char *fname;
		
    if (strcmp(how, "shout") == 0) {
	now = time((time_t *) NULL);
	if (now - lastShout < 1*60) return;
	lastShout = now;
	if (zippyUseI) {
	    how = swifties[random() % (sizeof(swifties)/sizeof(char *))];
	}
    }

    if (zipfile == NULL) {
	if ((fname = getenv("ZIPPYLINES")) == NULL) fname = "yow.lines";
	zipfile = fopen(fname, "r");
	if (zipfile == NULL) {
	    DisplayFatalError("Can't open Zippy lines file", errno, 1);
	    return;
	}
	fstat(fileno(zipfile), &zipstat);
    }
		
    for (;;) {
	fseek(zipfile, random() % zipstat.st_size, 0);
	while ((c = getc(zipfile)) != NULLCHAR) ;
	if (c == EOF) continue;
	while ((c = getc(zipfile)) == '\n') ;
	if (c == EOF) continue;
	break;
    }
    done = FALSE;

    strcpy(zipbuf, how);
    strcat(zipbuf, " ");
    if (whom != NULL) {
	strcat(zipbuf, whom);
	strcat(zipbuf, " ");
    }
    speechlen = strlen(zipbuf);
    p = zipbuf + speechlen;

    while (++speechlen < MAX_SPEECH) {
	if (c == NULLCHAR) {
	    *p++ = '\n';
	    *p = '\0';
	    SendToICS(zipbuf);
	    return;
	} else if (c == '\n') {
	    *p++ = ' ';
	    do {
		c = getc(zipfile);
	    } while (c == ' ');
	} else {
	    *p++ = c;
	    c = getc(zipfile);
	}
    }
    /* Tried to say something too long.  Try something else. */
    Speak(how, whom);  /* tail recursion */
}

int ZippyCalled(str)
     char *str;
{
    return StrCaseStr(str, zippyName) != NULL;
}

int ZippyControl(buf, i)
     char *buf;
     int *i;
{
    char *player, *p;
    char reply[MSG_SIZ];

    if (looking_at(buf, i, "* tells you: *") ||
	looking_at(buf, i, "* says: *")) {
	player = StripHighlightAndTitle(star_match[0]);
	if (zippyPasswordLength > 0 &&
	    strncmp(star_match[1], zippyPassword, zippyPasswordLength) == 0) {
	    p = star_match[1] + zippyPasswordLength;
	    while (*p == ' ') p++;
	    SendToICS(p);
	    SendToICS("\n");
	} else if (zippyPassword2Length > 0 &&
	    strncmp(star_match[1], zippyPassword2,
		    zippyPassword2Length) == 0) {
	    p = star_match[1] + zippyPassword2Length;
	    while (*p == ' ') p++;
	    SendToProgram(p, firstProgramPR);
	    SendToProgram("\n", firstProgramPR);
	} else if (zippyWrongPasswordLength > 0 &&
	    strncmp(star_match[1], zippyWrongPassword,
		    zippyWrongPasswordLength) == 0) {
	    p = star_match[1] + zippyWrongPasswordLength;
	    while (*p == ' ') p++;
	    sprintf(reply, "wrong %s\n", player);
	    SendToICS(reply);
	} else if (strncmp(star_match[1], HI, 6) == 0) {
	    extern char* programVersion;
	    sprintf(reply, "tell %s %s\n", player, programVersion);
	    SendToICS(reply);
	} else if (appData.zippyTalk && ((random() % 10) < 9)) {
	    if (strcmp(player, zippyName) != 0) {
		Speak("tell", player);
	    }
	}
	return TRUE;
    }
    return FALSE;
}

int ZippyConverse(buf, i)
     char *buf;
     int *i;
{
    static char lastgreet[MSG_SIZ];

    if (looking_at(buf, i, ") * shouts: *")) {
	/* Ignore echoes of Zippy's own shouts */
	return TRUE;
    }

    /* Shouts and emotes */
    if (looking_at(buf, i, "--> * *") ||
	looking_at(buf, i, "* shouts: *")) {
	char *player = StripHighlightAndTitle(star_match[0]);
	if (strcmp(player, "Norm") == 0) {
	    static int borecount = 0;
	    if (borecount++ % 5 == 0)
	      SendToICS("shout Norm is a BORE!  I am a PINHEAD!\n");
	    Speak("shout", NULL);
	} else if (StrCaseStr(star_match[1], "pinhead") != NULL) {
	    SendToICS("insult ");
	    SendToICS(star_match[0]);
	    SendToICS("\n");
	} else if (ZippyCalled(star_match[1])) {
	    Speak("shout", NULL);
	}
	return TRUE;
    }

    if (looking_at(buf, i, "* kibitzes: *") && ((random() % 10) < 9)) {
	Speak("kibitz", NULL);
	return TRUE;
    }

    if (looking_at(buf, i, "* whispers: *") && ((random() % 10) < 9)) {
	Speak("whisper", NULL);
	return TRUE;
    }

    if ((looking_at(buf, i, "You have * message*.") &&
	 atoi(star_match[0]) != 0) ||
	looking_at(buf, i, "* has left a message for you.") ||
	looking_at(buf, i, "* just sent you a message.")) {
	SendToICS("messages\nclearmessages\n");
	return TRUE;
    }

    /* Messages */
    if (looking_at(buf, i, "--* (*:*): *") ||
	looking_at(buf, i, "* at *:*: *")) {
	FILE *f;
	char *player = StripHighlightAndTitle(star_match[0]);

	if (strcmp(player, zippyName) != 0) {
	    if ((random() % 10) < 9)
	      Speak("message", player);
	    f = fopen("zip.messagelog", "a");
	    fprintf(f, "--%s (%s:%s): %s\n",
		    player, star_match[1], star_match[2], star_match[3]);
	    fclose(f);
	}
	return TRUE;
    }

    /* Channel tells */
    if (looking_at(buf, i, "*(*): *")) {
	if (strchr(star_match[0], ' ') ||
	    strchr(star_match[1], ' ')) return TRUE;
	if ((random() % 10) < 9) {
	    Speak("tell", star_match[1]);
	}
	return TRUE;
    }

    if (looking_at(buf, i, "Notification: * has arrived")) {
	if ((random() % 3) == 0) {
	    char *player = StripHighlightAndTitle(star_match[0]);
	    strcpy(lastgreet, player);
	    SendToICS("greet ");
	    SendToICS(player);
	    SendToICS("\n");
	    Speak("tell", player);
	}
    }	

    if (looking_at(buf, i, "Not sent -- * is censoring you")) {
	char *player = StripHighlightAndTitle(star_match[0]);
	if (strcmp(player, lastgreet) == 0) {
	    SendToICS("-notify ");
	    SendToICS(player);
	    SendToICS("\n");
	}
    }	

    if (looking_at(buf, i, "command is currently turned off")) {
	zippyUseI = 0;
    }

    return FALSE;
}

void ZippyGameStart(white, black)
     char *white, *black;
{
    /* do nothing */
}

/*
 * Routines to implement Zippy playing chess
 */

void ZippyHandleChallenge(srated, swild, sbase, sincrement, opponent)
     char *srated, *swild, *sbase, *sincrement, *opponent;
{
    char buf[MSG_SIZ];
    int wild, base, increment;
    char rated;

    rated = srated[0];
    if (swild[0] == 'w') {
	char* dwild;
	if ((dwild = (char *)strchr(swild, '(')) != NULL ||
	    (dwild = (char *)strchr(swild, '/')) != NULL) {
	    wild = atoi(dwild) + 1;
	} else {
	    /* Challenge message not understood */
	    return;
	}
    } else {
	wild = 0;
    }
    base = atoi(sbase);
    increment = atoi(sincrement);

    /* Are we blocking match requests from all but one person? */
    if ( zippyAcceptOnlyLength > 0 && StrCaseCmp(opponent, zippyAcceptOnly) )
      {
	sprintf( buf, "tell %s %s\ndecline %s\n", opponent,
		"Sorry, I'm testing xboard and cannot accept matches now.",
		opponent );
	SendToICS( buf );
	return;
      }
    
    if (wild == 1 || wild == 9 || wild == 16 || wild == 24) {
	/* Wild 1, 9, 16, and 24 have different rules from normal chess.
	   In wild 1, you can sometimes castle with the king on the d file.
	   In wild 9, there are two kings per side and rules are bizarre.
	   In wild 16 (kriegspiel), you can't see your opponent's pieces.
	   Wild 24 is bughouse.
	   GNU Chess can't handle these things.
	*/
	sprintf(buf, "tell %s %s\ndecline %s\n", opponent,
		appData.zippyTalk ?
		"I just CAN'T play that kind of wild; it's TOO MUCH FUN!!" :
		"Sorry, this computer can't handle that kind of wild",
		opponent);
	SendToICS(buf);
    } else {
	sprintf(buf, "accept %s\n", opponent);
	SendToICS(buf);
	if (appData.zippyTalk)
	  Speak("tell", opponent);
    }
}


int ZippyMatch(buf, i)
     char *buf;
     int *i;
{
    /* Accept matches */
    if (looking_at(buf, i, "* * match * * requested with * (*)")) {

	ZippyHandleChallenge(star_match[0], star_match[1],
			     star_match[2], star_match[3],
			     StripHighlightAndTitle(star_match[4]));
	return TRUE;
    }

    /* Old FICS 0-increment form */
    if (looking_at(buf, i, "* * match * requested with * (*)")) {

	ZippyHandleChallenge(star_match[0], star_match[1],
			     star_match[2], "0",
			     StripHighlightAndTitle(star_match[3]));
	return TRUE;
    }

    if (looking_at(buf, i,
		   "* has made an alternate proposal of * * match * *.")) {

	ZippyHandleChallenge(star_match[1], star_match[2],
			     star_match[3], star_match[4],
			     StripHighlightAndTitle(star_match[0]));
	return TRUE;
    }

    /* FICS wild/nonstandard forms */
    if (looking_at(buf, i, "Challenge: * (*) *(*) * * * * Loaded from *")) {
	/* note: star_match[2] can include "[white] " or "[black] "
	   before our own name. */
	ZippyHandleChallenge(star_match[4], star_match[8],
			     star_match[8], star_match[9],
			     StripHighlightAndTitle(star_match[0]));
	return TRUE;
    }

    if (looking_at(buf, i,
		   "Challenge: * (*) *(*) * * * * * * Loaded from *")) {
	/* note: star_match[2] can include "[white] " or "[black] "
	   before our own name. */
	ZippyHandleChallenge(star_match[4], star_match[10],
			     star_match[8], star_match[9],
			     StripHighlightAndTitle(star_match[0]));
	return TRUE;
    }

    /* Regular forms */
    if (looking_at(buf, i, "Challenge: * (*) *(*) * * * * * *")) {
	/* note: star_match[2] can include "[white] " or "[black] "
	   before our own name. */
	ZippyHandleChallenge(star_match[4], star_match[5],
			     star_match[8], star_match[9],
			     StripHighlightAndTitle(star_match[0]));
	return TRUE;
    }

    if (looking_at(buf, i, "Challenge: * (*) *(*) * * * *")) {
	/* note: star_match[2] can include "[white] " or "[black] "
	   before our own name. */
	ZippyHandleChallenge(star_match[4], star_match[5],
			     star_match[6], star_match[7],
			     StripHighlightAndTitle(star_match[0]));
	return TRUE;
    }

    if (looking_at(buf, i, "offers you a draw")) {
	SendToProgram("draw\n", firstProgramPR);
	return TRUE;
    }

    if (looking_at(buf, i, "requests that the game be aborted") ||
        looking_at(buf, i, "would like to abort the game")) {
	if ((gameMode == IcsPlayingWhite && whiteTimeRemaining < 0) ||
	    (gameMode == IcsPlayingBlack && blackTimeRemaining < 0)) {
	    SendToICS("abort\n");
	} else {
	    SendToICS("decline abort\n");
	    if (appData.zippyTalk)
	      SendToICS("say Whoa no!  I am having FUN!!\n");
	    else
	      SendToICS("say Sorry, this computer doesn't accept aborts.\n");
	}
	return TRUE;
    }

    if (looking_at(buf, i, "requests adjournment") ||
	looking_at(buf, i, "would like to adjourn the game")) {
      /* For now, only accept adjournments while in "development" mode */
      if ( zippyAcceptOnlyLength == 0 ) {
	SendToICS("decline adjourn\n");
	if (appData.zippyTalk)
	  SendToICS("say Whoa no!  I am having FUN playing NOW!!\n");
	else
	  SendToICS("say Sorry, this computer doesn't accept adjourns.\n");
      } else {
	SendToICS("adjourn\n");
    }
	return TRUE;
    }

    return FALSE;
}

/* This is needed only for gnuchess 4.0 pl62 and earlier; later versions
   do the estimation inside gnuchess */
int EstimateMoves(minutes)
     int minutes;
{
    static int Nmoves[] = {0, 50, 50, 50, 50, 50, 50, 52, 54, 56, 60, 64, 68,
			     72, 76, 80, 84, 88, 90, 94, 98}; /*!!*/

    if (minutes >= sizeof(Nmoves) / sizeof(int)) {
	return Nmoves[sizeof(Nmoves) / sizeof(int) - 1];
    } else {
	return Nmoves[minutes];
    }
}

void ZippyFirstBoard(moveNum, basetime, increment)
     int moveNum, basetime, increment;
{
    char buf[MSG_SIZ];
    int w, b;

    /* Get chess program going */
    if (startedFromSetupPosition || moveNum > 0) {
	/* !! In the moveNum > 0 case we really should
	   wait for the game history to come in and give
	   the program that, because the position doesn't
	   give us the whole story (about castling, en
	   passant, 50 move rule, and repetition).
	   */
	if (!WhiteOnMove(moveNum))
	  SendToProgram("force\na2a3\n", firstProgramPR);

	SendCurrentBoard(firstProgramPR);
    }

    if (gnuLevel == 0) {
	if (increment == 0) {
	    sprintf(buf, "gamein\nlevel %d %d\n",
		    EstimateMoves(basetime), basetime);
	} else {
	    sprintf(buf, "gamein\nlevel f %d %d\n", basetime, increment);
	}
    } else {
	sprintf(buf, "level 0 %d %d\n", basetime, increment);
    } 
    SendToProgram(buf, firstProgramPR);
    SendTimeRemaining(firstProgramPR);

    /* Ratings might be < 0 which means "we haven't seen a ratings
       message from ICS." Send Crafty 0 in that case */
    w = (gameInfo.whiteRating >= 0) ? gameInfo.whiteRating : 0;
    b = (gameInfo.blackRating >= 0) ? gameInfo.blackRating : 0;
    
    firstMove = FALSE;
    if (gameMode == IcsPlayingWhite) {
	sprintf(buf, "name %s\n", gameInfo.black);  /* for Crafty */
	SendToProgram(buf, firstProgramPR);
	strcpy(zippyName, gameInfo.white);
	sprintf(buf, "rating %d %d\n", w, b);
	SendToProgram(buf, firstProgramPR);
	if (WhiteOnMove(moveNum)) {
	    SendToProgram(appData.whiteString, firstProgramPR);
	} else {
	    firstMove = TRUE; /* need to send whiteString later */
	}
    } else if (gameMode == IcsPlayingBlack) {
	sprintf(buf, "name %s\n", gameInfo.white);
	SendToProgram(buf, firstProgramPR);
	strcpy(zippyName, gameInfo.black);
	sprintf(buf, "rating %d %d\n", b, w );
	SendToProgram(buf, firstProgramPR);
	if (!WhiteOnMove(moveNum)) {
	    SendToProgram(appData.blackString, firstProgramPR);
	} else {
	    /* nothing to do */
	}
    }
}


