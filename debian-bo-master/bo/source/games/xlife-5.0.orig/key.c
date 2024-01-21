/*
 * XLife Copyright 1989 Jon Bennett jb7m+@andrew.cmu.edu, jcrb@cs.cmu.edu
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of the copyright holders not be used in
 * advertising or publicity pertaining to distribution of the software without
 * specific, written prior permission.  The copyright holders make no
 * representations about the suitability of this software for any purpose.  It
 * is provided "as is" without express or implied warranty.
 *
 * THE COPYRIGHT HOLDERS DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 */

#include "defs.h"
#include "macros.h"

void getxstring(void)
{
    XComposeStatus status;
    int offset=0, buflen, windowlen;
    char tmpinpbuf[INPBUFLEN];

    state = STOP;
    for (;;)
    {
	XMaskEvent(disp, KeyPressMask | ButtonPressMask | Button1MotionMask 
		   | PointerMotionMask | Button3MotionMask | ExposureMask 
		   | StructureNotifyMask,&event);
	/* handle other kinds of events if no key is pressed  */ 
	strcpy(tmpinpbuf,inpbuf);
	switch(event.type)
	{
	case MotionNotify:
            Motion();
	    break;
	case ButtonPress:
	    Button();
	    break;
	case ConfigureNotify:
	    DoResize();
	    break;
	case Expose:
	    DoExpose(ClassifyWin(event.xexpose.window));
	default:
	    break;
	} 
	strcpy(inpbuf,tmpinpbuf);

	showcoord(FALSE);

	if (event.type != KeyPress)
	{
	    /* non KeyPress events might write on inputw */
	    XClearWindow(disp, inputw);
	    XDrawString(disp, inputw, ntextgc, ICOORDS(0,0), 
			inpbuf + offset, strlen(inpbuf));
        }
        else
	{ 
	    XLookupString(&event.xkey, keybuf, 16, &ks, &status);
	    
	    if (IsModifierKey(ks))
		continue;

	    /* compute window length for rescrolling */
            windowlen = 
		(width
		 - RULEW
		 - dispcoord * (COORDW+BORDERWIDTH)-BORDERWIDTH*2) / FONTWIDTH;
	    switch(ClassifyWin(event.xkey.window))
	    {
	    case INPUTWIN:
	    case LIFEWIN:
	    case HELPWIN:
		if ((ks != XK_Return) && (ks != XK_Linefeed))
		{
		    if ((ks == XK_BackSpace) || (ks == XK_Delete))
		    {
			buflen = strlen(inpbuf);
			if (buflen>minbuflen)
			{
			    inpbuf[buflen - 1] = 0;
			    XClearWindow(disp, inputw);
			    offset = (buflen > windowlen) ? buflen - windowlen : 0;
			    XDrawString(disp, inputw, ntextgc, ICOORDS(0,0), inpbuf + offset, buflen);
			}
		    }
		    else
		    {
			if (ks == '~')
			{
			    inpbuf[minbuflen] = '\0';
			    XClearWindow(disp,inputw);
			}
			strcat(inpbuf, keybuf);
			buflen = strlen(inpbuf);
			if (buflen > INPBUFLEN) inpbuf[INPBUFLEN] = 0;
			offset = (buflen > windowlen) ? buflen - windowlen : 0;
			if (offset) XClearWindow(disp, inputw);
			XDrawString(disp, inputw, ntextgc, ICOORDS(0,0), inpbuf + offset, buflen);
		    }
		}
		else
		{
		    XClearWindow(disp, inputw);
		    return;
		}
	    }
	}
    }
}

#if STATEBITS > 1

void test_transition(void)
/* interactively probe the transitions table */
{
    char outbuf[100];

#define TESTPROMPT	"Test transition: "
    if (state == STOP)
    {
	char	s, n1, n2, n3, n4;
	int	ns;

	announce(TESTPROMPT);
	getxstring();

	(void) strcpy(outbuf, inpbuf + sizeof(TESTPROMPT) - 1);
        if (sscanf(outbuf, "%c%c%c%c%c", &s, &n1, &n2, &n3, &n4) != 5
		|| !is_state(s)
		|| !is_state(n1) || !is_state(n2)
		|| !is_state(n3) || !is_state(n4))
	    announce("This command wants exactly five valid state codes.");
	else
	{
	    (void) sprintf(outbuf,"%c%c%c%c%c", s, n1, n2, n3, n4);
	    if ((ns = newstate(stoi(s), stoi(n1), stoi(n2), stoi(n3), stoi(n4))) == BADSTATE)
		(void) strcat(outbuf, " has no defined outcome.");
	    else
		(void) sprintf(outbuf + strlen(outbuf), ": %c", itos(ns));
	    announce(outbuf);
	}
    }
    else
    {
	(void) strcpy(inpbuf, TESTPROMPT);
	minbuflen = sizeof(TESTPROMPT);
	announce(inpbuf);
    }
}

void set_transition(void)
{
    char outbuf[100];
    extern char *parse_rule();

#define SETPROMPT	"Set transition: "
    if (state == STOP)
    {
	char	*err;

	(void) strcpy(outbuf, inpbuf + sizeof(SETPROMPT) - 1);
	if (err = parse_rule(outbuf))
	    announce(err);
	else
	    announce(outbuf);
    }
    else
    {
	(void) strcpy(inpbuf, SETPROMPT);
	minbuflen = sizeof(SETPROMPT);
	announce(inpbuf);
    }
}

int patch_transition(cell_t s, cell_t n1, cell_t n2, cell_t n3, cell_t n4)
{
    char outbuf[100];
    char	ns;

#define PATCHPROMPT	"New state for neighborhood %c%c%c%c%c: "

    (void) sprintf(outbuf, PATCHPROMPT,
		   itos(s), itos(n1), itos(n2), itos(n3), itos(n4));
    announce(outbuf);
    for (;;)
    {
	getxstring();

	ns = inpbuf[strlen(inpbuf) - 1];
	if (!is_state(ns))
	    announce("Please supply one valid state code: ");
	else
	    break;
    }
    (void) make_transition(s, n1, n2, n3, n4, stoi(ns));
    (void) sprintf(outbuf,
		   "%c%c%c%c%c: %c", 
		   itos(s),itos(n1),itos(n2),itos(n3),itos(n4), ns);
    announce(outbuf);

    return(stoi(ns));
}

#endif /* STATEBITS > 1 */
