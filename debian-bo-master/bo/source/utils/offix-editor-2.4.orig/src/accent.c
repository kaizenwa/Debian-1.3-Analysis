/*
 *  Editor - a menu-driven text editor
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 */

#include "xedit.h"

#include <X11/keysym.h>

static int WaitForKey(void);
static void isoPrint(int accent,char *input,char *translation);
static void PrintOneChar(int letter);

extern int overwritemode;

static Display *dpy;
static Window win;
static Widget wid;

int WaitForKey(void)
{
	XEvent event;
	int state,key=0;

	while(key==0)
	{	XNextEvent(dpy,&event);
		switch(event.type)
		{
			case KeyPress:
				state=(event.xkey.state & ShiftMask);
				key=XKeycodeToKeysym(dpy,event.xkey.keycode,
						     state!=0);
				if(key>255 && key!=XK_Escape) key=0;
				break;
			default:
				XtDispatchEvent(&event);
				break;
		}
	}
	
	return key;
}

void PrintOneChar(int letter)
{
   	XawTextPosition start,end;
	XawTextBlock text; char thetext[1];
	
	if(letter==XK_Escape) return;

	/* Get the current text position */
	start=XawTextGetInsertionPoint(wid);
	end=start;
	
	/* Initialize the text block */
	text.firstPos=0;
	text.ptr=thetext;
#ifdef XawFmt8Bit
	text.format=XawFmt8Bit;
#else
	text.format=FMT8BIT;
#endif
	text.length=1;
	thetext[0]=letter;

	if(overwritemode) end+=1;
	XawTextReplace(wid,start,end,&text);
	XawTextSetInsertionPoint(wid,start+1);
}


void isoPrint(int accent,char *input,char *translation)
{
	int key=WaitForKey();
	if(key==XK_Escape) return;

	/* If it's a valid key, print it's translation */
	if(strchr(input,key)!=NULL)
	{	PrintOneChar(*(strchr(input,key)-input+translation));
		return;
	}
	/* Else, print the character and the	*
	 * the pressed key			*/
	PrintOneChar(accent);
	PrintOneChar(key);
}

void isoAccent(Widget w,XEvent *event,String *params,Cardinal *num_params)
{
	int state,key;
	dpy=XtDisplay(w); win=XtWindow(w); wid=w;
	state=(event->xkey.state & ShiftMask);
	key=XKeycodeToKeysym(dpy,event->xkey.keycode,state!=0);
	switch(key)
	{	case '~':
			isoPrint('~',"aonAON ~","ãõñÃÕÑ~~"); break;
		case '\'':
			isoPrint('\'',"aeioucAEIOUC '","áéíóúçÁÉÍÓÚÇ''");
			break;
		case '`':
			isoPrint('`',"aeiouAEIOU `","àèìòùÀÈÌÒÙ``");
			break;
		case '"':
			isoPrint('"',"aeiousAEIOUS \"","äëïöüßÄËÏÖÜß\"\"");
			break;
		case '^':
			isoPrint('^',"aeiouAEIOU ^","âêîôûÂÊÎÔÛ^^");
			break;
		default:
			PrintOneChar(key);
			break;
	}
}
