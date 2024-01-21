/*	SC	A Spreadsheet Calculator
 *
 *	One line vi emulation
 *	$Revision: 1.1 $
 */

#include <config.h>
#include <curses.h>
#include <sys/types.h>

#include <signal.h>
#ifdef HAVE_X11_X_H
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#endif /* HAVE_X11_X_H */

#include <ctype.h>
#include "sc.h"

#ifdef HAVE_X11_X_H
#include "scXstuff.h"
#endif

#define istext(a) (isalnum(a) || ((a) == '_'))

static void	append_line PROTO((void));
static void	back_hist PROTO((void));
static int	back_line PROTO((void));
static int	back_word PROTO((void));
static void	back_space PROTO((void));
static void	change_cmd PROTO((void));
static void	col_0 PROTO((void));
static void	delete_cmd PROTO((void));
static void	del_chars PROTO((int, int));
static void	del_in_line PROTO((void));
static void	del_to_end PROTO((void));
static void	dotcmd PROTO((void));
static int	end_line PROTO((void));
static int	find_char PROTO((void));
static void	for_hist PROTO((void));
static int	for_line PROTO((int));
static int	for_word PROTO((int));
static int	get_motion PROTO((void));
static void	ins_in_line PROTO((int));
static void	last_col PROTO((void));
static void	rep_char PROTO((void));
static void	replace_in_line PROTO((int));
static void	replace_mode PROTO((void));
static void	restore_it PROTO((void));
static void	savedot PROTO((int));
static void	save_hist PROTO((void));
static void	search_again PROTO((void));
static void	search_hist PROTO((void));
static void	search_mode PROTO((void));
static void	stop_edit PROTO((void));
static int	to_char PROTO((void));
static void	u_save PROTO((int));

extern int showrange;
extern char mode_ind;		/* Mode indicator */

/* values for mode below */

#define INSERT_MODE	0	/* Insert mode */
#define EDIT_MODE       1	/* Edit mode */
#define REP_MODE        2	/* Replace mode */
#define SEARCH_MODE	3	/* Get arguments for '/' command */

#define	DOTLEN		200

static int mode = INSERT_MODE;
struct	hist {
	unsigned	int	len;
	char	*histline;
} history[HISTLEN];

static int histp = -1;
static int lasthist = -1;
static int endhist = -1;
static char *last_search = NULL;
static char *undo_line = NULL;
static int undo_lim;
static char dotb[DOTLEN];
static int doti = 0;
static int do_dot = 0;

void
write_line(c)
int c;
{
    if (mode == EDIT_MODE) {
	switch(c) {
	case (ctl('h')):	linelim = back_line();		break;
	case (ctl('m')):  cr_line();			break;
	case ESC:	stop_edit();			break;
	case '+':	for_hist();			break;
	case '-':	back_hist();			break;
	case '$':	last_col();			break;
	case '.':	dotcmd();			break;
	case '/':	search_mode();			break;
	case '0':	col_0();			break;
	case 'C':	u_save(c);del_to_end(); last_col();
			append_line();			break;
	case 'D':	u_save(c);del_to_end();		break;
	case 'I':	u_save(c);col_0();insert_mode();break;
	case 'R':	replace_mode();			break;
	case 'X':	u_save(c); back_space();	break;
	case 'a':	u_save(c); append_line();	break;
	case 'A':	u_save(c);last_col();append_line();	break;
	case 'b':	linelim = back_word();		break;
	case 'c':	u_save(c); change_cmd();	break;
	case 'd':	u_save(c); delete_cmd();	break;
	case 'f':	linelim = find_char();		break;
	case 'h':	linelim = back_line();		break;
	case 'i':	u_save(c); insert_mode();	break;
	case 'j':	for_hist();			break;
	case 'k':	back_hist();			break;
	case ' ':
	case 'l':	linelim = for_line(0);		break;
	case 'n':	search_again();			break;
	case 'q':	stop_edit();			break;
	case 'r':	u_save(c); rep_char();		break;
	case 't':	linelim = to_char();		break;
	case 'u':	restore_it();			break;
	case 'w':	linelim = for_word(0);		break;
	case 'x':	u_save(c); del_in_line();	break;
	default:	break;
	}
    } else if (mode == INSERT_MODE) { 
	savedot(c);
	switch(c) {
	case (ctl('h')):	back_space();			break;
	case (ctl('m')):  cr_line();			break;
	case ESC:	edit_mode();			break;
	default:	ins_in_line(c);			break;
	}
    } else if (mode == SEARCH_MODE) {
	switch(c) {
	case (ctl('h')):	back_space();			break;
	case (ctl('m')):  search_hist();			break;
	case ESC:	edit_mode();			break;
	default:	ins_in_line(c);			break;
	}
   } else if (mode == REP_MODE) {
	savedot(c);
	switch(c) {
	case (ctl('h')):	back_space();			break;
	case (ctl('m')):  cr_line();			break;
	case ESC:	edit_mode();			break;
	default:	replace_in_line(c);		break;
	}
    }
}

void
edit_mode()
{
    mode = EDIT_MODE;
    mode_ind = 'e';
    histp = -1;
    if (linelim < 0)	/* -1 says stop editing, ...so we still aren't */
	return;
    if (line[linelim] == '\0')
	linelim = back_line();
}

void
insert_mode()
{
    mode_ind = 'i';
    mode = INSERT_MODE;
}

static	void
search_mode()
{
    line[0] = '/';
    line[1] = '\0';
    linelim = 1;
    histp = -1;
    mode_ind = '/';
    mode = SEARCH_MODE;
}

static	void
replace_mode()
{
    mode_ind = 'R';
    mode = REP_MODE;
}

/* dot command functions.  Saves info so we can redo on a '.' command */

static	void
savedot(c)
int c;
{
    if (do_dot || (c == '\n'))
	return;

    if (doti < DOTLEN-1)
    {
	dotb[doti++] = c;
	dotb[doti] = '\0';
    }
}

static int dotcalled = 0;

static	void
dotcmd()
{
    int c;

    if (dotcalled)	/* stop recursive calling of dotcmd() */
	return;
    do_dot = 1;
    doti = 0;
    while(dotb[doti] != '\0') {
	c = dotb[doti++];
	dotcalled = 1;
	write_line(c);
    }
    do_dot = 0;
    doti = 0;
    dotcalled = 0;
}

int
vigetch()
{
    int c;

    if(do_dot) {
	if (dotb[doti] != '\0') {
	    return(dotb[doti++]);
	} else {
	    do_dot = 0;
	    doti = 0;
	    return(nmgetch());
	}
    }
    c = nmgetch();
    savedot(c);
    return(c);
}

/* saves the current line for possible use by an undo cmd */
static	void
u_save(c)
int c;
{   static	unsigned	undolen = 0;

    if (strlen(line)+1 > undolen)
    {	undolen = strlen(line)+40;

	undo_line = scxrealloc(undo_line, undolen);
    }
    (void) strcpy(undo_line, line);

    undo_lim = linelim;

    /* reset dot command if not processing it. */

    if (!do_dot) {
        doti = 0;
	savedot(c);
    }
}

/* Restores the current line saved by u_save() */
static	void
restore_it()
{
    static	char *tempc = NULL;
    static	unsigned templen = 0;
    int		tempi;

    if ((undo_line == NULL) || (*undo_line == '\0')) 
	return;

    if (strlen(line)+1 > templen)
    {	templen = strlen(line)+40;
	tempc = scxrealloc(tempc, templen);
    }

    strcpy(tempc, line);
    tempi = linelim;
    (void) strcpy(line, undo_line);
    linelim = undo_lim;
    strcpy(undo_line, tempc);
    undo_lim = tempi;
}

/* This command stops the editing process. */
static	void
stop_edit()
{
    showrange = 0;
    linelim = -1;
    if (!using_X)
    {	(void) move(1, 0);
	(void) clrtoeol();
    }
}

/*
 * Motion commands.  Forward motion commands take an argument
 * which, when set, cause the forward motion to continue onto
 * the null at the end of the line instead of stopping at the
 * the last character of the line.
 */
static	int
for_line(stop_null)
int stop_null;
{
    if (linelim >= 0 && line[linelim] != '\0' && 
    		        (line[linelim+1] != '\0' || stop_null))
	return(linelim+1);
    else
	return(linelim);
}

static int	
end_line()
{	return (strlen(line));
}


static	int
for_word(stop_null)
int stop_null;
{
    register int c;
    register int cpos;

    cpos = linelim;

    if (line[cpos] == ' ') {
	while (line[cpos] == ' ')
	    cpos++;
	if (cpos > 0 && line[cpos] == '\0')
	    --cpos;
	return(cpos);
    }

    if (istext(line[cpos])) {
    	while ((c = line[cpos]) && istext(c)) 
		cpos++;
    } else {
	while ((c = line[cpos]) && !istext(c) && c != ' ')
		cpos++;
    }

    while (line[cpos] == ' ')
        cpos++;

    if (cpos > 0 && line[cpos] == '\0' && !stop_null) 
        --cpos;

    return(cpos);
}

static	int
back_line()
{
    if (linelim)
        return(linelim-1);
    else
	return(0);
}

static	int
back_word()
{
    register int c;
    register int cpos;

    cpos = linelim;

    if (line[cpos] == ' ') {
	/* Skip white space */
        while (cpos > 0 && line[cpos] == ' ')
	    --cpos;
    } else if (cpos > 0 && (line[cpos-1] == ' ' 
		     || (istext(line[cpos]) && !istext(line[cpos-1]))
		     || (!istext(line[cpos]) &&  istext(line[cpos-1])))) {
	/* Started on the first char of a word - back up to prev. word */
	--cpos;
        while (cpos > 0 && line[cpos] == ' ')
	    --cpos;
    }

    /* Skip across the word - goes 1 too far */
    if (istext(line[cpos])) {
    	while (cpos > 0 && (c = line[cpos]) && istext(c)) 
		--cpos;
    } else {
	while (cpos > 0 && (c = line[cpos]) && !istext(c) && c != ' ')
		--cpos;
    }

    /* We are done - fix up the one too far */
    if (cpos > 0 && line[cpos] && line[cpos+1]) 
	cpos++;

    return(cpos);
}

/* Text manipulation commands */

static	void
del_in_line()
{
    register int len, i;

    if (linelim >= 0) {
	len = strlen(line);
	if (linelim == len && linelim > 0)
	    linelim--;
	for (i = linelim; i < len; i++)
	    line[i] = line[i+1];
    }
    if (linelim > 0 && line[linelim] == '\0')
	--linelim;
}

static	void
ins_in_line(c)
int c;
{
    register int i, len;

    if (linelim < 0)
    {	*line = '\0';
	linelim = 0;
    }
    len = strlen(line);
    for (i = len; i >= linelim; --i)
	line[i+1] = line[i];
    line[linelim++] = c;
    line[len+1] = '\0';
}

void
ins_string(s)
char *s;
{
    while (*s)
	ins_in_line(*s++);
}

static	void
append_line()
{
    register int i;

    i = linelim;
    if (i >= 0 && line[i])
	linelim++;
    insert_mode();
}

static	void
rep_char()
{
    int c;

    if (linelim < 0)
    {	linelim = 0;
	*line = '\0';
    }
    c = vigetch();
    if (line[linelim] != '\0') {
    	line[linelim] = c;
    } else {
	line[linelim] = c;
	line[linelim+1] = '\0';
    }
}

static	void
replace_in_line(c)
int	c;
{
    register int len;

    if (linelim < 0)
    {	linelim = 0;
	*line = '\0';
    }
    len = strlen(line);
    line[linelim++] = c;
    if (linelim > len)
	line[linelim] = '\0';
}

static	void
back_space()
{
    if (linelim == 0)
	return;

    if (line[linelim] == '\0') {
	linelim = back_line();
	del_in_line();
	linelim = strlen(line);
    } else {
	linelim = back_line();
	del_in_line();
    }
}

static	int
get_motion()
{
    int c;

    c = vigetch();
    switch (c) {
    case 'b':	return(back_word());
    case 'f':	return(find_char()+1);
    case 'h':	return(back_line());
    case 'l':	return(for_line(1));
    case 't':	return(to_char()+1);
    case 'w':	return(for_word(1));
    case '0':	return (0);
    case '$': 	return(end_line());
    default:	return(linelim);
    }
}

static del_end;

static	void
delete_cmd()
{
    int cpos;
    int	ll = strlen(line);
  
    del_end = 0;
    cpos = get_motion();
    if (cpos == ll || linelim == ll)
	del_end = 1;
    del_chars(cpos, linelim);
}

static	void
change_cmd()
{
    delete_cmd();
    if (del_end)
	append_line();
    else
	insert_mode();
}

static	void
del_chars(first, last)
register int first, last;
{
    int temp;

    if (first == last)
	return;

    if (last < first) {
	temp = last; last = first; first = temp;
    }

    linelim = first;
    while(first < last) {
	del_in_line();
	--last;
    }
}

static	void
del_to_end()
{
    if (linelim < 0)
	return;
    line[linelim] = '\0';
    linelim = back_line();
}

void
cr_line()
{
    insert_mode();
    if (linelim != -1) {
	showrange = 0;
	save_hist();
	linelim = 0;
	(void) yyparse ();
	linelim = -1;
    }
    else	/* '\n' alone will put you into insert mode */
    {	*line = '\0';
	linelim = 0;
    }
}

/* History functions */

static	void
save_hist()
{
    if (lasthist < 0)
    {	lasthist = 0;
    }
    else
	lasthist = (lasthist + 1) % HISTLEN;

    if (lasthist > endhist)
	endhist = lasthist;

    if (history[lasthist].len < strlen(line)+1)
    {	history[lasthist].len = strlen(line)+40;
	history[lasthist].histline = scxrealloc(history[lasthist].histline,
					      history[lasthist].len);
    }
    (void) strcpy(history[lasthist].histline, line);
}

static	void
back_hist()
{
    if (histp == -1)
	histp = lasthist;
    else
    if (histp == 0)
    {	if (endhist != lasthist)
		histp = endhist;
    }
    else
    if (histp != ((lasthist + 1) % (endhist + 1)))
	histp--;

    if (lasthist < 0)
	line[linelim = 0] = '\0';
    else {
    	(void) strcpy(line, history[histp].histline);
	linelim = 0;
    }
}

static	void
search_hist()
{
    static	unsigned lastsrchlen = 0;

    if(linelim < 1) {
	linelim = 0;
	edit_mode();
	return;
    }

    if (strlen(line)+1 > lastsrchlen)
    {	lastsrchlen = strlen(line)+40;
	last_search = scxrealloc(last_search, lastsrchlen);
    }
    (void)strcpy(last_search, line+1);
    search_again();
    mode = EDIT_MODE;
}

static	void
search_again()
{
    int found_it;
    int do_next;
    int prev_histp;
    char *look_here;

    prev_histp = histp;
    if ((last_search == NULL) || (*last_search == '\0'))
	return;

    do {
	back_hist();
	if (prev_histp == histp)
	    break;
	prev_histp = histp;
	look_here = line;
	found_it = do_next = 0;
	for ( look_here = strchr(look_here, last_search[0]);
	      look_here != NULL && !found_it && !do_next;
	      look_here = strchr(look_here, last_search[0]) )
	{
	    if (strncmp(look_here, last_search, strlen(last_search)) == 0)
		found_it++;
	    else if (look_here < line + strlen(line) - 1)
	        look_here++;
	    else
		do_next++;
	}
    } while (!found_it);
}

static	void
for_hist()
{
    if (histp == -1)
	histp = lasthist;
    else
    if (histp != lasthist)
	histp = (histp + 1) % (endhist + 1);

    if (lasthist < 0)
	line[linelim = 0] = '\0';
    else {
	(void) strcpy(line, history[histp].histline);
	linelim = 0;
    }
}

static	void
col_0()
{
    linelim = 0;
}

static	void
last_col()
{
    linelim = strlen(line);
    if (linelim > 0)
	--linelim;
}

static	int
find_char()
{
    register int c;
    register int i;


    c = vigetch();
    i = linelim;
    while(line[i] && line[i] != c)
	i++;
    if (!line[i])
	i = linelim;
    return(i);
}

static	int
to_char()
{
    register int i;

    i = find_char();
    if (i > 0 && i != linelim)
	--i;

    return(i);
}


char *
get_str(s, max_str_len)
   char *s;               /* prompt and returned string */
   int max_str_len;
{
  static char buf[1024]; /* hold the characters as they are typed */
#ifdef HAVE_X11_X_H
  int count=0;           /* how many characters have been entered */
  int maxcount;          /* the max number of chars to be entered */
  int done=0;            /* true when input is finished */
  int slen;              /* length of prompt string */
  XEvent event;          /* input event structure */
  char keystr[3];        /* ASCII version of keypress */

 if (using_X)
 {
  clearlines(0,0);
  slen = strlen(s);
  max_str_len--;      /* decrease this to save room for null byte */
  maxcount = maintextcols - slen;
  maxcount = ((maxcount < max_str_len) ? maxcount : max_str_len);
  buf[0]='_'; /* the "cursor" */

  if (slen)
     XDrawImageString(dpy,mainwin,maingc, textcol(0), textrow(0), s, slen);

  XDrawImageString(dpy,mainwin,maingc, textcol(slen+1), textrow(0), "_", 1 );
  while (!done){
    XNextEvent(dpy, &event);
    switch(event.type){

      case Expose:
        update(FALSE);
        if (slen)
            XDrawImageString(dpy, mainwin, maingc,
                           textcol(0), textrow(0), s, slen);
        if (count)
            XDrawImageString(dpy, mainwin, maingc,
                           textcol(slen), textrow(0), buf, count);

        XDrawImageString(dpy, mainwin, maingc,
                         textcol(slen+count), textrow(0), "_", 1 );
        break;

      case MappingNotify:
        XRefreshKeyboardMapping(&(event.xmapping));
        break;

      case ConfigureNotify:
        sc_handleresize(&event);
        maxcount = maintextcols - slen;
        maxcount = ((maxcount < max_str_len) ? maxcount : max_str_len);
        break;

      case KeyPress:
        if (XLookupString(&(event.xkey), keystr, 3, 0, 0)){
          switch( keystr[0]){
            case 10: /* linefeed */
            case 13: /* carriage return */
              done = 1;
              break;
            case ctl('h'):  /* backspace */
            case ctl('?'):  /* delete */
	    case DEL:
              if (count){
                buf[--count]='_';
                XDrawImageString(dpy,mainwin,maingc,
                                 textcol(slen+count+1),textrow(0), "_ ", 2);
              } else {
                XBell(dpy,50);
                /*fprintf(stderr,"\007");*/ /* bell */
              }
              break;

            default:
                if ((keystr[0]>=32) && (keystr[0]<127)){
                    if (count<maxcount){
                      buf[count++]=keystr[0];
                      buf[count]='_';
                      XDrawImageString(dpy,mainwin,maingc,
                                   textcol(slen+count),textrow(0),
				   buf+count-1, 2);
                    } else
                       XBell(dpy,50);
                       /*fprintf(stderr,"\007");*/
                } else
                    XBell(dpy,50);
                    /*fprintf(stderr,"\007");*/
              break;

          } /* switch keystr[0] */
        } /* if XLookupString */
      } /* switch event.type */
    } /* while !done */
    buf[count] = 0;
    strcpy(s, buf);
    clearlines(0,0);
    show_top_line();
    return s;
 } else
#endif /* HAVE_X11_X_H */
 {
	move(0,0);
	clrtoeol();
	printw("%s", s);
	refresh();

	echo();
	getstr(buf);
	noecho();
	strncpy(s, buf, max_str_len);
	s[max_str_len] = '\0';
	move(0,0);
	clrtoeol();
	show_top_line();
	return(s);
 } /* HAVE_X11_X_H, end curses */
}

