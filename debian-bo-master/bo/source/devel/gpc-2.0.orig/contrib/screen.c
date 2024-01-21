
#include <stdio.h>
#include <sys/ioctl.h>

/* A screen module for gpc .... */

/* Routine descriptions. */

int tgetent(char *bp, char *name);
int tgetnum (char *id);
int tgetflag (char *id);
char * tgetstr(char *id, char **area);
char * tgoto(char *cm, int destcol, int destline);
void   tputs(char *cp, int affcnt, int (*outc)(char));
char * getenv (char *);

/* The terminal description */
char _scr_bp[1024];

/* The terminal type. */
char *_scr_tname;

/* String Storage. */
char _scr_area[512];

/* Specific pointers. */
char *_scr_ce;  /* clear to end of line */
char *_scr_cd;  /* clear to end of display */
char *_scr_cl;  /* home and clear */
char *_scr_cm;  /* cursor motion. */
char *_scr_ho;  /* home cursor */

char *_scr_up;
char *_scr_do;
char *_scr_ri;
char *_scr_le;

char _scr_mode_changed = 0;

/* Have we done the init yet? */
int _scr_did_init = 0;

/* Functions ... */

/* The output function: */
int _scr_outc (char c)
{
  fputc (c, stdout);
}

static struct sgttyb orig_mode;
static struct sgttyb current_mode;
static struct sgttyb saved_mode;

int
c_tty_mode (mode)
     int mode;
{
  if (! _scr_did_init)
    {
      fprintf (stderr, "You need to call c_init() before calling this\n");
      return 1;
    }

  switch (mode) {
  case 0:	/* Restore original modes */
    current_mode = orig_mode;
    break;
    
  case 1:	/* Set RAW mode */
    current_mode.sg_flags |= (RAW);
    break;

  case 2:	/* Set Cooked mode (e.g. unset RAW */
    current_mode.sg_flags &= ~(RAW);
    break;

  case 3:	/* Set CBREAK mode */
    current_mode.sg_flags |= (CBREAK);
    break;

  case 4:	/* Unset CBREAK mode */
    current_mode.sg_flags &= ~(CBREAK);
    break;

  case 5:	/* Set NO ECHO mode */
    current_mode.sg_flags &= ~(ECHO);
    break;
    
  case 6:	/* Set ECHO mode */
    current_mode.sg_flags |= (ECHO);
    break;

  case 7:	/* Save current mode */
    saved_mode = current_mode;
    return 0;

  case 8:	/* Restore saved mode */
    current_mode = saved_mode;
    break;
  }
  
  if (ioctl (0, TIOCSETP, (char *)&current_mode) == -1)
    {
      perror ("c_tty_mode: Can't set tty mode");
      
      return 1;
    }
}

/*
 * Initialize the screen handling for gpc.
 */

/* Called to initialize the "module." */
int
c_init (how)
     int how;
{
  char *area;

  if (! how)
    {
      /* Called to reset everything before exiting */
      c_tty_mode (0);

      return 0;
    }

  _scr_tname = getenv ("TERM");
  tgetent (_scr_bp, _scr_tname);

  /* Get all the proper strings ... */
  area = _scr_area;

  _scr_cd = tgetstr ("cd", &area);
  _scr_ce = tgetstr ("ce", &area);
  _scr_cl = tgetstr ("cl", &area);
  _scr_cm = tgetstr ("cm", &area);
  _scr_ho = tgetstr ("ho", &area);
  
  _scr_up = tgetstr ("ku", &area);
  _scr_do = tgetstr ("kd", &area);
  _scr_le = tgetstr ("kl", &area);
  _scr_ri = tgetstr ("kr", &area);

  if (! _scr_ri)
    fprintf (stderr, "Cursor movement right is not supported\n");

  if (! _scr_le)
    fprintf (stderr, "Cursor movement left is not supported\n");

  if (! _scr_up)
    fprintf (stderr, "Cursor movement up is not supported\n");

  if (! _scr_do)
    fprintf (stderr, "Cursor movement down is not supported\n");

  if (ioctl (0, TIOCGETP, (char *)&orig_mode) == -1)
    {
      perror ("Can't save tty modes in c_init");
      return 1;
    }

  /* Save the original tty modes also here */
  saved_mode  = current_mode = orig_mode;

  /* Error Checks */
  if (_scr_cm == NULL)
    {
      fprintf (stderr, "Terminal is not powerful enough.\n");
      return 1;
    }

  _scr_did_init = 1;

  return 0;
}  

void c_right (count)
     int count;
{
  for (; count > 0; count--)
    tputs (_scr_ri, 1, _scr_outc);
}

void c_left (count)
     int count;
{
  for (; count > 0; count--)
    tputs (_scr_le, 1, _scr_outc);
}

void c_down (count)
     int count;
{
  for (; count > 0; count--)
    tputs (_scr_do, 1, _scr_outc);
}

void c_up (count)
     int count;
{
  for (; count > 0; count--)
    tputs (_scr_up, 1, _scr_outc);
}

/* Cursor Movement */
void c_gotoxy (int x, int y)
{
  tputs (tgoto (_scr_cm, x, y), 1, _scr_outc);
}


/* Clear the entire screen and move to home. */
void c_clearscreen ()
{
  tputs (_scr_cl, 1, _scr_outc);
}


/* Clear from cursor to the end of the screen. */
void c_cleartoeos ()
{
  tputs (_scr_cd, 1, _scr_outc);
}

/* Clear from cursor to the end of the line. */
void c_cleartoeol ()
{
  tputs (_scr_ce, 1, _scr_outc);
}

/* Go to the home position. */
void c_home()
{
  tputs (_scr_ho, 1, _scr_outc);
}

int c_getch()
{
  return getchar();
}
