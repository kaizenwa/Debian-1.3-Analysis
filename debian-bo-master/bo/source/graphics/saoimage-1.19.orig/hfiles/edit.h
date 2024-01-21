#ifndef lint
static char SccsEditId[] = "%W%  %G%";
#endif

/* Module:	Edit.h
 * Purpose:	Define the structs for text input edit sessions
 * Modified:	{0} Michael VanHilst	initial version		  7 July 1989
 *		{1} Doug Mink  increased stack size from 5 to 128 19 Oct 1995
 *		{n} <who> -- <does what> -- <when>
 */

#define STACK_SZ 128

typedef struct _editstring {
  int char_cnt;			/* number of characters in string */
  int oversize;			/* string-too-long-to-show-all */
  int *charsz;			/* array of character widths */
  char *string;			/* character string */
} EditString;

/* structure for an editor session's string info and buffers */
typedef struct _editstruct {
  Display *display;		/* display in which to write */
  Window ID;			/* window in which to write */
  XFontStruct *fontstruct;	/* info about font to use */
  Font font;			/* font for gc */
  GC gc;			/* drawing gc */
  int space_width;		/* width of space character in this font */
  unsigned long foreground;	/* text foreground color */
  unsigned long background;	/* text (and window?) background color */
  int x, y;			/* beginning coordinate of line */
  int oversize;			/* string-is-too-long-to-show-all-of-it */
  int first_char_shown;		/* first character in section shown */
  int last_char_shown;
  int last_x_shown;
  int left_mark_drawn;		/* left-overflow-mark-drawn */
  int right_mark_drawn;		/* right-overflow-mark-drawn */
  int area_y;
  int area_height;
  int max_pixlen;		/* maximum length of line in pixels */
  int max_char_cnt;		/* maximum number of characters in buffer */
  int active_position;		/* current text cursor position */
  int char_cnt;			/* number of characters in string */
  int *charsz;			/* array of character widths */
  char *string;			/* character string */
  int *pixlen;			/* pixel offset of beginning of each char */
  int stack_index;		/* last referenced stored line */
  int stack_cnt;		/* number of lines stored */
  int pad;			/* 28th int for even number */
  EditString buf[STACK_SZ];	/* line storage ring */
} EditStruct;

/* stucture for info about the popup window */
struct popRec {
  Display *display;
  Window ID;
  unsigned long foreground;
  unsigned long background;
  XFontStruct *fontstruct;
  Font font;
  int ref_width;		/* reference by which dimensions are set */
  int ref_height;
  int pointer_x, pointer_y;	/* position of pointer before popwin mapped */
  int width;
  int height;
  int font_height;
  int one_y;
  int two_y1;
  int two_y2;
  int prompt_x;
  int prompt_y;
};
