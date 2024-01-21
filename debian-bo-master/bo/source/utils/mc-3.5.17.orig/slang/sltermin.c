/* This file contains enough terminfo reading capabilities sufficient for
 * the slang SLtt interface.
 */

/* Copyright (c) 1992, 1995 John E. Davis
 * All rights reserved.
 * 
 * You may distribute under the terms of either the GNU General Public
 * License or the Perl Artistic License.
 */

#include <config.h>
#include <stdio.h>
#ifndef USE_SETUPTERM
#include "slang.h"
#include "_slang.h"

/*
 * The majority of the comments found in the file were taken from the
 * term(4) man page on an SGI.
 */
 
/* Short integers are stored in two 8-bit bytes.  The first byte contains
 * the least significant 8 bits of the value, and the second byte contains
 * the most significant 8 bits.  (Thus, the value represented is
 * 256*second+first.)  The value -1 is represented by 0377,0377, and the
 * value -2 is represented by 0376,0377; other negative values are illegal.
 * The -1 generally means that a capability is missing from this terminal.
 * The -2 means that the capability has been cancelled in the terminfo
 * source and also is to be considered missing.
 */

static int make_integer (unsigned char *buf)
{
   register int lo, hi;
   lo = (int) *buf++; hi = (int) *buf;
   if (hi == 0377)
     {
	if (lo == 0377) return -1;
	if (lo == 0376) return -2;
     }
   return lo + 256 * hi;
}

/*
 * The compiled file is created from the source file descriptions of the
 * terminals (see the -I option of infocmp) by using the terminfo compiler,
 * tic, and read by the routine setupterm [see curses(3X).]  The file is
 * divided into six parts in the following order:  the header, terminal
 * names, boolean flags, numbers, strings, and string table.
 * 
 * The header section begins the file.  This section contains six short
 * integers in the format described below.  These integers are (1) the magic
 * number (octal 0432); (2) the size, in bytes, of the names section; (3)
 * the number of bytes in the boolean section; (4) the number of short
 * integers in the numbers section; (5) the number of offsets (short
 * integers) in the strings section; (6) the size, in bytes, of the string
 * table.
 */

#define MAGIC 0432

/* In this structure, all char * fields are malloced EXCEPT if the 
 * structure is SLTERMCAP.  In that case, only terminal_names is malloced
 * and the other fields are pointers into it.
 */
typedef struct
{
#define SLTERMINFO 1
#define SLTERMCAP  2
   unsigned int flags;

   int name_section_size;
   char *terminal_names;
   
   int boolean_section_size;
   unsigned char *boolean_flags;
   
   int num_numbers;
   unsigned char *numbers;
   
   int num_string_offsets;
   unsigned char *string_offsets;
   
   int string_table_size;
   char *string_table;
   
} Terminfo_Type;

static char *tcap_getstr (char *, Terminfo_Type *);
static int tcap_getnum (char *, Terminfo_Type *);
static int tcap_getflag (char *, Terminfo_Type *);
static int tcap_getent (char *, Terminfo_Type *);

static FILE *open_terminfo (char *file, Terminfo_Type *h)
{
   FILE *fp;
   unsigned char buf[12];
   
   fp = fopen (file, "rb");
   if (fp == NULL) return NULL;
   
   if ((12 == fread ((char *) buf, 1, 12, fp) && (MAGIC == make_integer (buf))))
     {
	h->name_section_size = make_integer (buf + 2);
	h->boolean_section_size = make_integer (buf + 4);
	h->num_numbers = make_integer (buf + 6);
	h->num_string_offsets = make_integer (buf + 8);
	h->string_table_size = make_integer (buf + 10);
     }
   else 
     {
	fclose (fp);
	fp = NULL;
     }
   return fp;
}
   
/* 
 * The terminal names section comes next.  It contains the first line of the
 * terminfo description, listing the various names for the terminal,
 * separated by the bar ( | ) character (see term(5)).  The section is
 * terminated with an ASCII NUL character.
 */

/* returns pointer to malloced space */
static unsigned char *read_terminfo_section (FILE *fp, int size)
{
   char *s;
   
   if (NULL == (s = (char *) SLMALLOC (size))) return NULL;
   if (size != fread (s, 1, size, fp))
     {
	SLFREE (s);
	return NULL;
     }
   return (unsigned char *) s;
}

static char *read_terminal_names (FILE *fp, Terminfo_Type *t)
{
   return t->terminal_names = (char *) read_terminfo_section (fp, t->name_section_size);
}

/*
 * The boolean flags have one byte for each flag.  This byte is either 0 or
 * 1 as the flag is present or absent.  The value of 2 means that the flag
 * has been cancelled.  The capabilities are in the same order as the file
 * <term.h>.
 */

static unsigned char *read_boolean_flags (FILE *fp, Terminfo_Type *t)
{
   /* Between the boolean section and the number section, a null byte is
    * inserted, if necessary, to ensure that the number section begins on an
    * even byte offset. All short integers are aligned on a short word
    * boundary.
    */
   
   int size = (t->name_section_size + t->boolean_section_size) % 2;
   size += t->boolean_section_size;
   
   return t->boolean_flags = read_terminfo_section (fp, size);
}



/* 
 * The numbers section is similar to the boolean flags section.  Each
 * capability takes up two bytes, and is stored as a short integer.  If the
 * value represented is -1 or -2, the capability is taken to be missing.
 */

static unsigned char *read_numbers (FILE *fp, Terminfo_Type *t)
{
   return t->numbers = read_terminfo_section (fp, 2 * t->num_numbers);
}


/* The strings section is also similar.  Each capability is stored as a
 * short integer, in the format above.  A value of -1 or -2 means the
 * capability is missing.  Otherwise, the value is taken as an offset from
 * the beginning of the string table.  Special characters in ^X or \c
 * notation are stored in their interpreted form, not the printing
 * representation.  Padding information ($<nn>) and parameter information
 * (%x) are stored intact in uninterpreted form.
 */

static unsigned char *read_string_offsets (FILE *fp, Terminfo_Type *t)
{
   return t->string_offsets = (unsigned char *) read_terminfo_section (fp, 2 * t->num_string_offsets);
}


/* The final section is the string table.  It contains all the values of
 * string capabilities referenced in the string section.  Each string is
 * null terminated.
 */

static char *read_string_table (FILE *fp, Terminfo_Type *t)
{
   return t->string_table = (char *) read_terminfo_section (fp, t->string_table_size);
}


/*
 * Compiled terminfo(4) descriptions are placed under the directory
 * /usr/share/lib/terminfo.  In order to avoid a linear search of a huge
 * UNIX system directory, a two-level scheme is used:
 * /usr/share/lib/terminfo/c/name where name is the name of the terminal,
 * and c is the first character of name.  Thus, att4425 can be found in the
 * file /usr/share/lib/terminfo/a/att4425.  Synonyms for the same terminal
 * are implemented by multiple links to the same compiled file.
 */

#define MAX_TI_DIRS 4
static char *Terminfo_Dirs [MAX_TI_DIRS] = 
{
   NULL,
   "/usr/lib/terminfo",
   "/usr/share/lib/terminfo",
   "/usr/local/lib/terminfo"
};

char *SLtt_tigetent (char *term)
{
   char *tidir; 
   int i;
   FILE *fp = NULL;
   char file[256];
   Terminfo_Type *ti;

   if (term == NULL) return NULL;
   
   if (NULL == (ti = (Terminfo_Type *) SLMALLOC (sizeof (Terminfo_Type))))
     {
	return NULL;
     }
   
   /* If we are on a termcap based system, use termcap */
   if (0 == tcap_getent (term, ti)) return (char *) ti;
       
   Terminfo_Dirs[0] = getenv ("TERMINFO");
   i = 0;
   while (i < MAX_TI_DIRS)
     {
	tidir = Terminfo_Dirs[i];
	if (tidir != NULL)
	  {
	     sprintf (file, "%s/%c/%s", tidir, *term, term);
	     if (NULL != (fp = open_terminfo (file, ti))) break;
	  }
	i++;
     }
   
   if (fp != NULL) 
     {
	if (NULL != read_terminal_names (fp, ti))
	  {
	     if (NULL != read_boolean_flags (fp, ti))
	       {
		  if (NULL != read_numbers (fp, ti))
		    {
		       if (NULL != read_string_offsets (fp, ti))
			 {
			    if (NULL != read_string_table (fp, ti))
			      {
				 /* success */
				 fclose (fp);
				 ti->flags = SLTERMINFO;
				 return (char *) ti;
			      }
			    SLFREE (ti->string_offsets);
			 }
		       SLFREE (ti->numbers);
		    }
		  SLFREE (ti->boolean_flags);
	       }
	     SLFREE (ti->terminal_names);
	  }
	fclose (fp);
     }
   
   SLFREE (ti);
   return NULL;
}

typedef struct 
{
   char *name;
   int offset;
}
Tgetstr_Map_Type;
   
Tgetstr_Map_Type Tgetstr_Map [] =
{
   {"@7", 164},			       /* KEY End */
   {"AL", 110},			       /* parm_insert_line */
   {"DL", 106},			       /* parm_delete_line */
   {"RI", 112},			       /* parm_right_cursor */
   {"ac", 146},			       /* acs_chars */
   {"ae", 38},			       /* exit_alt_charset_mode */
   {"as", 25},			       /* enter_alt_charset_mode */
   {"ce", 6},			       /* clr_eol */
   {"cl", 5},			       /* clear_screen */
   {"cm", 10},			       /* cursor_address */
   {"cs", 3},			       /* change_scroll_region */
   {"dc", 21},			       /* delete_character */
   {"ei", 42},			       /* exit_insert_mode */
   {"eA", 155},			       /* enable alt char set */
   {"im", 31},			       /* enter_insert_mode */
   {"k0", 65},			       /* key_f0 */
   {"k1", 66},			       /* key_f1 */
   {"k2", 68},			       /* key_f2 */
   {"k3", 69},			       /* key_f3 */
   {"k4", 70},			       /* key_f4 */
   {"k5", 71},			       /* key_f5 */
   {"k6", 72},			       /* key_f6 */
   {"k7", 73},			       /* key_f7 */
   {"k8", 74},			       /* key_f8 */
   {"k9", 75},			       /* key_f9 */
   {"kA", 78},			       /* key_il */
   {"kC", 57},			       /* key_clear */
   {"kD", 59},			       /* key_dc */
   {"kE", 63},			       /* key_eol, */
   {"kF", 84},			       /* key_sf */
   {"kH", 80},			       /* key_ll */
   {"kI", 77},			       /* key_ic */
   {"kL", 60},			       /* key_dl */
   {"kM", 62},			       /* key_eic, */
   {"kN", 81},			       /* key_npage */
   {"kP", 82},			       /* key_ppage */
   {"kR", 85},			       /* key_sr */
   {"kS", 64},			       /* key_eos, */
   {"kT", 86},			       /* key_stab */
   {"ka", 56},			       /* key_catab */
   {"k;", 67},			       /* key_f10 */
   {"kb", 55},			       /* key_backspace */
   {"kd", 61},			       /* key_down */
   {"kh", 76},			       /* key_home */
   {"kl", 79},			       /* key_left */
   {"kr", 83},			       /* key_right */
   {"kt", 58},			       /* key_ctab */
   {"ku", 87},			       /* key_up */
   {"mb", 26},			       /* enter_blink_mode */
   {"md", 27},			       /* enter_bold_mode */
   {"me", 39},			       /* exit_attribute_mode */
   {"mr", 34},			       /* enter_reverse_mode */
   {"se", 43},			       /* exit_standout_mode */
   {"so", 35},			       /* enter_standout_mode */
   {"sr", 130},			       /* scroll_reverse */
   {"te", 40},			       /* end cursor addressing */
   {"ti", 28},			       /* begin cursor addressing */
   {"up", 19},			       /* cursor_up */
   {"us", 36},			       /* enter_underline_mode */
   {"vb", 45},			       /* flash_screen */
   {"vi", 13},			       /* make cursor invisible */
   {"vs", 20},			       /* make cursor visible */
   {"ke", 88},			       /* out of keypad transmit mode */
   {"ks", 89},			       /* keypad transmit mode */
   {"F1", 216},			       /* key_f11 */
   {"F2", 217},			       /* key_f12 */
   {"F3", 218},			       /* key_f13 */
   {"F4", 219},			       /* key_f14 */
   {"F5", 220},			       /* key_f15 */
   {"F6", 221},			       /* key_f16 */
   {"F7", 222},			       /* key_f17 */
   {"F8", 223},			       /* key_f18 */
   {"F9", 224},			       /* key_f19 */
   {"FA", 225},			       /* key_f20 */
   {"op", 297},                        /* original pair */
   {NULL, 0}
};

static int compute_cap_offset (char *cap, Terminfo_Type *t, Tgetstr_Map_Type *map, int max_ofs)
{
   char cha, chb;
   
   cha = *cap++; chb = *cap;
   
   while (map->name != NULL)
     {
	if ((cha == *map->name) && (chb == *(map->name + 1)))
	  {
	     if (map->offset >= max_ofs) return -1;
	     return map->offset;
	  }
	map++;
     }
   return -1;
}

   
char *SLtt_tigetstr (char *cap, char **pp)
{
   int offset;
   Terminfo_Type *t;
   
   if ((pp == NULL) || (NULL == (t = (Terminfo_Type *) *pp))) return NULL;
   
   if (t->flags == SLTERMCAP) return tcap_getstr (cap, t);
   
   offset = compute_cap_offset (cap, t, Tgetstr_Map, t->num_string_offsets);
   if (offset < 0) return NULL;
   offset = make_integer (t->string_offsets + 2 * offset);
   if (offset < 0) return NULL;
   return t->string_table + offset;
}

Tgetstr_Map_Type Tgetnum_Map[] =
{
   {"co", 0},			       /* columns */
   {"li", 2},			       /* lines */
   {NULL, -1}
};

int SLtt_tigetnum (char *cap, char **pp)
{
   int offset;
   Terminfo_Type *t;
   
   if ((pp == NULL) || (NULL == (t = (Terminfo_Type *) *pp))) return -1;

   if (t->flags == SLTERMCAP) return tcap_getnum (cap, t);

   offset = compute_cap_offset (cap, t, Tgetnum_Map, t->num_numbers);
   if (offset < 0) return -1;
   return make_integer (t->numbers + 2 * offset);
}

Tgetstr_Map_Type Tgetflag_Map[] =
{
   {"am", 1},			       /* auto_right_margin */
   {"ms", 14},			       /* move_standout_mode */
   {"xs", 3},			       /* ceol_standout_glitch */
   {"sg", 4},			       /* magic cookie glitch */
   {NULL, -1}
};

int SLtt_tigetflag (char *cap, char **pp)
{
   int offset;
   Terminfo_Type *t;
   
   if ((pp == NULL) || (NULL == (t = (Terminfo_Type *) *pp))) return -1;

   if (t->flags == SLTERMCAP) return tcap_getflag (cap, t);
   
   offset = compute_cap_offset (cap, t, Tgetflag_Map, t->boolean_section_size);
   
   if (offset < 0) return -1;
   return (int) *(t->boolean_flags + offset);
}



/* These are my termcap routines.  They only work with the TERMCAP environment
 * variable.  This variable must contain the termcap entry and NOT the file.
 */

static int tcap_getflag (char *cap, Terminfo_Type *t)
{
   char a, b;
   char *f = (char *) t->boolean_flags;
   char *fmax;
   
   if (f == NULL) return 0;
   fmax = f + t->boolean_section_size;

   a = *cap;
   b = *(cap + 1);
   while (f < fmax)
     {
	if ((a == f[0]) && (b == f[1]))
	  return 1;
	f += 2;
     }
   return 0;
}

static char *tcap_get_cap (unsigned char *cap, unsigned char *caps, int len)
{
   unsigned char c0, c1;
   unsigned char *caps_max;
   
   c0 = cap[0];
   c1 = cap[1];
   
   if (caps == NULL) return NULL;
   caps_max = caps + len;
   while (caps < caps_max)
     {
	if ((c0 == caps[0]) && (c1 == caps[1]))
	  {
	     return (char *) caps + 3;
	  }
	caps += (int) caps[2];
     }
   return NULL;
}

   
static int tcap_getnum (char *cap, Terminfo_Type *t)
{
   cap = tcap_get_cap ((unsigned char *) cap, t->numbers, t->num_numbers);
   if (cap == NULL) return -1;
   return atoi (cap);
}

static char *tcap_getstr (char *cap, Terminfo_Type *t)
{
   return tcap_get_cap ((unsigned char *) cap, (unsigned char *) t->string_table, t->string_table_size);
}

static int tcap_extract_field (unsigned char *t0)
{
   register unsigned char ch, *t = t0;
   while (((ch = *t) != 0) && (ch != ':')) t++;
   if (ch == ':') return (int) (t - t0);
   return -1;
}

int SLtt_try_termcap = 1;
static int tcap_getent (char *term, Terminfo_Type *ti)
{
   unsigned char *termcap, ch;
   unsigned char *buf, *b;
   unsigned char *t;
   int len;

   if (!SLtt_try_termcap == 0) return -1;
   
   termcap = (unsigned char *) getenv ("TERMCAP");
   if ((termcap == NULL) || (*termcap == '/')) return -1;
   
   /* We have a termcap so lets use it provided it does not have a reference 
    * to another terminal via tc=.  In that case, user terminfo.  The alternative
    * would be to parse the termcap file which I do not want to do right now.
    * Besides, this is a terminfo based system and if the termcap were parsed
    * terminfo would almost never get a chance to run.  In addition, the tc=
    * thing should not occur if tset is used to set the termcap entry.
    */
   t = termcap;
   while ((len = tcap_extract_field (t)) != -1)
     {
	if ((len > 3) && (t[0] == 't') && (t[1] == 'c') && (t[2] == '='))
	  return -1;
	t += (len + 1);
     }
   
   /* malloc some extra space just in case it is needed. */
   len = strlen ((char *) termcap) + 256;
   if (NULL == (buf = (unsigned char *) SLMALLOC (len))) return -1;

   b = buf;
   
   /* The beginning of the termcap entry contains the names of the entry.
    * It is terminated by a colon. 
    */
   
   ti->terminal_names = (char *) b;
   t = termcap;
   len = tcap_extract_field (t);
   if (len < 0)
     {
	SLFREE (buf);
	return -1;
     }
   strncpy ((char *) b, (char *) t, len);
   b[len] = 0;
   b += len + 1;
   ti->name_section_size = len;
   
   
   /* Now, we are really at the start of the termcap entries.  Point the 
    * termcap variable here since we want to refer to this a number of times.
    */
   termcap = t + (len + 1);
   
   
   /* Process strings first. */
   ti->string_table = (char *) b;
   t = termcap;
   while (-1 != (len = tcap_extract_field (t)))
     {
	unsigned char *b1;
	unsigned char *tmax;
	
	/* We are looking for: XX=something */
	if ((len < 4) || (t[2] != '=') || (*t == '.'))
	  {
	     t += len + 1;
	     continue;
	  }
	tmax = t + len;
	b1 = b;
	
	while (t < tmax)
	  {
	     ch = *t++;
	     if ((ch == '\\') && (t < tmax))
	       {
		  t = (unsigned char *) SLexpand_escaped_char ((char *) t, (char *) &ch);
	       }
	     else if ((ch == '^') && (t < tmax))
	       {
		  ch = *t++;
		  if (ch == '?') ch = 127;
		  else ch = (ch | 0x20) - ('a' - 1);
	       }
	     *b++ = ch;
	  }
	/* Null terminate it. */
	*b++ = 0;
	len = (int) (b - b1);
	b1[2] = (unsigned char) len;    /* replace the = by the length */
	/* skip colon to next field. */
	t++;
     }   
   ti->string_table_size = (int) (b - (unsigned char *) ti->string_table);

   /* Now process the numbers. */

   t = termcap;
   ti->numbers = b;
   while (-1 != (len = tcap_extract_field (t)))
     {
	unsigned char *b1;
	unsigned char *tmax;
	
	/* We are looking for: XX#NUMBER */
	if ((len < 4) || (t[2] != '#') || (*t == '.'))
	  {
	     t += len + 1;
	     continue;
	  }
	tmax = t + len;
	b1 = b;
	
	while (t < tmax)
	  {
	     *b++ = *t++;
	  }
	/* Null terminate it. */
	*b++ = 0;
	len = (int) (b - b1);
	b1[2] = (unsigned char) len;    /* replace the # by the length */
	t++;
     }   
   ti->num_numbers = (int) (b - ti->numbers);
   
   /* Now process the flags. */
   t = termcap;
   ti->boolean_flags = b;
   while (-1 != (len = tcap_extract_field (t)))
     {
	/* We are looking for: XX#NUMBER */
	if ((len != 2) || (*t == '.') || (*t <= ' ')) 
	  {
	     t += len + 1;
	     continue;
	  }
	b[0] = t[0];
	b[1] = t[1];
	t += 3;
	b += 2;
     }   
   ti->boolean_section_size = (int) (b - ti->boolean_flags);
   ti->flags = SLTERMCAP;
   return 0;
}

#else /* USE_SETUPTERM */

/* Ching Hui fixes so that it will work on AIX and OSF/1 */
#include <curses.h>
#include <term.h>

int SLtt_try_termcap = 1;

char *SLtt_tigetent (char *term)
{
    int rc;

    setupterm(term, 1, &rc);
    if (rc != 1)
	return NULL;
    return (char *)cur_term;
}

#define MATCH_CHAR(c, variable) \
    do { \
	 if (*(cap + 1) == c) \
	     return variable; \
     } while (0)

char *SLtt_tigetstr (char *cap, char **pp)
{
    if ((pp == NULL) || ((cur_term = (struct term *) *pp) == NULL))
	return NULL;

    switch(*cap) {
    case '@':
	MATCH_CHAR('7', key_end);
	break;
    case 'A':
	MATCH_CHAR('A', parm_insert_line);
	break;
    case 'D':
	MATCH_CHAR('L', parm_delete_line);
	break;
    case 'R':
	MATCH_CHAR('I', parm_right_cursor);
	break;
    case 'a':
#ifdef  acs_chars
	MATCH_CHAR('c', acs_chars);
#elif defined (box_chars_1)
	MATCH_CHAR('c', box_chars_1); /* AIX hack */
#else
	MATCH_CHAR('c', NULL);
#endif
	MATCH_CHAR('e', exit_alt_charset_mode);
	MATCH_CHAR('s', enter_alt_charset_mode);
	break;
    case 'c':
	MATCH_CHAR('e', clr_eol);
	MATCH_CHAR('l', clear_screen);
	MATCH_CHAR('m', cursor_address);
	MATCH_CHAR('s', change_scroll_region);
	break;
    case 'd':
	MATCH_CHAR('c', delete_character);
	break;
    case 'e':
	MATCH_CHAR('i', exit_insert_mode);
#ifdef ena_acs
	MATCH_CHAR('A', ena_acs); /* aix hack */
#else
	MATCH_CHAR('A', NULL);
#endif
	break;
    case 'i':
	MATCH_CHAR('m', enter_insert_mode);
	break;
    case 'k':
	MATCH_CHAR('0', key_f0);
	MATCH_CHAR('1', key_f1);
	MATCH_CHAR('1', key_f1);
	MATCH_CHAR('2', key_f2);
	MATCH_CHAR('3', key_f3);
	MATCH_CHAR('4', key_f4);
	MATCH_CHAR('5', key_f5);
	MATCH_CHAR('6', key_f6);
	MATCH_CHAR('7', key_f7);
	MATCH_CHAR('8', key_f8);
	MATCH_CHAR('9', key_f9);
	MATCH_CHAR('A', key_il);
	MATCH_CHAR('C', key_clear);
	MATCH_CHAR('D', key_dc);
	MATCH_CHAR('E', key_eol);
	MATCH_CHAR('F', key_sf);
	MATCH_CHAR('H', key_ll);
	MATCH_CHAR('I', key_ic);
	MATCH_CHAR('L', key_dl);
	MATCH_CHAR('M', key_eic);
	MATCH_CHAR('N', key_npage);
	MATCH_CHAR('P', key_ppage);
	MATCH_CHAR('R', key_sr);
	MATCH_CHAR('S', key_eos);
	MATCH_CHAR('T', key_stab);
	MATCH_CHAR('a', key_catab);
	MATCH_CHAR(';', key_f10);
	MATCH_CHAR('b', key_backspace);
	MATCH_CHAR('d', key_down);
	MATCH_CHAR('e', keypad_local);
	MATCH_CHAR('h', key_home);
	MATCH_CHAR('l', key_left);
	MATCH_CHAR('r', key_right);
	MATCH_CHAR('s', keypad_xmit);
	MATCH_CHAR('t', key_ctab);
	MATCH_CHAR('u', key_up);
	break;
    case 'm':
	MATCH_CHAR('b', enter_blink_mode);
	MATCH_CHAR('d', enter_bold_mode);
	MATCH_CHAR('e', exit_attribute_mode);
	MATCH_CHAR('r', enter_reverse_mode);
	break;
    case 's':
	MATCH_CHAR('e', exit_standout_mode);
	MATCH_CHAR('o', enter_standout_mode);
	MATCH_CHAR('r', scroll_reverse);
	break;
    case 't':
	MATCH_CHAR('e', exit_ca_mode);
	MATCH_CHAR('i', enter_ca_mode);
	break;
    case 'u':
	MATCH_CHAR('p', cursor_up);
	MATCH_CHAR('s', enter_underline_mode);
	break;
    case 'v':
	MATCH_CHAR('b', flash_screen);
	MATCH_CHAR('i', cursor_invisible);
	MATCH_CHAR('s', cursor_visible);
	break;
    case 'F':
	MATCH_CHAR('1', key_f11);
	MATCH_CHAR('2', key_f12);
	MATCH_CHAR('3', key_f13);
	MATCH_CHAR('4', key_f14);
	MATCH_CHAR('5', key_f15);
	MATCH_CHAR('6', key_f16);
	MATCH_CHAR('7', key_f17);
	MATCH_CHAR('8', key_f18);
	MATCH_CHAR('9', key_f19);
	MATCH_CHAR('A', key_f20);
	break;
#ifdef orig_pair
    case 'o':
	MATCH_CHAR('p', orig_pair);
	break;
#endif
    }
    return NULL;
}

int SLtt_tigetnum (char *cap, char **pp)
{
    if ((pp == NULL) || ((cur_term = (struct term *) *pp) == NULL))
	return NULL;
    switch(*cap) {
    case 'c':
	MATCH_CHAR('o', columns);
	break;
    case 'l':
	MATCH_CHAR('i', lines);
	break;
    }
    return -1;
}

int SLtt_tigetflag (char *cap, char **pp)
{
    if ((pp == NULL) || ((cur_term = (struct term *) *pp) == NULL))
	return NULL;
    switch(*cap) {
    case 'a':
	MATCH_CHAR('m', auto_right_margin);
	break;
    case 'm':
	MATCH_CHAR('s', move_standout_mode);
	break;
    case 'x':
	MATCH_CHAR('s', ceol_standout_glitch);
	break;
    case 's':
	MATCH_CHAR('g', magic_cookie_glitch);
	break;
    }
    return -1;
}

#endif /* !USE_SETUPTERM */
