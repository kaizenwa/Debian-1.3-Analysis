/* SLang Screen management routines */
/* Copyright (c) 1992, 1995 John E. Davis
 * All rights reserved.
 * 
 * You may distribute under the terms of either the GNU General Public
 * License or the Perl Artistic License.
 */
#include <config.h>
#include <stdio.h>
#include <string.h>

#include "config.h"
#include "slang.h"
#include "_slang.h"

typedef struct Screen_Type
  {
     int n;                    /* number of chars written last time */
     int flags;                /* line untouched, etc... */
     SLang_Char_Type *old, *neew;
#ifndef pc_system
     unsigned long old_hash, new_hash;
#endif
  }
Screen_Type;

#ifdef linux_unicode
struct _uniconv
  {
    unsigned char acs;
    unsigned short uni;
  }
UniConv_Table [] = {
{ SLSMG_HLINE_CHAR, SLUNI_HLINE_CHAR },
{ SLSMG_VLINE_CHAR, SLUNI_VLINE_CHAR },
{ SLSMG_ULCORN_CHAR, SLUNI_ULCORN_CHAR },
{ SLSMG_URCORN_CHAR, SLUNI_URCORN_CHAR },
{ SLSMG_LLCORN_CHAR, SLUNI_LLCORN_CHAR },
{ SLSMG_LRCORN_CHAR, SLUNI_LRCORN_CHAR },
{ SLSMG_CKBRD_CHAR, SLUNI_CKBRD_CHAR },
{ SLSMG_RTEE_CHAR, SLUNI_RTEE_CHAR },
{ SLSMG_LTEE_CHAR, SLUNI_LTEE_CHAR },
{ SLSMG_UTEE_CHAR, SLUNI_UTEE_CHAR },
{ SLSMG_DTEE_CHAR, SLUNI_DTEE_CHAR },
{ SLSMG_CROSS_CHAR, SLUNI_CROSS_CHAR }
};
#define colorshift 16
#else
#define colorshift 8
#endif

#define TOUCHED 0x1
#define TRASHED 0x2

#ifndef pc_system
#define MAX_SCREEN_SIZE 120
#else
#define MAX_SCREEN_SIZE 75
#endif

Screen_Type SL_Screen[MAX_SCREEN_SIZE];
static int Start_Col, Start_Row;
static int Screen_Cols, Screen_Rows;
static int This_Row, This_Col;
static int This_Color;		       /* only the first 8 bits of this
					* are used.  The highest bit is used
					* to indicate an alternate character
					* set.  This leaves 127 userdefineable
					* color combination.
					*/

#ifndef pc_system
#define ALT_CHAR_FLAG 0x80
#else
#define ALT_CHAR_FLAG 0x00
#endif


static void blank_line (SLang_Char_Type *p, int n, unsigned char ch)
{
   register SLang_Char_Type *pmax = p + n;
   
   while (p < pmax)
     {
	*p = (This_Color << colorshift) | (SLang_Char_Type) ch;
	p++;
     }
}


static void clear_region (int row, int n)
{
   int i;
   int imax = row + n;
   
   if (imax > Screen_Rows) imax = Screen_Rows;
   for (i = row; i < imax; i++)
     {
	if (i >= 0)
	  {
	     blank_line (SL_Screen[i].neew, Screen_Cols, ' ');
	     SL_Screen[i].flags |= TOUCHED;
	  }
     }
}

void SLsmg_erase_eol (void)
{
   int c = This_Col;
   if ((This_Row < 0) || (This_Row >= Screen_Rows)) return;
   if (c < 0) c = 0; else if (c >= Screen_Cols) return;
   blank_line (SL_Screen[This_Row].neew + c , Screen_Cols - c, ' ');
   SL_Screen[This_Row].flags |= TOUCHED;
}

void SLsmg_gotorc (int r, int c)
{
   This_Row = r;
   This_Col = c;
}

int SLsmg_get_row (void)
{
   return This_Row;
}

int SLsmg_get_column (void)
{
   return This_Col;
}

void SLsmg_erase_eos (void)
{
   SLsmg_erase_eol ();
   clear_region (This_Row + 1, Screen_Rows);
}

static int This_Alt_Char;

#ifndef pc_system
void SLsmg_set_char_set (int i)
{
#ifdef linux_unicode
   if (SLtt_Unicode) return; /* char set has no meaning :) */
#endif   
   if (SLtt_Use_Blink_For_ACS) return; /* alt chars not used and the alt bit
					* is used to indicate a blink.
					*/
   if (i) This_Alt_Char = ALT_CHAR_FLAG;
   else This_Alt_Char = 0;
   This_Color &= 0x7F;
   This_Color |= This_Alt_Char;
}
#endif

void SLsmg_set_color (int color)
{
   if (color < 0) return;
   This_Color = color | This_Alt_Char;
}


void SLsmg_reverse_video (void)
{
   SLsmg_set_color (1);
}


void SLsmg_normal_video (void)
{
   This_Color = This_Alt_Char;	       /* reset video but NOT char set. */
}


static int point_visible (int col_too)
{
   return ((This_Row >= Start_Row) && (This_Row < Start_Row + Screen_Rows)
	   && ((col_too == 0)
	       || ((This_Col >= Start_Col) 
		   && (This_Col < Start_Col + Screen_Cols))));
}
   
void SLsmg_printf (char *fmt, ...)
{
   char p[1000];
   va_list ap;

   va_start(ap, fmt);
   (void) vsprintf(p, fmt, ap);
   va_end(ap);
   
   SLsmg_write_string (p);
}

void SLsmg_write_string (char *str)
{
   SLsmg_write_nchars (str, strlen (str));
}

void SLsmg_write_nstring (char *str, int n)
{
   int width;
   char blank = ' ';
   if (str == NULL) width = 0;
   else 
     {
	width = strlen (str);
	if (width > n) width = n;
	SLsmg_write_nchars (str, width);
     }
   while (width++ < n) SLsmg_write_nchars (&blank, 1);
}

void SLsmg_write_wrapped_string (char *s, int r, int c, int dr, int dc, int fill)
{
   register char ch, *p;
   int maxc = dc;
   
   if ((dr == 0) || (dc == 0)) return;
   p = s;
   dc = 0;
   while (1)
     {
	ch = *p++;
	if ((ch == 0) || (ch == '\n'))
	  {
	     int diff;
	     
	     diff = maxc - dc;
	     
	     SLsmg_gotorc (r, c);
	     SLsmg_write_nchars (s, dc);
	     if (fill && (diff > 0))
	       {
		  while (diff--) SLsmg_write_char (' ');
	       }
	     if ((ch == 0) || (dr == 1)) break;
	     
	     r++;
	     dc = 0;
	     dr--;
	     s = p;
	  }
	else if (dc == maxc)
	  {
	     SLsmg_gotorc (r, c);
	     SLsmg_write_nchars (s, dc + 1);
	     if (dr == 1) break;

	     r++;
	     dc = 0;
	     dr--;
	     s = p;
	  }
	else dc++;
     }
}

   

int SLsmg_Tab_Width = 8;

/* Minimum value for which eight bit char is displayed as is. */

#ifndef pc_system
int SLsmg_Display_Eight_Bit = 160;
static unsigned char Alt_Char_Set[129];/* 129th is used as a flag */
#else
int SLsmg_Display_Eight_Bit = 128;
#endif

void SLsmg_write_nchars (char *str, int n)
{
   register SLang_Char_Type *p, old, neew;
   unsigned char ch;
   unsigned int flags;
   int len, start_len = Start_Col, max_len;
   char *str_max = str + n;
   
   if (point_visible (0) == 0) return;
   
   len = This_Col;
   max_len = start_len + Screen_Cols;

   p = SL_Screen[This_Row].neew;
   if (len > start_len) p += (len - start_len);
   
   flags = SL_Screen[This_Row].flags;
   while ((len < max_len) && (str < str_max))
     {	
	ch = (unsigned char) *str++;
#ifndef pc_system
#ifdef linux_unicode
        if (!SLtt_Unicode)
#endif
	  if (This_Alt_Char)
	    {
	       ch = Alt_Char_Set[ch & 0x7F];
	    }
#endif
	if (((ch >= ' ') && (ch < 127))
	    || (ch >= (unsigned char) SLsmg_Display_Eight_Bit))
	  {
	     len += 1;
	     if (len > start_len)
	       {
		  old = *p;
		  neew = (This_Color << colorshift) | (SLang_Char_Type) ch;
		  if (old != neew)
		    {
		       flags |= TOUCHED;
		       *p = neew;
		    }
		  p++;
	       }
	  }
	
	else if ((ch == '\t') && (SLsmg_Tab_Width > 0))
	  {
	     n = len;
	     n += SLsmg_Tab_Width;
	     n = SLsmg_Tab_Width - (n % SLsmg_Tab_Width);
	     if (len + n > max_len) n = max_len - len;
	     neew = (This_Color << colorshift) | (SLang_Char_Type) ' ';
	     while (n--)
	       {
		  len += 1;
		  if (len > start_len)
		    {
		       if (*p != neew) 
			 {
			    flags |= TOUCHED;
			    *p = neew;
			 }
		       p++;
		    }
	       }
	  }
	else if (ch == '\n') break;
	else
	  {
	     if (ch & 0x80)
	       {
		  neew = (This_Color << colorshift) | (SLang_Char_Type) '~';
		  len += 1;
		  if (len > start_len)
		    {
		       if (*p != neew)
			 {
			    *p = neew;
			    flags |= TOUCHED;
			 }
		       p++;
		       if (len == max_len) break;
		       ch &= 0x7F;
		    }
	       }
	     
	     len += 1;
	     if (len > start_len)
	       {
		  neew = (This_Color << colorshift) | (SLang_Char_Type) '^';
		  if (*p != neew)
		    {
		       *p = neew;
		       flags |= TOUCHED;
		    }
		  p++;
		  if (len == max_len) break;
	       }
	     
	     if (ch == 127) ch = '?'; else ch = ch + '@';
	     len++;
	     if (len > start_len)
	       {
		  neew = (This_Color << colorshift) | (SLang_Char_Type) ch;
		  if (*p != neew)
		    {
		       *p = neew;
		       flags |= TOUCHED;
		    }
		  p++;
	       }
	  }
     }
   
   SL_Screen[This_Row].flags = flags;
   This_Col = len;
}

void SLsmg_write_unicode (unsigned short ch)
{
#ifdef linux_unicode
   register SLang_Char_Type *p, old, neew;
   
   if (ch < 0x100) 
     {
        SLsmg_write_nchars ((char *) &ch, 1);
        return;
     }
   
   if (point_visible (0) == 0) return;
   
   p = SL_Screen[This_Row].neew;
   if (This_Col > Start_Col) p += (This_Col - Start_Col);
   if (This_Col >= Start_Col + Screen_Cols)
        return;
   This_Col++;
   if (This_Col > Start_Col)
     {
	old = *p;
	neew = (This_Color << colorshift) | (SLang_Char_Type) ch;
	if (old != neew)
	  {
	     SL_Screen[This_Row].flags |= TOUCHED;
	     *p = neew;
	  }
     }	
#endif
}

void SLsmg_write_char (char ch)
{
   SLsmg_write_nchars (&ch, 1);
}

static int Cls_Flag;


void SLsmg_cls (void)
{
   This_Color = This_Alt_Char;
   clear_region (0, Screen_Rows);
   Cls_Flag = 1;
}

static void do_copy (SLang_Char_Type *a, SLang_Char_Type *b)
{
   SLang_Char_Type *amax = a + Screen_Cols;
   
   while (a < amax) *a++ = *b++;
}

#ifndef pc_system
int SLsmg_Scroll_Hash_Border = 0;
static unsigned long compute_hash (SLang_Char_Type *s, int n)
{
   register unsigned long h = 0, g;
   register unsigned long sum = 0;
   register SLang_Char_Type *smax, ch;
   int is_blank = 2;
   
   s += SLsmg_Scroll_Hash_Border;
   smax = s + (n - SLsmg_Scroll_Hash_Border);
   while (s < smax) 
     {
	ch = *s++;
#ifdef linux_unicode
	if (is_blank && ((ch & 0xFFFF) != 32)) is_blank--;
#else	
	if (is_blank && ((ch & 0xFF) != 32)) is_blank--;
#endif	
	
	sum += ch & 0xFFFF;
	
	h = sum + (h << 3);
	if ((g = h & 0xE0000000L) != 0)
	  {
	     h = h ^ (g >> 24);
	     h = h ^ g;
	  }
#ifdef linux_unicode	  
	sum += (ch >> 16) & 0xFFFF;
	
	h = sum + (h << 3);
	if ((g = h & 0xE0000000L) != 0)
	  {
	     h = h ^ (g >> 24);
	     h = h ^ g;
	  }
#endif	  
     }
   if (is_blank) return 0;
   return h;
}

unsigned long Blank_Hash;

static void try_scroll (void)
{
   int i, j, di, r1, r2, rmin, rmax;
   unsigned long hash;
   int color, did_scroll = 0;
   SLang_Char_Type *tmp;
   int ignore;
   
   /* find region limits */
   for (rmax = Screen_Rows - 1; rmax > 0; rmax--)
     {
	if (SL_Screen[rmax].new_hash != SL_Screen[rmax].old_hash)
	  break;
     }
   
   for (rmin = 0; rmin < rmax; rmin++)
     {
	if (SL_Screen[rmin].new_hash != SL_Screen[rmin].old_hash)
	  break;
     }
   
   for (i = rmax; i > rmin; i--)
     {
	hash = SL_Screen[i].new_hash;
	if (hash == Blank_Hash) continue;
	
	if ((hash == SL_Screen[i].old_hash)
	    || ((i + 1 < Screen_Rows) && (hash == SL_Screen[i + 1].old_hash))
	    || ((i - 1 > rmin) && (SL_Screen[i].old_hash == SL_Screen[i - 1].new_hash)))
	  continue;
	
	for (j = i - 1; j >= rmin; j--)
	  {
	     if (hash == SL_Screen[j].old_hash) break;
	  }
	if (j < rmin) continue;
	
	r2 = i;			       /* end scroll region */
	
	di = i - j;
	j--;
	ignore = 0;
	while ((j >= rmin) && (SL_Screen[j].old_hash == SL_Screen[j + di].new_hash))
	  {
	     if (SL_Screen[j].old_hash == Blank_Hash) ignore++;
	     j--;
	  }
	r1 = j + 1;
	
	/* If this scroll only scrolls this line into place, don't do it.
	 */
	if ((di > 1) && (r1 + di + ignore == r2)) continue;
	
	/* If there is anything in the scrolling region that is ok, abort the 
	 * scroll.
	 */

	for (j = r1; j <= r2; j++)
	  {
	     if ((SL_Screen[j].old_hash != Blank_Hash)
		 && (SL_Screen[j].old_hash == SL_Screen[j].new_hash))
	       {
		  /* See if the scroll is happens to scroll this one into place. */
		  if ((j + di > r2) || (SL_Screen[j].old_hash != SL_Screen[j + di].new_hash))
		    break;
	       }
	  }
	if (j <= r2) continue;
	
	color = This_Color;  This_Color = 0;
	did_scroll = 1;
	SLtt_normal_video ();
	SLtt_set_scroll_region (r1, r2);
	SLtt_goto_rc (0, 0);
	SLtt_reverse_index (di);
	SLtt_reset_scroll_region ();
	/* Now we have a hole in the screen.  Make the virtual screen look 
	 * like it.
	 */
	for (j = r1; j <= r2; j++) SL_Screen[j].flags = TOUCHED;
	
	while (di--)
	  {
	     tmp = SL_Screen[r2].old;
	     for (j = r2; j > r1; j--)
	       {
		  SL_Screen[j].old = SL_Screen[j - 1].old;
		  SL_Screen[j].old_hash = SL_Screen[j - 1].old_hash;
	       }
	     SL_Screen[r1].old = tmp;
	     blank_line (SL_Screen[r1].old, Screen_Cols, ' ');
	     SL_Screen[r1].old_hash = Blank_Hash;
	     r1++;
	  }
	This_Color = color;
     }
   if (did_scroll) return;
   
   /* Try other direction */

   for (i = rmin; i < rmax; i++)
     {
	hash = SL_Screen[i].new_hash;
	if (hash == Blank_Hash) continue;
	if (hash == SL_Screen[i].old_hash) continue;
	
	/* find a match further down screen */
	for (j = i + 1; j <= rmax; j++)
	  {
	     if (hash == SL_Screen[j].old_hash) break;
	  }
	if (j > rmax) continue;
	
	r1 = i;			       /* beg scroll region */
	di = j - i;		       /* number of lines to scroll */
	j++;			       /* since we know this is a match */
	
	/* find end of scroll region */
	ignore = 0;
	while ((j <= rmax) && (SL_Screen[j].old_hash == SL_Screen[j - di].new_hash))
	  {
	     if (SL_Screen[j].old_hash == Blank_Hash) ignore++;
	     j++;
	  }
	r2 = j - 1;		       /* end of scroll region */
	
	/* If this scroll only scrolls this line into place, don't do it.
	 */
	if ((di > 1) && (r1 + di + ignore == r2)) continue;

	/* If there is anything in the scrolling region that is ok, abort the 
	 * scroll.
	 */
	
	for (j = r1; j <= r2; j++)
	  {
	     if ((SL_Screen[j].old_hash != Blank_Hash)
		 && (SL_Screen[j].old_hash == SL_Screen[j].new_hash))
	       {
		  if ((j - di < r1) || (SL_Screen[j].old_hash != SL_Screen[j - di].new_hash))
		    break;
	       }
	     
	  }
	if (j <= r2) continue;
	
	color = This_Color;  This_Color = 0;
	SLtt_normal_video ();
	SLtt_set_scroll_region (r1, r2);
	SLtt_goto_rc (0, 0);	       /* relative to scroll region */
	SLtt_delete_nlines (di);
	SLtt_reset_scroll_region ();
	/* Now we have a hole in the screen.  Make the virtual screen look 
	 * like it.
	 */
	for (j = r1; j <= r2; j++) SL_Screen[j].flags = TOUCHED;
	
	while (di--)
	  {
	     tmp = SL_Screen[r1].old;
	     for (j = r1; j < r2; j++)
	       {
		  SL_Screen[j].old = SL_Screen[j + 1].old;
		  SL_Screen[j].old_hash = SL_Screen[j + 1].old_hash;
	       }
	     SL_Screen[r2].old = tmp;
	     blank_line (SL_Screen[r2].old, Screen_Cols, ' ');
	     SL_Screen[r2].old_hash = Blank_Hash;
	     r2--;
	  }
	This_Color = color;
     }
}

#endif   /* NOT pc_system */
        
	
	
static int Smg_Inited;

void SLsmg_refresh (void)
{
   int i;
   
   if (Smg_Inited == 0) return;
#ifndef pc_system
   for (i = 0; i < Screen_Rows; i++)
     {
	if (SL_Screen[i].flags == 0) continue;
	SL_Screen[i].new_hash = compute_hash (SL_Screen[i].neew, Screen_Cols);
     }
#endif
   
   if (Cls_Flag) 
     {
	SLtt_normal_video ();  SLtt_cls ();
     }
#ifndef pc_system
   else if (SLtt_Term_Cannot_Scroll == 0) try_scroll ();
#endif

   for (i = 0; i < Screen_Rows; i++)
     {
	int trashed;
	
	if (SL_Screen[i].flags == 0) continue;
	
	if (SL_Screen[i].flags & TRASHED)
	  {
	     SLtt_goto_rc (i, -1); /* Force cursor to move */
	     SLtt_goto_rc (i, 0);
	     if (Cls_Flag == 0) SLtt_del_eol ();
	     trashed = 1;
	  }
	else trashed = 0;
	
	if (Cls_Flag || trashed) 
	  {
	     int color = This_Color;
	     This_Color = 0;
	     blank_line (SL_Screen[i].old, Screen_Cols, ' ');
	     This_Color = color;
	  }
	
	SL_Screen[i].old[Screen_Cols] = 0;
	SL_Screen[i].neew[Screen_Cols] = 0;
	
	SLtt_smart_puts (SL_Screen[i].neew, SL_Screen[i].old, Screen_Cols, i);
	do_copy (SL_Screen[i].old, SL_Screen[i].neew);
	SL_Screen[i].flags = 0;
#ifndef pc_system
	SL_Screen[i].old_hash = SL_Screen[i].new_hash;
#endif
     }
   
   if (point_visible (1)) SLtt_goto_rc (This_Row - Start_Row, This_Col - Start_Col);
   SLtt_flush_output ();
   Cls_Flag = 0;
}

static int compute_clip (int row, int n, int box_start, int box_end,
			 int *rmin, int *rmax)
{
   int row_max;
   
   if (n < 0) return 0;
   if (row >= box_end) return 0;
   row_max = row + n;
   if (row_max <= box_start) return 0;
   
   if (row < box_start) row = box_start;
   if (row_max >= box_end) row_max = box_end;
   *rmin = row;
   *rmax = row_max;
   return 1;
}

void SLsmg_touch_lines (int row, int n)
{
   int i;
   int r1, r2;
   
   if (0 == compute_clip (row, n, Start_Row, Start_Row + Screen_Rows, &r1, &r2))
     return;
   
   r1 -= Start_Row;
   r2 -= Start_Row;
   for (i = r1; i < r2; i++)
     {
	SL_Screen[i].flags |= TRASHED;
     }
}

#ifndef pc_system
static char Fake_Alt_Char_Pairs [] = "a:j+k+l+m+q-t+u+v+w+x|";

static void init_alt_char_set (void)
{
   int i = 0;
   unsigned char *p, *pmax, ch;
   
   if (Alt_Char_Set[128] == 128) return;

   while (i <= 128) 
     {
	Alt_Char_Set [i] = i;
	i++;
     }
   
   /* Map to VT100 */
   if (SLtt_Has_Alt_Charset)
     {
	p = (unsigned char *) SLtt_Graphics_Char_Pairs;
	if (p == NULL) return;
     }
   else	p = (unsigned char *) Fake_Alt_Char_Pairs;
   pmax = p + strlen ((char *) p);
   
   /* Some systems have messed up entries for this */
   while (p < pmax)
     {
	ch = *p++;
	ch &= 0x7F;		       /* should be unnecessary */
	Alt_Char_Set [ch] = *p;
	p++;
     }
}
#endif

void SLsmg_suspend_smg (void)
{
   SLtt_reset_video ();
}

void SLsmg_resume_smg (void)
{
   int i;
   SLtt_init_video ();
   Cls_Flag = 1;
   for (i = 0; i < Screen_Rows; i++)
     SL_Screen[i].flags |= TRASHED;
   SLsmg_refresh ();
}


int SLsmg_init_smg (void)
{
   int i, len;
   SLang_Char_Type *old, *neew;
   
   if (Smg_Inited) SLsmg_reset_smg ();
   SLtt_init_video ();
   Screen_Cols = SLtt_Screen_Cols;
   Screen_Rows = SLtt_Screen_Rows;
   This_Col = This_Row = Start_Col = Start_Row = 0;

   This_Color = 0;
   This_Alt_Char = 0;
   Cls_Flag = 1;
#ifndef pc_system
   init_alt_char_set ();
#endif
   len = Screen_Cols + 3;
   for (i = 0; i < Screen_Rows; i++)
     {
	if ((NULL == (old = (SLang_Char_Type *) SLMALLOC (sizeof(SLang_Char_Type) * len)))
	    || ((NULL == (neew = (SLang_Char_Type *) SLMALLOC (sizeof(SLang_Char_Type) * len)))))
	  {
	     SLang_Error = SL_MALLOC_ERROR;
	     return 0;
	  }
	blank_line (old, len, ' ');
	blank_line (neew, len, ' ');
	SL_Screen[i].old = old;
	SL_Screen[i].neew = neew;
	SL_Screen[i].flags = 0;
#ifndef pc_system
	Blank_Hash = compute_hash (old, Screen_Cols);
	SL_Screen[i].new_hash = SL_Screen[i].old_hash =  Blank_Hash;
#endif
     }
   Smg_Inited = 1;
   return 1;
}


void SLsmg_reset_smg (void)
{
   int i;
   
   if (Smg_Inited == 0) return;
   for (i = 0; i < Screen_Rows; i++)
     {
	if (SL_Screen[i].old != NULL) SLFREE (SL_Screen[i].old);
	if (SL_Screen[i].neew != NULL) SLFREE (SL_Screen[i].neew);
	SL_Screen[i].old = SL_Screen[i].neew = NULL;
     }
   SLtt_reset_video ();
   This_Alt_Char = This_Color = 0;
   Smg_Inited = 0;
}


SLang_Char_Type SLsmg_char_at (void)
{
   if (point_visible (1))
     {
	return SL_Screen[This_Row - Start_Row].neew[This_Col - Start_Col];
     }
   return 0;
}

void SLsmg_vprintf (char *fmt, va_list ap)
{
   char p[1000];
   
   (void) vsprintf(p, fmt, ap);
   
   SLsmg_write_string (p);
}

void SLsmg_set_screen_start (int *r, int *c)
{
   int or = Start_Row, oc = Start_Col;
   
   if (c == NULL) Start_Col = 0;
   else
     {
	Start_Col = *c;
	*c = oc;
     }
   if (r == NULL) Start_Row = 0;
   else
     {
	Start_Row = *r;
	*r = or;
     }
}

void SLsmg_draw_object (int r, int c, unsigned char object)
{
   This_Row = r;  This_Col = c;

#ifdef linux_unicode
   if (SLtt_Unicode)
     {
        int i;
        unsigned short ch = 0xfffd;
        
        for (i = 0; i < sizeof (UniConv_Table) / sizeof (UniConv_Table [0]);
            i++)
          if (object == UniConv_Table [i].acs)
            {
               ch = UniConv_Table [i].uni;
               break;
            }
        if (point_visible (1))
            SLsmg_write_unicode (SLtt_fix_unicode (ch));
        This_Col = c + 1;
        return;
     }
#endif
   if (point_visible (1))
     {
	int tac = This_Alt_Char;
	This_Alt_Char = ALT_CHAR_FLAG;
	This_Color |= This_Alt_Char;
	SLsmg_write_char (object);
	This_Alt_Char = tac;
	This_Color = (This_Color & 0x7F) | This_Alt_Char;
     }

   This_Col = c + 1;
}

void SLsmg_draw_unicode (int r, int c, unsigned short object)
{
#ifdef linux_unicode
    This_Row = r; This_Col = c;
    
    SLsmg_write_unicode (SLtt_fix_unicode (object));
    This_Col = c + 1;
#endif
}

void SLsmg_draw_hline (int n)
{
   static unsigned char hbuf[16];
   int count;
   int cmin, cmax;
   int final_col = This_Col + n;
   int tac = This_Alt_Char;
   
   if ((This_Row < Start_Row) || (This_Row >= Start_Row + Screen_Rows) 
       || (0 == compute_clip (This_Col, n, Start_Col, Start_Col + Screen_Cols,
			      &cmin, &cmax)))
     {
	This_Col = final_col;
	return;
     }

   n = cmax - cmin;
   This_Col = cmin;
   
#ifdef linux_unicode   
   if (SLtt_Unicode)
     {
        unsigned short ch = SLtt_fix_unicode (SLUNI_HLINE_CHAR);
        while (n-- > 0)
           SLsmg_write_unicode (ch);
        This_Col = final_col;
        return;
     }
#endif     
   if (hbuf[0] == 0)
     {
	MEMSET ((char *) hbuf, SLSMG_HLINE_CHAR, 16);
     }
   
   count = n / 16;
   This_Alt_Char = ALT_CHAR_FLAG;
   This_Color |= This_Alt_Char;
   
   SLsmg_write_nchars ((char *) hbuf, n % 16);
   while (count-- > 0)
     {
	SLsmg_write_nchars ((char *) hbuf, 16);
     }
   
   This_Alt_Char = tac;
   This_Color = (This_Color & 0x7F) | This_Alt_Char;
   This_Col = final_col;
}

void SLsmg_draw_double_hline (int n)
{
#ifdef linux_unicode
   int cmin, cmax;
   int final_col = This_Col + n;
   
   if (SLtt_Unicode)
     {
        unsigned short ch;
        
        if ((This_Row < Start_Row) || (This_Row >= Start_Row + Screen_Rows) 
            || (0 == compute_clip (This_Col, n, Start_Col, Start_Col + Screen_Cols,
			           &cmin, &cmax)))
          {
	     This_Col = final_col;
	     return;
          }

        n = cmax - cmin;
        This_Col = cmin;
        ch = SLtt_fix_unicode (SLUNI_DHLINE_CHAR);
   
        while (n-- > 0)
           SLsmg_write_unicode (ch);
        This_Col = final_col;
        return;
     }
   else
#endif
     SLsmg_draw_hline (n);
}

void SLsmg_draw_vline (int n)
{
   unsigned char ch = SLSMG_VLINE_CHAR;
   int tac = This_Alt_Char;
   int c = This_Col;   
   int rmin, rmax;
   int final_row = This_Row + n;

   if (((c < Start_Col) || (c >= Start_Col + Screen_Cols)) ||
       (0 == compute_clip (This_Row, n, Start_Row, Start_Row + Screen_Rows,
			  &rmin, &rmax)))
     {
	This_Row = final_row;
	return;
     }

#ifdef linux_unicode
   if (SLtt_Unicode)
     {
        unsigned short ch = SLtt_fix_unicode (SLUNI_VLINE_CHAR);
        
        for (This_Row = rmin; This_Row < rmax; This_Row++)
          {
             This_Col = c;
             SLsmg_write_unicode (ch);
          }
     }
   else
#endif   
     {
        This_Alt_Char = ALT_CHAR_FLAG;
        This_Color |= This_Alt_Char;
    
        for (This_Row = rmin; This_Row < rmax; This_Row++)
          {
	     This_Col = c;
	     SLsmg_write_nchars ((char *) &ch, 1);
          }
   
        This_Alt_Char = tac;
        This_Color = (This_Color & 0x7F) | This_Alt_Char;
     }
   
   This_Col = c;  This_Row = final_row;
}

void SLsmg_draw_double_vline (int n)
{
#ifdef linux_unicode
   int c = This_Col, rmin, rmax;
   int final_row = This_Row + n;

   if (SLtt_Unicode)
     {   
        unsigned short ch = SLtt_fix_unicode (SLUNI_DVLINE_CHAR);
        
        if (((c < Start_Col) || (c >= Start_Col + Screen_Cols)) ||
            (0 == compute_clip (This_Row, n, Start_Row, Start_Row + Screen_Rows,
			       &rmin, &rmax)))
          {
	     This_Row = final_row;
	     return;
          }
        
        for (This_Row = rmin; This_Row < rmax; This_Row++)
          {
             This_Col = c;
             SLsmg_write_unicode (ch);
          }
        This_Col = c;  This_Row = final_row;
     }
   else
#endif
     SLsmg_draw_vline (n);   
}

void SLsmg_draw_box (int r, int c, int dr, int dc)
{
   if (!dr || !dc) return; 
   This_Row = r;  This_Col = c;
   dr--; dc--;
   SLsmg_draw_hline (dc);  
   SLsmg_draw_vline (dr);
   This_Row = r;  This_Col = c;
   SLsmg_draw_vline (dr);
   SLsmg_draw_hline (dc);   
#ifdef linux_unicode
   if (SLtt_Unicode)
     {
        SLsmg_draw_unicode (r, c, SLUNI_ULCORN_CHAR);
        SLsmg_draw_unicode (r, c + dc, SLUNI_URCORN_CHAR);
        SLsmg_draw_unicode (r + dr, c, SLUNI_LLCORN_CHAR);
        SLsmg_draw_unicode (r + dr, c + dc, SLUNI_LRCORN_CHAR);
     }
   else
#endif
     {
        SLsmg_draw_object (r, c, SLSMG_ULCORN_CHAR);
        SLsmg_draw_object (r, c + dc, SLSMG_URCORN_CHAR);
        SLsmg_draw_object (r + dr, c, SLSMG_LLCORN_CHAR);
        SLsmg_draw_object (r + dr, c + dc, SLSMG_LRCORN_CHAR);
     }
   This_Row = r; This_Col = c;
}
   
void SLsmg_draw_double_box (int r, int c, int dr, int dc)
{
   if (!dr || !dc) return; 
   This_Row = r;  This_Col = c;
   dr--; dc--;
   SLsmg_draw_double_hline (dc);  
   SLsmg_draw_double_vline (dr);
   This_Row = r;  This_Col = c;
   SLsmg_draw_double_vline (dr);
   SLsmg_draw_double_hline (dc);   
#ifdef linux_unicode
   if (SLtt_Unicode)
     {
        SLsmg_draw_unicode (r, c, SLUNI_DULCORN_CHAR);
        SLsmg_draw_unicode (r, c + dc, SLUNI_DURCORN_CHAR);
        SLsmg_draw_unicode (r + dr, c, SLUNI_DLLCORN_CHAR);
        SLsmg_draw_unicode (r + dr, c + dc, SLUNI_DLRCORN_CHAR);
     }
   else
#endif
     {
        SLsmg_draw_object (r, c, SLSMG_ULCORN_CHAR);
        SLsmg_draw_object (r, c + dc, SLSMG_URCORN_CHAR);
        SLsmg_draw_object (r + dr, c, SLSMG_LLCORN_CHAR);
        SLsmg_draw_object (r + dr, c + dc, SLSMG_LRCORN_CHAR);
     }
   This_Row = r; This_Col = c;
}
   
void SLsmg_fill_region (int r, int c, int dr, int dc, unsigned char ch)
{
   static unsigned char hbuf[16];
   int count;
   int dcmax, rmax;
   
   
   if ((dc < 0) || (dr < 0)) return;
   
   SLsmg_gotorc (r, c);
   r = This_Row; c = This_Col;
   
   dcmax = Screen_Cols - This_Col;
   if (dc > dcmax) dc = dcmax;
   
   rmax = This_Row + dr;
   if (rmax > Screen_Rows) rmax = Screen_Rows;

#ifndef pc_system
   ch = Alt_Char_Set[ch];
#endif
   if (ch != hbuf[0]) MEMSET ((char *) hbuf, (char) ch, 16);
   
   for (This_Row = r; This_Row < rmax; This_Row++)
     {
	This_Col = c;
	count = dc / 16;
	SLsmg_write_nchars ((char *) hbuf, dc % 16);
	while (count-- > 0)
	  {
	     SLsmg_write_nchars ((char *) hbuf, 16);
	  }
     }
   
   This_Row = r;
}

