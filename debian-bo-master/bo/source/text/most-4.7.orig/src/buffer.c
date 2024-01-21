#include "config.h"

#include <stdio.h>

#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif

#include <string.h>

#include <slang.h>
#include "jdmacros.h"

#include "most.h"
#include "buffer.h"
#include "display.h"
#include "window.h"
#include "line.h"
#include "file.h"

int Most_W_Opt = 0;

unsigned char *Most_Beg;             /* beginning of current buffer */
unsigned char *Most_Eob;             /* end of current buffer */

Most_Buffer_Type *Most_Buf;

int Most_Num_Lines;

unsigned char *Most_C_Pos;
int Most_C_Line;

static unsigned char *beg_of_line1(void)
{
   unsigned char *pos;
    
   if (Most_C_Pos == Most_Beg) return Most_Beg;
   pos = Most_C_Pos;

   if (pos != Most_Eob)
     {
	if (*pos == '\n') 
	  {
	     pos--;
	     while ((*pos != '\n') && (pos > Most_Beg)) pos--;
	     if (*pos != '\n') return pos;
	     if (pos + 1 != Most_C_Pos) return pos + 1;
	  }
     }
   else pos--;
   
   if (*pos != '\n')
     {
	while ((*pos != '\n') && (pos > Most_Beg)) pos--;
	if (*pos != '\n') return Most_Beg;
	return pos + 1;
     }
   
   /* from here on *pos == '\n' */
   
   if (Most_S_Opt == 0) return pos + 1;
   
   while ((*pos == '\n') && (pos > Most_Beg)) pos--;
   pos += 2;
   if (pos > Most_C_Pos) pos = Most_C_Pos;
   return pos;
   
#if 0

    pos = Most_C_Pos;
    if (pos == Most_Eob) pos--;

    if ((*pos != '\n') && (*(pos-1) == '\n')) return(pos);
    
    if ((*pos == '\n') && (pos == Most_Beg)) return(Most_Beg);
    /* suppose Most_Beg[] is "....\nabcde\n\n\n\n\n..." and we are somwhere in
     * the middle of the \n's.  Then we want to return the 2nd \n if the
     * SQUEEZE is on otherwise just return where we are. */
    if ((*pos == '\n') && (*(pos - 1) == '\n'))
      {
	 if (Most_S_Opt)
            {
                /* if we are between '\n's then skip past all of them. */
                while ((pos > Most_Beg) && (*pos == '\n')) pos--;
                if ((pos == Most_Beg) && (*pos == '\n')) return (Most_Beg);
                pos += 2;
            }
          return (pos);
      }      
    
   while((pos > Most_Beg) && (*pos != '\n')) pos--;
   if (pos == Most_Beg) return pos;
   
   if (pos != Most_Eob) pos--;
    if (pos != Most_Beg) pos++;
    else if ((pos == Most_Beg) && (*pos == '\n')) pos++;
    return pos;
#endif
}

unsigned char *most_beg_of_line(void)
{
    unsigned char *b;
    int d,n;
    
    if (!Most_W_Opt) return beg_of_line1();
    b = beg_of_line1();
    d = Most_C_Pos - b;
    n = d / (SLtt_Screen_Cols - 1);
    return (b +  (int) (n * (SLtt_Screen_Cols - 1)));
}

/* does not move point */
static unsigned char *end_of_line1(void)
{
   register unsigned char *pos, *pmax = Most_Eob;
   int n, n2;

    pos = Most_C_Pos;
    if (pos >= Most_Eob)  return(Most_Eob);
    /*     if ((pos == Most_Beg) && (*pos == '\n')) return (Most_Beg); */
    
    /* find the first '\n' */
    if (*pos != '\n')
      {
	 n = pmax - pos;
	 n2 = n % 8;
	 pmax = pos + (n - 8);
	
	 while(pos <= pmax)
	   {
	      if (*pos == '\n') return pos;
	      if (*(pos + 1) == '\n') return pos + 1;
	      if (*(pos + 2) == '\n') return pos + 2;
	      if (*(pos + 3) == '\n') return pos + 3;
	      if (*(pos + 4) == '\n') return pos + 4;
	      if (*(pos + 5) == '\n') return pos + 5;
	      if (*(pos + 6) == '\n') return pos + 6;
	      if (*(pos + 7) == '\n') return pos + 7;
	      pos += 8;
	   }
	 pmax = pos + n2;
	 while ((pos < pmax) && (*pos != '\n')) pos++;
	 return(pos);
      }
   
    if (!Most_S_Opt) return (pos);
  /* file */
    /* if Most_Beg = "....abc\n\n\n\n\ndef..." then we are at some first \n.  We
       want to return the last '\n' unless we wre at the first '\n'. */

   /* Here we are on a \n and NOT at the end of the buffer */
   
   if ((pos > Most_Beg) && (*(pos - 1) != '\n')) return (pos); 
    
   while ((pos < Most_Eob) && (*pos == '\n')) pos++;
   if (pos == Most_Eob) return pos;
   if (pos != Most_Beg) pos--;
   return pos;
   /* if (pos == Most_Eob) return (pos - 1);
    return pos; */
}

static unsigned char *end_of_line(void)
{
    unsigned char *b, *e;
    int n;
    
    if (!Most_W_Opt) return end_of_line1();

    b = most_beg_of_line();
    e = end_of_line1();
   
   /* What a hack!! */
   n = (e - b) / (SLtt_Screen_Cols - 1);
   if (n) return(b + (SLtt_Screen_Cols - 1));
   return (e);
}

int most_forward_line(int save)
{
   int m;
   register int n = save;
   unsigned char *p;
   unsigned char *pmax;
   
   pmax = Most_Eob;
   
   if (n > 0) 
     {
	if (Most_B_Opt)
	  {
	     m = (Most_Eob - Most_C_Pos)/16;
	     if (n > m) n = m;
	     Most_C_Pos += n * 16;
	     Most_C_Line += n;
	     return n;
	  }
	
	while (n--)
	  {
	     Most_C_Pos = end_of_line();
	     if (Most_C_Pos == Most_Eob) return (save - (n + 1));
	     Most_C_Line++; Most_C_Pos++;
	     
	     if (Most_Selective_Display)
	       {
		  /* Skip past lines with too much indentation to the start
		   * of a valid one. 
		   */
		  p = Most_C_Pos;
		  while (p < pmax)
		    {
		       while ((p < pmax) && (*p <= ' ')) p++;
		       if (most_apparant_distance(p) < Most_Selective_Display)
		       	 break;
		       Most_C_Pos = p;
		       p = end_of_line ();
		       if (p < pmax) p++;
		    }
		  Most_C_Pos = p;
	       } 
	  }
     }
   else
     {
	if (Most_B_Opt)
	  {
	     m = (Most_Beg - Most_C_Pos)/16;
	     if (n < m) n = m;
	     Most_C_Pos += n * 16;
	     Most_C_Line += n;
	     return n;
	  }
	else while (n++)
	  {
	     Most_C_Pos = most_beg_of_line();
	     if (Most_C_Pos == Most_Beg) return (n - (save + 1));
	     Most_C_Line--;
	     Most_C_Pos--;
	     
	     if (Most_Selective_Display)
	       {
		  /* Skip past lines with too much indentation to the start
		   * of a valid one. 
		   */
		  p = Most_C_Pos;
		  while (p > Most_Beg)
		    {
		       /* skip all blank lines */
		       while ((p > Most_Beg) && (*p <= ' ')) p--;
		       Most_C_Pos = pmax = p;
		       p = Most_C_Pos = most_beg_of_line ();
		       while ((p < pmax) && (*p <= ' ')) p++;
		       if (most_apparant_distance(p) < Most_Selective_Display)
		       	 break;
		       Most_C_Pos = p;
		       p = most_beg_of_line ();
		       if (p > Most_Beg) p--;
		       Most_C_Pos = p;
		    }
		  Most_C_Pos = p;
	       }
	  }
     }
   return(save);
}

/* Count lines in the region.  A half line counts as 1 */
int most_count_lines(unsigned char *beg, unsigned char *end)
{
   int save_line, n;
   unsigned char *save_beg, *save_eob, *save_pos;
   int dn = 1000;

   if (Most_B_Opt) return(1 + (int)(end - beg) / 16);
   
   save_line = Most_C_Line; save_beg = Most_Beg; save_eob = Most_Eob; 
   save_pos = Most_C_Pos;
   
   Most_Beg = Most_C_Pos = beg; Most_Eob = end;
   
   n = 1;
   while((dn = most_forward_line(dn)) != 0) n += dn;
   Most_C_Pos = save_pos;
   Most_Eob = save_eob;
   Most_Beg = save_beg;
   Most_C_Line = save_line;
   return(n);
}

void most_goto_line(int line)
{
   int dif_c, dif_b,dif_t;

   if (line < 1) line = 1;
   most_read_to_line(line);
   if (line > Most_Num_Lines) line = Most_Num_Lines;

    if (Most_B_Opt)
      {
          Most_C_Pos = Most_Beg + (16 * (line - 1));
          Most_C_Line = line;
          return;
      }
    
    dif_c = line - Most_C_Line;
    dif_b = line - Most_Num_Lines;
    dif_t = line - 1;

    /* 4 possibilites */
    if (dif_c <= 0)
      {
          if (dif_t < -dif_c) /* go from top */
            {
                Most_C_Line = 1;
                Most_C_Pos = Most_Beg;
                (void) most_forward_line(dif_t);
            }
          else  /* from curr back */
            {
                (void) most_forward_line(dif_c);
            }
      }
    else if (dif_c > 0)
      {
          if ((dif_c + dif_b) < 0) /* go from curr */
            {
                (void) most_forward_line(dif_c);
            }
          else
            {
                Most_C_Line = Most_Num_Lines;
                Most_C_Pos = Most_Eob;
                (void) most_forward_line(dif_b);
            }
      }
}       

/* return line the point is on without the final '\n's
 * unless beg = end in which case we take care of it later ...
 *
 * returns 1 if the line should be wrapped.  It determines this very crudely.
 * Dont expect to to always get it right becuase it knows nothing about the 
 * displayable representation of the line.
 */ 
int most_extract_line(unsigned char **beg, unsigned char **end)
{
   unsigned char *pend, *pbeg;
   int ret = 0;
   
   pbeg = most_beg_of_line();
   pend = end_of_line();
   
   if (pend != Most_Eob) 
     {
	if (*pend != '\n') ret = 1;
     }
   
   /* while ((*pend == '\n') && (pend > pbeg)) pend--; */
   *beg = pbeg;
   *end = pend;
   return(ret);
}    

int most_what_line(unsigned char *pos)
{
    
    unsigned char *save_pos;
    int save_line, dir;
    register int dif_c, dif_b,dif_t;
    int ret;

    if (Most_B_Opt)
      {
          return (1 + (pos - Most_Beg)/16);
      }
    
   if (Most_Selective_Display) 
     {
        return most_count_lines (Most_Beg, pos);
     }
   
        
    save_pos = Most_C_Pos;
    save_line = Most_C_Line;
    
    dif_c = pos - Most_C_Pos;
    dif_b = pos - Most_Eob;
    dif_t = pos - Most_Beg;

    /* 4 possibilites */
    if (dif_c <= 0)
      {
          if (dif_t < -dif_c) /* go from top */
            {
                Most_C_Line = 1;
                Most_C_Pos = Most_Beg;
                dir = 1;
            }
          else  /* from curr back */
            {
                dir = -1;
            }
      }
    else if (dif_c > 0)
      {
          if ((dif_c + dif_b) < 0) /* go from curr */
            {
                dir = 1;
            }
          else
            {
                Most_C_Line = Most_Num_Lines;
                Most_C_Pos = Most_Eob;
                dir = -1;
            }
      }
    if (dir == 1)
      {
          while(Most_C_Pos = end_of_line(), Most_C_Pos < pos)
            {
                Most_C_Pos++;
                Most_C_Line++;
            }
      }
    else
      {
          while(Most_C_Pos = most_beg_of_line(), pos < Most_C_Pos)
            {
                Most_C_Line--;
                Most_C_Pos--;
            }
      }

    ret = Most_C_Line;
    Most_C_Pos = save_pos;
    Most_C_Line = save_line;
    return(ret);
}

/* given a buffer position, find the line and column */
void most_find_row_column(unsigned char *pos, int *r, int *c)
{
    unsigned char *beg, *save_pos;
    int save_line;


    if (pos <= Most_Beg)
      {
          *r = 1;
          *c = 1;
          return;
      }

    save_line = Most_C_Line;
    save_pos = Most_C_Pos;
    *r = most_what_line(pos);
    
    if (Most_B_Opt)
      {
          *c = (int) (pos - Most_Beg) - (*r - 1) * 16 + 1;
          return;
      }
    Most_C_Line = *r;
    Most_C_Pos = pos;
    
    /* Now we have found the line it is on so.... */
    beg = most_beg_of_line();
    *c = 1;
    while (beg++ < pos) *c = *c + 1;
    Most_C_Line = save_line;
    Most_C_Pos = save_pos;
}       

Most_Buffer_Type *most_switch_to_buffer(Most_Buffer_Type *nnew)
{
    Most_Buffer_Type *old;
    old = Most_Buf;
    Most_Buf = nnew;
    Most_Beg = Most_Buf->beg;
    Most_Eob = Most_Buf->end;
    return old;
}
/*
static void delete_buffer(Most_Buffer_Type *old)
{
   if (Most_Buf->fd != -1) close(Most_Buf->fd);
    (void) SLFREE(Most_Buf);
    Most_Buf = old;
    Most_Beg = Most_Buf->beg;
    Most_Eob = Most_Buf->end;
}
*/
Most_Buffer_Type *most_create_buffer(char *file)
{
   Most_Buffer_Type *buf;

   buf = (Most_Buffer_Type *) MOSTMALLOC(sizeof(Most_Buffer_Type));
   buf->mark = 0;
   buf->beg = buf->end = NULL;
   strcpy(buf->file,file);
   return(buf);
}

unsigned char *most_malloc(unsigned int n)
{
   unsigned char *b = (unsigned char *) SLMALLOC(n);
   if (b == NULL) 
     {
	most_exit_error("malloc: Memory Allocation Error.");
     }
   return b;
}

unsigned char *most_realloc(unsigned char *p, unsigned int n)
{
   unsigned char *b = (unsigned char *) SLREALLOC(p, n);
   if (b == NULL) 
     {
	most_exit_error("malloc: Memory Allocation Error.");
     }
   return b;
}
