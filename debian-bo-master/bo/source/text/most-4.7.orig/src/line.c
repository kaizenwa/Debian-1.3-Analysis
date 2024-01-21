#include "config.h"

#include <stdio.h>
#include <string.h>

#include <slang.h>
#include "jdmacros.h"

#include "most.h"
#include "line.h"
#include "window.h"
#include "display.h"
  
int Most_Tab_Width = 8;

#define MAX_LINE_LEN 8196
int Most_Selective_Display = 0;
/* take 16 binary characters and put them in displayable form */
static int ascii_format_line(char *buff, char *str, int max)
{
    int i,ii,j,k,ch,di,flag;
    char num_str[4];
   register char *s, *smax;
   
    ii = 0;
    di = 40;
    flag = 1;

   s = str;
   smax = s + 80; while (s < smax) 
     {
	*s++ = ' '; *s++ = ' '; *s++ = ' '; *s++ = ' ';
     }
   
    
    for (j = 0; j < 4; j++)
      {
          
          for (k = 0; k < 4; k++)
            {
                i = 4 * j + k;
	       if (i >= max) break;
	       
                ch = buff[i];
                if (Most_V_Opt)
                  {
                      if (((ch < 32) && (ch >= 0)) || (ch == 127))
                        {
                            str[ii++] = '^';
                            if (ch == 127)
                              str[ii++] = '?';
                            else
                              str[ii++] = ch + 'A' - 1;
                            str[i + di] = '.';
                        }
                      else if (ch < 0)
                        {
                            ch = ch + 256; 
                            sprintf(num_str,"%02X", ch);
                            str[ii++] = num_str[0];
                            str[ii++] = num_str[1];
                            str[i + di] = '.';
                        }
                      else
                        {
                            str[ii++] = ' ';
                            str[ii++] = ch;
                            str[i + di] = ch;
                        }
                  }
                else
                  {
                      if (ch < 32)
                        {
                            if (ch < 0) ch = ch + 256;
                            str[i + di] = '.';
                        }
                      else str[i+di] = ch;
                      
                      sprintf(num_str,"%02X", ch);
                      str[ii++] = num_str[0];
                      str[ii++] = num_str[1];
                  }
                
            } /* k */
          str[ii++] = ' ';
      }
    di += 16;
    str[di] = '\0';
    return (di);
}
    

int most_analyse_line(unsigned char *begg, unsigned char *endd, char *out, char *attributes)
{
   register unsigned char *beg = begg, *end = endd, ch;
   register int ii, i;
   int test, fold, ii_max, j, ok;
   char attr;
   
   test = 0;
   i = ii = 0;
   ii_max = 0;
   fold = 0;
   while ((beg < end) && ((ch = *beg) != '\n')) /*  && (ch != '\0')) */
     {
	beg++;
	if (ii > ii_max) fold = 0;  /* beyond previous high */
	attr = ' ';
	
	/*
	 *  set up bolding of line if ^M
	 */
	if (!Most_V_Opt && (ch == '\r'))                   /* ^M */
	  {
	     if (beg < end)
	       {
		  fold = 1;
		  if (ii > ii_max) ii_max = ii - 1; /* ^M contributes nil */
		  ii = 0;
	       }
	  }
	
	/*
	 * set up bolding or underlining of character if '\b' (^H)
	 */
	else if (!Most_V_Opt && (ch == '\b') 
		 && (ii != 0) && (i != 0))
	  {
	     test = 1;
	     ii--;
	     i--;
	  }
	/*
	 * assign bolding or underlining attributes
	 */
	else if (test || fold)
	  {
	     test = 0;
	     if (ch == (unsigned char) out[ii])
	       attr = 'b';
	     else if (out[ii] == '_')
	       attr = 'u';
	     
	     if (fold && ch == ' ')
	       ii++;
	     else
	       {
		  attributes[i++] = attr;
		  out[ii++] = ch;
	       }
	  }
	else if (!Most_T_Opt && (ch == '\t'))
	  {
	     j = Most_Tab_Width * (ii/Most_Tab_Width + 1) - ii;  /* Most_Tab_Width column tabs */
	     while (j--)
	       {
		  out[ii++] = ' ';
		  attributes[i++] = attr;
	       }
	  }
	else
	  {
	     out[ii++] = ch;
	     attributes[i++] = attr;
	     if (((int) ch & 0x7F) < 32) attributes[i++] = attr;
	  }
     }
   if (fold) ii = ii_max + 1;
   if ((beg > end) && Most_Selective_Display && !Most_W_Opt)
     {
	ok = 1;
	if (*beg == '\n') beg++;
	while ((*beg <= ' ') && ok)
	  {
	     if (*beg != '\n') beg++;
	     if (beg >= Most_Eob) break;
	     if ((*beg == '\n') || (most_apparant_distance(beg) >= Most_Selective_Display))
	       {
		  ok = 0;
	       }
	  }
	if (!ok)
	  {
	     ok = 3;
	     while(ok--)
	       {
		  out[ii++] = '.'; 
		  attributes[i++] = ' ';
	       }
	  }
	
     } /* Most_Selective_Display */
   out[ii] = '\0';
   return(ii);
}

static void output_with_attr (unsigned char *out, unsigned char *attr)
{
   unsigned char at, ch, lat;
   unsigned char *p = out;
   
   lat = ' ';

   if (Most_V_Opt) while ((ch = *p) != 0)
     {
	p++;
	attr++;
     }
   else while ((ch = *p) != 0)
     {	     
	if (lat != *attr)
	  {
	     if (p != out) 
	       {
		  SLsmg_write_nchars ((char *) out, (int) (p - out));
		  out = p;
	       }
	     
	     at = *attr;
	     if (at == 'u') 
	       {
		  most_tt_underline_video ();
	       }
	     else if (at == 'b')
	       {
		  most_tt_bold_video ();
	       }
	     else most_tt_normal_video ();
	     lat = at;
	  }
	p++;
	attr++;
     }
   
   if (p != out) SLsmg_write_nchars ((char *) out, (int) (p - out));
   if (lat != ' ') most_tt_normal_video ();
}

   
   
   

void most_output(unsigned char *line, int len, unsigned char *attr, char unsigned d_char)
{
   int i, ii, count, max_col;
   unsigned char ch, ch1;
   unsigned char out[1024];
   
   /* expand to the window start */ 
   i = count = 0;
   while ((i < len) && ((ch = line[i]) != '\n'))
     {
	ch1 = ch & 0x7F;
	if ((ch1 >= ' ') && (ch1 < 0x7F)) count++;
	else if (ch & 0x80) count += 3;
	else count += 2;
	
	if (count >= Most_Column) break;
	i++;
     }
   if (count < Most_Column) return;
   
   /* Now i is where we can start filling the left part of the screen */
   attr += i; count = 0; ii = 0;
   max_col = SLtt_Screen_Cols;
   while ((i < len) && (ii < max_col) && ((ch = line[i]) != '\n'))
     {
	ch1 = ch & 0x7F;
	if ((ch1 >= ' ') && (ch1 < 0x7F))
	  {
	     out[ii++] = ch;
	  }
	else 
	  {
	     if (ch & 0x80) 
	       {
		  out[ii++] = '~';
		  ch = ch1;
	       }
	     out[ii++] = '^';
	     if (ch == 0x7F) out[ii++] = '?'; 
	     else out[ii++] = ch + '@';
	  }
	i++;
	/* if (ii >= max_col) break; */
     }
   
   if ((ii > max_col) || ((ii == max_col) && (i < len))
       || ((d_char != '$') && (ii = max_col - 1)))
     {
	ii = max_col;
	out[ii - 1] = d_char;
     }
   out[ii] = '\0';
   
   output_with_attr (out, attr);
}


void most_display_line (void)
{
   unsigned char *beg, *end;
   int i, len, v = 0, t = 0;
   unsigned char buff[16];
   unsigned char the_line[MAX_LINE_LEN],  the_attr[MAX_LINE_LEN], *line,*attr, ch;
   
    line = the_line;
    attr = the_attr;
    /* This needs fixed for files with really big lines */
    if (!Most_B_Opt)
     {
	if (most_extract_line(&beg, &end) && Most_W_Opt) ch = '\\'; 
	else ch = '$';
          
	len = end - beg;  /* MOST4  --- was + 1 */
	if (len == 0) 
	  {
	     SLsmg_erase_eol ();
	     return;
	  }
	
	if (len > MAX_LINE_LEN)
	  {
	      v = Most_V_Opt;
	      t = Most_T_Opt;
	      Most_V_Opt = 1;
	      Most_T_Opt = 1;
	      line = beg;
	   }
	 else len = most_analyse_line(beg, end, (char *) line, (char *) attr);
      }
   else
     {	
	/* This needs cleaned up!! */
	ch = '$';
	i = 0;
	beg = Most_C_Pos;
	end = Most_C_Pos + 16;
	if (end > Most_Eob) end = Most_Eob;
	while (beg < end) buff[i++] = *beg++;
	/* while(i < 15) buff[i++] = 0; */
	sprintf ((char *)the_line, "0x%08X: ", (unsigned int)(Most_C_Pos - Most_Beg));
	len = ascii_format_line((char *) buff, (char *) the_line + 12, i);
	len += 12;
	i = 0; while(i < 80) attr[i++] = ' ';
     }
   
    
    most_output(line,len,attr, ch);
    if (len > MAX_LINE_LEN)
      {
          Most_V_Opt = v;
          Most_T_Opt = t;
      }
   SLsmg_erase_eol ();
}


/* given a position in a line, return apparant distance from bol
   expanding tabs, etc... up to pos */
int most_apparant_distance(unsigned char *pos)
{
    int i;
    unsigned char *cur_pos, *save_pos, ch;
   
    cur_pos = Most_C_Pos;
    save_pos = pos;
    Most_C_Pos = pos;
    pos = most_beg_of_line();
    Most_C_Pos = cur_pos;

    i = 0;
    while (pos < save_pos)
      {
	 ch = *pos++;
	 if (((ch >= ' ') && (ch <= 126))
	     || (ch >= 160)) 
	   {
	      i++;
	      continue;
	   }
	 
	 if (!Most_V_Opt && (ch == '\b'))
	   {
	      if (i > 0) i--;
	   }
	 else if (!Most_V_Opt && (ch == '\015')) /* ^M */
	   {
	      if (i != 1) i = 0;
	   }
	 else if (!Most_T_Opt && (ch == '\t'))
	   {
	      i = Most_Tab_Width * (i/Most_Tab_Width + 1);  /* Most_Tab_Width column tabs */
	   }
	 /* else Control char */
	 else 
	   {
	      if (ch & 0x80) i += 3;
	      else i += 2;
	   }
      }
    return (i);
}
