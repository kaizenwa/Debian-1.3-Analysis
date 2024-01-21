/*$Id: io_out.cc,v 11.28 96/03/03 23:07:54 al Exp $ -*- C++ -*-
 * output text to files, devices, or whatever
 * m???? = multiple output to a bunch of io devices.
 *	    with character count (so tab will work)
 * Will start a new line first if the entire output will not fit.
 * so wrap will not break a word or number.
 * Where is a bit mask of places to send the output.
 * A possible portability problem exists with the handle numbers.
 * It assumes they start at 0, and count up, and that there are no more than
 * the number of bits in an integer (MAXHANDLE).
 * but I have yet to find a system that did not meet this form.
 */
#include "io.h"
#include "u_opt.h"
/*--------------------------------------------------------------------------*/
	void mtab(int,int);
	void mprintf(int,const char*,...);
	void mputs(const char*,int);
	void mputc(int,int);
/*--------------------------------------------------------------------------*/
static int cpos[MAXHANDLE+1];		/* character counter    */
/*--------------------------------------------------------------------------*/
/* mtab: tab to column "count" on output devices "where"
 * by outputting spaces.
 * If already beyond, start new line, then tab to column.
 */
void mtab(int count, int where)
{
  int ii,mm;
  for (ii=0, mm=1;   ii<=MAXHANDLE;   ++ii, mm<<=1){
    if (where & mm){
      if (cpos[ii] > count)
	mputc('\n',mm);
      while (cpos[ii]<count)
	mputc(' ',mm);
    }
  }
}
/*--------------------------------------------------------------------------*/
/* mprintf: multiple printf
 * printf to "m" style files.
 */
void mprintf(int where, const char *fmt, ...)
{
  char buffer[BIGBUFLEN];
  va_list arg_ptr;
  
  va_start(arg_ptr,fmt);
  vsprintf(buffer,fmt,arg_ptr);
  va_end(arg_ptr);
  
  mputs(buffer,where);
}
/*--------------------------------------------------------------------------*/
/* mputs: multiple puts.
 * puts to "m" style files.
 * also....
 * starts new line, prefixes it with + if it would exceed width
 * width is handled separately for each file to support different widths
 * (which are not currently handled by .options)
 * and it is possible that current contents of lines may be different
 */
void mputs(const char *str, int where)
{
  int ii;	/* file counter */
  int mm;	/* file counter mask */
  int sl;	/* length of output string */
  int newline;	/* true if any destination is at beginning of line */
  
  newline = false;
  sl = strlen(str);
  for (ii=0, mm=1;   ii<=MAXHANDLE;   ++ii, mm<<=1){
    if (where & mm   &&   (sl+cpos[ii]) >= OPT::outwidth){
      mputc('\n',mm);
      mputc('+',mm);
    }
    if (cpos[ii]==0)
      newline = true;
  }
  if (IO::outcipher && newline)
    mputc('\t',where);
  while (*str)
    mputc(*str++,where);
}
/*--------------------------------------------------------------------------*/
/* mputc: multiple putc
 * multiple putc
 * also....
 * crunch spaces, if selected
 * encripts, if selected
 * keeps track of character count
 */
void mputc(int chr, int where)
{
  int ii,mm,suppress,count;
  static int old = '\0';
  static int cchr = 'w';		/* starting encryption seed	    */
					/* arbitrary printable character    */
  if (chr=='\t'){
    chr = ' ';
    count = false;
  }else{
    count = true;
  }
  
  suppress = (IO::pack && old==' ' && chr==' ');
  old = chr;
  if (IO::outcipher && !suppress && isprint(chr)){
    cchr += (unsigned int)chr;
    while (!isascii(cchr)  ||  !isprint(cchr))
      cchr -= (0x7f-0x20);
    chr = (char)cchr;
  }
  
  for (ii=0, mm=1;   ii<=MAXHANDLE;   ++ii, mm<<=1){
    if (where & mm){
      if (chr=='\b'){
	--cpos[ii];
	fflush(IO::stream[ii]);
      }else if (count){
	++cpos[ii];
      }
      if (chr=='\n'){
	cpos[ii] = 0;
	fflush(IO::stream[ii]);
      }else if (chr=='\r'){
	if (cpos[ii] == 0){
	  suppress = true;
	}else{
	  cpos[ii] = 0;
	  fflush(IO::stream[ii]);
	}
      }
      if (!suppress)
	fputc(chr,IO::stream[ii]);
    }
  }
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
