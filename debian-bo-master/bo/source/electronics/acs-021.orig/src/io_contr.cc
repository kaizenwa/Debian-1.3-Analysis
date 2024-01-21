/*$Id: io_contr.cc,v 11.37 96/03/24 10:10:25 al Exp $ -*- C++ -*-
 * Sets up output direction and format for most commands
 * returns  FALSE if it did nothing
 *	    fORD if nothing with a file
 *	    fOUT if it set up an output file
 *	    fIN	 if is set up an input file
 * updates pointers into the string
 * outreset starts it all over
 */
#include "ap.h"
#include "error.h"
#include "l_lib.h"
#include "io.h"
/*--------------------------------------------------------------------------*/
	void	initio(int,FILE*);
	void	decipher(char*);
	void	outreset(void);
	bool	outset(CS&,const char*,const char*);
static	FILE*	file_open(CS&,const char*,const char*,FILE*);
/*--------------------------------------------------------------------------*/
static FILE *fn;		/* write file				    */
/*--------------------------------------------------------------------------*/
/* initio: initialize file encryption, etc
 */
void initio(int Where, FILE *Whence)
{
  const char *tag;
  tag = "''''";
  if (IO::outcipher)			/* if writing an encrypted file,    */
    mprintf(Where,"%s\n",tag);		/* mark it as encrypted		    */
  if (Whence){
    char buf[BUFLEN];
    if (!fgets(buf, BUFLEN, Whence))	/* if the first line deciphers to   */
      return;				/* the cipher tag, it is encrypted  */
    IO::incipher = true;		/* otherwise,			    */
    decipher(buf);			/*	 rewind and read normally   */
    if (strcmp(buf,tag) != 0){   /* mismatch */
      IO::incipher = false;
      fseek(Whence,0L,SEEK_SET);
    }
  }
}
/*--------------------------------------------------------------------------*/
/* decipher: un-encrypt a line of text in place
 */
void decipher(char *buf)
{
  if (IO::incipher){
    for ( ; isprint(buf[1]); buf++ ){
      int fixed = (int)buf[1] - (int)buf[0];
      while (!isascii(fixed) || !isprint(fixed))
	fixed += (0x7f-0x20);
      buf[0] = (char)fixed;
    }
    buf[0] = '\0';
  }
}
/*--------------------------------------------------------------------------*/
/* outreset: close files and set i/o flags to standard values
 */
void outreset(void)
{
  xclose(&fn);
  xclose(&IO::whence);
  IO::where = IO::formaat = 0;
  IO::incipher = IO::outcipher = IO::pack = false;
  if (IO::echoflag)
    IO::where |= IO::mstdout;
  if (IO::printflag)
    IO::where |= IO::mprint;
}
/*--------------------------------------------------------------------------*/
/* outset: set up i/o for a command
 * return whether or not it did anything
 */
bool outset(CS& cmd, const char *inext, const char *outext)
{
  bool retcode = false;
  
  for (;;){
    if (cmd.pmatch("Printer")){
      IO::where |= IO::mprint;
    }else if (cmd.pmatch("Noprint")){
      IO::where &= ~IO::mprint;
    }else if (cmd.pmatch("Basic")){
      IO::formaat = ftos_EXP;
    }else if (cmd.pmatch("Cipher")){
      IO::outcipher = IO::pack = true;
    }else if (cmd.pmatch("Pack")){
      IO::pack = true;
    }else if (cmd.pmatch("Quiet")){
      IO::where &= ~IO::mstdout;
    }else if (cmd.pmatch("Echo") || cmd.pmatch("List")){
      IO::where |= IO::mstdout;
    }else if (outext && cmd.pmatch("SAve")){
      fn = file_open(cmd,outext,"w",fn);
      IO::where |= 1<<fileno(fn);
    }else if (outext && cmd.pmatch(">$$")){
      const char *rwaflag;
      rwaflag = (cmd.pmatch(">$$")) ? "a" : "w";
      fn = file_open(cmd,outext,rwaflag,fn);
      IO::where |= 1<<fileno(fn);
      IO::formaat = ftos_EXP;
    }else if (inext && cmd.pmatch("<$$")){
      IO::whence = file_open(cmd,inext,"r",IO::whence);
    }else{
      break;
    }
    retcode = true;
  }
  return retcode;
}
/*--------------------------------------------------------------------------*/
/* file_open: a different interface for xopen
 */
static FILE *file_open(
	CS& cmd,
	const char *ext,
	const char *rwaflag,
	FILE *fileptr)
{
  xclose(&fileptr);
  fileptr = xopen(cmd,ext,rwaflag);
  if (!fileptr)
    error(bERROR, "");
  return fileptr;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
