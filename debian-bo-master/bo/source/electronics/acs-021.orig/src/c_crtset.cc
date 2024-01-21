/*$Id: c_crtset.cc,v 11.28 96/03/03 23:06:52 al Exp $ -*- C++ -*-
 * set up crt plotting parameters
 */
#include "ap.h"
#include "error.h"
#include "io.h"
#include "pixelh.h"
#include "c_comand.h"
/*--------------------------------------------------------------------------*/
//	void		CMD::crtset(CS&);
static	void		setq(CS&,int*);
	int		testcrt();
	struct graph	*initcrt();
/*--------------------------------------------------------------------------*/
extern struct graph *initsun(struct graph *);
extern struct graph *initibm(struct graph *);
extern struct graph *initbasic(struct graph *);
extern struct graph *initpostscript(struct graph *);
extern struct graph *initunix(struct graph *);
extern struct graph *initx(struct graph *);
static int crttype;
static struct graph d;
/*--------------------------------------------------------------------------*/
void CMD::crtset(CS& cmd)
{
  static int sw_c, sh_c;
  static bool beenhere = false;

  if (!beenhere){
    d.sw    = 640;
    d.sh    = 200;
    sw_c    = 80;
    sh_c    = 25;
    d.ppc   = d.sw / sw_c;
    d.lpc   = d.sh / sh_c;
    d.gmode = 6;
    d.tmode = 2;
    d.pri   = 1;
    d.sec   = -2;
    d.grid  = -8;
    d.back  = 0;
    d.palette = 0;
  }
  beenhere = true;
  
  if (cmd.pmatch("Ascii")){
    crttype = '\0';
  }else if (cmd.pmatch("None")){
    crttype = '\0';
  }else if (cmd.pmatch("Basic")){
    crttype = 'b';
  }else if (cmd.pmatch("Hercules")){
    crttype = 'h';
  }else if (cmd.pmatch("Ibm")){
    crttype = 'i';
  }else if (cmd.pmatch("Postscript")){
    crttype = 'p';
  }else if (cmd.pmatch("Sun")){
    crttype = 's';
  }else if (cmd.pmatch("Unix")){
    crttype = 'u';
  }else if (cmd.pmatch("X")){
    crttype = 'x';
  }else if (cmd.pmatch("Zenith")){
    crttype = 'z';
  }else if (!cmd.peek()){
    mprintf(IO::mstdout,"%c\n", crttype);
    mprintf(IO::mstdout,"%dx%d, %dx%d\n", d.sw, d.sh, sw_c, sh_c);
    mprintf(IO::mstdout,"%d, %d\n", d.gmode, d.tmode);
    mprintf(IO::mstdout,"%d, %d, %d, %d\n", d.pri, d.sec, d.grid, d.back);
    mprintf(IO::mstdout,"%d\n", d.palette);
    return;
  }else{
    cmd.check(bERROR);
  }

  setq(cmd,&d.sw);
  setq(cmd,&d.sh);
  setq(cmd,&sw_c);
  setq(cmd,&sh_c);
  d.ppc = d.sw / sw_c;
  d.lpc = d.sh / sh_c;
  setq(cmd,&d.gmode);
  setq(cmd,&d.tmode);
  setq(cmd,&d.pri);
  setq(cmd,&d.sec);
  setq(cmd,&d.grid);
  setq(cmd,&d.back);
  setq(cmd,&d.palette);
}
/*--------------------------------------------------------------------------*/
static void setq(CS& cmd, int *value)
{
  cmd.skipbl();
  if (isdigit(cmd.peek()) || cmd.peek()=='-')
    *value = (int)cmd.ctof();
  else
    cmd.skiparg();
}
/*--------------------------------------------------------------------------*/
int testcrt()
{
  return crttype;
}
/*--------------------------------------------------------------------------*/
struct graph *initcrt()
{
  switch (crttype){
  case 'b':
    return initbasic(&d);
#ifdef MSDOS
  case 'i':
    return initibm(&d);
#endif
  case 'p':
    return initpostscript(&d);
#ifdef SUNVIEW
  case 's':
    return initsun(&d);
#endif
  case 'u':
    return initunix(&d);
  case 'x':
    return initx(&d);
  default:
    return (struct graph*)NULL;
  }
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
