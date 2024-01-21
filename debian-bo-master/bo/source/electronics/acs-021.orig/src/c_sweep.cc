/*$Id: c_sweep.cc,v 11.38 96/03/24 17:59:23 al Exp $ -*- C++ -*-
 * Step a parameter and repeat a group of commands
 */
#include "ap.h"
#include "l_lib.h"
#include "error.h"
#include "io.h"
#include "c_comand.h"
#include "declare.h"	/* getcmd */
/*--------------------------------------------------------------------------*/
//	void	CMD::sweep(CS&);
static	void	buildfile(CS&);
static	void	doit();
static	void	setup(CS&);
/*--------------------------------------------------------------------------*/
extern const bool crtplot;
extern int swp_count[], swp_steps[];
extern int swp_type[];
extern int swp_nest;
static char tempfile[] = STEPFILE;
/*--------------------------------------------------------------------------*/
void CMD::sweep(CS& cmd)
{
  if (cmd.more())
    buildfile(cmd);
  doit();
  unfault(cmd);
}
/*--------------------------------------------------------------------------*/
static void buildfile(CS& cmd)
{
  auto char buffer[BUFLEN];
  static FILE *fptr;
  
  setup(cmd);
  if (fptr)
    fclose(fptr);
  fptr = fopen( tempfile, "w" );
  if (!fptr)
    error(bERROR, "can't open temporary file\n" );
  fprintf(fptr, "%s\n", cmd.fullstring() );
  
  for (;;){
    getcmd( ">>>", buffer, BUFLEN );
    if ( pmatch(buffer,"GO") )
      break;
    fprintf(fptr,"%s\n",buffer);
  }
  fclose(fptr);
  fptr = (FILE*)NULL;
}
/*--------------------------------------------------------------------------*/
static void doit()
{
  auto char buffer[BUFLEN];
  static FILE *fptr;
  
  for ( swp_count[swp_nest]=0 ; swp_count[swp_nest]<=swp_steps[swp_nest] ;
       swp_count[swp_nest]++ ){
    if (fptr)
      fclose(fptr);
    fptr = fopen(tempfile, "r");
    if (!fptr)
      error(bERROR, "can't open %s\n", tempfile);
    fgets(buffer,BUFLEN,fptr);
    CS cmd(buffer);
    if (cmd.pmatch("SWeep"))
      setup(cmd);
    else
      error(bERROR, "bad file format: %s\n", tempfile);
    int ind = cmd.cursor();
    strncpy(buffer, "fault                              ", (size_t)ind);
    buffer[ind-1] = ' ';		/* make sure there is a delimiter   */
    					/* in case the words run together   */
    for (;;){				/* may wipe out one letter of fault */
      CMD::cmdproc(buffer);
      if (!fgets(buffer,BUFLEN,fptr))
	break;
      if (!crtplot)
	mprintf(IO::mstdout,"%u> %s", swp_count[swp_nest]+1, buffer);
    }
  }
  fclose(fptr);
  fptr = (FILE*)NULL;
  swp_count[swp_nest] = 0;
}
/*--------------------------------------------------------------------------*/
static void setup(CS& cmd)
{
  for (;;){
    if (cmd.is_digit()){
      swp_steps[swp_nest] = cmd.ctoi() ;
      swp_steps[swp_nest] = (swp_steps[swp_nest]) 
	? swp_steps[swp_nest]-1
	: 0;
    }else if (cmd.pmatch("LInear")){
      swp_type[swp_nest] = 0;
    }else if (cmd.pmatch("LOg")){
      swp_type[swp_nest] = 'L';
    }else{
      break;
    }
  }
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
