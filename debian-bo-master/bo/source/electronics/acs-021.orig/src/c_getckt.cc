/*$Id: c_getckt.cc,v 11.38 96/03/24 17:59:14 al Exp $ -*- C++ -*-
 * build, get, merge, "<" commands
 * process circuit files, and keyboard entry
 */

#include "ap.h"
#include "error.h"
#include "io.h"
#include "d_admit.h"
#include "d_cap.h"
#include "d_cccs.h"
#include "d_ccvs.h"
#include "d_coil.h"
#include "d_coment.h"
#include "d_cs.h"
#include "d_dot.h"
#include "d_diode.h"
#include "d_logic.h"
#include "d_mos.h"
#include "d_res.h"
#include "d_subckt.h"
#include "d_trln.h"
#include "d_vccs.h"
#include "d_vcvs.h"
#include "d_vs.h"
#include "d_switch.h"
#include "u_opt.h"
#include "u_status.h"
#include "l_compar.h"
#include "l_lib.h"
#include "c_comand.h"
#include "s__.h"
#include "declare.h"	/* getcmd */
/*--------------------------------------------------------------------------*/
//	void	CMD::build(CS&);
//	void	CMD::get(CS&);
//	void	CMD::merge(CS&);
//	void	CMD::run(CS&);
static  void    getmerge(CS&);
static	CARD *parsebranch(char*,int);
static	CARD *cparse(CS&);
static	CARD* dodot(CS&);
/*--------------------------------------------------------------------------*/
extern char head[];                 /* place to store title line            */
extern run_mode_t run_mode;
/*--------------------------------------------------------------------------*/
/* cmd_build: build command
 * get circuit description direct from keyboard (or stdin if redirected)
 * Command syntax: build <before>
 * Bare command: add to end of list
 * If there is an arg: add before that element
 * null line exits this mode
 * preset, but do not execute "dot cards"
 */
void CMD::build(CS& cmd)
{
  CARD *brh;

  STATUS::get.reset().start();
  STATUS::iter[iTOTAL] = 1;
  run_mode = rPRESET;
  SIM::uninit();
  CARD::putbefore = findbranch(cmd, CARD::first(), CARD::last());
  if (!(exists(CARD::putbefore))){
    CARD::putbefore = CARD::root();
    assert(CARD::putbefore);
  }
  do{
    char buffer[BIGBUFLEN];
    getcmd(">",buffer,BIGBUFLEN);
    brh = parsebranch(buffer,true);
  }while (exists(brh));
  STATUS::get.stop();
}
/*--------------------------------------------------------------------------*/
/* cmd_get: get command
 * get circuit from a file, after clearing the old one
 * preset, but do not execute "dot cards"
 */
void CMD::get(CS& cmd)
{
  STATUS::get.reset().start();
  run_mode = rPRESET;
  CS nil("");
  clear(nil);
  getmerge(cmd);
  STATUS::get.stop();
}
/*--------------------------------------------------------------------------*/
/* cmd_merge: merge command
 * as get, but do not clear first
 */
void CMD::merge(CS& cmd)
{
  STATUS::get.reset().start();
  run_mode = rPRESET;
  getmerge(cmd);
  STATUS::get.stop();
}
/*--------------------------------------------------------------------------*/
/* cmd_run: "<" and "<<" commands
 * run in batch mode
 * "<<" clears old circuit first, "<" does not
 * get circuit from file, execute dot cards in sequence
 */
void CMD::run(CS& cmd)
{
  STATUS::get.reset().start();
  while (cmd.match('<')){
    CS nil("");
    clear(nil);
    cmd.skip();
    cmd.skipbl();
  }
  run_mode = rEXECUTE;
  getmerge(cmd);
  STATUS::get.stop();
}
/*--------------------------------------------------------------------------*/
/* getmerge: actually do the work for "get", "merge", etc.
 */
static void getmerge(CS& cmd)
{
  char buffer[BIGBUFLEN];
  bool  echoon;		/* echo on/off flag (echo as read from file) */
  bool  liston;		/* list on/off flag (list actual values) */
  bool  quiet;		/* don't echo title */
  static FILE *filen;	/* file number (static for safety) */

  STATUS::iter[iTOTAL] = 1;
  SIM::uninit();
  xclose(&filen);
  filen = xopen(cmd,"","r");
  if (!filen)
    error(bERROR, "");
  
  echoon = liston = quiet = false;
  cmd.stuck();
  do{
    cmd.set("Echo",  &echoon, true);
    cmd.set("List",  &liston, true);
    cmd.set("Quiet", &quiet,  true);
  }while (cmd.more() && !cmd.stuck());
  cmd.check(bWARNING);
  
  if (!getlines(buffer, BIGBUFLEN, filen))	/* title */
    error(bWARNING, "empty circuit file\n");
  trim(buffer);
  if (!quiet)
    mprintf(IO::mstdout, "%s\n", buffer);
  if (*buffer)
    strcpy(head, buffer);
  
  CARD::putbefore = CARD::root();
  assert(CARD::putbefore);
  while (getlines(buffer, BIGBUFLEN, filen)){
    CARD *brh;
    if (echoon)
      mprintf(IO::mstdout, "%s\n", buffer);
    brh = parsebranch(buffer,false);
    if (liston  &&  exists(brh)){
      brh->print(IO::mstdout, false);
    }
  }
  xclose(&filen);
}
/*--------------------------------------------------------------------------*/
/* parsebranch: parse an input line, and process it
 */
static CARD *parsebranch(char *buffer, int alwaysdupcheck)
{
  CARD *brh;			/* place for cparse to return data */
  CARD *before;			/* actually insert here */

  brh = (CARD*)NULL;
  before = CARD::putbefore;	/* save insert place in case something like */
 				/* a subckt changes it */

  if (OPT::dupcheck ||  alwaysdupcheck){
    CS buf(buffer);
    brh = findbranch(buf, CARD::putbefore, CARD::putbefore->prev());
  }
 
  if (exists(brh)){
    error(bWARNING, "replacing [%s]\n", brh->printlabel());
    CS buf(buffer);
    brh->parse(buf);
  }else{
    CS buf(buffer);
    brh = cparse(buf);
    if (exists(brh)){
      brh->insertbefore(before);
    }
  }
  
  if (exists(brh) && brh->isdevice()){
    SIM::uninit();
  }
  return brh;
}
/*--------------------------------------------------------------------------*/
/* cparse: circuit parse: parse one line of a netlist
 * mostly, dispatches to the proper function.
 */
static CARD *cparse(CS& cmd)
{
  CARD *brh;

  cmd.skipbl();
  if (cmd.is_digit())
    cmd.ctoi();	/* ignore line numbers */
  
  brh = (CARD*)NULL;
  int here = cmd.cursor();
  switch (to_upper(cmd.peek())){
    case '\0':	/* nothing */			break;
    case '.':	brh = dodot(cmd);		break;
    case '\'':
    case '"':
    case ';':
    case '#':
    case '*':	brh = new DEV_COMMENT;		break;
    case 'A':	cmd.warn(bWARNING);		break;
    case 'B':	cmd.warn(bWARNING);		break;
    case 'C':	brh = new DEV_CAPACITANCE;	break;
    case 'D':	brh = new DEV_DIODE;		break;
    case 'E':	brh = new DEV_VCVS;		break;
    case 'F':	brh = new DEV_CCCS;		break;
    case 'G':	brh = new DEV_VCCS;		break;
    case 'H':	brh = new DEV_CCVS;		break;
    case 'I':	brh = new DEV_CS;		break;
    case 'J':	cmd.warn(bWARNING);		break;
    case 'K':	brh = new DEV_MUTUAL_L;		break;
    case 'L':	brh = new DEV_INDUCTANCE;	break;
    case 'M':	brh = new DEV_MOS;		break;
    case 'N':	cmd.warn(bWARNING);		break;
    case 'O':	cmd.warn(bWARNING);		break;
    case 'P':	cmd.warn(bWARNING);		break;
    case 'Q':	cmd.warn(bWARNING);		break;
    case 'R':	brh = new DEV_RESISTANCE;	break;
    case 'S':	brh = new DEV_VSWITCH;		break;
    case 'T':	brh = new DEV_TRANSLINE;	break;
    case 'U':	brh = new DEV_LOGIC;		break;
    case 'V':	brh = new DEV_VS;		break;
    case 'W':	brh = new DEV_CSWITCH;		break;
    case 'X':	brh = new DEV_SUBCKT;		break;
    case 'Y':	brh = new DEV_ADMITTANCE;	break;
    case 'Z':	cmd.warn(bWARNING);		break;
    default:	cmd.warn(bWARNING);		break;
  }
  cmd.reset(here);
  if (exists(brh)){
    brh->parse(cmd);
  }else if (brh){
    error(bWARNING, "internal error: branch has no type <%s>\n",
    	  cmd.fullstring());
    delete brh;
    brh = (CARD*)NULL;
  }
  return brh;
}
/*--------------------------------------------------------------------------*/
static CARD* dodot(CS& cmd)
{
  cmd.skip();
  if (cmd.pmatch("MODEL")){
    cmd.skiparg();
    if      (cmd.pmatch("D"    ))  return new MODEL_DIODE;
    else if (cmd.pmatch("NPn"  ))  cmd.warn(bWARNING);
    else if (cmd.pmatch("PNp"  ))  cmd.warn(bWARNING);
    else if (cmd.pmatch("NJf"  ))  cmd.warn(bWARNING);
    else if (cmd.pmatch("PJf"  ))  cmd.warn(bWARNING);
    else if (cmd.pmatch("NMos" ))  return new MODEL_MOS;
    else if (cmd.pmatch("PMos" ))  return new MODEL_MOS;
    else if (cmd.pmatch("Logic"))  return new MODEL_LOGIC;
    else if (cmd.pmatch("SW"   ))  return new MODEL_SWITCH;
    else if (cmd.pmatch("CSW"  ))  return new MODEL_SWITCH;
    else			   cmd.warn(bWARNING);
  }else if (cmd.pmatch("SUBCKT")){
    return new MODEL_SUBCKT;
  }else{
    return new DEV_DOT;	
  }
  return NULL;
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
