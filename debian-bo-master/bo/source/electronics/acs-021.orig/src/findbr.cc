/*$Id: findbr.cc,v 11.38 96/03/24 17:59:59 al Exp $ -*- C++ -*-
 * find a branch with matching label
 * returns the branch pointer
 */
#include "ap.h"
#include "l_lib.h"
#include "e_card.h"
#include "error.h"
/*--------------------------------------------------------------------------*/
	CARD *findbranch(CS&,CARD*,CARD*);
	CARD *findbranch_samescope(const char*,CARD*);
	const CARD *findbranch_sametype(const char*,const CARD*);
/*--------------------------------------------------------------------------*/
extern const char e_int[];
/*--------------------------------------------------------------------------*/
/* findbranch: find a matching label, by (ugh) linear search
 * 	start searching at "start", stop at "stop"
 *	label to look for is in command line: &cmd[*cnt]
 * return pointer to match if exists (and eat input)
 *	  pointer to a non-existent branch if no match (don't eat input)
 * caution: caller must check return value before using
 */
CARD *findbranch(CS& cmd, CARD *start, CARD *stop)
{
  int save;
  CARD* brh;
  char labelwanted[BUFLEN+1];
  char thislabel[BUFLEN+1];
  char* dot;
  char* wanted;
  
  save = cmd.cursor();			/* copy the name to local space	    */
  cmd.ctostr(labelwanted, BUFLEN, TOKENTERM);
  
  if (!labelwanted[0] || !labelwanted[1]){
    cmd.reset(save);			/* don't match single letter	    */
    return (CARD*)NULL;			/* probably a command		    */
  }
  
  labelwanted[BUFLEN] = thislabel[BUFLEN] = '\0';
  
  dot = strrchr(labelwanted,'.');	/* trim off front stuff for subckt  */
  if (dot){
    *dot = '\0';
    wanted = dot + 1;
  }else{
    wanted = labelwanted;
  }
  
  brh = start;
  do {
    CS lbl(brh->label);
    lbl.ctostr(thislabel, BUFLEN, TOKENTERM);
    if (wmatch(thislabel,wanted)){
      if (!dot){				/* found it */
	return (CARD*)brh;
      }else if(brh->subckt){
	CARD *subbrh;
	CS want(labelwanted);
	subbrh = findbranch(want, brh->subckt, brh->subckt->prev());
	if (exists(subbrh)){
	  return subbrh;
	}else if (brh == stop){			/* found subckt but */
	  cmd.reset(save);			/* no match within it */
	  return (CARD*)NULL;
	}
      }
    }else if (brh == stop){
      cmd.reset(save);
      return (CARD*)NULL;			/* didn't find it */
    }
  } while (brh = brh->next(),  brh != start);
  
  error(bWARNING, e_int, "findbranch: no stop");
  cmd.reset(save);				/* trap endless loop */
  return (CARD*)NULL;
}
/*--------------------------------------------------------------------------*/
/* findbranch_samescope: search for a match to "name" in the "list"
 * following same scope
 * return ptr to it, or NULL if it fails
 */
CARD *findbranch_samescope(const char *name, CARD *list)
{
  CARD *brh;
  brh = list;	
  for (;;){
    brh = brh->next();
    if (wmatch(brh->label, name)){
      return brh;				/* found it */
    }else if (brh == list){
      return (CARD*)NULL;			/* failed */
    }
  }
  /*NOTREACHED*/
}
/*--------------------------------------------------------------------------*/
/* findbranch_sametype: search for a match to "name" in the "list"
 * following same type thread
 * return ptr to it, or NULL if it fails
 */
const CARD *findbranch_sametype(const char *name, const CARD *list)
{
  const CARD *brh;
  brh = list;	
  for (;;){
    brh = brh->stnext;
    if (wmatch(brh->label, name)){
      return brh;				/* found it */
    }else if (brh == list){
      return (CARD*)NULL;			/* failed */
    }
  }
  /*NOTREACHED*/
}
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/
