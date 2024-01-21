/*
This software is provided as-is, with no warranties for suitability of
use or support implied.  This package makes no guarantees that it will
perform in any manner.  The authors and Texas A&M University accept no
liability for any damages incurred while using this software.

This software may be copied, redistributed, and updated in any
fashion as long as this comment header is not removed or altered.

Douglas Lee Schales
Doug.Schales@sc.tamu.edu
Texas A&M University
Supercomputer Center

01/26/1993
*/
#include <sys/types.h>
#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include <memory.h>
#include "amddefs.h"
#include "parser.h"

static struct record *head = 0;
static struct record *current = 0, *last = 0;
static struct entry *entry_head = 0, *entry_tail = 0;
static struct amdmap *filelist = 0;

char *globals[NUMVARS];
char *locglobals[NUMVARS];
int defswitch = 0;

int parse_error;

void
resetparser(void)
{
     int i;
     head = current = last = 0;
     entry_head = entry_tail = 0;
     parse_error = 0;
     for(i=0;i<NUMVARS;i++)
	  locglobals[i] = 0;
}

void
initglobals(void)
{
     int i;

     globals[VTYPE] = 0;
     globals[VRHOST] = 0;
     globals[VRFS] = 0;
     globals[VOPTS] = "rw";
     globals[VSUBLINK] = 0;
     globals[VFS] = "${autodir}/${rhost}${rfs}";
     globals[VPREF] = 0;
     globals[VMAP] = 0;
     for(i=0;i<NUMVARS;i++)
	  locglobals[i] = 0;
}

struct amdmap *
getmaps(void)
{
     return filelist;
}

void
checkcurrent(void)
{
     if(!current){
	  current = head = (struct record *)malloc(sizeof(struct record));
	  current->next = 0;
	  current->mountopts = 0;
	  current->sel = 0;
	  current->mtpnt = 0;
	  memset((char *)current->variables, 0, sizeof(char *)*NUMVARS);
     }
}

void
pushmap(char *mtpnt, char *name)
{
     struct amdmap *new;
     struct entry *rove;
     new = (struct amdmap *)malloc(sizeof(struct amdmap));
     new->next = filelist;
     filelist = new;
     new->name = strdup(name);
     new->mtpnt = strdup(mtpnt);
     new->realmtpnt = 0;
     new->entries = entry_head;
     entry_head = 0;
}

void
pushentry(char *key)
{
     struct entry *new;
     int i;

     if(head){
	  new = (struct entry *)malloc(sizeof(struct entry));
	  new->next = 0;
	  if(entry_tail)
	       entry_tail->next = new;
	  entry_tail = new;
	  if(!entry_head)
	       entry_head = new;
     
	  new->rec = head;
	  if(last && last->next){
	       free(last->next);
	       last->next = 0;
	  }
	  head = current = last = 0;
	  
	  for(i=0;i<NUMVARS;i++)
	       if(locglobals[i]){
		    free(locglobals[i]);
		    locglobals[i] = 0;
	       }
	  new->key = strdup(key);
	  new->flags = 0;
	  if(!strcmp(key, "/defaults"))
	       new->flags |= KEYDEFAULT;
     }
     free(key);
}

void
pushrecord(void)
{
     struct record *new;
     int i;

     if(current){
	  new = (struct record *)malloc(sizeof(struct record));
	  new->next = 0;
	  new->sel = 0;
	  new->mountopts = 0;
	  new->mtpnt = 0;
	  memset((char *)new->variables, 0, sizeof(char *)*NUMVARS);

	  for(i=0;i<NUMVARS;i++)
	       if(!current->variables[i] && locglobals[i])
		    current->variables[i] = strdup(locglobals[i]);
	  if(current)
	       current->next = new;
	  last = current;
	  current = new;
	  if(!head)
	       head = new;
     }
}

void
pushsel(int sel, int op, char *val)
{
     struct selector *node;

/*     printf("PUSHSEL(%s,%d,%s)\n",
	    selector[sel].name, op, val);
*/
     if(sel != -1){
	  node = (struct selector *)malloc(sizeof(struct selector));
	  node->seltype = sel;
	  node->oper = op;
	  node->value = strdup(val);
	  checkcurrent();
	  node->next = current->sel;
	  current->sel = node;
     }
     if(val)
	  free(val);
}

void
pushvar(int var, char *val)
{
     int vvar;

/*     printf("PUSHVAR(%s := %s\n",
	    options[var].name, val);
*/
     if(var != -1){
	  checkcurrent();
	  current->variables[var] = strdup(val);
	  if(defswitch)
	       locglobals[var] = strdup(val);
     }
     if(val)
	  free(val);
}

void
setdefaults(int onoff)
{
     defswitch = onoff;
}
