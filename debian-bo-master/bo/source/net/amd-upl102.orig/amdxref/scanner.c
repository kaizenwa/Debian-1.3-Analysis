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
#include <stdio.h>
#include <string.h>
#include "amddefs.h"
#include "parser.h"

extern char *globals[];
static char *vars[] = {
     "type", "rhost", "rfs", "opts", "sublink", "fs",
     "pref", "dev", "delay", "remopts", "mount",
     "unmount", "cache", "map", "key", "path"
     };

extern struct amdmap *getmaps(void);
extern char *expand(char *, struct record *);
extern void setvars(struct amdmap *, struct entry *);
extern void printrecord(struct amdmap *, struct record *);
extern void printsel(struct selector *);
extern void printvars(struct amdmap *, char *[]);
extern int scan(void);
extern int compselector(struct selector *, struct selector *);
extern void comprec(struct amdmap *, struct amdmap *,
		    struct entry *, struct entry *,
		    struct record *, struct record *);
extern void xrefmaps(struct amdmap *, struct amdmap *,
		     struct entry *, struct entry *);
extern unsigned long makemtopts(char *);

struct entry *
getentrybyname(struct amdmap *m, char *key)
{
     struct entry *rove = m->entries;
     for(;rove;rove=rove->next)
	  if(!strcmp(key, rove->key))
	       break;
     return rove;
}

struct entry *
getmapdefaults(struct amdmap *m)
{
     struct entry *rove = m->entries;
     for(;rove;rove=rove->next)
	  if(rove->flags & KEYDEFAULT)
	       break;
     return rove;
}

void
setvars(struct amdmap *m, struct entry *en)
{
     struct record *rec = en->rec;
     struct entry *defaults = m->defaults;
     struct record *defrec = 0;
     int i;

     if(defaults)
	  defrec = defaults->rec;
     for(;rec;rec=rec->next){
	  rec->mtpnt = 0;
	  for(i=0;i<NUMVARS;i++)
	       if(!rec->variables[i]){
		    if(defrec && defrec->variables[i])
			 rec->variables[i] = strdup(defrec->variables[i]);
		    else if(globals[i])
			 rec->variables[i] = strdup(globals[i]);
	       }
	  rec->variables[VKEY] = strdup(en->key);
	  rec->variables[VMAP] = strdup(m->name);

	  i = strlen(m->mtpnt);
	  rec->variables[VPATH] = (char *)malloc(strlen(en->key)+i+2);
	  strcpy(rec->variables[VPATH], m->mtpnt);
	  *(rec->variables[VPATH]+i) = '/';
	  strcpy(rec->variables[VPATH]+i+1, en->key);
     }
}

void
printrecord(struct amdmap *m, struct record *rec)
{
     struct selector *rsel;
     for(rsel = rec->sel;rsel;rsel = rsel->next){
	  printsel(rsel);
	  if(rsel->next)
	       putchar(';');
     }
     if(rec->sel)
	  putchar(';');
     printvars(m, rec->variables);
}

void
printsel(struct selector *sel)
{
     fputs(selector[sel->seltype].name, stdout);
     if(sel->oper)
	  fputs("==", stdout);
     else
	  fputs("!=", stdout);
     fputs(sel->value, stdout);
}

void
printvars(struct amdmap *m, char *tbl[])
{
     int i;
     int semi = 0;
     struct entry *defaults = m->defaults;

     for(i=0;i<NUMVARS;i++){
	  if(tbl[i]){
	       if(semi)
		    putchar(';');
	       printf("%s:=%s", vars[i], tbl[i]);
	       semi = 1;
	  }
	  else if(defaults && defaults->rec && defaults->rec->variables[i]){
	       if(semi)
		    putchar(';');
	       printf("%s:=%s", vars[i], defaults->rec->variables[i]);
	       semi = 1;
	  }
	  else if(globals[i]){
	       if(semi)
		    putchar(';');
	       printf("%s:=%s", vars[i], globals[i]);
	       semi = 1;
	  }
     }
}

static int errcount;

int
scan(void)
{
     register struct entry *rove1, *rove2, *rove;
     register struct amdmap *map1, *map2;
     struct record *defrec;

     if(!(map1 = getmaps()))
	  return;

     for(map2=map1;map2;map2=map2->next){
	  map2->defaults = getmapdefaults(map2);
	  for(rove2=map2->entries;rove2;rove2=rove2->next){
	       if(!(rove2->flags & KEYDEFAULT))
		    setvars(map2, rove2);
	  }
     }

     errcount = 0;

     for(;map1;map1=map1->next)
	  for(rove=map1->entries;rove;rove=rove->next)
	       if(!(rove->flags & KEYDEFAULT)){
		    if(rove->next)
			 xrefmaps(map1, map1, rove, rove->next);
		    if(map1->next)
			 for(map2=map1->next;map2;map2=map2->next)
			      xrefmaps(map1, map2, rove, map2->entries);
	       }
     fsdump();
     return !!errcount;
}

void
xrefmaps(struct amdmap *m1, struct amdmap *m2,
	 struct entry *ent, struct entry *start)
{
     register struct entry *rove;
     register struct record *rec1, *rec2;

     for(rove=start;rove;rove=rove->next)
	  if(!(rove->flags & KEYDEFAULT)){
	       if(m1 == m2)
		    if(strcmp(ent->key, rove->key) == 0){
			 printf(">>>> Duplicate key %s in map %s\n",
				rove->key,
				m1->name);
		    }
	       for(rec1=ent->rec;rec1;rec1=rec1->next)
		    for(rec2=rove->rec;rec2;rec2=rec2->next)
			 comprec(m1, m2, ent, rove, rec1, rec2);
	  }
}

void
comprec(struct amdmap *m1, struct amdmap *m2, 
	struct entry *e1, struct entry *e2,
	register struct record *rec1, register struct record *rec2)
{
     register char *mtpnt1, *mtpnt2;
     int status;

     if(!rec1->variables[VTYPE] || !rec2->variables[VTYPE])
	  return;
     if(strcmp(rec1->variables[VTYPE], "nfs"))
	  return;
     if(strcmp(rec2->variables[VTYPE], "nfs"))
	  return;
     if(!compselector(rec1->sel, rec2->sel))
	  return;

     if(!(mtpnt1 = rec1->mtpnt))
	  mtpnt1 = rec1->mtpnt = expand(rec1->variables[VFS], rec1);
     if(!(mtpnt2 = rec2->mtpnt))
	  mtpnt2 = rec2->mtpnt = expand(rec2->variables[VFS], rec2);

     if(!rec1->mountopts){
	  rec1->mountopts = makemtopts(rec1->variables[VOPTS]);
	  addfsent(rec1->variables[VRHOST],
		   rec1->variables[VRFS],
		   rec1->mountopts);

     }
     if(!rec2->mountopts){
	  rec2->mountopts = makemtopts(rec2->variables[VOPTS]);
	  addfsent(rec2->variables[VRHOST],
		   rec2->variables[VRFS],
		   rec2->mountopts);
     }

     status = subpath(mtpnt1, mtpnt2);

     if(status == 0){
	  return;
     }

     if(status == 3){
	  if(rec1->variables[VRFS] && rec2->variables[VRFS])
	       if(!strcmp(rec1->variables[VRFS], rec2->variables[VRFS])){
		    if(rec1->mountopts != rec2->mountopts){
			 errcount++;
			 printf("*** Mount options are radically different for %s/%s and %s/%s ***\n",
				m1->mtpnt, e1->key,
				m2->mtpnt, e2->key);
			 printf("(%s)%s: ", m1->name, e1->key);
			 printrecord(m1, rec1);
			 printf("\n\n(%s)%s: ", m2->name, e2->key);
			 printrecord(m1, rec2);
			 putchar('\n');
			 printf("------------------------------------------------------------------------\n");
		    }
	       }
	       else {
		    errcount++;
		    printf("Different file systems mounting at same location:\n");
		    printf("%s/%s @ %s == %s/%s @ %s\n",
			   m1->mtpnt, e1->key, mtpnt1,
			   m2->mtpnt, e2->key, mtpnt2);
		    printf("(%s)%s: ", m1->name, e1->key);
		    printrecord(m1, rec1);
		    printf("\n\n(%s)%s: ", m2->name, e2->key);
		    printrecord(m2, rec2);
		    putchar('\n');
		    printf("------------------------------------------------------------------------\n");
	       }
     }
     else if(status == 2){
	  errcount++;
	  printf("%s/%s can mount over %s/%s:\n",
		 m2->mtpnt, e2->key,
		 m1->mtpnt, e1->key);
	  printf("%s mounts at %s\n%s mounts at %s\n\n", e2->key, mtpnt2, e1->key, mtpnt1);
	  if(rec1->mountopts != rec2->mountopts)
	       printf("*** Mount options are radically different as well ****\n");
	  printf("(%s)%s: ", m1->name, e1->key);
	  printrecord(m1, rec1);
	  printf("\n\n(%s)%s: ", m2->name, e2->key);
	  printrecord(m2, rec2);
	  putchar('\n');
	  printf("------------------------------------------------------------------------\n");
     }
     else if(status == 1){
	  errcount++;
	  printf("%s/%s can mount over %s/%s:\n",
		 m1->mtpnt, e1->key,
		 m2->mtpnt, e2->key);
	  printf("%s mounts at %s\n%s mounts at %s\n\n", e1->key, mtpnt1, e2->key, mtpnt2);
	  if(rec1->mountopts != rec2->mountopts)
	       printf("*** Mount options are radically different as well ****\n");
	  printf("(%s)%s: ", m1->name, e1->key);
	  printrecord(m1, rec1);
	  printf("\n\n(%s)%s: ", m2->name, e2->key);
	  printrecord(m2, rec2);
	  putchar('\n');
	  printf("------------------------------------------------------------------------\n");
     }
}

char *
expand(char *str, struct record *rec)
{
     int bufsize = 32;
     char *result = (char *)malloc(bufsize);
     char *r, *s, *e, *a;
     int w, i, len;
     char varbuf[32];
     int basename, dirname, host, domain;

     for(r=str, w = 0;*r;r++){
	  if(bufsize > 1024){
	       printf("Bufsize error\n");
	       exit(1);
	  }
	  if(*r != '$' || *(r+1) != '{'){
	       if(w == bufsize){
		    bufsize *= 2;
		    result = (char *)realloc(result, bufsize);
	       }
	       result[w++] = *r;
	  }
	  else {
	       char *s = r;
	       basename = dirname = host = domain = 0;
	       r += 2;
	       for(e=r+1;*e && *e != '}';e++)
		    ;
	       len = e-r;
	       if(*r == '/'){
		    basename++;
		    r++;
		    len--;
	       }
	       else if(*r == '.'){
		    domain++;
		    r++;
		    len--;
	       }
	       else if(*(e-1) == '/'){
		    dirname++;
		    len--;
	       }
	       else if(*(e-1) == '.'){
		    host++;
		    len--;
	       }
	       memmove(varbuf, r, len);
	       varbuf[len] = 0;
	       if(*e)
		    r = e;
	       else
		    r = e-1;
	       for(i=0;i<NUMVARS;i++)
		    if(rec->variables[i]){
			 if(!strcmp(vars[i], varbuf)){
			      if(basename){
				   s = rec->variables[i];
				   for(e=s;*e;e++)
					if(*e == '/')
					     s = e+1;
			      }
			      else if(domain){
				   s = rec->variables[i];
				   for(e=s;*e;e++)
					if(*e == '.'){
					     s = ++e;
					     for(;*e;e++)
						  ;
					     break;
					}
			      }
			      else if(dirname){
				   s = rec->variables[i];
				   for(e=s;*e;e++)
					if(*e == '/')
					     break;
			      }
			      else if(host){
				   s = rec->variables[i];
				   for(e=s;*e;e++)
					if(*e == '.')
					     break;
			      }
			      else {
				   s = rec->variables[i];
				   for(e=s;*e;e++)
					;
			      }
			      for(a=s;*a && a < e;a++){
				   if(w == bufsize){
					bufsize *= 2;
					result = (char *)realloc(result, bufsize);
				   }
				   result[w++] = *a;
			      }
			      break;
			 }
		    }
	       if(i == NUMVARS){
		    if(w == bufsize){
			 bufsize *= 2;
			 result = (char *)realloc(result, bufsize);
		    }
		    result[w++] = *s;
		    r = s;
	       }
	  }
     }
     if(w == bufsize){
	  bufsize += 1;
	  result = (char *)realloc(result, bufsize);
     }
     result[w] = 0;
     return result;
}
     
int
compselector(struct selector *s1, struct selector *s2)
{
     struct selector *rove;
     for(;s1;s1=s1->next){
	  for(rove=s2;rove;rove=rove->next)
	       if(s1->seltype == rove->seltype){
		    if(!(s1->oper == rove->oper &&
			 !strcmp(s1->value , rove->value)))
			 return 0;
		    break;
	       }
     }
     return 1;
}

#define MTOPT_DONE   0x01
#define MTOPT_RO     0x02
#define MTOPT_NOSUID 0x04

unsigned long
makemtopts(char *moptstr)
{
     register unsigned long result = MTOPT_DONE;
     char buffer[256];
     register char *s;
     register int i;

     if(!(s = moptstr))
	  return result;
     if(*s == '-')
	  s++;
     i = 0;
     for(;*s;s++)
	  if(*s == ','){
	       buffer[i] = 0;
	       if(!strcmp(buffer, "ro"))
		    result |= MTOPT_RO;
	       else if(!strcmp(buffer, "nosuid"))
		    result |= MTOPT_NOSUID;
	       i = 0;
	  }
	  else
	       buffer[i++] = *s;
     return result;
}
