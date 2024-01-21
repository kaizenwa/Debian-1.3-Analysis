
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>

#include "patchlevel.h"

#ifndef	major
#include <sys/sysmacros.h>
#endif /* major */

#include "afio.h"


struct extnode { char *ext; struct extnode *next; };


struct extnode de1={ ".Z", NULL };
struct extnode de2={ ".z", &de1 };
struct extnode de3={ ".gz", &de2 };
struct extnode de4={ ".arc", &de3 };
struct extnode de5={ ".gif", &de4 };
struct extnode de6={ ".zip", &de5 };
struct extnode de7={ ".zoo", &de6 };
struct extnode de8={ ".lha", &de7 };
struct extnode de9={ ".tpz", &de8 };
struct extnode dea={ ".taz", &de9 };
struct extnode deb={ ".jpeg", &dea };
struct extnode dec={ ".jpg", &deb };
struct extnode ded={ ".tgz", &dec };
struct extnode defaultext={ ".tzg", &ded };






struct extnode *compexts=&defaultext;

/* Read file extensions of files that are not to be compressed
 * from compextsfile.
 * Extenstions in the file are seperated by whitespace.
 * a # begins a comment that lasts till the end of the line.
 */
int readcompexts(char *compextsfile)
{
 FILE *infile;
 char ex[81];
 int c;
 struct extnode *tmp;

 infile=fopen(compextsfile,"r");
 if(infile==0) return 0;

 compexts=NULL;

 while(fscanf(infile,"%80s",ex)!=EOF)
   {
     if(ex[0]=='#')
       { /* throw away comment. */
        do{
           c=fgetc(infile);
           if(c==EOF)  { fclose(infile); return 1; }
          }while(c!='\n');
        continue;
       }

     tmp=(struct extnode *)malloc(sizeof(struct extnode));
     if(tmp==NULL) break;
     if((tmp->ext=strdup(ex))==NULL) break;
     tmp->next=compexts;
     compexts=tmp;
   }

 fclose(infile);
 return 1;
}

int matchcompext(char *s)
{
 struct extnode *p;
 int sl;

 p=compexts;
 sl=strlen(s);

 while(p!=NULL)
   {
    if(sl >= strlen(p->ext))
            if(strcmp(s+sl-strlen(p->ext),p->ext)==0) return 1;
     p=p->next;
   }

 return 0;
}



