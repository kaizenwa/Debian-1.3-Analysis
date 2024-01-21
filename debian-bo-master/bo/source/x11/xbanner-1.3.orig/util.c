#include "xb_config.h"

#include <stdio.h>
#include <ctype.h>
#include <time.h>
#include <string.h>
#include <stdlib.h>

#ifndef vms
#include <malloc.h>
#endif

#include <X11/Xlib.h>
#include <X11/Xatom.h>

#include "xbanner.h"

#if !defined(HAS_USLEEP) && !defined(linux)

/* Ultrix has no usleep(), so I wrote it myself... */
#if !defined(vms)
_inline void usleep(const unsigned long usec)
{
  unsigned long diff=0;
  struct timeval start,now;

  gettimeofday(&start,NULL);	/* POSIX compliant, so should be OK */
  while(usec > diff)
  {
    gettimeofday(&now,NULL);
    diff = (now.tv_sec - start.tv_sec)*1000000L + (now.tv_usec - start.tv_usec);
  }
}
#else /* OpenVMS part ... */
void usleep(const unsigned long usec)
{
  lib$wait((float)usec/10000.0);
}
#endif /* vms */
#endif /* !def HAS_USLEEP */

#if defined(vms) || ( !defined(HAS_STRCMPI) && !defined(linux) && !defined(__BSD__) )

/* 2 functions to do case-insensitive comparison of strings */
int strcasecmp(char *s1,char *s2)
{
  while(toupper(*s1)==toupper(*s2) && *s1!='\0' && *s2!='\0')
  {
    s1++;
    s2++;
  }
  return (toupper(*s1)-toupper(*s2));
}

int strncasecmp(char *s1,char *s2,int n)
{
  while(toupper(*s1)==toupper(*s2) && n>=0 && *s1!='\0' && *s2!='\0')
  {
    n--;
    s1++;
    s2++;
  }
  return (toupper(*s1)-toupper(*s2));
}
#endif

/* from here on - stuff that I actually use... */

/* check if the property exists */
Bool XBPropExists(Display *disp,Window root,Atom xprop)
{
  int   num;
  Atom *proplist;

  /* get the list of properties of the root window */
  proplist=XListProperties(disp,root,&num);
  if(proplist==NULL || num==0)
    return False;

  /* look for our property... */
  do {
    num--;
    if(proplist[num] == xprop)
      return True;
  } while(num);

  return False;
}

/* check if a line is empty or a comment */
_inline Bool emptyline(char *s)
{
  char *p=s;

  while(isspace(*p))	/* skip the whitespaces */
    p++;
  if(*p=='#' || *p=='\0' || *p=='\r' || *p=='\n' || *p=='!')
    return True;
  return False;
}

/*
   general routine that takes a string, compares with a NULL terminated
   list of keywords and returns the index into the keyword list.
   May also print an error if not found
*/
int get_keyword(char *line, char *keywords[], char *err)
{
  int i;
  for(i=0;keywords[i]!=NULL;i++)
    if(strcmpi(keywords[i],line)==0)
      break;
  if(keywords[i]==NULL)
  {
    fprintf(stderr,"%s: %s:\n'%s'.\n",PRGCLASS,err,line);
    return -1;
  }
  return i;
}

/* check if s contains a color GradSpec */

/* #### NEEDS REWORK #### */

Bool is_grad_line(char *s)
{
  int parts=0;

  /* what's a typical color-gradient line? comma separated word list */
  while(*s)
  {
    if(is_sep(*s))	/* look for valid separators */
      parts++;
    else if(!isalnum(*s) && !isspace(*s) && *s!='#')
      return False;
    s++;
  }
  if(parts>0)
    return True;
  return False;    
}

/* some things needed for error reporting over the root window */
Bool show_errors=SHOW_ERR_DEFVAL;
char er_msg[1024] = " XBanner MSG: ";
char errline[256];	/* global for sprintf's */
Bool new_errors=False;

/* add an error/warning to the warning line */
static _inline void add_err(char *s)
{
  new_errors=True;
  strcat(er_msg,"# ");	/* for clarity */
  strcat(er_msg,s);
  strcat(er_msg," ");
}

/* actually draw the error line on the root window */
void display_errors(Display *disp,GC mgc)
{
  XFontStruct  *xfont;
  XCharStruct	xchrs;
  int dir,fasc,fdsc,lbear;      /* font ascent/descent/dir/bearing	*/
  int final_x,final_y;		/* where the text will finally appear	*/

  if(!new_errors)	/* nothing to display */
    return;

  /* some active things for the X server to do about the font... */
  xfont = XLoadQueryFont(disp,"fixed");		/* ask for the 'fixed'font */
  if(xfont == NULL)
  {
    fprintf(stderr,"%s: Could not get the font...\n",PRGCLASS);
    exit(1);
  }
  XSetFont(disp,mgc,xfont->fid);	/* make the GC use this font */
  /* get the font information */
  XQueryTextExtents(disp,xfont->fid,er_msg,strlen(er_msg),
  			&dir,&fasc,&fdsc,&xchrs);
  lbear = (xfont->per_char[0]).lbearing; /* important not to draw outside */

  /* calculate location */
  final_x = 16 + abs(lbear);
  final_y = 16 + fasc;

  /* get the colors - use BlackPixel and WhitePixel which are guaranteed */
  XSetForeground(disp,mgc,BlackPixel(disp,DefaultScreen(disp)));
  XSetBackground(disp,mgc,WhitePixel(disp,DefaultScreen(disp)));

  /* Draw the text and its BG */
  XDrawImageString(disp,RootWindow(disp,DefaultScreen(disp)),mgc,final_x,final_y,er_msg,strlen(er_msg));
  /* make sure the X server did this */
  XSync(disp,False);
}

void error(Display *disp,GC mgc, char *s, Bool Fatal)
{
  add_err(s);					/* add the error */
  fprintf(stderr,"%s: %s\n",PRGCLASS,s);	/* print to stderr */

  if(Fatal)		/* check if we need to exit */
  {
    if(show_errors)
      display_errors(disp,mgc);
    exit(1);
  }
}

/* generic routine that splits up a comma sep. line to components */
int split_names(char *line,char names[MAX_CYCS][MAX_CYCNAME])
{
  char *s = line;
  int idx=0;
  int j=0;

  while(*s)
  {
    if(is_sep(*s))
    {
      names[idx][j]='\0';
      j=0; idx++;
      s++; continue;
    }
    else
      names[idx][j]= *s;
    s++; j++;
  }
  names[idx][j]='\0';
  idx++;
  return idx;
} /* split names */

/* parse a string for environment variables */
static int do_env(char *env, char *dst, int *len)
{
  int ln=0;
  char *token;
  char *val;

  if(*env=='@')
  {
    *dst='@';
    *len = 1;
    return 1;
  }

  token = (char*)malloc(strlen(env)+1);
  if(token==NULL)
  {
    *len=1;
    free(token);
    return 1;
  }

  *len = 0;

  if(*env=='{')
  {
    *len = 2;	/* the { and the } */
    env++;
    while(*env!='}' && *env!='\0')
      token[ln++] = *env++;
  }
  else
  {
    while(isalnum(*env))
      token[ln++] = *env++;
  }
  token[ln]='\0';
  *len += ln;

  val=getenv(token);
  if(val==NULL)
  {
    *dst='\0';
    return 0;
  }
  strcpy(dst,val);
  free(token);
  return strlen(dst);
}

Bool parse_env(char *line)
{
  char *new,*k,*s;
  int len;

  new = (char*)malloc(strlen(line)+1);
  if(new==NULL)
  {
    fprintf(stderr,"%s: Couldn't allocate memory for env-var parsing.\n",PRGCLASS);
    return False;
  }
  k = new;
  s = line;

  while(*s)
  {
    if(*s == '$')
    {
      new += do_env(s+1,new,&len);
      s += len+1;
    }
    else
      *new++ = *s++;
  }
  *new='\0';
  strcpy(line,k);
  free(k);

  return True;
}
