/* system includes */
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

/* X11 includes */
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xresource.h>

#ifdef vms
#include <ssdef.h>
#endif

/* our own includes */
#include "xb_config.h"
#include "xbanner.h"

/* its a horrible kludge, but it's still OK */
#define XB_CHECK
#include "xres.c"

#define value(S) elem(S,-1)

#define LNLEN 512

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

#if !defined(HAS_STRCMPI) && !defined(linux) && !defined(__BSD__)

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

/* strip leading whitespace */
char *striplead(char *s)
{
  char *k=s;
  char t[LNLEN];

  while(isspace(*s))
    s++;
  strcpy(t,s);
  strcpy(k,t);
  return k;
}

/* get a certain element of the line */
char *elem(char *s, int elnum)
{
  static char element[LNLEN];
  int elem=0,j=0;

  element[0]='\0';

  while(*s)
  {
    if(*s=='*' || *s=='.')
    {
      if(elem!= -1)
        elem++;
      s++;
      continue;
    }
    if(*s==':')		/* this is already the value */
    {
      elem= -1;
      s++;
      if(elnum== -1)
      {
        strcpy(element,s);
        j=strlen(element);
        break;
      }
      continue;
    }
    if(elem==elnum)
      element[j++]= *s;
    s++;
  }
  element[j]='\0';

  return striplead(element);
}

/* boolean that checks if a string is numeric */
int isnumber(char *s)
{
  if(atoi(s)==0 && *s!='0')
    return 0;
 
  if(*s=='-') s++;
  while(*s)
  {
    if(!isdigit(*s))
      return 0;
    s++;
  }
  return 1;
}

/* return true if the resource is a color resource */
int iscolorres(int i)
{
  switch(i)
  {
    case FGC: case HICOLOR: case BGC:
    case SHDC: case ULC:
      return 1;
    default:
      return 0;
  }
}

/* check if #rrggbb or #rrrrggggbbbb */
int isvalidcolor(char *s)
{
  s++;					/* skip the # sign */
  if(strlen(s)!=6 && strlen(s)!=12)
    return 0;
  while(*s)
  {
    *s=toupper(*s);
    if(!isdigit(*s) && (*s < 'A' || *s > 'F'))
      return 0;
  }
  return 1;
}

/* check gradient spec. */
int valid_grad(char *s)
{
  char *p=s;
  int elems=0;
  
  while(*s)
  {
    if(is_sep(*s))
    {
      if(*(s+1) != '\0')
        elems++;
    }
    s++;
  }
  if(elems==0)
  {
    printf("Color gradient specifies no colors\n");
    return False;
  }
  if(elems > MAX_COLORS_PER_GRAD)
  {
    printf("Color gradient specification '%s' contains too many colors\nMaximum %d allowed.\n",p,MAX_COLORS_PER_GRAD);
    return False;
  }
  return True;
}

void CheckCornerMask(char *s)
{
  int  i,j;  
  CornerType k;
  char op[40];  /* 40 is enough */

  op[0]='\0';

  for(i=0,j=0;s[i];i++,j++)
  {
    if(is_sep(s[i]))
    {
      /* parse it */
      op[j]='\0';
      if(op[0]!='\0')
      {
        for(k=0;cornermask_keyword[k]!=NULL;k++)
          if(strcmpi(cornermask_keyword[k],op)==0)
            break;
        if(cornermask_keyword[k]==NULL)
        {    
          fprintf(stderr,"%s: Unknown corner mask keyword '%s'\n",PRGCLASS,op);
          continue;
        }    
      } /* if op[0] != 0 */
      /* and restart */
      op[0]='\0';
      j=0;
    } 
    else /* not sep */
      op[j]=s[i];
  } /* for i,j */
}

Bool valid_float(char *s)
{
  int numper=0,min=0;

  while(isspace(*s))	/* skip whitespace */
    s++;

  if(*s=='-')
    min++;

  do
  {
    if(*s=='.') { numper++; continue; }
    if(isdigit(*s)) continue;
    return False;
  } while (*++s);
  
  if(numper>1)
    return False;
    
  return True;
}

int main(int argc, char *argv[])
{
  char line[LNLEN];
  char elem1[LNLEN],elem2[LNLEN],val[LNLEN];
  int i,j,k;
  char namelist[MAX_CYCS][MAX_CYCNAME];
  char appname[LNLEN]="xbanner";

  if (argc>1)
  {
    if(*argv[1]=='-')
    {
      if(strcmpi(argv[1],"-h")==0)
      {
        fprintf(stderr,"xb_check - check XBanner resource files etc.\n\
Usage: 'xb_check [-n appname] < filename' or 'cat file | xb_check [-n appname]'\n\
xb_check reads stdin for the lines, so you can type 'xrdb -q | xb_check or'\n\
even 'appres <progname/classname> | xb_check' ! You can use the option \n\
-n myxbanner to check lines starting with 'myxbanner.Resource' as well.\n");
#ifndef vms
	exit(0);
#else
	exit(SS$_NORMAL);
#endif
      }
      if(strcmp(argv[1],"-n")==0 && argc>2)
        strcpy(appname,argv[2]);
    }
  }

  while(gets(line)!=NULL)
  {
    if(line[0]=='!')
      continue;
    strcpy(elem1,elem(line,0));
    strcpy(elem2,elem(line,1));
    strcpy(val,value(line));
    for(i=0;XBres[i].opt!=TERM;i++)
    {
      if(strcmp(XBres[i].option_keyword,elem2)==0)
        break;
      if(strcmp(XBres[i].option_class,elem2)==0)
        break;
    }
    switch(XBres[i].opt)
    {
      /* cases that take a floating point */
      case PLASMA_GRAIN: case FGPL_GRAIN:
	if(!valid_float(val))
	  printf("Value '%s' is not a floating point.\n",val);
        break;
      /* cases that take a character string */
      case FONT: case FGC: case HICOLOR:
      case SHDC: case LABEL: case ULC:
      case PIXFIL: case BGPIXFN: case DUMPRESFIL:
      case BGC: 
        if(strcmp(XBres[i].option_keyword,elem2)==0)
        {
          if(strcmp(elem1,PRGCLASS)!=0 && strcmp("xbanner",elem1)!=0 &&
          	strcmp(elem1,appname)!=0)
            printf("Valid resource name with unknown program name:\n%s\n",line);
          if(val[0]=='\0')
            printf("No value in line containing valid resource name:\n%s\n",line);
        }
        else
        {
          if(strcmp(elem1,PRGCLASS)!=0 && strcmp("xbanner",elem1)!=0 &&
          	strcmp(elem1,appname)!=0)
            printf("Valid resource class name with unknown program name:\n%s\n",line);
          if(val[0]=='\0')
            printf("No value in line containing valid resource class name:\n%s\n",line);
        }
        if(val[0]=='#' && iscolorres(i) && !isvalidcolor(val))
          printf("Value of %s is invalid.\n%s\n",val,line);
        break;
      /* all cases that take a numerical value... */
      case THICKNESS:  case X: case Y:   case XOFFS:     case YOFFS:
      case SHADOWS:    case SURMIN:      case SURMAX:    case SHDXOFFS:
      case SHDYOFFS:   case ULTHICKNESS: case BARSIZE:   case PX: case PY:
      case GLINTMAX:   case GLINTMIN:    case FGGBARSIZ: case CYCSPEED:
      case GLINTSPEED: case GLINTMAXT:   case GLINTMINT: case FGCYCNUMCOL:
      case BGGRADREP:  case FGPL_NCOL:   case PLASMA_NCOL:
      case RIPPLES:    case RIPPLECOLORS:
        if(strcmp(XBres[i].option_keyword,elem2)==0)
        {
          if(strcmp(elem1,PRGCLASS)!=0 && strcmp("xbanner",elem1)!=0 &&
          	strcmp(elem1,appname)!=0)
            printf("Valid resource name with unknown program name:\n%s\n",line);
          if(val[0]=='\0')
            printf("No value in line containing valid resource name:\n%s\n",line);
	  if(!isnumber(val))
	    printf("Value in line requiring a numerical value is not numerical:\n%s\n",line);
        }
        else
        {
          if(strcmp(elem1,PRGCLASS)!=0 && strcmp("xbanner",elem1)!=0 &&
          	strcmp(elem1,appname)!=0)
            printf("Valid resource class name with unknown program name:\n%s\n",line);
          if(val[0]=='\0')
            printf("No value in line containing valid resource class name:\n%s\n",line);
	  if(!isnumber(val))
	    printf("Value in line requiring a numerical value is not numerical:\n%s\n",line);
        }
        break;
      case CORNERMASK:
        CheckCornerMask(val);
        break;
      case CYCLE: case CYCREV: case CYCFOR:
        j=split_names(val,namelist);
        for(k=0;k<j;k++)
          get_keyword(namelist[k],cycletype_keyword,"Unknown cycle type");
        break;
      case PLACE:
        for(j=0;placement_keyword[j]!=NULL;j++)
          if(strcmpi(placement_keyword[j],val)==0)
            break;
        if(placement_keyword[j]==NULL)
          printf("Unknown placement '%s'\n",val);
        break;
      case EFFECT:
        for(j=0;effect_keyword[j]!=NULL;j++)
          if(strcmpi(effect_keyword[j],val)==0)
            break;
        if(effect_keyword[j]==NULL)
          printf("Unknown effect '%s'\n",val);
        break;
      case UNDERLINED: case DOPIX: case CALC: case LINGER: case GLINT:
      case DUMPRES: case BGFILL: case AUTO_FILL: case SHOWERR:
        if(strcmpi(val,"true")!=0 && strcmpi(val,"false")!=0 &&
           strcmpi(val,"on")!=0   && strcmpi(val,"off")!=0   &&
           strcmpi(val,"yes")!=0  && strcmpi(val,"no")!=0   &&
           strcmpi(val,"1")!=0    && strcmpi(val,"0")!=0)
          printf("Value '%s' is invalid in line:\n%s\n",val,line);
        break;
      case BGSTYLE:
        for(j=0;background_keyword[j]!=NULL;j++)
          if(strcmpi(background_keyword[j],val)==0)
            break;
        if(background_keyword[j]==NULL)
          printf("Unknown background type '%s' in line:\n%s\n",val,line);
        break;
      case BGGRAD: case BKLTGRAD: case FADEGRAD: case FATTXTGRAD:
      case FGGRADGRAD: case FGPL_GRAD: case FGCYCGRAD:
        if(!valid_grad(val))
          printf("Color gradient spec '%s' is invalid.\n",val);
        break;
      case TERM:
        if(strcmp(elem1,PRGCLASS)==0 || strcmp("xbanner",elem1)==0 ||
        	strcmp(elem1,appname)==0)
          printf("Resource name '%s' is invalid in line:\n%s\n",elem2,line);
        break;
    }
  }
  return 0;
}

#if 0

Possible things:

Well formed lines:

 Strictly belongs:
 .<valid>:	  Value
 XBanner.<valid>: Value

 Not as strict:
 <name>.<valid>:  Value

Ill formed lines:

 XBanner.<!valid>: Value (or empty or !valid)
 xbanner.<!valid>: Value (or empty or !valid)

 .<valid>: <invalid value>

Unconnected:

 .<!valid>:	foo
 
#endif
