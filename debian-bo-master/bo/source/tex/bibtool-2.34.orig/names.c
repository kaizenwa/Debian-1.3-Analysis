/******************************************************************************
** $Id: names.c,v 0.00 1995/01/ 1 19:47:34 gerd Exp $
*******************************************************************************
** 
** This file is part of BibTool.
** It is distributed under the GNU General Public License.
** See the file COPYING for details.
** 
** (c) 1996 Gerd Neugebauer
** 
** Net: gerd@informatik.uni-koblenz.de
** 
******************************************************************************/

#include <stdio.h>
#include "bibtool.h"
#include "names.h"
#include "error.h"
#include "symbols.h"
#include "stack.h"
#include "sbuffer.h"
#include "s_parse.h"
#include "type.h"

/*****************************************************************************/
/* Internal Programs                                                         */
/*===========================================================================*/

#ifdef __STDC__
#define _ARG(A) A
#else
#define _ARG(A) ()
#endif
 NameNode name_format _ARG((char *s));		   /* names.c                */
 char * pp_names _ARG((char *s,NameNode format,char *trans,int max,char *namesep,char *etal));/* names.c*/
 static NameNode new_name_node _ARG((int type,int strip,char *pre,char *mid,char *post));/* names.c*/
 char * pp_list_of_names _ARG((char **wa,NameNode format,char *trans,int max,char *comma,char *and,char *namesep,char *etal));/* names.c*/
 static int is_jr _ARG((char * s));		   /* names.c                */
 static int is_lower_word _ARG((char *s));	   /* names.c                */
 static void free_name_node _ARG((NameNode node)); /* names.c                */
 static void initial _ARG((char *s,char *trans,int len,StringBuffer *sb));/* names.c*/
 static void pp_one_name _ARG((StringBuffer *sb,char **w,NameNode format,char *trans,int len,char *comma,int commas));/* names.c*/
 static void set_type _ARG((char **sp,char **midp));/* names.c               */
 void set_name_format _ARG((NameNode *nodep,char *s));/* names.c             */

/*****************************************************************************/
/* External Programs                                                         */
/*===========================================================================*/

/*---------------------------------------------------------------------------*/


/*-----------------------------------------------------------------------------
** Function:	new_name_node()
** Purpose:	Allocate a name node structure and fill it with the values 
**		given as arguments.
**
** Arguments:
**	type	
**	strip	
**	pre	
**	mid	
**	post	
** Returns:	
**___________________________________________________			     */
static NameNode new_name_node(type,strip,pre,mid,post)/*                     */
  int      type;				   /*                        */
  int      strip;				   /*                        */
  char     *pre;				   /*                        */
  char     *mid;				   /*                        */
  char     *post;				   /*                        */
{ NameNode new;					   /*                        */
 						   /*                        */
  if ( (new = (NameNode)malloc(sizeof(SNameNode))) == NameNULL )/*           */
  { OUT_OF_MEMORY("NameNode"); }	   	   /*                        */
  NameType(new)  = type;			   /*                        */
  NameStrip(new) = strip;			   /*                        */
  NamePre(new)   = pre;				   /*                        */
  NameMid(new)   = mid;				   /*                        */
  NamePost(new)  = post;			   /*                        */
  NextName(new)  = NameNULL;			   /*                        */
  return new;					   /*                        */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	free_name_node()
** Purpose:	
**		
**
** Arguments:
**	node
** Returns:	nothing
**___________________________________________________			     */
static void free_name_node(node)		   /*                        */
  NameNode node;				   /*                        */
{ NameNode next;				   /*                        */
 						   /*                        */
  while ( node )				   /*                        */
  { next = NextName(node);			   /*                        */
    free(node);					   /*                        */
    node = next;				   /*                        */
  }						   /*                        */
}						   /*------------------------*/

#ifdef BIBTEX_SYNTAX
/*-----------------------------------------------------------------------------
** Function:	set_type()
** Purpose:	
**		
**
** Arguments:
**	sp
**	midp
** Returns:	
**___________________________________________________			     */
static void set_type(sp,midp)			   /*                        */
  char **sp;					   /*                        */
  char **midp;					   /*                        */
{ char c, *s, *mid;				   /*                        */
  int type = 0;					   /*                        */
   						   /*                        */
  s = *sp;					   /*                        */
  if ( *(s+1) == '{' )				   /*                        */
  { mid = ++s;					   /*                        */
    while ( *s && *s != '}' ) s++;		   /*                        */
    c = *s; *s = '\0';				   /*                        */
    *midp = symbol(++mid);			   /*                        */
    *s = c;					   /*                        */
  }						   /*                        */
  *sp = s;					   /*                        */
}						   /*------------------------*/

#define SetType(T,C) type = T;				\
	c = *s; *s = '\0'; pre = symbol(pre); *s = c;	\
	if ( *(s+1) == C ) { strip = -1; s++; }		\
	set_type(&s,&mid);				\
	post = s+1

/*-----------------------------------------------------------------------------
** Function:	set_name_format()
** Purpose:	
**		
**
** Arguments:
**	nodep	
**	s	
** Returns:	nothing
**___________________________________________________			     */
void set_name_format(nodep,s)			   /*                        */
  NameNode *nodep;				   /*                        */
  char     *s;					   /*                        */
{ int      n,					   /*                        */
	   type,				   /*                        */
    	   strip;				   /*                        */
  char     *mid,				   /*                        */
	   *pre,				   /*                        */
	   *post,				   /*                        */
	   c;					   /*                        */
  char     *space;				   /*                        */
  char     buffer[256];				   /*                        */
#define UseBuffer(S) strcpy(buffer,S);s=buffer
 						   /*                        */
  space = symbol(" ");				   /*                        */
  free_name_node(*nodep);			   /*                        */
  *nodep = NameNULL;				   /*                        */
 						   /*                        */
  if      (strcmp(s,"short")==0) { UseBuffer("{ll{-}}"); }
  else if (strcmp(s,"med")  ==0) { UseBuffer("{f{}}{ll{-}}"); }
 						   /*                        */
  while ( *s  )					   /*                        */
  {						   /*                        */
    if (*s && *s != '{') 			   /*                        */
    { mid = s;					   /*                        */
      while ( *s && *s != '{' ) s++;		   /*                        */
      c = *s;					   /*                        */
      *s = '\0';				   /*                        */
      mid = symbol(mid);			   /*                        */
      *s = c;					   /*                        */
      *nodep = new_name_node(NameString,-1,0L,mid,0L);/*                     */
      nodep  = &NextName(*nodep);		   /*                        */
 						   /*                        */
      if ( c == '\0' ) return;			   /*                        */
    }						   /*                        */
 						   /*                        */
    pre   = ++s;				   /*                        */
    type  = NoName;				   /*                        */
    mid   = space;				   /*                        */
    strip = 1;					   /*                        */
    for(n=1 ;n>0; ++s )				   /*                        */
    { switch (*s)				   /*                        */
      { case '{': n++; break;			   /*                        */
	case '}': n--; break;			   /*                        */
        case 'f': SetType(NameFirst,'f'); break;   /*                        */
	case 'l': SetType(NameLast,'l');  break;   /*                        */
        case 'v': SetType(NameVon,'v');   break;   /*                        */
        case 'j': SetType(NameJr,'j');    break;   /*                        */
        case '\0': return;			   /*                        */
      }						   /*                        */
    }						   /*                        */
 						   /*                        */
    if ( type == NoName ) 			   /*                        */
    {
    }
    else					   /*                        */
    { c = *(s-1);				   /*                        */
      *(s-1) = '\0';				   /*                        */
      post = symbol(post);			   /*                        */
      *(s-1) = c;				   /*                        */
#ifdef DEBUG
      switch ( type )
      { case NameFirst:	fprintf(err_file,"f "); break;
        case NameLast:	fprintf(err_file,"l "); break;
        case NameVon:	fprintf(err_file,"v "); break;
        case NameJr: 	fprintf(err_file,"j "); break;
        default:	fprintf(err_file,"??"); break;
      }
      fprintf(err_file,				   /*                        */
	      "(%d) pre = \"%s\"  mid = \"%s\"  post = \"%s\"\n",/*          */
	     strip,pre,mid,post);		   /*                        */
#endif
      *nodep = new_name_node(type,strip,pre,mid,post);/*                     */
      nodep  = &NextName(*nodep);		   /*                        */
    }						   /*                        */
  }						   /*                        */
}						   /*------------------------*/

#endif

/*-----------------------------------------------------------------------------
** Function:	name_format()
** Purpose:	
**		
**
** Arguments:
**	s
** Returns:	
**___________________________________________________			     */
NameNode name_format(s)				   /*                        */
  char *s;					   /*                        */
{ int  type,					   /*                        */
       strip;					   /*                        */
  char *pre,					   /*                        */
       *mid,					   /*                        */
       *post;					   /*                        */
  char c;					   /*                        */
  char *cp;					   /*                        */
  NameNode node = NameNULL;			   /*                        */
  NameNode *nnp;				   /*                        */
#define OptionalArg(S)					\
  if (*cp=='[')						\
  { for ( S=++cp; *cp && *cp!=']'; cp++) ;		\
    c = *cp;						\
    *cp = '\0';						\
    if ( *cp ) { S = symbol(S); } else { S = empty; }	\
    *cp = c;						\
    if ( *cp ) cp++;					\
  } else { S = empty; }
 						   /*                        */
  nnp = &node;					   /*                        */
  cp = s;					   /*                        */
  while (*cp)					   /*                        */
  {						   /*                        */
    if ( *cp == '%' )				   /*                        */
    { strip = 0;				   /*                        */
      cp++;					   /*                        */
      if ( is_digit(*cp) )			   /*                        */
      { strip = 0;				   /*                        */
        while(is_digit(*cp)) { strip = strip*10 + (*cp++)-'0';}/*            */
      } else { strip = -1; }			   /*                        */
 						   /*                        */
      switch ( *cp )				   /*                        */
      { case 'f': type = NameFirst; break;	   /*                        */
        case 'l': type = NameLast; break;	   /*                        */
        case 'v': type = NameVon; break;	   /*                        */
        case 'j': type = NameJr; break;		   /*                        */
        default:  type = NoName;		   /*                        */
	  error(ERR_ERROR|ERR_POINT,		   /*                        */
		"Wrong type of format.",	   /*                        */
		(char*)0,(char*)0,s,cp,0,(char*)0);/*                        */
      }						   /*                        */
      cp++;					   /*                        */
      OptionalArg(pre);				   /*                        */
      OptionalArg(mid);				   /*                        */
      OptionalArg(post);			   /*                        */
    }						   /*                        */
    else 					   /*                        */
    { mid = cp;					   /*                        */
      while(*cp && *cp!='%') {cp++;}		   /*                        */
      c = *cp;					   /*                        */
      *cp = '\0';				   /*                        */
      mid = symbol(mid);			   /*                        */
      *cp = c;					   /*                        */
      type  = NameString;			   /*                        */
      pre   = NULL;				   /*                        */
      post  = NULL;				   /*                        */
      strip = -1;				   /*                        */
    }						   /*                        */
    if ( type != NoName )			   /*                        */
    { *nnp = new_name_node(type,strip,pre,mid,post);/*                       */
      nnp = &NextName(*nnp);			   /*                        */
    }						   /*                        */
#ifdef DEBUG
    if ( strip > 0 )				   /*                        */
    { fprintf(err_file,"%%%d",strip);		   /*                        */
    } else { fprintf(err_file,"%%");		   /*                        */
    }						   /*                        */
    switch ( type )				   /*                        */
    { case NameFirst:  c = 'f'; break;		   /*                        */
      case NameLast:   c = 'l'; break;		   /*                        */
      case NameVon:    c = 'v'; break;		   /*                        */
      case NameJr:     c = 'j'; break;		   /*                        */
      case NameString: c = 's'; break;		   /*                        */
      default:	       c = '?'; break;		   /*                        */
    }						   /*                        */
    fprintf(err_file,"%c[%s][%s][%s]\n",c,pre,mid,post);/*                   */
#endif
  }						   /*                        */
  return node;					   /*                        */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	pp_list_of_names()
** Purpose:	
**		
**
** Arguments:
**	wa	Word array of name constituents
**	format
**	comma	","
**	and	name separator
**	namesep
**	etal
** Returns:	
**___________________________________________________			     */
char * pp_list_of_names(wa,format,trans,max,comma,and,namesep,etal)/*        */
  char     **wa;				   /*                        */
  NameNode format;				   /*                        */
  char     *trans;				   /*                        */
  int      max;					   /*                        */
  char     *comma;				   /*                        */
  char     *and;				   /*                        */
  char     *namesep;				   /*                        */
  char     *etal;				   /*                        */
{ char     **w;					   /*                        */
  char     *word;				   /*                        */
  int      commas;				   /*                        */
  int      first = TRUE;			   /*                        */
  static StringBuffer*sb = (StringBuffer*)0;	   /*                        */
 						   /*                        */
  if ( sb == (StringBuffer*)0 			   /*                        */
      && (sb=sbopen()) == (StringBuffer*)0 )	   /*                        */
  { OUT_OF_MEMORY("name list"); }		   /*                        */
  else						   /*                        */
  { sbrewind(sb); }				   /*                        */
 						   /*                        */
  while ( *wa )					   /*                        */
  {						   /*                        */
    if ( *(wa+1) == NULL			   /* Look at the end        */
	 && strcmp(*wa,"others")==0 )		   /*  for `and others'      */
    { sbputs(etal,sb);				   /*                        */
      break;					   /*                        */
    }						   /*                        */
    if ( max >= 0 && --max < 0 )		   /*                        */
    { sbputs(etal,sb);				   /*                        */
      break;					   /*                        */
    }						   /*                        */
 						   /*                        */
    commas = 0;					   /*                        */
    for(w=wa; *w && *w != and; w++ )		   /*                        */
    { if( *w == comma) commas++;		   /*                        */
      DebugPrint1(*w);				   /*                        */
    }						   /*                        */
    word = *w;					   /*                        */
    *w = NULL;					   /*                        */
    if ( first ) { first = FALSE; }		   /*                        */
    else { sbputs(namesep,sb); }		   /*                        */
    pp_one_name(sb,wa,format,trans,(int)(w-wa),comma,commas);/*              */
    *w = word;					   /*                        */
    wa = ( word != NULL? ++w : w ) ;		   /*                        */
  }						   /*                        */
 						   /*                        */
  return sbflush(sb);				   /*                        */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	pp_one_name()
** Purpose:	
**		
**
** Arguments:
**	sb
**	w
**	len
**	comma
**	commas
** Returns:	nothing
**___________________________________________________			     */
static void pp_one_name(sb,w,format,trans,len,comma,commas)/*                */
  StringBuffer *sb;				   /*                        */
  char         **w;				   /*                        */
  NameNode     format;				   /*                        */
  char	       *trans;				   /*                        */
  int	       len;				   /*                        */
  char         *comma;				   /*                        */
  int          commas;				   /*                        */
{ NameNode     nn;				   /*                        */
  char         t;				   /*                        */
  char         *type;				   /*                        */
  int          i,j,again;			   /*                        */
  int	       first,last,von,jr;		   /*                        */
 						   /*                        */
  first = last = von = jr = 0;			   /*                        */
  if ( len < 1 ) return;			   /*                        */
 						   /*                        */
  if ( (type=(char*)malloc(len)) == NULL )	   /*                        */
  { OUT_OF_MEMORY("name type"); }   		   /*                        */
 						   /*                        */
#ifdef DEBUG
  for (i=0;i<len;i++) { printf("/%s/ ",w[i]); }
  puts("");
#endif
 						   /*                        */
  if ( commas == 0 )				   /*------------------------*/
  {						   /* ff vv ll               */
    j = len-1;				   	   /* ff vv ll jr            */
    if ( is_jr(w[j]) ) { type[j--] = 'j'; jr++; }  /*                        */
    if ( j >= 0 )      { type[j] = 'l'; last++; }  /*                        */
    i = 0;					   /*                        */
    while ( i<j && !is_lower_word(w[i]) )	   /*                        */
    { type[i++] = 'f'; first++; }		   /*                        */
    while ( i<j && is_lower_word(w[i]) )	   /*                        */
    { type[i++] = 'v'; von++; }			   /*                        */
    while ( i<j )				   /*                        */
    { type[i++] = 'l'; last++; }		   /*                        */
  }						   /*                        */
  else if (   commas == 1			   /*------------------------*/
	   && len > 2				   /* ff vv ll, jj           */
	   && w[len-2] == comma			   /*                        */
	   && is_jr(w[len-1]) )			   /*                        */
  { j = len-1;					   /*                        */
    type[j--] = 'j'; jr++;			   /*                        */
    type[j--] = ',';				   /*                        */
    if ( j >= 0 ) { type[j--] = 'l'; last++; }	   /*                        */
    i = 0;					   /*                        */
    while ( i<j && !is_lower_word(w[i]) )	   /*                        */
    { type[i++] = 'f'; first++; }		   /*                        */
    while ( i<j && is_lower_word(w[i]) )	   /*                        */
    { type[i++] = 'v'; von++; }			   /*                        */
    while ( i<j )				   /*                        */
    { type[i++] = 'l'; last++; }		   /*                        */
  }						   /*                        */
  else						   /*                        */
  {						   /*                        */
    i = 0;			   		   /*------------------------*/
    if ( is_lower_word(w[i]) )		   	   /* vv ll, ff              */
    { while ( i<len && w[i]!=comma && is_lower_word(w[i]) )/*                */
      { type[i++] = 'v'; von++; }		   /*                        */
    }						   /*                        */
			   			   /*------------------------*/
    while ( i<len && w[i]!=comma )		   /* ll, ff vv              */
    { type[i++] = 'l'; last++; }		   /*                        */
    type[i++] = ',';				   /*                        */
			   			   /*------------------------*/
    if ( commas == 2 && i<len && is_jr(w[i]) )	   /* ll, jj, ff vv          */
    { type[i] = 'j'; jr++;			   /*                        */
      type[++i] = ',';				   /*                        */
    }						   /*                        */
    while ( i<len && !is_lower_word(w[i]) )	   /*                        */
    { type[i++] = 'f'; first++; }		   /*                        */
    while ( i<len && is_lower_word(w[i]) )	   /*                        */
    { type[i++] = 'v'; von++; }			   /*                        */
  }						   /*                        */
 						   /*                        */
#ifdef DEBUG
  for (i=0;i<len;i++)
  { printf("%c ",type[i]);
  }
  puts("");
#endif
 						   /*                        */
  for ( nn=format;				   /*                        */
        nn != NameNULL;				   /*                        */
        nn=NextName(nn) )			   /*                        */
  { switch(NameType(nn))	   		   /*                        */
    { case NameFirst:	t = 'f'; j = first; break; /*                        */
      case NameLast:	t = 'l'; j = last;  break; /*                        */
      case NameVon:	t = 'v'; j = von;   break; /*                        */
      case NameJr:	t = 'j'; j = jr;    break; /*                        */
      case NameString:	j = 0; sbputs(NameMid(nn),sb); break;/*              */
      default:		t = ' '; j = 0;     break; /*                        */
    }						   /*                        */
    if ( j > 0 )				   /*                        */
    { sbputs(NamePre(nn),sb);			   /*                        */
      again = FALSE;				   /*                        */
      for ( i=0; i<len; i++ )			   /*                        */
      { if ( type[i] == t )			   /*                        */
	{ if ( again ) sbputs(NameMid(nn),sb);	   /*                        */
	  else again = TRUE;			   /*                        */
 						   /*                        */
	  initial(w[i],trans,NameStrip(nn),sb);	   /*                        */
	}					   /*                        */
      }						   /*                        */
      sbputs(NamePost(nn),sb);			   /*                        */
    }						   /*                        */
  }						   /*                        */
 						   /*                        */
  free(type);					   /*                        */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	initial()
** Purpose:	
**		
**
** Arguments:
**	s
**	sb
** Returns:	nothing
**___________________________________________________			     */
static void initial(s,trans,len,sb)		   /*                        */
  char         *s;				   /*                        */
  char         *trans;				   /*                        */
  int          len;				   /*                        */
  StringBuffer *sb;				   /*                        */
{ 						   /*                        */
  if ( len < 0 ) { sbputs(s,sb); return; }	   /*                        */
 						   /*                        */
  while ( len > 0 )				   /*                        */
  { if (*s=='\0') { return; }			   /*                        */
    if ( is_alpha(*s) )				   /*                        */
    { sbputc(trans[*s],sb);			   /*                        */
      len--;					   /*                        */
    }						   /*                        */
    s++;					   /*                        */
  }					   	   /*                        */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	is_jr()
** Purpose:	
**		
**
** Arguments:
**	s	string to consider
** Returns:	
**___________________________________________________			     */
static int is_jr(s)				   /*                        */
  char * s;					   /*                        */
{						   /*                        */
  switch ( ToLower(*s) )			   /*                        */
  { case 'j':					   /*                        */
      s++;					   /*                        */
      return ( case_cmp(s,"r")    ||		   /*                        */
	       case_cmp(s,"r.")   ||		   /*                        */
	       case_cmp(s,"unior") );	   	   /*                        */
    case 's':					   /*                        */
      s++;					   /*                        */
      return ( case_cmp(s,"en")   ||		   /*                        */
	       case_cmp(s,"en.")  ||	   	   /*                        */
	       case_cmp(s,"enior") );	   	   /*                        */
    case 'p':					   /*                        */
      s++;					   /*                        */
      return ( case_cmp(s,"hD")   ||		   /*                        */
	       case_cmp(s,"hD.")  );	   	   /*                        */
    case 'i':					   /*                        */
      s++;					   /*                        */
      return ( *s == '\0' ||			   /*                        */
	       strcmp(s,"I")    == 0 ||	   	   /*                        */
	       strcmp(s,"II")   == 0 ||	   	   /*                        */
	       strcmp(s,"V")    == 0 ||	   	   /*                        */
	       strcmp(s,"X")    == 0 );	   	   /*                        */
    case 'v':					   /*                        */
      s++;					   /*                        */
      return ( *s == '\0' ||			   /*                        */
	       strcmp(s,"I")    == 0 ||	   	   /*                        */
	       strcmp(s,"II")   == 0 ||	   	   /*                        */
	       strcmp(s,"III")  == 0 );	   	   /*                        */
    case 'x':					   /*                        */
      s++;					   /*                        */
      return ( *s == '\0' ||			   /*                        */
	       strcmp(s,"I")     == 0 ||	   /*                        */
	       strcmp(s,"II")    == 0 ||	   /*                        */
	       strcmp(s,"III") 	 == 0 ||	   /*                        */
	       strcmp(s,"IV")    == 0 ||	   /*                        */
	       strcmp(s,"V")     == 0 ||	   /*                        */
	       strcmp(s,"VI")    == 0 ||	   /*                        */
	       strcmp(s,"VII")   == 0 ||	   /*                        */
	       strcmp(s,"VIII")  == 0 ||	   /*                        */
	       strcmp(s,"IX")    == 0 ||	   /*                        */
	       strcmp(s,"X")     == 0 ||	   /*                        */
	       strcmp(s,"XI")    == 0 ||	   /*                        */
	       strcmp(s,"XII")   == 0 ||	   /*                        */
	       strcmp(s,"XIII")  == 0 ||	   /*                        */
	       strcmp(s,"XIV")   == 0 ||	   /*                        */
	       strcmp(s,"XV")    == 0 ||	   /*                        */
	       strcmp(s,"XVI")   == 0 ||	   /*                        */
	       strcmp(s,"XVII")  == 0 ||	   /*                        */
	       strcmp(s,"XVIII") == 0 ||	   /*                        */
	       strcmp(s,"XIX")   == 0 ||	   /*                        */
	       strcmp(s,"XX")    == 0 );	   /*                        */
    case 'e':					   /*                        */
      return ( case_cmp(++s,"sc") ||	   	   /*                        */
	       case_cmp(s,"sc.")  );		   /*                        */
  }						   /*                        */
  return FALSE;					   /*                        */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:	is_lower_word()
** Purpose:	Test if the word given as argument starts with a lower case
**		letter. Non letter characters are ignored.
**		TeX control sequences are partially ignored: 
**
** Arguments:
**	s	Word to test
** Returns:	TRUE or FALSE
**___________________________________________________			     */
static int is_lower_word(s)			   /*                        */
  register char *s;				   /*                        */
{ 						   /*                        */
  while(*s)					   /*                        */
  { if ( is_lower(*s) ) return TRUE;		   /*                        */
    if ( is_upper(*s) ) return FALSE;		   /*                        */
    if ( *s == '\\' ) { s++; if (*s) s++; }	   /*                        */
    else s++;					   /*                        */
  }						   /*                        */
  return FALSE;					   /*                        */
}						   /*------------------------*/


/*---------------------------------------------------------------------------*/

#ifdef STANDALONE

/*-----------------------------------------------------------------------------
** Function:	pp_names()
** Purpose:	Take a string consisting of a list of names, parse it and
**		reformat it according to a format specification.
**		This function is modelled according to the specification
**		for format.name$ in BibTeX.
** Arguments:
**	s	string to be parsed and reformatted.
**	format	
**	trans	translation table
**	max	
**	namesep	string to be inserted between names.
**	etal	string to be added instead of `and others'
** Returns:	a new symbol with the reformatted names.
**___________________________________________________			     */
char * pp_names(s,format,trans,max,namesep,etal)   /*                        */
  char     *s;					   /*                        */
  NameNode format;				   /*                        */
  char	   *trans;				   /*                        */
  int	   max;					   /*                        */
  char     *namesep;				   /*                        */
  char     *etal;				   /*                        */
{ char     *wp,					   /*                        */
           *comma,				   /*                        */
           *and;				   /*                        */
  int      brace,				   /*                        */
           n,					   /*                        */
           cc;					   /*                        */
  char     **words;				   /*                        */
 						   /*                        */
  if ( format == NameNULL ) return s;	   	   /*                        */
 						   /*                        */
  s = new_string(s);				   /*                        */
  wp    = s;					   /*                        */
  brace = 0;					   /*                        */
  n     = 0;					   /*                        */
  comma = ",";					   /*                        */
  and   = "&";					   /*                        */
 						   /*                        */
  for ( ; *s; s++ )				   /*                        */
  { switch(*s)					   /*                        */
    { case '{': brace++; break;			   /*                        */
      case '}': brace--; break;			   /*                        */
      case ',':					   /*                        */
        if ( brace < 1 )			   /*                        */
	{ *s = '\0';				   /*                        */
	  if (*wp) { push_string(wp); n++; }	   /*                        */
	  push_string(comma); n++;		   /*                        */
	  wp = s+1;				   /*                        */
	}					   /*                        */
        break;					   /*                        */
      case ' ': case '~': case '\t': case '\n':	   /*                        */
      case '\r':   				   /*                        */
        if ( brace < 1 )			   /*                        */
	{ *s = '\0';				   /*                        */
	  if (*wp)				   /*                        */
	  { push_string(strcmp(wp,"and")==0?and:wp);/*                       */
	    n++;				   /*                        */
	  }					   /*                        */
	  wp = s+1;				   /*                        */
	}					   /*                        */
        break;					   /*                        */
    }						   /*                        */
  }						   /*                        */
  if (*wp) { push_string(wp); n++; }		   /*                        */
  push_string(NULL);				   /*                        */
  n++;						   /*                        */
 						   /*                        */
  if ( (words=(char**)malloc(n*sizeof(char*))) == (char**)0 )/* Allocate     */
  { OUT_OF_MEMORY("Names"); }	   		   /* temp. array of words.  */
 						   /*                        */
  while( --n >= 0 )				   /* Transfer the stack into*/
  { words[n] = pop_string();			   /*  the word array.       */
  }						   /*                        */
  						   /*                        */
  wp = symbol(pp_list_of_names(words,		   /*                        */
			       format,		   /*                        */
			       trans,		   /*                        */
			       max,		   /*                        */
			       comma,		   /*                        */
			       and,		   /*                        */
			       namesep,etal));	   /*                        */
 						   /*                        */
  free(words);					   /*                        */
  free(s);					   /*                        */
  return wp;				   	   /*                        */
}						   /*------------------------*/

/*-----------------------------------------------------------------------------
** Function:    main()
** Purpose:     Main rountine
** Arguments:
**	argc	Number of arguments (+1)
**	argv	Array of command line arguments (and program name)
** Returns:     
**___________________________________________________                        */
int main(argc,argv)				   /*                        */
  int  argc;					   /*                        */
  char *argv[];					   /*                        */
{ char s[1024];					   /*                        */
  NameNode format = NameNULL;			   /*                        */
 						   /*                        */
  init_type();					   /*                        */
 						   /*                        */
  fputs("Format: ",stdout);			   /*                        */
  gets(s);					   /*                        */
 						   /*                        */
  format = name_format(s);		   	   /*                        */
 						   /*                        */
  while ( gets(s) ) 				   /*                        */
  { puts (pp_names(s,format,trans_lower,-1," and "," and others"));/*        */
  }						   /*                        */
}						   /*------------------------*/

 int rsc_quiet = 1;
/*

   gcc -g -DDEBUG -DSTANDALONE names.c stack.o symbols.o sbuffer.o type.o error.o -o names
   ./names
*/

#endif
