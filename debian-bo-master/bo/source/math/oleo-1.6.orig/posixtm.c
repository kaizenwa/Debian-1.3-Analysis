
/*  A Bison parser, made from /phydeaux/ni/lord/unsupported/oleo/posixtm.y with Bison version GNU Bison version 1.22
  */

#define YYBISON 1  /* Identify Bison output.  */

#define	DIGIT	258

#line 19 "/phydeaux/ni/lord/unsupported/oleo/posixtm.y"

#ifdef __GNUC__
#define alloca __builtin_alloca
#else
#if defined (HAVE_ALLOCA_H) || (defined(sparc) && (defined(sun) || (!defined(USG) && !defined(SVR4) && !defined(__svr4__))))
#include <alloca.h>
#else
#ifdef _AIX
 #pragma alloca
#else
char *alloca ();
#endif
#endif
#endif

#include <stdio.h>
#include <sys/types.h>
#include <time.h>

#define YYDEBUG 1

/* Lexical analyzer's current scan position in the input string. */
static char *curpos;

/* The return value. */
static struct tm t;

time_t mktime ();

#define psxtparse posixtime_psxtparse
static int psxtlex ();
static int psxterror ();

#ifndef YYLTYPE
typedef
  struct psxtltype
    {
      int timestamp;
      int first_line;
      int first_column;
      int last_line;
      int last_column;
      char *text;
   }
  psxtltype;

#define YYLTYPE psxtltype
#endif

#ifndef YYSTYPE
#define YYSTYPE int
#endif
#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	YYFINAL		15
#define	YYFLAG		-32768
#define	YYNTBASE	5

#define YYTRANSLATE(x) ((unsigned)(x) <= 258 ? psxttranslate[x] : 9)

static const char psxttranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     4,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     1,     2,     3
};

#if YYDEBUG != 0
static const short psxtprhs[] = {     0,
     0,     7,     9,    12,    13,    14,    17
};

static const short psxtrhs[] = {     8,
     8,     8,     8,     6,     7,     0,     8,     0,     8,     8,
     0,     0,     0,     4,     8,     0,     3,     3,     0
};

#endif

#if YYDEBUG != 0
static const short psxtrline[] = { 0,
    56,    85,    92,    99,   110,   113,   122
};

static const char * const psxttname[] = {   "$","error","$illegal.","DIGIT","'.'",
"date","year","seconds","digitpair",""
};
#endif

static const short psxtr1[] = {     0,
     5,     6,     6,     6,     7,     7,     8
};

static const short psxtr2[] = {     0,
     6,     1,     2,     0,     0,     2,     2
};

static const short psxtdefact[] = {     0,
     0,     0,     7,     0,     0,     4,     5,     2,     0,     1,
     3,     6,     0,     0,     0
};

static const short psxtdefgoto[] = {    13,
     7,    10,     2
};

static const short psxtpact[] = {     2,
     5,     2,-32768,     2,     2,     2,    -3,     2,     2,-32768,
-32768,-32768,     9,    10,-32768
};

static const short psxtpgoto[] = {-32768,
-32768,-32768,    -2
};


#define	YYLAST		10


static const short psxttable[] = {     4,
     9,     5,     6,     8,     1,    11,    12,     3,    14,    15
};

static const short psxtcheck[] = {     2,
     4,     4,     5,     6,     3,     8,     9,     3,     0,     0
};
/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/usr/unsupported/lib/bison.simple"

/* Skeleton output parser for bison,
   Copyright (C) 1984, 1989, 1990 Bob Corbett and Richard Stallman

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 1, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */


#ifndef alloca
#ifdef __GNUC__
#define alloca __builtin_alloca
#else /* not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__) || defined (__sparc) || defined (__sgi)
#include <alloca.h>
#else /* not sparc */
#if defined (MSDOS) && !defined (__TURBOC__)
#include <malloc.h>
#else /* not MSDOS, or __TURBOC__ */
#if defined(_AIX)
#include <malloc.h>
 #pragma alloca
#else /* not MSDOS, __TURBOC__, or _AIX */
#ifdef __hpux
#ifdef __cplusplus
extern "C" {
void *alloca (unsigned int);
};
#else /* not __cplusplus */
void *alloca ();
#endif /* not __cplusplus */
#endif /* __hpux */
#endif /* not _AIX */
#endif /* not MSDOS, or __TURBOC__ */
#endif /* not sparc.  */
#endif /* not GNU C.  */
#endif /* alloca not defined.  */

/* This is the parser code that is written into each bison parser
  when the %semantic_parser declaration is not specified in the grammar.
  It was written by Richard Stallman by simplifying the hairy parser
  used when %semantic_parser is specified.  */

/* Note: there must be only one dollar sign in this file.
   It is replaced by the list of actions, each action
   as one case of the switch.  */

#define psxterrok		(psxterrstatus = 0)
#define psxtclearin	(psxtchar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	return(0)
#define YYABORT 	return(1)
#define YYERROR		goto psxterrlab1
/* Like YYERROR except do call psxterror.
   This remains here temporarily to ease the
   transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto psxterrlab
#define YYRECOVERING()  (!!psxterrstatus)
#define YYBACKUP(token, value) \
do								\
  if (psxtchar == YYEMPTY && psxtlen == 1)				\
    { psxtchar = (token), psxtlval = (value);			\
      psxtchar1 = YYTRANSLATE (psxtchar);				\
      YYPOPSTACK;						\
      goto psxtbackup;						\
    }								\
  else								\
    { psxterror ("syntax error: cannot back up"); YYERROR; }	\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

#ifndef YYPURE
#define YYLEX		psxtlex()
#endif

#ifdef YYPURE
#ifdef YYLSP_NEEDED
#define YYLEX		psxtlex(&psxtlval, &psxtlloc)
#else
#define YYLEX		psxtlex(&psxtlval)
#endif
#endif

/* If nonreentrant, generate the variables here */

#ifndef YYPURE

int	psxtchar;			/*  the lookahead symbol		*/
YYSTYPE	psxtlval;			/*  the semantic value of the		*/
				/*  lookahead symbol			*/

#ifdef YYLSP_NEEDED
YYLTYPE psxtlloc;			/*  location data for the lookahead	*/
				/*  symbol				*/
#endif

int psxtnerrs;			/*  number of parse errors so far       */
#endif  /* not YYPURE */

#if YYDEBUG != 0
int psxtdebug;			/*  nonzero means print parse trace	*/
/* Since this is uninitialized, it does not stop multiple parsers
   from coexisting.  */
#endif

/*  YYINITDEPTH indicates the initial size of the parser's stacks	*/

#ifndef	YYINITDEPTH
#define YYINITDEPTH 200
#endif

/*  YYMAXDEPTH is the maximum size the stacks can grow to
    (effective only if the built-in stack extension method is used).  */

#if YYMAXDEPTH == 0
#undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
#define YYMAXDEPTH 10000
#endif

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
int psxtparse (void);
#endif

#if __GNUC__ > 1		/* GNU C and GNU C++ define this.  */
#define __psxt_bcopy(FROM,TO,COUNT)	__builtin_memcpy(TO,FROM,COUNT)
#else				/* not GNU C or C++ */
#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__psxt_bcopy (from, to, count)
     char *from;
     char *to;
     int count;
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#else /* __cplusplus */

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__psxt_bcopy (char *from, char *to, int count)
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#endif
#endif

#line 184 "/usr/unsupported/lib/bison.simple"
int
psxtparse()
{
  register int psxtstate;
  register int psxtn;
  register short *psxtssp;
  register YYSTYPE *psxtvsp;
  int psxterrstatus;	/*  number of tokens to shift before error messages enabled */
  int psxtchar1 = 0;		/*  lookahead token as an internal (translated) token number */

  short	psxtssa[YYINITDEPTH];	/*  the state stack			*/
  YYSTYPE psxtvsa[YYINITDEPTH];	/*  the semantic value stack		*/

  short *psxtss = psxtssa;		/*  refer to the stacks thru separate pointers */
  YYSTYPE *psxtvs = psxtvsa;	/*  to allow psxtoverflow to reallocate them elsewhere */

#ifdef YYLSP_NEEDED
  YYLTYPE psxtlsa[YYINITDEPTH];	/*  the location stack			*/
  YYLTYPE *psxtls = psxtlsa;
  YYLTYPE *psxtlsp;

#define YYPOPSTACK   (psxtvsp--, psxtssp--, psxtlsp--)
#else
#define YYPOPSTACK   (psxtvsp--, psxtssp--)
#endif

  int psxtstacksize = YYINITDEPTH;

#ifdef YYPURE
  int psxtchar;
  YYSTYPE psxtlval;
  int psxtnerrs;
#ifdef YYLSP_NEEDED
  YYLTYPE psxtlloc;
#endif
#endif

  YYSTYPE psxtval;		/*  the variable used to return		*/
				/*  semantic values from the action	*/
				/*  routines				*/

  int psxtlen;

#if YYDEBUG != 0
  if (psxtdebug)
    fprintf(stderr, "Starting parse\n");
#endif

  psxtstate = 0;
  psxterrstatus = 0;
  psxtnerrs = 0;
  psxtchar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  psxtssp = psxtss - 1;
  psxtvsp = psxtvs;
#ifdef YYLSP_NEEDED
  psxtlsp = psxtls;
#endif

/* Push a new state, which is found in  psxtstate  .  */
/* In all cases, when you get here, the value and location stacks
   have just been pushed. so pushing a state here evens the stacks.  */
psxtnewstate:

  *++psxtssp = psxtstate;

  if (psxtssp >= psxtss + psxtstacksize - 1)
    {
      /* Give user a chance to reallocate the stack */
      /* Use copies of these so that the &'s don't force the real ones into memory. */
      YYSTYPE *psxtvs1 = psxtvs;
      short *psxtss1 = psxtss;
#ifdef YYLSP_NEEDED
      YYLTYPE *psxtls1 = psxtls;
#endif

      /* Get the current used size of the three stacks, in elements.  */
      int size = psxtssp - psxtss + 1;

#ifdef psxtoverflow
      /* Each stack pointer address is followed by the size of
	 the data in use in that stack, in bytes.  */
#ifdef YYLSP_NEEDED
      /* This used to be a conditional around just the two extra args,
	 but that might be undefined if psxtoverflow is a macro.  */
      psxtoverflow("parser stack overflow",
		 &psxtss1, size * sizeof (*psxtssp),
		 &psxtvs1, size * sizeof (*psxtvsp),
		 &psxtls1, size * sizeof (*psxtlsp),
		 &psxtstacksize);
#else
      psxtoverflow("parser stack overflow",
		 &psxtss1, size * sizeof (*psxtssp),
		 &psxtvs1, size * sizeof (*psxtvsp),
		 &psxtstacksize);
#endif

      psxtss = psxtss1; psxtvs = psxtvs1;
#ifdef YYLSP_NEEDED
      psxtls = psxtls1;
#endif
#else /* no psxtoverflow */
      /* Extend the stack our own way.  */
      if (psxtstacksize >= YYMAXDEPTH)
	{
	  psxterror("parser stack overflow");
	  return 2;
	}
      psxtstacksize *= 2;
      if (psxtstacksize > YYMAXDEPTH)
	psxtstacksize = YYMAXDEPTH;
      psxtss = (short *) alloca (psxtstacksize * sizeof (*psxtssp));
      __psxt_bcopy ((char *)psxtss1, (char *)psxtss, size * sizeof (*psxtssp));
      psxtvs = (YYSTYPE *) alloca (psxtstacksize * sizeof (*psxtvsp));
      __psxt_bcopy ((char *)psxtvs1, (char *)psxtvs, size * sizeof (*psxtvsp));
#ifdef YYLSP_NEEDED
      psxtls = (YYLTYPE *) alloca (psxtstacksize * sizeof (*psxtlsp));
      __psxt_bcopy ((char *)psxtls1, (char *)psxtls, size * sizeof (*psxtlsp));
#endif
#endif /* no psxtoverflow */

      psxtssp = psxtss + size - 1;
      psxtvsp = psxtvs + size - 1;
#ifdef YYLSP_NEEDED
      psxtlsp = psxtls + size - 1;
#endif

#if YYDEBUG != 0
      if (psxtdebug)
	fprintf(stderr, "Stack size increased to %d\n", psxtstacksize);
#endif

      if (psxtssp >= psxtss + psxtstacksize - 1)
	YYABORT;
    }

#if YYDEBUG != 0
  if (psxtdebug)
    fprintf(stderr, "Entering state %d\n", psxtstate);
#endif

  goto psxtbackup;
 psxtbackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* psxtresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  psxtn = psxtpact[psxtstate];
  if (psxtn == YYFLAG)
    goto psxtdefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* psxtchar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (psxtchar == YYEMPTY)
    {
#if YYDEBUG != 0
      if (psxtdebug)
	fprintf(stderr, "Reading a token: ");
#endif
      psxtchar = YYLEX;
    }

  /* Convert token to internal form (in psxtchar1) for indexing tables with */

  if (psxtchar <= 0)		/* This means end of input. */
    {
      psxtchar1 = 0;
      psxtchar = YYEOF;		/* Don't call YYLEX any more */

#if YYDEBUG != 0
      if (psxtdebug)
	fprintf(stderr, "Now at end of input.\n");
#endif
    }
  else
    {
      psxtchar1 = YYTRANSLATE(psxtchar);

#if YYDEBUG != 0
      if (psxtdebug)
	{
	  fprintf (stderr, "Next token is %d (%s", psxtchar, psxttname[psxtchar1]);
	  /* Give the individual parser a way to print the precise meaning
	     of a token, for further debugging info.  */
#ifdef YYPRINT
	  YYPRINT (stderr, psxtchar, psxtlval);
#endif
	  fprintf (stderr, ")\n");
	}
#endif
    }

  psxtn += psxtchar1;
  if (psxtn < 0 || psxtn > YYLAST || psxtcheck[psxtn] != psxtchar1)
    goto psxtdefault;

  psxtn = psxttable[psxtn];

  /* psxtn is what to do for this token type in this state.
     Negative => reduce, -psxtn is rule number.
     Positive => shift, psxtn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (psxtn < 0)
    {
      if (psxtn == YYFLAG)
	goto psxterrlab;
      psxtn = -psxtn;
      goto psxtreduce;
    }
  else if (psxtn == 0)
    goto psxterrlab;

  if (psxtn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */

#if YYDEBUG != 0
  if (psxtdebug)
    fprintf(stderr, "Shifting token %d (%s), ", psxtchar, psxttname[psxtchar1]);
#endif

  /* Discard the token being shifted unless it is eof.  */
  if (psxtchar != YYEOF)
    psxtchar = YYEMPTY;

  *++psxtvsp = psxtlval;
#ifdef YYLSP_NEEDED
  *++psxtlsp = psxtlloc;
#endif

  /* count tokens shifted since error; after three, turn off error status.  */
  if (psxterrstatus) psxterrstatus--;

  psxtstate = psxtn;
  goto psxtnewstate;

/* Do the default action for the current state.  */
psxtdefault:

  psxtn = psxtdefact[psxtstate];
  if (psxtn == 0)
    goto psxterrlab;

/* Do a reduction.  psxtn is the number of a rule to reduce with.  */
psxtreduce:
  psxtlen = psxtr2[psxtn];
  if (psxtlen > 0)
    psxtval = psxtvsp[1-psxtlen]; /* implement default value of the action */

#if YYDEBUG != 0
  if (psxtdebug)
    {
      int i;

      fprintf (stderr, "Reducing via rule %d (line %d), ",
	       psxtn, psxtrline[psxtn]);

      /* Print the symbols being reduced, and their result.  */
      for (i = psxtprhs[psxtn]; psxtrhs[i] > 0; i++)
	fprintf (stderr, "%s ", psxttname[psxtrhs[i]]);
      fprintf (stderr, " -> %s\n", psxttname[psxtr1[psxtn]]);
    }
#endif


  switch (psxtn) {

case 1:
#line 62 "/phydeaux/ni/lord/unsupported/oleo/posixtm.y"
{
	         if (psxtvsp[-5] >= 1 && psxtvsp[-5] <= 12)
		   t.tm_mon = psxtvsp[-5] - 1;
		 else {
		   YYABORT;
		 }
		 if (psxtvsp[-4] >= 1 && psxtvsp[-4] <= 31)
		   t.tm_mday = psxtvsp[-4];
		 else {
		   YYABORT;
		 }
		 if (psxtvsp[-3] >= 0 && psxtvsp[-3] <= 23)
		   t.tm_hour = psxtvsp[-3];
		 else {
		   YYABORT;
		 }
		 if (psxtvsp[-2] >= 0 && psxtvsp[-2] <= 59)
		   t.tm_min = psxtvsp[-2];
		 else {
		   YYABORT;
		 }
	       ;
    break;}
case 2:
#line 85 "/phydeaux/ni/lord/unsupported/oleo/posixtm.y"
{
                   t.tm_year = psxtvsp[0];
		   /* Deduce the century based on the year.
		      See POSIX.2 section 4.63.3.  */
		   if (psxtvsp[0] <= 68)
		     t.tm_year += 100;
		 ;
    break;}
case 3:
#line 92 "/phydeaux/ni/lord/unsupported/oleo/posixtm.y"
{
                            t.tm_year = psxtvsp[-1] * 100 + psxtvsp[0];
			    if (t.tm_year < 1900) {
			      YYABORT;
			    } else
			      t.tm_year -= 1900;
			  ;
    break;}
case 4:
#line 99 "/phydeaux/ni/lord/unsupported/oleo/posixtm.y"
{
                    time_t now;
		    struct tm *tmp;

                    /* Use current year.  */
                    time (&now);
		    tmp = localtime (&now);
		    t.tm_year = tmp->tm_year;
		  ;
    break;}
case 5:
#line 110 "/phydeaux/ni/lord/unsupported/oleo/posixtm.y"
{
                        t.tm_sec = 0;
		      ;
    break;}
case 6:
#line 113 "/phydeaux/ni/lord/unsupported/oleo/posixtm.y"
{
	                  if (psxtvsp[0] >= 0 && psxtvsp[0] <= 61)
			    t.tm_sec = psxtvsp[0];
			  else {
			    YYABORT;
			  }
			;
    break;}
case 7:
#line 122 "/phydeaux/ni/lord/unsupported/oleo/posixtm.y"
{
                          psxtval = psxtvsp[-1] * 10 + psxtvsp[0];
			;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 465 "/usr/unsupported/lib/bison.simple"

  psxtvsp -= psxtlen;
  psxtssp -= psxtlen;
#ifdef YYLSP_NEEDED
  psxtlsp -= psxtlen;
#endif

#if YYDEBUG != 0
  if (psxtdebug)
    {
      short *ssp1 = psxtss - 1;
      fprintf (stderr, "state stack now");
      while (ssp1 != psxtssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

  *++psxtvsp = psxtval;

#ifdef YYLSP_NEEDED
  psxtlsp++;
  if (psxtlen == 0)
    {
      psxtlsp->first_line = psxtlloc.first_line;
      psxtlsp->first_column = psxtlloc.first_column;
      psxtlsp->last_line = (psxtlsp-1)->last_line;
      psxtlsp->last_column = (psxtlsp-1)->last_column;
      psxtlsp->text = 0;
    }
  else
    {
      psxtlsp->last_line = (psxtlsp+psxtlen-1)->last_line;
      psxtlsp->last_column = (psxtlsp+psxtlen-1)->last_column;
    }
#endif

  /* Now "shift" the result of the reduction.
     Determine what state that goes to,
     based on the state we popped back to
     and the rule number reduced by.  */

  psxtn = psxtr1[psxtn];

  psxtstate = psxtpgoto[psxtn - YYNTBASE] + *psxtssp;
  if (psxtstate >= 0 && psxtstate <= YYLAST && psxtcheck[psxtstate] == *psxtssp)
    psxtstate = psxttable[psxtstate];
  else
    psxtstate = psxtdefgoto[psxtn - YYNTBASE];

  goto psxtnewstate;

psxterrlab:   /* here on detecting error */

  if (! psxterrstatus)
    /* If not already recovering from an error, report this error.  */
    {
      ++psxtnerrs;

#ifdef YYERROR_VERBOSE
      psxtn = psxtpact[psxtstate];

      if (psxtn > YYFLAG && psxtn < YYLAST)
	{
	  int size = 0;
	  char *msg;
	  int x, count;

	  count = 0;
	  /* Start X at -psxtn if nec to avoid negative indexes in psxtcheck.  */
	  for (x = (psxtn < 0 ? -psxtn : 0);
	       x < (sizeof(psxttname) / sizeof(char *)); x++)
	    if (psxtcheck[x + psxtn] == x)
	      size += strlen(psxttname[x]) + 15, count++;
	  msg = (char *) malloc(size + 15);
	  if (msg != 0)
	    {
	      strcpy(msg, "parse error");

	      if (count < 5)
		{
		  count = 0;
		  for (x = (psxtn < 0 ? -psxtn : 0);
		       x < (sizeof(psxttname) / sizeof(char *)); x++)
		    if (psxtcheck[x + psxtn] == x)
		      {
			strcat(msg, count == 0 ? ", expecting `" : " or `");
			strcat(msg, psxttname[x]);
			strcat(msg, "'");
			count++;
		      }
		}
	      psxterror(msg);
	      free(msg);
	    }
	  else
	    psxterror ("parse error; also virtual memory exceeded");
	}
      else
#endif /* YYERROR_VERBOSE */
	psxterror("parse error");
    }

  goto psxterrlab1;
psxterrlab1:   /* here on error raised explicitly by an action */

  if (psxterrstatus == 3)
    {
      /* if just tried and failed to reuse lookahead token after an error, discard it.  */

      /* return failure if at end of input */
      if (psxtchar == YYEOF)
	YYABORT;

#if YYDEBUG != 0
      if (psxtdebug)
	fprintf(stderr, "Discarding token %d (%s).\n", psxtchar, psxttname[psxtchar1]);
#endif

      psxtchar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token
     after shifting the error token.  */

  psxterrstatus = 3;		/* Each real token shifted decrements this */

  goto psxterrhandle;

psxterrdefault:  /* current state does not do anything special for the error token. */

#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */
  psxtn = psxtdefact[psxtstate];  /* If its default is to accept any token, ok.  Otherwise pop it.*/
  if (psxtn) goto psxtdefault;
#endif

psxterrpop:   /* pop the current state because it cannot handle the error token */

  if (psxtssp == psxtss) YYABORT;
  psxtvsp--;
  psxtstate = *--psxtssp;
#ifdef YYLSP_NEEDED
  psxtlsp--;
#endif

#if YYDEBUG != 0
  if (psxtdebug)
    {
      short *ssp1 = psxtss - 1;
      fprintf (stderr, "Error: state stack now");
      while (ssp1 != psxtssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

psxterrhandle:

  psxtn = psxtpact[psxtstate];
  if (psxtn == YYFLAG)
    goto psxterrdefault;

  psxtn += YYTERROR;
  if (psxtn < 0 || psxtn > YYLAST || psxtcheck[psxtn] != YYTERROR)
    goto psxterrdefault;

  psxtn = psxttable[psxtn];
  if (psxtn < 0)
    {
      if (psxtn == YYFLAG)
	goto psxterrpop;
      psxtn = -psxtn;
      goto psxtreduce;
    }
  else if (psxtn == 0)
    goto psxterrpop;

  if (psxtn == YYFINAL)
    YYACCEPT;

#if YYDEBUG != 0
  if (psxtdebug)
    fprintf(stderr, "Shifting error token, ");
#endif

  *++psxtvsp = psxtlval;
#ifdef YYLSP_NEEDED
  *++psxtlsp = psxtlloc;
#endif

  psxtstate = psxtn;
  goto psxtnewstate;
}
#line 126 "/phydeaux/ni/lord/unsupported/oleo/posixtm.y"

static int
psxtlex ()
{
  char ch = *curpos++;

  if (ch >= '0' && ch <= '9')
    {
      psxtlval = ch - '0';
      return DIGIT;
    }
  else if (ch == '.' || ch == 0)
    return ch;
  else
    return '?';			/* Cause an error.  */
}

static int
psxterror ()
{
  return 0;
}

/* Parse a POSIX-style date and return it, or (time_t)-1 for an error.  */

time_t
posixtime (s)
     char *s;
{
  curpos = s;
  /* Let mktime decide whether it is daylight savings time.  */
  t.tm_isdst = -1;
  if (psxtparse ())
    return (time_t)-1;
  else
    return mktime (&t);
}

/* Parse a POSIX-style date and return it, or NULL for an error.  */

struct tm *
posixtm (s)
     char *s;
{
  if (posixtime (s) == -1)
    return NULL;
  return &t;
}
