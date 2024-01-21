/* -*- mode: C; mode: fold; -*- */
/* Standard intrinsic functions for S-Lang.  Included here are string
   and array operations */
/* Copyright (c) 1992, 1995 John E. Davis
 * All rights reserved.
 * 
 * You may distribute under the terms of either the GNU General Public
 * License or the Perl Artistic License.
 */

#include "config.h"
/*{{{ Include Files */

#include <stdio.h>
#include <time.h>

#ifndef __QNX__
# if defined(__GO32__) || defined(__WATCOMC__)
#  include <dos.h>
#  include <bios.h>
# endif
#endif

#include "slang.h"
#include "_slang.h"
#include "slarray.h"

#ifdef FLOAT_TYPE
#include <math.h>
#endif

#include <string.h>

/*}}}*/

      
/* builtin stack manipulation functions */
void SLdo_pop(void) /*{{{*/
{
   SLang_Object_Type x;
   if (SLang_pop(&x)) return;

   SLang_free_object (&x);
}

/*}}}*/
static int SLdo_dup(void) /*{{{*/
{
   SLang_Object_Type x;
   if (SLang_pop(&x)) return(0);
   SLang_push(&x);
   if (x.sub_type == STRING_TYPE) SLang_push_string(x.v.s_val);
   else
     {
	if (x.sub_type >= ARRAY_TYPE) x.v.uobj->count += 1;
	SLang_push (&x);
     }
   return(1);
}

/*}}}*/

/*{{{ Utility Functions */

static char Utility_Char_Table [256];
static void set_utility_char_table (char *pos) /*{{{*/
{
   register char *t = Utility_Char_Table, *tmax;
   register unsigned char ch;

   tmax = t + 256;
   while (t < tmax) *t++ = 0;
   
   t = Utility_Char_Table;
   while ((ch = (unsigned char) *pos++) != 0) t[ch] = 1;
}

/*}}}*/

static int generic_equals (void) /*{{{*/
{
   SLang_Object_Type obj1, obj2;
   int ret = 0;

   /* SLang_pop guarantees that if there is a stack underflow, the type will
    * come back 0. */
   SLang_pop_non_object (&obj1);
   SLang_pop_non_object (&obj2);
   if (obj1.sub_type == obj2.sub_type)
     switch (obj1.sub_type)
     {
      case 0:			       /* stack underflow */
	break;
      case INT_TYPE: ret = obj1.v.i_val == obj2.v.i_val; break;
#ifdef FLOAT_TYPE
      case FLOAT_TYPE: ret = obj1.v.f_val == obj2.v.f_val; break;
#endif
      case STRING_TYPE:
	ret = !strcmp (obj1.v.s_val, obj2.v.s_val);
	break;
      default:
	ret = obj1.v.l_val == obj2.v.l_val;
     }

   if (obj1.sub_type == STRING_TYPE)
     SLFREE (obj1.v.s_val);
   if (obj2.sub_type == STRING_TYPE)
     SLFREE (obj2.v.s_val);
   return ret;
}

/*}}}*/
static int pop_two_strings (char **a, char **b) /*{{{*/
{
   if (SLpop_string (b)) return 1;
   if (SLpop_string (a))
     {
	SLFREE (b);
	return 1;
     }
   return 0;
}

/*}}}*/

static void reverse_stack (int *count)
{
   (void) _SLreverse_stack (*count);
}

/*}}}*/

static void SLdo_strcat(void) /*{{{*/
{
   char *a, *b, *c;
   unsigned int len, lena;
   
   if (pop_two_strings (&a, &b)) return;

   lena = strlen(a);
   len = lena + strlen(b) + 1;
   
   if ((NULL == (c = (char *) SLREALLOC(a, len))))
     {
	SLang_Error = SL_MALLOC_ERROR;
	return;
     }
   
   strcpy (c + lena, b);

   SLang_push_malloced_string (c);
   
   SLFREE(b);
}

/*}}}*/

static int do_trim (char **beg, char **end, char *white) /*{{{*/
{
   int len;
   char *a, *b;
   
   set_utility_char_table (white);

   a = *beg;
   len = strlen (a);
   b = a + (len - 1);
   while (Utility_Char_Table[(unsigned char) *a]) a++;
   while ((b >= a) && (Utility_Char_Table[(unsigned char) *b])) b--;
   b++;
   
   len = (int) (b - a);
   *beg = a;
   *end = b;
   return len;
}

/*}}}*/

static void SLdo_strtrim(void) /*{{{*/
{
   char *a, *beg, *end;

   if (SLpop_string(&a)) return;

   beg = a;
   (void) do_trim (&beg, &end, " \t\n");

   /* Since a is a malloced string that will be freed, massage it. */
   *end = 0;
   SLang_push_string (beg);
   
   SLFREE(a);
}

/*}}}*/

static void SLdo_strcompress (void) /*{{{*/
{
   char *str, *white, *c;
   unsigned char *s, *beg, *end;
   unsigned int len;

   if (pop_two_strings (&str, &white)) return;
   
   beg = (unsigned char *) str;
   (void) do_trim ((char **) &beg, (char **) &end, white);
   SLFREE (white);

   /* Determine the effective length */
   len = 0;
   s = (unsigned char *) beg;
   while (s < end)
     {
	len++;
	if (Utility_Char_Table[*s++])
	  {
	     while ((s < end) && Utility_Char_Table[*s]) s++;
	  }
     }
   
   if (NULL != (c = (char *) SLMALLOC(len + 1)))
     {
	s = (unsigned char *) c;
	
	while (beg < end)
	  {
	     *s++ = *beg;
	     if (Utility_Char_Table[*beg++])
	       {
		  while ((beg < end) && Utility_Char_Table[*beg]) beg++;
	       }
	  }
	
	*s = 0;

	SLang_push_malloced_string(c);
     }
   else SLang_Error = SL_MALLOC_ERROR; /* SLang_doerror("Lang Malloc error."); */
   
   SLFREE(str);
}

/*}}}*/

static int str_replace (void) /*{{{*/
{
   char *orig, *rep, *s, *newstr, *match;
   int ret;
   unsigned int rep_len, match_len, new_len;
   
   if (SLpop_string (&rep)) return 0;
   if (pop_two_strings (&orig, &match))
     {
	SLFREE (rep);
	return 0;
     }
   
   new_len = strlen (orig);
   
   if ((NULL != (s = strstr (orig, match)))
       && (NULL != (newstr = SLmake_nstring (orig, new_len))))
     {
	match_len = strlen (match);
	rep_len = strlen (rep);
	if (rep_len > match_len)
	  {
	     new_len += rep_len - match_len;
	     if (NULL == (newstr = (char *) SLREALLOC (newstr, new_len + 1)))
	       SLang_Error = SL_MALLOC_ERROR;
	  }
	if (!SLang_Error)
	  {
	     char *s1 = newstr + (int) (s - orig);
	     
	     strcpy (s1 + rep_len, s + match_len);
	     SLMEMCPY (s1, rep, rep_len);
	     SLang_push_malloced_string (newstr);
	  }
	ret = 1;
     }
   else ret = 0;
   
   SLFREE (orig);
   SLFREE (match);
   SLFREE (rep);
   return ret;
}

/*}}}*/

/* This routine returns the string with text removed between single character
   comment delimiters from the set b and e. */

static void uncomment_string (char *str, char *b, char *e) /*{{{*/
{
   unsigned char chb, che;
   unsigned char *s, *cbeg, *mark;
   
   if (strlen(b) != strlen(e))
     {
	SLang_doerror ("Comment delimiter length mismatch.");
	return;
     }
   
   set_utility_char_table (b);
   
   if (NULL == (str = (char *) SLmake_string(str))) return;
   
   s = (unsigned char *) str;
   
   while ((chb = *s++) != 0)
     {
	if (Utility_Char_Table [chb] == 0) continue;
	
	mark = s - 1;

	cbeg = (unsigned char *) b;
	while (*cbeg != chb) cbeg++;
	
	che = (unsigned char) *(e + (int) (cbeg - (unsigned char *) b));
	
	while (((chb = *s++) != 0) && (chb != che));
	  
	if (chb == 0)
	  {
	     /* end of string and end not found.  Just truncate it a return; */
	     *mark = 0;
	     break;
	  }
	
	strcpy ((char *) mark, (char *)s);
	s = mark;
     }
   SLang_push_malloced_string (str);
}

/*}}}*/

static void SLquote_string (void) /*{{{*/
{
   char *str, *quotes, *q;
   int slash;
   unsigned int len;
   register char *t, *s, *q1;
   register unsigned char ch;
   
   if (SLang_pop_integer (&slash)) return;
   if ((slash > 255) || (slash < 0))
     {
	SLang_Error = SL_INVALID_PARM;
	return;
     }
   
   if (pop_two_strings (&str, &quotes)) return;
   
   /* setup the utility table to have 1s at quote char postitions. */
   set_utility_char_table (quotes);
   
   t = Utility_Char_Table;
   t[(unsigned int) slash] = 1;
   
   /* calculate length */
   s = str;
   len = 0;
   while ((ch = (unsigned char) *s++) != 0) if (t[ch]) len++;
   len += (unsigned int) (s - str);
   
   if (NULL != (q = (char *) SLMALLOC(len)))
     {
	s = str; q1 = q;
	while ((ch = (unsigned char) *s++) != 0)
	  {
	     if (t[ch]) *q1++ = slash;
	     *q1++ = (char) ch;
	  }
	*q1 = 0;
	SLang_push_malloced_string(q);
     }
   else SLang_Error = SL_MALLOC_ERROR;
   
   SLFREE (quotes);
   SLFREE (str);
}

/*}}}*/

/* returns the position of substrin in a string or null */
static void SLdo_issubstr(void) /*{{{*/
{
   char *a, *b, *c;
   int n;

   if (pop_two_strings (&a, &b)) return;

   if (NULL == (c = (char *) strstr(a, b))) n = 0; else n = 1 + (int) (c - a);

   SLFREE(a);
   SLFREE(b);
   SLang_push_integer (n);
}

/*}}}*/

/* returns to stack string at pos n to n + m of a */
static void SLdo_substr(void) /*{{{*/
{
   char *a, *b;
   int n, m;
   int lena;

   if (SLang_pop_integer(&m) || SLang_pop_integer(&n) || (SLpop_string(&a))) return;
   lena = strlen (a);
   if (n > lena) n = lena + 1;
   if (n < 1)
     {
	SLang_Error = SL_INVALID_PARM;
     }
   else
     {
	n--;
	if (m < 0) m = lena; 
	if (n + m > lena) m = lena - n;
	b = SLmake_nstring (a + n, (unsigned int) m);
	if (b != NULL)
	  SLang_push_malloced_string (b);
     }
   SLFREE(a);
}

/*}}}*/

/* substitute char m at positin string n in string*/
static void SLdo_strsub(void) /*{{{*/
{
   char *a;
   int n, m;
   unsigned int lena;
   
   if (SLang_pop_integer(&m) || SLang_pop_integer(&n) || (SLpop_string(&a))) 
     return;
   
   lena = strlen (a);
   
   if ((n <= 0) || (lena < (unsigned int) n))
     {
	SLang_Error = SL_INVALID_PARM;
	SLFREE(a);
	return;
     }
   
   a[n - 1] = (char) m;
   SLang_push_malloced_string (a);
}

/*}}}*/

static void SLdo_strup(void) /*{{{*/
{
   unsigned char c, *a;

   if (SLpop_string((char **) &a)) 
     return;
   
   SLang_push_malloced_string ((char *) a);
   
   while ((c = *a) != 0)
     {
	/* if ((*a >= 'a') && (*a <= 'z')) *a -= 32; */
	*a = UPPER_CASE(c);
	a++;
     }
}

/*}}}*/

static int do_upper (void) /*{{{*/
{
   int ch;
   
   if (SLang_pop_integer (&ch)) return -1;
   return UPPER_CASE(ch);
}

/*}}}*/

static int do_lower (void) /*{{{*/
{
   int ch;
   
   if (SLang_pop_integer (&ch)) return -1;
   return LOWER_CASE(ch);
}

/*}}}*/

static void SLdo_strlow(void) /*{{{*/
{
   unsigned char c, *a;

   if (SLpop_string((char **) &a)) return;
   
   SLang_push_malloced_string ((char *) a);

   while ((c = *a) != 0)
     {
	/* if ((*a >= 'a') && (*a <= 'z')) *a -= 32; */
	*a = LOWER_CASE(c);
	a++;
     }
}

/*}}}*/

static int do_strchop (void) /*{{{*/
{
   int delim, quote, count;
   char *str, *s0, *elm;
   register char *s1;
   register unsigned char ch;
   int quoted;

   if (SLang_pop_integer (&quote)
       || (SLang_pop_integer (&delim))
       || (SLpop_string (&str)))
     return 0;
   
   if ((quote < 0) || (quote > 255) 
       || (delim <= 0) || (delim > 255))
     {
	SLang_Error = TYPE_MISMATCH;
	SLFREE (str);
	return 0;
     }
   
   s1 = s0 = str;
   
   quoted = 0;
   count = 0;
   
   while (1)
     {
	ch = (unsigned char) *s1;
	if ((ch == quote) && quote)
	  {
	     s1++;
	     quoted = 1;
	     if (*s1 != 0) s1++;
	  }
	else if ((ch == delim) || (ch == 0))
	  {
	     if (NULL == (elm = SLmake_nstring (s0, (unsigned int)(s1 - s0))))
	       break;

	     /* No unquote it */
	     if (quoted)
	       {
		  register char ch1, *p, *p1;
		  
		  p = p1 = elm;
		  do
		    {
		       ch1 = *p1++;
		       if (ch1 == '\\') ch1 = *p1++;
		       *p++ = ch1;
		    }
		  while (ch1 != 0);
		  quoted = 0;
	       }
	     
	     SLang_push_malloced_string (elm);
	     if (SLang_Error) break;
	     count++;
	     if (ch == 0) break;
	     
	     s1++;		       /* skip past delim */
	     s0 = s1;		       /* and reset */
	  }
	else s1++;
     }

   SLFREE (str);
   if (SLang_Error)
     {
	while (count != 0) 
	  {
	     count--;
	     SLdo_pop ();
	  }
	count = 0;
     }
   return count;
}

/*}}}*/

static int do_strchopr (void) /*{{{*/
{
   int count;
   
   count = do_strchop ();
   if (count <= 0) return count;
   
   _SLreverse_stack (count);
   return count;
}

/*}}}*/

static void SLdo_strcmp(void) /*{{{*/
{
   char *a, *b;

   if (pop_two_strings (&a, &b)) return;

   SLang_push_integer (strcmp(a, b));

   SLFREE(a);
   SLFREE(b);
}

/*}}}*/

static void SLdo_strncmp(void) /*{{{*/
{
   char *a, *b;
   int n;
   
   if (SLang_pop_integer(&n) || pop_two_strings (&a, &b))
     return;
   
   if (n < 0)
     {
	SLang_Error = SL_INVALID_PARM;
     }
   else
     SLang_push_integer (strncmp(a, b, (unsigned int) n));

   SLFREE(a);
   SLFREE(b);
}

/*}}}*/

static void SLdo_strlen(void) /*{{{*/
{
   char *a;

   if (SLpop_string(&a)) return;

   SLang_push_integer ((int) strlen(a));

   SLFREE(a);
}

/*}}}*/

static int SLdo_isdigit(char *what) /*{{{*/
{
   if ((*what >= '0') && (*what <= '9')) return(1); else return(0);
}

/*}}}*/

/* convert object to integer form */
static void SLdo_int (void) /*{{{*/
{
   SLang_Object_Type x;
   int i;
   unsigned char stype;

   if (SLang_pop_non_object (&x)) return;
   stype = x.sub_type;

   if (stype == INT_TYPE)
     {
	SLang_push(&x);
	return;
     }
   else if (stype == STRING_TYPE)
     {
	stype = x.main_type;
	i = (int) *(unsigned char *) x.v.s_val;
	
	SLFREE(x.v.s_val);
     }
#ifdef FLOAT_TYPE
   else if (stype == FLOAT_TYPE)
     {
	i = (int) x.v.f_val;
     }
#endif
   else
     {
	SLang_Error = TYPE_MISMATCH;
	return;
     }
   SLang_push_integer(i);
}

/*}}}*/

static char Float_Format[16] = "%f";

static void set_float_format (char *s) /*{{{*/
{
   strncpy (Float_Format, s, 15);
   Float_Format[15] = 0;
}

/*}}}*/


/* Conver string to integer */
static void SLdo_integer(void) /*{{{*/
{
   char *a;
   int i;
   
   if (SLang_Error) return;
   if (SLpop_string(&a)) return;
   
   i = SLatoi((unsigned char *) a);
   if (SLang_Error)
     {
	SLang_Error = INTRINSIC_ERROR;
	SLang_doerror ("The string cannot be converted to an integer.");
     }
   else SLang_push_integer (i);
   SLFREE(a);
}

/*}}}*/

/* convert integer to a sring of length 1 */
static void SLdo_char(void) /*{{{*/
{
   char ch, buf[2];
   int x;

   if (SLang_pop_integer(&x)) return;

   ch = (char) x;
   buf[0] = ch;
   buf[1] = 0;
   SLang_push_string((char *) buf);
}

/*}}}*/

/* format object into a string */
char *SLstringize_object (SLang_Object_Type *obj) /*{{{*/
{
   unsigned char stype;
   char *s;
   char buf [256];
   SLang_Class_Type *cl;

   buf[0] = '?'; buf[1] = 0;
   s = buf;
   
   stype = obj->sub_type;
   if (obj->main_type != 0) switch (stype)
     {
      case STRING_TYPE:
	s = obj->v.s_val;
	break;
	
      case INT_TYPE:
	sprintf(buf, "%d", obj->v.i_val);
	break;

#ifdef FLOAT_TYPE
      case FLOAT_TYPE:
	sprintf(buf, Float_Format,  obj->v.f_val);
	break;
#endif
      default:
	cl = SLang_Registered_Types[stype];
	if ((cl != NULL) && (cl->string != NULL)
	    && (NULL != (s = (*cl->string) ((VOID_STAR) obj->v.uobj))))
	  {
	     return s;
	  }
     }
   return SLmake_string (s);
}

/*}}}*/

static void SLdo_string(void) /*{{{*/
{
   SLang_Object_Type x;
   
   if (SLang_pop (&x)) return;
   SLang_push_malloced_string (SLstringize_object (&x));
   
   SLang_free_object (&x);
}

/*}}}*/

/* probably more useful to have a argc, argv thing */
int SLang_run_hooks(char *hook, char *opt1, char *opt2) /*{{{*/
{
   int ret = 0;

   if (SLang_Error || !SLang_is_defined(hook)) return(0);
   if (opt1 != NULL) SLang_push_string(opt1);
   if (opt2 != NULL) SLang_push_string(opt2);
   if (!SLang_Error) ret = SLang_execute_function(hook);
   return (ret && !SLang_Error);
}

/*}}}*/

static void lang_getenv_cmd(char *s) /*{{{*/
{
   char *t;
   if (NULL == (t = getenv(s))) t = "";
   SLang_push_string(t);
}

/*}}}*/

#ifdef HAVE_PUTENV
static void lang_putenv_cmd(void) /*{{{*/
{
   char *s;

   if (SLpop_string(&s)) return;

   if (putenv (s))
     {
	SLang_Error = INTRINSIC_ERROR;
	SLFREE (s);
     }
   
   /* Note that s is NOT freed */
}

/*}}}*/

#endif

static int lang_apropos1(char *s, SLang_Name_Type *table, int max) /*{{{*/
{
   int all = 0, n = 0;
   char *nm;

   if (*s == 0) all = 1;

   while(max && (nm = table->name, *nm != 0))
     {
	nm++;  /* lose hash */
	if ((*nm != 1) && (all || (NULL != strstr(nm, s))))
	  {
	     n++;
	     SLang_push_string (nm);
	     if (SLang_Error) return 1;
	  }
	table++;
	max--;
     }
   return n;
}

/*}}}*/

static void lang_apropos(char *s) /*{{{*/
{
   int n;
   SLName_Table *nt;
   
   n = lang_apropos1(s, SLang_Name_Table, SLANG_MAX_SYMBOLS);
   nt = SLName_Table_Root;
   while (nt != NULL)
     {
	n += lang_apropos1(s, nt->table, nt->n);
	nt = nt->next;
     }
   SLang_push_integer(n);
}

/*}}}*/

static int get_object_type (void) /*{{{*/
{
   SLang_Object_Type obj;
   
   if (SLang_pop (&obj))
     return -1;
   SLang_push (&obj);
   return obj.sub_type;
}

/*}}}*/

static void lang_print_stack (void) /*{{{*/
{
   SLang_Object_Type *x = SLStack_Pointer;
   int n;
   char *b, *t;
   char buf[132];
   char buf2[40];
   
   while (--x >= SLRun_Stack)
     {
	b = buf;
	n = (int) (x - SLRun_Stack);
	switch (x->sub_type)
	  {
	   case STRING_TYPE: 
	     b = x->v.s_val;
	     t = "(Str)"; break;
	   case INT_TYPE: sprintf(buf, "%d", x->v.i_val); t = "(Int)"; break;
#ifdef FLOAT_TYPE
	   case FLOAT_TYPE: 
	     sprintf(buf, Float_Format, x->v.f_val); t = "(float)"; break;
#endif
	   case SLANG_OBJ_TYPE: 
	     b = (char *) (x->v.n_val)->name + 1;
	     t = "(Ptr)";
	     break;
	   case ARRAY_TYPE:
	     *buf = 0;		       /* I could give some info here */
	     t = "(Array)";
	     break;
	   default: t = "(Unknown)"; *buf = 0;
	  }
	sprintf(buf2, "(%d) %s:", n, t);
	
	(*SLang_Dump_Routine)(buf2);
	(*SLang_Dump_Routine)(b);
	*buf = '\n'; *(buf + 1) = 0;
	(*SLang_Dump_Routine)(buf);
     }
}

/*}}}*/

/* sprintf functionality for S-Lang */

static char *SLdo_sprintf(char *fmt) /*{{{*/
{
   register char *p = fmt, ch;
   char *out = NULL, *outp = NULL;
   char dfmt[80];		       /* used to hold part of format */
   char *f;
   unsigned char stmp, ttmp;
   long *varp;
   int var, want_width, width, precis, use_varp;
   unsigned int len = 0, malloc_len = 0, dlen;
   int do_free, guess_size;
#ifdef FLOAT_TYPE
   int tmp1, tmp2, use_float;
   float64 x;
#endif
   
   
   while (1)
     {
	while ((ch = *p) != 0)
	  {
	     if (ch == '%')
	       break;
	     p++;
	  }
	
	/* p points at '%' or 0 */
	
	dlen = (unsigned int) (p - fmt);
	
	if (len + dlen >= malloc_len)
	  {
	     malloc_len = len + dlen;
	     if (out == NULL) outp = (char *) SLMALLOC(malloc_len + 1);
	     else outp = (char *) SLREALLOC(out, malloc_len + 1);
	     if (NULL == outp)
	       {
		  SLang_Error = SL_MALLOC_ERROR;
		  return out;
	       }
	     out = outp;
	     outp = out + len;
	  }
	
	strncpy(outp, fmt, dlen);
	len += dlen;
	outp = out + len;
	*outp = 0;
	if (ch == 0) break;

	/* bump it beyond '%' */
	++p;
	fmt = p;

	f = dfmt;
	*f++ = ch;
	/* handle flag char */
	ch = *p++;
	if ((ch == '-') || (ch == '+') || (ch == ' ') || (ch == '#'))
	  {
	     *f++ = ch;
	     ch = *p++;
	  }
	
	/* width */
	/* I have got to parse it myself so that I can see how big it needs
	   to be. */
	want_width = width = 0;
	if (ch == '*')
	  {
	     if (SLang_pop_integer(&width)) return (out);
	     want_width = 1;
	     ch = *p++;
	  }
	else 
	  {
	     if (ch == '0') 
	       {
		  *f++ = '0';
		  ch = *p++;
	       }
	     
	     while ((ch <= '9') && (ch >= '0'))
	       {
		  width = width * 10 + (ch - '0');
		  ch = *p++;
		  want_width = 1;
	       }
	  }
	
	if (want_width)
	  {
	     sprintf(f, "%d", width);
	     while (*f) f++;
	  }
	precis = 0;
	/* precision -- also indicates max number of chars from string */
	if (ch == '.')
	  {
	     *f++ = ch;
	     ch = *p++;
	     want_width = 0;
	     if (ch == '*')
	       {
		  if (SLang_pop_integer(&precis)) return (out);
		  ch = *p++;
		  want_width = 1;
	       }
	     else while ((ch <= '9') && (ch >= '0'))
	       {
		  precis = precis * 10 + (ch - '0');
		  ch = *p++;
		  want_width = 1;
	       }
	     if (want_width)
	       {
		  sprintf(f, "%d", precis);
		  while (*f) f++;
	       }
	     else precis = 0;
	  }
	
	/* not supported */
	if ((ch == 'l') || (ch == 'h')) ch = *p++;
	
	var = 0;
	varp = 0;
	guess_size = 32;
#ifdef FLOAT_TYPE
	use_float = 0;
#endif
	use_varp = 0;
	do_free = 0;
	
	/* Now the actual format specifier */
	switch(ch)
	  {
	     case 's': 
	     if (SLpop_string((char **) &varp)) return (out);
	     do_free = 1;
	     guess_size = strlen((char *) varp);
	     use_varp = 1;
	     break;

#if 1
	   case '%': 
	     guess_size = 1;
	     do_free = 0;
	     use_varp = 1;
	     varp = (long *) "%";
	     break;
#endif
	     
	     case 'c': guess_size = 1;
	     /* drop */
	     case 'd':
	     case 'i': 
	     case 'o': 
	     case 'u': 
	     case 'X': 
	     case 'x':
	     if (SLang_pop_integer(&var)) return(out);
	     break;
	     
	     case 'f': 
	     case 'e': 
	     case 'g': 
	     case 'E': 
	     case 'G': 
#ifdef FLOAT_TYPE
	     if (SLang_pop_float(&x, &tmp1, &tmp2)) return (out);
	     use_float = 1;
	     guess_size = 64;
	     (void) tmp1; (void) tmp2;
	     break;
#endif
	     case 'p': 
	     guess_size = 32;
	     if (NULL == (varp = SLang_pop_pointer(&stmp, &ttmp, &do_free)))
	       {
		  return (out);
	       }
	     (void) stmp; (void) ttmp;
	     use_varp = 1;
	     break;
	     
	   default: 
	     SLang_doerror("Invalid Format.");
	     return(out);
	  }
	*f++ = ch; *f = 0;
	
	width = width + precis;
	if (width > guess_size) guess_size = width;
	
	if (len + guess_size > malloc_len)
	  {
	     outp = (char *) SLREALLOC(out, len + guess_size + 1);
	     if (outp == NULL) 
	       {
		  SLang_Error = SL_MALLOC_ERROR;
		  return (out);
	       }
	     out = outp;
	     outp = out + len;
	     malloc_len = len + guess_size;
	  }
	
	if (use_varp)
	  {
	     sprintf(outp, dfmt, varp);
	     if (do_free) SLFREE(varp);
	  }
#ifdef FLOAT_TYPE
	else if (use_float) sprintf(outp, dfmt, x);
#endif
	else sprintf(outp, dfmt, var);
	
	len += strlen(outp);
	outp = out + len;
	fmt = p;
     }

   if (out != NULL)
     {
	outp = (char *) SLREALLOC(out, (unsigned int) (outp - out) + 1);
	if (outp != NULL) out = outp;
     }
   
   return (out);
}

/*}}}*/

static void SLsprintf(void) /*{{{*/
{
   register char *p, ch, *b;
   char buf[256], ch1, *fmt;
   int n = 1;
   SLang_Object_Type *ptr;
   
   if (SLang_pop_integer (&n)) return;
   if (NULL == (ptr = _SLreverse_stack (n + 1))) return;
   if (SLpop_string(&fmt)) return;
   
   strncpy(buf, fmt, 255);
   SLFREE(fmt);

   buf[255] = 0;
   p = b = buf;
   
   while ((ch = *p++) != 0)
     {
	if (ch == '\\')
	  {
	     p = SLexpand_escaped_char(p, &ch1);
	     if (SLang_Error) return;
	     ch = ch1;
	  }
	/* else if ((ch == '%') && (*p == '%')) p++; */
	*b++ = ch;
     }
   *b = 0;

   p = SLdo_sprintf(buf);
   
   while (SLStack_Pointer > ptr) SLdo_pop();
   
   if (SLang_Error)
     {
	if (p != NULL) SLFREE(p);
     }
   
   if (p != NULL) SLang_push_malloced_string (p);
}

/*}}}*/

/* converts string s to a form that can be used in an eval */
static void make_printable_string(char *s) /*{{{*/
{
   unsigned int len;
   register char *s1 = s, ch, *ss1;
   char *ss;
   
   /* compute length */
   len = 3;
   while ((ch = *s1++) != 0)
     {
	if ((ch == '\n') || (ch == '\\') || (ch == '"')) len++;
	len++;
     }
   if (NULL == (ss = (char *) SLMALLOC(len))) 
     {
	SLang_Error = SL_MALLOC_ERROR;
	return;
     }
   s1 = s;
   ss1 = ss;
   *ss1++ = '"';
   while ((ch = *s1++) != 0)
     {
	if (ch == '\n')
	  {
	     ch = 'n';
	     *ss1++ = '\\';
	  }
	else if ((ch == '\\') || (ch == '"'))
	  {
	     *ss1++ = '\\';
	  }
	*ss1++ = ch;
     }
   *ss1++ = '"';
   *ss1 = 0;
   SLang_push_string(ss);
}

/*}}}*/
   
char *SLang_extract_list_element(char *list, int *nth, int *delim) /*{{{*/
{
   int d = *delim, n = *nth;
   static char elem[256];
   char *el = elem;
   
   while (n-- > 0)
     {
	while (*list && (*list != (char) d)) list++;
	if (*list == 0) break;
	list++;
     }
   n = 255;
   while (n-- && *list && (*list != (char) d)) *el++ = *list++;
   *el = 0;
   return (elem);
}

/*}}}*/

static int SLang_is_list_element(char *list, char *elem, int *delim) /*{{{*/
{
   int d = *delim, n;
   unsigned int len;
   char *l = list;
   
   len = strlen (elem);
   n = 1;
   while (1)
     {
	while (*l && (*l != (char) d)) l++;
	if ((list + len == l) && (!strncmp (elem, list, len))) return n;
	if (*l == 0) break;
	list = l = l + 1;
	n++;
     }
   return (0);
}

/*}}}*/

/* Regular expression routines for strings */
static SLRegexp_Type regexp_reg;

static int string_match (char *str, char *pat, int *np) /*{{{*/
{
   int n = *np;
   unsigned int len;
   unsigned char rbuf[512], *match;

   regexp_reg.case_sensitive = 1;
   regexp_reg.buf = rbuf;
   regexp_reg.pat = (unsigned char *) pat;
   regexp_reg.buf_len = sizeof (rbuf);

   if (SLang_regexp_compile (&regexp_reg))
     {
	SLang_doerror ("Unable to compile pattern.");
	return 0;
     }
   
   n--;
   len = strlen(str);
   if ((n < 0) || ((unsigned int) n >= len))
     {
	/* SLang_Error = SL_INVALID_PARM; */
	return 0;
     }

   str += n;
   len -= n;
   
   if (NULL == (match = SLang_regexp_match((unsigned char *) str, len, &regexp_reg))) return 0;
   
   /* adjust offsets */
   regexp_reg.offset = n;
   
   return (1 + (int) ((char *) match - str));
}

/*}}}*/

static int string_match_nth(int *np) /*{{{*/
{
   int n = *np, beg;
   
   if ((n < 0) || (n > 9) || (regexp_reg.pat == NULL)
       || ((beg = regexp_reg.beg_matches[n]) == -1))
     {
	SLang_Error = SL_INVALID_PARM;
	return 0;
     }
   SLang_push_integer(beg + regexp_reg.offset);
   return regexp_reg.end_matches[n];
}

/*}}}*/

static int my_system (char *s) /*{{{*/
{
   return system (s);
}

/*}}}*/

#include <time.h>

#if defined(__GO32__) || (!defined(__QNX__) && defined(__WATCOMC__))
static char *djgpp_current_time (void) /*{{{*/
{
   union REGS rg;
   unsigned int year;
   unsigned char month, day, weekday, hour, minute, sec;
   char days[] = "SunMonTueWedThuFriSat";
   char months[] = "JanFebMarAprMayJunJulAugSepOctNovDec";
   static char the_date[26];
   
   rg.h.ah = 0x2A;
#ifndef __WATCOMC__
   int86(0x21, &rg, &rg);
   year = rg.x.cx & 0xFFFF;
#else
   int386(0x21, &rg, &rg);
   year = rg.x.ecx & 0xFFFF;
#endif    
  
   month = 3 * (rg.h.dh - 1);
   day = rg.h.dl;
   weekday = 3 * rg.h.al;
   
   rg.h.ah = 0x2C;
   
#ifndef __WATCOMC__
   int86(0x21, &rg, &rg);
#else
   int386(0x21, &rg, &rg);
#endif
  
   hour = rg.h.ch;
   minute = rg.h.cl;
   sec = rg.h.dh;
   
   /* we want this form: Thu Apr 14 15:43:39 1994\n  */
   sprintf(the_date, "%.3s %.3s%3d %02d:%02d:%02d %d\n",
	   days + weekday, months + month, 
	   day, hour, minute, sec, year);
   return the_date;
}

/*}}}*/

#endif

char *SLcurrent_time_string (void) /*{{{*/
{
   char *the_time;
#ifndef __GO32__
   time_t myclock;
   
   myclock = time((time_t *) 0);
   the_time = (char *) ctime(&myclock);
#else
   the_time = djgpp_current_time ();
#endif
   /* returns the form Sun Sep 16 01:03:52 1985\n\0 */
   the_time[24] = '\0';
   return(the_time);
}

/*}}}*/

static int load_file (void) /*{{{*/
{
   char *s;
   int ret;
   
   if (SLpop_string (&s)) return 0;
   ret = SLang_load_file (s);
   SLFREE (s);
   return ret;
}

/*}}}*/

/* These are here to let interpreter script have access to these values. */
static int Integer_Type_Var = INT_TYPE;
static int String_Type_Var = STRING_TYPE;
#ifdef FLOAT_TYPE
static int Float_Type_Var = FLOAT_TYPE;
#endif

SLang_Name_Type SLang_Basic_Table[] = /*{{{*/
{
   MAKE_INTRINSIC(".autoload",  SLang_autoload, VOID_TYPE, 2),
   /* Prototype: Void autoload(String fun, String file);
    * This function simply declares function @fun@ to the interpreter.  When
    * @fun@ is actually called, its actual function definition will be loaded
    * from @file@.  For example,
    * @ autoload ("bessel_j0", "bessel.sl");
    * tells the interpreter that the function definition for @bessel_j0@ from 
    * is to be loaded from the file @bessel.sl@ when the function is executed.
    * Related Functions: @evalfile@ */
   MAKE_INTRINSIC(".pop",  SLdo_pop, VOID_TYPE, 0),
   /* Prototype: Void pop ();
    * @pop@ is used to remove the top object from the S-Lang stack.  It is 
    * typically used to ignore values from function that return a value.  For
    * example, the @fflush@ function which is used to flush an output stream
    * returns an integer value that indicates whether it was sucessful or not.
    * Usually, it is safe to ignore this value and one simple writes
    * @ fflush (stdout); pop ();
    * Related Functions: @dup@
    */
   
   MAKE_INTRINSIC(".strcmp",  SLdo_strcmp, VOID_TYPE, 0),
   /* Prototype: Integer strcmp (String a, String b);
    * @strcmp@ performs a case sensitive comparison between two strings.  It
    * returns 0 if the strings are identical, a negative number if @a@ is less
    * than @b@, or a positive result if @a@ is greater than @b@ (in a
    * lexicographic sense).
    * 
    * To perform a case insensitive comparison, one can use the @strup@ function
    * to convert both parameters @a@ and @b@ to uppercase before calling @strcmp@.
    * Related Functions: @strup@, @strlow@, @strncmp@ */
   MAKE_INTRINSIC(".strcat",  SLdo_strcat, VOID_TYPE, 0),
   /* Prototype: String strcat(String a, String b);
    * This function takes two strings, @a@ and @b@, concatenates them together
    * and returns the result.  For example,
    * @ strcat ("Hello ", "World");
    * produces the string @"Hello World"@.
    * 
    * Related Functions: @Sprintf@ */
   MAKE_INTRINSIC(".strlen",  SLdo_strlen, VOID_TYPE, 0),
   /* Prototype: Integer strlen (String a);
    * The @strlen@ returns the length of the string @a@.  The length of a
    * string is simply the number of characters that comprise the string.
    * 
    * Related Functions: @char@
    */
   MAKE_INTRINSIC(".strchop", do_strchop, INT_TYPE, 0),
   /* Prototype: Integer strchop (String str, Integer delim, Integer quote);
    * This routine decomposes a string @str@ into a series of substrings 
    * and returns the substrings to the stack along with the number of substrings.
    * The string @str@ is assumed to consist of the substrings delimited 
    * by @delim@.  The character @quote@ is assumed to quote the delimiter.
    * For example,
    * @ strchop ("apples,oranges,pears", ',', 0);
    * will return @3@ to the top of the stack followed by the three strings:
    * @"apples"@, @"oranges"@, and @"pears"@
    * Related Functions: @strchopr@, @extract_element@
    */
   MAKE_INTRINSIC(".strchopr", do_strchopr, INT_TYPE, 0),
   /* Prototype: Integer strchopr (String str, Integer delim, Integer quote);
    * This routine performs exactly the same function as @strchop@ except 
    * that it returns the substrings in the reverse order.
    * Related Functions: @strchop@, @extract_element@
    */
     
   MAKE_INTRINSIC(".str_replace", str_replace, INT_TYPE, 0),
   /* Prototype: Integer str_replace (String a, String b, String c);
    * This function replaces the first occurance of @b@ in @a@ with @c@ and returns
    * an integer that indicates whether a replacement was made or not. If @b@
    * does not occur in @a@, zero is returned.  However, if @b@ occurs in @a@, 
    * a non-zero integer is returned as well as the new string resulting from the
    * replacement.  For example, 
    * @ define str_replace_all (orig, match, replacement)
    * @ {
    * @    while (str_replace (orig, match, replacement))
    * @        orig = ();
    * @    return orig;
    * @ }
    * is a function that replaces all occurances in a string.
    * Related Functions: @is_substr@, @strsub@, @strtrim@
    */
   
   MAKE_INTRINSIC(".is_defined",  SLang_is_defined, INT_TYPE, 1),
   /* Prototype: Integer is_defined (String obj);
    * This function is used to determine whether or not @obj@ has been defined.
    * If @obj@ is not defined, it returns 0.  Otherwise, it returns a non-zero
    * value that defpends on the type of object @obj@ represents.  Specifically:
    * @ +1 if arg is an intrinsic function 
    * @ +2 if user defined function
    * @ -1 if intrinsic variable
    * @ -2 if user defined variable 
    * @  0 if undefined
    * For example, consider the function:
    * @ define runhooks (hook)
    * @ { 
    * @    if (2 == is_defined(hook)) eval(hook);
    * @ }
    * This function could be called from another S-Lang function to allow customization 
    * of that function, e.g., if the function represents a mode, the hook could 
    * be called to setup keybinding for the mode.
    * Related Functions: @eval@, @autoload@
    */
   
   MAKE_INTRINSIC(".string",  SLdo_string, VOID_TYPE, 0),
   /* Prototype: String string (obj);
    * Here @obj@ can be of any type.  The function @string@ will return a string
    * representation of @obj@.
    * For example, @string(12.34)@ returns @"12.34"@.
    * Note: Not all objects may have a meaningful string representation.
    * Related Functions: @Sprintf@, @integer@, @char@
    */
   MAKE_INTRINSIC(".getenv",  lang_getenv_cmd, VOID_TYPE, 1),
   /* Prototype: String getenv(String var);
    * Returns value of an environment variable @var@ as a string.  The empty
    * string @""@ is returned if @var@ is not defined.  Here is a simple example
    * of how an environment variable can be used in a S-Lang program: 
    * @ if (strlen (getenv ("USE_COLOR")))
    * @   {
    * @     set_color ("normal", "white", "blue");
    * @     set_color ("status", "black", "gray");
    * @     USE_ANSI_COLORS = 1;
    * @   }
    * Related Functions: @putenv@, @strlen@ */
#ifdef HAVE_PUTENV
   MAKE_INTRINSIC(".putenv",  lang_putenv_cmd, VOID_TYPE, 0),
   /* Prototype: Void putenv (String s);
    * This functions adds string @s@ to the environment.  Typically, @s@ should
    * be a string of the form @"name=value"@.  It signals an error upon failure.
    * Note: This function is not available on all systems.
    * Related Functions: @getenv@
    */
#endif
   MAKE_INTRINSIC(".evalfile",  load_file, INT_TYPE, 0),
   /* Prototype: Integer evalfile (String file);
    * Load @file@ as S-Lang code.  If loading is successful, a non-zero result 
    * will be returned.  If @file@ is not found, zero will be returned.
    * Note: some applications embedding S-Lang may override the definition of
    * this function.  For example, the JED editor overrides the default definition
    * and will generate an error if @file@ cannot be opened.
    * Related Functions: @eval@, @autoload@ */
   
   MAKE_INTRINSIC(".char",  SLdo_char, VOID_TYPE, 0),
   /* Prototype: String char (Integer c);
    * This function takes and integer and returns a string of length one whose 
    * first character has ascii value @c@.  For example, @char('a')@ returns 
    * the string @"a"@.
    * Related Functions: @integer@, @string@.
    */
   MAKE_INTRINSIC(".eval",  SLang_load_string, VOID_TYPE, 1),
   /* Prototype: Void eval (String expression);
    * The @eval@ function parses a string as S-Lang code and executes the 
    * result.  This is a useful function in many contexts such as dynamically
    * generating function definitions where there is no way to generate them otherwise.
    * Related Functions: @is_defined@
    */
   MAKE_INTRINSIC(".dup",  SLdo_dup, VOID_TYPE, 0),
   /* Prototype: dup ();
    * This function returns an exact duplicate of the object on top of the
    * stack.  For some objects such as arrays, it creates a new reference to
    * the array.  However, for simple S-Lang types such as Strings, Integers, 
    * and Floats, it creates a new copy of the object.
    * Related Functions: @pop@, @copy_array@ */
   MAKE_INTRINSIC(".substr",  SLdo_substr, VOID_TYPE, 0),
   /* Prototype: String substr (String s, Integer n, Integer len);
    * This function returns a substring with length @len@ of string
    * @s@ beginning at position @n@.  If @len@ is @-1@, the entire
    * length of the string @s@ will be used for @len@.  The first
    * character of @s@ is given by @n@ equal to 1.  For example, 
    * @ substr ("To be or not to be", 7, 5);
    * returns @"or no"@.
    * Related Functions: @is_substr@
    */
   MAKE_INTRINSIC(".integer",  SLdo_integer, VOID_TYPE, 0),
   /* Prototype: Integer integer (String s)
    * This function converts the string representation of an integer back
    * to an integer.  
    * For example, @integer ("1234")@ returns the integer value @1234@.
    * Related Functions: @string@, @Sprintf@, @char@
    */
   MAKE_INTRINSIC(".is_substr",  SLdo_issubstr, VOID_TYPE, 0),
   /* Prototype: Integer is_substr (String a, String b);
    * This function may be used to determine if @a@ contains the string @b@.
    * If it does not, the function returns 0; otherwise it returns the position
    * of @b@ in @a@.  Like all S-Lang strings, positions start at 1.  That is, 
    * the first character of @a@ is given by position 1.
    * Related Functions: @substr@, @string_match@
    */
   MAKE_INTRINSIC(".strsub",  SLdo_strsub, VOID_TYPE, 0),
   /* Prototype: String strsub (String s, Integer pos, Integer ch);
    * This function replaces the character at position @pos@ in string @s@
    * by the character @ch@ and returns the resulting string.
    * The first character in the string @s@ is specified by @pos@ equal to 1.
    * For example, 
    * @ define replace_spaces_with_comma (s)
    * @ {
    * @   variable n;
    * @   while (n = is_substr (s, " "), n) s = strsub (s, n, ',');
    * @   return s;
    * @ }
    * replaces all spaces with a comma.
    * Related Functions: @is_substr@, @str_replace@
    */

   MAKE_INTRINSIC(".extract_element", SLang_extract_list_element, STRING_TYPE, 3),
   /* Prototype: String extract_element (String list, Integer nth, Integer delim);
    * Returns @nth@ element in @list@ where @delim@ separates elements of the 
    * list. @delim@ is an integer that represents the ascii value of the
    * character that serves as the delimiter.  Elements are numbered from 0.
    *
    * For example:
    * @  extract_element ("element 0, element 1, element 2", 1, ',');
    * returns the string @" element 1"@, whereas 
    * @  extract_element ("element 0, element 1, element 2", 1, ' ');
    * returns @"0,"@.
    * Related Functions: @is_list_element@, @is_substr@, @strchop@.
    */
   MAKE_INTRINSIC(".is_list_element", SLang_is_list_element, INT_TYPE, 3),
   /* Prototype: Integer is_list_element (String list, String elem, Integer delim);
    * If @elem@ is an element of @list@ where @list@ is a @delim@ seperated 
    * list of strings, this function returns 1 plus the matching element 
    * number.  If @elem@ is not a member of the list, zero is returned.
    * For example:
    * @  is_list_element ("element 0, element 1, element 2", "0,", ' ');
    * returns 2 since @"0,"@ is element number one of the list (numbered from
    * zero).
    * Related Functions: @extract_element@.
    */
   MAKE_INTRINSIC(".case", generic_equals, INT_TYPE, 0),
   /* Prototype: Integer case(Object a, Object b);
    * The @case@ function is a generic comparison operation that is able to
    * compare objects of different types.  The @case@ function returns zero if 
    * @a@ and @b@ have different types or have different values; otherewise, it 
    * returns one.
    * 
    * This function is parsed a little different than other functions in the sense 
    * that parenthesis are not required around its arguments.  If no parenthisis
    * follows the @case@ token, it is assumed that the first argument is present
    * on the stack and that the token immediately following the @case@ token is
    * the second argument.  This means that 
    * @ case (a, b);
    * and
    * @ a; case b;
    * are equivalent.  This parsing feature makes the @case@ function useful 
    * for use in a @switch@ statement.  In a @switch@ statment, it may be 
    * used as:
    * @  switch (token)
    * @  { case "return": return_function ();}
    * @  { case "break": break_function ();}
    * Unlike the C version, it one cannot use:
    * @  switch (i)
    * @  {case 10: case 20: do_ten_or_twenty (i);}
    * Instead, one must do either:
    * @  switch (i)
    * @  {case 10 or case (i, 20) : do_ten_or_twenty (i);}
    * or:
    * @  switch (i)
    * @  {case 10: do_ten_or_twenty (i);}
    * @  {case 20: do_ten_or_twenty (i);}
    * Related Functions: @strcmp@
    */
    
   MAKE_INTRINSIC(".string_match", string_match, INT_TYPE, 3),
   /* Prototype: Integer string_match(String str, String pat, Integer pos);
    * Returns 0 if @str@ does not match regular expression specified by
    * @pat@. This function performs the match starting at position @pos@ in
    * @str@.  The first character of @str@ corresponds to @pos@ equal to one.
    * This function returns the position of the start of the match.  To find
    * the exact substring actually matched, use @string_match_nth@. 
    * Related Functions: @string_match_nth@, @strcmp@, @strncmp@
    */
   MAKE_INTRINSIC(".string_match_nth", string_match_nth, INT_TYPE, 1),
   /* Prototype: Integer string_match_nth(Integer nth);
    * This function returns 2 integers describing the result of the last
    * call to @string_match@.  It returns both the offset into the string 
    * and the length of characters matches by the @nth@ submatch.  
    * By convention, @nth@ equal to zero means the entire match.  Otherwise,
    * @nth@ must be an integer, 1 to 9, and refers to the set of characters
    * matched by the @nth@ regular expression delimited by @\(@, @\)@ pairs.
    * For example, consider:
    * @  variable matched, pos, len;
    * @  matched = string_match("hello world", "\\([a-z]+\\) \\([a-z]+\\)", 1);
    * @  if (matched) {
    * @      (pos, len) = string_match_nth(2);
    * @  }
    * This will set @matched@ to 1 since a match will be found at the
    * first position, @pos@ to 6 since @w@ is the offset 6 characters
    * from the beginning of the string, and len to 5 since @"world"@
    * is 5 characters long.
    * 
    * Please note that the position offset is NOT affected by the
    * value of the offset parameter to the @string_match@ function.
    * For example, if the value of the last parameter to the
    * @string_match@ function had been 3, @pos@ would still have been set 
    * to 6.
    */
   MAKE_VARIABLE("._traceback", &SLang_Traceback, INT_TYPE, 0),
   /* Prototype Integer _traceback;
    * An error is generated when the integer variable @_traceback@ is
    * non-zero will result in a function traceback showing the functions that
    * were active at the time of the error as well as the values of any local
    * variable that the functions may have.
    * 
    * Local variables are represented in the form @$n@ where @n@ is an
    * integer numbered from zero.  More explicitly, @$0@ represents the
    * first declared local variable, @$1@ represents the seconds, and so on.
    * Please note that function parameters are local variables and that the
    * first parameter corresponds to @$0@.
    * 
    * Related functions: @_slangtrace@
    */
   
   MAKE_VARIABLE("._slangtrace", &SLang_Trace, INT_TYPE, 0),
   /*
    * If non-zero, begin tracing when function declared by 
    * @slang_trace_function@ is entered.  This does not trace intrinsic 
    * functions.
    */

   /* -------------------------------------------------------*/
   /* these are rarely ever referred to so make them last. */
    
   MAKE_INTRINSIC(".system",  my_system, INT_TYPE, 1),
   /* Prototype: Integer system (String cmd);
    * The system function may be used to execute the string expression @cmd@ in
    * an inferior shell.  It returns 127 if the inferior shell could not be 
    * invoked, -1 if there was some other error, otherwise it returns the return 
    * code for @cmd@.  For example,
    * @ define dir ()
    * @ {
    * @    () = system ("DIR");
    * @ }
    * displays a directory listing of the current directory under MSDOS or 
    * VMS.
    */
   MAKE_INTRINSIC(".slapropos",  lang_apropos, VOID_TYPE, 1),
   /* Prototype: Integer slapropos (String s);
    * The @slapropos@ function may be used to get a list of all defined
    * objects whose name consists of the substring @s@.  The number of
    * matches is returned. If the number returned is non-zero, that number
    * of strings which represent the names of the matched objects will will
    * also be present on the stack.  For example, the function
    * @ define apropos (s)
    * @ {
    * @   variable n, name;
    * @   n = slapropos (s);
    * @   if (n) print (Sprintf ("Found %d matches:", n, 1));
    * @   else print ("No matches.");
    * @   loop (n) 
    * @     {
    * @        name = ();
    * @        print (name);
    * @     }
    * @ }
    * prints a list of all matches.  It assumes the existence of a @print@ 
    * intrinsic function.
    * Related Functions: @is_defined@, @Sprintf@
    */
   
   MAKE_INTRINSIC(".slang_trace_function",  SLang_trace_fun, VOID_TYPE, 1),
   /* Prototype: Void slang_trace_function (String f);
    * This function declares that the S-Lang function with name @f@ is to be
    * traced when it is called.  Calling @slang_trace_function@ does not in
    * itself turn tracing on.  Tracing is turned on only when the variable
    * @_slangtrace@ is non-zero.
    * Related Functions: @print_stack@
    * Related Variables: @_slangtrace@, @_traceback@.
    */
   
   /* array ops: */
   MAKE_INTRINSIC(".create_array",  SLang_create_array, VOID_TYPE, 0),
   /* Prototype: Array create_array (Integer type, Integer i_1, i_2, ..., i_dim, Integer dim);
    * Creates an array of type @type@ with dimension @dim@.
    * @i_n@ is an integer which specifies the maximum size of array in 
    * direction @n@.   @type@ is a control integer which specifies the type 
    * of the array.
    * Types are:
    * @  's' : array of strings
    * @  'f' : array of floats
    * @  'i' : array of integers
    * @  'c' : array of characters
    * Currently, @dim@ cannot be larger than 3.
    * Also note that space is dynamically allocated for the array and that
    * copies of the array are NEVER put on the stack.  Rather, references to
    * the array are put on the stack.
    * For example,
    * @  variable a = create_array ('f', 10, 1);
    * creates a 1 dimensional array of 10 floats and assigns it to @a@.
    */
   MAKE_INTRINSIC(".aget",  SLarray_getelem, VOID_TYPE, 0),
   /* Prototype: Object aget (Integer i, Integer j,..., Integer k, Array a);
    * This function returns an element of the array @a@.  It is equivalent
    * to the S-Lang expression @a[i, j,..., k]@.
    * Note: This function should only be used if using S-Lang's RPN style 
    * syntax.
    * Related Functions: @aput@, @create_array@
    */
   MAKE_INTRINSIC(".__aput",  SLarray_putelem_r, VOID_TYPE, 0),
   MAKE_INTRINSIC(".aput",  SLarray_putelem, VOID_TYPE, 0),
   /* Prototype: Void aput (Integer i, Integer j,..., Integer k, Array a);
    * This function sets an element of the array @a@.  It is equivalent
    * to the S-Lang expression @a[i, j,..., k]@.
    * Note: This function should only be used if using S-Lang's RPN style 
    * syntax.
    * Related Functions: @aput@, @create_array@
    */

   MAKE_INTRINSIC(".strncmp",  SLdo_strncmp, VOID_TYPE, 0),
   /* Prototype: Integer strncmp (String a, String b, Integer n);
    * This function behaves like @strcmp@ except that it compares only the 
    * first @n@ characters in the strings @a@ and @b@.
    * For example,
    * @  strcmp ("apple", "appliance", 3);
    * returns zero since the first three characters match.
    * Related Functions: @strcmp@
    */
   MAKE_INTRINSIC(".strlow", SLdo_strlow, VOID_TYPE, 0),
   /* Prototype: String strlow (String s);
    * This function takes a string @s@ and returns another string identical
    * to @s@ except that all upper case characters that comprise @s@ will be
    * converted to lower case.  For example, the function
    * @ define Strcmp (a, b)
    * @ {
    * @   return strcmp (strlow (a), strlow (b));
    * @ }
    * performs a case insensitive comparison operation on two strings by
    * converting them to lower case first.
    * Related Functions: @strup@, @tolower@, @strcmp@, @strtrim@, @define_case@
    */
   MAKE_INTRINSIC(".tolower", do_lower, INT_TYPE, 0),
   /* Prototype: Integer lower (Integer ch);
    * This function takes an integer @ch@ and returns its lowercase
    * equivalent.
    * Related Functions: @toupper@, @strup@, @strlow@, @int@, @char@, @define_case@
    */
   MAKE_INTRINSIC(".toupper", do_upper, INT_TYPE, 0),
   /* Prototype: Integer upper (Integer ch);
    * This function takes an integer @ch@ and returns its uppercase
    * equivalent.
    * Related Functions: @tolower@, @strup@, @strlow@, @int@, @char@, @define_case@
    */
   MAKE_INTRINSIC(".strup", SLdo_strup, VOID_TYPE, 0),
   /* Prototype: String strup (String s);
    * This function takes a string @s@ and returns another string identical
    * to @s@ except that all lower case characters that comprise @s@ will be
    * converted to upper case.  For example, the function
    * @ define Strcmp (a, b)
    * @ {
    * @   return strcmp (strup (a), strup (b));
    * @ }
    * performs a case insensitive comparison operation on two strings by
    * converting them to upper case first.
    * Related Functions: @strlow@, @toupper@, @strcmp@, @strtrim@, @define_case@
    */
   MAKE_INTRINSIC(".isdigit",  SLdo_isdigit, INT_TYPE, 1),
   /* Prototype: Integer isdigit (String s);
    * This function returns a non-zero value if the first character in the
    * string @s@ is a digit; otherwise, it returns zero.  A simple, user defined
    * implementation of @isdigit@ is
    * @ define isdigit (s)
    * @ {
    * @    return ((int (s) <= '9') and (int (s) >= '0'));
    * @ }
    * However, the intrinsic function @isdigit@ executes up to ten times faster
    * than the equivalent representation defined above.
    * Related Functions: @int@, @integer@
    */
   
   MAKE_INTRINSIC(".strtrim", SLdo_strtrim, VOID_TYPE, 0),
   /* Prototype: String strtrim (String s);
    * The @strtrim@ function removes all leading and trailing whitespace
    * characters from the string @s@ and returns the result.  Whitespace is
    * defined to be any combination of spaces, tabs, and newline characters.
    * Related Functions: @int@, @strlow@, @strup@, @strcompress@
    */
     
   MAKE_INTRINSIC(".strcompress", SLdo_strcompress, VOID_TYPE, 0),
   /* Prototype: String strtrim (String s, String white);
    * The @strcompress@ function compresses the string @s@ by removing all 
    * repeated characters specified by @white@ from the interior of @s@.  In
    * addition, it also removes all leading and trailing characters from @s@
    * that are part of @white@.  For example,
    * @ strcompress (",;apple,,cherry;,banana", ",;");
    * returns the string @"apple,cherry;banana"@.
    * Related Functions: @strtrim@
    */
     

   MAKE_INTRINSIC(".int",  SLdo_int, VOID_TYPE, 0),
   /* Prototype: Integer int (String s);
    * This function returns the ascii value value of the first character of
    * the string @s@.  It is often used to convert single character strings
    * to integers.  For example, the intrinsic function @isdigit@ may be
    * defined as
    * @ define isdigit (s)
    * @ {
    * @   if ((int (s) >= '0') and (int (s) <= '9')) return 1;
    * @   return 0;
    * @ }
    * Related Functions: @char@, @isdigit@
    */
   MAKE_INTRINSIC(".array_sort", SLarray_sort, VOID_TYPE, 1),
   /* Prototype: Array array_sort (Array a, String f);
    * @array_sort@ sorts the array @a@ into ascending order according to the 
    * function @f@ and returns an integer array that represents the result of the 
    * sort.
    * 
    * The integer array returned by this function is simply an index that indicates the 
    * order of the sorted array.  The input array @a@ is not changed.  For example, 
    * if the input array consists of the three strings
    * @  {"gamma", "alpha", "beta"}
    * and the sort function @f@ is defined to be
    * @  define f (a, b)
    * @  { return strcmp (a, b); }
    * then the index array will be returned as:
    * @  {2, 0, 1}
    * 
    * Note that the comparison cannot be an intrinsic function; it must be a 
    * S-Lang user defined function.  The function takes two arguments
    * and returns an integer that is less than zero if the first parameter is 
    * considered to be less than the second, zero if they are equal, and a 
    * value greater than zero if the first is greater than the second.
    * 
    * Related Functions: @create_array@.
    */
   
   /* misc stuff */
   MAKE_INTRINSIC("._stkdepth", SLstack_depth, INT_TYPE, 0),
   /* Prototype: Integer: _stkdepth ();
    * This function returns number of items on stack prior to the call of
    * @_stkdepth@.
    * 
    * Related Functions: @print_stack@, @_obj_type@
    */
   MAKE_INTRINSIC("._stk_reverse", reverse_stack, VOID_TYPE, 1),
   /* Prototype: Void _stk_reverse (Integer n);
    */
   MAKE_INTRINSIC("._obj_type", get_object_type, INT_TYPE, 0),
   /* Prototype: Integer _obj_type ();
    * This function returns type information about the object on the top of the
    * stack.  It does not remove the object from the stack.  The value returned
    * in an integer with following meaning:
    * @   2  Integer
    * @   3  Float
    * @   6  Pointer
    * @  10  String
    * @  20  Array
    * Other values correspond to application dependent types.
    */
   MAKE_INTRINSIC(".print_stack", lang_print_stack, VOID_TYPE, 0),
   /* Prototype: Void print_stack (void);
    * This function dumps out what is currently on the S-Lang stack.  It does not
    * alter the stack and it is usually used for debugging purposes.
    * Related Functions: @_stkdepth@, @string@
    */
   MAKE_INTRINSIC("._stk_roll", SLroll_stack, VOID_TYPE, 1),
   /* Prototype: Void _stk_roll (Integer n);
    * If @n@ is positive, the top @n@ items on the stack are rotated up.  If
    * @n@ is negative, the top @abs(n)@ items on the stack are rotated down.
    * For example, if the stack looks like:
    * @ item-0
    * @ item-1
    * @ item-2
    * @ item-3
    * where @item-0@ is at the top of the stack, then @_stk_roll(-3)@ will
    * change the stack to:
    * @ item-2
    * @ item-0
    * @ item-1
    * @ item-3
    * Note that this function only has an effect for @abs(n) > 1@.
    * Related Functions: @_stkdepth@, @print_stack@
    */

   MAKE_INTRINSIC(".Sprintf", SLsprintf, VOID_TYPE, 0),
   /* Prototype:  String Sprintf(String format, ..., Integer n);
    * Sprintf formats a string from @n@ objects according to @format@.  
    * Unlike its C counterpart, Sprintf requires the number of items to
    * format.  Example:
    * @  Sprintf("%f is greater than %f but %s is better than %s\n",
    * @           PI, E, "Cake" "Pie", 4);
    * The final argument to @Sprintf@ is the number of items to format; in
    * this case, there are 4 items.
    * Related Functions: @string@
    */
   MAKE_INTRINSIC(".init_char_array", SLinit_char_array, VOID_TYPE, 0),
    /* Prototype: Void init_char_array(Array_Type a, String s);
     * This function may be used to initialize a character array.  Here @a@
     * is an array type character and @s@ is a string.  This function simply
     * sets the elements of the array @a@ to the corresponding characters of
     * the string @s@.  For example,
     * @ variable a = create_array ('c', 10, 1);
     * @ init_char_array (a, "HelloWorld");
     * creates an character array and initializes its elements to the
     * characters in the string @"HelloWorld"@. 
     * 
     * Note: The character array must be large enough to hold all the
     * characters of the initialization string.
     * Related Functions: @create_array@, @strlen@, @strcat@
     */	      

   MAKE_INTRINSIC(".byte_compile_file", SLang_byte_compile_file, VOID_TYPE, 2),
   /* Prototype: Void byte_compile_file (String file, Integer method);
    * byte compiles @file@ producing a new file with the same name except 
    * a @c@ is added to the output file name.  For example, 
    * @  byte_compile_file("site.sl");
    * produces a new file named @site.slc@.  If @method@ is non-zero, the 
    * file is preprocessed only.  Note that the resulting bytecompiled file
    * must only be used by the executable that produced it.  Set @method@ to 
    * a non-zero value to use the byte compiled file with more than one 
    * executable.
    */
   MAKE_INTRINSIC(".make_printable_string", make_printable_string, VOID_TYPE, 1),
   /* Prototype: String make_printable_string(String str);
    * Takes input string @str@ and creates a new string that may be used by the
    * interpreter as an argument to the @eval@ function.  The resulting string is
    * identical to @str@ except that it is enclosed in double quotes and the
    * backslash, newline, and double quote characters are expanded. 
    * Related Functions: @eval@, @str_quote_string@
    */
   MAKE_INTRINSIC(".str_quote_string", SLquote_string, VOID_TYPE, 0),
   /* Prototype: String str_quote_string(String str, String qlis, Integer quote);
    * Return a string identical to @str@ except that all characters in the 
    * string @qlis@ are escaped with the @quote@ character including the quote
    * character itself.
    * Related Functions: @str_uncomment_string@
    */
   MAKE_INTRINSIC(".str_uncomment_string", uncomment_string, VOID_TYPE, 3),
   /* Prototype: String str_uncomment_string(String s, String beg, String end);
    * @beg@ and @end@ are strings whose characters define a set of comment 
    * delimiters.  This function removes comments defined by the delimiter set
    * from the input string @s@ and returns it.  For example,
    * @  str_uncomment_string ("Hello (testing) 'example' World", "'(", "')");
    * returns the string @"Hello   World"@.
    * Note: this routine does not handle multicharacter comment delimiters and it
    * assumes that comments are not nested.
    * Related Functions: @str_quote_string@
    */
     
   MAKE_INTRINSIC(".define_case", SLang_define_case, VOID_TYPE, 2),
   /* Prototype: Void define_case (Integer ch_up, Integer ch_low);
    * This function defines an upper and lowercase relationship between two 
    * characters specified by the arguments.  This relationship is used by 
    * routines which perform uppercase and lowercase conversions.
    * The first integer @ch_up@ is the ascii value of the upprcase character 
    * and the second parameter @ch_low@ is the ascii value of its lowercase counterpart.
    * Related Functions: @strlow@, @strup@.
    */
   MAKE_INTRINSIC("._clear_error", SLang_clear_error, VOID_TYPE, 0),
   /* Prototype: Void _clear_error ();
    * This function may be used only in error blocks to clear the error that 
    * triggered the error block.  Execution resumes following the statement
    * that triggered the error.
    * Related Functions: @slang_trace_function@
    * Related Variables: @_slangtrace@, @_traceback@
    */
	
   MAKE_INTRINSIC(".set_float_format", set_float_format, VOID_TYPE, 1),
   /* Prototype: Void set_float_format (String fmt);
    * This function is used to set the floating point format to be used
    * when floating point numbers are printed.  The routines that use this
    * are the traceback routines and the @string@ function. The default
    * value is @"%f"@.
    * Related Functions: @string@, @float@
    */
   MAKE_INTRINSIC(".copy_array", SLcopy_array, VOID_TYPE, 0),
   /* Prototype: Void copy_array(Array b, Array a);
    * Copies the contents of array @a@ to array @b@.  Both arrays must be of
    * the same type and dimension.
    * Related Functions: @create_array@, @dup@
    */
   
   MAKE_INTRINSIC(".array_info", SLarray_info, VOID_TYPE, 0),
   /* Prototype: Var array_info (Array a);
    * This function returns information about the array @a@.  The number of
    * values returned depend on the type of array.  It always returns at
    * least three values to the stack.  For two and three dimensional
    * arrays, it returns four and fives values, resp.
    * The values are ordered in such a way that it may be used as an argument 
    * to the @create_array@ function.  For example, 
    * @ define duplicate_array (a)
    * @ {
    * @   variable b = create_array (array_info (a));
    * @   return copy_array (b, a);
    * @ }
    * Related Functions: @create_array@, @copy_array@
    */
   MAKE_INTRINSIC("._slang_guess_type", SLang_guess_type, INT_TYPE, 1),
   /* Prototype: Integer _slang_guess_type (String s);
    * This function tries to determine whether its argument @s@ represents 
    * an integer or a floating point number.  If it appears to be neither, 
    * then a string is assumed.  It returns one of three values depending on
    * the format of the string @s@:
    * @ INT_TYPE     :   If it appears to be an integer
    * @ FLOAT_TYPE   :   If it appears to be a float
    * @ STRING_TYPE  :   Anything else.
    * For example, @_slang_guess_type("1e2")@ returns @FLOAT_TYPE@ but 
    * @_slang_guess_type("e12")@ returns @STRING_TYPE@.
    * Related Functions: @integer@, @string@, @float@
    * Related Variables: @INT_TYPE@, @STRING_TYPE@, @FLOAT_TYPE@
    */
   MAKE_INTRINSIC(".time", SLcurrent_time_string, STRING_TYPE, 0),
   /* Prototype: String time ();
    * This function returns the current time as a string in the form:
    * @ Sun Apr 21 13:34:17 1996
    */
   MAKE_VARIABLE("._slang_version", &SLang_Version, INT_TYPE, 1),
   MAKE_VARIABLE(".INT_TYPE", &Integer_Type_Var, INT_TYPE, 1),
   MAKE_VARIABLE(".STRING_TYPE", &String_Type_Var, INT_TYPE, 1),
#ifdef FLOAT_TYPE
   MAKE_VARIABLE(".FLOAT_TYPE", &Float_Type_Var, INT_TYPE, 1),
#endif
   SLANG_END_TABLE
};

/*}}}*/


int init_SLang() /*{{{*/
{
   char name[3];
   int i;
#ifdef pc_system
# ifdef __os2__ 
   char *s = "OS2";
# else
   char *s = "MSDOS";
# endif
#else
# ifdef VMS
   char *s = "VMS";
# else
   char *s = "UNIX";
# endif
#endif

#ifdef MSWINDOWS
   SLang_Name_Table = (SLang_Name_Type *) SLCALLOC(SLANG_MAX_SYMBOLS, sizeof (SLang_Name_Type));
   if (SLang_Name_Table == NULL) return 0;
#endif
   
   if (-1 == SLregister_types ()) return 0;

   if (!SLang_add_table(SLang_Basic_Table, "_Basic")) return(0);
   SLadd_variable(SLANG_SYSTEM_NAME);
   
   if (!SLdefine_for_ifdef (s)) return 0;
#ifdef MSWINDOWS
   /* I am not sure whether this belongs here or not. */
   if (!SLdefine_for_ifdef ("MSWINDOWS")) return 0;
#endif
#ifdef FLOAT_TYPE
   if (!SLdefine_for_ifdef ("FLOAT_TYPE")) return 0;
#endif
   /* give temp global variables $0 --> $9 */
   name[2] = 0; name[0] = '$';
   for (i = 0; i < 10; i++)
     {
	name[1] = (char) (i + '0');
	SLadd_variable(name);
     }
   
   SLstupid_hash();
   
   SLang_init_case_tables ();
   return (SLang_Error == 0);
}

/*}}}*/
