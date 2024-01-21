#include "h/gsupport.h"
#include "itran/tproto.h"
#include "itran/tree.h"
#include "h/lexdef.h"

/* N_SUBSTR determines the number of elements in a string list */
#define N_SUBSTR(x) (TType((x)) == N_Binop ? (x)->n_field[2].n_val : 1)

/*
 * Prototypes for static functions.
 */
hidden novalue init_map Params((noargs));

static int line = 1;            /* line number for last token printed */
static char *fname = "";        /* file name for last token printed */

static struct toktab **tok_map; /* map token codes to token table entries */
static int min_type;            /* minimum token type code in token table */
static int max_type;            /* maximum token type code in token table */

extern int output_line;         /* flag: output #line information */

/* q - convert a string into a list of one element */
nodeptr q(s)
char *s;
{
    nodeptr n;
    n = StrNode(s, (int)strlen(s));

    Line(n) = 0;   /* #line directives should not be generated for this node */
    return n;
} 

/* concatenate n string lists using a binary tree representation */
nodeptr cat(n, slist, p2, p3, p4, p5, p6, p7, p8, p9)
int n;
nodeptr slist;
int p2, p3, p4, p5, p6, p7, p8, p9;

{
    nodeptr *p;
    nodeptr catstr;
    int size;

    catstr = slist;
    size = N_SUBSTR(catstr);
    for (p = &slist + 1; --n; ++p) {
	size += N_SUBSTR(*p);
	catstr = tree5(N_Binop, *p, catstr, *p, (nodeptr)size);
	}
    return catstr;
}

/*
 * quote - place specified quotes around a token, giving the quotes
 *   the same location as the token. (This prevents the generation
 *   of #line directives between the quotes and the string.)
 */
nodeptr quote(n, s)
nodeptr n;
char *s;
   {
   nodeptr n1, n2;

   n1 = q(s);
   Line(n1) = Line(n);
   File(n1) = File(n);
   n2 = q(s);
   Line(n2) = Line(n);
   File(n2) = File(n);
   return cat(3, n1, n, n2);
   }

/* item - return the nth item in the given list */
nodeptr item(slist, n)
nodeptr slist;
int n;
{
    int lsize;

    if (TType(slist) == N_Binop) {
	if ((lsize  = N_SUBSTR(Tree0(slist))) >= n)
	    return item(Tree0(slist), n);
	else
	    return item(Tree1(slist), n - lsize);
	}
    else {
	if (n == 1)
	    return slist;
	else
	    return NULL;
	}
}

/*
 * treefree - free node storage in a tree.
 */
treefree(x)
nodeptr x;
{
   if (TType(x) == N_Binop) {
      treefree(Tree0(x));
      treefree(Tree1(x));
      }
   free(x);
}

treeprt(x)
nodeptr x;
{
   int nxt_line;
   int this_end;
   int this_begin;
   struct toktab *ttp;
   char *s;
   static first_time = 1;
   static int lastend = 0;

   if (first_time) {
      first_time = 0;
      init_map();
      }

   if (TType(x) == N_Binop) {
      /*
       * Interior node of tree.
       */
      treeprt(Tree0(x));
      treeprt(Tree1(x));
      }
   else {
      /*
       * Leaf node: see if the string image is in the node or if we
       *   need to find it in the operator table. Also determine the
       *   beginner/ender flags for the token.
       */
      if (TType(x) == N_Op || TType(x) == N_Res) {
         ttp = tok_map[Val0(x) - min_type];
         s = ttp->t_word;
         this_begin = ttp->t_flags & Beginner;
         this_end = ttp->t_flags & Ender;
         }
      else {
         s = Str0(x);
         this_begin = 1; /* assume worst case */
         this_end = 1;   /* assume worst case */
         }

      /*
       * #line directives are suppressed if the -P option is used,
       *  the associated new-line could cause unwanted semi-colon
       *  insertion, or if this token does not contain valid line
       *  information.
       */
      nxt_line = Line(x);
      if (output_line && !(lastend && this_begin) && nxt_line != 0) {
         /*
          * See if we need a #line directive; optimize to a few new-lines
          *   when possible.
          */
         if (strcmp(fname, File(x)) != 0 || line > nxt_line ||
            line + 2 < nxt_line) {
            printf("\n#line %d \"%s\"\n", nxt_line - 1, File(x));
            line = nxt_line;
            fname = File(x);
            }
         else {
            while (line < nxt_line) {
              putchar('\n');
               ++line;
               }
            }
         }

      printf("%s", s);       /* print the image */

      /*
       * See if the image contains new-lines and see if it contains
       *  non white-space.
       */
      for ( ; *s != '\0'; ++s) {
         if (*s == '\n')
            ++line;
         else if (*s != ' ' && *s != '\t')
            lastend = this_end;  /* may be a real token with valid flags */
         }
      }
   }


/*
 * init_image - initialize array to map token type codes to their token
 *   table entries.
 */
static novalue init_map()
   {
   struct toktab *ttp;
   struct optab *otp;
   int i;

   /*
    * Find the range of token type codes.
    */
   min_type = MaxInt;
   max_type = 0;
   for (ttp = toktab; ttp->t_type != 0; ++ttp) {
       if (ttp->t_type < min_type)
          min_type = ttp->t_type;
       if (ttp->t_type > max_type)
          max_type = ttp->t_type;
       }
   for (otp = optab; otp->tok.t_type != 0; ++otp) {
       if (otp->tok.t_type < min_type)
          min_type = otp->tok.t_type;
       if (otp->tok.t_type > max_type)
          max_type = otp->tok.t_type;
       }

   /*
    * Create an array to perform the mapping.
    */
   tok_map = (struct toktab **)alloc((unsigned int)
      (max_type - min_type + 1) * sizeof(struct toktab *));
   for (i = 0; i <= (max_type - min_type); ++i)
      tok_map[i] = NULL;

   /*
    * Initialize array.
    */
   for (ttp = toktab; ttp->t_type != 0; ++ttp) {
      if (tok_map[ttp->t_type - min_type] == NULL)
         tok_map[ttp->t_type - min_type] = ttp;
      }
   for (otp = optab; otp->tok.t_type != 0; ++otp) {
       /*
        * Some operator tokens have different represenations to handle
        *   EBCDIC keyboards. If we already have a representation for
        *   this token, decide which one to use.
        */
      if (tok_map[otp->tok.t_type - min_type] == NULL ||
#if EBCDIC
         otp->tok.t_word[0] == '$')
#else					/* EBCDIC */
         otp->tok.t_word[0] != '$')
#endif					/* EBCDIC */
         tok_map[otp->tok.t_type - min_type] = &otp->tok;
      }
   }
