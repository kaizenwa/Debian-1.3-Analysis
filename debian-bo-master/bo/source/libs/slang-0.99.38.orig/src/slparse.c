/* infix to RPN parsing as well as file loading routines */
/* Copyright (c) 1992, 1995 John E. Davis
 * All rights reserved.
 * 
 * You may distribute under the terms of either the GNU General Public
 * License or the Perl Artistic License.
 */


#define NEW_STUFF 1

#include "config.h"

#include <stdio.h>
#include <string.h>


#include "slang.h"
#include "_slang.h"

#define NAME 256
#define ASSIGN 257
#define LEFT_P 258
#define RIGHT_P 259
#define END 260
#define COMMA 261
#define EOS 262
#define BRA 263
#define KET 264
#define EOS_BRA 265
#define EOS_KET 266
#define COLON 267
#define ASSIGN_M 268
#define ASSIGN_P 269
#define CASE_TYPE 270

#define IF_TYPE 300
#define ELSE_TYPE 301
#define WHILE_TYPE 302
#define FOREVER_TYPE 303
#define FOR_TYPE 304
#define LOOP_TYPE 305
#define SWITCH_TYPE 306
#define F_TYPE 307
#define V_TYPE 308
#define INLINE_TOK 309
#define IF_NOT_TYPE 310
#define ERROR_B_TYPE 311
#define CFOR_TYPE 312
#define DO_WHILE 313
#define RETURN_TYPE 314
#define EXIT_B_TYPE 315
#define USER_B_TYPE 316
#define ORELSE_TYPE 317
#define ANDELSE_TYPE 318
#define SPECIAL_AT_TOK 319
#define GV_TYPE 320

static int CTok;

static char *Token;
static char *Input;
static SLang_Load_Type *LLT;

static char *Terminate_String = ";";

static char Name_Stack[100][32];
static int Name_Stack_P = 0;

#define push_name() \
  if (Name_Stack_P >= 100) { Name_Stack_P = 0; parse_error ("Name stack overflow.", 0); } \
  if (*Token == '"') parse_error("Invalid Name", 0); else\
    strcpy(Name_Stack[Name_Stack_P++], Token);
    

#define push_literal_name(n) strcpy(Name_Stack[Name_Stack_P++], (n))

#define pop_name()  (*SLcompile_ptr)(Name_Stack[--Name_Stack_P])

static void push_token (char *, int);
static int pop_token (void);
static int get_token(void);
static void expression(void);
static void term(void);
static void basic(void);
static void arguments(int);

static void pop_eqsname(int what)
{
   char work[80], *w = work;
   
   if (what == ASSIGN_M) *w++ = '-'; else if (what == ASSIGN_P) *w++ = '+';
   *w++ = '=';
   strcpy(w, Name_Stack[--Name_Stack_P]);
   (*SLcompile_ptr)(work);
}

#ifdef IF_TYPE
static void block(void);
static void directive(void);
static void block_internal(void);
/* static int if_fudge = 0; */

static char *make_line_file_error (char *buf, int line, char *file)
{
   sprintf (buf, "at or before line %d: file: %s", line, file);
   return buf;
}

static void parse_error(char *str, int flag)
{
   char buf [1024];
   if (str == NULL) str = Token;
   
   make_line_file_error (buf, LLT->n, str);
   if (flag && SLang_Error) *buf = 0;
   SLang_doerror(buf);
}

static void do_line_file_error (int line, char *file)
{
   char buf[1024];
   SLang_doerror (make_line_file_error (buf, line, file));
}

   

static void function_args(void)
{
   int n = 0;
   get_token();
   while ((CTok != END) && (CTok != RIGHT_P))
     {
	if (CTok == NAME)
	  {
	     n++;
	     push_name();
	     if (n == 1) (*SLcompile_ptr)("[");
	     (*SLcompile_ptr)(Token);
	  }
	else if (CTok != COMMA)
	  {
	     parse_error("Expecting comma.", 0);
	  }
	if (SLang_Error) return;
	get_token();
     }
   get_token();
   if (n)
     {
	(*SLcompile_ptr)("]");
	while(n--) pop_eqsname(ASSIGN);
     }
}


/* This routine parses (a,b,..c) = express; to express =c ...=b =a */
static int try_multiple_assignment (void)
{
   int n = 0, save_ctok;
   char tok[32];
   
   /* I want to look ahead because if neither of the next 2 tokens is a comma,
    * it is unlikely that this is a multiple assignment so we will just 
    * return 0
    */
   get_token();
   
   if ((CTok != COMMA) && (CTok != NAME))
     {
	if (CTok == RIGHT_P)
	  {
	     get_token ();
	     push_token (Token, CTok);
	     if (CTok != ASSIGN)
	       {
		  push_token (")", RIGHT_P);
		  return 0;
	       }
	     CTok = RIGHT_P;
	  }
	else 
	  {
	     push_token (Token, CTok);
	     return 0;
	  }
     }
   
   /* the unget token stack is not designed for strings and if one occurs here
    * it is mst likely a syntax error so just flag it.
    */
   
   if (CTok == NAME)
     {
	if (*Token == '"')
	  {
	     SLang_Error = SYNTAX_ERROR;
	     return 1;
	  }
   
	strcpy (tok, Token);
	get_token ();
	
	if (CTok == NAME)
	  {
	     SLang_Error = SYNTAX_ERROR;
	     return 1;
	  }
	
	push_token (Token, CTok);
	push_token (tok, NAME);
	
	if (CTok != COMMA) return 0;
	get_token ();
     }
   
   save_ctok = COMMA;
   
   while ((CTok != END) && (CTok != RIGHT_P))
     {
	if (CTok == NAME)
	  {
	     n++;
	     push_name();
	  }
	else if (CTok == COMMA)
	  {
	     if (save_ctok == COMMA)
	       {
		  push_literal_name("pop");
		  n++;
	       }
	  }
	else parse_error("Expecting comma.", 0);
	if (SLang_Error) return 1;
	save_ctok = CTok;
	get_token ();
     }
   if (CTok != RIGHT_P)
     {
	parse_error("Unexpected end of source.", 1);
	return 1;
     }
   
   if (save_ctok == COMMA)
     {
	push_literal_name("pop");
	n++;
     }
   
   get_token();
   if ((CTok == ASSIGN) || (CTok == ASSIGN_P) || (CTok == ASSIGN_M))
     {
	save_ctok = CTok;
	get_token();
	expression();
	if (SLang_Error == 0) while (n--)
	  {
	     if (strcmp ("pop", Name_Stack[Name_Stack_P - 1]))
	       pop_eqsname(save_ctok);
	     else pop_name ();
	  }
     }
   else parse_error("Syntax Error.", 1);
   return 1;
}

static int Look_For_Muliple_Assignment;

static char at_handler_name[32];
void SLadd_at_handler (long *f, char *name)
{
   if (*at_handler_name) return;
   SLadd_name (name, (long) f, SLANG_INTRINSIC, SLANG_MAKE_ARGS(VOID_TYPE, 1));
   strncpy (at_handler_name, name, 31);
   at_handler_name[31] = 0;
}

static void handle_special_at_line (char *input)
{
   char line[512];
   register char *p, ch;
   
   p = line; *p++ = '"';
   while (((ch = *input++) != 0) && (ch != '\n')) 
     {
	if ((ch == '"') || (ch == '\\')) *p++ = '\\';
	*p++ = ch;
     }
   *p++ = '"';
   *p++ = ' ';
   strcpy (p, at_handler_name);
   SLang_rpn_interpret (line);
}


static void directive()
{
   int ctoks[3], t, i, bra, this_tok;
   
   Look_For_Muliple_Assignment = 1;
   
   switch (CTok)
     {
      case IF_TYPE: 
	get_token();
	/* if_fudge = 1; */
	Look_For_Muliple_Assignment = 0;
	expression();
	/* if_fudge = 0; */
	block();
	if (CTok == EOS) get_token();
	if (CTok == ELSE_TYPE)
	  {
	     directive();
	  }
	else (*SLcompile_ptr)("if");
	break;
		
      case RETURN_TYPE: 
	get_token();
	Look_For_Muliple_Assignment = 0;
	if (CTok == EOS) get_token(); else expression();
	(*SLcompile_ptr)("return");
	break;
	
      case ELSE_TYPE: 
	get_token();
	block();
	(*SLcompile_ptr)("else");
	break;

      case DO_WHILE: 
	get_token();
	block();
	if (CTok == EOS) get_token();
	if (CTok != WHILE_TYPE)
	  {
	     parse_error("Expecting while.", 0);
	     return;
	  }
	get_token();
	(*SLcompile_ptr)("{");
	expression();
	(*SLcompile_ptr)("}");
	(*SLcompile_ptr)("do_while");
	break;
	
      case WHILE_TYPE:
	get_token();
	Look_For_Muliple_Assignment = 0;
	(*SLcompile_ptr)("{");
	expression();
	(*SLcompile_ptr)("}");
	block();
	(*SLcompile_ptr)("while");
	break;
	
      case LOOP_TYPE:
      case FOR_TYPE:
      case IF_NOT_TYPE:
	Look_For_Muliple_Assignment = 0;
	push_name();
	get_token();
	expression();
	block();
	pop_name();
	break;
       
      case CFOR_TYPE: 
	push_name();
	get_token();
	if (CTok != LEFT_P) goto err;
	get_token();
	ctoks[0] = ctoks[1] = EOS, ctoks[2] = RIGHT_P;
	for (i = 0; i < 3; i++)
	  {
	     if (SLang_Error) return;
	     t = ctoks[i];
	     (*SLcompile_ptr)("{");
	     if (CTok != t) expression();
	     while (!SLang_Error && (CTok == COMMA)) 
	       {
		  get_token();
		  expression();
	       }
	     
	     if (CTok != t) goto err;
	     (*SLcompile_ptr)("}");
	     get_token();
	  }
	block();
	pop_name();
	break;
	
	
      case ERROR_B_TYPE: 
      case EXIT_B_TYPE: 
      case USER_B_TYPE: 
      case FOREVER_TYPE: 
	push_name();
	get_token();
	block();
	pop_name();
	break;
	
      case ORELSE_TYPE: 
      case ANDELSE_TYPE:
      case SWITCH_TYPE:
	push_name ();
	if (CTok == SWITCH_TYPE) 
	  {
	     get_token();
	     expression();
	  }
	else get_token ();
	
	
	while (!SLang_Error && (CTok == EOS_BRA)) block();
	pop_name ();
	break;
	
      case GV_TYPE:
      case V_TYPE:		       /* variable declaration */
	this_tok = CTok;
	get_token();
	bra = 0;
	while (!SLang_Error && (CTok == NAME))
	  {
	     strcpy(Name_Stack[Name_Stack_P], Token);
	     if (!bra)
	       {
		  if (this_tok == V_TYPE)
		    {
		       (*SLcompile_ptr)("[");
		       bra = 1;
		    }
		  else SLang_add_global_variable (Token);
	       }
	     
	     if (this_tok == V_TYPE) (*SLcompile_ptr)(Token);
	     
	     get_token();
	     if (CTok == ASSIGN)
	       {
		  Name_Stack_P++;
		  if (this_tok == V_TYPE) (*SLcompile_ptr)("]");
		  get_token();
		  expression();
		  pop_eqsname(ASSIGN);
		  bra = 0;
	       }
	     if (CTok == COMMA) get_token();
	     else 
	       {
		  break;
	       }
	  }
	if (bra && (this_tok == V_TYPE)) (*SLcompile_ptr)("]");
	if (CTok != EOS) parse_error("Expecting EOS.", 0);
	break;
	
      case F_TYPE:		       /* function declaration */
	get_token();
	if (CTok != NAME) 
	  {
	     parse_error("Expecting function name.", 0);
	     return;
	  }
	push_name();
	get_token();
	(*SLcompile_ptr)("(");
	if (CTok == LEFT_P) function_args();
	
	if (CTok == EOS_BRA) block_internal();
	else if (CTok != EOS)
	  {
	     parse_error("Expecting '{'", 0);
	     return;
	  }
	
	if (!SLang_Error) 
	  {
	     (*SLcompile_ptr)(")");
	     pop_name();
	  }
	break;
	
      case INLINE_TOK: 
	Input = SLang_rpn_interpret(Input + 1);
	if (!SLang_Error) 
	  {	
	     Input = NULL;
	     get_token();
	  }
	break;
	
      case SPECIAL_AT_TOK:
	handle_special_at_line (Input + 1);
	Input = NULL;
	if (!SLang_Error) get_token ();
	break;
#if 0
      case LEFT_P:
	if (try_multiple_assignment ()) break;
	push_token ("(", LEFT_P);
	get_token ();
	/* drop */
#endif
      default: 
	expression();
     }
   return;
   err:
   parse_error("Syntax Error.", 1);
}


static void block_internal(void)
{
   if (CTok != EOS_BRA) expression();
   else 
     {
	get_token();
	while (!SLang_Error && (CTok != END) && (CTok != EOS_KET)) 
	  {
	     directive (); /* expression(); */
	     if (CTok == EOS) get_token();
	     /* if (CTok != EOS_KET)
	       {
		  SLang_Error = SYNTAX_ERROR;
		  return;
	       } */
	  }
	
	if (CTok == END) 
	  {
	     parse_error("Unexpected end of source.", 1);
	     return;
	  }
	get_token();
     }
}



static void block(void)
{
   (*SLcompile_ptr)("{");
   if (CTok != EOS) block_internal(); else get_token();
   if (!SLang_Error) (*SLcompile_ptr)("}");
}
#endif

static void expression(void)
{
   int anything_terminates = 0;
   
   /* if (CTok == END) return; */
   
   if (CTok == COLON) 
     {
	(*SLcompile_ptr)(":");
	get_token();
     }
   
#ifdef IF_TYPE
   if (CTok >= IF_TYPE)
     {
	directive();
	return;
     }
#endif


   if (CTok == LEFT_P)
     {
	anything_terminates = 1;
	if (Look_For_Muliple_Assignment)
	  {
	     if (try_multiple_assignment ()) return;
	     CTok = LEFT_P;
	  }
	Look_For_Muliple_Assignment = 1;
     }
   
   term();
   
   while (!SLang_Error)
     {
	switch (CTok)
	  {
	   case (SLANG_PLUS): 
	   case (SLANG_MINUS): 
	     push_name();
	     get_token();
	     term();
	     pop_name();
	     break;
	     
	   case (SLANG_AND): 
	   case (SLANG_BAND):
	   case (SLANG_BOR): 
	   case (SLANG_OR): 
	   case (SLANG_BXOR): 
	   case (SLANG_EQ): 
	   case (SLANG_NE): 
	   case (SLANG_GT): 
	   case (SLANG_GE): 
	   case (SLANG_LT): 
	   case (SLANG_SHL): 
	   case (SLANG_SHR):
	   case (SLANG_LE):
	     /* It seems that to achieve C precedence levels, I should also
	      * push a precedence number for the token as well.  Then I would
	      * need to check the level of the last on an somehow pop it if it
	      * is higher than this one.
	      */
	     push_name();
	     get_token();
	     expression();
	     pop_name();
	     break;

	   case EOS: case EOS_KET: case KET: case RIGHT_P:
	   case EOS_BRA: case COMMA:
	     return;
	     
	   case COLON:
	     return;
	     /* (*SLcompile_ptr)(":");
	      * CTok = EOS;
	      * return;
	      */
	     
	   case NAME: 
	     /* allow things like 'if (i == 2) i = 3;' and ':' */
	     if ((*Token == ':') && (Token[1] == 0))
	       {
		  CTok = COLON;
		  return;
	       }
	     
	   default: 
	     if (anything_terminates) return;
	     parse_error("Expecting ';'", 0);
	     return;
	  }
     }
}

static void term(void)
{
   basic();
   while(!SLang_Error)
     {
	switch (CTok)
	  {
	   case (SLANG_MOD): 
	   case (SLANG_TIMES): 
	   case (SLANG_DIVIDE): 
	     push_name();
	     get_token();
	     basic();
	     pop_name();
	     break;
	   default: return;
	  }
     }
}

static void basic(void)
{
   int save_ctok;   
#if !NEW_STUFF
   char *save, *save_again;
   int count;
#endif   

   switch(CTok)
     {
      case CASE_TYPE:
	push_name ();
	get_token ();
	basic ();
	pop_name ();
	break;
	
      case NAME:
	if (*Token == '"')
	  {
	     (*SLcompile_ptr)(Token);
	     get_token();
	     break;
	  }
	
	push_name();
	get_token();
	switch(CTok)
	  {
	   case (ASSIGN_P): 
	   case (ASSIGN_M): 
	   case (ASSIGN):
	     save_ctok = CTok;
	     get_token();
	     expression();
	     pop_eqsname(save_ctok);
	     return;
	      
	   case (LEFT_P): 
	     get_token();
#ifdef SLANG_NOOP
	     (*SLcompile_ptr)("__noop__");
#endif
	     arguments(RIGHT_P);
	     break;
	
	   case BRA: 
	     /* find end of argument list */
#if !NEW_STUFF
	     save = Input;
	     count = 1;
	     while (count)
	       {
		  get_token();
		  if ((CTok == END) /* || (CTok == EOS) */
		      || (CTok == EOS_BRA) || (CTok == EOS_KET))
		    {
		       parse_error("Incomplete Statement.", 0);
		       return;
		    }
		  else if (CTok == BRA) count++;
		  else if (CTok == KET) count--;
	       }
#else
	     do
	       {
		  get_token ();
		  expression ();
		  if (CTok == KET) break;
	       }
	     while (CTok == COMMA);
	     
	     if (CTok != KET)
	       {
		  parse_error ("Expecting ']'", 1);
		  return;
	       }
#endif	     
	     get_token();
	     if (CTok == ASSIGN)
	       {
		  get_token();
		  expression();
#if !NEW_STUFF
		  save_again = Input;  save_ctok = CTok;
		  Input = save;
		  get_token();
		  arguments(KET);
		  pop_name();
		  (*SLcompile_ptr)("aput");
		  Input = save_again;  CTok = save_ctok;
#else
		  pop_name();
		  (*SLcompile_ptr)("__aput");
#endif
		  
	       }
	     else
	       {
#if !NEW_STUFF
		  Input = save;
		  get_token();
		  arguments(KET);
#endif
		  pop_name();
		  (*SLcompile_ptr)("aget");
	       }
	     return;
	  }
	pop_name();		       /* this push at 'case NAME:' */
	break;
	
      case SLANG_MINUS: 
	get_token();
	basic();
	(*SLcompile_ptr)("chs");
	break;
      case LEFT_P:
	
	while (!SLang_Error && (CTok != END) && (CTok != RIGHT_P))
	  {
	     get_token();
	     if (CTok != RIGHT_P) expression();
	  }
	if (!SLang_Error && (CTok != RIGHT_P)) SLang_doerror("Unbalanced parenthesis!");
	get_token();
	break;
	
      default: 
/*      case BRA:
	case
      case END:
      case EOS_BRA: 
      case EOS_KET: 
      case EOS: */
	parse_error("Syntax Error.", 0);
     }
}


static void arguments(int match)
{
   while (!SLang_Error)
     {
	if (CTok == match)
	  {
	     get_token();
	     return;
	  }
	else if (CTok == COMMA) get_token();  /* was EOS */
	else if ((CTok == END) || /* (CTok == EOS) || */
		 (CTok == EOS_BRA) || (CTok == EOS_KET))
	  {
	     parse_error("Incomplete list", 0);
	  }
	else expression();
     }
}

/* To avoid compiler warnings, the 3rd dummy parameter is used */
static int fast_extract_token(char **sp, char *t, int len)
{
   /* first char is the length of token - 32 */
   char *s = *sp;
   
   len = (unsigned char) *s++;
   if (len <= 32) return 0;
   len -= 32;
   
   if (*s == '"')
     {
	SLexpand_escaped_string (t, s, s + len);
     }
   else 
     { 
	strncpy (t, s, (unsigned int) len);
	t[len] = 0;
     }
   
   *sp += len + 1;
   return 1;
}

   
   
static char *(*Get_Token_Read_Fun)(SLang_Load_Type *);

/* interprets line-- returns offset of last part of line evaluated */
char *SLang_rpn_interpret(char *line)
{
   char token[256];
   char *ret;
   int (*extract_token_ptr)(char **, char *, int);
   int byte_comp = ((long) SLcompile != (long) SLcompile_ptr);
   
   /* @ is signature to use fast method */
   if (*line == '@') 
     {
	extract_token_ptr = fast_extract_token;
	line++;
     }
   else extract_token_ptr = SLang_extract_token;
   
   while(ret = line, (*extract_token_ptr) (&line,token, byte_comp))
     {
	if (SLang_Error) break;
	if (*token == '%') break;
	(*SLcompile_ptr)(token);
	if (SLang_Error) break;
	/* puts(token); */
     }
   /* if (SLang_Error) SLang_doerror(NULL); */
   return(ret);
}

#if 0

#define MAX_DEFINES 10
#define MAX_DEFINE_LEN 16

static char SLdefines[MAX_DEFINES][MAX_DEFINE_LEN];

int SLdefine_for_ifdef (char *s)
{
   int n, i;
   char *place;
   
   for (i = 0; i < MAX_DEFINES; i++)
     {
	place = SLdefines[i];
	if (*place == 0)
	  {
	     n = strlen (s);
	     if (n > MAX_DEFINE_LEN - 2) n = MAX_DEFINE_LEN - 2;
	     *place++ = (char) n;
	     strncpy(place, s, n);
	     *(place + n) = 0;
	     return 1;
	  }
     }
   return 0;
}


static int is_any_defined(char *buf)
{
   char *sys, *buf_save = buf;
   int i = 0, n;
   register char ch;

   while ((i < MAX_DEFINES) && (sys = SLdefines[i], (n = (int) *sys++) != 0))
     {
	buf = buf_save;
	while (1)
	  {
	     while ((ch = *buf), ch && (ch != '\n') && (ch <= ' ')) buf++;
	     if ((ch <= '\n') || (ch == '%')) break;
	     if (!strncmp(buf, sys, n))
	       {
		  buf += n;
		  if ((*buf <= ' ') || (*buf == '%')) return 1;
	       }
	     else
	       {
		  while ((ch = *buf), (ch > ' ') && (ch != '%')) buf++;
	       }
	  }
	i++;
     }
   return 0;
}

#ifdef SLANG_WITH_IFISDEF
/* See below for what this is supposed to be about */
static int check_if_intrinsic (unsigned char *buf)
{
   unsigned char buffer[32], *bmax, *b;
   int n;
   
   while ((*buf == ' ') || (*buf == '\t')) buf++;
   if ((*buf == 0) || (*buf == '\n')) return 0;
   b = buffer;
   bmax = b + 31;
   while ((b < bmax) && (*buf > ' ')) *b++ = *buf++;
   if (*buf >  ' ') return 0;
   *b = 0;
   n = SLang_is_defined ((char *)buffer);
   return (n * n == 1);
}
#endif
   
static int slang_line_ok (char *buf, int *levelp, int *exec_levelp)
{
   int level = *levelp;
   int exec_level = *exec_levelp;
   
   if (buf == NULL) return (1);
   if (*buf == '\n') return (0);
   if (*buf == '%') return (0);     /* since '%' is a comment */
	
   if (*buf == '#')
     {
	buf++;
	if (!strncmp(buf, "ifdef", 5))
	  {
	     if (level != exec_level) level++;
	     else
	       {
		  level++;
		  if (is_any_defined(buf + 5))
		    {
		       exec_level = level;
		    }
	       }
	  }
	else if (!strncmp(buf, "ifndef", 6))
	  {
	     if (level != exec_level) level++;
	     else
	       {
		  level++;
		  if (!is_any_defined(buf + 6))
		    {
		       exec_level = level;
		    }
	       }
	  }
	else if (!strncmp(buf, "else", 4))
	  {
	     if (level == exec_level + 1) exec_level = level;
	     else if (level == exec_level) exec_level--;
	  }
	else if (!strncmp(buf, "endif", 5))
	  {
	     if (level == exec_level) exec_level--;
	     level--;
	  }
#ifdef SLANG_WITH_IFISDEF
	else if (!strncmp(buf, "ifisdef", 7))
	  {
	     if (level != exec_level) level++;
	     else
	       {
		  level++;
		  if (check_if_intrinsic ((unsigned char *) buf + 7))
		    {
		       exec_level = level;
		    }
	       }
	  }
	else if (!strncmp(buf, "ifisndef", 8))
	  {
	     if (level != exec_level) level++;
	     else
	       {
		  level++;
		  if (!check_if_intrinsic((unsigned char *) buf + 8))
		    {
		       exec_level = level;
		    }
	       }
	  }
#endif /* SLANG_WITH_IFISDEF */
	/* Allow '#!' to pass.  This could be a shell script with something
	   like '#! /local/bin/slang'  */
	else if (*buf == '!') return 0;
	else return 1;  /* It will Bomb. */

	if (exec_level < 0) return 1;

	*levelp = level;
	*exec_levelp = exec_level;
	return 0;
     }
   return (level == exec_level);
}
   
/* preprocessor */

#endif  /* if 0 */

static SLPreprocess_Type *This_SLpp;

#define STREQS(a,b) ((*(a) == *(b)) && !strcmp(a,b))



#define MAX_PUSHED_TOKENS 3
static struct 
{
   char token[32];
   int ctok;
} Token_Stack[MAX_PUSHED_TOKENS];

static int Num_Pushed_Tokens;

static void push_token (char *t, int n)
{
   if ((Num_Pushed_Tokens == MAX_PUSHED_TOKENS) || (*t == '"'))
     {
	SLang_Error = SYNTAX_ERROR;
	Num_Pushed_Tokens = 0;
	return;
     }
   strcpy (Token_Stack[Num_Pushed_Tokens].token, t);
   Token_Stack[Num_Pushed_Tokens].ctok = n;
   Num_Pushed_Tokens++;
}

static int pop_token ()
{
   if (Num_Pushed_Tokens == 0) return 0;
   Num_Pushed_Tokens--;
   strcpy (Token, Token_Stack[Num_Pushed_Tokens].token);
   CTok = Token_Stack[Num_Pushed_Tokens].ctok;
   return 1;
}

   
static int get_token(void)
{
   int type;
   int byte_comp = ((long) SLcompile != (long) SLcompile_ptr);
   if (SLang_Error) return (CTok = END);
   
   if (pop_token ()) return CTok;
   
   if ((Input != NULL) && (Input != Terminate_String)
       && (Input != Terminate_String + 1))
     LLT->ofs = (int) (Input - LLT->buf);
   
   while ((Input == NULL) || (0 == SLang_extract_token(&Input, Token, byte_comp))
	  || (*Token == '%'))
     {
	do
	  {
	     LLT->n++;
	     if ((NULL == (Input = (*Get_Token_Read_Fun)(LLT))) 
		 || SLang_Error) return(CTok = END);
	     
	  }
	while (!SLprep_line_ok(Input, This_SLpp));
	/* lines beginning with a '.' are RPN */
	if (*Input == '.') return(CTok = INLINE_TOK);
	else if ((*Input == '@') && (*at_handler_name)) return CTok = SPECIAL_AT_TOK;
     }
   

   if (0 == Token[1])
     {
	switch (*Token)
	  {
	   case '(':  return(CTok = LEFT_P);
	   case ')':  return(CTok = RIGHT_P);
	   case ',':  return(CTok = COMMA);
	   case ';':    return(CTok = EOS);
	   case '=':  return(CTok = ASSIGN);
	   case '[':  return(CTok = BRA);
	   case ']':  return(CTok = KET);
	   case '{':  return(CTok = EOS_BRA);
	   case '}':  return(CTok = EOS_KET);
	  }
     }
   
   if (*Token == '"') return (CTok = NAME);
   else if (0 != (type = _SLeqs_name(Token, SL_Binary_Ops)))
     {
	/* Hack for dealing with && and || by C programmer. */
	if ((Input == NULL) || (*Input != *Token))
	  return (CTok = abs(type));
	parse_error ("Binary op not supported.", 1);
	return (CTok = END);
     }

   if (STREQS("!if", Token)) return (CTok = IF_NOT_TYPE);
   if (STREQS("if", Token)) return (CTok = IF_TYPE);
   if (STREQS("else", Token)) return (CTok = ELSE_TYPE);
   if (STREQS("forever", Token)) return (CTok = FOREVER_TYPE);
   if (STREQS("while", Token)) return (CTok = WHILE_TYPE);
   if (STREQS("variable", Token)) return (CTok = V_TYPE);
   if (STREQS("define", Token)) return (CTok = F_TYPE);
   if (STREQS("for", Token)) return (CTok = CFOR_TYPE);
   if (STREQS("loop", Token)) return (CTok = LOOP_TYPE);
   if (STREQS("switch", Token)) return (CTok = SWITCH_TYPE);
   if (STREQS("orelse", Token)) return (CTok = ORELSE_TYPE);
   if (STREQS("andelse", Token)) return (CTok = ANDELSE_TYPE);
   if (STREQS("return", Token)) return (CTok = RETURN_TYPE);
   if (STREQS("+=", Token)) return (CTok = ASSIGN_P);
   if (STREQS("-=", Token)) return (CTok = ASSIGN_M);
   if (STREQS("_for", Token)) return (CTok = FOR_TYPE);
   if (STREQS("do", Token)) return (CTok = DO_WHILE);
   if (STREQS("ERROR_BLOCK", Token)) return(CTok = ERROR_B_TYPE);
   if (STREQS("EXIT_BLOCK", Token)) return(CTok = EXIT_B_TYPE);
   if ((*Token == 'U') && !strncmp(Token, "USER_BLOCK", 10)
       && (Token[10] < '5') && (Token[10] >= '0') && (Token[11] == 0))
     return CTok = USER_B_TYPE;
   if (STREQS("case", Token)) return(CTok = CASE_TYPE);
   if (STREQS("global_variable", Token)) return(CTok = GV_TYPE);
   if (*Token == '@')
     {
	parse_error("Illegal Name.", 1);
	return (CTok = END);
     }
   if (STREQS("&&", Token) || STREQS("||", Token))
     {
	parse_error ("Binary ops `||' and `&&' not supported", 1);
	return (CTok = END);
     }

   return (CTok = NAME);
}

/* Since these routines must be re-entrant, the context is saved and 
   later restored. */


static int prep_exists_function (char *line, char comment)
{
   char buf[256], *b, *bmax;
   unsigned char ch;
   
   bmax = buf + 255;
   
   while (1)
     {
	/* skip whitespace */
	while ((ch = (unsigned char) *line), 
	       ch && (ch != '\n') && (ch <= ' '))
	  line++;
	
	if ((ch <= '\n') 
	    || (ch == (unsigned char) comment)) break;
	
	b = buf;
	while ((ch = (unsigned char) *line) > ' ')
	  {
	     if (b < bmax) *b++ = (char) ch;
	     line++;
	  }
	*b = 0;
	
	if (SLang_is_defined (buf))
	  return 1;
     }
   
   return 0;
}

static void SLang_eval_object(SLang_Load_Type *x)
{
   char *(*last_read_fun)(SLang_Load_Type *) = Get_Token_Read_Fun;
   char *last_token = Token, *last_input = Input;
   SLPreprocess_Type *last_pp, this_pp;
   SLang_Load_Type *last_llt = LLT;   
   int last_ctok = CTok;
   
   last_pp = This_SLpp;
   
   if (SLprep_exists_hook == NULL)
     SLprep_exists_hook = prep_exists_function;
   
   if (-1 == SLprep_open_prep (&this_pp))
     return;
   
   This_SLpp = &this_pp;

   x->ofs = x->n = 0;
   Get_Token_Read_Fun = x->read;
   Token = x->token;
   LLT = x;
   
   Input = NULL;
   
   /* Name_Stack_P = 0; */  /* This should be put in restart_slang */
   
   LLT->top_level = 1;
   get_token();
   while (!SLang_Error && (CTok != END))
     {
	LLT->top_level = 0;
	if ((CTok == EOS)
#if NEW_STUFF
	    || (CTok == COMMA)
#endif
	    )
	  {
	     LLT->top_level = 1;
	     get_token();
	  }
	else if (CTok != END) directive();
     }
   
   if (SLang_Error) 
     SLang_restart (0);
   
   /* x.ptr = Input */
   
   Get_Token_Read_Fun = last_read_fun;
   Token = last_token;
   CTok = last_ctok;
   LLT = last_llt;
   Input = last_input;
   This_SLpp = last_pp;
}




int (*SLang_User_Open_Slang_Object)(SLang_Load_Type *);
int (*SLang_User_Close_Slang_Object)(SLang_Load_Type *);
char *SLang_User_Prompt;

static char *slang_read_from_file (SLang_Load_Type *x)
{
   if ((x->handle == (long) stdin) && (SLang_User_Prompt != NULL))
     {
	fputs(SLang_User_Prompt, stdout);
	fflush(stdout);
     }
   
   return fgets((char *) x->buf, 511, (FILE *) x->handle);
}

static char *slang_read_from_string (SLang_Load_Type *x)
{
   char *s, ch, *s1;
   
   if (x->handle == -1) return (NULL);
   else if (x->handle == 0)
     {
	x->handle = -1;
	return Terminate_String;
     }
   
   s1 = s = x->ptr;
   while ((ch = *s++) != 0) if (ch == '\n') break;
   x->handle--;
   x->ptr = s;
   return (s1);
}


static int slang_close_object(SLang_Load_Type *x)
{
   int status;
   if ((SLang_User_Close_Slang_Object != NULL)
       && ((status = (*SLang_User_Close_Slang_Object)(x)) != SL_OBJ_UNKNOWN))
     {
	return(status);
     }
   
   switch (x->type)
     {
      case 'C':  /* File */
      case 'F':  /* File */
	
	if (x->handle != (long) stdin) fclose((FILE *) x->handle);
	SLFREE(x->buf);
	x->ptr = NULL;
	return (0);
	
      case 'S':  /* string */
	return (0);
	
      default: return SL_OBJ_UNKNOWN;
     }
}


/* returns 0 if successful */
static int slang_open_object(SLang_Load_Type *x)
{
   int status, n;
   char *s, ch;

   if (SLang_User_Open_Slang_Object != NULL)
     {
	status = (*SLang_User_Open_Slang_Object)(x);
	if ((status == 0) || (status == SL_OBJ_NOPEN)) return(status);
	
	/* pass control to default */
     }
   
   switch (x->type)
     {
      case 'C': 
      case 'F':  /* File */
	
	x->read = slang_read_from_file;
	if ((x->name == 0) || (*(char *)(x->name) == 0))
	  {
	     x->name = (long) "<stdin>";
	     x->handle = (long) stdin;
	  }
	else if (0 == (x->handle = (long) fopen((char *) x->name, "r")))
	  {
	     return (SL_OBJ_NOPEN);
	  }
	
	if (NULL == (x->buf = (char *) SLMALLOC(512)))
	  {
	     SLang_Error = SL_MALLOC_ERROR;
	     if (x->handle != (long) stdin) fclose((FILE *) x->handle);
	     return(SL_OBJ_NOPEN);
	  }
	x->ptr = x->buf;
	return (0);
	
      case 'S':  /* string */
	
	x->read = slang_read_from_string;
	s = (char *) x->name;
	x->ptr = x->buf = s;
	
	/* handle represents the number of lines in the string. */
	n = 1; 
	while ((ch = *s++) != 0) 
	  {
	     if (ch == '\n') n++;
	  }
	x->handle = n;
	
	return (0);
	
      default: return SL_OBJ_UNKNOWN;
     }
}


int SLang_load_object(SLang_Load_Type *x)
{
   int status;
   
   status = slang_open_object(x);
   if (status != 0) return (status);
   SLang_eval_object(x);
   slang_close_object(x);
   if (SLang_Error) 
     {
	Name_Stack_P = 0;
	Num_Pushed_Tokens = 0;
     }
   
   return SLang_Error;
}

   

   
/* Note that file could be freed from Slang during run of this routine
   so get it and store it !! (e.g., autoloading) */
      
int SLang_load_file (char *f)
{
   SLang_Load_Type x;
   char file[256]; 
   
   if (f != NULL) strcpy(file, f); else *file = 0;
   
   x.name = (long) file;
   x.type = 'F';
   if (SL_OBJ_NOPEN == SLang_load_object(&x))
     {
	_SLdo_error ("%s: open error", file);
	return 0;
     }
   else if (SLang_Error) 
     {
	do_line_file_error( x.n, (char *) x.name);
	return 0;
     }
   return 1;
}

char *SLang_load_string(char *string)
{
   SLang_Load_Type x;
   
   x.name = (long) string;
   x.type = 'S';
   SLang_load_object(&x);
   if (SLang_Error) 
     {
	_SLdo_error ("eval: %s", string);
     } 
   return((char *)x.name + x.ofs);
}
   

static FILE *byte_compile_fp;
static int Slang_Line_Len;
static int defining_variables;

static void SLang_byte_compile(char *s)
{
   int n = Slang_Line_Len;
   int dn;
   unsigned char ch;
   
   if (SLang_Error) return;
   
   if (!defining_variables) s = SLbyte_compile_name(s);
   if (*s == 0) return;
   if (*s == '[') defining_variables = 1;
   else if (*s == ']') defining_variables = 0;
   
   dn = strlen(s);
   n += dn;
   if (n > 250)
     {
	fputs("\n.", byte_compile_fp);
	if (SLPreprocess_Only != 2)
	  {
	     fputc ('@', byte_compile_fp);
	  }
	n = dn;
     }
   
   if (SLPreprocess_Only != 2)
     {
	ch = (unsigned char) dn;
	ch += 32;
     }
   else ch = ' ';
   
   putc((char) ch, byte_compile_fp);
   
   fputs(s, byte_compile_fp);
   Slang_Line_Len = n + 1;
}

void SLang_byte_compile_file(char *f, int *method)
{
   char file[256];
   SLang_Load_Type x;
   int status;
   
   SLPreprocess_Only = *method;
   sprintf(file, "%sc", f);
   if ((byte_compile_fp = fopen(file, "w")) == NULL)
     {
	_SLdo_error("%s: unable to open", file);
	return;
     }
   
   x.name = (long) f;
   x.type = 'C';

   Slang_Line_Len = 1;
   fputs(".@", byte_compile_fp);
   SLcompile_ptr = SLang_byte_compile;
   status = SLang_load_object(&x);
   SLcompile_ptr = SLcompile;
      
   putc('\n', byte_compile_fp);
   fclose(byte_compile_fp);

   if (SL_OBJ_NOPEN == status)
     {
	_SLdo_error ("%s: Error opening for byte compile.", f);
     }
   else if (SLang_Error) 
     {
	do_line_file_error (x.n, (char *) x.name);
     }
}


