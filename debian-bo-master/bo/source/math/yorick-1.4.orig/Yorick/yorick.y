/*
    YORICK.Y

    YACC description of Yorick grammar.

    $Id$
 */
/*    Copyright (c) 1994.  The Regents of the University of California.
                    All rights reserved.  */

/*--------------------------------------------------------------------------*/
/* Begin C declarations required for actions */
%{

#include "parse.h"
#include "defstr.h"

/* The HP yacc needs this to avoid stack overflow errors --
   The default max stack depth of 150 only allows about 12 else-if
   clauses before it blows.  If this sort of problem occurs many other
   platforms, should consider making a generic solution in sysdep.h.  */
#define __RUNTIME_YYMAXDEPTH

extern int ypSkipIncludes;  /* in yinput.c */

/* ScanForFunc is a "pseudo-parser" which merely calls yylex until
   it finds a func, struct, or extern definition for fname.  It returns
   the location of the line on which the definition began, suitable
   for use with fseek.  */
extern long ScanForFunc(const char *fname, int notExtern);
extern long YpStandby(void);
extern char *YpLitName(Literal);

#ifdef DO_DEBUG
#include <stdio.h>
#define YYDEBUG 1
/* must also set yydebug non-0 */
#endif

#ifdef YY_PROTOTYPES
extern int yylex(void);
extern void yyerror(char *s);
#endif

/* The Yorick grammar requires two "lexical tie-ins"-- both alter the
   token returned by yylex when an end-of-line (EOL) is encountered:
   (1) If parentheses () and square brackets [] are balanced, then
       EOL returns a ';' token, unless the previous token was ';'.
       If the token immediately before EOL was an operator which needs
       a right operand, yylex treats it as if there were an open
       parenthesis-- no ';' token is generated.
   (2) If there are no open control structures (func or struct definitions,
       if, while, do, for or compound statements), then EOL returns the
       YACC end-of-file (EOF) token.  This is applied after rule (1).
 */
static int nOpenCS;     /* The openCS rule increments nOpenCS, while the
			   action when the control structure is complete
			   decrements it.  */
static int parenDepth;  /* The openP rule increments parenDepth; the
			   closeP rule decrements it.  */
static int braceDepth;  /* The openC rule increments braceDepth; the
			   closeC rule decrements it.  */
static int needOperand; /* set if previous token needs right operand */
static int supressSemi; /* set to prevent yylex from returning ';' at EOL
			   -- used for if, while, do, and for */
static int needBody;    /* set if looking for open curly brace */
static int keyCount;    /* key_expr state information */

/* A bizzare lexical tie-in is required for graphics routines which
   need the textual form of their arguments to generate default
   legend strings...  */
static int nQuines, quining;
static void BeginQuine(void);
static void GrabQuine(char *stop);
static int DoQuine(void);

static int EndOfLine(void);

%}
/* End C declarations required for actions */
/*--------------------------------------------------------------------------*/



/*--------------------------------------------------------------------------*/
/* Begin YACC symbol declarations */

%union {
  long l;
  double d;
  Quote q;
  int i;            /* counter or index */
  CodeBlock cb;     /* code block index (see parse.c) */
  Literal lit;      /* literal table index (see parse.c) */
  CBorLit def;      /* CodeBlock or Literal for assign_predicate */
}

%token <l> CHAR SHORT INT LONG CHAR_ERR
%token <d> FLOAT DOUBLE IMAGINARY
%token <q> STRING STRING_ERR SYSCALL
%token <lit> NAME
%token <i> RFNAME
%token FUNC STRUCT
%token IF ELSE WHILE DO FOR GOTO CONTINUE BREAK RETURN EXTERN LOCAL

%token DOTDOT NEXT_ARG MORE_ARGS

/* operators from lowest to highest precedence */
%right '=' PEQ MEQ TMSEQ DIVEQ MODEQ SHLEQ SHREQ ANDEQ XOREQ OREQ
%left OR
%left AND
%left '|'
%left '~'
%left '&'
%left EQ NE
%left '<' '>' LE GE
%left SHL SHR
%left '+' '-'
%left '*' '/' '%'
%right '^'
%right UNARY   /* fictitious terminal for unary operator precedence */
%right '!'
%left '.' ARROW
%nonassoc PLUSPLUS MINUSMINUS

%token NOINPUT

%type <i> assign_op pointer range_func
%type <i> arg_list func_arg_list lj_arg_list rj_arg_list expr_list
%type <def> assign_predicate

%type <cb> compound statement_list statement nil_statement get_pc
%type <cb> assign_expr range_expr simple_range cond_expr expr
%type <cb> for_expr assign_expr_list unary_expr term pm_expr basic_expr
%type <cb> constant variable struct_name member rfname
%type <cb> nil_expr key_expr

/* End YACC symbol declarations */
/*--------------------------------------------------------------------------*/



/*--------------------------------------------------------------------------*/
/* Begin YACC grammar rules */
%%

program_unit:                       /* function, structure, or main program */
    NOINPUT
  | main_program
           { YpFunc(1, 1); }

  | openCS FUNC func_name compound
           { nOpenCS--; YpFunc(0, EndOfLine()); }
  | openCS FUNC func_name openP param_list closeP_wait compound
           { nOpenCS--; YpFunc(0, EndOfLine()); }
  | openCS STRUCT struct_name openC declar_list closeC
           { nOpenCS--; YpStruct($3, EndOfLine()); }

  | openCS FUNC func_name openP error closeP compound
           { nOpenCS--; YpFunc(0, EndOfLine()); }
  | openCS FUNC func_name error
           { nOpenCS--; needBody=0; YpFunc(0, EndOfLine()); }
  | openCS STRUCT struct_name openC error closeC
           { nOpenCS--; YpStruct($3, EndOfLine()); }
  | openCS STRUCT error
           { nOpenCS--; needBody=0; YpStruct(NONE, EndOfLine()); }
  ;

main_program:
    statement
  | EXTERN RFNAME ';'            { YpSpecial($2); }
  | main_program statement
  | error ';'
  ;

/* Every rule in which openCS occurs MUST do nOpenCS--; in its action.
   Should come BEFORE the token which introduces the control structure,
   so that when the introductory token becomes the look-ahead token,
   the lexical tie-in is activated.
   IMPORTANT-- every rule containing openCS must have a corresponding
   error rule so that nOpenCS is adjusted correctly when a syntax
   error is detected.  */
openCS: /* empty */ { nOpenCS++; } ;

openP: '('       { parenDepth++; } ;
closeP: ')'      { parenDepth--; } ;
openB: '['       { parenDepth++; } ;
closeB: ']'      { parenDepth--; } ;

/* Open and close curly braces can be optionally followed by a semicolon
   in order to allow for automatically generated semicolons if the line
   ends at that point.  The close curly brace should allow for an
   optional actual semicolon it any case.  */
openC: '{' optional_semi       { braceDepth++; } ;
closeC: '}' optional_semi      { braceDepth--; } ;

optional_semi:
    /* empty */
  | ';'
  ;

/* ------------------------------ */

statement:
    EXTERN extern_vars ';'                   { $$= YpNoop(); }
  | LOCAL local_vars ';'                     { $$= YpNoop(); }

  | SYSCALL ';'                              { $$= YpSyscall($1); }

  | compound

  | assign_expr ';'                          { $$= YpAssignOrPrint($1); }

  | variable { if ((nQuines= YpCallInit($1))) quining= 1; } arg_sep
             lj_arg_list ';'  { nQuines= quining= 0; $$= YpCall($1, $4); }

  | if_test IF openP assign_expr brfalse closeP_wait statement if_done
                              { $$= YpIfElse($4, $7, NONE); }
  | if_test IF openP assign_expr brfalse closeP_wait statement if_done
            ELSE { nOpenCS++; } branch statement
                              { nOpenCS--; $$= YpIfElse($4, $7, $12); }
    /* Note-  yylex sets supressSemi when it detects ELSE */

  | wloop WHILE openP assign_expr brfalse closeP_wait nil_statement
                              { nOpenCS--; $$= YpWhile($4, $7); }
  | dloop DO statement WHILE openP assign_expr closeP ';'
                              { nOpenCS--; $$= YpDo($3, $6); }
  | floop FOR openP for_expr ';' assign_expr brfalse ';' inc_expr closeP_wait
          nil_statement
                              { nOpenCS--; $$= YpFor($4, $6, $11); }
  | floop FOR openP for_expr ';' ';' inc_expr closeP_wait nil_statement
                              { nOpenCS--; $$= YpFor($4, NONE, $9); }

  | CONTINUE ';'              { $$= YpContinue(); }
  | BREAK ';'                 { $$= YpBreak(); }
  | RETURN ';'                { $$= YpReturn(NONE); }
  | RETURN assign_expr ';'    { $$= YpReturn($2); }
  | RETURN range_expr ';'     { $$= YpReturn($2); }
  | GOTO NAME ';'             { $$= YpGoto($2); }
  | NAME lab_delim            { $$= YpLabel($1); }

  | if_test IF openP error closeP            { nOpenCS--; $$= YpReset(); }
  | if_test IF error                         { nOpenCS--; $$= YpReset(); }
  | wloop WHILE openP error closeP           { nOpenCS--; $$= YpReset(); }
  | wloop WHILE error                        { nOpenCS--; $$= YpReset(); }
  | dloop DO statement WHILE openP error closeP
                                             { nOpenCS--; $$= YpReset(); }
  | dloop DO statement WHILE error           { nOpenCS--; $$= YpReset(); }
  | dloop DO error ';'                       { nOpenCS--; $$= YpReset(); }
          /* DO: note that an error is also a statement */
  | floop FOR openP error closeP             { nOpenCS--; $$= YpReset(); }
  | floop FOR error                          { nOpenCS--; $$= YpReset(); }

  /*  | error ';'                                { $$= YpReset(); }
     If an error is a statement, there is no graceful way to recover
     from an error after DO but before WHILE.  Instead, better error
     recovery within a compound statement allows the parse to continue.  */
  ;

nil_statement:
    ';'                                      { $$= YpNoop(); }
  | statement
  ;

compound:                                             /* compound statement */
    openC closeC                             { $$= YpNoop(); }
  | openC statement_list closeC              { $$= $2; }
  | openC error closeC                       { $$= YpReset(); }
  | openC error statement_list closeC        { $$= YpReset(); }
  ;

statement_list:                               /* list of several statements */
    statement
  | statement_list statement
  ;

branch:
    /* empty */     { YpBranch(0); }
  ;

brfalse:
    /* empty */     { YpBranch(1); }
  ;

brtrue:
    /* empty */     { YpBranch(2); }
  ;

branch_pop:
    /* empty */     { YpBranch(3); }
  ;

closeP_wait:
    { supressSemi= 1; } closeP
  ;

if_test:
    /* openCS */    { supressSemi= 1; nOpenCS++; }
  ;

if_done:
    /* openCS */    { nOpenCS--; }
  ;

wloop:
    /* openCS */    { supressSemi= 1; nOpenCS++; YpLoop(0); }
  ;

dloop:
    /* openCS */    { supressSemi= 1; nOpenCS++; YpLoop(1); }
  ;

floop:
    /* openCS */    { supressSemi= 1; nOpenCS++; YpLoop(2); }
  ;

lab_delim:  { supressSemi= 1; } ':' ;

/* ------------------------------ */

declar_list:        /* list of member declarations for structure definition */
    data_type declarators ';'
  | declar_list data_type declarators ';'
  ;

data_type:
    NAME             { YpDataType($1); }
  ;

declarators:                         /* list of member names and dimensions */
    declarator
  | declarators ',' declarator
  ;

declarator:                           /* one member name and its dimensions */
    pointer member                           { YpDeclarator($1, $2, 0); }
  | pointer member openP expr_list closeP    { YpDeclarator($1, $2, $4); }

  | pointer member openP error closeP        { YpReset(); }
  ;

pointer:              /* all levels of indirection are equivalent in Yorick */
    /* empty */   { $$= 0; }
  | pointer '*'   { $$= $1+1; }
  ;

expr_list:  /* list of non-nil expressions for declarator dimensions, [] op */
    expr                  { $$= 1; }
  | simple_range          { $$= 1; }
  | expr_list ',' expr    { $$= $1 + 1; }
  ;

member:  NAME     { $$= YpQuoted($1); }
  ;

func_name:  NAME         { needBody= 1; YpInitFunc($1); }
  ;

struct_name:  { needBody= 1; } variable   { $$= $2; }
  ;

/* ------------------------------ */

param_list:                 /* dummy parameter list for function definition */
    positionals
  | positionals ',' keywords
  | keywords
  | dot_dot
  | positionals ',' dot_dot
  | positionals ',' dot_dot ',' keywords
  | dot_dot ',' keywords 
  ;

positionals:                           /* positional part of parameter list */
    NAME                          { YpPosParam($1, 0); }
  | positionals ',' NAME          { YpPosParam($3, 0); }
  | '&' NAME                      { YpPosParam($2, 1); }
  | positionals ',' '&' NAME      { YpPosParam($4, 1); }
  ;

keywords:                                 /* keyword part of parameter list */
    NAME '='                      { YpKeyParam($1); }
  | keywords ',' NAME '='         { YpKeyParam($3); }
  ;

dot_dot:
    DOTDOT                        { YpHasPosList(); }
  ;

/* ------------------------------ */

arg_list: /* turn off argument quining for function calls or array indexing */
    { if (nQuines) quining++; } func_arg_list
                                      { $$= $2; if (nQuines) quining--; }
  ;

arg_sep: { if (quining==1) BeginQuine(); } ',' ;

func_arg_list:                            /* actual parameter or index list */
    lj_arg_list
  | rj_arg_list
  | lj_arg_list arg_sep rj_arg_list   { $$= $1 + $3; }
  ;

lj_arg_list:                    /* left justified part of actual index list */
    key_expr                          { $$= keyCount; YpCheckRef($1); }
  | lj_arg_list arg_sep key_expr      { $$= $1 + keyCount; YpCheckRef($3); }
  ;

rj_arg_list:                   /* right justified part of actual index list */
    dotdot                            { $$= 1; }
  | dotdot ',' lj_arg_list            { $$= 1 + $3; }
  ;

dotdot:
    DOTDOT      { YpDotDot(0); }
  | '*'         { YpDotDot(1); }
  ;

key_expr:                     /* one actual positional or keyword parameter */
    nil_expr                     { $$= $1; keyCount= 1 + DoQuine(); }
  | range_expr                   { $$= $1; keyCount= 1 + DoQuine(); }
  | NAME '=' nil_expr            { $$= $3; keyCount= 2; YpKeyword($1, $3); }
  | NAME '=' range_expr          { $$= $3; keyCount= 2; YpKeyword($1, $3); }
  ;

nil_expr:            /* non-assignment/range/ternary expression, or nothing */
    /* empty */                       { $$= YpNil(); }
  | expr
  ;

range_expr:
    simple_range
  | range_func                          { $$= YpRangeFunc($1, NONE); }
  | range_func ':'                      { $$= YpRangeFunc($1, NONE); }
  | range_func ':' simple_range         { $$= YpRangeFunc($1, $3); }
  ;

simple_range:
    nil_expr ':' nil_expr               { $$= YpRange($1, 0); }
  | nil_expr ':' nil_expr ':' nil_expr  { $$= YpRange($1, 1); }
  ;

range_func:
    RFNAME
  | '+'        { $<i>$= 14; }
  | '-'        { $<i>$= 15; }
  ;

extern_vars:                               /* arguments to EXTERN statement */
    NAME                          { YpExtern($1); }
  | extern_vars ',' NAME          { YpExtern($3); }
  ;

local_vars:                                 /* arguments to LOCAL statement */
    NAME                          { YpLocal($1); }
  | local_vars ',' NAME           { YpLocal($3); }
  ;

/* ------------------------------ */

assign_expr:                          /* expression or assignment statement */
    cond_expr
  | assign_predicate assign_expr      { $$= YpAssign($1, $2); }
  | assign_predicate range_expr       { $$= YpAssign($1, $2); }
  | unary_expr assign_op assign_expr  { $$= YpIncrement($1, $2, $3); }
  ;

assign_predicate:
    unary_expr '='                    { $$= YpCheckDefine($1); }
  ;

assign_op:            /* increment operator += -= *= /= %= <<= >>= &= ~= |= */
    PEQ       { $$= 0; }
  | MEQ       { $$= 1; }
  | TMSEQ     { $$= 2; }
  | DIVEQ     { $$= 3; }
  | MODEQ     { $$= 4; }
  | SHLEQ     { $$= 5; }
  | SHREQ     { $$= 6; }
  | ANDEQ     { $$= 7; }
  | XOREQ     { $$= 8; }
  | OREQ      { $$= 9; }
  ;

/* ------------------------------ */

cond_expr:               /* ternary operator -- not necessarily implemented */
    expr
  | expr openCS '?' brfalse expr ':' branch_pop cond_expr
                              { nOpenCS--; $$= YpTernop($1, $5, $8); }
  | expr openCS '?' error
                              { nOpenCS--; $$= YpReset(); }
  ;

/* ------------------------------ */

expr:   /* expression not involving assignment, index range, or ?: operator */
    pm_expr

  | expr '*' expr      { $$= YpMultop($1); }
  | expr '/' expr      { $$= YpBinop($1, 2); }
  | expr '%' expr      { $$= YpBinop($1, 3); }

  | expr '+' expr      { $$= YpBinop($1, 4); }
  | expr '-' expr      { $$= YpBinop($1, 5); }

  | expr SHL expr      { $$= YpBinop($1, 6); }
  | expr SHR expr      { $$= YpBinop($1, 7); }

  | expr '<' expr      { $$= YpBinop($1, 8); }
  | expr '>' expr      { $$= YpBinop($1, 9); }
  | expr LE expr       { $$= YpBinop($1, 10); }
  | expr GE expr       { $$= YpBinop($1, 11); }
  | expr EQ expr       { $$= YpBinop($1, 12); }
  | expr NE expr       { $$= YpBinop($1, 13); }

  | expr '&' expr      { $$= YpBinop($1, 14); }
  | expr '~' expr      { $$= YpBinop($1, 15); }
  | expr '|' expr      { $$= YpBinop($1, 16); }

  | expr AND brfalse expr     { $$= YpLogop($1, 0, $4); }
  | expr OR brtrue expr       { $$= YpLogop($1, 1, $4); }
  ;

pm_expr:
    term
  | '+' term           { $$= YpUnop(2, $2); }
  | '-' term           { $$= YpUnop(3, $2); }
  ;

term:
    unary_expr
  | unary_expr '^' expr      { $$= YpBinop($1, 0); }
  ;

unary_expr:                /* one term, possibly preceded by unary operator */
    basic_expr

  | '*' unary_expr  %prec UNARY  { $$= YpUnop(0, $2); }
  | '&' basic_expr  %prec UNARY  { YpCheckRef($2); $$= YpUnop(1, $2); }

  | '~' basic_expr  %prec UNARY  { $$= YpUnop(4, $2); }
  | '!' basic_expr  %prec UNARY  { $$= YpUnop(5, $2); }

  | PLUSPLUS basic_expr   %prec UNARY  { $$= YpUnop(6, YpCheckDefine($2)); }
  | MINUSMINUS basic_expr %prec UNARY  { $$= YpUnop(7, YpCheckDefine($2)); }
  ;

basic_expr:                     /* one term, including any postfix operator */
    constant
  | variable

  | basic_expr openP { YpEvalInit($1); } arg_list closeP
                                          { $$= YpEval($1, $4); }
  | rfname openP arg_list closeP          { $$= YpEval($1, $3); }
  | NEXT_ARG openP closeP                 { $$= YpNextArg(0); }
  | MORE_ARGS openP closeP                { $$= YpNextArg(1); }
  | openB get_pc expr_list closeB         { $$= YpBuild($2, $3); }
  | openB closeB                          { $$= YpNil(); }
    /* Note-- cant use curly braces similarly, because, e.g.-
       { variable, arg1, arg2, arg3, arg4 }     and
       { variable, arg1, arg2, arg3, arg4; }
       could not be distinguished until the final semicolon (the second
       is a compound statement with a single subroutine call, the first
       would be some kind of list composed of 5 objects)  */

  | basic_expr '.' NAME            { $$= YpMember($1, 0, $3); }
  | basic_expr ARROW NAME          { $$= YpMember($1, 1, $3); }

  | basic_expr PLUSPLUS            { $$= YpPostfix(YpCheckDefine($1), 0); }
  | basic_expr MINUSMINUS          { $$= YpPostfix(YpCheckDefine($1), 1); }

  | openP assign_expr closeP       { $$= $2; }

  | basic_expr openP error closeP     { $$= YpReset(); }
  | rfname openP error closeP         { $$= YpReset(); }
  | openB error closeB                { $$= YpReset(); }
  | openP error closeP                { $$= YpReset(); }
  ;

/* ------------------------------ */

get_pc:  /* return current pc */    { $$= YpNoop(); }
  ;

for_expr:                            /* initialize section of FOR statement */
    /* empty */                         { $$= YpNoop(); }
  | assign_expr_list
  ;

inc_expr:                             /* increment section of FOR statement */
    /* empty */
  | { YpBeginInc(); } assign_expr_list  { YpEndInc($2); }
  ;

assign_expr_list:
    assign_expr                         { $$= $1; YpDrop(); }
  | assign_expr_list ',' assign_expr    { $$= $1; YpDrop(); }
  ;

/* ------------------------------ */

variable:  NAME   { $$= YpVariable($1); }
  ;

rfname:  RFNAME   { $$= YpPushRF($1); }
  ;

constant:
    CHAR          { $$= YpChar($1); }
  | SHORT         { $$= YpShort($1); }
  | INT           { $$= YpInt($1); }
  | LONG          { $$= YpLong($1); }
  | FLOAT         { $$= YpFloat($1); }
  | DOUBLE        { $$= YpDouble($1); }
  | STRING        { $$= YpString($1); }
  | IMAGINARY     { $$= YpImaginary($1); }
  ;

%%
/* End YACC grammar rules */
/*--------------------------------------------------------------------------*/



/*--------------------------------------------------------------------------*/
/* Begin C code */

static char *curLine, *nextChar= 0;
static int curChar;
static char *prevToken;

static int prevWasSemi, prevWasComma, noInputYet;
static char *quineBegin, *quineEnd, *quineText;

static int EndOfLine(void);

/* CheckMulti checks for multi-character special symbols.
   In the "multis" list, CheckMulti assumes that any 3 character
   symbol comes immediately after a matching 2 character symbol, AND
   no other symbols with the same FIRST character follow.
   More than one 3-symbol may not match a 2-symbol, and symbols of
   more than 3 characters are not allowed.  */
static int CheckMulti(void);

/* CheckName checks for reserved words and range function names.
   The "reserved" list must be alphabetized.  */
static int CheckName(char *name);

static char *AppendString(char *s, long ns, char *t, long nt);
static int StringValue(void);
static int CharValue(void);
static int NumberValue(void);

/* CheckSyscall checks for $ system escape sequence */
static int CheckSyscall(void);

/* ------------------------------ */

int YpParse(void *func)
{
  YpParseInit(func);
  nOpenCS= 0;    /* initialize number of open control structures */
  parenDepth= 0; /* initialize number of open parentheses/square brackets */
  braceDepth= 0; /* initialize number of open curly braces */
  needOperand= prevWasSemi= prevWasComma= supressSemi= 0; /* more EOL flags */
  nQuines= quining= 0;
  ypSkipIncludes= 0;  /* see ScanForFunc */
  curLine= nextChar= YpNextLine(NL_MAIN);
  prevToken= 0;
  if (!nextChar) return 0;
  noInputYet= 1;

  /* Note: If YACC does not generate a prototype for yyparse, it may
     be slightly dangerous to do so here (e.g.- Could it be a macro?
     Is it "int yyparse(void)" or "int yyparse()"?)... */
  return yyparse();            /* invoke YACC generated parser */
}

/* ------------------------------ */

static char *multis[]= { "!=", "%=", "&&", "&=", "*=", "++", "+=",
  "--", "-=", "->", "..", "/=", "<<", "<<=", "<=", "==", ">=", ">>", ">>=",
  "|=", "||", "~=", "" };

static int multiTypes[]= { NE, MODEQ, AND, ANDEQ, TMSEQ, PLUSPLUS, PEQ,
  MINUSMINUS, MEQ, ARROW, DOTDOT, DIVEQ, SHL, SHLEQ, LE, EQ, GE, SHR, SHREQ,
  OREQ, OR, XOREQ };

static char *reserved[]= { "avg", "break", "continue", "cum", "dif", "do",
  "else", "extern", "for", "func", "goto", "if", "local", "max", "min", "mnx",
  "more_args", "mxx", "next_arg", "pcen", "psum", "ptp", "return", "rms",
  "struct", "sum", "uncp", "while", "zcen", "" };

static int reservedTypes[]= { RFNAME, BREAK, CONTINUE, RFNAME, RFNAME, DO,
  ELSE, EXTERN, FOR, FUNC, GOTO, IF, LOCAL, RFNAME, RFNAME, RFNAME,
  MORE_ARGS, RFNAME, NEXT_ARG, RFNAME, RFNAME, RFNAME, RETURN, RFNAME,
  STRUCT, RFNAME, RFNAME, WHILE, RFNAME };

static int reservedRFs[]= { 0, -1, -1, 1, 2, -1,
  -1, -1, -1, -1, -1, -1, -1, 3, 4, 5,
  -1, 6, -1, 7, 8, 9, -1, 10, -1, 11,
  12, -1, 13 };

/* ------------------------------ */

static int EndOfLine(void)
{
  if (!nextChar) return 1;
  while ((curChar= *nextChar) && (curChar==' ' || curChar=='\t'))
    nextChar++;        /* skip over tabs and blanks */
  return !curChar;
}

static int CheckMulti(void)
{
  char **now= multis;
  char *cur= *now;
  int token;

  token= curChar;
  nextChar++;
  while (cur[0] && curChar!=cur[0]) cur= *(++now);

  if (*cur++) {
    /* first character of symbol matches at least one multi-symbol */
    int second= *nextChar;

    do {
      if (second == *cur) {
	/* second character of symbol matches at least one multi-symbol */
	nextChar++;
	now++;
	if (curChar==(*now)[0] && *nextChar && *nextChar==(*now)[2]) {
	  nextChar++;        /* found 3 character multi-symbol */
	} else {
	  now--;             /* found 2 character multi-symbol */
	}
	token= multiTypes[now-multis];
	break;
      }
      cur= *(++now);
    } while (curChar == (cur++)[0]);

    if (token!=DOTDOT && token!=PLUSPLUS && token!=MINUSMINUS)
      needOperand= 1;

  } else if (curChar=='^') {
    /* Caret is the only operator which is NOT the first character of
       at least one multi-symbol.  */
    needOperand= 1;

  } else if (curChar==',') {
    if (quining==1) quineEnd= nextChar-1;
    prevWasComma= 1;
  } else if (needBody && curChar=='{') {
    needBody= 0;  /* no need to supress semicolon on EOL any more */
  }

  return token;
}

static int CheckName(char *name)
{
  char *now= name;
  int c= *now++;

  if (c>='a' && c<='z') { /* from "avg" to "zcen" NOT a to z! */
    char *match, **lst, **fst= reserved;
    int i= 1;

    /* find position of 1st character of name in reserved list */
    while (**fst<c) fst++;
    lst= fst;
    while (**lst==c) lst++;
    lst--;

    /* if first character matches, procede with following characters */
    while (fst <= lst) {
      if (fst==lst) {
	match= &(*fst)[i];   /* only one possible match */
	while (now<nextChar && match[0]==now[0]) { match++; now++; }
	if (*match || now!=nextChar) break;

	yylval.i= reservedRFs[fst-reserved];
	if (reservedTypes[fst-reserved]==ELSE) supressSemi= 1;
	return reservedTypes[fst-reserved];     /* found a reserved word */

      }
      c= *now++;
      while (fst<=lst && (*fst)[i]<c) fst++;
      while (fst<=lst && (*lst)[i]>c) lst--;
      i++;
    }
  }

  yylval.lit= YpName(name, nextChar-name);
  return NAME;             /* NOT a reserved word, just an ordinary name */
}

/* ------------------------------ */

static char *AppendString(char *s, long ns, char *t, long nt)
{
  char *new= StrAlloc(ns+nt);
  if (ns) strcpy(new, s);
  StrFree(s);
  if (nt) strncpy(new+ns, t, nt);
  new[ns+nt]= '\0';
  return new;
}

static char *quote= 0;  /* for StringValue and CheckSyscall */

static int StringValue(void)
{
  char c, *begin, *next;
  long n, len= 0;

  begin= quote;
  quote= 0;
  StrFree(begin);

  begin= ++nextChar;  /* skip open quote */
  while ((c=*nextChar++)) {
    if (c=='\\') {
      if (*nextChar) {
	nextChar[-1]= YpEscapeSeq(nextChar, &next);
	quote= AppendString(quote, len, begin, nextChar-begin);
	len+= nextChar-begin;
	begin= nextChar= next;

      } else {
	/* escaped newline */
	n= nextChar-begin-1;
	quote= AppendString(quote, len, begin, n);
	len+= n;
	if (quining==1) GrabQuine(nextChar-1);
	curLine= nextChar= begin= prevToken= YpNextLine(NL_QUOTE);
	if (quining==1) quineBegin= quineEnd= nextChar;
	if (!curLine) {
	  if (len) StrFree(quote);
	  YpError("open \" at end-of-file");
	  return 0;                    /* EOF reached (inside string) */
	}
      }

    } else if (c=='\"') {
      /* only legitimate way out is close quote */
      quote= AppendString(quote, len, begin, nextChar-begin-1);
      yylval.q= YpQuoteConst(quote);
      return STRING;
    }
  }

  if (len) {
    begin= quote;
    quote= 0;
    StrFree(begin);
  }
  YpError("missing close \"");
  yylval.q= 0;
  return STRING_ERR;  /* unmatched open quote */
}

static int CharValue(void)
{
  int c= *(++nextChar);
  if (*nextChar) nextChar++;
  if (c=='\\') c= YpEscapeSeq(nextChar, &nextChar);
  if (*nextChar && (*nextChar++ != '\'')) {
    /* skip past matching close quote, if one on current line */
    while (*nextChar && (*nextChar++ != '\''));
    YpError("missing close \'");
    yylval.l= 0;
    return CHAR_ERR;
  }
  yylval.l= c;
  return CHAR;
}

static int NumberValue(void)
{
  char *begin= nextChar;
  int isReal= (curChar=='.');
  long trialValue= 0;
  char testc;

  if (!isReal) {
    /* strtoul does not exist on Suns, but the Sun strtol works
       properly there, hence this switch */
#ifndef NO_STRTOUL
    trialValue= strtoul(begin, &nextChar, 0);
#else
    trialValue= strtol(begin, &nextChar, 0);
#endif
    testc= nextChar[0];

    /* Mac MetroWerks C library strtoul does not advance nextChar
       past all digits if there are too many to represent a number;
       instead, it leaves nextChar==begin, which is a disaster.  Try
       to avert the disaster here. */
    if (begin[0]=='0') {
      if (begin[1]=='x') {
	while ((testc>='0' && testc<='9') ||
	       (testc>='A' && testc<='F') ||
	       (testc>='a' && testc<='f')) testc= *(++nextChar);
      } else {
	while (testc>='0' && testc<='7') testc= *(++nextChar);
      }
    } else {
      while (testc>='0' && testc<='9') testc= *(++nextChar);
    }

    if (testc=='.') {
      isReal= 1;
    } else if (testc=='e' || testc=='E') {
      int i= 1;
      if (nextChar[1]=='+' || nextChar[1]=='-') i= 2;
      if (nextChar[i]>='0' && nextChar[i]<='9') isReal= 1;
    }
  }

  if (isReal) {
    yylval.d= strtod(begin, &nextChar);

    if (nextChar==begin) {
      /* avert MetroWerks disaster described above */
      if (begin[0]=='.') nextChar++;
      testc= nextChar[0];
      while (testc>='0' && testc<='9') testc= *(++nextChar);
    }

    if (*nextChar=='f' || *nextChar=='F') {
      nextChar++;
      return FLOAT;
    } else if (*nextChar=='i' || *nextChar=='I') {
      /* alternative suggestion: 1j for imaginary, 1i for int */
      nextChar++;
      return IMAGINARY;
    } else {
      return DOUBLE;
    }

  } else {
    yylval.l= trialValue;
    if (*nextChar=='l' || *nextChar=='L') {
      nextChar++;
      return LONG;
    } else if (*nextChar=='i' || *nextChar=='I') {
      nextChar++;
      yylval.d= (double)trialValue;
      return IMAGINARY;
    } else if (*nextChar=='s' || *nextChar=='S') {
      nextChar++;
      return SHORT;
    } else if (*nextChar=='n' || *nextChar=='N') {
      /* alternative suggestion: 1j for imaginary, 1i for int */
      nextChar++;
      return INT;
    } else {
      return LONG;
    }
  }
}

/* ------------------------------ */

static int CheckSyscall(void)
{
  long n, len;
  int more;

  if (EndOfLine() || curChar!='$') return 0;
  nextChar++;
  for (n=0 ; nextChar[n] ; n++)
    if (nextChar[n]!=' ' && nextChar[n]!='\t') break;
  if (!nextChar[n]) return 0;

  nextChar+= n;               /* skip past leading whitespace */
  len= 0;

  if (quote) {
    char *prev= quote;
    quote= 0;
    StrFree(prev);
  }
  for (;;) {
    n= strlen(nextChar);
    more= (n && nextChar[n-1]=='\\');
    if (more) n--;

    quote= AppendString(quote, len, nextChar, n);
    len+= n;
    nextChar+= n;

    if (!more) break;
    curLine= nextChar= prevToken= YpNextLine(NL_QUOTE);
    if (!curLine) break;               /* EOF reached */
  }

  yylval.q= YpQuoteConst(quote);
  return 1;
}

/* ------------------------------ */

static void BeginQuine(void)
{
  /* nextChar is just past comma for argument to be quined */
  EndOfLine();   /* skip whitespace */
  if (quineText) StrFree(quineText);
  quineText= 0;
  quineBegin= quineEnd= nextChar;
  if (curChar=='\\' && !nextChar[1]) quineBegin++;
}

static void GrabQuine(char *stop)
{
  long n= stop>quineBegin? stop-quineBegin : 0;
  char *text= StrNCat(quineText, quineBegin, n);
  StrFree(quineText);
  quineText= text;
}

static int DoQuine(void)
{
  if (quining!=1) return 0;
  while (quineEnd>curLine && (quineEnd[-1]==' ' || quineEnd[-1]=='\t'))
    quineEnd--;
  GrabQuine(quineEnd);
  YpString(YpQuoteConst(quineText));
  StrFree(quineText);
  quineText= quineBegin= quineEnd= 0;
  nQuines--;
  if (nQuines==0) quining= 0;
  return 1;
}

/* ------------------------------ */

int yylex(void)
{
  int eol, continuation, bol;
  int supress= parenDepth || needOperand || supressSemi;
  if (noInputYet==2) return 0;

  for (;;) { /* loop past all whitespace, comments, and escaped newlines */

    for (;;) {  /* loop past blank lines */
      bol= (nextChar && nextChar==curLine);
      eol= EndOfLine();
      continuation= (curChar=='\\' && !nextChar[1]);
      bol= (bol && !continuation && !supress);
      if (!eol && !continuation) break;

      if (!continuation && !supress && !noInputYet &&
	  !prevWasComma && !needBody) {
	if (!prevWasSemi) {
	  /* if there are no open parentheses or operators, and the
	     previous token was NOT ';', EOL is same as ';' */
	  if (quining==1) quineEnd= nextChar;
	  prevWasSemi= 1;
	  return ';';
	}
	/* EOL (after ';') is same as EOF if no reason to continue */
	if (!nOpenCS && !braceDepth) return 0;
      }

      if (quining==1) GrabQuine(nextChar);
      curLine= nextChar= prevToken=
	YpNextLine(noInputYet? NL_NOINPUT : NL_CONTINUE);
      if (quining==1) quineBegin= quineEnd= nextChar;

      if (!curLine) {                        /* hit EOF */
	if (noInputYet) {
	  noInputYet= 2;
	  return NOINPUT;
	} else {
	  return 0;
	}
      }
    }

    if (curChar=='/' && nextChar[1]=='/') {
      while (*nextChar) nextChar++;
      continue;
    }

    if (curChar!='/' || nextChar[1]!='*') break;

    nextChar+= 2;                        /* skip past open comment */
    curChar= *nextChar++;
    while (curChar!='*' || *nextChar!='/') {  /* skip comment body */
      if (!curChar) {
	if (quining==1) GrabQuine(nextChar-1);
	curLine= nextChar= prevToken= YpNextLine(NL_COMMENT);
	if (quining==1) quineBegin= quineEnd= nextChar;
	if (!curLine) {
	  YpError("open comment at end-of-file");
	  return 0;           /* EOF reached (in comment) */
	}
      }
      curChar= *nextChar++;
    }
    nextChar++;
  }

  /* At last!  A real token has arrived! */

  needOperand= supressSemi= prevWasSemi= prevWasComma= 0;

  prevToken= nextChar;
  noInputYet= 0;

  if (curChar=='\"') {
    /* token is quoted string */
    return StringValue();         /* may include escaped newlines */

  } else if (curChar=='\'') {
    /* token is character constant */
    return CharValue();

  } else if ((curChar>='0' && curChar<='9') ||
	     (curChar=='.' && nextChar[1]>='0' && nextChar[1]<='9')) {
    /* token is some sort of number */
    return NumberValue();

  } else {
    /* token is not a number, check for a variable name */
    while ((curChar>='a'&&curChar<='z') || (curChar>='A'&&curChar<='Z') ||
	   (curChar>='0'&&curChar<='9') || curChar=='_')
      curChar= *(++nextChar);

    if (nextChar > prevToken) {
      /* token is a name */
      return CheckName(prevToken);

    } else if (curChar!=';') {
      /* token must be a symbol */
      if (bol && CheckSyscall()) return SYSCALL;
      return CheckMulti();
    } else {
      if (quining==1) quineEnd= nextChar;
      prevWasSemi= 1;
      nextChar++;
      return ';';
    }
  }
}

/* ------------------------------ */

void yyerror(char *msg)
{
  char errorMsg[81];
  long n;

  if (yychar==CHAR_ERR || yychar==STRING_ERR) return;

  n= strlen(msg);
  strncpy(errorMsg, msg, 80);
  if (n>=80) {
    errorMsg[80]= '\0';
  } else if (n>=70) {
    errorMsg[n]= '\0';
  } else {
    msg= errorMsg+n;
    strcpy(msg, " near ");
    msg+= 6;
    n= 80-n;
    if (n>16) n= 16;
    if (yychar==0) {
      strcpy(msg, "<EOF>");
    } else if (!prevToken || *prevToken==0) {
      strcpy(msg, "<EOL>");
    } else if (yychar==STRING && *prevToken!='\"') {
      strcpy(msg, "\"...\"");
    } else {
      strncpy(msg, prevToken, n);
      msg[n]= '\0';
    }
  }

  YpError(errorMsg);
}

/* ------------------------------ */

long ScanForFunc(const char *fname, int notExtern)
{
  int type, counter= 0;
  long position= -1;
  int token;
  YpParseInit((void *)0); /* necessary to receive literals and quotes */
  nOpenCS= 0;    /* initialize number of open control structures */
  parenDepth= 0; /* initialize number of open parentheses/square brackets */
  braceDepth= 0; /* initialize number of open curly braces */
  needOperand= prevWasSemi= prevWasComma= supressSemi= 0; /* more EOL flags */
  nQuines= quining= 0;
  /* this time, want to skip any include files and remember address of
     each line before it is read */
  ypSkipIncludes= 1;
  /* Get 1st line in NL_NOINPUT mode.  This is not really too important
     here, but if input were from keyboard, this would force YpNextLine
     to wait for input instead of possibly returning eof to handle
     input from another source.  */
  curLine= nextChar= YpNextLine(NL_NOINPUT);
  prevToken= 0;
  if (!nextChar) return 0;

  for (;;) {
    noInputYet= 1;     /* keep calling YpNextLine(NL_NOINPUT) --
			  assures that YpNextLine will keep track of
			  beginning line number */
    token= yylex();
    if (!token || token==NOINPUT) break;
    if (token=='{') braceDepth++;
    else if (token=='}') braceDepth--;
    else if (!braceDepth) {
      if (token==FUNC) type= 1;
      else if (token==STRUCT) type= 2;
      else if (!notExtern && (token==EXTERN||token==LOCAL)) type= 3;
      else type= 0;
      if (type) {
	position= YpStandby();
	noInputYet= 0;     /* if necessary, YpNextLine(NL_CONTINUE) */
	supressSemi= 1;    /* dont get confused by newline */
	token= yylex();
	supressSemi= 0;
	if (token==NAME && strcmp(fname, YpLitName(yylval.lit))==0) break;
	if (type==3) {
	  while ((token= yylex())==',') {
	    token= yylex();
	    if (token==NAME && strcmp(fname, YpLitName(yylval.lit))==0)
	      goto found;
	  }
	}
	position= -1;
	if (!token || token==NOINPUT) break;
      }
    } else if (token==NAME || token==STRING || token==SYSCALL) {
      /* try to keep the literal and constant tables reasonably small */
      counter++;
      if (counter>=32) { YpParseInit((void *)0); counter= 0; }
    }
  }
 found:

  return position;
}

/* End C code */
/*--------------------------------------------------------------------------*/
