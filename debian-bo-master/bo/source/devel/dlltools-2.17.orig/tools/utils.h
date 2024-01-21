/* 
   Copyright (C) 1993, Eric Youngdale.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <sys/types.h>
#include <errno.h>

#if !defined(__linux__) && !defined(__svr4__) && !defined(__ultrix__) && !defined(sgi) && !defined(__sun__)
typedef unsigned int u_int;
#define remove	unlink
#endif

#ifdef __sun__
#define strtoul strtol
#endif

extern const char* version_string;

/* The DLL symbol enables the assembly code rewriting that is required
   to add indirections to all references to global data.  Generally this
   should only be undefined you you suspect that jumpas is doing the
   wrong thing, and it can be used to help establish where the problem lies. */

#define DLL

/* The PREFIX is used with binaries */
#ifndef PREFIX
#ifdef linux
#define PREFIX ""
#else
#define PREFIX "l-"
#endif
#endif

/* We need this as an argument to nm */
#ifdef __svr4__
#define NO_CPLUS "+no-cplus"
#else
#define NO_CPLUS "--no-cplus"
#endif

#define JUMP_FUNCTIONS "jump.funcs"
#define JUMP_VARS      "jump.vars"
#define JUMP_IMPORT    "jump.import"
#define JUMP_IGNORE    "jump.ignore"
#define JUMP_PARAMS    "jump.params"
#define JUMP_LOG       "jump.log"
#define JUMPAS_SUBLOG  "jumpas.log"
#define JUMP_UNDEFS    "jump.undefs"

#ifndef LONG_FILENAMES
/* These are the safe macros that we use with the minix filesystem due
   to the 14 character limit on the length of a filename */
#define GVAR_FILENAME(BUFFER, FILEDIR, GPNT) \
	sprintf(BUFFER, "%s_GVAR_%5.5d.s", FILEDIR, (GPNT)->index)

#define GCMN_FILENAME(BUFFER, FILEDIR, GPNT) \
	sprintf(BUFFER, "%s_GCMN_%5.5d.s", FILEDIR, (GPNT)->index)

#define GENERATE_SECONDARY_INDEX(BUFFER, GPNT) \
  sprintf(BUFFER, "_%5.5d_", (GPNT)->index);

#else

/* These are the macros that we use with the other filesystems that
   tend to allow us to use > 14 character filenames.  I would guess that
   256 would be perfectly adequate for our purposes at all times. These
   are preferable, because this allows us much more freedom when we merge
   sub-libraries when we create one large library.  */

#define GVAR_FILENAME(BUFFER, FILEDIR, GPNT) \
	sprintf(BUFFER, "%s_GVAR_%s_%s.s", FILEDIR,  \
		(GPNT)->library, (GPNT)->name)

#define GCMN_FILENAME(BUFFER, FILEDIR, GPNT) \
	sprintf(BUFFER, "%s_GCMN_%s_%s.s", FILEDIR,  \
		(GPNT)->library, (GPNT)->name)

#define GENERATE_SECONDARY_INDEX(BUFFER, GPNT) \
  sprintf(BUFFER, "_%s_%s_", (GPNT)->library, (GPNT)->name);

#endif

#define CHECKSIZE 1
#define JUSTNM 2

typedef struct global
{
  struct global *next;
  int index;
  int type;
  u_int offset;
  u_int size;
  char *name;
  char *library;
  char *objfile;
  char * override_undefs;
} GLOBAL;

extern char *program;

#ifdef USE_VARARGS
void usage();
void error();
void xsystem();
#else
void usage(char *format, ...);
void error(char *format, ...);
void xsystem(char *format, ...);
#endif

void xfree (void *);
void *xmalloc(size_t size);
char *xstrdup(char *string);
GLOBAL *readgbls(char *filtpath, char *filename, int checksize, char * allowed);
void vfyparams(char *, char *, unsigned int,
	       unsigned int, unsigned int, unsigned int, int, int, int);



typedef struct line
{
  struct line *next;
  char *text;
} LINE;

typedef struct symbol
{
  struct symbol * next;
  char * name;
  char static_var;
  char * newname;
  LINE * where;
} SYMBOL;



extern LINE *fhead;
extern GLOBAL *gdhead;		/* Global symbols to be diverted */
extern GLOBAL *gfhead;		/* List of global functions */
extern GLOBAL *gihead;		/* List of symbols to be ignored */

#ifndef IMPLIED_IMPORT
extern GLOBAL *gphead;		/* List of symbols to be imported */
#endif

extern SYMBOL *shead;
extern SYMBOL *stail;

#define PLACEHOLDER "__DUMMY__"

#define MAX_TOKENS 30

struct operand{
  char ntokens;
  char nstrings;
  char regused;
  char needs_fix;
  char complex;
  char token[MAX_TOKENS];
  char *strings[MAX_TOKENS];
};

extern struct instruction{
  char opcode[32];
  char needs_fix;
  char uses_stack;
  int operands;
  struct operand ops[3];
} ins, ins1;
