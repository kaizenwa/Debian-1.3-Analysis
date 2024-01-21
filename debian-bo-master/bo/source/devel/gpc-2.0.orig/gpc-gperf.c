/* C code produced by gperf version 2.5 (GNU C++ version) */
/* Command-line: gperf -g -o -j1 -t -p -N is_reserved_word ./gpc.gperf  */ 
/* ISO Pascal 7185 && Extended Pascal (Jan 29, 1989 Draft) reserved words.

   Copyright (C) 1989, 1993, Free Software Foundation, Inc.

This file is part of GNU GPC.

GNU GPC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU GPC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU GPC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/*
 * For GNU Pascal compiler (GPC) by Jukka Virtanen, jtv@hut.fi
 *
 * run this through the Doug Schmidt's gperf program
 * with command
 * gperf  -g -o -j1 -t -p -N is_reserved_word
 *
 * Thanks, Doug.
 */

/* ISO Pascal 7185 keywords are recognized as keywords.
 *
 * To treat PASCAL_EXTEND type keywords as keywords,
 * you must give -fextended-pascal when compiling.
 *
 * To treat PASCAL_OBJECT keywords as keywords, give
 * -fobject-pascal.
 *
 * To treat PASCAL_BORLAND keywords as keywords, give
 * -fborland-pascal.
 *
 * To treat PASCAL_SC keywords as keywords, give
 * -fpascal-sc
 *
 * Otherwise they are recognized, but may be redfined.
 * This violates Extended Pascal standard, but works anyhow,
 * if your program does not redefine them. And it also makes
 * ISO standard pascal program compile without modifications with GPC.
 *
 * Keywords are dynamically enabled and disabled such that a correct
 * ISO Pascal program will not notice Borland keywords at all.
 * If you change this, Borland extensions won't work. -- PG
 */

struct resword { char *name;
		 short token;
		 short iclass;
		 short informed;
                 short disabled;
	       };
;

#define TOTAL_KEYWORDS 72
#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 14
#define MIN_HASH_VALUE 3
#define MAX_HASH_VALUE 95
/* maximum key range = 93, duplicates = 0 */

#ifdef __GNUC__
inline
#endif
static unsigned int
hash (str, len)
     register char *str;
     register int unsigned len;
{
  static unsigned char asso_values[] =
    {
     96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
     96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
     96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
     96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
     96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
     96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
     96, 96, 96, 96, 96, 14, 10, 50, 65, 59,
     39,  8, 96,  0, 96, 96,  9, 34, 24, 28,
      6, 13, 14,  0,  3, 50, 45,  9,  7, 96,
     96, 96, 96, 96, 96, 96, 96, 96, 96, 96,
      4,  0, 21, 96,  4, 96, 96, 96, 17, 45,
     31,  0, 96, 96,  0,  2,  5, 96,  5, 46,
     96, 33, 96, 96, 96, 96, 96, 96,
    };
  return len + asso_values[str[len - 1]] + asso_values[str[0]];
}

#ifdef __GNUC__
inline
#endif
struct resword *
is_reserved_word (str, len)
     register char *str;
     register unsigned int len;
{
  static struct resword wordlist[] =
    {
      {"",}, {"",}, {"",}, 
      {"Shr", 		BP_SHR,		PASCAL_BORLAND, 0, 1},
      {"Is", 		OP_IS,		PASCAL_OBJECT,	0, 0},
      {"To", 		TO,		PASCAL_ISO,	0, 0},
      {"Inline", 		BP_INLINE,	PASCAL_BORLAND, 0, 1},
      {"Type", 		TYPE,		PASCAL_ISO,	0, 0},
      {"Set", 		SET,		PASCAL_ISO,	0, 0},
      {"Interface", 	INTERFACE,	PASCAL_EXTEND,  0, 1},
      {"Xor", 		BP_XOR,		PASCAL_BORLAND, 0, 1},
      {"Import",  	IMPORT, 	PASCAL_EXTEND,	0, 0},
      {"Goto", 		GOTO,		PASCAL_ISO,	0, 0},
      {"Inherited", 	OP_INHERITED,	PASCAL_OBJECT,	0, 1},
      {"While", 		WHILE,		PASCAL_ISO,	0, 0},
      {"Procedure",  	PROCEDURE, 	PASCAL_ISO,	0, 0},
      {"Packed", 		PACKED,		PASCAL_ISO,	0, 0},
      {"With", 		WITH,		PASCAL_ISO,	0, 0},
      {"Bindable",  	BINDABLE,	PASCAL_EXTEND,	0, 0},
      {"Protected",  	PROTECTED, 	PASCAL_EXTEND,	0, 1},
      {"Shl", 		BP_SHL,		PASCAL_BORLAND, 0, 1},
      {"And", 		AND,		PASCAL_ISO,	0, 0},
      {"Absolute", 	BP_ABSOLUTE,	PASCAL_BORLAND,	0, 1},
      {"If", 		IF,		PASCAL_ISO,	0, 0},
      {"Record", 		RECORD,		PASCAL_ISO,	0, 0},
      {"Repeat", 		REPEAT,		PASCAL_ISO,	0, 0},
      {"Qualified",  	QUALIFIED, 	PASCAL_EXTEND,	0, 0},
      {"Abstract", 	OP_ABSTRACT,	PASCAL_OBJECT,	0, 0},
      {"Restricted",    	RESTRICTED,	PASCAL_EXTEND,	0, 0},
      {"",}, 
      {"Or", 		OR,		PASCAL_ISO,	0, 0},
      {"Label", 		LABEL,		PASCAL_ISO,	0, 0},
      {"Not", 		NOT,		PASCAL_ISO,	0, 0},
      {"In", 		IN,		PASCAL_ISO,	0, 0},
      {"All", 		ALL,		PASCAL_EXTEND,	0, 1},
      {"Or_else",  	OR_ELSE, 	PASCAL_EXTEND,	0, 0},
      {"Operator", 	PXSC_OPERATOR,	PASCAL_SC,	0, 1},
      {"Otherwise",  	OTHERWISE, 	PASCAL_EXTEND,	0, 0},
      {"Then", 		THEN,		PASCAL_ISO,	0, 0},
      {"Object", 		BP_OBJECT,	PASCAL_BORLAND,	0, 1},
      {"Module",  	MODULE, 	PASCAL_EXTEND,	0, 0},
      {"Mod", 		MOD,		PASCAL_ISO,	0, 0},
      {"For", 		FOR,		PASCAL_ISO,	0, 0},
      {"File", 		FILE_,		PASCAL_ISO,	0, 0},
      {"Nil", 		NIL,		PASCAL_ISO,	0, 0},
      {"Implementation", 	IMPLEMENTATION,	PASCAL_EXTEND,	0, 1},
      {"Begin", 		BEGIN,		PASCAL_ISO,	0, 0},
      {"Property", 	OP_PROPERTY, 	PASCAL_OBJECT,	0, 0},
      {"Var", 		VAR,		PASCAL_ISO,	0, 0},
      {"Library", 	BP_LIBRARY,	PASCAL_BORLAND, 0, 1},
      {"Value",   	VALUE,  	PASCAL_EXTEND,	0, 0},
      {"Of", 		OF,		PASCAL_ISO,	0, 0},
      {"Array", 		ARRAY,		PASCAL_ISO,	0, 0},
      {"And_then",  	AND_THEN, 	PASCAL_EXTEND,	0, 0},
      {"Case", 		CASE,		PASCAL_ISO,	0, 0},
      {"Pow",     	POW,    	PASCAL_EXTEND,	0, 0},
      {"Uses", 		BP_USES,	PASCAL_BORLAND, 0, 0},
      {"Class", 		OP_CLASS,	PASCAL_OBJECT,	0, 0},
      {"Program",    	PROGRAM,   	PASCAL_ISO,	0, 0},
      {"Unit", 		BP_UNIT,	PASCAL_BORLAND, 0, 0},
      {"Const", 		CONST,		PASCAL_ISO,	0, 0},
      {"Constructor", 	OP_CONSTRUCTOR,	PASCAL_OBJECT,	0, 1},
      {"Asm", 	        BP_ASM,         PASCAL_BORLAND, 0, 1},
      {"Else", 		ELSE,		PASCAL_ISO,	0, 0},
      {"",}, 
      {"Only",    	ONLY,   	PASCAL_EXTEND,	0, 0},
      {"End", 		END,		PASCAL_ISO,	0, 0},
      {"Do", 		DO,		PASCAL_ISO,	0, 0},
      {"Exports",         BP_EXPORTS,     PASCAL_BORLAND, 0, 1},
      {"Virtual", 	BP_VIRTUAL,	PASCAL_BORLAND,	0, 1},
      {"Export",  	EXPORT, 	PASCAL_EXTEND,	0, 0},
      {"Downto",  	DOWNTO,		PASCAL_ISO,	0, 0},
      {"Until", 		UNTIL,		PASCAL_ISO,	0, 0},
      {"Div", 		DIV,		PASCAL_ISO,	0, 0},
      {"",}, 
      {"Destructor", 	OP_DESTRUCTOR,	PASCAL_OBJECT,	0, 1},
      {"",}, {"",}, 
      {"Function",  	FUNCTION, 	PASCAL_ISO,	0, 0},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"View", 		OP_VIEW,	PASCAL_OBJECT,	0, 0},
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register int key = hash (str, len);

      if (key <= MAX_HASH_VALUE && key >= 0)
        {
          register char *s = wordlist[key].name;

          if (*s == *str && !strcmp (str + 1, s + 1))
            return &wordlist[key];
        }
    }
  return 0;
}
