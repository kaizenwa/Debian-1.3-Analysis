%{
/* cfengine for GNU
 
        Copyright (C) 1995
        Free Software Foundation, Inc.
 
   This file is part of GNU cfengine - written and maintained 
   by Mark Burgess, Dept of Computing and Engineering, Oslo College,
   Dept. of Theoretical physics, University of Oslo
 
   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2, or (at your option) any
   later version.
 
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
 
  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA

*/
 

/*******************************************************************/
/*                                                                 */
/*  PARSER for cfengine                                            */
/*                                                                 */
/*******************************************************************/

#include <stdio.h>
#include "cf.defs.h"
#include "cf.extern.h"

extern char *yytext;

%}

%token ID LIST ITEM VARPATH PATH LBRACK RBRACK CONTROL GROUPS
%token IMAGE RESOLVE PROCESSES FILES TIDY HOMESERVERS BINSERVERS
%token LINKS IMPORT SCRIPTS ARROW EQUALS WILDCARD REQUIRED EDITFILES
%token NETMASK BROADCAST TIMEZONE QSTRING DISABLE MAKEPATH
%token FILEIGNORE MOUNTPATH HOMEPAT VARITEM MAILSERVER MOUNTABLES
%token DEFAULTROUTE MISCMOUNTS UNMOUNT LBRACE RBRACE PARSECLASS LARROW

%%

specification:       { yyerror("Warning: action contains an invalid statement"); }
                     | statements;

statements:            statement
                     | statements statement;

statement:             CONTROL controllist
                     | GROUPS declarations
                     | IMAGE classlist
                     | RESOLVE classlist
                     | PROCESSES classlist
                     | FILES classlist
                     | TIDY classlist
                     | HOMESERVERS classlist
                     | BINSERVERS classlist
                     | MAILSERVER classlist
                     | REQUIRED classlist
                     | MOUNTABLES classlist
                     | LINKS classlist
                     | IMPORT classlist
                     | SCRIPTS classlist
                     | DISABLE classlist
                     | MAKEPATH classlist
                     | FILEIGNORE classlist
                     | BROADCAST classlist
                     | DEFAULTROUTE classlist
                     | MISCMOUNTS classlist
                     | UNMOUNT classlist
                     | EDITFILES edits;

controllist:           declarations
                     | PARSECLASS declarations
                     | controllist PARSECLASS declarations;

declarations:          declaration
                     | declarations declaration;

classlist:             list
                     | PARSECLASS list
                     | classlist PARSECLASS list;

declaration:           ID EQUALS bracketlist;

bracketlist:           LBRACK list RBRACK;

list:                  entry
                     | list entry;

entry:                 ITEM              
                     | PATH ARROW PATH
                     | PATH ARROW VARPATH 
                     | PATH LARROW PATH
                     | PATH LARROW ITEM
                     | PATH LARROW VARPATH
                     | PATH ARROW WILDCARD
                     | VARPATH ARROW WILDCARD
                     | PATH LARROW WILDCARD
                     | VARPATH LARROW WILDCARD
                     | VARPATH ARROW PATH
                     | VARPATH ARROW VARPATH 
                     | VARPATH LARROW PATH
                     | VARPATH LARROW ITEM
                     | VARPATH LARROW VARPATH
                     | PATH           
                     | VARPATH
                     | VARITEM
                     | WILDCARD       
                     | QSTRING;


edits:                 editbrackets
                     | PARSECLASS editbrackets
                     | edits PARSECLASS editbrackets;

editbrackets:          editbracket
                     | editbrackets editbracket;

editbracket:           LBRACE PATH edlist RBRACE
                     | LBRACE VARPATH edlist RBRACE;

edlist:                ed
                     | edlist ed;

ed:                    ITEM QSTRING
                     | ITEM;

%%

/*****************************************************************/

yyerror(s)

char *s;

{
fprintf (stderr, "cfengine:%s:%d: %s \n",VCURRENTFILE,LINENUMBER,s);

ERRORCOUNT++;

if (ERRORCOUNT > 10)
   {
   FatalError("Too many errors");
   }
}

/*****************************************************************/

/* EOF */







