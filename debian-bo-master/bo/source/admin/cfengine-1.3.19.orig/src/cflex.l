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
/*  LEXER for cfengine                                             */
/*                                                                 */
/*                                                                 */
/*  Mark 2/3/93. This is wide screen entertainment. Resize         */
/*  your window before viewing!                                    */
/*                                                                 */
/*  The routines called by the lexer and the yacc parser are       */
/*  defined in the file parseinfo.c                                */
/*                                                                 */
/*                                                                 */
/*******************************************************************/


 /* Here we define a two state lexer. The states exclude
    certain regular expression matches so as to more easily
    distinguish between class constructions like 

       compound.class::

    identifiers like

       group = ( .... )

    and items inside the parentheses like

       .... = ( alpha beta gamma )

    The state
    <IDENT> recognizes identifiers and <INITIAL> recognizes what
    is an item in a LisT.

 */


#include "y.tab.h"       /* Note that BISON produces a file called */
#include "cf.defs.h"     /* mconfig.tab.h instead of y.tab.h !!    */
#include "cf.extern.h"


  /* Note the %o %a declarations to allow more internal workspace */

%}

%s IDENT
%o 19000
%a 12000
%p 8000
%n 800

space     [ \t]+
comment   #.*
qstring   \"[^"\n]*\"
sqstring  \'[^'\n]*\'
bqstring  `[^`\n]*`
item      [+]?[-.,@a-zA-Z0-9_]+
id        [.|!a-zA-Z0-9_]+
path      [-:./a-zA-Z0-9_]+
varpath   [/$<][-.a-zA-Z0-9<>@+:$*?(){}/_]*
varitem   [-.a-zA-Z0-9<>$*?()={}/_]*
wildcard  [^\135 ()>\t\n|&@\175]+
newline   [\n]
lbrack    \(
rbrack    \)
lbrace    \{
rbrace    \}
equals    =
forcearrow ->!
lforcearrow \+>!
arrow     ->
larrow    \+>

%%

control:               {
                       BEGIN IDENT;
                       SetAction(control);
                       return CONTROL;
                       }

groups:                {
                       BEGIN IDENT;
                       SetAction(groups);
                       return GROUPS;
                       }

copy:                  {
                       BEGIN INITIAL;
                       SetAction(image);
                       return IMAGE;
                       }

resolve:               {
                       BEGIN INITIAL;
                       SetAction(resolve);
                       return RESOLVE;
                       }


files:                 {
                       BEGIN INITIAL;
                       SetAction(files);
                       return FILES;
                       }

tidy:                  {
                       BEGIN INITIAL;
                       SetAction(tidy);
                       return TIDY;
                       }

homeservers:           {
                       BEGIN INITIAL;
                       SetAction(homeservers);
                       return HOMESERVERS;
                       }

binservers:            {
                       BEGIN INITIAL;
                       SetAction(binservers);
                       return BINSERVERS;
                       }

mailserver:            {
                       BEGIN INITIAL;
                       SetAction(mailserver);
                       return MAILSERVER;
                       }

required:              {
                       BEGIN INITIAL;
                       SetAction(required);
                       return REQUIRED;
                       }

mountables:            {
                       BEGIN INITIAL;
                       SetAction(mountables);
                       return MOUNTABLES;
                       }

links:                 {
                       BEGIN INITIAL;
                       SetAction(links);
                       return LINKS;
                       }

import:                {
                       BEGIN INITIAL;
                       SetAction(import);
                       return IMPORT;
                       }

shellcommands:          {
                        BEGIN INITIAL;
                        SetAction(shellcommands);
                        return SCRIPTS;
                        }

disable:               {
                       BEGIN INITIAL;
                       SetAction(disable);
                       return DISABLE;
                       }

directories:           {
                       BEGIN INITIAL;
                       SetAction(makepath);
                       return MAKEPATH;
                       }


ignore:                {
                       BEGIN INITIAL;
                       SetAction(ignore);
                       return FILEIGNORE;
                       }

broadcast:             {
                       BEGIN INITIAL;
                       SetAction(broadcast);
                       return BROADCAST;
                       }

defaultroute:          {
                       BEGIN INITIAL;
                       SetAction(defaultroute);
                       return DEFAULTROUTE;
                       }

miscmounts:            {
                       BEGIN INITIAL;
                       SetAction(misc_mounts);
                       return MISCMOUNTS;
                       }

unmount:               {
                       BEGIN INITIAL;
                       SetAction(unmounta);
                       return(UNMOUNT);
                       }


editfiles:             {
                       BEGIN INITIAL;
                       SetAction(editfiles);
                       return EDITFILES;
                       }

processes:             {
                       BEGIN INITIAL;
                       SetAction(processes);
                       return PROCESSES;
                       }


{newline}              {
                       LINENUMBER++;
                       }

{space}                {
                       }

{comment}              {
                       }


{lforcearrow}          {
                       strcpy(LINKFROM,CURRENTPATH);
                       ACTION_IS_LINKCHILDREN = true;
                       FORCELINK=true;
                       return LARROW;
                       }


{forcearrow}           {
                       strcpy(LINKFROM,CURRENTPATH);
                       ACTION_IS_LINK = true;
                       FORCELINK=true;
                       return ARROW;
                       }

{arrow}                {
                       strcpy(LINKFROM,CURRENTPATH);
                       ACTION_IS_LINK = true;
                       FORCELINK=false;
                       return ARROW;
                       }


{larrow}               {
                       strcpy(LINKFROM,CURRENTPATH);
                       ACTION_IS_LINKCHILDREN = true;
                       FORCELINK=false;
                       return LARROW;
                       }


{equals}               {
                       return EQUALS;
                       }


{id}::                 {
                       if (DEBUG || D1)
                          {
                          printf("Class:: %s\n",yytext);   
                          }

                       yytext[strlen((char *)yytext)-2] = '\0';
                       HandleClass(yytext);
                       return PARSECLASS;
                       }

<IDENT>{id}            {
                       HandleId(yytext);
                       return(ID);
                       }


{lbrack}               {
                       if (DEBUG || D1) puts("LEFTBRACK\n");
                       BEGIN INITIAL;
                       return LBRACK;
                       }

{lbrace}               {
                       return LBRACE;
                       }

{rbrack}               {
                       if (DEBUG || D1) puts("RIGHTBRACK\n");
                       BEGIN IDENT;
                       return RBRACK;
                       }

{rbrace}               {
                       if (EDITGROUPLEVEL != 0)
                          {
                          yyerror("Editfiles Begin/End mismatch before or");
                          }

                       if (SEARCHREPLACELEVEL != 0)
                          {
                          yyerror("ReplaceAll/With mismatch before or");
                          }

                       if (MOUNT_FROM || MOUNT_ONTO)
                          {
                          yyerror("Missing rw/ro mode in mount syntax");
                          MOUNT_FROM = false;
                          MOUNT_ONTO = false;
                          }

                       
                       return RBRACE;
                       }


<INITIAL>{item}        {
                       if (ACTION == shellcommands)
                          {
                          yyerror("Shell commands must be quoted strings");
                          }

                       if (ACTION == editfiles)
                          {
                          if (strcmp((char *)yytext,"EndGroup") == 0 || strcmp((char *)yytext,"GotoLastLine") == 0 || strcmp((char *)yytext,"EmptyEntireFilePlease") == 0)
                             {
                             HandleEdit(CURRENTPATH,yytext,NULL);
                             return ITEM;
                             }
                          }

                       if (ACTION != control && strcmp((char *)yytext,"home") == 0)
                          {
                          if (DEBUG || D1)
                             {
                             printf("SPECIAL VARIABLE home\n");
                             }
                          HandlePath(yytext);
                          return PATH;
                          }
                       else if (ACTION == tidy)
                          {
                          if (yytext[0] == '\\')       /* ignore \ in \# */
                             {
                             HandleWildcard(yytext+1);
                             }
                          else
                             {
                             HandleWildcard(yytext);
                             }
                          return WILDCARD;
                          }
                       else
                          {
                          HandleItem(yytext);
                          return ITEM;
                          }
                       }


<INITIAL>{path}        {
                       if (ACTION == shellcommands)
                          {
                          yyerror("Shell commands must be quoted strings");
                          }

                       if (strncmp("home",(char *)yytext,4)==0)
                          {
                          HandlePath(yytext);
                          return PATH;
                          }

                       if (ACTION == control)
                          {
                          HandleItem(yytext);
                          return ITEM;
                          }

                       if (ACTION != mountables  && ACTION != mailserver &&
                           ACTION != misc_mounts &&  ACTION != unmounta && yytext[0] != '/')
                          {
                          if (CLASS == homepat)
                             {
                             HandleWildcard(yytext);
                             return WILDCARD;
                             }

                          if (ACTION_IS_LINK || ACTION_IS_LINKCHILDREN)
                             {
                             HandlePath(yytext);
                             return PATH;
                             }

                          sprintf(ERROR,"%s is not an absolute pathname",yytext);
                          yyerror(ERROR);
                          }

                       HandlePath(yytext);
                       return PATH;
                       }

{qstring}              {
                       if (ACTION == editfiles)
                          {
                          *(yytext+strlen((char *)yytext)-1) = '\0';
                          HandleEdit(CURRENTPATH,CURRENTITEM,yytext+1);
                          return QSTRING;
                          }
                       else if (ACTION == groups)
                          {
                          HandleItem(yytext);
                          return ITEM;
                          }
                       else if (ACTION == control)
                          {
                          *(yytext+strlen((char *)yytext)-1) = '\0';
                          HandleItem(yytext+1);
                          Debug1("Quoted macro string %s\n",yytext+1);
                          return QSTRING;
                          }
                       else
                          {
                          *(yytext+strlen((char *)yytext)-1) = '\0';
 
                          if (yytext[1] == '/')
                             {
                             HandlePath(yytext+1);
                             }
                          else
                             {
                             HandleItem(yytext+1);
                             }
                          return QSTRING;
                          }
                       }

{sqstring}             {
                       if (ACTION == editfiles)
                          {
                          *(yytext+strlen((char *)yytext)-1) = '\0';
                          HandleEdit(CURRENTPATH,CURRENTITEM,yytext+1);
                          return QSTRING;
                          }
                       else if (ACTION == groups)
                          {
                          HandleItem(yytext);
                          return ITEM;
                          }
                       else if (ACTION == control)
                          {
                          *(yytext+strlen((char *)yytext)-1) = '\0';
                          HandleItem(yytext+1);
                          Debug1("Quoted macro string %s\n",yytext+1);
                          return QSTRING;
                          }
                       else
                          {
                          *(yytext+strlen((char *)yytext)-1) = '\0';
 
                          if (yytext[1] == '/')
                             {
                             HandlePath(yytext+1);
                             }
                          else
                             {
                             HandleItem(yytext+1);
                             }
                          return QSTRING;
                          }
                       }

{bqstring}             {
                       if (ACTION == editfiles)
                          {
                          *(yytext+strlen((char *)yytext)-1) = '\0';
                          HandleEdit(CURRENTPATH,CURRENTITEM,yytext+1);
                          return QSTRING;
                          }
                       else if (ACTION == groups)
                          {
                          HandleItem(yytext);
                          return ITEM;
                          }
                       else if (ACTION == control)
                          {
                          *(yytext+strlen((char *)yytext)-1) = '\0';
                          HandleItem(yytext+1);
                          Debug1("Quoted macro string %s\n",yytext+1);
                          return QSTRING;
                          }
                       else
                          {
                          *(yytext+strlen((char *)yytext)-1) = '\0';
 
                          if (yytext[1] == '/')
                             {
                             HandlePath(yytext+1);
                             }
                          else
                             {
                             HandleItem(yytext+1);
                             }
                          return QSTRING;
                          }
                       }


{varpath}              {
                       IsVarString(yytext);        /* Syntax check */
                       HandleVarpath(yytext);
                       return VARPATH;
                       }

{varitem}              {
                       if (ACTION == shellcommands)
                          {
                          yyerror("Shell commands must be quoted strings");
                          }

                       if (ACTION_IS_LINK || ACTION_IS_LINKCHILDREN)
                          {
                          if (strlen(LINKTO) == 0)
                             {
                             HandlePath(yytext);
                             return PATH;
                             }
                          }

                       HandleWildcard(yytext);

                       if (IsVarString(yytext))
                          {
                          return VARITEM;
                          }

                       return WILDCARD;
                       }


{wildcard}             {
                       if (ACTION == shellcommands)
                          {
                          yyerror("Shell commands must be quoted strings");
                          }

                       if (ACTION_IS_LINK || ACTION_IS_LINKCHILDREN)
                          {
                          if (strlen(LINKTO) == 0)
                             {
                             HandlePath(yytext);
                             return PATH;
                             }
                          }

                       if (IsVarString(yytext))
                          {
                          HandleWildcard(yytext);
                          return VARITEM;
                          }

                       HandleWildcard(yytext);
                       return WILDCARD;
                       }


.                      {
                       return yytext[0];
                       }

%%

/* EOF */
