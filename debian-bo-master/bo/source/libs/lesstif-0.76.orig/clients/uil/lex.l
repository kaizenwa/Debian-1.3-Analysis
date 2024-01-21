%{
/**
 *
 * $Id: lex.l,v 1.7 1996/11/12 03:53:58 miers Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *
 *  Original author:  Geoffrey W. Ritchey
 *                    codesmit@southwind.net
 *
*/ 
#include <string.h>
#include "uil.h"
#define YYSTYPE char *
#include "misc.h"
#include "y.tab.h"
#include "glue.h"
#include "Include.h"

#define MAX_INCLUDE_DEPTH 10
YY_BUFFER_STATE include_stack[MAX_INCLUDE_DEPTH];

FileData Files[MAX_INCLUDE_DEPTH];
extern YYSTYPE yylval;

char *FileName = Files[0].Name;
int LineNumber = 1;

int include_stack_ptr = 0;
%}

/*
 * Define start condition
 * This thing is to be used in parsing C style comments.
 * The code to parse these comments is borrowed from the flexdoc(1) manual page.
 */
%x comment

%%

<<EOF>> {
	LineNumber = Files[include_stack_ptr].lineno;
	if (--include_stack_ptr < 0) {
		yyterminate();
	}
	else
		yy_switch_to_buffer(include_stack[include_stack_ptr]);
	FileName = Files[include_stack_ptr].Name;
}
		
include[ \t]+file[ \t]+('|\(?\")[^'"]*('|\"\)?)\; {
			char *s;

			if (include_stack_ptr >= MAX_INCLUDE_DEPTH){
				fprintf(stderr,"Includes nested too deeply\n");
				exit(1);
			}
			s = strpbrk(yytext,"'\"")+1;
			s[strcspn(s,"\"'")] = 0;
			include_stack[include_stack_ptr++] = YY_CURRENT_BUFFER;
			strcpy(Files[include_stack_ptr].Name,s);
			Files[include_stack_ptr].lineno = LineNumber;
			FileName = Files[include_stack_ptr].Name;
			LineNumber = 1;
			yyin = IncludeOpenFile(s);
			if (!yyin)
				perror(s);
			yy_switch_to_buffer(yy_create_buffer(yyin,YY_BUF_SIZE));
		}
\"[^"]*\"	{
			yylval = Store((char *)yytext);
			return STRING;
		}
'[^']*'		{
			yytext[strlen(yytext)-1] = yytext[0] = '"';
			yylval = Store(yytext);
			return STRING;
		}
-?[0-9]+\.[0-9]*	{
			yylval = Store(yytext);
			return FLOAT;
		}
-?[0-9]+	{
			yylval = Store(yytext);
			return INTEGER;
		}
background[ \t]+color	{
			return BACKGROUND_COLOR;
		}
foreground[ \t]+color	{
			return FOREGROUND_COLOR;
		}
font_unit	{
			return FONT_UNIT;
		}
font_table	{
			return FONT_TABLE;
		}
font		{
			return FONT;
		}
xbitmapfile	{
			return XBITMAPFILE;
		}
xpixmapfile	{
			return XPIXMAPFILE;
		}
color_table	{
			return COLOR_TABLE;
		}
color		{
			return COLOR;
		}
user_defined	{
			return USER_DEFINED;
		}
boolean		{
			return BOOLEAN_TYPE;
		}
reason		{
			return REASON;
		}
private		{
			return PRIVATE;
		}
gadget		{
			return GADGET;
		}
character_set	{
			return CHAR_SET;
		}
icon 		{
			return ICON;
		}
keysym 		{
			return KEYSYM;
		}
unmanaged	{
			return UNMANAGED;
		}
widget		{
			return WIDGET;
		}
list		{
			return LIST;
		}
names		{
			return NAMES;
		}
version		{
			return VERSION;
		}
module		{
			return MODULE;
		}
value		{
			return VALUE;
		}
string_table	{
			return STRING_TABLE;
		}
imported	{
			return IMPORTED;
		}
exported	{
			return EXPORTED;
		}
controls	{
			return CONTROLS;
		}
arguments	{
			return ARGUMENTS;
		}
argument	{
			return ARGUMENT;
		}
objects		{
			return OBJECTS;
		}
object		{
			return OBJECT;
		}
callbacks	{
			return CALLBACK;
		}
end 		{
			return END;
		}
any		{
			return ANY_TYPE;
		}
string		{
			return STRING_TYPE;
		}
float		{
			return FLOAT_TYPE;
		}
integer		{
			return INTEGER_TYPE;
		}
false		{
			yylval = 0;
			return BOOL;
		}
true		{
			yylval = (char *)1;
			return BOOL;
		}
separate	{
			return SEPARATE;
		}
compound_string	{
			return COMPOUND_STRING;
		}
procedures	{
			return PROCEDURES;
		}
procedure	{
			return PROCEDURE;
		}
[a-zA-Z][a-zA-Z_0-9]*	{
			yylval = (char *)Store(yytext);
			return ID;
		}
"/*"			BEGIN(comment);
<comment>[^*\n]*	/* Eat */
<comment>"*"+[^*/\n]*	/* Eat */
<comment>\n		LineNumber++;
<comment>"*"+"/"	BEGIN(INITIAL);
"!"[^\n]*	{
		}
[ \t]+
\n		LineNumber++;

.		{
			return yytext[0];
		}

%%

