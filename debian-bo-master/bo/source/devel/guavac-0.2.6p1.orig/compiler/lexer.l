%{
// Copyright (c) 1995  David Engberg  All rights reserved
// $Id: lexer.l,v 1.8 1997/03/03 18:00:27 geppetto Exp $
 #include <string>
 #include <cstdio>
 #include <cctype>
 #include <stream.h>
 #include "unicode_string.h"
 #include "parser_decls.h"
 #include "parser.h"

 YY_BUFFER_STATE gInputBuffer;

 unicode_string LexStringToUnicode(const char* inputString);
 unsigned long long ParseLong(const char* inputString, unsigned short base);
 unsigned long ParseInt(const char* inputString, unsigned short base);
%}

%option yylineno
%option noyywrap
%option 8bit
%option noreject
%option never-interactive
%option outfile="lexer.C"

%x INCOMMENT

IDENTIFIER          [A-Za-z_$][A-Za-z_$0-9]*
DIGIT               [0-9]
HEXDIGIT            [A-Fa-f0-9]
OCTDIGIT            [0-7]
DECNUMBER           [1-9]{DIGIT}*
HEXNUMBER           0[Xx]{HEXDIGIT}+
OCTNUMBER           0{OCTDIGIT}*
DECLONG             {DECNUMBER}[Ll]
HEXLONG             {HEXNUMBER}[Ll]
OCTLONG             {OCTNUMBER}[Ll]
EXPONENT            [Ee][+-]?{DIGIT}+
FLOATBASE           ((({DIGIT}+\.{DIGIT}*)|({DIGIT}*\.{DIGIT}+)){EXPONENT}?)|({DIGIT}+{EXPONENT})
/*
 * The {DIGIT}+ part of both of these is contrary to the written spec, but
 * their compiler accepts 0f as a valid float literal, so I needed to add this
 * for compatibility.
 */
DOUBLE              ({FLOATBASE}[Dd]?)|({DIGIT}+[Dd])
FLOAT               ({FLOATBASE}|({DIGIT}+))[Ff]
/* haven't dealt with their Unicode scheme yet. */
UNICODE_CHARACTER   \\u{HEXDIGIT}{4}
ONECHAR             [^\\"']|(\\.)|(\\[0123]?{OCTDIGIT}?{OCTDIGIT})|{UNICODE_CHARACTER}
CHARLITCHAR         {ONECHAR}|\"
CHARACTER           "'"{CHARLITCHAR}"'"
STRINGCHAR          {ONECHAR}|'
STRING              \"{STRINGCHAR}*\"
CHAR_OP             [-;{},;()[\].&|!~=+*/%<>^?:]
WHITESPACE          [ \n\r\t]+

/* This is a little hack to allow C preprocessor line number directives */
LINE_DIRECTIVE      (#\ {DECNUMBER}\ {STRING}.*\n)

%%

{LINE_DIRECTIVE} { yylineno = ::atoi(yytext + 2); }

<INCOMMENT>"/*" ;
<INCOMMENT>"*/" { BEGIN(INITIAL); }
<INCOMMENT>.    ;
<INCOMMENT>\n   ;
"/*"            { BEGIN(INCOMMENT); }
"//".*          ;
{WHITESPACE}    ;
{DECLONG}       { yytext[yyleng - 1] = '\0';
                  yylval.doubleLong = ::ParseLong(yytext, 10);
                  return(LONG_LITERAL); }
{HEXLONG}       { yytext[yyleng - 1] = '\0';
                  yylval.doubleLong = ::ParseLong(yytext + 2, 16);
                  return(LONG_LITERAL); }
{OCTLONG}       { yytext[yyleng - 1] = '\0';
                  yylval.doubleLong = ::ParseLong(yytext + 1, 8);
                  return(LONG_LITERAL); }
{DECNUMBER}     { yylval.longNumber = ParseInt(yytext, 10);
                  return(INT_LITERAL); }
{HEXNUMBER}     { yylval.longNumber = ParseInt(yytext + 2, 16);
                  return(INT_LITERAL); }
{OCTNUMBER}     { yylval.longNumber = ParseInt(yytext + 1, 8);
                  return(INT_LITERAL); }
{CHARACTER}     { yytext[yyleng - 1] = '\0';
                  unicode_string tempString = ::LexStringToUnicode(yytext + 1);
		  if (tempString.length() > 0) {
		    yylval.longNumber = tempString[tempString.length() - 1];
		    return (CHARACTER_LITERAL);
		  } else {
		    return(ERROR);
		  }
                }
{FLOAT}         { yylval.floatNumber = ::atof(yytext); return(FLOAT_LITERAL); }
{DOUBLE}        { yylval.doubleFloat = ::atof(yytext);
                  return(DOUBLE_LITERAL); }
{STRING}        { yytext[yyleng - 1] = '\0';
                  yylval.text =
                     new unicode_string(::LexStringToUnicode(yytext + 1));
                  yytext[yyleng] = '"';   // restore it, just in case
                  return(STRING_LITERAL); }
"abstract"      { return(ABSTRACT); }
"boolean"       { return(BOOLEAN); }
"break"         { return(BREAK); }
"byte"          { return(BYTE); }
"case"          { return(CASE); }
"catch"         { return(CATCH); }
"char"          { return(CHAR); }
"class"         { return(CLASS); }
"continue"      { return(CONTINUE); }
"default"       { return(DEFAULT ); }
"do"            { return(DO); }
"double"        { return(DOUBLE); }
"else"          { return(ELSE); }
"extends"       { return(EXTENDS); }
"false"         { return(FALSE_TOKEN); }
"final"         { return(FINAL); }
"finally"       { return(FINALLY); }
"float"         { return(FLOAT); }
"for"           { return(FOR); }
"if"            { return(IF); }
"implements"    { return(IMPLEMENTS ); }
"import"        { return(IMPORT); }
"instanceof"    { return(INSTANCEOF); }
"int"           { return(INT); }
"interface"     { return(INTERFACE); }
"long"          { return(LONG); }
"native"        { return(NATIVE); }
"new"           { return(NEW); }
"null"          { return(NULL_TOKEN); }
"package"       { return(PACKAGE ); }
"private"       { return(PRIVATE); }
"protected"     { return(PROTECTED); }
"public"        { return(PUBLIC); }
"return"        { return(RETURN); }
"short"         { return(SHORT); }
"static"        { return(STATIC); }
"super"         { return(SUPER); }
"switch"        { return(SWITCH); }
"synchronized"  { return(SYNCHRONIZED ); }
"this"          { return(THIS); }
"volatile"      { return(VOLATILE); }
"throw"         { return(THROW); }
"throws"        { return(THROWS); }
"transient"     { return(TRANSIENT); }
"true"          { return(TRUE_TOKEN); }
"try"           { return(TRY); }
"void"          { return(VOID); }
"while"         { return(WHILE ); }
">>="           { return(SHIFT_RIGHT_EQUALS); }
">>>="          { return(FILL_SHIFT_RIGHT_EQUALS); }
"<<="           { return(SHIFT_LEFT_EQUALS); }
"+="            { return(ADD_EQUALS); }
"-="            { return(SUB_EQUALS); }
"*="            { return(MUL_EQUALS); }
"/="            { return(DIV_EQUALS); }
"%="            { return(MOD_EQUALS); }
"&="            { return(AND_EQUALS); }
"^="            { return(XOR_EQUALS); }
"|="            { return(OR_EQUALS); }
">>"            { return(BITSHIFT_RIGHT); }
">>>"           { return(FILL_SHIFT_RIGHT); }
"<<"            { return(SHIFT_LEFT); }
"++"            { return(INCR); }
"--"            { return(DECR); }
"&&"            { return(AND); }
"||"            { return(OR); }
"<="            { return(LTEQ); }
">="            { return(GTEQ); }
"=="            { return(EQUAL_COMPARE); }
"!="            { return(NOT_EQUAL); }
{CHAR_OP}       { return(*yytext); }
{IDENTIFIER}    { yylval.text =
                     new unicode_string(::LexStringToUnicode(yytext));
                  return(SYMBOL); }
.               { yylval.longNumber = *yytext;  return(ERROR); }

%%

void
InitializeLexer(const string& tokenizeString)
{
  yylineno = 1;
  const char* inputBytes = tokenizeString.c_str();
  gInputBuffer = yy_scan_bytes(inputBytes, tokenizeString.length());
}

void
FinishLexer()
{
  yy_delete_buffer(gInputBuffer);
}

unicode_string
LexStringToUnicode(const char* inputString) {
  unicode_string tempString;
  while (*inputString != '\0') {
    unsigned short characterValue = 0;
    if (*inputString == '\\') {
      switch (inputString[1]) {
        case '0': case '1': case '2': case '3':
	case '4': case '5': case '6': case '7':
          characterValue = inputString[1] - '0';
	  if (inputString[2] >= '0' && inputString[2] <= '9') {
	    characterValue *= 8;
	    characterValue += inputString[2] - '0';
	    ++inputString;
	  }
	  if ((*(inputString - 2) <= '3') &&
	      (inputString[2] >= '0' && inputString[2] <= '9')) {
	    characterValue *= 8;
	    characterValue += inputString[2] - '0';
	    ++inputString;
	  }
	  tempString += characterValue;
	  break;
	case 'u':
	  if (isxdigit(inputString[2]) && isxdigit(inputString[3]) &&
	      isxdigit(inputString[4]) && isxdigit(inputString[5])) {
	    char hexBuffer[5];
	    ::strncpy(hexBuffer, inputString + 2, 4);
	    tempString += (unicode_char)::strtol(hexBuffer, 0, 16);
	    inputString += 4;
	  } else {
	    tempString += 0x5c;
	  }
	  break;
	case 'b':
	  tempString += 0x8;
	  break;
	case 't':
	  tempString += 0x9;
	  break;
	case 'n':
	  tempString += 0xa;
	  break;
	case 'f':
	  tempString += 0xc;
          break;
        case 'r':
          tempString += 0xd;
	  break;
	case '"':
          tempString += 0x22;
          break;
        case '\'':
          tempString += 0x27;
          break;
        case '\\':
          tempString += 0x5c;
          break;
        default:
          tempString += 0x5c;
      }
      ++inputString;
    } else {
      tempString += (unicode_string::value_type)*inputString;
    }
    inputString++;
  }
  return tempString;
}

unsigned long long
ParseLong(const char* inputString, unsigned short base) {
  unsigned long long result = 0;
  for (; *inputString != 0; ++inputString) {
    unsigned long adder;
    if (*inputString >= '0' && *inputString <= '9') {
      adder = *inputString - '0';
    } else if (*inputString >= 'a' && *inputString <= 'z') {
      adder = (*inputString - 'a') + 10;
    } else if (*inputString >= 'A' && *inputString <= 'Z') {
      adder = (*inputString - 'A') + 10;
    }
    result = result * base + adder;
  }
  return result;
}

unsigned long
ParseInt(const char* inputString, unsigned short base) {
  unsigned long result = 0;
  for (; *inputString != 0; ++inputString) {
    unsigned long adder;
    if (*inputString >= '0' && *inputString <= '9') {
      adder = *inputString - '0';
    } else if (*inputString >= 'a' && *inputString <= 'z') {
      adder = (*inputString - 'a') + 10;
    } else if (*inputString >= 'A' && *inputString <= 'Z') {
      adder = (*inputString - 'A') + 10;
    }
    result = result * base + adder;
  }
  return result;
}
