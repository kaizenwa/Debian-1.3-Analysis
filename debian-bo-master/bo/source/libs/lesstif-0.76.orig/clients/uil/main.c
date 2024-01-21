/**
 *
 * $Id: main.c,v 1.2 1996/11/06 04:22:55 miers Exp $
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
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

#ifdef HAVE_GETOPT_H
#include <getopt.h>
#else
#include "../lib/getopt.h"
#endif

#include "FakeWidget.h"
#include "uil.h"
#include "glue.h"
#include "Include.h"

static char *rcsid = "$Id: main.c,v 1.2 1996/11/06 04:22:55 miers Exp $";

void EmitWidgetTree(FakeWidgetList *);

int yydebug;
extern FileData Files[];
int yyparse (void);

#define MAX_PATH 10
char *IncludePath[MAX_PATH];
int pathCount = 0;

FakeWidgetList WidgetTable;
ExpressionList GlobalSymbolTable;
ExpressionList LocalSymbolTable;

char OutputFileName[256];

int main(int argc, char **argv)
{
  int Generate_Binary_Code = False;
  int c;
  FILE *input, *output;

  FakeWidgetListNew(&WidgetTable);
  ExpressionListNew(&GlobalSymbolTable);
  ExpressionListNew(&LocalSymbolTable);
  yydebug = 0;
  OutputFileName[0] = 0;

  while (-1 != (c = getopt(argc, argv, "dcI:mo:sv:w"))) {
    switch (c) {
    case 'c':
    case 'd':
      yydebug = 1;
      break;
    case 'I':	
      IncludeAddDirectory(optarg);
      break;
    case 'm':
      Generate_Binary_Code = True;
      break;
    case 'o':	
      strcpy(OutputFileName, optarg);
      break;
    case 's':	
      fprintf(stderr,"set locale\n");
      break;
    case 'V':	
      puts(rcsid);
      exit(0);
      break;
    case 'v':	
      fprintf(stderr,"generate listing to %s\n",
	      optarg);
      break;
    case 'w':	
      fprintf(stderr,"surpress all warnings\n");
      break;
    case 'W':	
      fprintf(stderr,"WMD\n");
      break;
    case '?':
      exit(1);
      break;
    default:
      fprintf(stderr,"?? getopt returned character code 0%o \n",c);
      exit(1);
      break;
    }
  }

  if (optind >= argc){
    fprintf(stderr,"\n\nsevere: no source file specified\n");
    exit(1);
  }

  input = freopen(argv[optind],"r",stdin);
  if (input == NULL){
    fprintf(stderr,"\n\nsevere: error opening source file: %s\n",
	    argv[optind]);
    exit(1);
  }
  strcpy(Files[0].Name,argv[optind]);
  Files[0].lineno = 1;
  if (0 == OutputFileName[0])
    strcpy(OutputFileName,"a.uid");

  output = freopen(OutputFileName,"w",stdout);
  if (output == NULL){
    Exit(LOC, "\n\nsevere: error opening output file: %s\n",
	 OutputFileName);
  }

  yyparse();
  FakeWidgetListIndex(&WidgetTable);
  EmitWidgetTree(&WidgetTable);
  exit(0);
}

void
EmitWidgetTree( FakeWidgetList *wil)
{
  FakeWidgetListEmitList(wil);/* Emit a list of the widgets (maybe this can go) */
  ExpressionListEmit(&GlobalSymbolTable); /* Global symbols defined in this uil */
  putchar((unsigned char) 0); 
  ExpressionListEmit(&LocalSymbolTable);  /* Symbols only usable within this uil */
  putchar((unsigned char) 0); 
  FakeWidgetListEmit(wil);
}

