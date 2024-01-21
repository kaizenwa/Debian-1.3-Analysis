/*
 * wall.c	Write to all users logged in.
 *
 * Usage:	wall [text]
 *
 * Version:	@(#)wall  1.01  18-Nov-1992  MvS
 *
 *		This file is part of the sysvinit suite,
 *		Copyright 1991-1995 Miquel van Smoorenburg.
 *
 *		This program is free software; you can redistribute it and/or
 *		modify it under the terms of the GNU General Public License
 *		as published by the Free Software Foundation; either version
 *		2 of the License, or (at your option) any later version.
 */

#include <string.h>
#include <stdio.h>

char *Version = "@(#) wall 1.01 18-11-1992 MvS";
#define MAXLEN 4096

extern void wall();

int main(argc, argv)
int argc;
char **argv;
{
  char buf[MAXLEN];
  char line[83];
  int f;
  int len = 0;
  char *p;
  
  buf[0] = 0;

  if (argc > 1) {
  	for(f = 1; f < argc; f++) {
  		len += strlen(argv[f]) + 1;
  		if (len >= MAXLEN) break;
  		strcat(buf, argv[f]);
  		strcat(buf, " ");
  	}
  	strcat(buf, "\r\n");
  } else {
  	while(fgets(line, 80, stdin)) {
  		/* Take care that line ends in \r\n */
  		for(p = line; *p && *p != '\n'; p++)
  			;
  		strcpy(p, "\r\n");
  		len += strlen(line);
  		if (len >= MAXLEN) break;
  		strcat(buf, line);
  	}
  }
  wall(buf, 0);
  return(0);
}
