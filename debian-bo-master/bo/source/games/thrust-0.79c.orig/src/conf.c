
/* Written by Peter Ekberg, peda@lysator.liu.se */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "thrust_types.h"
#include "keyboard.h"
#include "conf.h"
#include "font5x5.h"
#include "thrust.h"
#include "keyboard.h"
#include "gr_drv.h"

int
getscancode(int old, int x, int y)
{
  int result;
  static char str[40];
  int oldcolor;

  oldcolor=chcolor;
  sprintf(str, "%s", keystring(old));
  printgs(x, y, str);

  displayscreen();
  result = getonemultiplekey();
  if(!strcasecmp("ESCAPE", keystring(result)))
    result = old;
  chcolor=0;
  printgs(x, y, str);
  chcolor=oldcolor;
  sprintf(str, "%s", keystring(result));
  printgs(x, y, str);

  return(result);
}

#define DESCRIPTIONS (5)

void
conf()
{
  static char *descriptions[DESCRIPTIONS] = {
    "TURN LEFT",
    "TURN RIGHT",
    "THRUST",
    "FIRE",
    "PICKUP & SHIELD"
  };
  int i, end;
  char *keyname;

  chcolor = HIGHLIGHT;
  gcenter(45, "CONFIGURATION");
  chcolor = TEXTCOLOR;
  for(i=0; i<DESCRIPTIONS; i++) {
    printgs(160-gstrlen(descriptions[i]), 65+i*8, descriptions[i]);
    printgs(161, 65+i*8, ":");
    printgs(167, 65+i*8, keystring(scancode[i]));
  }
  gcenter(125, "PRESS ENTER TO CHANGE A KEY");
  gcenter(132, "(DON'T USE Q OR P, THESE ARE TAKEN)");
  gcenter(139, "USE CURSOR UP/DOWN TO MOVE AROUND");
  gcenter(148, "PRESS ESCAPE FOR THE MAIN MENU");

  i=0;
  end=0;
  chcolor = HIGHLIGHT;
  printgs(160-gstrlen(descriptions[i]), 65+i*8, descriptions[i]);
  chcolor = TEXTCOLOR;

  fade_in();

  do {
    keyname = keystring(getonemultiplekey());

    if(!strcasecmp("CURSOR UP", keyname)
       || !strcasecmp("KEYPAD UP", keyname)
       || !strcasecmp("UP", keyname)
       || !strcasecmp("KP UP", keyname)) {
      printgs(160-gstrlen(descriptions[i]), 65+i*8, descriptions[i]);
      i=(i+DESCRIPTIONS-1)%DESCRIPTIONS;
    }

    if(!strcasecmp("CURSOR DOWN", keyname)
       || !strcasecmp("KEYPAD DOWN", keyname)
       || !strcasecmp("DOWN", keyname)
       || !strcasecmp("KP DOWN", keyname)) {
      printgs(160-gstrlen(descriptions[i]), 65+i*8, descriptions[i]);
      i=(i+1)%DESCRIPTIONS;
    }

    if(!strcasecmp("ENTER", keyname)
       || !strcasecmp("KEYPAD ENTER", keyname)
       || !strcasecmp("KP ENTER", keyname)
       || !strcasecmp("RETURN", keyname)) {
      printgs(160-gstrlen(descriptions[i]), 65+i*8, descriptions[i]);

      chcolor = HIGHLIGHT;
      scancode[i]=getscancode(scancode[i], 167, 65+i*8);
      printgs(160-gstrlen(descriptions[i]), 65+i*8, descriptions[i]);
      chcolor = TEXTCOLOR;

      printgs(167, 65+i*8, keystring(scancode[i]));
      displayscreen();
    }

    if(!strcasecmp("ESCAPE", keyname)
       || !strcasecmp("Q", keyname)) {
      end=1;
    }

    chcolor = HIGHLIGHT;
    printgs(160-gstrlen(descriptions[i]), 65+i*8, descriptions[i]);
    chcolor = TEXTCOLOR;

    displayscreen();
  } while(!end);
  
  chcolor = TEXTCOLOR;
  
  fade_out();
}

void initkeys(void)
{
  char *home;
  char *thrustrc;
  char *keyboarddriver;
  FILE *f;
  int rows=0;
  int res;
  unsigned char row[256], field[256], value[256], driver[256];

  home=getenv("HOME");
  if(home==NULL || home[0]=='\0')
    return;

  thrustrc = malloc(strlen(home) + 11);
  if(thrustrc == NULL)
    return;

  strcpy(thrustrc, home);
  if(home[strlen(home)-1]!='/') {
    thrustrc[strlen(home)] = '/';
    thrustrc[strlen(home)+1] = '\0';
  }
  strcat(thrustrc, ".thrustrc");

  f = fopen(thrustrc, "r");
  if(f == NULL)
    return;

  keyboarddriver = keyname();

  while(!feof(f)) {
    rows++;
    if(fgets(row, 255, f) == NULL) {
      if(ferror(f)) {
	perror("Error while parsing row %d of \"%s\"\n");
	break;
      }
      else
	row[0] = '\0';
    }
    if(!feof(f)) {
      if(row[strlen(row)-1] != '\n') {
	printf("Row %d of \"%s\" is too long.\n", rows, thrustrc);
	break;
      }
      else
	row[strlen(row)-1] = '\0';
    }
    if(row[0] != '#') {
      res = sscanf(row, "%[^-]-%s %s", driver, field, value);
      if(res==2)
	printf("Syntax error in row %d of \"%s\".\n", rows, thrustrc);
      else if(res>=3) {
	if(!strcasecmp(driver, "SVGA")
	   || !strcasecmp(driver, "X11")) {
	  if(!strcasecmp(driver, keyboarddriver)) {
	    if(!strcasecmp(field, "counterclockwise")) {
	      scancode[0] = keycode(value);
	    }
	    else if(!strcasecmp(field, "clockwise")) {
	      scancode[1] = keycode(value);
	    }
	    else if(!strcasecmp(field, "thrust")) {
	      scancode[2] = keycode(value);
	    }
	    else if(!strcasecmp(field, "fire")) {
	      scancode[3] = keycode(value);
	    }
	    else if(!strcasecmp(field, "pickup")) {
	      scancode[4] = keycode(value);
	    }
	    else {
	      printf("Illegal keyboard field \"%s\" specified in row %d.\n",
		     field, rows);
	    }
	  }
	}
	else {
	  printf("Illegal keyboard driver \"%s\" specified in row %d.\n",
		 driver, rows);
	}
      }
    }
  }

  fclose(f);
}
