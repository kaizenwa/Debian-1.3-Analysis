
/* Written by Peter Ekberg, peda@lysator.liu.se */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <vga.h>
#include <vgakeyboard.h>
#include <linux/keyboard.h>
#include <string.h>

#include "keyboard.h"

#ifndef HAVE_DEFINE_SCANCODE_P
#define SCANCODE_P 25
#endif

int scancode[5] = {
  SCANCODE_A,
  SCANCODE_S,
  SCANCODE_RIGHTCONTROL,
  SCANCODE_ENTER,
  SCANCODE_SPACE
};

/* All unknown keys are unknown since different keyboards have different
   keys at these scancodes. It would be nice if there were existed some
   library routine to map a scancode to a character. I havn't researched
   this, so this routine might exist without me knowing it. There exists
   a program called getkeycodes that seems to do something like this. */

char *keynames[NR_KEYS] = {
  "NOT AVAILABLE",
  "ESCAPE",            /*   1 */
  "1",                 /*   2 */
  "2",
  "3",
  "4",
  "5",
  "6",
  "7",
  "8",
  "9",
  "0",                 /*  11 */
  "UNKNOWN",
  "UNKNOWN",
  "BACKSPACE",         /*  14 */
  "TAB",               /*  15 */
  "Q",                 /*  16 */
  "W",
  "E",
  "R",
  "T",
  "Y",
  "U",
  "I",
  "O",
  "P",                 /*  25 */
  "UNKNOWN",
  "UNKNOWN",
  "ENTER",             /*  28 */
  "LEFT CONTROL",      /*  29 */
  "A",                 /*  30 */
  "S",
  "D",
  "F",
  "G",
  "H",
  "J",
  "K",
  "L",                 /*  38 */
  "UNKNOWN",
  "UNKNOWN",
  "UNKNOWN",
  "LEFT SHIFT",        /*  42 */
  "UNKNOWN",
  "Z",                 /*  44 */
  "X",
  "C",
  "V",
  "B",
  "N",
  "M",                 /*  50 */
  "UNKNOWN",
  "UNKNOWN",
  "UNKNOWN",
  "RIGHT SHIFT",       /*  54 */
  "UNKNOWN",
  "LEFT ALT",          /*  56 */
  "SPACE",             /*  57 */
  "CAPS LOCK",         /*  58 */
  "F1",                /*  59 */
  "F2",
  "F3",
  "F4",
  "F5",
  "F6",
  "F7",
  "F8",
  "F9",
  "F10",               /*  68 */
  "NUM LOCK",          /*  69 */
  "SCROLL LOCK",       /*  70 */
  "KEYPAD HOME",       /*  71 */
  "KEYPAD UP",
  "KEYPAD PGUP",       /*  73 */
  "UNKNOWN",
  "KEYPAD LEFT",       /*  75 */
  "KEYPAD 5",
  "KEYPAD RIGHT",      /*  77 */
  "UNKNOWN",
  "KEYPAD END",        /*  79 */
  "KEYPAD DOWN",
  "KEYPAD PGDN",       /*  81 */
  "KEYPAD 0",          /*  82 */
  "KEYPAD DECIMAL",    /*  83 */
  "UNKNOWN",
  "UNKNOWN",
  "UNKNOWN",
  "F11",               /*  87 */
  "F12",               /*  88 */
  "UNKNOWN",
  "UNKNOWN",
  "UNKNOWN",
  "UNKNOWN",
  "UNKNOWN",
  "UNKNOWN",
  "UNKNOWN",
  "KEYPAD ENTER",      /*  96 */
  "RIGHT CONTROL",     /*  97 */
  "UNKNOWN",
  "PRINT SCREEN",      /*  99 */
  "RIGHT ALT",         /* 100 */
  "UNKNOWN",
  "HOME",              /* 102 */
  "CURSOR UP",         /* 103 */
  "PAGE UP",           /* 104 */
  "CURSOR LEFT",       /* 105 */
  "CURSOR RIGHT",      /* 106 */
  "END",               /* 107 */
  "CURSOR DOWN",       /* 108 */
  "PAGE DOWN",         /* 109 */
  "INSERT",            /* 110 */
  "DELETE"             /* 111 */
};

void
singlekey(void)
{
  keyboard_close();
  vga_unlockvc();
}

void
multiplekeys(void)
{
  vga_lockvc();
  keyboard_init();
}

int
getonemultiplekey(void)
{
  char *keys;
  int i;
  int result=0, gotkey=0;

  keys = keyboard_getstate();

  do {
    usleep(20000L);
    keyboard_update();
    for(i=0; i<NR_KEYS; i++) {
      if(keys[i]) {
	gotkey=i;
	i=NR_KEYS;
      }
    }
  } while(!gotkey);

  do {
    usleep(20000L);
    keyboard_update();
    if(!keys[gotkey])
      result=gotkey;
  } while(!result);

  return(result>111?0:result);
}

int
getkey(void)
{
  return vga_getkey();
}

unsigned char
getkeys(void)
{
  unsigned char keybits=0;

  keyboard_update();

  if(keyboard_keypressed(SCANCODE_P))
    keybits|=pause_bit;
  else if(keyboard_keypressed(SCANCODE_ESCAPE) ||
	  keyboard_keypressed(SCANCODE_Q))
    keybits|=escape_bit;
  if(keyboard_keypressed(scancode[0]))
    keybits|=left_bit;
  if(keyboard_keypressed(scancode[1]))
    keybits|=right_bit;
  if(keyboard_keypressed(scancode[2]))
    keybits|=thrust_bit;
  if(keyboard_keypressed(scancode[3]))
    keybits|=fire_bit;
  if(keyboard_keypressed(scancode[4]))
    keybits|=pickup_bit;

  return keybits;
}

char *
keystring(int key)
{
  static char keybuffer[100];

  if(key<0 || key>111)
    return NULL;

  strncpy(keybuffer, keynames[key], 99);
  keybuffer[99] = '\0';

  return keybuffer;
}

int
keycode(char *keyname)
{
  static char keybuffer[100];
  int i;

  strncpy(keybuffer, keyname, 99);
  for(i=0; i<strlen(keybuffer); i++) {
    if(keybuffer[i] == '_')
      keybuffer[i] = ' ';
  }

  for(i=0; i<=111; i++) {
    if(!strcasecmp(keynames[i], keybuffer))
      return(i);
  }

  return(0);
}

void
flushkeyboard(void)
{
  keyboard_update();
}

int
keywaiting(void)
{
  return keyboard_update();
}

int
keyinit(void)
{
  return keyboard_init();
}

int
keyclose(void)
{
  keyboard_close();
  return 0;
}

char *
keyname(void)
{
  static char name[] = "SVGA";

  return name;
}
