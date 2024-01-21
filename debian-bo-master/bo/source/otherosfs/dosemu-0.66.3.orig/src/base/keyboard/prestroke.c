/* This is file prestroke.c
 *
 * (C) 1997 under GPL, Hans Lermen <lermen@fgan.de>
 */

#include <stdlib.h>
#include <string.h>
#include "emu.h"
#include "keyboard.h"
#include "keymaps.h"

#define NUM_KEYS 97 /* (sizeof(key_map_us)) */


static int scantable(unsigned char *table, unsigned char ch)
{
  int i;
  for (i=0; i < NUM_KEYS; i++) if (ch == table[i]) return i;
  return -1;
}


static int default_stroke_pause = -1;
static int stroke_pause;

#define GETNUMBER(s) ({ \
  int ret; char *end; \
  ret = strtol(s, &end, 0); \
  s = end; \
  if (*s) s++; \
  ret; \
})

static unsigned char *recode(unsigned short *out, unsigned char *in)
{
  int keynum,esc;
  unsigned char ch;
  static char ctrl[]="JMGHLIK";

  stroke_pause = default_stroke_pause;
  *out = 0;
  if (!in || !in[0]) return 0;
  ch=*(in++);
  switch (ch) {
    case '\\': {
      ch=*(in++);
      if (!ch) return 0;
      esc = -1;
      switch (ch) {
        case 'v': esc++;
        case 't': esc++;
        case 'f': esc++;
        case 'b': esc++;
        case 'a': esc++;
        case 'r': esc++;
        case 'n': esc++;
        case '^': { /* example:  \a  == \^G  == <Ctrl>G
                                 \r  == \^M  == <Ctrl>M == <ENTER> */
          if (esc >= 0) ch = ctrl[esc];
          else ch=*(in++);
          if (((keynum = scantable(config.key_map, ch)) >0) ||
              ((keynum = scantable(config.shift_map, ch)) >0 )) {
            if (keynum == 50) {
              *(out++) = 0x0d1c;
              *(out++) = 0x0d1c | 0x80;
            }
            else {
              keynum |= (ch & 31) << 8;
              *(out++) = 0x1d;           /* Ctrl pressed */
              *(out++) = keynum;         /* key pressed */
              *(out++) = keynum | 0x80;  /* key released */
              *(out++) = 0x9d;           /* Ctrl released */
            }
            *(out++) = 0;
          }
          return in;
        }
        case 'A': { /* example:  \Az  == <Alt>z */
          ch=*(in++);
          if (((keynum = scantable(config.key_map, ch)) >0) ||
              ((keynum = scantable(config.shift_map, ch)) >0 )) {
            *(out++) = 0x38;           /* Alt pressed */
            *(out++) = keynum;         /* key pressed */
            *(out++) = keynum | 0x80;  /* key released */
            *(out++) = 0xb8;           /* Alt released */
            *(out++) = 0;
          }
          return in;
        }
        case 'F': { /* example:  \F12;  == key F12
                                 \F1;   == key F1 */
          keynum = GETNUMBER(in);
          if ((keynum > 0) && (keynum < 12)) {
            keynum += 0x3a;
            *(out++) = keynum;         /* key pressed */
            *(out++) = keynum | 0x80;  /* key released */
            *(out++) = 0;
          }
          return in;
        }
        case 'p': { /* example:  \p100; == pause one second */
          keynum = GETNUMBER(in);
          if ((keynum > 0) && (keynum < 10000)) {
            stroke_pause = keynum;
          }
          return in;
        }
        case 'P': { /* example:  \P15; == set rate to 100/15 cps */
          keynum = GETNUMBER(in);
          if (keynum > 0) {
            keynum--;
            if (keynum > 100) keynum = 100;
            default_stroke_pause = keynum;
          }
          return in;
        }
        default:
        /* fall through */
      }
    }
    default: {
      if ((keynum = scantable(config.key_map, ch)) >0 ) {
        keynum |= ch <<8;
        *(out++) = keynum;         /* key pressed */
        *(out++) = keynum | 0x80;  /* key released */
        *(out++) = 0;
        return in;
      }
      if ((keynum = scantable(config.shift_map, ch)) >0 ) {
        keynum |= ch <<8;
        *(out++) = 0x2a;           /* shift pressed */
        *(out++) = keynum;         /* key pressed */
        *(out++) = keynum | 0x80;  /* key released */
        *(out++) = 0xaa;           /* shift released */
        *(out++) = 0;
        return in;
      }
    }
  }
  return in;
}

int type_in_pre_strokes()
{
  if (config.pre_stroke) {
    unsigned short out[16], *o;
    config.pre_stroke = recode(out, config.pre_stroke);
    if (config.pre_stroke) {
      o=out;
      while (*o) {
#ifdef NEW_KBD_CODE
        putkey((*o & 0x80)==0, (t_keysym)*o & 0xff, *o >>8);
        o++;
#else
        add_scancode_to_queue(*(o++) & 0xff);
#endif
      }
    }
  }
  return stroke_pause;
}


void append_pre_strokes(unsigned char *s)
{
  if (config.pre_stroke) {
    int l1,l2;
    unsigned char *n;

    l1 = strlen(config.pre_stroke);
    l2 = strlen(s);
    n = malloc(l1+l2+1);
    if (!n) return;
    memcpy(n, config.pre_stroke, l1);
    memcpy(n+l1, s, l2+1);
    free(config.pre_stroke);
    config.pre_stroke = n;
  }
  else {
    config.pre_stroke = strdup(s);
  }
}
