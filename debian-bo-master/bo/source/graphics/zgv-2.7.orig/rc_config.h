/* Zgv v2.7 - GIF, JPEG and PBM/PGM/PPM viewer, for VGA PCs running Linux.
 * Copyright (C) 1993-1995 Russell Marks. See README for license details.
 *
 * rc_config.h - Defined! the zgv_config structure.
 *                       photographic evidence on page 2. :-)
 */

#include "mouse.h"

struct zgv_config {
  int videomode,zoom,vkludge,jpeg24bit,jpegspeed;
  int centreflag,blockcursor,thicktext;
  int betterpgm;  /* grinds PGM to 24-bit internally allowing 256 shades */
  int hicolmodes;	/* force zgv to think card has high-colour modes */
  int nodelprompt,perfectindex;
  int xvpic_index;
  int onefile_progress,cleartext;
  int force16fs;
  int repeat_timer,tag_timer;
  int revert,gamma,selecting;
  int dontchangestdin;
  int fs16col;
  
  int brightness;
  double contrast;
  int black_r,black_g,black_b;
  int dark_r,dark_g,dark_b;
  int medium_r,medium_g,medium_b;
  int light_r,light_g,light_b;
  int marked_r,marked_g,marked_b;
  int mode_allowed[256];

  int loop;		/* loop forever in slideshow */
  mouse_type mtype;	/* mouse type to use */
  
  char cmd[1024];	/* alternative command to run instead of viewing */
  int errignore;
  };
