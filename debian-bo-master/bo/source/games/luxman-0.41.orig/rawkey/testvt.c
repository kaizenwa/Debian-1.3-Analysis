/* testvt - better example of how to use the rawkey library (using svgalib).
 *
 * The cursor keys move a dot around the screen - esc or x ends.
 * Using scan_keyboard() and is_key_pressed() obviously allows you to have
 * more than one key pressed at once, and avoids the key repeat problem.
 * You can also switch virtual consoles with this version.
 *
 * Feel free to pinch bits out of this, or whatever.
 */

#include <stdio.h>
#include <unistd.h>
#include <vga.h>
#include <rawkey.h>


/* better have at least minimal prototypes, or gcc will moan */
void switchfrom(),switchback();


main()
{
int c,x,y,f;

vga_init();

vga_setmode(G320x200x256);

if(!rawmode_init())
  {
  printf("Couldn't startup RAW mode.\n");
  exit(1);
  }

/* this sets up the VT switching stuff */
set_switch_functions(switchfrom,switchback);
allow_switch(1);

x=160; y=100;

do
  {
  vga_drawpixel(x,y);
  usleep(10000);	/* sleep for a bit to avoid burning CPU */

  scan_keyboard();

  /* the CURSOR_LEFT etc. are defined in rawkey.h, and are scancodes -
   * for, say, the Q key, you'd say "scancode_trans('q')".
   * note that symbols only obtained by shifting won't be recognised by
   * scancode_trans(), so you should only really use it for alphanumerics.
   */
  if(is_key_pressed(CURSOR_LEFT)  && x>0)	x--;
  if(is_key_pressed(CURSOR_RIGHT) && x<319)	x++;
  if(is_key_pressed(CURSOR_UP)    && y>0)	y--;
  if(is_key_pressed(CURSOR_DOWN)  && y<199)	y++;
  }
while(!is_key_pressed(scancode_trans('x')) && !is_key_pressed(ESCAPE_KEY));

allow_switch(0);
rawmode_exit();
vga_setmode(TEXT);
exit(0);
}


/* these are very simple examples for the two routines required by
 * the VT switching mechanism. In practice, you might want to redraw
 * some stuff when you 'come back'. You could even play a sound for
 * console switching, or something like that.
 */

void switchfrom()
{
vga_setmode(TEXT);
}

void switchback()
{
vga_setmode(G320x200x256);
}
