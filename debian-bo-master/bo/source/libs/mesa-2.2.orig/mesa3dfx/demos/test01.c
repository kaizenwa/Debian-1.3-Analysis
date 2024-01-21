/*
 * This program is in the public domain.
 * Use at your own risk.
 *
 * written by David Bucciarelli (tech.hmw@plus.it)
 *            Humanware s.r.l.
 */

#include <stdio.h>
#include <time.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include "gltk.h"

#ifdef FX
#include <fx/fxmesa.h>
#endif

/*
Pentium@133Mhz+Win95+Mesa+Monster 3D:

	(my pentium is too slow for the M3D)
	driver v0.1
	218000 Pnts/sec

	driver v0.11
	244000 Pnts/sec

	driver v0.13
	273000 Pnts/sec

*/

/*
Pentium@133Mhz+Linux+Mesa+X11 8bit:

	167441 Pnts/sec
*/

/*
Indy 4600SC@133MHz+SGI OpenGL 24bit:

    657534.2 Pnts/sec
*/

#define NUM 5000

int main(int ac, char **av)
{
  int x,y;
  clock_t st,et,dt;
  float t;

   tkInitPosition(0, 0, 640, 480);
   tkInitDisplayMode(TK_RGB|TK_DIRECT|TK_SINGLE);

   if (tkInitWindow("Point Bench") == GL_FALSE) {
      tkQuit();
   }

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluOrtho2D(-0.5, 639.5, 0.0, 479.5);
  glMatrixMode(GL_MODELVIEW);

  glShadeModel(GL_FLAT);
  glDrawBuffer(GL_FRONT);
  glDisable(GL_DEPTH_TEST);

  glClearColor(0.0, 0.1, 1.0, 0.0);
  glClear(GL_COLOR_BUFFER_BIT);
  glColor3f(1.0,0.0,0.0);

  st=clock();
  glBegin(GL_POINTS);
  for(y=0;y<NUM;y++)
    for(x=0;x<480;x++)
      glVertex2i(x,x);
  glEnd();
  glFinish();

  et=clock();

  dt=et-st;
  t=(float)dt/(float)CLOCKS_PER_SEC;

  printf("Points/sec (%d/%f): %f\n",480*NUM,t,(480*NUM)/t);

  tkQuit();

  return 0;
}
