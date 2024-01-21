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

	driver v0.1
	SIZE=25  => 42520 Tri/sec (my pentium is too slow for the M3D)
	SIZE=50  => 40760 Tri/sec
	SIZE=100 => 21668 Tri/sec
	SIZE=250 => 3870 Tri/sec
	SIZE=480 => 1286 Tri/sec

	driver v0.13
	SIZE=25  => 47021 Tri/sec (my pentium is too slow for the M3D)
	SIZE=50  => 45560 Tri/sec
	SIZE=100 => 19642 Tri/sec
	SIZE=250 => 3194 Tri/sec
	SIZE=480 => 865 Tri/sec
*/

/*
Pentium@133Mhz+Linux+Mesa+X11 (XFree 3.2) 8bit (Diamond Stealth 64 2MB VRAM):

	SIZE=25  => 2480.6 Tri/sec
	SIZE=50  => 845 Tri/sec
	SIZE=100 => 269.2 Tri/sec
	SIZE=250 => 48.26 Tri/sec
	SIZE=480 => 13.7 Tri/sec
*/

/*
Indy 4600SC@133MHz+SGI OpenGL 24bit:

	SIZE=25  => 42613 Tri/sec
	SIZE=50  => 28037 Tri/sec
	SIZE=100 => 13274 Tri/sec
	SIZE=250 => 3369 Tri/sec
	SIZE=480 => 1293 Tri/sec
*/

#define SIZE 25
#define NUM 6000

int main(int ac, char **av)
{
  int x,y;
  clock_t st,et,dt;
  float t;

  tkInitPosition(0, 0, 640, 480);
  tkInitDisplayMode(TK_RGB|TK_DIRECT|TK_SINGLE);

  if (tkInitWindow("Smooth Triangle Bench") == GL_FALSE) {
    tkQuit();
  }

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluOrtho2D(-0.5, 639.5, 0.0, 479.5);
  glMatrixMode(GL_MODELVIEW);

  glShadeModel(GL_SMOOTH);
  glDrawBuffer(GL_FRONT);

  glClearColor(0.0, 0.1, 1.0, 0.0);
  glClear(GL_COLOR_BUFFER_BIT);

  st=clock();
  glBegin(GL_TRIANGLES);
  for(y=0;y<NUM;y++)
    for(x=0;x<SIZE;x++) {
      glColor3f(0.0,1.0,0.0);
      glVertex2i(0,x);
      glColor3f(1.0,0.0,x/(float)SIZE);
      glVertex2i(SIZE-1-x,0);
      glColor3f(1.0,x/(float)SIZE,0.0);
      glVertex2i(x,SIZE-1-x);
    }
  glEnd();
  glFinish();

  et=clock();

  dt=et-st;
  t=(float)dt/(float)CLOCKS_PER_SEC;

  printf("Smooth Triangles/sec (%d/%f): %f\n",SIZE*NUM,t,(SIZE*NUM)/t);

  tkQuit();

  return 0;
}

