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
	SIZE=50 => 65900 Lin/sec
	SIZE=100 => 64400 Lin/sec
	SIZE=250 => 65900 Lin/sec
	SIZE=480 => 38570 Lin/sec

	driver v0.13
	SIZE=50 => 73891 Lin/sec
	SIZE=100 => 72463 Lin/sec
	SIZE=250 => 67024 Lin/sec
	SIZE=480 => 34951 Lin/sec
*/

/*
Pentium@133Mhz+Linux+Mesa+X11 (XFree 3.2) 8bit (Diamond Stealth 64 2MB VRAM):

	SIZE=50 => 5084 Lin/sec
	SIZE=100 => 2631 Lin/sec
	SIZE=250 => 1016 Lin/sec
	SIZE=480 => 536 Lin/sec
*/

/*
Indy 4600SC@133MHz+SGI OpenGL 24bit:

	SIZE=50 => 181818 Lin/sec
	SIZE=100 => 179640 Lin/sec
	SIZE=250 => 123915 Lin/sec
	SIZE=480 => 77170 Lin/sec
*/

#define SIZE 50
#define NUM 6000

int main(int ac, char **av)
{
  int x,y;
  clock_t st,et,dt;
  float t;

  tkInitPosition(0, 0, 640, 480);
  tkInitDisplayMode(TK_RGB|TK_DIRECT|TK_SINGLE);

  if (tkInitWindow("Smooth Line Bench") == GL_FALSE) {
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
    
  glShadeModel(GL_SMOOTH);

  st=clock();
  glBegin(GL_LINES);
  for(y=0;y<NUM;y++)
    for(x=0;x<SIZE;x++) {
      glColor3f(0.0,1.0,y/(float)NUM);
      glVertex2i(0,SIZE-1);
      glColor3f(1.0,0.0,x/(float)SIZE);
      glVertex2i(x,x);
    }
  glEnd();
  glFinish();

  et=clock();

  dt=et-st;
  t=(float)dt/(float)CLOCKS_PER_SEC;

  printf("Smooth Lines/sec (%d/%f): %f\n",SIZE*NUM,t,(SIZE*NUM)/t);

  tkQuit();

  return 0;
}

