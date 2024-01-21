/*
 * This program is in the public domain.
 * Use at your own risk.
 *
 * written by David Bucciarelli (tech.hmw@plus.it)
 *            Humanware s.r.l.
 */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <GL/gl.h>
#include <GL/glu.h>

#include <gltk.h>

#ifdef FX
#include <fx/fxmesa.h>
#include <glide.h>
#endif

/*
Pentium@133Mhz+Win95+Monster 3D:

	driver v0.1
	SIZE=25  => 40200 Tri/sec
	SIZE=50  => 36900 Tri/sec
	SIZE=100 => 16700 Tri/sec
	SIZE=250 => 2990 Tri/sec
	SIZE=480 => 928 Tri/sec

  driver v0.11
	SIZE=25  => 50600 Tri/sec
	SIZE=50  => 48000 Tri/sec
	SIZE=100 => 17094 Tri/sec
	SIZE=250 => 2814 Tri/sec (?)
	SIZE=480 => 771 Tri/sec (?)

  driver v0.13
	SIZE=25  => 48449 Tri/sec
	SIZE=50  => 49549 Tri/sec
	SIZE=100 => 15406 Tri/sec
	SIZE=250 => 2463 Tri/sec
	SIZE=480 => 677 Tri/sec

*/

/*
Pentium@133Mhz+Linux+Mesa+X11 (XFree 3.2) 8bit (Diamond Stealth 64 2MB VRAM):

	SIZE=25  => 2102 Tri/sec
	SIZE=50  => 691.8 Tri/sec
	SIZE=100 => 222 Tri/sec
	SIZE=250 => 44.8 Tri/sec
	SIZE=480 => 12.3 Tri/sec
*/

/*
PentiumPro@200MHz+Linux+Mesa2.2+X11 (XFree 3.2) (Joe Waters):

	SIZE=480 => 45 Tri/sec
*/

/*
Indy 4600SC@133MHz+SGI OpenGL 24bit:

	SIZE=25  => 31531 Tri/sec
	SIZE=50  => 13888 Tri/sec
	SIZE=100 => 4545 Tri/sec
	SIZE=250 => 757 Tri/sec
	SIZE=480 => 186 Tri/sec
*/

#define SIZE 25
#define NUM 10000

int main(int ac, char **av)
{
  int x,y,z;
  clock_t st,et,dt;
  float t;

  tkInitPosition(0, 0, 640, 480);
  tkInitDisplayMode(TK_RGB|TK_DIRECT|TK_SINGLE|TK_DEPTH);

  if (tkInitWindow("Z Smooth Trangle Bench") == GL_FALSE) {
    tkQuit();
  }

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(-0.5, 639.5, 0.0, 479.5, 1.0, -(float)NUM*SIZE);
  glMatrixMode(GL_MODELVIEW);

  glShadeModel(GL_SMOOTH);
  glDrawBuffer(GL_FRONT);
  glEnable(GL_DEPTH_TEST);

  glClearColor(0.0, 0.1, 1.0, 0.0);
  glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);

  glDepthFunc(GL_ALWAYS);

  st=clock();
  glBegin(GL_TRIANGLES);
  for(y=0;y<NUM;y++)
    for(x=0;x<SIZE;x++) {
      z=NUM*SIZE-(y*SIZE+x);
      glColor3f(0.0,1.0,0.0);
      glVertex3i(0,x,z);

      glColor3f(1.0,0.0,x/(float)SIZE);
      glVertex3i(SIZE-1-x,0,z);

      glColor3f(1.0,x/(float)SIZE,0.0);
      glVertex3i(x,SIZE-1-x,z);
    }
  glEnd();
  glFinish();

  et=clock();

  dt=et-st;
  t=(float)dt/(float)CLOCKS_PER_SEC;

  printf("Z Smooth Triangles/sec (%d/%f): %f\n",SIZE*NUM,t,(SIZE*NUM)/t);

  tkQuit();

  return 0;
}

