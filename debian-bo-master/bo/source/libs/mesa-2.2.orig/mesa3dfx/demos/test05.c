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
#include "gltk.h"

#ifdef FX
#include <windows.h>
#include <fx/fxmesa.h>
#include <glide.h>
#endif

/*
Pentium@133Mhz+Win95+Monster 3D:

	driver v0.11
  SIZE=25  => 65104 Tri/sec
	SIZE=50  => 64516 Tri/sec
	SIZE=100 => 17341 Tri/sec
	SIZE=250 => 2780 Tri/sec
	SIZE=480 => 770 Tri/sec

*/

/*
Pentium@133Mhz+Linux+Mesa+X11 (XFree 3.2) 8bit (Diamond Stealth 64 2MB VRAM):

  SIZE=25  => 456.2 Tri/sec
	SIZE=50  => 128.2 Tri/sec
	SIZE=100 => 34.2 Tri/sec
	SIZE=250 => 5.7 Tri/sec
	SIZE=480 => 1.2 Tri/sec
*/

/*
Indy 4600SC@133MHz+SGI OpenGL 24bit:

  SIZE=25  => 1582.2 Tri/sec
	SIZE=50  => 419.5 Tri/sec
	SIZE=100 => 108.1 Tri/sec
	SIZE=250 => 16.5 Tri/sec
	SIZE=480 => 4.5 Tri/sec

*/

/************************* Test Modified *************************/

/*
Pentium@133Mhz+Win95+Monster 3D:

	driver v0.13
  SIZE=25  => 44169 Tri/sec
	SIZE=50  => 44000 Tri/sec
	SIZE=100 => 15151 Tri/sec
	SIZE=250 => 2475 Tri/sec
	SIZE=480 => 682 Tri/sec

*/

/*
Pentium@133Mhz+Linux+Mesa+X11 (XFree 3.2) 8bit (Diamond Stealth 64 2MB VRAM):

*/

/*
Indy 4600SC@133MHz+SGI OpenGL 24bit:

*/

#define SIZE 25
#define NUM 10000

#define TEXSIZE 128

int main(int ac, char **av)
{
  int x,y,z;
  clock_t st,et,dt;
  float t;
  GLubyte tex[TEXSIZE][TEXSIZE][3];
  GLenum gluerr;

  tkInitPosition(0, 0, 640, 480);
  tkInitDisplayMode(TK_RGB|TK_DIRECT|TK_SINGLE|TK_DEPTH);

  if (tkInitWindow("Z Tex Smooth Trangle Bench") == GL_FALSE) {
    tkQuit();
  }

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(-0.5, 639.5, 0.0, 479.5, 1.0, -(float)NUM*SIZE);
  /*glOrtho(-0.5, 639.5, 0.0, 479.5, 0.0, -100.0);*/

  glMatrixMode(GL_MODELVIEW);

  glShadeModel(GL_SMOOTH);
  glDrawBuffer(GL_FRONT);
  glEnable(GL_DEPTH_TEST);

  for(y=0;y<TEXSIZE;y++)
    for(x=0;x<TEXSIZE;x++) {
	  tex[x][y][0]=((x % (TEXSIZE/4)) < (TEXSIZE/8)) ? 255 : 0;
	  tex[x][y][1]=((y % (TEXSIZE/4)) < (TEXSIZE/8)) ? 255 : 0;
	  tex[x][y][2]=x;
	}

  glPixelStorei(GL_UNPACK_ALIGNMENT,1);
  if((gluerr=gluBuild2DMipmaps(GL_TEXTURE_2D, 3, TEXSIZE, TEXSIZE, GL_RGB,
	  GL_UNSIGNED_BYTE, (GLvoid *)(&tex[0][0][0])))) {
	  fprintf(stderr,"GLULib%s\n",gluErrorString(gluerr));
	  exit(-1);
  }

  glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_REPEAT);
  glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,GL_REPEAT);

	glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_NEAREST_MIPMAP_LINEAR);
  glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);

  glTexEnvf(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_MODULATE);
  glEnable(GL_TEXTURE_2D);

  glDepthFunc(GL_ALWAYS);

  glClearColor(0.0, 0.1, 1.0, 0.0);
  glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);

#if 0
  glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_CLAMP);
  glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,GL_CLAMP);

  glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
  glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_NEAREST);

  glTexEnvf(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_DECAL);
  glShadeModel(GL_FLAT);

  glBegin(GL_QUADS);
  glColor3f(1.0,0.0,0.0);
  glTexCoord2f(-3.0,-3.0);
  glVertex3i(0,0,10); 

  glColor3f(0.0,1.0,0.0);
  glTexCoord2f(3.0,-3.0);
  glVertex3i(480,0,10);

  glColor3f(0.0,0.0,1.0);
  glTexCoord2f(3.0,3.0);
  glVertex3i(480,480,10);

  glColor3f(0.0,1.0,1.0);
  glTexCoord2f(-3.0,3.0);
  glVertex3i(0,480,10);
  glEnd();
  glFinish();
  Sleep(5000);
#endif

#if 1
  st=clock();

  glBegin(GL_TRIANGLES);
  for(y=0;y<NUM;y++)
    for(x=0;x<SIZE;x++) {
      z=NUM*SIZE-(y*SIZE+x);
			glTexCoord2f(1.0,1.0);
			glColor3f(1.0,0.0,0.0);
      glVertex3i(0,x,z);

			glTexCoord2f(0.0,1.0);
			glColor3f(0.0,1.0,0.0);
      glVertex3i(SIZE-1-x,0,z);

			glTexCoord2f(1.0,0.0);
			glColor3f(0.0,0.0,1.0);
      glVertex3i(x,SIZE-1-x,z);
    }
  glEnd();
  glFinish();

  et=clock();

  dt=et-st;
  t=(float)dt/(float)CLOCKS_PER_SEC;

  printf("Z Smooth Tex Triangles/sec (%d/%f): %f\n",SIZE*NUM,t,(SIZE*NUM)/t);
#endif
  tkQuit();

  return 0;
}

