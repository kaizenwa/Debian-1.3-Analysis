/*
 * This program is in the public domain.
 * Use at your own risk.
 *
 * written by David Bucciarelli (tech.hmw@plus.it)
 *            Humanware s.r.l.
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include "gltk.h"

#ifdef FX
#include <windows.h>
#include <fx/fxmesa.h>
#endif

/*
Pentium@133Mhz+Win95+Monster 3D:

	driver v0.13
	27.1 frames/sec
*/

/*
Pentium@133Mhz+Linux+Mesa+X11 (XFree 3.2) 8bit (Diamond Stealth 64 2MB VRAM):

	0.233281 frames/sec (without fog !!!)
*/

/*
Indy 4600SC@133MHz+SGI OpenGL 24bit:


*/

#define vinit(a,i,j,k) {\
	(a)[0]=i;\
	(a)[1]=j;\
	(a)[2]=k;\
}

#define vinit4(a,i,j,k,w) {\
	(a)[0]=i;\
	(a)[1]=j;\
	(a)[2]=k;\
	(a)[3]=w;\
}


#define vadds(a,dt,b) {\
	(a)[0]+=(dt)*(b)[0];\
	(a)[1]+=(dt)*(b)[1];\
	(a)[2]+=(dt)*(b)[2];\
}

#define vequ(a,b) {\
	(a)[0]=(b)[0];\
	(a)[1]=(b)[1];\
	(a)[2]=(b)[2];\
}

#define vinter(a,dt,b,c) {\
	(a)[0]=(dt)*(b)[0]+(1.0-dt)*(c)[0];\
	(a)[1]=(dt)*(b)[1]+(1.0-dt)*(c)[1];\
	(a)[2]=(dt)*(b)[2]+(1.0-dt)*(c)[2];\
}

#define clamp(a)        ((a) < 0.0 ? 0.0 : ((a) < 1.0 ? (a) : 1.0))

#define vclamp(v) {\
	(v)[0]=clamp((v)[0]);\
	(v)[1]=clamp((v)[1]);\
	(v)[2]=clamp((v)[2]);\
}

#define FRAME 100
#define DIMP 10.0
#define DIMTP 8.0

#define RIDCOL 0.4

#define AGRAV -9.8

typedef struct {
  int age;
  float p[3][3];
  float v[3];
  float c[3][4];
} part;

static float black[3]={0.0,0.0,0.0};
static float white[3]={1.0,1.0,1.0};
static float blu[3]={0.0,0.2,1.0};
static float blu2[3]={0.0,1.0,1.0};
static float red[3]={1.0,0.0,0.0};
static float yellow[3]={1.0,1.0,0.0};
static float light[3]={-1.0,1.0,1.0};

static float fogcolor[4]={1.0,0.0,0.0,1.0};

#ifdef FX
static GrFog_t fogtable[GR_FOG_TABLE_SIZE];
#endif

static float q[4][3]={
  {-DIMP,0.0,-DIMP},
  {DIMP,0.0,-DIMP},
  {DIMP,0.0,DIMP},
  {-DIMP,0.0,DIMP}
};

static float qt[4][2]={
  {-DIMTP,-DIMTP},
  {DIMTP,-DIMTP},
  {DIMTP,DIMTP},
  {-DIMTP,DIMTP}
};

static int np;
static float eject_r,dt,maxage,eject_vy,eject_vl;
static short shadows;
static float ridtri;

static part *p;

static int autorot=1;
static float xrot=0.0;
static float yrot=0.0;

static float gettime(void)
{
  static clock_t told=0;
  clock_t tnew,ris;

  tnew=clock();

  ris=tnew-told;

  told=tnew;

  return(ris/(float)CLOCKS_PER_SEC);
}

float vrnd(void)
{
  return(((float)rand())/RAND_MAX);
}

static void setnewpart(part *p)
{
  float a,v[3],*c;

  p->age=0;

  a=vrnd()*3.14159265359*2.0;

	vinit(v,sin(a)*eject_r*vrnd(),0.15,cos(a)*eject_r*vrnd());
	vinit(p->p[0],v[0]+vrnd()*ridtri,v[1]+vrnd()*ridtri,v[2]+vrnd()*ridtri);
	vinit(p->p[1],v[0]+vrnd()*ridtri,v[1]+vrnd()*ridtri,v[2]+vrnd()*ridtri);
	vinit(p->p[2],v[0]+vrnd()*ridtri,v[1]+vrnd()*ridtri,v[2]+vrnd()*ridtri);

	vinit(p->v,v[0]*eject_vl/(eject_r/2),vrnd()*eject_vy+eject_vy/2,v[2]*eject_vl/(eject_r/2));

	c=blu;

	vinit4(p->c[0],c[0]*((1.0-RIDCOL)+vrnd()*RIDCOL),
		c[1]*((1.0-RIDCOL)+vrnd()*RIDCOL),
		c[2]*((1.0-RIDCOL)+vrnd()*RIDCOL),
		1.0);
	vinit4(p->c[1],c[0]*((1.0-RIDCOL)+vrnd()*RIDCOL),
		c[1]*((1.0-RIDCOL)+vrnd()*RIDCOL),
		c[2]*((1.0-RIDCOL)+vrnd()*RIDCOL),
		1.0);
	vinit4(p->c[2],c[0]*((1.0-RIDCOL)+vrnd()*RIDCOL),
		c[1]*((1.0-RIDCOL)+vrnd()*RIDCOL),
		c[2]*((1.0-RIDCOL)+vrnd()*RIDCOL),
		1.0);
}

static void setpart(part *p)
{
	float fact;

  if(p->p[0][1]<0.1) {
    setnewpart(p);
    return;
  }

  p->v[1]+=AGRAV*dt;

	vadds(p->p[0],dt,p->v);
	vadds(p->p[1],dt,p->v);
	vadds(p->p[2],dt,p->v);

  p->age++;

  if((p->age)>maxage) {
		vequ(p->c[0],blu2);
		vequ(p->c[1],blu2);
		vequ(p->c[2],blu2);
  } else {
		fact=1.0/maxage;
		vadds(p->c[0],fact,blu2);
		vclamp(p->c[0]);
		p->c[0][3]=fact*(maxage-p->age);

		vadds(p->c[1],fact,blu2);
		vclamp(p->c[1]);
		p->c[1][3]=fact*(maxage-p->age);

		vadds(p->c[2],fact,blu2);
		vclamp(p->c[2]);
		p->c[2][3]=fact*(maxage-p->age);
  }
}

static void drawfire(void)
{
  static int count=0;
	int	j;
  float fr;

  glClearColor(1.0,1.0,1.0,1.0);
  glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);

	glPushMatrix();
	gluLookAt(1.0,2.5,2.0,
		0.0,0.0,0.0,
		0.0,1.0,0.0);

	if(autorot)
			yrot=(float)count;

  glRotatef(xrot,1.0,0.0,0.0);
  glRotatef(yrot,0.0,1.0,0.0);

	glEnable(GL_TEXTURE_2D);
  glColor4f(1.0,1.0,1.0,1.0);
  glBegin(GL_QUADS);
	glTexCoord2fv(qt[0]);
  glVertex3fv(q[0]);
	glTexCoord2fv(qt[1]);
  glVertex3fv(q[1]);
	glTexCoord2fv(qt[2]);
  glVertex3fv(q[2]);
	glTexCoord2fv(qt[3]);
  glVertex3fv(q[3]);
	glEnd();
  glDisable(GL_TEXTURE_2D);

  if(shadows) {
	  glShadeModel(GL_FLAT);

		glBegin(GL_TRIANGLES);
    for(j=0;j<np;j++) {
			glColor4f(black[0],black[1],black[2],p[j].c[0][3]);
			glVertex3f(p[j].p[0][0],0.1,p[j].p[0][2]);

			glColor4f(black[0],black[1],black[2],p[j].c[1][3]);
			glVertex3f(p[j].p[1][0],0.1,p[j].p[1][2]);

			glColor4f(black[0],black[1],black[2],p[j].c[2][3]);
			glVertex3f(p[j].p[2][0],0.1,p[j].p[2][2]);
		}
		glEnd();

	  glShadeModel(GL_SMOOTH);
	}

	glBegin(GL_TRIANGLES);
  for(j=0;j<np;j++) {
		glColor4fv(p[j].c[0]);
		glVertex3fv(p[j].p[0]);

		glColor4fv(p[j].c[1]);
		glVertex3fv(p[j].p[1]);

		glColor4fv(p[j].c[2]);
		glVertex3fv(p[j].p[2]);

    setpart(&p[j]);
  }
	glEnd();

  glPopMatrix();

	tkSwapBuffers();

  if((count % FRAME)==0) {
    fr=gettime();
    printf("Frame rate: %f\n",FRAME/fr);
  }

	count++;
}

static GLenum key(int key, GLenum mask)
{
	switch (key) {
	case TK_ESCAPE:
		tkQuit();
		break;
	case TK_LEFT:
		yrot-=1.5;
	break;
	case TK_RIGHT:
		yrot+=1.5;
	break;
	case TK_DOWN:
		xrot-=1.5;
	break;
	case TK_UP:
		xrot+=1.5;
	break;

	case TK_a:
		autorot= !autorot;
		break;
	case TK_s:
		shadows= !shadows;
		break;
	case TK_r:
		eject_r-=0.03;
		break;
	case TK_R:
		eject_r+=0.03;
		break;
	case TK_t:
		ridtri+=0.005;
		break;
	case TK_T:
		ridtri-=0.005;
		break;
	default:
		return GL_FALSE;
  }

  return GL_TRUE;
}

static void reshape(int width, int height)
{
  glViewport(0,0,(GLint)width,(GLint)height);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(70.0,width/(float)height,0.5,10.0);

  glMatrixMode(GL_MODELVIEW);
}

int main(int ac,char **av)
{
	TK_RGBImageRec *img;
  GLenum gluerr;
  int xs,ys,i;

  printf("Fire Demo V1.0\n");

  /* Default settings */

  xs=640;
  ys=480;
	np=800;
  eject_r=0.1;
  dt=0.015;
  eject_vy=4;
  eject_vl=1;
  shadows=1;
	ridtri=0.1;

  maxage=1.0/dt;

	if(ac==2)
		np=atoi(av[1]);

	if(ac==4) {
		xs=atoi(av[2]);
		ys=atoi(av[3]);
	}

	tkInitPosition(0,0,xs,ys);
  tkInitDisplayMode(TK_RGB|TK_DIRECT|TK_SINGLE|TK_DEPTH);

  if (tkInitWindow("Fire") == GL_FALSE) {
		printf("Error opening a window.\n");

    tkQuit();
  }

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(70.0,xs/(float)ys,0.5,10.0);

  glMatrixMode(GL_MODELVIEW);

	if(!(img=tkRGBImageLoad("s128.rgb"))) {
		fprintf(stderr,"Error reading the texture.\n");
		tkQuit();
	}

  glPixelStorei(GL_UNPACK_ALIGNMENT,1);
  if((gluerr=gluBuild2DMipmaps(GL_TEXTURE_2D, 3, img->sizeX, img->sizeY, GL_RGB,
	  GL_UNSIGNED_BYTE, (GLvoid *)(img->data)))) {
	  fprintf(stderr,"GLULib%s\n",gluErrorString(gluerr));
		tkQuit();
  }

  glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_REPEAT);
  glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,GL_REPEAT);

	glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_NEAREST_MIPMAP_LINEAR);
  glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);

  glTexEnvf(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_DECAL);

  glShadeModel(GL_SMOOTH);
  glEnable(GL_DEPTH_TEST);

	glEnable(GL_BLEND);
	glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);

#ifdef FX
  grFogMode(GR_FOG_WITH_TABLE);
  grFogColorValue(0xFFFFFFFF);
  guFogGenerateExp(fogtable,0.000000000001);
  grFogTable(fogtable);
#else
/*	glEnable(GL_FOG); TO DO
	glFogi(GL_FOG_MODE,GL_EXP);
	glFogfv(GL_FOG_COLOR,fogcolor);
	glFogf(GL_FOG_DENSITY,0.35);*/
#endif

  p=malloc(sizeof(part)*np);

  for(i=0;i<np;i++)
    setnewpart(&p[i]);

  tkKeyDownFunc(key);
  tkIdleFunc(drawfire);
  tkExec();

  tkQuit();

  return(0);
}
