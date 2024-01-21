#include "mesadraw.h"
#include <3d.h>

extern void perpnorm(GLdouble[3],GLdouble[3],GLdouble[3],GLdouble[3]);
extern unsigned long random();
extern void bcopy(void*,void*,unsigned long);

#define DIM 30
#define MAX_HEIGHT (300/DIM)
#define RADS(x) ((x)/57.29577951)

static GLdouble mesh[DIM][DIM][3];
static GLubyte cols[DIM][DIM][3];

/*
 * Create a random matrix of heights and colours
 */

void make_matrix(void)
{
  int i,j;

  for(i=0;i<DIM;i++)
    for(j=0;j<DIM;j++)
     {
        mesh[i][j][0]=((100.0*i)/(DIM-1.0))-50.0;
        mesh[i][j][2]=((100.0*j)/(DIM-1.0))-50.0;
        mesh[i][j][1]=(random()%MAX_HEIGHT);
       cols[i][j][0]=(mesh[i][j][1]*255)/MAX_HEIGHT;
       cols[i][j][1]=255-cols[i][j][0];
       cols[i][j][2]=0;
     }
}

/*
 * Initialise the OpenGL stuff
 */

void my_init (float w, float h) 
{
  GLfloat light_ambient[] = { 0.0, 0.0, 0.0, 1.0 };
  GLfloat light_diffuse[] = { 1.0, 1.0, 1.0, 1.0 };
  GLfloat light_specular[] = { 1.0, 1.0, 1.0, 1.0 };
  GLfloat light_position[] = { 5.0, 3.0, 9.0, 0.0 };

  glMatrixMode (GL_PROJECTION);
  glLoadIdentity ();
  glMatrixMode (GL_MODELVIEW);
  glLoadIdentity ();

  glShadeModel(GL_SMOOTH);

  glLightfv(GL_LIGHT0, GL_AMBIENT, light_ambient);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, light_diffuse);
  glLightfv(GL_LIGHT0, GL_SPECULAR, light_specular);
  glLightfv(GL_LIGHT0, GL_POSITION, light_position);

  glEnable(GL_LIGHT0);
  glEnable(GL_LIGHTING);

  glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
  glEnable(GL_COLOR_MATERIAL);

  glViewport(0,0,w,h);
  glMatrixMode (GL_PROJECTION);
  gluPerspective (60.0, w/h, 1.0, 400.0);

  glEnable(GL_DEPTH_TEST);
}

/*
 * Set the viewpoint
 */

void set_viewpoint(float zoomDist, float elevAngle, float spinAngle, int white)
{
    float flat_radius, x, y, z, xl, zl;
    flat_radius=zoomDist*cos(RADS(elevAngle));
    x=0.0-(flat_radius*sin(RADS(spinAngle)));
    xl=50.0*sin(RADS(spinAngle));
    z=flat_radius*cos(RADS(spinAngle));
    zl= -50.0*cos(RADS(spinAngle));
    y=zoomDist*sin(RADS(elevAngle));

    if(white)
     glClearColor(1,1,1,1);
    else
     glClearColor(0,0,0,1);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glMatrixMode (GL_MODELVIEW);
    glLoadIdentity ();
/*
 * we make a somewhat spurious "up" vector because we know we are 
 * looking at the origin and that it's mapped into the plane of
 * the screen anyway so the percise direction is irrelevent.
 */
    gluLookAt (x, y, z,
               0, 0, 0,
               xl, 50, zl);
}

/*
 * Get a normal to a corner of a quad.
 */

void get_quad_normal(int x, int z, int cx, int cz, GLdouble n[3])
{
        GLdouble *c1, *c2, *c3, *c4;
        c1 = &mesh[x][z][0];
        c2 = &mesh[x+1][z][0];
        c3 = &mesh[x+1][z+1][0];
        c4 = &mesh[x][z+1][0];
        if((cx==0) && (cz==0))
         return perpnorm(c4,c1,c2,n);
        if((cx==0) && (cz==0))
         return perpnorm(c4,c1,c2,n);
        if((cx==1) && (cz==0))
          return perpnorm(c1,c2,c3,n);
        if((cx==1) && (cz==1))
          return perpnorm(c2,c3,c4,n);
        if((cx==0) && (cz==1))
          return perpnorm(c3,c4,c1,n);
}

/*
 * Get a normal to a plane as the average of
 * the normals at the four corners of the quad.
 */

void get_plane_normal(int x, int z, GLdouble n[3])
{
   GLdouble total[3]={0,0,0};
   get_quad_normal(x,z,0,0,n);
   add3(total,n,total);
   get_quad_normal(x,z,0,1,n);
   add3(total,n,total);
   get_quad_normal(x,z,1,0,n);
   add3(total,n,total);
   get_quad_normal(x,z,1,1,n);
   add3(total,n,total);
   normalize(total);
   n[0]=total[0];
   n[1]=total[1];
   n[2]=total[2];
}

/*
 * Get a normal to a point as the average
 * of the surrounding quads.
 */

void get_vertex_normal(int x, int z, GLdouble n[3])
{
   GLdouble total[3]={0,0,0};
   if((x>0) && (z>0))
    {
     get_quad_normal(x-1,z-1,1,1,n);
     add3(total,n,total);
    }
   if((x>0) && (z<(DIM-1)))
    {
     get_quad_normal(x-1,z,1,0,n);
     add3(total,n,total);
    }
   if((x<(DIM-1)) && (z>0))
    {
     get_quad_normal(x,z-1,0,1,n);
     add3(total,n,total);
    }
   if((x<(DIM-1)) && (z<(DIM-1)))
    {
     get_quad_normal(x,z,0,0,n);
     add3(total,n,total);
    }
   normalize(total);
   n[0]=total[0];
   n[1]=total[1];
   n[2]=total[2];
}

/*
 * Draw the scene
 */

void draw_scene(int av)
{
    int xstrip, zrow;
    for(xstrip=0;xstrip<DIM-1;xstrip++)
     {
      glBegin(GL_QUADS);
      for(zrow=0;zrow<DIM-1;zrow++)
       {
        GLdouble *c1, *c2, *c3, *c4;
        GLdouble n1[3], n2[3], n3[3], n4[3];
        int passes,toggle;
        c1 = &mesh[xstrip][zrow][0];
        c2 = &mesh[xstrip+1][zrow][0];
        c3 = &mesh[xstrip+1][zrow+1][0];
        c4 = &mesh[xstrip][zrow+1][0];
        if(av)
         {
          get_vertex_normal(xstrip,zrow,n1);
          get_vertex_normal(xstrip+1,zrow,n2);
          get_vertex_normal(xstrip+1,zrow+1,n3);
          get_vertex_normal(xstrip,zrow+1,n4);
         }
        else
         {
          get_plane_normal(xstrip,zrow,n1);
          bcopy(n1,n2,3*sizeof(GLdouble));
          bcopy(n1,n3,3*sizeof(GLdouble));
          bcopy(n1,n4,3*sizeof(GLdouble));
         }

        /*
         * we do this to draw the quads in the opposite direction for
         * each strip as this reduces the effects of the triangluarisation
         * of the quad by the rendering algorithm.
         */
        for(passes=0;passes<4;passes++)
         {
         if((xstrip&1) == (zrow&1))
          toggle=3-passes;
         else
          toggle=passes;
         switch (toggle)
          {
          case 0:
           glColor3ubv(&cols[xstrip][zrow][0]);
           glNormal3dv(n1);
           glVertex3dv(c1);
           break;

          case 1:
           glColor3ubv(&cols[xstrip+1][zrow][0]);
           glNormal3dv(n2);
           glVertex3dv(c2);
           break;

          case 2:
           glColor3ubv(&cols[xstrip+1][zrow+1][0]);
           glNormal3dv(n3);
           glVertex3dv(c3);
           break;
   
          case 3:
           glColor3ubv(&cols[xstrip][zrow+1][0]);
           glNormal3dv(n4);
           glVertex3dv(c4);
           break;
          }
         }
       }
      glEnd();
     }
}

/*
 * We use the display postscript operators to send
 * an outline of the base area directly into the window
 * by projecting points and drawing lines. We also draw
 * a small arrow representing "north" to hep orientation.
 */

void outline_scene(void)
{
    GLdouble model[16], proj[16];
    GLint viewport[4];
    GLdouble winx, winy, winz;

    glGetDoublev(GL_MODELVIEW_MATRIX, model);
    glGetDoublev(GL_PROJECTION_MATRIX, proj);
    glGetIntegerv(GL_VIEWPORT, viewport);

    /* Having got these we can draw the base */
    PSnewpath();
    gluProject(-50.0, 0, -50.0, model, proj, viewport, &winx, &winy, &winz);
    PSmoveto(winx,winy);
    gluProject(50.0, 0, -50.0, model, proj, viewport, &winx, &winy, &winz);
    PSlineto(winx,winy);
    gluProject(50.0, 0, 50.0, model, proj, viewport, &winx, &winy, &winz);
    PSlineto(winx,winy);
    gluProject(-50.0, 0, 50.0, model, proj, viewport, &winx, &winy, &winz);
    PSlineto(winx,winy);
    PSclosepath();
    PSstroke();

    /* draw the "north" arrow */
    PSnewpath();
    gluProject(-10.0, 0, 0.0, model, proj, viewport, &winx, &winy, &winz);
    PSmoveto(winx,winy);
    gluProject(10.0, 0, 0.0, model, proj, viewport, &winx, &winy, &winz);
    PSlineto(winx,winy);
    gluProject(0.0, 0, 10.0, model, proj, viewport, &winx, &winy, &winz);
    PSmoveto(winx,winy);
    gluProject(0.0, 0, -15.0, model, proj, viewport, &winx, &winy, &winz);
    PSlineto(winx,winy);
    gluProject(-5.0, 0, -10.0, model, proj, viewport, &winx, &winy, &winz);
    PSmoveto(winx,winy);
    gluProject(0.0, 0, -15.0, model, proj, viewport, &winx, &winy, &winz);
    PSlineto(winx,winy);
    gluProject(5.0, 0, -10.0, model, proj, viewport, &winx, &winy, &winz);
    PSlineto(winx,winy);
    PSstroke();
}
