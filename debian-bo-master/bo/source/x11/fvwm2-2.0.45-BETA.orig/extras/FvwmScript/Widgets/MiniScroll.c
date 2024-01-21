#include "Widget.h"


/***********************************************/
/* Fonction pour MiniScroll                      */
/***********************************************/
void InitMiniScroll(struct XObj *xobj)
{
 unsigned long mask;
 XSetWindowAttributes Attr;
 unsigned long White,Black;
 XColor TempColor;
 int i;

 /* Enregistrement des couleurs et de la police */
 if (!XAllocNamedColor(xobj->display,*xobj->colormap,
	xobj->forecolor,&xobj->TabColor[fore],&TempColor)) 
   fprintf(stderr,"Can't alloc named color.\n");
 if (!XAllocNamedColor(xobj->display,*xobj->colormap,
	xobj->backcolor,&xobj->TabColor[back],&TempColor))
   fprintf(stderr,"Can't alloc named color.\n");
 if (!XAllocNamedColor(xobj->display,*xobj->colormap,
	xobj->licolor,&xobj->TabColor[li],&TempColor)) 
   fprintf(stderr,"Can't alloc named color.\n");
 if (!XAllocNamedColor(xobj->display,*xobj->colormap,
	xobj->shadcolor,&xobj->TabColor[shad],&TempColor))
   fprintf(stderr,"Can't alloc named color.\n");
 if (!XAllocNamedColor(xobj->display,*xobj->colormap,
	"black",&xobj->TabColor[black],&TempColor)) 
   fprintf(stderr,"Can't alloc named color.\n");
 if (!XAllocNamedColor(xobj->display,*xobj->colormap,
	"white",&xobj->TabColor[white],&TempColor))
   fprintf(stderr,"Can't alloc named color.\n");

 mask=0;
 Attr.background_pixel=xobj->TabColor[back].pixel;
 mask|=CWBackPixel;

 /* La taille du widget est fixe */
 xobj->width=20;
 xobj->height=30;
 xobj->ObjWin=XCreateWindow(xobj->display,*xobj->ParentWin,
		xobj->x,xobj->y,xobj->width,xobj->height,1,
		CopyFromParent,InputOutput,CopyFromParent,
		mask,&Attr);
 xobj->gc=XCreateGC(xobj->display,xobj->ObjWin,0,NULL);
 XSetForeground(xobj->display,xobj->gc,xobj->TabColor[fore].pixel);
 XSetBackground(xobj->display,xobj->gc,xobj->TabColor[back].pixel);
 if ((xobj->xfont=XLoadQueryFont(xobj->display,xobj->font))==NULL)
   fprintf(stderr,"Can't load font %s\n",xobj->font);
 else
  XSetFont(xobj->display,xobj->gc,xobj->xfont->fid);
 XSetLineAttributes(xobj->display,xobj->gc,1,LineSolid,CapRound,JoinMiter);
 if (xobj->value2>xobj->value3)
 {
  i=xobj->value2;
  xobj->value2=xobj->value3;
  xobj->value3=i;
 }
 if ((xobj->value<xobj->value2)||(xobj->value>xobj->value3))
  xobj->value=xobj->value2;
}

void DrawArrows(struct XObj *xobj,int Press)
{
 XSegment segm[4];

 segm[0].x1=9;
 segm[0].y1=4;
 segm[0].x2=3;
 segm[0].y2=10;
 segm[1].x1=10;
 segm[1].y1=4;
 segm[1].x2=4;
 segm[1].y2=10;
 segm[2].x1=10;
 segm[2].y1=4;
 segm[2].x2=16;
 segm[2].y2=10; 
 segm[3].x1=11;
 segm[3].y1=4;
 segm[3].x2=16;
 segm[3].y2=9;
 if (Press==1)
  XSetForeground(xobj->display,xobj->gc,xobj->TabColor[shad].pixel);
 else if (Press==-1)
  XSetForeground(xobj->display,xobj->gc,xobj->TabColor[li].pixel);
 else
  XSetForeground(xobj->display,xobj->gc,xobj->TabColor[li].pixel);
 XDrawSegments(xobj->display,xobj->ObjWin,xobj->gc,segm,4);
 
 segm[0].x1=5;
 segm[0].y1=10;
 segm[0].x2=15;
 segm[0].y2=10;
 segm[1].x1=6;
 segm[1].y1=9;
 segm[1].x2=14;
 segm[1].y2=9;
 if (Press==1)
  XSetForeground(xobj->display,xobj->gc,xobj->TabColor[li].pixel);
 else if (Press==-1)
  XSetForeground(xobj->display,xobj->gc,xobj->TabColor[shad].pixel);
 else
  XSetForeground(xobj->display,xobj->gc,xobj->TabColor[shad].pixel);
 XDrawSegments(xobj->display,xobj->ObjWin,xobj->gc,segm,2);


  /* Dessin de la fleche du bas */
 segm[0].x1=4;
 segm[0].y1=20;
 segm[0].x2=16;
 segm[0].y2=20;
 segm[1].x1=5;
 segm[1].y1=20;
 segm[1].x2=10;
 segm[1].y2=25;
 segm[2].x1=3;
 segm[2].y1=19;
 segm[2].x2=16;
 segm[2].y2=19;
 segm[3].x1=4;
 segm[3].y1=20;
 segm[3].x2=9;
 segm[3].y2=25;
 if (Press==1)
  XSetForeground(xobj->display,xobj->gc,xobj->TabColor[li].pixel);
 else if (Press==-1)
  XSetForeground(xobj->display,xobj->gc,xobj->TabColor[shad].pixel);
 else
  XSetForeground(xobj->display,xobj->gc,xobj->TabColor[li].pixel);
 XDrawSegments(xobj->display,xobj->ObjWin,xobj->gc,segm,4);
 
 segm[0].x1=14;
 segm[0].y1=21;
 segm[0].x2=10;
 segm[0].y2=25;
 segm[1].x1=16;
 segm[1].y1=20;
 segm[1].x2=11;
 segm[1].y2=25;
 if (Press==1)
  XSetForeground(xobj->display,xobj->gc,xobj->TabColor[shad].pixel);
 else if (Press==-1)
  XSetForeground(xobj->display,xobj->gc,xobj->TabColor[li].pixel);
 else
  XSetForeground(xobj->display,xobj->gc,xobj->TabColor[shad].pixel);
 XDrawSegments(xobj->display,xobj->ObjWin,xobj->gc,segm,2);
}

void DestroyMiniScroll(struct XObj *xobj)
{
 XFreeFont(xobj->display,xobj->xfont);
 XFreeGC(xobj->display,xobj->gc);
 XDestroyWindow(xobj->display,xobj->ObjWin);
}

void DrawMiniScroll(struct XObj *xobj)
{
 int i,j;

 DrawReliefRect(-1,-1,xobj->width+2,xobj->height+2,xobj,xobj->TabColor[li].pixel,
 		xobj->TabColor[shad].pixel,xobj->TabColor[black].pixel,-1);

 /* Dessin de la fleche du haut */
 DrawArrows(xobj,0);
}

void EvtMouseMiniScroll(struct XObj *xobj,XButtonEvent *EvtButton)
{
 static XEvent event;
 int x,y,w,h;
 int oldx=0;
 int oldvalue=-1;
 int newvalue;
 int x1,y1,x2,y2;
 Window Win1,Win2;
 unsigned int modif;
 int Pos=0;
 struct timeval *tv;
 long tus,ts;
 
 do
 {
  XQueryPointer(xobj->display,*xobj->ParentWin,&Win1,&Win2,&x1,&y1,&x2,&y2,&modif);
  /* Determiner l'option courante */
  y2=y2-xobj->y;
  x2=x2-xobj->x;
  if ((x2>0)&&(x2<xobj->width)&&(y2>0)&&(y2<xobj->height/2))
  {
   if (Pos==1)
   {
    tv=(struct timeval*)calloc(1,sizeof(struct timeval));
    gettimeofday(tv,NULL);
    tus=tv->tv_usec;
    ts=tv->tv_sec;
    while (((tv->tv_usec-tus)+(tv->tv_sec-ts)*1000000)<16667*8)
     gettimeofday(tv,NULL);
    free(tv);
   }
   else
   {
    DrawArrows(xobj,1);
    Pos=1;
   }
   xobj->value++;
   if (xobj->value>xobj->value3)
    xobj->value=xobj->value2;
   SendMsg(xobj,ModifValue);
   
  }
  else if ((x2>0)&&(x2<xobj->width)&&(y2>xobj->height/2)&&(y2<xobj->height))
  {
   if (Pos==-1)
   {
    tv=(struct timeval*)calloc(1,sizeof(struct timeval));
    gettimeofday(tv,NULL);
    tus=tv->tv_usec;
    ts=tv->tv_sec;
    while (((tv->tv_usec-tus)+(tv->tv_sec-ts)*1000000)<16667*8)
     gettimeofday(tv,NULL);
    free(tv);
   }
   else
   {
    DrawArrows(xobj,-1);
    Pos=-1;
   }
   xobj->value--;
   if (xobj->value<xobj->value2)
    xobj->value=xobj->value3;
   SendMsg(xobj,ModifValue);
  }
  else if (Pos!=0)
  {
   Pos=0;
   DrawArrows(xobj,0);
  }
 }
 while (!XCheckTypedEvent(xobj->display,ButtonRelease,&event));
 DrawArrows(xobj,0);
}

void EvtKeyMiniScroll(struct XObj *xobj,XKeyEvent *EvtKey)
{
}

void ProcessMsgMiniScroll(struct XObj *xobj,unsigned long type,unsigned long *body)
{
}







