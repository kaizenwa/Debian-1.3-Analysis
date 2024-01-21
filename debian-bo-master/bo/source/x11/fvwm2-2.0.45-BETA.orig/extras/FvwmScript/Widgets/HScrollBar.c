#include "Widget.h"



/***********************************************/
/* Fonction pour HScrollBar                    */
/***********************************************/
void DrawThumbH(struct XObj *xobj)
{
 int x,y,w,h;
 XSegment segm;
 char str[20];

 x=3+(xobj->width-36)*(xobj->value-xobj->value2)/(xobj->value3-xobj->value2);
 y=xobj->height/2-9;
 w=30;
 h=18;
 DrawReliefRect(x,y,w,h,xobj,xobj->TabColor[li].pixel,xobj->TabColor[shad].pixel,
 		xobj->TabColor[black].pixel,-1);
 segm.x1=x+15;
 segm.y1=y+3;
 segm.x2=x+15;
 segm.y2=y+h-3;
 XSetForeground(xobj->display,xobj->gc,xobj->TabColor[shad].pixel);
 XDrawSegments(xobj->display,xobj->ObjWin,xobj->gc,&segm,1);
 segm.x1=x+16;
 segm.y1=y+3;
 segm.x2=x+16;
 segm.y2=y+h-3;
 XSetForeground(xobj->display,xobj->gc,xobj->TabColor[li].pixel);
 XDrawSegments(xobj->display,xobj->ObjWin,xobj->gc,&segm,1);
 XSetForeground(xobj->display,xobj->gc,xobj->TabColor[fore].pixel);
 
 sprintf(str,"%d",xobj->value);
 x=x+15-(XTextWidth(xobj->xfont,str,strlen(str))/2);
 y=y-xobj->xfont->max_bounds.descent-4;
 DrawReliefString(xobj->display,xobj->ObjWin,xobj->gc,x,y,str,
	strlen(str),xobj->TabColor[fore].pixel,xobj->TabColor[li].pixel);

}

void HideThumbH(struct XObj *xobj)
{
 int x,y,w,h;

 x=4+(xobj->width-36)*(xobj->value-xobj->value2)/(xobj->value3-xobj->value2);
 y=xobj->height/2-8;
 XSetForeground(xobj->display,xobj->gc,xobj->TabColor[back].pixel);
 XFillRectangle(xobj->display,xobj->ObjWin,xobj->gc,x,y,28,16);
 XFillRectangle(xobj->display,xobj->ObjWin,xobj->gc,x-xobj->xfont->max_bounds.ascent
 			-xobj->xfont->max_bounds.descent,y-xobj->xfont->max_bounds.ascent-10,90,
 			xobj->xfont->max_bounds.ascent+xobj->xfont->max_bounds.descent+2);
}

void InitHScrollBar(struct XObj *xobj)
{
 unsigned long mask;
 XSetWindowAttributes Attr;
 unsigned long White,Black;
 XColor TempColor;
 int i;
 char str[20];

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
 Attr.cursor=XCreateFontCursor(xobj->display,XC_hand2); 
 mask|=CWCursor;		/* Curseur pour la fenetre */

 xobj->ObjWin=XCreateWindow(xobj->display,*xobj->ParentWin,
		xobj->x,xobj->y,xobj->width,xobj->height,0,
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
 
 if ((xobj->value3-xobj->value2)<=0)
  xobj->value3=xobj->value2+10;
 if (!((xobj->value>=xobj->value2)&&(xobj->value<=xobj->value3)))
  xobj->value=xobj->value2;
 xobj->height=(xobj->xfont->max_bounds.ascent+xobj->xfont->max_bounds.descent)*2+30;
 sprintf(str,"%d",xobj->value2);
 i=XTextWidth(xobj->xfont,str,strlen(str));
 sprintf(str,"%d",xobj->value3);
 i=XTextWidth(xobj->xfont,str,strlen(str))+i+20;
 if (xobj->width<i)
  xobj->width=i;
 XResizeWindow(xobj->display,xobj->ObjWin,xobj->width,xobj->height);
}

void DestroyHScrollBar(struct XObj *xobj)
{
 XFreeFont(xobj->display,xobj->xfont);
 XFreeGC(xobj->display,xobj->gc);
 XDestroyWindow(xobj->display,xobj->ObjWin);
}

void DrawHScrollBar(struct XObj *xobj)
{
 int x,y,w,h;
 char str[20];

 /* Calcul de la taille de l'ascenseur */
 x=0;
 y=xobj->height/2-12;
 w=xobj->width;
 h=24;
 DrawReliefRect(x,y,w,h,xobj,xobj->TabColor[shad].pixel,xobj->TabColor[li].pixel,
 		xobj->TabColor[black].pixel,1);
 DrawThumbH(xobj);
 
 /* Ecriture des valeurs */
 sprintf(str,"%d",xobj->value2);
 x=4;
 y=y+xobj->xfont->max_bounds.ascent+h;
 DrawReliefString(xobj->display,xobj->ObjWin,xobj->gc,x,y,str,
	strlen(str),xobj->TabColor[fore].pixel,xobj->TabColor[li].pixel);
 sprintf(str,"%d",xobj->value3);
 x=w-XTextWidth(xobj->xfont,str,strlen(str))-4;
 DrawReliefString(xobj->display,xobj->ObjWin,xobj->gc,x,y,str,
	strlen(str),xobj->TabColor[fore].pixel,xobj->TabColor[li].pixel);
}

void EvtMouseHScrollBar(struct XObj *xobj,XButtonEvent *EvtButton)
{
 static XEvent event;
 int x,y,w,h;
 int oldx=0;
 int oldvalue=-1;
 int newvalue;
 int x1,y1,x2,y2;
 Window Win1,Win2;
 unsigned int modif;

 x=3+((xobj->width-36)*xobj->value)/(xobj->value3-xobj->value2);
 y=xobj->height/2-9;
 w=30;
 h=18;

 
 do
 {
  /* On suit les mouvements de la souris */
  XQueryPointer(xobj->display,*xobj->ParentWin,&Win1,&Win2,&x1,&y1,&x2,&y2,&modif);
  x2=x2-xobj->x;
  if (x2<15) x2=15;
  if (x2>xobj->width-21) x2=xobj->width-21;
  if (oldx!=x2)
  {
   oldx=x2;
   /* calcule de xobj->value */
   newvalue=(x2-15)*xobj->width/(xobj->width-36)*(xobj->value3-xobj->value2)/(xobj->width)+xobj->value2;
   if (newvalue!=oldvalue)
   {
    HideThumbH(xobj);
    xobj->value=newvalue;
    DrawThumbH(xobj);
    oldvalue=newvalue;
    SendMsg(xobj,ModifValue);
   }
  } 
 }
 while (!XCheckTypedEvent(xobj->display,ButtonRelease,&event));
}

void EvtKeyHScrollBar(struct XObj *xobj,XKeyEvent *EvtKey)
{
}

void ProcessMsgHScrollBar(struct XObj *xobj,unsigned long type,unsigned long *body)
{
}













