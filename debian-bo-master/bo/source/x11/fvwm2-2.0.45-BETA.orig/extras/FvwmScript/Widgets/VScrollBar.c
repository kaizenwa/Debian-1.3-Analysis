#include "Widget.h"



/***********************************************/
/* Fonction pour VScrollBar                    */
/***********************************************/
void DrawThumbV(struct XObj *xobj)
{
 int x,y,w,h;
 XSegment segm;
 char str[20];

 x=xobj->width/2-9;
 y=3+(xobj->height-36)*(xobj->value-xobj->value3)/(xobj->value2-xobj->value3);
 w=18;
 h=30;
 DrawReliefRect(x,y,w,h,xobj,xobj->TabColor[li].pixel,xobj->TabColor[shad].pixel,
 		xobj->TabColor[black].pixel,-1);
 segm.x1=x+3;
 segm.y1=y+15;
 segm.x2=x+w-3;
 segm.y2=y+15;
 XSetForeground(xobj->display,xobj->gc,xobj->TabColor[shad].pixel);
 XDrawSegments(xobj->display,xobj->ObjWin,xobj->gc,&segm,1);
 segm.x1=x+3;
 segm.y1=y+16;
 segm.x2=x+w-3;
 segm.y2=y+16;
 XSetForeground(xobj->display,xobj->gc,xobj->TabColor[li].pixel);
 XDrawSegments(xobj->display,xobj->ObjWin,xobj->gc,&segm,1);
 XSetForeground(xobj->display,xobj->gc,xobj->TabColor[fore].pixel);
 
 sprintf(str,"%d",xobj->value);
 x=x-XTextWidth(xobj->xfont,str,strlen(str))-6;
 y=y+13+xobj->xfont->max_bounds.ascent/2;
 DrawReliefString(xobj->display,xobj->ObjWin,xobj->gc,x,y,str,
	strlen(str),xobj->TabColor[fore].pixel,xobj->TabColor[li].pixel);

}

void HideThumbV(struct XObj *xobj)
{
 int x,y,w,h;
 char str[20];

 x=xobj->width/2-8;
 y=4+(xobj->height-36)*(xobj->value-xobj->value3)/(xobj->value2-xobj->value3);
 XSetForeground(xobj->display,xobj->gc,xobj->TabColor[back].pixel);
 XFillRectangle(xobj->display,xobj->ObjWin,xobj->gc,x,y,16,28);
 sprintf(str,"%d",xobj->value);
 x=x-XTextWidth(xobj->xfont,str,strlen(str))-7;
 XFillRectangle(xobj->display,xobj->ObjWin,xobj->gc,x,y,XTextWidth(xobj->xfont,
 	str,strlen(str)),xobj->xfont->max_bounds.ascent+xobj->xfont->max_bounds.descent+8);
}

void InitVScrollBar(struct XObj *xobj)
{
 unsigned long mask;
 XSetWindowAttributes Attr;
 unsigned long White,Black;
 XColor TempColor;
 int i,j;
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
  
 i=(xobj->xfont->max_bounds.ascent+xobj->xfont->max_bounds.descent)*2+30;
 if (xobj->height<i)
  xobj->height=i;
 sprintf(str,"%d",xobj->value2);
 i=XTextWidth(xobj->xfont,str,strlen(str));
 sprintf(str,"%d",xobj->value3);
 j=XTextWidth(xobj->xfont,str,strlen(str));
 if (i<j)
  i=j*2+30;
 else
  i=i*2+30;
 xobj->width=i;
 XResizeWindow(xobj->display,xobj->ObjWin,xobj->width,xobj->height);

}

void DestroyVScrollBar(struct XObj *xobj)
{
 XFreeFont(xobj->display,xobj->xfont);
 XFreeGC(xobj->display,xobj->gc);
 XDestroyWindow(xobj->display,xobj->ObjWin);
}

void DrawVScrollBar(struct XObj *xobj)
{
 int x,y,w,h;
 char str[20];

 /* Calcul de la taille de l'ascenseur */
 x=xobj->width/2-12;
 y=0;
 w=24;
 h=xobj->height;
 DrawReliefRect(x,y,w,h,xobj,xobj->TabColor[shad].pixel,xobj->TabColor[li].pixel,
 		xobj->TabColor[black].pixel,1);
 DrawThumbV(xobj);
 
 /* Ecriture des valeurs */
 sprintf(str,"%d",xobj->value3);
 x=x+26;
 y=xobj->xfont->max_bounds.ascent+2;
 DrawReliefString(xobj->display,xobj->ObjWin,xobj->gc,x,y,str,
	strlen(str),xobj->TabColor[fore].pixel,xobj->TabColor[li].pixel);
 sprintf(str,"%d",xobj->value2);
 y=h-xobj->xfont->max_bounds.descent-2;
 DrawReliefString(xobj->display,xobj->ObjWin,xobj->gc,x,y,str,
	strlen(str),xobj->TabColor[fore].pixel,xobj->TabColor[li].pixel);
}

void EvtMouseVScrollBar(struct XObj *xobj,XButtonEvent *EvtButton)
{
 static XEvent event;
 int x,y,w,h;
 int oldy=0;
 int oldvalue=-1;
 int newvalue;
 int x1,y1,x2,y2;
 Window Win1,Win2;
 unsigned int modif;

 do
 {
  /* On suit les mouvements de la souris */
  XQueryPointer(xobj->display,*xobj->ParentWin,&Win1,&Win2,&x1,&y1,&x2,&y2,&modif);
  y2=y2-xobj->y;
  if (y2<15) y2=15;
  if (y2>xobj->height-21) y2=xobj->height-21;
  if (oldy!=y2)
  {
   oldy=y2;
   /* calcule de xobj->value */
   newvalue=(y2-15)*xobj->height/(xobj->height-36)*(xobj->value2-xobj->value3)/
   		(xobj->height)+xobj->value3;
   if (newvalue!=oldvalue)
   {
    HideThumbV(xobj);
    xobj->value=newvalue;
    DrawThumbV(xobj);
    oldvalue=newvalue;
    SendMsg(xobj,ModifValue);
   }
  } 
 }
 while (!XCheckTypedEvent(xobj->display,ButtonRelease,&event));
}

void EvtKeyVScrollBar(struct XObj *xobj,XKeyEvent *EvtKey)
{
 fprintf(stderr,"Evt key\n");
}

void ProcessMsgVScrollBar(struct XObj *xobj,unsigned long type,unsigned long *body)
{
}


