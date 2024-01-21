#include "Widget.h"


/***********************************************/
/* Fonction pour ItemDraw                      */
/***********************************************/
void InitItemDraw(struct XObj *xobj)
{
 unsigned long mask;
 XSetWindowAttributes Attr;
 unsigned long White,Black;
 XColor TempColor;
 int h,w;

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
 
 /* Redimensionnement du widget */
 if (xobj->icon==NULL)
 {
  if (strlen(xobj->title)!=0)
  {
   xobj->height=xobj->xfont->max_bounds.ascent+xobj->xfont->max_bounds.descent+2;
   xobj->width=XTextWidth(xobj->xfont,xobj->title,strlen(xobj->title))+2;
  }
 }
 else if (strlen(xobj->title)==0)
 {
  if (xobj->height<xobj->icon_h)
   xobj->height=xobj->icon_h;
  if (xobj->width<xobj->icon_w)
   xobj->width=xobj->icon_w;
 }
 else
 {
  if (xobj->icon_w>XTextWidth(xobj->xfont,xobj->title,strlen(xobj->title))+2)
  {
   if (xobj->width<xobj->icon_w)
    xobj->width=xobj->icon_w;
  }
  else
   xobj->width=XTextWidth(xobj->xfont,xobj->title,strlen(xobj->title))+2;
  xobj->height=xobj->icon_h+2*(xobj->xfont->max_bounds.ascent+xobj->xfont->max_bounds.descent+15);
 }
 XResizeWindow(xobj->display,xobj->ObjWin,xobj->width,xobj->height);
}

void DestroyItemDraw(struct XObj *xobj)
{
 XFreeFont(xobj->display,xobj->xfont);
 XFreeGC(xobj->display,xobj->gc);
 XDestroyWindow(xobj->display,xobj->ObjWin);
}

void DrawItemDraw(struct XObj *xobj)
{
 int i,j;

 /* Calcul de la position de la chaine de charactere */
 XSetForeground(xobj->display,xobj->gc,xobj->TabColor[back].pixel);
 XFillRectangle(xobj->display,xobj->ObjWin,xobj->gc,0,0,xobj->width,xobj->height);
 DrawIconStr(0,xobj,False);
}

void EvtMouseItemDraw(struct XObj *xobj,XButtonEvent *EvtButton)
{
}

void EvtKeyItemDraw(struct XObj *xobj,XKeyEvent *EvtKey)
{
}

void ProcessMsgItemDraw(struct XObj *xobj,unsigned long type,unsigned long *body)
{
}








