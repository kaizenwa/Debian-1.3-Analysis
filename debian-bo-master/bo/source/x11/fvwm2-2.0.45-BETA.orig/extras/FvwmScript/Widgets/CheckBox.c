#include "Widget.h"


/***********************************************/
/* Fonction pour CheckBox                      */
/***********************************************/
void InitCheckBox(struct XObj *xobj)
{
 unsigned long mask;
 XSetWindowAttributes Attr;
 unsigned long White,Black;
 XColor TempColor;

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
 
 /* Redimensionnement du widget */
 xobj->height=xobj->xfont->max_bounds.ascent+xobj->xfont->max_bounds.descent+5;
 xobj->width=XTextWidth(xobj->xfont,xobj->title,strlen(xobj->title))+30;
 XResizeWindow(xobj->display,xobj->ObjWin,xobj->width,xobj->height);
}

void DestroyCheckBox(struct XObj *xobj)
{
 XFreeFont(xobj->display,xobj->xfont);
 XFreeGC(xobj->display,xobj->gc);
 XDestroyWindow(xobj->display,xobj->ObjWin);
}

void DrawCheckBox(struct XObj *xobj)
{
 XSegment segm[2];

 /* Dessin du rectangle arrondi */
 DrawReliefRect(0,0+xobj->xfont->max_bounds.ascent-12,15,15,xobj,xobj->TabColor[li].pixel,
 		xobj->TabColor[shad].pixel,xobj->TabColor[black].pixel,0);

 /* Calcul de la position de la chaine de charactere */
 DrawReliefString(xobj->display,xobj->ObjWin,xobj->gc,23,xobj->xfont->max_bounds.ascent,xobj->title,
	strlen(xobj->title),xobj->TabColor[fore].pixel,xobj->TabColor[li].pixel);
 /* Dessin de la croix */
 if (xobj->value)
 {
  XSetLineAttributes(xobj->display,xobj->gc,2,LineSolid,CapProjecting,JoinMiter);
  segm[0].x1=5;
  segm[0].y1=5+xobj->xfont->max_bounds.ascent-12;
  segm[0].x2=9;
  segm[0].y2=9+xobj->xfont->max_bounds.ascent-12;
  segm[1].x1=5;
  segm[1].y1=9+xobj->xfont->max_bounds.ascent-12;
  segm[1].x2=9;
  segm[1].y2=5+xobj->xfont->max_bounds.ascent-12;
  XSetForeground(xobj->display,xobj->gc,xobj->TabColor[black].pixel);
  XDrawSegments(xobj->display,xobj->ObjWin,xobj->gc,segm,2);
  XSetLineAttributes(xobj->display,xobj->gc,1,LineSolid,CapRound,JoinMiter);
 }
}

void EvtMouseCheckBox(struct XObj *xobj,XButtonEvent *EvtButton)
{
 static XEvent event;
 int End=1;
 unsigned int modif;
 int x1,x2,y1,y2,i,j;
 Window Win1,Win2;
 Window WinBut=0;
 int In;
 char **Buff;
 XSegment segm[2];

 while (End)
 {
  XNextEvent(xobj->display, &event);
  switch (event.type)
    {
      case EnterNotify:
	   XQueryPointer(xobj->display,*xobj->ParentWin,
		&Win1,&Win2,&x1,&y1,&x2,&y2,&modif);
	   if (WinBut==0)
	   {
	    WinBut=Win2;
	    /* Mouse on button */
            DrawReliefRect(0,xobj->xfont->max_bounds.ascent-12,15,15,xobj,xobj->TabColor[shad].pixel,
            		xobj->TabColor[li].pixel,xobj->TabColor[black].pixel,0);
	    In=1;
	   }
	   else
	   {
	    if (Win2==WinBut)
	    {
	    /* Mouse on button */
             DrawReliefRect(0,xobj->xfont->max_bounds.ascent-12,15,15,xobj,xobj->TabColor[shad].pixel,
             		xobj->TabColor[li].pixel,xobj->TabColor[black].pixel,0);
	     In=1;
	    }
	    else if (In)
	    {
	     In=0;
	     /* Mouse not on button */
             DrawReliefRect(0,xobj->xfont->max_bounds.ascent-12,15,15,xobj,xobj->TabColor[li].pixel,
             		xobj->TabColor[shad].pixel,xobj->TabColor[black].pixel,0);
	    }
	   }
	  break;
      case LeaveNotify:
	   XQueryPointer(xobj->display,*xobj->ParentWin,
		&Win1,&Win2,&x1,&y1,&x2,&y2,&modif);
	   if (Win2==WinBut)
	   {
	    In=1;
	    /* Mouse on button */
            DrawReliefRect(0,xobj->xfont->max_bounds.ascent-12,15,15,xobj,xobj->TabColor[shad].pixel,
            	xobj->TabColor[li].pixel,xobj->TabColor[black].pixel,0);
	   }
	   else if (In)
	   {
	    /* Mouse not on button */
            DrawReliefRect(0,xobj->xfont->max_bounds.ascent-12,15,15,xobj,xobj->TabColor[li].pixel,
            	xobj->TabColor[shad].pixel,xobj->TabColor[black].pixel,0);
	    In=0;
	   }
	  break;
      case ButtonRelease:
	   End=0;
	   /* Mouse not on button */
	   if (In)
	   {
	    /* Envoie d'un message vide de type ModifValue pour un clique souris */
	    xobj->value=!xobj->value;
            DrawReliefRect(0,xobj->xfont->max_bounds.ascent-12,15,15,xobj,xobj->TabColor[li].pixel,
            	xobj->TabColor[shad].pixel,xobj->TabColor[black].pixel,0);
	    SendMsg(xobj,ModifValue);
	   }
	   if (xobj->value)
	   {
  	    XSetLineAttributes(xobj->display,xobj->gc,2,LineSolid,CapProjecting,JoinMiter);
	    segm[0].x1=5;
	    segm[0].y1=5+xobj->xfont->max_bounds.ascent-12;
	    segm[0].x2=9;
	    segm[0].y2=9+xobj->xfont->max_bounds.ascent-12;
	    segm[1].x1=5;
	    segm[1].y1=9+xobj->xfont->max_bounds.ascent-12;
	    segm[1].x2=9;
	    segm[1].y2=5+xobj->xfont->max_bounds.ascent-12;
	    XSetForeground(xobj->display,xobj->gc,xobj->TabColor[black].pixel);
	    XDrawSegments(xobj->display,xobj->ObjWin,xobj->gc,segm,2);
	    XSetLineAttributes(xobj->display,xobj->gc,1,LineSolid,CapRound,JoinMiter);
	   }
	   else
	   {
	    XSetForeground(xobj->display,xobj->gc,xobj->TabColor[back].pixel);
	    XFillRectangle(xobj->display,xobj->ObjWin,xobj->gc,3,xobj->xfont->max_bounds.ascent-9,9,9);
	   }
	  break;
     }
 }
}

void EvtKeyCheckBox(struct XObj *xobj,XKeyEvent *EvtKey)
{
}


void ProcessMsgCheckBox(struct XObj *xobj,unsigned long type,unsigned long *body)
{
}




