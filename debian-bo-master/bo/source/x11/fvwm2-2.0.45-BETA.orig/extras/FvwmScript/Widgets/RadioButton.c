#include "Widget.h"



/***********************************************/
/* Fonction pour RadioButton                    */
/***********************************************/
void InitRadioButton(struct XObj *xobj)
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
 xobj->width=XTextWidth(xobj->xfont,xobj->title,strlen(xobj->title))+20;
 XResizeWindow(xobj->display,xobj->ObjWin,xobj->width,xobj->height);

}

void DestroyRadioButton(struct XObj *xobj)
{
 XFreeFont(xobj->display,xobj->xfont);
 XFreeGC(xobj->display,xobj->gc);
 XDestroyWindow(xobj->display,xobj->ObjWin);
}

void DrawRadioButton(struct XObj *xobj)
{
 int i,j;

 j=xobj->height/2+3;
 i=16;
 /* Dessin du cercle arrondi */
 XSetForeground(xobj->display,xobj->gc,xobj->TabColor[shad].pixel);
 XDrawArc(xobj->display,xobj->ObjWin,xobj->gc,1,j-11,11,11,45*64,180*64);
 XSetForeground(xobj->display,xobj->gc,xobj->TabColor[li].pixel);  
 XDrawArc(xobj->display,xobj->ObjWin,xobj->gc,1,j-11,11,11,225*64,180*64); 
 XSetForeground(xobj->display,xobj->gc,xobj->TabColor[white].pixel);  
 XFillArc(xobj->display,xobj->ObjWin,xobj->gc,2,j-10,9,9,0*64,360*64);
 XSetForeground(xobj->display,xobj->gc,xobj->TabColor[black].pixel);  
 XDrawArc(xobj->display,xobj->ObjWin,xobj->gc,2,j-10,9,9,0*64,360*64);
 if (xobj->value)
 {
  XSetForeground(xobj->display,xobj->gc,xobj->TabColor[black].pixel);  
  XFillArc(xobj->display,xobj->ObjWin,xobj->gc,3,j-9,7,7,0*64,360*64);
 }

 /* Calcul de la position de la chaine de charactere */
 DrawReliefString(xobj->display,xobj->ObjWin,xobj->gc,i,j,xobj->title,
	strlen(xobj->title),xobj->TabColor[fore].pixel,xobj->TabColor[li].pixel);
}

void EvtMouseRadioButton(struct XObj *xobj,XButtonEvent *EvtButton)
{
 static XEvent event;
 int End=1;
 unsigned int modif;
 int x1,x2,y1,y2,i,j;
 Window Win1,Win2;
 Window WinBut=0;
 int In;
 char **Buff;

 j=xobj->height/2+3;
 i=(xobj->width-XTextWidth(xobj->xfont,xobj->title,strlen(xobj->title)))/2;

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
	    XSetForeground(xobj->display,xobj->gc,xobj->TabColor[back].pixel);  
	    XFillArc(xobj->display,xobj->ObjWin,xobj->gc,3,j-9,7,7,0*64,360*64);
	    In=1;
	   }
	   else
	   {
	    if (Win2==WinBut)
	    {
	    /* Mouse on button */
	     XSetForeground(xobj->display,xobj->gc,xobj->TabColor[back].pixel);  
	     XFillArc(xobj->display,xobj->ObjWin,xobj->gc,3,j-9,7,7,0*64,360*64); 
	     In=1;
	    }
	    else if (In)
	    {
	     In=0;
	     /* Mouse not on button */
	     XSetForeground(xobj->display,xobj->gc,xobj->TabColor[white].pixel);  
	     XFillArc(xobj->display,xobj->ObjWin,xobj->gc,3,j-9,7,7,0*64,360*64); 
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
	    XSetForeground(xobj->display,xobj->gc,xobj->TabColor[back].pixel);  
	    XFillArc(xobj->display,xobj->ObjWin,xobj->gc,3,j-9,7,7,0*64,360*64); 
	   }
	   else if (In)
	   {
	    /* Mouse not on button */
	    XSetForeground(xobj->display,xobj->gc,xobj->TabColor[white].pixel);  
	    XFillArc(xobj->display,xobj->ObjWin,xobj->gc,3,j-9,7,7,0*64,360*64); 
	    In=0;
	   }
	  break;
      case ButtonRelease:
	   End=0;
	   /* Mouse not on button */
	   if (In)
	   {
	    /* Envoie d'un message vide de type ModifValue pour un clique souris */
	    xobj->value=1;
	    XSetForeground(xobj->display,xobj->gc,xobj->TabColor[black].pixel);  
	    XFillArc(xobj->display,xobj->ObjWin,xobj->gc,3,j-9,7,7,0*64,360*64);
	    SendMsg(xobj,ModifValue);
	   }
	   else if (xobj->value)
	   {
	    XSetForeground(xobj->display,xobj->gc,xobj->TabColor[black].pixel);  
	    XFillArc(xobj->display,xobj->ObjWin,xobj->gc,3,j-9,7,7,0*64,360*64);
	   }
	  break;
     }
 }
}

void EvtKeyRadioButton(struct XObj *xobj,XKeyEvent *EvtKey)
{
}

void ProcessMsgRadioButton(struct XObj *xobj,unsigned long type,unsigned long *body)
{
}





