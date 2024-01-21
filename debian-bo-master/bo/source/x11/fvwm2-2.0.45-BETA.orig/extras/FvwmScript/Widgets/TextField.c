#include "Widget.h"


/***********************************************/
/* Fonction for TextField                      */
/***********************************************/
void InitTextField(struct XObj *xobj)
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
 Attr.cursor=XCreateFontCursor(xobj->display,XC_xterm); 
 mask|=CWCursor;		/* Curseur pour la fenetre */
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
 /* value2 représente la fin de la zone selectionnee */
 if (xobj->value>strlen(xobj->title))
  xobj->value=strlen(xobj->title);
 xobj->value2=xobj->value;
 
 /* Redimensionnement du widget */
 xobj->height=xobj->xfont->max_bounds.ascent+xobj->xfont->max_bounds.descent+10;
 i=XTextWidth(xobj->xfont,xobj->title,strlen(xobj->title))+40;
 if (xobj->width<i)
  xobj->width=i;
 XResizeWindow(xobj->display,xobj->ObjWin,xobj->width,xobj->height);

}

void DestroyTextField(struct XObj *xobj)
{
 XFreeFont(xobj->display,xobj->xfont);
 XFreeGC(xobj->display,xobj->gc);
 XDestroyWindow(xobj->display,xobj->ObjWin);
}

/* Dessin du curseur du texte */
void DrawPointTxt(struct XObj *xobj,unsigned int color)
{
 #define dec 2
 int x,y;
 XSegment segm[2];

 x=XTextWidth(xobj->xfont,xobj->title,xobj->value)+5;
 y=xobj->xfont->max_bounds.ascent+5;

 segm[0].x1=x;
 segm[0].y1=y;
 segm[0].x2=x-dec;
 segm[0].y2=y+dec;
 segm[1].x1=x;
 segm[1].y1=y;
 segm[1].x2=x+dec;
 segm[1].y2=y+dec;
 XSetForeground(xobj->display,xobj->gc,color);
 XDrawSegments(xobj->display,xobj->ObjWin,xobj->gc,segm,2);
}

/* Dessin du contenu du champs texte */
void DrawTextField(struct XObj *xobj)
{
 int x1,y1;
 int x2,y2;

 DrawReliefRect(0,0,xobj->width,xobj->height,xobj,
	xobj->TabColor[shad].pixel,xobj->TabColor[li].pixel,xobj->TabColor[black].pixel,1);
 XSetForeground(xobj->display,xobj->gc,xobj->TabColor[fore].pixel);
 y1=xobj->xfont->max_bounds.ascent;
 XDrawImageString(xobj->display,xobj->ObjWin,xobj->gc,5,y1+5,xobj->title,strlen(xobj->title));
 
 /* Dessin de la zone selectionnee */
 XSetFunction(xobj->display,xobj->gc,GXinvert);
 if (xobj->value2>xobj->value)		/* Curseur avant la souris */
 {
  x1=XTextWidth(xobj->xfont,&xobj->title[0],xobj->value);
  x2=XTextWidth(xobj->xfont,&xobj->title[xobj->value],xobj->value2-xobj->value);
 }
 else		/* Curseur apres la souris */
 {
  x1=XTextWidth(xobj->xfont,&xobj->title[0],xobj->value2);
  x2=XTextWidth(xobj->xfont,&xobj->title[xobj->value2],xobj->value-xobj->value2);
 }
 XFillRectangle(xobj->display,xobj->ObjWin,xobj->gc,x1+5,7,x2,y1+xobj->xfont->max_bounds.descent-2);
 XSetFunction(xobj->display,xobj->gc,GXcopy);

 /* Dessin du point d'insertion */
 DrawPointTxt(xobj,xobj->TabColor[fore].pixel);
}

void EvtMouseTextField(struct XObj *xobj,XButtonEvent *EvtButton)
{
 unsigned int modif;
 int x1,x2,y1,y2,i,j;
 Window Win1,Win2;
 int PosCurs=0;
 int SizeBuf;
 char *str;
 int NewPos;
 Atom type,prop;
 XEvent event;
 int ButPress=1;
 int format;
 unsigned long longueur,octets_restant;
 unsigned char *donnees="";
 
 /* On deplace le curseur a la position de la souris */
 /* On recupere la position de la souris */
 switch (EvtButton->button)
 {
  case Button1:
    XQueryPointer(xobj->display,*xobj->ParentWin,&Win1,&Win2,&x1,&y1,&x2,&y2,&modif);
    x2=x2-xobj->x;
    while ((PosCurs<strlen(xobj->title))&&(x2>XTextWidth(xobj->xfont,xobj->title,PosCurs)+12))
     PosCurs++;
    DrawPointTxt(xobj,xobj->TabColor[back].pixel);
    xobj->value=PosCurs;
    xobj->value2=PosCurs;
    DrawPointTxt(xobj,xobj->TabColor[fore].pixel);
    DrawTextField(xobj);

    while (ButPress)
    {
     XNextEvent(xobj->display, &event);
     switch (event.type)
     {
      case MotionNotify:
       XQueryPointer(xobj->display,*xobj->ParentWin,&Win1,&Win2,&x1,&y1,&x2,&y2,&modif);
       x2=x2-xobj->x;
       while ((PosCurs<strlen(xobj->title))&&(x2>XTextWidth(xobj->xfont,xobj->title,PosCurs)+12))
     	PosCurs++;
       xobj->value2=PosCurs;
       DrawTextField(xobj);
       PosCurs=0;
      break;
      case ButtonRelease:
       ButPress=0;
      break;
     }
    }
    /* Enregistrement de la selection dans le presse papier */
    str=(char*)GetText(xobj,xobj->value2);
    for (i=0;i<=7;i++)
     XStoreBuffer(xobj->display,str,strlen(str),i);
    free(str);
   break;
   
   case Button2:			/* Colle le texte */
    /* Demande le contenu d'une selection */
    XConvertSelection(xobj->display,XA_PRIMARY,XA_STRING,propriete,*xobj->ParentWin,
    			EvtButton->time);
    while (!(XCheckTypedEvent(xobj->display,SelectionNotify,&event)))
     ;
    if (event.xselection.property!=None)
     if (event.xselection.selection==XA_PRIMARY)
     {
      XGetWindowProperty(xobj->display,event.xselection.requestor,event.xselection.property,0,
      			 8192,False,event.xselection.target,&type,&format,&longueur,&octets_restant,
      			 &donnees);
      if (longueur>0)
      {
       Scrapt=strcpy(Scrapt,donnees);
       XDeleteProperty(xobj->display,event.xselection.requestor,event.xselection.property);
       XFree(donnees);
      }
     }
    SizeBuf=strlen(Scrapt);
    if (SizeBuf>0)
    {
     NewPos=InsertText(xobj,Scrapt,SizeBuf);
     DrawPointTxt(xobj,xobj->TabColor[back].pixel);
     xobj->value=NewPos;
     xobj->value2=NewPos;
     DrawPointTxt(xobj,xobj->TabColor[fore].pixel);
     DrawTextField(xobj);
    }
    SendMsg(xobj,ModifValue);
    break;
   
   case Button3:		/* Appuie sur le troisieme bouton */
    XQueryPointer(xobj->display,*xobj->ParentWin,&Win1,&Win2,&x1,&y1,&x2,&y2,&modif);
    x2=x2-xobj->x;
    while ((PosCurs<strlen(xobj->title))&&
 		(x2>XTextWidth(xobj->xfont,xobj->title,PosCurs)+12))
     PosCurs++;
    if ((PosCurs<xobj->value) && (xobj->value<xobj->value2))
      xobj->value=xobj->value2;
    if ((PosCurs>xobj->value) && (xobj->value>xobj->value2))
      xobj->value=xobj->value2;
    xobj->value2=PosCurs;
    DrawTextField(xobj);

    while (ButPress)
    {
     XNextEvent(xobj->display, &event);
     switch (event.type)
     {
      case MotionNotify:
       XQueryPointer(xobj->display,*xobj->ParentWin,&Win1,&Win2,&x1,&y1,&x2,&y2,&modif);
       x2=x2-xobj->x;
       while ((PosCurs<strlen(xobj->title))&&(x2>XTextWidth(xobj->xfont,xobj->title,PosCurs)+12))
     	PosCurs++;
       xobj->value2=PosCurs;
       DrawTextField(xobj);
       PosCurs=0;
      break;
      case ButtonRelease:
       ButPress=0;
      break;
     }
    }
    str=(char*)GetText(xobj,xobj->value2);
    for (i=0;i<=7;i++)
     XStoreBuffer(xobj->display,str,strlen(str),i);
    free(str);
   break;
 }
}

void EvtKeyTextField(struct XObj *xobj,XKeyEvent *EvtKey)
{
 int i,y,x2,y2;
 char car[11];
 char *carks;
 KeySym ks;
 int Size;
 int NewPos;

 /* Recherche du charactere */
 i=XLookupString(EvtKey,car,10,&ks,NULL);
 NewPos=xobj->value;
 car[i]='\0';
 carks=XKeysymToString(ks);

 if (carks!=NULL)
 {
  if (strcmp(carks,"Right")==0)
  {
   NewPos++;
   if (NewPos>strlen(xobj->title))
    NewPos=strlen(xobj->title);
  }
  else if (strcmp(carks,"Left")==0)
  {
   NewPos--;
   if (NewPos<0)
    NewPos=0;
  }
  else if (strcmp(carks,"Return")==0)
  {
   ;
  }
  else if ((strcmp(carks,"Delete")==0)||(strcmp(carks,"BackSpace")==0))
  {
   if (NewPos>0)
   {
    Size=strlen(xobj->title);
    memmove(&xobj->title[NewPos-1],&xobj->title[NewPos],
			Size-NewPos+1);
    xobj->title=(char*)realloc(xobj->title,(Size)*sizeof(char));
    NewPos--;
   }
  }
  else if (i!=0)	/* Cas d'un caractere normal */
  {
   /* Insertion du caractere dans le titre */
   Size=strlen(xobj->title);
   xobj->title=(char*)realloc(xobj->title,(2+Size)*sizeof(char));
   memmove(&xobj->title[NewPos+1],&xobj->title[NewPos],
			Size-NewPos+1);
   xobj->title[NewPos]=car[0];
   NewPos++;
  }

 }

 XSetForeground(xobj->display,xobj->gc,xobj->TabColor[back].pixel);
 x2=XTextWidth(xobj->xfont,xobj->title,strlen(xobj->title));
 XFillRectangle(xobj->display,xobj->ObjWin,xobj->gc,x2+4,4,xobj->width-x2-8,xobj->height-8);

 xobj->value=NewPos;
 xobj->value2=NewPos;
 DrawPointTxt(xobj,xobj->TabColor[fore].pixel);
 DrawTextField(xobj);
 SendMsg(xobj,ModifValue);
}

void ProcessMsgTextField(struct XObj *xobj,unsigned long type,unsigned long *body)
{
}


