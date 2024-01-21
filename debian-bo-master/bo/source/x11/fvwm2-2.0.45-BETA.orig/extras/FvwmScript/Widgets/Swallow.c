#include "Widget.h"

extern int fd[2];

/***********************************************/
/* Fonction pour Swallow                       */
/***********************************************/

void DrawRelief(struct XObj *xobj)
{
 XSegment segm[2];
 int i;
 int j;
 
 if (xobj->value!=0)
 {
  for (i=1;i<4;i++)
  {
   segm[0].x1=xobj->x-i;
   segm[0].y1=xobj->y-i;
   segm[0].x2=xobj->x+xobj->width+i-2;
   segm[0].y2=xobj->y-i;
   segm[1].x1=xobj->x-i;
   segm[1].y1=xobj->y-i;
   segm[1].x2=xobj->x-i;
   segm[1].y2=xobj->y+xobj->height+i-2;
   if (xobj->value==-1)
    XSetForeground(xobj->display,xobj->gc,xobj->TabColor[shad].pixel);
   else
    XSetForeground(xobj->display,xobj->gc,xobj->TabColor[li].pixel);
   XDrawSegments(xobj->display,*xobj->ParentWin,xobj->gc,segm,2);
 
   segm[0].x1=xobj->x-i;
   segm[0].y1=xobj->y+xobj->height+i-1;
   segm[0].x2=xobj->x+xobj->width+i-1;
   segm[0].y2=xobj->y+xobj->height+i-1;
   segm[1].x1=xobj->x+xobj->width+i-1;
   segm[1].y1=xobj->y-i;
   segm[1].x2=xobj->x+xobj->width+i-1;
   segm[1].y2=xobj->y+xobj->height+i-1;
   if (xobj->value==-1)
    XSetForeground(xobj->display,xobj->gc,xobj->TabColor[li].pixel);
   else
    XSetForeground(xobj->display,xobj->gc,xobj->TabColor[shad].pixel);
   XDrawSegments(xobj->display,*xobj->ParentWin,xobj->gc,segm,2);
  }
 }

}

void InitSwallow(struct XObj *xobj)
{
 fd_set in_fdset;
 int count = 0;
 unsigned long header[HEADER_SIZE];
 unsigned long *body;
 unsigned long mask;
 XSetWindowAttributes Attr;
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
 xobj->ObjWin=XCreateWindow(xobj->display,*xobj->ParentWin,
		-1000,-1000,xobj->width,xobj->height,0,
		CopyFromParent,InputOutput,CopyFromParent,
		mask,&Attr);

 /* Redimensionnement du widget */
 if (xobj->height<30)
  xobj->height=30;
 if (xobj->width<30) 
  xobj->width=30;

 if (xobj->swallow!=NULL)
 {
  SendText(fd,xobj->swallow,0);
 }
 else
  fprintf(stderr,"Error\n");
}

/***************************************************************************
 *
 * ICCCM Client Messages - Section 4.2.8 of the ICCCM dictates that all
 * client messages will have the following form:
 *
 *     event type	ClientMessage
 *     message type	_XA_WM_PROTOCOLS
 *     window		tmp->w
 *     format		32
 *     data[0]		message atom
 *     data[1]		time stamp
 *
 ****************************************************************************/
void my_send_clientmessage (Window w, Atom a, Time timestamp,Display *dpy)
{
  XClientMessageEvent ev;
  Atom _XA_WM_PROTOCOLS;

  _XA_WM_PROTOCOLS = XInternAtom (dpy,"WM_PROTOCOLS",False);
  ev.type = ClientMessage;
  ev.window = w;
  ev.message_type = _XA_WM_PROTOCOLS;
  ev.format = 32;
  ev.data.l[0] = a;
  ev.data.l[1] = timestamp;
  XSendEvent (dpy, w, False, 0L, (XEvent *) &ev);
}

void DestroySwallow(struct XObj *xobj)
{
 static Atom wm_del_win;

 /* Arrete le programme swallow */
 wm_del_win = XInternAtom(xobj->display,"WM_DELETE_WINDOW",False);
 XSetWMProtocols(xobj->display,*xobj->ParentWin,&wm_del_win,1);
 my_send_clientmessage(xobj->ObjWin,wm_del_win,CurrentTime,xobj->display);

}

void DrawSwallow(struct XObj *xobj)
{
 DrawRelief(xobj);
}

void EvtMouseSwallow(struct XObj *xobj,XButtonEvent *EvtButton)
{
}

void EvtKeySwallow(struct XObj *xobj,XKeyEvent *EvtKey)
{
 fprintf(stderr,"Evt key\n");
}

/* Recupere le pointeur de la fenetre Swallow */
void CheckForHangon(struct XObj *xobj,unsigned long *body)
{
  int button,i,j;
  char *cbody;

  cbody = (char *)&body[3];
  /*XDestroyWindow(xobj->display,xobj->ObjWin);*/
  if(strcmp(cbody,xobj->title)==0)
  {
   xobj->ObjWin = (Window)body[0];
   strcpy(xobj->title,"");
   XUnmapWindow(xobj->display,xobj->ObjWin);
   XSetWindowBorderWidth(xobj->display,xobj->ObjWin,0);
  }
}

void swallow(struct XObj *xobj,unsigned long *body)
{
 unsigned long mask;
 XSetWindowAttributes Attr;

 if(xobj->ObjWin == (Window)body[0])
 {
  XReparentWindow(xobj->display,xobj->ObjWin,*xobj->ParentWin,xobj->x,xobj->y);
  XResizeWindow(xobj->display,xobj->ObjWin,xobj->width,xobj->height);
  XMapWindow(xobj->display,xobj->ObjWin);
  DrawRelief(xobj);
  XSelectInput(xobj->display,xobj->ObjWin,PropertyChangeMask|StructureNotifyMask);
 }
}

void ProcessMsgSwallow(struct XObj *xobj,unsigned long type,unsigned long *body)
{
 switch(type)
 {
  case M_MAP:
   swallow(xobj,body);
  case M_RES_NAME:
  case M_RES_CLASS:
  case M_WINDOW_NAME:
   CheckForHangon(xobj,body);
  break;
  default:
  break;
 }
}






