#include "Widget.h"

/***********************************************/
/* Fonction d'ecriture en relief               */
/***********************************************/
void DrawReliefString(Display *disp,Window win,GC gc,int x,
	int y,char *str,int strl,unsigned long ForeC,unsigned long BackC)
{
 XSetForeground(disp,gc,BackC);
 XDrawImageString(disp,win,gc,x+1,y+1,str,strl);
 XSetForeground(disp,gc,ForeC);
 XDrawString(disp,win,gc,x,y,str,strl);
}


/**************************************************/
/* Retourne le titre de l'option id du menu       */
/**************************************************/
char* GetMenuTitle(char *str,int id)
{
 int i=1;
 int w=0;
 int w2=0;
 char* TempStr;
 
 TempStr=(char*)calloc(1,30);
 while ((str[w+w2]!='\0')&&(str[w+w2]!='|'))
  w2++;

 while ((i<id)&&(str[w]!='\0'))
 {
  i++;
  if (str[w+w2]=='|') w2++;
  w=w+w2;
  w2=0;
  while ((str[w+w2]!='\0')&&(str[w+w2]!='|'))
   w2++;
 }
 TempStr=strncpy(TempStr,&str[w],w2);
 return TempStr;
}

/***********************************************************/
/* Dessine le contenu de la fenetre du popup-menu          */
/***********************************************************/
void DrawMenu(struct XObj *xobj,Window WinPop,int h,int StrtOpt)
{
 XSegment segm[2];
 int i;
 char *str;
 int x,y;
 int width,height;
 Window Root;
 
 XGetGeometry(xobj->display,WinPop,&Root,&x,&y,&width,&height,&i,&i);
 for (i=0;i<2;i++)
 {
  segm[0].x1=i;
  segm[0].y1=i;
  segm[0].x2=width-i-1;
  segm[0].y2=i;
  
  segm[1].x1=i;
  segm[1].y1=i;
  segm[1].x2=i;
  segm[1].y2=height-i-1;
  XSetForeground(xobj->display,xobj->gc,xobj->TabColor[li].pixel);
  XDrawSegments(xobj->display,WinPop,xobj->gc,segm,2);
 
  segm[0].x1=1+i;
  segm[0].y1=height-i-1;
  segm[0].x2=width-i-1;
  segm[0].y2=height-i-1;
  
  segm[1].x1=width-i-1;
  segm[1].y1=i;
  segm[1].x2=width-i-1;
  segm[1].y2=height-i-1;
  XSetForeground(xobj->display,xobj->gc,xobj->TabColor[shad].pixel);
  XDrawSegments(xobj->display,WinPop,xobj->gc,segm,2);
 }
 /* Ecriture des options */
 x=0;
 for (i=StrtOpt;i<=xobj->value3;i++)
 {
  str=(char*)GetMenuTitle(xobj->title,i);
  if (x<XTextWidth(xobj->xfont,str,strlen(str)))
   x=XTextWidth(xobj->xfont,str,strlen(str));
  free(str);
 }
 for (i=StrtOpt;i<=xobj->value3;i++)
 {
  str=(char*)GetMenuTitle(xobj->title,i);
  y=h*(i-StrtOpt)+xobj->xfont->max_bounds.ascent+4;
  DrawReliefString(xobj->display,WinPop,xobj->gc,width-x-8,y,str,
  		strlen(str),xobj->TabColor[fore].pixel,xobj->TabColor[li].pixel);
 free(str);
 }
}

/***********************************************************/
/* Dessine l'option active d'un menu                       */
/***********************************************************/
void SelectMenu(struct XObj *xobj,Window WinPop,int hOpt,int newvalue,int Show)
{
 XSegment segm[2];
 int i;
 char *str;
 int x,y;
 int width,height;
 Window Root;
 
 XGetGeometry(xobj->display,WinPop,&Root,&x,&y,&width,&height,&i,&i);
 y=hOpt*(newvalue-1);
 for (i=0;i<2;i++)
 {
  segm[0].x1=i+2;
  segm[0].y1=i+y+2;
  segm[0].x2=width-i-3;
  segm[0].y2=i+y+2;
  
  segm[1].x1=i+2;
  segm[1].y1=i+y+2;
  segm[1].x2=i+2;
  segm[1].y2=y+hOpt-4-i;
  if (Show)
   XSetForeground(xobj->display,xobj->gc,xobj->TabColor[li].pixel);
  else
   XSetForeground(xobj->display,xobj->gc,xobj->TabColor[back].pixel);
  XDrawSegments(xobj->display,WinPop,xobj->gc,segm,2);
 
  segm[0].x1=i+3;
  segm[0].y1=y-i-3+hOpt;
  segm[0].x2=width-i-3;
  segm[0].y2=y-i-3+hOpt;
  
  segm[1].x1=width-i-3;
  segm[1].y1=i+y+2;
  segm[1].x2=width-i-3;
  segm[1].y2=i+y-4+hOpt;
  if (Show)
   XSetForeground(xobj->display,xobj->gc,xobj->TabColor[shad].pixel);
  else
   XSetForeground(xobj->display,xobj->gc,xobj->TabColor[back].pixel);
  XDrawSegments(xobj->display,WinPop,xobj->gc,segm,2);
 }
}

/**************************************************/
/* Compte le nombre d'option contenu dans un menu */
/**************************************************/
int CountOption(char *str)
{
 int i=1;
 int w=0;

 while (str[w]!='\0')
 {
  if (str[w]=='|') i++;
  w++;
 }
 
 return i;
}


/*****************************************/
/* Dessine l'icone et le titre du widget */
/*****************************************/
void DrawIconStr(int offset,struct XObj *xobj,int DoRedraw)
{
 int i,j;
 char *str;

 if (DoRedraw)
 {
  XSetForeground(xobj->display,xobj->gc,xobj->TabColor[back].pixel);
  XFillRectangle(xobj->display,xobj->ObjWin,xobj->gc,4,4,xobj->width-8,xobj->height-8);
 }
 
 if (xobj->iconPixmap==None)			/* Si l'icone n'existe pas */
 {
  str=GetMenuTitle(xobj->title,1);
  j=xobj->height/2+(xobj->xfont->max_bounds.ascent+xobj->xfont->max_bounds.descent)/2+offset-3;
  i=(xobj->width-XTextWidth(xobj->xfont,str,strlen(str)))/2+offset;
  DrawReliefString(xobj->display,xobj->ObjWin,xobj->gc,i,j,str,
	strlen(str),xobj->TabColor[fore].pixel,xobj->TabColor[li].pixel);
  free(str);
 }
 else					/* Si l'icone existe */
 {
  if (xobj->title!=NULL)
  {
   str=GetMenuTitle(xobj->title,1);
   if (strlen(str)!=0)
   {
    i=(xobj->width-XTextWidth(xobj->xfont,str,strlen(str)))/2+offset;
    j=((xobj->height - xobj->icon_h)/4)*3 + xobj->icon_h+offset+
   		(xobj->xfont->max_bounds.ascent+xobj->xfont->max_bounds.descent)/2-3;
    DrawReliefString(xobj->display,xobj->ObjWin,xobj->gc,i,j,str,
	 strlen(str),xobj->TabColor[fore].pixel,xobj->TabColor[li].pixel);
   }
   free(str);
  }
  /* Dessin de l'icone */
  if (xobj->icon_maskPixmap!=None)
   XSetClipMask(xobj->display,xobj->gc,xobj->icon_maskPixmap);
  j=(xobj->height - xobj->icon_h)/2+offset;
  i=(xobj->width - xobj->icon_w)/2+offset;
  XSetClipOrigin(xobj->display,xobj->gc,i,j);
  XSetForeground(xobj->display,xobj->gc,xobj->TabColor[fore].pixel);
  XCopyArea(xobj->display,xobj->iconPixmap,xobj->ObjWin,xobj->gc,0,0,
  						xobj->icon_w,xobj->icon_h,i,j);
  XSetClipMask(xobj->display,xobj->gc,None);
 }
}

/***********************************************/
/* Fonction de dessin d'un rectangle en relief */
/***********************************************/
void DrawReliefRect(int x,int y,int width,int height,struct XObj *xobj,
		unsigned int LiC, unsigned int ShadC,unsigned int ForeC,int RectIn)
{
 XSegment segm[2];
 int i;
 int j; 

/* XSetForeground(xobj->display,xobj->gc,xobj->TabColor[back].pixel);
 XFillRectangle(xobj->display,xobj->ObjWin,xobj->gc,x,y,width,height);*/
 width--;
 height--;

 for (i=1;i<3;i++)
 {
  j=-1-i;
  segm[0].x1=i+x;
  segm[0].y1=i+y;
  segm[0].x2=i+x;
  segm[0].y2=height+j+y+1;
  segm[1].x1=i+x;
  segm[1].y1=i+y;
  segm[1].x2=width+j+x+1;
  segm[1].y2=i+y;
  XSetForeground(xobj->display,xobj->gc,LiC);
  XDrawSegments(xobj->display,xobj->ObjWin,xobj->gc,segm,2);
 
  segm[0].x1=width+j+x+1;
  segm[0].y1=i+1+y;
  segm[0].x2=width+j+x+1;
  segm[0].y2=height+j+y+1;
  segm[1].x1=i+1+x;
  segm[1].y1=height+j+y+1;
  segm[1].x2=width+j+x+1;
  segm[1].y2=height+j+y+1;
  XSetForeground(xobj->display,xobj->gc,ShadC);
  XDrawSegments(xobj->display,xobj->ObjWin,xobj->gc,segm,2);
 }
 XSetForeground(xobj->display,xobj->gc,ForeC);
 if (RectIn==1)			/* Rectangle a l'interieur */
  XDrawRectangle(xobj->display,xobj->ObjWin,xobj->gc,x+3,y+3,
 		width-6,height-6);
 else if (RectIn==0)		/* Rectangle a l'exterieur */
  XDrawRectangle(xobj->display,xobj->ObjWin,xobj->gc,x,y,width,height);
 XSetForeground(xobj->display,xobj->gc,xobj->TabColor[fore].pixel);
}

/***********************************************/
/* Calcul ascent de la police                  */
/***********************************************/
int GetAscFont(XFontStruct *xfont)
{
 return xfont->max_bounds.ascent;
}

/***********************************************/
/* Insertion d'un str dans le titre d'un objet */
/***********************************************/
int InsertText(struct XObj *xobj,char *str,int SizeStr)
{
 int Size;
 int NewPos;
 int i;

 /* Insertion du caractere dans le titre */
 NewPos=xobj->value;
 Size=strlen(xobj->title);
 xobj->title=(char*)realloc(xobj->title,(1+SizeStr+Size)*sizeof(char));
 memmove(&xobj->title[NewPos+SizeStr],&xobj->title[NewPos],
			Size-NewPos+1);
 for (i=NewPos;i<NewPos+SizeStr;i++)
  xobj->title[i]=str[i-NewPos];
 NewPos=NewPos+SizeStr;
 return NewPos;
}

/******************************************************/
/* Lecture d'un morceau de texte de xobj->value à End */
/******************************************************/
char *GetText(struct XObj *xobj,int End)
{
 char *str;
 int a,b;

 if (End>xobj->value)
 {
  a=xobj->value;
  b=End;
 }
 else
 {
  b=xobj->value;
  a=End;
 }
 str=(char*)calloc(b-a+2,1);
 memcpy(str,&xobj->title[a],b-a);
 str[b-a+1]='\0';
 return str;
}


















