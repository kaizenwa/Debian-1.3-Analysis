/* our config file... */
#include "xb_config.h"

/* C library includes */
#include <string.h>

#include <stdio.h>
#include <ctype.h>
#ifndef vms
#include <malloc.h>
#endif

/* X11 include(s) */
#include <X11/Xlib.h>
#include <X11/Xutil.h>

/* our include */
#include "xbanner.h"

XColor	xfgc,xshdc,xlghtc,xexact,xulc;

/* for any R/W colors capable display */
Pixel color_cell;

char FGCOLOR[CNLEN]   = FGCOL_DEFVAL;
char SHDCOLOR[CNLEN]  = SHDCOL_DEFVAL;
char LGHTCOLOR[CNLEN] = LGHTCOL_DEFVAL;
char ULCOLOR[CNLEN]   = ULC_DEFVAL;

GradSpec grad[NUM_GRADS];

/* take ASCII representaion of #rrggbb or #rrrrggggbbbb and make an XColor */
static void AtoXColor(char *color, XColor *xcolor)
{
  int i;
  unsigned short red,green,blue;
  
  color++;		/* skip the # sign */

  /* upcase the whole thing */
  for(i=0;color[i]!='\0';i++)
    color[i]=toupper(color[i]) - '0';

  if(i!=6 && i!=12)
  {
    sprintf(errline,"Spec. '%s' is too long or too short, using Black",color);
    error(disp,mgc,errline,False);
    xcolor->pixel = BlackPixel(disp,DefaultScreen(disp));
    return;
  }

  if(i==6) {	/* #rrggbb representation */
    red =   (color[0]>9?color[0]-7:color[0])*16+
	    (color[1]>9?color[1]-7:color[1]);
    green = (color[2]>9?color[2]-7:color[2])*16+
	    (color[3]>9?color[3]-7:color[3]);
    blue =  (color[4]>9?color[4]-7:color[4])*16+
	    (color[5]>9?color[5]-7:color[5]);
    xcolor->red   = ((red   << 8) | red  );
    xcolor->green = ((green << 8) | green);
    xcolor->blue  = ((blue  << 8) | blue );
  }

  if(i==12)	/* #rrrrggggbbbb representation */
  {
    red =   (color[0] > 9? color[0]-7  :color[0]) *4069+
	    (color[1] > 9? color[1]-7  :color[1]) *256+
	    (color[2] > 9? color[2]-7  :color[2]) *16+
	    (color[3] > 9? color[3]-7  :color[3]);
    green = (color[4] > 9? color[4]-7  :color[4]) *4069+
	    (color[5] > 9? color[5]-7  :color[5]) *256+
	    (color[6] > 9? color[6]-7  :color[6]) *16+
	    (color[7] > 9? color[7]-7  :color[7]);
    blue =  (color[8] > 9? color[8]-7  :color[8]) *4069+
	    (color[9] > 9? color[9]-7  :color[9]) *256+
	    (color[10]> 9? color[10]-7 :color[10])*16+
	    (color[11]> 9? color[11]-7 :color[11]);
    xcolor->red   = red;
    xcolor->green = green;
    xcolor->blue  = blue;
  }
  /* all parts are there */
  xcolor->flags=DoRed|DoGreen|DoBlue;
}

/*
   The process of allocating fade colors is simple... I take the RGB components
   of each color, and divide by the Thickness. This gives me a "step" var. for
   each component. Now we go, and each iter adds the "step" to each component
   so we get the colors right.
*/

static Bool AllocColorGrad(XColors ThruCol, int NumThru, XColors Grad,
		int NumGrad, Bool RWcolors, Bool really_alloc)
{
  short r,g,b;
  int r_step,b_step,g_step;
  int gidx,i,j,status;
  Visual *visual;
  Pixel *RWpixels=NULL;
  int steps_per_thrucol;

  steps_per_thrucol = NumGrad / (NumThru-1);

  if(RWcolors && really_alloc)	/* allocate space for RW pixels */
  {
    visual = DefaultVisual(disp,DefaultScreen(disp));
    if(visual->class==DirectColor || visual->class==PseudoColor ||
       visual->class==GrayScale)
    {
      RWpixels = (Pixel*)calloc(NumGrad,sizeof(Pixel));
      if(RWpixels==NULL)
      {
        error(disp,mgc,"REPROT: Alloc list RW pixels",False);
        return False;
      }
      status = XAllocColorCells(disp,cmap,True,NULL,0,RWpixels,NumGrad);
      if(!status)
      {
        sprintf(errline,"Could not allocate %d R/W color cells",NumGrad);
        error(disp,mgc,errline,True);
      }
    }
    else
    {
      error(disp,mgc,"Color cycling not supported on current visual",False);
      return False;
    }
  }

  /* now create the colors */
  for(gidx=0,i=0;i<NumThru-1;i++)
  {
    if(i==NumThru-2 && NumGrad > (steps_per_thrucol * i))
      steps_per_thrucol = NumGrad - i*steps_per_thrucol;
    memcpy(&(Grad[gidx]),&(ThruCol[i]),sizeof(XColor));
    r_step = ((int)ThruCol[i+1].red   - (int)ThruCol[i].red)   / steps_per_thrucol;
    g_step = ((int)ThruCol[i+1].green - (int)ThruCol[i].green) / steps_per_thrucol;
    b_step = ((int)ThruCol[i+1].blue  - (int)ThruCol[i].blue)  / steps_per_thrucol;
    r = Grad[gidx].red; g = Grad[gidx].green; b = Grad[gidx].blue;
    r += r_step; g+=g_step; b+=b_step;
    for(j=0;j<steps_per_thrucol;j++, gidx++, r+=r_step, g+=g_step, b+=b_step)
    {
      Grad[gidx].red=r; Grad[gidx].green=g; Grad[gidx].blue=b;
      Grad[gidx].flags = DoRed|DoGreen|DoBlue;
      if(!RWcolors)
      {
        status = XAllocColor(disp,cmap,&(Grad[gidx]));
        if(!status)
        {
          sprintf(errline,"Couldn't alloc color #%d of %d",gidx,NumGrad);
	  error(disp,mgc,errline,True);
        }
      } /* !RWcolors */
      else if(really_alloc)
        Grad[gidx].pixel = RWpixels[gidx];
    } /* for j=0-steps */
  } /* for i=0-NumThru */
  if(RWcolors && really_alloc)
    XStoreColors(disp,cmap,Grad,NumGrad);
  return True;
} /* AllocColorGrad */

/* allocate the main colors */
void AllocColors(void)
{
  int status;
  Visual *visual;
  /* start by allocating the colors */

  if(FGCOLOR[0]!='#')
    status = XAllocNamedColor(disp,cmap,FGCOLOR,&xfgc,&xexact);
  else	/* allocate by RGB! */
  {
    AtoXColor(FGCOLOR,&xfgc);
    status = XAllocColor(disp,cmap,&xfgc);
  }
  if(!status)
  {
    error(disp,mgc,"Could not get the fg color, trying Black",False);
    xfgc.pixel = BlackPixel(disp,DefaultScreen(disp));
    return;
  }

  visual = DefaultVisual(disp,DefaultScreen(disp));
  if(visual->class==DirectColor || visual->class==PseudoColor ||
     visual->class==GrayScale)
  {
    /* replace the FG color with a single R/W color */
    status = XAllocColorCells(disp,cmap,False,NULL,0,&color_cell,1);
    if(!status)
      error(disp,mgc,"Couldn't alloc a RW color cell for FG color, using RO",False);
    else
    {
      xfgc.pixel = color_cell;	/* change the pixel value */
      xfgc.flags = DoRed|DoGreen|DoBlue;
      XStoreColor(disp,cmap,&xfgc);
    }
  }

  if(SHDCOLOR[0]!='#')
    status = XAllocNamedColor(disp,cmap,SHDCOLOR,&xshdc,&xexact);
  else	/* allocate by RGB! */
  {
    AtoXColor(SHDCOLOR,&xshdc);
    status = XAllocColor(disp,cmap,&xshdc);
  }
  if(!status)
  {
    error(disp,mgc,"Could not get the shadow color, using Black",False);
    xshdc.pixel = BlackPixel(disp,DefaultScreen(disp));
  }

  if(LGHTCOLOR[0]!='#')
    status = XAllocNamedColor(disp,cmap,LGHTCOLOR,&xlghtc,&xexact);
  else	/* allocate by RGB! */
  {
    AtoXColor(LGHTCOLOR,&xlghtc);
    status = XAllocColor(disp,cmap,&xlghtc);
  }
  if(!status)
  {
    error(disp,mgc,"Could not get the highlight color, using White",False);
    xlghtc.pixel = WhitePixel(disp,DefaultScreen(disp));
  }

  if(Underlined)
  {
    if(strcmpi(ULCOLOR,"fgc")==0)	/* use the FG color... */
      memcpy(&xulc,&xfgc,sizeof(xfgc));
    else
    {
      if(ULCOLOR[0]!='#')
        status = XAllocNamedColor(disp,cmap,ULCOLOR,&xulc,&xexact);
      else	/* allocate by RGB! */
      {
        AtoXColor(ULCOLOR,&xulc);
        status = XAllocColor(disp,cmap,&xulc);
      }
      if(!status)
      {
        error(disp,mgc,"Could not get the underline color, using Black",False);
	xulc.pixel = BlackPixel(disp,DefaultScreen(disp));
      }
    }
  } /* Underlined */

  if(bgstyle == FILL || do_fill)
  {
    if(BGCOLOR[0]!='#')
      status = XAllocNamedColor(disp,cmap,BGCOLOR,&xbgc,&xexact);
    else	/* allocate by RGB! */
    {
      AtoXColor(BGCOLOR,&xbgc);
      status = XAllocColor(disp,cmap,&xbgc);
    }
    if(!status)
    {
      error(disp,mgc,"Could not get the color for the BG-fill, disabled",False);
      do_fill = False;
      bgstyle = NOBG;
    }
  }
} /* AllocColors */

/* split gradient color names to several things */
static int split_colornames(int grad_num,char cname[MAX_COLORS_PER_GRAD][CNLEN])
{
  char *s = grad[grad_num].color_names;
  int idx=0;
  int j=0;

  while(*s)
  {
    if(is_sep(*s))
    {
      cname[idx][j]='\0';
      j=0; idx++;
      if(idx==MAX_COLORS_PER_GRAD)
      {
        sprintf(errline,"Too many colors in spec: ");
        sprintf(errline,".Grad%d: %s",grad_num+1,grad[grad_num].color_names);
        error(disp,mgc,errline,True);
      }
      s++; continue;
    }
    else
      cname[idx][j]= *s;
    s++; j++;
  }
  cname[idx][j]='\0';
  idx++;
  if(idx>MAX_COLORS_PER_GRAD)
  {
    sprintf(errline,"Too many colors in spec: ");
    sprintf(errline,".Grad%d: %s",grad_num+1,grad[grad_num].color_names);
    error(disp,mgc,errline,True);
  }
  return idx;
} /* split color names */

/* fill in the grad struct and call AllocColorGrad */
Bool AllocGrad(int grad_num, int steps, Bool RW)
{
  XColor dummy;
  char cname[MAX_COLORS_PER_GRAD][CNLEN];
  int  num_colors;
  int  i,status;

  if(grad_num == -1)
    error(disp,mgc,"Missing a color gradient definition",True);

  /* first let's get the names of the colors */
  num_colors = split_colornames(grad_num,cname);
  grad[grad_num].xcolors = num_colors;

#if 0 /* not useful for now because of the way is_grad_line() works */
  if(num_colors == 1)
  {
    error(disp,mgc,"Grad definition contains only one color, fixing",False);
    strcpy(errline,grad[grad_num].color_names);
    strcat(errline,",");
    strcat(errline,grad[grad_num].color_names);
    strcpy(grad[grad_num].color_names,errline);
    num_colors++;
  }
#endif

  /* fill in the XColors of the grad without allocating them */
  for(i=0;i<num_colors;i++)
  {
    if(cname[i][0] == '#')
      AtoXColor(cname[i],&(grad[grad_num].xcolor[i]));
    else
    {
      status = XLookupColor(disp,cmap,cname[i],&(grad[grad_num].xcolor[i]),&dummy);
      if(!status)
      {
        sprintf(errline,"Cannot resolve color name '%s'",cname[i]);
        error(disp,mgc,errline,True);
      }
    }
  } /* for i=0-num_colors */

  /* and prepare to alloc the grad */
  grad[grad_num].gradcolors = steps;
  grad[grad_num].grad = (XColor*)calloc(steps,sizeof(XColor));
  if(grad[grad_num].grad == NULL)
  {
    sprintf(errline,"REPORT: Alloc %d Grad XColors",steps);
    error(disp,mgc,errline,True);
  }
  if(FgCycGrad == grad_num)
    return AllocColorGrad(grad[grad_num].xcolor,num_colors,grad[grad_num].grad,steps,RW,False);
  else
    return AllocColorGrad(grad[grad_num].xcolor,num_colors,grad[grad_num].grad,steps,RW,True);
} /* AllocGrad */

#if 0
/* generic function that inverses the cycle-direction */
static _inline CycleDirection inv_cycdir(CycleDirection cycdir)
{
  return (cycdir==CYC_FORWARD ? CYC_REVERSE : CYC_FORWARD);
}
#endif

/* color cycling for more than 1 thing */
Cycle_Info cyc_inf[MAX_CYCLES];
int num_used_cyc = 0;

static void CycleGrad(Display *disp,Cycle_Info *ci)
{
  /* declared static to avoid reallocating them on the stack every time */
  static Pixel pixel;
  static int i,j=0;
  GradSpec *gs= &(grad[ci->grad_num]);

  if(ci->dir == CYC_REVERSE)
  {
    pixel = gs->grad[0].pixel;
    for(i=0;i<(gs->gradcolors)-1;i++)
      gs->grad[i].pixel   = gs->grad[i+1].pixel;
  }
  else /* ci->dir == CYC_FORWARD */
  {
    pixel   = gs->grad[gs->gradcolors-1].pixel;
    for(i=gs->gradcolors-1;i>0;i--)
      gs->grad[i].pixel   = gs->grad[i-1].pixel;
  }
  /* in both cases i is OK for the following lines: */
  gs->grad[i].pixel = pixel;
  if(!(ci->only_first))
    XStoreColors(disp,cmap,gs->grad,gs->gradcolors);
  else
  {
    gs->grad[j].pixel = color_cell;	/* the FG color cell */
    XStoreColor(disp,cmap,&(gs->grad[j]));
    j++;
    if(j>=(gs->gradcolors))
      j=0;
  }
} /* CycleGrad() */

void DoColorCycling(Display *disp)
{
  static int i;

  /* *** ADD CHECK FOR redc/greenc/bluec INDIVIDUAL COMPONENT CYCLING *** */

  for(i=0;i<num_used_cyc;i++)
    CycleGrad(disp,&(cyc_inf[i]));
}
