/*
 * Copyright (C) 1992  Board of Regents of the University of Wisconsin
 * on behalf of the Department of Electrical Engineering and Computer
 * Science, University of Wisconsin-Milwaukee, Milwaukee, WI 53201.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * a copy of which is included here in file "GNU_GENERAL"
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * The programs in this directory were developed by software engineering 
 * teams as part of the course "Introduction to Software Engineering" 
 * under the supervision of Professor G. Davida.
 * This is a modification of a program written or modified by
 * others.  The original copyrights, as per GNU General Public License,
 * may still be applicable.  The UWM copyright is applicable only
 * the those parts generated at UWM.
 *
 * Please send all changes, enhancements, and other comments about this
 * software to
 *     		soft-eng@cs.uwm.edu
 *
 * No Warranty, expressed or implied, comes with this software.
 * This software is intended to be used by not-for-profit
 * organizations or by individuals for personal HOME use. 
 * This software, or any of its parts, may not be used by for-profit
 * organization, regardless of application or intended product or
 * customer, without the permission of the Board of Regents of the 
 * University  of Wisconsin. 
 *
 * Contact:	soft-eng@cs.uwm.edu
 *			or
 *		
 *		Software Engineering Coordinator
 *		Computer Science
 *    		Department of EECS
 *		University of Wisconsin - Milwaukee
 *		Milwaukee, WI  53201
 *		414-229-4677
 *
 *		HISTORY,CLAIMS and CONTRIBUTIONS
 */
/* This function will produce a bar graph for the data input by the user. */
/* All sections of this function are explained as they are implemented.   */
/*    Written by: Georgiane Kondich   August 1990                         */
/*                                                                        */
/* Modified 10/27/91 - Dan Gruber.  Changed plot_bar to check for a valid */
/* number of X labels before it Maps the window to the screen and makes   */
/* it visible to the user.  This gives plot_bar a cleaner exit.           */
/*                                                                        */
/* Modified 12/10/91 - Dan Gruber.  Completely revised plot_bar.  It now  */
/* has the ability to display up to six columns of data instead of only   */
/* one.  It also uses patterns located in pattern.h for each column.      */
/*                                                                        */ 
/* Modified 5/94 - Dan Coppersmith.  Improved user friendliness of error  */
/* function messages. */
/**************************************************************************/

#include <config.h>

#ifdef HAVE_X11_X_H	/* this code for now is X specific */

#include <curses.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "plot.h"
#include "pattern.h"
#include "graphic_gvar.h"
#include "sc.h"
#include "scXstuff.h"
#include <math.h>
#define TITLE "XSPREAD-Grapher (Bar Plot)"
#define PIX_SEP 8   /* The distance between each group of bars */

#ifndef irint 
#define irint(x) floor((x)+0.5)
#endif

  XWMHints   Xwmh = {
     (InputHint|StateHint),  /*flags*/
     False,                  /*input*/
     NormalState,            /*initial_state*/
     0,                      /*icon pixmap*/
     0,                      /*icon window*/
     0, 0,                   /*icon location*/
     0,                      /*icon mask*/
     0,                      /*Window group*/
  };

void
plot_bar()
   { int argc = 0;
    char **argv = 0;

  
      double max_y;      /* Maximum Y value                   */
      double min_y;      /* Minimum Y value                   */
      double Y_Range;    /* "max_y - min_y"                   */ 

      int i,j /* ,k */;         /* Iteration variables               */
      int len;           /* Length of a Label                 */
      int grid_l_x;      /* Size of grid separators           */ 
      int grid_l_y;
      int grid_h;
      char str[100];     /* The Label to be drawn             */
      int height,width;  /* Height and Width of the bar       */
      int text_w;        /* Text width variable               */
      int x,y;           /* Current x and y position          */
      struct ent *p;
      int looping = 1;   /* Continue looping flag             */
      int draw = 0;      /* Continue drawing flag             */
      int first_bar=1;   /* The first bar flag                */
      int num_rows = 0;  /* The number of rows to be drawn    */
      int num_cols = 0;  /* The number of columns to be drawn */
      int row;           /* The current row position          */
      int bars;          /* The current bar being processed   */
      int labelsize;     /* The size of the Label             */
      Pixmap Pattern;    /* The type of pattern to display    */

/*---- Colors added by Rama Devi ---------*/
Colormap cmap;
XColor exactBlack, exactOrange;
XColor exactGreen, exactGreenYellow, exactOrchid;
XColor exactYellow, exactAquamarine, exactGoldenrod;

XColor Black, Orange;
XColor Green, GreenYellow, Orchid, Yellow, Aquamarine, Goldenrod;
 
extern XFontStruct *curfont;       /* Font descriptor struct for current font */
extern Font       curfontid;       /* resource id of current font */
extern int        curfontwidth,
           curfontheight;   /* pixel dimensions of current font */

    /*
     * Decide the scale for x and y axes
     */
  

   bars = 0;
   for (i=graphic_range[0].r1; i<=graphic_range[0].r2; i++) 
   {
	p = lookat(i,graphic_range[0].c);
	if (!(p->label)) {
	   continue;
	}
	bars++;
   }

  num_rows = bars;/*------line added by Rama Devi -------*/

   if (bars < 1) 
   {
	  fprintf(stderr,"\007");
	  if (graphic_range[0].r2 - graphic_range[0].r1 > 1)
	      message("x values must be strings");
	  else
	      message("Not enough valid X labels");
	  return;
   }

     if (g_auto_man[1] == 'M') {
        max_y = graphic_scale[1][1];
        min_y = graphic_scale[1][0];
     }    
     else {
       for (j=1; j < GRAPHRANGES; j++) {
           if (*(graphic_range[j].col) == '\0') 
	       continue; 
           else 
	       num_cols++;
           row = 0;
       for (i=graphic_range[j].r1; i<=graphic_range[j].r2; i++) 
	   {
               
		p=lookat(i,graphic_range[j].c);
	 	if(!(p->flags & is_valid)) continue; 
                row++; 
                
		if (first_bar)
		{
		   max_y = p->v;      /*find the maximum y value*/
		   min_y = p->v;      /*find the minimum y value*/
		   first_bar = 0;
		}
		else
		{
		  max_y = Max(max_y, p->v);
		  min_y = Min(min_y, p->v);
		}
           /*---- num_rows = Max(row,num_rows); ---commented by Rama Devi---*/
	   }
           num_rows = Max(row,num_rows);  /*-- Rama Devi ----*/
        }
        min_y -= 0.1*(fabs(min_y)); /* so smallest bar shows */ 
     }
     min_y = floor(min_y);
     max_y = ceil(max_y);

    /*
     * Select colors for the border,  the window background,  and the
     * foreground.
     */

    /*---- Colors and fonts added by Rama Devi --*/
    cmap = DefaultColormap(dpy, DefaultScreen(dpy));
    if ((XAllocNamedColor(dpy, cmap, "Black", &exactBlack,
             &Black) != 0) && (XAllocNamedColor (dpy, cmap,
             "Orange", &exactOrange, &Orange) != 0))
      {
      bd = Orange.pixel;
      bg = Black.pixel;
      fg = Orange.pixel;
      }
    else 
      {
      bd = WhitePixel(dpy, DefaultScreen(dpy));
      bg = BlackPixel(dpy, DefaultScreen(dpy));
      fg = WhitePixel(dpy, DefaultScreen(dpy));
      }

  if (userfont == NULL)
     curfont = XLoadQueryFont(dpy, SC_FONT);
  else
     curfont = XLoadQueryFont(dpy, userfont);
  if (curfont == NULL)
  {
    fprintf(stderr, "%s: Display %s doesn't know font \"%s\" \n",
          progname, DisplayString(dpy), userfont == NULL ? SC_FONT : userfont);
    exit(1);
  }
  /* initialize the font-related globals */
  /* usefont(curfont); */
  curfontid = curfont->fid;
  curfontheight = curfont->max_bounds.ascent + curfont->max_bounds.descent;
  curfontwidth = curfont->max_bounds.width;

    /*
     * Set the border width of the window,  and the gap between the text
     * and the edge of the window, "pad".
     */

    pad = BORDER;
    bw = 1;

    /*
     * Deal with providing the window with an initial position & size.
     * Fill out the XSizeHints struct to inform the window manager. See
     * Sections 9.1.6 & 10.3.
     */

    xsh.width = XTextWidth(curfont, TITLE, 80 + pad * 2);   
    xsh.flags = (PPosition | PSize);
    xsh.height = WIN_H;
    xsh.width =  WIN_W; 
    xsh.x = (DisplayWidth(dpy, DefaultScreen(dpy)) - xsh.width) / 2;
    xsh.y = (DisplayHeight(dpy, DefaultScreen(dpy)) - xsh.height) / 2;

    /*
     * Create the Window with the information in the XSizeHints, the
     * border width,  and the border & background pixels. See Section 3.3.
     */

    win = XCreateSimpleWindow(dpy, DefaultRootWindow(dpy),
			      xsh.x, xsh.y, xsh.width, xsh.height,
                            /*---  bw, bd, bg); ---*/
                              bw, fg, bg);

    /*
     * Set the standard properties for the window managers. See Section
     * 9.1.
     */

    XSetStandardProperties(dpy, win, TITLE, TITLE, None, argv,
			   argc, &xsh);
    XSetWMHints(dpy, win, &Xwmh);

    /*
     * Ensure that the window's colormap field points to the default
     * colormap,  so that the window manager knows the correct colormap to
     * use for the window.  See Section 3.2.9. Also,  set the window's Bit
     * Gravity to reduce Expose events.
     */

    xswa.colormap = DefaultColormap(dpy, DefaultScreen(dpy));
    xswa.bit_gravity = CenterGravity;
    XChangeWindowAttributes(dpy, win, (CWColormap | CWBitGravity), &xswa);

/*--- The following statement is added by Rama Devi ---*/
    cmap = DefaultColormap(dpy, DefaultScreen(dpy));
    if ((XAllocNamedColor (dpy, cmap, "Orange", &exactOrange, &Orange) != 0))
       {
       maingcreversed = XCreateGC(dpy, win, 0,0);
       XSetFont(dpy, maingcreversed, curfontid);
       XSetForeground (dpy, maingcreversed, Orange.pixel);
       XSetBackground (dpy, maingcreversed, Black.pixel);
       }
    else
       {
       maingcreversed = XCreateGC(dpy, win, 0,0);
       XSetFont(dpy, maingcreversed, curfontid);
       XSetForeground (dpy, maingcreversed, fg);
       XSetBackground (dpy, maingcreversed, bg);
       }  /*---*/

    /*
     * Map the window to make it visible.  See Section 3.5.
     */
   
     XMapWindow(dpy, win);


     /*
     * Find out how big the window is now,  so that we can center
     * the text in it.
     */

    if (XGetWindowAttributes(dpy, win, &xwa) == 0){
       fprintf(stderr,"Bar Plot: Can't access attributes for graph window\n");
       exit(1);	
    }

    XClearWindow(dpy, win);
    XSelectInput(dpy,win, ButtonPressMask|KeyPressMask|ExposureMask);


    while (looping) {
	    XEvent event;

	    XNextEvent(dpy,&event);

	    switch(event.type)
	    {
		case KeyPress:
		case ButtonPress: looping = 0;
				  break;
		case Expose: draw = 1;
	    }
		       

	   if (!draw) continue;

           XDrawRectangle(dpy,win,maingcreversed,
			  RECT_X,RECT_Y,RECT_W,RECT_H);
     
 /* ----------------------------------------------------------------  */
 /* This next section will draw the grid on which the bar graphs will */
 /* be drawn and will display the title of the graph as well as       */
 /* label the x and y axis. Written by Georgiane Kondich    Aug 1990  */
 /*                                                                   */
 /* Modified 12/10/91 - Dan Gruber.  This graph now displays a value  */
 /*                     for each tick mark on the Y Axis.             */
 /* ----------------------------------------------------------------- */
          /*
	   * grid_l_x and grid_l_y are the length of the grid lines  
	   * to be drawn.  They have a value of '0' if no grids are 
	   * drawn.
           */

           if  (graphic_grid == 'H') 
           {
             grid_l_x = RECT_W;
             grid_l_y = 0;
           }
           else if (graphic_grid == 'V')
           {
             grid_l_x = 0;
             grid_l_y = RECT_H;
           }  
           else if (graphic_grid == 'B')
           {
             grid_l_x = RECT_W;
             grid_l_y = RECT_H;
           }  
           else 
           {
             grid_l_x = 0;
             grid_l_y = 0;
           }  

           grid_h = RECT_H /10;

	   Y_Range = max_y - min_y;

         /* Calculate the width of a single bar */
	   width = (RECT_W - (PIX_SEP*num_rows)) / (num_cols*num_rows);
						  /*^^^^^^^ we can draw
						    multiple bars on one
						    graph */
	 
	 /* Draw the grid, along with the labels on the Y axis */

	   for (i=0; i<11; i++) {
             if (i > 0 && i < 10)
	        XDrawLine(dpy,win,maingcreversed,
		       RECT_X-4, RECT_Y+i*grid_h,
		       RECT_X, RECT_Y+i*grid_h);
	     XDrawLine(dpy,win,maingcreversed,
		       RECT_X, RECT_Y+i*grid_h,
		       RECT_X+grid_l_x, RECT_Y+i*grid_h);
	     XDrawLine(dpy,win,maingcreversed,
		       RECT_X + RECT_W,RECT_Y+i*grid_h,
		       RECT_X+RECT_W - grid_l_x, RECT_Y+i*grid_h);

             sprintf(str,"%.1f",(max_y - Y_Range*i/10));
             len = strlen(str);
             text_w = XTextWidth(curfont,str,len+2);
             XDrawImageString(dpy,win,maingcreversed,
                        RECT_X - text_w,
                        RECT_Y + i*grid_h + curfontheight/3,
                        str,len);
           
	   } /*end for loop*/
           
           for (i=1; i < num_rows; i++) { 
             x = RECT_X + PIX_SEP*i + i*width*(num_cols);
	     XDrawLine(dpy,win,maingcreversed,
		       x, RECT_Y, x, RECT_Y+grid_l_y);
	     XDrawLine(dpy,win,maingcreversed,
		       x, RECT_Y+RECT_H, x, RECT_Y+RECT_H-grid_l_y);
	   } /*end for loop*/

/*
 *
 * Display titles at top of graph 
 *
 */

	   len = strlen(graphic_title[0]);    
	   text_w = XTextWidth(curfont, graphic_title[0], len);
	   XDrawImageString(dpy,win,maingcreversed, 
			    (WIN_W - text_w)/2, RECT_Y/3, 
			    graphic_title[0], len);
	 

	   len = strlen(graphic_title[1]);
	   text_w = XTextWidth(curfont, graphic_title[1],len);
	   XDrawImageString(dpy,win,maingcreversed, 
			   (WIN_W - text_w)/2, RECT_Y*2/3,
			   graphic_title[1],len);

	   len = strlen(graphic_title[2]);
	   text_w=XTextWidth(curfont,graphic_title[2],len);
	   XDrawImageString(dpy,win,maingcreversed, 
			    (WIN_W-text_w)/2, RECT_H+RECT_Y*3/2,
			    graphic_title[2],len);

           len = strlen(graphic_title[3]);
           text_w=XTextWidth(curfont,graphic_title[3],len);
           if ((RECT_X-text_w) < 0)
               x = curfontwidth;
           else
               x = RECT_X-text_w;
           XDrawImageString(dpy,win,maingcreversed,
                            x, RECT_Y - curfontheight,
                            graphic_title[3],len);


  /* --------------------------------------------------------------------*/
  /* This next section will read values of the x and y center points     */
  /* and find the maximum and minimum values for each, then set the      */
  /* width and height paramters, convert x and y so that the origin      */
  /* is at the lower left corner of the window, and then finally         */
  /* draw and fill the bars.                                             */
  /* ------------------------------------------------------------------- */
  




/*
 *  Draw Labels 
 */
           labelsize = irint(floor( (width*num_cols)/(double)curfontwidth) -1);
	   y = RECT_Y + RECT_H + curfontheight;
	   bars = 0;
	   for (i=graphic_range[0].r1;i<=graphic_range[0].r2; i++)
	   {
	      p = lookat(i,graphic_range[0].c);
	      if (!(p->label)) continue;
	      bars++;
	      strncpy(str, p->label, labelsize);
              str[labelsize] = '\0';
              len = strlen(str);
              text_w = XTextWidth(curfont, str, len);
	      x = RECT_X+(PIX_SEP)*(bars-1)+width*(bars-1)*num_cols;
              x = x + (width*num_cols/2)-(text_w / 2); 
	      XDrawImageString(dpy,win,maingcreversed,
		   	       x,y,
			       str,strlen(str));
	   }
     /*
      * Now draw all of the bars in the graph with a different 
      * pattern for each column of data.
      *
      */

        for(i = 1; i < GRAPHRANGES; i++){
            if (*(graphic_range[i].col) == '\0') continue;
            row = 0;

	/*-----for(j=graphic_range[0].r1,k=graphic_range[i].r1;
	   (j<=graphic_range[0].r2) && (k<=graphic_range[i].r2); j++, k++) 
	{ ----------*/

        /*------ The  above three lines are is commented by Rama devi --------

        if the graphic_range[i].r1 is greater than graphic_range[0].r1 and the
        difference between graphic_range[i].r1 and graphic_range[i].r2 is
        less than the difference between graphic_range[0].r1 and graphic_
        range[0].r2 then the loop is executed only the difference between 
        graphic_range[0].r1 and graphic_range[0].r2 times and comes out without
        drawing the graphs for all the specified ranges
        _____________________________________________________*/
        /* --------The follwoing lines added by Rama Devi -------*/

         for (j = graphic_range[0].r1; j<=graphic_range[1].r2; j++)

              {
              /*---- row++; -- commented by Rama Devi---*/
	      if (!((p=lookat(j,graphic_range[0].c))->label)) continue;
                 
              row++; /*-----line brought down by Rama Devi ----- */
              x = RECT_X+(row-1)*(PIX_SEP)+(row-1)*(num_cols)*width+width*(i-1);

        /*------- The followin two lines commented by Rama Devi ---

	-----if (!((p=lookat(j,graphic_range[i].c))->flags & is_valid) 
	      || (p->v < min_y) || (p->v > max_y)) continue;-----------*/

/*-------- The following lines added by Rama Devi ----------------*/
             if ((j >= graphic_range[i].r1) && (j <= graphic_range[i].r2))
                    {
              p = lookat(j, graphic_range[i].c);
              if (!(p->flags & is_valid)) continue;
/*-----------------------------------------------------------*/

              /*--------commented by Rama Devi -----------
              ------ if (!((p=lookat(j,graphic_range[i].c))->flags & is_valid)
              || (p->v < min_y) || (p->v > max_y)) continue; ----------*/

	      height = irint((p->v - min_y) / Y_Range * RECT_H);

	      y = RECT_Y + RECT_H - height;  

	      /* If there are grids to be drawn, clear the area before
	       * the bar is drawn .
               */ 
              if (graphic_grid != 'C')
                  XClearArea(dpy,win,x,y,width,height,False);

              /* Choose a different pattern for each column */
	      switch (i) {

        case 1 : Pattern = XCreateBitmapFromData(dpy,win,dark_wide_weave_bits,
                     dark_wide_weave_width, dark_wide_weave_height);

    /*---- The following two statements added by Rama Devi ----*/
    cmap = DefaultColormap(dpy, DefaultScreen(dpy));
    if ((XAllocNamedColor (dpy, cmap, "Green", &exactGreen, &Green) != 0))
       {
       XSetForeground (dpy, maingcreversed, Green.pixel);
       XSetBackground (dpy, maingcreversed, Black.pixel);
       }
    else
       {
       XSetForeground (dpy, maingcreversed, fg);
       XSetBackground (dpy, maingcreversed, bg);
       }
                             break;

        case 2: Pattern = XCreateBitmapFromData(dpy,win,dark_root_weave_bits,
                     dark_root_weave_width, dark_root_weave_height);

    /*---- The following two statements added by Rama Devi ----*/
    cmap = DefaultColormap(dpy, DefaultScreen(dpy));
    if ((XAllocNamedColor (dpy, cmap, "Orchid", &exactOrchid, &Orchid) != 0))
       {
       XSetForeground (dpy, maingcreversed, Orchid.pixel);
       }
    else
       {
       XSetForeground (dpy, maingcreversed, fg);
       }

                             break;

       case 3: Pattern = XCreateBitmapFromData(dpy,win,dark_cross_weave_bits,
                     dark_cross_weave_width, dark_cross_weave_height);

    /*---- The following two statements added by Rama Devi ----*/
    cmap = DefaultColormap(dpy, DefaultScreen(dpy));
    if ((XAllocNamedColor (dpy, cmap, "GreenYellow", &exactGreenYellow, &GreenYellow) != 0))
       {
       XSetForeground (dpy, maingcreversed, GreenYellow.pixel);
       }
    else
       {
       XSetForeground (dpy, maingcreversed, fg);
       }

                             break;

        case 4: Pattern = XCreateBitmapFromData(dpy,win,light_wide_weave_bits,
                     light_wide_weave_width, light_wide_weave_height);

    /*---- The following two statements added by Rama Devi ----*/
    cmap = DefaultColormap(dpy, DefaultScreen(dpy));
    if ((XAllocNamedColor (dpy, cmap, "Goldenrod", &exactGoldenrod, &Goldenrod) != 0))
       {
       XSetForeground (dpy, maingcreversed, Goldenrod.pixel);
       }
    else
       {
       XSetForeground (dpy, maingcreversed, fg);
       }
  
                             break;

       case 5 : Pattern = XCreateBitmapFromData(dpy,win,light_root_weave_bits,
                     light_root_weave_width, light_root_weave_height);

    /*---- The following two statements added by Rama Devi ----*/
    cmap = DefaultColormap(dpy, DefaultScreen(dpy));
    if ((XAllocNamedColor (dpy, cmap, "Aquamarine", &exactAquamarine, &Aquamarine) != 0))
       {
       XSetForeground (dpy, maingcreversed, Aquamarine.pixel);
       }
    else
       {
       XSetForeground (dpy, maingcreversed, fg);
       }
 
                             break;

        case 6: Pattern = XCreateBitmapFromData(dpy,win,light_cross_weave_bits,
                     light_cross_weave_width, light_cross_weave_height);

    /*---- The following two statements added by Rama Devi ----*/
    cmap = DefaultColormap(dpy, DefaultScreen(dpy));
    if ((XAllocNamedColor (dpy, cmap, "Yellow", &exactYellow, &Yellow) != 0))
       {
       XSetForeground (dpy, maingcreversed, Yellow.pixel);
       }
    else
       {
       XSetForeground (dpy, maingcreversed, fg);
       }
 
                             break; }

              /* Draw the bar with a filled pattern */
              XSetStipple(dpy, maingcreversed, Pattern);
              XSetFillStyle(dpy, maingcreversed, FillStippled);
              XFillRectangle(dpy,win,maingcreversed,x,y,width,height);

              /* Outline the bar with a solid rectangle */
              XSetFillStyle(dpy, maingcreversed, FillSolid);
	      XDrawRectangle(dpy,win,maingcreversed,x,y,width,height);
                       } /*-------*/
        }
     }

    /*---- The following two statements added by Rama Devi ----*/
    cmap = DefaultColormap(dpy, DefaultScreen(dpy));
    if ((XAllocNamedColor (dpy, cmap, "Orange", &exactOrange, &Orange) != 0))
       {
       XSetForeground (dpy, maingcreversed, Orange.pixel);
       XSetBackground (dpy, maingcreversed, Black.pixel);
       }
    else
       {
       XSetForeground (dpy, maingcreversed, fg);
       XSetBackground (dpy, maingcreversed, bg);
       }

   }


    /*---- The following statements added by Rama Devi ----*/
cmap = DefaultColormap (dpy, DefaultScreen (dpy));
if (backg && (XAllocNamedColor (dpy, cmap, backg, &exactOrange, &Orange) != 0))
    {
    bg = Orange.pixel;
    }
   else
    {
    bg = WhitePixel(dpy, DefaultScreen(dpy)); /* background */
    }
if (foreg && (XAllocNamedColor (dpy, cmap, foreg, &exactBlack, &Black) != 0))
    {
    fg = Black.pixel;
    }
   else
    {
    fg = BlackPixel(dpy, DefaultScreen(dpy)); /* foreground */
    }
      XSetForeground (dpy, maingc, fg);
      XSetBackground (dpy, maingc, bg);
      XSetForeground (dpy, maingcreversed, bg);
      XSetBackground (dpy, maingcreversed, fg);

XSetPlaneMask(dpy, invertgc, bg^fg);

   XUnmapWindow(dpy, win);
   XDestroyWindow(dpy, win);

}
#endif /* HAVE_X11_X_H	this code for now is X specific */
