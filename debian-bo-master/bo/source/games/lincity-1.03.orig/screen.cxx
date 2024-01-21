//   screen.cxx   part of lincity
// Copyright (c) I J Peters 1995,1996.  Please read the file 'COPYRIGHT'.

#include <stdio.h>
#include <stdlib.h>

#ifdef LC_X11
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>
#include "lcx11.h"
#else
#include <vga.h>
#include <vgagl.h>
#include <vgamouse.h>
#endif

#include <math.h>
#include <strings.h>
#include <unistd.h>
#include "lctypes.h"
#include "lin-city.h"
#include "screen.h"

#ifdef CS_PROFILE
void FPgl_write(int a,int b,char *s)
{
	gl_write(a,b,s);
}
void FPgl_getbox(int a,int b,int c,int d,void *e)
{
	gl_getbox(a,b,c,d,e);
}
void FPgl_putbox(int a,int b,int c,int d,void *e)
{
	gl_putbox(a,b,c,d,e);
}
void FPgl_fillbox(int a,int b,int c,int d,int e)
{
	gl_fillbox(a,b,c,d,e);
}
void FPgl_hline(int a,int b,int c,int d)
{
	gl_hline(a,b,c,d);
}
void FPgl_line(int a,int b,int c,int d,int e)
{
	gl_line(a,b,c,d,e);
}
void FPgl_setpixel(int a,int b,int c)
{
	gl_setpixel(a,b,c);
}
#endif

#ifdef	THMO_SCREEN_STUFF

  /* combined all check_*_graphics() functions into one... -- (ThMO) */

void
refresh_main_screen()
{
  static const short
    power_table[ 16]= {
	CST_POWERL_H_D,  CST_POWERL_V_D,   CST_POWERL_H_D,   CST_POWERL_RD_D,
	CST_POWERL_H_D,  CST_POWERL_LD_D,  CST_POWERL_H_D,   CST_POWERL_LDR_D,
	CST_POWERL_V_D,  CST_POWERL_V_D,   CST_POWERL_RU_D,  CST_POWERL_UDR_D,
	CST_POWERL_LU_D, CST_POWERL_LDU_D, CST_POWERL_LUR_D, CST_POWERL_LUDR_D
      },
    track_table[ 16]= {
	CST_TRACK_LR, CST_TRACK_LR,  CST_TRACK_UD,  CST_TRACK_LU,
	CST_TRACK_LR, CST_TRACK_LR,  CST_TRACK_UR,  CST_TRACK_LUR,
	CST_TRACK_UD, CST_TRACK_LD,  CST_TRACK_UD,  CST_TRACK_LUD,
	CST_TRACK_DR, CST_TRACK_LDR, CST_TRACK_UDR, CST_TRACK_LUDR
      },
    road_table[ 16]= {
	CST_ROAD_LR, CST_ROAD_LR,  CST_ROAD_UD,  CST_ROAD_LU,
	CST_ROAD_LR, CST_ROAD_LR,  CST_ROAD_UR,  CST_ROAD_LUR,
	CST_ROAD_UD, CST_ROAD_LD,  CST_ROAD_UD,  CST_ROAD_LUD,
	CST_ROAD_DR, CST_ROAD_LDR, CST_ROAD_UDR, CST_ROAD_LUDR
      },
    rail_table[ 16]= {
   	 CST_RAIL_LR, CST_RAIL_LR,  CST_RAIL_UD,  CST_RAIL_LU,
   	 CST_RAIL_LR, CST_RAIL_LR,  CST_RAIL_UR,  CST_RAIL_LUR,
   	 CST_RAIL_UD, CST_RAIL_LD,  CST_RAIL_UD,  CST_RAIL_LUD,
   	 CST_RAIL_DR, CST_RAIL_LDR, CST_RAIL_UDR, CST_RAIL_LUDR
      },
    water_table[ 16]= {
	CST_WATER,    CST_WATER_D,   CST_WATER_R,   CST_WATER_RD,
	CST_WATER_L,  CST_WATER_LD,  CST_WATER_LR,  CST_WATER_LRD,
	CST_WATER_U,  CST_WATER_UD,  CST_WATER_UR,  CST_WATER_URD,
	CST_WATER_LU, CST_WATER_LUD, CST_WATER_LUR, CST_WATER_LURD
      };

  if ( !( market_cb_flag || port_cb_flag))
    {
      int  x, y, mask, tflags, group, type;

      for ( x= main_screen_originx;
	    x < main_screen_originx+ MAIN_WIN_W/ 16;
	    x++)
	for ( y= main_screen_originy;
	      y < main_screen_originy+ MAIN_WIN_W/ 16;
	      y++)
	  switch ( main_types[ mappointtype[ x][ y]].group)
	    {
	      case GROUP_POWER_LINE:
		mask= 0;
#ifdef	THOMMY_MAY_BE_WRONG	/* just in case -- (ThMO) */
		if ( y > 0)
#endif
		  {	/* up -- (ThMO) */
		    group= main_types[ mappointtype[ x][ y- 1]].group;

		    /* see if dug under track, rail or road */

		    if ( y > 1 && ( group == GROUP_TRACK
				    || group == GROUP_RAIL
				    || group == GROUP_ROAD
				    || group == GROUP_WATER))
		      group= main_types[ mappointtype[ x][ y- 2]].group;
		    switch ( group)
		      {
#ifdef	notdef
			case GROUP_WINDMILL:
			  if ( mappoint[ x][ y].int_2 < MODERN_WINDMILL_TECH)
			    break;
#endif
			case GROUP_POWER_LINE:
			case GROUP_POWER_SOURCE:
			case GROUP_SUBSTATION:
			case GROUP_POWER_SOURCE_COAL:
			  mask |= 8;
			  break;
		      }
		  }
#ifdef	THOMMY_MAY_BE_WRONG	/* just in case -- (ThMO) */
		if ( x > 0)
#endif
		  {	/* left -- (ThMO) */
		    group= main_types[ mappointtype[ x- 1][ y]].group;
		    if ( x > 1 && ( group == GROUP_TRACK
				    || group == GROUP_RAIL
				    || group == GROUP_ROAD
				    || group == GROUP_WATER))
		      group= main_types[ mappointtype[ x- 2][ y]].group;
		    switch ( group)
		      {
#ifdef	notdef
			case GROUP_WINDMILL:
			  if ( mappoint[ x][ y].int_2 < MODERN_WINDMILL_TECH)
			    break;
#endif
			case GROUP_POWER_LINE:
			case GROUP_POWER_SOURCE:
			case GROUP_SUBSTATION:
			case GROUP_POWER_SOURCE_COAL:
			  mask |= 4;
			  break;
		      }
		  }
#ifdef	THOMMY_MAY_BE_WRONG	/* just in case -- (ThMO) */
		if ( x < WORLD_SIDE_LEN- 1)
#endif
		  {	/* right -- (ThMO) */
		    group= main_types[ mappointtype[ x+ 1][ y]].group;
		    if ( x < WORLD_SIDE_LEN- 2 && ( group == GROUP_TRACK
						    || group == GROUP_RAIL
						    || group == GROUP_ROAD
						    || group == GROUP_WATER))
		      group= main_types[ mappointtype[ x+ 2][ y]].group;
		    switch ( group)
		      {
			case GROUP_WINDMILL:
			  if ( mappoint[ x+ 1][ y].int_2 < MODERN_WINDMILL_TECH)
			    break;
			case GROUP_POWER_LINE:
			case GROUP_POWER_SOURCE:
			case GROUP_SUBSTATION:
			case GROUP_POWER_SOURCE_COAL:
			  mask |= 2;
			  break;
		      }
		  }
#ifdef	THOMMY_MAY_BE_WRONG	/* just in case -- (ThMO) */
		if ( y < WORLD_SIDE_LEN- 1)
#endif
		  {	/* down -- (ThMO) */
		    group= main_types[ mappointtype[ x][ y+ 1]].group;
		    if ( y < WORLD_SIDE_LEN- 2 && ( group == GROUP_TRACK
						    || group == GROUP_RAIL
						    || group == GROUP_ROAD
						    || group == GROUP_WATER))
		      group= main_types[ mappointtype[ x][ y+ 2]].group;
		    switch ( group)
		      {
			case GROUP_WINDMILL:
			  if ( mappoint[ x][ y+ 1].int_2 < MODERN_WINDMILL_TECH)
			    break;
			case GROUP_POWER_LINE:
			case GROUP_POWER_SOURCE:
			case GROUP_SUBSTATION:
			case GROUP_POWER_SOURCE_COAL:
			  ++mask;
			  break;
		      }
		  }
		mappointtype[ x][ y]= power_table[ mask];
		if ( mappoint[ x][ y].int_1 != 0)
		  mappointtype[ x][ y] -= 11;
		break;

	      case GROUP_TRACK:
#if	FLAG_LEFT != 1 || FLAG_UP != 2 || FLAG_RIGHT != 4 || FLAG_DOWN != 8
  #error  check_track_graphics(): you loose
  #error  this algorithm depends on proper flag settings -- (ThMO)
#endif
		mask= 0;
#ifdef	THOMMY_MAY_BE_WRONG	/* just in case -- (ThMO) */
		if ( y > 0)
#endif
		  {
		    if ( main_types[ mappointtype[ x][ y- 1]].group
			 == GROUP_TRACK)
		      mask |= FLAG_UP;
		  }
#ifdef	THOMMY_MAY_BE_WRONG	/* just in case -- (ThMO) */
		if ( x > 0)
#endif
		  {
		    if ( main_types[ mappointtype[ x- 1][ y]].group
			 == GROUP_TRACK)
		      mask |= FLAG_LEFT;
		  }
		tflags= mask;
#ifdef	THOMMY_MAY_BE_WRONG	/* just in case -- (ThMO) */
		if ( x < WORLD_SIDE_LEN- 1)
#endif
		  {
		    switch ( main_types[ mappointtype[ x+ 1][ y]].group)
		      {
			case GROUP_TRACK:
			  tflags |= FLAG_RIGHT;
			case GROUP_COMMUNE:
			case GROUP_COALMINE:
			case GROUP_OREMINE:
			case GROUP_INDUSTRY_L:
			case GROUP_INDUSTRY_H:
			case GROUP_RECYCLE:
			case GROUP_TIP:
			case GROUP_PORT:
			  mask |= FLAG_RIGHT;
			  break;
			default:
#ifdef	THOMMY_MAY_BE_WRONG	/* just in case -- (ThMO) */
			  if ( y > 0)
#endif
			    if ( main_types[ mappointtype[ x+ 1][ y- 1]].group
				 == GROUP_POWER_SOURCE_COAL)
			      mask |= FLAG_RIGHT;
			  break;
		      }
		  }
#ifdef	THOMMY_MAY_BE_WRONG	/* just in case -- (ThMO) */
		if ( y < WORLD_SIDE_LEN- 1)
#endif
		  {
		    switch ( main_types[ mappointtype[ x][ y+ 1]].group)
		      {
			case GROUP_TRACK:
			  tflags |= FLAG_DOWN;
			case GROUP_COMMUNE:
			case GROUP_COALMINE:
			case GROUP_OREMINE:
			case GROUP_INDUSTRY_L:
			case GROUP_INDUSTRY_H:
			case GROUP_RECYCLE:
			case GROUP_TIP:
			case GROUP_PORT:
			  mask |= FLAG_DOWN;
			  break;
			default:
#ifdef	THOMMY_MAY_BE_WRONG	/* just in case -- (ThMO) */
			  if ( x > 0)
#endif
			    if ( main_types[ mappointtype[ x- 1][ y+ 1]].group
				 == GROUP_POWER_SOURCE_COAL)
			      mask |= FLAG_DOWN;
			  break;
		      }
		  }
		mappoint[ x][ y].flags &= ~( FLAG_UP | FLAG_DOWN | FLAG_LEFT
					     | FLAG_RIGHT);
		mappoint[ x][ y].flags |= tflags;
		mappointtype[ x][ y]= track_table[ mask];
		break;

	      case GROUP_ROAD:
#if	FLAG_LEFT != 1 || FLAG_UP != 2 || FLAG_RIGHT != 4 || FLAG_DOWN != 8
  #error  check_road_graphics(): you loose
  #error  this algorithm depends on proper flag settings -- (ThMO)
#endif
		mask= 0;
#ifdef	THOMMY_MAY_BE_WRONG	/* just in case -- (ThMO) */
		if ( y > 0)
#endif
		  {
		    if ( main_types[ mappointtype[ x][ y- 1]].group
			 == GROUP_ROAD)
		      mask |= FLAG_UP;
		  }
#ifdef	THOMMY_MAY_BE_WRONG	/* just in case -- (ThMO) */
		if ( x > 0)
#endif
		  {
		    if ( main_types[ mappointtype[ x- 1][ y]].group
			 == GROUP_ROAD)
		      mask |= FLAG_LEFT;
		  }
		tflags= mask;
#ifdef	THOMMY_MAY_BE_WRONG	/* just in case -- (ThMO) */
		if ( x < WORLD_SIDE_LEN- 1)
#endif
		  {
		    switch ( main_types[ mappointtype[ x+ 1][ y]].group)
		      {
			case GROUP_ROAD:
			  tflags |= FLAG_RIGHT;
			case GROUP_COMMUNE:
			case GROUP_COALMINE:
			case GROUP_OREMINE:
			case GROUP_INDUSTRY_L:
			case GROUP_INDUSTRY_H:
			case GROUP_RECYCLE:
			case GROUP_TIP:
			case GROUP_PORT:
			  mask |= FLAG_RIGHT;
			  break;
			default:
#ifdef	THOMMY_MAY_BE_WRONG	/* just in case -- (ThMO) */
			  if ( y > 0)
#endif
			    if ( main_types[ mappointtype[ x+ 1][ y- 1]].group
				 == GROUP_POWER_SOURCE_COAL)
			      mask |= FLAG_RIGHT;
			  break;
		      }
		  }
#ifdef	THOMMY_MAY_BE_WRONG	/* just in case -- (ThMO) */
		if ( y < WORLD_SIDE_LEN- 1)
#endif
		  {
		    switch ( main_types[ mappointtype[ x][ y+ 1]].group)
		      {
			case GROUP_ROAD:
			  tflags |= FLAG_DOWN;
			case GROUP_COMMUNE:
			case GROUP_COALMINE:
			case GROUP_OREMINE:
			case GROUP_INDUSTRY_L:
			case GROUP_INDUSTRY_H:
			case GROUP_RECYCLE:
			case GROUP_TIP:
			case GROUP_PORT:
			  mask |= FLAG_DOWN;
			  break;
			default:
#ifdef	THOMMY_MAY_BE_WRONG	/* just in case -- (ThMO) */
			  if ( x > 0)
#endif
			    if ( main_types[ mappointtype[ x- 1][ y+ 1]].group
				 == GROUP_POWER_SOURCE_COAL)
			      mask |= FLAG_DOWN;
			  break;
		      }
		  }
		mappoint[ x][ y].flags &= ~( FLAG_UP | FLAG_DOWN | FLAG_LEFT
					     | FLAG_RIGHT);
		mappoint[ x][ y].flags |= tflags;
		mappointtype[ x][ y]= road_table[ mask];
		break;

	      case GROUP_RAIL:
#if	FLAG_LEFT != 1 || FLAG_UP != 2 || FLAG_RIGHT != 4 || FLAG_DOWN != 8
  #error  check_rail_graphics(): you loose
  #error  this algorithm depends on proper flag settings -- (ThMO)
#endif
		mask= 0;
#ifdef	THOMMY_MAY_BE_WRONG	/* just in case -- (ThMO) */
		if ( y > 0)
#endif
		  {
		    if ( main_types[ mappointtype[ x][ y- 1]].group
			 == GROUP_RAIL)
		      mask |= FLAG_UP;
		  }
#ifdef	THOMMY_MAY_BE_WRONG	/* just in case -- (ThMO) */
		if ( x > 0)
#endif
		  {
		    if ( main_types[ mappointtype[ x- 1][ y]].group
			 == GROUP_RAIL)
		      mask |= FLAG_LEFT;
		  }
		tflags= mask;
#ifdef	THOMMY_MAY_BE_WRONG	/* just in case -- (ThMO) */
		if ( x < WORLD_SIDE_LEN- 1)
#endif
		  {
		    switch ( main_types[ mappointtype[ x+ 1][ y]].group)
		      {
			case GROUP_RAIL:
			  tflags |= FLAG_RIGHT;
			case GROUP_COMMUNE:
			case GROUP_COALMINE:
			case GROUP_OREMINE:
			case GROUP_INDUSTRY_L:
			case GROUP_INDUSTRY_H:
			case GROUP_RECYCLE:
			case GROUP_TIP:
			case GROUP_PORT:
			  mask |= FLAG_RIGHT;
			  break;
			default:
#ifdef	THOMMY_MAY_BE_WRONG	/* just in case -- (ThMO) */
			  if ( y > 0)
#endif
			    if ( main_types[ mappointtype[ x+ 1][ y- 1]].group
				 == GROUP_POWER_SOURCE_COAL)
			      mask |= FLAG_RIGHT;
			  break;
		      }
		  }
#ifdef	THOMMY_MAY_BE_WRONG	/* just in case -- (ThMO) */
		if ( y < WORLD_SIDE_LEN- 1)
#endif
		  {
		    switch ( main_types[ mappointtype[ x][ y+ 1]].group)
		      {
			case GROUP_RAIL:
			  tflags |= FLAG_DOWN;
			case GROUP_COMMUNE:
			case GROUP_COALMINE:
			case GROUP_OREMINE:
			case GROUP_INDUSTRY_L:
			case GROUP_INDUSTRY_H:
			case GROUP_RECYCLE:
			case GROUP_TIP:
			case GROUP_PORT:
			  mask |= FLAG_DOWN;
			  break;
			default:
#ifdef	THOMMY_MAY_BE_WRONG	/* just in case -- (ThMO) */
			  if ( x > 0)
#endif
			    if ( main_types[ mappointtype[ x- 1][ y+ 1]].group
				 == GROUP_POWER_SOURCE_COAL)
			      mask |= FLAG_DOWN;
			  break;
		      }
		  }
		mappoint[ x][ y].flags &= ~( FLAG_UP | FLAG_DOWN | FLAG_LEFT
					     | FLAG_RIGHT);
		mappoint[ x][ y].flags |= tflags;
		mappointtype[ x][ y]= rail_table[ mask];
		break;

	      case GROUP_WATER:
		mask= 0;
#ifdef	THOMMY_MAY_BE_WRONG	/* just in case -- (ThMO) */
		if ( y > 0)
#endif
		  {	/* up -- (ThMO) */
		    if ( main_types[ mappointtype[ x][ y- 1]].group
			 == GROUP_WATER)
		      mask |= 8;
		  }
#ifdef	THOMMY_MAY_BE_WRONG	/* just in case -- (ThMO) */
		if ( x > 0)
#endif
		  {	/* left -- (ThMO) */
		    type= mappointtype[ x- 1][ y];
		    if ( ( type == CST_USED
			   && main_types[ mappointtype[ mappoint[ x- 1][ y]
							.int_1]
						      [ mappoint[ x- 1][ y]
							.int_2]
					].group == GROUP_EX_PORT)
			 || main_types[ type].group == GROUP_WATER)
		      mask |= 4;
		  }
#ifdef	THOMMY_MAY_BE_WRONG	/* just in case -- (ThMO) */
		if ( x < WORLD_SIDE_LEN- 1)
#endif
		  {	/* right -- (ThMO) */
		    if ( main_types[ mappointtype[ x+ 1][ y]].group
			 == GROUP_WATER)
		      mask |= 2;
		  }
#ifdef	THOMMY_MAY_BE_WRONG	/* just in case -- (ThMO) */
		if ( y < WORLD_SIDE_LEN- 1)
#endif
		  {	/* down -- (ThMO) */
		    if ( main_types[ mappointtype[ x][ y+ 1]].group
			 == GROUP_WATER)
		      ++mask;
		  }
		mappointtype[ x][ y]= water_table[ mask];
		break;
	    }

      screen_refresh_flag= 1;
      update_main_screen();
      update_mini_screen();
    }
}

/*static INLINE*/ void	/* worth inlining -- (ThMO) */
clip_main_window()
{
  Fgl_enableclipping();
  Fgl_setclippingwindow( MAIN_WIN_X, MAIN_WIN_Y, MAIN_WIN_X+ MAIN_WIN_W- 1,
			 MAIN_WIN_Y+ MAIN_WIN_H- 1);
}

/*static INLINE*/ void	/* worth inlining -- (ThMO) */
unclip_main_window()
{
  Fgl_disableclipping();
}

#else	/* !defined( THMO_SCREEN_STUFF) */

void refresh_main_screen(void)
{
	if (market_cb_flag || port_cb_flag)
		return;
        check_track_graphics();
        check_rail_graphics();
        check_road_graphics();
	check_water_graphics();
	check_power_line_graphics();
	screen_refresh_flag=1;
	update_main_screen();
	update_mini_screen();
}

#endif

void update_main_screen(void)
{
	int x,y,t1,t2,xm,ym;
#ifdef USE_PIXMAPS
	int sx,sy,dx,dy,x1,y1;
#endif
//  main_screen_origin[x|y] contain the mappoint of the top left of win
#ifdef DEBUG_MAIN_SCREEN
	printf("Updating main screen\n");
#endif
	if (help_flag || load_flag || save_flag)
		return;
	xm=main_screen_originx;
	if (xm>3)
		xm=3;
	ym=main_screen_originy;
	if (ym>3)
		ym=3;
	if (mouse_type==MOUSE_TYPE_SQUARE)
		hide_mouse();
	clip_main_window();
	for (y=main_screen_originy-ym;y<main_screen_originy
			+(MAIN_WIN_H/16);y++)
		for (x=main_screen_originx-xm;x<main_screen_originx
				+(MAIN_WIN_W/16);x++)
		{
			if ((t1=mappointoldtype[x][y])
				!=(t2=mappointtype[x][y])
				|| (screen_refresh_flag!=0
				&& mappointtype[x][y]!=CST_USED))
			{
				mappointoldtype[x][y]=mappointtype[x][y];
#ifdef USE_PIXMAPS
				if (icon_pixmap[t2]!=0)
				{
//printf("x=%d y=%d XCA %x %x %x\n",x,y
//,display.dpy,icon_pixmap[t2],display.win);
			x1=y1=0;
			if (x<main_screen_originx)
				x1=(main_screen_originx-x)*16;
			if (y<main_screen_originy)
				y1=(main_screen_originy-y)*16;
			sx=sy=main_types[t2].size;
			if ((sx+x)>(main_screen_originx+(MAIN_WIN_W/16)))
				sx=(main_screen_originx+(MAIN_WIN_W/16))-x;
			if ((sy+y)>(main_screen_originy+(MAIN_WIN_H/16)))
				sy=(main_screen_originy+(MAIN_WIN_H/16))-y;
			sx=(sx<<4)-x1;
			sy=(sy<<4)-y1;
			dx=MAIN_WIN_X+(x-main_screen_originx)*16+x1;
			dy=MAIN_WIN_Y+(y-main_screen_originy)*16+y1;
			if (sx>0 && sy>0)
			{
#ifdef ALLOW_PIX_DOUBLING
			if (pix_double)
				XCopyArea(display.dpy
					,icon_pixmap[t2]
					,display.win
					,display.pixcolour_gc[0]
					,x1*2,y1*2,sx*2,sy*2
					,dx*2,dy*2);
				else
#endif
				XCopyArea(display.dpy
					,icon_pixmap[t2]
					,display.win
					,display.pixcolour_gc[0]
					,x1,y1,sx,sy
					,dx+borderx,dy+bordery);
				update_pixmap(x1,y1,sx,sy,dx,dy
					,main_types[t2].size
					,main_types[t2].graphic);
			}
				}
				else
#endif
					Fgl_putbox(MAIN_WIN_X
					+(x-main_screen_originx)*16
					,MAIN_WIN_Y
					+(y-main_screen_originy)*16
					,16*main_types[t2].size
					,16*main_types[t2].size
					,main_types[t2].graphic);
			}
		}
	unclip_main_window();
	screen_refresh_flag=0;
	if (mouse_type==MOUSE_TYPE_SQUARE)
		redraw_mouse();
}

#ifndef THMO_SCREEN_STUFF

void clip_main_window(void)
{
	Fgl_enableclipping();
	Fgl_setclippingwindow(MAIN_WIN_X,MAIN_WIN_Y
		,MAIN_WIN_X+MAIN_WIN_W-1,MAIN_WIN_Y+MAIN_WIN_H-1);
}

void unclip_main_window(void)
{
	Fgl_disableclipping();
}

void check_power_line_graphics(void)
{
	int x,y,t,u,l,r,d,tu,tl,tr,td;
        for (y=main_screen_originy;y<main_screen_originy
			+(MAIN_WIN_W/16);y++)
                for (x=main_screen_originx
			;x<main_screen_originx+(MAIN_WIN_W/16);x++)
		{
			t=mappointtype[x][y];
			if (main_types[t].group==GROUP_POWER_LINE)
			{
				tu=tl=tr=td=CST_GREEN;
				if (y>0)
				{
					tu=mappointtype[x][y-1];
			//  see if dug under track, rail or road
					if (y>1 && (main_types[tu].group
						==GROUP_TRACK
					|| main_types[tu].group==GROUP_RAIL
					|| main_types[tu].group==GROUP_ROAD
					|| main_types[tu].group==GROUP_WATER))
					tu=mappointtype[x][y-2];
				}
				if (x>0)
				{
					tl=mappointtype[x-1][y];
					if (x>1 && (main_types[tl].group
						==GROUP_TRACK
					|| main_types[tl].group==GROUP_RAIL
					|| main_types[tl].group==GROUP_ROAD
					|| main_types[tl].group==GROUP_WATER))
					tl=mappointtype[x-2][y];
				}
				if (x<(WORLD_SIDE_LEN-1))
				{
					tr=mappointtype[x+1][y];
					if (x<(WORLD_SIDE_LEN-2)
					&& (main_types[tr].group==GROUP_TRACK
					|| main_types[tr].group==GROUP_RAIL
					|| main_types[tr].group==GROUP_ROAD
					|| main_types[tr].group==GROUP_WATER))
					tr=mappointtype[x+2][y];
				}
				if (y<(WORLD_SIDE_LEN-1))
				{
					td=mappointtype[x][y+1];
					if (y<(WORLD_SIDE_LEN-2)
					&& (main_types[td].group==GROUP_TRACK
					|| main_types[td].group==GROUP_RAIL
					|| main_types[td].group==GROUP_ROAD
					|| main_types[td].group==GROUP_WATER))
					td=mappointtype[x][y+2];
				}
				u=l=r=d=0;
				if (main_types[tu].group==GROUP_POWER_LINE
				|| main_types[tu].group==GROUP_POWER_SOURCE
				|| main_types[tu].group==GROUP_SUBSTATION
//			|| (main_types[tu].group==GROUP_WINDMILL
//			&& mappoint[x][y].int_2>=MODERN_WINDMILL_TECH)
			|| main_types[tu].group==GROUP_POWER_SOURCE_COAL)
					u=1;
                                if (main_types[tl].group==GROUP_POWER_LINE
                                || main_types[tl].group==GROUP_POWER_SOURCE
				|| main_types[tl].group==GROUP_SUBSTATION
//			|| (main_types[tl].group==GROUP_WINDMILL
//			&& mappoint[x][y].int_2>=MODERN_WINDMILL_TECH)
			|| main_types[tl].group==GROUP_POWER_SOURCE_COAL)
                                        l=1;
                                if (main_types[tr].group==GROUP_POWER_LINE
                                || main_types[tr].group==GROUP_POWER_SOURCE
				|| main_types[tr].group==GROUP_SUBSTATION
			|| (main_types[tr].group==GROUP_WINDMILL
			&& mappoint[x+1][y].int_2>=MODERN_WINDMILL_TECH)
			|| main_types[tr].group==GROUP_POWER_SOURCE_COAL)
                                        r=1;
                                if (main_types[td].group==GROUP_POWER_LINE
                                || main_types[td].group==GROUP_POWER_SOURCE
				|| main_types[td].group==GROUP_SUBSTATION
			|| (main_types[td].group==GROUP_WINDMILL
			&& mappoint[x][y+1].int_2>=MODERN_WINDMILL_TECH)
			|| main_types[td].group==GROUP_POWER_SOURCE_COAL)
                                        d=1;
				mappointtype[x][y]
					=select_power_line_type(u,l,r,d);
// do the live powerline stuff
				if (mappoint[x][y].int_1!=0)
					mappointtype[x][y]-=11;
  			}
		}
}

int select_power_line_type(int u,int l,int r,int d)
{
	if (u && l && r && d)
		return(CST_POWERL_LUDR_D);
	if (u && d && r)
		return(CST_POWERL_UDR_D);
	if (l && u && r)
		return(CST_POWERL_LUR_D);
	if (l && d && r)
		return(CST_POWERL_LDR_D);
	if (l && d && u)
		return(CST_POWERL_LDU_D);
	if (r && u)
		return(CST_POWERL_RU_D);
	if (l && u)
		return(CST_POWERL_LU_D);
	if (r && d)
		return(CST_POWERL_RD_D);
	if (l && d)
		return(CST_POWERL_LD_D);
	if (u || d)
		return(CST_POWERL_V_D);
	return(CST_POWERL_H_D);
}


void check_track_graphics(void)
{
	int x,y,t,u,l,r,d,tu,tl,tr,trc,td,tdc,trc2,trc3;
	tu=tl=tr=trc=td=tdc=trc2=trc3=0;
        for (y=main_screen_originy;y<main_screen_originy
			+(MAIN_WIN_W/16);y++)
                for (x=main_screen_originx
			;x<main_screen_originx+(MAIN_WIN_W/16);x++)
		{
			t=mappointtype[x][y];
			if (main_types[t].group==GROUP_TRACK)
			{
				tu=tl=tr=td=trc=trc2=trc3=CST_GREEN;
				if (y>0)
					tu=mappointtype[x][y-1];
				if (x>0)
					tl=mappointtype[x-1][y];
				if (x<(WORLD_SIDE_LEN-1))
				{
					tr=mappointtype[x+1][y];
					if (y>0)
						trc=mappointtype[x+1][y-1];
					if (y>1)
						trc2=mappointtype[x+1][y-2];
					if (y>2)
						trc3=mappointtype[x+1][y-3];
				}
				if (y<(WORLD_SIDE_LEN-1))
				{
					td=mappointtype[x][y+1];
					if (x>0)
						tdc=mappointtype[x-1][y+1];
				}
				u=l=r=d=0;
				mappoint[x][y].flags &= 0xfffffff0;
				if (main_types[tu].group==GROUP_TRACK)
				{
					u=1;
					mappoint[x][y].flags |= FLAG_UP;
				}
                                if (main_types[tl].group==GROUP_TRACK)
				{
					l=1;
					mappoint[x][y].flags |= FLAG_LEFT;
				}
				if (main_types[tr].group==GROUP_TRACK)
					mappoint[x][y].flags |= FLAG_RIGHT;
                                if (main_types[tr].group==GROUP_TRACK
				|| main_types[tr].group==GROUP_COALMINE
				|| main_types[tr].group==GROUP_INDUSTRY_L
				|| main_types[tr].group==GROUP_INDUSTRY_H
				|| main_types[tr].group==GROUP_OREMINE
				|| main_types[tr].group==GROUP_RECYCLE
				|| main_types[tr].group==GROUP_TIP
				|| main_types[tr].group==GROUP_PORT
				|| main_types[tr].group==GROUP_COMMUNE
			|| main_types[trc].group==GROUP_POWER_SOURCE_COAL
								)
                                        r=1;
				if (main_types[td].group==GROUP_TRACK)
					mappoint[x][y].flags |= FLAG_DOWN;
                                if (main_types[td].group==GROUP_TRACK
				|| main_types[td].group==GROUP_COALMINE
				|| main_types[td].group==GROUP_INDUSTRY_L
				|| main_types[td].group==GROUP_INDUSTRY_H
				|| main_types[td].group==GROUP_OREMINE
				|| main_types[td].group==GROUP_RECYCLE
				|| main_types[td].group==GROUP_COMMUNE
				|| main_types[td].group==GROUP_TIP
				|| main_types[td].group==GROUP_PORT
			|| main_types[tdc].group==GROUP_POWER_SOURCE_COAL)
                                        d=1;
				mappointtype[x][y]
					=select_track_type(u,l,r,d);
  			}
		}
}

int select_track_type(int u,int l,int r,int d)
{
	if (u && l && r && d)
		return(CST_TRACK_LUDR);
	if (u && d && r)
		return(CST_TRACK_UDR);
	if (l && u && r)
		return(CST_TRACK_LUR);
	if (l && d && r)
		return(CST_TRACK_LDR);
	if (l && d && u)
		return(CST_TRACK_LUD);
	if (r && u)
		return(CST_TRACK_UR);
	if (l && u)
		return(CST_TRACK_LU);
	if (r && d)
		return(CST_TRACK_DR);
	if (l && d)
		return(CST_TRACK_LD);
	if (u || d)
		return(CST_TRACK_UD);
	return(CST_TRACK_LR);
}


void check_water_graphics(void)
{
	int x,y,t,u,l,r,d,tu,tl,tr,td,xx,yy;
	tu=tl=tr=td=0;
        for (y=main_screen_originy;y<main_screen_originy
			+(MAIN_WIN_W/16);y++)
                for (x=main_screen_originx
			;x<main_screen_originx+(MAIN_WIN_W/16);x++)
		{
			t=mappointtype[x][y];
			if (main_types[t].group==GROUP_WATER)
			{
				tu=tl=tr=td=CST_GREEN;
				if (y>0)
					tu=mappointtype[x][y-1];
				if (x>0)
					tl=mappointtype[x-1][y];
				if (x<(WORLD_SIDE_LEN-1))
					tr=mappointtype[x+1][y];
				if (y<(WORLD_SIDE_LEN-1))
					td=mappointtype[x][y+1];
				u=l=r=d=0;
				if (main_types[tu].group==GROUP_WATER)
				{
					u=1;
				}
                                if (main_types[tl].group==GROUP_WATER)
				{
                                        l=1;
				}
				if (tl==CST_USED)
				{
					xx=mappoint[x-1][y].int_1;
					yy=mappoint[x-1][y].int_2;
		if (main_types[mappointtype[xx][yy]].group==GROUP_EX_PORT)
					l=1;
				}
				if (main_types[tr].group==GROUP_WATER)
					r=1;
				if (main_types[td].group==GROUP_WATER)
					d=1;
				mappointtype[x][y]
					=select_water_type(u,l,r,d);
  			}
		}
}

int select_water_type(int u,int l,int r,int d)
{
        if (u && l && r && d)
                return(CST_WATER_LURD);
        if (u && d && r)
                return(CST_WATER_URD);
        if (l && u && r)
                return(CST_WATER_LUR);
        if (l && d && r)
                return(CST_WATER_LRD);
        if (l && d && u)
                return(CST_WATER_LUD);
        if (r && u)
                return(CST_WATER_UR);
        if (l && u)
                return(CST_WATER_LU);
        if (r && d)
                return(CST_WATER_RD);
        if (l && d)
                return(CST_WATER_LD);
        if (u && d)
                return(CST_WATER_UD);
	if (l && r)
	        return(CST_WATER_LR);
	if (l)
		return(CST_WATER_L);
	if (u)
		return(CST_WATER_U);
	if (r)
		return(CST_WATER_R);
	if (d)
		return(CST_WATER_D);
	return(CST_WATER);
}


void check_rail_graphics(void)
{
	int x,y,t,u,l,r,d,tu,tl,tr,trc,td,tdc,trc2,trc3;
	tu=tl=tr=trc=td=tdc=trc2=trc3=0;
        for (y=main_screen_originy;y<main_screen_originy
			+(MAIN_WIN_W/16);y++)
                for (x=main_screen_originx
			;x<main_screen_originx+(MAIN_WIN_W/16);x++)
		{
			t=mappointtype[x][y];
			if (main_types[t].group==GROUP_RAIL)
			{
				tu=tl=tr=td=tdc=trc=trc2=trc3=CST_GREEN;
				if (y>0)
					tu=mappointtype[x][y-1];
				if (x>0)
					tl=mappointtype[x-1][y];
				if (x<(WORLD_SIDE_LEN-1))
				{
					tr=mappointtype[x+1][y];
					if (y>0)
						trc=mappointtype[x+1][y-1];
					if (y>1)
						trc2=mappointtype[x+1][y-2];
					if (y>2)
						trc3=mappointtype[x+1][y-3];
				}
				if (y<(WORLD_SIDE_LEN-1))
				{
					td=mappointtype[x][y+1];
					if (x>0)
						tdc=mappointtype[x-1][y+1];
				}
				u=l=r=d=0;
				mappoint[x][y].flags &= 0xfffffff0;
				if (main_types[tu].group==GROUP_RAIL)
				{
					u=1;
					mappoint[x][y].flags |= FLAG_UP;
				}
                                if (main_types[tl].group==GROUP_RAIL)
				{
                                        l=1;
					mappoint[x][y].flags |= FLAG_LEFT;
				}
				if (main_types[tr].group==GROUP_RAIL)
					mappoint[x][y].flags |= FLAG_RIGHT;
                                if (main_types[tr].group==GROUP_RAIL
				|| main_types[tr].group==GROUP_COALMINE
				|| main_types[tr].group==GROUP_INDUSTRY_L
				|| main_types[tr].group==GROUP_INDUSTRY_H
				|| main_types[tr].group==GROUP_OREMINE
				|| main_types[tr].group==GROUP_RECYCLE
				|| main_types[tr].group==GROUP_TIP
				|| main_types[tr].group==GROUP_PORT
			|| main_types[trc].group==GROUP_POWER_SOURCE_COAL
								)
                                        r=1;
				if (main_types[td].group==GROUP_RAIL)
					mappoint[x][y].flags |= FLAG_DOWN;
                                if (main_types[td].group==GROUP_RAIL
				|| main_types[td].group==GROUP_COALMINE
				|| main_types[td].group==GROUP_INDUSTRY_L
				|| main_types[td].group==GROUP_INDUSTRY_H
				|| main_types[td].group==GROUP_OREMINE
				|| main_types[td].group==GROUP_RECYCLE
				|| main_types[td].group==GROUP_TIP
				|| main_types[td].group==GROUP_PORT
			|| main_types[tdc].group==GROUP_POWER_SOURCE_COAL)
                                        d=1;
				mappointtype[x][y]
					=select_rail_type(u,l,r,d);
  			}
		}
}

int select_rail_type(int u,int l,int r,int d)
{
	if (u && l && r && d)
		return(CST_RAIL_LUDR);
	if (u && d && r)
		return(CST_RAIL_UDR);
	if (l && u && r)
		return(CST_RAIL_LUR);
	if (l && d && r)
		return(CST_RAIL_LDR);
	if (l && d && u)
		return(CST_RAIL_LUD);
	if (r && u)
		return(CST_RAIL_UR);
	if (l && u)
		return(CST_RAIL_LU);
	if (r && d)
		return(CST_RAIL_DR);
	if (l && d)
		return(CST_RAIL_LD);
	if (u || d)
		return(CST_RAIL_UD);
	return(CST_RAIL_LR);
}


void check_road_graphics(void)
{
	int x,y,t,u,l,r,d,tu,tl,tr,trc,td,tdc,trc2,trc3;
	tu=tl=tr=trc=td=tdc=trc2=trc3=0;
        for (y=main_screen_originy;y<main_screen_originy
			+(MAIN_WIN_W/16);y++)
                for (x=main_screen_originx
			;x<main_screen_originx+(MAIN_WIN_W/16);x++)
		{
			t=mappointtype[x][y];
			if (main_types[t].group==GROUP_ROAD)
			{
				tu=tl=tr=td=tdc=trc=trc2=trc3=CST_GREEN;
				if (y>0)
					tu=mappointtype[x][y-1];
				if (x>0)
					tl=mappointtype[x-1][y];
				if (x<(WORLD_SIDE_LEN-1))
				{
					tr=mappointtype[x+1][y];
					if (y>0)
						trc=mappointtype[x+1][y-1];
					if (y>1)
						trc2=mappointtype[x+1][y-2];
					if (y>2)
						trc3=mappointtype[x+1][y-3];
				}
				if (y<(WORLD_SIDE_LEN-1))
				{
					td=mappointtype[x][y+1];
					if (x>0)
						tdc=mappointtype[x-1][y+1];
				}
				u=l=r=d=0;
				mappoint[x][y].flags &= 0xfffffff0;
				if (main_types[tu].group==GROUP_ROAD)
				{
					u=1;
					mappoint[x][y].flags |= FLAG_UP;
				}
                                if (main_types[tl].group==GROUP_ROAD)
				{
                                        l=1;
					mappoint[x][y].flags |= FLAG_LEFT;
				}
				if (main_types[tr].group==GROUP_ROAD)
					mappoint[x][y].flags |= FLAG_RIGHT;
                                if (main_types[tr].group==GROUP_ROAD
				|| main_types[tr].group==GROUP_COALMINE
				|| main_types[tr].group==GROUP_INDUSTRY_L
				|| main_types[tr].group==GROUP_INDUSTRY_H
				|| main_types[tr].group==GROUP_OREMINE
				|| main_types[tr].group==GROUP_RECYCLE
				|| main_types[tr].group==GROUP_TIP
				|| main_types[tr].group==GROUP_PORT
			|| main_types[trc].group==GROUP_POWER_SOURCE_COAL
								)
                                        r=1;
				if (main_types[td].group==GROUP_ROAD)
					mappoint[x][y].flags |= FLAG_DOWN;
                                if (main_types[td].group==GROUP_ROAD
				|| main_types[td].group==GROUP_COALMINE
				|| main_types[td].group==GROUP_INDUSTRY_L
				|| main_types[td].group==GROUP_INDUSTRY_H
				|| main_types[td].group==GROUP_OREMINE
				|| main_types[td].group==GROUP_RECYCLE
				|| main_types[td].group==GROUP_TIP
				|| main_types[td].group==GROUP_PORT
			|| main_types[tdc].group==GROUP_POWER_SOURCE_COAL)
                                        d=1;
				mappointtype[x][y]
					=select_road_type(u,l,r,d);
  			}
		}
}

int select_road_type(int u,int l,int r,int d)
{
	if (u && l && r && d)
		return(CST_ROAD_LUDR);
	if (u && d && r)
		return(CST_ROAD_UDR);
	if (l && u && r)
		return(CST_ROAD_LUR);
	if (l && d && r)
		return(CST_ROAD_LDR);
	if (l && d && u)
		return(CST_ROAD_LUD);
	if (r && u)
		return(CST_ROAD_UR);
	if (l && u)
		return(CST_ROAD_LU);
	if (r && d)
		return(CST_ROAD_DR);
	if (l && d)
		return(CST_ROAD_LD);
	if (u || d)
		return(CST_ROAD_UD);
	return(CST_ROAD_LR);
}

#endif	/* THMO_SCREEN_STUFF */

//  *******************
//                        SCREEN SETUP
//  *******************
void screen_setup(void)
{
	int i;
// draw the graph boxes
	do_monthgraph();
// load the pbar graphics
	up_pbar1_graphic=load_graphic("pbarup1.csi");
	up_pbar2_graphic=load_graphic("pbarup2.csi");
	down_pbar1_graphic=load_graphic("pbardown1.csi");
	down_pbar2_graphic=load_graphic("pbardown2.csi");
	pop_pbar_graphic=load_graphic("pbarpop.csi");
	tech_pbar_graphic=load_graphic("pbartech.csi");
	food_pbar_graphic=load_graphic("pbarfood.csi");
	jobs_pbar_graphic=load_graphic("pbarjobs.csi");
	coal_pbar_graphic=load_graphic("pbarcoal.csi");
	goods_pbar_graphic=load_graphic("pbargoods.csi");
	ore_pbar_graphic=load_graphic("pbarore.csi");
	steel_pbar_graphic=load_graphic("pbarsteel.csi");
	money_pbar_graphic=load_graphic("pbarmoney.csi");
	do_pbar_population(0);
	do_pbar_tech(0);
	do_pbar_food(0);
	do_pbar_jobs(0);
	do_pbar_coal(0);
	do_pbar_goods(0);
	do_pbar_ore(0);
	do_pbar_steel(0);
	do_pbar_money(0);
// draw the box around the main window
	draw_main_window_box(green(8));
// load the checked and unchecked box graphics
	checked_box_graphic=load_graphic("checked_box.csi");
	unchecked_box_graphic=load_graphic("unchecked_box.csi");
// draw the mini screen normal button
	ms_normal_button_graphic=load_graphic("ms-normal-button.csi");
	Fgl_putbox(MS_NORMAL_BUTTON_X,MS_NORMAL_BUTTON_Y
		,16,16,ms_normal_button_graphic);
// draw the pollution button
	ms_pollution_button_graphic=load_graphic("ms-pollution-button.csi");
	Fgl_putbox(MS_POLLUTION_BUTTON_X,MS_POLLUTION_BUTTON_Y
		,16,16,ms_pollution_button_graphic);
// draw the fire cover button
        ms_fire_cover_button_graphic
		=load_graphic("ms-fire-cover-button.csi");
        Fgl_putbox(MS_FIRE_COVER_BUTTON_X,MS_FIRE_COVER_BUTTON_Y
                ,16,16,ms_fire_cover_button_graphic);
// draw the health cover button
        ms_health_cover_button_graphic
                =load_graphic("ms-health-cover-button.csi");
        Fgl_putbox(MS_HEALTH_COVER_BUTTON_X,MS_HEALTH_COVER_BUTTON_Y
                ,16,16,ms_health_cover_button_graphic);
// draw the cricket cover button
        ms_cricket_cover_button_graphic
                =load_graphic("ms-cricket-cover-button.csi");
        Fgl_putbox(MS_CRICKET_COVER_BUTTON_X,MS_CRICKET_COVER_BUTTON_Y
                ,16,16,ms_cricket_cover_button_graphic);
// draw the ub40 button
        ms_ub40_button_graphic=load_graphic("ms-ub40-button.csi");
        Fgl_putbox(MS_UB40_BUTTON_X,MS_UB40_BUTTON_Y
                ,16,16,ms_ub40_button_graphic);
// draw the coal button
        ms_coal_button_graphic=load_graphic("ms-coal-button.csi");
        Fgl_putbox(MS_COAL_BUTTON_X,MS_COAL_BUTTON_Y
                ,16,16,ms_coal_button_graphic);
// draw the starving button
        ms_starve_button_graphic=load_graphic("ms-starve-button.csi");
        Fgl_putbox(MS_STARVE_BUTTON_X,MS_STARVE_BUTTON_Y
                ,16,16,ms_starve_button_graphic);
// draw the power mini screen button
        ms_power_button_graphic=load_graphic("ms-power-button.csi");
        Fgl_putbox(MS_POWER_BUTTON_X,MS_POWER_BUTTON_Y
                ,16,16,ms_power_button_graphic);
// draw the other cost button
        ms_ocost_button_graphic=load_graphic("ms-ocost-button.csi");
        Fgl_putbox(MS_OCOST_BUTTON_X,MS_OCOST_BUTTON_Y
                ,16,16,ms_ocost_button_graphic);

// draw the pause button
	pause_button1_off=load_graphic("pause-offl.csi");
	Fgl_putbox(PAUSE_BUTTON_X,PAUSE_BUTTON_Y,16,16,pause_button1_off);
	pause_button2_off=load_graphic("pause-offr.csi");
	Fgl_putbox(PAUSE_BUTTON_X+16,PAUSE_BUTTON_Y
		,16,16,pause_button2_off);
	pause_button1_on=load_graphic("pause-onl.csi");
	pause_button2_on=load_graphic("pause-onr.csi");

// draw the slow button
	slow_button1_off=load_graphic("slow-offl.csi");
	Fgl_putbox(SLOW_BUTTON_X,SLOW_BUTTON_Y,16,16,slow_button1_off);
	slow_button2_off=load_graphic("slow-offr.csi");
	Fgl_putbox(SLOW_BUTTON_X+16,SLOW_BUTTON_Y
		,16,16,slow_button2_off);
	slow_button1_on=load_graphic("slow-onl.csi");
	slow_button2_on=load_graphic("slow-onr.csi");

// draw the medium button
	med_button1_off=load_graphic("norm-offl.csi");
	Fgl_putbox(MED_BUTTON_X,MED_BUTTON_Y,16,16,med_button1_off);
	med_button2_off=load_graphic("norm-offr.csi");
	Fgl_putbox(MED_BUTTON_X+16,MED_BUTTON_Y
		,16,16,med_button2_off);
	med_button1_on=load_graphic("norm-onl.csi");
	med_button2_on=load_graphic("norm-onr.csi");

// draw the fast button
	fast_button1_off=load_graphic("fast-offl.csi");
	Fgl_putbox(FAST_BUTTON_X,FAST_BUTTON_Y,16,16,fast_button1_off);
	fast_button2_off=load_graphic("fast-offr.csi");
	Fgl_putbox(FAST_BUTTON_X+16,FAST_BUTTON_Y
		,16,16,fast_button2_off);
	fast_button1_on=load_graphic("fast-onl.csi");
	fast_button2_on=load_graphic("fast-onr.csi");

// draw the results button
        results_button1=load_graphic("results-l.csi");
        Fgl_putbox(RESULTSBUT_X,RESULTSBUT_Y,16,16,results_button1);
        results_button2=load_graphic("results-r.csi");
        Fgl_putbox(RESULTSBUT_X+16,RESULTSBUT_Y
                ,16,16,results_button2);

// draw the t-overwrite button and load 'on button'
	toveron_button1=load_graphic("tover1-on.csi");
        toveroff_button1=load_graphic("tover1-off.csi");
        Fgl_putbox(TOVERBUT_X,TOVERBUT_Y,16,16,toveroff_button1);
	toveron_button2=load_graphic("tover2-on.csi");
        toveroff_button2=load_graphic("tover2-off.csi");
        Fgl_putbox(TOVERBUT_X+16,TOVERBUT_Y
                ,16,16,toveroff_button2);


#ifdef LC_X11
// draw the confine mouse button
	confine_button=load_graphic("mouse-confined.csi");
	unconfine_button=load_graphic("mouse-free.csi");
	Fgl_putbox(CONFINEBUT_X,CONFINEBUT_Y,16,16,unconfine_button);
#endif

// draw the help button
	help_button_graphic=load_graphic("help-button.csi");
	Fgl_putbox(HELP_BUTTON_X,HELP_BUTTON_Y,HELP_BUTTON_W,HELP_BUTTON_H
		,help_button_graphic);

// draw the quit button
	quit_button_graphic=load_graphic("quit-button.csi");
	Fgl_putbox(QUIT_BUTTON_X,QUIT_BUTTON_Y,QUIT_BUTTON_W,QUIT_BUTTON_H
		,quit_button_graphic);

// draw the load button
        load_button_graphic=load_graphic("load-button.csi");
        Fgl_putbox(LOAD_BUTTON_X,LOAD_BUTTON_Y,LOAD_BUTTON_W,LOAD_BUTTON_H
                ,load_button_graphic);

// draw the save button
        save_button_graphic=load_graphic("save-button.csi");
        Fgl_putbox(SAVE_BUTTON_X,SAVE_BUTTON_Y,SAVE_BUTTON_W,SAVE_BUTTON_H
                ,save_button_graphic);

// draw border around mini screen
        for (i=1;i<8;i++)
        {
                Fgl_hline(MINI_SCREEN_X-1-i,MINI_SCREEN_Y-1-i
                        ,MINI_SCREEN_X+WORLD_SIDE_LEN+1+i
                        ,yellow(16));
                Fgl_line(MINI_SCREEN_X-1-i,MINI_SCREEN_Y-1-i
                        ,MINI_SCREEN_X-1-i
                        ,MINI_SCREEN_Y+WORLD_SIDE_LEN+1+i
                        ,yellow(14));
                Fgl_hline(MINI_SCREEN_X-1-i
                        ,MINI_SCREEN_Y+WORLD_SIDE_LEN+1+i
                        ,MINI_SCREEN_X+WORLD_SIDE_LEN+1+i
                        ,yellow(22));
                Fgl_line(MINI_SCREEN_X+WORLD_SIDE_LEN+1+i
                        ,MINI_SCREEN_Y-1-i
                        ,MINI_SCREEN_X+WORLD_SIDE_LEN+1+i
                        ,MINI_SCREEN_Y+WORLD_SIDE_LEN+1+i
                        ,yellow(24));
        }


// draw the select button box
	Fgl_fillbox(SELECT_BUTTON_WIN_X,SELECT_BUTTON_WIN_Y
		,SELECT_BUTTON_WIN_W,SELECT_BUTTON_WIN_H,white(20));
	redraw_mouse();
	select_button_graphic[sbut[0]]=load_graphic("powerline-button.csi");
	select_button_type[sbut[0]]=CST_POWERL_H_L;
	strcpy(select_button_help[sbut[0]],"powerline.hlp");
	draw_select_button_graphic(sbut[0],select_button_graphic[sbut[0]]);
	select_button_cost[sbut[0]]=GROUP_POWER_LINE_COST;
	select_button_cost_mul[sbut[0]]=GROUP_POWER_LINE_COST_MUL;
	select_button_bul_cost[sbut[0]]=GROUP_POWER_LINE_BUL_COST;
	select_button_tech[sbut[0]]=GROUP_POWER_LINE_TECH*MAX_TECH_LEVEL/1000;

	select_button_graphic[sbut[1]]=load_graphic("powerssolar-button.csi");
	draw_select_button_graphic(sbut[1],select_button_graphic[sbut[1]]);
	select_button_type[sbut[1]]=CST_POWERS_SOLAR;
	strcpy(select_button_help[sbut[1]],"powerssolar.hlp");
	select_button_cost[sbut[1]]=GROUP_POWER_SOURCE_COST;
	select_button_cost_mul[sbut[1]]=GROUP_POWER_SOURCE_COST_MUL;
	select_button_bul_cost[sbut[1]]=GROUP_POWER_SOURCE_BUL_COST;
	select_button_tech[sbut[1]]=GROUP_POWER_SOURCE_TECH*MAX_TECH_LEVEL/1000;

	select_button_graphic[sbut[2]]=load_graphic("substation-button.csi");
	draw_select_button_graphic(sbut[2],select_button_graphic[sbut[2]]);
	select_button_type[sbut[2]]=CST_SUBSTATION_R;
	strcpy(select_button_help[sbut[2]],"substation.hlp");
	select_button_cost[sbut[2]]=GROUP_SUBSTATION_COST;
	select_button_cost_mul[sbut[2]]=GROUP_SUBSTATION_COST_MUL;
	select_button_bul_cost[sbut[2]]=GROUP_SUBSTATION_BUL_COST;
	select_button_tech[sbut[2]]=GROUP_SUBSTATION_TECH*MAX_TECH_LEVEL/1000;

	select_button_graphic[sbut[3]]=load_graphic("residence-button.csi");
	draw_select_button_graphic(sbut[3],select_button_graphic[sbut[3]]);
	select_button_type[sbut[3]]=CST_RESIDENCE_LL;
	strcpy(select_button_help[sbut[3]],"residential.hlp");
	select_button_cost[sbut[3]]=GROUP_RESIDENCE_COST;
	select_button_cost_mul[sbut[3]]=GROUP_RESIDENCE_COST_MUL;
	select_button_bul_cost[sbut[3]]=GROUP_RESIDENCE_BUL_COST;
	select_button_tech[sbut[3]]=GROUP_RESIDENCE_TECH*MAX_TECH_LEVEL/1000;

        select_button_graphic[sbut[4]]=load_graphic("organic-farm-button.csi");
        draw_select_button_graphic(sbut[4],select_button_graphic[sbut[4]]);
        select_button_type[sbut[4]]=CST_FARM_O0;
	strcpy(select_button_help[sbut[4]],"farm.hlp");
	select_button_cost[sbut[4]]=GROUP_ORGANIC_FARM_COST;
	select_button_cost_mul[sbut[4]]=GROUP_ORGANIC_FARM_COST_MUL;
	select_button_bul_cost[sbut[4]]=GROUP_ORGANIC_FARM_BUL_COST;
	select_button_tech[sbut[4]]=GROUP_ORGANIC_FARM_TECH*MAX_TECH_LEVEL/1000;

        select_button_graphic[sbut[5]]=load_graphic("market-button.csi");
        draw_select_button_graphic(sbut[5],select_button_graphic[sbut[5]]);
        select_button_type[sbut[5]]=CST_MARKET_EMPTY;
	strcpy(select_button_help[sbut[5]],"market.hlp");
	select_button_cost[sbut[5]]=GROUP_MARKET_COST;
	select_button_cost_mul[sbut[5]]=GROUP_MARKET_COST_MUL;
	select_button_bul_cost[sbut[5]]=GROUP_MARKET_BUL_COST;
	select_button_tech[sbut[5]]=GROUP_MARKET_TECH*MAX_TECH_LEVEL/1000;

	select_button_graphic[sbut[6]]=load_graphic("buldoze-button.csi");
	draw_select_button_graphic(sbut[6],select_button_graphic[sbut[6]]);
	select_button_type[sbut[6]]=CST_GREEN;
	strcpy(select_button_help[sbut[6]],"bulldoze.hlp");
	select_button_cost[sbut[6]]=GROUP_BARE_COST;
	select_button_cost_mul[sbut[6]]=GROUP_BARE_COST_MUL;
	select_button_bul_cost[sbut[6]]=GROUP_BARE_BUL_COST;
	select_button_tech[sbut[6]]=GROUP_BARE_TECH*MAX_TECH_LEVEL/1000;

        select_button_graphic[sbut[7]]=load_graphic("track-button.csi");
        draw_select_button_graphic(sbut[7],select_button_graphic[sbut[7]]);
        select_button_type[sbut[7]]=CST_TRACK_LR;
	strcpy(select_button_help[sbut[7]],"track.hlp");
	select_button_help_flag[sbut[7]]=1;
	select_button_cost[sbut[7]]=GROUP_TRACK_COST;
	select_button_cost_mul[sbut[7]]=GROUP_TRACK_COST_MUL;
	select_button_bul_cost[sbut[7]]=GROUP_TRACK_BUL_COST;
	select_button_tech[sbut[7]]=GROUP_TRACK_TECH*MAX_TECH_LEVEL/1000;

        select_button_graphic[sbut[8]]=load_graphic("coalmine-button.csi");
        draw_select_button_graphic(sbut[8],select_button_graphic[sbut[8]]);
        select_button_type[sbut[8]]=CST_COALMINE_EMPTY;
	strcpy(select_button_help[sbut[8]],"coalmine.hlp");
	select_button_cost[sbut[8]]=GROUP_COALMINE_COST;
	select_button_cost_mul[sbut[8]]=GROUP_COALMINE_COST_MUL;
	select_button_bul_cost[sbut[8]]=GROUP_COALMINE_BUL_COST;
	select_button_tech[sbut[8]]=GROUP_COALMINE_TECH*MAX_TECH_LEVEL/1000;

        select_button_graphic[sbut[9]]=load_graphic("rail-button.csi");
        draw_select_button_graphic(sbut[9],select_button_graphic[sbut[9]]);
        select_button_type[sbut[9]]=CST_RAIL_LR;
	strcpy(select_button_help[sbut[9]],"rail.hlp");
	select_button_cost[sbut[9]]=GROUP_RAIL_COST;
	select_button_cost_mul[sbut[9]]=GROUP_RAIL_COST_MUL;
	select_button_bul_cost[sbut[9]]=GROUP_RAIL_BUL_COST;
	select_button_tech[sbut[9]]=GROUP_RAIL_TECH*MAX_TECH_LEVEL/1000;

        select_button_graphic[sbut[10]]=load_graphic("powerscoal-button.csi");
        draw_select_button_graphic(sbut[10],select_button_graphic[sbut[10]]);
        select_button_type[sbut[10]]=CST_POWERS_COAL_EMPTY;
	strcpy(select_button_help[sbut[10]],"powerscoal.hlp");
	select_button_cost[sbut[10]]=GROUP_POWER_SOURCE_COAL_COST;
	select_button_cost_mul[sbut[10]]=GROUP_POWER_SOURCE_COAL_COST_MUL;
	select_button_bul_cost[sbut[10]]=GROUP_POWER_SOURCE_COAL_BUL_COST;
	select_button_tech[sbut[10]]=GROUP_POWER_SOURCE_COAL_TECH*MAX_TECH_LEVEL/1000;

        select_button_graphic[sbut[11]]=load_graphic("road-button.csi");
        draw_select_button_graphic(sbut[11],select_button_graphic[sbut[11]]);
        select_button_type[sbut[11]]=CST_ROAD_LR;
	strcpy(select_button_help[sbut[11]],"road.hlp");
	select_button_cost[sbut[11]]=GROUP_ROAD_COST;
	select_button_cost_mul[sbut[11]]=GROUP_ROAD_COST_MUL;
	select_button_bul_cost[sbut[11]]=GROUP_ROAD_BUL_COST;
	select_button_tech[sbut[11]]=GROUP_ROAD_TECH*MAX_TECH_LEVEL/1000;

        select_button_graphic[sbut[12]]=load_graphic("industryl-button.csi");
        draw_select_button_graphic(sbut[12],select_button_graphic[sbut[12]]);
        select_button_type[sbut[12]]=CST_INDUSTRY_L_C;
	strcpy(select_button_help[sbut[12]],"industryl.hlp");
        select_button_cost[sbut[12]]=GROUP_INDUSTRY_L_COST;
	select_button_cost_mul[sbut[12]]=GROUP_INDUSTRY_L_COST_MUL;
	select_button_bul_cost[sbut[12]]=GROUP_INDUSTRY_L_BUL_COST;
	select_button_tech[sbut[12]]=GROUP_INDUSTRY_L_TECH*MAX_TECH_LEVEL/1000;

        select_button_graphic[sbut[13]]=load_graphic("university-button.csi");
        draw_select_button_graphic(sbut[13],select_button_graphic[sbut[13]]);
        select_button_type[sbut[13]]=CST_UNIVERSITY;
	strcpy(select_button_help[sbut[13]],"university.hlp");
        select_button_cost[sbut[13]]=GROUP_UNIVERSITY_COST;
	select_button_cost_mul[sbut[13]]=GROUP_UNIVERSITY_COST_MUL;
	select_button_bul_cost[sbut[13]]=GROUP_UNIVERSITY_BUL_COST;
	select_button_tech[sbut[13]]=GROUP_UNIVERSITY_TECH*MAX_TECH_LEVEL/1000;

        select_button_graphic[sbut[14]]=load_graphic("commune-button.csi");
        draw_select_button_graphic(sbut[14],select_button_graphic[sbut[14]]);
        select_button_type[sbut[14]]=CST_COMMUNE_1;
	strcpy(select_button_help[sbut[14]],"commune.hlp");
        select_button_cost[sbut[14]]=GROUP_COMMUNE_COST;
	select_button_cost_mul[sbut[14]]=GROUP_COMMUNE_COST_MUL;
	select_button_bul_cost[sbut[14]]=GROUP_COMMUNE_BUL_COST;
	select_button_tech[sbut[14]]=GROUP_COMMUNE_TECH*MAX_TECH_LEVEL/1000;

        select_button_graphic[sbut[15]]=load_graphic("oremine-button.csi");
        draw_select_button_graphic(sbut[15],select_button_graphic[sbut[15]]);
        select_button_type[sbut[15]]=CST_OREMINE_1;
	strcpy(select_button_help[sbut[15]],"oremine.hlp");
        select_button_cost[sbut[15]]=GROUP_OREMINE_COST;
	select_button_cost_mul[sbut[15]]=GROUP_OREMINE_COST_MUL;
	select_button_bul_cost[sbut[15]]=GROUP_OREMINE_BUL_COST;
	select_button_tech[sbut[15]]=GROUP_OREMINE_TECH*MAX_TECH_LEVEL/1000;

        select_button_graphic[sbut[16]]=load_graphic("tip-button.csi");
        draw_select_button_graphic(sbut[16],select_button_graphic[sbut[16]]);
        select_button_type[sbut[16]]=CST_TIP_0;
	strcpy(select_button_help[sbut[16]],"tip.hlp");
        select_button_cost[sbut[16]]=GROUP_TIP_COST;
	select_button_cost_mul[sbut[16]]=GROUP_TIP_COST_MUL;
	select_button_bul_cost[sbut[16]]=GROUP_TIP_BUL_COST;
	select_button_tech[sbut[16]]=GROUP_TIP_TECH*MAX_TECH_LEVEL/1000;

        select_button_graphic[sbut[17]]=load_graphic("port-button.csi");
        draw_select_button_graphic(sbut[17],select_button_graphic[sbut[17]]);
        select_button_type[sbut[17]]=CST_EX_PORT;
	strcpy(select_button_help[sbut[17]],"port.hlp");
        select_button_cost[sbut[17]]=GROUP_EX_PORT_COST;
	select_button_cost_mul[sbut[17]]=GROUP_EX_PORT_COST_MUL;
	select_button_bul_cost[sbut[17]]=GROUP_EX_PORT_BUL_COST;
	select_button_tech[sbut[17]]=GROUP_EX_PORT_TECH*MAX_TECH_LEVEL/1000;

        select_button_graphic[sbut[18]]=load_graphic("industryh-button.csi");
        draw_select_button_graphic(sbut[18],select_button_graphic[sbut[18]]);
        select_button_type[sbut[18]]=CST_INDUSTRY_H_C;
	strcpy(select_button_help[sbut[18]],"industryh.hlp");
        select_button_cost[sbut[18]]=GROUP_INDUSTRY_H_COST;
	select_button_cost_mul[sbut[18]]=GROUP_INDUSTRY_H_COST_MUL;
	select_button_bul_cost[sbut[18]]=GROUP_INDUSTRY_H_BUL_COST;
	select_button_tech[sbut[18]]=GROUP_INDUSTRY_H_TECH*MAX_TECH_LEVEL/1000;

        select_button_graphic[sbut[19]]=load_graphic("parkland-button.csi");
        draw_select_button_graphic(sbut[19],select_button_graphic[sbut[19]]);
        select_button_type[sbut[19]]=CST_PARKLAND_PLANE;
	strcpy(select_button_help[sbut[19]],"park.hlp");
        select_button_cost[sbut[19]]=GROUP_PARKLAND_COST;
	select_button_cost_mul[sbut[19]]=GROUP_PARKLAND_COST_MUL;
	select_button_bul_cost[sbut[19]]=GROUP_PARKLAND_BUL_COST;
	select_button_tech[sbut[19]]=GROUP_PARKLAND_TECH*MAX_TECH_LEVEL/1000;

        select_button_graphic[sbut[20]]=load_graphic("recycle-button.csi");
        draw_select_button_graphic(sbut[20],select_button_graphic[sbut[20]]);
        select_button_type[sbut[20]]=CST_RECYCLE;
	strcpy(select_button_help[sbut[20]],"recycle.hlp");
        select_button_cost[sbut[20]]=GROUP_RECYCLE_COST;
	select_button_cost_mul[sbut[20]]=GROUP_RECYCLE_COST_MUL;
	select_button_bul_cost[sbut[20]]=GROUP_RECYCLE_BUL_COST;
	select_button_tech[sbut[20]]=GROUP_RECYCLE_TECH*MAX_TECH_LEVEL/1000;

        select_button_graphic[sbut[21]]=load_graphic("water-button.csi");
        draw_select_button_graphic(sbut[21],select_button_graphic[sbut[21]]);
        select_button_type[sbut[21]]=CST_WATER;
	strcpy(select_button_help[sbut[21]],"river.hlp");
        select_button_cost[sbut[21]]=GROUP_WATER_COST;
	select_button_cost_mul[sbut[21]]=GROUP_WATER_COST_MUL;
	select_button_bul_cost[sbut[21]]=GROUP_WATER_BUL_COST;
	select_button_tech[sbut[21]]=GROUP_WATER_TECH*MAX_TECH_LEVEL/1000;

        select_button_graphic[sbut[22]]=load_graphic("health-button.csi");
        draw_select_button_graphic(sbut[22],select_button_graphic[sbut[22]]);
        select_button_type[sbut[22]]=CST_HEALTH;
	strcpy(select_button_help[sbut[22]],"health.hlp");
        select_button_cost[sbut[22]]=GROUP_HEALTH_COST;
	select_button_cost_mul[sbut[22]]=GROUP_HEALTH_COST_MUL;
	select_button_bul_cost[sbut[22]]=GROUP_HEALTH_BUL_COST;
	select_button_tech[sbut[22]]=GROUP_HEALTH_TECH*MAX_TECH_LEVEL/1000;

        select_button_graphic[sbut[23]]=load_graphic("rocket-button.csi");
        draw_select_button_graphic(sbut[23],select_button_graphic[sbut[23]]);
        select_button_type[sbut[23]]=CST_ROCKET_1;
	strcpy(select_button_help[sbut[23]],"rocket.hlp");
        select_button_cost[sbut[23]]=GROUP_ROCKET_COST;
	select_button_cost_mul[sbut[23]]=GROUP_ROCKET_COST_MUL;
	select_button_bul_cost[sbut[23]]=GROUP_ROCKET_BUL_COST;
	select_button_tech[sbut[23]]=GROUP_ROCKET_TECH*MAX_TECH_LEVEL/1000;

        select_button_graphic[sbut[24]]=load_graphic("windmill-button.csi");
        draw_select_button_graphic(sbut[24],select_button_graphic[sbut[24]]);
        select_button_type[sbut[24]]=CST_WINDMILL_1_R;
	strcpy(select_button_help[sbut[24]],"windmill.hlp");
        select_button_cost[sbut[24]]=GROUP_WINDMILL_COST;
	select_button_cost_mul[sbut[24]]=GROUP_WINDMILL_COST_MUL;
	select_button_bul_cost[sbut[24]]=GROUP_WINDMILL_BUL_COST;
	select_button_tech[sbut[24]]=GROUP_WINDMILL_TECH*MAX_TECH_LEVEL/1000;

        select_button_graphic[sbut[25]]=load_graphic("monument-button.csi");
        draw_select_button_graphic(sbut[25],select_button_graphic[sbut[25]]);
        select_button_type[sbut[25]]=CST_MONUMENT_0;
	strcpy(select_button_help[sbut[25]],"monument.hlp");
        select_button_cost[sbut[25]]=GROUP_MONUMENT_COST;
	select_button_cost_mul[sbut[25]]=GROUP_MONUMENT_COST_MUL;
	select_button_bul_cost[sbut[25]]=GROUP_MONUMENT_BUL_COST;
	select_button_tech[sbut[25]]=GROUP_MONUMENT_TECH*MAX_TECH_LEVEL/1000;

        select_button_graphic[sbut[26]]=load_graphic("school-button.csi");
        draw_select_button_graphic(sbut[26],select_button_graphic[sbut[26]]);
        select_button_type[sbut[26]]=CST_SCHOOL;
	strcpy(select_button_help[sbut[26]],"school.hlp");
        select_button_cost[sbut[26]]=GROUP_SCHOOL_COST;
	select_button_cost_mul[sbut[26]]=GROUP_SCHOOL_COST_MUL;
        select_button_bul_cost[sbut[26]]=GROUP_SCHOOL_BUL_COST;
        select_button_tech[sbut[26]]=GROUP_SCHOOL_TECH*MAX_TECH_LEVEL/1000;

        select_button_graphic[sbut[27]]=load_graphic("blacksmith-button.csi");
        draw_select_button_graphic(sbut[27],select_button_graphic[sbut[27]]);
        select_button_type[sbut[27]]=CST_BLACKSMITH_0;
	strcpy(select_button_help[sbut[27]],"blacksmith.hlp");
        select_button_cost[sbut[27]]=GROUP_BLACKSMITH_COST;
	select_button_cost_mul[sbut[27]]=GROUP_BLACKSMITH_COST_MUL;
        select_button_bul_cost[sbut[27]]=GROUP_BLACKSMITH_BUL_COST;
        select_button_tech[sbut[27]]=GROUP_BLACKSMITH_TECH*MAX_TECH_LEVEL/1000;

        select_button_graphic[sbut[28]]=load_graphic("mill-button.csi");
        draw_select_button_graphic(sbut[28],select_button_graphic[sbut[28]]);
        select_button_type[sbut[28]]=CST_MILL_0;
	strcpy(select_button_help[sbut[28]],"mill.hlp");
        select_button_cost[sbut[28]]=GROUP_MILL_COST;
	select_button_cost_mul[sbut[28]]=GROUP_MILL_COST_MUL;
        select_button_bul_cost[sbut[28]]=GROUP_MILL_BUL_COST;
        select_button_tech[sbut[28]]=GROUP_MILL_TECH*MAX_TECH_LEVEL/1000;

        select_button_graphic[sbut[29]]=load_graphic("pottery-button.csi");
        draw_select_button_graphic(sbut[29],select_button_graphic[sbut[29]]);
        select_button_type[sbut[29]]=CST_POTTERY_0;
	strcpy(select_button_help[sbut[29]],"pottery.hlp");
        select_button_cost[sbut[29]]=GROUP_POTTERY_COST;
	select_button_cost_mul[sbut[29]]=GROUP_POTTERY_COST_MUL;
        select_button_bul_cost[sbut[29]]=GROUP_POTTERY_BUL_COST;
        select_button_tech[sbut[29]]=GROUP_POTTERY_TECH*MAX_TECH_LEVEL/1000;

        select_button_graphic[sbut[30]]=load_graphic("firestation-button.csi");
        draw_select_button_graphic(sbut[30],select_button_graphic[sbut[30]]);
        select_button_type[sbut[30]]=CST_FIRESTATION_1;
	strcpy(select_button_help[sbut[30]],"firestation.hlp");
        select_button_cost[sbut[30]]=GROUP_FIRESTATION_COST;
        select_button_cost_mul[sbut[30]]=GROUP_FIRESTATION_COST_MUL;
        select_button_bul_cost[sbut[30]]=GROUP_FIRESTATION_BUL_COST;
        select_button_tech[sbut[30]]=GROUP_FIRESTATION_TECH*MAX_TECH_LEVEL/1000;

        select_button_graphic[sbut[31]]=load_graphic("cricket-button.csi");
        draw_select_button_graphic(sbut[31],select_button_graphic[sbut[31]]);
        select_button_type[sbut[31]]=CST_CRICKET_1;
	strcpy(select_button_help[sbut[31]],"cricket.hlp");
        select_button_cost[sbut[31]]=GROUP_CRICKET_COST;
        select_button_cost_mul[sbut[31]]=GROUP_CRICKET_COST_MUL;
        select_button_bul_cost[sbut[31]]=GROUP_CRICKET_BUL_COST;
        select_button_tech[sbut[31]]=GROUP_CRICKET_TECH*MAX_TECH_LEVEL/1000;

	select_button_bul_cost[32]=GROUP_BURNT_BUL_COST;

	for (i=0;i<NUMOF_SELECT_BUTTONS;i++)
		select_button_tflag[i]=0;   // disable all the buttons
// then enable the ones that are available at the start
	update_select_buttons();
	update_main_screen();
}

void update_select_buttons(void)
{
#ifdef SELECT_BUTTONS_NEED_TECH
	int i,f;
	for (i=0;i<NUMOF_SELECT_BUTTONS;i++)
	{
		f=select_button_tflag[i];
		if (tech_level>=select_button_tech[i])
		{
			if (select_button_tflag[i]==0)
				call_select_change_up(i);
			f=1;
		}
		else if (select_button_tflag[i]!=0 && tech_level
			<(select_button_tech[i]-(select_button_tech[i]/10)))
			f=0;
		if (select_button_tflag[i]!=f)
		{
			select_button_tflag[i]=f;
// Can take this 'if' out when all the buttons are there.
// Leave the command though!
 			if (select_button_graphic[i]!=0)
				draw_select_button_graphic(i
					,select_button_graphic[i]);
		}
	}
#else
	for (i=0;i<NUMOF_SELECT_BUTTONS;i++)  // make sure they are activated
		select_button_tflag[i]=1;   // - just a hack. 
#endif
}


void draw_main_window_box(int colour)
{
	int x;
        for (x=0;x<8;x++)
        {
                Fgl_hline(MAIN_WIN_X-1-x,MAIN_WIN_Y-1-x
                        ,MAIN_WIN_X+MAIN_WIN_W+x,colour+x+x);
                Fgl_hline(MAIN_WIN_X-1-x,MAIN_WIN_Y+MAIN_WIN_H+x
                        ,MAIN_WIN_X+MAIN_WIN_W+x,colour+x+x);
                Fgl_line(MAIN_WIN_X-1-x,MAIN_WIN_Y-x,MAIN_WIN_X-1-x
                        ,MAIN_WIN_Y+MAIN_WIN_H+x,colour+x+x);
                Fgl_line(MAIN_WIN_X+MAIN_WIN_W+x,MAIN_WIN_Y-x
                        ,MAIN_WIN_X+MAIN_WIN_W+x,MAIN_WIN_Y+MAIN_WIN_H+x
                        ,colour+x+x);
        }
}

void draw_select_button_graphic(int button,char *graphic)
{
	int x,y,xx,yy;
	if (button<NUMOF_SELECT_BUTTONS_DOWN)
	{
		x=8;
		y=8+(button*24);
	}
	else
	{
		x=8+24;
		y=8+((button-NUMOF_SELECT_BUTTONS_DOWN)*24);
	}
	Fgl_putbox(x+SELECT_BUTTON_WIN_X,y+SELECT_BUTTON_WIN_Y,16,16,graphic);
	unhighlight_select_button(button);
	hide_mouse();
	if (select_button_tflag[button]==0)
        {
		for (yy=-3;yy<19;yy++)
			for (xx=-3;xx<19;xx+=2)
                		Fgl_setpixel(x+xx+(yy%2)+SELECT_BUTTON_WIN_X
					,y+yy+SELECT_BUTTON_WIN_Y,white(15));
        }
	redraw_mouse();

}

#ifndef LC_X11
void setcustompalette(void)
{
        char s[100];
        int i,n,r,g,b,flag[256];
        FILE *inf;
        Palette pal;
        for (i=0;i<256;i++)
                flag[i]=0;
        if ((inf=fopen(colour_pal_file,"r"))==0)
	{
		printf("The colour palette file <%s>... ",colour_pal_file);
                do_error("Can't find it.");
	}
        while (feof(inf)==0)
        {
                fgets(s,99,inf);
                if (sscanf(s,"%d %d %d %d",&n,&r,&g,&b)==4)
                {
                        pal.color[n].red=r;
                        pal.color[n].green=g;
                        pal.color[n].blue=b;
                        flag[n]=1;
                }
        }
        fclose (inf);
        for (i=0;i<256;i++)
	{
                if (flag[i]==0)
                {
                        printf("Colour %d not loaded\n",i);
                        do_error("Can't continue");
                }
		pal.color[i].red=(unsigned char)((pal.color[i].red
		*(1-gamma_correct_red))+(64*sin((float)pal.color[i].red
		*M_PI/128))*gamma_correct_red);

                pal.color[i].green=(unsigned char)((pal.color[i].green
                *(1-gamma_correct_green))+(64*sin((float)pal.color[i].green
		*M_PI/128))*gamma_correct_green);

                pal.color[i].blue=(unsigned char)((pal.color[i].blue
                *(1-gamma_correct_blue))+(64*sin((float)pal.color[i].blue
		*M_PI/128))*gamma_correct_blue);
	}
        gl_setpalette(&pal);
}
#endif


#ifndef LC_X11
void initfont()
{
#ifdef USE_EXPANDED_FONT
        void *font;
        font = malloc(256 * 8 * 8 * BYTESPERPIXEL);
        gl_expandfont(8, 8, white(24), gl_font8x8, font);
        gl_setfont(8, 8, font);
#else
	gl_setwritemode(FONT_COMPRESSED);
	gl_setfont(8,8,gl_font8x8);
	Fgl_setfontcolors(TEXT_BG_COLOUR,TEXT_FG_COLOUR);
#endif
}
#endif

void update_mini_screen(void)
{
        switch (mini_screen_flags)
        {
		case (0): draw_mini_screen(); break;
	        case (MINI_SCREEN_POL_FLAG):
                        draw_mini_screen_pollution(); break;
                case (MINI_SCREEN_UB40_FLAG):
                        draw_mini_screen_ub40(); break;
                case (MINI_SCREEN_STARVE_FLAG):
                        draw_mini_screen_starve(); break;
		case (MINI_SCREEN_PORT_FLAG):
			draw_mini_screen_port(); break;
		case (MINI_SCREEN_POWER_FLAG):
			draw_mini_screen_power(); break;
        }
}

void draw_mini_screen(void)
{
	int x,y,xx,yy;
	mini_screen_flags=0;
	Fgl_write(MINI_SCREEN_X+12,MINI_SCREEN_Y+110,"          ");
	for (y=0;y<WORLD_SIDE_LEN;y++)
	for (x=0;x<WORLD_SIDE_LEN;x++)
	{
		if (mappointtype[x][y]==CST_USED)
		{
			xx=mappoint[x][y].int_1;
			yy=mappoint[x][y].int_2;
// printf("%d %d %d %d\n",x,y,xx,yy);
			Fgl_setpixel(MINI_SCREEN_X+x,MINI_SCREEN_Y+y
			,group_colour[main_types\
			[mappointtype[xx][yy]].group]);
		}
		else
			Fgl_setpixel(MINI_SCREEN_X+x,MINI_SCREEN_Y+y
			,group_colour[main_types[mappointtype[x][y]].group]);
	}
	draw_mini_screen_cursor();
}

void draw_big_mini_screen(void)
{
        int x,y,xx,yy;
//        mini_screen_flags=0;
//        Fgl_write(MINI_SCREEN_X+12,MINI_SCREEN_Y+110,"          ");
        for (y=0;y<WORLD_SIDE_LEN;y++)
        for (x=0;x<WORLD_SIDE_LEN;x++)
        {
                if (mappointtype[x][y]==CST_USED)
                {
                        xx=mappoint[x][y].int_1;
                        yy=mappoint[x][y].int_2;
// printf("%d %d %d %d\n",x,y,xx,yy);
                        Fgl_fillbox(MAIN_WIN_X+x*4,MAIN_WIN_Y+y*4,4,4
                        ,group_colour[main_types\
                        [mappointtype[xx][yy]].group]);
                }
                else
                        Fgl_fillbox(MAIN_WIN_X+x*4,MAIN_WIN_Y+y*4,4,4
			,group_colour[main_types[mappointtype[x][y]].group]);
        }
//        draw_mini_screen_cursor();
}


void draw_mini_screen_pollution(void)
{
        int x,y,col;
	mini_screen_flags=MINI_SCREEN_POL_FLAG;
	Fgl_write(MINI_SCREEN_X+12,MINI_SCREEN_Y+110,"Pollution ");
        for (y=0;y<WORLD_SIDE_LEN;y++)
        for (x=0;x<WORLD_SIDE_LEN;x++)
        {
		if (mappointpol[x][y]<4)
			col=green(24);
		else if (mappointpol[x][y]<600)
			col=green(23-(mappointpol[x][y]/45));
		else
		{
			col=(int)sqrt((float)(mappointpol[x][y]-600))/9;
			if (col>20)
				col=20;
			col+=red(11);
		}
                Fgl_setpixel(MINI_SCREEN_X+x,MINI_SCREEN_Y+y,col);
        }
	draw_mini_screen_cursor();
}

void draw_mini_screen_fire_cover(void)
{
        int x,y;
	draw_mini_screen(); // this sets mini_screen_flags=0;
	Fgl_write(MINI_SCREEN_X+12,MINI_SCREEN_Y+110,"Fire cover");
        for (y=0;y<WORLD_SIDE_LEN;y++)
        for (x=0;x<WORLD_SIDE_LEN;x++)
        {
                if ((mappoint[x][y].flags&FLAG_FIRE_COVER)==0)
                	Fgl_setpixel(MINI_SCREEN_X+x
				,MINI_SCREEN_Y+y,green(10));
        }
}

void draw_mini_screen_cricket_cover(void)
{
        int x,y;
        draw_mini_screen();
	Fgl_write(MINI_SCREEN_X+12,MINI_SCREEN_Y+110,"Crickt cvr");
        for (y=0;y<WORLD_SIDE_LEN;y++)
        for (x=0;x<WORLD_SIDE_LEN;x++)
        {
                if ((mappoint[x][y].flags&FLAG_CRICKET_COVER)==0)
                        Fgl_setpixel(MINI_SCREEN_X+x
                                ,MINI_SCREEN_Y+y,green(10));
        }
}


void draw_mini_screen_health_cover(void)
{
        int x,y;
        draw_mini_screen();
	Fgl_write(MINI_SCREEN_X+12,MINI_SCREEN_Y+110,"Health cvr");
        for (y=0;y<WORLD_SIDE_LEN;y++)
        for (x=0;x<WORLD_SIDE_LEN;x++)
        {
                if ((mappoint[x][y].flags&FLAG_HEALTH_COVER)==0)
                        Fgl_setpixel(MINI_SCREEN_X+x
                                ,MINI_SCREEN_Y+y,green(10));
        }
}

void draw_mini_screen_ub40(void)
{
        int x,y,col;
	mini_screen_flags=MINI_SCREEN_UB40_FLAG;
	Fgl_write(MINI_SCREEN_X+12,MINI_SCREEN_Y+110,"Unemploy't");
	Fgl_fillbox(MINI_SCREEN_X,MINI_SCREEN_Y
		,WORLD_SIDE_LEN,WORLD_SIDE_LEN,green(14));
        for (y=0;y<WORLD_SIDE_LEN;y++)
        for (x=0;x<WORLD_SIDE_LEN;x++)
                if (main_types[mappointtype[x][y]].group==GROUP_RESIDENCE)
		{
			if (mappoint[x][y].int_1<-20)
				col=red(28);
			else if (mappoint[x][y].int_1<10)
                        	col=red(14);
			else
				col=green(20);
                	Fgl_fillbox(MINI_SCREEN_X+x,MINI_SCREEN_Y+y,3,3,col);
		}
	draw_mini_screen_cursor();
}

void draw_mini_screen_starve(void)
{
        int x,y,col;
	mini_screen_flags=MINI_SCREEN_STARVE_FLAG;
	Fgl_write(MINI_SCREEN_X+12,MINI_SCREEN_Y+110,"Starvation");
        Fgl_fillbox(MINI_SCREEN_X,MINI_SCREEN_Y
                ,WORLD_SIDE_LEN,WORLD_SIDE_LEN,green(14));
        for (y=0;y<WORLD_SIDE_LEN;y++)
        for (x=0;x<WORLD_SIDE_LEN;x++)
                if (main_types[mappointtype[x][y]].group==GROUP_RESIDENCE)
                {
                        if ((total_time-mappoint[x][y].int_2)<20)
                                col=red(28);
                        else if ((total_time-mappoint[x][y].int_2)<100)
                                col=red(14);
                        else
                                col=green(20);
			Fgl_fillbox(MINI_SCREEN_X+x,MINI_SCREEN_Y+y,3,3,col);
                }
	draw_mini_screen_cursor();
}

void draw_mini_screen_coal(void)
{
        int x,y,col;
	mini_screen_flags=0;
	Fgl_write(MINI_SCREEN_X+12,MINI_SCREEN_Y+110,"Coal resrv");
        for (y=0;y<WORLD_SIDE_LEN;y++)
        for (x=0;x<WORLD_SIDE_LEN;x++)
        {
                if (mappoint[x][y].coal_reserve==0)
                        col=white(4);
                else if (mappoint[x][y].coal_reserve>=COAL_RESERVE_SIZE/2)
                        col=white(18);
                else if (mappoint[x][y].coal_reserve<COAL_RESERVE_SIZE/2)
			col=white(28);
                Fgl_setpixel(MINI_SCREEN_X+x,MINI_SCREEN_Y+y,col);
        }
	draw_mini_screen_cursor();
}

void draw_mini_screen_power(void)
{
        int x,y,i,xx,yy,xmin,xmax,ymin,ymax;
        mini_screen_flags=MINI_SCREEN_POWER_FLAG;
        Fgl_write(MINI_SCREEN_X+24,MINI_SCREEN_Y+110,"Power");
        Fgl_fillbox(MINI_SCREEN_X,MINI_SCREEN_Y
                ,WORLD_SIDE_LEN,WORLD_SIDE_LEN,green(8));
//        for (y=0;y<WORLD_SIDE_LEN;y++)

        for (i=0;i<numof_substations;i++)
	{
		x=substationx[i];
		y=substationy[i];
		if ((xmin=x-SUBSTATION_RANGE)<1)
			xmin=1;
		if ((ymin=y-SUBSTATION_RANGE)<1)
			ymin=1;
		if ((xmax=x+SUBSTATION_RANGE)>=WORLD_SIDE_LEN-1)
			xmax=WORLD_SIDE_LEN-2;
		if ((ymax=y+SUBSTATION_RANGE)>=WORLD_SIDE_LEN-1)
			ymax=WORLD_SIDE_LEN-2;
		if (mappoint[x][y].int_1>0)  // we have power
		{
			for (xx=xmin;xx<xmax;xx++)
			for (yy=ymin;yy<ymax;yy++)
			{
                		if (mappointtype[xx][yy]==CST_USED)
                		{
                        		x=mappoint[xx][yy].int_1;
                        		y=mappoint[xx][yy].int_2;
		                        Fgl_setpixel(MINI_SCREEN_X+xx
					,MINI_SCREEN_Y+yy
                ,group_colour[main_types[mappointtype[x][y]].group]);
                		}
                		else
                        		Fgl_setpixel(MINI_SCREEN_X+xx
						,MINI_SCREEN_Y+yy
		,group_colour[main_types[mappointtype[xx][yy]].group]);

                	}
		}
	}
        draw_mini_screen_cursor();
}


void draw_mini_screen_ocost(void)
{
        char s[100];
	mini_screen_flags=0;
	Fgl_write(MINI_SCREEN_X+12,MINI_SCREEN_Y+110,"Other cost");
        Fgl_fillbox(MINI_SCREEN_X,MINI_SCREEN_Y
                ,WORLD_SIDE_LEN,WORLD_SIDE_LEN,green(12));
	Fgl_setfontcolors(green(12),TEXT_FG_COLOUR);
	sprintf(s,"OC yr %04d",(total_time/NUMOF_DAYS_IN_YEAR)-1);
	Fgl_write(MINI_SCREEN_X+10,MINI_SCREEN_Y+2,s);

        if (ly_interest>19999)
        {
                if (ly_interest>1000000)
                        ly_interest=1000000;
                sprintf(s,"Int %4dK",ly_interest/1000);
        }
        else
                sprintf(s,"Int %5d",ly_interest);
	Fgl_write(MINI_SCREEN_X+10,MINI_SCREEN_Y+16,s);

        if (ly_school_cost>19999)
        {
                if (ly_school_cost>1999999)
                        sprintf(s,"Scl %4dM",ly_school_cost/1000000);
                else
                        sprintf(s,"Scl %4dK",ly_school_cost/1000);
        }
        else
                sprintf(s,"Scl %5d",ly_school_cost);
        Fgl_write(MINI_SCREEN_X+10,MINI_SCREEN_Y+24,s);

        if (ly_university_cost>19999)
        {
                if (ly_university_cost>1999999)
                        sprintf(s,"Uni %4dM",ly_university_cost/1000000);
                else
                        sprintf(s,"Uni %4dK",ly_university_cost/1000);
        }
        else
                sprintf(s,"Uni %5d",ly_university_cost);
	Fgl_write(MINI_SCREEN_X+10,MINI_SCREEN_Y+32,s);

        if (ly_deaths_cost>19999)
        {
                if (ly_deaths_cost>1999999)
                        sprintf(s,"UnD %4dM",ly_deaths_cost/1000000);
                else
                        sprintf(s,"UnD %4dK",ly_deaths_cost/1000);
        }
        else
                sprintf(s,"UnD %5d",ly_deaths_cost);
        Fgl_write(MINI_SCREEN_X+10,MINI_SCREEN_Y+40,s);

        if (ly_windmill_cost>19999)
        {
                if (ly_windmill_cost>1999999)
                        sprintf(s,"WiM %4dM",ly_windmill_cost/1000000);
                else
                        sprintf(s,"WiM %4dK",ly_windmill_cost/1000);
        }
        else
                sprintf(s,"WiM %5d",ly_windmill_cost);
        Fgl_write(MINI_SCREEN_X+10,MINI_SCREEN_Y+48,s);

        if (ly_recycle_cost>19999)
        {
                if (ly_recycle_cost>1999999)
                        sprintf(s,"Rcy %4dM",ly_recycle_cost/1000000);
                else
                        sprintf(s,"Rcy %4dK",ly_recycle_cost/1000);
        }
        else
                sprintf(s,"Rcy %5d",ly_recycle_cost);
        Fgl_write(MINI_SCREEN_X+10,MINI_SCREEN_Y+56,s);

        if (ly_health_cost>19999)
        {
                if (ly_health_cost>1999999)
                        sprintf(s,"Hth %4dM",ly_health_cost/1000000);
                else
                        sprintf(s,"Hth %4dK",ly_health_cost/1000);
        }
        else
                sprintf(s,"Hth %5d",ly_health_cost);
        Fgl_write(MINI_SCREEN_X+10,MINI_SCREEN_Y+64,s);

        if (ly_rocket_pad_cost>19999)
        {
                if (ly_rocket_pad_cost>1999999)
                        sprintf(s,"Rok %4dM",ly_rocket_pad_cost/1000000);
                else
                        sprintf(s,"Rok %4dK",ly_rocket_pad_cost/1000);
        }
        else
                sprintf(s,"Rok %5d",ly_rocket_pad_cost);
        Fgl_write(MINI_SCREEN_X+10,MINI_SCREEN_Y+72,s);

        if (ly_fire_cost>19999)
        {
                if (ly_fire_cost>1999999)
                        sprintf(s,"Fst %4dM",ly_fire_cost/1000000);
                else
                        sprintf(s,"Fst %4dK",ly_fire_cost/1000);
        }
        else
                sprintf(s,"Fst %5d",ly_fire_cost);
        Fgl_write(MINI_SCREEN_X+10,MINI_SCREEN_Y+80,s);

        if (ly_cricket_cost>19999)
        {
                if (ly_cricket_cost>1999999)
                        sprintf(s,"Ckt %4dM",ly_cricket_cost/1000000);
                else
                        sprintf(s,"Ckt %4dK",ly_cricket_cost/1000);
        }
        else
                sprintf(s,"Ckt %5d",ly_cricket_cost);
        Fgl_write(MINI_SCREEN_X+10,MINI_SCREEN_Y+88,s);

	Fgl_setfontcolors(TEXT_BG_COLOUR,TEXT_FG_COLOUR);
}

void draw_mini_screen_port(void)
{
	char buy[256],sell[256],s[256];
	int l,i,*p1,*p2;
	mini_screen_flags=MINI_SCREEN_PORT_FLAG;
	Fgl_fillbox(MINI_SCREEN_X,MINI_SCREEN_Y
		,WORLD_SIDE_LEN,WORLD_SIDE_LEN,green(12));
	Fgl_setfontcolors(green(12),TEXT_FG_COLOUR);
	sprintf(s," Port %2d %2d",mini_screen_port_x,mini_screen_port_y);
	Fgl_write(MINI_SCREEN_X+4,MINI_SCREEN_Y+2,s);
	Fgl_write(MINI_SCREEN_X+4,MINI_SCREEN_Y+16," F C O G S");
	Fgl_write(MINI_SCREEN_X+4,MINI_SCREEN_Y+26," Buy   Sell");

	p1=&(mappoint[mini_screen_port_x][mini_screen_port_y+1].int_3);
	p2=&(mappoint[mini_screen_port_x][mini_screen_port_y+2].int_3);
	for (i=0;i<5;i++)
	{
		l=*(p1++)/100;
        	if (l>19999)
        	{
        	        if (l>1999999)
        	                sprintf(buy,"%4dM",l/1000000);
        	        else
        	                sprintf(buy,"%4dK",l/1000);
        	}
        	else
        	        sprintf(buy,"%5d",l);
        	l=*(p2++)/100;
        	if (l>19999)
        	{
        	        if (l>1999999)
        	                sprintf(sell,"%4dM",l/1000000);
        	        else
        	                sprintf(sell,"%4dK",l/1000);
        	}
        	else
        	        sprintf(sell,"%5d",l);
		sprintf(s,"%s %s",buy,sell);
        	Fgl_write(MINI_SCREEN_X+10,MINI_SCREEN_Y+40+i*8,s);
	}

	Fgl_setfontcolors(TEXT_BG_COLOUR,TEXT_FG_COLOUR);
}


void draw_mini_screen_cursor(void)
{
	Fgl_hline(MINI_SCREEN_X+main_screen_originx
		,MINI_SCREEN_Y+main_screen_originy
		,MINI_SCREEN_X+main_screen_originx+MAIN_WIN_W/16-1
		,green(30));
	Fgl_hline(MINI_SCREEN_X+main_screen_originx
		,MINI_SCREEN_Y+main_screen_originy+MAIN_WIN_H/16-1
		,MINI_SCREEN_X+main_screen_originx+MAIN_WIN_W/16-1
		,green(30));
	Fgl_line(MINI_SCREEN_X+main_screen_originx
		,MINI_SCREEN_Y+main_screen_originy
		,MINI_SCREEN_X+main_screen_originx
		,MINI_SCREEN_Y+main_screen_originy+MAIN_WIN_H/16-1
		,green(30));
	Fgl_line(MINI_SCREEN_X+main_screen_originx+MAIN_WIN_W/16-1
		,MINI_SCREEN_Y+main_screen_originy
		,MINI_SCREEN_X+main_screen_originx+MAIN_WIN_W/16-1
		,MINI_SCREEN_Y+main_screen_originy+MAIN_WIN_H/16-1
		,green(30));

}

void print_stats(void)
{
	static int flag=0;
	static int tfood_in_markets=0,tjobs_in_markets=0;
	static int tcoal_in_markets=0,tgoods_in_markets=0;
	static int tore_in_markets=0,tsteel_in_markets=0;
	int i;
	char s[100];
	if (flag==0)
	{
		hide_mouse();
		Fgl_write(STATS_X,STATS_Y,"Population ");
		Fgl_write(STATS_X,STATS_Y+8,"% Starving ");
		Fgl_write(STATS_X,STATS_Y+16,"Foodstore ");
		Fgl_write(STATS_X,STATS_Y+24,"% u-ployed ");
		Fgl_write(STATS_X+18*8,STATS_Y,"Tech level   ");
		Fgl_write(STATS_X+18*8,STATS_Y+8,"Un-nat deaths ");
		Fgl_write(STATS_X+18*8,STATS_Y+16,"Rockets  ");
		Fgl_write(STATS_X+18*8,STATS_Y+24,"Shanty towns");
		for (i=1;i<8;i++)
                {
// stats window
                        Fgl_hline(STATS_X-1-i,STATS_Y-1-i
                                ,STATS_X+STATS_W+1+i
                                ,yellow(16));
                        Fgl_line(STATS_X-1-i,STATS_Y-1-i
                                ,STATS_X-1-i
                                ,STATS_Y+STATS_H+1+i
                                ,yellow(14));
                        Fgl_hline(STATS_X-1-i
                                ,STATS_Y+STATS_H+1+i
                                ,STATS_X+STATS_W+1+i
                                ,yellow(22));
                        Fgl_line(STATS_X+STATS_W+1+i
                                ,STATS_Y-1-i
                                ,STATS_X+STATS_W+1+i
                                ,STATS_Y+STATS_H+1+i
                                ,yellow(24));

// finance window
                        Fgl_hline(FINANCE_X-1-i,FINANCE_Y-1-i
                                ,FINANCE_X+FINANCE_W+1+i
                                ,yellow(16));
                        Fgl_line(FINANCE_X-1-i,FINANCE_Y-1-i
                                ,FINANCE_X-1-i
                                ,FINANCE_Y+FINANCE_H+1+i
                                ,yellow(14));
                        Fgl_hline(FINANCE_X-1-i
                                ,FINANCE_Y+FINANCE_H+1+i
                                ,FINANCE_X+FINANCE_W+1+i
                                ,yellow(22));
                        Fgl_line(FINANCE_X+FINANCE_W+1+i
                                ,FINANCE_Y-1-i
                                ,FINANCE_X+FINANCE_W+1+i
                                ,FINANCE_Y+FINANCE_H+1+i
                                ,yellow(24));
		}
		Fgl_write(FINANCE_X,FINANCE_Y,"Income  Expend");
		Fgl_write(FINANCE_X,FINANCE_Y+8,"IT");
		Fgl_write(FINANCE_X,FINANCE_Y+16,"CT");
		Fgl_write(FINANCE_X,FINANCE_Y+24,"GT");
		Fgl_write(FINANCE_X,FINANCE_Y+32,"XP");
		Fgl_write(FINANCE_X+64,FINANCE_Y+8,"OC");
		Fgl_write(FINANCE_X+64,FINANCE_Y+16,"UC");
		Fgl_write(FINANCE_X+64,FINANCE_Y+24,"TC");
		Fgl_write(FINANCE_X+64,FINANCE_Y+32,"IP");
		Fgl_write(FINANCE_X,FINANCE_Y+48,"Tot");
		income_tax_rate=INCOME_TAX_RATE;
		coal_tax_rate=COAL_TAX_RATE;
		goods_tax_rate=GOODS_TAX_RATE;
		dole_rate=DOLE_RATE;
		transport_cost_rate=TRANSPORT_COST_RATE;
		import_cost_rate=IM_PORT_COST_RATE;
		flag=1;
		redraw_mouse();
	}
	tpopulation+=population;
	tstarving_population+=starving_population;
	tfood_in_markets+=food_in_markets/1000;
	tjobs_in_markets+=jobs_in_markets/1000;
	tcoal_in_markets+=coal_in_markets/250;
	tgoods_in_markets+=goods_in_markets/500;
	tore_in_markets+=ore_in_markets/500;
	tsteel_in_markets+=steel_in_markets/25;
	tunemployed_population+=unemployed_population;

	if (total_time%NUMOF_DAYS_IN_MONTH==(NUMOF_DAYS_IN_MONTH-5))
		update_select_buttons();
	hide_mouse();
	if (total_time%NUMOF_DAYS_IN_MONTH==(NUMOF_DAYS_IN_MONTH-20))
	{
		do_pbar_population(population);
		do_pbar_tech(tech_level);
		do_pbar_money(total_money/400);
	}
	else if (total_time%NUMOF_DAYS_IN_MONTH==(NUMOF_DAYS_IN_MONTH-1))
	{
		do_monthgraph();
		housed_population=(tpopulation/NUMOF_DAYS_IN_MONTH);
		if ((housed_population+people_pool)>max_pop_ever)
			max_pop_ever=housed_population+people_pool;
		sprintf(s,"%5d ",housed_population+people_pool);
		Fgl_write(STATS_X+8*11,STATS_Y,s);

		i=((tstarving_population/NUMOF_DAYS_IN_MONTH)
			*1000)/((tpopulation/NUMOF_DAYS_IN_MONTH)+1);
		sprintf(s,"%3d.%1d ",i/10,i%10);
		Fgl_write(STATS_X+8*11,STATS_Y+8,s);

		sprintf(s,"%5d ",tfood_in_markets/NUMOF_DAYS_IN_MONTH);
		Fgl_write(STATS_X+8*11,STATS_Y+16,s);
		do_pbar_food(tfood_in_markets/NUMOF_DAYS_IN_MONTH);
		do_pbar_jobs(tjobs_in_markets/NUMOF_DAYS_IN_MONTH);
		do_pbar_coal(tcoal_in_markets/NUMOF_DAYS_IN_MONTH);
		do_pbar_goods(tgoods_in_markets/NUMOF_DAYS_IN_MONTH);
		do_pbar_ore(tore_in_markets/NUMOF_DAYS_IN_MONTH);
		do_pbar_steel(tsteel_in_markets/NUMOF_DAYS_IN_MONTH);
		i=((tunemployed_population/NUMOF_DAYS_IN_MONTH)
			*1000)/((tpopulation/NUMOF_DAYS_IN_MONTH)+1);
		sprintf(s,"%3d.%1d ",i/10,i%10);
		Fgl_write(STATS_X+8*11,STATS_Y+24,s);

//		people_pool-=(people_pool/50);
// 
		if (people_pool>100)
		{
			if (rand()%1000<people_pool)
				people_pool-=10;
//                	if (rand()%1000<people_pool)
//                	        people_pool-=5;
		}
		if (people_pool<0)
			people_pool=0;

                sprintf(s,"%5d ",numof_shanties);
                Fgl_write(STATS_X+8*(12+20),STATS_Y+24,s);


		if (tech_level>TECH_LEVEL_LOSS_START)
		{
			tech_level-=(tech_level/(TECH_LEVEL_LOSS
				-((tpopulation/NUMOF_DAYS_IN_MONTH)/120)));
		}
		else
			tech_level+=TECH_LEVEL_UNAIDED;
// we can go over 100, but it's even more difficult
		if (tech_level>MAX_TECH_LEVEL)
			tech_level-=((tech_level-MAX_TECH_LEVEL)
				/(TECH_LEVEL_LOSS
				-((tpopulation/NUMOF_DAYS_IN_MONTH)/80)));
		if (highest_tech_level<tech_level)
			highest_tech_level=tech_level;
		sprintf(s,"%5.1f ",(float)tech_level*100.0/MAX_TECH_LEVEL);
		Fgl_write(STATS_X+(20+12)*8,STATS_Y,s);

                sprintf(s,"%5d ",unnat_deaths);
                Fgl_write(STATS_X+(20+12)*8,STATS_Y+8,s);
		deaths_cost+=unnat_deaths*UNNAT_DEATHS_COST;

	        sprintf(s,"%2d/%2d",rockets_launched_success,rockets_launched);
        	Fgl_write(STATS_X+(20+12)*8,STATS_Y+16,s);

		tpopulation=tstarving_population=tfood_in_markets
			=tjobs_in_markets=tcoal_in_markets=tgoods_in_markets
			=tore_in_markets=tsteel_in_markets
			=tunemployed_population=unnat_deaths=0;
	}	
	unemployed_population=0;
	if (total_time%NUMOF_DAYS_IN_YEAR==(NUMOF_DAYS_IN_YEAR-1))
	{
		income_tax=(income_tax*income_tax_rate)/100;
		if (income_tax>19999)
		{
			if (income_tax>1999999)
				sprintf(s,"%4dM",income_tax/1000000);
			else
				sprintf(s,"%4dK",income_tax/1000);
		}
		else
			sprintf(s,"%5d",income_tax);
		total_money+=income_tax;
		Fgl_write(FINANCE_X+2*8,FINANCE_Y+8,s);

		coal_tax=(coal_tax*coal_tax_rate)/100;
		if (coal_tax>19999)
		{
			if (coal_tax>1999999)
				sprintf(s,"%4dM",coal_tax/1000000);
			else
				sprintf(s,"%4dK",coal_tax/1000);
		}
		else
			sprintf(s,"%5d",coal_tax);
		total_money+=coal_tax;
		Fgl_write(FINANCE_X+2*8,FINANCE_Y+16,s);

                goods_tax=(goods_tax*goods_tax_rate)/100;
		goods_tax+=(int)((float)(goods_tax*goods_tax_rate)
			*(float)tech_level/2000000.0);
                if (goods_tax>19999)
                {
                        if (goods_tax>1999999)
                                sprintf(s,"%4dM",goods_tax/1000000);
                        else
                                sprintf(s,"%4dK",goods_tax/1000);
                }
                else
                        sprintf(s,"%5d",goods_tax);
                total_money+=goods_tax;
                Fgl_write(FINANCE_X+2*8,FINANCE_Y+24,s);

// The price of exports on the world market drops as you export more.
// The exporters have to discount there wares, therefore the tax take is
// less.
		if (export_tax>ex_tax_dis[0])
		{
			int discount,disi;
			discount=0;
			for (disi=0;disi<NUMOF_DISCOUNT_TRIGGERS
				&& export_tax>ex_tax_dis[disi];disi++)
				discount+=(export_tax-ex_tax_dis[disi])/10;
			export_tax-=discount;
// debug_writeval(discount);
		}

                if (export_tax>19999)
                {
                        if (export_tax>1999999)
                                sprintf(s,"%4dM",export_tax/1000000);
                        else
                                sprintf(s,"%4dK",export_tax/1000);
                }
                else
                        sprintf(s,"%5d",export_tax);
                total_money+=export_tax;
                Fgl_write(FINANCE_X+2*8,FINANCE_Y+32,s);
		ly_university_cost=university_cost;
		ly_recycle_cost=recycle_cost;
		ly_deaths_cost=deaths_cost;
		ly_health_cost=(health_cost*(tech_level/10000)
			*HEALTH_RUNNING_COST_MUL)/(MAX_TECH_LEVEL/10000);
		ly_rocket_pad_cost=rocket_pad_cost;
		ly_school_cost=school_cost;
		ly_windmill_cost=windmill_cost;
		ly_fire_cost=(fire_cost*(tech_level/10000)
			*FIRESTATION_RUNNING_COST_MUL)
			/(MAX_TECH_LEVEL/10000);
		ly_cricket_cost=cricket_cost;
		if (total_money<0)
			ly_interest=(-total_money*INTEREST_RATE)/1000;
		else
			ly_interest=0;
		other_cost=university_cost+recycle_cost+deaths_cost
			+ly_health_cost+rocket_pad_cost+school_cost
			+ly_interest+windmill_cost+ly_fire_cost
			+ly_cricket_cost;
                if (other_cost>19999)
                {
                        if (other_cost>1999999)
                                sprintf(s,"%4dM",other_cost/1000000);
                        else
                                sprintf(s,"%4dK",other_cost/1000);
                }
                else
                        sprintf(s,"%5d",other_cost);
		total_money-=other_cost;
		Fgl_write(FINANCE_X+10*8,FINANCE_Y+8,s);

		unemployment_cost=(unemployment_cost*dole_rate)/100;
                if (unemployment_cost>19999)
                {
                        if (unemployment_cost>1999999)
                                sprintf(s,"%4dM",unemployment_cost/1000000);
                        else
                                sprintf(s,"%4dK",unemployment_cost/1000);
                }
                else
                        sprintf(s,"%5d",unemployment_cost);
		total_money-=unemployment_cost;
		Fgl_write(FINANCE_X+10*8,FINANCE_Y+16,s);

		transport_cost=(transport_cost*transport_cost_rate)/100;
                if (transport_cost>19999)
                {
                        if (transport_cost>1999999)
                                sprintf(s,"%4dM",transport_cost/1000000);
                        else
                                sprintf(s,"%4dK",transport_cost/1000);
                }
                else
                        sprintf(s,"%5d",transport_cost);
		total_money-=transport_cost;
		Fgl_write(FINANCE_X+10*8,FINANCE_Y+24,s);

                import_cost=(import_cost*import_cost_rate)/100;
                if (import_cost>19999)
                {
                        if (import_cost>1999999)
                                sprintf(s,"%4dM",import_cost/1000000);
                        else
                                sprintf(s,"%4dK",import_cost/1000);
                }
                else
                        sprintf(s,"%5d",import_cost);
                total_money-=import_cost;
                Fgl_write(FINANCE_X+10*8,FINANCE_Y+32,s);
	        income_tax=coal_tax=unemployment_cost=transport_cost
        	        =goods_tax=export_tax=import_cost=windmill_cost
			=university_cost=recycle_cost=deaths_cost
			=health_cost=rocket_pad_cost=school_cost
			=fire_cost=cricket_cost=0;

		print_total_money();
	}
	redraw_mouse();
}

void print_total_money(void)
{
	char s[100],s2[100],i;
	if (total_money>2000000000)
		total_money=2000000000;
	else if (total_money<-2000000000)
		total_money=-2000000000;
	if (total_money/1000000==0)
		strcpy(s,"     ");
	else
		sprintf(s,"%5d",total_money/1000000);
	if ((total_money<0) && (total_money/1000000 !=0))
		sprintf(s2," %06d",-total_money%1000000);
	else if (total_money/1000000 !=0)
		sprintf(s2," %06d",total_money%1000000);
	else
        	sprintf(s2,"%7d",total_money%1000000);
	if (total_money/1000000!=0)
		s2[0]=MONEY_SEPARATOR;
	strcat(s,s2);
	i=3;
	if (total_money<0)
		Fgl_setfontcolors(TEXT_BG_COLOUR,red(30));
        Fgl_write(FINANCE_X+i*8,FINANCE_Y+48,s);
	if (total_money<0)
		Fgl_setfontcolors(TEXT_BG_COLOUR,TEXT_FG_COLOUR);
}

void do_monthgraph(void)
{
	static int flag=0;
	int i;
	float f;
	if (flag==0)
	{
	        Fgl_fillbox(MONTHGRAPH_X,MONTHGRAPH_Y,MONTHGRAPH_W+1
                	,MONTHGRAPH_H+1,GRAPHS_B_COLOUR);
                Fgl_fillbox(MAPPOINT_STATS_X
                        ,MAPPOINT_STATS_Y,MAPPOINT_STATS_W
                        ,MAPPOINT_STATS_H,GRAPHS_B_COLOUR);
		flag=1;
// draw borders
                for (i=1;i<8;i++)
                {
// pop graph
                        Fgl_hline(MONTHGRAPH_X-1-i,MONTHGRAPH_Y-1-i
                                ,MONTHGRAPH_X+MONTHGRAPH_W+1+i
                                ,yellow(16));
                        Fgl_line(MONTHGRAPH_X-1-i,MONTHGRAPH_Y-1-i
                                ,MONTHGRAPH_X-1-i
                                ,MONTHGRAPH_Y+MONTHGRAPH_H+1+i
                                ,yellow(14));
                        Fgl_hline(MONTHGRAPH_X-1-i
                                ,MONTHGRAPH_Y+MONTHGRAPH_H+1+i
                                ,MONTHGRAPH_X+MONTHGRAPH_W+1+i
                                ,yellow(22));
                        Fgl_line(MONTHGRAPH_X+MONTHGRAPH_W+1+i
                                ,MONTHGRAPH_Y-1-i
                                ,MONTHGRAPH_X+MONTHGRAPH_W+1+i
                                ,MONTHGRAPH_Y+MONTHGRAPH_H+1+i
                                ,yellow(24));
// diff graph
                        Fgl_hline(MAPPOINT_STATS_X-1-i,MAPPOINT_STATS_Y-1-i
                                ,MAPPOINT_STATS_X+MAPPOINT_STATS_W+1+i
                                ,yellow(16));
                        Fgl_line(MAPPOINT_STATS_X-1-i,MAPPOINT_STATS_Y-1-i
                                ,MAPPOINT_STATS_X-1-i
                                ,MAPPOINT_STATS_Y+MAPPOINT_STATS_H+1+i
                                ,yellow(14));
                        Fgl_hline(MAPPOINT_STATS_X-1-i
                                ,MAPPOINT_STATS_Y+MAPPOINT_STATS_H+1+i
                                ,MAPPOINT_STATS_X+MAPPOINT_STATS_W+1+i
                                ,yellow(22));
                        Fgl_line(MAPPOINT_STATS_X+MAPPOINT_STATS_W+1+i
                                ,MAPPOINT_STATS_Y-1-i
                                ,MAPPOINT_STATS_X+MAPPOINT_STATS_W+1+i
                                ,MAPPOINT_STATS_Y+MAPPOINT_STATS_H+1+i
                                ,yellow(24));

                }
	}
	for (i=MONTHGRAPH_W-1;i>0;i--)
	{
		Fgl_setpixel(MONTHGRAPH_X+MONTHGRAPH_W-i
			,MONTHGRAPH_Y+MONTHGRAPH_H-monthgraph_pop[i]
			,GRAPHS_B_COLOUR);
		monthgraph_pop[i]=monthgraph_pop[i-1];
		Fgl_setpixel(MONTHGRAPH_X+MONTHGRAPH_W-i
			,MONTHGRAPH_Y+MONTHGRAPH_H-monthgraph_pop[i],7);

                Fgl_setpixel(MONTHGRAPH_X+MONTHGRAPH_W-i
                        ,MONTHGRAPH_Y+MONTHGRAPH_H-monthgraph_ppool[i]
                        ,GRAPHS_B_COLOUR);
                monthgraph_ppool[i]=monthgraph_ppool[i-1];
                Fgl_setpixel(MONTHGRAPH_X+MONTHGRAPH_W-i
                        ,MONTHGRAPH_Y+MONTHGRAPH_H-monthgraph_ppool[i]
                        ,cyan(24));

                Fgl_line(MONTHGRAPH_X+MONTHGRAPH_W-i
			,MONTHGRAPH_Y+MONTHGRAPH_H-monthgraph_nojobs[i]
			,MONTHGRAPH_X+MONTHGRAPH_W-i
                        ,MONTHGRAPH_Y+MONTHGRAPH_H
                        ,GRAPHS_B_COLOUR);
                monthgraph_nojobs[i]=monthgraph_nojobs[i-1];
                Fgl_line(MONTHGRAPH_X+MONTHGRAPH_W-i
			,MONTHGRAPH_Y+MONTHGRAPH_H-monthgraph_nojobs[i]
			,MONTHGRAPH_X+MONTHGRAPH_W-i
                        ,MONTHGRAPH_Y+MONTHGRAPH_H
			,yellow(24));

                Fgl_line(MONTHGRAPH_X+MONTHGRAPH_W-i
                        ,MONTHGRAPH_Y+MONTHGRAPH_H-monthgraph_starve[i]
			,MONTHGRAPH_X+MONTHGRAPH_W-i
			,MONTHGRAPH_Y+MONTHGRAPH_H
                        ,GRAPHS_B_COLOUR);
                monthgraph_starve[i]=monthgraph_starve[i-1];
                Fgl_line(MONTHGRAPH_X+MONTHGRAPH_W-i
                        ,MONTHGRAPH_Y+MONTHGRAPH_H-monthgraph_starve[i]
			,MONTHGRAPH_X+MONTHGRAPH_W-i
			,MONTHGRAPH_Y+MONTHGRAPH_H
                        ,red(24));


// diff graph stuff. We can't draw it because the box might be used to 
// show the mappoint stats.
		diffgraph_power[i]=diffgraph_power[i-1];
		diffgraph_population[i]=diffgraph_population[i-1];
		diffgraph_coal[i]=diffgraph_coal[i-1];
		diffgraph_goods[i]=diffgraph_goods[i-1];
		diffgraph_ore[i]=diffgraph_ore[i-1];
	}
	if (tpopulation>0)
	{
        Fgl_setpixel(MONTHGRAPH_X+MONTHGRAPH_W
                ,MONTHGRAPH_Y+MONTHGRAPH_H-monthgraph_pop[0],GRAPHS_B_COLOUR);
        Fgl_setpixel(MONTHGRAPH_X+MONTHGRAPH_W,MONTHGRAPH_Y
		+MONTHGRAPH_H-monthgraph_starve[0],GRAPHS_B_COLOUR);
        Fgl_setpixel(MONTHGRAPH_X+MONTHGRAPH_W
                ,MONTHGRAPH_Y+MONTHGRAPH_H-monthgraph_nojobs[0]
		,GRAPHS_B_COLOUR);
        Fgl_setpixel(MONTHGRAPH_X+MONTHGRAPH_W
                ,MONTHGRAPH_Y+MONTHGRAPH_H-monthgraph_ppool[0]
		,GRAPHS_B_COLOUR);
	monthgraph_pop[0]=((int)(log((tpopulation/NUMOF_DAYS_IN_MONTH)+1)
		*MONTHGRAPH_H/15))-5;
	if (monthgraph_pop[0]<0)
		monthgraph_pop[0]=0;
	f=((float)tstarving_population/((float)tpopulation+1.0))*100.0;
	if (tpopulation>3000)  // double the scale if pop > 3000
		f+=f;
	if (tpopulation>6000)  // double it AGAIN if pop > 6000
		f+=f;
	monthgraph_starve[0]=(int)f;
// max out at 32
	if (monthgraph_starve[0]>=MONTHGRAPH_H)
		monthgraph_starve[0]=MONTHGRAPH_H-1;
	f=((float)tunemployed_population/((float)tpopulation+1.0))*100.0;
        if (tpopulation>3000)  // double the scale if pop > 3000
                f+=f;
        if (tpopulation>6000)  // double it AGAIN if pop > 6000
                f+=f;
	monthgraph_nojobs[0]=(int)f;
// max out at 32 
	if(monthgraph_nojobs[0]>=MONTHGRAPH_H)
		monthgraph_nojobs[0]=MONTHGRAPH_H-1;
	monthgraph_ppool[0]=((int)(sqrt(people_pool+1)*MONTHGRAPH_H)/35);
	if (monthgraph_ppool[0]<0)
		monthgraph_ppool[0]=0;
	if (monthgraph_ppool[0]>=MONTHGRAPH_H)
		monthgraph_ppool[0]=MONTHGRAPH_H-1;
	Fgl_setpixel(MONTHGRAPH_X+MONTHGRAPH_W
		,MONTHGRAPH_Y+MONTHGRAPH_H-monthgraph_pop[0],7);
	Fgl_setpixel(MONTHGRAPH_X+MONTHGRAPH_W
		,MONTHGRAPH_Y+MONTHGRAPH_H-monthgraph_starve[0],red(24));
	Fgl_setpixel(MONTHGRAPH_X+MONTHGRAPH_W
		,MONTHGRAPH_Y+MONTHGRAPH_H-monthgraph_nojobs[0],yellow(24));
	Fgl_setpixel(MONTHGRAPH_X+MONTHGRAPH_W
		,MONTHGRAPH_Y+MONTHGRAPH_H-monthgraph_ppool[0],cyan(24));
	}
// diff graph new values
	i=(power_made-power_used)*4;
	power_made=power_used=0;
	if (i==0)
		diffgraph_power[0]=diffgraph_power[1]/2;
	else if (i<0)
	{
		if (i<-((MAPPOINT_STATS_H/4)-1))
			i=-((MAPPOINT_STATS_H/4)-1);
		diffgraph_power[0]=(i+diffgraph_power[1])/2;
	}
	else
	{
		if (i>(MAPPOINT_STATS_H/4)-1)
			i=(MAPPOINT_STATS_H/4)-1;
		diffgraph_power[0]=(i+diffgraph_power[1])/2;
	}
	i=(population-diff_old_population);
	diff_old_population=population;
	if (i==0)
		diffgraph_population[0]=diffgraph_population[1]/2;
	else if (i<0)
	{
		if (i<-((MAPPOINT_STATS_H/4)-1))
			i=-((MAPPOINT_STATS_H/4)-1);
		diffgraph_population[0]=(i+diffgraph_population[1])/2;
	}
	else
	{
		if (i>(MAPPOINT_STATS_H/4)-1)
			i=(MAPPOINT_STATS_H/4)-1;
		diffgraph_population[0]=(i+diffgraph_population[1])/2;
	}
	i=coal_made-coal_used;
	coal_made=coal_used=0;
	if (i==0)
		diffgraph_coal[0]=diffgraph_coal[1]/2;
	else if (i<0)
	{
		i=(int)(log(-i));
		if (i>(MAPPOINT_STATS_H/4)-1)
			i=(MAPPOINT_STATS_H/4)-1;
		diffgraph_coal[0]=(-i+diffgraph_coal[1])/2;
	}
	else
	{
		i=(int)(log(i));
		if (i>(MAPPOINT_STATS_H/4)-1)
			i=(MAPPOINT_STATS_H/4)-1;
		diffgraph_coal[0]=(i+diffgraph_coal[1])/2;
	}
	i=(goods_made-goods_used);
	goods_made=goods_used=0;
	if (i==0)
		diffgraph_goods[0]=diffgraph_goods[1]/2;
	else if (i<0)
	{
		i=(int)(log(-i));
		if (i>(MAPPOINT_STATS_H/4)-1)
			i=(MAPPOINT_STATS_H/4)-1;
		diffgraph_goods[0]=(-i+diffgraph_goods[1])/2;
	}
	else
	{
		i=(int)(log(i));
		if (i>(MAPPOINT_STATS_H/4)-1)
			i=(MAPPOINT_STATS_H/4)-1;
		diffgraph_goods[0]=(i+diffgraph_goods[1])/2;
	}
	i=(ore_made-ore_used)/2;
	ore_made=ore_used=0;
	if (i==0)
		diffgraph_ore[0]=diffgraph_ore[1]/2;
	else if (i<0)
	{
		i=(int)(log(-i));
		if (i>(MAPPOINT_STATS_H/4)-1)
			i=(MAPPOINT_STATS_H/4)-1;
		diffgraph_ore[0]=(-i+diffgraph_ore[1])/2;
	}
	else
	{
		i=(int)(log(i));
		if (i>(MAPPOINT_STATS_H/4)-1)
			i=(MAPPOINT_STATS_H/4)-1;
		diffgraph_ore[0]=(i+diffgraph_ore[1])/2;
	}
// now draw the graph if not used by mappoint stats
	if (mappoint_stats_flag==0)
		draw_diffgraph();
}

void draw_diffgraph(void)
{
	int i;
	// got to fill the whole box because we lost the old data
        Fgl_fillbox(MAPPOINT_STATS_X
                ,MAPPOINT_STATS_Y,MAPPOINT_STATS_W+1
                ,MAPPOINT_STATS_H+1,GRAPHS_B_COLOUR);
	for (i=MONTHGRAPH_W-1;i>=0;i--)
	{
		Fgl_setpixel(MAPPOINT_STATS_X+MAPPOINT_STATS_W-i
			,(MAPPOINT_STATS_Y+(3*MAPPOINT_STATS_H)/4)
			-diffgraph_ore[i],DIFFGRAPH_ORE_COLOUR);
                Fgl_setpixel(MAPPOINT_STATS_X+MAPPOINT_STATS_W-i
                        ,(MAPPOINT_STATS_Y+(3*MAPPOINT_STATS_H)/4)
                        -diffgraph_goods[i],DIFFGRAPH_GOODS_COLOUR);
                Fgl_setpixel(MAPPOINT_STATS_X+MAPPOINT_STATS_W-i
                        ,(MAPPOINT_STATS_Y+(3*MAPPOINT_STATS_H)/4)
                        -diffgraph_coal[i],DIFFGRAPH_COAL_COLOUR);
                Fgl_setpixel(MAPPOINT_STATS_X+MAPPOINT_STATS_W-i
                        ,(MAPPOINT_STATS_Y+MAPPOINT_STATS_H/4)
                        -diffgraph_population[i],DIFFGRAPH_POPULATION_COLOUR);
                Fgl_setpixel(MAPPOINT_STATS_X+MAPPOINT_STATS_W-i
                        ,(MAPPOINT_STATS_Y+MAPPOINT_STATS_H/4)
                        -diffgraph_power[i],DIFFGRAPH_POWER_COLOUR);

	}
        // centre lines
        Fgl_hline(MAPPOINT_STATS_X
                ,MAPPOINT_STATS_Y+MAPPOINT_STATS_H/4
                ,MAPPOINT_STATS_X+MAPPOINT_STATS_W-1,white(22));
        Fgl_hline(MAPPOINT_STATS_X
                ,MAPPOINT_STATS_Y+(3*MAPPOINT_STATS_H)/4
                ,MAPPOINT_STATS_X+MAPPOINT_STATS_W-1,white(22));
        Fgl_hline(MAPPOINT_STATS_X
                ,MAPPOINT_STATS_Y+MAPPOINT_STATS_H/2
                ,MAPPOINT_STATS_X+MAPPOINT_STATS_W-1,0);
}

void clicked_market_cb(int x,int y)
{
	market_cb_flag=1;
	mcbx=x;
	mcby=y;
}

//void draw_market_cb(int x,int y)  // x & y are the market's pos.
void draw_market_cb(void)  // x & y are the market's pos.
{
	int x,y,flags;
	char s[100];
#ifdef LC_X11
	market_cb_drawn_flag=1;
#endif
	x=mcbx;
	y=mcby;
	flags=mappoint[x][y].flags;
	Fgl_getbox(MARKET_CB_X,MARKET_CB_Y,MARKET_CB_W,MARKET_CB_H
		,market_cb_gbuf);
	Fgl_fillbox(MARKET_CB_X,MARKET_CB_Y,MARKET_CB_W,MARKET_CB_H,28);
#ifdef USE_EXPANDED_FONT
	gl_setwritemode(WRITEMODE_MASKED | FONT_EXPANDED);
#endif
	Fgl_write(MARKET_CB_X+6*8,MARKET_CB_Y+1,"Market");
	Fgl_write(MARKET_CB_X+7*8,MARKET_CB_Y+4*8,"FOOD");
	Fgl_write(MARKET_CB_X+7*8,MARKET_CB_Y+7*8,"JOBS");
	Fgl_write(MARKET_CB_X+7*8,MARKET_CB_Y+10*8,"COAL");
	Fgl_write(MARKET_CB_X+7*8+4,MARKET_CB_Y+13*8,"ORE");
	Fgl_write(MARKET_CB_X+6*8+4,MARKET_CB_Y+16*8,"GOODS");
	Fgl_write(MARKET_CB_X+6*8+4,MARKET_CB_Y+19*8,"STEEL");
	Fgl_write(MARKET_CB_X+12,MARKET_CB_Y+2*8,"Buy");
	Fgl_write(MARKET_CB_X+14*8,MARKET_CB_Y+2*8,"Sell");
	sprintf(s,"%3d",x);
	s[3]=',';
	s[4]=0; 
	Fgl_write(MARKET_CB_X+5*8+4,MARKET_CB_Y+2*8,s);
	sprintf(s,"%3d",y);
	Fgl_write(MARKET_CB_X+9*8+4,MARKET_CB_Y+2*8,s);
#ifdef USE_EXPANDED_FONT
	gl_setwritemode(WRITEMODE_OVERWRITE | FONT_EXPANDED);
#endif
	if ((flags & FLAG_MB_FOOD)!=0)
		Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+3*8+4,16,16
			,checked_box_graphic);
	else
		Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+3*8+4,16,16
			,unchecked_box_graphic);
	if ((flags & FLAG_MS_FOOD)!=0)
		Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+3*8+4,16,16
			,checked_box_graphic);
	else
		Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+3*8+4,16,16
			,unchecked_box_graphic);

	if ((flags & FLAG_MB_JOBS)!=0)
                Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+6*8+4,16,16
                        ,checked_box_graphic);
        else
                Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+6*8+4,16,16
                        ,unchecked_box_graphic);
        if ((flags & FLAG_MS_JOBS)!=0)
                Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+6*8+4,16,16
                        ,checked_box_graphic);
        else
                Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+6*8+4,16,16
                        ,unchecked_box_graphic);

        if ((flags & FLAG_MB_COAL)!=0)
                Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+9*8+4,16,16
                        ,checked_box_graphic);
        else
                Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+9*8+4,16,16
                        ,unchecked_box_graphic);
        if ((flags & FLAG_MS_COAL)!=0)
                Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+9*8+4,16,16
                        ,checked_box_graphic);
        else
                Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+9*8+4,16,16
                        ,unchecked_box_graphic);

        if ((flags & FLAG_MB_ORE)!=0)
                Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+12*8+4,16,16
                        ,checked_box_graphic);
        else
                Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+12*8+4,16,16
                        ,unchecked_box_graphic);
        if ((flags & FLAG_MS_ORE)!=0)
                Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+12*8+4,16,16
                        ,checked_box_graphic);
        else
                Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+12*8+4,16,16
                        ,unchecked_box_graphic);

        if ((flags & FLAG_MB_GOODS)!=0)
                Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+15*8+4,16,16
                        ,checked_box_graphic);
        else
                Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+15*8+4,16,16
                        ,unchecked_box_graphic);
        if ((flags & FLAG_MS_GOODS)!=0)
                Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+15*8+4,16,16
                        ,checked_box_graphic);
        else
                Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+15*8+4,16,16
                        ,unchecked_box_graphic);

        if ((flags & FLAG_MB_STEEL)!=0)
                Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+18*8+4,16,16
                        ,checked_box_graphic);
        else
                Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+18*8+4,16,16
                        ,unchecked_box_graphic);
        if ((flags & FLAG_MS_STEEL)!=0)
                Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+18*8+4,16,16
                        ,checked_box_graphic);
        else
                Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+18*8+4,16,16
                        ,unchecked_box_graphic);
}

void close_market_cb(void)
{
	market_cb_flag=0;
#ifdef LC_X11
	market_cb_drawn_flag=0;
#endif
	Fgl_putbox(MARKET_CB_X,MARKET_CB_Y,MARKET_CB_W,MARKET_CB_H
                ,market_cb_gbuf);
// when exiting market cb, stop the mouse repeating straight away
        cs_mouse_button=MOUSE_LEFTBUTTON;
        cs_mouse_button_repeat=real_time+1000;

}


void clicked_port_cb(int x,int y)
{
	port_cb_flag=1;
	mcbx=x;
	mcby=y;
}

//void draw_port_cb(int x,int y)  // x & y are the market's pos.
void draw_port_cb(void)  // x & y are the market's pos.
{
	int x,y,flags;
	char s[100];
#ifdef LC_X11
	port_cb_drawn_flag=1;
#endif
	x=mcbx;
	y=mcby;

//	mini_screen_port_x=x;
//	mini_screen_port_y=y;
//	mini_screen_flags=MINI_SCREEN_PORT_FLAG;

	flags=mappoint[x][y].flags;
// use the market cb resources where possible.
	Fgl_getbox(MARKET_CB_X,MARKET_CB_Y,MARKET_CB_W,MARKET_CB_H
		,market_cb_gbuf);
	Fgl_fillbox(MARKET_CB_X,MARKET_CB_Y,MARKET_CB_W,MARKET_CB_H,28);
#ifdef USE_EXPANDED_FONT
	gl_setwritemode(WRITEMODE_MASKED | FONT_EXPANDED);
#endif
	Fgl_write(MARKET_CB_X+7*8,MARKET_CB_Y+1,"Port");
	Fgl_write(MARKET_CB_X+7*8,MARKET_CB_Y+7*8,"FOOD");
	Fgl_write(MARKET_CB_X+7*8,MARKET_CB_Y+10*8,"COAL");
	Fgl_write(MARKET_CB_X+7*8+4,MARKET_CB_Y+13*8,"ORE");
	Fgl_write(MARKET_CB_X+6*8+4,MARKET_CB_Y+16*8,"GOODS");
	Fgl_write(MARKET_CB_X+6*8+4,MARKET_CB_Y+19*8,"STEEL");
	Fgl_write(MARKET_CB_X+12,MARKET_CB_Y+2*8,"Buy");
	Fgl_write(MARKET_CB_X+14*8,MARKET_CB_Y+2*8,"Sell");
	sprintf(s,"%3d",x);
	s[3]=',';
	s[4]=0; 
	Fgl_write(MARKET_CB_X+5*8+4,MARKET_CB_Y+2*8,s);
	sprintf(s,"%3d",y);
	Fgl_write(MARKET_CB_X+9*8+4,MARKET_CB_Y+2*8,s);
#ifdef USE_EXPANDED_FONT
	gl_setwritemode(WRITEMODE_OVERWRITE | FONT_EXPANDED);
#endif

        if ((flags & FLAG_MB_FOOD)!=0)
                Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+6*8+4,16,16
                        ,checked_box_graphic);
        else
                Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+6*8+4,16,16
                        ,unchecked_box_graphic);
        if ((flags & FLAG_MS_FOOD)!=0)
                Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+6*8+4,16,16
                        ,checked_box_graphic);
        else
                Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+6*8+4,16,16
                        ,unchecked_box_graphic);


        if ((flags & FLAG_MB_COAL)!=0)
                Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+9*8+4,16,16
                        ,checked_box_graphic);
        else
                Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+9*8+4,16,16
                        ,unchecked_box_graphic);
        if ((flags & FLAG_MS_COAL)!=0)
                Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+9*8+4,16,16
                        ,checked_box_graphic);
        else
                Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+9*8+4,16,16
                        ,unchecked_box_graphic);

        if ((flags & FLAG_MB_ORE)!=0)
                Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+12*8+4,16,16
                        ,checked_box_graphic);
        else
                Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+12*8+4,16,16
                        ,unchecked_box_graphic);
        if ((flags & FLAG_MS_ORE)!=0)
                Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+12*8+4,16,16
                        ,checked_box_graphic);
        else
                Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+12*8+4,16,16
                        ,unchecked_box_graphic);

        if ((flags & FLAG_MB_GOODS)!=0)
                Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+15*8+4,16,16
                        ,checked_box_graphic);
        else
                Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+15*8+4,16,16
                        ,unchecked_box_graphic);
        if ((flags & FLAG_MS_GOODS)!=0)
                Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+15*8+4,16,16
                        ,checked_box_graphic);
        else
                Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+15*8+4,16,16
                        ,unchecked_box_graphic);

        if ((flags & FLAG_MB_STEEL)!=0)
                Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+18*8+4,16,16
                        ,checked_box_graphic);
        else
                Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+18*8+4,16,16
                        ,unchecked_box_graphic);
        if ((flags & FLAG_MS_STEEL)!=0)
                Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+18*8+4,16,16
                        ,checked_box_graphic);
        else
                Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+18*8+4,16,16
                        ,unchecked_box_graphic);
}

void close_port_cb(void)
{
	port_cb_flag=0;
#ifdef LC_X11
	port_cb_drawn_flag=0;
#endif
	Fgl_putbox(MARKET_CB_X,MARKET_CB_Y,MARKET_CB_W,MARKET_CB_H
                ,market_cb_gbuf);
// when exiting port cb, stop the mouse repeating straight away
        cs_mouse_button=MOUSE_LEFTBUTTON;
        cs_mouse_button_repeat=real_time+1000;

}


int yn_dial_box(char *title,char *s1,char *s2,char *s3)
{
	int x,y,h,i;
	unsigned int w=0;
	cs_mouse_button_delay=0;
// find len of longest string
	if (strlen(s1)>w)
		w=strlen(s1);
	if (strlen(s2)>w)
		w=strlen(s2);
	if (strlen(s3)>w)
		w=strlen(s3);
	w+=4;  // add a few spaces to the sides
	if (w<20)  // min width
		w=20;
	w*=8;  // convert chars to pixels
	x=(640/2)-(w/2);
	y=(480/2)-(8*9/2);
	h=9*8;
	char *ss = new char[(w+16)*(h+16)];
	hide_mouse();
	Fgl_getbox(x-8,y-8,w+16,h+16,ss);
	Fgl_fillbox(x,y,w,h,YN_DIALBOX_BG_COLOUR);
	for (i=1;i<8;i++)
	{
		Fgl_hline(x-i,y-i,x+w+i-1,YN_DIALBOX_BG_COLOUR+i+i);
		Fgl_hline(x-i,y+h+i-1,x+w+i-1,YN_DIALBOX_BG_COLOUR+i+i);
		Fgl_line(x-i,y-i,x-i,y+h+i-1,YN_DIALBOX_BG_COLOUR+i+i);
		Fgl_line(x+w+i-1,y-i,x+w+i-1,y+h+i-1,YN_DIALBOX_BG_COLOUR+i+i);
	}
	db_yesbox_x1=x+w/4-(2*8);
	db_yesbox_x2=db_yesbox_x1+4*8;
	db_yesbox_y1=(y+h-14-1);
	db_yesbox_y2=db_yesbox_y1+10+2;
	db_nobox_x1=x+((3*w)/4)-(2*8);
	db_nobox_x2=db_nobox_x1+4*8;
	db_nobox_y1=(y+h-14-1);
	db_nobox_y2=db_nobox_y1+10+2;
	Fgl_fillbox(db_yesbox_x1,db_yesbox_y1,4*8,10+2,white(0));
	Fgl_fillbox(db_nobox_x1,db_nobox_y1,4*8,10+2,white(0));
	Fgl_write(db_yesbox_x1+4,db_yesbox_y1+2,"YES");
	Fgl_write(db_nobox_x1+8,db_nobox_y1+2,"NO");
#ifdef USE_EXPANDED_FONT
	gl_setwritemode(WRITEMODE_MASKED | FONT_EXPANDED);
#else
	Fgl_setfontcolors(YN_DIALBOX_BG_COLOUR,TEXT_FG_COLOUR);
#endif
	Fgl_write((x+w/2)-(strlen(title)*4),y+4,title);
	Fgl_write((x+w/2)-(strlen(s1)*4),y+20,s1);
	Fgl_write((x+w/2)-(strlen(s2)*4),y+30,s2);
	Fgl_write((x+w/2)-(strlen(s3)*4),y+40,s3);
#ifdef USE_EXPANDED_FONT
	gl_setwritemode(WRITEMODE_OVERWRITE | FONT_EXPANDED);
#else
	Fgl_setfontcolors(TEXT_BG_COLOUR,TEXT_FG_COLOUR);
#endif
	redraw_mouse();
	db_flag=1;
// shake the mouse a bit to make sure we have the correct cursor.
	cs_mouse_handler(0,-1,0);
	cs_mouse_handler(0,1,0);
	db_yesbox_clicked=0;
	db_nobox_clicked=0;
	db_yesbox_clicked=0;
	db_nobox_clicked=0;
	do
	{
                lc_usleep(1000); 

#ifdef LC_X11
		call_event();
		i=x_key_value;
		x_key_value=0;
#else
		mouse_update();
		i=vga_getkey();
#endif
		if (i==10 || i==13 || i==' ' || i=='y' || i=='Y')
			db_yesbox_clicked=1;
		else if (i=='n' || i==127 || i=='N')
			db_nobox_clicked=1;
	} while (db_yesbox_clicked==0 && db_nobox_clicked==0);
	db_flag=0;
	hide_mouse();
	Fgl_putbox(x-8,y-8,w+16,h+16,ss);
	redraw_mouse();
// shake the mouse a bit to make sure we have the correct cursor.
        cs_mouse_handler(0,-1,0);
        cs_mouse_handler(0,1,0);

	delete /* [(w+16)*(h+16)] */ ss;

// this flag is there to reset the mouse button, mouse_update() is not
// 'very' re-entrant, so I've got to clean it up after we get out of here.
// Yet another hack that I don't like! I need to get these dial boxes
// in the main loop!
	reset_mouse_flag=1;
	cs_mouse_button_repeat=real_time+1000;
	if (db_yesbox_clicked!=0)
		return(1);
	return(0);
}

void ok_dial_box(char *fn,int good_bad,char *xs)
{
	char s[100];
	int i,l,x,y,h,w,colour;
	FILE *inf;
	cs_mouse_button_delay=0;
// select which colour to draw the box in.
	if (suppress_ok_buttons!=0)
		return;
	if (good_bad==GOOD || good_bad==RESULTS)
		colour=green(14);
	else if (good_bad==BAD)
		colour=red(12);
	else
		colour=white(12);
	if (good_bad==RESULTS)
		strcpy(s,fn);
	else
	{
		strcpy(s,message_path);
		strcat(s,fn);
	}
	if ((inf=fopen(s,"r"))==NULL)
	{
		printf("Can't open message <%s> for OK dialog box\n",s);


		strcpy(s,message_path);
		strcat(s,"error.mes");
		if ((inf=fopen(s,"r"))==NULL)
		{
			fprintf(stderr
			,"Can't open default message <%s> either\n",s); 
			fprintf(stderr," ...it was not displayed");
			return;
		}
	}
// static 74*22 char array for the message info array
	l=0;
	while (feof(inf)==0 && l<20)
	{
		if (fgets(okmessage[l],70,inf)==0)
			break;
		l++;
	}
	fclose(inf);
	if (xs!=0)
	{
		strncpy(okmessage[l],xs,70);
		l++;
	}
// 'l' is now the number of lines. Work out the height of the box.
	h=(l+3)*10;  // half a line above and below the title, 2 lines
		    // for the ok button. 10 pixels per line
	w=0;
// Get rid of new line and work out the width of the logest line.
	for (i=0;i<l;i++)
	{
// get rid of the newline
	        if (okmessage[i][strlen(okmessage[i])-1]==0xa)
        	        okmessage[i][strlen(okmessage[i])-1]=0;
		if (strlen(okmessage[i])>(unsigned int)w)
			w=strlen(okmessage[i]);
	}
	w=(w+2)*8; // leave a space at either side.
// now we can work out the x and y points.
	x=(640/2)-(w/2);
	y=(480/2)-(h/2);
	char *ss = new char[(w+16)*(h+16)];
	hide_mouse();
	Fgl_getbox(x-8,y-8,w+16,h+16,ss);
        Fgl_fillbox(x,y,w,h,colour);
        for (i=1;i<8;i++)
        {
                Fgl_hline(x-i,y-i,x+w+i-1,colour+i+i);
                Fgl_hline(x-i,y+h+i-1,x+w+i-1,colour+i+i);
                Fgl_line(x-i,y-i,x-i,y+h+i-1,colour+i+i);
                Fgl_line(x+w+i-1,y-i,x+w+i-1,y+h+i-1,colour+i+i);
        }
	db_okbox_x1=x+w/2-(2*8);
	db_okbox_x2=db_okbox_x1+4*8;
	db_okbox_y1=y+h-15;
	db_okbox_y2=db_okbox_y1+12;
	Fgl_fillbox(db_okbox_x1,db_okbox_y1,4*8,10+2,white(0));
	Fgl_write(db_okbox_x1+8,db_okbox_y1+2,"OK");
#ifdef USE_EXPANDED_FONT
	gl_setwritemode(WRITEMODE_MASKED | FONT_EXPANDED);
#else
	Fgl_setfontcolors(colour,TEXT_FG_COLOUR);
#endif
	Fgl_write((x+w/2)-(strlen(okmessage[0])*4),y+4,okmessage[0]);
	for (i=1;i<l;i++)
		Fgl_write((x+w/2)-(strlen(okmessage[i])*4)
			,y+10+i*10,okmessage[i]);
#ifdef USE_EXPANDED_FONT
	gl_setwritemode(WRITEMODE_OVERWRITE | FONT_EXPANDED);
#else
	Fgl_setfontcolors(TEXT_BG_COLOUR,TEXT_FG_COLOUR);
#endif
	redraw_mouse();
	db_okflag=1;
// shake the mouse a bit to make sure we have the correct cursor.
        cs_mouse_handler(0,-1,0);
        cs_mouse_handler(0,1,0);

//	mouse_waitforupdate(); // dummy one to catch still pressed button
#ifdef LC_X11
        call_event();
#else
	mouse_update();
#endif
	db_okbox_clicked=0;
	cs_mouse_button=MOUSE_LEFTBUTTON;
//	mouse_waitforupdate();
#ifdef LC_X11
        call_event();
#else
	mouse_update();
#endif
	db_okbox_clicked=0;
	do
	{
                lc_usleep(1000);

#ifdef LC_X11
	        call_wait_event();
	        i=x_key_value;
		x_key_value=0;
#else
		mouse_update();
		i=vga_getkey();
#endif
		if (i==10 || i==13 || i==' ')
			db_okbox_clicked=1;
	} while (db_okbox_clicked==0);
	db_okflag=0;
	hide_mouse();
	Fgl_putbox(x-8,y-8,w+16,h+16,ss);
	redraw_mouse();
// shake the mouse a bit to make sure we have the correct cursor.
        cs_mouse_handler(0,-1,0);
        cs_mouse_handler(0,1,0);

	delete /* [(w+16)*(h+16)] */ ss;
// when exiting dial box, stop the mouse repeating straight away
	reset_mouse_flag=1;
        cs_mouse_button_repeat=real_time+1000;
}

int inv_sbut(int button)
{
        int i,j;
        for (i=0;i<32;i++)    // just a test recode later
                if (sbut[i]==button)
                {
                        j=i;
                        return(j);
                }
	printf("Button=%d\n",button);
	for (i=0;i<32;i++)
		printf("%5d",sbut[i]);
	printf("\n");
        do_error("An inv_sbut error has happened. This is impossible!!");
	return(-1);  // can't get here
}


void call_select_change_up(int button)
{
	button=inv_sbut(button);
	if (button==GROUP_WINDMILL)
		ok_dial_box("windmillup.mes",GOOD,0L);
	else if (button==GROUP_POWER_SOURCE_COAL)
		ok_dial_box("coalpowerup.mes",GOOD,0L);
        else if (button==(GROUP_POWER_SOURCE-1))  // -1 a hack to make it
					// work. Really dirty :(
// Caused by the fact that groups and buttons are different until after the
// bulldoze button, then they are the same.
                ok_dial_box("solarpowerup.mes",GOOD,0L);
        else if (button==GROUP_COALMINE)
                ok_dial_box("coalmineup.mes",GOOD,0L);
        else if (button==GROUP_RAIL)
                ok_dial_box("railwayup.mes",GOOD,0L);
        else if (button==GROUP_ROAD)
                ok_dial_box("roadup.mes",GOOD,0L);
        else if (button==GROUP_INDUSTRY_L)
                ok_dial_box("ltindustryup.mes",GOOD,0L);
        else if (button==GROUP_UNIVERSITY)
                ok_dial_box("universityup.mes",GOOD,0L);
        else if (button==GROUP_OREMINE)
	{
		if (GROUP_OREMINE_TECH>0)
                	ok_dial_box("oremineup.mes",GOOD,0L);
	}
        else if (button==GROUP_EX_PORT) // exports are the same
                ok_dial_box("import-exportup.mes",GOOD,0L);
        else if (button==GROUP_INDUSTRY_H)
                ok_dial_box("hvindustryup.mes",GOOD,0L);
        else if (button==GROUP_PARKLAND)
	{
			if (GROUP_PARKLAND_TECH>0)
                	ok_dial_box("parkup.mes",GOOD,0L);
	}
        else if (button==GROUP_RECYCLE)
                ok_dial_box("recycleup.mes",GOOD,0L);
        else if (button==GROUP_RIVER)
	{
		if (GROUP_WATER_TECH>0)
                	ok_dial_box("riverup.mes",GOOD,0L);
	}
        else if (button==GROUP_HEALTH)
                ok_dial_box("healthup.mes",GOOD,0L);
        else if (button==GROUP_ROCKET)
                ok_dial_box("rocketup.mes",GOOD,0L);
	else if (button==GROUP_SCHOOL)
	{
		if (GROUP_SCHOOL_TECH>0)
			ok_dial_box("schoolup.mes",GOOD,0L);
	}
	else if (button==GROUP_BLACKSMITH)
	{
		if (GROUP_BLACKSMITH_TECH>0)
			ok_dial_box("blacksmithup.mes",GOOD,0L);
	}
	else if (button==GROUP_MILL)
	{
		if (GROUP_MILL_TECH>0)
			ok_dial_box("millup.mes",GOOD,0L);
	}
	else if (button==GROUP_POTTERY)
	{
		if (GROUP_POTTERY_TECH>0)
			ok_dial_box("potteryup.mes",GOOD,0L);
	}
	else if (button==GROUP_FIRESTATION)
		ok_dial_box("firestationup.mes",GOOD,0L);
	else if (button==GROUP_CRICKET)
		ok_dial_box("cricketup.mes",GOOD,0L);
}

// add spaces to the begining of the string.
void beg_space_pad(char *s,int l)
{
	char ss[100];
	int x,y;
	if ((x=strlen(s))>=l)
		return;
	x-=l;
	ss[0]=0;
	for (y=0;y<x;y++)
		strcat(ss," ");
	strcat(ss,s);
	strcpy(s,ss);
}

void draw_up_pbar(int x,int y,int val,int oldval)
		// offset is sqrt(val) - max = (PBAR_SIZE_Y/2)-8
{
	int offset,oldoffset;
	offset=(int)sqrt(val);
	if (offset>(PBAR_SIZE_Y/2)-8)
		offset=(PBAR_SIZE_Y/2)-8;
	oldoffset=(int)sqrt(oldval);
	if (oldoffset>(PBAR_SIZE_Y/2)-8)
		oldoffset=(PBAR_SIZE_Y/2)-8;
	if (offset==oldoffset)
		return;
	Fgl_enableclipping();
	Fgl_setclippingwindow(x+2,y+2,x+2+PBAR_SIZE_X
		,y+2+(PBAR_SIZE_Y/2)-8);
	if (offset<oldoffset) // make sure no garbage left above new point
		Fgl_fillbox(x+2,y+2,PBAR_SIZE_X
			,(PBAR_SIZE_Y/2)-6-offset,0);
	Fgl_putbox(x+2,y+2+(PBAR_SIZE_Y/2)-9-offset,16,16,up_pbar1_graphic);
	if (offset>15)
		Fgl_putbox(x+2,y+2+(PBAR_SIZE_Y/2)+7-offset
			,16,16,up_pbar2_graphic);
	Fgl_disableclipping();
}

void draw_down_pbar(int x,int y,int val,int oldval)
                // offset is sqrt(val) - max = (PBAR_SIZE_Y/2)-8
{
        int offset,oldoffset;
        offset=(int)sqrt(val);
        if (offset>(PBAR_SIZE_Y/2)-8)
                offset=(PBAR_SIZE_Y/2)-8;
        oldoffset=(int)sqrt(oldval);
        if (oldoffset>(PBAR_SIZE_Y/2)-8)
                oldoffset=(PBAR_SIZE_Y/2)-8;
        if (offset==oldoffset)
                return;
        Fgl_enableclipping();
        Fgl_setclippingwindow(x+2,y+2+(PBAR_SIZE_Y/2)+8
		,x+2+PBAR_SIZE_X,y+2+PBAR_SIZE_Y-2);
        if (offset<oldoffset) // make sure no garbage left above new point
                Fgl_fillbox(x+2,y+2+(PBAR_SIZE_Y/2)+7+offset
			,PBAR_SIZE_X,PBAR_SIZE_Y-(PBAR_SIZE_Y/2)-7,0);
        Fgl_putbox(x+2,y+2+(PBAR_SIZE_Y/2)-8+offset
		,16,16,down_pbar1_graphic);
        if (offset>15)
                Fgl_putbox(x+2,y+2+(PBAR_SIZE_Y/2)-24+offset
			,16,16,down_pbar2_graphic);
	Fgl_disableclipping();
}

void do_pbar_population(int pop)
{
	static int flag=0;
	int i,tot=0,diff=0;
	if (flag==0)
	{
		flag=1;
		for (i=0;i<12;i++)  //  Don't think this is needed
			pbar_pops[i]=0;  //  Do it anyway.
		for (i=0;i<2;i++)
		{
			Fgl_hline(PBAR_POP_X+i,PBAR_POP_Y+i
				,PBAR_POP_X+(PBAR_SIZE_X+3)-i,yellow(12));
			Fgl_hline(PBAR_POP_X+i,PBAR_POP_Y+(PBAR_SIZE_Y+3)-i
				,PBAR_POP_X+(PBAR_SIZE_X+3)-i,yellow(24));
			Fgl_line(PBAR_POP_X+i,PBAR_POP_Y+1+i,PBAR_POP_X+i
				,PBAR_POP_Y+(PBAR_SIZE_Y+2)-i,yellow(14));
			Fgl_line(PBAR_POP_X+(PBAR_SIZE_X+3)-i,PBAR_POP_Y+1+i
				,PBAR_POP_X+(PBAR_SIZE_X+3)-i
				,PBAR_POP_Y+(PBAR_SIZE_Y+2)-i,yellow(22));
		}
		Fgl_fillbox(PBAR_POP_X+2,PBAR_POP_Y+2
			,PBAR_SIZE_X,PBAR_SIZE_Y,0);
		Fgl_putbox(PBAR_POP_X+2,PBAR_POP_Y+(PBAR_SIZE_Y/2)-6
			,16,16
			,pop_pbar_graphic);
	}
	for (i=0;i<11;i++)               // go on, it's only 11 :)
		tot+=(pbar_pops[i]=pbar_pops[i+1]);
	tot+=(pbar_pops[11]=pop);
	diff=tot-pbar_pop_oldtot;
	if (diff>=0)
	{
		if (pbar_pop_olddiff<0)  // get rid of any -ve bar
		{
			Fgl_fillbox(PBAR_POP_X+2,PBAR_POP_Y+(PBAR_SIZE_Y/2)+8
				,PBAR_SIZE_X
				,PBAR_SIZE_Y-((PBAR_SIZE_Y/2)+8),0);
			pbar_pop_olddiff=0;
		}
		draw_up_pbar(PBAR_POP_X,PBAR_POP_Y,diff*2,pbar_pop_olddiff*2);
	}
	else
	{
		if (pbar_pop_olddiff>=0)  // get rid of any +ve bar
		{
			Fgl_fillbox(PBAR_POP_X+2,PBAR_POP_Y+2
				,PBAR_SIZE_X
				,PBAR_SIZE_Y-((PBAR_SIZE_Y/2)+7),0);
			pbar_pop_olddiff=0;
		}
		draw_down_pbar(PBAR_POP_X,PBAR_POP_Y,-diff*2
			,-pbar_pop_olddiff*2);
	}
	pbar_pop_oldtot=tot;
	pbar_pop_olddiff=diff;
}

void do_pbar_tech(int tech)
{
        static int flag=0;
        int i,tot=0,diff=0;
        if (flag==0)
        {
                flag=1;
                for (i=0;i<12;i++)  //  Don't think this is needed
                        pbar_techs[i]=0;  //  Do it anyway.
                for (i=0;i<2;i++)
                {
			Fgl_hline(PBAR_TECH_X+i,PBAR_TECH_Y+i
				,PBAR_TECH_X+(PBAR_SIZE_X+3)-i,yellow(12));
			Fgl_hline(PBAR_TECH_X+i,PBAR_TECH_Y+(PBAR_SIZE_Y+3)-i
				,PBAR_TECH_X+(PBAR_SIZE_X+3)-i,yellow(24));
                        Fgl_line(PBAR_TECH_X+i,PBAR_TECH_Y+1+i,PBAR_TECH_X+i
                                ,PBAR_TECH_Y+(PBAR_SIZE_Y+2)-i,yellow(14));
                        Fgl_line(PBAR_TECH_X+(PBAR_SIZE_X+3)-i
				,PBAR_TECH_Y+1+i
				,PBAR_TECH_X+(PBAR_SIZE_X+3)-i
				,PBAR_TECH_Y+(PBAR_SIZE_Y+2)-i,yellow(22));
		}
                Fgl_fillbox(PBAR_TECH_X+2,PBAR_TECH_Y+2,PBAR_SIZE_X
			,PBAR_SIZE_Y,0);
                Fgl_putbox(PBAR_TECH_X+2,PBAR_TECH_Y+(PBAR_SIZE_Y/2)-6
			,16,16,tech_pbar_graphic);
        }
        for (i=0;i<11;i++)               // go on, it's only 11 :)
                tot+=(pbar_techs[i]=pbar_techs[i+1]);
        tot+=(pbar_techs[11]=tech/8);
        diff=tot-pbar_tech_oldtot;
        if (diff>=0)
        {
                if (pbar_tech_olddiff<0)  // get rid of any -ve bar
                {
                        Fgl_fillbox(PBAR_TECH_X+2
				,PBAR_TECH_Y+(PBAR_SIZE_Y/2)+8
				,PBAR_SIZE_X
				,PBAR_SIZE_Y-((PBAR_SIZE_Y/2)+8),0);
                        pbar_tech_olddiff=0;
                }
                draw_up_pbar(PBAR_TECH_X,PBAR_TECH_Y
			,(diff/4)+1,(pbar_tech_olddiff/4)+1);
        }
        else
        {
                if (pbar_tech_olddiff>=0)  // get rid of any +ve bar
                {
                        Fgl_fillbox(PBAR_TECH_X+2,PBAR_TECH_Y+2
				,PBAR_SIZE_X
				,PBAR_SIZE_Y-((PBAR_SIZE_Y/2)+7),0);
                        pbar_tech_olddiff=0;
                }
                draw_down_pbar(PBAR_TECH_X,PBAR_TECH_Y
			,(-diff/2)+1,(-pbar_tech_olddiff/2)+1);
        }
        pbar_tech_oldtot=tot;
        pbar_tech_olddiff=diff;
}

void do_pbar_food(int food)
{
        static int flag=0;
        int i,tot=0,diff=0;
        if (flag==0)
        {
                flag=1;
                for (i=0;i<12;i++)  //  Don't think this is needed
                        pbar_foods[i]=0;  //  Do it anyway.
                for (i=0;i<2;i++)
                {
			Fgl_hline(PBAR_FOOD_X+i,PBAR_FOOD_Y+i
				,PBAR_FOOD_X+(PBAR_SIZE_X+3)-i,yellow(12));
			Fgl_hline(PBAR_FOOD_X+i,PBAR_FOOD_Y+(PBAR_SIZE_Y+3)-i
				,PBAR_FOOD_X+(PBAR_SIZE_X+3)-i,yellow(24));
                        Fgl_line(PBAR_FOOD_X+i,PBAR_FOOD_Y+1+i,PBAR_FOOD_X+i
                                        ,PBAR_FOOD_Y+(PBAR_SIZE_Y+2)-i
					,yellow(14));
                        Fgl_line(PBAR_FOOD_X+(PBAR_SIZE_X+3)-i
				,PBAR_FOOD_Y+1+i
				,PBAR_FOOD_X+(PBAR_SIZE_X+3)-i
				,PBAR_FOOD_Y+(PBAR_SIZE_Y+2)-i
				,yellow(22));
		}
                Fgl_fillbox(PBAR_FOOD_X+2,PBAR_FOOD_Y+2
			,PBAR_SIZE_X,PBAR_SIZE_Y,0);
                Fgl_putbox(PBAR_FOOD_X+2,PBAR_FOOD_Y+(PBAR_SIZE_Y/2)-6
			,16,16,food_pbar_graphic);
        }
        for (i=0;i<11;i++)               // go on, it's only 11 :)
                tot+=(pbar_foods[i]=pbar_foods[i+1]);
        tot+=(pbar_foods[11]=food);
        diff=tot-pbar_food_oldtot;
        if (diff>=0)
        {
                if (pbar_food_olddiff<0)  // get rid of any -ve bar
                {
			Fgl_fillbox(PBAR_FOOD_X+2,PBAR_FOOD_Y+(PBAR_SIZE_Y/2)+8
				,PBAR_SIZE_X
				,PBAR_SIZE_Y-((PBAR_SIZE_Y/2)+8),0);
                        pbar_food_olddiff=0;
                }
                draw_up_pbar(PBAR_FOOD_X,PBAR_FOOD_Y
			,(diff/2)+1,(pbar_food_olddiff/2)+1);
        }
        else
        {
                if (pbar_food_olddiff>=0)  // get rid of any +ve bar
                {
                        Fgl_fillbox(PBAR_FOOD_X+2,PBAR_FOOD_Y+2
				,PBAR_SIZE_X
				,PBAR_SIZE_Y-((PBAR_SIZE_Y/2)+7),0);
                        pbar_food_olddiff=0;
                }
                draw_down_pbar(PBAR_FOOD_X,PBAR_FOOD_Y
			,(-diff)+1,(-pbar_food_olddiff)+1);
        }
        pbar_food_oldtot=tot;
        pbar_food_olddiff=diff;
}

void do_pbar_jobs(int jobs)
{
        static int flag=0;
        int i,tot=0,diff=0;
        if (flag==0)
        {
                flag=1;
                for (i=0;i<12;i++)  //  Don't think this is needed
                        pbar_jobs[i]=0;  //  Do it anyway.
                for (i=0;i<2;i++)
                {
			Fgl_hline(PBAR_JOBS_X+i,PBAR_JOBS_Y+i
				,PBAR_JOBS_X+(PBAR_SIZE_X+3)-i,yellow(12));
			Fgl_hline(PBAR_JOBS_X+i,PBAR_JOBS_Y+(PBAR_SIZE_Y+3)-i
				,PBAR_JOBS_X+(PBAR_SIZE_X+3)-i,yellow(24));
                        Fgl_line(PBAR_JOBS_X+i,PBAR_JOBS_Y+1+i,PBAR_JOBS_X+i
                                        ,PBAR_JOBS_Y+(PBAR_SIZE_Y+2)-i
					,yellow(14));
                        Fgl_line(PBAR_JOBS_X+(PBAR_SIZE_X+3)-i
				,PBAR_JOBS_Y+1+i
				,PBAR_JOBS_X+(PBAR_SIZE_X+3)-i
				,PBAR_JOBS_Y+(PBAR_SIZE_Y+2)-i
				,yellow(22));
		}
                Fgl_fillbox(PBAR_JOBS_X+2,PBAR_JOBS_Y+2
			,PBAR_SIZE_X,PBAR_SIZE_Y,0);
                Fgl_putbox(PBAR_JOBS_X+2,PBAR_JOBS_Y+(PBAR_SIZE_Y/2)-6
			,16,16,jobs_pbar_graphic);
        }
        for (i=0;i<11;i++)               // go on, it's only 11 :)
                tot+=(pbar_jobs[i]=pbar_jobs[i+1]);
        tot+=(pbar_jobs[11]=jobs);
        diff=tot-pbar_jobs_oldtot;
        if (diff>=0)
        {
                if (pbar_jobs_olddiff<0)  // get rid of any -ve bar
                {
			Fgl_fillbox(PBAR_JOBS_X+2,PBAR_JOBS_Y+(PBAR_SIZE_Y/2)+8
				,PBAR_SIZE_X
				,PBAR_SIZE_Y-((PBAR_SIZE_Y/2)+8),0);
                        pbar_jobs_olddiff=0;
                }
                draw_up_pbar(PBAR_JOBS_X,PBAR_JOBS_Y
			,(diff/2)+1,(pbar_jobs_olddiff/2)+1);
        }
        else
        {
                if (pbar_jobs_olddiff>=0)  // get rid of any +ve bar
                {
                        Fgl_fillbox(PBAR_JOBS_X+2,PBAR_JOBS_Y+2
				,PBAR_SIZE_X
				,PBAR_SIZE_Y-((PBAR_SIZE_Y/2)+7),0);
                        pbar_jobs_olddiff=0;
                }
                draw_down_pbar(PBAR_JOBS_X,PBAR_JOBS_Y
			,(-diff)+1,(-pbar_jobs_olddiff)+1);
        }
        pbar_jobs_oldtot=tot;
        pbar_jobs_olddiff=diff;
}

void do_pbar_coal(int coal)
{
        static int flag=0;
        int i,tot=0,diff=0;
        if (flag==0)
        {
                flag=1;
                for (i=0;i<12;i++)  //  Don't think this is needed
                        pbar_coal[i]=0;  //  Do it anyway.
                for (i=0;i<2;i++)
                {
			Fgl_hline(PBAR_COAL_X+i,PBAR_COAL_Y+i
				,PBAR_COAL_X+(PBAR_SIZE_X+3)-i,yellow(12));
			Fgl_hline(PBAR_COAL_X+i,PBAR_COAL_Y+(PBAR_SIZE_Y+3)-i
				,PBAR_COAL_X+(PBAR_SIZE_X+3)-i,yellow(24));
                        Fgl_line(PBAR_COAL_X+i,PBAR_COAL_Y+1+i,PBAR_COAL_X+i
                                        ,PBAR_COAL_Y+(PBAR_SIZE_Y+2)-i
					,yellow(14));
                        Fgl_line(PBAR_COAL_X+(PBAR_SIZE_X+3)-i
				,PBAR_COAL_Y+1+i
				,PBAR_COAL_X+(PBAR_SIZE_X+3)-i
				,PBAR_COAL_Y+(PBAR_SIZE_Y+2)-i
				,yellow(22));
		}
                Fgl_fillbox(PBAR_COAL_X+2,PBAR_COAL_Y+2
			,PBAR_SIZE_X,PBAR_SIZE_Y,0);
                Fgl_putbox(PBAR_COAL_X+2,PBAR_COAL_Y+(PBAR_SIZE_Y/2)-6
			,16,16,coal_pbar_graphic);
        }
        for (i=0;i<11;i++)               // go on, it's only 11 :)
                tot+=(pbar_coal[i]=pbar_coal[i+1]);
        tot+=(pbar_coal[11]=coal);
        diff=tot-pbar_coal_oldtot;
        if (diff>=0)
        {
                if (pbar_coal_olddiff<0)  // get rid of any -ve bar
                {
			Fgl_fillbox(PBAR_COAL_X+2,PBAR_COAL_Y+(PBAR_SIZE_Y/2)+8
				,PBAR_SIZE_X
				,PBAR_SIZE_Y-((PBAR_SIZE_Y/2)+8),0);
                        pbar_coal_olddiff=0;
                }
                draw_up_pbar(PBAR_COAL_X,PBAR_COAL_Y
			,(diff/2)+1,(pbar_coal_olddiff/2)+1);
        }
        else
        {
                if (pbar_coal_olddiff>=0)  // get rid of any +ve bar
                {
                        Fgl_fillbox(PBAR_COAL_X+2,PBAR_COAL_Y+2
				,PBAR_SIZE_X
				,PBAR_SIZE_Y-((PBAR_SIZE_Y/2)+7),0);
                        pbar_coal_olddiff=0;
                }
                draw_down_pbar(PBAR_COAL_X,PBAR_COAL_Y
			,(-diff)+1,(-pbar_coal_olddiff)+1);
        }
        pbar_coal_oldtot=tot;
        pbar_coal_olddiff=diff;
}

void do_pbar_goods(int goods)
{
        static int flag=0;
        int i,tot=0,diff=0;
        if (flag==0)
        {
                flag=1;
                for (i=0;i<12;i++)  //  Don't think this is needed
                        pbar_goods[i]=0;  //  Do it anyway.
                for (i=0;i<2;i++)
                {
			Fgl_hline(PBAR_GOODS_X+i,PBAR_GOODS_Y+i
				,PBAR_GOODS_X+(PBAR_SIZE_X+3)-i,yellow(12));
			Fgl_hline(PBAR_GOODS_X+i
				,PBAR_GOODS_Y+(PBAR_SIZE_Y+3)-i
				,PBAR_GOODS_X+(PBAR_SIZE_X+3)-i,yellow(24));
                        Fgl_line(PBAR_GOODS_X+i
				,PBAR_GOODS_Y+1+i,PBAR_GOODS_X+i
                                ,PBAR_GOODS_Y+(PBAR_SIZE_Y+2)-i
				,yellow(14));
                        Fgl_line(PBAR_GOODS_X+(PBAR_SIZE_X+3)-i
				,PBAR_GOODS_Y+1+i
				,PBAR_GOODS_X+(PBAR_SIZE_X+3)-i
				,PBAR_GOODS_Y+(PBAR_SIZE_Y+2)-i
				,yellow(22));
		}
                Fgl_fillbox(PBAR_GOODS_X+2,PBAR_GOODS_Y+2
			,PBAR_SIZE_X,PBAR_SIZE_Y,0);
                Fgl_putbox(PBAR_GOODS_X+2,PBAR_GOODS_Y+(PBAR_SIZE_Y/2)-6
			,16,16,goods_pbar_graphic);
        }
        for (i=0;i<11;i++)               // go on, it's only 11 :)
                tot+=(pbar_goods[i]=pbar_goods[i+1]);
        tot+=(pbar_goods[11]=goods);
        diff=tot-pbar_goods_oldtot;
        if (diff>=0)
        {
                if (pbar_goods_olddiff<0)  // get rid of any -ve bar
                {
			Fgl_fillbox(PBAR_GOODS_X+2
				,PBAR_GOODS_Y+(PBAR_SIZE_Y/2)+8
				,PBAR_SIZE_X
				,PBAR_SIZE_Y-((PBAR_SIZE_Y/2)+8),0);
                        pbar_goods_olddiff=0;
                }
                draw_up_pbar(PBAR_GOODS_X,PBAR_GOODS_Y
			,(diff/2)+1,(pbar_goods_olddiff/2)+1);
        }
        else
        {
                if (pbar_goods_olddiff>=0)  // get rid of any +ve bar
                {
                        Fgl_fillbox(PBAR_GOODS_X+2,PBAR_GOODS_Y+2
				,PBAR_SIZE_X
				,PBAR_SIZE_Y-((PBAR_SIZE_Y/2)+7),0);
                        pbar_goods_olddiff=0;
                }
                draw_down_pbar(PBAR_GOODS_X,PBAR_GOODS_Y
			,(-diff)+1,(-pbar_goods_olddiff)+1);
        }
        pbar_goods_oldtot=tot;
        pbar_goods_olddiff=diff;
}

void do_pbar_ore(int ore)
{
        static int flag=0;
        int i,tot=0,diff=0;
        if (flag==0)
        {
                flag=1;
                for (i=0;i<12;i++)  //  Don't think this is needed
                        pbar_ore[i]=0;  //  Do it anyway.
                for (i=0;i<2;i++)
                {
			Fgl_hline(PBAR_ORE_X+i,PBAR_ORE_Y+i
				,PBAR_ORE_X+(PBAR_SIZE_X+3)-i,yellow(12));
			Fgl_hline(PBAR_ORE_X+i
				,PBAR_ORE_Y+(PBAR_SIZE_Y+3)-i
				,PBAR_ORE_X+(PBAR_SIZE_X+3)-i,yellow(24));
                        Fgl_line(PBAR_ORE_X+i
				,PBAR_ORE_Y+1+i,PBAR_ORE_X+i
                                ,PBAR_ORE_Y+(PBAR_SIZE_Y+2)-i
				,yellow(14));
                        Fgl_line(PBAR_ORE_X+(PBAR_SIZE_X+3)-i
				,PBAR_ORE_Y+1+i
				,PBAR_ORE_X+(PBAR_SIZE_X+3)-i
				,PBAR_ORE_Y+(PBAR_SIZE_Y+2)-i
				,yellow(22));
		}
                Fgl_fillbox(PBAR_ORE_X+2,PBAR_ORE_Y+2
			,PBAR_SIZE_X,PBAR_SIZE_Y,0);
                Fgl_putbox(PBAR_ORE_X+2,PBAR_ORE_Y+(PBAR_SIZE_Y/2)-6
			,16,16,ore_pbar_graphic);
        }
        for (i=0;i<11;i++)               // go on, it's only 11 :)
                tot+=(pbar_ore[i]=pbar_ore[i+1]);
        tot+=(pbar_ore[11]=ore);
        diff=tot-pbar_ore_oldtot;
        if (diff>=0)
        {
                if (pbar_ore_olddiff<0)  // get rid of any -ve bar
                {
			Fgl_fillbox(PBAR_ORE_X+2
				,PBAR_ORE_Y+(PBAR_SIZE_Y/2)+8
				,PBAR_SIZE_X
				,PBAR_SIZE_Y-((PBAR_SIZE_Y/2)+8),0);
                        pbar_ore_olddiff=0;
                }
                draw_up_pbar(PBAR_ORE_X,PBAR_ORE_Y
			,(diff/2)+1,(pbar_ore_olddiff/2)+1);
        }
        else
        {
                if (pbar_ore_olddiff>=0)  // get rid of any +ve bar
                {
                        Fgl_fillbox(PBAR_ORE_X+2,PBAR_ORE_Y+2
				,PBAR_SIZE_X
				,PBAR_SIZE_Y-((PBAR_SIZE_Y/2)+7),0);
                        pbar_ore_olddiff=0;
                }
                draw_down_pbar(PBAR_ORE_X,PBAR_ORE_Y
			,(-diff)+1,(-pbar_ore_olddiff)+1);
        }
        pbar_ore_oldtot=tot;
        pbar_ore_olddiff=diff;
}

void do_pbar_steel(int steel)
{
        static int flag=0;
        int i,tot=0,diff=0;
        if (flag==0)
        {
                flag=1;
                for (i=0;i<12;i++)  //  Don't think this is needed
                        pbar_steel[i]=0;  //  Do it anyway.
                for (i=0;i<2;i++)
                {
			Fgl_hline(PBAR_STEEL_X+i,PBAR_STEEL_Y+i
				,PBAR_STEEL_X+(PBAR_SIZE_X+3)-i,yellow(12));
			Fgl_hline(PBAR_STEEL_X+i
				,PBAR_STEEL_Y+(PBAR_SIZE_Y+3)-i
				,PBAR_STEEL_X+(PBAR_SIZE_X+3)-i,yellow(24));
                        Fgl_line(PBAR_STEEL_X+i
				,PBAR_STEEL_Y+1+i,PBAR_STEEL_X+i
                                ,PBAR_STEEL_Y+(PBAR_SIZE_Y+2)-i
				,yellow(14));
                        Fgl_line(PBAR_STEEL_X+(PBAR_SIZE_X+3)-i
				,PBAR_STEEL_Y+1+i
				,PBAR_STEEL_X+(PBAR_SIZE_X+3)-i
				,PBAR_STEEL_Y+(PBAR_SIZE_Y+2)-i
				,yellow(22));
		}
                Fgl_fillbox(PBAR_STEEL_X+2,PBAR_STEEL_Y+2
			,PBAR_SIZE_X,PBAR_SIZE_Y,0);
                Fgl_putbox(PBAR_STEEL_X+2,PBAR_STEEL_Y+(PBAR_SIZE_Y/2)-6
			,16,16,steel_pbar_graphic);
        }
        for (i=0;i<11;i++)               // go on, it's only 11 :)
                tot+=(pbar_steel[i]=pbar_steel[i+1]);
        tot+=(pbar_steel[11]=steel);
        diff=tot-pbar_steel_oldtot;
        if (diff>=0)
        {
                if (pbar_steel_olddiff<0)  // get rid of any -ve bar
                {
			Fgl_fillbox(PBAR_STEEL_X+2
				,PBAR_STEEL_Y+(PBAR_SIZE_Y/2)+8
				,PBAR_SIZE_X
				,PBAR_SIZE_Y-((PBAR_SIZE_Y/2)+8),0);
                        pbar_steel_olddiff=0;
                }
                draw_up_pbar(PBAR_STEEL_X,PBAR_STEEL_Y
			,(diff/2)+1,(pbar_steel_olddiff/2)+1);
        }
        else
        {
                if (pbar_steel_olddiff>=0)  // get rid of any +ve bar
                {
                        Fgl_fillbox(PBAR_STEEL_X+2,PBAR_STEEL_Y+2
				,PBAR_SIZE_X
				,PBAR_SIZE_Y-((PBAR_SIZE_Y/2)+7),0);
                        pbar_steel_olddiff=0;
                }
                draw_down_pbar(PBAR_STEEL_X,PBAR_STEEL_Y
			,(-diff)+1,(-pbar_steel_olddiff)+1);
        }
        pbar_steel_oldtot=tot;
        pbar_steel_olddiff=diff;
}


void do_pbar_money(int money)
{
        static int flag=0;
        int i,tot=0,diff=0;
        if (flag==0)
        {
                flag=1;
                for (i=0;i<12;i++)  //  Don't think this is needed
                        pbar_money[i]=0;  //  Do it anyway.
                for (i=0;i<2;i++)
                {
			Fgl_hline(PBAR_MONEY_X+i,PBAR_MONEY_Y+i
				,PBAR_MONEY_X+(PBAR_SIZE_X+3)-i,yellow(12));
			Fgl_hline(PBAR_MONEY_X+i,PBAR_MONEY_Y
				+(PBAR_SIZE_Y+3)-i
				,PBAR_MONEY_X+(PBAR_SIZE_X+3)-i,yellow(24));
                        Fgl_line(PBAR_MONEY_X+i,PBAR_MONEY_Y+1+i
				,PBAR_MONEY_X+i
                                ,PBAR_MONEY_Y+(PBAR_SIZE_Y+2)-i
				,yellow(14));
                        Fgl_line(PBAR_MONEY_X+(PBAR_SIZE_X+3)-i
				,PBAR_MONEY_Y+1+i
				,PBAR_MONEY_X+(PBAR_SIZE_X+3)-i
				,PBAR_MONEY_Y+(PBAR_SIZE_Y+2)-i
				,yellow(22));
		}
                Fgl_fillbox(PBAR_MONEY_X+2,PBAR_MONEY_Y+2
			,PBAR_SIZE_X,PBAR_SIZE_Y,0);
                Fgl_putbox(PBAR_MONEY_X+2,PBAR_MONEY_Y+(PBAR_SIZE_Y/2)-6
			,16,16,money_pbar_graphic);
        }
        for (i=0;i<11;i++)               // go on, it's only 11 :)
                tot+=(pbar_money[i]=pbar_money[i+1]);
        tot+=(pbar_money[11]=money);
        diff=tot-pbar_money_oldtot;
        if (diff>=0)
        {
                if (pbar_money_olddiff<0)  // get rid of any -ve bar
                {
			Fgl_fillbox(PBAR_MONEY_X+2,PBAR_MONEY_Y
				+(PBAR_SIZE_Y/2)+8,PBAR_SIZE_X
				,PBAR_SIZE_Y-((PBAR_SIZE_Y/2)+8),0);
                        pbar_money_olddiff=0;
                }
                draw_up_pbar(PBAR_MONEY_X,PBAR_MONEY_Y
			,(diff/2)+1,(pbar_money_olddiff/2)+1);
        }
        else
        {
                if (pbar_money_olddiff>=0)  // get rid of any +ve bar
                {
                        Fgl_fillbox(PBAR_MONEY_X+2,PBAR_MONEY_Y+2
				,PBAR_SIZE_X
				,PBAR_SIZE_Y-((PBAR_SIZE_Y/2)+7),0);
                        pbar_money_olddiff=0;
                }
                draw_down_pbar(PBAR_MONEY_X,PBAR_MONEY_Y
			,(-diff)+1,(-pbar_money_olddiff)+1);
        }
        pbar_money_oldtot=tot;
        pbar_money_olddiff=diff;
}


void prog_box(char *title,int percent)
{
	static int flag=0,oldpercent=0;
	char s[100];
	int i;
	if (flag==0)
	{
		hide_mouse();
		Fgl_getbox(PROGBOXX-8,PROGBOXY-8,PROGBOXW+16
			,PROGBOXH+16,progbox);
		Fgl_fillbox(PROGBOXX,PROGBOXY
			,PROGBOXW,PROGBOXH,PROGBOX_BG_COLOUR);
                for (i=1;i<8;i++)
                {
                        Fgl_hline(PROGBOXX-i,PROGBOXY-i
                                ,PROGBOXX+PROGBOXW+i-1
                                ,PROGBOX_BG_COLOUR+i+i);
                        Fgl_hline(PROGBOXX-i,PROGBOXY+PROGBOXH+i-1
                                ,PROGBOXX+PROGBOXW+i-1
                                ,PROGBOX_BG_COLOUR+i+i);
                        Fgl_line(PROGBOXX-i,PROGBOXY-i
                                ,PROGBOXX-i,PROGBOXY+PROGBOXH+i-1
                                ,PROGBOX_BG_COLOUR+i+i);
                        Fgl_line(PROGBOXX+PROGBOXW+i-1,PROGBOXY-i
                                ,PROGBOXX+PROGBOXW+i-1
                                ,PROGBOXY+PROGBOXH+i-1
                                ,PROGBOX_BG_COLOUR+i+i);
                }


		Fgl_setfontcolors(PROGBOX_BG_COLOUR,TEXT_FG_COLOUR);
		Fgl_write((PROGBOXX+PROGBOXW/2)
			-(strlen(title)*4),PROGBOXY+8,title);
		Fgl_setfontcolors(TEXT_BG_COLOUR,TEXT_FG_COLOUR);

		flag=1;
	}
	if (percent!=oldpercent)
	{
		Fgl_fillbox(PROGBOXX+20,PROGBOXY+60
			,((PROGBOXW-40)*percent)/100
			,20,PROGBOX_DONE_COL);
		Fgl_fillbox(PROGBOXX+20+(((PROGBOXW-40)*percent)/100)
			,PROGBOXY+60
			,PROGBOXW-40-(((PROGBOXW-40)*percent)/100)
			,20,PROGBOX_NOTDONE_COL);
		oldpercent=percent;
		Fgl_fillbox(PROGBOXX+(PROGBOXW/2)-20
			,PROGBOXY+24,40,16,PROGBOX_BG_COLOUR);
		sprintf(s,"%3d%%",percent);
		Fgl_setfontcolors(PROGBOX_BG_COLOUR,TEXT_FG_COLOUR);
		Fgl_write(PROGBOXX+(PROGBOXW/2)-20,PROGBOXY+24,s);
		Fgl_setfontcolors(TEXT_BG_COLOUR,TEXT_FG_COLOUR);
	}
#ifdef LC_X11
	XSync(display.dpy,FALSE);
#endif
	if (percent<100)
	{
		return;
	}


	Fgl_putbox(PROGBOXX-8,PROGBOXY-8,PROGBOXW+16,PROGBOXH+16,progbox);
	redraw_mouse();
	flag=0;
}

void draw_sustainable_window(void)
{
	static int flag=0;
	int i;
	if (flag==0)
	{
		flag=1;
// draw border around the sustainable screen
	        for (i=1;i<8;i++)
	        {
	                Fgl_hline(SUST_SCREEN_X-1-i,SUST_SCREEN_Y-1-i
	                        ,SUST_SCREEN_X+SUST_SCREEN_W+1+i
	                        ,yellow(16));
	                Fgl_line(SUST_SCREEN_X-1-i,SUST_SCREEN_Y-1-i
	                        ,SUST_SCREEN_X-1-i
	                        ,SUST_SCREEN_Y+SUST_SCREEN_H+1+i
	                        ,yellow(14));
	                Fgl_hline(SUST_SCREEN_X-1-i
	                        ,SUST_SCREEN_Y+SUST_SCREEN_H+1+i
	                        ,SUST_SCREEN_X+SUST_SCREEN_W+1+i
	                        ,yellow(22));
	                Fgl_line(SUST_SCREEN_X+SUST_SCREEN_W+1+i
	                        ,SUST_SCREEN_Y-1-i
	                        ,SUST_SCREEN_X+SUST_SCREEN_W+1+i
	                        ,SUST_SCREEN_Y+SUST_SCREEN_H+1+i
	                        ,yellow(24));
	        }
		Fgl_fillbox(SUST_SCREEN_X,SUST_SCREEN_Y
			,SUST_SCREEN_W,SUST_SCREEN_H,0);
		Fgl_line(SUST_SCREEN_X+7,SUST_SCREEN_Y
			,SUST_SCREEN_X+7,SUST_SCREEN_Y+20
			,yellow(24));
// ore coal
		Fgl_hline(SUST_SCREEN_X+3,SUST_SCREEN_Y+2
			,SUST_SCREEN_X+6,SUST_ORE_COAL_COL); 
// import export
		Fgl_hline(SUST_SCREEN_X+3,SUST_SCREEN_Y+5
			,SUST_SCREEN_X+6,SUST_PORT_COL);
// money
		Fgl_hline(SUST_SCREEN_X+3,SUST_SCREEN_Y+8
			,SUST_SCREEN_X+6,SUST_MONEY_COL);
// population
		Fgl_hline(SUST_SCREEN_X+3,SUST_SCREEN_Y+11
			,SUST_SCREEN_X+6,SUST_POP_COL);
// tech
		Fgl_hline(SUST_SCREEN_X+3,SUST_SCREEN_Y+14
			,SUST_SCREEN_X+6,SUST_TECH_COL);
// fire
		Fgl_hline(SUST_SCREEN_X+3,SUST_SCREEN_Y+17
			,SUST_SCREEN_X+6,SUST_FIRE_COL);
	}
	if (sust_dig_ore_coal_count>=SUST_ORE_COAL_YEARS_NEEDED
		&& sust_port_count>=SUST_PORT_YEARS_NEEDED
		&& sust_old_money_count>=SUST_MONEY_YEARS_NEEDED
		&& sust_old_population_count>=SUST_POP_YEARS_NEEDED
		&& sust_old_tech_count>=SUST_TECH_YEARS_NEEDED
		&& sust_fire_count>=SUST_FIRE_YEARS_NEEDED)
	{
		if (sustain_flag==0)
			ok_dial_box("sustain.mes",GOOD,0L);
		sustain_flag=1;
	}
	else
		sustain_flag=0;
	draw_sustline(0,sust_dig_ore_coal_count
		,SUST_ORE_COAL_YEARS_NEEDED,SUST_ORE_COAL_COL);
	draw_sustline(3,sust_port_count
		,SUST_PORT_YEARS_NEEDED,SUST_PORT_COL);
	draw_sustline(6,sust_old_money_count
		,SUST_MONEY_YEARS_NEEDED,SUST_MONEY_COL);
	draw_sustline(9,sust_old_population_count
		,SUST_POP_YEARS_NEEDED,SUST_POP_COL);
	draw_sustline(12,sust_old_tech_count
		,SUST_TECH_YEARS_NEEDED,SUST_TECH_COL);
	draw_sustline(15,sust_fire_count
		,SUST_FIRE_YEARS_NEEDED,SUST_FIRE_COL);

}

void draw_sustline(int yoffset,int count,int max,int col)
{
	int split;
	if (count>=max)
		split=60;
	else
		split=60*count/max;
	Fgl_hline(SUST_SCREEN_X+8,SUST_SCREEN_Y+2+yoffset
		,SUST_SCREEN_X+8+split,col);
	if (split<60)
		Fgl_hline(SUST_SCREEN_X+8+split,SUST_SCREEN_Y+2+yoffset
			,SUST_SCREEN_X+8+60,0);
}

