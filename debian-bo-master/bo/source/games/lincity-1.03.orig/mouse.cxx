//  mouse.cxx   part of lin-city
// Copyright (c) I J Peters 1995,1996.  Please read the file 'COPYRIGHT'.
 
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

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

#include "lctypes.h"
#include "lin-city.h"
#include "mouse.h"


#ifndef LC_X11
unsigned char mouse_pointer[]= {
	255,255,255,255,1,1,1,1,
	255,1,1,1,255,255,1,1,
	255,1,255,255,255,1,255,1,
	255,1,255,1,1,255,1,1,
	1,255,255,1,1,1,255,1,
	1,255,1,255,1,1,1,1,
	1,1,255,1,255,1,1,1,
	1,1,1,1,1,1,1,1 };
unsigned char under_mouse_pointer[8*8];

void mouse_setup(void)
{
	int m;
        m=mouse_init("/dev/mouse",lc_mouse_type,MOUSE_DEFAULTSAMPLERATE);
        mouse_setxrange(0, 640 - 1);
        mouse_setyrange(0, 480 - 1);
        mouse_setwrap(MOUSE_NOWRAP);
	if (m!=0)
		do_error("Can't initialise mouse");
	cs_mouse_x=0;
	cs_mouse_y=0;
	cs_mouse_button=0;
	cs_mouse_xmax=640-1;
	cs_mouse_ymax=480-1;
	Fgl_getbox(mox,moy,8,8,under_mouse_pointer);
	mouse_seteventhandler(cs_mouse_handler);
}

void cs_mouse_repeat(void)
{
// called from main loop when cs_mouse_button==MOUSE_LEFTBUTTON
//	if (market_cb_flag)
//		return;
	if (cs_mouse_button_repeat<=real_time)
		cs_mouse_handler(cs_mouse_button,0,0);
// cs_mouse_button_repeat zeroed by handler
}
#endif

void cs_mouse_handler(int button, int dx, int dy)
{
	int x,y;
// printf("b=%d dx=%d dy=%d ox=%d oy=%d\n"
// ,button,dx,dy,cs_mouse_x,cs_mouse_y);
	x=cs_mouse_x;
	y=cs_mouse_y;
#ifndef LC_X11
	cs_mouse_x+=dx*MOUSE_SENSITIVITY;
	cs_mouse_y+=dy*MOUSE_SENSITIVITY;
#else
	cs_mouse_x+=dx;
	cs_mouse_y+=dy;
#endif

#ifndef LC_X11
	if (cs_mouse_x>=cs_mouse_xmax)
		cs_mouse_x=cs_mouse_xmax;
	if (cs_mouse_y>=cs_mouse_ymax)
		cs_mouse_y=cs_mouse_ymax;
	if (cs_mouse_x<0)
		cs_mouse_x=0;
	if (cs_mouse_y<0)
		cs_mouse_y=0;
#endif
	if (cs_mouse_x!=x || cs_mouse_y!=y)
		move_mouse(cs_mouse_x,cs_mouse_y);
	x=cs_mouse_x;
	y=cs_mouse_y;
#ifdef DEBUG_MOUSE
        printf("cs_mouse_button=%d  button=%d\n",cs_mouse_button,button);
#endif
// Latest hack for getting rid of mouse 'droppings' when you click on
// a dialogue box.
	if (must_release_button!=0)
	{
		if (button==0)
			must_release_button=0;
		else
			return;
	}
#ifdef LC_X11
        if (mt_flag && cs_mouse_shifted==0)
	{
                do_multi_transport(x,y,MT_FAIL);
		return;
	}
#endif
	if (button!=MOUSE_LEFTBUTTON)
		cs_mouse_button_repeat=real_time+800;
// Left button has auto repeat.
	if ((cs_mouse_button==0 
		|| cs_mouse_button_repeat<=real_time)
//		&& (button==MOUSE_LEFTBUTTON || button==MOUSE_RIGHTBUTTON)
		&& button!=0
		&& cs_mouse_button_delay==0)
	{
		cs_mouse_button_repeat=real_time+500;
//printf("Mouse button pressed at %d %d\n",x,y);
		if (market_cb_flag)
			do_market_cb_mouse(x,y);
		else if (port_cb_flag)
			do_port_cb_mouse(x,y);
		else if (help_flag)
			do_help_mouse(x,y,button);
		else if (db_flag)
			do_db_mouse(x,y);
		else if (db_okflag)
			do_db_okmouse(x,y);
		else if (load_flag || save_flag)
			return;
		else if (x>SELECT_BUTTON_WIN_X
			&& x<(SELECT_BUTTON_WIN_X+SELECT_BUTTON_WIN_W)
			&& y>SELECT_BUTTON_WIN_Y
			&& y<(SELECT_BUTTON_WIN_Y+SELECT_BUTTON_WIN_H))
			do_mouse_select_buttons(x,y,button);
		else if (x>MAIN_WIN_X && x<(MAIN_WIN_X+MAIN_WIN_W)
			&& y>MAIN_WIN_Y && y<(MAIN_WIN_Y+MAIN_WIN_H))
		{
			do_mouse_main_win(x,y,button);
			refresh_main_screen();
		}
		else
			do_mouse_other_buttons(x,y,button);
	}
	cs_mouse_button=button;
}

void move_mouse(int x,int y)
{
	int size;
#ifdef DEBUGMOUSE
	printf("In move_mouse()\n");
#endif
	size=(main_types[selected_type].size)*16;
	
	if (x>MAIN_WIN_X && x<(MAIN_WIN_X+MAIN_WIN_W)
		&& y>MAIN_WIN_Y && y<(MAIN_WIN_Y+MAIN_WIN_H)
		&& market_cb_flag==0 && port_cb_flag==0 && help_flag==0 
		&& db_flag==0 && db_okflag==0)
	{
		if (x>(MAIN_WIN_X+MAIN_WIN_W)-size)
			x=(MAIN_WIN_X+MAIN_WIN_W)-size;
		if (y>(MAIN_WIN_Y+MAIN_WIN_H)-size)
			y=(MAIN_WIN_Y+MAIN_WIN_H)-size;
		x&=0xff0;
		y&=0xff0;
                x+=(MAIN_WIN_X%16);
                y+=(MAIN_WIN_Y%16);
		hide_mouse();
		mouse_hide_count--;
		draw_square_mouse(x,y,size);
		if (mt_flag)
			do_multi_transport(((x-MAIN_WIN_X)/16)
			+main_screen_originx
			,((y-MAIN_WIN_Y)/16)
			+main_screen_originy,MT_CONTINUE);
	}
	else
	{
#ifdef DEBUGMOUSE
	printf("mox=%d moy=%d x=%d y=%d\n",mox,moy,x,y);
#endif
		hide_mouse();
		mouse_hide_count--;
		draw_normal_mouse(x,y);
		if (mappoint_stats_flag!=0 && market_cb_flag==0
			&& port_cb_flag==0)
		{
			mappoint_stats_flag=0;
			draw_diffgraph();
		}
	}
#ifdef DEBUGMOUSE
	printf("Got to the end of move_mouse()\n");
#endif
}

void hide_mouse(void)
{
	if (mouse_type==MOUSE_TYPE_SQUARE)
		hide_square_mouse();
	else
		hide_normal_mouse();
	mouse_hide_count++;
}

void redraw_mouse(void)
{
	if (--mouse_hide_count>0)
		return;
	mouse_hide_count=0;
	if (mouse_type==MOUSE_TYPE_SQUARE)
		redraw_square_mouse();
	else
		redraw_normal_mouse();
}

void draw_square_mouse(int x,int y,int size)  // size is pixels
{
	if (mouse_type==MOUSE_TYPE_NORMAL)
	{
		hide_normal_mouse();
		mouse_type=MOUSE_TYPE_SQUARE;
		kmouse_val=16;
	}
	Fgl_getbox(x-2,y-2,size+4,2,under_square_mouse_pointer_top);
        Fgl_getbox(x-2,y,2,size,under_square_mouse_pointer_left);
        Fgl_getbox(x+size,y,2,size,under_square_mouse_pointer_right);
        Fgl_getbox(x-2,y+size,size+4,2,under_square_mouse_pointer_bottom);

	Fgl_hline(x-2,y-2,x+size+1,yellow(31));
	Fgl_hline(x-1,y-1,x+size,blue(31));
	Fgl_hline(x-2,y+size+1,x+size+1,yellow(31));
	Fgl_hline(x-1,y+size,x+size,blue(31));
	Fgl_line(x-2,y-1,x-2,y+size+1,yellow(31));
	Fgl_line(x-1,y,x-1,y+size,blue(31));
	Fgl_line(x+size+1,y-1,x+size+1,y+size+1,yellow(31));
	Fgl_line(x+size,y,x+size,y+size,blue(31));
	omx=x;
	omy=y;
}

void hide_square_mouse(void)
{
	int size;
	size=(main_types[selected_type].size)*16;
        Fgl_putbox(omx-2,omy-2,size+4,2,under_square_mouse_pointer_top);
        Fgl_putbox(omx-2,omy,2,size,under_square_mouse_pointer_left);
        Fgl_putbox(omx+size,omy,2,size,under_square_mouse_pointer_right);
        Fgl_putbox(omx-2,omy+size,size+4,2,under_square_mouse_pointer_bottom);
}

void redraw_square_mouse(void)
{
	int size;
	size=(main_types[selected_type].size)*16;

        Fgl_getbox(omx-2,omy-2,size+4,2,under_square_mouse_pointer_top);
        Fgl_getbox(omx-2,omy,2,size,under_square_mouse_pointer_left);
        Fgl_getbox(omx+size,omy,2,size,under_square_mouse_pointer_right);
        Fgl_getbox(omx-2,omy+size,size+4,2,under_square_mouse_pointer_bottom);

        Fgl_hline(omx-2,omy-2,omx+size+1,yellow(31));
        Fgl_hline(omx-1,omy-1,omx+size,blue(31));
        Fgl_hline(omx-2,omy+size+1,omx+size+1,yellow(31));
        Fgl_hline(omx-1,omy+size,omx+size,blue(31));
        Fgl_line(omx-2,omy-1,omx-2,omy+size+1,yellow(31));
        Fgl_line(omx-1,omy,omx-1,omy+size,blue(31));
        Fgl_line(omx+size+1,omy-1,omx+size+1,omy+size+1,yellow(31));
        Fgl_line(omx+size,omy,omx+size,omy+size,blue(31));

}

void draw_normal_mouse(int x,int y)
{
	if (mouse_type==MOUSE_TYPE_SQUARE)
	{
		hide_square_mouse();
		mouse_type=MOUSE_TYPE_NORMAL;
		kmouse_val=8;
	}
#ifndef LC_X11
        Fgl_getbox(x,y,8,8,under_mouse_pointer);
	if (x>631 || y>471)
	{
		Fgl_enableclipping();
		Fgl_setclippingwindow(0,0,639,479);
		Fgl_putbox(x,y,8,8,mouse_pointer);
		Fgl_disableclipping();
	}
	else
	        Fgl_putbox(x,y,8,8,mouse_pointer);
#endif
        mox=x;
        moy=y;
}

void hide_normal_mouse(void)
{
#ifndef LC_X11
	if (mox>631 || moy>471)
	{
		Fgl_enableclipping();
		Fgl_setclippingwindow(0,0,639,479);
		Fgl_putbox(mox,moy,8,8,under_mouse_pointer);
		Fgl_disableclipping();
	}
	else
		Fgl_putbox(mox,moy,8,8,under_mouse_pointer);
#endif
}

void redraw_normal_mouse(void)
{
#ifndef LC_X11
	if (mox>631 || moy>471)
	{
		Fgl_enableclipping();
		Fgl_setclippingwindow(0,0,639,479);
		Fgl_getbox(mox,moy,8,8,under_mouse_pointer);
		Fgl_putbox(mox,moy,8,8,mouse_pointer);
		Fgl_disableclipping();
	}
	else
	{
		Fgl_getbox(mox,moy,8,8,under_mouse_pointer); // may have changed
		Fgl_putbox(mox,moy,8,8,mouse_pointer);
	}
#endif
}

void do_mouse_select_buttons(int x,int y,int mbutton)
{
	int xx,yy;
	xx=x-SELECT_BUTTON_WIN_X;
	yy=y-SELECT_BUTTON_WIN_Y;
#ifdef DEBUG_MOUSE
	printf("do_mouse_select_buttons() xx=%d yy=%d\n",xx,yy);
#endif
	if (xx>=8 && xx<=(8+16))
	{
		if (yy>=8 && yy<=(8+16))
			do_select_button(0,mbutton);
		else if (yy>=32 && yy<=(32+16))
			do_select_button(1,mbutton);
                else if (yy>=(32+24) && yy<=(32+16+24))
                        do_select_button(2,mbutton);
                else if (yy>=(32+24+24) && yy<=(32+16+24+24))
                        do_select_button(3,mbutton);
		else if (yy>=(32+24+24+24) && yy<=(32+16+24+24+24))
			do_select_button(4,mbutton);
		else if (yy>=(32+24+24+24+24) && yy<=(32+16+24+24+24+24))
			do_select_button(5,mbutton);
                else if (yy>=(32+24+24+24+24+24) && yy<=(32+16+24+24+24+24+24))
                        do_select_button(6,mbutton);
                else if (yy>=(32+24+24+24+24+24+24) 
			&& yy<=(32+16+24+24+24+24+24+24))
                        do_select_button(7,mbutton);
                else if (yy>=(32+24+24+24+24+24+24+24)
                        && yy<=(32+16+24+24+24+24+24+24+24))
                        do_select_button(8,mbutton);
                else if (yy>=(32+24+24+24+24+24+24+24+24)
                        && yy<=(32+16+24+24+24+24+24+24+24+24))
                        do_select_button(9,mbutton);
                else if (yy>=(32+24+24+24+24+24+24+24+24+24)
                        && yy<=(32+16+24+24+24+24+24+24+24+24+24))
                        do_select_button(10,mbutton);
                else if (yy>=(32+24+24+24+24+24+24+24+24+24+24)
                        && yy<=(32+16+24+24+24+24+24+24+24+24+24+24))
                        do_select_button(11,mbutton);
                else if (yy>=(32+24+24+24+24+24+24+24+24+24+24+24)
                        && yy<=(32+16+24+24+24+24+24+24+24+24+24+24+24))
                        do_select_button(12,mbutton);
                else if (yy>=(32+24+24+24+24+24+24+24+24+24+24+24+24)
                        && yy<=(32+16+24+24+24+24+24+24+24+24+24+24+24+24))
                        do_select_button(13,mbutton);
                else if (yy>=(32+24+24+24+24+24+24+24+24+24+24+24+24+24)
                        && yy<=(32+16+24+24+24+24+24+24+24+24+24+24+24+24+24))
                        do_select_button(14,mbutton);
                else if (yy>=(32+24+24+24+24+24+24+24+24+24+24+24+24+24+24)
                        && yy<=(32+16+24+24+24+24+24+24+24+24+24+24+24
				+24+24+24))
                        do_select_button(15,mbutton);
	}
	else if (xx>=32 && xx<=(32+16))
	{
                if (yy>=8 && yy<=(8+16))
                        do_select_button(16,mbutton);
                else if (yy>=32 && yy<=(32+16))
                        do_select_button(17,mbutton);
                else if (yy>=(32+24) && yy<=(32+16+24))
                        do_select_button(18,mbutton);
                else if (yy>=(32+24+24) && yy<=(32+16+24+24))
                        do_select_button(19,mbutton);
                else if (yy>=(32+24+24+24) && yy<=(32+16+24+24+24))
                        do_select_button(20,mbutton);
                else if (yy>=(32+24+24+24+24) && yy<=(32+16+24+24+24+24))
                        do_select_button(21,mbutton);
                else if (yy>=(32+24+24+24+24+24) 
				&& yy<=(32+16+24+24+24+24+24))
			do_select_button(22,mbutton);
                else if (yy>=(32+24+24+24+24+24+24)
                                && yy<=(32+16+24+24+24+24+24+24))
                        do_select_button(23,mbutton);
                else if (yy>=(32+24+24+24+24+24+24+24)
                                && yy<=(32+16+24+24+24+24+24+24+24))
                        do_select_button(24,mbutton);
                else if (yy>=(32+24+24+24+24+24+24+24+24)
                                && yy<=(32+16+24+24+24+24+24+24+24+24))
                        do_select_button(25,mbutton);
                else if (yy>=(32+24+24+24+24+24+24+24+24+24)
                                && yy<=(32+16+24+24+24+24+24+24+24+24+24))
                        do_select_button(26,mbutton);
                else if (yy>=(32+24+24+24+24+24+24+24+24+24+24)
                                && yy<=(32+16+24+24+24+24+24+24+24+24+24+24))
                        do_select_button(27,mbutton);
                else if (yy>=(32+24+24+24+24+24+24+24+24+24+24+24)
                                && yy<=(32+16+24+24+24+24+24+24+24+24+24
					+24+24))
                        do_select_button(28,mbutton);
                else if (yy>=(32+24+24+24+24+24+24+24+24+24+24+24+24)
                                && yy<=(32+16+24+24+24+24+24+24+24+24+24
                                        +24+24+24))
                        do_select_button(29,mbutton);
                else if (yy>=(32+24+24+24+24+24+24+24+24+24+24+24+24+24)
                                && yy<=(32+16+24+24+24+24+24+24+24+24+24
                                        +24+24+24+24))
                        do_select_button(30,mbutton);
                else if (yy>=(32+24+24+24+24+24+24+24+24+24+24+24+24+24+24)
                                && yy<=(32+16+24+24+24+24+24+24+24+24+24
                                        +24+24+24+24+24))
                        do_select_button(31,mbutton);
 
	}

}

void do_select_button(int button,int mbutton)
{
	char s[100];
	int t;
        if (select_button_tflag[button]==0 && mbutton!=MOUSE_RIGHTBUTTON)
        {
                ok_dial_box("not_enough_tech.mes",BAD,0L);
                return;
        }
	if (mbutton==MOUSE_RIGHTBUTTON || select_button_help_flag[button]==0)
	{
		help_flag=1;
		activate_help(select_button_help[button]);
		if (mbutton!=MOUSE_RIGHTBUTTON)
			select_button_help_flag[button]=1;
		if (mbutton==MOUSE_RIGHTBUTTON)
			return;
	}
//	if (select_button_tflag[button]==0)
//	{
//		ok_dial_box("not_enough_tech.mes",BAD,0L);
//		return;
//	}
	selected_button=button;
	unhighlight_select_button(old_selected_button);
	highlight_select_button(button);
	old_selected_button=button;
	selected_type=select_button_type[button];
	if (selected_type==CST_RESIDENCE_LL)
		choose_residence();
	t=main_types[selected_type].group; // t is the group
#ifdef USE_EXPANDED_FONT
	Fgl_fillbox(SELECT_BUTTON_MESSAGE_X,SELECT_BUTTON_MESSAGE_Y
		,44*8,8,0);
	Fgl_fillbox(SELECT_BUTTON_MESSAGE_X,SELECT_BUTTON_MESSAGE_Y
		,44*8,8,blue(10));
	gl_setwritemode(WRITEMODE_MASKED | FONT_EXPANDED);
#else
	Fgl_fillbox(SELECT_BUTTON_MESSAGE_X,SELECT_BUTTON_MESSAGE_Y
		,42*8,8,TEXT_BG_COLOUR);
#endif
	selected_type_cost=(int)((float)select_button_cost[button]
		+((float)select_button_cost[button]
		*((float)select_button_cost_mul[button]
		*(float)tech_level)/(float)MAX_TECH_LEVEL));
	if (t==GROUP_BARE)
	{
		sprintf(s," Bulldoze - cost POA");
#ifdef LC_X11
		XDefineCursor(display.dpy,display.win,pirate_cursor);
#endif
	}
	else
	{
		sprintf(s,"%s %d  Bulldoze %d",type_string[t]
			,selected_type_cost,select_button_bul_cost[button]);
#ifdef LC_X11
		XDefineCursor(display.dpy,display.win,None);
#endif
	}
	Fgl_write(SELECT_BUTTON_MESSAGE_X,SELECT_BUTTON_MESSAGE_Y,s);
#ifdef USE_EXPANDED_FONT
	gl_setwritemode(WRITEMODE_OVERWRITE | FONT_EXPANDED);
#endif
	if (selected_type==CST_GREEN)
		draw_main_window_box(red(8));
	else
	{
		draw_main_window_box(green(8));
		monument_bul_flag=0;
		river_bul_flag=0;
	}
}

void highlight_select_button(int button)
{
	int x,y,q;
	if (button<NUMOF_SELECT_BUTTONS_DOWN)
	{
                x=8+SELECT_BUTTON_WIN_X;
                y=8+(button*24)+SELECT_BUTTON_WIN_Y;
        }
        else
        {
                x=8+24+SELECT_BUTTON_WIN_X;
                y=8+((button-NUMOF_SELECT_BUTTONS_DOWN)*24)
			+SELECT_BUTTON_WIN_Y;
        }
	hide_mouse();
	for (q=0;q<3;q++)
	{
		Fgl_hline(x-1-q,y-1-q,x+16+q,yellow(16+q*4));
		Fgl_hline(x-1-q,y+q+16,x+16+q,yellow(16+q*4));
		Fgl_line(x-1-q,y-1-q,x-1-q,y+q+16,yellow(16+q*4));
		Fgl_line(x+16+q,y-1-q,x+16+q,y+q+16,yellow(16+q*4));
	}
	redraw_mouse();
}

void unhighlight_select_button(int button)
{
        int x,y,q;
        if (button<NUMOF_SELECT_BUTTONS_DOWN)
        {
                x=8+SELECT_BUTTON_WIN_X;
                y=8+(button*24)+SELECT_BUTTON_WIN_Y;
        }
        else
        {
                x=8+24+SELECT_BUTTON_WIN_X;
                y=8+((button-NUMOF_SELECT_BUTTONS_DOWN)*24)
			+SELECT_BUTTON_WIN_Y;
        }
	hide_mouse();
        for (q=0;q<3;q++)
        {
                Fgl_hline(x-1-q,y-1-q,x+16+q,blue(16+q*4));
                Fgl_hline(x-1-q,y+q+16,x+16+q,blue(16+q*4));
                Fgl_line(x-1-q,y-1-q,x-1-q,y+q+16,blue(16+q*4));
                Fgl_line(x+16+q,y-1-q,x+16+q,y+q+16,blue(16+q*4));
        }
	redraw_mouse();
}


void do_mouse_main_win(int x,int y,int button)
{
#ifdef DEBUG_MAIN_SCREEN
        printf("In do_mouse_main_win() mappoint x=%d y=%d type=%d\n"
		,(x-MAIN_WIN_X)/16,(y-MAIN_WIN_Y)/16,selected_type);
#endif
	int size;
	size=(main_types[selected_type].size)*16;
        if (x>(MAIN_WIN_X+MAIN_WIN_W)-size)
                x=(MAIN_WIN_X+MAIN_WIN_W)-size;
        if (y>(MAIN_WIN_Y+MAIN_WIN_H)-size)
                y=(MAIN_WIN_Y+MAIN_WIN_H)-size;
        x+=(MAIN_WIN_X%16);
        y+=(MAIN_WIN_Y%16);
	x=(x-MAIN_WIN_X)/16;
	y=(y-MAIN_WIN_Y)/16;
	x+=main_screen_originx;
	y+=main_screen_originy;
	if (mappointtype[x][y]!=0 || button==MOUSE_RIGHTBUTTON)
	{
#ifdef LC_X11
                if (mt_flag)
		{
			if (cs_mouse_shifted)
                        	do_multi_transport(x,y,MT_SUCCESS);
			else
				do_multi_transport(x,y,MT_FAIL);
			return;
		}
#endif
		if (selected_type==CST_GREEN && button!=MOUSE_RIGHTBUTTON)
		{
			bulldoze_area(x,y);
			return;
		}
		else if (overwrite_transport_flag
			&& (main_types[selected_type].group==GROUP_TRACK
			|| main_types[selected_type].group==GROUP_ROAD
			|| main_types[selected_type].group==GROUP_RAIL)
                        && (main_types[mappointtype[x][y]].group==GROUP_TRACK
                        || main_types[mappointtype[x][y]].group==GROUP_ROAD
                        || main_types[mappointtype[x][y]].group==GROUP_RAIL)
			&& main_types[selected_type].group
				!=main_types[mappointtype[x][y]].group)
		{
			bulldoze_area(x,y); // continue to place new
		}
		else if (mappointtype[x][y]==CST_USED)
		{
			mappoint_stats(mappoint[x][y].int_1
				,mappoint[x][y].int_2,button);
			return;
		}
		else
		{
			mappoint_stats(x,y,button);
			return;
		}
	} 
#ifdef LC_X11
	if (cs_mouse_shifted!=0 &&
		(main_types[selected_type].group==GROUP_TRACK
		|| main_types[selected_type].group==GROUP_ROAD
		|| main_types[selected_type].group==GROUP_RAIL
		|| main_types[selected_type].group==GROUP_POWER_LINE))
	{
		if (mt_flag!=0)
			do_multi_transport(x,y,MT_SUCCESS);
		else
			do_multi_transport(x,y,MT_START);
		return;
	}
	if (mt_flag)
	{
		do_multi_transport(x,y,MT_FAIL);
		return;
	}
#endif
        if (main_types[selected_type].size>=2)
        {
                if (mappointtype[x+1][y]!=0
                        || mappointtype[x][y+1]!=0
                        || mappointtype[x+1][y+1]!=0)
                        return;
	}
	if (main_types[selected_type].size>=3)
	{
		if (mappointtype[x+2][y]!=0
			|| mappointtype[x+2][y+1]!=0
			|| mappointtype[x+2][y+2]!=0
			|| mappointtype[x+1][y+2]!=0
			|| mappointtype[x][y+2]!=0)
			return;
	}
	if (main_types[selected_type].size==4)
	{
		if (mappointtype[x+3][y]!=0
			|| mappointtype[x+3][y+1]!=0
			|| mappointtype[x+3][y+2]!=0
			|| mappointtype[x+3][y+3]!=0
			|| mappointtype[x+2][y+3]!=0
			|| mappointtype[x+1][y+3]!=0
			|| mappointtype[x][y+3]!=0)
			return;
	}

        if (no_credit_build()!=0) // Puts up a message box if you can't
                return;  // build because credit is not available.
        if (main_types[selected_type].group==GROUP_SUBSTATION
		|| main_types[selected_type].group==GROUP_WINDMILL)
                if (add_a_substation(x,y)==0)
			return;
	if (main_types[selected_type].group==GROUP_EX_PORT)
	{
		if (is_real_river(x+4,y)!=1 || is_real_river(x+4,y+1)!=1
		|| is_real_river(x+4,y+2)!=1 || is_real_river(x+4,y+3)!=1)
		{
                        if (yn_dial_box("WARNING"
                                ,"Ports need to be"
                                ,"connected to rivers!"
				,"Want to make a cup of tea?")==0)
                                return;
			else 
				while (yn_dial_box("TEA BREAK"
				,"Boil->pour->wait->stir"
				,"stir->pour->stir->wait->drink...ahhh"
				,"Have you finished yet?")==0);
			return;
		}
	}
	if (main_types[selected_type].group==GROUP_COMMUNE)
		numof_communes++;
	if (main_types[selected_type].group==GROUP_MARKET)
	{
		if (add_a_market(x,y)==0)
			return;
		mappoint[x][y].flags+=(FLAG_MB_FOOD | FLAG_MB_JOBS
			| FLAG_MB_COAL | FLAG_MB_ORE | FLAG_MB_STEEL
			| FLAG_MB_GOODS | FLAG_MS_FOOD | FLAG_MS_JOBS
			| FLAG_MS_COAL | FLAG_MS_GOODS | FLAG_MS_ORE
			| FLAG_MS_STEEL);
	}
	last_built_x=x;
	last_built_y=y;
	if (main_types[selected_type].group==GROUP_POWER_SOURCE
		|| main_types[selected_type].group==GROUP_WINDMILL)
	{
		mappoint[x][y].int_2=tech_level;
		let_one_through=1;
	}
//	else if (main_types[selected_type].group==GROUP_RESIDENCE)
//		mappoint[x][y].population=2;
	else if (main_types[selected_type].group==GROUP_RECYCLE
		|| main_types[selected_type].group==GROUP_POWER_SOURCE_COAL)
		mappoint[x][y].int_4=tech_level;
	else if (main_types[selected_type].group==GROUP_ORGANIC_FARM)
		mappoint[x][y].int_1=tech_level;
	else if (main_types[selected_type].group==GROUP_TRACK
		|| main_types[selected_type].group==GROUP_ROAD
		|| main_types[selected_type].group==GROUP_RAIL)
		mappoint[x][y].flags|=FLAG_IS_TRANSPORT;
	else if (main_types[selected_type].group==GROUP_COALMINE
		|| main_types[selected_type].group==GROUP_OREMINE)
		let_one_through=1;
	if (main_types[selected_type].size==2)
	{
//		if (mappointtype[x+1][y]!=0
//			|| mappointtype[x][y+1]!=0
//			|| mappointtype[x+1][y+1]!=0)
//			return;
		mappointtype[x+1][y]=CST_USED;
		set_mappoint_ints(x,y,x+1,y);
		mappointtype[x][y+1]=CST_USED;
		set_mappoint_ints(x,y,x,y+1);
		mappointtype[x+1][y+1]=CST_USED;
		set_mappoint_ints(x,y,x+1,y+1);
	}
	else if (main_types[selected_type].size==3)
	{
//		if (mappointtype[x+1][y]!=0
//			|| mappointtype[x+2][y]!=0
//			|| mappointtype[x+1][y+1]!=0
//			|| mappointtype[x+2][y+1]!=0
//			|| mappointtype[x+1][y+2]!=0
//			|| mappointtype[x+2][y+2]!=0
//			|| mappointtype[x][y+1]!=0
//			|| mappointtype[x][y+2]!=0)
//			return;
		mappointtype[x+1][y]=CST_USED;
		set_mappoint_ints(x,y,x+1,y);
		mappointtype[x+2][y]=CST_USED;
		set_mappoint_ints(x,y,x+2,y);
		mappointtype[x+1][y+1]=CST_USED;
		set_mappoint_ints(x,y,x+1,y+1);
		mappointtype[x+2][y+1]=CST_USED;
		set_mappoint_ints(x,y,x+2,y+1);
		mappointtype[x+1][y+2]=CST_USED;
		set_mappoint_ints(x,y,x+1,y+2);
		mappointtype[x+2][y+2]=CST_USED;
		set_mappoint_ints(x,y,x+2,y+2);
		mappointtype[x][y+1]=CST_USED;
		set_mappoint_ints(x,y,x,y+1);
		mappointtype[x][y+2]=CST_USED;
		set_mappoint_ints(x,y,x,y+2);
	}
        else if (main_types[selected_type].size==4)
        {
//                if (mappointtype[x+1][y]!=0
//                        || mappointtype[x+2][y]!=0
//                        || mappointtype[x+1][y+1]!=0
//                        || mappointtype[x+2][y+1]!=0
//                        || mappointtype[x+1][y+2]!=0
//                        || mappointtype[x+2][y+2]!=0
//                        || mappointtype[x][y+1]!=0
//                        || mappointtype[x][y+2]!=0

//			|| mappointtype[x+3][y]!=0
//			|| mappointtype[x+3][y+1]!=0
//			|| mappointtype[x+3][y+2]!=0
//			|| mappointtype[x+3][y+3]!=0
//			|| mappointtype[x][y+3]!=0
//			|| mappointtype[x+1][y+3]!=0
//			|| mappointtype[x+2][y+3]!=0)
//			return;
                mappointtype[x+1][y]=CST_USED;
		set_mappoint_ints(x,y,x+1,y);
                mappointtype[x+2][y]=CST_USED;
		set_mappoint_ints(x,y,x+2,y);
                mappointtype[x+1][y+1]=CST_USED;
		set_mappoint_ints(x,y,x+1,y+1);
                mappointtype[x+2][y+1]=CST_USED;
		set_mappoint_ints(x,y,x+2,y+1);
                mappointtype[x+1][y+2]=CST_USED;
		set_mappoint_ints(x,y,x+1,y+2);
                mappointtype[x+2][y+2]=CST_USED;
		set_mappoint_ints(x,y,x+2,y+2);
                mappointtype[x][y+1]=CST_USED;
		set_mappoint_ints(x,y,x,y+1);
                mappointtype[x][y+2]=CST_USED;
		set_mappoint_ints(x,y,x,y+2);

                mappointtype[x+3][y]=CST_USED;
		set_mappoint_ints(x,y,x+3,y);
                mappointtype[x+3][y+1]=CST_USED;
		set_mappoint_ints(x,y,x+3,y+1);
                mappointtype[x+3][y+2]=CST_USED;
		set_mappoint_ints(x,y,x+3,y+2);
                mappointtype[x+3][y+3]=CST_USED;
		set_mappoint_ints(x,y,x+3,y+3);
                mappointtype[x][y+3]=CST_USED;
		set_mappoint_ints(x,y,x,y+3);
                mappointtype[x+1][y+3]=CST_USED;
		set_mappoint_ints(x,y,x+1,y+3);
                mappointtype[x+2][y+3]=CST_USED;
		set_mappoint_ints(x,y,x+2,y+3);

        }
	mappointtype[x][y]=selected_type;
	if (main_types[selected_type].group==GROUP_RIVER)
		connect_rivers();
	total_money-=selected_type_cost;
	print_total_money();
}

void set_mappoint_ints(int fromx,int fromy,int x,int y)
{
	mappoint[x][y].int_1=fromx;
	mappoint[x][y].int_2=fromy;
}

void do_mouse_other_buttons(int x,int y,int button)
{

// these must be checked for before the scroll areas.

// this is the quit button
        if (x>QUIT_BUTTON_X && x<QUIT_BUTTON_X+QUIT_BUTTON_W
                && y>QUIT_BUTTON_Y && y<QUIT_BUTTON_Y+QUIT_BUTTON_H)
		{
                	quit_flag=1;
			if (pause_flag)
				let_one_through=1;
		}

// this is the load button
        else if (x>LOAD_BUTTON_X && x<LOAD_BUTTON_X+LOAD_BUTTON_W
                && y>LOAD_BUTTON_Y && y<LOAD_BUTTON_Y+LOAD_BUTTON_H)
	{
		if (save_flag==0)
                	load_flag=1;
	}

// this is the pause button
        else if (x>PAUSE_BUTTON_X && x<PAUSE_BUTTON_X+PAUSE_BUTTON_W
                && y>PAUSE_BUTTON_Y && y<PAUSE_BUTTON_Y+PAUSE_BUTTON_H)
        {
                if (button==MOUSE_RIGHTBUTTON)
                {
                        help_flag=1;
                        activate_help("pause.hlp");
                        return;
                }
                select_pause();
        }

// main screen border scroll areas
// up
	else if (x>=(MAIN_WIN_X-8) && x<(MAIN_WIN_X+MAIN_WIN_W+8)
		&& y>=(MAIN_WIN_Y-8) && y<MAIN_WIN_Y
		&& main_screen_originy>1)
		{
			if (button==MOUSE_RIGHTBUTTON)
			{
				main_screen_originy-=RIGHT_MOUSE_MOVE_VAL;
				if (main_screen_originy<1)
					main_screen_originy=1;
			}
			else
				main_screen_originy--;
			hide_mouse();  // we have a normal mouse so it
			refresh_main_screen();  // won't be taken care
			redraw_mouse();   // of in refresh_main_screen.
		}
// down
	else if (x>=(MAIN_WIN_X-8) && x<(MAIN_WIN_X+MAIN_WIN_W+8)
		&& y>(MAIN_WIN_Y+MAIN_WIN_H)
		&& y<(MAIN_WIN_Y+MAIN_WIN_H+16)  // 16 for bigger area
		&& main_screen_originy<(WORLD_SIDE_LEN-MAIN_WIN_H/16)-1)
		{
			if (button==MOUSE_RIGHTBUTTON)
			{
				main_screen_originy+=RIGHT_MOUSE_MOVE_VAL;
				if (main_screen_originy
					>(WORLD_SIDE_LEN-MAIN_WIN_H/16)-2)
					main_screen_originy
					=(WORLD_SIDE_LEN-MAIN_WIN_H/16)-2;
			}
			main_screen_originy++;
			refresh_main_screen();
		}
// left
	else if (x>=(MAIN_WIN_X-16) && x<MAIN_WIN_X
		&& y>=(MAIN_WIN_Y-8) && y<(MAIN_WIN_Y+MAIN_WIN_H+8)
		&& main_screen_originx>1)
		{
			if (button==MOUSE_RIGHTBUTTON)
			{
				main_screen_originx-=RIGHT_MOUSE_MOVE_VAL;
				if (main_screen_originx<1)
					main_screen_originx=1;
			}
			else
				main_screen_originx--;
			hide_mouse();
			refresh_main_screen();
			redraw_mouse();
		}
// right
	else if (x>(MAIN_WIN_X+MAIN_WIN_W)
		&& x<(MAIN_WIN_X+MAIN_WIN_W+8)
		&& y>(MAIN_WIN_Y-8) && y<(MAIN_WIN_Y+MAIN_WIN_H+8)
		&& main_screen_originx<(WORLD_SIDE_LEN-MAIN_WIN_H/16)-1)
		{
			if (button==MOUSE_RIGHTBUTTON)
			{
				main_screen_originx+=RIGHT_MOUSE_MOVE_VAL;
				if (main_screen_originx
					>(WORLD_SIDE_LEN-MAIN_WIN_H/16)-2)
					main_screen_originx
					=(WORLD_SIDE_LEN-MAIN_WIN_H/16)-2;
			}
			main_screen_originx++;
			refresh_main_screen();
		}

// This is the mini window. Clicking here move the main window to this point
	else if (x>(MINI_SCREEN_X-3) && x<(MINI_SCREEN_X+WORLD_SIDE_LEN)
		&& y>(MINI_SCREEN_Y-3) && y<(MINI_SCREEN_Y+WORLD_SIDE_LEN))
	{
		if (button==MOUSE_RIGHTBUTTON)
		{
			help_flag=1;
			activate_help("mini-screen.hlp");
			return;
		}
		main_screen_originx=x-MINI_SCREEN_X-MAIN_WIN_W/32;
		if (main_screen_originx>(WORLD_SIDE_LEN-MAIN_WIN_W/16)-1)
			main_screen_originx=(WORLD_SIDE_LEN-MAIN_WIN_H/16)-1;
		if (main_screen_originx<=0)
			main_screen_originx=1;
		main_screen_originy=y-MINI_SCREEN_Y-MAIN_WIN_H/32;
		if (main_screen_originy>(WORLD_SIDE_LEN-MAIN_WIN_H/16)-1)
			main_screen_originy=(WORLD_SIDE_LEN-MAIN_WIN_H/16)-1;
		if (main_screen_originy<=0)
			main_screen_originy=1;
		hide_mouse();
		refresh_main_screen();
		redraw_mouse();
		if (mini_screen_flags==MINI_SCREEN_PORT_FLAG)
			draw_mini_screen();
	}

// this is the normal button to return the mini screen to normal.
	else if (x>MS_NORMAL_BUTTON_X && x<MS_NORMAL_BUTTON_X+16
		&& y>MS_NORMAL_BUTTON_Y && y<MS_NORMAL_BUTTON_Y+16)
	{
                if (button==MOUSE_RIGHTBUTTON)
                {
                        help_flag=1;
                        activate_help("msb-normal.hlp");
                        return;
                }
		draw_mini_screen();
	}
// this is the pollution button to activate the mini screen pol'n mode.
	else if (x>MS_POLLUTION_BUTTON_X && x<MS_POLLUTION_BUTTON_X+16
		&& y>MS_POLLUTION_BUTTON_Y && y<MS_POLLUTION_BUTTON_Y+16)
	{
                if (button==MOUSE_RIGHTBUTTON)
                {
                        help_flag=1;
                        activate_help("msb-pol.hlp");
                        return;
                }
		draw_mini_screen_pollution();
	}
// this is the fire cover button to activate the mini screen.
        else if (x>MS_FIRE_COVER_BUTTON_X && x<MS_FIRE_COVER_BUTTON_X+16
                && y>MS_FIRE_COVER_BUTTON_Y 
		&& y<MS_FIRE_COVER_BUTTON_Y+16)
	{
		if (button==MOUSE_RIGHTBUTTON)
                {
                        help_flag=1;
                        activate_help("msb-fire.hlp");
                        return;
                }
                draw_mini_screen_fire_cover();
	}
// this is the health cover button to activate the mini screen.
        else if (x>MS_HEALTH_COVER_BUTTON_X && x<MS_HEALTH_COVER_BUTTON_X+16
                && y>MS_HEALTH_COVER_BUTTON_Y
                && y<MS_HEALTH_COVER_BUTTON_Y+16)
	{
		if (button==MOUSE_RIGHTBUTTON)
                {
                        help_flag=1;
                        activate_help("msb-health.hlp");
                        return;
                }
                draw_mini_screen_health_cover();
	}
// this is the cricket cover button to activate the mini screen.
        else if (x>MS_CRICKET_COVER_BUTTON_X && x<MS_CRICKET_COVER_BUTTON_X+16
                && y>MS_CRICKET_COVER_BUTTON_Y
                && y<MS_CRICKET_COVER_BUTTON_Y+16)
	{
		if (button==MOUSE_RIGHTBUTTON)
                {
                        help_flag=1;
                        activate_help("msb-cricket.hlp");
                        return;
                }
                draw_mini_screen_cricket_cover();
	}
// this is the ub40 button
	else if (x>MS_UB40_BUTTON_X && x<MS_UB40_BUTTON_X+16
		&& y>MS_UB40_BUTTON_Y && y<MS_UB40_BUTTON_Y+16)
	{
		if (button==MOUSE_RIGHTBUTTON)
                {
                        help_flag=1;
                        activate_help("msb-ub40.hlp");
                        return;
                }
		draw_mini_screen_ub40();
	}
// this is the coal reserve button
        else if (x>MS_COAL_BUTTON_X && x<MS_COAL_BUTTON_X+16
                && y>MS_COAL_BUTTON_Y && y<MS_COAL_BUTTON_Y+16)
	{
		if (button==MOUSE_RIGHTBUTTON)
                {
                        help_flag=1;
                        activate_help("msb-coal.hlp");
                        return;
                }
		if (coal_survey_done==0)
		{
			if (yn_dial_box("Coal survey"
				,"This will cost you 1 million"
				,"After that it's is free to call again"
				,"Do coal survey?")!=0)
			{
				total_money-=1000000;
				print_total_money();
                		draw_mini_screen_coal();
				coal_survey_done=1;
			}
			return;
		}
		else
			draw_mini_screen_coal();
	}
// this is the starving button
        else if (x>MS_STARVE_BUTTON_X && x<MS_STARVE_BUTTON_X+16
                && y>MS_STARVE_BUTTON_Y && y<MS_STARVE_BUTTON_Y+16)
	{
		if (button==MOUSE_RIGHTBUTTON)
                {
                        help_flag=1;
                        activate_help("msb-starve.hlp");
                        return;
                }
                draw_mini_screen_starve();
	}
// this is the other costs button
        else if (x>MS_OCOST_BUTTON_X && x<MS_OCOST_BUTTON_X+16
                && y>MS_OCOST_BUTTON_Y && y<MS_OCOST_BUTTON_Y+16)
	{
		if (button==MOUSE_RIGHTBUTTON)
                {
                        help_flag=1;
                        activate_help("msb-money.hlp");
                        return;
                }
                draw_mini_screen_ocost();
	}

// this is the mini screen power button
        else if (x>MS_POWER_BUTTON_X && x<MS_POWER_BUTTON_X+16
                && y>MS_POWER_BUTTON_Y && y<MS_POWER_BUTTON_Y+16)
        {
                if (button==MOUSE_RIGHTBUTTON)
                {
                        help_flag=1;
                        activate_help("msb-power.hlp");
                        return;
                }
                draw_mini_screen_power();
        }


// this is the help button
	else if (x>HELP_BUTTON_X && x<HELP_BUTTON_X+HELP_BUTTON_W
		&& y>HELP_BUTTON_Y && y<HELP_BUTTON_Y+HELP_BUTTON_H)
		activate_help("index.hlp");


// this is the save button
	else if (x>SAVE_BUTTON_X && x<SAVE_BUTTON_X+SAVE_BUTTON_W
		&& y>SAVE_BUTTON_Y && y<SAVE_BUTTON_Y+SAVE_BUTTON_H)
	{
		if (load_flag==0)
			save_flag=1;
	}

// this is the results (stats) button
	else if (x>RESULTSBUT_X && x<RESULTSBUT_X+RESULTSBUT_W
		&& y>RESULTSBUT_Y && y<RESULTSBUT_Y+RESULTSBUT_H)
		window_results();

// this is the overwrite transport button button
        else if (x>TOVERBUT_X && x<TOVERBUT_X+TOVERBUT_W
                && y>TOVERBUT_Y && y<TOVERBUT_Y+TOVERBUT_H)
	{
                if (button==MOUSE_RIGHTBUTTON)
                {
                        help_flag=1;
                        activate_help("tover.hlp");
                        return;
                }
		if (overwrite_transport_flag)
		{
			hide_mouse();
			Fgl_putbox(TOVERBUT_X,TOVERBUT_Y
				,16,16,toveroff_button1);
			Fgl_putbox(TOVERBUT_X+16,TOVERBUT_Y
				,16,16,toveroff_button2);
			redraw_mouse();
			overwrite_transport_flag=0;
		}
		else
		{
			hide_mouse();
			Fgl_putbox(TOVERBUT_X,TOVERBUT_Y
				,16,16,toveron_button1);
			Fgl_putbox(TOVERBUT_X+16,TOVERBUT_Y
				,16,16,toveron_button2);
			redraw_mouse();
			overwrite_transport_flag=1;
		}
	}
#ifdef LC_X11
// this is the confine mouse pointer button
	else if (x>CONFINEBUT_X && x<CONFINEBUT_X+16
		&& y>CONFINEBUT_Y && y<CONFINEBUT_Y+16)
	{
                if (button==MOUSE_RIGHTBUTTON)
                {
                        help_flag=1;
                        activate_help("confinep.hlp");
                        return;
                }
		if (confine_flag)
		{
			XUngrabPointer(display.dpy,CurrentTime);
			confine_flag=0;
			Fgl_putbox(CONFINEBUT_X,CONFINEBUT_Y
				,16,16,unconfine_button);
		}
		else
		{
			XGrabPointer(display.dpy,display.win,0,
			ButtonPressMask|ButtonReleaseMask|PointerMotionMask,
			GrabModeAsync,
			GrabModeAsync,display.win,None,CurrentTime);
			confine_flag=1;
			Fgl_putbox(CONFINEBUT_X,CONFINEBUT_Y
				,16,16,confine_button);
		}
	}
#endif

// this is the slow button
        else if (x>SLOW_BUTTON_X && x<SLOW_BUTTON_X+SLOW_BUTTON_W
                && y>SLOW_BUTTON_Y && y<SLOW_BUTTON_Y+SLOW_BUTTON_H)
	{
		if (button==MOUSE_RIGHTBUTTON)
                {
                        help_flag=1;
                        activate_help("slow.hlp");
                        return;
                }
                select_slow();
	}

// this is the medium button
        else if (x>MED_BUTTON_X && x<MED_BUTTON_X+MED_BUTTON_W
                && y>MED_BUTTON_Y && y<MED_BUTTON_Y+MED_BUTTON_H)
	{
		if (button==MOUSE_RIGHTBUTTON)
                {
                        help_flag=1;
                        activate_help("medium.hlp");
                        return;
                }
                select_medium();
	}

// this is the fast button
        else if (x>FAST_BUTTON_X && x<FAST_BUTTON_X+FAST_BUTTON_W
                && y>FAST_BUTTON_Y && y<FAST_BUTTON_Y+FAST_BUTTON_H)
	{
		if (button==MOUSE_RIGHTBUTTON)
                {
                        help_flag=1;
                        activate_help("fast.hlp");
                        return;
                }
                select_fast();
	}


// no more buttons to click on, see if it's help for somewhere else.
// ***************************
	if (button!=MOUSE_RIGHTBUTTON)
		return;
// check for help with pbars
	if (x>=PBAR_POP_X && x<=(PBAR_POP_X+PBAR_SIZE_X)
		&& y>=PBAR_POP_Y && y<=(PBAR_POP_Y+PBAR_SIZE_Y))
	{
		help_flag=1;
		activate_help("pbar-pop.hlp");
		return;
	}
        if (x>=PBAR_TECH_X && x<=(PBAR_TECH_X+PBAR_SIZE_X)
                && y>=PBAR_TECH_Y && y<=(PBAR_TECH_Y+PBAR_SIZE_Y))
        {
                help_flag=1;
                activate_help("pbar-tech.hlp");
                return;
        }
        if (x>=PBAR_FOOD_X && x<=(PBAR_FOOD_X+PBAR_SIZE_X)
                && y>=PBAR_FOOD_Y && y<=(PBAR_FOOD_Y+PBAR_SIZE_Y))
        {
                help_flag=1;
                activate_help("pbar-food.hlp");
                return;
        }
        if (x>=PBAR_JOBS_X && x<=(PBAR_JOBS_X+PBAR_SIZE_X)
                && y>=PBAR_JOBS_Y && y<=(PBAR_JOBS_Y+PBAR_SIZE_Y))
        {
                help_flag=1;
                activate_help("pbar-jobs.hlp");
                return;
        }
        if (x>=PBAR_MONEY_X && x<=(PBAR_MONEY_X+PBAR_SIZE_X)
                && y>=PBAR_MONEY_Y && y<=(PBAR_MONEY_Y+PBAR_SIZE_Y))
        {
                help_flag=1;
                activate_help("pbar-money.hlp");
                return;
        }
        if (x>=PBAR_COAL_X && x<=(PBAR_COAL_X+PBAR_SIZE_X)
                && y>=PBAR_COAL_Y && y<=(PBAR_COAL_Y+PBAR_SIZE_Y))
        {
                help_flag=1;
                activate_help("pbar-coal.hlp");
                return;
        }
        if (x>=PBAR_GOODS_X && x<=(PBAR_GOODS_X+PBAR_SIZE_X)
                && y>=PBAR_GOODS_Y && y<=(PBAR_GOODS_Y+PBAR_SIZE_Y))
        {
                help_flag=1;
                activate_help("pbar-goods.hlp");
                return;
        }
        if (x>=PBAR_ORE_X && x<=(PBAR_ORE_X+PBAR_SIZE_X)
                && y>=PBAR_ORE_Y && y<=(PBAR_ORE_Y+PBAR_SIZE_Y))
        {
                help_flag=1;
                activate_help("pbar-ore.hlp");
                return;
        }
        if (x>=PBAR_STEEL_X && x<=(PBAR_STEEL_X+PBAR_SIZE_X)
                && y>=PBAR_STEEL_Y && y<=(PBAR_STEEL_Y+PBAR_SIZE_Y))
        {
                help_flag=1;
                activate_help("pbar-steel.hlp");
                return;
        }
// now check for finance window
	if (x>=FINANCE_X && x<=(FINANCE_X+FINANCE_W)
		&& y>=FINANCE_Y && y<=(FINANCE_Y+FINANCE_H))
        {
                help_flag=1;
                activate_help("finance.hlp");
                return;
        }
// now check for economy window (monthgraph)
	if (x>=MONTHGRAPH_X && x<=(MONTHGRAPH_X+MONTHGRAPH_W)
		&& y>=MONTHGRAPH_Y && y<=(MONTHGRAPH_Y+MONTHGRAPH_H))
	{
		help_flag=1;
		activate_help("economy.hlp");
		return;
	}
// now check for the mappoint stats window
	if (x>=MAPPOINT_STATS_X && x<=(MAPPOINT_STATS_X+MAPPOINT_STATS_W)
		&& y>MAPPOINT_STATS_Y
		&& y<=(MAPPOINT_STATS_Y+MAPPOINT_STATS_H))
	{
		help_flag=1;
		activate_help("infowin.hlp");
		return;
	}
// now check for the stats window
        if (x>=STATS_X && x<=(STATS_X+STATS_W)
                && y>STATS_Y && y<=(STATS_Y+STATS_H))
        {
                help_flag=1;
                activate_help("stats.hlp");
                return;
        }
// now check for the sustain window
        if (x>=SUST_SCREEN_X && x<=(SUST_SCREEN_X+SUST_SCREEN_W)
                && y>SUST_SCREEN_Y && y<=(SUST_SCREEN_Y+SUST_SCREEN_H))
        {
                help_flag=1;
                activate_help("sustain.hlp");
                return;
        }

}


void bulldoze_area(int x,int y)
{
	int xx,yy,g;
        if (mappointtype[x][y]==CST_USED)
        {
                xx=mappoint[x][y].int_1;
                yy=mappoint[x][y].int_2;
        }
	else
	{
		xx=x;
		yy=y;
	}
	g=main_types[mappointtype[xx][yy]].group;
	if (g<7)
		g--;   // translate into button type
	if (xx!=last_built_x || yy!=last_built_y)
	{
		if (g==GROUP_MONUMENT && monument_bul_flag==0)
		{
			if (yn_dial_box("WARNING"
				,"Bulldozing a monument costs"
				,"a lot of money.","Want to buldoze?")==0)
				return;
			monument_bul_flag=1;
		}
		else if (g==GROUP_RIVER && river_bul_flag==0)
		{
			if (yn_dial_box("WARNING"
				,"Bulldozing a section of river"
				,"costs a lot of money."
				,"Want to buldoze?")==0)
				return;
			river_bul_flag=1;
		}
                else if (g==GROUP_SHANTY)
                {
                        if (yn_dial_box("WARNING"
                                ,"Bulldozing a shanty town costs a"
                                ,"lot of money and may cause a fire."
                                ,"Want to buldoze?")==0)
                                return;
                        fire_area(xx,yy);
// this is done by fire_area	numof_shanties--;
			total_money-=GROUP_SHANTY_BUL_COST;
			print_total_money();
			return;
                }

		else if (g==GROUP_FIRE)  // just put it out
		{
			if (mappoint[xx][yy].int_2>=FIRE_LENGTH)
				return;
			mappoint[xx][yy].int_2=FIRE_LENGTH+1;
			mappointtype[xx][yy]=CST_FIRE_DONE1;
			total_money-=GROUP_BURNT_BUL_COST;
			print_total_money();
			return;
		}
		else if (g==GROUP_TIP)
		{
			ok_dial_box("nobull-tip.mes",BAD,0L);
			return;
		}
//printf("Bull cost %d g=%d sbut[g]=%d\n"
//	,select_button_bul_cost[sbut[g]],g,sbut[g]);
	        total_money-=select_button_bul_cost[sbut[g]];
	        print_total_money();
	}
	else   // it was a mistake - so give the money back.
	{
		total_money+=select_button_cost[sbut[g]];
		print_total_money();
	}
// this is to stop being able to build something, bulldozing other things
// then being able to bulldoze the last thing you built for free.
	last_built_x=-1;
	last_built_y=-1;
	do_bulldoze_area(CST_GREEN,x,y);
	if (g==GROUP_OREMINE)
	{
		int i,j;
		for (j=0;j<4;j++)
		for (i=0;i<4;i++)
			if (mappoint[xx+i][yy+j].ore_reserve<ORE_RESERVE/2)
				do_bulldoze_area(CST_WATER,xx+i,yy+j);
	}
}

void fire_area(int x,int y)
{
	do_bulldoze_area(CST_FIRE_1,x,y);
	refresh_main_screen();  // update transport or we get stuff put in
				// the area from connected tracks etc.
}


void do_bulldoze_area(int fill,int xx,int yy)
{
	int size,x,y;
	if (mappointtype[xx][yy]==CST_USED)
	{
		x=mappoint[xx][yy].int_1;
		y=mappoint[xx][yy].int_2;
	}
	else
	{
		x=xx;
		y=yy;
	}
	size=main_types[mappointtype[x][y]].size;
	if (main_types[mappointtype[x][y]].group==GROUP_SUBSTATION
		|| main_types[mappointtype[x][y]].group==GROUP_WINDMILL)
                 remove_a_substation(x,y);
        else if (main_types[mappointtype[x][y]].group==GROUP_MARKET)
                 remove_a_market(x,y);
	else if (main_types[mappointtype[x][y]].group==GROUP_SHANTY)
		numof_shanties--;
	else if (main_types[mappointtype[x][y]].group==GROUP_COMMUNE)
		numof_communes--;

	people_pool+=mappoint[x][y].population;
	clear_mappoint(fill,x,y);
	if (size>1) // do size 2
	{
		clear_mappoint(fill,x+1,y);
		clear_mappoint(fill,x,y+1);
		clear_mappoint(fill,x+1,y+1);
	}
	if (size>2) // do size 3
	{
		clear_mappoint(fill,x+2,y);
		clear_mappoint(fill,x+2,y+1);
		clear_mappoint(fill,x+2,y+2);
		clear_mappoint(fill,x,y+2);
		clear_mappoint(fill,x+1,y+2);
	}
	if (size>3) // do size 4
	{
		clear_mappoint(fill,x+3,y);
		clear_mappoint(fill,x+3,y+1);
		clear_mappoint(fill,x+3,y+2);
		clear_mappoint(fill,x+3,y+3);
		clear_mappoint(fill,x,y+3);
		clear_mappoint(fill,x+1,y+3);
		clear_mappoint(fill,x+2,y+3);
	}
}

void clear_mappoint(int fill,int x,int y)
{
        mappointtype[x][y]=fill;
        mappoint[x][y].population=0;
	mappoint[x][y].flags=0;
        mappoint[x][y].int_1=0;
        mappoint[x][y].int_2=0;
        mappoint[x][y].int_3=0;
	mappoint[x][y].int_4=0;
	mappoint[x][y].int_5=0;
	mappoint[x][y].int_6=0;
	mappoint[x][y].int_7=0;
}

void do_market_cb_mouse(int x,int y)
{
	hide_mouse();
//	if (x>=MARKET_CB_X+2*8 && x<=MARKET_CB_X+4*8) // buy col
	if (x>=MARKET_CB_X+2*8 && x<=MARKET_CB_X+16*8) // buy col
	{
		if (y>=MARKET_CB_Y+3*8+4 && y<=MARKET_CB_Y+5*8+4) // food
		{
			mappoint[mcbx][mcby].flags ^= FLAG_MB_FOOD;
		        if ((mappoint[mcbx][mcby].flags & FLAG_MB_FOOD)!=0)
                	Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+3*8+4,16,16
                        	,checked_box_graphic);
        		else
                	Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+3*8+4,16,16
                        	,unchecked_box_graphic);
		}
		else if (y>=MARKET_CB_Y+6*8+4 && y<=MARKET_CB_Y+8*8+4) // jobs
		{ 
			mappoint[mcbx][mcby].flags ^= FLAG_MB_JOBS;
       			if ((mappoint[mcbx][mcby].flags & FLAG_MB_JOBS)!=0)
                		Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+6*8+4
                	        	,16,16,checked_box_graphic);
        		else
                		Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+6*8+4
                        		,16,16,unchecked_box_graphic);
		}
		else if (y>=MARKET_CB_Y+9*8+4 && y<=MARKET_CB_Y+11*8+4) //coal
		{
			mappoint[mcbx][mcby].flags ^= FLAG_MB_COAL;
		        if ((mappoint[mcbx][mcby].flags & FLAG_MB_COAL)!=0)
		                Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+9*8+4
                        		,16,16,checked_box_graphic);
 		        else
                		Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+9*8+4
                        		,16,16,unchecked_box_graphic);
		}
		else if (y>=MARKET_CB_Y+12*8+4 && y<=MARKET_CB_Y+14*8+4) //ore
		{
			mappoint[mcbx][mcby].flags ^= FLAG_MB_ORE;
		        if ((mappoint[mcbx][mcby].flags & FLAG_MB_ORE)!=0)
		                Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+12*8+4
		                        ,16,16,checked_box_graphic);
		        else
		                Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+12*8+4
		                        ,16,16,unchecked_box_graphic);
		}
		else if (y>=MARKET_CB_Y+15*8+4 && y<=MARKET_CB_Y+17*8+4) //goods
		{
			mappoint[mcbx][mcby].flags ^= FLAG_MB_GOODS;
		        if ((mappoint[mcbx][mcby].flags & FLAG_MB_GOODS)!=0)
		                Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+15*8+4
		                        ,16,16,checked_box_graphic);
		        else
		                Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+15*8+4
		                        ,16,16,unchecked_box_graphic);
		}
		else if (y>=MARKET_CB_Y+18*8+4 && y<=MARKET_CB_Y+20*8+4) //steel
		{
			mappoint[mcbx][mcby].flags ^= FLAG_MB_STEEL;
		        if ((mappoint[mcbx][mcby].flags & FLAG_MB_STEEL)!=0)
		                Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+18*8+4
		                        ,16,16,checked_box_graphic);
		        else
		                Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+18*8+4
		                        ,16,16,unchecked_box_graphic);
		}
	}
//	else if (x>=MARKET_CB_X+14*8 && x<=MARKET_CB_X+16*8) // sell col
	if (x>=MARKET_CB_X+2*8 && x<=MARKET_CB_X+16*8) // sell col
	{
		if (y>=MARKET_CB_Y+3*8+4 && y<=MARKET_CB_Y+5*8+4) // food
		{
			mappoint[mcbx][mcby].flags ^= FLAG_MS_FOOD;
		        if ((mappoint[mcbx][mcby].flags & FLAG_MS_FOOD)!=0)
                	Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+3*8+4,16,16
                        	,checked_box_graphic);
        		else
                	Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+3*8+4,16,16
                        	,unchecked_box_graphic);
		}
		else if (y>=MARKET_CB_Y+6*8+4 && y<=MARKET_CB_Y+8*8+4) // jobs
		{ 
			mappoint[mcbx][mcby].flags ^= FLAG_MS_JOBS;
       			if ((mappoint[mcbx][mcby].flags & FLAG_MS_JOBS)!=0)
                		Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+6*8+4
                	        	,16,16,checked_box_graphic);
        		else
                		Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+6*8+4
                        		,16,16,unchecked_box_graphic);
		}
		else if (y>=MARKET_CB_Y+9*8+4 && y<=MARKET_CB_Y+11*8+4) //coal
		{
			mappoint[mcbx][mcby].flags ^= FLAG_MS_COAL;
		        if ((mappoint[mcbx][mcby].flags & FLAG_MS_COAL)!=0)
		                Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+9*8+4
                        		,16,16,checked_box_graphic);
 		        else
                		Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+9*8+4
                        		,16,16,unchecked_box_graphic);
		}
		else if (y>=MARKET_CB_Y+12*8+4 && y<=MARKET_CB_Y+14*8+4) //ore
		{
			mappoint[mcbx][mcby].flags ^= FLAG_MS_ORE;
		        if ((mappoint[mcbx][mcby].flags & FLAG_MS_ORE)!=0)
		                Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+12*8+4
		                        ,16,16,checked_box_graphic);
		        else
		                Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+12*8+4
		                        ,16,16,unchecked_box_graphic);
		}
		else if (y>=MARKET_CB_Y+15*8+4 && y<=MARKET_CB_Y+17*8+4) //goods
		{
			mappoint[mcbx][mcby].flags ^= FLAG_MS_GOODS;
		        if ((mappoint[mcbx][mcby].flags & FLAG_MS_GOODS)!=0)
		                Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+15*8+4
		                        ,16,16,checked_box_graphic);
		        else
		                Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+15*8+4
		                        ,16,16,unchecked_box_graphic);
		}
		else if (y>=MARKET_CB_Y+18*8+4 && y<=MARKET_CB_Y+20*8+4) //steel
		{
			mappoint[mcbx][mcby].flags ^= FLAG_MS_STEEL;
		        if ((mappoint[mcbx][mcby].flags & FLAG_MS_STEEL)!=0)
		                Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+18*8+4
		                        ,16,16,checked_box_graphic);
		        else
		                Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+18*8+4
		                        ,16,16,unchecked_box_graphic);
		}
	}
	else
		close_market_cb();
	redraw_mouse();
}


void do_port_cb_mouse(int x,int y)
{
	hide_mouse();
	if (x>=MARKET_CB_X+2*8 && x<=MARKET_CB_X+4*8) // buy col
	{
                if (y>=MARKET_CB_Y+6*8+4 && y<=MARKET_CB_Y+8*8+4) // food
                {
                        mappoint[mcbx][mcby].flags ^= FLAG_MB_FOOD;
                        if ((mappoint[mcbx][mcby].flags & FLAG_MB_FOOD)!=0)
                        Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+6*8+4,16,16
                                ,checked_box_graphic);
                        else
                        Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+6*8+4,16,16
                                ,unchecked_box_graphic);
                }

		else if (y>=MARKET_CB_Y+9*8+4 && y<=MARKET_CB_Y+11*8+4) //coal
		{
			mappoint[mcbx][mcby].flags ^= FLAG_MB_COAL;
		        if ((mappoint[mcbx][mcby].flags & FLAG_MB_COAL)!=0)
		                Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+9*8+4
                        		,16,16,checked_box_graphic);
 		        else
                		Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+9*8+4
                        		,16,16,unchecked_box_graphic);
		}
		else if (y>=MARKET_CB_Y+12*8+4 && y<=MARKET_CB_Y+14*8+4) //ore
		{
			mappoint[mcbx][mcby].flags ^= FLAG_MB_ORE;
		        if ((mappoint[mcbx][mcby].flags & FLAG_MB_ORE)!=0)
		                Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+12*8+4
		                        ,16,16,checked_box_graphic);
		        else
		                Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+12*8+4
		                        ,16,16,unchecked_box_graphic);
		}
		else if (y>=MARKET_CB_Y+15*8+4 && y<=MARKET_CB_Y+17*8+4) //goods
		{
			mappoint[mcbx][mcby].flags ^= FLAG_MB_GOODS;
		        if ((mappoint[mcbx][mcby].flags & FLAG_MB_GOODS)!=0)
		                Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+15*8+4
		                        ,16,16,checked_box_graphic);
		        else
		                Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+15*8+4
		                        ,16,16,unchecked_box_graphic);
		}
		else if (y>=MARKET_CB_Y+18*8+4 && y<=MARKET_CB_Y+20*8+4) //steel
		{
			mappoint[mcbx][mcby].flags ^= FLAG_MB_STEEL;
		        if ((mappoint[mcbx][mcby].flags & FLAG_MB_STEEL)!=0)
		                Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+18*8+4
		                        ,16,16,checked_box_graphic);
		        else
		                Fgl_putbox(MARKET_CB_X+2*8,MARKET_CB_Y+18*8+4
		                        ,16,16,unchecked_box_graphic);
		}
	}
	else if (x>=MARKET_CB_X+14*8 && x<=MARKET_CB_X+16*8) // sell col
	{
                if (y>=MARKET_CB_Y+6*8+4 && y<=MARKET_CB_Y+8*8+4) // food
                {
                        mappoint[mcbx][mcby].flags ^= FLAG_MS_FOOD;
                        if ((mappoint[mcbx][mcby].flags & FLAG_MS_FOOD)!=0)
                        Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+6*8+4,16,16
                                ,checked_box_graphic);
                        else
                        Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+6*8+4,16,16
                                ,unchecked_box_graphic);
                }
		else if (y>=MARKET_CB_Y+9*8+4 && y<=MARKET_CB_Y+11*8+4) //coal
		{
			mappoint[mcbx][mcby].flags ^= FLAG_MS_COAL;
		        if ((mappoint[mcbx][mcby].flags & FLAG_MS_COAL)!=0)
		                Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+9*8+4
                        		,16,16,checked_box_graphic);
 		        else
                		Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+9*8+4
                        		,16,16,unchecked_box_graphic);
		}
		else if (y>=MARKET_CB_Y+12*8+4 && y<=MARKET_CB_Y+14*8+4) //ore
		{
			mappoint[mcbx][mcby].flags ^= FLAG_MS_ORE;
		        if ((mappoint[mcbx][mcby].flags & FLAG_MS_ORE)!=0)
		                Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+12*8+4
		                        ,16,16,checked_box_graphic);
		        else
		                Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+12*8+4
		                        ,16,16,unchecked_box_graphic);
		}
		else if (y>=MARKET_CB_Y+15*8+4 && y<=MARKET_CB_Y+17*8+4) //goods
		{
			mappoint[mcbx][mcby].flags ^= FLAG_MS_GOODS;
		        if ((mappoint[mcbx][mcby].flags & FLAG_MS_GOODS)!=0)
		                Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+15*8+4
		                        ,16,16,checked_box_graphic);
		        else
		                Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+15*8+4
		                        ,16,16,unchecked_box_graphic);
		}
		else if (y>=MARKET_CB_Y+18*8+4 && y<=MARKET_CB_Y+20*8+4) //steel
		{
			mappoint[mcbx][mcby].flags ^= FLAG_MS_STEEL;
		        if ((mappoint[mcbx][mcby].flags & FLAG_MS_STEEL)!=0)
		                Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+18*8+4
		                        ,16,16,checked_box_graphic);
		        else
		                Fgl_putbox(MARKET_CB_X+14*8,MARKET_CB_Y+18*8+4
		                        ,16,16,unchecked_box_graphic);
		}
	}
	else
		close_port_cb();
	redraw_mouse();
}


void do_db_mouse(int x,int y)
{
	if (x>db_yesbox_x1 && x<db_yesbox_x2
		&& y>db_yesbox_y1 && y<db_yesbox_y2)
		db_yesbox_clicked=1;
	else if (x>db_nobox_x1 && x<db_nobox_x2
		&& y>db_nobox_y1 && y<db_nobox_y2)
		db_nobox_clicked=1;
}

void do_db_okmouse(int x,int y)
{
        if (x>db_okbox_x1 && x<db_okbox_x2
                && y>db_okbox_y1 && y<db_okbox_y2)
                db_okbox_clicked=1;

}

int no_credit_build(void)
{
	if (total_money>=0)
		return(0);
#ifdef GROUP_POWER_SOURCE_NO_CREDIT
	if (main_types[selected_type].group==GROUP_POWER_SOURCE)
	{
		ok_dial_box("no-credit-solar-power.mes",BAD,0L);
		return(1);
	}
#endif
#ifdef GROUP_UNIVERSITY_NO_CREDIT
	if (main_types[selected_type].group==GROUP_UNIVERSITY)
	{
		ok_dial_box("no-credit-university.mes",BAD,0L);
		return(1);
	}
#endif
#ifdef GROUP_PARKLAND_NO_CREDIT
	if (main_types[selected_type].group==GROUP_PARKLAND)
	{
		ok_dial_box("no-credit-parkland.mes",BAD,0L);
		return(1);
	}
#endif
#ifdef GROUP_RECYCLE_NO_CREDIT
	if (main_types[selected_type].group==GROUP_RECYCLE)
	{
		ok_dial_box("no-credit-recycle.mes",BAD,0L);
		return(1);
	}
#endif
#ifdef GROUP_ROCKET
	if (main_types[selected_type].group==GROUP_ROCKET)
	{
		ok_dial_box("no-credit-rocket.mes",BAD,0L);
		return(1);
	}
#endif
	return(0);
}

void connect_rivers(void)
{
	int x,y,count;
	count=1;
	while (count>0)
	{
		count=0;
		for (y=0;y<WORLD_SIDE_LEN;y++)
		for (x=0;x<WORLD_SIDE_LEN;x++)
		{
			if (is_real_river(x,y)==1)
			{
				if (is_real_river(x-1,y)==-1)
				{
					mappoint[x-1][y].flags|=FLAG_IS_RIVER;
					count++;
				}
				if (is_real_river(x,y-1)==-1)
				{
					mappoint[x][y-1].flags|=FLAG_IS_RIVER;
					count++;
				}
				if (is_real_river(x+1,y)==-1)
				{
					mappoint[x+1][y].flags|=FLAG_IS_RIVER;
					count++;
				}
				if (is_real_river(x,y+1)==-1)
				{
					mappoint[x][y+1].flags|=FLAG_IS_RIVER;
					count++;
				}
			}
		}
	}
}

int is_real_river(int x,int y)
{
// returns zero if not water at all or if out of bounds.
	if (x<0 || x>=WORLD_SIDE_LEN || y<0 || y>=WORLD_SIDE_LEN)
		return(0);
	if (main_types[mappointtype[x][y]].group !=GROUP_WATER)
		return(0);
	if (mappoint[x][y].flags & FLAG_IS_RIVER)
		return(1);
	return(-1);
}

void select_fast(void)
{
	if (pause_flag)
		select_pause();
	slow_flag=0;
	hide_mouse();
	Fgl_putbox(SLOW_BUTTON_X,SLOW_BUTTON_Y,16,16,slow_button1_off);
	Fgl_putbox(SLOW_BUTTON_X+16,SLOW_BUTTON_Y,16,16,slow_button2_off);
	med_flag=0;
	Fgl_putbox(MED_BUTTON_X,MED_BUTTON_Y,16,16,med_button1_off);
	Fgl_putbox(MED_BUTTON_X+16,MED_BUTTON_Y,16,16,med_button2_off);

	fast_flag=1;
	Fgl_putbox(FAST_BUTTON_X,FAST_BUTTON_Y,16,16,fast_button1_on);
	Fgl_putbox(FAST_BUTTON_X+16,FAST_BUTTON_Y,16,16,fast_button2_on);
	redraw_mouse();
}

void select_medium(void)
{
	if (pause_flag)
		select_pause();
	slow_flag=0;
	hide_mouse();
	Fgl_putbox(SLOW_BUTTON_X,SLOW_BUTTON_Y,16,16,slow_button1_off);
	Fgl_putbox(SLOW_BUTTON_X+16,SLOW_BUTTON_Y,16,16,slow_button2_off);

	fast_flag=0;
	Fgl_putbox(FAST_BUTTON_X,FAST_BUTTON_Y,16,16,fast_button1_off);
	Fgl_putbox(FAST_BUTTON_X+16,FAST_BUTTON_Y,16,16,fast_button2_off);
	med_flag=1;
	Fgl_putbox(MED_BUTTON_X,MED_BUTTON_Y,16,16,med_button1_on);
	Fgl_putbox(MED_BUTTON_X+16,MED_BUTTON_Y,16,16,med_button2_on);
	redraw_mouse();
}

void select_slow(void)
{
	if (pause_flag)
		select_pause();
	med_flag=0;
	hide_mouse();
	Fgl_putbox(MED_BUTTON_X,MED_BUTTON_Y,16,16,med_button1_off);
	Fgl_putbox(MED_BUTTON_X+16,MED_BUTTON_Y,16,16,med_button2_off);

	fast_flag=0;
	Fgl_putbox(FAST_BUTTON_X,FAST_BUTTON_Y,16,16,fast_button1_off);
	Fgl_putbox(FAST_BUTTON_X+16,FAST_BUTTON_Y,16,16,fast_button2_off);
	slow_flag=1;
	Fgl_putbox(SLOW_BUTTON_X,SLOW_BUTTON_Y,16,16,slow_button1_on);
	Fgl_putbox(SLOW_BUTTON_X+16,SLOW_BUTTON_Y,16,16,slow_button2_on);
	redraw_mouse();
}

void select_pause(void)
{
	if (pause_flag)
	{   // unpause it
		pause_flag=0;
		hide_mouse();
		Fgl_putbox(PAUSE_BUTTON_X,PAUSE_BUTTON_Y
			,16,16,pause_button1_off);
		Fgl_putbox(PAUSE_BUTTON_X+16,PAUSE_BUTTON_Y
			,16,16,pause_button2_off);
		redraw_mouse();
		if (fast_flag)
			select_fast();
		else if (med_flag)
			select_medium();
		else if (slow_flag)
			select_slow();
		else
			select_medium();
	}
	else
	{
		pause_flag=1;
		hide_mouse();
		Fgl_putbox(FAST_BUTTON_X,FAST_BUTTON_Y
			,16,16,fast_button1_off);
		Fgl_putbox(FAST_BUTTON_X+16,FAST_BUTTON_Y
			,16,16,fast_button2_off);
		Fgl_putbox(MED_BUTTON_X,MED_BUTTON_Y,16,16,med_button1_off);
		Fgl_putbox(MED_BUTTON_X+16,MED_BUTTON_Y
			,16,16,med_button2_off);
		Fgl_putbox(SLOW_BUTTON_X,SLOW_BUTTON_Y
			,16,16,slow_button1_off);
		Fgl_putbox(SLOW_BUTTON_X+16,SLOW_BUTTON_Y
			,16,16,slow_button2_off);
		Fgl_putbox(PAUSE_BUTTON_X,PAUSE_BUTTON_Y
			,16,16,pause_button1_on);
		Fgl_putbox(PAUSE_BUTTON_X+16,PAUSE_BUTTON_Y
			,16,16,pause_button2_on);
		redraw_mouse();
	}
}

void choose_residence(void)
{
	int cost;
	char s[256];
	FILE *tempfile;
        strcpy(s,getenv("HOME"));
        strcat(s,"/");
        strcat(s,LC_SAVE_DIR);
        strcat(s,"/res.tmp");
	if ((tempfile=fopen(s,"w"))==0)
		do_error("Can't open $HOME/LC_SAVE_DIR/res.tmp");
	fprintf(tempfile,
		"text -1 20 Choose the type of residence you want\n");
	fprintf(tempfile,
		"text -1 30 =====================================\n");
	fprintf(tempfile,
		"text -1 45 (LB=Low Birthrate HB=High Birthrate)\n");
	fprintf(tempfile,
		"text -1 55 (LD=Low Deathrate HD=High Deathrate)\n");
	fprintf(tempfile,"text -1 85 Low Tech\n");

        cost=(int)((float)GROUP_RESIDENCE_COST1
                +((float)GROUP_RESIDENCE_COST1
                *((float)GROUP_RESIDENCE_COST_MUL
                *(float)tech_level)/(float)MAX_TECH_LEVEL));
	fprintf(tempfile,"text 68 106 Cost %4d\n",cost);
	fprintf(tempfile,"icon 85 120 reslowlow.csi\n");
	fprintf(tempfile,"button 83 118 52 52 return1\n");
	fprintf(tempfile,"tbutton 82 180 return1 pop 50\n");
	fprintf(tempfile,"text 89 195 LB HD\n");

        cost=(int)((float)GROUP_RESIDENCE_COST2
                +((float)GROUP_RESIDENCE_COST2
                *((float)GROUP_RESIDENCE_COST_MUL
                *(float)tech_level)/(float)MAX_TECH_LEVEL));
        fprintf(tempfile,"text 155 106 Cost %4d\n",cost);
	fprintf(tempfile,"icon 170 120 resmedlow.csi\n");
	fprintf(tempfile,"button 168 118 52 52 return2\n");
	fprintf(tempfile,"tbutton 164 180 return2 pop 100\n");
	fprintf(tempfile,"text 175 195 HB LD\n");

        cost=(int)((float)GROUP_RESIDENCE_COST3
                +((float)GROUP_RESIDENCE_COST3
                *((float)GROUP_RESIDENCE_COST_MUL
                *(float)tech_level)/(float)MAX_TECH_LEVEL));
        fprintf(tempfile,"text 238 106 Cost %4d\n",cost);
	fprintf(tempfile,"icon 255 120 reshilow.csi\n");
	fprintf(tempfile,"button 253 118 52 52 return3\n");
	fprintf(tempfile,"tbutton 250 180 return3 pop 200\n");
	fprintf(tempfile,"text 261 195 HB HD\n");

	fprintf(tempfile,"text -1 215 Click on one to select\n");
	fprintf(tempfile,"text -1 225 ======================\n");

	if (((tech_level*10)/MAX_TECH_LEVEL)>2)
	{
		fprintf(tempfile,"text -1 255 High Tech\n");

        	cost=(int)((float)GROUP_RESIDENCE_COST4
        	        +((float)GROUP_RESIDENCE_COST4
        	        *((float)GROUP_RESIDENCE_COST_MUL
        	        *(float)tech_level)/(float)MAX_TECH_LEVEL));
        	fprintf(tempfile,"text 68 276 Cost %4d\n",cost);
	        fprintf(tempfile,"icon 85 290 reslowhi.csi\n");
	        fprintf(tempfile,"button 83 288 52 52 return4\n");
	        fprintf(tempfile,"tbutton 78 350 return4 pop 100\n");
		fprintf(tempfile,"text 89 365 LB HD\n");

        	cost=(int)((float)GROUP_RESIDENCE_COST5
        	        +((float)GROUP_RESIDENCE_COST5
        	        *((float)GROUP_RESIDENCE_COST_MUL
        	        *(float)tech_level)/(float)MAX_TECH_LEVEL));
        	fprintf(tempfile,"text 155 276 Cost %4d\n",cost);
	        fprintf(tempfile,"icon 170 290 resmedhi.csi\n");
	        fprintf(tempfile,"button 168 288 52 52 return5\n");
	        fprintf(tempfile,"tbutton 164 350 return5 pop 200\n");
		fprintf(tempfile,"text 175 365 HB LD\n");

        	cost=(int)((float)GROUP_RESIDENCE_COST6
        	        +((float)GROUP_RESIDENCE_COST6
        	        *((float)GROUP_RESIDENCE_COST_MUL
        	        *(float)tech_level)/(float)MAX_TECH_LEVEL));
        	fprintf(tempfile,"text 238 276 Cost %4d\n",cost);
	        fprintf(tempfile,"icon 255 290 reshihi.csi\n");
	       	fprintf(tempfile,"button 253 288 52 52 return6\n");
	       	fprintf(tempfile,"tbutton 250 350 return6 pop 400\n");
		fprintf(tempfile,"text 261 365 HB HD\n");
	}


	fclose(tempfile);
	block_help_exit=1;
	help_flag=1;
	activate_help("res.tmp");
}

void do_multi_transport(int x, int y, int action)
{
	int i;
printf("do_mt x=%d y=%d action=%d mt_flag=%d\n",x,y,action,mt_flag);
	switch(action)
	{
		case (MT_START):
			mt_start_posx=x; // x and y are mappoint coords
			mt_start_posy=y; // for the start.
			mt_current_posx=x;
			mt_current_posy=y;
			mt_flag=1;
			draw_main_window_box(cyan(20));
			return;
		case (MT_CONTINUE):
			if (x==mt_current_posx && y==mt_current_posy)
				return;
			mt_draw(x,y);
			mt_current_posx=x;
			mt_current_posy=y;
			return;
		case (MT_SUCCESS):
			i=mt_draw(x,y);
			total_money-=selected_type_cost*i;
			print_total_money();
			mt_flag=0;
			draw_main_window_box(green(8));
			// clear the tline array,
			mt_draw(-1,0);
			return;
		case (MT_FAIL):
			// this forces a 'no transport' situation
			mt_draw(mt_start_posx+1,mt_start_posy+1);
			mt_flag=0;
			draw_main_window_box(green(8));
			return;
	}
	printf("Bad action (%d) in multi transport\n",action);
}

int mt_draw(int x,int y)
{
	static flag=0;
	static int tlinex[30],tliney[30]; // only need 25
// printf("mt_draw x=%d y=%d\n",x,y);
	int i,j,dx,dy;
	if (flag==0)
	{
		flag=1;
		for (i=0;i<27;i++)
		{
			tlinex[i]=-1;
			tliney[i]=-1;
		}
	}
	if (x<0)  // we must clear the tline array without undrawing
	{
		for (i=0;i<27;i++)
			tlinex[i]=-1;
		return(0);
	}
// first undo the last draws
	for (i=0;i<25;i++)
	{
		if (tlinex[i]>=0)
		{
			mappointtype[tlinex[i]][tliney[i]]=CST_GREEN;
			tlinex[i]=-1;
//			tliney[i]=-1;
		}
		else
			break;
	}

// if we don't have a straight line, the following code does nothing.
// But we need it to get through to the update_main_screen at the bottom. 
// horizontal
	dx=x-mt_start_posx;
	dy=y-mt_start_posy;
	if (dy==0)
	{
		if (dx>0)
		{
			for (i=0;i<=dx;i++)
			{
				if (mappointtype[mt_start_posx+i]
					[mt_start_posy]!=CST_GREEN)
				{
					tlinex[0]=-1;
					break;
				}
				else
				{
					tlinex[i]=mt_start_posx+i;
					tliney[i]=mt_start_posy;
					tlinex[i+1]=-1;
				}
			}
		}
		if (dx<0)
		{
			j=0;
			for (i=dx;i<=0;i++,j++)
			{
				if (mappointtype[mt_start_posx+i]
					[mt_start_posy]!=CST_GREEN)
				{
					tlinex[0]=-1;
					break;
				}
				else
				{
					tlinex[j]=mt_start_posx+i;
					tliney[j]=mt_start_posy;
					tlinex[j+1]=-1;
				}
			}
		}
	}

// vertical
	else if (dx==0)
	{
		if (dy>0)
		{
			for (i=0;i<=dy;i++)
			{
				if (mappointtype[mt_start_posx]
					[mt_start_posy+i]!=CST_GREEN)
				{
					tlinex[0]=-1;
					break;
				}
				else
				{
					tlinex[i]=mt_start_posx;
					tliney[i]=mt_start_posy+i;
					tlinex[i+1]=-1;
				}
			}
		}
		if (dy<0)
		{
			j=0;
			for (i=dy;i<=0;i++,j++)
			{
				if (mappointtype[mt_start_posx]
					[mt_start_posy+i]!=CST_GREEN)
				{
					tlinex[0]=-1;
					break;
				}
				else
				{
					tlinex[j]=mt_start_posx;
					tliney[j]=mt_start_posy+i;
					tlinex[j+1]=-1;
				}
			}
		}
	}
// special case if dx==0 and dy==0
	if (dx==0 && dy==0)
	{
		tlinex[0]=mt_start_posx;
		tliney[0]=mt_start_posy;
		tlinex[1]=-1;
	}
// now draw the transport
	i=0;
	while (tlinex[i]>=0)
	{
// printf("dt %d %d    ",tlinex[i],tliney[i]);
		mappointtype[tlinex[i]][tliney[i]]=selected_type;
		i++;
	}
	refresh_main_screen();
	return(i);
}
