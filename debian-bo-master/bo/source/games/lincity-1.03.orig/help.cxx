// help.cxx      part of lin-city
// Copyright (c) I J Peters 1995,1996.  Please read the file 'COPYRIGHT'.

#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <ctype.h>
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
#include "help.h"

void activate_help(char *hp)
{
	help_flag=1;
	help_history_count=0;
	help_return_val=0;
#ifdef USE_EXPANDED_FONT
	Fgl_setwritemode(WRITEMODE_MASKED | FONT_EXPANDED);
#else
	Fgl_setfontcolors(HELPBACKGROUNDCOLOUR,TEXT_FG_COLOUR);
#endif
	draw_help_page(hp);
}

void do_help_mouse(int x,int y,int mbutton)
{
	if (x>MAIN_WIN_X && x<MAIN_WIN_X+MAIN_WIN_W
		&& y>MAIN_WIN_Y && y<MAIN_WIN_Y+MAIN_WIN_H)
		do_help_buttons(x,y);
	else if (x>HELP_BUTTON_X && x<HELP_BUTTON_X+HELP_BUTTON_W
		&& y>HELP_BUTTON_Y && y<HELP_BUTTON_Y+HELP_BUTTON_H
		&& help_history_count>1)
	{
		help_history_count-=2;
		draw_help_page(help_button_history[help_history_count]);
	}
	else if (mbutton==MOUSE_RIGHTBUTTON)
	{
		if (x>SELECT_BUTTON_WIN_X
                        && x<(SELECT_BUTTON_WIN_X+SELECT_BUTTON_WIN_W)
                        && y>SELECT_BUTTON_WIN_Y
                        && y<(SELECT_BUTTON_WIN_Y+SELECT_BUTTON_WIN_H))
			do_mouse_select_buttons(x,y,MOUSE_RIGHTBUTTON);
		else
                        do_mouse_other_buttons(x,y,mbutton);
	}
//		cs_mouse_handler(mbutton,0,0);
	else if (block_help_exit==0)
	{
		help_flag=0;
#ifdef USE_EXPANDED_FONT
		Fgl_setwritemode(WRITEMODE_OVERWRITE | FONT_EXPANDED);
#else
		Fgl_setfontcolors(TEXT_BG_COLOUR,TEXT_FG_COLOUR);
#endif
		refresh_main_screen();
	}
}

void draw_help_page(char *helppage)
{
	char s[256],ss[50];
	int i;
	FILE *inf;
	if (strncmp(helppage,"return",6)==0)
	{
		sscanf(&(helppage[6]),"%d",&help_return_val);
                help_flag=0;
#ifdef USE_EXPANDED_FONT
                Fgl_setwritemode(WRITEMODE_OVERWRITE | FONT_EXPANDED);
#else
                Fgl_setfontcolors(TEXT_BG_COLOUR,TEXT_FG_COLOUR);
#endif

// help_return_val is -1 for go_back and -2 for go_out
		if (help_return_val==-1 && help_history_count>1)
		{
			strcpy(helppage
				,help_button_history[help_history_count-2]);
			help_history_count-=2;
			help_flag=1;
			goto continue_with_help;
		}
		refresh_main_screen();
		if (help_return_val==-2)
			;  // nothing to do?
// Watch it! Digging an emu's nest here :)
		if (help_history_count>0 &&
			strcmp(help_button_history[help_history_count-1]
			,"res.tmp")==0)
		{
	        switch(help_return_val)
	        {
	case(-2):
	case(-1):
	case(0): 
//		selected_type=CST_RESIDENCE_0; 
//	        selected_type_cost=(int)((float)GROUP_RESIDENCE_COST1
//               +((float)GROUP_RESIDENCE_COST1
//                *((float)GROUP_RESIDENCE_COST_MUL
//                *(float)tech_level)/(float)MAX_TECH_LEVEL));
//		break;
	case(1): selected_type=CST_RESIDENCE_LL;
                selected_type_cost=(int)((float)GROUP_RESIDENCE_COST1
                +((float)GROUP_RESIDENCE_COST1
                *((float)GROUP_RESIDENCE_COST_MUL
                *(float)tech_level)/(float)MAX_TECH_LEVEL));
		break;
	case(2): selected_type=CST_RESIDENCE_ML;
                selected_type_cost=(int)((float)GROUP_RESIDENCE_COST2
                +((float)GROUP_RESIDENCE_COST2
                *((float)GROUP_RESIDENCE_COST_MUL
                *(float)tech_level)/(float)MAX_TECH_LEVEL));
		break;
	case(3): selected_type=CST_RESIDENCE_HL;
                selected_type_cost=(int)((float)GROUP_RESIDENCE_COST3
                +((float)GROUP_RESIDENCE_COST3
                *((float)GROUP_RESIDENCE_COST_MUL
                *(float)tech_level)/(float)MAX_TECH_LEVEL));
		break;
	case(4): selected_type=CST_RESIDENCE_LH;
                selected_type_cost=(int)((float)GROUP_RESIDENCE_COST4
                +((float)GROUP_RESIDENCE_COST4
                *((float)GROUP_RESIDENCE_COST_MUL
                *(float)tech_level)/(float)MAX_TECH_LEVEL));
		break;
	case(5): selected_type=CST_RESIDENCE_MH;
                selected_type_cost=(int)((float)GROUP_RESIDENCE_COST5
                +((float)GROUP_RESIDENCE_COST5
                *((float)GROUP_RESIDENCE_COST_MUL
                *(float)tech_level)/(float)MAX_TECH_LEVEL));
		break;
	case(6): selected_type=CST_RESIDENCE_HH;
                selected_type_cost=(int)((float)GROUP_RESIDENCE_COST6
                +((float)GROUP_RESIDENCE_COST6
                *((float)GROUP_RESIDENCE_COST_MUL
                *(float)tech_level)/(float)MAX_TECH_LEVEL));
		break;
	        }
		}
		else if (help_history_count>0 &&
			strcmp(help_button_history[help_history_count-1]
				,"opening.hlp")==0)
		{
			switch (help_return_val)
			{
				case(-2):
				case(-1):
				case(0): random_start(); break;
				case(1): break;
			}
		}
                else if (help_history_count>0 &&
                        strcmp(help_button_history[help_history_count-1]
                                ,"openload.hlp")==0)
                {
                        switch (help_return_val)
                        {
                                case(-2):
                                case(-1):
                                case(0): random_start(); break;
                                case(1): redraw_mouse();
					load_op_city("good_times.scn");
					hide_mouse(); break;
				case(2): load_op_city("bad_times.scn");
					break;
				case(9): load_flag=1; break;
                        }
                }
		block_help_exit=0;
		return;
	}
	else if (strncmp(helppage,"mini-screen.hlp",15)==0)
	{
		draw_big_mini_screen();
		goto continue_with_help2;
	}
continue_with_help:
	strcpy(ss,helppage);  // cos string is changed when page changed
	numof_help_buttons=0;
	strcpy(s,helppath);
	strcat(s,helppage);
	if ((inf=fopen(s,"r"))==0)
	{
	        strcpy(s,getenv("HOME"));
        	strcat(s,"/");
        	strcat(s,LC_SAVE_DIR);
        	strcat(s,"/");
		strcat(s,helppage);
		if ((inf=fopen(s,"r"))==0)
		{
			strcpy(s,helppath);
			strcat(s,HELPERRORPAGE);
			if ((inf=fopen(s,"r"))==0)
				do_error("Help error");
		}
	}
// background
	Fgl_fillbox(MAIN_WIN_X,MAIN_WIN_Y,MAIN_WIN_W,MAIN_WIN_H
		,HELPBACKGROUNDCOLOUR);
	while (feof(inf)==0)
	{
		if (fgets(s,250,inf)==0)
			break;
		parse_helpline(s);
	}
	fclose(inf);
continue_with_help2:
	if (help_history_count>0)
	{
		parse_helpline("tcolour 122 153");
		parse_helpline("tbutton 4 387 return-1 BACK");
	}
	parse_helpline("tcolour 188 153");
	parse_helpline("tbutton 370 387 return-2 OUT");
	parse_helpline("tcolour -1 -1");

	strcpy(help_button_history[help_history_count],ss);
	help_history_count++;
// printf("hhc=%d\n",help_history_count);
	if (help_history_count>=MAX_HELP_HISTORY)
	{
		for (i=0;i<(MAX_HELP_HISTORY-1);i++)
			strcpy(help_button_history[i]
				,help_button_history[i+1]);
		help_history_count--;
	}
}

void parse_helpline(char *s)
{
	if (strncmp(s,"text",4)==0)
		parse_textline(s);
	else if (strncmp(s,"icon",4)==0)
		parse_iconline(s);
	else if (strncmp(s,"button",6)==0)
		parse_buttonline(s);
	else if (strncmp(s,"tbutton",7)==0)
		parse_tbuttonline(s);
	else if (strncmp(s,"tcolour",7)==0)
		parse_tcolourline(s);
}

void parse_tcolourline(char *st)
{
	char s[100];
	int f,b;
	strcpy(s,st);  // hpux fix?  we can live with this.
	sscanf(s,"tcolour %d %d",&f,&b);
	if (f<0 || b<0)
		Fgl_setfontcolors(TEXT_BG_COLOUR,TEXT_FG_COLOUR);
	else
		Fgl_setfontcolors(b,f);
}

void parse_textline(char *st)
{
	int i,j,x,y;
	sscanf(st,"text %d %d",&x,&y);
// find start of string
	i=0;
	for (j=0;j<3;j++)
	{
		while (isspace(st[i])==0)
		{
			if (st[i]==0)
				return;   // just silently ignore
			i++;
		}
		while (isspace(st[i])!=0)
		{
			if (st[i]==0)
				return;
			i++;
		}
	}
	st+=i;
// get rid of the newline
        if (st[strlen(st)-1]==0xa)
                st[strlen(st)-1]=0;
// centre text if x -ve
	if (x<0)
	{
		x=(MAIN_WIN_W/2)-(strlen(st)*4);
		if (x<0)
			return;   // line was too long
	}
// check to see if text runs off the end
	if ((int)(strlen(st)*8) > (MAIN_WIN_W-x))
		return;
	Fgl_write(MAIN_WIN_X+x,MAIN_WIN_Y+y,st);
}


void parse_iconline(char *st)
{
        int i,j,x,y;
        sscanf(st,"icon %d %d",&x,&y);
// find start of string
        i=0;
        for (j=0;j<3;j++)
        {
                while (isspace(st[i])==0)
                {
                        if (st[i]==0)
                                return;   // just silently ignore
                        i++;
                }
                while (isspace(st[i])!=0)
                {
                        if (st[i]==0)
                                return;
                        i++;
                }
        }
        st+=i;
// get rid of the newline
        if (st[strlen(st)-1]==0xa)
                st[strlen(st)-1]=0;
	draw_help_icon(x,y,st);
}

void draw_help_icon(int x,int y,char *icon)
{
        int i,l,w,h;
        char ss[100];
        FILE *inf;
        strcpy(ss,graphic_path);
        strcat(ss,icon);
        if ((inf=fopen(ss,"rb"))==NULL)
        {
		return;
        }
        fseek(inf,0L,SEEK_END);
        l=ftell(inf);
        fseek(inf,0L,SEEK_SET);
	if (l==256)
		w=h=16;
	else if (l==1024)
		w=h=32;
	else if (l==2304)
		w=h=48;
	else if (l==4096)
		w=h=64;
	else
	{
		fclose(inf);
		return;
	}
        for (i=0;i<l;i++)
                *(help_graphic+i)=fgetc(inf);
        fclose(inf);
	if (x>0 && y>0 && ((x+w)<MAIN_WIN_W) && ((y+h)<MAIN_WIN_H))
		Fgl_putbox(MAIN_WIN_X+x,MAIN_WIN_Y+y,w,h,help_graphic);
        return;
}



void parse_buttonline(char *st)
{
        int i,j,x,y,w,h;
        sscanf(st,"button %d %d %d %d",&x,&y,&w,&h);
// find start of string
        i=0;
        for (j=0;j<5;j++)
        {
                while (isspace(st[i])==0)
                {
                        if (st[i]==0)
                                return;   // just silently ignore
                        i++;
                }
                while (isspace(st[i])!=0)
                {
                        if (st[i]==0)
                                return;
                        i++;
                }
        }
        st+=i;
// get rid of the newline
        if (st[strlen(st)-1]==0xa)
                st[strlen(st)-1]=0;
// centre x of box if x -ve
	if (x<0)
	{
		x=(MAIN_WIN_W/2)-(w/2);
		if (x<0)
			return;
	}
// see if the button runs off the end
	if ((x+w) > MAIN_WIN_W)
		return;
	if (numof_help_buttons>=MAX_NUMOF_HELP_BUTTONS)
		return;
	help_button_x[numof_help_buttons]=x+MAIN_WIN_X;
	help_button_y[numof_help_buttons]=y+MAIN_WIN_Y;
	help_button_w[numof_help_buttons]=w;
	help_button_h[numof_help_buttons]=h;
	if (strlen(st)>=MAX_LENOF_HELP_FILENAME)
		return;
	strcpy(help_button_s[numof_help_buttons],st);
	numof_help_buttons++;
// draw the box
	Fgl_hline(MAIN_WIN_X+x,MAIN_WIN_Y+y
		,MAIN_WIN_X+x+w,HELPBUTTON_COLOUR);
	Fgl_hline(MAIN_WIN_X+x,MAIN_WIN_Y+y+h
		,MAIN_WIN_X+x+w,HELPBUTTON_COLOUR);
	Fgl_line(MAIN_WIN_X+x,MAIN_WIN_Y+y
			,MAIN_WIN_X+x,MAIN_WIN_Y+y+h,HELPBUTTON_COLOUR);
	Fgl_line(MAIN_WIN_X+x+w,MAIN_WIN_Y+y
			,MAIN_WIN_X+x+w,MAIN_WIN_Y+y+h,HELPBUTTON_COLOUR);
}

void do_help_buttons(int x,int y)
{
	int i;
	if (numof_help_buttons<=0)
		return;
	for (i=0;i<numof_help_buttons;i++)
		if (x>help_button_x[i]
			&& x<(help_button_x[i]+help_button_w[i])
			&& y>help_button_y[i]
			&& y<(help_button_y[i]+help_button_h[i]) )
		{
			hide_mouse();
			draw_help_page(help_button_s[i]);
			redraw_mouse();
			break;
		}
}

void parse_tbuttonline(char *st)
{
	char s[100],ss[120],s1[100];
        int i,j,x,y;
	strcpy(s1,st);  // hpux fix?  we can live with this.
        sscanf(s1,"tbutton %d %d %s",&x,&y,s);
// find start of string
        i=0;
        for (j=0;j<4;j++)
        {
                while (isspace(st[i])==0)
                {
                        if (st[i]==0)
                                return;   // just silently ignore
                        i++;
                }
                while (isspace(st[i])!=0)
                {
                        if (st[i]==0)
                                return;
                        i++;
                }
        }
        st+=i;
// get rid of the newline
        if (st[strlen(st)-1]==0xa)
                st[strlen(st)-1]=0;
	if (x<0)
		x-=2; // needed to keep text centred
	sprintf(ss,"text %d %d ",x+2,y+2);
	strcat(ss,st);
	parse_textline(ss);
	sprintf(ss,"button %d %d %d %d %s",x,y,(strlen(st)*8)+4,12,s);
	parse_buttonline(ss);
	
}
