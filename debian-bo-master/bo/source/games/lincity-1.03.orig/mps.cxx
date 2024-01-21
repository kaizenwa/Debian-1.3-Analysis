//   mps.cxx   part of lin-city
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
#include "mps.h"


void mappoint_stats(int x,int y,int button)
{
	static xx=0,yy=0,oldbut=0;
	char s[100];
//	int i;

	if (x<0 || y<0)
	{
		x=xx;
		y=yy;
		button=oldbut;
	}
	else
	{
		if (mappoint_stats_flag==1 && x==xx && y==yy
			&& button==MOUSE_LEFTBUTTON)
		{
 
			if (main_types[mappointtype[xx][yy]].group
				==GROUP_MARKET)
			{
				clicked_market_cb(xx,yy);
				return;
			}
			else if (main_types[mappointtype[xx][yy]].group
				==GROUP_PORT)
			{
				clicked_port_cb(xx,yy);
				return;
			}
//			else if (main_types[mappointtype[xx][yy]].group
//				&& mappointtype[xx][yy]==CST_ROCKET_FLOWN)
//			{
//				if (yn_dial_box("ROCKET LAUNCH"
//		,"You can launch the rocket now or wait until later."
//		,"If you wait, it costs you *only* money to keep the"
//		,"rocket ready.    Launch?")!=0)
//					launch_rocket(xx,yy);
//				else
//					return;
//			}
		}
		xx=x;
		yy=y;
		oldbut=button;
		mappoint_stats_flag=1;
// draw centre of box
		Fgl_fillbox(MAPPOINT_STATS_X,MAPPOINT_STATS_Y
			,MAPPOINT_STATS_W+1,MAPPOINT_STATS_H+1,14);
// write static stuff
#ifdef USE_EXPANDED_FONT
		gl_setwritemode(WRITEMODE_MASKED | FONT_EXPANDED);
#else
		Fgl_setfontcolors(14,TEXT_FG_COLOUR);
#endif
		strcpy(s,type_string[main_types[mappointtype[x][y]].group]);
		Fgl_write(MAPPOINT_STATS_X+(14-strlen(s))*4
			,MAPPOINT_STATS_Y,s);

		if (button==MOUSE_RIGHTBUTTON)
			mps_right_setup();
		else switch(main_types[mappointtype[x][y]].group)
		{
			case (GROUP_RESIDENCE): mps_res_setup(); break;
			case (GROUP_ROAD):      mps_transport_setup(); break;
			case (GROUP_RAIL):      mps_transport_setup(); break;
			case (GROUP_TRACK):     mps_transport_setup(); break;
			case (GROUP_ORGANIC_FARM): mps_farm_setup(); break;
			case (GROUP_MARKET):    mps_market_setup(); break;
			case (GROUP_INDUSTRY_L):   mps_indl_setup(); break;
			case (GROUP_INDUSTRY_H):   mps_indh_setup(); break;
			case (GROUP_COALMINE): mps_coalmine_setup(); break;
			case (GROUP_POWER_SOURCE_COAL):
				mps_power_source_coal_setup(); break;
			case (GROUP_POWER_SOURCE):
				mps_power_source_setup(); break;
			case (GROUP_UNIVERSITY): 
				mps_university_setup(); break;
			case (GROUP_OREMINE): mps_oremine_setup(); break;
			case (GROUP_RECYCLE): mps_recycle_setup(); break; 
			case (GROUP_SUBSTATION): 
				mps_substation_setup(); break;
			case (GROUP_ROCKET): mps_rocket_setup(); break;
			case (GROUP_WINDMILL):
				mps_windmill_setup(x,y); break;
			case (GROUP_MONUMENT):
				mps_monument_setup(); break;
			case (GROUP_SCHOOL): mps_school_setup(); break;
			case (GROUP_BLACKSMITH):
				mps_blacksmith_setup(); break;
			case (GROUP_MILL): mps_mill_setup(); break;
			case (GROUP_POTTERY):
				mps_pottery_setup(); break;
			case (GROUP_PORT):
				mps_port_setup(); break;
			case (GROUP_TIP):
				mps_tip_setup(); break;
			case (GROUP_COMMUNE):
				mps_commune_setup(); break;
			case (GROUP_FIRESTATION):
				mps_firestation_setup(); break;
			case (GROUP_CRICKET):
				mps_cricket_setup(); break;
			case (GROUP_HEALTH):
				mps_health_setup(); break;
		}
	}
#ifdef USE_EXPANDED_FONT
        Fgl_fillbox(MAPPOINT_STATS_X+7*8,MAPPOINT_STATS_Y+8
                ,MAPPOINT_STATS_W-7*8,MAPPOINT_STATS_H-8,14);
        gl_setwritemode(WRITEMODE_MASKED | FONT_EXPANDED);
#else
	Fgl_setfontcolors(14,TEXT_FG_COLOUR);
#endif
	if (button==MOUSE_RIGHTBUTTON)
		mps_right(x,y);
        else switch(main_types[mappointtype[x][y]].group)
        {
                case (GROUP_RESIDENCE):    mps_res(x,y); break;
                case (GROUP_ROAD):         mps_road(x,y); break;
                case (GROUP_RAIL):         mps_rail(x,y); break;
                case (GROUP_TRACK):        mps_track(x,y); break;
                case (GROUP_ORGANIC_FARM): mps_farm(x,y); break;
		case (GROUP_MARKET):       mps_market(x,y); break;
		case (GROUP_INDUSTRY_L):   mps_indl(x,y); break;
		case (GROUP_INDUSTRY_H):   mps_indh(x,y); break;
		case (GROUP_COALMINE):     mps_coalmine(x,y); break;
		case (GROUP_POWER_SOURCE_COAL):
			mps_power_source_coal(x,y); break;
		case (GROUP_POWER_SOURCE): mps_power_source(x,y); break;
		case (GROUP_UNIVERSITY):   mps_university(x,y); break;
		case (GROUP_OREMINE):      mps_oremine(x,y); break;
		case (GROUP_RECYCLE):      mps_recycle(x,y); break;
		case (GROUP_SUBSTATION):   mps_substation(x,y); break;
		case (GROUP_ROCKET):       mps_rocket(x,y); break;
		case (GROUP_WINDMILL):     mps_windmill(x,y); break;
		case (GROUP_MONUMENT):     mps_monument(x,y); break;
		case (GROUP_SCHOOL):       mps_school(x,y); break;
		case (GROUP_BLACKSMITH):   mps_blacksmith(x,y); break;
		case (GROUP_MILL):         mps_mill(x,y); break;
		case (GROUP_POTTERY):      mps_pottery(x,y); break;
		case (GROUP_WATER):        mps_water(x,y);   break;
		case (GROUP_PORT):         mps_port(x,y); break;
		case (GROUP_TIP):          mps_tip(x,y); break;
		case (GROUP_COMMUNE):      mps_commune(x,y); break;
		case (GROUP_FIRESTATION):  mps_firestation(x,y); break;
		case (GROUP_CRICKET):      mps_cricket(x,y); break;
		case (GROUP_HEALTH):       mps_health(x,y); break;
        }

#ifdef USE_EXPANDED_FONT
	gl_setwritemode(WRITEMODE_OVERWRITE | FONT_EXPANDED);
#else
	Fgl_setfontcolors(TEXT_BG_COLOUR,TEXT_FG_COLOUR);
#endif
}

void mps_res_setup(void)
{
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+8,"People");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+16,"Power");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+24,"Fed");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+32,"Empld");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+40,"H cov");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+48,"F cov");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+56,"C cov");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+64,"Poll'n");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+80,"Job pro");
}

void mps_res(int x,int y)
{
	char s[100];
        sprintf(s,"%d ",mappoint[x][y].population);
        Fgl_write(MAPPOINT_STATS_X+7*8,MAPPOINT_STATS_Y+8,s);
        if ((mappoint[x][y].flags & FLAG_POWERED)!=0)
                strcpy(s,"YES");
        else
                strcpy(s,"NO ");
        Fgl_write(MAPPOINT_STATS_X+7*8,MAPPOINT_STATS_Y+16,s);

        if ((mappoint[x][y].flags & FLAG_FED)!=0)
                strcpy(s,"YES");
        else
                strcpy(s,"NO ");
        Fgl_write(MAPPOINT_STATS_X+7*8,MAPPOINT_STATS_Y+24,s);

        if ((mappoint[x][y].flags & FLAG_EMPLOYED)!=0)
                strcpy(s,"YES");
        else
                strcpy(s,"NO ");
        Fgl_write(MAPPOINT_STATS_X+7*8,MAPPOINT_STATS_Y+32,s);

        if ((mappoint[x][y].flags & FLAG_HEALTH_COVER)!=0)
                strcpy(s,"YES");
        else
                strcpy(s,"NO ");
        Fgl_write(MAPPOINT_STATS_X+7*8,MAPPOINT_STATS_Y+40,s);

        if ((mappoint[x][y].flags & FLAG_FIRE_COVER)!=0)
                strcpy(s,"YES");
        else
                strcpy(s,"NO ");
        Fgl_write(MAPPOINT_STATS_X+7*8,MAPPOINT_STATS_Y+48,s);

        if ((mappoint[x][y].flags & FLAG_CRICKET_COVER)!=0)
                strcpy(s,"YES");
        else
                strcpy(s,"NO ");
        Fgl_write(MAPPOINT_STATS_X+7*8,MAPPOINT_STATS_Y+56,s);

// pollution
        sprintf(s,"%7d",mappointpol[x][y]);
        Fgl_write(MAPPOINT_STATS_X+7*8,MAPPOINT_STATS_Y+64,s);

// job prospects
	if (mappoint[x][y].int_1>=10)
        	sprintf(s,"   good");
	else
		sprintf(s,"%7d",mappoint[x][y].int_1);
        Fgl_write(MAPPOINT_STATS_X+7*8,MAPPOINT_STATS_Y+80,s);
}

void mps_transport_setup(void)
{
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+32,"Food");
        Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+40,"Jobs");
        Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+48,"Coal");
        Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+56,"Goods");
        Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+64,"Ore");
        Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+72,"Steel");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+80,"Waste");
}

void mps_road(int x,int y)
{
	char s[100];
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_1*100.0
		/MAX_FOOD_ON_ROAD);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+32,s);
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_2*100.0
		/MAX_JOBS_ON_ROAD);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+40,s);
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_3*100.0
		/MAX_COAL_ON_ROAD);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+48,s);
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_4*100.0
		/MAX_GOODS_ON_ROAD);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+56,s);
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_5*100.0
		/MAX_ORE_ON_ROAD);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+64,s);
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_6*100
		/MAX_STEEL_ON_ROAD);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+72,s);
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_7*100
                /MAX_WASTE_ON_ROAD);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+80,s);

}

void mps_rail(int x,int y)
{
        char s[100];
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_1*100.0
                /MAX_FOOD_ON_RAIL);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+32,s);
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_2*100.0
                /MAX_JOBS_ON_RAIL);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+40,s);
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_3*100.0
                /MAX_COAL_ON_RAIL);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+48,s);
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_4*100.0
                /MAX_GOODS_ON_RAIL);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+56,s);
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_5*100.0
                /MAX_ORE_ON_RAIL);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+64,s);
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_6*100
                /MAX_STEEL_ON_RAIL);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+72,s);
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_7*100
                /MAX_WASTE_ON_RAIL);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+80,s);

}

void mps_track(int x,int y)
{
        char s[100];
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_1*100.0
                /MAX_FOOD_ON_TRACK);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+32,s);
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_2*100.0
                /MAX_JOBS_ON_TRACK);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+40,s);
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_3*100.0
                /MAX_COAL_ON_TRACK);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+48,s);
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_4*100.0
                /MAX_GOODS_ON_TRACK);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+56,s);
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_5*100.0
                /MAX_ORE_ON_TRACK);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+64,s);
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_6*100
                /MAX_STEEL_ON_TRACK);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+72,s);
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_7*100
                /MAX_WASTE_ON_TRACK);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+80,s);

}

void mps_market_setup(void)
{
	mps_transport_setup();
// put flags in
}

void mps_market(int x,int y)
{
        char s[100];
	float f;
	f=(float)mappoint[x][y].int_1*100.0/MAX_FOOD_IN_MARKET;
	if (f>100.0)
		f=100.0;
        sprintf(s,"%5.1f%%",f);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+32,s);
	f=(float)mappoint[x][y].int_2*100.0/MAX_JOBS_IN_MARKET;
	if (f>100.0)
		f=100.0;
        sprintf(s,"%5.1f%%",f);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+40,s);
	f=(float)mappoint[x][y].int_3*100.0/MAX_COAL_IN_MARKET;
	if (f>100.0)
		f=100.0;
        sprintf(s,"%5.1f%%",f);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+48,s);
	f=(float)mappoint[x][y].int_4*100.0/MAX_GOODS_IN_MARKET;
	if (f>100.0)
		f=100.0;
        sprintf(s,"%5.1f%%",f);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+56,s);
	f=(float)mappoint[x][y].int_5*100.0/MAX_ORE_IN_MARKET;
	if (f>100.0)
		f=100.0;
        sprintf(s,"%5.1f%%",f);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+64,s);
	f=(float)mappoint[x][y].int_6*100/MAX_STEEL_IN_MARKET;
	if (f>100.0)
		f=100.0;
        sprintf(s,"%5.1f%%",f);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+72,s);
        f=(float)mappoint[x][y].int_7*100/MAX_WASTE_IN_MARKET;
        if (f>100.0)
                f=100.0;
        sprintf(s,"%5.1f%%",f);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+80,s);

}



void mps_farm_setup(void)
{
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+16,"Power");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+40,"Tech");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+48,"Prod");
}

void mps_farm(int x,int y)
{
	char s[100];
        if ((mappoint[x][y].flags & FLAG_POWERED)!=0)
                strcpy(s,"YES");
        else
                strcpy(s,"NO ");
        Fgl_write(MAPPOINT_STATS_X+7*8,MAPPOINT_STATS_Y+16,s);
	sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_1*100.0
		/MAX_TECH_LEVEL);
	Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+40,s);
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_4*100.0
                /1200.0);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+48,s);

}

void mps_indl_setup(void)
{
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+16,"Power");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+40,"Output");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+48,"Store");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+56,"Ore");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+64,"Steel");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+80,"Capacity");
}

void mps_indl(int x,int y)
{
	char s[100];
	float f;
        if ((mappoint[x][y].flags & FLAG_POWERED)!=0)
                strcpy(s,"YES");
        else
                strcpy(s,"NO ");
        Fgl_write(MAPPOINT_STATS_X+7*8,MAPPOINT_STATS_Y+16,s);
        sprintf(s,"%7d",mappoint[x][y].int_1);
        Fgl_write(MAPPOINT_STATS_X+7*8,MAPPOINT_STATS_Y+40,s);
	f=(float)mappoint[x][y].int_2*100.0/MAX_GOODS_AT_INDUSTRY_L;
	if (f>100.0)
		f=100.0;
        sprintf(s,"%3.1f%%",f);
        Fgl_write(MAPPOINT_STATS_X+9*8,MAPPOINT_STATS_Y+48,s);
	f=(float)mappoint[x][y].int_3*100.0/MAX_ORE_AT_INDUSTRY_L;
	if (f>100.0)
		f=100.0;
        sprintf(s,"%3.1f%%",f);
        Fgl_write(MAPPOINT_STATS_X+9*8,MAPPOINT_STATS_Y+56,s);
	f=(float)mappoint[x][y].int_4*100.0/MAX_STEEL_AT_INDUSTRY_L;
	if (f>100.0)
		f=100.0;
        sprintf(s,"%3.1f%%",f);
        Fgl_write(MAPPOINT_STATS_X+9*8,MAPPOINT_STATS_Y+64,s);
        sprintf(s,"%4d%%",mappoint[x][y].int_6);
        Fgl_write(MAPPOINT_STATS_X+9*8,MAPPOINT_STATS_Y+80,s);

}

void mps_indh_setup(void)
{
        Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+16,"Power");
        Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+40,"Output");
        Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+48,"Store");
        Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+56,"Ore");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+64,"Coal");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+80,"Capacity");
}

void mps_indh(int x,int y)
{
        char s[100];
	float f;
        if ((mappoint[x][y].flags & FLAG_POWERED)!=0)
                strcpy(s,"YES");
        else
                strcpy(s,"NO ");
        Fgl_write(MAPPOINT_STATS_X+7*8,MAPPOINT_STATS_Y+16,s);
        sprintf(s,"%7d",mappoint[x][y].int_1);
        Fgl_write(MAPPOINT_STATS_X+7*8,MAPPOINT_STATS_Y+40,s);
	f=(float)mappoint[x][y].int_2*100.0/MAX_STEEL_AT_INDUSTRY_H;
	if (f>100.0)
		f=100.0;
        sprintf(s,"%5.1f%%",f);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+48,s);
	f=(float)mappoint[x][y].int_3*100.0/MAX_ORE_AT_INDUSTRY_H;
	if (f>100.0)
		f=100.0;
        sprintf(s,"%5.1f%%",f);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+56,s);
	f=(float)mappoint[x][y].int_4*100.0/MAX_COAL_AT_INDUSTRY_H;
	if (f>100.0)
		f=100.0;
        sprintf(s,"%5.1f%%",f);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+64,s);
        sprintf(s,"%4d%%",mappoint[x][y].int_5);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+80,s);
}

void mps_coalmine_setup(void)
{
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+40,"Stock");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+48,"Reserve");
}

void mps_coalmine(int x,int y)
{
	char s[100];
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_1*100
		/MAX_COAL_AT_MINE);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+40,s);
	if (mappoint[x][y].int_2>0)
	        sprintf(s,"%7d",mappoint[x][y].int_2);
	else
		sprintf(s," EMPTY ");
        Fgl_write(MAPPOINT_STATS_X+7*8,MAPPOINT_STATS_Y+48,s);	
}

void mps_power_source_coal_setup(void)
{
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+40,"Cycle");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+48,"Coal");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+56,"Jobs");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+64,"Tech");
}

void mps_power_source_coal(int x,int y)
{
	char s[100];
	float f;
	f=(float)mappoint[x][y].int_1*100/POWER_LINE_CAPACITY;
	if (f>100.0)
		f=100.0;
	sprintf(s,"%5.1f%%",f);
	Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+40,s);
	f=(float)mappoint[x][y].int_2*100/MAX_COAL_AT_POWER_STATION;
	if (f>100.0)
		f=100.0;
	sprintf(s,"%5.1f%%",f);
	Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+48,s);
	sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_3*100
		/MAX_JOBS_AT_COALPS);
	Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+56,s);
// tech level is int_4
	sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_4*100
		/MAX_TECH_LEVEL);
	Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+64,s);
}

void mps_power_source_setup(void)
{
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+40,"Cycle");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+48,"Tech");
}

void mps_power_source(int x,int y)
{
	char s[100];
	sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_1*100
		/POWER_LINE_CAPACITY);
	Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+40,s);
	sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_2*100
		/MAX_TECH_LEVEL);
	Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+48,s);
}
	
void mps_university_setup(void)
{
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+40,"Jobs");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+48,"Goods");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+56,"T made");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+64,"Capacity");
}

void mps_university(int x,int y)
{
	char s[100];
	sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_1*100
		/UNIVERSITY_JOBS_STORE);
	Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+40,s);
	sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_2*100
		/UNIVERSITY_GOODS_STORE);
	Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+48,s);
        sprintf(s,"%6.1f",(float)mappoint[x][y].int_3*100.0
        	/MAX_TECH_LEVEL);
	Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+56,s);
	sprintf(s,"%4d%%",mappoint[x][y].int_5);
	Fgl_write(MAPPOINT_STATS_X+9*8,MAPPOINT_STATS_Y+64,s);
}

void mps_recycle_setup(void)
{
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+16,"Power");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+40,"O stock");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+48,"W store");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+56,"S store");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+64,"Tech");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+80,"Capacity");
}

void mps_recycle(int x,int y)
{
	char s[100];
        if ((mappoint[x][y].flags & FLAG_POWERED)!=0)
                strcpy(s,"YES");
        else
                strcpy(s,"NO ");
        Fgl_write(MAPPOINT_STATS_X+7*8,MAPPOINT_STATS_Y+16,s);
	sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_1*100
		/MAX_ORE_AT_RECYCLE);
	Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+40,s);
	sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_2*100
		/MAX_WASTE_AT_RECYCLE);
	Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+48,s);
	Fgl_write(MAPPOINT_STATS_X+9*8,MAPPOINT_STATS_Y+56,"-");
	sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_4*100
		/MAX_TECH_LEVEL);
	Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+64,s);
        sprintf(s,"%4d%%",mappoint[x][y].int_6);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+80,s);
}

void mps_oremine_setup(void)
{
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+40,"Stock");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+48,"Reserve");
}

void mps_oremine(int x,int y)
{
	char s[100];
	sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_1*100
		/DIG_MORE_ORE_TRIGGER);
	Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+40,s);
	sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_2*100
		/(ORE_RESERVE*16));
	Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+48,s);
}

void mps_substation_setup(void)
{
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+40,"Power");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+48,"Tech");
}

void mps_substation(int x,int y)
{
	char s[100];
	sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_1*100
		/POWER_LINE_CAPACITY);
	Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+40,s);
	sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_2*100
		/MAX_TECH_LEVEL);
	Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+48,s);
}

void mps_rocket_setup(void)
{
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+40,"Jobs");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+48,"Goods");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+56,"Steel");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+64,"Launch");
}

void mps_rocket(int x,int y)
{
	char s[100];
	sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_1*100
		/ROCKET_PAD_JOBS_STORE);
	Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+40,s);
	sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_2*100
		/ROCKET_PAD_GOODS_STORE);
	Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+48,s);
	sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_3*100
		/ROCKET_PAD_STEEL_STORE);
	Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+56,s);
	sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_4*100
		/ROCKET_PAD_LAUNCH);
	Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+64,s);
	if (mappointtype[x][y]==CST_ROCKET_5)
	{
                if (yn_dial_box("ROCKET LAUNCH"
                ,"You can launch the rocket now or wait until later."
                ,"If you wait, it costs you *only* money to keep the"
                ,"rocket ready.    Launch?")!=0)
                        launch_rocket(x,y);
	}
}

void mps_windmill_setup(int x,int y)
{
	if (mappoint[x][y].int_2>=MODERN_WINDMILL_TECH)
        	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+40,"Power");
        Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+48,"Tech");
}

void mps_windmill(int x,int y)
{
	char s[100];
	if (mappoint[x][y].int_2>=MODERN_WINDMILL_TECH)
	{
		sprintf(s,"%7d",mappoint[x][y].int_1);
		Fgl_write(MAPPOINT_STATS_X+7*8,MAPPOINT_STATS_Y+40,s);
	}
	sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_2*100
		/MAX_TECH_LEVEL);
	Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+48,s);
}

void mps_monument_setup(void)
{
        Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+40,"Built");
        Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+48,"T made");
}


void mps_monument(int x,int y)
{
        char s[100];
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_1*100
		/BUILD_MONUMENT_JOBS);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+40,s);
        sprintf(s,"%5.1f",(float)mappoint[x][y].int_2*100
                /MAX_TECH_LEVEL);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+48,s);
}

void mps_school_setup(void)
{
        Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+40,"Jobs");
        Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+48,"Goods");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+56,"T made");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+64,"Capacity");
}

void mps_school(int x,int y)
{
        char s[100];
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_1*100
                /MAX_JOBS_AT_SCHOOL);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+40,s);
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_2*100
                /MAX_GOODS_AT_SCHOOL);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+48,s);
        sprintf(s,"%6.1f",(float)mappoint[x][y].int_3*100
                /MAX_TECH_LEVEL);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+56,s);
        sprintf(s,"%5d%%",mappoint[x][y].int_5*4);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+64,s);

}

void mps_blacksmith_setup(void)
{
        Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+40,"G store");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+48,"C store");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+56,"Capacity");
}

void mps_blacksmith(int x,int y)
{
        char s[100];
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_1*100
                /MAX_GOODS_AT_BLACKSMITH);
	Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+40,s);
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_3*100
                /MAX_COAL_AT_BLACKSMITH);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+48,s);
	sprintf(s,"%5d%%",mappoint[x][y].int_6);
	Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+56,s);
}

void mps_mill_setup(void)
{
        Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+40,"G store");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+48,"F store");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+56,"C store");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+64,"Capacity");
}

void mps_mill(int x,int y)
{
        char s[100];
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_1*100
                /MAX_GOODS_AT_MILL);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+40,s);
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_2*100
                /MAX_FOOD_AT_MILL);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+48,s);
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_3*100
                /MAX_COAL_AT_MILL);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+56,s);
	sprintf(s,"%5d%%",mappoint[x][y].int_6);
	Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+64,s);
}

void mps_pottery_setup(void)
{
        Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+40,"G store");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+48,"O store");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+56,"C store");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+64,"Capacity");
}

void mps_pottery(int x,int y)
{
        char s[100];
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_1*100
                /MAX_GOODS_AT_POTTERY);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+40,s);
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_2*100
                /MAX_ORE_AT_POTTERY);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+48,s);
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_3*100
                /MAX_COAL_AT_POTTERY);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+56,s);
	sprintf(s,"%5d%%",mappoint[x][y].int_6);
	Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+64,s);
}

void mps_water(int x,int y)
{
	if (mappoint[x][y].flags & FLAG_IS_RIVER)
		Fgl_write(MAPPOINT_STATS_X+1*8,MAPPOINT_STATS_Y+40
			,"  CONNECTED  ");
	else
		Fgl_write(MAPPOINT_STATS_X+1*8,MAPPOINT_STATS_Y+40
			,"NOT CONNECTED");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+48,"to river sytem");
}

void mps_port_setup(void)
{
        Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+12,"  Values for");
        Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+20,"  last month");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+32,"Import=");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+48,"Export=");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+68,"Click again");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+76,"for controls");
}

void mps_port(int x,int y)
{
        char s[100];
        sprintf(s,"%7d.%02d",mappoint[x][y].int_5/100
                                ,mappoint[x][y].int_5%100);
        Fgl_write(MAPPOINT_STATS_X+3*8,MAPPOINT_STATS_Y+40,s);
        sprintf(s,"%7d.%02d",mappoint[x][y].int_2/100
                                ,mappoint[x][y].int_2%100);
        Fgl_write(MAPPOINT_STATS_X+3*8,MAPPOINT_STATS_Y+56,s);
        mini_screen_port_x=x;
        mini_screen_port_y=y;
        mini_screen_flags=MINI_SCREEN_PORT_FLAG;
}

void mps_tip_setup(void)
{
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+32,"  Waste stored");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+40,"   last month");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+64,"       % full");
}

void mps_tip(int x,int y)
{
	char s[100];
	sprintf(s,"%7d",mappoint[x][y].int_3);
	Fgl_write(MAPPOINT_STATS_X+3*8,MAPPOINT_STATS_Y+52,s);
	sprintf(s,"%4.1f",(float)(mappoint[x][y].int_1*100)
		/(float)MAX_WASTE_AT_TIP);
	Fgl_write(MAPPOINT_STATS_X+3*8,MAPPOINT_STATS_Y+64,s);
}

void mps_commune_setup(void)
{
        Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+12,"   Activity");
        Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+20,"  last month");
        Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+36,"  Coal");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+44,"   Ore");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+52," Steel");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+60," Waste");
}

void mps_commune(int x,int y)
{
//        char s[100];
        if ((mappoint[x][y].int_5 & 1)!=0)
        	Fgl_write(MAPPOINT_STATS_X+7*8,MAPPOINT_STATS_Y+36,"YES");
	else
		Fgl_write(MAPPOINT_STATS_X+7*8,MAPPOINT_STATS_Y+36,"NO ");
	if ((mappoint[x][y].int_5 & 2)!=0)
		Fgl_write(MAPPOINT_STATS_X+7*8,MAPPOINT_STATS_Y+44,"YES");
	else
		Fgl_write(MAPPOINT_STATS_X+7*8,MAPPOINT_STATS_Y+44,"NO ");
	if ((mappoint[x][y].int_5 & 4)!=0)
		Fgl_write(MAPPOINT_STATS_X+7*8,MAPPOINT_STATS_Y+52,"YES");
	else
		Fgl_write(MAPPOINT_STATS_X+7*8,MAPPOINT_STATS_Y+52,"NO ");
	if ((mappoint[x][y].int_5 & 8)!=0)
		Fgl_write(MAPPOINT_STATS_X+7*8,MAPPOINT_STATS_Y+60,"YES");
	else
		Fgl_write(MAPPOINT_STATS_X+7*8,MAPPOINT_STATS_Y+60,"NO ");
}


void mps_right_setup(void)
{
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+8,"  Fire cover");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+24," Health cover");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+40,"Cricket cover");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+56,"  Pollution");
	Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+76," Bull");
}

void mps_right(int x,int y)
{
	char s[100];
	int g;
	if ((mappoint[x][y].flags & FLAG_FIRE_COVER)!=0)
		Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+16,"YES");
	else
		Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+16,"NO ");
	if ((mappoint[x][y].flags & FLAG_HEALTH_COVER)!=0)
		Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+32,"YES");
	else
		Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+32,"NO ");
        if ((mappoint[x][y].flags & FLAG_CRICKET_COVER)!=0)
                Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+48,"YES");
        else
                Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+48,"NO ");
	sprintf(s,"%5d ",mappointpol[x][y]);
	if (mappointpol[x][y]<10)
		strcat(s,"(clear) ");
        else if (mappointpol[x][y]<25)
                strcat(s,"(good)  ");
        else if (mappointpol[x][y]<70)
                strcat(s,"(fair)  ");
        else if (mappointpol[x][y]<190)
                strcat(s,"(smelly)");
        else if (mappointpol[x][y]<450)
                strcat(s,"(smokey)");
        else if (mappointpol[x][y]<1000)
                strcat(s,"(smoggy)");
        else if (mappointpol[x][y]<1700)
                strcat(s,"(bad)   ");
        else if (mappointpol[x][y]<3000)
                strcat(s,"(v bad) ");
        else
                strcat(s,"(death!)");
	Fgl_write(MAPPOINT_STATS_X+8,MAPPOINT_STATS_Y+64,s);
	g=main_types[mappointtype[x][y]].group;
	if (g<7)
		g--; // translate into button type
	sprintf(s,"%7d",select_button_bul_cost[sbut[g]]);
	Fgl_write(MAPPOINT_STATS_X+48,MAPPOINT_STATS_Y+76,s);
}

void mps_firestation_setup(void)
{
        Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+40,"Jobs");
        Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+48,"Goods");
}

void mps_firestation(int x,int y)
{
        char s[100];
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_1*100
                /MAX_JOBS_AT_FIRESTATION);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+40,s);
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_2*100
                /MAX_GOODS_AT_FIRESTATION);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+48,s);

}

void mps_cricket_setup(void)
{
        Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+40,"Jobs");
        Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+48,"Goods");
}

void mps_cricket(int x,int y)
{
        char s[100];
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_1*100
                /MAX_JOBS_AT_CRICKET);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+40,s);
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_2*100
                /MAX_GOODS_AT_CRICKET);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+48,s);

}
 
void mps_health_setup(void)
{
        Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+40,"Jobs");
        Fgl_write(MAPPOINT_STATS_X,MAPPOINT_STATS_Y+48,"Goods");
}

void mps_health(int x,int y)
{
        char s[100];
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_1*100
                /MAX_JOBS_AT_HEALTH_CENTRE);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+40,s);
        sprintf(s,"%5.1f%%",(float)mappoint[x][y].int_2*100
                /MAX_GOODS_AT_HEALTH_CENTRE);
        Fgl_write(MAPPOINT_STATS_X+8*8,MAPPOINT_STATS_Y+48,s);

}
