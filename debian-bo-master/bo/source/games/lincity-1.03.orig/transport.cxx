// transport.cxx	 part of lin-city
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
#include "lctypes.h"
#include "lin-city.h"
#include "transport.h"


void do_power_line(int x,int y)
{
// use int_1 to hold the packet
// use int_2 to hold current direction of packets
// use int_3 to hold the max count in a direction. Used so packets can get
// round T junctions.
	static int dirrand=1,disrand=1;
	int r,nx,ny;
#ifdef DEBUG_ENGINE
	printf("In do_power_line\n");
#endif
//  int_1 is the boolian status of the line. 0=no packet 1=packet.
	if (mappoint[x][y].int_1==0)  // i suppose there's nothing to do
		return;
// go and search for a place to put it.
// If we have a direction, use it, else search a random one.
	if (mappoint[x][y].int_2!=0)
		r=mappoint[x][y].int_2;
	else
	{
//		r=(rand()%4)+1;
		r=dirrand++;
		if (dirrand>=5)
			dirrand=1;
//		mappoint[x][y].int_3=rand()%100;
		mappoint[x][y].int_3=disrand++;
		if (disrand>40)
			disrand=0;
	}
//	if (r==1 && y>0)
	if (r==1)
	{
		nx=x;
		ny=y-1;
		if (y>1 && (mappoint[nx][ny].flags & (FLAG_IS_TRANSPORT
				+FLAG_IS_RIVER))!=0)
			ny--;
	}
//	else if (r==2 && x>0)
	else if (r==2)
	{
		nx=x-1;
		ny=y;
		if (x>1 && (mappoint[nx][ny].flags & (FLAG_IS_TRANSPORT
				+FLAG_IS_RIVER))!=0)
			nx--;
	}
//	else if (r==3 && x<WORLD_SIDE_LEN-1)
	else if (r==3)
	{
		nx=x+1;
		ny=y;
		if (x<WORLD_SIDE_LEN-2
			&& (mappoint[nx][ny].flags & (FLAG_IS_TRANSPORT
				+FLAG_IS_RIVER))!=0)
			nx++;
	}
//	else if (r==4 && y<WORLD_SIDE_LEN-1)
	else if (r==4)
	{
		nx=x;
		ny=y+1;
		if (y<WORLD_SIDE_LEN-2
			&& (mappoint[nx][ny].flags & (FLAG_IS_TRANSPORT
				+FLAG_IS_RIVER))!=0)
			ny++;
	}
	else
	{
		mappoint[x][y].int_2=0;
		return;
	}
	if (main_types[mappointtype[nx][ny]].group==GROUP_POWER_LINE
		&& mappoint[nx][ny].int_1==0)
	{
		mappoint[nx][ny].int_1=1;
		if (mappointtype[nx][ny]>CST_POWERL_LUDR_L)
			mappointtype[nx][ny]-=11;
		mappoint[x][y].int_1=0;
		if (mappointtype[x][y]<=CST_POWERL_LUDR_L)
			mappointtype[x][y]+=11;
		if (mappoint[x][y].int_3-- >0)
		{
			mappoint[nx][ny].int_2=r;
			mappoint[nx][ny].int_3=mappoint[x][y].int_3;
			mappoint[x][y].int_3=0;
		}
	}
	mappoint[x][y].int_2=0;
}

#ifdef THMO_TRANSPORT
void do_track(int x,int y)  //  mainly -- (ThMO)
{
  static int
    wb_count= 0;

  general_transport( &mappoint[x][y],&mappointpol[x][y]
		, MAX_WASTE_ON_TRACK, &wb_count);
}

#else

void do_track(int x,int y)
{
	static int wb_count=0;
	switch((mappoint[x][y].flags)&0xf)
	{
		case (0): return;
		case (1): t_av_l(x,y);
			break;
		case (2): t_av_u(x,y);
			break;
		case (3): t_av_lu(x,y);
			break;
		case (4): t_av_r(x,y);
			break;
		case (5): t_av_lr(x,y);
			break;
		case (6): t_av_ur(x,y);
			break;
		case (7): t_av_lur(x,y);
			break;
		case (8): t_av_d(x,y);
			break;
		case (9): t_av_ld(x,y);
			break;
		case (10): t_av_ud(x,y);
			break;
		case (11): t_av_lud(x,y);
			break;
		case (12): t_av_rd(x,y);
			break;
		case (13): t_av_lrd(x,y);
			break;
		case (14): t_av_urd(x,y);
			break;
		case (15): t_av_lurd(x,y);
			break;
		default: do_error("Transport switch bad flags");
	}
	if (mappoint[x][y].int_7>=MAX_WASTE_ON_TRACK)
	{
		mappoint[x][y].int_7-=WASTE_BURN_ON_TRANSPORT;
		mappointpol[x][y]++;
		if (wb_count++>TRANSPORT_BURN_WASTE_COUNT)
		{
			mini_screen_flags=MINI_SCREEN_POL_FLAG;
			wb_count=0;
		}
	}
}

#endif

// int_1 contains the amount of food on the track
// int_2 contains the amount of jobs on the track
// int_3 contains the amount of coal on the track
// int_4 contains the amount of goods on the track
// int_5 contains the amount of ore on the track
// int_6 contains the amount of steel on the track
// int_7 contains the amount of waste on the track

#ifdef THMO_TRANSPORT

void do_rail(int x,int y)    //  mainly -- (ThMO)
{
        static int wb_count=0;
	int *pol=&mappointpol[x][y];
	struct MAPPOINT *map=&mappoint[x][y];
  transport_cost += 3;
  if ( total_time % DAYS_PER_RAIL_POLLUTION == 0)
    *pol += RAIL_POLLUTION;
  if ( ( total_time & RAIL_GOODS_USED_MASK) == 0 && map->int_4 > 0)
    {
      --map->int_4;
      ++map->int_7;
    }
  if ( ( total_time & RAIL_STEEL_USED_MASK) == 0 && map->int_6 > 0)
    {
      --map->int_6;
      ++map->int_7;
    }
  general_transport( map, pol, MAX_WASTE_ON_RAIL, &wb_count);
}

#else

void do_rail(int x,int y)
{
	static int wb_count=0;
       transport_cost+=3;
	if ((total_time%DAYS_PER_RAIL_POLLUTION)==0)
        	mappointpol[x][y]+=RAIL_POLLUTION;
        if ((total_time&RAIL_GOODS_USED_MASK)==0 && mappoint[x][y].int_4>0)
	{
                mappoint[x][y].int_4--;
//		if (mappoint[x][y].int_7<MAX_WASTE_ON_RAIL)
			mappoint[x][y].int_7++;
	}
        if ((total_time&RAIL_STEEL_USED_MASK)==0 && mappoint[x][y].int_6>0)
	{
                mappoint[x][y].int_6--;
//		if (mappoint[x][y].int_7<MAX_WASTE_ON_RAIL)
			mappoint[x][y].int_7++;
	}
	switch((mappoint[x][y].flags)&0xf)
	{
		case (0): return;
		case (1): t_av_l(x,y);
			break;
		case (2): t_av_u(x,y);
			break;
		case (3): t_av_lu(x,y);
			break;
		case (4): t_av_r(x,y);
			break;
		case (5): t_av_lr(x,y);
			break;
		case (6): t_av_ur(x,y);
			break;
		case (7): t_av_lur(x,y);
			break;
		case (8): t_av_d(x,y);
			break;
		case (9): t_av_ld(x,y);
			break;
		case (10): t_av_ud(x,y);
			break;
		case (11): t_av_lud(x,y);
			break;
		case (12): t_av_rd(x,y);
			break;
		case (13): t_av_lrd(x,y);
			break;
		case (14): t_av_urd(x,y);
			break;
		case (15): t_av_lurd(x,y);
			break;
		default: do_error("Transport switch bad flags");
	}
       if (mappoint[x][y].int_7>=MAX_WASTE_ON_RAIL)
        {
                mappoint[x][y].int_7-=WASTE_BURN_ON_TRANSPORT;
                mappointpol[x][y]++;
		if (wb_count++>TRANSPORT_BURN_WASTE_COUNT)
		{
               	mini_screen_flags=MINI_SCREEN_POL_FLAG;
			wb_count=0;
		}
        }
}

#endif

// int_1 contains the amount of food on the rail
// int_2 contains the jobs on the rail
// int_3 contains the coal on the rail
// int_4 contains the goods on the rail
// int_5 contains the ore on the rail
// int_6 contains the steel on the rail
// int_7 contains the waste on the rail

#ifdef THMO_TRANSPORT

void do_road(int x,int y)
{
        static int wb_count=0;
	int *pol=&mappointpol[x][y];
	struct MAPPOINT *map=&mappoint[x][y];
  ++transport_cost;
  if ( total_time % DAYS_PER_ROAD_POLLUTION == 0)
    *pol += ROAD_POLLUTION;
  if ( ( total_time & ROAD_GOODS_USED_MASK) == 0 && map->int_4 > 0)
    {
      --map->int_4;
      ++map->int_7;
    }
  general_transport( map, pol, MAX_WASTE_ON_ROAD, &wb_count);
}

#else

void do_road(int x,int y)
{
	static int wb_count=0;
        transport_cost++;
	if ((total_time%DAYS_PER_ROAD_POLLUTION)==0)
       	mappointpol[x][y]+=ROAD_POLLUTION;
	if ((total_time&ROAD_GOODS_USED_MASK)==0 && mappoint[x][y].int_4>0)
	{
		mappoint[x][y].int_4--;
//		if (mappoint[x][y].int_7<MAX_WASTE_ON_RAIL)
			mappoint[x][y].int_7++;
	}
	switch((mappoint[x][y].flags)&0xf)
	{
		case (0): return;
		case (1): t_av_l(x,y);
			break;
		case (2): t_av_u(x,y);
			break;
		case (3): t_av_lu(x,y);
			break;
		case (4): t_av_r(x,y);
			break;
		case (5): t_av_lr(x,y);
			break;
		case (6): t_av_ur(x,y);
			break;
		case (7): t_av_lur(x,y);
			break;
		case (8): t_av_d(x,y);
			break;
		case (9): t_av_ld(x,y);
			break;
		case (10): t_av_ud(x,y);
			break;
		case (11): t_av_lud(x,y);
			break;
		case (12): t_av_rd(x,y);
			break;
		case (13): t_av_lrd(x,y);
			break;
		case (14): t_av_urd(x,y);
			break;
		case (15): t_av_lurd(x,y);
			break;
		default: do_error("Transport switch bad flags");
	}
       if (mappoint[x][y].int_7>=MAX_WASTE_ON_ROAD)
        {
                mappoint[x][y].int_7-=WASTE_BURN_ON_TRANSPORT;
                mappointpol[x][y]++;
		if (wb_count++>TRANSPORT_BURN_WASTE_COUNT)
		{	
               	mini_screen_flags=MINI_SCREEN_POL_FLAG;
			wb_count=0;
		}
        }
}
// int_1 contains the amount of food on the road
// int_2 contains the jobs on the road
// int_3 contains the coal on the road
// int_4 contains the goods on the road
// int_5 contains the ore on the road
// int_6 contains the steel on the road
// int_7 contains the waste on the road

#endif

#ifndef THMO_TRANSPORT

void t_av_l(int x,int y)   // 1
{
	int tot,av;
	tot=mappoint[x][y].int_1;
	tot+=mappoint[x-1][y].int_1;
	av=tot/2;
	mappoint[x-1][y].int_1=av;
	mappoint[x][y].int_1=av+(tot%2);

        tot=mappoint[x][y].int_2;
        tot+=mappoint[x-1][y].int_2;
        av=tot/2;
        mappoint[x-1][y].int_2=av;
        mappoint[x][y].int_2=av+(tot%2);

        tot=mappoint[x][y].int_3;
        tot+=mappoint[x-1][y].int_3;
        av=tot/2;
        mappoint[x-1][y].int_3=av;
        mappoint[x][y].int_3=av+(tot%2);

        tot=mappoint[x][y].int_4;
        tot+=mappoint[x-1][y].int_4;
        av=tot/2;
        mappoint[x-1][y].int_4=av;
        mappoint[x][y].int_4=av+(tot%2);

        tot=mappoint[x][y].int_5;
        tot+=mappoint[x-1][y].int_5;
        av=tot/2;
        mappoint[x-1][y].int_5=av;
        mappoint[x][y].int_5=av+(tot%2);

        tot=mappoint[x][y].int_6;
        tot+=mappoint[x-1][y].int_6;
        av=tot/2;
        mappoint[x-1][y].int_6=av;
        mappoint[x][y].int_6=av+(tot%2);

        tot=mappoint[x][y].int_7;
        tot+=mappoint[x-1][y].int_7;
        av=tot/2;
        mappoint[x-1][y].int_7=av;
        mappoint[x][y].int_7=av+(tot%2);

}

void t_av_u(int x,int y)  // 2
{
	int tot,av;
        tot=mappoint[x][y].int_1;
        tot+=mappoint[x][y-1].int_1;
        av=tot/2;
        mappoint[x][y-1].int_1=av;
        mappoint[x][y].int_1=av+(tot%2);

        tot=mappoint[x][y].int_2;
        tot+=mappoint[x][y-1].int_2;
        av=tot/2;
        mappoint[x][y-1].int_2=av;
        mappoint[x][y].int_2=av+(tot%2);

        tot=mappoint[x][y].int_3;
        tot+=mappoint[x][y-1].int_3;
        av=tot/2;
        mappoint[x][y-1].int_3=av;
        mappoint[x][y].int_3=av+(tot%2);

        tot=mappoint[x][y].int_4;
        tot+=mappoint[x][y-1].int_4;
        av=tot/2;
        mappoint[x][y-1].int_4=av;
        mappoint[x][y].int_4=av+(tot%2);

        tot=mappoint[x][y].int_5;
        tot+=mappoint[x][y-1].int_5;
        av=tot/2;
        mappoint[x][y-1].int_5=av;
        mappoint[x][y].int_5=av+(tot%2);

        tot=mappoint[x][y].int_6;
        tot+=mappoint[x][y-1].int_6;
        av=tot/2;
        mappoint[x][y-1].int_6=av;
        mappoint[x][y].int_6=av+(tot%2);

        tot=mappoint[x][y].int_7;
        tot+=mappoint[x][y-1].int_7;
        av=tot/2;
        mappoint[x][y-1].int_7=av;
        mappoint[x][y].int_7=av+(tot%2);

}

void t_av_lu(int x,int y)  // 3
{
	int tot,av;
        tot=mappoint[x][y].int_1;
	tot+=mappoint[x-1][y].int_1;
        tot+=mappoint[x][y-1].int_1;
        av=tot/3;
	mappoint[x-1][y].int_1=av;
        mappoint[x][y-1].int_1=av;
        mappoint[x][y].int_1=av+(tot%3);

        tot=mappoint[x][y].int_2;
        tot+=mappoint[x-1][y].int_2;
        tot+=mappoint[x][y-1].int_2;
        av=tot/3;
        mappoint[x-1][y].int_2=av;
        mappoint[x][y-1].int_2=av;
        mappoint[x][y].int_2=av+(tot%3);

        tot=mappoint[x][y].int_3;
        tot+=mappoint[x-1][y].int_3;
        tot+=mappoint[x][y-1].int_3;
        av=tot/3;
        mappoint[x-1][y].int_3=av;
        mappoint[x][y-1].int_3=av;
        mappoint[x][y].int_3=av+(tot%3);

        tot=mappoint[x][y].int_4;
        tot+=mappoint[x-1][y].int_4;
        tot+=mappoint[x][y-1].int_4;
        av=tot/3;
        mappoint[x-1][y].int_4=av;
        mappoint[x][y-1].int_4=av;
        mappoint[x][y].int_4=av+(tot%3);

        tot=mappoint[x][y].int_5;
        tot+=mappoint[x-1][y].int_5;
        tot+=mappoint[x][y-1].int_5;
        av=tot/3;
        mappoint[x-1][y].int_5=av;
        mappoint[x][y-1].int_5=av;
        mappoint[x][y].int_5=av+(tot%3);

        tot=mappoint[x][y].int_6;
        tot+=mappoint[x-1][y].int_6;
        tot+=mappoint[x][y-1].int_6;
        av=tot/3;
        mappoint[x-1][y].int_6=av;
        mappoint[x][y-1].int_6=av;
        mappoint[x][y].int_6=av+(tot%3);

        tot=mappoint[x][y].int_7;
        tot+=mappoint[x-1][y].int_7;
        tot+=mappoint[x][y-1].int_7;
        av=tot/3;
        mappoint[x-1][y].int_7=av;
        mappoint[x][y-1].int_7=av;
        mappoint[x][y].int_7=av+(tot%3);

}

void t_av_r(int x,int y)  // 4
{
	int tot,av;
        tot=mappoint[x][y].int_1;
        tot+=mappoint[x+1][y].int_1;
        av=tot/2;
        mappoint[x+1][y].int_1=av;
        mappoint[x][y].int_1=av+(tot%2);

        tot=mappoint[x][y].int_2;
        tot+=mappoint[x+1][y].int_2;
        av=tot/2;
        mappoint[x+1][y].int_2=av;
        mappoint[x][y].int_2=av+(tot%2);

        tot=mappoint[x][y].int_3;
        tot+=mappoint[x+1][y].int_3;
        av=tot/2;
        mappoint[x+1][y].int_3=av;
        mappoint[x][y].int_3=av+(tot%2);

        tot=mappoint[x][y].int_4;
        tot+=mappoint[x+1][y].int_4;
        av=tot/2;
        mappoint[x+1][y].int_4=av;
        mappoint[x][y].int_4=av+(tot%2);

        tot=mappoint[x][y].int_5;
        tot+=mappoint[x+1][y].int_5;
        av=tot/2;
        mappoint[x+1][y].int_5=av;
        mappoint[x][y].int_5=av+(tot%2);

        tot=mappoint[x][y].int_6;
        tot+=mappoint[x+1][y].int_6;
        av=tot/2;
        mappoint[x+1][y].int_6=av;
        mappoint[x][y].int_6=av+(tot%2);

        tot=mappoint[x][y].int_7;
        tot+=mappoint[x+1][y].int_7;
        av=tot/2;
        mappoint[x+1][y].int_7=av;
        mappoint[x][y].int_7=av+(tot%2);

}

void t_av_lr(int x,int y)  // 5
{
	int tot,av;
        tot=mappoint[x][y].int_1;
	tot+=mappoint[x-1][y].int_1;
        tot+=mappoint[x+1][y].int_1;
        av=tot/3;
	mappoint[x-1][y].int_1=av;
        mappoint[x+1][y].int_1=av;
        mappoint[x][y].int_1=av+(tot%3);

        tot=mappoint[x][y].int_2;
        tot+=mappoint[x-1][y].int_2;
        tot+=mappoint[x+1][y].int_2;
        av=tot/3;
        mappoint[x-1][y].int_2=av;
        mappoint[x+1][y].int_2=av;
        mappoint[x][y].int_2=av+(tot%3);

        tot=mappoint[x][y].int_3;
        tot+=mappoint[x-1][y].int_3;
        tot+=mappoint[x+1][y].int_3;
        av=tot/3;
        mappoint[x-1][y].int_3=av;
        mappoint[x+1][y].int_3=av;
        mappoint[x][y].int_3=av+(tot%3);

        tot=mappoint[x][y].int_4;
        tot+=mappoint[x-1][y].int_4;
        tot+=mappoint[x+1][y].int_4;
        av=tot/3;
        mappoint[x-1][y].int_4=av;
        mappoint[x+1][y].int_4=av;
        mappoint[x][y].int_4=av+(tot%3);

        tot=mappoint[x][y].int_5;
        tot+=mappoint[x-1][y].int_5;
        tot+=mappoint[x+1][y].int_5;
        av=tot/3;
        mappoint[x-1][y].int_5=av;
        mappoint[x+1][y].int_5=av;
        mappoint[x][y].int_5=av+(tot%3);

        tot=mappoint[x][y].int_6;
        tot+=mappoint[x-1][y].int_6;
        tot+=mappoint[x+1][y].int_6;
        av=tot/3;
        mappoint[x-1][y].int_6=av;
        mappoint[x+1][y].int_6=av;
        mappoint[x][y].int_6=av+(tot%3);

        tot=mappoint[x][y].int_7;
        tot+=mappoint[x-1][y].int_7;
        tot+=mappoint[x+1][y].int_7;
        av=tot/3;
        mappoint[x-1][y].int_7=av;
        mappoint[x+1][y].int_7=av;
        mappoint[x][y].int_7=av+(tot%3);

}

void t_av_ur(int x,int y)  // 6
{
	int tot,av;
        tot=mappoint[x][y].int_1;
        tot+=mappoint[x][y-1].int_1;
        tot+=mappoint[x+1][y].int_1;
        av=tot/3;
        mappoint[x][y-1].int_1=av;
        mappoint[x+1][y].int_1=av;
        mappoint[x][y].int_1=av+(tot%3);

        tot=mappoint[x][y].int_2;
        tot+=mappoint[x][y-1].int_2;
        tot+=mappoint[x+1][y].int_2;
        av=tot/3;
        mappoint[x][y-1].int_2=av;
        mappoint[x+1][y].int_2=av;
        mappoint[x][y].int_2=av+(tot%3);

        tot=mappoint[x][y].int_3;
        tot+=mappoint[x][y-1].int_3;
        tot+=mappoint[x+1][y].int_3;
        av=tot/3;
        mappoint[x][y-1].int_3=av;
        mappoint[x+1][y].int_3=av;
        mappoint[x][y].int_3=av+(tot%3);

        tot=mappoint[x][y].int_4;
        tot+=mappoint[x][y-1].int_4;
        tot+=mappoint[x+1][y].int_4;
        av=tot/3;
        mappoint[x][y-1].int_4=av;
        mappoint[x+1][y].int_4=av;
        mappoint[x][y].int_4=av+(tot%3);

        tot=mappoint[x][y].int_5;
        tot+=mappoint[x][y-1].int_5;
        tot+=mappoint[x+1][y].int_5;
        av=tot/3;
        mappoint[x][y-1].int_5=av;
        mappoint[x+1][y].int_5=av;
        mappoint[x][y].int_5=av+(tot%3);

        tot=mappoint[x][y].int_6;
        tot+=mappoint[x][y-1].int_6;
        tot+=mappoint[x+1][y].int_6;
        av=tot/3;
        mappoint[x][y-1].int_6=av;
        mappoint[x+1][y].int_6=av;
        mappoint[x][y].int_6=av+(tot%3);

        tot=mappoint[x][y].int_7;
        tot+=mappoint[x][y-1].int_7;
        tot+=mappoint[x+1][y].int_7;
        av=tot/3;
        mappoint[x][y-1].int_7=av;
        mappoint[x+1][y].int_7=av;
        mappoint[x][y].int_7=av+(tot%3);

}

void t_av_lur(int x,int y)  //  7
{
	int tot,av;
        tot=mappoint[x][y].int_1;
	tot+=mappoint[x-1][y].int_1;
        tot+=mappoint[x][y-1].int_1;
        tot+=mappoint[x+1][y].int_1;
        av=tot/4;
	mappoint[x-1][y].int_1=av;
        mappoint[x][y-1].int_1=av;
        mappoint[x+1][y].int_1=av;
        mappoint[x][y].int_1=av+(tot%4);

        tot=mappoint[x][y].int_2;
        tot+=mappoint[x-1][y].int_2;
        tot+=mappoint[x][y-1].int_2;
        tot+=mappoint[x+1][y].int_2;
        av=tot/4;
        mappoint[x-1][y].int_2=av;
        mappoint[x][y-1].int_2=av;
        mappoint[x+1][y].int_2=av;
        mappoint[x][y].int_2=av+(tot%4);

        tot=mappoint[x][y].int_3;
        tot+=mappoint[x-1][y].int_3;
        tot+=mappoint[x][y-1].int_3;
        tot+=mappoint[x+1][y].int_3;
        av=tot/4;
        mappoint[x-1][y].int_3=av;
        mappoint[x][y-1].int_3=av;
        mappoint[x+1][y].int_3=av;
        mappoint[x][y].int_3=av+(tot%4);

        tot=mappoint[x][y].int_4;
        tot+=mappoint[x-1][y].int_4;
        tot+=mappoint[x][y-1].int_4;
        tot+=mappoint[x+1][y].int_4;
        av=tot/4;
        mappoint[x-1][y].int_4=av;
        mappoint[x][y-1].int_4=av;
        mappoint[x+1][y].int_4=av;
        mappoint[x][y].int_4=av+(tot%4);

        tot=mappoint[x][y].int_5;
        tot+=mappoint[x-1][y].int_5;
        tot+=mappoint[x][y-1].int_5;
        tot+=mappoint[x+1][y].int_5;
        av=tot/4;
        mappoint[x-1][y].int_5=av;
        mappoint[x][y-1].int_5=av;
        mappoint[x+1][y].int_5=av;
        mappoint[x][y].int_5=av+(tot%4);

        tot=mappoint[x][y].int_6;
        tot+=mappoint[x-1][y].int_6;
        tot+=mappoint[x][y-1].int_6;
        tot+=mappoint[x+1][y].int_6;
        av=tot/4;
        mappoint[x-1][y].int_6=av;
        mappoint[x][y-1].int_6=av;
        mappoint[x+1][y].int_6=av;
        mappoint[x][y].int_6=av+(tot%4);

        tot=mappoint[x][y].int_7;
        tot+=mappoint[x-1][y].int_7;
        tot+=mappoint[x][y-1].int_7;
        tot+=mappoint[x+1][y].int_7;
        av=tot/4;
        mappoint[x-1][y].int_7=av;
        mappoint[x][y-1].int_7=av;
        mappoint[x+1][y].int_7=av;
        mappoint[x][y].int_7=av+(tot%4);

}

void t_av_d(int x,int y)  // 8
{
	int tot,av;
        tot=mappoint[x][y].int_1;
        tot+=mappoint[x][y+1].int_1;
        av=tot/2;
        mappoint[x][y+1].int_1=av;
        mappoint[x][y].int_1=av+(tot%2);

        tot=mappoint[x][y].int_2;
        tot+=mappoint[x][y+1].int_2;
        av=tot/2;
        mappoint[x][y+1].int_2=av;
        mappoint[x][y].int_2=av+(tot%2);

        tot=mappoint[x][y].int_3;
        tot+=mappoint[x][y+1].int_3;
        av=tot/2;
        mappoint[x][y+1].int_3=av;
        mappoint[x][y].int_3=av+(tot%2);

        tot=mappoint[x][y].int_4;
        tot+=mappoint[x][y+1].int_4;
        av=tot/2;
        mappoint[x][y+1].int_4=av;
        mappoint[x][y].int_4=av+(tot%2);

        tot=mappoint[x][y].int_5;
        tot+=mappoint[x][y+1].int_5;
        av=tot/2;
        mappoint[x][y+1].int_5=av;
        mappoint[x][y].int_5=av+(tot%2);

        tot=mappoint[x][y].int_6;
        tot+=mappoint[x][y+1].int_6;
        av=tot/2;
        mappoint[x][y+1].int_6=av;
        mappoint[x][y].int_6=av+(tot%2);

        tot=mappoint[x][y].int_7;
        tot+=mappoint[x][y+1].int_7;
        av=tot/2;
        mappoint[x][y+1].int_7=av;
        mappoint[x][y].int_7=av+(tot%2);

}

void t_av_ld(int x,int y)  //  9
{
	int tot,av;
        tot=mappoint[x][y].int_1;
	tot+=mappoint[x-1][y].int_1;
        tot+=mappoint[x][y+1].int_1;
        av=tot/3;
	mappoint[x-1][y].int_1=av;
        mappoint[x][y+1].int_1=av;
        mappoint[x][y].int_1=av+(tot%3);

        tot=mappoint[x][y].int_2;
        tot+=mappoint[x-1][y].int_2;
        tot+=mappoint[x][y+1].int_2;
        av=tot/3;
        mappoint[x-1][y].int_2=av;
        mappoint[x][y+1].int_2=av;
        mappoint[x][y].int_2=av+(tot%3);
  
        tot=mappoint[x][y].int_3;
        tot+=mappoint[x-1][y].int_3;
        tot+=mappoint[x][y+1].int_3;
        av=tot/3;
        mappoint[x-1][y].int_3=av;
        mappoint[x][y+1].int_3=av;
        mappoint[x][y].int_3=av+(tot%3);

        tot=mappoint[x][y].int_4;
        tot+=mappoint[x-1][y].int_4;
        tot+=mappoint[x][y+1].int_4;
        av=tot/3;
        mappoint[x-1][y].int_4=av;
        mappoint[x][y+1].int_4=av;
        mappoint[x][y].int_4=av+(tot%3);

        tot=mappoint[x][y].int_5;
        tot+=mappoint[x-1][y].int_5;
        tot+=mappoint[x][y+1].int_5;
        av=tot/3;
        mappoint[x-1][y].int_5=av;
        mappoint[x][y+1].int_5=av;
        mappoint[x][y].int_5=av+(tot%3);

        tot=mappoint[x][y].int_6;
        tot+=mappoint[x-1][y].int_6;
        tot+=mappoint[x][y+1].int_6;
        av=tot/3;
        mappoint[x-1][y].int_6=av;
        mappoint[x][y+1].int_6=av;
        mappoint[x][y].int_6=av+(tot%3);

        tot=mappoint[x][y].int_7;
        tot+=mappoint[x-1][y].int_7;
        tot+=mappoint[x][y+1].int_7;
        av=tot/3;
        mappoint[x-1][y].int_7=av;
        mappoint[x][y+1].int_7=av;
        mappoint[x][y].int_7=av+(tot%3);

}

void t_av_ud(int x,int y)  //  10
{
	int tot,av;
        tot=mappoint[x][y].int_1;
        tot+=mappoint[x][y-1].int_1;
        tot+=mappoint[x][y+1].int_1;
        av=tot/3;
        mappoint[x][y-1].int_1=av;
        mappoint[x][y+1].int_1=av;
        mappoint[x][y].int_1=av+(tot%3);

        tot=mappoint[x][y].int_2;
        tot+=mappoint[x][y-1].int_2;
        tot+=mappoint[x][y+1].int_2;
        av=tot/3;
        mappoint[x][y-1].int_2=av;
        mappoint[x][y+1].int_2=av;
        mappoint[x][y].int_2=av+(tot%3);

        tot=mappoint[x][y].int_3;
        tot+=mappoint[x][y-1].int_3;
        tot+=mappoint[x][y+1].int_3;
        av=tot/3;
        mappoint[x][y-1].int_3=av;
        mappoint[x][y+1].int_3=av;
        mappoint[x][y].int_3=av+(tot%3);

        tot=mappoint[x][y].int_4;
        tot+=mappoint[x][y-1].int_4;
        tot+=mappoint[x][y+1].int_4;
        av=tot/3;
        mappoint[x][y-1].int_4=av;
        mappoint[x][y+1].int_4=av;
        mappoint[x][y].int_4=av+(tot%3);

        tot=mappoint[x][y].int_5;
        tot+=mappoint[x][y-1].int_5;
        tot+=mappoint[x][y+1].int_5;
        av=tot/3;
        mappoint[x][y-1].int_5=av;
        mappoint[x][y+1].int_5=av;
        mappoint[x][y].int_5=av+(tot%3);

        tot=mappoint[x][y].int_6;
        tot+=mappoint[x][y-1].int_6;
        tot+=mappoint[x][y+1].int_6;
        av=tot/3;
        mappoint[x][y-1].int_6=av;
        mappoint[x][y+1].int_6=av;
        mappoint[x][y].int_6=av+(tot%3);

        tot=mappoint[x][y].int_7;
        tot+=mappoint[x][y-1].int_7;
        tot+=mappoint[x][y+1].int_7;
        av=tot/3;
        mappoint[x][y-1].int_7=av;
        mappoint[x][y+1].int_7=av;
        mappoint[x][y].int_7=av+(tot%3);

}

void t_av_lud(int x,int y)  // 11
{
	int tot,av;
        tot=mappoint[x][y].int_1;
	tot+=mappoint[x-1][y].int_1;
        tot+=mappoint[x][y-1].int_1;
        tot+=mappoint[x][y+1].int_1;
        av=tot/4;
	mappoint[x-1][y].int_1=av;
        mappoint[x][y-1].int_1=av;
        mappoint[x][y+1].int_1=av;
        mappoint[x][y].int_1=av+(tot%4);

        tot=mappoint[x][y].int_2;
        tot+=mappoint[x-1][y].int_2;
        tot+=mappoint[x][y-1].int_2;
        tot+=mappoint[x][y+1].int_2;
        av=tot/4;
        mappoint[x-1][y].int_2=av;
        mappoint[x][y-1].int_2=av;
        mappoint[x][y+1].int_2=av;
        mappoint[x][y].int_2=av+(tot%4);

        tot=mappoint[x][y].int_3;
        tot+=mappoint[x-1][y].int_3;
        tot+=mappoint[x][y-1].int_3;
        tot+=mappoint[x][y+1].int_3;
        av=tot/4;
        mappoint[x-1][y].int_3=av;
        mappoint[x][y-1].int_3=av;
        mappoint[x][y+1].int_3=av;
        mappoint[x][y].int_3=av+(tot%4);

        tot=mappoint[x][y].int_4;
        tot+=mappoint[x-1][y].int_4;
        tot+=mappoint[x][y-1].int_4;
        tot+=mappoint[x][y+1].int_4;
        av=tot/4;
        mappoint[x-1][y].int_4=av;
        mappoint[x][y-1].int_4=av;
        mappoint[x][y+1].int_4=av;
        mappoint[x][y].int_4=av+(tot%4);

        tot=mappoint[x][y].int_5;
        tot+=mappoint[x-1][y].int_5;
        tot+=mappoint[x][y-1].int_5;
        tot+=mappoint[x][y+1].int_5;
        av=tot/4;
        mappoint[x-1][y].int_5=av;
        mappoint[x][y-1].int_5=av;
        mappoint[x][y+1].int_5=av;
        mappoint[x][y].int_5=av+(tot%4);

        tot=mappoint[x][y].int_6;
        tot+=mappoint[x-1][y].int_6;
        tot+=mappoint[x][y-1].int_6;
        tot+=mappoint[x][y+1].int_6;
        av=tot/4;
        mappoint[x-1][y].int_6=av;
        mappoint[x][y-1].int_6=av;
        mappoint[x][y+1].int_6=av;
        mappoint[x][y].int_6=av+(tot%4);

        tot=mappoint[x][y].int_7;
        tot+=mappoint[x-1][y].int_7;
        tot+=mappoint[x][y-1].int_7;
        tot+=mappoint[x][y+1].int_7;
        av=tot/4;
        mappoint[x-1][y].int_7=av;
        mappoint[x][y-1].int_7=av;
        mappoint[x][y+1].int_7=av;
        mappoint[x][y].int_7=av+(tot%4);

}

void t_av_rd(int x,int y)  //  12
{
	int tot,av;
        tot=mappoint[x][y].int_1;
        tot+=mappoint[x+1][y].int_1;
        tot+=mappoint[x][y+1].int_1;
        av=tot/3;
        mappoint[x+1][y].int_1=av;
        mappoint[x][y+1].int_1=av;
        mappoint[x][y].int_1=av+(tot%3);

        tot=mappoint[x][y].int_2;
        tot+=mappoint[x+1][y].int_2;
        tot+=mappoint[x][y+1].int_2;
        av=tot/3;
        mappoint[x+1][y].int_2=av;
        mappoint[x][y+1].int_2=av;
        mappoint[x][y].int_2=av+(tot%3);

        tot=mappoint[x][y].int_3;
        tot+=mappoint[x+1][y].int_3;
        tot+=mappoint[x][y+1].int_3;
        av=tot/3;
        mappoint[x+1][y].int_3=av;
        mappoint[x][y+1].int_3=av;
        mappoint[x][y].int_3=av+(tot%3);

        tot=mappoint[x][y].int_4;
        tot+=mappoint[x+1][y].int_4;
        tot+=mappoint[x][y+1].int_4;
        av=tot/3;
        mappoint[x+1][y].int_4=av;
        mappoint[x][y+1].int_4=av;
        mappoint[x][y].int_4=av+(tot%3);

        tot=mappoint[x][y].int_5;
        tot+=mappoint[x+1][y].int_5;
        tot+=mappoint[x][y+1].int_5;
        av=tot/3;
        mappoint[x+1][y].int_5=av;
        mappoint[x][y+1].int_5=av;
        mappoint[x][y].int_5=av+(tot%3);

        tot=mappoint[x][y].int_6;
        tot+=mappoint[x+1][y].int_6;
        tot+=mappoint[x][y+1].int_6;
        av=tot/3;
        mappoint[x+1][y].int_6=av;
        mappoint[x][y+1].int_6=av;
        mappoint[x][y].int_6=av+(tot%3);

        tot=mappoint[x][y].int_7;
        tot+=mappoint[x+1][y].int_7;
        tot+=mappoint[x][y+1].int_7;
        av=tot/3;
        mappoint[x+1][y].int_7=av;
        mappoint[x][y+1].int_7=av;
        mappoint[x][y].int_7=av+(tot%3);

}

void t_av_lrd(int x,int y)  //  13
{
	int tot,av;
        tot=mappoint[x][y].int_1;
	tot+=mappoint[x-1][y].int_1;
        tot+=mappoint[x+1][y].int_1;
        tot+=mappoint[x][y+1].int_1;
        av=tot/4;
	mappoint[x-1][y].int_1=av;
        mappoint[x+1][y].int_1=av;
        mappoint[x][y+1].int_1=av;
        mappoint[x][y].int_1=av+(tot%4);

        tot=mappoint[x][y].int_2;
        tot+=mappoint[x-1][y].int_2;
        tot+=mappoint[x+1][y].int_2;
        tot+=mappoint[x][y+1].int_2;
        av=tot/4;
        mappoint[x-1][y].int_2=av;
        mappoint[x+1][y].int_2=av;
        mappoint[x][y+1].int_2=av;
        mappoint[x][y].int_2=av+(tot%4);

        tot=mappoint[x][y].int_3;
        tot+=mappoint[x-1][y].int_3;
        tot+=mappoint[x+1][y].int_3;
        tot+=mappoint[x][y+1].int_3;
        av=tot/4;
        mappoint[x-1][y].int_3=av;
        mappoint[x+1][y].int_3=av;
        mappoint[x][y+1].int_3=av;
        mappoint[x][y].int_3=av+(tot%4);

        tot=mappoint[x][y].int_4;
        tot+=mappoint[x-1][y].int_4;
        tot+=mappoint[x+1][y].int_4;
        tot+=mappoint[x][y+1].int_4;
        av=tot/4;
        mappoint[x-1][y].int_4=av;
        mappoint[x+1][y].int_4=av;
        mappoint[x][y+1].int_4=av;
        mappoint[x][y].int_4=av+(tot%4);

        tot=mappoint[x][y].int_5;
        tot+=mappoint[x-1][y].int_5;
        tot+=mappoint[x+1][y].int_5;
        tot+=mappoint[x][y+1].int_5;
        av=tot/4;
        mappoint[x-1][y].int_5=av;
        mappoint[x+1][y].int_5=av;
        mappoint[x][y+1].int_5=av;
        mappoint[x][y].int_5=av+(tot%4);

        tot=mappoint[x][y].int_6;
        tot+=mappoint[x-1][y].int_6;
        tot+=mappoint[x+1][y].int_6;
        tot+=mappoint[x][y+1].int_6;
        av=tot/4;
        mappoint[x-1][y].int_6=av;
        mappoint[x+1][y].int_6=av;
        mappoint[x][y+1].int_6=av;
        mappoint[x][y].int_6=av+(tot%4);

        tot=mappoint[x][y].int_7;
        tot+=mappoint[x-1][y].int_7;
        tot+=mappoint[x+1][y].int_7;
        tot+=mappoint[x][y+1].int_7;
        av=tot/4;
        mappoint[x-1][y].int_7=av;
        mappoint[x+1][y].int_7=av;
        mappoint[x][y+1].int_7=av;
        mappoint[x][y].int_7=av+(tot%4);

}

void t_av_urd(int x,int y)  //  14
{
	int tot,av;
        tot=mappoint[x][y].int_1;
        tot+=mappoint[x][y-1].int_1;
        tot+=mappoint[x+1][y].int_1;
        tot+=mappoint[x][y+1].int_1;
        av=tot/4;
        mappoint[x][y-1].int_1=av;
        mappoint[x+1][y].int_1=av;
        mappoint[x][y+1].int_1=av;
        mappoint[x][y].int_1=av+(tot%4);

        tot=mappoint[x][y].int_2;
        tot+=mappoint[x][y-1].int_2;
        tot+=mappoint[x+1][y].int_2;
        tot+=mappoint[x][y+1].int_2;
        av=tot/4;
        mappoint[x][y-1].int_2=av;
        mappoint[x+1][y].int_2=av;
        mappoint[x][y+1].int_2=av;
        mappoint[x][y].int_2=av+(tot%4);

        tot=mappoint[x][y].int_3;
        tot+=mappoint[x][y-1].int_3;
        tot+=mappoint[x+1][y].int_3;
        tot+=mappoint[x][y+1].int_3;
        av=tot/4;
        mappoint[x][y-1].int_3=av;
        mappoint[x+1][y].int_3=av;
        mappoint[x][y+1].int_3=av;
        mappoint[x][y].int_3=av+(tot%4);

        tot=mappoint[x][y].int_4;
        tot+=mappoint[x][y-1].int_4;
        tot+=mappoint[x+1][y].int_4;
        tot+=mappoint[x][y+1].int_4;
        av=tot/4;
        mappoint[x][y-1].int_4=av;
        mappoint[x+1][y].int_4=av;
        mappoint[x][y+1].int_4=av;
        mappoint[x][y].int_4=av+(tot%4);

        tot=mappoint[x][y].int_5;
        tot+=mappoint[x][y-1].int_5;
        tot+=mappoint[x+1][y].int_5;
        tot+=mappoint[x][y+1].int_5;
        av=tot/4;
        mappoint[x][y-1].int_5=av;
        mappoint[x+1][y].int_5=av;
        mappoint[x][y+1].int_5=av;
        mappoint[x][y].int_5=av+(tot%4);

        tot=mappoint[x][y].int_6;
        tot+=mappoint[x][y-1].int_6;
        tot+=mappoint[x+1][y].int_6;
        tot+=mappoint[x][y+1].int_6;
        av=tot/4;
        mappoint[x][y-1].int_6=av;
        mappoint[x+1][y].int_6=av;
        mappoint[x][y+1].int_6=av;
        mappoint[x][y].int_6=av+(tot%4);

        tot=mappoint[x][y].int_7;
        tot+=mappoint[x][y-1].int_7;
        tot+=mappoint[x+1][y].int_7;
        tot+=mappoint[x][y+1].int_7;
        av=tot/4;
        mappoint[x][y-1].int_7=av;
        mappoint[x+1][y].int_7=av;
        mappoint[x][y+1].int_7=av;
        mappoint[x][y].int_7=av+(tot%4);

}

void t_av_lurd(int x,int y)  //  15
{
	int tot,av;
        tot=mappoint[x][y].int_1;
	tot+=mappoint[x-1][y].int_1;
        tot+=mappoint[x][y-1].int_1;
        tot+=mappoint[x+1][y].int_1;
        tot+=mappoint[x][y+1].int_1;
        av=tot/5;
	mappoint[x-1][y].int_1=av;
        mappoint[x][y-1].int_1=av;
        mappoint[x+1][y].int_1=av;
        mappoint[x][y+1].int_1=av;
        mappoint[x][y].int_1=av+(tot%5);

        tot=mappoint[x][y].int_2;
        tot+=mappoint[x-1][y].int_2;
        tot+=mappoint[x][y-1].int_2;
        tot+=mappoint[x+1][y].int_2;
        tot+=mappoint[x][y+1].int_2;
        av=tot/5;
        mappoint[x-1][y].int_2=av;
        mappoint[x][y-1].int_2=av;
        mappoint[x+1][y].int_2=av;
        mappoint[x][y+1].int_2=av;
        mappoint[x][y].int_2=av+(tot%5);

        tot=mappoint[x][y].int_3;
        tot+=mappoint[x-1][y].int_3;
        tot+=mappoint[x][y-1].int_3;
        tot+=mappoint[x+1][y].int_3;
        tot+=mappoint[x][y+1].int_3;
        av=tot/5;
        mappoint[x-1][y].int_3=av;
        mappoint[x][y-1].int_3=av;
        mappoint[x+1][y].int_3=av;
        mappoint[x][y+1].int_3=av;
        mappoint[x][y].int_3=av+(tot%5);

        tot=mappoint[x][y].int_4;
        tot+=mappoint[x-1][y].int_4;
        tot+=mappoint[x][y-1].int_4;
        tot+=mappoint[x+1][y].int_4;
        tot+=mappoint[x][y+1].int_4;
        av=tot/5;
        mappoint[x-1][y].int_4=av;
        mappoint[x][y-1].int_4=av;
        mappoint[x+1][y].int_4=av;
        mappoint[x][y+1].int_4=av;
        mappoint[x][y].int_4=av+(tot%5);

        tot=mappoint[x][y].int_5;
        tot+=mappoint[x-1][y].int_5;
        tot+=mappoint[x][y-1].int_5;
        tot+=mappoint[x+1][y].int_5;
        tot+=mappoint[x][y+1].int_5;
        av=tot/5;
        mappoint[x-1][y].int_5=av;
        mappoint[x][y-1].int_5=av;
        mappoint[x+1][y].int_5=av;
        mappoint[x][y+1].int_5=av;
        mappoint[x][y].int_5=av+(tot%5);

        tot=mappoint[x][y].int_6;
        tot+=mappoint[x-1][y].int_6;
        tot+=mappoint[x][y-1].int_6;
        tot+=mappoint[x+1][y].int_6;
        tot+=mappoint[x][y+1].int_6;
        av=tot/5;
        mappoint[x-1][y].int_6=av;
        mappoint[x][y-1].int_6=av;
        mappoint[x+1][y].int_6=av;
        mappoint[x][y+1].int_6=av;
        mappoint[x][y].int_6=av+(tot%5);

        tot=mappoint[x][y].int_7;
        tot+=mappoint[x-1][y].int_7;
        tot+=mappoint[x][y-1].int_7;
        tot+=mappoint[x+1][y].int_7;
        tot+=mappoint[x][y+1].int_7;
        av=tot/5;
        mappoint[x-1][y].int_7=av;
        mappoint[x][y-1].int_7=av;
        mappoint[x+1][y].int_7=av;
        mappoint[x][y+1].int_7=av;
        mappoint[x][y].int_7=av+(tot%5);

}

#else

// 30. Oct 1996 -- Thomas M. Ott <Labalutsch@aol.com> (ThMO)

void general_transport(struct MAPPOINT  *map,int  *pol
	,int max_waste ,int *waste_count)
//  struct MAPPOINT  *map;
//  int              *pol, *waste_count;
//  const int        max_waste;
{
  int  tot, av, *base, xm1, xp1, ym1, yp1;

  /* 30. Oct 1996:
   * we'll use a loop with pointers here instead of doin' each
   * operation by hand.  this reduces code complexity and should
   * lead to a higher cache hit ratio - theoretically
   * (ThMO)
   *
   * 12. Dec 1996:
   * as this is a heavy used routine, another speedup improvement is
   * needed.  we'll now use 1 pointer, which will be incremented and
   * 1 .. 4 constant indices, which replaces the old pointer-pure
   * version.
   * advantages:  elimination of unnecessary pointer increments.
   * Note:  this *only* works, if the related addresses use one and the
   *        same address space - which is naturally for 2-dimensional
   *        arrays.
   * (ThMO)
   */

  base= &map->int_1;
  switch ( map->flags & 0x0F)
    {
      case 0:
        return ;

      case 1:           /* inlined t_av_l() -- (ThMO) */
        xm1= &map[ -WORLD_SIDE_LEN].int_1- base;
        do
          {
            tot= *base+ base[ xm1];
            av= tot/ 2;
            base[ xm1]= av;
            *base++= av+ tot % 2;
          }
        while ( base <= &map->int_7);
        break;

      case 2:           /* inlined t_av_u() -- (ThMO) */
        ym1= &map[ -1].int_1- base;
        do
          {
            tot= *base+ base[ ym1];
            av= tot/ 2;
            base[ ym1]= av;
            *base++= av+ tot % 2;
          }
        while ( base <= &map->int_7);
        break;

      case 3:           /* inlined t_av_lu() -- (ThMO) */
        xm1= &map[ -WORLD_SIDE_LEN].int_1- base;
        ym1= &map[ -1].int_1- base;
        do
          {
            tot= *base+ base[ xm1]+ base[ ym1];
            av= tot/ 3;
            base[ xm1]= base[ ym1]= av;
            *base++= av+ tot % 3;
          }
        while ( base <= &map->int_7);
        break;

      case 4:           /* inlined t_av_r() -- (ThMO) */
        xp1= &map[ WORLD_SIDE_LEN].int_1- base;
        do
          {
            tot= *base+ base[ xp1];
            av= tot/ 2;
            base[ xp1]= av;
            *base++= av+ tot % 2;
          }
        while ( base <= &map->int_7);
        break;

      case 5:           /* inlined t_av_lr() -- (ThMO) */
        xm1= &map[ -WORLD_SIDE_LEN].int_1- base;
        xp1= &map[ WORLD_SIDE_LEN].int_1- base;
        do
          {
            tot= *base+ base[ xm1]+ base[ xp1];
            av= tot/ 3;
            base[ xm1]= base[ xp1]= av;
            *base++= av+ tot % 3;
          }
        while ( base <= &map->int_7);
        break;

      case 6:           /* inline t_av_ur() -- (ThMO) */
        ym1= &map[ -1].int_1- base;
        xp1= &map[ WORLD_SIDE_LEN].int_1- base;
        do
          {
            tot= *base+ base[ ym1]+ base[ xp1];
            av= tot/ 3;
            base[ ym1]= base[ xp1]= av;
            *base++= av+ tot % 3;
          }
        while ( base <= &map->int_7);
        break;

      case 7:           /* inlined t_av_lur() -- (ThMO) */
        xm1= &map[ -WORLD_SIDE_LEN].int_1- base;
        ym1= &map[ -1].int_1 -base;
        xp1= &map[ WORLD_SIDE_LEN].int_1- base;
        do
          {
            tot= *base+ base[ xm1]+ base[ ym1]+ base[ xp1];
            av= tot/ 4;
            base[ xm1]= base[ ym1]= base[ xp1]= av;
            *base++= av+ tot % 4;
          }
        while ( base <= &map->int_7);
        break;

      case 8:           /* inlined t_av_d() -- (ThMO) */
        yp1= &map[ 1].int_1- base;
        do
          {
            tot= *base+ base[ yp1];
            av= tot/ 2;
            base[ yp1]= av;
            *base++= av+ tot % 2;
          }
        while ( base <= &map->int_7);
        break;

      case 9:           /* inlined t_av_ld() -- (ThMO) */
        xm1= &map[ -WORLD_SIDE_LEN].int_1- base;
        yp1= &map[ 1].int_1- base;
        do
          {
            tot= *base+ base[ xm1]+ base[ yp1];
            av= tot/ 3;
            base[ xm1]= base[ yp1]= av;
            *base++= av+ tot % 3;
          }
        while ( base <= &map->int_7);
        break;

      case 10:          /* inlined t_av_ud() -- (ThMO) */
        ym1= &map[ -1].int_1- base;
        yp1= &map[ 1].int_1- base;
        do
          {
            tot= *base+ base[ ym1]+ base[ yp1];
            av= tot/ 3;
            base[ ym1]= base[ yp1]= av;
            *base++= av+ tot % 3;
          }
        while ( base <= &map->int_7);
        break;

      case 11:          /* inlined t_av_lud() -- (ThMO) */
        xm1= &map[ -WORLD_SIDE_LEN].int_1- base;
        ym1= &map[ -1].int_1- base;
        yp1= &map[ 1].int_1- base;
        do
          {
            tot= *base+ base[ xm1]+ base[ ym1]+ base[ yp1];
            av= tot/ 4;
            base[ xm1]= base[ ym1]= base[ yp1]= av;
            *base++= av+ tot % 4;
          }
        while ( base <= &map->int_7);
        break;

      case 12:          /* inlined t_av_rd() -- (ThMO) */
        xp1= &map[ WORLD_SIDE_LEN].int_1- base;
        yp1= &map[ 1].int_1- base;
        do
          {
            tot= *base+ base[ xp1]+ base[ yp1];
            av= tot/ 3;
            base[ xp1]= base[ yp1]= av;
            *base++= av+ tot % 3;
          }
        while ( base <= &map->int_7);
        break;

      case 13:          /* inlined t_av_lrd() -- (ThMO) */
        xm1= &map[ -WORLD_SIDE_LEN].int_1- base;
        xp1= &map[ WORLD_SIDE_LEN].int_1- base;
        yp1= &map[ 1].int_1- base;
        do
          {
            tot= *base+ base[ xm1]+ base[ xp1]+ base[ yp1];
            av= tot/ 4;
            base[ xm1]= base[ xp1]= base[ yp1]= av;
            *base++= av+ tot % 4;
          }
        while ( base <= &map->int_7);
        break;

      case 14:          /* inlined t_av_urd() -- (ThMO) */
        ym1= &map[ -1].int_1- base;
        xp1= &map[ WORLD_SIDE_LEN].int_1- base;
        yp1= &map[ 1].int_1- base;
        do
          {
            tot= *base+ base[ ym1]+ base[ xp1]+ base[ yp1];
            av= tot/ 4;
            base[ ym1]= base[ xp1]= base[ yp1]= av;
            *base++= av+ tot % 4;
          }
        while ( base <= &map->int_7);
        break;

      case 15:          /* inlined t_av_lurd() -- (ThMO) */
        xm1= &map[ -WORLD_SIDE_LEN].int_1- base;
        ym1= &map[ -1].int_1- base;
        xp1= &map[ WORLD_SIDE_LEN].int_1- base;
        yp1= &map[ 1].int_1- base;
        do
          {
            tot= *base+ base[ xm1]+ base[ ym1]+ base[ xp1]+ base[ yp1];
            av= tot/ 5;
            base[ xm1]= base[ ym1]= base[ xp1]= base[ yp1]= av;
            *base++= av+ tot % 5;
          }
        while ( base <= &map->int_7);
        break;
    }
  if ( *--base >= max_waste)
    {
      *base -= WASTE_BURN_ON_TRANSPORT;
      ++*pol;
      if ( *waste_count > TRANSPORT_BURN_WASTE_COUNT)
        {
          mini_screen_flags= MINI_SCREEN_POL_FLAG;
          *waste_count= 0;
        }
      else
        ++*waste_count;
    }
}

#endif


