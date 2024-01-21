// market.cxx	 part of lin-city
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
#include "market.h"


int get_jobs(int x,int y,int jobs)  //  
{
        int q;
        if (numof_markets>0)
        {
	        for (q=0;q<numof_markets;q++)
	        {
	                if ((abs(marketx[q]-x)<MARKET_RANGE
	                        && abs(markety[q]-y)<MARKET_RANGE
	                        && (mappoint[marketx[q]][markety[q]].int_2
	                                > (3*jobs/2))))
	                {
				mappoint[marketx[q]][markety[q]].int_2-=jobs;
				income_tax+=jobs;
	                        return(1);
	                }
	        }
	}
	if (get_stuff(x,y,jobs,T_JOBS)!=0)
	{
		income_tax+=jobs;
		return(1);
	}
        return(0);
}

int put_jobs(int x,int y,int jobs)  //
{
        int q;
        if (numof_markets>0)
        {
	        for (q=0;q<numof_markets;q++)
	        {
	                if (mappoint[marketx[q]][markety[q]].int_2
	                        >=(MAX_JOBS_IN_MARKET-jobs))
	                        continue;
	                if (abs(marketx[q]-x)<EMPLOYER_RANGE
	                        && abs(markety[q]-y)<EMPLOYER_RANGE)
	                {
	                        mappoint[marketx[q]][markety[q]].int_2+=jobs;
	                        return(1);
	                }
	        }
	}
	if (put_stuff(x,y,jobs,T_JOBS)!=0)
		return(1);
        return(0);
}

int get_food(int x,int y,int food)  
{
        int q;
        if (numof_markets>0)
        {
	        for (q=0;q<numof_markets;q++)
	        {
	                if ((abs(marketx[q]-x)<MARKET_RANGE)
	                        && (abs(markety[q]-y)<MARKET_RANGE)
	                        && (mappoint[marketx[q]][markety[q]].int_1
	                                > food))
	                {
				mappoint[marketx[q]][markety[q]].int_1-=food;
	                        return(1);
	                }
	        }
	}
	if (get_stuff(x,y,food,T_FOOD)!=0)
		return(1);
        return(0);
}

int put_food(int x,int y,int food) 
{
        int q;
        if (numof_markets>0)
        {
	        for (q=0;q<numof_markets;q++)
        	{
			if (mappoint[marketx[q]][markety[q]].int_1
				>=(MAX_FOOD_IN_MARKET-food))
				continue;
        	        if ((abs(marketx[q]-x)<ORG_FARM_RANGE)
				&& (abs(markety[q]-y)<ORG_FARM_RANGE)) 
        	        {
				mappoint[marketx[q]][markety[q]].int_1+=food;
        	                return(1);
        	        }
        	}
	}
	if (put_stuff(x,y,food,T_FOOD)!=0)
		return(1);
        return(0);
}


int get_goods(int x,int y,int goods)
{
        int q;
        if (numof_markets>0)
        {
	        for (q=0;q<numof_markets;q++)
	        {
	                if (abs(marketx[q]-x)<MARKET_RANGE
	                        && abs(markety[q]-y)<MARKET_RANGE
	                        && (mappoint[marketx[q]][markety[q]].int_4
	                                > goods))
	                {
	                        mappoint[marketx[q]][markety[q]].int_4-=goods;
				goods_tax+=goods;
				goods_used+=goods;
	// make the waste here.
				mappoint[marketx[q]][markety[q]].int_7+=goods/3;
	//			waste_goods+=goods;
	//			if (waste_goods>MAX_WASTE_GOODS)
	//				waste_goods=MAX_WASTE_GOODS;
	                        return(1);
	                }
		}
	}
	if (get_stuff(x,y,goods,T_GOODS)!=0)
	{
		put_stuff(x,y,goods/3,T_WASTE);
		goods_tax+=goods;
		goods_used+=goods;
        	return(1);
	}
        return(0);
}

int put_goods(int x,int y,int goods)
{
        int q;
        if (numof_markets>0)
        {
	        for (q=0;q<numof_markets;q++)
	        {
	                if (mappoint[marketx[q]][markety[q]].int_4
	                        >=(MAX_GOODS_IN_MARKET-goods))
	                        continue;
	                if ((abs(marketx[q]-x)<MARKET_RANGE)
	                        && (abs(markety[q]-y)<MARKET_RANGE))
	                {
	                        mappoint[marketx[q]][markety[q]].int_4+=goods;
	                        return(1);
	                }
	        }
	}
	if (put_stuff(x,y,goods,T_GOODS)!=0)
		return(1);
        return(0);
}

int put_waste(int x,int y,int waste)
{
        int q;
        if (numof_markets>0)
        {
	        for (q=0;q<numof_markets;q++)
	        {
	                if (mappoint[marketx[q]][markety[q]].int_7
	                        >=(MAX_WASTE_IN_MARKET-waste))
	                        continue;
	                if ((abs(marketx[q]-x)<MARKET_RANGE)
	                        && (abs(markety[q]-y)<MARKET_RANGE))
	                {
	                        mappoint[marketx[q]][markety[q]].int_7+=waste;
	                        return(1);
	                }
	        }
	}
	if (put_stuff(x,y,waste,T_WASTE)!=0)
		return(1);
        return(0);
}

int get_waste(int x,int y,int waste)
{
        int q;
        if (numof_markets>0)
        {
	        for (q=0;q<numof_markets;q++)
	        {
	                if ((abs(marketx[q]-x)<MARKET_RANGE)
	                        && (abs(markety[q]-y)<MARKET_RANGE)
	                        && (mappoint[marketx[q]][markety[q]].int_7
	                                > waste))
	                {
	                        mappoint[marketx[q]][markety[q]].int_7-=waste;
	                        return(1);
	                }
	        }
	}
	if (get_stuff(x,y,waste,T_WASTE)!=0)
		return(1);
        return(0);
}


int get_steel(int x,int y,int steel)
{
        int q;
        if (numof_markets>0)
        {
	        for (q=0;q<numof_markets;q++)
	        {
	                if ((abs(marketx[q]-x)<MARKET_RANGE)
	                        && (abs(markety[q]-y)<MARKET_RANGE)
	                        && (mappoint[marketx[q]][markety[q]].int_6
	                                > steel))
	                {
	                        mappoint[marketx[q]][markety[q]].int_6-=steel;
	                        return(1);
	                }
	        }
	}
	if (get_stuff(x,y,steel,T_STEEL)!=0)
		return(1);
        return(0);
}

int put_steel(int x,int y,int steel)
{
        int q;
        if (numof_markets>0)
        {
	        for (q=0;q<numof_markets;q++)
	        {
	                if (mappoint[marketx[q]][markety[q]].int_6
	                        >=(MAX_STEEL_IN_MARKET-steel))
	                        continue;
	                if ((abs(marketx[q]-x)<MARKET_RANGE)
	                        && (abs(markety[q]-y)<MARKET_RANGE))
	                {
	                        mappoint[marketx[q]][markety[q]].int_6+=steel;
	                        return(1);
	                }
	        }
	}
        if (put_stuff(x,y,steel,T_STEEL)!=0)
                return(1);
        return(0);
}


int get_ore(int x,int y,int ore)
{
        int q;
        if (numof_markets>0)
        {
	        for (q=0;q<numof_markets;q++)
	        {
	                if ((abs(marketx[q]-x)<MARKET_RANGE)
	                        && (abs(markety[q]-y)<MARKET_RANGE)
	                        && (mappoint[marketx[q]][markety[q]].int_5
	                                > ore))
	                {
	                        mappoint[marketx[q]][markety[q]].int_5-=ore;
	                        return(1);
	                }
	        }
	}
	if (get_stuff(x,y,ore,T_ORE)!=0)
		return(1);
        return(0);
}

int put_ore(int x,int y,int ore)
{
        int q;
        if (numof_markets>0)
        {
	        for (q=0;q<numof_markets;q++)
	        {
	                if (mappoint[marketx[q]][markety[q]].int_5
	                        >=(MAX_ORE_IN_MARKET-ore))
	                        continue;
	                if ((abs(marketx[q]-x)<MARKET_RANGE)
	                        && (abs(markety[q]-y)<MARKET_RANGE))
	                {
	                        mappoint[marketx[q]][markety[q]].int_5+=ore;
	                        return(1);
	                }
	        }
	}
        if (put_stuff(x,y,ore,T_ORE)!=0)
                return(1);
        return(0);
}


int get_coal(int x,int y,int coal)
{
        int q;
        if (numof_markets>0)
        {
	        for (q=0;q<numof_markets;q++)
	        {
	                if ((abs(marketx[q]-x)<MARKET_RANGE)
	                        && (abs(markety[q]-y)<MARKET_RANGE)
	                        && (mappoint[marketx[q]][markety[q]].int_3
	                                > coal))
	                {
	                        mappoint[marketx[q]][markety[q]].int_3-=coal;
	                        return(1);
	                }
	        }
	}
	if (get_stuff(x,y,coal,T_COAL)!=0)
		return(1);
        return(0);
}

int put_coal(int x,int y,int coal)
{
        int q;
        if (numof_markets>0)
        {
	        for (q=0;q<numof_markets;q++)
	        {
	                if (mappoint[marketx[q]][markety[q]].int_3
	                        >=(MAX_COAL_IN_MARKET-coal))
	                        continue;
	                if ((abs(marketx[q]-x)<MARKET_RANGE)
	                        && (abs(markety[q]-y)<MARKET_RANGE))
	                {
	                        mappoint[marketx[q]][markety[q]].int_3+=coal;
	                        return(1);
	                }
	        }
	}
        if (put_stuff(x,y,coal,T_COAL)!=0)
                return(1);
        return(0);
}


int add_a_market(int x,int y)   // add to marketx markety to list
{
        if (numof_markets>=MAX_NUMOF_MARKETS)
                return(0);
        marketx[numof_markets]=x;
        markety[numof_markets]=y;
        numof_markets++;
// oh dear. Got to bootstap markets with jobs, otherwise power won't work
	mappoint[x][y].int_2=2000;
        return(1);
}

void remove_a_market(int x,int y)
{
	int q;
	for (q=0;q<numof_markets;q++)
		if (marketx[q]==x && markety[q]==y)
			break;
	for (;q<numof_markets;q++)
	{
		marketx[q]=marketx[q+1];
		markety[q]=markety[q+1];
	}
	numof_markets--;
}	

void do_market(int x,int y)
{
//  int_1 contains the food it holds
//  int_2 contains the jobs
//  int_3 contains the coal
//  int_4 contains the goods
//  int_5 contains the ore
//  int_6 contains the steel
//  int_7 contains the waste

	int extra_jobs=0;

	shuffle_markets();

	if (x>0 && (mappoint[x-1][y].flags & FLAG_IS_TRANSPORT)!=0)
		extra_jobs+=deal_with_transport(x,y,x-1,y);
        if (x>0 && (mappoint[x-1][y+1].flags & FLAG_IS_TRANSPORT)!=0)
                extra_jobs+=deal_with_transport(x,y,x-1,y+1);
        if (y>0 && (mappoint[x][y-1].flags & FLAG_IS_TRANSPORT)!=0)
                extra_jobs+=deal_with_transport(x,y,x,y-1);
        if (y>0 && (mappoint[x+1][y-1].flags & FLAG_IS_TRANSPORT)!=0)
                extra_jobs+=deal_with_transport(x,y,x+1,y-1);
	if (x<WORLD_SIDE_LEN-2
		&& (mappoint[x+2][y].flags & FLAG_IS_TRANSPORT)!=0)
		extra_jobs+=deal_with_transport(x,y,x+2,y);
        if (x<WORLD_SIDE_LEN-2
                && (mappoint[x+2][y+1].flags & FLAG_IS_TRANSPORT)!=0)
                extra_jobs+=deal_with_transport(x,y,x+2,y+1);
        if (y<WORLD_SIDE_LEN-2
                && (mappoint[x][y+2].flags & FLAG_IS_TRANSPORT)!=0)
                extra_jobs+=deal_with_transport(x,y,x,y+2);
        if (y<WORLD_SIDE_LEN-2
                && (mappoint[x+1][y+2].flags & FLAG_IS_TRANSPORT)!=0)
                extra_jobs+=deal_with_transport(x,y,x+1,y+2);

	if (mappoint[x][y].int_1>MAX_FOOD_IN_MARKET)
		mappoint[x][y].int_1=MAX_FOOD_IN_MARKET;
	if (mappoint[x][y].int_2>MAX_JOBS_IN_MARKET)
		mappoint[x][y].int_2=MAX_JOBS_IN_MARKET;
        if (mappoint[x][y].int_4>MAX_GOODS_IN_MARKET)
                mappoint[x][y].int_4=MAX_GOODS_IN_MARKET;

// now choose a graphic only dependent on food (for now anyway)
	if (total_time%25==17)
	{
		if (mappoint[x][y].int_1<=0)
		{
			if (mappoint[x][y].int_2>0)
				mappointtype[x][y]=CST_MARKET_LOW;
			else
				mappointtype[x][y]=CST_MARKET_EMPTY;
		}
		else if (mappoint[x][y].int_1<(MARKET_FOOD_SEARCH_TRIGGER/2))
			mappointtype[x][y]=CST_MARKET_LOW;
		else if (mappoint[x][y].int_1
				<(MAX_FOOD_IN_MARKET-MAX_FOOD_IN_MARKET/4))
			mappointtype[x][y]=CST_MARKET_MED;
		else
			mappointtype[x][y]=CST_MARKET_FULL;
	}
	food_in_markets+=mappoint[x][y].int_1;
// now employ some people
	get_jobs(x,y,1+(extra_jobs/5));
// this is for the  pbar indicators
	jobs_in_markets+=mappoint[x][y].int_2;
	coal_in_markets+=mappoint[x][y].int_3;
	goods_in_markets+=mappoint[x][y].int_4;
	ore_in_markets+=mappoint[x][y].int_5;
	steel_in_markets+=mappoint[x][y].int_6;
}

void shuffle_markets(void)
{
	register int x;
	int q,r,m;
	m=(numof_markets/4)+1;
	for (x=0;x<m;x++)
	{
		r=rand()%numof_markets;
		if (r==x)
			continue;
		q=marketx[x];
		marketx[x]=marketx[r];
		marketx[r]=q;
		q=markety[x];
		markety[x]=markety[r];
		markety[r]=q;
	}
}

/* ************

int deal_with_transport(int x,int y,int tx,int ty)
{
	int extra_jobs=0,flags;
	flags=mappoint[x][y].flags;
// tracks
	if (main_types[mappointtype[tx][ty]].group==GROUP_TRACK)
	{
// food
		if ( ((flags & FLAG_MB_FOOD)!=0) &&
			(mappoint[x][y].int_1<MARKET_FOOD_SEARCH_TRIGGER
			|| (mappoint[tx][ty].int_1*10)>(MAX_FOOD_ON_TRACK*9))
			&& mappoint[tx][ty].int_1>0
			&& mappoint[x][y].int_1<(MAX_FOOD_IN_MARKET
				-MAX_FOOD_IN_MARKET/10))
		{
			mappoint[x][y].int_1+=(mappoint[tx][ty].int_1/2)
				+(mappoint[tx][ty].int_1%2);
			mappoint[tx][ty].int_1/=2;
			extra_jobs++;
		}
		else if ( ((flags & FLAG_MS_FOOD)!=0) && mappoint[x][y].int_1
			>MAX_FOOD_ON_TRACK-mappoint[tx][ty].int_1)
		{
			mappoint[x][y].int_1
			-=(MAX_FOOD_ON_TRACK-mappoint[tx][ty].int_1);
			mappoint[tx][ty].int_1=MAX_FOOD_ON_TRACK;
			extra_jobs++;
		}
// jobs
	        if ( ((flags & FLAG_MB_JOBS)!=0) &&
			(mappoint[x][y].int_2<MARKET_JOBS_SEARCH_TRIGGER
	                || (mappoint[tx][ty].int_2*10)>(MAX_JOBS_ON_TRACK*9))
			&& mappoint[tx][ty].int_2>0
	                && mappoint[x][y].int_2<(MAX_JOBS_IN_MARKET
	                        -MAX_JOBS_IN_MARKET/10))
	        {
	                mappoint[x][y].int_2+=(mappoint[tx][ty].int_2/2)
	                        +(mappoint[tx][ty].int_2%2);
	                mappoint[tx][ty].int_2/=2;
			extra_jobs++;
	        }
	        else if ( ((flags & FLAG_MS_JOBS)!=0) && mappoint[x][y].int_2
	                >MAX_JOBS_ON_TRACK-mappoint[tx][ty].int_2)
	        {
	                mappoint[x][y].int_2
	                -=(MAX_JOBS_ON_TRACK-mappoint[tx][ty].int_2);
	                mappoint[tx][ty].int_2=MAX_JOBS_ON_TRACK;
	        }
// coal
                if ( ((flags & FLAG_MB_COAL)!=0) &&
			(mappoint[x][y].int_3<MARKET_COAL_SEARCH_TRIGGER
                        || (mappoint[tx][ty].int_3*10)>(MAX_COAL_ON_TRACK*9))
                        && mappoint[tx][ty].int_3>0
                        && mappoint[x][y].int_3<(MAX_COAL_IN_MARKET
                                -MAX_COAL_IN_MARKET/10))
                {
                        mappoint[x][y].int_3+=(mappoint[tx][ty].int_3/2)
                                +(mappoint[tx][ty].int_3%2);
                        mappoint[tx][ty].int_3/=2;
                        extra_jobs++;
                }
                else if ( ((flags & FLAG_MS_COAL)!=0) && mappoint[x][y].int_3
                        >MAX_COAL_ON_TRACK-mappoint[tx][ty].int_3)
                {
                        mappoint[x][y].int_3
                        -=(MAX_COAL_ON_TRACK-mappoint[tx][ty].int_3);
                        mappoint[tx][ty].int_3=MAX_COAL_ON_TRACK;
                }
// goods
	        if ( ((flags & FLAG_MB_GOODS)!=0) &&
			(mappoint[x][y].int_4<MARKET_GOODS_SEARCH_TRIGGER
	                || (mappoint[tx][ty].int_4*10)>(MAX_GOODS_ON_TRACK*9))
			&& mappoint[tx][ty].int_4>0
	                && mappoint[x][y].int_4<(MAX_GOODS_IN_MARKET
	                        -MAX_GOODS_IN_MARKET/10))
	        {
	                mappoint[x][y].int_4+=(mappoint[tx][ty].int_4/2)
	                        +(mappoint[tx][ty].int_4%2);
	                mappoint[tx][ty].int_4/=2;
	                extra_jobs++;
	        }
	        else if ( ((flags & FLAG_MS_GOODS)!=0) && mappoint[x][y].int_4
	               >MAX_GOODS_ON_TRACK-mappoint[tx][ty].int_4)
	        {
	                mappoint[x][y].int_4
	                -=(MAX_GOODS_ON_TRACK-mappoint[tx][ty].int_4);
	                mappoint[tx][ty].int_4=MAX_GOODS_ON_TRACK;
	        }	
// ore
                if ( ((flags & FLAG_MB_ORE)!=0) &&
			(mappoint[x][y].int_5<MARKET_ORE_SEARCH_TRIGGER
                        || (mappoint[tx][ty].int_5*10)>(MAX_ORE_ON_TRACK*9))
                        && mappoint[tx][ty].int_5>0
                        && mappoint[x][y].int_5<(MAX_ORE_IN_MARKET
                                -MAX_ORE_IN_MARKET/10))
                {
                        mappoint[x][y].int_5+=(mappoint[tx][ty].int_5/2)
                                +(mappoint[tx][ty].int_5%2);
                        mappoint[tx][ty].int_5/=2;
                        extra_jobs++;
                }
                else if ( ((flags & FLAG_MS_ORE)!=0) && mappoint[x][y].int_5
                       >MAX_ORE_ON_TRACK-mappoint[tx][ty].int_5)
                {
                        mappoint[x][y].int_5
                        -=(MAX_ORE_ON_TRACK-mappoint[tx][ty].int_5);
                        mappoint[tx][ty].int_5=MAX_ORE_ON_TRACK;
                }
// steel
                if ( ((flags & FLAG_MB_STEEL)!=0) &&
			(mappoint[x][y].int_6<MARKET_STEEL_SEARCH_TRIGGER
                        || (mappoint[tx][ty].int_6*10)>(MAX_STEEL_ON_TRACK*9))
                        && mappoint[tx][ty].int_6>0
                        && mappoint[x][y].int_6<(MAX_STEEL_IN_MARKET
                                -MAX_STEEL_IN_MARKET/10))
                {
                        mappoint[x][y].int_6+=(mappoint[tx][ty].int_6/2)
                                +(mappoint[tx][ty].int_6%2);
                        mappoint[tx][ty].int_6/=2;
                        extra_jobs++;
                }
                else if ( ((flags & FLAG_MS_STEEL)!=0) && mappoint[x][y].int_6
                       >MAX_STEEL_ON_TRACK-mappoint[tx][ty].int_6)
                {
                        mappoint[x][y].int_6
                        -=(MAX_STEEL_ON_TRACK-mappoint[tx][ty].int_6);
                        mappoint[tx][ty].int_6=MAX_STEEL_ON_TRACK;
                }

// waste
                if ((mappoint[x][y].int_7<MARKET_WASTE_SEARCH_TRIGGER
                        || (mappoint[tx][ty].int_7*10)>(MAX_WASTE_ON_TRACK*9))
                        && mappoint[tx][ty].int_7>0
                        && mappoint[x][y].int_7<(MAX_WASTE_IN_MARKET
                                -MAX_WASTE_IN_MARKET/10))
                {
                        mappoint[x][y].int_7+=(mappoint[tx][ty].int_7/2)
                                +(mappoint[tx][ty].int_7%2);
                        mappoint[tx][ty].int_7/=2;
                        extra_jobs++;
                }
                else if ( mappoint[x][y].int_7
                       >MAX_WASTE_ON_TRACK-mappoint[tx][ty].int_7)
                {
                        mappoint[x][y].int_7
                        -=(MAX_WASTE_ON_TRACK-mappoint[tx][ty].int_7);
                        mappoint[tx][ty].int_7=MAX_WASTE_ON_TRACK;
                }
// if it's full of waste, burn some to make pollution.
		if (mappoint[x][y].int_7>=MAX_WASTE_IN_MARKET)
		{
			mappointpol[x][y]+=3000;
			mappoint[x][y].int_7-=(7*MAX_WASTE_IN_MARKET)/10;
// this should fire up the pollution mini screen within a second.
			mini_screen_flags=MINI_SCREEN_POL_FLAG;
		}
        }


// do rail traffic
	else if (main_types[mappointtype[tx][ty]].group==GROUP_RAIL)
	{
// food
		if ( ((flags & FLAG_MB_FOOD)!=0) &&
			(mappoint[x][y].int_1<MARKET_FOOD_SEARCH_TRIGGER
			|| (mappoint[tx][ty].int_1*10)>(MAX_FOOD_ON_RAIL*9))
			&& mappoint[tx][ty].int_1>0
			&& mappoint[x][y].int_1<(MAX_FOOD_IN_MARKET
				-MAX_FOOD_IN_MARKET/10))
		{
			mappoint[x][y].int_1+=(mappoint[tx][ty].int_1/2)
				+(mappoint[tx][ty].int_1%2);
			mappoint[tx][ty].int_1/=2;
			extra_jobs++;
		}
		else if ( ((flags & FLAG_MS_FOOD)!=0) && mappoint[x][y].int_1
			>MAX_FOOD_ON_RAIL-mappoint[tx][ty].int_1)
		{
			mappoint[x][y].int_1
			-=(MAX_FOOD_ON_RAIL-mappoint[tx][ty].int_1);
			mappoint[tx][ty].int_1=MAX_FOOD_ON_RAIL;
			extra_jobs++;
		}
// jobs
	        if ( ((flags & FLAG_MB_JOBS)!=0) &&
			(mappoint[x][y].int_2<MARKET_JOBS_SEARCH_TRIGGER
	                || (mappoint[tx][ty].int_2*10)>(MAX_JOBS_ON_RAIL*9))
			&& mappoint[tx][ty].int_2>0
	                && mappoint[x][y].int_2<(MAX_JOBS_IN_MARKET
	                        -MAX_JOBS_IN_MARKET/10))
	        {
	                mappoint[x][y].int_2+=(mappoint[tx][ty].int_2/2)
	                        +(mappoint[tx][ty].int_2%2);
	                mappoint[tx][ty].int_2/=2;
			extra_jobs++;
	        }
	        else if ( ((flags & FLAG_MS_JOBS)!=0) && mappoint[x][y].int_2
	                >MAX_JOBS_ON_RAIL-mappoint[tx][ty].int_2)
	        {
	                mappoint[x][y].int_2
	                -=(MAX_JOBS_ON_RAIL-mappoint[tx][ty].int_2);
	                mappoint[tx][ty].int_2=MAX_JOBS_ON_RAIL;
	        }
// coal
                if ( ((flags & FLAG_MB_COAL)!=0) &&
			(mappoint[x][y].int_3<MARKET_COAL_SEARCH_TRIGGER
                        || (mappoint[tx][ty].int_3*10)>(MAX_COAL_ON_RAIL*9))
                        && mappoint[tx][ty].int_3>0
                        && mappoint[x][y].int_3<(MAX_COAL_IN_MARKET
                                -MAX_COAL_IN_MARKET/10))
                {
                        mappoint[x][y].int_3+=(mappoint[tx][ty].int_3/2)
                                +(mappoint[tx][ty].int_3%2);
                        mappoint[tx][ty].int_3/=2;
                        extra_jobs++;
                }
                else if ( ((flags & FLAG_MS_COAL)!=0) && mappoint[x][y].int_3
                        >MAX_COAL_ON_RAIL-mappoint[tx][ty].int_3)
                {
                        mappoint[x][y].int_3
                        -=(MAX_COAL_ON_RAIL-mappoint[tx][ty].int_3);
                        mappoint[tx][ty].int_3=MAX_COAL_ON_RAIL;
                }
// goods
	        if ( ((flags & FLAG_MB_GOODS)!=0) &&
			(mappoint[x][y].int_4<MARKET_GOODS_SEARCH_TRIGGER
	                || (mappoint[tx][ty].int_4*10)>(MAX_GOODS_ON_RAIL*9))
			&& mappoint[tx][ty].int_4>0
	                && mappoint[x][y].int_4<(MAX_GOODS_IN_MARKET
	                        -MAX_GOODS_IN_MARKET/10))
	        {
	                mappoint[x][y].int_4+=(mappoint[tx][ty].int_4/2)
	                        +(mappoint[tx][ty].int_4%2);
	                mappoint[tx][ty].int_4/=2;
	                extra_jobs++;
	        }
	        else if ( ((flags & FLAG_MS_GOODS)!=0) && mappoint[x][y].int_4
	                >MAX_GOODS_ON_RAIL-mappoint[tx][ty].int_4)
	        {
	                mappoint[x][y].int_4
	                -=(MAX_GOODS_ON_RAIL-mappoint[tx][ty].int_4);
	                mappoint[tx][ty].int_4=MAX_GOODS_ON_RAIL;
	        }
// ore
                if ( ((flags & FLAG_MB_ORE)!=0) &&
			(mappoint[x][y].int_5<MARKET_ORE_SEARCH_TRIGGER
                        || (mappoint[tx][ty].int_5*10)>(MAX_ORE_ON_RAIL*9))
                        && mappoint[tx][ty].int_5>0
                        && mappoint[x][y].int_5<(MAX_ORE_IN_MARKET
                                -MAX_ORE_IN_MARKET/10))
                {
                        mappoint[x][y].int_5+=(mappoint[tx][ty].int_5/2)
                                +(mappoint[tx][ty].int_5%2);
                        mappoint[tx][ty].int_5/=2;
                        extra_jobs++;
                }
                else if ( ((flags & FLAG_MS_ORE)!=0) && mappoint[x][y].int_5
                        >MAX_ORE_ON_RAIL-mappoint[tx][ty].int_5)
                {
                        mappoint[x][y].int_5
                        -=(MAX_ORE_ON_RAIL-mappoint[tx][ty].int_5);
                        mappoint[tx][ty].int_5=MAX_ORE_ON_RAIL;
                }
// steel
                if ( ((flags & FLAG_MB_STEEL)!=0) &&
			(mappoint[x][y].int_6<MARKET_STEEL_SEARCH_TRIGGER
                        || (mappoint[tx][ty].int_6*10)>(MAX_STEEL_ON_RAIL*9))
                        && mappoint[tx][ty].int_6>0
                        && mappoint[x][y].int_6<(MAX_STEEL_IN_MARKET
                                -MAX_STEEL_IN_MARKET/10))
                {
                        mappoint[x][y].int_6+=(mappoint[tx][ty].int_6/2)
                                +(mappoint[tx][ty].int_6%2);
                        mappoint[tx][ty].int_6/=2;
                        extra_jobs++;
                }
                else if ( ((flags & FLAG_MS_STEEL)!=0) && mappoint[x][y].int_6
                        >MAX_STEEL_ON_RAIL-mappoint[tx][ty].int_6)
                {
                        mappoint[x][y].int_6
                        -=(MAX_STEEL_ON_RAIL-mappoint[tx][ty].int_6);
                        mappoint[tx][ty].int_6=MAX_STEEL_ON_RAIL;
                }

// waste
                if ((mappoint[x][y].int_7<MARKET_WASTE_SEARCH_TRIGGER
                        || (mappoint[tx][ty].int_7*10)>(MAX_WASTE_ON_RAIL*9))
                        && mappoint[tx][ty].int_7>0
                        && mappoint[x][y].int_7<(MAX_WASTE_IN_MARKET
                                -MAX_WASTE_IN_MARKET/10))
                {
                        mappoint[x][y].int_7+=(mappoint[tx][ty].int_7/2)
                                +(mappoint[tx][ty].int_7%2);
                        mappoint[tx][ty].int_7/=2;
                        extra_jobs++;
                }
                else if ( mappoint[x][y].int_7
                        >MAX_WASTE_ON_RAIL-mappoint[tx][ty].int_7)
                {
                        mappoint[x][y].int_7
                        -=(MAX_WASTE_ON_RAIL-mappoint[tx][ty].int_7);
                        mappoint[tx][ty].int_7=MAX_WASTE_ON_RAIL;
                }
        }

// do road traffic
	else if (main_types[mappointtype[tx][ty]].group==GROUP_ROAD)
	{
// food
		if ( ((flags & FLAG_MB_FOOD)!=0) &&
			(mappoint[x][y].int_1<MARKET_FOOD_SEARCH_TRIGGER
			|| (mappoint[tx][ty].int_1*10)>(MAX_FOOD_ON_ROAD*9))
			&& mappoint[tx][ty].int_1>0
			&& mappoint[x][y].int_1<(MAX_FOOD_IN_MARKET
				-MAX_FOOD_IN_MARKET/10))
		{
			mappoint[x][y].int_1+=(mappoint[tx][ty].int_1/2)
				+(mappoint[tx][ty].int_1%2);
			mappoint[tx][ty].int_1/=2;
			extra_jobs++;
		}
		else if ( ((flags & FLAG_MS_FOOD)!=0) && mappoint[x][y].int_1
			>MAX_FOOD_ON_ROAD-mappoint[tx][ty].int_1)
		{
			mappoint[x][y].int_1
			-=(MAX_FOOD_ON_ROAD-mappoint[tx][ty].int_1);
			mappoint[tx][ty].int_1=MAX_FOOD_ON_ROAD;
			extra_jobs++;
		}
// jobs
	        if ( ((flags & FLAG_MB_JOBS)!=0) &&
			(mappoint[x][y].int_2<MARKET_JOBS_SEARCH_TRIGGER
	                || (mappoint[tx][ty].int_2*10)>(MAX_JOBS_ON_ROAD*9))
			&& mappoint[tx][ty].int_2>0
	                && mappoint[x][y].int_2<(MAX_JOBS_IN_MARKET
	                        -MAX_JOBS_IN_MARKET/10))
	        {
	                mappoint[x][y].int_2+=(mappoint[tx][ty].int_2/2)
	                        +(mappoint[tx][ty].int_2%2);
		                mappoint[tx][ty].int_2/=2;
			extra_jobs++;
	        }
	        else if ( ((flags & FLAG_MS_JOBS)!=0) && mappoint[x][y].int_2
	                >MAX_JOBS_ON_ROAD-mappoint[tx][ty].int_2)
	        {
	                mappoint[x][y].int_2
	                -=(MAX_JOBS_ON_ROAD-mappoint[tx][ty].int_2);
	                mappoint[tx][ty].int_2=MAX_JOBS_ON_ROAD;
	        }
// coal
                if ( ((flags & FLAG_MB_COAL)!=0) &&
			(mappoint[x][y].int_3<MARKET_COAL_SEARCH_TRIGGER
                        || (mappoint[tx][ty].int_3*10)>(MAX_COAL_ON_ROAD*9))
                        && mappoint[tx][ty].int_3>0
                        && mappoint[x][y].int_3<(MAX_COAL_IN_MARKET
                                -MAX_COAL_IN_MARKET/10))
                {
                        mappoint[x][y].int_3+=(mappoint[tx][ty].int_3/2)
                                +(mappoint[tx][ty].int_3%2);
                                mappoint[tx][ty].int_3/=2;
                        extra_jobs++;
                }
                else if ( ((flags & FLAG_MS_COAL)!=0) && mappoint[x][y].int_3
                        >MAX_COAL_ON_ROAD-mappoint[tx][ty].int_3)
                {
                        mappoint[x][y].int_3
                        -=(MAX_COAL_ON_ROAD-mappoint[tx][ty].int_3);
                        mappoint[tx][ty].int_3=MAX_COAL_ON_ROAD;
                }
// goods
	        if ( ((flags & FLAG_MB_GOODS)!=0) &&
			(mappoint[x][y].int_4<MARKET_GOODS_SEARCH_TRIGGER
	                || (mappoint[tx][ty].int_4*10)>(MAX_GOODS_ON_ROAD*9))
			&& mappoint[tx][ty].int_4>0
	                && mappoint[x][y].int_4<(MAX_GOODS_IN_MARKET
	                        -MAX_GOODS_IN_MARKET/10))
	        {
	                mappoint[x][y].int_4+=(mappoint[tx][ty].int_4/2)
	                        +(mappoint[tx][ty].int_4%2);
	                mappoint[tx][ty].int_4/=2;
	                extra_jobs++;
	        }
	        else if ( ((flags & FLAG_MS_GOODS)!=0) && mappoint[x][y].int_4
	                >MAX_GOODS_ON_ROAD-mappoint[tx][ty].int_4)
	        {
	                mappoint[x][y].int_4
	                -=(MAX_GOODS_ON_ROAD-mappoint[tx][ty].int_4);
	                mappoint[tx][ty].int_4=MAX_GOODS_ON_ROAD;
	        }
// ore
                if ( ((flags & FLAG_MB_ORE)!=0) &&
			(mappoint[x][y].int_5<MARKET_ORE_SEARCH_TRIGGER
                        || (mappoint[tx][ty].int_5*10)>(MAX_ORE_ON_ROAD*9))
                        && mappoint[tx][ty].int_5>0
                        && mappoint[x][y].int_5<(MAX_ORE_IN_MARKET
                                -MAX_ORE_IN_MARKET/10))
                {
                        mappoint[x][y].int_5+=(mappoint[tx][ty].int_5/2)
                                +(mappoint[tx][ty].int_5%2);
                        mappoint[tx][ty].int_5/=2;
                        extra_jobs++;
                }
                else if ( ((flags & FLAG_MS_ORE)!=0) && mappoint[x][y].int_5
                        >MAX_ORE_ON_ROAD-mappoint[tx][ty].int_5)
                {
                        mappoint[x][y].int_5
                        -=(MAX_ORE_ON_ROAD-mappoint[tx][ty].int_5);
                        mappoint[tx][ty].int_5=MAX_ORE_ON_ROAD;
                }
// steel
                if ( ((flags & FLAG_MB_STEEL)!=0) &&
			(mappoint[x][y].int_6<MARKET_STEEL_SEARCH_TRIGGER
                        || (mappoint[tx][ty].int_6*10)>(MAX_STEEL_ON_ROAD*9))
                        && mappoint[tx][ty].int_6>0
                        && mappoint[x][y].int_6<(MAX_STEEL_IN_MARKET
                                -MAX_STEEL_IN_MARKET/10))
                {
                        mappoint[x][y].int_6+=(mappoint[tx][ty].int_6/2)
                                +(mappoint[tx][ty].int_6%2);
                        mappoint[tx][ty].int_6/=2;
                        extra_jobs++;
                }
                else if ( ((flags & FLAG_MS_STEEL)!=0) && mappoint[x][y].int_6
                        >MAX_STEEL_ON_ROAD-mappoint[tx][ty].int_6)
                {
                        mappoint[x][y].int_6
                        -=(MAX_STEEL_ON_ROAD-mappoint[tx][ty].int_6);
                        mappoint[tx][ty].int_6=MAX_STEEL_ON_ROAD;
                }

// waste
                if ((mappoint[x][y].int_7<MARKET_WASTE_SEARCH_TRIGGER
                        || (mappoint[tx][ty].int_7*10)>(MAX_WASTE_ON_ROAD*9))
                        && mappoint[tx][ty].int_7>0
                        && mappoint[x][y].int_7<(MAX_WASTE_IN_MARKET
                                -MAX_WASTE_IN_MARKET/10))
                {
                        mappoint[x][y].int_7+=(mappoint[tx][ty].int_7/2)
                                +(mappoint[tx][ty].int_7%2);
                        mappoint[tx][ty].int_7/=2;
                        extra_jobs++;
                }
                else if (mappoint[x][y].int_7
                        >MAX_WASTE_ON_ROAD-mappoint[tx][ty].int_7)
                {
                        mappoint[x][y].int_7
                        -=(MAX_WASTE_ON_ROAD-mappoint[tx][ty].int_7);
                        mappoint[tx][ty].int_7=MAX_WASTE_ON_ROAD;
                }
        }


	return(extra_jobs);
}	

 ******* */

int deal_with_transport(int x,int y,int tx,int ty)
{
	int i,r,extra_jobs=3,flags;
	flags=mappoint[x][y].flags;
// tracks
	if (main_types[mappointtype[tx][ty]].group==GROUP_TRACK)
	{
// food
		if ((flags & FLAG_MB_FOOD)!=0)
		{
			r=(MAX_FOOD_IN_MARKET*1000)
				/(MAX_FOOD_ON_TRACK+MAX_FOOD_IN_MARKET);
//       yes, I know r is a constant. Trade off to keep mess down.
			i=mappoint[x][y].int_1+mappoint[tx][ty].int_1;
			mappoint[x][y].int_1=(i*r)/1000;
			mappoint[tx][ty].int_1=i-mappoint[x][y].int_1;
		}

// jobs
	        if ((flags & FLAG_MB_JOBS)!=0)
		{
			r=(MAX_JOBS_IN_MARKET*1000)
				/(MAX_JOBS_ON_TRACK+MAX_JOBS_IN_MARKET);
			i=mappoint[x][y].int_2+mappoint[tx][ty].int_2;
			mappoint[x][y].int_2=(i*r)/1000;
			mappoint[tx][ty].int_2=i-mappoint[x][y].int_2;
		}

// coal
                if ((flags & FLAG_MB_COAL)!=0)
		{
			r=(MAX_COAL_IN_MARKET*1000)
				/(MAX_COAL_ON_TRACK+MAX_COAL_IN_MARKET);
			i=mappoint[x][y].int_3+mappoint[tx][ty].int_3;
			mappoint[x][y].int_3=(i*r)/1000;
			mappoint[tx][ty].int_3=i-mappoint[x][y].int_3;
		}

// goods
	        if ((flags & FLAG_MB_GOODS)!=0)
		{
			r=(MAX_GOODS_IN_MARKET*1000)
				/(MAX_GOODS_ON_TRACK+MAX_GOODS_IN_MARKET);
			i=mappoint[x][y].int_4+mappoint[tx][ty].int_4;
			mappoint[x][y].int_4=(i*r)/1000;
			mappoint[tx][ty].int_4=i-mappoint[x][y].int_4;
		}

// ore
                if ((flags & FLAG_MB_ORE)!=0)
		{
			r=(MAX_ORE_IN_MARKET*1000)
				/(MAX_ORE_ON_TRACK+MAX_ORE_IN_MARKET);
			i=mappoint[x][y].int_5+mappoint[tx][ty].int_5;
			mappoint[x][y].int_5=(i*r)/1000;
			mappoint[tx][ty].int_5=i-mappoint[x][y].int_5;
		}


// steel
                if ((flags & FLAG_MB_STEEL)!=0)
		{
			r=(MAX_STEEL_IN_MARKET*1000)
				/(MAX_STEEL_ON_TRACK+MAX_STEEL_IN_MARKET);
			i=mappoint[x][y].int_6+mappoint[tx][ty].int_6;
			mappoint[x][y].int_6=(i*r)/1000;
			mappoint[tx][ty].int_6=i-mappoint[x][y].int_6;
		}

// waste
		r=(MAX_WASTE_IN_MARKET*1000)
			/(MAX_WASTE_ON_TRACK+MAX_WASTE_IN_MARKET);
		i=mappoint[x][y].int_7+mappoint[tx][ty].int_7;
		mappoint[x][y].int_7=(i*r)/1000;
		mappoint[tx][ty].int_7=i-mappoint[x][y].int_7;


// if it's full of waste, burn some to make pollution.
		if (mappoint[x][y].int_7>=(99*MAX_WASTE_IN_MARKET/100))
		{
			mappointpol[x][y]+=3000;
			mappoint[x][y].int_7-=(7*MAX_WASTE_IN_MARKET)/10;
// this should fire up the pollution mini screen within a second.
			mini_screen_flags=MINI_SCREEN_POL_FLAG;
		}
        }


// do rail traffic
	else if (main_types[mappointtype[tx][ty]].group==GROUP_RAIL)
	{
// food
		if ((flags & FLAG_MB_FOOD)!=0)
		{
			r=(MAX_FOOD_IN_MARKET*1000)
				/(MAX_FOOD_ON_RAIL+MAX_FOOD_IN_MARKET);
			i=mappoint[x][y].int_1+mappoint[tx][ty].int_1;
			mappoint[x][y].int_1=(i*r)/1000;
			mappoint[tx][ty].int_1=i-mappoint[x][y].int_1;
		}
  
// jobs
	        if ((flags & FLAG_MB_JOBS)!=0)
		{
			r=(MAX_JOBS_IN_MARKET*1000)
				/(MAX_JOBS_ON_RAIL+MAX_JOBS_IN_MARKET);
			i=mappoint[x][y].int_2+mappoint[tx][ty].int_2;
			mappoint[x][y].int_2=(i*r)/1000;
			mappoint[tx][ty].int_2=i-mappoint[x][y].int_2;
		}

// coal
                if ((flags & FLAG_MB_COAL)!=0)
		{
			r=(MAX_COAL_IN_MARKET*1000)
				/(MAX_COAL_ON_RAIL+MAX_COAL_IN_MARKET);
			i=mappoint[x][y].int_3+mappoint[tx][ty].int_3;
			mappoint[x][y].int_3=(i*r)/1000;
			mappoint[tx][ty].int_3=i-mappoint[x][y].int_3;
		}

// goods
	        if ((flags & FLAG_MB_GOODS)!=0)
		{
			r=(MAX_GOODS_IN_MARKET*1000)
				/(MAX_GOODS_ON_RAIL+MAX_GOODS_IN_MARKET);
			i=mappoint[x][y].int_4+mappoint[tx][ty].int_4;
			mappoint[x][y].int_4=(i*r)/1000;
			mappoint[tx][ty].int_4=i-mappoint[x][y].int_4;
		}

// ore
                if ((flags & FLAG_MB_ORE)!=0)
		{
			r=(MAX_ORE_IN_MARKET*1000)
				/(MAX_ORE_ON_RAIL+MAX_ORE_IN_MARKET);
			i=mappoint[x][y].int_5+mappoint[tx][ty].int_5;
			mappoint[x][y].int_5=(i*r)/1000;
			mappoint[tx][ty].int_5=i-mappoint[x][y].int_5;
		}

// steel
                if ((flags & FLAG_MB_STEEL)!=0)
		{
			r=(MAX_STEEL_IN_MARKET*1000)
				/(MAX_STEEL_ON_RAIL+MAX_STEEL_IN_MARKET);
			i=mappoint[x][y].int_6+mappoint[tx][ty].int_6;
			mappoint[x][y].int_6=(i*r)/1000;
			mappoint[tx][ty].int_6=i-mappoint[x][y].int_6;
		}

// waste
		r=(MAX_WASTE_IN_MARKET*1000)
			/(MAX_WASTE_ON_RAIL+MAX_WASTE_IN_MARKET);
		i=mappoint[x][y].int_7+mappoint[tx][ty].int_7;
		mappoint[x][y].int_7=(i*r)/1000;
		mappoint[tx][ty].int_7=i-mappoint[x][y].int_7;

        }

// do road traffic
	else if (main_types[mappointtype[tx][ty]].group==GROUP_ROAD)
	{
// food
		if ((flags & FLAG_MB_FOOD)!=0)
		{
			r=(MAX_FOOD_IN_MARKET*1000)
				/(MAX_FOOD_ON_ROAD+MAX_FOOD_IN_MARKET);
			i=mappoint[x][y].int_1+mappoint[tx][ty].int_1;
			mappoint[x][y].int_1=(i*r)/1000;
			mappoint[tx][ty].int_1=i-mappoint[x][y].int_1;
		}

// jobs
	        if ((flags & FLAG_MB_JOBS)!=0)
		{
			r=(MAX_JOBS_IN_MARKET*1000)
				/(MAX_JOBS_ON_ROAD+MAX_JOBS_IN_MARKET);
			i=mappoint[x][y].int_2+mappoint[tx][ty].int_2;
			mappoint[x][y].int_2=(i*r)/1000;
			mappoint[tx][ty].int_2=i-mappoint[x][y].int_2;
		}

// coal
                if ((flags & FLAG_MB_COAL)!=0)
		{
			r=(MAX_COAL_IN_MARKET*1000)
				/(MAX_COAL_ON_ROAD+MAX_COAL_IN_MARKET);
			i=mappoint[x][y].int_3+mappoint[tx][ty].int_3;
			mappoint[x][y].int_3=(i*r)/1000;
			mappoint[tx][ty].int_3=i-mappoint[x][y].int_3;
		}

// goods
	        if ((flags & FLAG_MB_GOODS)!=0)
		{
			r=(MAX_GOODS_IN_MARKET*1000)
				/(MAX_GOODS_ON_ROAD+MAX_GOODS_IN_MARKET);
			i=mappoint[x][y].int_4+mappoint[tx][ty].int_4;
			mappoint[x][y].int_4=(i*r)/1000;
			mappoint[tx][ty].int_4=i-mappoint[x][y].int_4;
		}

// ore
                if ((flags & FLAG_MB_ORE)!=0)
		{
			r=(MAX_ORE_IN_MARKET*1000)
				/(MAX_ORE_ON_ROAD+MAX_ORE_IN_MARKET);
			i=mappoint[x][y].int_5+mappoint[tx][ty].int_5;
			mappoint[x][y].int_5=(i*r)/1000;
			mappoint[tx][ty].int_5=i-mappoint[x][y].int_5;
		}

// steel
                if ((flags & FLAG_MB_STEEL)!=0)
		{
			r=(MAX_STEEL_IN_MARKET*1000)
				/(MAX_STEEL_ON_ROAD+MAX_STEEL_IN_MARKET);
			i=mappoint[x][y].int_6+mappoint[tx][ty].int_6;
			mappoint[x][y].int_6=(i*r)/1000;
			mappoint[tx][ty].int_6=i-mappoint[x][y].int_6;
		}

// waste
		r=(MAX_WASTE_IN_MARKET*1000)
			/(MAX_WASTE_ON_ROAD+MAX_WASTE_IN_MARKET);
		i=mappoint[x][y].int_7+mappoint[tx][ty].int_7;
		mappoint[x][y].int_7=(i*r)/1000;
		mappoint[tx][ty].int_7=i-mappoint[x][y].int_7;
	}
	else
		extra_jobs=0;

	return(extra_jobs);
}	

#ifdef THMO_GP_STUFF
// Mostly THMO's code. I've put (IJP) on the lines I've changed.
// The changes are needed to make it fit easily into the existing structure.

// static int // -- (IJP)
// get_stuff( map, type, stuff, stuff_type)  // -- (IJP)
int get_stuff(int x,int y,int stuff,int stuff_type) // -- (IJP)
//  struct MAPPOINT  *map;  -- (IJP)
//  int              type, stuff;  //  -- (IJP)
//  const int        stuff_type;  //  -- (IJP)
{
  int  res=0;
  struct MAPPOINT *map=&mappoint[x][y];  // -- (IJP)
//   switch ( main_types[ type].size)   // -- (IJP)
    switch ( main_types[mappointtype[x][y]].size)   // -- (IJP)
    {
      case 2:
        res= get_stuff2( map, stuff, stuff_type);
        break;
      case 3:
        res= get_stuff3( map, stuff, stuff_type);
        break;
      case 4:
        res= get_stuff4( map, stuff, stuff_type);
        break;
      default:
//        __NORETURN do_error( "Bad area size in %s_stuff()", "get");
	do_error("Bad area size in get_stuff()"); // -- (IJP);
    }
  return( res);
}

#else

int get_stuff(int x,int y,int stuff,int stuff_type)
{
	int size=main_types[mappointtype[x][y]].size;
//printf("get_stuff: x=%d y=%d st=%d ty=%d size=%d\n",x,y,stuff,stuff_type,size);
	if (size==2)
		return(get_stuff2(x,y,stuff,stuff_type));
	if (size==3)
		return(get_stuff3(x,y,stuff,stuff_type));
	if (size==4)
		return(get_stuff4(x,y,stuff,stuff_type));
	do_error("Bad area size in get_stuff()");
	return(0);
}

#endif

#ifdef THMO_GP_STUFF

static const int
  t2[ 8]= {
      -1,
      WORLD_SIDE_LEN- 1,
      2* WORLD_SIDE_LEN,
      2* WORLD_SIDE_LEN+ 1,
      WORLD_SIDE_LEN+ 2,
      2,
      1- WORLD_SIDE_LEN,
      -WORLD_SIDE_LEN
    };

// static INLINE int       /* worth inlining -- (ThMO) */
//get_stuff2( map, stuff, stuff_type)
int get_stuff2(struct MAPPOINT *map,int stuff,int stuff_type) // -- (IJP)
//  struct MAPPOINT  *map;
//  int              stuff;
//  const int        stuff_type;
{
  static int
    tstart2= 0;

  int  i, st, tst, *ip,
       *stack[ 8], **ssp;       /* stack is really a pipe -- (ThMO) */

  /* we'll stack our found pointers so to avoid re-looping and
   * testing again
   * (ThMO)
   */

  tst= tstart2;

  /* can we find enough on the transport? */

  for ( ssp= stack, st= 0, i= 0;  i < 8;  i++)
    {
      if ( map[ t2[ tst]].flags & FLAG_IS_TRANSPORT)
        {
          ip= &map[ t2[ tst]].int_1;
          ip += stuff_type;
          st += *ip;
          *ssp++= ip;           /* push it -- (ThMO) */
          if ( st >= stuff)
            {
              ssp= stack;
              do
                {
                  ip= *ssp++;   /* pop it -- (ThMO) */
                  *ip= ( stuff -= *ip) < 0 ? -stuff : 0;
                }
              while ( stuff > 0);
              tstart2= ++tst & 7;
              return( 1);
            }
        }
      if ( ++tst >= 8)
        tst= 0;
    }
  return( 0);
}

#else

int get_stuff2(int x,int y,int stuff,int stuff_type)
{
	static int tstart=0;
	static int tx[8]={  0, 1, 2, 2, 1, 0,-1,-1};
	static int ty[8]={ -1,-1, 0, 1, 2, 2, 1, 0};
	int i,st=0,tst,*ip;
	tst=tstart;
// can we find enough on the transport?
	for (i=0;i<8;i++)
	{
		if ((mappoint[x+tx[tst]][y+ty[tst]].flags 
			& FLAG_IS_TRANSPORT)!=0)
		{
			ip=&(mappoint[x+tx[tst]][y+ty[tst]].int_1);
			st+=*(ip+stuff_type);
			if (st>=stuff)
				break;
		}
		tst++;
		if (tst>=8)
			tst=0;
	}
	if (st<stuff)
		return(0);
	st=stuff;
	while (st>0)
	{
		if ((mappoint[x+tx[tstart]][y+ty[tstart]].flags
			& FLAG_IS_TRANSPORT)!=0)
		{
			ip=&(mappoint[x+tx[tstart]][y+ty[tstart]].int_1);
			ip+=stuff_type;
			if (st>*ip)
			{
				st-=*ip;
				*ip=0;
			}
			else
			{
				*ip-=st;
				st=0;
			}
		}
		tstart++;
		if (tstart>=8)
			tstart=0;
	}
	return(1);
}

#endif

#ifdef THMO_GP_STUFF

static const int
  t3[ 12]= {
      -1,
      WORLD_SIDE_LEN- 1,
      2* WORLD_SIDE_LEN- 1,
      3* WORLD_SIDE_LEN,
      3* WORLD_SIDE_LEN+ 1,
      3* WORLD_SIDE_LEN+ 2,
      2* WORLD_SIDE_LEN+ 3,
      WORLD_SIDE_LEN+ 3,
      3,
      2- WORLD_SIDE_LEN,
      1- WORLD_SIDE_LEN,
      -WORLD_SIDE_LEN
    };

//static INLINE int       /* worth inlining -- (ThMO) */
int get_stuff3(struct MAPPOINT *map,int stuff,int stuff_type)
//  struct MAPPOINT  *map;
//  int              stuff;
//  const int        stuff_type;
{
  static int
    tstart3= 0;

  int  i, st, tst, *ip,
       *stack[ 12], **ssp;      /* stack is really a pipe -- (ThMO) */

  /* we'll stack our found pointers so to avoid re-looping and
   * testing again
   * (ThMO)
   */

  tst= tstart3;

  /* can we find enough on the transport? */

  for ( ssp= stack, st= 0, i= 0;  i < 12;  i++)
    {
      if ( map[ t3[ tst]].flags & FLAG_IS_TRANSPORT)
        {
          ip= &map[ t3[ tst]].int_1;
          ip += stuff_type;
          st += *ip;
          *ssp++= ip;           /* push it -- (ThMO) */
          if ( st >= stuff)
            {
              ssp= stack;
              do
                {
                  ip= *ssp++;   /* pop it -- (ThMO) */
                  *ip= ( stuff -= *ip) < 0 ? -stuff : 0;
                }
              while ( stuff > 0);
              ++tst;
              tstart3= tst >= 12 ? 0 : tst;
              return( 1);
            }
        }
      if ( ++tst >= 12)
        tst= 0;
    }
  return( 0);
}

#else

int get_stuff3(int x,int y,int stuff,int stuff_type)
{
	static int tstart=0;
	static int tx[12]={  0, 1, 2, 3, 3, 3, 2, 1, 0,-1,-1,-1};
	static int ty[12]={ -1,-1,-1, 0, 1, 2, 3, 3, 3, 2, 1, 0};
	int i,st=0,tst,*ip;
	tst=tstart;
// can we find enough on the transport?
	for (i=0;i<12;i++)
	{
		if ((mappoint[x+tx[tst]][y+ty[tst]].flags 
			& FLAG_IS_TRANSPORT)!=0)
		{
			ip=&(mappoint[x+tx[tst]][y+ty[tst]].int_1);
			st+=*(ip+stuff_type);
			if (st>=stuff)
				break;
		}
		tst++;
		if (tst>=12)
			tst=0;
	}
	if (st<stuff)
		return(0);
	st=stuff;
	while (st>0)
	{
		if ((mappoint[x+tx[tstart]][y+ty[tstart]].flags
			& FLAG_IS_TRANSPORT)!=0)
		{
			ip=&(mappoint[x+tx[tstart]][y+ty[tstart]].int_1);
			ip+=stuff_type;
			if (st>*ip)
			{
				st-=*ip;
				*ip=0;
			}
			else
			{
				*ip-=st;
				st=0;
			}
		}
		tstart++;
		if (tstart>=12)
			tstart=0;
	}
	return(1);
}

#endif

#ifdef THMO_GP_STUFF

static const int
  t4[ 16]= {
      -1,
      WORLD_SIDE_LEN- 1,
      2* WORLD_SIDE_LEN- 1,
      3* WORLD_SIDE_LEN- 1,
      4* WORLD_SIDE_LEN,
      4* WORLD_SIDE_LEN+ 1,
      4* WORLD_SIDE_LEN+ 2,
      4* WORLD_SIDE_LEN+ 3,
      3* WORLD_SIDE_LEN+ 4,
      2* WORLD_SIDE_LEN+ 4,
      WORLD_SIDE_LEN+ 4,
      4,
      3- WORLD_SIDE_LEN,
      2- WORLD_SIDE_LEN,
      1- WORLD_SIDE_LEN,
      -WORLD_SIDE_LEN
    };

// static INLINE int       /* worth inlining -- (ThMO) */
int get_stuff4(struct MAPPOINT *map,int stuff,int stuff_type)
//  struct MAPPOINT  *map;
//  int              stuff;
//  const int        stuff_type;
{
  static int
    tstart4= 0;

  int  i, st, tst, *ip,
       *stack[ 16], **ssp;      /* stack is really a pipe -- (ThMO) */

  /* we'll stack our found pointers so to avoid re-looping and
   * testing again
   * (ThMO)
   */

  tst= tstart4;

  /* can we find enough on the transport? */

  for ( ssp= stack, st= 0, i= 0;  i < 16;  i++)
    {
      if ( map[ t4[ tst]].flags & FLAG_IS_TRANSPORT)
        {
          ip= &map[ t4[ tst]].int_1;
          ip += stuff_type;
          st += *ip;
          *ssp++= ip;           /* push it -- (ThMO) */
          if ( st >= stuff)
            {
              ssp= stack;
              do
                {
                  ip= *ssp++;   /* pop it -- (ThMO) */
                  *ip= ( stuff -= *ip) < 0 ? -stuff : 0;
                }
              while ( stuff > 0);
              tstart4= ++tst & 15;
              return( 1);
            }
        }
      if ( ++tst >= 16)
        tst= 0;
    }
  return( 0);
}

#else

int get_stuff4(int x,int y,int stuff,int stuff_type)
{
	static int tstart=0;
	static int tx[16]={  0, 1, 2, 3, 4, 4, 4, 4, 3, 2, 1, 0,-1,-1,-1,-1};
	static int ty[16]={ -1,-1,-1,-1, 0, 1, 2, 3, 4, 4, 4, 4, 3, 2, 1, 0};
	int i,st=0,tst,*ip;
	tst=tstart;
// can we find enough on the transport?
	for (i=0;i<16;i++)
	{
		if ((mappoint[x+tx[tst]][y+ty[tst]].flags 
			& FLAG_IS_TRANSPORT)!=0)
		{
			ip=&(mappoint[x+tx[tst]][y+ty[tst]].int_1);
			st+=*(ip+stuff_type);
			if (st>=stuff)
				break;
		}
		tst++;
		if (tst>=16)
			tst=0;
	}
	if (st<stuff)
		return(0);
	st=stuff;
	while (st>0)
	{
		if ((mappoint[x+tx[tstart]][y+ty[tstart]].flags
			& FLAG_IS_TRANSPORT)!=0)
		{
			ip=&(mappoint[x+tx[tstart]][y+ty[tstart]].int_1);
			ip+=stuff_type;
			if (st>*ip)
			{
				st-=*ip;
				*ip=0;
			}
			else
			{
				*ip-=st;
				st=0;
			}
		}
		tstart++;
		if (tstart>=16)
			tstart=0;
	}
	return(1);
}

#endif

#ifdef THMO_GP_STUFF

//static int
//put_stuff( map, type, stuff, stuff_type)
int put_stuff(int x,int y,int stuff,int stuff_type)
//  struct MAPPOINT  *map;
//  short            *type;
//  int              stuff;
//  const int        stuff_type;
{
  int res=0;
  short *type=&mappointtype[x][y];
  struct MAPPOINT  *map=&mappoint[x][y];
  switch ( main_types[ *type].size)
    {
      case 2:
        res= put_stuff2( map, type, stuff, stuff_type);
        break;
      case 3:
        res= put_stuff3( map, type, stuff, stuff_type);
        break;
      case 4:
        res= put_stuff4( map, type, stuff, stuff_type);
        break;
      default:
//        __NORETURN do_error( "Bad area size in %s_stuff()", "put");
	do_error( "Bad area size in put_stuff()");  // -- (IJP)
    }
  return( res);
}

#else

int put_stuff(int x,int y,int stuff,int stuff_type)
{
	int size=main_types[mappointtype[x][y]].size;
//printf("put stuff: x=%d y=%d st=%d ty=%d size=%d\n",x,y,stuff,stuff_type,size);
	if (size==2)
		return(put_stuff2(x,y,stuff,stuff_type));
	if (size==3)
		return(put_stuff3(x,y,stuff,stuff_type));
	if (size==4)
		return(put_stuff4(x,y,stuff,stuff_type));
	do_error("Bad area size in put_stuff()");
	return(0);
}

#endif

int tmax[3][7]={ 
	{ MAX_FOOD_ON_TRACK,MAX_JOBS_ON_TRACK
        ,MAX_COAL_ON_TRACK,MAX_GOODS_ON_TRACK,MAX_ORE_ON_TRACK
        ,MAX_STEEL_ON_TRACK,MAX_WASTE_ON_TRACK },
        { MAX_FOOD_ON_ROAD,MAX_JOBS_ON_ROAD
        ,MAX_COAL_ON_ROAD,MAX_GOODS_ON_ROAD,MAX_ORE_ON_ROAD
        ,MAX_STEEL_ON_ROAD,MAX_WASTE_ON_ROAD },
        { MAX_FOOD_ON_RAIL,MAX_JOBS_ON_RAIL
        ,MAX_COAL_ON_RAIL,MAX_GOODS_ON_RAIL,MAX_ORE_ON_RAIL
        ,MAX_STEEL_ON_RAIL,MAX_WASTE_ON_RAIL }
	};

#ifdef THMO_GP_STUFF

struct stack {
  int  *ip,
       max_val;
  };

//static INLINE int       /* worth inlining -- (ThMO) */
//put_stuff2( map, type, stuff, stuff_type)
//  struct MAPPOINT  *map;
//  short            *type;
//  int              stuff;
//  const int        stuff_type;
int put_stuff2(struct MAPPOINT  *map,short *type,int stuff,int stuff_type)
{
  static int
    tstart2= 0;

  int           i, st, tst, *ip, tp=0;
  struct stack  stack[ 8], *ssp;        /* stack is really a pipe -- (ThMO)
*/

  /* we'll stack our found pointers so to avoid re-looping and
   * testing again
   * (ThMO)
   */

  tst= tstart2;

  /* can we put enough on the transport? */

  for ( ssp= stack, st= 0, i= 0;  i < 8;  i++)
    {
      if ( map[ t2[ tst]].flags & FLAG_IS_TRANSPORT)
        {
          switch ( main_types[ type[ t2[ tst]]].group)
            {
              case GROUP_TRACK:
                tp= tmax[ 0][ stuff_type];
                break;
              case GROUP_ROAD:
                tp= tmax[ 1][ stuff_type];
                break;
              case GROUP_RAIL:
                tp= tmax[ 2][ stuff_type];
                break;
              default:
//                __NORETURN do_error( "Bad transport type in put_stuff%c()",
//'2');
		do_error( "Bad transport type in put_stuff2"); // -- (IJP)
            }
          ip= &map[ t2[ tst]].int_1;
          ssp->ip= ip += stuff_type;            /* push it -- (ThMO) */
          ssp++->max_val= tp;
          st += tp- *ip;
          if ( st >= stuff)
            {
              ssp= stack;
              do
                {
                  tp= ssp->max_val- *ssp->ip;  /* pop it -- (ThMO) */
                  if ( ( stuff -= tp) < 0)
                    *ssp->ip += tp+ stuff;     /* == orig. stuff -- (ThMO) */
                  else
                    *ssp->ip= ssp->max_val;
                  ++ssp;
                }
              while ( stuff > 0);
              tstart2= ++tst & 7;
              return( 1);
            }
        }
      if ( ++tst >= 8)
        tst = 0;
    }
  return( 0);
}

#else

int put_stuff2(int x,int y,int stuff,int stuff_type)
{
	static int tstart=0;
	static int tx[8]={  0, 1, 2, 2, 1, 0,-1,-1};
	static int ty[8]={ -1,-1, 0, 1, 2, 2, 1, 0};
	int i,j,st=0,tst,*ip,tt;
	tst=tstart;
// can we put enough on the transport?
	for (i=0;i<8;i++)
	{
		if ((mappoint[x+tx[tst]][y+ty[tst]].flags 
			& FLAG_IS_TRANSPORT)!=0)
		{
			switch(main_types[mappointtype[x+tx[tst]]\
				[y+ty[tst]]].group)
			{
				case(GROUP_TRACK): tt=0; break;
				case(GROUP_ROAD):  tt=1; break;
				case(GROUP_RAIL):  tt=2; break;
				default: do_error(
					"Bad transport type in put_stuff");
			}
			ip=&(mappoint[x+tx[tst]][y+ty[tst]].int_1);
			st+=tmax[tt][stuff_type]-(*(ip+stuff_type));
			if (st>=stuff)
				break;
		}
		tst++;
		if (tst>=8)
			tst=0;
	}
	if (st<stuff)
		return(0);
	st=stuff;
	while (st>0)
	{
		if ((mappoint[x+tx[tstart]][y+ty[tstart]].flags
			& FLAG_IS_TRANSPORT)!=0)
		{
			switch(main_types[mappointtype[x+tx[tstart]]\
				[y+ty[tstart]]].group)
			{
				case(GROUP_TRACK): tt=0; break;
				case(GROUP_ROAD):  tt=1; break;
				case(GROUP_RAIL):  tt=2; break;
				default: do_error(
					"Bad transport type in put_stuff");
			}
			ip=&(mappoint[x+tx[tstart]][y+ty[tstart]].int_1);
			ip+=stuff_type;
			if (st>(tmax[tt][stuff_type]-(*ip)))
			{
				st-=(tmax[tt][stuff_type]-(*ip));
				*ip=tmax[tt][stuff_type];
			}
			else
			{
				*ip+=st;
				st=0;
			}
		}
		tstart++;
		if (tstart>=8)
			tstart=0;
	}
	return(1);
}

#endif

#ifdef THMO_GP_STUFF

//static INLINE int       /* worth inlining -- (ThMO) */
//put_stuff3( map, type, stuff, stuff_type)
//  struct MAPPOINT  *map;
//  short            *type;
//  int              stuff;
//  const int        stuff_type;
int put_stuff3(struct MAPPOINT  *map,short *type,int stuff,int stuff_type)
{
  static int
    tstart3= 0;

  int           i, st, tst, *ip, tp=0;
  struct stack  stack[ 12], *ssp;       /* stack is really a pipe -- (ThMO)
*/

  /* we'll stack our found pointers so to avoid re-looping and
   * testing again
   * (ThMO)
   */

  tst= tstart3;

  /* can we put enough on the transport? */

  for ( ssp= stack, st= 0, i= 0;  i < 12;  i++)
    {
      if ( map[ t3[ tst]].flags & FLAG_IS_TRANSPORT)
        {
          switch ( main_types[ type[ t3[ tst]]].group)
            {
              case GROUP_TRACK:
                tp= tmax[ 0][ stuff_type];
                break;
              case GROUP_ROAD:
                tp= tmax[ 1][ stuff_type];
                break;
              case GROUP_RAIL:
                tp= tmax[ 2][ stuff_type];
                break;
              default:
//                __NORETURN do_error( "Bad transport type in put_stuff%c()",
//'3');
		do_error( "Bad transport type in put_stuff3"); // -- (IJP)
            }
          ip= &map[ t3[ tst]].int_1;
          ssp->ip= ip += stuff_type;            /* push it -- (ThMO) */
          ssp++->max_val= tp;
          st += tp- *ip;
          if ( st >= stuff)
            {
              ssp= stack;
              do
                {
                  tp= ssp->max_val- *ssp->ip;   /* pop it -- (ThMO) */
                  if ( ( stuff -= tp) < 0)
                    *ssp->ip += tp+ stuff;      /* == orig. stuff -- (ThMO)
*/
                  else
                    *ssp->ip= ssp->max_val;
                  ++ssp;
                }
              while ( stuff > 0);
              ++tst;
              tstart3= tst >= 12 ? 0 : tst;
              return( 1);
            }
        }
      if ( ++tst >= 12)
        tst = 0;
    }
  return( 0);
}

#else

int put_stuff3(int x,int y,int stuff,int stuff_type)
{
	static int tstart=0;
        static int tx[12]={  0, 1, 2, 3, 3, 3, 2, 1, 0,-1,-1,-1};
        static int ty[12]={ -1,-1,-1, 0, 1, 2, 3, 3, 3, 2, 1, 0};
	int i,j,st=0,tst,*ip,tt;
	tst=tstart;
// can we put enough on the transport?
	for (i=0;i<12;i++)
	{
		if ((mappoint[x+tx[tst]][y+ty[tst]].flags 
			& FLAG_IS_TRANSPORT)!=0)
		{
			switch(main_types[mappointtype[x+tx[tst]]\
				[y+ty[tst]]].group)
			{
				case(GROUP_TRACK): tt=0; break;
				case(GROUP_ROAD):  tt=1; break;
				case(GROUP_RAIL):  tt=2; break;
				default: do_error(
					"Bad transport type in put_stuff");
			}
			ip=&(mappoint[x+tx[tst]][y+ty[tst]].int_1);
			st+=tmax[tt][stuff_type]-(*(ip+stuff_type));
			if (st>=stuff)
				break;
		}
		tst++;
		if (tst>=12)
			tst=0;
	}
	if (st<stuff)
		return(0);
	st=stuff;
	while (st>0)
	{
		if ((mappoint[x+tx[tstart]][y+ty[tstart]].flags
			& FLAG_IS_TRANSPORT)!=0)
		{
			switch(main_types[mappointtype[x+tx[tstart]]\
				[y+ty[tstart]]].group)
			{
				case(GROUP_TRACK): tt=0; break;
				case(GROUP_ROAD):  tt=1; break;
				case(GROUP_RAIL):  tt=2; break;
				default: do_error(
					"Bad transport type in put_stuff");
			}
			ip=&(mappoint[x+tx[tstart]][y+ty[tstart]].int_1);
			ip+=stuff_type;
			if (st>(tmax[tt][stuff_type]-(*ip)))
			{
				st-=(tmax[tt][stuff_type]-(*ip));
				*ip=tmax[tt][stuff_type];
			}
			else
			{
				*ip+=st;
				st=0;
			}
		}
		tstart++;
		if (tstart>=12)
			tstart=0;
	}
	return(1);
}

#endif

#ifdef THMO_GP_STUFF

//static INLINE int       /* worth inlining -- (ThMO) */
//put_stuff4( map, type, stuff, stuff_type)
//  struct MAPPOINT  *map;
//  short            *type;
//  int              stuff;
//  const int       stuff_type;
int put_stuff4(struct MAPPOINT  *map,short *type,int stuff,int stuff_type)
{
  static int
    tstart4= 0;

  int           i, st, tst, *ip, tp=0;
  struct stack  stack[ 16], *ssp;       /* stack is really a pipe -- (ThMO)
*/

  /* we'll stack our found pointers so to avoid re-looping and
   * testing again
   * (ThMO)
   */

  tst= tstart4;

  /* can we put enough on the transport? */

  for ( ssp= stack, st= 0, i= 0;  i < 16;  i++)
    {
      if ( map[ t4[ tst]].flags & FLAG_IS_TRANSPORT)
        {
          switch ( main_types[ type[ t4[ tst]]].group)
            {
              case GROUP_TRACK:
                tp= tmax[ 0][ stuff_type];
                break;
              case GROUP_ROAD:
                tp= tmax[ 1][ stuff_type];
                break;
              case GROUP_RAIL:
                tp= tmax[ 2][ stuff_type];
                break;
              default:
//                __NORETURN do_error( "Bad transport type in put_stuff%c()",
//'4');
		do_error( "Bad transport type in put_stuff4"); // -- (IJP)
            }
          ip= &map[ t4[ tst]].int_1;
          ssp->ip= ip += stuff_type;            /* push it -- (ThMO) */
          ssp++->max_val= tp;
          st += tp- *ip;
          if ( st >= stuff)
            {
              ssp= stack;
              do
                {
                  tp= ssp->max_val- *ssp->ip;   /* pop it -- (ThMO) */
                  if ( ( stuff -= tp) < 0)
                    *ssp->ip += tp+ stuff;      /* == orig. stuff -- (ThMO)
*/
                  else
                    *ssp->ip= ssp->max_val;
                  ++ssp;
                }
              while ( stuff > 0);
              tstart4= ++tst & 15;
              return( 1);
            }
        }
      if ( ++tst >= 16)
        tst = 0;
    }
  return( 0);
}

#else

int put_stuff4(int x,int y,int stuff,int stuff_type)
{
	static int tstart=0;
        static int tx[16]={  0, 1, 2, 3, 4, 4, 4, 4, 3, 2, 1, 0,-1,-1,-1,-1};
        static int ty[16]={ -1,-1,-1,-1, 0, 1, 2, 3, 4, 4, 4, 4, 3, 2, 1, 0};
	int i,j,st=0,tst,*ip,tt;
	tst=tstart;
// can we put enough on the transport?
	for (i=0;i<16;i++)
	{
		if ((mappoint[x+tx[tst]][y+ty[tst]].flags 
			& FLAG_IS_TRANSPORT)!=0)
		{
			switch(main_types[mappointtype[x+tx[tst]]\
				[y+ty[tst]]].group)
			{
				case(GROUP_TRACK): tt=0; break;
				case(GROUP_ROAD):  tt=1; break;
				case(GROUP_RAIL):  tt=2; break;
				default: do_error(
					"Bad transport type in put_stuff");
			}
			ip=&(mappoint[x+tx[tst]][y+ty[tst]].int_1);
			st+=tmax[tt][stuff_type]-(*(ip+stuff_type));
			if (st>=stuff)
				break;
		}
		tst++;
		if (tst>=16)
			tst=0;
	}
	if (st<stuff)
		return(0);
	st=stuff;
	while (st>0)
	{
		if ((mappoint[x+tx[tstart]][y+ty[tstart]].flags
			& FLAG_IS_TRANSPORT)!=0)
		{
			switch(main_types[mappointtype[x+tx[tstart]]\
				[y+ty[tstart]]].group)
			{
				case(GROUP_TRACK): tt=0; break;
				case(GROUP_ROAD):  tt=1; break;
				case(GROUP_RAIL):  tt=2; break;
				default: do_error(
					"Bad transport type in put_stuff");
			}
			ip=&(mappoint[x+tx[tstart]][y+ty[tstart]].int_1);
			ip+=stuff_type;
			if (st>(tmax[tt][stuff_type]-(*ip)))
			{
				st-=(tmax[tt][stuff_type]-(*ip));
				*ip=tmax[tt][stuff_type];
			}
			else
			{
				*ip+=st;
				st=0;
			}
		}
		tstart++;
		if (tstart>=16)
			tstart=0;
	}
	return(1);
}

#endif
