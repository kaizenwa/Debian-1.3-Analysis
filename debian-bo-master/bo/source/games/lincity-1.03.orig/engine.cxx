// engine.cxx	 part of lin-city
// Copyright (c) I J Peters 1995,1996.  Please read the file 'COPYRIGHT'.

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
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
#include "engine.h"


void do_time_step(void)
{
	static int flag=0;
	int x,y,t,xx,yy;
	char s[50];
#ifdef DEBUG_ENGINE
	printf("In do_time_step\n");
#endif
	if (flag==0)
	{
		for (x=0;x<WORLD_SIDE_LEN;x++)
		{
			mappoint_array_x[x]=x;
			mappoint_array_y[x]=x;
		}
		flag=1;
	}
	total_time++;
	if (total_time%NUMOF_DAYS_IN_MONTH==1)
	{
		sprintf(s,"Date %s %04d ",months[(total_time%NUMOF_DAYS_IN_YEAR)
			/NUMOF_DAYS_IN_MONTH],total_time/NUMOF_DAYS_IN_YEAR);
		Fgl_write(10,440,s);
	}
	population=0;
	starving_population=0;
	food_in_markets=0;
	jobs_in_markets=0;
	coal_in_markets=0;
	goods_in_markets=0;
	ore_in_markets=0;
	steel_in_markets=0;
	shuffle_mappoint_array();
	for (yy=0;yy<WORLD_SIDE_LEN;yy++)
	for (xx=0;xx<WORLD_SIDE_LEN;xx++)
	{
// indirection to rand array to stop lots of linear effects
		x=mappoint_array_x[xx];
		y=mappoint_array_y[yy];
		t=mappointtype[x][y];
		if (t==CST_USED || t==CST_GREEN)
//  || main_types[t].group==GROUP_RIVER)
			continue;
// put ageing out stuff here

		switch (main_types[t].group)
		{
			case(GROUP_TRACK): do_track(x,y); break;
			case(GROUP_RAIL):  do_rail(x,y);  break;
			case(GROUP_ROAD):  do_road(x,y);  break;
			case(GROUP_ORGANIC_FARM): 
				do_organic_farm(x,y); break;
			case(GROUP_MARKET): do_market(x,y); break;
			case(GROUP_RESIDENCE): do_residence(x,y); break;
			case(GROUP_POWER_LINE): do_power_line(x,y); break;
			case(GROUP_POWER_SOURCE): 
				do_power_source(x,y); break;
			case(GROUP_SUBSTATION): 
				do_power_substation(x,y); break;
			case(GROUP_COALMINE): do_coalmine(x,y); break;
			case(GROUP_POWER_SOURCE_COAL):
				do_power_source_coal(x,y); break;
			case(GROUP_INDUSTRY_L): do_industry_l(x,y); break;
			case(GROUP_INDUSTRY_H): do_industry_h(x,y); break;
			case(GROUP_COMMUNE): do_commune(x,y); break;
			case(GROUP_OREMINE): do_oremine(x,y); break;
			case(GROUP_PORT): do_port(x,y); break;
			case(GROUP_TIP): do_tip(x,y); break;
			case(GROUP_PARKLAND): do_parkland(x,y); break;
			case(GROUP_UNIVERSITY): do_university(x,y); break;
			case(GROUP_RECYCLE): do_recycle(x,y); break;
			case(GROUP_HEALTH): do_health_centre(x,y); break;
			case(GROUP_ROCKET): do_rocket_pad(x,y); break;
			case(GROUP_WINDMILL): do_windmill(x,y); break;
			case(GROUP_MONUMENT): do_monument(x,y); break;
			case(GROUP_SCHOOL): do_school(x,y); break;
			case(GROUP_BLACKSMITH): do_blacksmith(x,y); break;
			case(GROUP_MILL): do_mill(x,y); break;
			case(GROUP_POTTERY): do_pottery(x,y); break;
			case(GROUP_FIRESTATION): do_firestation(x,y); break;
			case(GROUP_CRICKET): do_cricket(x,y); break;
			case(GROUP_FIRE): do_fire(x,y); break;
			case(GROUP_SHANTY): do_shanty(x,y); break;
		}
	}
}

void do_residence(int x,int y)
{
// int_1 is a job swingometer to choose +/- JOB_SWING% of normal
// int_2 is the date of the last starve
// int 3 is the real time for the next icon update
// int_4 is the birth rate modifier.
// int_5 is the death rate modifier.
	int p,bad=35,good=30,r,po,swing,hc=0,brm=0,drm=0,cc=0;
	p=mappoint[x][y].population;
	if ((mappoint[x][y].flags&FLAG_HEALTH_COVER)!=0)
	{
		brm+=RESIDENCE_BRM_HEALTH;
		good+=15;
		hc=1;
	}
	if ((mappoint[x][y].flags&FLAG_FIRE_COVER)==0)
		bad+=5;
	else
		good+=15;
        if ((mappoint[x][y].flags&FLAG_CRICKET_COVER)!=0)
	{
                good+=20;
		cc=CRICKET_JOB_SWING;
	}
// normal deaths + pollution deaths
	po=((mappointpol[x][y]/50)+1);
	if ((RESIDENCE_BASE_DR-mappoint[x][y].int_5-po)>1)
		r=rand()%(RESIDENCE_BASE_DR-mappoint[x][y].int_5-po);
	else
		r=2;
	if (p>0 && (r<po))
	{
//		printf("r=%d po=%d hc=%d ",r,po,hc);
		if (r==0 || hc==0)
			p--;
		else if (hc!=0 && po>10 && rand()%4==0)
		{
			p--;
			unnat_deaths++;
			total_pollution_deaths++;
			pollution_deaths_history+=1.0;
			bad+=100;
		}
		if (r>0 && hc==0)
		{
			unnat_deaths++;
			total_pollution_deaths++;
			pollution_deaths_history+=1.0;
			bad+=100;
		}
	}	
// normal births - must have food and jobs... and people
	if ((mappoint[x][y].flags 
		& (FLAG_FED+FLAG_EMPLOYED))
		==(FLAG_FED+FLAG_EMPLOYED)
		&& (rand()%(RESIDENCE_BASE_BR+mappoint[x][y].int_4)
			==1) && p>0)
	{
		p++;
		total_births++;
		good+=50;
	}
// are people starving.
	if ((mappoint[x][y].flags & FLAG_FED)==0 && p>0)
	{
		if (rand()%DAYS_PER_STARVE==1)
		{
			p--;
			unnat_deaths++;
			total_starve_deaths++;
			starve_deaths_history+=1.0;
		}
//		if (p<0)
//			p=0;
		starving_population+=p;
		bad+=250;
		drm+=100;
		mappoint[x][y].int_2=total_time;  // for the starve screen
	}
// kick one out if overpopulated
//	if (mappointtype[x][y]==CST_RESIDENCE_0)
//	{
//		printf("Error CST_RESIDENCE_0 active\n");
//	}
//	else if (mappointtype[x][y]==CST_RESIDENCE_LL)
	if (mappointtype[x][y]==CST_RESIDENCE_LL)
	{
		brm+=RESIDENCE1_BRM;
		drm+=p*8;
		if (p>50)
		{
			p--;
			people_pool++;
			brm+=20;
		}
	}
        else if (mappointtype[x][y]==CST_RESIDENCE_ML)
        {
		brm+=RESIDENCE2_BRM;
		drm+=p*3;
                if (p>100)
                {
                        p--;
                        people_pool++;
			brm+=10;
                }
        }
        else if (mappointtype[x][y]==CST_RESIDENCE_HL)
        {
		brm+=RESIDENCE3_BRM;
		drm+=p;
		good+=40;
                if (p>200)
                {
                        p--;
                        people_pool++;
			brm+=10;
                }
        }
        else if (mappointtype[x][y]==CST_RESIDENCE_LH)
        {
		brm+=RESIDENCE4_BRM;
		drm+=p*5;
                if (p>100)
                {
                        p--;
                        people_pool++;
			brm+=20;
                }
        }
        else if (mappointtype[x][y]==CST_RESIDENCE_MH)
        {
		brm+=RESIDENCE5_BRM;
		drm+=p/2;
                if (p>200)
                {
                        p--;
                        people_pool++;
			brm+=10;
                }
        }
        else if (mappointtype[x][y]==CST_RESIDENCE_HH)
        {
		good+=100;
		brm+=RESIDENCE6_BRM;
		drm+=p; 
                if (p>400)
                {
                        p--;
                        people_pool++;
			brm+=10;
                }
        }

	population+=p;

// now get power
	if (get_power(x,y,POWER_RES_OVERHEAD
		+(POWER_USE_PER_PERSON*p),0)!=0)
	{
		mappoint[x][y].flags|=FLAG_POWERED;
		mappoint[x][y].flags|=FLAG_HAD_POWER;
		good+=10;
	}
	else
	{
		mappoint[x][y].flags&=(0xffffffff-FLAG_POWERED);
		bad+=15;
		if ((mappoint[x][y].flags&FLAG_HAD_POWER)!=0)
			bad+=50;
	}
// now get fed
	if (get_food(x,y,p)!=0)
	{
		mappoint[x][y].flags|=FLAG_FED;
		good+=10;
	}
	else
		mappoint[x][y].flags&=(0xffffffff-FLAG_FED);
// now supply jobs and buy goods if employed
	if (mappoint[x][y].int_1>0)
		swing=JOB_SWING+(hc*HC_JOB_SWING)+cc;
	else
		swing=-(JOB_SWING+(hc*HC_JOB_SWING)+cc);
	if (put_jobs(x,y,((p*(WORKING_POP_PERCENT+swing))/100))!=0)
		{
			mappoint[x][y].flags|=FLAG_EMPLOYED;
			mappoint[x][y].int_1++;
			if (mappoint[x][y].int_1>10)
				mappoint[x][y].int_1=10;
			good+=20;
			if (get_goods(x,y,p/4)!=0)
			{
				good+=10;
				if (get_power(x,y,p/2,0)!=0) // goods use power
				{
					good+=5;
					brm+=10;
//     buy more goods if got power for them
					if (get_goods(x,y,p/4)!=0)
						good+=5;
				}
				else
					bad+=5;
			}
		}
	else if (mappoint[x][y].int_1<10)
	{
		mappoint[x][y].flags&=(0xffffffff-FLAG_EMPLOYED);
		mappoint[x][y].int_1-=11;
		if (mappoint[x][y].int_1<-300)
			mappoint[x][y].int_1=-300;
		unemployed_population+=p;
		total_unemployed_days+=p;
		if (total_unemployed_days>=NUMOF_DAYS_IN_YEAR)
		{
			total_unemployed_years++;
// think we're ok doing this, max of about 120 added each time.
			total_unemployed_days-=NUMOF_DAYS_IN_YEAR;
			unemployed_history+=1.0;
		}
		unemployment_cost+=p; // hmmm
		bad+=70;
	}
	else
	{
		mappoint[x][y].int_1-=20;
		bad+=50;
	}
	drm+=p/4;
// people_pool stuff
	bad+=p/2;
	bad+=mappointpol[x][y]/20;
	good+=people_pool/27;
	r=rand()%((good+bad)*RESIDENCE_PPM);
	if (r<bad)
	{
		if (p>MIN_RES_POPULATION)
		{
			p--;
			people_pool++;
		}
	}
	else if (people_pool>0 && r>((good+bad)*(RESIDENCE_PPM-1)+bad))
	{
		p++;
		people_pool--;
	}
	mappoint[x][y].population=p;
	mappoint[x][y].int_4=brm;
	mappoint[x][y].int_5=drm;
}

void debug_print(int v)
{
	char s[100];
	sprintf(s,"%d    ",v);
	Fgl_write(2,480-32,s);
}


void do_power_source(int x,int y)
{
//  int_1 is the power to give away,  this must be >= POWER_LINE_CAPACITY
//  int_2 is the tech level when it was built.
//  for it to pass it to a power line. Not added to if > POWER_LINE_CAPACITY
#ifdef DEBUG_ENGINE
	printf("In do_power_source\n");
#endif	
	if (mappoint[x][y].int_1>POWER_LINE_CAPACITY)
		;  // do nothing
	else 
	{
		mappoint[x][y].int_1+=(POWERS_SOLAR_OUTPUT
			+((POWERS_SOLAR_OUTPUT*2)*mappoint[x][y].int_2)
			/MAX_TECH_LEVEL/2);
		return;
	}
// Now try and give it away. Only two places it can go; up or left
// Do upper one first. (Maybe change to rand at some point.)
	if (main_types[mappointtype[x][y-1]].group==GROUP_POWER_LINE
		&& mappoint[x][y-1].int_1==0)
	{
		if (get_jobs(x,y,50)!=0)
		{
			mappoint[x][y].int_1-=POWER_LINE_CAPACITY;
			mappoint[x][y-1].int_1=1;
	                if (mappointtype[x][y-1]>CST_POWERL_LUDR_L)
	                        mappointtype[x][y-1]-=11;
			power_made++;
		}
	}
	else if (main_types[mappointtype[x-1][y]].group==GROUP_POWER_LINE
		&& mappoint[x-1][y].int_1==0)
	{
		if (get_jobs(x,y,50)!=0)
		{
			mappoint[x][y].int_1-=POWER_LINE_CAPACITY;
			mappoint[x-1][y].int_1=1;
	                if (mappointtype[x-1][y]>CST_POWERL_LUDR_L)
	                        mappointtype[x-1][y]-=11;
			power_made++;
		}
	}

}	

void do_power_source_coal(int x,int y)
{
//  int_1 is the power to give away,  this must be >= POWER_LINE_CAPACITY
//  for it to pass it to a power line. Not added to if > POWER_LINE_CAPACITY
// int_2 is the coal at the power station
// int_3 is the collected jobs, if there's a shortage.
// int_4 is the tech level when built

#ifdef DEBUG_ENGINE
	printf("In do_power_source\n");
#endif	
// first get some coal
	if (mappoint[x][y].int_2<MAX_COAL_AT_POWER_STATION
			&& ( (mappoint[x-1][y+1].flags & FLAG_IS_TRANSPORT)
				!=0 ) 
			&& mappoint[x-1][y+1].int_3>0)
		if (get_jobs(x,y,JOBS_LOAD_COAL)!=0)
		{
			mappoint[x][y].int_2+=(mappoint[x-1][y+1].int_3/2
				+((mappoint[x-1][y+1].int_3)%2));
			mappoint[x-1][y+1].int_3/=2;
			mappointpol[x][y]++;
		}
        if (mappoint[x][y].int_2<MAX_COAL_AT_POWER_STATION
                        && ( (mappoint[x+1][y-1].flags & FLAG_IS_TRANSPORT)
				!=0 )
			&& mappoint[x+1][y-1].int_3>0)
                if (get_jobs(x,y,JOBS_LOAD_COAL)!=0)
                {
                        mappoint[x][y].int_2+=(mappoint[x+1][y-1].int_3/2
                                +((mappoint[x+1][y-1].int_3)%2));
                        mappoint[x+1][y-1].int_3/=2;
			mappointpol[x][y]++;
                }
// get jobs
	if ((mappoint[x][y].int_3+JOBS_COALPS_GENERATE+10)
		< MAX_JOBS_AT_COALPS)
		if (get_jobs(x,y,JOBS_COALPS_GENERATE+10)!=0)
			mappoint[x][y].int_3+=JOBS_COALPS_GENERATE+10;

// generate electricity
	if (mappoint[x][y].int_1<POWER_LINE_CAPACITY)
	{
		if (mappoint[x][y].int_2>POWERS_COAL_OUPUT/500)
		{
			if (mappoint[x][y].int_3>JOBS_COALPS_GENERATE)
			{
				mappoint[x][y].int_3-=JOBS_COALPS_GENERATE;
				mappoint[x][y].int_2-=POWERS_COAL_OUPUT/600;
				coal_used+=POWERS_COAL_OUPUT/600;
				mappoint[x][y].int_1+=(POWERS_COAL_OUPUT
			+(POWERS_COAL_OUPUT*mappoint[x][y].int_4)
					/MAX_TECH_LEVEL);
				mappointpol[x][y]+=POWERS_COAL_POLLUTION;
			}
//			else if (get_jobs(x,y,JOBS_COALPS_GENERATE/10)!=0)
//				mappoint[x][y].int_3+=JOBS_COALPS_GENERATE/10;
		}
//		else
//			mappoint[x][y].int_2=0;
	}
// choose a graphic
        if (mappoint[x][y].int_2>(MAX_COAL_AT_POWER_STATION
		-(MAX_COAL_AT_POWER_STATION/5)))
                mappointtype[x][y]=CST_POWERS_COAL_FULL;
        else if (mappoint[x][y].int_2>(MAX_COAL_AT_POWER_STATION/2))
                mappointtype[x][y]=CST_POWERS_COAL_MED;
        else if (mappoint[x][y].int_2>(MAX_COAL_AT_POWER_STATION/10))
                mappointtype[x][y]=CST_POWERS_COAL_LOW;
        else
                mappointtype[x][y]=CST_POWERS_COAL_EMPTY;


	if (mappoint[x][y].int_1<POWER_LINE_CAPACITY)
		return;   // not enough to give away
// Now try and give it away. Only two places it can go; up or left
// Do upper one first. (Maybe change to rand at some point.)
	if (main_types[mappointtype[x][y-1]].group==GROUP_POWER_LINE
		&& mappoint[x][y-1].int_1==0)
	{
		if (get_jobs(x,y,50)!=0)
		{
			mappoint[x][y].int_1-=POWER_LINE_CAPACITY;
			mappoint[x][y-1].int_1=1;
	                if (mappointtype[x][y-1]>CST_POWERL_LUDR_L)
	                        mappointtype[x][y-1]-=11;
			power_made++;
		}
	}
	else if (main_types[mappointtype[x-1][y]].group==GROUP_POWER_LINE
		&& mappoint[x-1][y].int_1==0)
	{
		if (get_jobs(x,y,50)!=0)
		{
			mappoint[x][y].int_1-=POWER_LINE_CAPACITY;
			mappoint[x-1][y].int_1=1;
	                if (mappointtype[x-1][y]>CST_POWERL_LUDR_L)
	                        mappointtype[x-1][y]-=11;
			power_made++;
		}
	}

}	

void do_industry_l(int x,int y)
{
	int goods=0;
// int_1 is the goods produced this month so far
// int_2 is the amount of goods in store.
// int_3 is the amount of ore in store.
// int_4 is the amount of steel in store.
// int_5 is the jobs stored.
// int_6 is the percent of capacity last month.
// int 7 is the next animation frame time.

// first get some jobs
	if (mappoint[x][y].int_5<MAX_JOBS_AT_INDUSTRY_L-INDUSTRY_L_GET_JOBS)
	{
		if (get_jobs(x,y,INDUSTRY_L_GET_JOBS)!=0)
			mappoint[x][y].int_5+=INDUSTRY_L_GET_JOBS;
		else if (get_jobs(x,y,INDUSTRY_L_GET_JOBS/10)!=0)
			mappoint[x][y].int_5+=INDUSTRY_L_GET_JOBS/10;
	}
// if we don't have enough jobs we can't do anything
	if (mappoint[x][y].int_5<MIN_JOBS_AT_INDUSTRY_L)
		return;
// get some ore
	if (mappoint[x][y].int_3<(MAX_ORE_AT_INDUSTRY_L
			-INDUSTRY_L_GET_ORE))
	{
		if ((mappoint[x-1][y].flags & FLAG_IS_TRANSPORT)!=0
			&& mappoint[x-1][y].int_5>0)
		{
			if (mappoint[x-1][y].int_5>=INDUSTRY_L_GET_ORE)
			{
				mappoint[x][y].int_3+=INDUSTRY_L_GET_ORE;
				mappoint[x-1][y].int_5-=INDUSTRY_L_GET_ORE;
			}
			else
			{
				mappoint[x][y].int_3+=mappoint[x-1][y].int_5;
				mappoint[x-1][y].int_5=0;
			}
			mappoint[x][y].int_5-=INDUSTRY_L_JOBS_LOAD_ORE;
		}
	}
// do we still need some ore?
        if (mappoint[x][y].int_3<(MAX_ORE_AT_INDUSTRY_L
                        -INDUSTRY_L_GET_ORE))
        {
                if ((mappoint[x][y-1].flags & FLAG_IS_TRANSPORT)!=0
                        && mappoint[x][y-1].int_5>0)
                {
                        if (mappoint[x][y-1].int_5>=INDUSTRY_L_GET_ORE)
                        {
                                mappoint[x][y].int_3+=INDUSTRY_L_GET_ORE;
                                mappoint[x][y-1].int_5-=INDUSTRY_L_GET_ORE;
                        }
                        else
                        {
				mappoint[x][y].int_3+=mappoint[x][y-1].int_5;
                                mappoint[x][y-1].int_5=0;
                        }
			mappoint[x][y].int_5-=INDUSTRY_L_JOBS_LOAD_ORE;
                }
        }
// then get some steel
        if (mappoint[x][y].int_4<(MAX_STEEL_AT_INDUSTRY_L
                        -INDUSTRY_L_GET_STEEL))
        {
                if ((mappoint[x-1][y].flags & FLAG_IS_TRANSPORT)!=0
                        && mappoint[x-1][y].int_6>0)
                {
                        if (mappoint[x-1][y].int_6>=INDUSTRY_L_GET_STEEL)
                        {
                                mappoint[x][y].int_4+=INDUSTRY_L_GET_STEEL;
                                mappoint[x-1][y].int_6-=INDUSTRY_L_GET_STEEL;
                        }
                        else
                        {
				mappoint[x][y].int_4
					+=mappoint[x-1][y].int_6;
                                mappoint[x-1][y].int_6=0;
                        }
			mappoint[x][y].int_5-=INDUSTRY_L_JOBS_LOAD_STEEL;
                }
        }
// do we still need some steel?
        if (mappoint[x][y].int_4<(MAX_STEEL_AT_INDUSTRY_L
                        -INDUSTRY_L_GET_STEEL))
        {
                if ((mappoint[x][y-1].flags & FLAG_IS_TRANSPORT)!=0
                        && mappoint[x][y-1].int_6>0)
                {
                        if (mappoint[x][y-1].int_6>=INDUSTRY_L_GET_STEEL)
                        {
                                mappoint[x][y].int_4+=INDUSTRY_L_GET_STEEL;
                                mappoint[x][y-1].int_6-=INDUSTRY_L_GET_STEEL;
                        }
                        else
                        {
				mappoint[x][y].int_4
					+=mappoint[x][y-1].int_6;
                                mappoint[x][y-1].int_6=0;
                        }
			mappoint[x][y].int_5-=INDUSTRY_L_JOBS_LOAD_STEEL;
                }
        }
// now make some goods
	if (mappoint[x][y].int_2<(MAX_GOODS_AT_INDUSTRY_L
		-(INDUSTRY_L_MAKE_GOODS*8)) && mappoint[x][y].int_3
		>= INDUSTRY_L_ORE_USED)
	{
		goods+=INDUSTRY_L_MAKE_GOODS;
		mappoint[x][y].int_3-=INDUSTRY_L_ORE_USED;
		ore_used+=INDUSTRY_L_ORE_USED;
		mappoint[x][y].int_5-=INDUSTRY_L_JOBS_USED;
		mappointpol[x][y]+=INDUSTRY_L_POLLUTION;
	
// multiply by 2 if we have steel.
		if (mappoint[x][y].int_4>=INDUSTRY_L_STEEL_USED)
		{
			mappoint[x][y].int_4-=INDUSTRY_L_STEEL_USED;
			goods+=goods;
		}
// multipy by 4 if we can get power.
		
		if (mappoint[x][y].int_3>=INDUSTRY_L_ORE_USED
			&& get_power(x,y,goods*10,1)!=0)
		{
			goods*=4;
			mappoint[x][y].flags|=FLAG_POWERED;
			// and use more ore
			mappoint[x][y].int_3-=INDUSTRY_L_ORE_USED;
			ore_used+=INDUSTRY_L_ORE_USED;
		}
		else
			mappoint[x][y].flags&=(0xffffffff-FLAG_POWERED);
	}

	mappoint[x][y].int_1+=goods;
	mappoint[x][y].int_2+=goods;
	goods_made+=goods;

// now sell the goods to the road/rail/track
	if (main_types[mappointtype[x][y-1]].group
		==GROUP_ROAD && (MAX_GOODS_ON_ROAD
			-mappoint[x][y-1].int_4)
			<= mappoint[x][y].int_2)
	{
		mappoint[x][y].int_2-=(MAX_GOODS_ON_ROAD
			-mappoint[x][y-1].int_4);
		mappoint[x][y-1].int_4=MAX_GOODS_ON_ROAD;
	}
        else if (main_types[mappointtype[x][y-1]].group
                ==GROUP_RAIL && (MAX_GOODS_ON_RAIL
                        -mappoint[x][y-1].int_4)
                        <= mappoint[x][y].int_2)
        {
                mappoint[x][y].int_2-=(MAX_GOODS_ON_RAIL
                        -mappoint[x][y-1].int_4);
                mappoint[x][y-1].int_4=MAX_GOODS_ON_RAIL;
        }
        else if (main_types[mappointtype[x][y-1]].group
                ==GROUP_TRACK && (MAX_GOODS_ON_TRACK
                        -mappoint[x][y-1].int_4)
                        <= mappoint[x][y].int_2)
        {
                mappoint[x][y].int_2-=(MAX_GOODS_ON_TRACK
                        -mappoint[x][y-1].int_4);
                mappoint[x][y-1].int_4=MAX_GOODS_ON_TRACK;
        }


        if (main_types[mappointtype[x-1][y]].group
                ==GROUP_ROAD && (MAX_GOODS_ON_ROAD
                        -mappoint[x-1][y].int_4)
                        <= mappoint[x][y].int_2)
        {
                mappoint[x][y].int_2-=(MAX_GOODS_ON_ROAD
                        -mappoint[x-1][y].int_4);
                mappoint[x-1][y].int_4=MAX_GOODS_ON_ROAD;
        }
        else if (main_types[mappointtype[x-1][y]].group
                ==GROUP_RAIL && (MAX_GOODS_ON_RAIL
                        -mappoint[x-1][y].int_4)
                        <= mappoint[x][y].int_2)
        {
                mappoint[x][y].int_2-=(MAX_GOODS_ON_RAIL
                        -mappoint[x-1][y].int_4);
                mappoint[x-1][y].int_4=MAX_GOODS_ON_RAIL;
        }
        else if (main_types[mappointtype[x-1][y]].group
                ==GROUP_TRACK && (MAX_GOODS_ON_TRACK
                        -mappoint[x-1][y].int_4)
                        <= mappoint[x][y].int_2)
        {
                mappoint[x][y].int_2-=(MAX_GOODS_ON_TRACK
                        -mappoint[x-1][y].int_4);
                mappoint[x-1][y].int_4=MAX_GOODS_ON_TRACK;
        }

// now choose a graphic every month
	if ((total_time%NUMOF_DAYS_IN_MONTH)==NUMOF_DAYS_IN_MONTH-1)
	{
                mappoint[x][y].int_6=(mappoint[x][y].int_1)
                                /(INDUSTRY_L_MAKE_GOODS*8);
                mappoint[x][y].int_1=0;
		if (mappoint[x][y].int_6>80)
		{
                        switch(mappointtype[x][y])
                        {
                                case(CST_INDUSTRY_L_H1):
                                case(CST_INDUSTRY_L_H2):
                                case(CST_INDUSTRY_L_H3):
                                case(CST_INDUSTRY_L_H4): break;
                                default:
               			mappointtype[x][y]=CST_INDUSTRY_L_H1;
			}
		}
		else if (mappoint[x][y].int_6>55)
		{
			switch(mappointtype[x][y])
			{
                                case(CST_INDUSTRY_L_M1):
                                case(CST_INDUSTRY_L_M2):
                                case(CST_INDUSTRY_L_M3):
                                case(CST_INDUSTRY_L_M4): break;
				default:
				mappointtype[x][y]=CST_INDUSTRY_L_M1;
			}
		}
                else if (mappoint[x][y].int_6>25)
                {
                        switch(mappointtype[x][y])
                        {
                                case(CST_INDUSTRY_L_L1):
                                case(CST_INDUSTRY_L_L2):
                                case(CST_INDUSTRY_L_L3):
                                case(CST_INDUSTRY_L_L4): break;
                                default:
                                mappointtype[x][y]=CST_INDUSTRY_L_L1;
                        }
                }
		else if (mappoint[x][y].int_6>0)
                {
                        switch(mappointtype[x][y])
                        {
                                case(CST_INDUSTRY_L_Q1):
                                case(CST_INDUSTRY_L_Q2):
                                case(CST_INDUSTRY_L_Q3):
                                case(CST_INDUSTRY_L_Q4): break;
                                default:
				mappointtype[x][y]=CST_INDUSTRY_L_Q1;
			}
		}
		else
			mappointtype[x][y]=CST_INDUSTRY_L_C;
	}
// now animate
        if (real_time>=mappoint[x][y].int_7)
        {
                mappoint[x][y].int_7=real_time+INDUSTRY_L_ANIM_SPEED;
                switch (mappointtype[x][y])
                {
                        case (CST_INDUSTRY_L_Q1):
                                mappointtype[x][y]=CST_INDUSTRY_L_Q2; break;
                        case (CST_INDUSTRY_L_Q2):
                                mappointtype[x][y]=CST_INDUSTRY_L_Q3; break;
                        case (CST_INDUSTRY_L_Q3):
                                mappointtype[x][y]=CST_INDUSTRY_L_Q4; break;
                        case (CST_INDUSTRY_L_Q4):
                                mappointtype[x][y]=CST_INDUSTRY_L_Q1; break;
                        case (CST_INDUSTRY_L_L1):
                                mappointtype[x][y]=CST_INDUSTRY_L_L2; break;
                        case (CST_INDUSTRY_L_L2):
                                mappointtype[x][y]=CST_INDUSTRY_L_L3; break;
                        case (CST_INDUSTRY_L_L3):
                                mappointtype[x][y]=CST_INDUSTRY_L_L4; break;
                        case (CST_INDUSTRY_L_L4):
                                mappointtype[x][y]=CST_INDUSTRY_L_L1; break;
                        case (CST_INDUSTRY_L_M1):
                                mappointtype[x][y]=CST_INDUSTRY_L_M2; break;
                        case (CST_INDUSTRY_L_M2):
                                mappointtype[x][y]=CST_INDUSTRY_L_M3; break;
                        case (CST_INDUSTRY_L_M3):
                                mappointtype[x][y]=CST_INDUSTRY_L_M4; break;
                        case (CST_INDUSTRY_L_M4):
                                mappointtype[x][y]=CST_INDUSTRY_L_M1; break;
                        case (CST_INDUSTRY_L_H1):
                                mappointtype[x][y]=CST_INDUSTRY_L_H2; break;
                        case (CST_INDUSTRY_L_H2):
                                mappointtype[x][y]=CST_INDUSTRY_L_H3; break;
                        case (CST_INDUSTRY_L_H3):
                                mappointtype[x][y]=CST_INDUSTRY_L_H4; break;
                        case (CST_INDUSTRY_L_H4):
                                mappointtype[x][y]=CST_INDUSTRY_L_H1; break;
		}
	}
}

void do_industry_h(int x,int y)
{
	int rawm,steel=0;
// int_1 is the steel produced this month so far
// int_2 is the amount of steel in store
// int_3 is the amount of raw materials in store (ore)
// int_4 is the coal in store
// int_5 is the percent max production last month
// int_6 is the time of the next animation frame.

// see if there's any raw materials (ore) on the road/rail. If so, use some
// jobs to get it.
// first get some ore
        if (mappoint[x][y].int_3<MAX_ORE_AT_INDUSTRY_H
                        && ((mappoint[x-1][y].flags & FLAG_IS_TRANSPORT)
				!=0) && mappoint[x-1][y].int_5>0)
                if (get_jobs(x,y,JOBS_LOAD_ORE)!=0)
                {
                        mappoint[x][y].int_3+=(mappoint[x-1][y].int_5/2
                                +((mappoint[x-1][y].int_5)%2));
                        mappoint[x-1][y].int_5/=2;
                }
        if (mappoint[x][y].int_3<MAX_ORE_AT_INDUSTRY_H
                        && ((mappoint[x][y-1].flags & FLAG_IS_TRANSPORT)
				!=0) && mappoint[x][y-1].int_5>0)
                if (get_jobs(x,y,JOBS_LOAD_ORE)!=0)
                {
                        mappoint[x][y].int_3+=(mappoint[x][y-1].int_5/2
                                +((mappoint[x][y-1].int_5)%2));
                        mappoint[x][y-1].int_5/=2;
                }
// then get some coal if needed
        if (mappoint[x][y].int_4<MAX_COAL_AT_INDUSTRY_H
                        && ((mappoint[x-1][y].flags & FLAG_IS_TRANSPORT)
				!=0) && mappoint[x-1][y].int_3>0)
                if (get_jobs(x,y,JOBS_LOAD_COAL)!=0)
                {
                        mappoint[x][y].int_4+=(mappoint[x-1][y].int_3/2
                                +((mappoint[x-1][y].int_3)%2));
                        mappoint[x-1][y].int_3/=2;
                }
        if (mappoint[x][y].int_4<MAX_ORE_AT_INDUSTRY_H
                        && ((mappoint[x][y-1].flags & FLAG_IS_TRANSPORT)
				!=0) && mappoint[x][y-1].int_3>0)
                if (get_jobs(x,y,JOBS_LOAD_COAL)!=0)
                {
                        mappoint[x][y].int_4+=(mappoint[x][y-1].int_3/2
                                +((mappoint[x][y-1].int_3)%2));
                        mappoint[x][y-1].int_3/=2;
                }

	rawm=mappoint[x][y].int_3;
	if (rawm>MAX_MADE_AT_INDUSTRY_H)
		rawm=MAX_MADE_AT_INDUSTRY_H;
// turn it into steel
	if (rawm>0 && mappoint[x][y].int_2<(MAX_STEEL_ON_RAIL*10))
	{
		if (get_jobs(x,y,((rawm/2)/JOBS_MAKE_STEEL)+1)!=0)
		{
			steel=(rawm)/ORE_MAKE_STEEL;
			mappointpol[x][y]+=INDUSTRY_H_POLLUTION/2;
		}
		else if (get_jobs(x,y,((rawm/4)/JOBS_MAKE_STEEL)+1)!=0)
		{
			steel=(rawm/2)/ORE_MAKE_STEEL;
			mappointpol[x][y]+=INDUSTRY_H_POLLUTION/4;
		}
		else if (get_jobs(x,y,((rawm/10)/JOBS_MAKE_STEEL)+1)!=0)
		{
			steel=(rawm/5)/ORE_MAKE_STEEL;
			mappointpol[x][y]+=INDUSTRY_H_POLLUTION/10;
		}
	}
// do this here rather than later 'cos maybe steel/=5
	mappoint[x][y].int_3-=steel*ORE_MAKE_STEEL;
	ore_used+=steel*ORE_MAKE_STEEL;
// check there was enough electricity, or back up to 1/10 of the 
// production. ie same work and material useage for less production. 
	if (get_power(x,y,steel*POWER_MAKE_STEEL,1)==0)
	{
		if (mappoint[x][y].int_4<(steel*2))
		{
			mappoint[x][y].flags&=(0xffffffff-FLAG_POWERED);
			steel/=5;
		}
		else
		{
			mappoint[x][y].int_4-=(steel*2);
			coal_used+=(steel*2);
			mappoint[x][y].flags|=FLAG_POWERED;
		}
	}
	else
		mappoint[x][y].flags|=FLAG_POWERED;
	mappoint[x][y].int_1+=steel;
	mappoint[x][y].int_2+=steel;
// now sell the steel to the road/rail
	if (main_types[mappointtype[x][y-1]].group
		==GROUP_ROAD && (MAX_STEEL_ON_ROAD
			-mappoint[x][y-1].int_6)
			<= mappoint[x][y].int_2)
	{
		mappoint[x][y].int_2-=(MAX_STEEL_ON_ROAD
			-mappoint[x][y-1].int_6);
		mappoint[x][y-1].int_6=MAX_STEEL_ON_ROAD;
	}
        else if (main_types[mappointtype[x][y-1]].group
                ==GROUP_RAIL && (MAX_STEEL_ON_RAIL
                        -mappoint[x][y-1].int_6)
                        <= mappoint[x][y].int_2)
        {
                mappoint[x][y].int_2-=(MAX_STEEL_ON_RAIL
                        -mappoint[x][y-1].int_6);
                mappoint[x][y-1].int_6=MAX_STEEL_ON_RAIL;
        }
        else if (main_types[mappointtype[x][y-1]].group
                ==GROUP_TRACK && (MAX_STEEL_ON_TRACK
                        -mappoint[x][y-1].int_6)
                        <= mappoint[x][y].int_2)
        {
                mappoint[x][y].int_2-=(MAX_STEEL_ON_TRACK
                        -mappoint[x][y-1].int_6);
                mappoint[x][y-1].int_6=MAX_STEEL_ON_TRACK;
        }

        if (main_types[mappointtype[x-1][y]].group
                ==GROUP_ROAD && (MAX_STEEL_ON_ROAD
                        -mappoint[x-1][y].int_6)
                        <= mappoint[x][y].int_2)
        {
                mappoint[x][y].int_2-=(MAX_STEEL_ON_ROAD
                        -mappoint[x-1][y].int_6);
                mappoint[x-1][y].int_6=MAX_STEEL_ON_ROAD;
        }
        else if (main_types[mappointtype[x-1][y]].group
                ==GROUP_RAIL && (MAX_STEEL_ON_RAIL
                        -mappoint[x-1][y].int_6)
                        <= mappoint[x][y].int_2)
        {
                mappoint[x][y].int_2-=(MAX_STEEL_ON_RAIL
                        -mappoint[x-1][y].int_6);
            mappoint[x-1][y].int_6=MAX_STEEL_ON_RAIL;
        }
        else if (main_types[mappointtype[x-1][y]].group
                ==GROUP_TRACK && (MAX_STEEL_ON_TRACK
                        -mappoint[x-1][y].int_6)
                        <= mappoint[x][y].int_2)
        {
                mappoint[x][y].int_2-=(MAX_STEEL_ON_TRACK
                        -mappoint[x-1][y].int_6);
                mappoint[x-1][y].int_6=MAX_STEEL_ON_TRACK;
        }


// now choose a graphic every month
	if ((total_time%NUMOF_DAYS_IN_MONTH)==NUMOF_DAYS_IN_MONTH-1)
	{
//		mappoint[x][y].int_5=mappoint[x][y].int_1
//			/(MAX_ORE_AT_INDUSTRY_H/(ORE_MAKE_STEEL*8));
		mappoint[x][y].int_5=mappoint[x][y].int_1
			/(MAX_MADE_AT_INDUSTRY_H/ORE_MAKE_STEEL);
		mappoint[x][y].int_1=0;
		if (mappoint[x][y].int_5>80)
		{
			switch(mappointtype[x][y])
			{
				case(CST_INDUSTRY_H_H1):
				case(CST_INDUSTRY_H_H2):
				case(CST_INDUSTRY_H_H3):
				case(CST_INDUSTRY_H_H4):
				case(CST_INDUSTRY_H_H5):
				case(CST_INDUSTRY_H_H6):
				case(CST_INDUSTRY_H_H7):
				case(CST_INDUSTRY_H_H8): break;
				default:
				mappointtype[x][y]=CST_INDUSTRY_H_H1;
			}
		}
		else if (mappoint[x][y].int_5>30)
		{
			switch(mappointtype[x][y])
			{
				case(CST_INDUSTRY_H_M1):
				case(CST_INDUSTRY_H_M2):
				case(CST_INDUSTRY_H_M3):
				case(CST_INDUSTRY_H_M4):
				case(CST_INDUSTRY_H_M5):
				case(CST_INDUSTRY_H_M6):
				case(CST_INDUSTRY_H_M7):
				case(CST_INDUSTRY_H_M8): break;
				default:
				mappointtype[x][y]=CST_INDUSTRY_H_M1;
			}
		}
		else if (mappoint[x][y].int_5>0)
		{
			switch(mappointtype[x][y])
			{
				case(CST_INDUSTRY_H_L1):
                                case(CST_INDUSTRY_H_L2):
                                case(CST_INDUSTRY_H_L3):
                                case(CST_INDUSTRY_H_L4):
                                case(CST_INDUSTRY_H_L5):
                                case(CST_INDUSTRY_H_L6):
                                case(CST_INDUSTRY_H_L7):
                                case(CST_INDUSTRY_H_L8): break;
				default:
				mappointtype[x][y]=CST_INDUSTRY_H_L1;
			}
		}
		else
			mappointtype[x][y]=CST_INDUSTRY_H_C;
	}
// now animate
	if (real_time>=mappoint[x][y].int_6)
	{
		mappoint[x][y].int_6=real_time+INDUSTRY_H_ANIM_SPEED;
		switch (mappointtype[x][y])
		{
			case (CST_INDUSTRY_H_L1):
				mappointtype[x][y]=CST_INDUSTRY_H_L2; break;
                        case (CST_INDUSTRY_H_L2):
                                mappointtype[x][y]=CST_INDUSTRY_H_L3; break;
                        case (CST_INDUSTRY_H_L3):
                                mappointtype[x][y]=CST_INDUSTRY_H_L4; break;
                        case (CST_INDUSTRY_H_L4):
                                mappointtype[x][y]=CST_INDUSTRY_H_L5; break;
                        case (CST_INDUSTRY_H_L5):
                                mappointtype[x][y]=CST_INDUSTRY_H_L6; break;
                        case (CST_INDUSTRY_H_L6):
                                mappointtype[x][y]=CST_INDUSTRY_H_L7; break;
                        case (CST_INDUSTRY_H_L7):
                                mappointtype[x][y]=CST_INDUSTRY_H_L8; break;
                        case (CST_INDUSTRY_H_L8):
                                mappointtype[x][y]=CST_INDUSTRY_H_L1; break;

                        case (CST_INDUSTRY_H_M1):
                                mappointtype[x][y]=CST_INDUSTRY_H_M2; break;
                        case (CST_INDUSTRY_H_M2):
                                mappointtype[x][y]=CST_INDUSTRY_H_M3; break;
                        case (CST_INDUSTRY_H_M3):
                                mappointtype[x][y]=CST_INDUSTRY_H_M4; break;
                        case (CST_INDUSTRY_H_M4):
                                mappointtype[x][y]=CST_INDUSTRY_H_M5; break;
                        case (CST_INDUSTRY_H_M5):
                                mappointtype[x][y]=CST_INDUSTRY_H_M6; break;
                        case (CST_INDUSTRY_H_M6):
                                mappointtype[x][y]=CST_INDUSTRY_H_M7; break;
                        case (CST_INDUSTRY_H_M7):
                                mappointtype[x][y]=CST_INDUSTRY_H_M8; break;
                        case (CST_INDUSTRY_H_M8):
                                mappointtype[x][y]=CST_INDUSTRY_H_M1; break;

                        case (CST_INDUSTRY_H_H1):
                                mappointtype[x][y]=CST_INDUSTRY_H_H2; break;
                        case (CST_INDUSTRY_H_H2):
                                mappointtype[x][y]=CST_INDUSTRY_H_H3; break;
                        case (CST_INDUSTRY_H_H3):
                                mappointtype[x][y]=CST_INDUSTRY_H_H4; break;
                        case (CST_INDUSTRY_H_H4):
                                mappointtype[x][y]=CST_INDUSTRY_H_H5; break;
                        case (CST_INDUSTRY_H_H5):
                                mappointtype[x][y]=CST_INDUSTRY_H_H6; break;
                        case (CST_INDUSTRY_H_H6):
                                mappointtype[x][y]=CST_INDUSTRY_H_H7; break;
                        case (CST_INDUSTRY_H_H7):
                                mappointtype[x][y]=CST_INDUSTRY_H_H8; break;
                        case (CST_INDUSTRY_H_H8):
                                mappointtype[x][y]=CST_INDUSTRY_H_H1; break;

		}
	}
}


void do_power_substation(int x,int y)
{
#ifdef DEBUG_ENGINE
	printf("In do_power_substation\n");
#endif
	shuffle_substations();
// int_1 has the current power left
	if (mappoint[x][y].int_1>POWER_LINE_CAPACITY/2)
	{
		mappointtype[x][y]=CST_SUBSTATION_G;
		return;
	}
	else if (mappoint[x][y].int_1<=0)
		mappointtype[x][y]=CST_SUBSTATION_R;
	else if (mappoint[x][y].int_1<=POWER_LINE_CAPACITY/2)
		mappointtype[x][y]=CST_SUBSTATION_RG;
	if (main_types[mappointtype[x][y-1]].group==GROUP_POWER_LINE
		&& mappoint[x][y-1].int_1!=0)
	{
		mappoint[x][y].int_1+=POWER_LINE_CAPACITY;
		mappoint[x][y-1].int_1=0;
                if (mappointtype[x][y-1]<=CST_POWERL_LUDR_L)
                        mappointtype[x][y-1]+=11;
		power_used++;
	}
	else if (main_types[mappointtype[x-1][y]].group
		==GROUP_POWER_LINE
		&& mappoint[x-1][y].int_1!=0)
	{
		mappoint[x][y].int_1+=POWER_LINE_CAPACITY;
		mappoint[x-1][y].int_1=0;
                if (mappointtype[x-1][y]<=CST_POWERL_LUDR_L)
                        mappointtype[x-1][y]+=11;
		power_used++;
	}

}

int get_power(int x,int y,int power,int block_industry)
{
	int q;
#ifdef DEBUG_GET_POWER
	char s[100];
#endif
	if (numof_substations==0)
		return(0);
	for (q=0;q<numof_substations;q++)
	{
		if (abs(substationx[q]-x)<SUBSTATION_RANGE
			&& abs(substationy[q]-y)<SUBSTATION_RANGE
			&& mappoint[substationx[q]][substationy[q]].int_1
				> 0)  // OK, go even if lower than power
		{
			if (block_industry!=0 && main_types[mappointtype
				[substationx[q]][substationy[q]]
				].group==GROUP_WINDMILL)
				continue;
			mappoint[substationx[q]][substationy[q]].int_1-=power;
		if (mappoint[substationx[q]][substationy[q]].int_1<0)
			mappoint[substationx[q]][substationy[q]].int_1=0;
#ifdef DEBUG_GET_POWER
			Fgl_write(100,100,"Connected  ");
			sprintf(s,"%d %d %d ",x,y
			,mappoint[substationx[q]][substationy[q]].int_1);
			Fgl_write(100,108,s);
#endif
			return(1);
		}
	}
#ifdef DEBUG_GET_POWER
	Fgl_write(100,100,"UnConnected ");
	sprintf(s,"%d %d %d ",x,y
		,mappoint[substationx[q]][substationy[q]].int_1);
	Fgl_write(100,108,s);
#endif
	return(0);
}

int add_a_substation(int x,int y)   // add to substationx substationy to list
{
	if (numof_substations>=MAX_NUMOF_SUBSTATIONS)
		return(0);
	substationx[numof_substations]=x;
	substationy[numof_substations]=y;
	numof_substations++;
	return(1);
}

void remove_a_substation(int x,int y)
{
        int q;
        for (q=0;q<numof_substations;q++)
                if (substationx[q]==x && substationy[q]==y)
                        break;
        for (;q<numof_substations;q++)
        {
                substationx[q]=substationx[q+1];
                substationy[q]=substationy[q+1];
        }
        numof_substations--;
}

void do_organic_farm(int x,int y)
{
// int_1 is the tech level of the farm when built
// int_2 is a flag so we don't create a farm with nearly ripe crops.
// int_3 is the food sold count so far this year.
// int_4 is the food made last year.
// int_5 is the random crop rotation key.
// int_6 is the random month stagger, so they don't all flash at once
	int i;
	if (mappoint[x][y].int_5==0)  // this should be done when we create
	{                 // the area!
		mappoint[x][y].int_5=(rand()%4)+1;
		mappoint[x][y].int_6=rand()%300;
	}
	mappoint[x][y].flags&=(0xffffffff-FLAG_POWERED);
	if (get_jobs(x,y,1)==0)
		put_food(x,y,30);
	else if (get_jobs(x,y,FARM_JOBS_USED)!=0)
	{
		if (get_power(x,y,ORG_FARM_POWER_REC,0)!=0)
		{
			if (put_food(x,y,((ORGANIC_FARM_FOOD_OUTPUT
			+(mappoint[x][y].int_1*ORGANIC_FARM_FOOD_OUTPUT)
				/MAX_TECH_LEVEL)))==0)
				put_jobs(x,y,FARM_JOBS_USED);
			else
				mappoint[x][y].int_3++;
			mappoint[x][y].flags|=FLAG_POWERED;
		}
		else
		{
			if (put_food(x,y,(ORGANIC_FARM_FOOD_OUTPUT/4))==0)
				put_jobs(x,y,FARM_JOBS_USED);
			else
				mappoint[x][y].int_3++;
		}
	}
        else if (get_jobs(x,y,FARM_JOBS_USED/4)!=0)
        {
                if (get_power(x,y,ORG_FARM_POWER_REC,0)!=0)
                {
                        if (put_food(x,y,((ORGANIC_FARM_FOOD_OUTPUT
                        +(mappoint[x][y].int_1*(ORGANIC_FARM_FOOD_OUTPUT/4))
                                /MAX_TECH_LEVEL)))==0)
                                put_jobs(x,y,FARM_JOBS_USED/4);
			else
				mappoint[x][y].int_3++;
                        mappoint[x][y].flags|=FLAG_POWERED;
                }
                else
		{
                        if (put_food(x,y,(ORGANIC_FARM_FOOD_OUTPUT/(4*4)))==0)
                                put_jobs(x,y,FARM_JOBS_USED/4);
			else
				mappoint[x][y].int_3++;
		}
        }
        else
        {
                if (get_power(x,y,ORG_FARM_POWER_REC,0)!=0)
                {
                        if (put_food(x,y,((ORGANIC_FARM_FOOD_OUTPUT
                        +(mappoint[x][y].int_1*(ORGANIC_FARM_FOOD_OUTPUT/8))
                                /MAX_TECH_LEVEL)))!=0)
				mappoint[x][y].int_3++;
                        mappoint[x][y].flags|=FLAG_POWERED;
                }
                else
                        if (put_food(x,y,30
				+(ORGANIC_FARM_FOOD_OUTPUT/(4*8)))!=0)
				mappoint[x][y].int_3++;
        }
	if ((total_time&0x7f)==0)
		if ((mappoint[x][y].flags & FLAG_POWERED)!=0)
			get_waste(x,y,0x80*ORG_FARM_WASTE_GET);
	if ((total_time%1200)==0)
	{
		mappoint[x][y].int_4=mappoint[x][y].int_3;
		mappoint[x][y].int_3=0;
	}
	i=((total_time+(mappoint[x][y].int_5*1200)
				+mappoint[x][y].int_6)%4800);
	if (i%300==0)
	{
		i/=300;	
		if ( /* mappoint[x][y].int_2!=0 &&  */ mappoint[x][y].int_4
			>MIN_FOOD_SOLD_FOR_ANIM)
		{
			if (i%4==0)
			{
				mappoint[x][y].int_6=rand()%100;
			}		
			switch (i)
			{
                        case (0): mappointtype[x][y]=CST_FARM_O3; break;
                        case (1): mappointtype[x][y]=CST_FARM_O3; break;
                        case (2): mappointtype[x][y]=CST_FARM_O3; break;
                        case (3): mappointtype[x][y]=CST_FARM_O3; break;
                        case (4): mappointtype[x][y]=CST_FARM_O7; break;
                        case (5): mappointtype[x][y]=CST_FARM_O7; break;
                        case (6): mappointtype[x][y]=CST_FARM_O7; break;
                        case (7): mappointtype[x][y]=CST_FARM_O7; break;
                        case (8): mappointtype[x][y]=CST_FARM_O11; break;
                        case (9): mappointtype[x][y]=CST_FARM_O11; break;
                        case (10): mappointtype[x][y]=CST_FARM_O11; break;
                        case (11): mappointtype[x][y]=CST_FARM_O11; break;
                        case (12): mappointtype[x][y]=CST_FARM_O15; break;
                        case (13): mappointtype[x][y]=CST_FARM_O15; break;
                        case (14): mappointtype[x][y]=CST_FARM_O15; break;
                        case (15): mappointtype[x][y]=CST_FARM_O15; break;

			}
		}
		else 
		{
			mappointtype[x][y]=CST_FARM_O0;
		}
	}
}

void shuffle_mappoint_array(void)
{
	int i,x,a;
	for (i=0;i<SHUFFLE_MAPPOINT_COUNT;i++)
	{
		x=rand()%WORLD_SIDE_LEN;
		a=mappoint_array_x[i];
		mappoint_array_x[i]=mappoint_array_x[x];
		mappoint_array_x[x]=a;
                x=rand()%WORLD_SIDE_LEN;
                a=mappoint_array_y[i];
                mappoint_array_y[i]=mappoint_array_y[x];
                mappoint_array_y[x]=a;
	}
}

void shuffle_substations(void)
{
        int q,x,r,m;
	m=(numof_substations/2)+1;
        for (x=0;x<m;x++)
        {
                r=rand()%numof_substations;
                if (r==x)
                        continue;
                q=substationx[x];
                substationx[x]=substationx[r];
                substationx[r]=q;
                q=substationy[x];
                substationy[x]=substationy[r];
                substationy[r]=q;
        }
}


void do_coalmine(int x,int y)
{
// int_1 is the coal at the surface
// int_2 is the coal reserve under the ground. More than one mine can
// int_3 is the jobs collected.

// claim the coal under ground!
	int xx,yy,xs,ys,xe,ye,cr;
	if (mappoint[x][y].int_1<(DIG_MORE_COAL_TRIGGER-1000))
	{
		if (mappoint[x][y].int_2<0)
			return;  // run out of reserves
		xs=x-COAL_RESERVE_SEARCH_LEN;
		if (xs<0)
			xs=0;
		ys=y-COAL_RESERVE_SEARCH_LEN;
		if (ys<0)
			ys=0;
		xe=x+COAL_RESERVE_SEARCH_LEN;
		if (xe>WORLD_SIDE_LEN)
			xe=WORLD_SIDE_LEN;
		ye=y+COAL_RESERVE_SEARCH_LEN;
		if (ye>WORLD_SIDE_LEN)
			ye=WORLD_SIDE_LEN;
		cr=0;
		for (yy=ys;yy<ye;yy++)
		for (xx=xs;xx<xe;xx++)
			cr+=mappoint[xx][yy].coal_reserve;
		mappoint[x][y].int_2=cr;
		if (cr>0)
		{
			if  (get_jobs(x,y,JOBS_DIG_COAL
					-mappoint[x][y].int_3)!=0)
			{
				mappoint[x][y].int_3=0;
				for (yy=ys;yy<ye;yy++)
				for (xx=xs;xx<xe;xx++)
					if (mappoint[xx][yy].coal_reserve>0)
					{
					mappoint[xx][yy].coal_reserve--;
					mappoint[x][y].int_1+=1000;
					coal_made+=1000;
					coal_tax+=1000;
					sust_dig_ore_coal_tip_flag=0;
					mappointpol[x][y]
						+=COALMINE_POLLUTION;
					yy=ye;
					xx=xe; // break out
					}
			}
			else if (get_jobs(x,y,JOBS_DIG_COAL/10)!=0)
				mappoint[x][y].int_3+=JOBS_DIG_COAL/10;
			else if (get_jobs(x,y,JOBS_DIG_COAL/50)!=0)
				mappoint[x][y].int_3+=JOBS_DIG_COAL/50;
		}
		else
		{
			mappoint[x][y].int_1=0;
			mappoint[x][y].int_2=-1;
		}
	}
// put it on the railway
	if (main_types[mappointtype[x-1][y]].group==GROUP_RAIL
		&& mappoint[x-1][y].int_3<MAX_COAL_ON_RAIL
		&& mappoint[x][y].int_1>=(MAX_COAL_ON_RAIL
			-mappoint[x-1][y].int_3))
	{
		if (get_jobs(x,y,JOBS_LOAD_COAL)!=0)
		{
			mappoint[x][y].int_1
				-=(MAX_COAL_ON_RAIL-mappoint[x-1][y].int_3);
			mappoint[x-1][y].int_3=MAX_COAL_ON_RAIL;
		}
	}
        if (main_types[mappointtype[x][y-1]].group==GROUP_RAIL
                && mappoint[x][y-1].int_3<MAX_COAL_ON_RAIL
                && mappoint[x][y].int_1>=(MAX_COAL_ON_RAIL
                        -mappoint[x][y-1].int_3))
        {
		if (get_jobs(x,y,JOBS_LOAD_COAL)!=0)
		{
                	mappoint[x][y].int_1
                        	-=(MAX_COAL_ON_RAIL-mappoint[x][y-1].int_3);
                	mappoint[x][y-1].int_3=MAX_COAL_ON_RAIL;
		}
        }
// put it on the road
        if (main_types[mappointtype[x-1][y]].group==GROUP_ROAD
                && mappoint[x-1][y].int_3<MAX_COAL_ON_ROAD
                && mappoint[x][y].int_1>=(MAX_COAL_ON_ROAD
                        -mappoint[x-1][y].int_3))
        {
                if (get_jobs(x,y,JOBS_LOAD_COAL)!=0)
                {
                        mappoint[x][y].int_1
                                -=(MAX_COAL_ON_ROAD-mappoint[x-1][y].int_3);
                        mappoint[x-1][y].int_3=MAX_COAL_ON_ROAD;
                }
        }
        if (main_types[mappointtype[x][y-1]].group==GROUP_ROAD
                && mappoint[x][y-1].int_3<MAX_COAL_ON_ROAD
                && mappoint[x][y].int_1>=(MAX_COAL_ON_ROAD
                        -mappoint[x][y-1].int_3))
        {
                if (get_jobs(x,y,JOBS_LOAD_COAL)!=0)
                {
                        mappoint[x][y].int_1
                                -=(MAX_COAL_ON_ROAD-mappoint[x][y-1].int_3);
                        mappoint[x][y-1].int_3=MAX_COAL_ON_ROAD;
                }
        }
// put it on the tracks
        if (main_types[mappointtype[x-1][y]].group==GROUP_TRACK
                && mappoint[x-1][y].int_3<MAX_COAL_ON_TRACK
                && mappoint[x][y].int_1>=(MAX_COAL_ON_TRACK
                        -mappoint[x-1][y].int_3))
        {
                if (get_jobs(x,y,JOBS_LOAD_COAL)!=0)
                {
                        mappoint[x][y].int_1
                                -=(MAX_COAL_ON_TRACK-mappoint[x-1][y].int_3);
                        mappoint[x-1][y].int_3=MAX_COAL_ON_TRACK;
                }
        }
        if (main_types[mappointtype[x][y-1]].group==GROUP_TRACK
                && mappoint[x][y-1].int_3<MAX_COAL_ON_TRACK
                && mappoint[x][y].int_1>=(MAX_COAL_ON_TRACK
                        -mappoint[x][y-1].int_3))
        {
                if (get_jobs(x,y,JOBS_LOAD_COAL)!=0)
                {
                        mappoint[x][y].int_1
                                -=(MAX_COAL_ON_TRACK-mappoint[x][y-1].int_3);
                        mappoint[x][y-1].int_3=MAX_COAL_ON_TRACK;
                }
        }
		
// choose a graphic
	if (mappoint[x][y].int_1>(MAX_COAL_AT_MINE-(MAX_COAL_AT_MINE/5)))
		mappointtype[x][y]=CST_COALMINE_FULL;
	else if (mappoint[x][y].int_1>(MAX_COAL_AT_MINE/2))
		mappointtype[x][y]=CST_COALMINE_MED;
	else if (mappoint[x][y].int_1>0)
		mappointtype[x][y]=CST_COALMINE_LOW;
	else
		mappointtype[x][y]=CST_COALMINE_EMPTY;
}

void do_oremine(int x,int y)
{
// int_1 is the ore at in stock
// int_2 is the ore reserve under the ground or at the surface really.
	int xx,yy,xs,ys,xe,ye,cr;
	if (mappoint[x][y].int_1<DIG_MORE_ORE_TRIGGER-5000)
	{
		xs=x;
		ys=y;
		xe=x+4;
		ye=y+4;
		cr=0;
		for (yy=ys;yy<ye;yy++)
		for (xx=xs;xx<xe;xx++)
			cr+=mappoint[xx][yy].ore_reserve;
		mappoint[x][y].int_2=cr;
		if (cr>0)
		if (get_jobs(x,y,JOBS_DIG_ORE)!=0)
			for (yy=ys;yy<ye;yy++)
			for (xx=xs;xx<xe;xx++)
				if (mappoint[xx][yy].ore_reserve>0)
				{
					mappoint[xx][yy].ore_reserve--;
					mappoint[x][y].int_1+=5000;
					ore_made+=5000;
					sust_dig_ore_coal_tip_flag=0;
				// maybe want an ore tax?
					yy=ye;
					xx=xe; // break out
				}
	}

	if ((mappoint[x-1][y].flags & FLAG_IS_TRANSPORT)!=0)
	{
        if (main_types[mappointtype[x-1][y]].group==GROUP_RAIL
                && mappoint[x-1][y].int_5<MAX_ORE_ON_RAIL
                && mappoint[x][y].int_1>=(MAX_ORE_ON_RAIL
                        -mappoint[x-1][y].int_5))
        {
                if (get_jobs(x,y,JOBS_LOAD_ORE)!=0)
                {
                        mappoint[x][y].int_1
                                -=(MAX_ORE_ON_RAIL-mappoint[x-1][y].int_5);
                        mappoint[x-1][y].int_5=MAX_ORE_ON_RAIL;
                }
        }
        else if (main_types[mappointtype[x-1][y]].group==GROUP_ROAD
                && mappoint[x-1][y].int_5<MAX_ORE_ON_ROAD
                && mappoint[x][y].int_1>=(MAX_ORE_ON_ROAD
                        -mappoint[x-1][y].int_5))
        {
                if (get_jobs(x,y,JOBS_LOAD_ORE)!=0)
                {
                        mappoint[x][y].int_1
                                -=(MAX_ORE_ON_ROAD-mappoint[x-1][y].int_5);
                        mappoint[x-1][y].int_5=MAX_ORE_ON_ROAD;
                }
        }
        else if (main_types[mappointtype[x-1][y]].group==GROUP_TRACK
                && mappoint[x-1][y].int_5<MAX_ORE_ON_TRACK
                && mappoint[x][y].int_1>=(MAX_ORE_ON_TRACK
                        -mappoint[x-1][y].int_5))
        {
                if (get_jobs(x,y,JOBS_LOAD_ORE)!=0)
                {
                        mappoint[x][y].int_1
                                -=(MAX_ORE_ON_TRACK-mappoint[x-1][y].int_5);
                        mappoint[x-1][y].int_5=MAX_ORE_ON_TRACK;
                }
        }
	}

	if ((mappoint[x][y-1].flags & FLAG_IS_TRANSPORT)!=0)
	{
        if (main_types[mappointtype[x][y-1]].group==GROUP_RAIL
                && mappoint[x][y-1].int_5<MAX_ORE_ON_RAIL
                && mappoint[x][y].int_1>=(MAX_ORE_ON_RAIL
                        -mappoint[x][y-1].int_5))
        {
                if (get_jobs(x,y,JOBS_LOAD_ORE)!=0)
                {
                        mappoint[x][y].int_1
                                -=(MAX_ORE_ON_RAIL-mappoint[x][y-1].int_5);
                        mappoint[x][y-1].int_5=MAX_ORE_ON_RAIL;
                }
        }
        else if (main_types[mappointtype[x][y-1]].group==GROUP_ROAD
                && mappoint[x][y-1].int_5<MAX_ORE_ON_ROAD
                && mappoint[x][y].int_1>=(MAX_ORE_ON_ROAD
                        -mappoint[x][y-1].int_5))
        {
                if (get_jobs(x,y,JOBS_LOAD_ORE)!=0)
                {
                        mappoint[x][y].int_1
                                -=(MAX_ORE_ON_ROAD-mappoint[x][y-1].int_5);
                        mappoint[x][y-1].int_5=MAX_ORE_ON_ROAD;
                }
        }
        else if (main_types[mappointtype[x][y-1]].group==GROUP_TRACK
                && mappoint[x][y-1].int_5<MAX_ORE_ON_TRACK
                && mappoint[x][y].int_1>=(MAX_ORE_ON_TRACK
                        -mappoint[x][y-1].int_5))
        {
                if (get_jobs(x,y,JOBS_LOAD_ORE)!=0)
                {
                        mappoint[x][y].int_1
                                -=(MAX_ORE_ON_TRACK-mappoint[x][y-1].int_5);
                        mappoint[x][y-1].int_5=MAX_ORE_ON_TRACK;
                }
        }
	}


// choose a graphic
	if ((total_time&0x7f)==0)
	{
		xx=7*(mappoint[x][y].int_2+(3*ORE_RESERVE/2))
				/(16*ORE_RESERVE);
		switch (xx)
		{
			case(0): mappointtype[x][y]=CST_OREMINE_8; break;
			case(1): mappointtype[x][y]=CST_OREMINE_7; break;
			case(2): mappointtype[x][y]=CST_OREMINE_6; break;
			case(3): mappointtype[x][y]=CST_OREMINE_5; break;
			case(4): mappointtype[x][y]=CST_OREMINE_4; break;
			case(5): mappointtype[x][y]=CST_OREMINE_3; break;
			case(6): mappointtype[x][y]=CST_OREMINE_2; break;
			case(7): mappointtype[x][y]=CST_OREMINE_1; break;
		}
		if (mappoint[x][y].int_2<=0)
		{
			do_bulldoze_area(CST_WATER,x,y);
			connect_rivers();
			refresh_main_screen();
		}
	}	
}



void do_commune(int x,int y)
{
// int_1 is the animation trigger time
// int_2 is the steelflag
// int_3 is the coal sold in the last 100 days 200 units is 100%
// steel adds more.
// int_4 is the months without selling much coal,steel,ore
// int_5 is the coal, ore, steel waste flags for last month
// int_6 is the coal, ore, steel waste flags so far this month
	int trackflag=0;
// stick coal and ore on tracks, in SMALL doses.
	if (put_coal(x,y,2)!=0)
	{
		trackflag=1;
		mappoint[x][y].int_3++;
		mappoint[x][y].int_6 |= 1;
	}
	if (put_ore(x,y,6)!=0)
	{
		trackflag=1;
		mappoint[x][y].int_3++;
		mappoint[x][y].int_6 |= 2;
	}
// recycle a bit of waste
	if (get_waste(x,y,20)!=0)
	{
		trackflag=1;
		mappoint[x][y].int_3++;
		mappoint[x][y].int_6 |= 8;
	}
	if (total_time%10==0)
	{
		if (put_steel(x,y,2)!=0)
		{
			mappoint[x][y].int_2=1;
			mappoint[x][y].int_3++;
			mappoint[x][y].int_6 |= 4;
		}
		else
			mappoint[x][y].int_2=0;
	}
	if (total_time%100==48)
	{
		mappoint[x][y].int_5=mappoint[x][y].int_6;
		mappoint[x][y].int_6=0;
		if (mappoint[x][y].int_2==0 || trackflag==0)
		{
			if (mappointtype[x][y]>CST_COMMUNE_7)
				mappointtype[x][y]-=7;
		}
		else if (mappointtype[x][y]<=CST_COMMUNE_7)
			mappointtype[x][y]+=7;
		if (mappoint[x][y].int_3>0)  //  >0%
		{
			mappoint[x][y].int_3=0;
			if (--mappoint[x][y].int_4<0)
				mappoint[x][y].int_4=0;
		}
		else
		{
			mappoint[x][y].int_3=0;
			mappoint[x][y].int_4++;
			if (mappoint[x][y].int_4>120)  // 10 years
			{
				do_bulldoze_area(CST_PARKLAND_PLANE,x,y);
				return;
			}
		}
	}
// animate
	if (real_time>=mappoint[x][y].int_1)
	{
	// printf("commune=%d\n",mappointtype[x][y]);
	// fflush(stdout); 
		mappoint[x][y].int_1=real_time+COMMUNE_ANIM_SPEED;
                switch(mappointtype[x][y])
                {
                        case(CST_COMMUNE_1):
                                mappointtype[x][y]=CST_COMMUNE_2;
                                break;
                        case(CST_COMMUNE_2):
                                mappointtype[x][y]=CST_COMMUNE_3;
                                break;
                        case(CST_COMMUNE_3):
                                mappointtype[x][y]=CST_COMMUNE_4;
                                break;
                        case(CST_COMMUNE_4):
                                mappointtype[x][y]=CST_COMMUNE_5;
                                break;
                        case(CST_COMMUNE_5):
                                mappointtype[x][y]=CST_COMMUNE_6;
                                break;
                        case(CST_COMMUNE_6):
                                mappointtype[x][y]=CST_COMMUNE_1;
                                break;
                        case(CST_COMMUNE_8):
                                mappointtype[x][y]=CST_COMMUNE_9;
                                break;
                        case(CST_COMMUNE_9):
                                mappointtype[x][y]=CST_COMMUNE_10;
                                break;
                        case(CST_COMMUNE_10):
                                mappointtype[x][y]=CST_COMMUNE_11;
                                break;
                        case(CST_COMMUNE_11):
                                mappointtype[x][y]=CST_COMMUNE_12;
                                break;
                        case(CST_COMMUNE_12):
                                mappointtype[x][y]=CST_COMMUNE_13;
                                break;
                        case(CST_COMMUNE_13):
                                mappointtype[x][y]=CST_COMMUNE_8;
                                break;

		}
	}

}

void do_port(int x,int y)
{
// int_1 is the money made so far this month
// int_2 is the money made last month
// int_3 holds the 'pence/pennies/bits' to add next time round.
// int_4 is the import costs so far this month
// int_5 is the import costs for last month
// Use int_3 to int_7 of (x+1,y) to hold the individual buy values
//                       (x,y+1) is last month's
// Use int_3 to int_7 of (x+2,y) to hold the individual sell values
//                       (x,y+2) is last month's
        int i,et=0,ic=0,flags,*b1,*b2,*s1,*s2;
// left connection first
	flags=mappoint[x][y].flags;
        if (x>0 && (mappoint[x-1][y].flags
                & FLAG_IS_TRANSPORT)!=0)
        {
                if ((flags & FLAG_MB_FOOD)!=0)
		{
			i=buy_food(x-1,y);
                        ic+=i;
			mappoint[x+1][y].int_3+=i;
		}
                if ((flags & FLAG_MS_FOOD)!=0)
		{
			i=sell_food(x-1,y);
                        et+=i;
			mappoint[x+2][y].int_3+=i;
		}
		if ((flags & FLAG_MB_COAL)!=0)
		{
			i=buy_coal(x-1,y);
			ic+=i;
			mappoint[x+1][y].int_4+=i;
		}
		if ((flags & FLAG_MS_COAL)!=0)
		{
			i=sell_coal(x-1,y);
			et+=i;
			mappoint[x+2][y].int_4+=i;
		}
                if ((flags & FLAG_MB_ORE)!=0)
		{
			i=buy_ore(x-1,y);
                        ic+=i;
			mappoint[x+1][y].int_5+=i;
		}
                if ((flags & FLAG_MS_ORE)!=0)
		{
			i=sell_ore(x-1,y);
                        et+=i;
			mappoint[x+2][y].int_5+=i;
		}
                if ((flags & FLAG_MB_GOODS)!=0)
		{
			i=buy_goods(x-1,y);
                        ic+=i;
			mappoint[x+1][y].int_6+=i;
		}
                if ((flags & FLAG_MS_GOODS)!=0)
		{
			i=sell_goods(x-1,y);
                        et+=i;
			mappoint[x+2][y].int_6+=i;
		}
                if ((flags & FLAG_MB_STEEL)!=0)
		{
			i=buy_steel(x-1,y);
                        ic+=i;
			mappoint[x+1][y].int_7+=i;
		}
                if ((flags & FLAG_MS_STEEL)!=0)
		{
			i=sell_steel(x-1,y);
                        et+=i;
			mappoint[x+2][y].int_7+=i;
		}
	}
// upper gate next
	if (y>0 && (mappoint[x][y-1].flags
		& FLAG_IS_TRANSPORT)!=0)
	{
                if ((flags & FLAG_MB_FOOD)!=0)
		{
			i=buy_food(x,y-1);
                        ic+=i;
			mappoint[x+1][y].int_3+=i;
		}
                if ((flags & FLAG_MS_FOOD)!=0)
		{
			i=sell_food(x,y-1);
                        et+=i;
			mappoint[x+2][y].int_3+=i;
		}
                if ((flags & FLAG_MB_COAL)!=0)
		{
			i=buy_coal(x,y-1);
                        ic+=i;
			mappoint[x+1][y].int_4+=i;
		}
                if ((flags & FLAG_MS_COAL)!=0)
		{
			i=sell_coal(x,y-1);
                        et+=i;
			mappoint[x+2][y].int_4+=i;
		}
                if ((flags & FLAG_MB_ORE)!=0)
		{
			i=buy_ore(x,y-1);
                        ic+=i;
			mappoint[x+1][y].int_5+=i;
		}
                if ((flags & FLAG_MS_ORE)!=0)
		{
			i=sell_ore(x,y-1);
                        et+=i;
			mappoint[x+2][y].int_5+=i;
		}
                if ((flags & FLAG_MB_GOODS)!=0)
		{
			i=buy_goods(x,y-1);
                        ic+=i;
			mappoint[x+1][y].int_6+=i;
		}
                if ((flags & FLAG_MS_GOODS)!=0)
		{
			i=sell_goods(x,y-1);
                        et+=i;
			mappoint[x+2][y].int_6+=i;
		}
                if ((flags & FLAG_MB_STEEL)!=0)
		{
			i=buy_steel(x,y-1);
                        ic+=i;
			mappoint[x+1][y].int_7+=i;
		}
                if ((flags & FLAG_MS_STEEL)!=0)
		{
			i=sell_steel(x,y-1);
                        et+=i;
			mappoint[x+2][y].int_7+=i;
		}
        }
	mappoint[x][y].int_1+=et;
	mappoint[x][y].int_4+=ic;
        if (total_time%100==0)
        {
                mappoint[x][y].int_2=mappoint[x][y].int_1;
                mappoint[x][y].int_1=0;
		mappoint[x][y].int_5=mappoint[x][y].int_4;
		mappoint[x][y].int_4=0;
		b1=&(mappoint[x+1][y].int_3);
		s1=&(mappoint[x+2][y].int_3);
		b2=&(mappoint[x][y+1].int_3);
		s2=&(mappoint[x][y+2].int_3);
		for (i=0;i<5;i++)
		{
			*(b2++)=*b1;
			*(s2++)=*s1;
			*(b1++)=0;
			*(s1++)=0;
		}		
        }
	if (et>0)
	{
		sust_port_flag=0;
		tech_level++;
	}
	if (ic>0)
	{
		sust_port_flag=0;
		tech_level++;
	}
        et+=mappoint[x][y].int_3;  // int_3 holds the 'pence'
        export_tax+=et/100;
        mappoint[x][y].int_3=et%100;
	import_cost+=ic;
}

int buy_food(int xt,int yt)
{
        int i=0;
        if (main_types[mappointtype[xt][yt]].group==GROUP_TRACK)
        {
                if (mappoint[xt][yt].int_1<MAX_FOOD_ON_TRACK)
                        i=MAX_FOOD_ON_TRACK-mappoint[xt][yt].int_1;
        }
        else if (main_types[mappointtype[xt][yt]].group==GROUP_ROAD)
        {
                if (mappoint[xt][yt].int_1<MAX_FOOD_ON_ROAD)
                        i=MAX_FOOD_ON_ROAD-mappoint[xt][yt].int_1;
        }
        else if (main_types[mappointtype[xt][yt]].group==GROUP_RAIL)
        {
                if (mappoint[xt][yt].int_1<MAX_FOOD_ON_RAIL)
                        i=MAX_FOOD_ON_RAIL-mappoint[xt][yt].int_1;
        }
        i=(i*PORT_IMPORT_RATE)/1000;
        mappoint[xt][yt].int_1+=i;
        return(i*PORT_FOOD_RATE);
}

int buy_coal(int xt,int yt)
{
        int i=0;
        if (main_types[mappointtype[xt][yt]].group==GROUP_TRACK)
        {
                if (mappoint[xt][yt].int_3<MAX_COAL_ON_TRACK)
                        i=MAX_COAL_ON_TRACK-mappoint[xt][yt].int_3;
        }
        else if (main_types[mappointtype[xt][yt]].group==GROUP_ROAD)
        {
                if (mappoint[xt][yt].int_3<MAX_COAL_ON_ROAD)
                        i=MAX_COAL_ON_ROAD-mappoint[xt][yt].int_3;
        }
        else if (main_types[mappointtype[xt][yt]].group==GROUP_RAIL)
        {
                if (mappoint[xt][yt].int_3<MAX_COAL_ON_RAIL)
                        i=MAX_COAL_ON_RAIL-mappoint[xt][yt].int_3;
        }
        i=(i*PORT_IMPORT_RATE)/1000;
        mappoint[xt][yt].int_3+=i;
        return(i*PORT_COAL_RATE);
}

int buy_ore(int xt,int yt)
{
        int i=0;
        if (main_types[mappointtype[xt][yt]].group==GROUP_TRACK)
        {
                if (mappoint[xt][yt].int_5<MAX_ORE_ON_TRACK)
                        i=MAX_ORE_ON_TRACK-mappoint[xt][yt].int_5;
        }
        else if (main_types[mappointtype[xt][yt]].group==GROUP_ROAD)
        {
                if (mappoint[xt][yt].int_5<MAX_ORE_ON_ROAD)
                        i=MAX_ORE_ON_ROAD-mappoint[xt][yt].int_5;
        }
        else if (main_types[mappointtype[xt][yt]].group==GROUP_RAIL)
        {
                if (mappoint[xt][yt].int_5<MAX_ORE_ON_RAIL)
                        i=MAX_ORE_ON_RAIL-mappoint[xt][yt].int_5;
        }
        i=(i*PORT_IMPORT_RATE)/1000;
        mappoint[xt][yt].int_5+=i;
        return(i*PORT_ORE_RATE);
}

int buy_goods(int xt,int yt)
{
        int i=0;
        if (main_types[mappointtype[xt][yt]].group==GROUP_TRACK)
        {
                if (mappoint[xt][yt].int_4<MAX_GOODS_ON_TRACK)
                        i=MAX_GOODS_ON_TRACK-mappoint[xt][yt].int_4;
        }
        else if (main_types[mappointtype[xt][yt]].group==GROUP_ROAD)
        {
                if (mappoint[xt][yt].int_4<MAX_GOODS_ON_ROAD)
                        i=MAX_GOODS_ON_ROAD-mappoint[xt][yt].int_4;
        }
        else if (main_types[mappointtype[xt][yt]].group==GROUP_RAIL)
        {
                if (mappoint[xt][yt].int_4<MAX_GOODS_ON_RAIL)
                        i=MAX_GOODS_ON_RAIL-mappoint[xt][yt].int_4;
        }
        i=(i*PORT_IMPORT_RATE)/1000;
        mappoint[xt][yt].int_4+=i;
        return(i*PORT_GOODS_RATE);
}


int buy_steel(int xt,int yt)
{
	int i=0;
	if (main_types[mappointtype[xt][yt]].group==GROUP_TRACK)
	{
		if (mappoint[xt][yt].int_6<MAX_STEEL_ON_TRACK)
			i=MAX_STEEL_ON_TRACK-mappoint[xt][yt].int_6;
	}
        else if (main_types[mappointtype[xt][yt]].group==GROUP_ROAD)
	{
                if (mappoint[xt][yt].int_6<MAX_STEEL_ON_ROAD)
                        i=MAX_STEEL_ON_ROAD-mappoint[xt][yt].int_6;
	}
        else if (main_types[mappointtype[xt][yt]].group==GROUP_RAIL)
	{
                if (mappoint[xt][yt].int_6<MAX_STEEL_ON_RAIL)
                        i=MAX_STEEL_ON_RAIL-mappoint[xt][yt].int_6;
	}
	i=(i*PORT_IMPORT_RATE)/1000;
	mappoint[xt][yt].int_6+=i;
	return(i*PORT_STEEL_RATE);
}

int sell_food(int xt,int yt)
{
        int i=0;
        i=(mappoint[xt][yt].int_1*PORT_EXPORT_RATE)/1000;
        mappoint[xt][yt].int_1-=i;
        return(i*PORT_FOOD_RATE);
}

int sell_coal(int xt,int yt)
{
        int i=0;
        i=(mappoint[xt][yt].int_3*PORT_EXPORT_RATE)/1000;
        mappoint[xt][yt].int_3-=i;
        return(i*PORT_COAL_RATE);
}

int sell_ore(int xt,int yt)
{
        int i=0;
        i=(mappoint[xt][yt].int_5*PORT_EXPORT_RATE)/1000;
        mappoint[xt][yt].int_5-=i;
        return(i*PORT_ORE_RATE);
}

int sell_goods(int xt,int yt)
{
        int i=0;
        i=(mappoint[xt][yt].int_4*PORT_EXPORT_RATE)/1000;
        mappoint[xt][yt].int_4-=i;
        return(i*PORT_GOODS_RATE);
}

int sell_steel(int xt,int yt)
{
        int i=0;
        i=(mappoint[xt][yt].int_6*PORT_EXPORT_RATE)/1000;
        mappoint[xt][yt].int_6-=i;
        return(i*PORT_STEEL_RATE);
}

#ifdef THMO_POLLUTION

#if     1
  #define  POL_DIV      64      /* this shouldn't hurt much -- (ThMO) */
#else
  #define  POL_DIV      50
#endif

// static INLINE void      /* worth inlining -- (ThMO) */
// No it's not - (IJP)
void do_pollution()
{                       /* spread it out or perhaps kill some. */
  int  x, *pol, p;

  pol= &mappointpol[ 0][ 0];
  do           /* kill some pollution from horizontal edge -- (ThMO) */
    if ( *pol > 0)
      *pol /= POL_DIV;

  while ( ++pol < &mappointpol[ 1][ 0]);
  x= 1;
  do
    {
      if ( *pol++ > 0)  /* kill some pollution from vertical edge -- (ThMO) */
        *( pol- 1) /= POL_DIV;
      do
        if ( *pol > 10)
          {
            p= *pol/ 16;
            *pol -= p;
            switch ( rand() % 11)
              {         /* prevailing wind is *from* SW ie right down */
                case 0:
                case 1: /* up */
                case 2:
                  *( pol- 1) += p;
                  break;
                case 3:
                case 4: /* right */
                case 5:
                  *( pol+ WORLD_SIDE_LEN) += p;
                  break;
                case 6: /* down */
                case 7:
                  *( pol+ 1) += p;
                  break;
                case 8: /* left */
                case 9:
                  *( pol- WORLD_SIDE_LEN) += p;
                  break;
                case 10:
                  *pol += p- 2;
                  break;
              }
          }
      while ( ++pol < &mappointpol[ x][ WORLD_SIDE_LEN- 1]);
      if ( *pol > 0)   /* kill some pollution from vertical edge -- (ThMO) */
        *pol /= POL_DIV;
      ++x;
    }
  while ( ++pol < &mappointpol[ WORLD_SIDE_LEN- 1][ 0]);
  do               /* kill some pollution from horizontal edge --(ThMO) */
    if ( *pol > 0)
      *pol /= POL_DIV;
  while ( ++pol < &mappointpol[ WORLD_SIDE_LEN][ 0]);
}

#else

void do_pollution(void)
{  //                  spread it out or perhaps kill some.
	int x,y,i,p;
//	for (y=1;y<WORLD_SIDE_LEN-1;y++)
	for (x=1;x<WORLD_SIDE_LEN-1;x++)
	for (y=1;y<WORLD_SIDE_LEN-1;y++)
	if (mappointpol[x][y]>10)
	{
		i=rand()%11; // prevailing wind is *from* SW ie right down
		p=mappointpol[x][y]/16;
		mappointpol[x][y]-=p;
		switch (i)
		{
			case(0):
			case(1):  // up
			case(2): mappointpol[x][y-1]+=p; break;
			case(3):
			case(4):  // right
			case(5): mappointpol[x+1][y]+=p; break;
			case(6):  // down
			case(7): mappointpol[x][y+1]+=p; break;
			case(8):  // left
			case(9): mappointpol[x-1][y]+=p; break;
			case(10): mappointpol[x][y]+=p-2; break;
		}
	}
// now kill some pollution from the edges.
	for (x=0;x<WORLD_SIDE_LEN;x++)
	{
		// top
		if (mappointpol[x][0]>0)
			mappointpol[x][0]/=50;
		// left side
		if (mappointpol[0][x]>0)
			mappointpol[0][x]/=50;
		// right side
		if (mappointpol[WORLD_SIDE_LEN-1][x]>0)
			mappointpol[WORLD_SIDE_LEN-1][x]/=50;
		// bottom
		if (mappointpol[x][WORLD_SIDE_LEN-1]>0)
			mappointpol[x][WORLD_SIDE_LEN-1]/=50;
	}
}

#endif

void do_parkland(int x,int y)
{
	if (mappointpol[x][y]>10 && (total_time&1)==0)
		mappointpol[x][y]-=1;
}

void do_university(int x,int y)
{
// You need UNIVERSITY_JOBS and UNIVERSITY_GOODS to add 1 to tech_level
// int_1 is the stored jobs
// int_2 is the stored goods
// int_3 is the total tech points made by this uni.
// int_4 is the count so far in this 100 days
// int_5 is the count in the last 100 days to give a percent value
// get some jobs
	if (mappoint[x][y].int_1<UNIVERSITY_JOBS_STORE)
	{
		if (get_jobs(x,y,UNIVERSITY_JOBS+10)!=0)
			mappoint[x][y].int_1+=UNIVERSITY_JOBS+10;
	}
// get goods
	if (mappoint[x][y].int_2<UNIVERSITY_GOODS_STORE)
	{
		if (get_goods(x,y,UNIVERSITY_GOODS+10)!=0)
			mappoint[x][y].int_2+=UNIVERSITY_GOODS+10;
		else if (get_goods(x,y,UNIVERSITY_GOODS/5)!=0)
			mappoint[x][y].int_2+=UNIVERSITY_GOODS/5;
		else if (get_goods(x,y,UNIVERSITY_GOODS/20)!=0)
			mappoint[x][y].int_2+=UNIVERSITY_GOODS/20;
	}
// now do some research
	if (mappoint[x][y].int_1>=UNIVERSITY_JOBS
		&& mappoint[x][y].int_2>=UNIVERSITY_GOODS)
	{
		mappoint[x][y].int_1-=UNIVERSITY_JOBS;
		mappoint[x][y].int_2-=UNIVERSITY_GOODS;
		goods_used+=UNIVERSITY_GOODS;
		if (university_intake_rate>=(total_time%100))
		{
			mappoint[x][y].int_3+=UNIVERSITY_TECH_MADE;
			tech_level+=UNIVERSITY_TECH_MADE;
			mappoint[x][y].int_4++;
		}
	}
	if (total_time%100==0)
	{
		mappoint[x][y].int_5=mappoint[x][y].int_4;
		mappoint[x][y].int_4=0;
	}
	university_cost+=UNIVERSITY_RUNNING_COST;
}

void do_recycle(int x,int y)
{
	int i;
// int_1 is the ore made and waiting to go out
// int_2 is the used goods in store
// int_3 is the used steel in store       NOT USED at this time
// int_4 is the tech level when built
// int_5 is the recycling done so far this month
// int_6 is the percent of max recycling last month
// cost
	recycle_cost+=RECYCLE_RUNNING_COST;

// let these go through, even if we're full of waste. It's a waste of time
// checking.
        if (x>0 && (mappoint[x-1][y].flags & FLAG_IS_TRANSPORT)!=0)
        {
                i=mappoint[x-1][y].int_7;
		if (i>MAX_WASTE_AT_RECYCLE-mappoint[x][y].int_2)
			i=MAX_WASTE_AT_RECYCLE-mappoint[x][y].int_2;
                mappoint[x][y].int_2+=i;
                mappoint[x-1][y].int_7-=i;
        }
        if (y>0 && (mappoint[x][y-1].flags & FLAG_IS_TRANSPORT)!=0)
        {
                i=mappoint[x][y-1].int_7;
		if (i>MAX_WASTE_AT_RECYCLE-mappoint[x][y].int_2)
			i=MAX_WASTE_AT_RECYCLE-mappoint[x][y].int_2;
                mappoint[x][y].int_2+=i;
                mappoint[x][y-1].int_7-=i;
        }

// get some startup power if not powered yet
	if ((mappoint[x][y].flags & FLAG_POWERED)==0)
		if (get_power(x,y,GOODS_RECYCLED,1)!=0)
			mappoint[x][y].flags|=FLAG_POWERED;

// no steel recycling yet - no point, it's only used to make goods.
// recycle to ore.
	if (mappoint[x][y].int_1<MAX_ORE_AT_RECYCLE
		&& mappoint[x][y].int_2>GOODS_RECYCLED
		&& (mappoint[x][y].flags & FLAG_POWERED)!=0)
		if (get_jobs(x,y,RECYCLE_GOODS_JOBS)!=0)
		{
			if (get_power(x,y,GOODS_RECYCLED/2,1)==0)
				mappoint[x][y].flags
					&=(0xffffffff-FLAG_POWERED);
			else
				mappoint[x][y].flags|=FLAG_POWERED;
			mappoint[x][y].int_2-=GOODS_RECYCLED;
			i=(GOODS_RECYCLED*(10+((50*mappoint[x][y].int_4)
				/MAX_TECH_LEVEL)))/100;
			if (i>(GOODS_RECYCLED*8)/10)
				i=(GOODS_RECYCLED*8)/10;
			mappoint[x][y].int_1+=i;
			ore_made+=i;
			mappoint[x][y].int_5++;
		}
        if (total_time%100==0)
        {
                mappoint[x][y].int_6=mappoint[x][y].int_5;
                mappoint[x][y].int_5=0;
        }
// now bung the ore out
// put it on the railway
	if (x>0 && main_types[mappointtype[x-1][y]].group==GROUP_RAIL
		&& mappoint[x-1][y].int_5<MAX_ORE_ON_RAIL
		&& mappoint[x][y].int_1>=(MAX_ORE_ON_RAIL
			-mappoint[x-1][y].int_5))
	{
		if (get_jobs(x,y,JOBS_LOAD_ORE)!=0)
		{
			mappoint[x][y].int_1
				-=(MAX_ORE_ON_RAIL-mappoint[x-1][y].int_5);
			mappoint[x-1][y].int_5=MAX_ORE_ON_RAIL;
		}
	}
        if (y>0 && main_types[mappointtype[x][y-1]].group==GROUP_RAIL
                && mappoint[x][y-1].int_5<MAX_ORE_ON_RAIL
                && mappoint[x][y].int_1>=(MAX_ORE_ON_RAIL
                        -mappoint[x][y-1].int_5))
        {
		if (get_jobs(x,y,JOBS_LOAD_ORE)!=0)
		{
                	mappoint[x][y].int_1
                        	-=(MAX_ORE_ON_RAIL-mappoint[x][y-1].int_5);
                	mappoint[x][y-1].int_5=MAX_ORE_ON_RAIL;
		}
        }
// put it on the road
        if (x>0 && main_types[mappointtype[x-1][y]].group==GROUP_ROAD
                && mappoint[x-1][y].int_5<MAX_ORE_ON_ROAD
                && mappoint[x][y].int_1>=(MAX_ORE_ON_ROAD
                        -mappoint[x-1][y].int_5))
        {
                if (get_jobs(x,y,JOBS_LOAD_ORE)!=0)
                {
                        mappoint[x][y].int_1
                                -=(MAX_ORE_ON_ROAD-mappoint[x-1][y].int_5);
                        mappoint[x-1][y].int_5=MAX_ORE_ON_ROAD;
                }
        }
        if (y>0 && main_types[mappointtype[x][y-1]].group==GROUP_ROAD
                && mappoint[x][y-1].int_5<MAX_ORE_ON_ROAD
                && mappoint[x][y].int_1>=(MAX_ORE_ON_ROAD
                        -mappoint[x][y-1].int_5))
        {
                if (get_jobs(x,y,JOBS_LOAD_ORE)!=0)
                {
                        mappoint[x][y].int_1
                                -=(MAX_ORE_ON_ROAD-mappoint[x][y-1].int_5);
                        mappoint[x][y-1].int_5=MAX_ORE_ON_ROAD;
                }
        }
// put it on the tracks
        if (x>0 && main_types[mappointtype[x-1][y]].group==GROUP_TRACK
                && mappoint[x-1][y].int_5<MAX_ORE_ON_TRACK
                && mappoint[x][y].int_1>=(MAX_ORE_ON_TRACK
                        -mappoint[x-1][y].int_5))
        {
                if (get_jobs(x,y,JOBS_LOAD_ORE)!=0)
                {
                        mappoint[x][y].int_1
                                -=(MAX_ORE_ON_TRACK-mappoint[x-1][y].int_5);
                        mappoint[x-1][y].int_5=MAX_ORE_ON_TRACK;
                }
        }
        if (y>0 && main_types[mappointtype[x][y-1]].group==GROUP_TRACK
                && mappoint[x][y-1].int_5<MAX_ORE_ON_TRACK
                && mappoint[x][y].int_1>=(MAX_ORE_ON_TRACK
                        -mappoint[x][y-1].int_5))
        {
                if (get_jobs(x,y,JOBS_LOAD_ORE)!=0)
                {
                        mappoint[x][y].int_1
                                -=(MAX_ORE_ON_TRACK-mappoint[x][y-1].int_5);
                        mappoint[x][y-1].int_5=MAX_ORE_ON_TRACK;
                }
        }
// if we've still got >90% ore and waste in stock, burn some waste
// cleanly.
	if (mappoint[x][y].int_1>(MAX_ORE_AT_RECYCLE*9/10)
		&& mappoint[x][y].int_2>(MAX_WASTE_AT_RECYCLE*9/10))
		mappoint[x][y].int_2-=BURN_WASTE_AT_RECYCLE;
}

void do_health_centre(int x,int y)
{
// int_1 is the jobs stored at the health centre
// int_2 is the goods stored at the health centre
        if (mappoint[x][y].int_1
		<(MAX_JOBS_AT_HEALTH_CENTRE-HEALTH_CENTRE_GET_JOBS))
                if (get_jobs(x,y,HEALTH_CENTRE_GET_JOBS)!=0)
                        mappoint[x][y].int_1+=HEALTH_CENTRE_GET_JOBS;
        if (mappoint[x][y].int_2
		<(MAX_GOODS_AT_HEALTH_CENTRE-HEALTH_CENTRE_GET_GOODS))
                if (get_goods(x,y,HEALTH_CENTRE_GET_GOODS)!=0)
                        mappoint[x][y].int_2+=HEALTH_CENTRE_GET_GOODS;

// That's all. Cover is done by different functions every 3 months or so.

	health_cost+=HEALTH_RUNNING_COST;
}

void do_rocket_pad(int x,int y)
{
// You need ROCKET_PAD_JOBS, ROCKET_PAD_GOODS and ROCKET_PAD_STEEL 
// to add 1 to % of ready to fire.
// int_1 is the stored jobs
// int_2 is the stored goods
// int_3 is the stored steel
// int_4 is the count which gets to ROCKET_PAD_LAUNCH to fire.
// int_5 is the time of the next animation frame, when waiting for launch.

// get some jobs
	if (mappointtype[x][y]==CST_ROCKET_FLOWN)
		return;   // The rocket has been launched.
	if (mappoint[x][y].int_1<ROCKET_PAD_JOBS_STORE)
	{
		if (get_jobs(x,y,ROCKET_PAD_JOBS+10)!=0)
			mappoint[x][y].int_1+=ROCKET_PAD_JOBS;
	}
// get goods
	if (mappoint[x][y].int_2<ROCKET_PAD_GOODS_STORE)
	{
		if (get_goods(x,y,ROCKET_PAD_GOODS+10)!=0)
			mappoint[x][y].int_2+=ROCKET_PAD_GOODS;
		else if (get_goods(x,y,ROCKET_PAD_GOODS/10)!=0)
			mappoint[x][y].int_2+=ROCKET_PAD_GOODS/5;
		else if (get_goods(x,y,ROCKET_PAD_GOODS/50)!=0)
			mappoint[x][y].int_2+=ROCKET_PAD_GOODS/20;
	}
// get steel
        if (mappoint[x][y].int_3<ROCKET_PAD_STEEL_STORE)
        {
                if (get_steel(x,y,ROCKET_PAD_STEEL+10)!=0)
                        mappoint[x][y].int_3+=ROCKET_PAD_STEEL+10;
                else if (get_steel(x,y,ROCKET_PAD_STEEL/5)!=0)
                        mappoint[x][y].int_3+=ROCKET_PAD_STEEL/5;
                else if (get_steel(x,y,ROCKET_PAD_STEEL/20)!=0)
                        mappoint[x][y].int_3+=ROCKET_PAD_STEEL/20;
        }
#ifdef DEBUG_ROCKETS
	mappoint[x][y].int_4++;
#else
// now build the rocket.  Unlike uni's need a full store to make +1%
	if (mappointtype[x][y]<CST_ROCKET_5
		&& mappoint[x][y].int_1>=ROCKET_PAD_JOBS_STORE
		&& mappoint[x][y].int_2>=ROCKET_PAD_GOODS_STORE
		&& mappoint[x][y].int_3>=ROCKET_PAD_STEEL_STORE)
	{
		mappoint[x][y].int_1-=ROCKET_PAD_JOBS_STORE;
		mappoint[x][y].int_2-=ROCKET_PAD_GOODS_STORE;
		mappoint[x][y].int_3-=ROCKET_PAD_STEEL_STORE;
		mappoint[x][y].int_4++;
		goods_used+=ROCKET_PAD_GOODS_STORE;

	}
#endif
	rocket_pad_cost+=ROCKET_PAD_RUNNING_COST;
// animate and return if already said no to launch
	if (mappointtype[x][y]>=CST_ROCKET_5
		&& mappoint[x][y].int_4>=(100*ROCKET_PAD_LAUNCH)/100)
	{
		if (real_time>=mappoint[x][y].int_5)
		{
			mappoint[x][y].int_5
				=real_time+ROCKET_ANIMATION_SPEED;
			switch (mappointtype[x][y])
			{
				case(CST_ROCKET_5):
					mappointtype[x][y]=CST_ROCKET_6;
					break;
				case(CST_ROCKET_6):
					mappointtype[x][y]=CST_ROCKET_7;
					break;
				case(CST_ROCKET_7):
					mappointtype[x][y]=CST_ROCKET_5;
					break;
			}
		}
		return;
	}
// now choose a graphic
	if (mappoint[x][y].int_4<(25*ROCKET_PAD_LAUNCH)/100)
		mappointtype[x][y]=CST_ROCKET_1;
	else if (mappoint[x][y].int_4<(60*ROCKET_PAD_LAUNCH)/100)
		mappointtype[x][y]=CST_ROCKET_2;
	else if (mappoint[x][y].int_4<(90*ROCKET_PAD_LAUNCH)/100)
		mappointtype[x][y]=CST_ROCKET_3;
	else if (mappoint[x][y].int_4<(100*ROCKET_PAD_LAUNCH)/100)
		mappointtype[x][y]=CST_ROCKET_4;
	else if (mappoint[x][y].int_4>=(100*ROCKET_PAD_LAUNCH)/100)
	{
		mappointtype[x][y]=CST_ROCKET_5;
		update_main_screen();
		if (yn_dial_box("Rocket ready to launch"
			,"You can launch it now or wait until later."
			,"If you wait it will continue costing you money."
			,"Launch it later by clicking on the rocket area.")
			!=0)
			launch_rocket(x,y);
		if (x==last_built_x && y==last_built_y) // so we don't get
		{				// get our money back when
			x=0;			// we bulldoze.
			y=0;
		}
	}
}

void launch_rocket(int x,int y)
{
//	char s[100];
	int i,r,xx,yy,xxx,yyy;
	rockets_launched++;
	mappointtype[x][y]=CST_ROCKET_FLOWN;
	update_main_screen();
	r=rand()%MAX_TECH_LEVEL;
#ifdef DEBUG_ROCKETS
	if (0)
#else
	if (r>tech_level || rand()%100>(rockets_launched*15+25))
  // the launch failed
#endif
	{
		ok_dial_box("launch-fail.mes",BAD,0L);
		rockets_launched_success=0;
		xx=((rand()%40)-20)+x;
		yy=((rand()%40)-20)+y;
		for (i=0;i<20;i++)
		{
			xxx=((rand()%20)-10)+xx;
			yyy=((rand()%20)-10)+yy;
			if (xxx>0 && xxx<WORLD_SIDE_LEN
				&& yyy>0 && yyy<WORLD_SIDE_LEN)
			{  // don't crash on it's own area
				if (xxx>=x && xxx<(x+4) && yyy>=y
					&& yyy<(y+4))
					continue;
				fire_area(xxx,yyy);
// make a sound perhaps
			}
		}
	}		
	else
	{
		
		rockets_launched_success++;
		if (rockets_launched_success>5)
		{
			ok_dial_box("launch-evac.mes",GOOD,0L);
			remove_people(1000);
		}
		else
			ok_dial_box("launch-good.mes",GOOD,0L);
	}
}
 

void remove_people(int num)
{
	int x,y,f;
	time_t t;
	f=1;
	t=time(0);
	while (f && (num>0))
	{
		f=0;
		for (y=0;y<WORLD_SIDE_LEN;y++)
		for (x=0;x<WORLD_SIDE_LEN;x++)
			if (main_types[mappointtype[x][y]].group
					==GROUP_RESIDENCE
				&& mappoint[x][y].population>0)
			{
				f=1;
				mappoint[x][y].population--;
				num--;
				total_evacuated++;
			}
	}
	while (num>0 && people_pool>0)
	{
		num--;
		total_evacuated++;
		people_pool--;
//		if (people_pool<=0)
//			ok_dial_box("launch-gone.mes",GOOD,0L);
	}
	if (num>0)  // last ship wasn't full so everyone has gone.
	{
		if (t>HOF_START && t<HOF_STOP)
			ok_dial_box("launch-gone-mail.mes",GOOD,0L);
		else
			ok_dial_box("launch-gone.mes",GOOD,0L);
		housed_population=0;
		if (t>HOF_START && t<HOF_STOP)
			if (yn_dial_box("Mail the LinCity hall of fame?"
				,"If your system can send mail, you can"
				,"automatically be added to the hall of fame."
				,"www.floot.demon.co.uk/lc-hof.html")!=0)
				mail_results();
	}
}

void do_windmill(int x,int y)
{
// int_1 has the current power left
// int_2 is the tech level when built
// int_3 is the sail count - to choose the right sail.
// int_4 is the last real time that a sail was turned
	int flag=0;
	if (mappoint[x][y].int_1<POWER_LINE_CAPACITY)  // sep the ifs, to
	{
	if (get_jobs(x,y,WINDMILL_JOBS)!=0) //make sure jobs not got when full 
	{
		mappoint[x][y].int_1+=WINDMILL_POWER+((WINDMILL_POWER
			*mappoint[x][y].int_2)/MAX_TECH_LEVEL);
		if (total_time%WINDMILL_RCOST==0)
			windmill_cost++;
		flag=1;
	}
	else
	{
		mappoint[x][y].int_4=real_time+WINDMILL_ANIM_SPEED;
	}
	}
	else
	{
		mappoint[x][y].int_4=real_time+WINDMILL_ANIM_SPEED;
		flag=1;
	}

	if (real_time>mappoint[x][y].int_4)
	{
		mappoint[x][y].int_3++;
		mappoint[x][y].int_4=real_time+WINDMILL_ANIM_SPEED;
		if (mappoint[x][y].int_2<MODERN_WINDMILL_TECH)
			mappoint[x][y].int_4+=WINDMILL_ANIM_SPEED; // halve
						// the speed for 'old' ones
	}
	if (mappoint[x][y].int_2<MODERN_WINDMILL_TECH)
	{
		switch((mappoint[x][y].int_3)%3) 
		{
			case (0):
				mappointtype[x][y]=CST_WINDMILL_2_W;
				break;
			case (1):
				mappointtype[x][y]=CST_WINDMILL_3_W;
				break;
			case (2):
				mappointtype[x][y]=CST_WINDMILL_1_W;
				break;
			default: mappointtype[x][y]=CST_WINDMILL_1_W;
		}
	}
	else if ((mappoint[x][y].int_3)%3==0)
	{
		if (flag!=0 && mappoint[x][y].int_1>POWER_LINE_CAPACITY/2)
			mappointtype[x][y]=CST_WINDMILL_1_G;
		else if (flag!=0)
			mappointtype[x][y]=CST_WINDMILL_1_RG;
		else
			mappointtype[x][y]=CST_WINDMILL_1_R;
			
	}
	else if ((mappoint[x][y].int_3)%3==1)
	{
		if (flag!=0 && mappoint[x][y].int_1>POWER_LINE_CAPACITY/2)
			mappointtype[x][y]=CST_WINDMILL_2_G;
		else if (flag!=0)
			mappointtype[x][y]=CST_WINDMILL_2_RG;
		else
			mappointtype[x][y]=CST_WINDMILL_2_R;
	}
	else
	{
		if (flag!=0 && mappoint[x][y].int_1>POWER_LINE_CAPACITY/2)
			mappointtype[x][y]=CST_WINDMILL_3_G;
		else if (flag!=0)
			mappointtype[x][y]=CST_WINDMILL_3_RG;
		else
			mappointtype[x][y]=CST_WINDMILL_3_R;
	}
	if (mappoint[x][y].int_1>=POWER_LINE_CAPACITY)
	{
		if (main_types[mappointtype[x-1][y]].group==GROUP_POWER_LINE
			&& mappoint[x-1][y].int_1==0)
		{
			mappoint[x-1][y].int_1=1;
			mappoint[x][y].int_1-=POWER_LINE_CAPACITY;
                        if (mappointtype[x-1][y]>CST_POWERL_LUDR_L)
                                mappointtype[x-1][y]-=11;
                        power_made++;
		}
		else if (main_types[mappointtype[x][y-1]].group
			==GROUP_POWER_LINE && mappoint[x][y-1].int_1==0)
		{
			mappoint[x][y-1].int_1=1;
			mappoint[x][y].int_1-=POWER_LINE_CAPACITY;
                        if (mappointtype[x][y-1]>CST_POWERL_LUDR_L)
                                mappointtype[x][y-1]-=11;
                        power_made++;
		}
	}
}

void do_monument(int x,int y)
{
// int_1 holds the jobs used
// int_2 holds the tech points made
// int_3 holds the tail off count
	if (mappoint[x][y].int_1<BUILD_MONUMENT_JOBS)
		if (get_jobs(x,y,MONUMENT_GET_JOBS)!=0)
			mappoint[x][y].int_1+=MONUMENT_GET_JOBS;

// now choose a graphic
	if (mappoint[x][y].int_1>=BUILD_MONUMENT_JOBS)
	{
		mappointtype[x][y]=CST_MONUMENT_5;
// inc tech level only if fully built and tech less than MONUMENT_TECH_EXPIRE
	        if (tech_level<(MONUMENT_TECH_EXPIRE*1000)
			&& (total_time%MONUMENT_DAYS_PER_TECH)==1)
	        {
			if (mappoint[x][y].int_3++>(tech_level/10000)-2)
			{
	                	tech_level++;
	                	mappoint[x][y].int_2++;
				mappoint[x][y].int_3=0;
			}
	        }
	}
	else if (mappoint[x][y].int_1>=((BUILD_MONUMENT_JOBS*4)/5))
		mappointtype[x][y]=CST_MONUMENT_4;
	else if (mappoint[x][y].int_1>=((BUILD_MONUMENT_JOBS*3)/5))
		mappointtype[x][y]=CST_MONUMENT_3;
	else if (mappoint[x][y].int_1>=((BUILD_MONUMENT_JOBS*2)/5))
		mappointtype[x][y]=CST_MONUMENT_2;
	else if (mappoint[x][y].int_1>=(BUILD_MONUMENT_JOBS/20))
		mappointtype[x][y]=CST_MONUMENT_1;
	else
		mappointtype[x][y]=CST_MONUMENT_0;
}

void do_school(int x,int y)
{ 
// int_1 contains the job pool
// int_2 contains the goods at the school
// int_3 has the tech points made
// int_4 is the tech count so far this 100 days
// int_5 is the tech count last 100 days to give a % of max production
	if (mappoint[x][y].int_1<(MAX_JOBS_AT_SCHOOL-SCHOOL_JOBS))
		if (get_jobs(x,y,SCHOOL_JOBS)!=0)
			mappoint[x][y].int_1+=SCHOOL_JOBS;
	if (mappoint[x][y].int_2<(MAX_GOODS_AT_SCHOOL-SCHOOL_GOODS))
		if (get_goods(x,y,SCHOOL_GOODS)!=0)
			mappoint[x][y].int_2+=SCHOOL_GOODS;
	if (mappoint[x][y].int_1>=JOBS_MAKE_TECH_SCHOOL
		&& mappoint[x][y].int_2>=GOODS_MAKE_TECH_SCHOOL)
	{
		mappoint[x][y].int_1-=JOBS_MAKE_TECH_SCHOOL;
		mappoint[x][y].int_2-=GOODS_MAKE_TECH_SCHOOL;
		mappoint[x][y].int_3+=TECH_MADE_BY_SCHOOL;
		mappoint[x][y].int_4++;
		tech_level+=TECH_MADE_BY_SCHOOL;
	}
	school_cost+=SCHOOL_RUNNING_COST;
	if ((total_time%100)==0)
	{
		mappoint[x][y].int_5=mappoint[x][y].int_4;
		mappoint[x][y].int_4=0;
	}
}

void do_blacksmith(int x,int y)
{
// int_1 contains the goods at the blacksmith
// int_2 contains the goods made - for the animation
// int_3 contains the coal store
// int_4 is the animation trigger time
// int_5 is the % made so far this month
// int_6 is the % capacity last month
	if (mappoint[x][y].int_3<MAX_COAL_AT_BLACKSMITH)
		if (get_coal(x,y,BLACKSMITH_GET_COAL)!=0)
			mappoint[x][y].int_3+=BLACKSMITH_GET_COAL;
	if (mappoint[x][y].int_1<MAX_GOODS_AT_BLACKSMITH
		&& mappoint[x][y].int_3>=BLACKSMITH_COAL_USED)
	{
		if (get_steel(x,y,BLACKSMITH_STEEL_USED)!=0)
		{
			mappoint[x][y].int_1+=GOODS_MADE_BY_BLACKSMITH;
			mappoint[x][y].int_3-=BLACKSMITH_COAL_USED;
		}
	}
	if (get_jobs(x,y,BLACKSMITH_JOBS)!=0)
	{
		if (mappoint[x][y].int_1>GOODS_MADE_BY_BLACKSMITH)
		{
			if (put_goods(x,y,GOODS_MADE_BY_BLACKSMITH-1)!=0)
			{
				mappoint[x][y].int_1-=(GOODS_MADE_BY_BLACKSMITH-1);
				mappoint[x][y].int_2+=(GOODS_MADE_BY_BLACKSMITH-1);
				mappoint[x][y].int_5++;
			}
			else
				put_jobs(x,y,BLACKSMITH_JOBS);
		}
		else
			put_jobs(x,y,BLACKSMITH_JOBS);
	}
	else
		mappointtype[x][y]=CST_BLACKSMITH_0;
	if (mappoint[x][y].int_2>BLACKSMITH_BATCH
	&& real_time>=mappoint[x][y].int_4)
	{
		mappoint[x][y].int_4=real_time+BLACKSMITH_ANIM_SPEED;
		switch(mappointtype[x][y])
		{
			case(CST_BLACKSMITH_0):
				mappointtype[x][y]=CST_BLACKSMITH_1;
				break;
			case(CST_BLACKSMITH_1):
				mappointtype[x][y]=CST_BLACKSMITH_2;
				break;
			case(CST_BLACKSMITH_2):
				mappointtype[x][y]=CST_BLACKSMITH_3;
				break;
			case(CST_BLACKSMITH_3):
				mappointtype[x][y]=CST_BLACKSMITH_4;
				break;
			case(CST_BLACKSMITH_4):
				mappointtype[x][y]=CST_BLACKSMITH_5;
				break;
			case(CST_BLACKSMITH_5):
				mappointtype[x][y]=CST_BLACKSMITH_6;
				break;
			case(CST_BLACKSMITH_6):
				mappointtype[x][y]=CST_BLACKSMITH_1;
				mappoint[x][y].int_2=0;
				mappointpol[x][y]++;
				break;
		}
	}
	if (total_time%100==0)
	{
		mappoint[x][y].int_6=mappoint[x][y].int_5;
		mappoint[x][y].int_5=0;
	}
}

void do_mill(int x,int y)
{
// int_1 contains the goods at the mill
// int_2 contains the food store
// int_3 contains the coal store
// int_4 contains the animation trigger time
// int_5 is the % count so far this month
// int_6 is the % capacity last month
// get food
	int block_anim=0;
        if (mappoint[x][y].int_2<MAX_FOOD_AT_MILL)
                if (get_food(x,y,MILL_GET_FOOD)!=0)
                        mappoint[x][y].int_2+=MILL_GET_FOOD;
// get coal
        if (mappoint[x][y].int_3<MAX_COAL_AT_MILL)
	{
                if (get_coal(x,y,MILL_GET_COAL)!=0)
			mappoint[x][y].int_3+=MILL_GET_COAL;
		else
			if (get_power(x,y,MILL_GET_COAL
				*MILL_POWER_PER_COAL,0)!=0)
				mappoint[x][y].int_3+=MILL_GET_COAL;
	}
        if (mappoint[x][y].int_1<MAX_GOODS_AT_MILL)
	{
                if (mappoint[x][y].int_2>FOOD_USED_BY_MILL
			&& mappoint[x][y].int_3>COAL_USED_BY_MILL)
		{
			if (get_jobs(x,y,MILL_JOBS)!=0)
			{
				mappoint[x][y].int_2-=FOOD_USED_BY_MILL;
				mappoint[x][y].int_3-=COAL_USED_BY_MILL;
				mappoint[x][y].int_1+=GOODS_MADE_BY_MILL;
				mappoint[x][y].int_5++;
			}
			else
			{
				mappointtype[x][y]=CST_MILL_0;
				block_anim=1;
			}
		}
		else
			block_anim=1;
	}

        if (mappoint[x][y].int_1>0)
                if (put_goods(x,y,mappoint[x][y].int_1)!=0)
                        mappoint[x][y].int_1=0;

	if (total_time%100==0)
	{
		mappoint[x][y].int_6=mappoint[x][y].int_5;
		mappoint[x][y].int_5=0;
	}
        if (real_time>=mappoint[x][y].int_4 && block_anim==0)
        {
                mappoint[x][y].int_4=real_time+MILL_ANIM_SPEED;
		switch(mappointtype[x][y])
		{
			case(CST_MILL_0):
				mappointtype[x][y]=CST_MILL_1;
				break;
			case(CST_MILL_1):
				mappointtype[x][y]=CST_MILL_2;
				break;
			case(CST_MILL_2):
				mappointtype[x][y]=CST_MILL_3;
				break;
			case(CST_MILL_3):
				mappointtype[x][y]=CST_MILL_4;
				break;
			case(CST_MILL_4):
				mappointtype[x][y]=CST_MILL_5;
				break;
			case(CST_MILL_5):
				mappointtype[x][y]=CST_MILL_6;
				break;
			case(CST_MILL_6):
				mappointtype[x][y]=CST_MILL_1;
				mappointpol[x][y]++;
				break;
		}
        }
}

void do_pottery(int x,int y)
{
// int_1 contains the goods at the pottery
// int_2 contains the ore at the pottery
// int_3 contains the coal at the pottery
// int_4 is the animation trigger time
// int_5 is the % made so far this month or the close time if -ve
// int_6 is the % capacity last month
// int_7 contains the jobs stored at the pottery
	if (mappoint[x][y].int_5<0)
	{
		mappoint[x][y].int_5++;
		return;
	}
	if (mappoint[x][y].int_1<(MAX_GOODS_AT_POTTERY-POTTERY_MADE_GOODS))
	{
		if (mappoint[x][y].int_2
				<(MAX_ORE_AT_POTTERY-POTTERY_GET_ORE))
			if (get_ore(x,y,POTTERY_GET_ORE)!=0)
				mappoint[x][y].int_2+=POTTERY_GET_ORE;
		if (mappoint[x][y].int_3
				<(MAX_COAL_AT_POTTERY-POTTERY_GET_COAL))
			if (get_coal(x,y,POTTERY_GET_COAL)!=0)
				mappoint[x][y].int_3+=POTTERY_GET_COAL;
                if (mappoint[x][y].int_7
                                <(MAX_JOBS_AT_POTTERY-POTTERY_GET_JOBS))
                        if (get_jobs(x,y,POTTERY_GET_JOBS)!=0)
                                mappoint[x][y].int_7+=POTTERY_GET_JOBS;

		if (mappoint[x][y].int_2>POTTERY_ORE_MAKE_GOODS
			&& mappoint[x][y].int_3>POTTERY_COAL_MAKE_GOODS
			&& mappoint[x][y].int_7>POTTERY_JOBS)
		{
			mappoint[x][y].int_1+=POTTERY_MADE_GOODS;
			mappoint[x][y].int_2-=POTTERY_ORE_MAKE_GOODS;
			mappoint[x][y].int_3-=POTTERY_COAL_MAKE_GOODS;
			mappoint[x][y].int_7-=POTTERY_JOBS;
			mappoint[x][y].int_5++;
		}
		else
		{
			mappointtype[x][y]=CST_POTTERY_1;
			mappoint[x][y].int_6=0;
			mappoint[x][y].int_5=-POTTERY_CLOSE_TIME;
			return;
		}
	}
        if (mappoint[x][y].int_1>0)
            	if (put_goods(x,y,mappoint[x][y].int_1)!=0)
                	mappoint[x][y].int_1=0;

	if (total_time%100==0)
	{
		mappoint[x][y].int_6=mappoint[x][y].int_5;
		mappoint[x][y].int_5=0;
	}
	if (real_time>=mappoint[x][y].int_4 /* && block_anim==0 */ )
	{
		mappoint[x][y].int_4=real_time+POTTERY_ANIM_SPEED;
		switch (mappointtype[x][y])
		{
			case(CST_POTTERY_0):
				mappointtype[x][y]=CST_POTTERY_1;
				break;
			case(CST_POTTERY_1):
				mappointtype[x][y]=CST_POTTERY_2;
				break;
			case(CST_POTTERY_2):
				mappointtype[x][y]=CST_POTTERY_3;
				break;
			case(CST_POTTERY_3):
				mappointtype[x][y]=CST_POTTERY_4;
				break;
			case(CST_POTTERY_4):
				mappointtype[x][y]=CST_POTTERY_5;
				break;
			case(CST_POTTERY_5):
				mappointtype[x][y]=CST_POTTERY_6;
				break;
			case(CST_POTTERY_6):
				mappointtype[x][y]=CST_POTTERY_7;
				break;
			case(CST_POTTERY_7):
				mappointtype[x][y]=CST_POTTERY_8;
				break;
			case(CST_POTTERY_8):
				mappointtype[x][y]=CST_POTTERY_9;
				break;
			case(CST_POTTERY_9):
				mappointtype[x][y]=CST_POTTERY_10;
				break;
			case(CST_POTTERY_10):
				mappointtype[x][y]=CST_POTTERY_1;
				mappointpol[x][y]++;
				break;
		}
	}
}

void do_firestation(int x,int y)
{
// int_1 is the jobs stored at the fire station
// int_2 is the goods stored at the fire station
// int_3 is the animation flag
// int_4 is the time of the next frame
// int_5 is the pause counter
	if (mappoint[x][y].int_1
		<(MAX_JOBS_AT_FIRESTATION-FIRESTATION_GET_JOBS))
		if (get_jobs(x,y,FIRESTATION_GET_JOBS)!=0)
			mappoint[x][y].int_1+=FIRESTATION_GET_JOBS;
	if (mappoint[x][y].int_2
		<(MAX_GOODS_AT_FIRESTATION-FIRESTATION_GET_GOODS))
		if (get_goods(x,y,FIRESTATION_GET_GOODS)!=0)
			mappoint[x][y].int_2+=FIRESTATION_GET_GOODS;
// animate
        if (mappoint[x][y].int_3 && real_time>mappoint[x][y].int_4)
        {
                mappoint[x][y].int_4=real_time+FIRESTATION_ANIMATION_SPEED;
		if (mappoint[x][y].int_5>0)
			mappoint[x][y].int_5--;
		else
		{
                	switch(mappointtype[x][y])
                	{
                        case(CST_FIRESTATION_1):
                                mappointtype[x][y]=CST_FIRESTATION_2;
                                break;
                        case(CST_FIRESTATION_2):
                                mappointtype[x][y]=CST_FIRESTATION_3;
                                break;
                        case(CST_FIRESTATION_3):
                                mappointtype[x][y]=CST_FIRESTATION_4;
                                break;
                        case(CST_FIRESTATION_4):
                                mappointtype[x][y]=CST_FIRESTATION_5;
                                break;
                        case(CST_FIRESTATION_5):
                                mappointtype[x][y]=CST_FIRESTATION_6;
                                break;
                        case(CST_FIRESTATION_6):
                                mappointtype[x][y]=CST_FIRESTATION_7;
				mappoint[x][y].int_5=10; // pause
                                break;
                        case(CST_FIRESTATION_7):
                                mappointtype[x][y]=CST_FIRESTATION_8;
                                break;
                        case(CST_FIRESTATION_8):
                                mappointtype[x][y]=CST_FIRESTATION_9;
                                break;
                        case(CST_FIRESTATION_9):
                                mappointtype[x][y]=CST_FIRESTATION_10;
                                break;
                        case(CST_FIRESTATION_10):
                                mappointtype[x][y]=CST_FIRESTATION_1;
				mappoint[x][y].int_3=0; // stop
                                break;


			}
		}
	}
// That's all. Cover is done by different functions every 3 months or so.

	fire_cost+=FIRESTATION_RUNNING_COST;
}

void do_cricket(int x,int y)
{
// int_1 is the jobs stored at the pavillion
// int_2 is the goods stored at the pavillion
// int_3 is the animation flag
// int_4 is the time of the next frame
        if (mappoint[x][y].int_1
		<(MAX_JOBS_AT_CRICKET-CRICKET_GET_JOBS))
                if (get_jobs(x,y,CRICKET_GET_JOBS)!=0)
                        mappoint[x][y].int_1+=CRICKET_GET_JOBS;
        if (mappoint[x][y].int_2
		<(MAX_GOODS_AT_CRICKET-CRICKET_GET_GOODS))
                if (get_goods(x,y,CRICKET_GET_GOODS)!=0)
                        mappoint[x][y].int_2+=CRICKET_GET_GOODS;
// animate
	if (mappoint[x][y].int_3 && real_time>mappoint[x][y].int_4)
	{
		mappoint[x][y].int_4=real_time+CRICKET_ANIMATION_SPEED;
		switch(mappointtype[x][y])
		{
			case(CST_CRICKET_1): 
				mappointtype[x][y]=CST_CRICKET_2;
				break;
			case(CST_CRICKET_2):
				mappointtype[x][y]=CST_CRICKET_3;
				break;
			case(CST_CRICKET_3):
				mappointtype[x][y]=CST_CRICKET_4;
				break;
			case(CST_CRICKET_4):
				mappointtype[x][y]=CST_CRICKET_5;
				break;
			case(CST_CRICKET_5):
				mappointtype[x][y]=CST_CRICKET_6;
				break;
			case(CST_CRICKET_6):
				mappointtype[x][y]=CST_CRICKET_7;
				break;
			case(CST_CRICKET_7):
				mappointtype[x][y]=CST_CRICKET_1;
				mappoint[x][y].int_3=0; // disable anim
				break;
		}
	}

// That's all. Cover is done by different functions every 3 months or so.

        cricket_cost+=CRICKET_RUNNING_COST;
}


void clear_fire_health_and_cricket_cover(void)
{
	int x,y,m;
	m=0xffffffff-(FLAG_FIRE_COVER | FLAG_HEALTH_COVER 
		| FLAG_CRICKET_COVER);
	for (y=0;y<WORLD_SIDE_LEN;y++)
		for (x=0;x<WORLD_SIDE_LEN;x++)
			mappoint[x][y].flags&=m;
// Wow... chache misses or what!
}

void do_fire_health_and_cricket_cover(void)
{
	int x,y;
        for (y=0;y<WORLD_SIDE_LEN;y++)
                for (x=0;x<WORLD_SIDE_LEN;x++)
		{  //  The next few lines need changing to test for
		   //  the group if these areas are animated.
			if (main_types[mappointtype[x][y]].group
					==GROUP_FIRESTATION)
				do_fire_cover(x,y);
			else if (mappointtype[x][y]==CST_HEALTH)
				do_health_cover(x,y);
			else if (main_types[mappointtype[x][y]].group
					==GROUP_CRICKET)
				do_cricket_cover(x,y);
		}
}

void do_fire_cover(int x, int y)
{
	int xx,x1,x2,y1,y2;
        if (mappoint[x][y].int_1<(FIRESTATION_JOBS*DAYS_BETWEEN_COVER) ||
		mappoint[x][y].int_2<(FIRESTATION_GOODS*DAYS_BETWEEN_COVER))
                        return;
        mappoint[x][y].int_1-=(FIRESTATION_JOBS*DAYS_BETWEEN_COVER);
        mappoint[x][y].int_2-=(FIRESTATION_GOODS*DAYS_BETWEEN_COVER);
	mappoint[x][y].int_3=1; // turn on animation
	x1=x-FIRESTATION_RANGE;
	if (x1<0)
		x1=0;
	x2=x+FIRESTATION_RANGE;
	if (x2>WORLD_SIDE_LEN)
		x2=WORLD_SIDE_LEN;
	y1=y-FIRESTATION_RANGE;
	if (y1<0)
		y1=0;
	y2=y+FIRESTATION_RANGE;
	if (y2>WORLD_SIDE_LEN)
		y2=WORLD_SIDE_LEN;
	for (;y1<y2;y1++)
		for (xx=x1;xx<x2;xx++)
			mappoint[xx][y1].flags|=FLAG_FIRE_COVER;
}

void do_health_cover(int x, int y)
{
        int xx,x1,x2,y1,y2;
	if (mappoint[x][y].int_1<(HEALTH_CENTRE_JOBS*DAYS_BETWEEN_COVER) ||
		mappoint[x][y].int_2<(HEALTH_CENTRE_GOODS*DAYS_BETWEEN_COVER))
			return;
	mappoint[x][y].int_1-=(HEALTH_CENTRE_JOBS*DAYS_BETWEEN_COVER);
	mappoint[x][y].int_2-=(HEALTH_CENTRE_GOODS*DAYS_BETWEEN_COVER);
        x1=x-HEALTH_CENTRE_RANGE;
        if (x1<0)
                x1=0;
        x2=x+HEALTH_CENTRE_RANGE;
        if (x2>WORLD_SIDE_LEN)
                x2=WORLD_SIDE_LEN;
        y1=y-HEALTH_CENTRE_RANGE;
        if (y1<0)
                y1=0;
        y2=y+HEALTH_CENTRE_RANGE;
        if (y2>WORLD_SIDE_LEN)
                y2=WORLD_SIDE_LEN;
        for (;y1<y2;y1++)
                for (xx=x1;xx<x2;xx++)
                        mappoint[xx][y1].flags|=FLAG_HEALTH_COVER;
}

void do_cricket_cover(int x, int y)
{
        int xx,x1,x2,y1,y2;
        if (mappoint[x][y].int_1<(CRICKET_JOBS*DAYS_BETWEEN_COVER) ||
                mappoint[x][y].int_2<(CRICKET_GOODS*DAYS_BETWEEN_COVER))
                        return;
        mappoint[x][y].int_1-=(CRICKET_JOBS*DAYS_BETWEEN_COVER);
        mappoint[x][y].int_2-=(CRICKET_GOODS*DAYS_BETWEEN_COVER);
	mappoint[x][y].int_3=1; // turn on animation
        x1=x-CRICKET_RANGE;
        if (x1<0)
                x1=0;
        x2=x+CRICKET_RANGE;
        if (x2>WORLD_SIDE_LEN)
                x2=WORLD_SIDE_LEN;
        y1=y-CRICKET_RANGE;
        if (y1<0)
                y1=0;
        y2=y+CRICKET_RANGE;
        if (y2>WORLD_SIDE_LEN)
                y2=WORLD_SIDE_LEN;
        for (;y1<y2;y1++)
                for (xx=x1;xx<x2;xx++)
                        mappoint[xx][y1].flags|=FLAG_CRICKET_COVER;
}


void do_random_fire(int x,int y,int pwarning) // well random if x=y=-1
{
	int xx,yy;
	if (x==-1 && y==-1)
	{
		x=rand()%WORLD_SIDE_LEN;
		y=rand()%WORLD_SIDE_LEN;
	}
	else
	{
		if (x<0 || x>=WORLD_SIDE_LEN || y<0 || y>=WORLD_SIDE_LEN)
			return;
	}
	if (mappointtype[x][y]==CST_USED)
	{
		xx=mappoint[x][y].int_1;
		yy=mappoint[x][y].int_2;
		x=xx;
		y=yy;
	}
	xx=rand()%100;
	if (xx>=(fire_chance[main_types[mappointtype[x][y]].group]))
		return;
	if ((mappoint[x][y].flags&FLAG_FIRE_COVER)!=0)
			return;
	if (pwarning)
	{
	if (main_types[mappointtype[x][y]].group==GROUP_POWER_LINE)
		ok_dial_box("fire.mes",BAD,"It's at a power line.");
        else if (main_types[mappointtype[x][y]].group==GROUP_POWER_SOURCE)
                ok_dial_box("fire.mes",BAD,"It's at a solar power station.");
        else if (main_types[mappointtype[x][y]].group==GROUP_SUBSTATION)
                ok_dial_box("fire.mes",BAD,"It's at a substation.");
        else if (main_types[mappointtype[x][y]].group==GROUP_RESIDENCE)
                ok_dial_box("fire.mes",BAD,"It's at a residential area.");
        else if (main_types[mappointtype[x][y]].group==GROUP_ORGANIC_FARM)
                ok_dial_box("fire.mes",BAD,"It's at a farm.");
        else if (main_types[mappointtype[x][y]].group==GROUP_MARKET)
                ok_dial_box("fire.mes",BAD,"It's at a market.");
        else if (main_types[mappointtype[x][y]].group==GROUP_TRACK)
                ok_dial_box("fire.mes",BAD,"It's at a track.");
        else if (main_types[mappointtype[x][y]].group==GROUP_COALMINE)
                ok_dial_box("fire.mes",BAD,"It's at a coal mine.");
        else if (main_types[mappointtype[x][y]].group==GROUP_RAIL)
                ok_dial_box("fire.mes",BAD,"It's at a railway.");
        else if (main_types[mappointtype[x][y]].group==GROUP_POWER_SOURCE_COAL)
                ok_dial_box("fire.mes",BAD,"It's at a coal power station.");
        else if (main_types[mappointtype[x][y]].group==GROUP_ROAD)
                ok_dial_box("fire.mes",BAD,"It's at a road.");
        else if (main_types[mappointtype[x][y]].group==GROUP_INDUSTRY_L)
                ok_dial_box("fire.mes",BAD,"It's at light industry.");
        else if (main_types[mappointtype[x][y]].group==GROUP_UNIVERSITY)
                ok_dial_box("fire.mes",BAD,"It's at a university.");
        else if (main_types[mappointtype[x][y]].group==GROUP_COMMUNE)
                ok_dial_box("fire.mes",BAD,"It's at a commune.");
        else if (main_types[mappointtype[x][y]].group==GROUP_TIP)
                ok_dial_box("fire.mes",BAD,"It's at a tip.");
        else if (main_types[mappointtype[x][y]].group==GROUP_EX_PORT)
                ok_dial_box("fire.mes",BAD,"It's at a port.");
        else if (main_types[mappointtype[x][y]].group==GROUP_INDUSTRY_H)
                ok_dial_box("fire.mes",BAD,"It's at a steel works.");
        else if (main_types[mappointtype[x][y]].group==GROUP_RECYCLE)
                ok_dial_box("fire.mes",BAD,"It's at a recycle centre.");
        else if (main_types[mappointtype[x][y]].group==GROUP_HEALTH)
                ok_dial_box("fire.mes",BAD,"It's at a health centre.");
        else if (main_types[mappointtype[x][y]].group==GROUP_ROCKET)
                ok_dial_box("fire.mes",BAD,"It's at a rocket site.");
        else if (main_types[mappointtype[x][y]].group==GROUP_WINDMILL)
                ok_dial_box("fire.mes",BAD,"It's at a windmill.");
        else if (main_types[mappointtype[x][y]].group==GROUP_SCHOOL)
                ok_dial_box("fire.mes",BAD,"It's at a school.");
        else if (main_types[mappointtype[x][y]].group==GROUP_BLACKSMITH)
                ok_dial_box("fire.mes",BAD,"It's at a blacksmith.");
        else if (main_types[mappointtype[x][y]].group==GROUP_MILL)
                ok_dial_box("fire.mes",BAD,"It's at a mill.");
        else if (main_types[mappointtype[x][y]].group==GROUP_POTTERY)
                ok_dial_box("fire.mes",BAD,"It's at a pottery.");
        else if (main_types[mappointtype[x][y]].group==GROUP_FIRESTATION)
                ok_dial_box("fire.mes",BAD,"It's at a fire station!!!.");
	else if (main_types[mappointtype[x][y]].group==GROUP_CRICKET)
		ok_dial_box("fire.mes",BAD,"It's at a cricket pitch!!!.");
	else if (main_types[mappointtype[x][y]].group==GROUP_SHANTY)
		ok_dial_box("fire.mes",BAD,"It's at a shanty town.");
	else
		ok_dial_box("fire.mes",BAD,"UNKNOWN!");
	}
//	if (main_types[mappointtype[x][y]].group==GROUP_SHANTY)
//		numof_shanties--;
	fire_area(x,y);
}

void do_fire(int x,int y)
{
// int_1 is the next animation frame time
// int_2 is the fire length
// int_3 is the real_time before the fire can spread or -1 if triggered 
// int_4 is the idle land length
	int i;
	if (mappoint[x][y].int_2==0) // this so we don't get whole blocks
				// changing in one go.
		mappoint[x][y].int_2=rand()%(FIRE_LENGTH/5);
	if (mappoint[x][y].int_2>FIRE_LENGTH)
	{
		if (mappoint[x][y].int_4==0) // rand length here also
			mappoint[x][y].int_4=rand()%(AFTER_FIRE_LENGTH/6);
		mappoint[x][y].int_4++;
		if (mappoint[x][y].int_4>AFTER_FIRE_LENGTH)
			do_bulldoze_area(CST_GREEN,x,y);
		else if (mappoint[x][y].int_4>(3*AFTER_FIRE_LENGTH)/4)
			mappointtype[x][y]=CST_FIRE_DONE4;
		else if (mappoint[x][y].int_4>(2*AFTER_FIRE_LENGTH)/4)
			mappointtype[x][y]=CST_FIRE_DONE3;
		else if (mappoint[x][y].int_4>(AFTER_FIRE_LENGTH)/4)
			mappointtype[x][y]=CST_FIRE_DONE2;
		else
			mappointtype[x][y]=CST_FIRE_DONE1;
		return;
	}
	mappoint[x][y].int_2++;
	if (real_time>mappoint[x][y].int_1)
	{
		mappoint[x][y].int_1=real_time+FIRE_ANIMATION_SPEED;
		if (mappointtype[x][y]==CST_FIRE_1)
			mappointtype[x][y]=CST_FIRE_2;
		else if (mappointtype[x][y]==CST_FIRE_2)
			mappointtype[x][y]=CST_FIRE_3;
		else if (mappointtype[x][y]==CST_FIRE_3)
			mappointtype[x][y]=CST_FIRE_4;
		else if (mappointtype[x][y]==CST_FIRE_4)
			mappointtype[x][y]=CST_FIRE_5;
		else if (mappointtype[x][y]==CST_FIRE_5)
			mappointtype[x][y]=CST_FIRE_1;
	}
	if (mappoint[x][y].int_3==-1)
	{	
		if ((rand()%FIRE_DAYS_PER_SPREAD)==1)
		{
			i=rand()%4;
			switch (i)
			{
				case (0): do_random_fire(x-1,y,0); break;
				case (1): do_random_fire(x,y-1,0); break;
				case (2): do_random_fire(x+1,y,0); break;
				case (3): do_random_fire(x,y+1,0); break;
			}
		}
	}
	else if (mappoint[x][y].int_3==0) // check here 'cos we can wait 
				    // in the ok box for ever.
		mappoint[x][y].int_3=real_time+15000; // 15 secs seem fair
	else if (real_time>=mappoint[x][y].int_3)
		mappoint[x][y].int_3=-1;
}

// spiral round from startx,starty until we hit something of group group.
// return the x y coords encoded as x+y*WORLD_SIDE_LEN
// return -1 if we don't find one.

int spiral_find_group(int startx, int starty,int group)
{
	int i,j,x,y;
	x=startx;
	y=starty;
// let's just do a complete spiral for now, work out the bounds later
	for (i=1;i<(WORLD_SIDE_LEN+WORLD_SIDE_LEN);i++)
	{
		for (j=0;j<i;j++)
		{
			x--;
			if (x>0 && x<WORLD_SIDE_LEN && y>0 && y<WORLD_SIDE_LEN)
				if (main_types[mappointtype[x][y]].group
						==group)
					return(x+y*WORLD_SIDE_LEN);
		}
		for (j=0;j<i;j++)
		{
			y--;
			if (x>0 && x<WORLD_SIDE_LEN && y>0 && y<WORLD_SIDE_LEN)
				if (main_types[mappointtype[x][y]].group
						==group)
					return(x+y*WORLD_SIDE_LEN);
		}
		i++;
		for (j=0;j<i;j++)
		{
			x++;
			if (x>0 && x<WORLD_SIDE_LEN && y>0 && y<WORLD_SIDE_LEN)
				if (main_types[mappointtype[x][y]].group
						==group)
					return(x+y*WORLD_SIDE_LEN);
		}
		for (j=0;j<i;j++)
		{
			y++;
			if (x>0 && x<WORLD_SIDE_LEN && y>0 && y<WORLD_SIDE_LEN)
				if (main_types[mappointtype[x][y]].group
						==group)
					return(x+y*WORLD_SIDE_LEN);
		}
	}
	return(-1);
}



// spiral round from startx,starty until we hit a 2x2 space.
// return the x y coords encoded as x+y*WORLD_SIDE_LEN
// return -1 if we don't find one.

int spiral_find_2x2(int startx, int starty)
{
	int i,j,x,y;
	x=startx;
	y=starty;
// let's just do a complete spiral for now, work out the bounds later
	for (i=1;i<(WORLD_SIDE_LEN+WORLD_SIDE_LEN);i++)
	{
		for (j=0;j<i;j++)
		{
			x--;
			if (x>0 && x<WORLD_SIDE_LEN-1 && y>0 
				&& y<WORLD_SIDE_LEN-1)
				if (mappointtype[x][y]==CST_GREEN
					&& mappointtype[x+1][y]==CST_GREEN
					&& mappointtype[x][y+1]==CST_GREEN
					&& mappointtype[x+1][y+1]==CST_GREEN)
					return(x+y*WORLD_SIDE_LEN);
		}
		for (j=0;j<i;j++)
		{
			y--;
			if (x>0 && x<WORLD_SIDE_LEN-1 && y>0
				&& y<WORLD_SIDE_LEN-1)
				if (mappointtype[x][y]==CST_GREEN
					&& mappointtype[x+1][y]==CST_GREEN
					&& mappointtype[x][y+1]==CST_GREEN
					&& mappointtype[x+1][y+1]==CST_GREEN)
					return(x+y*WORLD_SIDE_LEN);
		}
		i++;
		for (j=0;j<i;j++)
		{
			x++;
			if (x>0 && x<WORLD_SIDE_LEN-1 && y>0 
				&& y<WORLD_SIDE_LEN-1)
				if (mappointtype[x][y]==CST_GREEN
					&& mappointtype[x+1][y]==CST_GREEN
					&& mappointtype[x][y+1]==CST_GREEN
					&& mappointtype[x+1][y+1]==CST_GREEN)
					return(x+y*WORLD_SIDE_LEN);
		}
		for (j=0;j<i;j++)
		{
			y++;
			if (x>0 && x<WORLD_SIDE_LEN && y>0 && y<WORLD_SIDE_LEN)
				if (mappointtype[x][y]==CST_GREEN
					&& mappointtype[x+1][y]==CST_GREEN
					&& mappointtype[x][y+1]==CST_GREEN
					&& mappointtype[x+1][y+1]==CST_GREEN)
					return(x+y*WORLD_SIDE_LEN);
		}
	}
	return(-1);
}

void add_a_shanty(void)
{
	int r,x,y;
	x=rand()%WORLD_SIDE_LEN;
	y=rand()%WORLD_SIDE_LEN;
	if (numof_shanties>0 && rand()%8!=0)
	{
		r=spiral_find_group(x,y,GROUP_SHANTY);
		if (r==-1)
		{
			printf(
	"Looked for a shanty, without any! x=%d y=%d\n",x,y);
			return;
		}
		y=r/WORLD_SIDE_LEN;
		x=r%WORLD_SIDE_LEN;
		r=spiral_find_2x2(x,y);
		if (r==-1)
		{
			printf("Adding a shanty (s), no space for it?!\n");
			return;
		}
		y=r/WORLD_SIDE_LEN;
		x=r%WORLD_SIDE_LEN;
	}
	else
	{
		r=spiral_find_group(x,y,GROUP_MARKET);
		if (r==-1)
			return;  // silently return, we havn't started yet.
		y=r/WORLD_SIDE_LEN;
		x=r%WORLD_SIDE_LEN;
		r=spiral_find_2x2(x,y);
		if (r==-1)
		{
			printf("Adding a shanty (r), no space for it?!\n");
			return;
		}
		y=r/WORLD_SIDE_LEN;
		x=r%WORLD_SIDE_LEN;
	}
	mappointtype[x][y]=CST_SHANTY;
        mappointtype[x+1][y]=CST_USED;
        set_mappoint_ints(x,y,x+1,y);
        mappointtype[x][y+1]=CST_USED;
        set_mappoint_ints(x,y,x,y+1);
        mappointtype[x+1][y+1]=CST_USED;
        set_mappoint_ints(x,y,x+1,y+1);
	numof_shanties++;
}

void remove_a_shanty(void)
{
	int x,y,r;
	if (numof_shanties<=0)
		return;
	x=rand()%WORLD_SIDE_LEN;
	y=rand()%WORLD_SIDE_LEN;
	r=spiral_find_group(x,y,GROUP_SHANTY);
	if (r==-1)
	{
		printf("Can't find a shanty to remove! ?\n");
		return;
	}
	y=r/WORLD_SIDE_LEN;
	x=r%WORLD_SIDE_LEN;
	fire_area(x,y);
// done in fire area	numof_shanties--;
// now put the fire out
	mappoint[x][y].int_2=FIRE_LENGTH+1;
	mappoint[x+1][y].int_2=FIRE_LENGTH+1;
	mappoint[x][y+1].int_2=FIRE_LENGTH+1;
	mappoint[x+1][y+1].int_2=FIRE_LENGTH+1;
}

void update_shanty(void)
{
	int i,pp;
	pp=people_pool-(COMMUNE_POP*numof_communes);
	i=(pp-SHANTY_MIN_PP)/SHANTY_POP;
	if (i>numof_shanties)
		add_a_shanty();  //                   vv-- schmitt trigger
	else if (numof_shanties>0 && i<(numof_shanties-1))
		remove_a_shanty();
	else if (numof_shanties>0 && rand()%100==10)
		remove_a_shanty();  // randomly close some down.
}

void do_shanty(int x,int y)
{  // just steal some stuff and make pollution.
	get_food(x,y,SHANTY_GET_FOOD);
	if (get_goods(x,y,SHANTY_GET_GOODS)!=0)
		if ((goods_tax-=SHANTY_GET_GOODS*2)<0)
			goods_tax=0;
	get_ore(x,y,SHANTY_GET_ORE);
	get_steel(x,y,SHANTY_GET_STEEL);
	if (get_jobs(x,y,SHANTY_GET_JOBS)!=0)
		if ((income_tax-=SHANTY_GET_JOBS*2)<0)
			income_tax=0;
	if (get_coal(x,y,SHANTY_GET_COAL)!=0)
		if ((coal_tax-=SHANTY_GET_COAL*2)<0)
			coal_tax=0;
	if ((total_time&1)==0)
		mappointpol[x][y]++;
	else
		mappointpol[x+1][y+1]++;
}

void do_tip(int x,int y)
{
// int_1 is the amount of waste on the site.
// int_2 if the amount that has flowed in so far this month
// int_3 is the amount stored last month.
	int i;
	if (mappointtype[x][y]==CST_TIP_8)
		return;
// just grab as much as we can from transport
        if (x>0 && (mappoint[x-1][y].flags & FLAG_IS_TRANSPORT)!=0)
	{
		i=mappoint[x-1][y].int_7/10;
		mappoint[x][y].int_1+=i;
		mappoint[x][y].int_2+=i;
		mappoint[x-1][y].int_7-=i*10;
		sust_dig_ore_coal_tip_flag=0;
	}
        if (y>0 && (mappoint[x][y-1].flags & FLAG_IS_TRANSPORT)!=0)
        {
		i=mappoint[x][y-1].int_7/10;
                mappoint[x][y].int_1+=i;
		mappoint[x][y].int_2+=i;
                mappoint[x][y-1].int_7-=i*10;
		sust_dig_ore_coal_tip_flag=0;
        }
// now choose an icon.
	if ((total_time%NUMOF_DAYS_IN_MONTH)==0)
	{
		i=(mappoint[x][y].int_1*7)/MAX_WASTE_AT_TIP;
		if (mappoint[x][y].int_1>0)
			i++;
		switch (i)
		{
			case(0): mappointtype[x][y]=CST_TIP_0; break;
			case(1): mappointtype[x][y]=CST_TIP_1; break;
			case(2): mappointtype[x][y]=CST_TIP_2; break;
			case(3): mappointtype[x][y]=CST_TIP_3; break;
			case(4): mappointtype[x][y]=CST_TIP_4; break;
			case(5): mappointtype[x][y]=CST_TIP_5; break;
			case(6): mappointtype[x][y]=CST_TIP_6; break;
			case(7): mappointtype[x][y]=CST_TIP_7; break;
			case(8): mappointtype[x][y]=CST_TIP_8;
					mappoint[x][y].int_2=0; break;

		}
		mappoint[x][y].int_3=mappoint[x][y].int_2;
		mappoint[x][y].int_2=0;
	}
}

