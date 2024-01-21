// main.cxx    part of lin-city
// Copyright (c) I J Peters 1995,1996.  Please read the file 'COPYRIGHT'.


#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#ifdef LC_X11
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include "lcx11.h"
#else
#include <vga.h>
#include <vgagl.h>
#include <vgamouse.h>
#endif

// this is for OS/2 - RVI
#ifdef __EMX__
#include <sys/select.h>
#define LIBDIR "."
#endif

#include <time.h>
#include <dirent.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/time.h>
//#include <sys/times.h>
#include <ctype.h>
#include "lctypes.h"
#include "lin-city.h"
#include "main.h"

unsigned char start_font1[2048];
unsigned char start_font2[4096];
unsigned char start_font3[4096];

void dump_screen(void);
void debug_stuff(void);

#ifdef CS_PROFILE
int prof_countdown=PROFILE_COUNTDOWN;
#endif

main(int argc,char *argv[])
{
	int obx=0,oby=0;
#ifdef LC_X11
	int x,y;
#else
	int q;
#endif
	main_screen_originx=1;
	main_screen_originy=1;
	given_scene[0]=0;
#ifdef LC_X11
	XEvent xev;
#endif
	quit_flag=load_flag=save_flag=cheat_flag=monument_bul_flag
		=river_bul_flag=no_init_help=0;
	kmouse_val=8;
	check_for_old_save_dir();
	check_savedir();
	init_path_strings();
#ifndef CS_PROFILE
#ifdef SEED_RAND
	srand(time(0));
#endif
#endif

#ifndef LC_X11
	lincityrc();
#endif

	order_select_buttons();
#ifdef LC_X11
        char *geometry=NULL;
	borderx=BORDERX;
	bordery=BORDERY;
        parse_xargs(argc,argv,&geometry);
// printf("Got past parse args\n");
        Create_Window(geometry);
/// printf("Got past create_window, proper\n");
///        XSync(display.dpy,FALSE);

//        while (XPending(display.dpy)==0);
//        XNextEvent(display.dpy, &xev);
//        HandleEvent(&xev);

///        printf("Got past create window\n");
	pirate_cursor=XCreateFontCursor(display.dpy,XC_pirate);
#else
	parse_args(argc,argv);
	vga_init();
	q=vga_setmode(G640x480x256);
	gl_setcontextvga(G640x480x256);
//	printf("vga_setmode returned %d\n",q);
#endif

	load_start_image();
#ifdef LC_X11
	x_key_value=0;
	XDestroyWindow(display.dpy,display.win);
        do
        {
                while (XPending(display.dpy)==0);
                XNextEvent(display.dpy, &xev);
                HandleEvent(&xev);
        } while (xev.type!=UnmapNotify);

	Create_Window(geometry);
//        XSync(display.dpy,FALSE);
	setcustompalette();
	suppress_next_expose=1;
	do
	{
	        while (XPending(display.dpy)==0);
	        XNextEvent(display.dpy, &xev);
	        HandleEvent(&xev);
	} while (xev.type!=MapNotify);

	for (y=0;y<(480+BORDERY*2);y++)
		for (x=0;x<(640+BORDERX*2);x++)
			pixmap[x+y*(640+BORDERX*2)]=0;
#else
	setcustompalette();
#endif 

#ifdef LC_X11
	XSync(display.dpy,FALSE);
        while (XPending(display.dpy)==0);
        XNextEvent(display.dpy, &xev);
        HandleEvent(&xev);
#endif
	initfont();


#ifdef LC_X11
	obx=borderx;
	oby=bordery;
	borderx=0;
	bordery=0;
#endif
	Fgl_fillbox(0,0,640+obx*2,480+oby*2,TEXT_BG_COLOUR); //background
#ifdef LC_X11
	borderx=obx;
	bordery=oby;
#endif
	prog_box("Loading the game",1);
#ifndef LC_X11
	mouse_setup();
#endif
	init_types();
//	printf("Got past init_types\n");
	prog_box("",95);
	clear_data();
	prog_box("",100);
#ifdef USE_PIXMAPS
	prog_box("Creating pixmaps",1);
	init_pixmaps();
	prog_box("",100);
#endif
//	prog_box("",100);
//	printf("Starting screen_setup\n");
	draw_normal_mouse(1,1);
	screen_setup();
	draw_sustainable_window();
//	printf("Done - screen_setup\n");
	coal_reserve_setup();
	setup_river();
	ore_reserve_setup();
	people_pool=100;
	test();
#ifndef LC_X11
	mouse_close();
	vga_setmode(TEXT);
#endif

	print_results();

	exit(0);
}

void test(void)
{
	int q,next_time_step=0,real_quit_flag=0,u;
#ifndef LC_X11
	int w;
#endif
	if (gettimeofday(&lc_timeval,0)!=0)
		do_error("Can't get timeofday");
	real_start_time=lc_timeval.tv_sec;
//	printf("Calling draw_mini_screen\n");
	draw_mini_screen();
	if (no_init_help==0)
	{
		block_help_exit=1;
		help_flag=1;
		activate_help("opening.hlp");
	}
//	printf("Starting main loop\n");
	selected_type=CST_TRACK_LR;
	selected_type_cost=GROUP_TRACK_COST;
	old_selected_button=sbut[7];
	highlight_select_button(sbut[7]);  // 7 is track.  Watch out though!
	refresh_main_screen();
#ifdef CS_PROFILE
	select_fast();
#else
	select_medium();   // speed
#endif
	do
	{
//printf("Help flag=%d\n",help_flag);
		if (reset_mouse_flag!=0)
		{
			cs_mouse_handler(0,0,0);
			reset_mouse_flag=0;
			must_release_button=1;
		}
		get_real_time();
#ifdef LC_X11
		call_event();
		q=x_key_value;
#else
		q=vga_getkey();
#endif
//		debug_writeval(cs_mouse_button);
		if (q==' ' || q==10 || q==13 || q==127)
		{
#ifdef LC_X11
			x_key_value=0;
#endif
			if (q==127)
				cs_mouse_handler(MOUSE_RIGHTBUTTON,0,0);
			else
				cs_mouse_handler(MOUSE_LEFTBUTTON,0,0);
//			cs_mouse_button=0;
			cs_mouse_handler(0,0,0); // ouch!
//  No, the following line is not really there - you're seeing things :)
//  This is a good time to go to bed.
//  OK... I'll remove it as soon as I can
			goto this_is_not_really_here;
		}
#ifndef LC_X11
		if (q==91)
		{
			w=vga_getkey();
			switch (w) 
			{
				case ('A'): cs_mouse_handler(0,0,-kmouse_val);
						break;
				case ('B'): cs_mouse_handler(0,0,kmouse_val);
						break;
				case ('C'): cs_mouse_handler(0,kmouse_val,0);
						break;
				case ('D'): cs_mouse_handler(0,-kmouse_val,0);
						break;
			}
			continue;
		}
#endif
		if (q=='p')
		{
			select_pause();
#ifdef LC_X11
			x_key_value=0;
#endif
		}
		if (market_cb_flag==0 && help_flag==0 && port_cb_flag==0)
		{
			if ((--cs_mouse_button_delay)<0)
				cs_mouse_button_delay=0;
#ifdef LC_X11
			call_event();
#else
			mouse_update();
#endif
this_is_not_really_here:  // :)

// ooch!
			if ((real_time<next_time_step 
		|| pause_flag || mt_flag)
		&& market_cb_flag==0 && help_flag==0 && port_cb_flag==0
		&& save_flag==0 && load_flag==0)
			{
				if ((let_one_through==0 && q!='l' 
					&& q!='s') || mt_flag)
				{
					lc_usleep(1);
//printf(".");
//fflush(stdout);
					continue;
				}
				else
					let_one_through=0;
			}
			if (slow_flag)
				next_time_step=real_time+(SLOW_TIME_FOR_YEAR
					*1000/NUMOF_DAYS_IN_YEAR);
			else if (fast_flag)
				next_time_step=real_time+(FAST_TIME_FOR_YEAR
					*1000/NUMOF_DAYS_IN_YEAR);
			else
				next_time_step=real_time+(MED_TIME_FOR_YEAR
					*1000/NUMOF_DAYS_IN_YEAR);
			do_time_step();
#ifdef CS_PROFILE
	if (--prof_countdown<=0)
		real_quit_flag=1;
#endif
#ifdef MP_SANITY_CHECK
			sanity_check();
#endif
			update_main_screen();
			if ((total_time%NUMOF_DAYS_IN_YEAR)==0)
			{
				sustainability_test();
				time_for_year();
				pollution_deaths_history
					-=pollution_deaths_history/100.0;
				starve_deaths_history
					-=starve_deaths_history/100.0;
				unemployed_history
					-=unemployed_history/100.0;
				u=count_groups(GROUP_UNIVERSITY);
				if (u>0)
				{
					university_intake_rate
				=(count_groups(GROUP_SCHOOL)*20)/u;
					if (university_intake_rate>100)
						university_intake_rate=100;
				}
				else
					university_intake_rate=50;
			}
			if (((total_time%DAYS_BETWEEN_FIRES)==9
				&& tech_level>(GROUP_FIRESTATION_TECH
				*MAX_TECH_LEVEL/1000))
				|| q=='f')
				do_random_fire(-1,-1,1);
			if ((total_time%DAYS_BETWEEN_COVER)==75)
			{
				clear_fire_health_and_cricket_cover();
				do_fire_health_and_cricket_cover();
			}
			if ((total_time%DAYS_BETWEEN_SHANTY)==85
				&& tech_level
				>(GROUP_HEALTH_TECH*MAX_TECH_LEVEL/1000))
			{
				update_shanty();
//				debug_writeval(numof_shanties);
			}
			if (mappoint_stats_flag 
				&& real_time>mappoint_stats_time)
			{
				mappoint_stats_time=real_time+1000;
				mappoint_stats(-1,-1,-1);
			}
			if (mini_screen_flags
				&& real_time>mini_screen_time)
			{
				mini_screen_time=real_time+1000;
				update_mini_screen();			
			}
			print_stats();
			if ((total_time%DAYS_PER_POLLUTION)==3)
				do_pollution();
#ifndef LC_X11
			if (cs_mouse_button==MOUSE_LEFTBUTTON)
				cs_mouse_repeat();
#endif
			if (market_cb_flag)
				draw_market_cb();
			else if (port_cb_flag)  // else- can't have both
				draw_port_cb();
                }
                else
		{
#ifdef LC_X11
			if (market_cb_flag!=0 && market_cb_drawn_flag==0)
				draw_market_cb();
			if (port_cb_flag!=0 && port_cb_drawn_flag==0)
				draw_port_cb();
#endif
			cs_mouse_button_delay=0;
#ifndef LC_X11
                        mouse_update();
#endif
		}
#ifdef DEBUG_KEYS 
		if (q=='o')
		{
#ifdef LC_X11
                        x_key_value=0;
#endif
			if (cheat()!=0)
				people_pool+=100;
		}
		else if (q=='d')
		{
#ifdef LC_X11
                        x_key_value=0;
#endif
			if (cheat()!=0)
				dump_screen();
		}
                else if (q=='D')
                {
#ifdef LC_X11
                        x_key_value=0;
#endif
                        dump_tcore();
                }

		else if (q=='t')
		{
#ifdef LC_X11
                        x_key_value=0;
#endif
			if (cheat()!=0)
				tech_level+=1000;
		}
		else if (q=='T')
		{
#ifdef LC_X11
                        x_key_value=0;
#endif
			if (cheat()!=0)
				tech_level+=10000;
		}
		else if (q=='m')
		{
#ifdef LC_X11
                        x_key_value=0;
#endif
			if (cheat()!=0)
			{
				total_money+=1000000;
				print_total_money();
			}
		}
#endif
		if (q=='l' || load_flag!=0)
		{
#ifdef LC_X11
                        x_key_value=0;
#endif
			if (help_flag==0) // block loading when in help
				do_load_city();
			load_flag=0;
			let_one_through=1;  // if we are paused we need
		}                           // this to redraw the screen
		else if (q=='s' || save_flag!=0)
		{
#ifdef LC_X11
                        x_key_value=0;
#endif
			if (help_flag==0)
				do_save_city();
			save_flag=0;
			let_one_through=1;
		}
 		if (q=='q' || q=='Q' || quit_flag!=0)
		{
#ifdef LC_X11
                        x_key_value=0;
#endif
			if (help_flag!=0)  //  mmm... could do
				;          // this better.
                                       // this means we can't quit in help
			else if (yn_dial_box("Quit The Game?"
				,"If you want to save the game select NO"
				,"here, then click on the save button."
				,"Do you really want to quit?")!=0)
				real_quit_flag=1;
			else
				quit_flag=0;
// printf("Help flag %d   Quit flag %d\n",help_flag,quit_flag);
		}
		if (help_flag!=0)
			lc_usleep(1);
	}
	while ( /* q!='q' && q!='Q' && */ real_quit_flag==0 );
}

void do_error(char *s)
{
#ifdef LC_X11
	HandleError(s,FATAL);
#else
	vga_setmode(TEXT);
	printf("%s\n",s);
	exit(1);
#endif
}

void clear_data(void)
{
	int x,y;
	for (y=0;y<WORLD_SIDE_LEN;y++)
        for (x=0;x<WORLD_SIDE_LEN;x++)
		mappointtype[x][y]=0;
	mouse_hide_count=0;
	coal_survey_done=0;
	suppress_ok_buttons=0;
}

void do_save_city()
{
        char s[200],c;
//        int i;
        hide_mouse();
        Fgl_fillbox(MAIN_WIN_X,MAIN_WIN_Y,MAIN_WIN_W,MAIN_WIN_H
                        ,SAVE_BG_COLOUR);
        Fgl_setfontcolors(SAVE_BG_COLOUR,TEXT_FG_COLOUR);
        Fgl_write(MAIN_WIN_X+100,MAIN_WIN_Y+15,"Save a scene");
        Fgl_write(MAIN_WIN_X+8,MAIN_WIN_Y+35
                ,"Choose the number of the scene you want to save");
	Fgl_write(MAIN_WIN_X+110,MAIN_WIN_Y+210
		,"Press space to cancel.");
        draw_save_dir(SAVE_BG_COLOUR);
#ifdef LC_X11
//        db_flag=1;
        redraw_mouse();
	db_flag=1;
        cs_mouse_handler(0,-1,0);
        cs_mouse_handler(0,1,0);
	do
	{
	        call_event();
                c=x_key_value;
	} while (c==0);
	x_key_value=0;
//	db_flag=0;
//	cs_mouse_handler(0,-1,0);
//	cs_mouse_handler(0,1,0);
#else
        c=getchar();
	redraw_mouse();
#endif
        if (c>'0' && c<='9')
	{
		Fgl_write(MAIN_WIN_X+40,MAIN_WIN_Y+300
			,"Type comment for the saved scene");
		Fgl_write(MAIN_WIN_X+16,MAIN_WIN_Y+310
			,"The comment may be up to 40 characters");
		Fgl_write(MAIN_WIN_X+40,MAIN_WIN_Y+320
			,"and may contain spaces or % . - + ,");
		strcpy(s,&(save_names[c-'0'][2]));
		input_save_filename(s);
		remove_scene(save_names[c-'0']);
		sprintf(save_names[c-'0'],"%d_",c-'0');
		strcat(save_names[c-'0'],s);
		Fgl_fillbox(MAIN_WIN_X+5,MAIN_WIN_Y+300
			,360,30,SAVE_BG_COLOUR);
		Fgl_write(MAIN_WIN_X+70,MAIN_WIN_Y+310
			,"Saving city scene... please wait");
		save_city(save_names[c-'0']);
	}
        db_flag=0;
        cs_mouse_handler(0,-1,0);
        cs_mouse_handler(0,1,0);
	hide_mouse();
        Fgl_setfontcolors(TEXT_BG_COLOUR,TEXT_FG_COLOUR);
        refresh_main_screen();
        redraw_mouse();
}

void remove_scene(char *cname)
{
	char s[200];
        strcpy(s,getenv("HOME"));
        strcat(s,"/");
        strcat(s,LC_SAVE_DIR);
        strcat(s,"/");
        strcat(s,cname);
	remove(s);
}

void save_city(char *cname)
{
	char s[200],s2[200];
	int x,y,z,q,n;
	FILE *ofile;
        strcpy(s,getenv("HOME"));
        strcat(s,"/");
        strcat(s,LC_SAVE_DIR);
        strcat(s,"/");
        strcat(s,cname);
//	strcpy(s2,"gzip -c -9 > ");
	strcpy(s2,"gzip -c > ");
	strcat(s2,s);
	if ((ofile=popen(s2,"w"))==NULL)
	{
		printf("Save pipe <%s> - ",s2);
		do_error("Can't open it!");
	}
	fprintf(ofile,"%d\n",(int)VERSION);
	q=sizeof(struct MAPPOINT);
	prog_box("Saving scene",0);
	check_endian();
	for (x=0;x<WORLD_SIDE_LEN;x++)
	{
		for (y=0;y<WORLD_SIDE_LEN;y++)
		{
			for (z=0;z<q;z++)
			{
				n=*(((unsigned char *)&mappoint[x][y])+z);
				fprintf(ofile,"%d\n",n);
			}
			fprintf(ofile,"%d\n",(int)mappointpol[x][y]);
			fprintf(ofile,"%d\n",(int)mappointtype[x][y]);
		}
		prog_box("",(90*x)/WORLD_SIDE_LEN);
	}
	check_endian();  // we have to put the byte order back.
	fprintf(ofile,"%d\n",main_screen_originx);
	fprintf(ofile,"%d\n",main_screen_originy);
	fprintf(ofile,"%d\n",total_time);
	for (x=0;x<MAX_NUMOF_SUBSTATIONS;x++)
	{
		fprintf(ofile,"%d\n",substationx[x]);
		fprintf(ofile,"%d\n",substationy[x]);
	}
	prog_box("",92);
	fprintf(ofile,"%d\n",numof_substations);
	for (x=0;x<MAX_NUMOF_MARKETS;x++)
	{
		fprintf(ofile,"%d\n",marketx[x]);
		fprintf(ofile,"%d\n",markety[x]);
	}
	prog_box("",94);
	fprintf(ofile,"%d\n",numof_markets);
	fprintf(ofile,"%d\n",people_pool);
	fprintf(ofile,"%d\n",total_money);
	fprintf(ofile,"%d\n",income_tax_rate);
	fprintf(ofile,"%d\n",coal_tax_rate);
	fprintf(ofile,"%d\n",dole_rate);
	fprintf(ofile,"%d\n",transport_cost_rate);
	fprintf(ofile,"%d\n",goods_tax_rate);
	fprintf(ofile,"%d\n",export_tax);
	fprintf(ofile,"%d\n",export_tax_rate);
	fprintf(ofile,"%d\n",import_cost);
	fprintf(ofile,"%d\n",import_cost_rate);
	fprintf(ofile,"%d\n",tech_level);
	fprintf(ofile,"%d\n",tpopulation);
	fprintf(ofile,"%d\n",tstarving_population);
	fprintf(ofile,"%d\n",tunemployed_population);
	fprintf(ofile,"%d\n",waste_goods);
	fprintf(ofile,"%d\n",power_made);
	fprintf(ofile,"%d\n",power_used);
	fprintf(ofile,"%d\n",coal_made);
	fprintf(ofile,"%d\n",coal_used);
	fprintf(ofile,"%d\n",goods_made);
	fprintf(ofile,"%d\n",goods_used);
	fprintf(ofile,"%d\n",ore_made);
	fprintf(ofile,"%d\n",ore_used);
	fprintf(ofile,"%d\n",diff_old_population);
	prog_box("",96);
	for (x=0;x<MAPPOINT_STATS_W;x++)
	{
		fprintf(ofile,"%d\n",monthgraph_pop[x]);
		fprintf(ofile,"%d\n",monthgraph_starve[x]);
		fprintf(ofile,"%d\n",monthgraph_nojobs[x]);
		fprintf(ofile,"%d\n",monthgraph_ppool[x]);
		fprintf(ofile,"%d\n",diffgraph_power[x]);
		fprintf(ofile,"%d\n",diffgraph_coal[x]);
		fprintf(ofile,"%d\n",diffgraph_goods[x]);
		fprintf(ofile,"%d\n",diffgraph_ore[x]);
		fprintf(ofile,"%d\n",diffgraph_population[x]);
	}
	prog_box("",98);
	fprintf(ofile,"%d\n",rockets_launched);
	fprintf(ofile,"%d\n",rockets_launched_success);
	fprintf(ofile,"%d\n",coal_survey_done);
	for (x=0;x<12;x++)
	{
		fprintf(ofile,"%d\n",pbar_pops[x]);
		fprintf(ofile,"%d\n",pbar_techs[x]);
		fprintf(ofile,"%d\n",pbar_foods[x]);
		fprintf(ofile,"%d\n",pbar_jobs[x]);
		fprintf(ofile,"%d\n",pbar_money[x]);
		fprintf(ofile,"%d\n",pbar_coal[x]);
		fprintf(ofile,"%d\n",pbar_goods[x]);
		fprintf(ofile,"%d\n",pbar_ore[x]);
		fprintf(ofile,"%d\n",pbar_steel[x]);
	}
	prog_box("",99);
	fprintf(ofile,"%d\n",pbar_pop_oldtot);
	fprintf(ofile,"%d\n",pbar_pop_olddiff);
	fprintf(ofile,"%d\n",pbar_tech_oldtot);
	fprintf(ofile,"%d\n",pbar_tech_olddiff);
	fprintf(ofile,"%d\n",pbar_food_oldtot);
	fprintf(ofile,"%d\n",pbar_food_olddiff);
        fprintf(ofile,"%d\n",pbar_jobs_oldtot);
        fprintf(ofile,"%d\n",pbar_jobs_olddiff);
        fprintf(ofile,"%d\n",pbar_money_oldtot);
        fprintf(ofile,"%d\n",pbar_money_olddiff);
        fprintf(ofile,"%d\n",pbar_coal_oldtot);
        fprintf(ofile,"%d\n",pbar_coal_olddiff);
        fprintf(ofile,"%d\n",pbar_goods_oldtot);
        fprintf(ofile,"%d\n",pbar_goods_olddiff);
        fprintf(ofile,"%d\n",pbar_ore_oldtot);
        fprintf(ofile,"%d\n",pbar_ore_olddiff);
        fprintf(ofile,"%d\n",pbar_steel_oldtot);
        fprintf(ofile,"%d\n",pbar_steel_olddiff);

	fprintf(ofile,"%d\n",cheat_flag);
        fprintf(ofile,"%d\n",total_pollution_deaths);
        fprintf(ofile,"%f\n",pollution_deaths_history);
        fprintf(ofile,"%d\n",total_starve_deaths);
        fprintf(ofile,"%f\n",starve_deaths_history);
        fprintf(ofile,"%d\n",total_unemployed_years);
        fprintf(ofile,"%f\n",unemployed_history);
	fprintf(ofile,"%d\n",max_pop_ever);
	fprintf(ofile,"%d\n",total_evacuated);
	fprintf(ofile,"%d\n",total_births);
	for (x=0;x<NUMOF_SELECT_BUTTONS;x++)
		fprintf(ofile,"%d\n",select_button_help_flag[x]);
	fprintf(ofile,"%d\n",0);  // dummy values
	fprintf(ofile,"%d\n",0);  // backward compatibility
	if (strlen(given_scene)>1)
		fprintf(ofile,"%s\n",given_scene);
	else
		fprintf(ofile,"dummy\n");  // 1
	fprintf(ofile,"%d\n",highest_tech_level);  // 2
	fprintf(ofile,"sust %d %d %d %d %d %d %d %d %d %d\n"
		,sust_dig_ore_coal_count,sust_port_count
		,sust_old_money_count,sust_old_population_count
		,sust_old_tech_count,sust_fire_count
		,sust_old_money,sust_old_population,sust_old_tech
		,sustain_flag);  // 3
	fprintf(ofile,"dummy\n");  // 4
	fprintf(ofile,"dummy\n");  // 5
	fprintf(ofile,"dummy\n");  // 6
	fprintf(ofile,"dummy\n");  // 7
	fprintf(ofile,"dummy\n");  // 8
	fprintf(ofile,"dummy\n");  // 9
	fprintf(ofile,"dummy\n");  // 10
	pclose(ofile);
	prog_box("",100);
}

void do_load_city(void)
{
	char c;
//	int i;
	hide_mouse();
	Fgl_fillbox(MAIN_WIN_X,MAIN_WIN_Y,MAIN_WIN_W,MAIN_WIN_H
			,LOAD_BG_COLOUR);
	Fgl_setfontcolors(LOAD_BG_COLOUR,TEXT_FG_COLOUR);
	Fgl_write(MAIN_WIN_X+140,MAIN_WIN_Y+15,"Load a file");
	Fgl_write(MAIN_WIN_X+40,MAIN_WIN_Y+35
		,"Choose the number of the scene you want");
	Fgl_write(MAIN_WIN_X+40,MAIN_WIN_Y+50
		,"Entries coloured red are either not there,");
	Fgl_write(MAIN_WIN_X+44,MAIN_WIN_Y+60
		,"or they are from an earlier version, they");
	Fgl_write(MAIN_WIN_X+110,MAIN_WIN_Y+70
		,"might not load properly.");
	Fgl_write(MAIN_WIN_X+110,MAIN_WIN_Y+210
		,"Press space to cancel.");
	draw_save_dir(LOAD_BG_COLOUR);
    do
    {
#ifdef LC_X11
	db_flag=1;
	redraw_mouse();
	cs_mouse_handler(0,-1,0);
	cs_mouse_handler(0,1,0);
	do
	{
	        call_event();
                c=x_key_value;
	} while (c==0);
	x_key_value=0;
#else
	c=getchar();
//	redraw_mouse();
#endif
	if (c>'0' && c<='9')
		if (strlen(save_names[c-'0'])<1)
		{
			redraw_mouse();
			if (yn_dial_box("No scene."
				,"There is no save scene with this number."
				,"Do you want to"
				,"try again?")!=0)
				c=0;
			else
				c=' ';
			hide_mouse();
		}
    } while ((c<='0' || c>'9') && c!=' ');
	redraw_mouse();
//	db_flag=1;
//	cs_mouse_handler(0,-1,0);
	if (c>'0' && c<='9')
	{
		if (yn_dial_box("Loading Scene"
				,"Do you want to load the scene"
				,save_names[c-'0']
				,"and forget the current game?")!=0)
		{
			Fgl_write(MAIN_WIN_X+70,MAIN_WIN_Y+310
				,"Loading scene...  please wait");
			load_city(save_names[c-'0']);
		}
	}
        db_flag=0;
        cs_mouse_handler(0,-1,0);
        cs_mouse_handler(0,1,0);
	hide_mouse();
	Fgl_setfontcolors(TEXT_BG_COLOUR,TEXT_FG_COLOUR);
	refresh_main_screen();
	suppress_ok_buttons=1;
	update_select_buttons();
	suppress_ok_buttons=0;
	redraw_mouse();
}

void load_op_city(char *s)
{
	load_city(s);
	strcpy(given_scene,s);
        db_flag=0;
        cs_mouse_handler(0,-1,0);
        cs_mouse_handler(0,1,0);
//        hide_mouse();
        Fgl_setfontcolors(TEXT_BG_COLOUR,TEXT_FG_COLOUR);
        refresh_main_screen();
        suppress_ok_buttons=1;
        update_select_buttons();
        suppress_ok_buttons=0;
//        redraw_mouse();
}

void load_city(char *cname)
{
	char s[200],s2[200];
        int x,y,z,q,n,v,i;
        FILE *ofile;
	if (cname[1]=='_')
	{
        	strcpy(s,getenv("HOME"));
        	strcat(s,"/");
        	strcat(s,LC_SAVE_DIR);
		strcat(s,"/");
	}
	else
	{
		strcpy(s,LIBDIR);
		strcat(s,"/opening/");
	}
	strcat(s,cname);
	strcpy(s2,"gzip -d -c ");
	strcat(s2,s);
        if ((ofile=popen(s2,"r"))==NULL)
        {
                printf("Load pipe <%s> - ",s2);
                do_error("Can't open it!");
        }
        fscanf(ofile,"%d",&v);
	if (v<MIN_LOAD_VERSION)
	{
		ok_dial_box("too-old.mes",BAD,0L);
		pclose(ofile);
		return;
	}
//		do_error("Sorry wrong version of load file");
	unprint_cheat();
        q=sizeof(struct MAPPOINT);
	if (q!=40)
	{
//		char sq[100];
		sprintf(s," %d %d ",sizeof(int),sizeof(unsigned short));
		ok_dial_box("wrong-mpsize.mes",BAD,s);
	}

	prog_box("Loading scene",0);

        for (x=0;x<WORLD_SIDE_LEN;x++)
	{
                for (y=0;y<WORLD_SIDE_LEN;y++)
		{
//printf("x=%d y=%d  ",x,y);
                        for (z=0;z<q;z++)
                        {
                                fscanf(ofile,"%d",&n);
//printf("n=%d ",n);
				*(((unsigned char *)&mappoint[x][y])+z)=n;
                        }
			fscanf(ofile,"%d",&n);
			mappointpol[x][y]=(unsigned short)n;
			fscanf(ofile,"%d",&n);
			mappointtype[x][y]=(unsigned short)n;

//if (mappointtype[x][y]==CST_USED)
//{
//	printf("px=%d py=%d\n",mappoint[x][y].int_1,mappoint[x][y].int_2);
//	if (mappoint[x][y].int_1<0 || mappoint[x][y].int_1>255
//		|| mappoint[x][y].int_2<0 || mappoint[x][y].int_2>255)
//		exit(1);
//}
//else
//	printf(" Type=%d\n",mappointtype[x][y]);
		if (((93*x)/WORLD_SIDE_LEN)%3==0)
			prog_box("",(93*x)/WORLD_SIDE_LEN);
		}
	}
	check_endian();
	fscanf(ofile,"%d",&main_screen_originx);
	fscanf(ofile,"%d",&main_screen_originy);
        fscanf(ofile,"%d",&total_time);
	if (v<=MM_MS_C_VER)
		i=OLD_MAX_NUMOF_SUBSTATIONS;
	else
		i=MAX_NUMOF_SUBSTATIONS;
        for (x=0;x<i;x++)
        {
                fscanf(ofile,"%d",&substationx[x]);
                fscanf(ofile,"%d",&substationy[x]);
        }
	prog_box("",92);
        fscanf(ofile,"%d",&numof_substations);
	if (v<=MM_MS_C_VER)
		i=OLD_MAX_NUMOF_MARKETS;
	else
		i=MAX_NUMOF_MARKETS;
        for (x=0;x<i;x++)
        {
                fscanf(ofile,"%d",&marketx[x]);
                fscanf(ofile,"%d",&markety[x]);
        }
	prog_box("",94);
        fscanf(ofile,"%d",&numof_markets);
	fscanf(ofile,"%d",&people_pool);
	fscanf(ofile,"%d",&total_money);
	fscanf(ofile,"%d",&income_tax_rate);
	fscanf(ofile,"%d",&coal_tax_rate);
	fscanf(ofile,"%d",&dole_rate);
	fscanf(ofile,"%d",&transport_cost_rate);
	fscanf(ofile,"%d",&goods_tax_rate);
	fscanf(ofile,"%d",&export_tax);
	fscanf(ofile,"%d",&export_tax_rate);
	fscanf(ofile,"%d",&import_cost);
	fscanf(ofile,"%d",&import_cost_rate);
	fscanf(ofile,"%d",&tech_level);
	fscanf(ofile,"%d",&tpopulation);
	fscanf(ofile,"%d",&tstarving_population);
	fscanf(ofile,"%d",&tunemployed_population);
	fscanf(ofile,"%d",&waste_goods);
	fscanf(ofile,"%d",&power_made);
	fscanf(ofile,"%d",&power_used);
	fscanf(ofile,"%d",&coal_made);
	fscanf(ofile,"%d",&coal_used);
	fscanf(ofile,"%d",&goods_made);
	fscanf(ofile,"%d",&goods_used);
	fscanf(ofile,"%d",&ore_made);
	fscanf(ofile,"%d",&ore_used);
	fscanf(ofile,"%d",&diff_old_population);
	prog_box("",96);
	for (x=0;x<MAPPOINT_STATS_W;x++)
	{
		fscanf(ofile,"%d",&monthgraph_pop[x]);
		fscanf(ofile,"%d",&monthgraph_starve[x]);
		fscanf(ofile,"%d",&monthgraph_nojobs[x]);
		fscanf(ofile,"%d",&monthgraph_ppool[x]);
		fscanf(ofile,"%d",&diffgraph_power[x]);
		fscanf(ofile,"%d",&diffgraph_coal[x]);
		fscanf(ofile,"%d",&diffgraph_goods[x]);
		fscanf(ofile,"%d",&diffgraph_ore[x]);
		fscanf(ofile,"%d",&diffgraph_population[x]);
	} 
	prog_box("",98);
	fscanf(ofile,"%d",&rockets_launched);
	fscanf(ofile,"%d",&rockets_launched_success);
	fscanf(ofile,"%d",&coal_survey_done);
        for (x=0;x<12;x++)
        {
                fscanf(ofile,"%d",&pbar_pops[x]);
                fscanf(ofile,"%d",&pbar_techs[x]);
                fscanf(ofile,"%d",&pbar_foods[x]);
		fscanf(ofile,"%d",&pbar_jobs[x]);
		fscanf(ofile,"%d",&pbar_money[x]);
		fscanf(ofile,"%d",&pbar_coal[x]);
		fscanf(ofile,"%d",&pbar_goods[x]);
		fscanf(ofile,"%d",&pbar_ore[x]);
		fscanf(ofile,"%d",&pbar_steel[x]);

        }
	prog_box("",99);
        fscanf(ofile,"%d",&pbar_pop_oldtot);
        fscanf(ofile,"%d",&pbar_pop_olddiff);
        fscanf(ofile,"%d",&pbar_tech_oldtot);
        fscanf(ofile,"%d",&pbar_tech_olddiff);
        fscanf(ofile,"%d",&pbar_food_oldtot);
        fscanf(ofile,"%d",&pbar_food_olddiff);
        fscanf(ofile,"%d",&pbar_jobs_oldtot);
        fscanf(ofile,"%d",&pbar_jobs_olddiff);
        fscanf(ofile,"%d",&pbar_money_oldtot);
        fscanf(ofile,"%d",&pbar_money_olddiff);
        fscanf(ofile,"%d",&pbar_coal_oldtot);
        fscanf(ofile,"%d",&pbar_coal_olddiff);
        fscanf(ofile,"%d",&pbar_goods_oldtot);
        fscanf(ofile,"%d",&pbar_goods_olddiff);
        fscanf(ofile,"%d",&pbar_ore_oldtot);
        fscanf(ofile,"%d",&pbar_ore_olddiff);
        fscanf(ofile,"%d",&pbar_steel_oldtot);
        fscanf(ofile,"%d",&pbar_steel_olddiff);
pbar_money_olddiff=0;
	fscanf(ofile,"%d",&cheat_flag);
	fscanf(ofile,"%d",&total_pollution_deaths);
	fscanf(ofile,"%f",&pollution_deaths_history);
	fscanf(ofile,"%d",&total_starve_deaths);
	fscanf(ofile,"%f",&starve_deaths_history);
	fscanf(ofile,"%d",&total_unemployed_years);
	fscanf(ofile,"%f",&unemployed_history);
	fscanf(ofile,"%d",&max_pop_ever);
	fscanf(ofile,"%d",&total_evacuated);
	fscanf(ofile,"%d",&total_births);
        for (x=0;x<NUMOF_SELECT_BUTTONS;x++)
                fscanf(ofile,"%d",&(select_button_help_flag[x]));
	fscanf(ofile,"%d",&x); // just dummy reads
	fscanf(ofile,"%d",&x); // for backwards compatibility.
// 10 dummy strings, for missed out things, have been put in save.
// Input from this point uses them.
	fscanf(ofile,"%s",given_scene);
	if (strncmp(given_scene,"dummy",5)==0 || strlen(given_scene)<3)
		given_scene[0]=0;
	fscanf(ofile,"%s",s);
	if (strncmp(given_scene,"dummy",5)!=0)
		sscanf(s,"%d",&highest_tech_level);
	else
		highest_tech_level=0;
	fgets(s,80,ofile); // this is the CR
	fgets(s,80,ofile);
        if (sscanf(s,"sust %d %d %d %d %d %d %d %d %d %d"
                ,&sust_dig_ore_coal_count,&sust_port_count
                ,&sust_old_money_count,&sust_old_population_count
                ,&sust_old_tech_count,&sust_fire_count
                ,&sust_old_money,&sust_old_population,&sust_old_tech
		,&sustain_flag)==10)
	{
		sust_dig_ore_coal_tip_flag=sust_port_flag=1;
		draw_sustainable_window();
	}
	else
                sustain_flag=sust_dig_ore_coal_count=sust_port_count
                =sust_old_money_count=sust_old_population_count
                =sust_old_tech_count=sust_fire_count
                =sust_old_money=sust_old_population=sust_old_tech=0;
	pclose(ofile);
	numof_shanties=count_groups(GROUP_SHANTY);
	numof_communes=count_groups(GROUP_COMMUNE);
	prog_box("",100);
	if (cheat_flag!=0)
		print_cheat();
// clear whatever was in the monthgraph box.
        Fgl_fillbox(MONTHGRAPH_X,MONTHGRAPH_Y,MONTHGRAPH_W+1
                ,MONTHGRAPH_H+1,GRAPHS_B_COLOUR);
// set up the university intake.
	x=count_groups(GROUP_UNIVERSITY);
	if (x>0)
	{
		university_intake_rate
			=(count_groups(GROUP_SCHOOL)*20)/x;
		if (university_intake_rate>100)
			university_intake_rate=100;
	}
	else
	university_intake_rate=50;


	reset_animation_times();
}

void reset_animation_times(void)
{
	int x,y;
	for (y=0;y<WORLD_SIDE_LEN;y++)
	for (x=0;x<WORLD_SIDE_LEN;x++)
	{
		if (main_types[mappointtype[x][y]].group==GROUP_RESIDENCE)
			mappoint[x][y].int_3=0;
		else if (main_types[mappointtype[x][y]].group
			==GROUP_WINDMILL)
			mappoint[x][y].int_4=0;
		else if (main_types[mappointtype[x][y]].group
			==GROUP_BLACKSMITH)
			mappoint[x][y].int_4=0;
		else if (main_types[mappointtype[x][y]].group
			==GROUP_MILL)
			mappoint[x][y].int_4=0;
		else if (main_types[mappointtype[x][y]].group
			==GROUP_POTTERY)
			mappoint[x][y].int_4=0;
		else if (main_types[mappointtype[x][y]].group
			==GROUP_CRICKET)
			mappoint[x][y].int_4=0;
                else if (main_types[mappointtype[x][y]].group
                        ==GROUP_FIRESTATION)
                        mappoint[x][y].int_4=0;
		else if (main_types[mappointtype[x][y]].group
			==GROUP_FIRE)
		{
			mappoint[x][y].int_1=0;
			mappoint[x][y].int_3=0;
		}
		else if (main_types[mappointtype[x][y]].group
			==GROUP_COMMUNE)
			mappoint[x][y].int_1=0;
		else if (main_types[mappointtype[x][y]].group
			==GROUP_ROCKET)
			mappoint[x][y].int_5=0;
                else if (main_types[mappointtype[x][y]].group
                        ==GROUP_INDUSTRY_H)
                        mappoint[x][y].int_6=0;
                else if (main_types[mappointtype[x][y]].group
                        ==GROUP_INDUSTRY_L)
                        mappoint[x][y].int_7=0;

	}
}


void coal_reserve_setup(void)
{
	int i,j,x,y,xx,yy;
	for (i=0;i<NUMOF_COAL_RESERVES/5;i++)
	{
		x=(rand()%(WORLD_SIDE_LEN-12))+6;
		y=(rand()%(WORLD_SIDE_LEN-10))+6;
		do
		{
			xx=(rand()%3)-1;
			yy=(rand()%3)-1;
		} while (xx==0 && yy==0);
		for (j=0;j<5;j++)
		{
			mappoint[x][y].coal_reserve
				+=rand()%COAL_RESERVE_SIZE;
			x+=xx;
			y+=yy;
		}
	}
}


void ore_reserve_setup(void)
{
	int x,y;
	for (y=0;y<WORLD_SIDE_LEN;y++)
		for (x=0;x<WORLD_SIDE_LEN;x++)
			mappoint[x][y].ore_reserve=ORE_RESERVE;
}


void dump_screen(void)
{
#ifndef LC_X11
	int x,y,r,g,b;
	FILE *outf;
	if ((outf=fopen("screendump.raw","wb"))==NULL)
		do_error("Can't open screencump.raw");
	for (y=0;y<480;y++)
	for (x=0;x<640;x++)
	{
		gl_getpixelrgb(x,y,&r,&g,&b);
		fputc(r,outf);
		fputc(g,outf);
		fputc(b,outf);
	}
	fclose(outf);
#endif
}

void setup_river(void)
{
	int x,y,i,j;
	x=WORLD_SIDE_LEN/2;
	y=WORLD_SIDE_LEN-1;
	i=(rand()%12)+6;
	for (j=0;j<i;j++)
	{
		x+=(rand()%3)-1;
		mappointtype[x][y]=CST_WATER;
		mappoint[x][y].flags|=FLAG_IS_RIVER;
		mappointtype[x+1][y]=CST_WATER;
		mappoint[x+1][y].flags|=FLAG_IS_RIVER;
		mappointtype[x-1][y]=CST_WATER;
		mappoint[x-1][y].flags|=FLAG_IS_RIVER;
		y--;
	}
	mappointtype[x][y]=CST_WATER;
	mappoint[x][y].flags|=FLAG_IS_RIVER;
	mappointtype[x+1][y]=CST_WATER;
	mappoint[x+1][y].flags|=FLAG_IS_RIVER;
	mappointtype[x-1][y]=CST_WATER;
	mappoint[x-1][y].flags|=FLAG_IS_RIVER;
	setup_river2(x-1,y,-1);   // left tributary
	setup_river2(x+1,y,1);    // right tributary
	printf("River setup done\n");
}

void setup_river2(int x,int y,int d)
{
	int i,j,r;
	i=(rand()%55)+15;
	for (j=0;j<i;j++)
	{
		r=(rand()%3)-1+(d*(rand()%3));
		if (r<-1)
			r=-1;
		else if (r>1)
			r=1;
		x+=r;
		if (mappointtype[x+(d+d)][y]!=0
			|| mappointtype[x+(d+d+d)][y]!=0)
			return;
		if (x>5 && x<WORLD_SIDE_LEN-5)
		{
			mappointtype[x][y]=CST_WATER;
			mappoint[x][y].flags|=FLAG_IS_RIVER;
			mappointtype[x+d][y]=CST_WATER;
			mappoint[x+d][y].flags|=FLAG_IS_RIVER;
		}
		if (--y <10 || x<5 || x>WORLD_SIDE_LEN-5)
			break;
	}
	if (y>20)
	{
		if (x>5 && x<WORLD_SIDE_LEN-5)
			setup_river2(x,y,-1);
		if (x>5 && x<WORLD_SIDE_LEN-5)
			setup_river2(x,y,1);
	}
}

void check_savedir(void)
{
	char s[100],c;
	DIR *dp;
	strcpy(lc_save_dir,LC_SAVE_DIR);
        strcpy(s,getenv("HOME"));
        strcat(s,"/");
        strcat(s,lc_save_dir);
        if ((dp=opendir(s))==NULL)
        {
		printf(
"\n  LinCity is about to create a directory in which to save your games.\n");
		printf("\n It is called %s\n",lc_save_dir);
		printf("       Is this OK?  (Y/n):");
		c=getchar();
		if (c=='n' || c=='N')
		{
			printf(
" Sorry, can't continue without this directory\n");
			exit(0);
		}

                mkdir(s,0755);
#ifndef __EMX__
		chown(s,getuid(),getgid());
#endif
                if ((dp=opendir(s))==NULL)
                {
                        printf("Couldn't create the save directory %s\n",s);
                        return;
                }
        }
	closedir(dp);
}


void draw_save_dir(int bg_colour)
{
	char s[100],s2[200];
	int i,j,l;
	DIR *dp;
	struct dirent *ep;

	strcpy(s,getenv("HOME"));
	strcat(s,"/");
	strcat(s,LC_SAVE_DIR);
	if ((dp=opendir(s))==NULL)
	{
//		mkdir(s,0755);
//		if ((dp=opendir(s))==NULL)
//		{
			printf("Couldn't find the save directory %s\n",s);
			return;
//		}
	}
	for (i=1;i<10;i++)
	{
		save_names[i][0]=0;
		while ((ep=readdir(dp))) // extra brackets to stop warning
		{
			if ( *(ep->d_name)==(i+'0')
				&& *((ep->d_name)+1)=='_')
			{
				sprintf(s2,"%2d ",i); 
				strncpy(save_names[i],ep->d_name,40);
				if (strlen(save_names[i])>2)
					strncat(s2,&(save_names[i][2]),40);
				else
					strcat(s2,"???");
			}
		}
		if (strlen(save_names[i])<1)
			sprintf(s2," %d .....",i);
		else
		{
			l=strlen(s2);
			for (j=0;j<l;j++)
				if (s2[j]=='_')
					s2[j]=' ';
		}
		if (verify_city(save_names[i])==0)
			Fgl_setfontcolors(bg_colour,red(28));
		else
			Fgl_setfontcolors(bg_colour,green(28));
//printf("%s\n",s2);
		Fgl_write(MAIN_WIN_X+24,MAIN_WIN_Y+10*(10+i),s2);
		rewinddir(dp);
	}
	closedir(dp);
	Fgl_setfontcolors(bg_colour,TEXT_FG_COLOUR);
}

void input_save_filename(char *s)
{
	char c;
	int i,t,on;
	c=0;
	if (strlen(s)>40)
		s[40]=0;
	if ((t=strlen(s))>1)
		for (i=0;i<t;i++)
			if (s[i]=='_')
				s[i]=' ';
	while (c!=0xd && c!=0xa)
	{
		Fgl_write(MAIN_WIN_X+24,MAIN_WIN_Y+340,s);
		Fgl_write(MAIN_WIN_X+24+(strlen(s)*8)
			,MAIN_WIN_Y+340,"_");
		on=1;
		get_real_time();
		t=real_time;
#ifdef LC_X11
		call_event();
		while ((c=x_key_value)==0)
#else
		while((c=vga_getkey())==0)
#endif
		{
#ifdef LC_X11
			call_event();
#endif
			get_real_time();
			if (real_time > t+200)
			{
				if (on==1)
				{
					Fgl_write(MAIN_WIN_X+24+(strlen(s)*8)
						,MAIN_WIN_Y+340," ");
					on=0;
				}
				else
				{
					Fgl_write(MAIN_WIN_X+24+(strlen(s)*8)
						,MAIN_WIN_Y+340,"_");
					on=1;
				}
				get_real_time();
				t=real_time;
			}
		}
#ifdef LC_X11
		x_key_value=0;
#endif
		if ((isalnum(c) || c==' ' || c=='.' || c=='%' || c==','
			|| c=='-' || c=='+') && strlen(s)<40)
		{
			t=strlen(s);
			s[t]=c;
			s[t+1]=0;
		}
		else if (c==0x7f && strlen(s)>0)
		{
			Fgl_write(MAIN_WIN_X+24+(strlen(s)*8)
				,MAIN_WIN_Y+340," ");
			s[strlen(s)-1]=0;
		}
	}
	t=strlen(s);
	for (i=0;i<t;i++)
		if (s[i]==' ')
			s[i]='_';
}

int verify_city(char *cname)
{
        char s[200],s2[200];
        int v;
	v=0;
        FILE *ofile;
        strcpy(s,getenv("HOME"));
        strcat(s,"/");
        strcat(s,LC_SAVE_DIR);
        strcat(s,"/");
        strcat(s,cname);
        strcpy(s2,"gzip -d -c ");
        strcat(s2,s);
	strcat(s2," 2> /dev/null");
        if ((ofile=popen(s2,"r"))==NULL)
		return(0);
        fscanf(ofile,"%d",&v);
	pclose(ofile);
        if (v!=VERSION)
                return(0);
	return(1);
}

#define SI_BLACK 252
#define SI_RED 253
#define SI_GREEN 254
#define SI_YELLOW 255

void load_start_image(void)
{
#ifdef LC_X11
	XColor pal[256];
#endif
	char s[256];
        long x,y,l,r,g,b;
        FILE *fp;
#ifdef LC_X11
        XEvent xev;
#endif
	strcpy(s,"gzip -d -c ");
	strcat(s,opening_pic);
        if ((fp=popen(s,"r"))==NULL)
        	return;
        for (x=0;x<7;x++)
                l=fgetc(fp);
	l&=0xff;
        if (l==0)
                l=256;
        for (x=0;x<l;x++)
        {
                r=fgetc(fp);
                g=fgetc(fp);
                b=fgetc(fp);
#ifdef LC_X11
		pal[x].red=r;
		pal[x].green=g;
		pal[x].blue=b;
		pal[x].flags= DoRed | DoGreen | DoBlue;
#else
                gl_setpalettecolor(x,r,g,b);
#endif
        }
// use last 4 colours for text
#ifdef LC_X11
	pal[SI_BLACK].red=0;
	pal[SI_BLACK].green=0;
	pal[SI_BLACK].blue=0;
	pal[SI_BLACK].flags= DoRed | DoGreen | DoBlue;
        pal[SI_RED].red=60;
        pal[SI_RED].green=0;
        pal[SI_RED].blue=0;
        pal[SI_RED].flags= DoRed | DoGreen | DoBlue;
        pal[SI_GREEN].red=0;
        pal[SI_GREEN].green=60;
        pal[SI_GREEN].blue=0;
        pal[SI_GREEN].flags= DoRed | DoGreen | DoBlue;
        pal[SI_YELLOW].red=60;
        pal[SI_YELLOW].green=60;
        pal[SI_YELLOW].blue=0;
        pal[SI_YELLOW].flags= DoRed | DoGreen | DoBlue;
	open_setcustompalette(pal);
//	XSync(display.dpy,FALSE);
        suppress_next_expose=1;
        do
        {
                while (XPending(display.dpy)==0);
                XNextEvent(display.dpy, &xev);
                HandleEvent(&xev);
        } while (xev.type!=MapNotify);

#else
	gl_setpalettecolor(SI_BLACK,0,0,0);
	gl_setpalettecolor(SI_RED,60,0,0);
	gl_setpalettecolor(SI_GREEN,0,60,0);
	gl_setpalettecolor(SI_YELLOW,60,60,0);
#endif
        for (y=0;y<480;y++)
                for (x=0;x<640;x++)
                {
                        l=fgetc(fp);
		//	printf("l=%d x=%d y=%d ",l,x,y);
// octree doesn't seem to want to generate images with 252 colours!
// So the next best thing (well the easyest) is to just map the pixels
// coloured as the last 4 colours to the 4 before that.
// If it looks OK, leave it.
			if (l==SI_BLACK)
				l=SI_BLACK-4;
			if (l==SI_RED)
				l=SI_RED-4;
			if (l==SI_GREEN)
				l=SI_GREEN-4;
			if (l==SI_YELLOW)
				l=SI_YELLOW-4;
                        Fgl_setpixel(x,y,l);
		//	printf("Set the pixel\n");
                }
        fclose(fp);
	start_image_text();

}

void start_image_text(void)
{
	char s[128];
	int i;
	FILE *inf;
	strcpy(s, LIBDIR );
	strcat(s,"/opening/8x8thin");
	if ((inf=fopen(s,"r"))==NULL)
		do_error("Can't open opening screen font 8x8thin");
	for (i=0;i<2048;i++)
		start_font1[i]=fgetc(inf);
	fclose(inf);
	strcpy(s,LIBDIR);
	strcat(s,"/opening/scrawl_w.fnt");
        if ((inf=fopen(s,"r"))==NULL)
                do_error("Can't open opening screen font scrawl_w.fnt");
        for (i=0;i<4096;i++)
                start_font2[i]=fgetc(inf);
        fclose(inf);
	strcpy(s,LIBDIR);
	strcat(s,"/opening/scrawl_s.fnt");
        if ((inf=fopen(s,"r"))==NULL)
                do_error("Can't open opening screen font scrawl_s.fnt");
        for (i=0;i<4096;i++)
                start_font3[i]=fgetc(inf);
        fclose(inf);

#ifdef LC_X11
	open_font=start_font1;
	open_font_height=8;
#else
        gl_setwritemode(FONT_COMPRESSED);
        gl_setfont(8,8,start_font1);
	gl_setwritemode(FONT_COMPRESSED);
#endif
        Fgl_setfontcolors(SI_BLACK,SI_RED);
	si_scroll_text();
}

void si_scroll_text(void)
{
	char s[100],line1[100],line2[100],line3[100],c;
	int i,t,l1c=0,l2c=0,l3c=0;
	FILE *inf1,*inf2,*inf3;
#ifdef LC_X11
	XEvent xev;
#endif
	Fgl_enableclipping();
	strcpy(s,LIBDIR);
	strcat(s,"/opening/text1");
	if ((inf1=fopen(s,"r"))==NULL)
		do_error("Can't open opening/text1");
	for (i=0;i<52;i++)
		line1[i]=si_next_char(inf1);
	line1[52]=0;
	strcpy(s,LIBDIR);
	strcat(s,"/opening/text2");
        if ((inf2=fopen(s,"r"))==NULL)
                do_error("Can't open opening/text2");
        for (i=0;i<52;i++)
                line2[i]=si_next_char(inf2);
        line2[52]=0;
	strcpy(s,LIBDIR);
	strcat(s,"/opening/text3");
        if ((inf3=fopen(s,"r"))==NULL)
                do_error("Can't open opening/text3");
        for (i=0;i<52;i++)
                line3[i]=si_next_char(inf3);
        line3[52]=0;
#ifdef LC_X11
//	suppress_next_expose=1;
#endif
	do
	{
		get_real_time();
		t=real_time+30;
#ifdef LC_X11
//                call_event();
//		suppress_next_expose=1;
	        if (XPending(display.dpy) )  // || wait!=0)
	        {
	                XNextEvent(display.dpy, &xev);
	                HandleEvent(&xev);
	        }

                c=x_key_value;
#else
		c=vga_getkey();
#endif
		if (l1c>=8)
		{
			for (i=0;i<51;i++)
				line1[i]=line1[i+1];
			line1[51]=si_next_char(inf1);
			l1c=0;
		}
#ifdef LC_X11
		open_font=start_font1;
		open_font_height=8;
#else
		gl_setfont(8,8,start_font1);
#endif
		Fgl_setclippingwindow(120,30,520,40);
		Fgl_setfontcolors(SI_BLACK,SI_RED);
#ifdef LC_X11
		open_write(120-l1c,31,line1);
#else
		Fgl_write(120-l1c,31,line1);
#endif
		l1c++;

                if (l2c>=8)
                {
                        for (i=0;i<51;i++)
                                line2[i]=line2[i+1];
                        line2[51]=si_next_char(inf2);
                        l2c=0;
                }
#ifdef LC_X11
		open_font=start_font2;
		open_font_height=16;
#else
		gl_setfont(8,16,start_font2);
#endif
                Fgl_setclippingwindow(120,55,520,73);
		Fgl_setfontcolors(SI_BLACK,SI_GREEN);
#ifdef LC_X11
		open_write(120-l2c,57,line2);
#else
                Fgl_write(120-l2c,57,line2);
#endif
                l2c+=2;

                if (l3c>=8)
                {
                        for (i=0;i<51;i++)
                                line3[i]=line3[i+1];
                        line3[51]=si_next_char(inf3);
                        l3c=0;
                }
#ifdef LC_X11
		open_font=start_font3;
		open_font_height=16;
#else
                gl_setfont(8,16,start_font3);
#endif
                Fgl_setclippingwindow(120,88,520,106);
                Fgl_setfontcolors(SI_BLACK,SI_YELLOW);
#ifdef LC_X11
		open_write(120-l3c,90,line3);
#else
                Fgl_write(120-l3c,90,line3);
#endif
                l3c+=2;
		while (real_time<t)
		{
			lc_usleep(1);
			get_real_time();
		}
	} while (c==0);
	fclose(inf1);
	fclose(inf2);
	fclose(inf3);
	Fgl_disableclipping();
}

char si_next_char(FILE *inf)
{
	char c;
	if (feof(inf)!=0)
		fseek(inf,0L,SEEK_SET);
	c=fgetc(inf);
	if (c==0xa || c==0xd)
		c=' ';
	return(c);
}


void get_real_time(void)
{
	gettimeofday(&lc_timeval,0);
	real_time=(lc_timeval.tv_sec-real_start_time)*1000
		+(lc_timeval.tv_usec/1000);
}

void debug_writeval(int v)
{
	char s[100];
	sprintf(s,"%d  ",v);
	Fgl_write(280,471,s);
}

void time_for_year(void)
{
	static time_last_year=0;
	char s[100];
	float f;
	f=(float)(real_time-time_last_year)/1000.0;
	if (f>3600.0)
		sprintf(s,"%5.1f MINS/year  ",f/60.0);
	else
		sprintf(s,"%5.1f secs/year  V %d.%02d "
			,f,VERSION/100,VERSION%100);
	Fgl_write(330,470,s);
	time_last_year=real_time;
}

void debug_stuff(void)
{
	int i;
//	for (i=0;i<10;i++)
//	{
//		printf("Buf1[%d]=%d\n",i,buf1[i]);
//		buf1[i]=1;
//	}
//	for (i=0;i<10;i++)
//	{
//		printf("Buf2[%d]=%d\n",i,buf2[i]);
//		buf2[i]=1;
//	}
	for (i=0;i<NUMOF_GROUPS;i++)
		printf("FC[%d]=%d\n",i,fire_chance[i]);

}

int cheat(void)
{
	if (cheat_flag!=0)
		return(1);
	if (yn_dial_box("TEST","You have pressed a test key"
		,"You will only see this message once"
		,"Do you really want to play in test mode...")!=0)
	{
		print_cheat();
		cheat_flag=1;
		return(1);
	}
	return(0);
}

void print_cheat(void)
{
	Fgl_write(TESTM_X,TESTM_Y,"TEST MODE!");
}

void unprint_cheat(void)
{
	Fgl_fillbox(TESTM_X,TESTM_Y,8*10,8,TEXT_BG_COLOUR);
}

void order_select_buttons(void)
{
	sbut[0]=13; // powerline
	sbut[1]=15; // solar power
	sbut[2]=14; // substation
	sbut[3]=0; // residence
	sbut[4]=1; // farm
	sbut[5]=2; // market
	sbut[6]=16; // buldoze
	sbut[7]=19; // track
	sbut[8]=10; // coalmine
	sbut[9]=28; // rail
	sbut[10]=29; // coal power
	sbut[11]=25; // road
	sbut[12]=27; // light industry
	sbut[13]=11; // university
	sbut[14]=3; // commune
	sbut[15]=4; // oremine
	sbut[16]=5; // tip
	sbut[17]=9; // export
	sbut[18]=12; // heavy industry
	sbut[19]=6; // parkland
	sbut[20]=30; // recycle
	sbut[21]=20; // water
	sbut[22]=26; // health
	sbut[23]=31; // rocket
	sbut[24]=24; // windmill
	sbut[25]=17; // monument
	sbut[26]=21; // school
	sbut[27]=22; // blacksmith
	sbut[28]=8; // mill
	sbut[29]=18; // pottery
	sbut[30]=23; // fire station
	sbut[31]=7; // cricket 
}

void lincityrc(void)
{
	char s[100],s1[100];
	int i;
	FILE *rc;
	strcpy(s,getenv("HOME"));
	strcat(s,"/");
	strcat(s,".lincityrc");
	if ((rc=fopen(s,"r"))==0)
	{
		do {
		strcpy(s1,"cat ");
		strcat(s1,message_path);
		strcat(s1,"mousetype.mes");
		system(s1);
#ifdef LC_X11
		do
		{
	                call_event();
        	        i=x_key_value;
		} while (i==0);
		x_key_value=0;
#else
		i=getchar();
#endif
		} while (i<'0' || i>'6');
		if ((rc=fopen(s,"w"))==0)
		{
			printf("Can't open %s for writing, can't continue\n",s);
			exit(1);
		}
		fprintf(rc,"mouse=%d\n",i-'0');
//		fprintf(rc,"lc_save_dir=%s\n",lc_save_dir);
		fclose(rc);
#ifndef __EMX__
		chown(s,getuid(),getgid());
#endif
		if ((rc=fopen(s,"r"))==0)
		{
			printf("What!! can't open %s for reading after writing???\n",s);
			exit(1);
		}
	}
	while (feof(rc)==0)
	{
		fgets(s,99,rc);
		if (sscanf(s,"mouse=%d",&i)!=0)
			lc_mouse_type=i;
//		if (sscanf(s1,"lc_save_dir=%s",s)!=0)
//			strcpy(lc_save_dir,s1);
	}
	fclose(rc);
}

void check_for_old_save_dir(void)
{
	char s[100],s1[100],command[100],c;
	DIR *dp;
        strcpy(s,getenv("HOME"));
        strcat(s,"/");
	strcpy(s1,s);
	strcat(s,OLD_LC_SAVE_DIR);
	strcat(s1,LC_SAVE_DIR);
	if ((dp=opendir(s))!=0)
	{  // we have an old lc_save_dir here.
		closedir(dp);
		printf(
		"\nLinCity has found an old version of the directory\n");
		printf(
	"where the saved games and some working files are stored.\n");
		printf("  It is called %s/\n",OLD_LC_SAVE_DIR);
		printf("     Select an option below:\n");
		printf("\n     1. Rename %s to %s (no loss of data)\n"
		,OLD_LC_SAVE_DIR,LC_SAVE_DIR);
printf("     2. Forget the old one and start with an empty directory\n");
printf("           (The old one will not be deleted.)\n");
printf("     3. Forget the whole thing and exit LinCity.\n");
		do
		{
			c=getchar();
			switch (c)
			{
				case('1'):
					strcpy(command,"mv ");
					strcat(command,s);
					strcat(command," ");
					strcat(command,s1);
					if (system(command)!=0)
					{
		printf("Shell command %s failed\n",command);
						exit(1);
					}
#ifndef __EMX__
					chown(s1,getuid(),getgid());
#endif
					return;
				case('2'):
					mkdir(s1,0755);
#ifndef __EMX__
					chown(s1,getuid(),getgid());
#endif
					return;
				case('3'):
					exit(0);
			}
		} while (1);
	}
}


int count_groups(int g)
{
	int x,y,i;
	i=0;
	for (y=0;y<WORLD_SIDE_LEN;y++)
		for (x=0;x<WORLD_SIDE_LEN;x++)
			if (main_types[mappointtype[x][y]].group==g)
				i++;
	return(i);
}

void count_all_groups(void)
{
	int x,y,t;
	for (x=0;x<NUMOF_GROUPS;x++)
		group_count[x]=0;
	for (y=0;y<WORLD_SIDE_LEN;y++)
	for (x=0;x<WORLD_SIDE_LEN;x++)
	{
		t=mappointtype[x][y];
		if (t!=CST_USED && t!=CST_GREEN)
			group_count[main_types[t].group]++;
	}
}

int compile_results(void)
{
	char s[256];
//	int l=0;
	FILE *outf;
        strcpy(s,getenv("HOME"));
        strcat(s,"/");
        strcat(s,LC_SAVE_DIR);
        strcat(s,"/");
        strcat(s,RESULTS_FILENAME);

	count_all_groups();
	if ((outf=fopen(s,"w"))==0)
	{
		printf("Unable to open %s\n",RESULTS_FILENAME);
		return(0);
	}
	if (cheat_flag)
		fprintf(outf,"----- IN TEST MODE -------\n");
	fprintf(outf,"Results from LinCity Version %d.%02d\n"
		,VERSION/100,VERSION%100);
	if (strlen(given_scene)>3)
		fprintf(outf,"Initial loaded scene - %s\n",given_scene);
	if (sustain_flag)
		fprintf(outf,"Economy is sustainable\n");
	fprintf(outf,"Population  %d  of which  %d  are not housed.\n"
		,housed_population+people_pool,people_pool);
	fprintf(outf,
	"Max population %d  Number evacuated %d Total births %d\n"
		,max_pop_ever,total_evacuated,total_births);
	fprintf(outf," Date  %s %04d   Money %8d   Tech-level %5.1f (%5.1f)\n"
		,months[(total_time%NUMOF_DAYS_IN_YEAR)/NUMOF_DAYS_IN_MONTH]
		,total_time/NUMOF_DAYS_IN_YEAR,total_money
		,(float)tech_level*100.0/MAX_TECH_LEVEL
		,(float)highest_tech_level*100.0/MAX_TECH_LEVEL);
	fprintf(outf," Deaths by starvation %7d   History %8.3f\n"
		,total_starve_deaths,starve_deaths_history);
        fprintf(outf,"Deaths from pollution %7d   History %8.3f\n"
                ,total_pollution_deaths,pollution_deaths_history);
	fprintf(outf,"Years of unemployment %7d   History %8.3f\n"
		,total_unemployed_years,unemployed_history);
	fprintf(outf,"    Residences %4d         Markets %4d            Farms %4d\n"
		,group_count[GROUP_RESIDENCE],group_count[GROUP_MARKET]
		,group_count[GROUP_ORGANIC_FARM]); 
	fprintf(outf,"        Tracks %4d           Roads %4d             Rail %4d\n"
                ,group_count[GROUP_TRACK],group_count[GROUP_ROAD]
                ,group_count[GROUP_RAIL]);
	fprintf(outf,"     Potteries %4d     Blacksmiths %4d            Mills %4d\n"
                ,group_count[GROUP_POTTERY],group_count[GROUP_BLACKSMITH]
                ,group_count[GROUP_MILL]);
	fprintf(outf,"     Monuments %4d         Schools %4d     Universities %4d\n"
                ,group_count[GROUP_MONUMENT],group_count[GROUP_SCHOOL]
                ,group_count[GROUP_UNIVERSITY]);
	fprintf(outf," Fire stations %4d           Parks %4d     Cricket gnds %4d\n"
                ,group_count[GROUP_FIRESTATION],group_count[GROUP_PARKLAND]
                ,group_count[GROUP_CRICKET]);
	fprintf(outf,"    Coal mines %4d       Ore mines %4d         Communes %4d\n"
                ,group_count[GROUP_COALMINE],group_count[GROUP_OREMINE]
                ,group_count[GROUP_COMMUNE]);
	fprintf(outf,"     Windmills %4d     Coal powers %4d     Solar powers %4d\n"
		,group_count[GROUP_WINDMILL]
		,group_count[GROUP_POWER_SOURCE_COAL]
                ,group_count[GROUP_POWER_SOURCE]);
	fprintf(outf,"   Substations %4d     Power lines %4d            Ports %4d\n"
                ,group_count[GROUP_SUBSTATION],group_count[GROUP_POWER_LINE]
                ,group_count[GROUP_PORT]);
	fprintf(outf,"    Light inds %4d      Heavy inds %4d        Recyclers %4d\n"
                ,group_count[GROUP_INDUSTRY_L],group_count[GROUP_INDUSTRY_H]
                ,group_count[GROUP_RECYCLE]);
	fprintf(outf,"Health centres %4d            Tips %4d         Shanties %4d\n"
		,group_count[GROUP_HEALTH],group_count[GROUP_TIP]
		,group_count[GROUP_SHANTY]);
	fclose(outf);
	return(1);
}


void print_results(void)
{
	char s[256];
	if (compile_results()==0)
		return;
	strcpy(s,"cat ");
        strcat(s,getenv("HOME"));
        strcat(s,"/");
        strcat(s,LC_SAVE_DIR);
        strcat(s,"/");
        strcat(s,RESULTS_FILENAME);
//	printf("\n\n   LinCity   Results\n\n");
	printf("\n");
	system(s);
	printf("\n");
}

void mail_results(void)
{
        char s[256];
        if (compile_results()==0)
                return;
        strcpy(s,"mail -s 'LinCity results' lc-results@floot.demon.co.uk < ");
        strcat(s,getenv("HOME"));
        strcat(s,"/");
        strcat(s,LC_SAVE_DIR);
        strcat(s,"/");
        strcat(s,RESULTS_FILENAME);
        system(s);
}


void window_results(void)
{
	char s[256];
	if (compile_results()==0)
		return;
        strcpy(s,getenv("HOME"));
        strcat(s,"/");
        strcat(s,LC_SAVE_DIR);
        strcat(s,"/");
        strcat(s,RESULTS_FILENAME);
	ok_dial_box(s,RESULTS,"Game statistics");
}


void random_start(void)
{
	int x,y,xx,yy,flag;
// first find a place that has some water.
	if (total_time>NUMOF_DAYS_IN_YEAR)
		return;  // don't put a new rand village if we already
                         // one year in.
	do
	{
		do
		{
			xx=rand()%(WORLD_SIDE_LEN-25);
			yy=rand()%(WORLD_SIDE_LEN-25);
			flag=0;
			for (y=yy+2;y<yy+23;y++)
			for (x=xx+2;x<xx+23;x++)
				if (main_types[mappointtype[x][y]].group
				== GROUP_WATER)
				{
					flag=1;
					x=xx+23;
					y=yy+23;
				}
		} while (flag==0);
		for (y=yy+4;y<yy+22;y++)
		for (x=xx+4;x<xx+22;x++)
			if (main_types[mappointtype[x][y]].group
				!= GROUP_BARE)
			{
				flag=0;
				x=xx+22;
				y=yy+22;
			}
	} while (flag==0);
	main_screen_originx=xx;
	main_screen_originy=yy;

//  Draw the start scene.
	quick_start_add(xx+5,yy+5,CST_FARM_O0,4);
	quick_start_add(xx+9,yy+6,CST_RESIDENCE_ML,3);
	mappoint[xx+9][yy+6].population=50;
	mappoint[xx+9][yy+6].flags|=(FLAG_FED+FLAG_EMPLOYED);
	quick_start_add(xx+7,yy+9,CST_MARKET_EMPTY,2);
        marketx[numof_markets]=xx+7;
        markety[numof_markets]=yy+9;
        numof_markets++;
// Bootstap markets with some stuff.
	mappoint[xx+7][yy+9].int_1=2000;
        mappoint[xx+7][yy+9].int_2=10000;
	mappoint[xx+7][yy+9].int_3=100;
	mappoint[xx+7][yy+9].int_5=10000;
	mappoint[xx+7][yy+9].flags|= (FLAG_MB_FOOD+FLAG_MS_FOOD+FLAG_MB_JOBS
		 +FLAG_MS_JOBS+FLAG_MB_COAL+FLAG_MS_COAL+FLAG_MB_ORE
		 +FLAG_MS_ORE+FLAG_MB_GOODS+FLAG_MS_GOODS+FLAG_MB_STEEL
		 +FLAG_MS_STEEL);


	quick_start_add(xx+14,yy+6,CST_RESIDENCE_ML,3);
	mappoint[xx+14][yy+6].population=50;
	mappoint[xx+14][yy+6].flags|=(FLAG_FED+FLAG_EMPLOYED);
	quick_start_add(xx+17,yy+5,CST_FARM_O0,4);
        quick_start_add(xx+17,yy+9,CST_MARKET_EMPTY,2);
        marketx[numof_markets]=xx+17;
        markety[numof_markets]=yy+9;
        numof_markets++;
        mappoint[xx+17][yy+9].int_1=2000;
        mappoint[xx+17][yy+9].int_2=8000;
        mappoint[xx+17][yy+9].flags|= (FLAG_MB_FOOD+FLAG_MS_FOOD+FLAG_MB_JOBS
                 +FLAG_MS_JOBS+FLAG_MB_COAL+FLAG_MS_COAL+FLAG_MB_ORE
                 +FLAG_MS_ORE+FLAG_MB_GOODS+FLAG_MS_GOODS+FLAG_MB_STEEL
                 +FLAG_MS_STEEL);

	for (x=5;x<19;x++)
	{
		quick_start_add(xx+x,yy+11,CST_TRACK_LR,1);
		mappoint[xx+x][yy+11].flags|=FLAG_IS_TRANSPORT;
	}
	for (y=12;y<18;y++)
	{
		quick_start_add(xx+5,yy+y,CST_TRACK_LR,1);
		mappoint[xx+5][yy+y].flags|=FLAG_IS_TRANSPORT;
	}
	quick_start_add(xx+6,yy+12,CST_COMMUNE_1,4);
	quick_start_add(xx+6,yy+17,CST_COMMUNE_1,4);
	quick_start_add(xx+11,yy+12,CST_COMMUNE_1,4);
	quick_start_add(xx+11,yy+17,CST_COMMUNE_1,4);
	quick_start_add(xx+16,yy+12,CST_COMMUNE_1,4);
	quick_start_add(xx+16,yy+17,CST_COMMUNE_1,4);
	for (x=6;x<17;x++)
	{
		quick_start_add(xx+x,yy+16,CST_TRACK_LR,1);
		mappoint[xx+x][yy+16].flags|=FLAG_IS_TRANSPORT;
	}
	for (y=12;y<16;y++)
	{
		quick_start_add(xx+10,yy+y,CST_TRACK_LR,1);
		mappoint[xx+10][yy+y].flags|=FLAG_IS_TRANSPORT;
		quick_start_add(xx+15,yy+y,CST_TRACK_LR,1);
		mappoint[xx+15][yy+y].flags|=FLAG_IS_TRANSPORT;
	}
	quick_start_add(xx+10,yy+17,CST_TRACK_LR,1);
	mappoint[xx+10][yy+17].flags|=FLAG_IS_TRANSPORT;
	quick_start_add(xx+15,yy+17,CST_TRACK_LR,1);
	mappoint[xx+15][yy+17].flags|=FLAG_IS_TRANSPORT;

	quick_start_add(xx+9,yy+9,CST_POTTERY_0,2);

	refresh_main_screen();
}

void quick_start_add(int x,int y,int type,int size)
{
	int xx,yy;
	if (size==1)
	{
		mappointtype[x][y]=type;
		return;
	}
	for (yy=0;yy<size;yy++)
	for (xx=0;xx<size;xx++)
	{
		if (xx==0 && yy==0)
			continue;
		mappointtype[x+xx][y+yy]=CST_USED;
		set_mappoint_ints(x,y,x+xx,y+yy);
	}
	mappointtype[x][y]=type;
}

void init_path_strings(void)
{
	strcpy(colour_pal_file,LIBDIR);
	strcat(colour_pal_file,"/colour.pal");
	strcpy(opening_pic,LIBDIR);
	strcat(opening_pic,"/opening/open.tga.gz");
	strcpy(graphic_path,LIBDIR);
	strcat(graphic_path,"/icons/");
	strcpy(helppath,LIBDIR);
	strcat(helppath,"/help/");
	strcpy(message_path,LIBDIR);
	strcat(message_path,"/messages/");
	strcpy(fontfile,LIBDIR);
	strcat(fontfile,"/opening/alt-8x8");
}

void lc_usleep(unsigned long t)
{
	struct timeval timeout;
        timeout.tv_sec = t / 1000000;
        timeout.tv_usec = t - 1000000 * timeout.tv_sec;
        select(1, NULL, NULL, NULL, &timeout);
}

void dump_tcore(void)
{
#ifdef ALLOW_TCORE_DUMP
	char s[200];
	int x,y,z,q,n;
	FILE *ofile;
        strcpy(s,getenv("HOME"));
        strcat(s,"/");
        strcat(s,LC_SAVE_DIR);
        strcat(s,"/");
        strcat(s,"tcore.txt");
//	strcpy(s2,"gzip -c -9 > ");
//	strcpy(s2,"gzip -c > ");
//	strcat(s2,s);
	if ((ofile=fopen(s,"w"))==NULL)
	{
		printf("Tcore file <%s> - ",s);
		do_error("Can't open it!");
	}
	fprintf(ofile,"Version=%d\n",(int)VERSION);
	q=sizeof(struct MAPPOINT);
	prog_box("Saving tcore",0);
	for (x=0;x<WORLD_SIDE_LEN;x++)
	{
		for (y=0;y<WORLD_SIDE_LEN;y++)
		{
			for (z=0;z<q;z++)
			{
				n=*(((unsigned char *)&mappoint[x][y])+z);
				fprintf(ofile,"mappoint[%d][%d]b%d=%d\n"
					,x,y,z,n);
			}
			fprintf(ofile,"mappointpol[%d][%d]=%d\n"
				,x,y,(int)mappointpol[x][y]);
			fprintf(ofile,"mappointtype[%d][%d]=%d\n"
				,x,y,(int)mappointtype[x][y]);
		}
		prog_box("",(90*x)/WORLD_SIDE_LEN);
	}
	fprintf(ofile,"Origx=%d\n",main_screen_originx);
	fprintf(ofile,"Origy=%d\n",main_screen_originy);
	fprintf(ofile,"Total time=%d\n",total_time);
	for (x=0;x<MAX_NUMOF_SUBSTATIONS;x++)
	{
		fprintf(ofile,"SustationX[%d]=%d\n",x,substationx[x]);
		fprintf(ofile,"Substation[%d]=%d\n",x,substationy[x]);
	}
	prog_box("",92);
	fprintf(ofile,"Num of substations=%d\n",numof_substations);
	for (x=0;x<MAX_NUMOF_MARKETS;x++)
	{
		fprintf(ofile,"MarketX[%d]=%d\n",x,marketx[x]);
		fprintf(ofile,"MarketY[%d]=%d\n",x,markety[x]);
	}
	prog_box("",94);
	fprintf(ofile,"numof_markets=%d\n",numof_markets);
	fprintf(ofile,"people_pool=%d\n",people_pool);
	fprintf(ofile,"total_money=%d\n",total_money);
	fprintf(ofile,"income_tax_rate=%d\n",income_tax_rate);
	fprintf(ofile,"coal_tax_rate=%d\n",coal_tax_rate);
	fprintf(ofile,"dole_rate=%d\n",dole_rate);
	fprintf(ofile,"transport_cost_rate=%d\n",transport_cost_rate);
	fprintf(ofile,"goods_tax_rate=%d\n",goods_tax_rate);
	fprintf(ofile,"export_tax=%d\n",export_tax);
	fprintf(ofile,"export_tax_rate=%d\n",export_tax_rate);
	fprintf(ofile,"import_cost=%d\n",import_cost);
	fprintf(ofile,"import_cost_rate=%d\n",import_cost_rate);
	fprintf(ofile,"tech_level=%d\n",tech_level);
	fprintf(ofile,"tpopulation=%d\n",tpopulation);
	fprintf(ofile,"tstarving_population=%d\n",tstarving_population);
	fprintf(ofile,"tunemployed_population=%d\n",tunemployed_population);
	fprintf(ofile,"waste_goods=%d\n",waste_goods);
	fprintf(ofile,"power_made=%d\n",power_made);
	fprintf(ofile,"power_used=%d\n",power_used);
	fprintf(ofile,"coal_made=%d\n",coal_made);
	fprintf(ofile,"coal_used=%d\n",coal_used);
	fprintf(ofile,"goods_made=%d\n",goods_made);
	fprintf(ofile,"goods_used=%d\n",goods_used);
	fprintf(ofile,"ore_made=%d\n",ore_made);
	fprintf(ofile,"ore_used=%d\n",ore_used);
	fprintf(ofile,"diff_old_population=%d\n",diff_old_population);
	prog_box("",96);
//	for (x=0;x<MAPPOINT_STATS_W;x++)
//	{
//		fprintf(ofile,"%d\n",monthgraph_pop[x]);
//		fprintf(ofile,"%d\n",monthgraph_starve[x]);
//		fprintf(ofile,"%d\n",monthgraph_nojobs[x]);
//		fprintf(ofile,"%d\n",monthgraph_ppool[x]);
//		fprintf(ofile,"%d\n",diffgraph_power[x]);
//		fprintf(ofile,"%d\n",diffgraph_coal[x]);
//		fprintf(ofile,"%d\n",diffgraph_goods[x]);
//		fprintf(ofile,"%d\n",diffgraph_ore[x]);
//		fprintf(ofile,"%d\n",diffgraph_population[x]);
//	}
	prog_box("",98);
	fprintf(ofile,"rockets_launched=%d\n",rockets_launched);
	fprintf(ofile,"rockets_launched_success=%d\n",rockets_launched_success);
	fprintf(ofile,"coal_survey_done=%d\n",coal_survey_done);
//	for (x=0;x<12;x++)
//	{
//		fprintf(ofile,"%d\n",pbar_pops[x]);
//		fprintf(ofile,"%d\n",pbar_techs[x]);
//		fprintf(ofile,"%d\n",pbar_foods[x]);
//		fprintf(ofile,"%d\n",pbar_jobs[x]);
//		fprintf(ofile,"%d\n",pbar_money[x]);
//		fprintf(ofile,"%d\n",pbar_coal[x]);
//		fprintf(ofile,"%d\n",pbar_goods[x]);
//		fprintf(ofile,"%d\n",pbar_ore[x]);
//		fprintf(ofile,"%d\n",pbar_steel[x]);
//	}
	prog_box("",99);
//	fprintf(ofile,"%d\n",pbar_pop_oldtot);
//	fprintf(ofile,"%d\n",pbar_pop_olddiff);
//	fprintf(ofile,"%d\n",pbar_tech_oldtot);
//	fprintf(ofile,"%d\n",pbar_tech_olddiff);
//	fprintf(ofile,"%d\n",pbar_food_oldtot);
//	fprintf(ofile,"%d\n",pbar_food_olddiff);
//       fprintf(ofile,"%d\n",pbar_jobs_oldtot);
//        fprintf(ofile,"%d\n",pbar_jobs_olddiff);
//        fprintf(ofile,"%d\n",pbar_money_oldtot);
//        fprintf(ofile,"%d\n",pbar_money_olddiff);
//        fprintf(ofile,"%d\n",pbar_coal_oldtot);
//        fprintf(ofile,"%d\n",pbar_coal_olddiff);
//        fprintf(ofile,"%d\n",pbar_goods_oldtot);
//        fprintf(ofile,"%d\n",pbar_goods_olddiff);
//        fprintf(ofile,"%d\n",pbar_ore_oldtot);
//        fprintf(ofile,"%d\n",pbar_ore_olddiff);
//        fprintf(ofile,"%d\n",pbar_steel_oldtot);
//        fprintf(ofile,"%d\n",pbar_steel_olddiff);

//	fprintf(ofile,"%d\n",cheat_flag);
//        fprintf(ofile,"%d\n",total_pollution_deaths);
//        fprintf(ofile,"%f\n",pollution_deaths_history);
//        fprintf(ofile,"%d\n",total_starve_deaths);
//        fprintf(ofile,"%f\n",starve_deaths_history);
//        fprintf(ofile,"%d\n",total_unemployed_years);
//        fprintf(ofile,"%f\n",unemployed_history);
//	fprintf(ofile,"%d\n",max_pop_ever);
//	fprintf(ofile,"%d\n",total_evacuated);
//	fprintf(ofile,"%d\n",total_births);

	fclose(ofile);
	prog_box("",100);
#endif
}

#ifdef MP_SANITY_CHECK
void sanity_check(void)
{
	static int flag=0;
	int x,y,xx,yy;
	for (x=0;x<WORLD_SIDE_LEN;x++)
	for (y=0;y<WORLD_SIDE_LEN;y++)
	{
		if (mappointtype[x][y]==CST_USED)
		{
			xx=mappoint[x][y].int_1;
			yy=mappoint[x][y].int_2;
			if (xx<(x-4) || yy<(y-4) || xx>x || yy>y)
			{
	printf("Sanity failed at %d %d, points to %d %d\n",x,y,xx,yy);
	if (flag==0)
		yn_dial_box("MP sanity check error"
			,"Please mail  ijp@floot.demon.co.uk"
			,"telling me what you just did."
			,"Do you think I'll find this bug?");
	flag=1;
			}
		}
	}
}
#endif

#ifndef LC_X11

void parse_args(int argc, char **argv)
{
        int         option;
//        extern int  optind;
        extern char *optarg;

//        int bits,i;

        while ((option = getopt(argc,argv,"wR:G:B:")) != EOF)
        {
                switch (option)
                {

			case 'w':
				gamma_correct_red=GAMMA_CORRECT_RED;
				gamma_correct_green=GAMMA_CORRECT_GREEN;
				gamma_correct_blue=GAMMA_CORRECT_BLUE;
				break;
			case 'R':
				sscanf(optarg,"%f",&gamma_correct_red);
				break;
			case 'G':
				sscanf(optarg,"%f",&gamma_correct_green);
				break;
			case 'B':
				sscanf(optarg,"%f",&gamma_correct_blue);
				break;
		}
	}
}

#endif

void sustainability_test(void)
{
	int i;
	if (sust_dig_ore_coal_tip_flag==0)
	{
		sust_dig_ore_coal_tip_flag=1;
		sust_dig_ore_coal_count=0;
	}
	else
		sust_dig_ore_coal_count++;

	if (sust_port_flag==0)
	{
		sust_port_flag=1;
		sust_port_count=0;
	}
	else
		sust_port_count++;

// Money must be going up or the same. (ie can't build.)
	if (sust_old_money>total_money)
		sust_old_money_count=0;
	else
		sust_old_money_count++;
	sust_old_money=total_money;

// population must be withing 2% of when it started.
	i=(housed_population+people_pool)-sust_old_population;
// printf("tpopulation=%d i=%d sop=%d\n",tpopulation,i,sust_old_population);
	if ( abs(i) > (sust_old_population/40)  // 2.5% 
		|| (housed_population+people_pool)<SUST_MIN_POPULATION)
	{
		sust_old_population=(housed_population+people_pool);
		sust_old_population_count=0;
	}
	else
		sust_old_population_count++;

// tech level must be going up or not fall more than 0.5% from it's
// highest during the sus count

	i=tech_level-sust_old_tech;
	if (i<0 || tech_level<SUST_MIN_TECH_LEVEL)
	{
		i=-i;
		if ((i>sust_old_tech/100) || tech_level<SUST_MIN_TECH_LEVEL)
		{
			sust_old_tech_count=0;
			sust_old_tech=tech_level;
		}
		else
			sust_old_tech_count++;
	}
	else
	{
		sust_old_tech_count++;
		sust_old_tech=tech_level;
	}

// check fire cover only every three years
	if (total_time%(NUMOF_DAYS_IN_YEAR*3)==0)
	{
		if (sust_fire_cover()!=0)
			sust_fire_count+=3;
		else
			sust_fire_count=0;

	}
	draw_sustainable_window();
}

int sust_fire_cover(void)
{
	int x,y;
	for (x=0;x<WORLD_SIDE_LEN;x++)
	for (y=0;y<WORLD_SIDE_LEN;y++)
	{
		if (main_types[mappointtype[x][y]].group==GROUP_BARE
		|| mappointtype[x][y]==CST_USED
		|| main_types[mappointtype[x][y]].group==GROUP_WATER
		|| main_types[mappointtype[x][y]].group==GROUP_POWER_LINE
		|| main_types[mappointtype[x][y]].group==GROUP_OREMINE
		|| main_types[mappointtype[x][y]].group==GROUP_ROCKET
		|| main_types[mappointtype[x][y]].group==GROUP_MONUMENT
		|| main_types[mappointtype[x][y]].group==GROUP_BURNT)
			;  // do nothing
		else
			if ((mappoint[x][y].flags&FLAG_FIRE_COVER)==0)
				return(0);
	}
//printf("We have total fire cover in year %d\n"
//,total_time/NUMOF_DAYS_IN_YEAR);

	return(1);
}

void check_endian(void)
{
	static int flag=0;
	char *cs;
	int t,x,y;
	t=0;
	cs=(char *)&t;
	*cs=1;
	if (t==1)  // little endian
		return;
printf("t=%x\n",t);
	if (flag==0)
	{
		printf("Big endian CPU detected, please e-mail\n");
		printf(
	"ijp@floot.demon.co.uk if you have problems loaing and saving.\n");
		flag=1;
	}
	for (y=0;y<WORLD_SIDE_LEN;y++)
	for (x=0;x<WORLD_SIDE_LEN;x++)
	{
		eswap32(&(mappoint[x][y].population));
		eswap32(&(mappoint[x][y].flags));
		if (sizeof(short)==2)
		{
			eswap16(&(mappoint[x][y].coal_reserve));
			eswap16(&(mappoint[x][y].ore_reserve));
		}
		else if (sizeof(short)==4)
		{
			eswap32((int *)&(mappoint[x][y].coal_reserve));
			eswap32((int *)&(mappoint[x][y].ore_reserve));
		}
		else
			printf(
		"Strange size (%d) for short, please mail me.\n"
		,sizeof(short));
		eswap32(&(mappoint[x][y].int_1));
		eswap32(&(mappoint[x][y].int_2));
		eswap32(&(mappoint[x][y].int_3));
		eswap32(&(mappoint[x][y].int_4));
		eswap32(&(mappoint[x][y].int_5));
		eswap32(&(mappoint[x][y].int_6));
		eswap32(&(mappoint[x][y].int_7));
	}
}		

void eswap32(int *i)
{
	char *cs,c1,c2,c3,c4;
	cs=(char *)i;
	c1=*cs;
	c2=*(cs+1);
	c3=*(cs+2);
	c4=*(cs+3);
	*(cs++)=c4;
	*(cs++)=c3;
	*(cs++)=c2;
	*cs=c1;
}

void eswap16(unsigned short *i)
{
	char *cs,c1,c2;
	cs=(char *)i;
	c1=*cs;
	c2=*(cs+1);
	*(cs++)=c2;
	*cs=c1;
}

