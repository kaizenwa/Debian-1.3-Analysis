// typeinit.cxx  part of lin-city
// initialises main_types
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
#include <strings.h>
#include "lctypes.h"
#include "lin-city.h"
#include "typeinit.h"

void init_types(void)
{
// type CST_GREEN is nothing
	main_types[CST_GREEN].group=GROUP_BARE;
	main_types[CST_GREEN].size=1;
	main_types[CST_GREEN].graphic=load_graphic(CST_GREEN_G);

	strcpy(type_string[GROUP_POWER_LINE],"Power line");
	main_types[CST_POWERL_H_L].group=GROUP_POWER_LINE;
        main_types[CST_POWERL_H_L].size=1;
        main_types[CST_POWERL_H_L].graphic=load_graphic(CST_POWERL_H_L_G);

	main_types[CST_POWERL_V_L].group=GROUP_POWER_LINE;
        main_types[CST_POWERL_V_L].size=1;
        main_types[CST_POWERL_V_L].graphic=load_graphic(CST_POWERL_V_L_G);

	main_types[CST_POWERL_LD_L].group=GROUP_POWER_LINE;
        main_types[CST_POWERL_LD_L].size=1;
        main_types[CST_POWERL_LD_L].graphic=load_graphic(CST_POWERL_LD_L_G);

        main_types[CST_POWERL_RD_L].group=GROUP_POWER_LINE;
        main_types[CST_POWERL_RD_L].size=1;
        main_types[CST_POWERL_RD_L].graphic=load_graphic(CST_POWERL_RD_L_G);

        main_types[CST_POWERL_LU_L].group=GROUP_POWER_LINE;
        main_types[CST_POWERL_LU_L].size=1;
        main_types[CST_POWERL_LU_L].graphic=load_graphic(CST_POWERL_LU_L_G);

        main_types[CST_POWERL_RU_L].group=GROUP_POWER_LINE;
        main_types[CST_POWERL_RU_L].size=1;
        main_types[CST_POWERL_RU_L].graphic=load_graphic(CST_POWERL_RU_L_G);

        main_types[CST_POWERL_LDU_L].group=GROUP_POWER_LINE;
        main_types[CST_POWERL_LDU_L].size=1;
        main_types[CST_POWERL_LDU_L].graphic=load_graphic(CST_POWERL_LDU_L_G);

        main_types[CST_POWERL_LDR_L].group=GROUP_POWER_LINE;
        main_types[CST_POWERL_LDR_L].size=1;
        main_types[CST_POWERL_LDR_L].graphic=load_graphic(CST_POWERL_LDR_L_G);

        main_types[CST_POWERL_LUR_L].group=GROUP_POWER_LINE;
        main_types[CST_POWERL_LUR_L].size=1;
        main_types[CST_POWERL_LUR_L].graphic=load_graphic(CST_POWERL_LUR_L_G);

        main_types[CST_POWERL_UDR_L].group=GROUP_POWER_LINE;
        main_types[CST_POWERL_UDR_L].size=1;
        main_types[CST_POWERL_UDR_L].graphic=load_graphic(CST_POWERL_UDR_L_G);

        main_types[CST_POWERL_LUDR_L].group=GROUP_POWER_LINE;
        main_types[CST_POWERL_LUDR_L].size=1;
        main_types[CST_POWERL_LUDR_L].graphic
		=load_graphic(CST_POWERL_LUDR_L_G);

        main_types[CST_POWERL_H_D].group=GROUP_POWER_LINE;
        main_types[CST_POWERL_H_D].size=1;
        main_types[CST_POWERL_H_D].graphic=load_graphic(CST_POWERL_H_D_G);

        main_types[CST_POWERL_V_D].group=GROUP_POWER_LINE;
        main_types[CST_POWERL_V_D].size=1;
        main_types[CST_POWERL_V_D].graphic=load_graphic(CST_POWERL_V_D_G);

        main_types[CST_POWERL_LD_D].group=GROUP_POWER_LINE;
        main_types[CST_POWERL_LD_D].size=1;
        main_types[CST_POWERL_LD_D].graphic=load_graphic(CST_POWERL_LD_D_G);

        main_types[CST_POWERL_RD_D].group=GROUP_POWER_LINE;
        main_types[CST_POWERL_RD_D].size=1;
        main_types[CST_POWERL_RD_D].graphic=load_graphic(CST_POWERL_RD_D_G);

        main_types[CST_POWERL_LU_D].group=GROUP_POWER_LINE;
        main_types[CST_POWERL_LU_D].size=1;
        main_types[CST_POWERL_LU_D].graphic=load_graphic(CST_POWERL_LU_D_G);

        main_types[CST_POWERL_RU_D].group=GROUP_POWER_LINE;
        main_types[CST_POWERL_RU_D].size=1;
        main_types[CST_POWERL_RU_D].graphic=load_graphic(CST_POWERL_RU_D_G);

        main_types[CST_POWERL_LDU_D].group=GROUP_POWER_LINE;
        main_types[CST_POWERL_LDU_D].size=1;
        main_types[CST_POWERL_LDU_D].graphic=load_graphic(CST_POWERL_LDU_D_G);

        main_types[CST_POWERL_LDR_D].group=GROUP_POWER_LINE;
        main_types[CST_POWERL_LDR_D].size=1;
        main_types[CST_POWERL_LDR_D].graphic=load_graphic(CST_POWERL_LDR_D_G);

        main_types[CST_POWERL_LUR_D].group=GROUP_POWER_LINE;
        main_types[CST_POWERL_LUR_D].size=1;
        main_types[CST_POWERL_LUR_D].graphic=load_graphic(CST_POWERL_LUR_D_G);

        main_types[CST_POWERL_UDR_D].group=GROUP_POWER_LINE;
        main_types[CST_POWERL_UDR_D].size=1;
        main_types[CST_POWERL_UDR_D].graphic=load_graphic(CST_POWERL_UDR_D_G);

        main_types[CST_POWERL_LUDR_D].group=GROUP_POWER_LINE;
        main_types[CST_POWERL_LUDR_D].size=1;
        main_types[CST_POWERL_LUDR_D].graphic
		=load_graphic(CST_POWERL_LUDR_D_G);

	strcpy(type_string[GROUP_POWER_SOURCE],"Solar PS");
        main_types[CST_POWERS_SOLAR].group=GROUP_POWER_SOURCE;
        main_types[CST_POWERS_SOLAR].size=4;
        main_types[CST_POWERS_SOLAR].graphic=load_graphic(CST_POWERS_SOLAR_G);

	strcpy(type_string[GROUP_POWER_SOURCE_COAL],"Coal PS");
        main_types[CST_POWERS_COAL_EMPTY].group=GROUP_POWER_SOURCE_COAL;
        main_types[CST_POWERS_COAL_EMPTY].size=4;
        main_types[CST_POWERS_COAL_EMPTY].graphic
		=load_graphic(CST_POWERS_COAL_EMPTY_G);

        main_types[CST_POWERS_COAL_LOW].group=GROUP_POWER_SOURCE_COAL;
        main_types[CST_POWERS_COAL_LOW].size=4;
        main_types[CST_POWERS_COAL_LOW].graphic
		=load_graphic(CST_POWERS_COAL_LOW_G);

	prog_box("",12);

        main_types[CST_POWERS_COAL_MED].group=GROUP_POWER_SOURCE_COAL;
        main_types[CST_POWERS_COAL_MED].size=4;
        main_types[CST_POWERS_COAL_MED].graphic
		=load_graphic(CST_POWERS_COAL_MED_G);

        main_types[CST_POWERS_COAL_FULL].group=GROUP_POWER_SOURCE_COAL;
        main_types[CST_POWERS_COAL_FULL].size=4;
        main_types[CST_POWERS_COAL_FULL].graphic
		=load_graphic(CST_POWERS_COAL_FULL_G);

	strcpy(type_string[GROUP_SUBSTATION],"Substation");
        main_types[CST_SUBSTATION_R].group=GROUP_SUBSTATION;
        main_types[CST_SUBSTATION_R].size=2;
        main_types[CST_SUBSTATION_R].graphic=load_graphic(CST_SUBSTATION_R_G);

        main_types[CST_SUBSTATION_G].group=GROUP_SUBSTATION;
        main_types[CST_SUBSTATION_G].size=2;
        main_types[CST_SUBSTATION_G].graphic=load_graphic(CST_SUBSTATION_G_G);

        main_types[CST_SUBSTATION_RG].group=GROUP_SUBSTATION;
        main_types[CST_SUBSTATION_RG].size=2;
        main_types[CST_SUBSTATION_RG].graphic
		=load_graphic(CST_SUBSTATION_RG_G);

	strcpy(type_string[GROUP_RESIDENCE],"Residential");
//        main_types[CST_RESIDENCE_0].group=GROUP_RESIDENCE;
//        main_types[CST_RESIDENCE_0].size=3;
//        main_types[CST_RESIDENCE_0].graphic=load_graphic(CST_RESIDENCE_0_G);

        main_types[CST_RESIDENCE_LL].group=GROUP_RESIDENCE;
        main_types[CST_RESIDENCE_LL].size=3;
        main_types[CST_RESIDENCE_LL].graphic=load_graphic(CST_RESIDENCE_LL_G);

        main_types[CST_RESIDENCE_ML].group=GROUP_RESIDENCE;
        main_types[CST_RESIDENCE_ML].size=3;
        main_types[CST_RESIDENCE_ML].graphic
		=load_graphic(CST_RESIDENCE_ML_G);

        main_types[CST_RESIDENCE_HL].group=GROUP_RESIDENCE;
        main_types[CST_RESIDENCE_HL].size=3;
        main_types[CST_RESIDENCE_HL].graphic
                =load_graphic(CST_RESIDENCE_HL_G);

        main_types[CST_RESIDENCE_LH].group=GROUP_RESIDENCE;
        main_types[CST_RESIDENCE_LH].size=3;
	main_types[CST_RESIDENCE_LH].graphic=load_graphic(CST_RESIDENCE_LH_G);

        main_types[CST_RESIDENCE_MH].group=GROUP_RESIDENCE;
        main_types[CST_RESIDENCE_MH].size=3;
        main_types[CST_RESIDENCE_MH].graphic
                =load_graphic(CST_RESIDENCE_MH_G);

        main_types[CST_RESIDENCE_HH].group=GROUP_RESIDENCE;
        main_types[CST_RESIDENCE_HH].size=3;
        main_types[CST_RESIDENCE_HH].graphic
                =load_graphic(CST_RESIDENCE_HH_G);


	strcpy(type_string[GROUP_ORGANIC_FARM],"Organic farm");
        main_types[CST_FARM_O0].group=GROUP_ORGANIC_FARM;
        main_types[CST_FARM_O0].size=4;
        main_types[CST_FARM_O0].graphic=load_graphic(CST_FARM_O0_G);

        main_types[CST_FARM_O1].group=GROUP_ORGANIC_FARM;
        main_types[CST_FARM_O1].size=4;
        main_types[CST_FARM_O1].graphic=load_graphic(CST_FARM_O1_G);

        main_types[CST_FARM_O2].group=GROUP_ORGANIC_FARM;
        main_types[CST_FARM_O2].size=4;
        main_types[CST_FARM_O2].graphic=load_graphic(CST_FARM_O2_G);

        main_types[CST_FARM_O3].group=GROUP_ORGANIC_FARM;
        main_types[CST_FARM_O3].size=4;
        main_types[CST_FARM_O3].graphic=load_graphic(CST_FARM_O3_G);

        main_types[CST_FARM_O4].group=GROUP_ORGANIC_FARM;
        main_types[CST_FARM_O4].size=4;
        main_types[CST_FARM_O4].graphic=load_graphic(CST_FARM_O4_G);

        main_types[CST_FARM_O5].group=GROUP_ORGANIC_FARM;
        main_types[CST_FARM_O5].size=4;
        main_types[CST_FARM_O5].graphic=load_graphic(CST_FARM_O5_G);

        main_types[CST_FARM_O6].group=GROUP_ORGANIC_FARM;
        main_types[CST_FARM_O6].size=4;
        main_types[CST_FARM_O6].graphic=load_graphic(CST_FARM_O6_G);

        main_types[CST_FARM_O7].group=GROUP_ORGANIC_FARM;
        main_types[CST_FARM_O7].size=4;
        main_types[CST_FARM_O7].graphic=load_graphic(CST_FARM_O7_G);

        main_types[CST_FARM_O8].group=GROUP_ORGANIC_FARM;
        main_types[CST_FARM_O8].size=4;
        main_types[CST_FARM_O8].graphic=load_graphic(CST_FARM_O8_G);

        main_types[CST_FARM_O9].group=GROUP_ORGANIC_FARM;
        main_types[CST_FARM_O9].size=4;
        main_types[CST_FARM_O9].graphic=load_graphic(CST_FARM_O9_G);

        main_types[CST_FARM_O10].group=GROUP_ORGANIC_FARM;
        main_types[CST_FARM_O10].size=4;
        main_types[CST_FARM_O10].graphic=load_graphic(CST_FARM_O10_G);

        main_types[CST_FARM_O11].group=GROUP_ORGANIC_FARM;
        main_types[CST_FARM_O11].size=4;
        main_types[CST_FARM_O11].graphic=load_graphic(CST_FARM_O11_G);

        main_types[CST_FARM_O12].group=GROUP_ORGANIC_FARM;
        main_types[CST_FARM_O12].size=4;
        main_types[CST_FARM_O12].graphic=load_graphic(CST_FARM_O12_G);

        main_types[CST_FARM_O13].group=GROUP_ORGANIC_FARM;
        main_types[CST_FARM_O13].size=4;
        main_types[CST_FARM_O13].graphic=load_graphic(CST_FARM_O13_G);

        main_types[CST_FARM_O14].group=GROUP_ORGANIC_FARM;
        main_types[CST_FARM_O14].size=4;
        main_types[CST_FARM_O14].graphic=load_graphic(CST_FARM_O14_G);

        main_types[CST_FARM_O15].group=GROUP_ORGANIC_FARM;
        main_types[CST_FARM_O15].size=4;
        main_types[CST_FARM_O15].graphic=load_graphic(CST_FARM_O15_G);

        main_types[CST_FARM_O16].group=GROUP_ORGANIC_FARM;
        main_types[CST_FARM_O16].size=4;
        main_types[CST_FARM_O16].graphic=load_graphic(CST_FARM_O16_G);


	strcpy(type_string[GROUP_MARKET],"Market");
        main_types[CST_MARKET_EMPTY].group=GROUP_MARKET;
        main_types[CST_MARKET_EMPTY].size=2;
        main_types[CST_MARKET_EMPTY].graphic
		=load_graphic(CST_MARKET_EMPTY_G);

        main_types[CST_MARKET_LOW].group=GROUP_MARKET;
        main_types[CST_MARKET_LOW].size=2;
        main_types[CST_MARKET_LOW].graphic
                =load_graphic(CST_MARKET_LOW_G);

        main_types[CST_MARKET_MED].group=GROUP_MARKET;
        main_types[CST_MARKET_MED].size=2;
        main_types[CST_MARKET_MED].graphic
                =load_graphic(CST_MARKET_MED_G);

        main_types[CST_MARKET_FULL].group=GROUP_MARKET;
        main_types[CST_MARKET_FULL].size=2;
        main_types[CST_MARKET_FULL].graphic
                =load_graphic(CST_MARKET_FULL_G);

	strcpy(type_string[GROUP_TRACK],"Track");
        main_types[CST_TRACK_LR].group=GROUP_TRACK;
        main_types[CST_TRACK_LR].size=1;
        main_types[CST_TRACK_LR].graphic=load_graphic(CST_TRACK_LR_G);

        main_types[CST_TRACK_LU].group=GROUP_TRACK;
        main_types[CST_TRACK_LU].size=1;
        main_types[CST_TRACK_LU].graphic=load_graphic(CST_TRACK_LU_G);

        main_types[CST_TRACK_LD].group=GROUP_TRACK;
        main_types[CST_TRACK_LD].size=1;
        main_types[CST_TRACK_LD].graphic=load_graphic(CST_TRACK_LD_G);

        main_types[CST_TRACK_UD].group=GROUP_TRACK;
        main_types[CST_TRACK_UD].size=1;
        main_types[CST_TRACK_UD].graphic=load_graphic(CST_TRACK_UD_G);

        main_types[CST_TRACK_UR].group=GROUP_TRACK;
        main_types[CST_TRACK_UR].size=1;
        main_types[CST_TRACK_UR].graphic=load_graphic(CST_TRACK_UR_G);

        main_types[CST_TRACK_DR].group=GROUP_TRACK;
        main_types[CST_TRACK_DR].size=1;
        main_types[CST_TRACK_DR].graphic=load_graphic(CST_TRACK_DR_G);

        main_types[CST_TRACK_LUR].group=GROUP_TRACK;
        main_types[CST_TRACK_LUR].size=1;
        main_types[CST_TRACK_LUR].graphic=load_graphic(CST_TRACK_LUR_G);

        main_types[CST_TRACK_LDR].group=GROUP_TRACK;
        main_types[CST_TRACK_LDR].size=1;
        main_types[CST_TRACK_LDR].graphic=load_graphic(CST_TRACK_LDR_G);

        main_types[CST_TRACK_LUD].group=GROUP_TRACK;
        main_types[CST_TRACK_LUD].size=1;
        main_types[CST_TRACK_LUD].graphic=load_graphic(CST_TRACK_LUD_G);

        main_types[CST_TRACK_UDR].group=GROUP_TRACK;
        main_types[CST_TRACK_UDR].size=1;
        main_types[CST_TRACK_UDR].graphic=load_graphic(CST_TRACK_UDR_G);

        main_types[CST_TRACK_LUDR].group=GROUP_TRACK;
        main_types[CST_TRACK_LUDR].size=1;
        main_types[CST_TRACK_LUDR].graphic=load_graphic(CST_TRACK_LUDR_G);

	strcpy(type_string[GROUP_COALMINE],"Coalmine");
        main_types[CST_COALMINE_EMPTY].group=GROUP_COALMINE;
        main_types[CST_COALMINE_EMPTY].size=4;
        main_types[CST_COALMINE_EMPTY].graphic
		=load_graphic(CST_COALMINE_EMPTY_G);

	prog_box("",25);

        main_types[CST_COALMINE_LOW].group=GROUP_COALMINE;
        main_types[CST_COALMINE_LOW].size=4;
        main_types[CST_COALMINE_LOW].graphic=load_graphic(CST_COALMINE_LOW_G);

        main_types[CST_COALMINE_MED].group=GROUP_COALMINE;
        main_types[CST_COALMINE_MED].size=4;
        main_types[CST_COALMINE_MED].graphic=load_graphic(CST_COALMINE_MED_G);

        main_types[CST_COALMINE_FULL].group=GROUP_COALMINE;
        main_types[CST_COALMINE_FULL].size=4;
        main_types[CST_COALMINE_FULL].graphic
		=load_graphic(CST_COALMINE_FULL_G);


	strcpy(type_string[GROUP_RAIL],"Railway");
        main_types[CST_RAIL_LR].group=GROUP_RAIL;
        main_types[CST_RAIL_LR].size=1;
        main_types[CST_RAIL_LR].graphic=load_graphic(CST_RAIL_LR_G);

        main_types[CST_RAIL_LU].group=GROUP_RAIL;
        main_types[CST_RAIL_LU].size=1;
        main_types[CST_RAIL_LU].graphic=load_graphic(CST_RAIL_LU_G);

        main_types[CST_RAIL_LD].group=GROUP_RAIL;
        main_types[CST_RAIL_LD].size=1;
        main_types[CST_RAIL_LD].graphic=load_graphic(CST_RAIL_LD_G);

        main_types[CST_RAIL_UD].group=GROUP_RAIL;
        main_types[CST_RAIL_UD].size=1;
        main_types[CST_RAIL_UD].graphic=load_graphic(CST_RAIL_UD_G);

        main_types[CST_RAIL_UR].group=GROUP_RAIL;
        main_types[CST_RAIL_UR].size=1;
        main_types[CST_RAIL_UR].graphic=load_graphic(CST_RAIL_UR_G);

        main_types[CST_RAIL_DR].group=GROUP_RAIL;
        main_types[CST_RAIL_DR].size=1;
        main_types[CST_RAIL_DR].graphic=load_graphic(CST_RAIL_DR_G);

        main_types[CST_RAIL_LUR].group=GROUP_RAIL;
        main_types[CST_RAIL_LUR].size=1;
        main_types[CST_RAIL_LUR].graphic=load_graphic(CST_RAIL_LUR_G);

        main_types[CST_RAIL_LDR].group=GROUP_RAIL;
        main_types[CST_RAIL_LDR].size=1;
        main_types[CST_RAIL_LDR].graphic=load_graphic(CST_RAIL_LDR_G);

        main_types[CST_RAIL_LUD].group=GROUP_RAIL;
        main_types[CST_RAIL_LUD].size=1;
        main_types[CST_RAIL_LUD].graphic=load_graphic(CST_RAIL_LUD_G);

	prog_box("",30);

        main_types[CST_RAIL_UDR].group=GROUP_RAIL;
        main_types[CST_RAIL_UDR].size=1;
        main_types[CST_RAIL_UDR].graphic=load_graphic(CST_RAIL_UDR_G);

        main_types[CST_RAIL_LUDR].group=GROUP_RAIL;
        main_types[CST_RAIL_LUDR].size=1;
        main_types[CST_RAIL_LUDR].graphic=load_graphic(CST_RAIL_LUDR_G);

	strcpy(type_string[GROUP_ROAD],"Road");
        main_types[CST_ROAD_LR].group=GROUP_ROAD;
        main_types[CST_ROAD_LR].size=1;
        main_types[CST_ROAD_LR].graphic=load_graphic(CST_ROAD_LR_G);

        main_types[CST_ROAD_LU].group=GROUP_ROAD;
        main_types[CST_ROAD_LU].size=1;
        main_types[CST_ROAD_LU].graphic=load_graphic(CST_ROAD_LU_G);

        main_types[CST_ROAD_LD].group=GROUP_ROAD;
        main_types[CST_ROAD_LD].size=1;
        main_types[CST_ROAD_LD].graphic=load_graphic(CST_ROAD_LD_G);

        main_types[CST_ROAD_UD].group=GROUP_ROAD;
        main_types[CST_ROAD_UD].size=1;
        main_types[CST_ROAD_UD].graphic=load_graphic(CST_ROAD_UD_G);

        main_types[CST_ROAD_UR].group=GROUP_ROAD;
        main_types[CST_ROAD_UR].size=1;
        main_types[CST_ROAD_UR].graphic=load_graphic(CST_ROAD_UR_G);

        main_types[CST_ROAD_DR].group=GROUP_ROAD;
        main_types[CST_ROAD_DR].size=1;
        main_types[CST_ROAD_DR].graphic=load_graphic(CST_ROAD_DR_G);

        main_types[CST_ROAD_LUR].group=GROUP_ROAD;
        main_types[CST_ROAD_LUR].size=1;
        main_types[CST_ROAD_LUR].graphic=load_graphic(CST_ROAD_LUR_G);

        main_types[CST_ROAD_LDR].group=GROUP_ROAD;
        main_types[CST_ROAD_LDR].size=1;
        main_types[CST_ROAD_LDR].graphic=load_graphic(CST_ROAD_LDR_G);

        main_types[CST_ROAD_LUD].group=GROUP_ROAD;
        main_types[CST_ROAD_LUD].size=1;
        main_types[CST_ROAD_LUD].graphic=load_graphic(CST_ROAD_LUD_G);

        main_types[CST_ROAD_UDR].group=GROUP_ROAD;
        main_types[CST_ROAD_UDR].size=1;
        main_types[CST_ROAD_UDR].graphic=load_graphic(CST_ROAD_UDR_G);

        main_types[CST_ROAD_LUDR].group=GROUP_ROAD;
        main_types[CST_ROAD_LUDR].size=1;
        main_types[CST_ROAD_LUDR].graphic=load_graphic(CST_ROAD_LUDR_G);

	strcpy(type_string[GROUP_INDUSTRY_L],"Lt. Industry");
        main_types[CST_INDUSTRY_L_C].group=GROUP_INDUSTRY_L;
        main_types[CST_INDUSTRY_L_C].size=3;
        main_types[CST_INDUSTRY_L_C].graphic
		=load_graphic(CST_INDUSTRY_L_C_G);

        main_types[CST_INDUSTRY_L_Q1].group=GROUP_INDUSTRY_L;
        main_types[CST_INDUSTRY_L_Q1].size=3;
        main_types[CST_INDUSTRY_L_Q1].graphic
                =load_graphic(CST_INDUSTRY_L_Q1_G);

        main_types[CST_INDUSTRY_L_Q2].group=GROUP_INDUSTRY_L;
        main_types[CST_INDUSTRY_L_Q2].size=3;
        main_types[CST_INDUSTRY_L_Q2].graphic
                =load_graphic(CST_INDUSTRY_L_Q2_G);

        main_types[CST_INDUSTRY_L_Q3].group=GROUP_INDUSTRY_L;
        main_types[CST_INDUSTRY_L_Q3].size=3;
        main_types[CST_INDUSTRY_L_Q3].graphic
                =load_graphic(CST_INDUSTRY_L_Q3_G);

        main_types[CST_INDUSTRY_L_Q4].group=GROUP_INDUSTRY_L;
        main_types[CST_INDUSTRY_L_Q4].size=3;
        main_types[CST_INDUSTRY_L_Q4].graphic
                =load_graphic(CST_INDUSTRY_L_Q4_G);

        main_types[CST_INDUSTRY_L_L1].group=GROUP_INDUSTRY_L;
        main_types[CST_INDUSTRY_L_L1].size=3;
        main_types[CST_INDUSTRY_L_L1].graphic
		=load_graphic(CST_INDUSTRY_L_L1_G);

        main_types[CST_INDUSTRY_L_L2].group=GROUP_INDUSTRY_L;
        main_types[CST_INDUSTRY_L_L2].size=3;
        main_types[CST_INDUSTRY_L_L2].graphic
                =load_graphic(CST_INDUSTRY_L_L2_G);

        main_types[CST_INDUSTRY_L_L3].group=GROUP_INDUSTRY_L;
        main_types[CST_INDUSTRY_L_L3].size=3;
        main_types[CST_INDUSTRY_L_L3].graphic
                =load_graphic(CST_INDUSTRY_L_L3_G);

        main_types[CST_INDUSTRY_L_L4].group=GROUP_INDUSTRY_L;
        main_types[CST_INDUSTRY_L_L4].size=3;
        main_types[CST_INDUSTRY_L_L4].graphic
                =load_graphic(CST_INDUSTRY_L_L4_G);


        main_types[CST_INDUSTRY_L_M1].group=GROUP_INDUSTRY_L;
        main_types[CST_INDUSTRY_L_M1].size=3;
        main_types[CST_INDUSTRY_L_M1].graphic
		=load_graphic(CST_INDUSTRY_L_M1_G);

        main_types[CST_INDUSTRY_L_M2].group=GROUP_INDUSTRY_L;
        main_types[CST_INDUSTRY_L_M2].size=3;
        main_types[CST_INDUSTRY_L_M2].graphic
                =load_graphic(CST_INDUSTRY_L_M2_G);

        main_types[CST_INDUSTRY_L_M3].group=GROUP_INDUSTRY_L;
        main_types[CST_INDUSTRY_L_M3].size=3;
        main_types[CST_INDUSTRY_L_M3].graphic
                =load_graphic(CST_INDUSTRY_L_M3_G);

        main_types[CST_INDUSTRY_L_M4].group=GROUP_INDUSTRY_L;
        main_types[CST_INDUSTRY_L_M4].size=3;
        main_types[CST_INDUSTRY_L_M4].graphic
                =load_graphic(CST_INDUSTRY_L_M4_G);


        main_types[CST_INDUSTRY_L_H1].group=GROUP_INDUSTRY_L;
        main_types[CST_INDUSTRY_L_H1].size=3;
        main_types[CST_INDUSTRY_L_H1].graphic
		=load_graphic(CST_INDUSTRY_L_H1_G);

        main_types[CST_INDUSTRY_L_H2].group=GROUP_INDUSTRY_L;
        main_types[CST_INDUSTRY_L_H2].size=3;
        main_types[CST_INDUSTRY_L_H2].graphic
                =load_graphic(CST_INDUSTRY_L_H2_G);

        main_types[CST_INDUSTRY_L_H3].group=GROUP_INDUSTRY_L;
        main_types[CST_INDUSTRY_L_H3].size=3;
        main_types[CST_INDUSTRY_L_H3].graphic
                =load_graphic(CST_INDUSTRY_L_H3_G);

        main_types[CST_INDUSTRY_L_H4].group=GROUP_INDUSTRY_L;
        main_types[CST_INDUSTRY_L_H4].size=3;
        main_types[CST_INDUSTRY_L_H4].graphic
                =load_graphic(CST_INDUSTRY_L_H4_G);



	strcpy(type_string[GROUP_COMMUNE],"Commune");
        main_types[CST_COMMUNE_1].group=GROUP_COMMUNE;
        main_types[CST_COMMUNE_1].size=4;
        main_types[CST_COMMUNE_1].graphic=load_graphic(CST_COMMUNE_1_G);

        main_types[CST_COMMUNE_2].group=GROUP_COMMUNE;
        main_types[CST_COMMUNE_2].size=4;
        main_types[CST_COMMUNE_2].graphic=load_graphic(CST_COMMUNE_2_G);

        main_types[CST_COMMUNE_3].group=GROUP_COMMUNE;
        main_types[CST_COMMUNE_3].size=4;
        main_types[CST_COMMUNE_3].graphic=load_graphic(CST_COMMUNE_3_G);

        main_types[CST_COMMUNE_4].group=GROUP_COMMUNE;
        main_types[CST_COMMUNE_4].size=4;
        main_types[CST_COMMUNE_4].graphic=load_graphic(CST_COMMUNE_4_G);

        main_types[CST_COMMUNE_5].group=GROUP_COMMUNE;
        main_types[CST_COMMUNE_5].size=4;
        main_types[CST_COMMUNE_5].graphic=load_graphic(CST_COMMUNE_5_G);

        main_types[CST_COMMUNE_6].group=GROUP_COMMUNE;
        main_types[CST_COMMUNE_6].size=4;
        main_types[CST_COMMUNE_6].graphic=load_graphic(CST_COMMUNE_6_G);

        main_types[CST_COMMUNE_7].group=GROUP_COMMUNE;
        main_types[CST_COMMUNE_7].size=4;
        main_types[CST_COMMUNE_7].graphic=load_graphic(CST_COMMUNE_7_G);

        main_types[CST_COMMUNE_8].group=GROUP_COMMUNE;
        main_types[CST_COMMUNE_8].size=4;
        main_types[CST_COMMUNE_8].graphic=load_graphic(CST_COMMUNE_8_G);

        main_types[CST_COMMUNE_9].group=GROUP_COMMUNE;
        main_types[CST_COMMUNE_9].size=4;
        main_types[CST_COMMUNE_9].graphic=load_graphic(CST_COMMUNE_9_G);

        main_types[CST_COMMUNE_10].group=GROUP_COMMUNE;
        main_types[CST_COMMUNE_10].size=4;
        main_types[CST_COMMUNE_10].graphic=load_graphic(CST_COMMUNE_10_G);

        main_types[CST_COMMUNE_11].group=GROUP_COMMUNE;
        main_types[CST_COMMUNE_11].size=4;
        main_types[CST_COMMUNE_11].graphic=load_graphic(CST_COMMUNE_11_G);

        main_types[CST_COMMUNE_12].group=GROUP_COMMUNE;
        main_types[CST_COMMUNE_12].size=4;
        main_types[CST_COMMUNE_12].graphic=load_graphic(CST_COMMUNE_12_G);

        main_types[CST_COMMUNE_13].group=GROUP_COMMUNE;
        main_types[CST_COMMUNE_13].size=4;
        main_types[CST_COMMUNE_13].graphic=load_graphic(CST_COMMUNE_13_G);

        main_types[CST_COMMUNE_14].group=GROUP_COMMUNE;
        main_types[CST_COMMUNE_14].size=4;
        main_types[CST_COMMUNE_14].graphic=load_graphic(CST_COMMUNE_14_G);


	strcpy(type_string[GROUP_UNIVERSITY],"University");
        main_types[CST_UNIVERSITY].group=GROUP_UNIVERSITY;
        main_types[CST_UNIVERSITY].size=3;
        main_types[CST_UNIVERSITY].graphic=load_graphic(CST_UNIVERSITY_G);

	strcpy(type_string[GROUP_OREMINE],"Ore mine");
        main_types[CST_OREMINE_1].group=GROUP_OREMINE;
        main_types[CST_OREMINE_1].size=4;
        main_types[CST_OREMINE_1].graphic=load_graphic(CST_OREMINE_1_G);

        main_types[CST_OREMINE_2].group=GROUP_OREMINE;
        main_types[CST_OREMINE_2].size=4;
        main_types[CST_OREMINE_2].graphic=load_graphic(CST_OREMINE_2_G);

        main_types[CST_OREMINE_3].group=GROUP_OREMINE;
        main_types[CST_OREMINE_3].size=4;
        main_types[CST_OREMINE_3].graphic=load_graphic(CST_OREMINE_3_G);

        main_types[CST_OREMINE_4].group=GROUP_OREMINE;
        main_types[CST_OREMINE_4].size=4;
        main_types[CST_OREMINE_4].graphic=load_graphic(CST_OREMINE_4_G);

        main_types[CST_OREMINE_5].group=GROUP_OREMINE;
        main_types[CST_OREMINE_5].size=4;
        main_types[CST_OREMINE_5].graphic=load_graphic(CST_OREMINE_5_G);

        main_types[CST_OREMINE_6].group=GROUP_OREMINE;
        main_types[CST_OREMINE_6].size=4;
        main_types[CST_OREMINE_6].graphic=load_graphic(CST_OREMINE_6_G);

        main_types[CST_OREMINE_7].group=GROUP_OREMINE;
        main_types[CST_OREMINE_7].size=4;
        main_types[CST_OREMINE_7].graphic=load_graphic(CST_OREMINE_7_G);

        main_types[CST_OREMINE_8].group=GROUP_OREMINE;
        main_types[CST_OREMINE_8].size=4;
        main_types[CST_OREMINE_8].graphic=load_graphic(CST_OREMINE_8_G);

	strcpy(type_string[GROUP_TIP],"Rubbish tip");
        main_types[CST_TIP_0].group=GROUP_TIP;
        main_types[CST_TIP_0].size=4;
        main_types[CST_TIP_0].graphic=load_graphic(CST_TIP_0_G);

        main_types[CST_TIP_1].group=GROUP_TIP;
        main_types[CST_TIP_1].size=4;
        main_types[CST_TIP_1].graphic=load_graphic(CST_TIP_1_G);

        main_types[CST_TIP_2].group=GROUP_TIP;
        main_types[CST_TIP_2].size=4;
        main_types[CST_TIP_2].graphic=load_graphic(CST_TIP_2_G);

        main_types[CST_TIP_3].group=GROUP_TIP;
        main_types[CST_TIP_3].size=4;
        main_types[CST_TIP_3].graphic=load_graphic(CST_TIP_3_G);

        main_types[CST_TIP_4].group=GROUP_TIP;
        main_types[CST_TIP_4].size=4;
        main_types[CST_TIP_4].graphic=load_graphic(CST_TIP_4_G);

        main_types[CST_TIP_5].group=GROUP_TIP;
        main_types[CST_TIP_5].size=4;
        main_types[CST_TIP_5].graphic=load_graphic(CST_TIP_5_G);

        main_types[CST_TIP_6].group=GROUP_TIP;
        main_types[CST_TIP_6].size=4;
        main_types[CST_TIP_6].graphic=load_graphic(CST_TIP_6_G);

        main_types[CST_TIP_7].group=GROUP_TIP;
        main_types[CST_TIP_7].size=4;
        main_types[CST_TIP_7].graphic=load_graphic(CST_TIP_7_G);

        main_types[CST_TIP_8].group=GROUP_TIP;
        main_types[CST_TIP_8].size=4;
        main_types[CST_TIP_8].graphic=load_graphic(CST_TIP_8_G);


        strcpy(type_string[GROUP_PORT],"Port");
        main_types[CST_EX_PORT].group=GROUP_PORT;
        main_types[CST_EX_PORT].size=4;
        main_types[CST_EX_PORT].graphic=load_graphic(CST_EX_PORT_G);


	strcpy(type_string[GROUP_INDUSTRY_H],"Hv. Industry");
        main_types[CST_INDUSTRY_H_C].group=GROUP_INDUSTRY_H;
        main_types[CST_INDUSTRY_H_C].size=4;
        main_types[CST_INDUSTRY_H_C].graphic
		=load_graphic(CST_INDUSTRY_H_C_G);

        main_types[CST_INDUSTRY_H_L1].group=GROUP_INDUSTRY_H;
        main_types[CST_INDUSTRY_H_L1].size=4;
        main_types[CST_INDUSTRY_H_L1].graphic
		=load_graphic(CST_INDUSTRY_H_L1_G);

        main_types[CST_INDUSTRY_H_L2].group=GROUP_INDUSTRY_H;
        main_types[CST_INDUSTRY_H_L2].size=4;
        main_types[CST_INDUSTRY_H_L2].graphic
                =load_graphic(CST_INDUSTRY_H_L2_G);

        main_types[CST_INDUSTRY_H_L3].group=GROUP_INDUSTRY_H;
        main_types[CST_INDUSTRY_H_L3].size=4;
        main_types[CST_INDUSTRY_H_L3].graphic
                =load_graphic(CST_INDUSTRY_H_L3_G);

        main_types[CST_INDUSTRY_H_L4].group=GROUP_INDUSTRY_H;
        main_types[CST_INDUSTRY_H_L4].size=4;
        main_types[CST_INDUSTRY_H_L4].graphic
                =load_graphic(CST_INDUSTRY_H_L4_G);

        main_types[CST_INDUSTRY_H_L5].group=GROUP_INDUSTRY_H;
        main_types[CST_INDUSTRY_H_L5].size=4;
        main_types[CST_INDUSTRY_H_L5].graphic
                =load_graphic(CST_INDUSTRY_H_L5_G);

        main_types[CST_INDUSTRY_H_L6].group=GROUP_INDUSTRY_H;
        main_types[CST_INDUSTRY_H_L6].size=4;
        main_types[CST_INDUSTRY_H_L6].graphic
                =load_graphic(CST_INDUSTRY_H_L6_G);

        main_types[CST_INDUSTRY_H_L7].group=GROUP_INDUSTRY_H;
        main_types[CST_INDUSTRY_H_L7].size=4;
        main_types[CST_INDUSTRY_H_L7].graphic
                =load_graphic(CST_INDUSTRY_H_L7_G);

        main_types[CST_INDUSTRY_H_L8].group=GROUP_INDUSTRY_H;
        main_types[CST_INDUSTRY_H_L8].size=4;
        main_types[CST_INDUSTRY_H_L8].graphic
                =load_graphic(CST_INDUSTRY_H_L8_G);


        main_types[CST_INDUSTRY_H_M1].group=GROUP_INDUSTRY_H;
        main_types[CST_INDUSTRY_H_M1].size=4;
        main_types[CST_INDUSTRY_H_M1].graphic
		=load_graphic(CST_INDUSTRY_H_M1_G);

        main_types[CST_INDUSTRY_H_M2].group=GROUP_INDUSTRY_H;
        main_types[CST_INDUSTRY_H_M2].size=4;
        main_types[CST_INDUSTRY_H_M2].graphic
                =load_graphic(CST_INDUSTRY_H_M2_G);

        main_types[CST_INDUSTRY_H_M3].group=GROUP_INDUSTRY_H;
        main_types[CST_INDUSTRY_H_M3].size=4;
        main_types[CST_INDUSTRY_H_M3].graphic
                =load_graphic(CST_INDUSTRY_H_M3_G);

        main_types[CST_INDUSTRY_H_M4].group=GROUP_INDUSTRY_H;
        main_types[CST_INDUSTRY_H_M4].size=4;
        main_types[CST_INDUSTRY_H_M4].graphic
                =load_graphic(CST_INDUSTRY_H_M4_G);

        main_types[CST_INDUSTRY_H_M5].group=GROUP_INDUSTRY_H;
        main_types[CST_INDUSTRY_H_M5].size=4;
        main_types[CST_INDUSTRY_H_M5].graphic
                =load_graphic(CST_INDUSTRY_H_M5_G);

        main_types[CST_INDUSTRY_H_M6].group=GROUP_INDUSTRY_H;
        main_types[CST_INDUSTRY_H_M6].size=4;
        main_types[CST_INDUSTRY_H_M6].graphic
                =load_graphic(CST_INDUSTRY_H_M6_G);

        main_types[CST_INDUSTRY_H_M7].group=GROUP_INDUSTRY_H;
        main_types[CST_INDUSTRY_H_M7].size=4;
        main_types[CST_INDUSTRY_H_M7].graphic
                =load_graphic(CST_INDUSTRY_H_M7_G);

        main_types[CST_INDUSTRY_H_M8].group=GROUP_INDUSTRY_H;
        main_types[CST_INDUSTRY_H_M8].size=4;
        main_types[CST_INDUSTRY_H_M8].graphic
                =load_graphic(CST_INDUSTRY_H_M8_G);

        main_types[CST_INDUSTRY_H_H1].group=GROUP_INDUSTRY_H;
        main_types[CST_INDUSTRY_H_H1].size=4;
        main_types[CST_INDUSTRY_H_H1].graphic
		=load_graphic(CST_INDUSTRY_H_H1_G);

        main_types[CST_INDUSTRY_H_H2].group=GROUP_INDUSTRY_H;
        main_types[CST_INDUSTRY_H_H2].size=4;
        main_types[CST_INDUSTRY_H_H2].graphic
                =load_graphic(CST_INDUSTRY_H_H2_G);

        main_types[CST_INDUSTRY_H_H3].group=GROUP_INDUSTRY_H;
        main_types[CST_INDUSTRY_H_H3].size=4;
        main_types[CST_INDUSTRY_H_H3].graphic
                =load_graphic(CST_INDUSTRY_H_H3_G);

        main_types[CST_INDUSTRY_H_H4].group=GROUP_INDUSTRY_H;
        main_types[CST_INDUSTRY_H_H4].size=4;
        main_types[CST_INDUSTRY_H_H4].graphic
                =load_graphic(CST_INDUSTRY_H_H4_G);

        main_types[CST_INDUSTRY_H_H5].group=GROUP_INDUSTRY_H;
        main_types[CST_INDUSTRY_H_H5].size=4;
        main_types[CST_INDUSTRY_H_H5].graphic
                =load_graphic(CST_INDUSTRY_H_H5_G);

        main_types[CST_INDUSTRY_H_H6].group=GROUP_INDUSTRY_H;
        main_types[CST_INDUSTRY_H_H6].size=4;
        main_types[CST_INDUSTRY_H_H6].graphic
                =load_graphic(CST_INDUSTRY_H_H6_G);

        main_types[CST_INDUSTRY_H_H7].group=GROUP_INDUSTRY_H;
        main_types[CST_INDUSTRY_H_H7].size=4;
        main_types[CST_INDUSTRY_H_H7].graphic
                =load_graphic(CST_INDUSTRY_H_H7_G);

        main_types[CST_INDUSTRY_H_H8].group=GROUP_INDUSTRY_H;
        main_types[CST_INDUSTRY_H_H8].size=4;
        main_types[CST_INDUSTRY_H_H8].graphic
                =load_graphic(CST_INDUSTRY_H_H8_G);


	strcpy(type_string[GROUP_PARKLAND],"Park");
        main_types[CST_PARKLAND_PLANE].group=GROUP_PARKLAND;
        main_types[CST_PARKLAND_PLANE].size=1;
        main_types[CST_PARKLAND_PLANE].graphic
		=load_graphic(CST_PARKLAND_PLANE_G);

	prog_box("",50);

        main_types[CST_PARKLAND_LAKE].group=GROUP_PARKLAND;
        main_types[CST_PARKLAND_LAKE].size=1;
        main_types[CST_PARKLAND_LAKE].graphic
		=load_graphic(CST_PARKLAND_LAKE_G);

	strcpy(type_string[GROUP_RECYCLE],"Recycle");
        main_types[CST_RECYCLE].group=GROUP_RECYCLE;
        main_types[CST_RECYCLE].size=2;
        main_types[CST_RECYCLE].graphic=load_graphic(CST_RECYCLE_G);

	strcpy(type_string[GROUP_WATER],"Water");
        main_types[CST_WATER].group=GROUP_WATER;
        main_types[CST_WATER].size=1;
        main_types[CST_WATER].graphic=load_graphic(CST_WATER_G);

        main_types[CST_WATER_D].group=GROUP_WATER;
        main_types[CST_WATER_D].size=1;
        main_types[CST_WATER_D].graphic=load_graphic(CST_WATER_D_G);

        main_types[CST_WATER_R].group=GROUP_WATER;
        main_types[CST_WATER_R].size=1;
        main_types[CST_WATER_R].graphic=load_graphic(CST_WATER_R_G);

        main_types[CST_WATER_U].group=GROUP_WATER;
        main_types[CST_WATER_U].size=1;
        main_types[CST_WATER_U].graphic=load_graphic(CST_WATER_U_G);

        main_types[CST_WATER_L].group=GROUP_WATER;
        main_types[CST_WATER_L].size=1;
        main_types[CST_WATER_L].graphic=load_graphic(CST_WATER_L_G);

        main_types[CST_WATER_LR].group=GROUP_WATER;
	main_types[CST_WATER_LR].size=1;
        main_types[CST_WATER_LR].graphic=load_graphic(CST_WATER_LR_G);

        main_types[CST_WATER_UD].group=GROUP_WATER;
        main_types[CST_WATER_UD].size=1;
        main_types[CST_WATER_UD].graphic=load_graphic(CST_WATER_UD_G);

        main_types[CST_WATER_LD].group=GROUP_WATER;
        main_types[CST_WATER_LD].size=1;
        main_types[CST_WATER_LD].graphic=load_graphic(CST_WATER_LD_G);

        main_types[CST_WATER_RD].group=GROUP_WATER;
        main_types[CST_WATER_RD].size=1;
        main_types[CST_WATER_RD].graphic=load_graphic(CST_WATER_RD_G);

        main_types[CST_WATER_LU].group=GROUP_WATER;
        main_types[CST_WATER_LU].size=1;
        main_types[CST_WATER_LU].graphic=load_graphic(CST_WATER_LU_G);

        main_types[CST_WATER_UR].group=GROUP_WATER;
        main_types[CST_WATER_UR].size=1;
        main_types[CST_WATER_UR].graphic=load_graphic(CST_WATER_UR_G);

        main_types[CST_WATER_LUD].group=GROUP_WATER;
        main_types[CST_WATER_LUD].size=1;
        main_types[CST_WATER_LUD].graphic=load_graphic(CST_WATER_LUD_G);

        main_types[CST_WATER_LRD].group=GROUP_WATER;
        main_types[CST_WATER_LRD].size=1;
        main_types[CST_WATER_LRD].graphic=load_graphic(CST_WATER_LRD_G);

        main_types[CST_WATER_LUR].group=GROUP_WATER;
        main_types[CST_WATER_LUR].size=1;
        main_types[CST_WATER_LUR].graphic=load_graphic(CST_WATER_LUR_G);

        main_types[CST_WATER_URD].group=GROUP_WATER;
        main_types[CST_WATER_URD].size=1;
        main_types[CST_WATER_URD].graphic=load_graphic(CST_WATER_URD_G);

        main_types[CST_WATER_LURD].group=GROUP_WATER;
        main_types[CST_WATER_LURD].size=1;
        main_types[CST_WATER_LURD].graphic=load_graphic(CST_WATER_LURD_G);


	strcpy(type_string[GROUP_HEALTH],"Health centre");
        main_types[CST_HEALTH].group=GROUP_HEALTH;
        main_types[CST_HEALTH].size=2;
        main_types[CST_HEALTH].graphic=load_graphic(CST_HEALTH_G);

	strcpy(type_string[GROUP_ROCKET],"Rocket pad");
        main_types[CST_ROCKET_1].group=GROUP_ROCKET;
        main_types[CST_ROCKET_1].size=4;
        main_types[CST_ROCKET_1].graphic=load_graphic(CST_ROCKET_1_G);

        main_types[CST_ROCKET_2].group=GROUP_ROCKET;
        main_types[CST_ROCKET_2].size=4;
        main_types[CST_ROCKET_2].graphic=load_graphic(CST_ROCKET_2_G);

        main_types[CST_ROCKET_3].group=GROUP_ROCKET;
        main_types[CST_ROCKET_3].size=4;
        main_types[CST_ROCKET_3].graphic=load_graphic(CST_ROCKET_3_G);

        main_types[CST_ROCKET_4].group=GROUP_ROCKET;
        main_types[CST_ROCKET_4].size=4;
        main_types[CST_ROCKET_4].graphic=load_graphic(CST_ROCKET_4_G);

	prog_box("",60);

        main_types[CST_ROCKET_5].group=GROUP_ROCKET;
        main_types[CST_ROCKET_5].size=4;
        main_types[CST_ROCKET_5].graphic=load_graphic(CST_ROCKET_5_G);

        main_types[CST_ROCKET_6].group=GROUP_ROCKET;
        main_types[CST_ROCKET_6].size=4;
        main_types[CST_ROCKET_6].graphic=load_graphic(CST_ROCKET_6_G);

        main_types[CST_ROCKET_7].group=GROUP_ROCKET;
        main_types[CST_ROCKET_7].size=4;
        main_types[CST_ROCKET_7].graphic=load_graphic(CST_ROCKET_7_G);

        main_types[CST_ROCKET_FLOWN].group=GROUP_ROCKET;
        main_types[CST_ROCKET_FLOWN].size=4;
        main_types[CST_ROCKET_FLOWN].graphic
			=load_graphic(CST_ROCKET_FLOWN_G);

        main_types[CST_BURNT].group=GROUP_BURNT;
        main_types[CST_BURNT].size=1;
        main_types[CST_BURNT].graphic=load_graphic(CST_BURNT_G);

	strcpy(type_string[GROUP_WINDMILL],"Windmill");
        main_types[CST_WINDMILL_1_G].group=GROUP_WINDMILL;
        main_types[CST_WINDMILL_1_G].size=2;
        main_types[CST_WINDMILL_1_G].graphic
			=load_graphic(CST_WINDMILL_1_G_G);

        main_types[CST_WINDMILL_2_G].group=GROUP_WINDMILL;
        main_types[CST_WINDMILL_2_G].size=2;
        main_types[CST_WINDMILL_2_G].graphic
			=load_graphic(CST_WINDMILL_2_G_G);

        main_types[CST_WINDMILL_3_G].group=GROUP_WINDMILL;
        main_types[CST_WINDMILL_3_G].size=2;
        main_types[CST_WINDMILL_3_G].graphic
			=load_graphic(CST_WINDMILL_3_G_G);

        main_types[CST_WINDMILL_1_RG].group=GROUP_WINDMILL;
        main_types[CST_WINDMILL_1_RG].size=2;
        main_types[CST_WINDMILL_1_RG].graphic
                        =load_graphic(CST_WINDMILL_1_RG_G);

        main_types[CST_WINDMILL_2_RG].group=GROUP_WINDMILL;
        main_types[CST_WINDMILL_2_RG].size=2;
        main_types[CST_WINDMILL_2_RG].graphic
                        =load_graphic(CST_WINDMILL_2_RG_G);

        main_types[CST_WINDMILL_3_RG].group=GROUP_WINDMILL;
        main_types[CST_WINDMILL_3_RG].size=2;
        main_types[CST_WINDMILL_3_RG].graphic
                        =load_graphic(CST_WINDMILL_3_RG_G);

        main_types[CST_WINDMILL_1_R].group=GROUP_WINDMILL;
        main_types[CST_WINDMILL_1_R].size=2;
        main_types[CST_WINDMILL_1_R].graphic
                        =load_graphic(CST_WINDMILL_1_R_G);

        main_types[CST_WINDMILL_2_R].group=GROUP_WINDMILL;
        main_types[CST_WINDMILL_2_R].size=2;
        main_types[CST_WINDMILL_2_R].graphic
                        =load_graphic(CST_WINDMILL_2_R_G);

        main_types[CST_WINDMILL_3_R].group=GROUP_WINDMILL;
        main_types[CST_WINDMILL_3_R].size=2;
        main_types[CST_WINDMILL_3_R].graphic
                        =load_graphic(CST_WINDMILL_3_R_G);

        main_types[CST_WINDMILL_1_W].group=GROUP_WINDMILL;
        main_types[CST_WINDMILL_1_W].size=2;
        main_types[CST_WINDMILL_1_W].graphic
                        =load_graphic(CST_WINDMILL_1_W_G);

        main_types[CST_WINDMILL_2_W].group=GROUP_WINDMILL;
        main_types[CST_WINDMILL_2_W].size=2;
        main_types[CST_WINDMILL_2_W].graphic
                        =load_graphic(CST_WINDMILL_2_W_G);

        main_types[CST_WINDMILL_3_W].group=GROUP_WINDMILL;
        main_types[CST_WINDMILL_3_W].size=2;
        main_types[CST_WINDMILL_3_W].graphic
                        =load_graphic(CST_WINDMILL_3_W_G);


	strcpy(type_string[GROUP_MONUMENT],"Monument");
        main_types[CST_MONUMENT_0].group=GROUP_MONUMENT;
        main_types[CST_MONUMENT_0].size=2;
        main_types[CST_MONUMENT_0].graphic=load_graphic(CST_MONUMENT_0_G);

        main_types[CST_MONUMENT_1].group=GROUP_MONUMENT;
        main_types[CST_MONUMENT_1].size=2;
        main_types[CST_MONUMENT_1].graphic=load_graphic(CST_MONUMENT_1_G);

        main_types[CST_MONUMENT_2].group=GROUP_MONUMENT;
        main_types[CST_MONUMENT_2].size=2;
        main_types[CST_MONUMENT_2].graphic=load_graphic(CST_MONUMENT_2_G);

        main_types[CST_MONUMENT_3].group=GROUP_MONUMENT;
        main_types[CST_MONUMENT_3].size=2;
        main_types[CST_MONUMENT_3].graphic=load_graphic(CST_MONUMENT_3_G);

        main_types[CST_MONUMENT_4].group=GROUP_MONUMENT;
        main_types[CST_MONUMENT_4].size=2;
        main_types[CST_MONUMENT_4].graphic=load_graphic(CST_MONUMENT_4_G);

        main_types[CST_MONUMENT_5].group=GROUP_MONUMENT;
        main_types[CST_MONUMENT_5].size=2;
        main_types[CST_MONUMENT_5].graphic=load_graphic(CST_MONUMENT_5_G);


	strcpy(type_string[GROUP_SCHOOL],"School");
        main_types[CST_SCHOOL].group=GROUP_SCHOOL;
        main_types[CST_SCHOOL].size=2;
        main_types[CST_SCHOOL].graphic=load_graphic(CST_SCHOOL_G);

	strcpy(type_string[GROUP_BLACKSMITH],"Blacksmith");
        main_types[CST_BLACKSMITH_0].group=GROUP_BLACKSMITH;
        main_types[CST_BLACKSMITH_0].size=2;
        main_types[CST_BLACKSMITH_0].graphic
		=load_graphic(CST_BLACKSMITH_0_G);

        main_types[CST_BLACKSMITH_1].group=GROUP_BLACKSMITH;
        main_types[CST_BLACKSMITH_1].size=2;
        main_types[CST_BLACKSMITH_1].graphic
                =load_graphic(CST_BLACKSMITH_1_G);

        main_types[CST_BLACKSMITH_2].group=GROUP_BLACKSMITH;
        main_types[CST_BLACKSMITH_2].size=2;
        main_types[CST_BLACKSMITH_2].graphic
                =load_graphic(CST_BLACKSMITH_2_G);

        main_types[CST_BLACKSMITH_3].group=GROUP_BLACKSMITH;
        main_types[CST_BLACKSMITH_3].size=2;
        main_types[CST_BLACKSMITH_3].graphic
                =load_graphic(CST_BLACKSMITH_3_G);

        main_types[CST_BLACKSMITH_4].group=GROUP_BLACKSMITH;
        main_types[CST_BLACKSMITH_4].size=2;
        main_types[CST_BLACKSMITH_4].graphic
                =load_graphic(CST_BLACKSMITH_4_G);

        main_types[CST_BLACKSMITH_5].group=GROUP_BLACKSMITH;
        main_types[CST_BLACKSMITH_5].size=2;
        main_types[CST_BLACKSMITH_5].graphic
                =load_graphic(CST_BLACKSMITH_5_G);

        main_types[CST_BLACKSMITH_6].group=GROUP_BLACKSMITH;
        main_types[CST_BLACKSMITH_6].size=2;
        main_types[CST_BLACKSMITH_6].graphic
                =load_graphic(CST_BLACKSMITH_6_G);

	prog_box("",75);

	strcpy(type_string[GROUP_MILL],"Mill");
        main_types[CST_MILL_0].group=GROUP_MILL;
        main_types[CST_MILL_0].size=2;
        main_types[CST_MILL_0].graphic=load_graphic(CST_MILL_0_G);

        main_types[CST_MILL_1].group=GROUP_MILL;
        main_types[CST_MILL_1].size=2;
        main_types[CST_MILL_1].graphic=load_graphic(CST_MILL_1_G);

        main_types[CST_MILL_2].group=GROUP_MILL;
        main_types[CST_MILL_2].size=2;
        main_types[CST_MILL_2].graphic=load_graphic(CST_MILL_2_G);

        main_types[CST_MILL_3].group=GROUP_MILL;
        main_types[CST_MILL_3].size=2;
        main_types[CST_MILL_3].graphic=load_graphic(CST_MILL_3_G);

        main_types[CST_MILL_4].group=GROUP_MILL;
        main_types[CST_MILL_4].size=2;
        main_types[CST_MILL_4].graphic=load_graphic(CST_MILL_4_G);

        main_types[CST_MILL_5].group=GROUP_MILL;
        main_types[CST_MILL_5].size=2;
        main_types[CST_MILL_5].graphic=load_graphic(CST_MILL_5_G);

        main_types[CST_MILL_6].group=GROUP_MILL;
        main_types[CST_MILL_6].size=2;
        main_types[CST_MILL_6].graphic=load_graphic(CST_MILL_6_G);

        strcpy(type_string[GROUP_POTTERY],"Pottery");
        main_types[CST_POTTERY_0].group=GROUP_POTTERY;
        main_types[CST_POTTERY_0].size=2;
        main_types[CST_POTTERY_0].graphic=load_graphic(CST_POTTERY_0_G);

        main_types[CST_POTTERY_1].group=GROUP_POTTERY;
        main_types[CST_POTTERY_1].size=2;
        main_types[CST_POTTERY_1].graphic=load_graphic(CST_POTTERY_1_G);

        main_types[CST_POTTERY_2].group=GROUP_POTTERY;
        main_types[CST_POTTERY_2].size=2;
        main_types[CST_POTTERY_2].graphic=load_graphic(CST_POTTERY_2_G);

        main_types[CST_POTTERY_3].group=GROUP_POTTERY;
        main_types[CST_POTTERY_3].size=2;
        main_types[CST_POTTERY_3].graphic=load_graphic(CST_POTTERY_3_G);

        main_types[CST_POTTERY_4].group=GROUP_POTTERY;
        main_types[CST_POTTERY_4].size=2;
        main_types[CST_POTTERY_4].graphic=load_graphic(CST_POTTERY_4_G);

        main_types[CST_POTTERY_5].group=GROUP_POTTERY;
        main_types[CST_POTTERY_5].size=2;
        main_types[CST_POTTERY_5].graphic=load_graphic(CST_POTTERY_5_G);

        main_types[CST_POTTERY_6].group=GROUP_POTTERY;
        main_types[CST_POTTERY_6].size=2;
        main_types[CST_POTTERY_6].graphic=load_graphic(CST_POTTERY_6_G);

        main_types[CST_POTTERY_7].group=GROUP_POTTERY;
        main_types[CST_POTTERY_7].size=2;
        main_types[CST_POTTERY_7].graphic=load_graphic(CST_POTTERY_7_G);

        main_types[CST_POTTERY_8].group=GROUP_POTTERY;
        main_types[CST_POTTERY_8].size=2;
        main_types[CST_POTTERY_8].graphic=load_graphic(CST_POTTERY_8_G);

        main_types[CST_POTTERY_9].group=GROUP_POTTERY;
        main_types[CST_POTTERY_9].size=2;
        main_types[CST_POTTERY_9].graphic=load_graphic(CST_POTTERY_9_G);

        main_types[CST_POTTERY_10].group=GROUP_POTTERY;
        main_types[CST_POTTERY_10].size=2;
        main_types[CST_POTTERY_10].graphic=load_graphic(CST_POTTERY_10_G);

	strcpy(type_string[GROUP_FIRESTATION],"Fire sta'n");
        main_types[CST_FIRESTATION_1].group=GROUP_FIRESTATION;
        main_types[CST_FIRESTATION_1].size=2;
        main_types[CST_FIRESTATION_1].graphic
		=load_graphic(CST_FIRESTATION_1_G);

        main_types[CST_FIRESTATION_2].group=GROUP_FIRESTATION;
        main_types[CST_FIRESTATION_2].size=2;
        main_types[CST_FIRESTATION_2].graphic
                =load_graphic(CST_FIRESTATION_2_G);

        main_types[CST_FIRESTATION_3].group=GROUP_FIRESTATION;
        main_types[CST_FIRESTATION_3].size=2;
        main_types[CST_FIRESTATION_3].graphic
                =load_graphic(CST_FIRESTATION_3_G);

        main_types[CST_FIRESTATION_4].group=GROUP_FIRESTATION;
        main_types[CST_FIRESTATION_4].size=2;
        main_types[CST_FIRESTATION_4].graphic
                =load_graphic(CST_FIRESTATION_4_G);

        main_types[CST_FIRESTATION_5].group=GROUP_FIRESTATION;
        main_types[CST_FIRESTATION_5].size=2;
        main_types[CST_FIRESTATION_5].graphic
                =load_graphic(CST_FIRESTATION_5_G);

        main_types[CST_FIRESTATION_6].group=GROUP_FIRESTATION;
        main_types[CST_FIRESTATION_6].size=2;
        main_types[CST_FIRESTATION_6].graphic
                =load_graphic(CST_FIRESTATION_6_G);

        main_types[CST_FIRESTATION_7].group=GROUP_FIRESTATION;
        main_types[CST_FIRESTATION_7].size=2;
        main_types[CST_FIRESTATION_7].graphic
                =load_graphic(CST_FIRESTATION_7_G);

        main_types[CST_FIRESTATION_8].group=GROUP_FIRESTATION;
        main_types[CST_FIRESTATION_8].size=2;
        main_types[CST_FIRESTATION_8].graphic
                =load_graphic(CST_FIRESTATION_8_G);

        main_types[CST_FIRESTATION_9].group=GROUP_FIRESTATION;
        main_types[CST_FIRESTATION_9].size=2;
        main_types[CST_FIRESTATION_9].graphic
                =load_graphic(CST_FIRESTATION_9_G);

        main_types[CST_FIRESTATION_10].group=GROUP_FIRESTATION;
        main_types[CST_FIRESTATION_10].size=2;
        main_types[CST_FIRESTATION_10].graphic
                =load_graphic(CST_FIRESTATION_10_G);


        strcpy(type_string[GROUP_CRICKET],"Cricket pitch");
        main_types[CST_CRICKET_1].group=GROUP_CRICKET;
        main_types[CST_CRICKET_1].size=2;
        main_types[CST_CRICKET_1].graphic
                =load_graphic(CST_CRICKET_1_G);

        main_types[CST_CRICKET_2].group=GROUP_CRICKET;
        main_types[CST_CRICKET_2].size=2;
        main_types[CST_CRICKET_2].graphic
                =load_graphic(CST_CRICKET_2_G);

        main_types[CST_CRICKET_3].group=GROUP_CRICKET;
        main_types[CST_CRICKET_3].size=2;
        main_types[CST_CRICKET_3].graphic
                =load_graphic(CST_CRICKET_3_G);

        main_types[CST_CRICKET_4].group=GROUP_CRICKET;
        main_types[CST_CRICKET_4].size=2;
        main_types[CST_CRICKET_4].graphic
                =load_graphic(CST_CRICKET_4_G);

	prog_box("",90);

        main_types[CST_CRICKET_5].group=GROUP_CRICKET;
        main_types[CST_CRICKET_5].size=2;
        main_types[CST_CRICKET_5].graphic
                =load_graphic(CST_CRICKET_5_G);

        main_types[CST_CRICKET_6].group=GROUP_CRICKET;
        main_types[CST_CRICKET_6].size=2;
        main_types[CST_CRICKET_6].graphic
                =load_graphic(CST_CRICKET_6_G);

        main_types[CST_CRICKET_7].group=GROUP_CRICKET;
        main_types[CST_CRICKET_7].size=2;
        main_types[CST_CRICKET_7].graphic
                =load_graphic(CST_CRICKET_7_G);

        strcpy(type_string[GROUP_FIRE],"Fire");
        main_types[CST_FIRE_1].group=GROUP_FIRE;
        main_types[CST_FIRE_1].size=1;
        main_types[CST_FIRE_1].graphic
                =load_graphic(CST_FIRE_1_G);

        main_types[CST_FIRE_2].group=GROUP_FIRE;
        main_types[CST_FIRE_2].size=1;
        main_types[CST_FIRE_2].graphic
                =load_graphic(CST_FIRE_2_G);

        main_types[CST_FIRE_3].group=GROUP_FIRE;
        main_types[CST_FIRE_3].size=1;
        main_types[CST_FIRE_3].graphic
                =load_graphic(CST_FIRE_3_G);

        main_types[CST_FIRE_4].group=GROUP_FIRE;
        main_types[CST_FIRE_4].size=1;
        main_types[CST_FIRE_4].graphic
                =load_graphic(CST_FIRE_4_G);

        main_types[CST_FIRE_5].group=GROUP_FIRE;
        main_types[CST_FIRE_5].size=1;
        main_types[CST_FIRE_5].graphic
                =load_graphic(CST_FIRE_5_G);

        main_types[CST_FIRE_DONE1].group=GROUP_FIRE;
        main_types[CST_FIRE_DONE1].size=1;
        main_types[CST_FIRE_DONE1].graphic
                =load_graphic(CST_FIRE_DONE1_G);

        main_types[CST_FIRE_DONE2].group=GROUP_FIRE;
        main_types[CST_FIRE_DONE2].size=1;
        main_types[CST_FIRE_DONE2].graphic
                =load_graphic(CST_FIRE_DONE2_G);

        main_types[CST_FIRE_DONE3].group=GROUP_FIRE;
        main_types[CST_FIRE_DONE3].size=1;
        main_types[CST_FIRE_DONE3].graphic
                =load_graphic(CST_FIRE_DONE3_G);

        main_types[CST_FIRE_DONE4].group=GROUP_FIRE;
        main_types[CST_FIRE_DONE4].size=1;
        main_types[CST_FIRE_DONE4].graphic
                =load_graphic(CST_FIRE_DONE4_G);

        strcpy(type_string[GROUP_SHANTY],"Shanty town");
        main_types[CST_SHANTY].group=GROUP_SHANTY;
        main_types[CST_SHANTY].size=2;
        main_types[CST_SHANTY].graphic
                =load_graphic(CST_SHANTY_G);

/*
	main_types[].group=;
        main_types[].size=1;
        main_types[].graphic=load_graphic(_G);
*/

	fire_chance[GROUP_BARE]=GROUP_BARE_FIREC;
	fire_chance[GROUP_POWER_LINE]=GROUP_POWER_LINE_FIREC;
	fire_chance[GROUP_POWER_SOURCE]=GROUP_POWER_SOURCE_FIREC;
	fire_chance[GROUP_SUBSTATION]=GROUP_SUBSTATION_FIREC;
	fire_chance[GROUP_RESIDENCE]=GROUP_RESIDENCE_FIREC;
	fire_chance[GROUP_ORGANIC_FARM]=GROUP_ORGANIC_FARM_FIREC;
	fire_chance[GROUP_MARKET]=GROUP_MARKET_FIREC;
	fire_chance[GROUP_TRACK]=GROUP_TRACK_FIREC;
	fire_chance[GROUP_COALMINE]=GROUP_COALMINE_FIREC;
	fire_chance[GROUP_RAIL]=GROUP_RAIL_FIREC;
	fire_chance[GROUP_POWER_SOURCE_COAL]=GROUP_POWER_SOURCE_COAL_FIREC;
	fire_chance[GROUP_ROAD]=GROUP_ROAD_FIREC;
	fire_chance[GROUP_INDUSTRY_L]=GROUP_INDUSTRY_L_FIREC;
	fire_chance[GROUP_UNIVERSITY]=GROUP_UNIVERSITY_FIREC;
	fire_chance[GROUP_COMMUNE]=GROUP_COMMUNE_FIREC;
	fire_chance[GROUP_OREMINE]=GROUP_OREMINE_FIREC;
	fire_chance[GROUP_TIP]=GROUP_TIP_FIREC;
	fire_chance[GROUP_EX_PORT]=GROUP_EX_PORT_FIREC;
	fire_chance[GROUP_INDUSTRY_H]=GROUP_INDUSTRY_H_FIREC;
	fire_chance[GROUP_PARKLAND]=GROUP_PARKLAND_FIREC;
	fire_chance[GROUP_RECYCLE]=GROUP_RECYCLE_FIREC;
	fire_chance[GROUP_WATER]=GROUP_WATER_FIREC;
	fire_chance[GROUP_HEALTH]=GROUP_HEALTH_FIREC;
	fire_chance[GROUP_ROCKET]=GROUP_ROCKET_FIREC;
	fire_chance[GROUP_WINDMILL]=GROUP_WINDMILL_FIREC;
	fire_chance[GROUP_MONUMENT]=GROUP_MONUMENT_FIREC;
	fire_chance[GROUP_SCHOOL]=GROUP_SCHOOL_FIREC;
	fire_chance[GROUP_BLACKSMITH]=GROUP_BLACKSMITH_FIREC;
	fire_chance[GROUP_MILL]=GROUP_MILL_FIREC;
	fire_chance[GROUP_POTTERY]=GROUP_POTTERY_FIREC;
	fire_chance[GROUP_FIRESTATION]=GROUP_FIRESTATION_FIREC;
	fire_chance[GROUP_CRICKET]=GROUP_CRICKET_FIREC;
	fire_chance[GROUP_BURNT]=GROUP_BURNT_FIREC;

}

char *load_graphic(char *s)
{
	int x,l;
        char ss[100],*graphic;
	FILE *inf;
	strcpy(ss,graphic_path);
	strcat(ss,s);
        if ((inf=fopen(ss,"rb"))==NULL)
	{
		strcat(ss," -- UNABLE TO LOAD");
                do_error(ss);
	}
        fseek(inf,0L,SEEK_END);
        l=ftell(inf);
        fseek(inf,0L,SEEK_SET);
	graphic=(char *)malloc(l);
	for (x=0;x<l;x++)
		*(graphic+x)=fgetc(inf);
	fclose(inf);
	return(graphic);
}
