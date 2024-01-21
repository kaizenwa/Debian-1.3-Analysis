// market.h    part of cim-sity
// Copyright (c) I J Peters 1995,1996.  Please read the file 'COPYRIGHT'.

extern struct TYPE main_types[];
extern struct MAPPOINT mappoint[WORLD_SIDE_LEN][WORLD_SIDE_LEN];
extern short mappointtype[WORLD_SIDE_LEN][WORLD_SIDE_LEN];
extern int mappointpol[WORLD_SIDE_LEN][WORLD_SIDE_LEN];
extern int total_time;
//extern int substationx[],substationy[],numof_substations;
extern int marketx[],markety[],numof_markets;
extern int population,starving_population,food_in_markets,jobs_in_markets;
extern int coal_in_markets,goods_in_markets,ore_in_markets,steel_in_markets;
extern int mappoint_array_x[],mappoint_array_y[];
extern int unemployed_population;
extern int income_tax,coal_tax;
extern int capital_cost,unemployment_cost,transport_cost;
extern int income_tax,coal_tax,goods_tax,total_money;
extern int capital_cost,unemployment_cost,transport_cost;
extern int people_pool;
//extern char *months[];
extern int export_tax,export_tax_rate,import_cost,import_cost_rate;
extern int waste_goods,goods_used,mini_screen_flags;


