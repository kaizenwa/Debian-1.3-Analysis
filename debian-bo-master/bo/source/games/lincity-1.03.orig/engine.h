// engine.h    part of cim-sity
// Copyright (c) I J Peters 1995,1996.  Please read the file 'COPYRIGHT'.

extern struct TYPE main_types[];
extern struct MAPPOINT mappoint[WORLD_SIDE_LEN][WORLD_SIDE_LEN];
extern int mappointpol[WORLD_SIDE_LEN][WORLD_SIDE_LEN];
extern short mappointtype[WORLD_SIDE_LEN][WORLD_SIDE_LEN];
extern int total_time,housed_population;
extern int substationx[],substationy[],numof_substations;
extern int marketx[],markety[],numof_markets;
extern int population,starving_population,food_in_markets,jobs_in_markets;
extern int coal_in_markets,goods_in_markets,ore_in_markets,steel_in_markets;
extern int mappoint_array_x[],mappoint_array_y[];
extern int unemployed_population;
extern int income_tax,coal_tax;
extern int unemployment_cost,transport_cost;
extern int university_cost,recycle_cost,windmill_cost,cricket_cost;
extern int income_tax,coal_tax,goods_tax,total_money,school_cost;
extern int capital_cost,unemployment_cost,transport_cost;
extern int people_pool,unnat_deaths;
extern int fire_chance[];
extern int real_time,university_intake_rate;
extern int numof_shanties,numof_communes;
extern int total_evacuated,total_births;
extern int sust_dig_ore_coal_tip_flag,sust_port_flag;

extern char *months[];
extern int export_tax,export_tax_rate,import_cost,import_cost_rate;
extern int tech_level,waste_goods;
extern int power_made,power_used,coal_made,coal_used;
extern int goods_made,goods_used,ore_made,ore_used;
extern int numof_health_centres,health_cost,rocket_pad_cost,fire_cost;
extern int rockets_launched,rockets_launched_success;
extern int last_built_x,last_built_y;
extern int total_pollution_deaths,total_starve_deaths;
extern int total_unemployed_days,total_unemployed_years;
extern float pollution_deaths_history,starve_deaths_history;
extern float unemployed_history;

