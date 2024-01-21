/* XBlast 2.1.8 level */
static BMLevelData Open_Warfare =
{
  /* BMLevel */
  {
    "Open Warfare",
    "Keith Gillow",
    "xblast.useOpenWarfare",
    "You've got plenty of time to run and hide",
    GM_Random | GM_234_Player | GM_All, 
    (void *) &Open_Warfare, 
    NULL,
  },
  /* BMShrinkData */
  {
    shrink_void,
    SCRAMBLE_VOID,
    SCRAMBLE_VOID,
  },
  /* BMFuncData */
  {
    special_init_special_bombs_30,
    special_game_void,
    special_extra_special_bomb,
    special_key_special_bomb,
  },
  /* BMPlayerData */
  {
    6, 9,
    {
      { 6, 1 },
      { 1, 7 },
      { 6, 13 },
      { 11, 7 },
    },
    PM_Circle, 2,
    Healthy, IF_Kick, 
  },
  /* BMBombData */
  {
    bomb_click_none, bomb_click_none, bomb_click_thru,
    GoStop, FUSElong,
    BMTnapalm, BMTpyro, BMTnormal,
  },
  /* BMGraphicsData */
  {
    {
      { BLDarkWay,    "Black",     "SpringGreen",     "Tomato" },
      { BLDarkWay_S,  "Black",     "SpringGreen",     "Tomato" },
      { BLRIP,        "BLack",     "firebrick1",       "SpringGreen" },
      { BLRIPRise,    "Black",     "firebrick1",       "SpringGreen" },
      { BLPumpkin,    "Black",     "LightGoldenrod", "Green" },
      { BLPumpkin_O,  "Black",     "LightGoldenrod", "Green" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_BUTTON,
      { BLScoreFloor, "Black",     "Black",     "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowFull, DEnone,
    {0,0,0,0,0},
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B },
      { B,X,_,_,_,_,_,_,_,_,_,X,B },
      { B,_,X,_,_,_,_,_,_,_,X,_,B },
      { B,_,_,X,_,B,X,B,_,X,_,_,B },
      { B,_,_,_,X,_,_,_,X,_,_,_,B },
      { B,_,_,B,_,X,_,X,_,B,_,_,B },
      { B,_,_,X,_,_,X,_,_,X,_,_,B },
      { B,_,_,X,_,_,B,_,_,X,_,_,B },
      { B,_,_,X,_,_,X,_,_,X,_,_,B },
      { B,_,_,B,_,X,_,X,_,B,_,_,B },
      { B,_,_,_,X,_,_,_,X,_,_,_,B },
      { B,_,_,X,_,B,X,B,_,X,_,_,B },
      { B,_,X,_,_,_,_,_,_,_,X,_,B },
      { B,X,_,_,_,_,_,_,_,_,_,X,B },
      { B,B,B,B,B,B,B,B,B,B,B,B,B }  
    },
  },
};
