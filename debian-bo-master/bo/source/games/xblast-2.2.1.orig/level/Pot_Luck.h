/* XBlast 2.1.8 level */
static BMLevelData Pot_Luck =
{
  /* BMLevel */
  {
    "Pot Luck",
    "Keith Gillow",
    "xblast.usePotLuck",
    "Find a timer if you can",
    GM_Random | GM_234_Player | GM_All,
    (void *) &Pot_Luck, 
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
    special_init_void,
    special_game_void,
    special_extra_RC,
    special_key_RC,
  },
  /* BMPlayerData */
  {
    1, 2,
    {
      { 3, 1 },
      { 3, 13 },
      { 9, 13 },
      { 9, 1 },
    },
    PM_Vertical, 2,
    Healthy, IF_None, 
  },
  /* BMBombData */
  {
    bomb_click_none, bomb_click_none, bomb_click_none,
    GoStop, FUSEshort,
    BMTnormal, BMTnormal, BMTnormal,
  },
  /* BMGraphicsData */
  {
    {
      { BLIronFloor,     "Black",        "Red",       "Yellow" },
      { BLIronFloor_S,   "Black",        "Red",       "Yellow" },
      { BLDarkBlock,     "Black",    "DarkGoldenrod",   "LightGoldenrodYellow" },
      { BLDarkBlockRise, "Black",    "DarkGoldenrod",   "LightGoldenrodYellow" },
      { BLExtra,         "Black",      "SpringGreen", "ForestGreen" },
      { BLExtra_O,       "Black",      "SpringGreen", "ForestGreen" },
      EXTRA_RC,
      EXTRA_RC,
      EXTRA_RC,
      EXTRA_RC,
      { BLScoreFloor,    "BlueViolet", "BlueViolet",  "BlueViolet" },
    },
  },
  /* BMMapData */
  {
    ShadowNone, DEall,
    {14,28,40,48,50},
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B },
      { B,_,X,_,X,_,X,_,X,_,X,_,B },
      { B,X,B,X,B,X,B,X,B,X,B,X,B },
      { B,_,X,_,X,_,X,_,X,_,X,_,B },
      { B,X,B,_,B,X,B,X,B,_,B,X,B },
      { B,_,X,_,X,_,_,_,X,_,X,_,B },
      { B,X,B,_,B,_,_,_,B,_,B,X,B },
      { B,_,X,_,X,_,B,_,X,_,X,_,B },
      { B,X,B,_,B,_,_,_,B,_,B,X,B },
      { B,_,X,_,X,_,_,_,X,_,X,_,B },
      { B,X,B,_,B,X,B,X,B,_,B,X,B },
      { B,_,X,_,X,_,X,_,X,_,X,_,B },
      { B,X,B,X,B,X,B,X,B,X,B,X,B },
      { B,_,X,_,X,_,X,_,X,_,X,_,B },
      { B,B,B,B,B,B,B,B,B,B,B,B,B }
    },
  },
};
