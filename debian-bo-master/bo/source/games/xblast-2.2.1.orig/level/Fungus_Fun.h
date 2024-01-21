/* XBlast 2.1.8 level */
static BMPosition Fungus_Fun_Dele[] = {
 {5,6},{5,8},{7,6},{7,8},{2,6},{2,7},{2,8},{10,6},{10,7},{10,8},{5,2},{6,2},{7,2},{5,12},{6,12},{7,12} };

static BMPosition Fungus_Fun_Draw[] = { 
  {6,7},{6,4},{6,10},{3,7},{9,7} };

static BMLevelData Fungus_Fun =
{
  /* BMLevel */
  {
    "Fungus Fun",
    "Keith Gillow",
    "xblast.useFungusFun",
    "Create havoc for your opponents with fungus bombs",
    GM_Random | GM_23456_Player | GM_All,
    (void *) &Fungus_Fun,
    NULL,
  },
  /* BMShrinkData */
  {
    shrink_early_spiral,
    {GAME_TIME/2, 5, Fungus_Fun_Draw},
    {GAME_TIME/2, 16, Fungus_Fun_Dele},
  },
  /* BMFuncData */
  {
    special_init_special_bombs_30,
    special_game_void,
    special_extra_void,
    special_key_special_bomb,
  },
  /* BMPlayerData */
  {
    3, 10,
    {
      {  2, 2 },
      {  2, 12 },
      { 10, 12 },
      { 10, 2 },
      {  2, 7 },
      { 10, 7 },
    },
    PM_Inner, 2,
    Healthy, IF_Kick, 
  },
  /* BMBombData */
  {
    bomb_click_contact, bomb_click_contact, bomb_click_thru,
    GoStop, FUSEnormal,
    BMTnormal, BMTfungus, BMTnormal,
  },
  /* BMGraphicsData */
  {
    {
      { BLIronFloor,     "Black",     "CornflowerBlue",  "OrangeRed" },
      { BLIronFloor_S,   "Black",     "CornflowerBlue",  "OrangeRed" },
      { BLDarkBlock,     "Black",     "CadetBlue2",    "Magenta" },
      { BLDarkBlockRise, "Black",     "CadetBlue2",    "Green" },
      { BLExtra,         "Black",     "Firebrick",      "Black" },
      { BLExtra,         "Black",     "Firebrick",      "Black" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_KICK,
      { BLScoreFloor,    "SteelBlue", "SteelBlue", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowBlock, DEnone,
    { 0, 0, 0, 0, 0},
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B },
      { B,_,_,_,_,_,_,_,_,_,_,_,B },
      { B,_,_,_,B,_,_,_,B,_,_,_,B },
      { B,_,_,_,_,_,_,_,_,_,_,_,B },
      { B,_,B,_,_,_,_,_,_,_,B,_,B },
      { B,_,_,_,_,_,_,_,_,_,_,_,B },
      { B,_,_,_,_,B,_,B,_,_,_,_,B },
      { B,_,_,_,_,_,_,_,_,_,_,_,B },
      { B,_,_,_,_,B,_,B,_,_,_,_,B },
      { B,_,_,_,_,_,_,_,_,_,_,_,B },
      { B,_,B,_,_,_,_,_,_,_,B,_,B },
      { B,_,_,_,_,_,_,_,_,_,_,_,B },
      { B,_,_,_,B,_,_,_,B,_,_,_,B },
      { B,_,_,_,_,_,_,_,_,_,_,_,B },
      { B,B,B,B,B,B,B,B,B,B,B,B,B }
    },
  },
};
