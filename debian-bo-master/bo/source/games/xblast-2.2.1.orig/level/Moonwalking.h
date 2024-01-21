/* XBlast 2.1.8 level */
static BMLevelData Moonwalking =
{
  /* BMLevel */
  {
    "Moonwalking",
    "Keith Gillow",
    "xblast.useMoonwalking",
    "Keep cool and get to the center",
    GM_Random | GM_234_Player | GM_All, 
    (void *) &Moonwalking, 
    NULL,
  },
  /* BMShrinkData */
  {
    shrink_compound,
    SCRAMBLE_VOID,
    SCRAMBLE_VOID,
  },
  /* BMFuncData */
  {
    special_init_void,
    special_game_void,
    special_extra_air,
    special_key_air,
  },
  /* BMPlayerData */
  {
    2, 2,
    {
      { 1, 1 },
      { 1, 13 },
      { 11, 13 },
      { 11, 1 },
    },
    PM_Polar, 2,
    IllReverse, IF_None, 
  },
  /* BMBombData */
  {
    bomb_click_none, bomb_click_none, bomb_click_none,
    GoStop, FUSEnormal,
    BMTnormal, BMTnormal, BMTnormal,
  },
  /* BMGraphicsData */
  {
    {
      { BLButtonFloor,   "Black", "Gray75",    "Black" },
      { BLButtonFloor,   "Black", "Gray75",    "Black" },
      { BLDarkBlock,     "Black", "Yellow", "DarkTurquoise" },
      { BLDarkBlockRise, "Black", "Yellow", "DarkTurquoise" },
      { BLBox,           "Black", "Yellow", "Red" },
      { BLBox,           "Black", "HotPink", "Red" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_AIRPUMP,
      { BLScoreFloor,    "Red",   "Red",       "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowNone, DEall,
    {18,46,48,53,54},
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B },
      { B,_,_,_,X,_,X,_,X,_,_,_,B },
      { B,_,B,X,B,X,B,X,B,X,B,_,B },
      { B,_,X,_,X,_,X,_,X,_,X,_,B },
      { B,X,B,X,B,X,B,X,B,X,B,X,B },
      { B,_,X,_,X,_,X,_,X,_,X,_,B },
      { B,X,B,X,B,X,B,X,B,X,B,X,B },
      { B,_,X,_,X,_,X,_,X,_,X,_,B },
      { B,X,B,X,B,X,B,X,B,X,B,X,B },
      { B,_,X,_,X,_,X,_,X,_,X,_,B },
      { B,X,B,X,B,X,B,X,B,X,B,X,B },
      { B,_,X,_,X,_,X,_,X,_,X,_,B },
      { B,_,B,X,B,X,B,X,B,X,B,_,B },
      { B,_,_,_,X,_,X,_,X,_,_,_,B },
      { B,B,B,B,B,B,B,B,B,B,B,B,B }
    },
  },
};
