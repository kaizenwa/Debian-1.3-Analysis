/* XBlast 2.1.8 level */
static BMPosition Halloween_Del[] = { 
  {6,8},{6,6} 
};
static BMLevelData Halloween =
{
  /* BMLevel */
  {
    "Hallowe'en",
    "Oliver Vogel",
    "xblast.useHalloween",
    "Mind the buttons",
    GM_Random | GM_23456_Player | GM_All,
    (void *) &Halloween,
    NULL,
  },
  /* BMShrinkData */
  {
    shrink_spiral_3,
    SCRAMBLE_VOID,
    { 3*GAME_TIME/4, 2, Halloween_Del },
  },
  /* BMFuncData */
  {
    special_init_void,
    special_game_void,
    special_extra_ignite_all,
    special_key_void,
  },
  /* BMPlayerData */
  {
    1, 2,
    {
      {  3,  3 },
      {  3, 11 },
      {  9, 11 },
      {  9,  3 },
      {  5,  5 },
      {  7,  9 },
    },
    PM_Polar, 1,
    Healthy, IF_None,
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
      { BLDarkWay, "Black", "Green", "Gray80" },
      { BLDarkWay_S, "Black", "Green", "Gray80" },
      { BLRIP, "Black", "Gray60", "Green" },
      { BLRIPRise, "Black", "Gray60", "Green" },
      { BLPumpkin, "Black", "Goldenrod", "Green" },
      { BLPumpkin_O, "Black", "Goldenrod", "Green" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_BUTTON,
      { BLScoreFloor, "Black", "Black", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowFull, DEget,
    { 16, 32, 40, 50, 58 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,_,X,_,_,X,_,X,_,_,X,_,B, },
      { B,X,B,X,B,B,X,B,B,X,B,X,B, },
      { B,_,X,_,_,X,_,X,_,_,X,_,B, },
      { B,_,B,_,B,B,X,B,B,_,B,_,B, },
      { B,_,X,_,X,_,_,_,X,_,X,_,B, },
      { B,X,B,X,B,_,B,X,B,X,B,X,B, },
      { B,_,X,_,X,_,B,_,X,_,X,_,B, },
      { B,X,B,X,B,X,B,_,B,X,B,X,B, },
      { B,_,X,_,X,_,_,_,X,_,X,_,B, },
      { B,_,B,_,B,B,X,B,B,_,B,_,B, },
      { B,_,X,_,_,X,_,X,_,_,X,_,B, },
      { B,X,B,X,B,B,X,B,B,X,B,X,B, },
      { B,_,X,_,_,X,_,X,_,_,X,_,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
