/* XBlast 2.1.8 level */
static BMLevelData HotStuff =
{
  /* BMLevel */
  {
    "Hot Stuff",
    "Garth Denley",
    "xblast.useHotStuff",
    "Kick bombs into the long-lasting explosions",
    GM_Random | GM_23456_Player | GM_All,
    (void *) &HotStuff,
    NULL,
  },
  /* BMShrinkData */
  {
    shrink_compound_solid,
    SCRAMBLE_VOID,
    SCRAMBLE_VOID,
  },
  /* BMFuncData */
  {
    special_init_void,
    special_game_void,
    special_extra_void,
    special_key_void,
  },
  /* BMPlayerData */
  {
    6, 49,
    {
      {  5,  5 },
      {  5,  9 },
      {  7,  9 },
      {  7,  5 },
      {  6,  4 },
      {  6, 10 },
    },
    PM_Inner, -2,
    Healthy, IF_Kick,
  },
  /* BMBombData */
  {
    bomb_click_none, bomb_click_none, bomb_click_thru,
    GoStop, FUSEnormal,
    BMTnormal, BMTnormal, BMTnormal,
  },
  /* BMGraphicsData */
  {
    {
      { BLDarkWay, "Black", "SpringGreen", "OrangeRed" },
      { BLDarkWay_S, "Black", "SpringGreen", "OrangeRed" },
      { BLRIP, "Black", "SkyBlue", "SpringGreen" },
      { BLRIPRise, "Black", "SkyBlue", "SpringGreen" },
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
    ShadowFull, DEnone,
    { 0, 0, 0, 0, 0 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,_,B,B,B,_,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,B,_,_,_,_,_,B,_,_,B, },
      { B,_,_,B,_,_,_,_,_,B,_,_,B, },
      { B,_,_,B,_,_,_,_,_,B,_,_,B, },
      { B,_,_,B,_,_,_,_,_,B,_,_,B, },
      { B,_,_,B,_,_,_,_,_,B,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,_,B,B,B,_,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
