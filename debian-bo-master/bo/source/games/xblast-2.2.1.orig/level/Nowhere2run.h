/* XBlast 2.1.8 level */
static BMLevelData Nowhere2run =
{
  /* BMLevel */
  {
    "Nowhere to run",
    "Garth Denley / Joachim Kaltz",
    "xblast.useNowhere2run",
    "Time your bombs... no-one will come to you",
    GM_Random | GM_23456_Player | GM_All,
    (void *) &Nowhere2run,
    NULL,
  },
  /* BMShrinkData */
  {
    shrink_spiral,
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
    10, 15,
    {
      {  5,  5 },
      {  5,  9 },
      {  7,  9 },
      {  7,  5 },
      {  4,  7 },
      {  8,  7 },
    },
    PM_Polar, -1,
    Healthy, IF_Kick,
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
      { BLIronFloor, "Black", "Gray20", "SpringGreen" },
      { BLIronFloor_S, "Black", "Gray20", "SpringGreen" },
      { BLDarkBlock, "MidnightBlue", "RoyalBlue", "Black" },
      { BLDarkBlockRise, "RoyalBlue", "White", "White" },
      { BLExtra, "Black", "SaddleBrown", "Black" },
      { BLExtra, "Black", "SaddleBrown", "Black" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_INVINC,
      { BLScoreFloor, "RoyalBlue", "RoyalBlue", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowBlock, DEnone,
    { 0, 0, 0, 0, 0 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
