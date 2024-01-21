/* XBlast 2.1.8 level */
static BMLevelData Seek_N_Destroy =
{
  /* BMLevel */
  {
    "Seek'n Destroy",
    "Oliver Vogel",
    "xblast.useSeekNDestroy",
    "The classical one ...",
    GM_Random | GM_23456_Player | GM_All,
    (void *) &Seek_N_Destroy,
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
    special_extra_kick,
    special_key_void,
  },
  /* BMPlayerData */
  {
    1, 2,
    {
      {  1,  1 },
      {  1, 13 },
      { 11, 13 },
      { 11,  1 },
      {  1,  7 },
      { 11,  7 },
    },
    PM_Polar, 2,
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
      { BLKaroLight, "Black", "SlateBlue", "SteelBlue" },
      { BLKaroLight_S, "Black", "SlateBlue", "SteelBlue" },
      { BLPyramid, "Black", "Goldenrod", "Goldenrod" },
      { BLPyramidRise, "Black", "Goldenrod", "LightGoldenrod" },
      { BLExtra, "Black", "Sienna", "Orange" },
      { BLExtra_O, "Black", "Sienna", "Orange" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_KICK,
      { BLScoreFloor, "RoyalBlue", "RoyalBlue", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowFull, DEall,
    { 16, 48, 48, 60, 60 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,_,_,_,X,_,X,_,X,_,_,_,B, },
      { B,_,B,X,B,X,B,X,B,X,B,_,B, },
      { B,_,X,_,X,_,X,_,X,_,X,_,B, },
      { B,X,B,X,B,X,B,X,B,X,B,X,B, },
      { B,_,X,_,X,_,X,_,X,_,X,_,B, },
      { B,_,B,X,B,X,B,X,B,X,B,X,B, },
      { B,_,_,_,X,_,X,_,X,_,_,_,B, },
      { B,X,B,X,B,X,B,X,B,X,B,_,B, },
      { B,_,X,_,X,_,X,_,X,_,X,_,B, },
      { B,X,B,X,B,X,B,X,B,X,B,X,B, },
      { B,_,X,_,X,_,X,_,X,_,X,_,B, },
      { B,_,B,X,B,X,B,X,B,X,B,_,B, },
      { B,_,_,_,X,_,X,_,X,_,_,_,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
