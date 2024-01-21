/* XBlast 2.1.8 level */
static BMLevelData Gravitation =
{
  /* BMLevel */
  {
    "Gravitation",
    "Oliver Vogel",
    "xblast.useGravitation",
    "Catch your foe with falling bombs",
    GM_Random | GM_23456_Player | GM_All,
    (void *) &Gravitation,
    NULL,
  },
  /* BMShrinkData */
  {
    shrink_down,
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
    1, 2,
    {
      {  1,  5 },
      {  1,  9 },
      {  1, 13 },
      {  1,  1 },
      {  1,  3 },
      {  1, 11 },
    },
    PM_Below, 1,
    Healthy, IF_None,
  },
  /* BMBombData */
  {
    bomb_click_none, bomb_click_none, bomb_click_initial,
    GoDown, FUSEnormal,
    BMTnormal, BMTnormal, BMTnormal,
  },
  /* BMGraphicsData */
  {
    {
      { BLIronFloor, "Black", "LightSeaGreen", "Black" },
      { BLIronFloor_S, "Black", "LightSeaGreen", "Black" },
      { BLDarkBlock, "Black", "Orchid", "Orchid" },
      { BLDarkBlockRise, "DarkOrchid", "Orchid", "Orchid" },
      { BLBricks, "Black", "Firebrick1", "Black" },
      { BLBricks_O, "Black", "Firebrick1", "Black" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      { BLScoreFloor, "Black", "Black", "Black" },
      { BLScoreFloor, "Black", "Firebrick", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowFull, DEnone,
    { 12, 28, 28, 28, 36 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,_,_,_,_,_,X,_,X,_,X,X,B, },
      { B,B,B,B,B,X,B,X,B,X,B,X,B, },
      { B,_,_,_,_,_,X,_,X,_,X,X,B, },
      { B,B,B,B,B,X,B,X,B,X,B,X,B, },
      { B,_,_,_,_,_,X,_,X,_,X,X,B, },
      { B,B,B,B,B,X,B,X,B,X,B,X,B, },
      { B,_,_,_,_,_,X,_,X,_,X,X,B, },
      { B,B,B,B,B,X,B,X,B,X,B,X,B, },
      { B,_,_,_,_,_,X,_,X,_,X,X,B, },
      { B,B,B,B,B,X,B,X,B,X,B,X,B, },
      { B,_,_,_,_,_,X,_,X,_,X,X,B, },
      { B,B,B,B,B,X,B,X,B,X,B,X,B, },
      { B,_,_,_,_,_,X,_,X,_,X,X,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
