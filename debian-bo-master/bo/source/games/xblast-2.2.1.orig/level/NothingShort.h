/* XBlast 2.1.8 level */
static BMLevelData NothingShort =
{
  /* BMLevel */
  {
    "Nothing Short of Warfare",
    "Garth Denley",
    "xblast.useNothingShort",
    "Beware! You have NAPALM bombs as default!",
    GM_Random | GM_23456_Player | GM_All,
    (void *) &NothingShort,
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
    special_init_special_bombs_30,
    special_game_void,
    special_extra_void,
    special_key_special_bomb,
  },
  /* BMPlayerData */
  {
    3, 8,
    {
      {  1,  1 },
      {  1, 13 },
      { 11, 13 },
      { 11,  1 },
      {  5,  7 },
      {  7,  7 },
    },
    PM_Inner, 4,
    Healthy, IF_Kick,
  },
  /* BMBombData */
  {
    bomb_click_none, bomb_click_none, bomb_click_thru,
    GoStop, FUSEnormal,
    BMTnapalm, BMTfirecracker, BMTnormal,
  },
  /* BMGraphicsData */
  {
    {
      { BLRockFloor, "Black", "SandyBrown", "Black" },
      { BLRockFloor_S, "Black", "SandyBrown", "Black" },
      { BLWeight, "Black", "Azure", "Gold" },
      { BLWeightRise, "Black", "LightSteelBlue", "Black" },
      { BLBricks, "Black", "OliveDrab", "Black" },
      { BLBricks_O, "Black", "OliveDrab", "Black" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      { BLScoreFloor, "RoyalBlue", "RoyalBlue", "RoyalBlue" },
      { BLScoreFloor, "RoyalBlue", "RoyalBlue", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowBlock, DEnone,
    { 0, 0, 0, 0, 0 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,_,_,_,_,_,X,_,_,_,_,_,B, },
      { B,_,_,_,_,_,X,_,_,_,_,_,B, },
      { B,_,_,_,X,B,X,B,X,_,_,_,B, },
      { B,_,_,X,_,X,_,X,_,X,_,_,B, },
      { B,_,_,X,_,_,_,_,_,X,_,_,B, },
      { B,_,_,X,_,_,_,_,_,X,_,_,B, },
      { B,X,X,B,X,_,B,_,X,B,X,X,B, },
      { B,_,_,X,_,_,_,_,_,X,_,_,B, },
      { B,_,_,X,_,_,_,_,_,X,_,_,B, },
      { B,_,_,X,_,X,_,X,_,X,_,_,B, },
      { B,_,_,_,X,B,X,B,X,_,_,_,B, },
      { B,_,_,_,_,_,X,_,_,_,_,_,B, },
      { B,_,_,_,_,_,X,_,_,_,_,_,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
