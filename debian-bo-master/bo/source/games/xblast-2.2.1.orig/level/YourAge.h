/* XBlast 2.1.8 level */
static BMLevelData YourAge =
{
  /* BMLevel */
  {
    "When I was Your Age",
    "Garth Denley",
    "xblast.useYourAge",
    "Use the long fuses to set up traps",
    GM_Random | GM_23456_Player | GM_All,
    (void *) &YourAge,
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
    special_extra_void,
    special_key_void,
  },
  /* BMPlayerData */
  {
    7, 3,
    {
      {  4,  5 },
      {  4,  9 },
      {  8,  9 },
      {  8,  5 },
      {  6,  6 },
      {  6,  8 },
    },
    PM_Polar, 1,
    Healthy, IF_None,
  },
  /* BMBombData */
  {
    bomb_click_none, bomb_click_none, bomb_click_none,
    GoStop, FUSElong,
    BMTnormal, BMTnormal, BMTnormal,
  },
  /* BMGraphicsData */
  {
    {
      { BLCityFree, "Black", "Gray65", "Gray40" },
      { BLCityFree_S, "Black", "Gray65", "Gray40" },
      { BLDarkHouse, "Black", "Firebrick", "Burlywood" },
      { BLDarkHouseRise, "Black", "Firebrick", "Burlywood" },
      { BLScoreFloor, "Black", "Tan", "Black" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      { BLIronFloor, "Black", "Gray25", "OrangeRed" },
      { BLIronFloor, "Black", "Gray25", "OrangeRed" },
      { BLScoreFloor, "RoyalBlue", "RoyalBlue", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowFull, DEnone,
    { 0, 0, 0, 0, 0 },
    {
      { q,q,q,q,B,X,X,X,B,q,q,q,q, },
      { q,q,q,q,B,X,X,X,B,q,q,q,q, },
      { q,q,q,q,B,X,X,X,B,q,q,q,q, },
      { q,q,B,B,B,B,B,B,B,B,B,q,q, },
      { B,B,B,_,_,_,_,_,_,_,B,B,B, },
      { X,X,B,_,_,_,_,_,_,_,B,X,X, },
      { X,X,B,_,_,B,_,B,_,_,B,X,X, },
      { X,X,B,_,_,_,_,_,_,_,B,X,X, },
      { X,X,B,_,_,B,_,B,_,_,B,X,X, },
      { X,X,B,_,_,_,_,_,_,_,B,X,X, },
      { B,B,B,_,_,_,_,_,_,_,B,B,B, },
      { q,q,B,B,B,B,B,B,B,B,B,q,q, },
      { q,q,q,q,B,X,X,X,B,q,q,q,q, },
      { q,q,q,q,B,X,X,X,B,q,q,q,q, },
      { q,q,q,q,B,X,X,X,B,q,q,q,q, },
    },
  },
};
