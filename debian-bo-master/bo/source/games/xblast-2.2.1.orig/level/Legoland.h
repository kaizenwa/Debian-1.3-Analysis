/* XBlast 2.1.8 level */
static BMPosition Lego_Draw[] = {
  {3,3}, {3,11}, {9,11}, {9,3},
};
static BMLevelData Legoland =
{
  /* BMLevel */
  {
    "Legoland",
    "Oliver Vogel",
    "xblast.useLegoland",
    "Catch them in a corner",
    GM_Random | GM_23456_Player | GM_All,
    (void *) &Legoland,
    NULL,
  },
  /* BMShrinkData */
  {
    shrink_void,
    { 2*GAME_TIME/3, 4, Lego_Draw, },
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
      {  3,  3 },
      {  3, 11 },
      {  9, 11 },
      {  9,  3 },
      {  5,  7 },
      {  7,  7 },
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
      { BLLegoFloor, "Black", "ForestGreen", "ForestGreen" },
      { BLLegoFloor_S, "Black", "ForestGreen", "ForestGreen" },
      { BLLegoWhite, "Black", "Blue", "Black" },
      { BLLegoFloor, "Black", "ForestGreen", "Gray60" },
      { BLLegoBlack, "Red", "Red", "Black" },
      { BLLegoBlack_O, "Red", "Black", "Black" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      { BLScoreFloor, "Black", "Black", "Black" },
      { BLLegoFloor_S, "Black", "ForestGreen", "Gray60" },
    },
  },
  /* BMMapData */
  {
    ShadowFull, DEnone,
    { 18, 45, 61, 61, 61 },
    {
      { V,V,V,V,V,V,V,V,V,V,V,V,V, },
      { V,R,R,R,R,R,R,R,R,R,R,R,V, },
      { V,R,B,B,B,B,B,B,B,B,B,R,V, },
      { V,R,B,_,_,X,X,X,_,_,B,R,V, },
      { V,R,B,_,B,_,B,_,B,_,B,R,V, },
      { V,R,B,X,X,X,X,X,X,X,B,R,V, },
      { V,R,B,_,B,X,B,_,B,_,B,R,V, },
      { V,R,B,X,_,_,X,_,_,X,B,R,V, },
      { V,R,B,_,B,_,B,X,B,_,B,R,V, },
      { V,R,B,X,X,X,X,X,X,X,B,R,V, },
      { V,R,B,_,B,_,B,_,B,_,B,R,V, },
      { V,R,B,_,_,X,X,X,_,_,B,R,V, },
      { V,R,B,B,B,B,B,B,B,B,B,R,V, },
      { V,R,v,v,v,v,v,v,v,v,v,R,V, },
      { V,V,V,V,V,V,V,V,V,V,V,V,V, },
    },
  },
};
