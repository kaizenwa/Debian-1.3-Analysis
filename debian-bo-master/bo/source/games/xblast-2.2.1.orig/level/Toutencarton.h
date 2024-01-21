/* XBlast 2.1.8 level */
static BMLevelData Toutencarton =
{
  /* BMLevel */
  {
    "Toutencarton",
    "Laurent Marsan",
    "xblast.useToutencarton",
    "Getting the Remote Control is imperative",
    GM_Random | GM_234_Player | GM_All,
    (void *) &Toutencarton,
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
    special_extra_RC,
    special_key_RC,
  },
  /* BMPlayerData */
  {
    1, 2,
    {
      {  1,  1 },
      {  1, 13 },
      { 11, 13 },
      { 11,  1 },
      { -1, -1 },
      { -1, -1 },
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
      { BLKaroDark, "MidnightBlue", "SpringGreen", "Cyan" },
      { BLKaroDark_S, "MidnightBlue", "SpringGreen", "Cyan" },
      { BLWall, "Black", "ForestGreen", "Black" },
      { BLWallRise, "Black", "ForestGreen", "Black" },
      { BLChest, "Black", "RoyalBlue", "Gray80" },
      { BLChest_O, "Black", "RoyalBlue", "Gray80" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_RC,
      { BLScoreFloor, "RoyalBlue", "RoyalBlue", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowFull, DEsingle,
    { 25, 50, 63, 0, 0 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,_,_,X,r,b,r,b,r,X,_,_,B, },
      { B,_,B,B,B,B,X,B,B,B,B,_,B, },
      { B,X,B,_,_,X,s,X,_,_,B,X,B, },
      { B,r,B,_,B,B,X,B,B,_,B,r,B, },
      { B,b,B,_,B,s,s,s,B,_,B,b,B, },
      { B,r,B,X,B,s,B,s,B,X,B,r,B, },
      { B,b,X,s,X,s,q,s,X,s,X,b,B, },
      { B,r,B,X,B,s,B,s,B,X,B,r,B, },
      { B,b,B,_,B,s,s,s,B,_,B,b,B, },
      { B,r,B,_,B,B,X,B,B,_,B,r,B, },
      { B,X,B,_,_,X,s,X,_,_,B,X,B, },
      { B,_,B,B,B,B,X,B,B,B,B,_,B, },
      { B,_,_,X,r,b,r,b,r,X,_,_,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
