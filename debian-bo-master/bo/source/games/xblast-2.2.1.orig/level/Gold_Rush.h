/* XBlast 2.1.8 level */
static BMLevelData Gold_Rush =
{
  /* BMLevel */
  {
    "La Ruee Vers L'Or",
    "Laurent Marsan",
    "xblast.useGoldRush",
    "Teamwork might be useful",
    GM_2_Player | GM_4_Player | GM_Single,
    (void *) &Gold_Rush,
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
      {  5,  1 },
      {  5,  2 },
      {  7,  2 },
      {  7,  1 },
      { -1, -1 },
      { -1, -1 },
    },
    PM_Same, 0,
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
      { BLHex, "Black", "Sienna", "ForestGreen" },
      { BLHex, "Black", "Sienna", "ForestGreen" },
      { BLHexWall, "Black", "Sienna", "RoyalBlue" },
      { BLHexExtra, "Black", "Sienna", "RoyalBlue" },
      { BLHexExtra, "Black", "Sienna", "Gold" },
      { BLHexExtra_O, "Black", "Sienna", "Gold" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_KICK,
      { BLScoreFloor, "RoyalBlue", "RoyalBlue", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowNone, DEall,
    { 16, 48, 48, 48, 48 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,X,X,B,_,_,B,_,_,B,X,X,B, },
      { B,X,X,X,_,_,B,_,_,X,X,X,B, },
      { B,X,X,B,B,B,B,B,B,B,X,X,B, },
      { B,s,s,B,_,_,_,_,_,B,s,s,B, },
      { B,s,s,B,_,B,B,B,_,B,s,s,B, },
      { B,s,s,B,_,_,_,_,_,B,s,s,B, },
      { B,s,s,B,_,B,_,B,_,B,s,s,B, },
      { B,s,s,B,_,_,_,_,_,B,s,s,B, },
      { B,s,s,B,_,B,B,B,_,B,s,s,B, },
      { B,s,s,B,_,_,_,_,_,B,s,s,B, },
      { B,X,X,B,B,B,X,B,B,B,X,X,B, },
      { B,X,X,b,r,b,r,b,r,b,X,X,B, },
      { B,q,X,r,b,r,b,r,b,r,X,q,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
