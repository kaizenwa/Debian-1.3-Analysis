/* XBlast 2.1.8 level */
static BMPosition Haunted_Scramble[] = { 
  {5,6}, {5,8}, {7,6}, {7,8}, 
};
static BMLevelData Haunted_House =
{
  /* BMLevel */
  {
    "Haunted House",
    "Garth Denley",
    "xblast.useHauntedHouse",
    "Keep 2 steps away from haunted bombs",
    GM_Random | GM_234_Player | GM_All,
    (void *) &Haunted_House,
    NULL,
  },
  /* BMShrinkData */
  {
    shrink_spiral,
    SCRAMBLE_VOID,
    { 3*GAME_TIME/4, 4, Haunted_Scramble, },
  },
  /* BMFuncData */
  {
    special_init_void,
    special_game_haunt,
    special_extra_RC,
    special_key_RC,
  },
  /* BMPlayerData */
  {
    1, 2,
    {
      {  3,  1 },
      {  3, 13 },
      {  9, 13 },
      {  9,  1 },
      { -1, -1 },
      { -1, -1 },
    },
    PM_Vertical, -2,
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
      { BLCityFree, "Black", "Tan", "Sienna" },
      { BLCityFree_S, "Black", "Tan", "Sienna" },
      { BLRIP, "Black", "MintCream", "Tan" },
      { BLRIPRise, "Black", "MintCream", "Tan" },
      { BLLightHouse, "Black", "Gray30", "LightSlateBlue" },
      { BLLightHouse_O, "Black", "IndianRed", "LightSlateBlue" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_RC,
      { BLScoreFloor, "RoyalBlue", "RoyalBlue", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowFull, DEall,
    { 15, 28, 34, 40, 60 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,_,_,_,B,X,X,X,B,_,_,_,B, },
      { B,_,B,_,X,_,B,_,X,_,B,_,B, },
      { B,_,_,_,X,_,B,_,X,_,_,_,B, },
      { B,B,X,B,B,X,X,X,B,B,X,B,B, },
      { B,_,X,_,X,_,B,_,X,_,X,_,B, },
      { B,X,B,_,B,B,B,B,B,_,B,X,B, },
      { B,X,B,X,X,X,q,X,X,X,B,X,B, },
      { B,X,B,_,B,B,B,B,B,_,B,X,B, },
      { B,_,X,_,X,_,B,_,X,_,X,_,B, },
      { B,B,X,B,B,X,X,X,B,B,X,B,B, },
      { B,_,_,_,X,_,B,_,X,_,_,_,B, },
      { B,_,B,_,X,_,B,_,X,_,B,_,B, },
      { B,_,_,_,B,X,X,X,B,_,_,_,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
