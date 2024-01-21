/* XBlast 2.1.8 level */
static BMLevelData Four_Corners =
{
  /* BMLevel */
  {
    "Four Corners",
    "Garth Denley",
    "xblast.useFourCorners",
    "Don't hide too far in the corner",
    GM_Random | GM_234_Player | GM_All,
    (void *) &Four_Corners,
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
      {  3,  3 },
      {  3, 11 },
      {  9, 11 },
      {  9,  3 },
      { -1, -1 },
      { -1, -1 },
    },
    PM_Horizontal, 2,
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
      { BLIronFloor, "Black", "ForestGreen", "Yellow" },
      { BLIronFloor_S, "Black", "ForestGreen", "Yellow" },
      { BLDarkBlock, "Black", "Red", "PeachPuff" },
      { BLDarkBlockRise, "Black", "Red", "PEachPuff" },
      { BLExtra, "Black", "SaddleBrown", "Black" },
      { BLExtra, "Black", "SaddleBrown", "Black" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_KICK,
      { BLScoreFloor, "RoyalBlue", "RoyalBlue", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowBlock, DEnone,
    { 0, 0, 0, 0, 0 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,_,_,_,_,_,B,_,_,_,_,_,B, },
      { B,_,_,_,_,_,B,_,_,_,_,_,B, },
      { B,_,_,_,_,_,B,_,_,_,_,_,B, },
      { B,_,_,_,_,_,B,_,_,_,_,_,B, },
      { B,_,_,_,_,_,B,_,_,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,B,B,B,B,_,B,_,B,B,B,B,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,_,_,B,_,_,_,_,_,B, },
      { B,_,_,_,_,_,B,_,_,_,_,_,B, },
      { B,_,_,_,_,_,B,_,_,_,_,_,B, },
      { B,_,_,_,_,_,B,_,_,_,_,_,B, },
      { B,_,_,_,_,_,B,_,_,_,_,_,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
