/* XBlast 2.1.8 level */
static BMLevelData Congratulations5 =
{
  /* BMLevel */
  {
    "CONGRATULATIONS",
    "",
    "xblast.useCongratulations",
    "This level really needs no description",
    GM_5_Player | GM_NoGrid | GM_Single,
    (void *) &Congratulations5,
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
    0, 0,
    {
      { BLOCK_HEIGHT*19/2, BLOCK_WIDTH*1 },
      { BLOCK_HEIGHT*19/2, BLOCK_WIDTH*5 },
      { BLOCK_HEIGHT*19/2, BLOCK_WIDTH*9 },
      { BLOCK_HEIGHT*19/2, BLOCK_WIDTH*13 },
      { BLOCK_HEIGHT*13/2, BLOCK_WIDTH*7 },
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
      { BLScoreRightUp, "Black", "LightSteelBlue", "Black" },
      { BLScoreRightDown, "Black", "FireBrick1", "Black" },
      { BLScoreMidUp, "Black", "SpringGreen", "LightSteelBlue" },
      { BLScoreMidDown, "Black", "Firebrick1", "LightSteelBlue" },
      { BLScoreLeftUp, "Black", "SpringGreen", "LightSteelBlue" },
      { BLScoreLeftDown, "Black", "Firebrick1", "LightSteelBlue" },
      { BLScoreFloor, "Black", "SpringGreen", "Black" },
      { BLScoreStep, "Black", "Firebrick1", "LightSteelBlue" },
      { BLScoreDrop, "Black", "Firebrick1", "SpringGreen" },
      { BLScoreFloor, "Black", "SteelBlue", "RoyalBlue" },
      { BLScoreFloor, "RoyalBlue", "RoyalBlue", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowNone, DEnone,
    { 0, 0, 0, 0, 0 },
    {
      { 7,7,7,8,6,6,6,6,6,6,6,6,6, },
      { 7,7,7,8,6,6,6,6,6,6,4,5,6, },
      { 7,7,7,8,6,6,6,6,6,6,6,6,6, },
      { 7,7,7,8,6,6,6,6,6,6,6,6,6, },
      { 7,7,7,8,6,6,6,6,6,6,6,6,6, },
      { 7,7,7,8,6,6,6,6,6,6,4,5,6, },
      { 7,7,7,8,6,6,0,0,1,6,6,6,6, },
      { 7,7,7,8,6,6,0,0,1,6,6,6,6, },
      { 7,7,7,8,6,6,0,0,1,6,6,6,6, },
      { 7,7,7,8,6,6,6,6,6,6,4,5,6, },
      { 7,7,7,8,6,6,6,6,6,6,6,6,6, },
      { 7,7,7,8,6,6,6,6,6,6,6,6,6, },
      { 7,7,7,8,6,6,6,6,6,6,6,6,6, },
      { 7,7,7,8,6,6,6,6,6,6,4,5,6, },
      { 7,7,7,8,6,6,6,6,6,6,6,6,6, },
    },
  },
};
