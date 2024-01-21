/* XBlast 2.1.8 level */
static BMLevelData ScoreBoard3x2 =
{
  /* BMLevel */
  {
    "SCOREBOARD",
    "xblast.useScoreBoard",
    "",
    "Even this level needs no description",
    GM_6_Player | GM_NoGrid | GM_Team,
    (void *) &ScoreBoard3x2,
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
      { BLOCK_HEIGHT*7/2, BLOCK_WIDTH*4 },
      { BLOCK_HEIGHT*7/2, BLOCK_WIDTH*5 },
      { BLOCK_HEIGHT*13/2, BLOCK_WIDTH*4 },
      { BLOCK_HEIGHT*13/2, BLOCK_WIDTH*5 },
      { BLOCK_HEIGHT*19/2, BLOCK_WIDTH*4 },
      { BLOCK_HEIGHT*19/2, BLOCK_WIDTH*5 },
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
      { BLScoreFloor, "Black", "Black", "Black" },
      { BLScoreFloor, "RoyalBlue", "RoyalBlue", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowNone, DEnone,
    { 0, 0, 0, 0, 0 },
    {
      { 7,7,7,6,0,1,6,0,1,6,0,1,6, },
      { 7,7,7,6,0,1,6,0,1,6,0,1,6, },
      { 7,7,7,6,0,1,6,0,1,6,0,1,6, },
      { 7,7,7,6,2,3,6,2,3,6,2,3,6, },
      { 7,7,7,6,4,5,6,4,5,6,4,5,6, },
      { 7,7,7,6,4,5,6,4,5,6,4,5,6, },
      { 7,7,7,6,4,5,6,4,5,6,4,5,6, },
      { 7,7,7,6,4,5,6,4,5,6,4,5,6, },
      { 7,7,7,6,4,5,6,4,5,6,4,5,6, },
      { 7,7,7,6,4,5,6,4,5,6,4,5,6, },
      { 7,7,7,6,4,5,6,4,5,6,4,5,6, },
      { 7,7,7,6,4,5,6,4,5,6,4,5,6, },
      { 7,7,7,6,4,5,6,4,5,6,4,5,6, },
      { 7,7,7,6,4,5,6,4,5,6,4,5,6, },
      { 7,7,7,6,4,5,6,4,5,6,4,5,6, },
    },
  },
};
