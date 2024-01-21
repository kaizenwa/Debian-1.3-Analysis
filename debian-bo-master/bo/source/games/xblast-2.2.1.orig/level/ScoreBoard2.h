/* XBlast 2.1.8 level */
static BMLevelData ScoreBoard2 =
{
  /* BMLevel */
  {
    "SCOREBOARD",
    "xblast.useScoreBoard",
    "",
    "Even this level needs no description",
    GM_2_Player | GM_NoGrid | GM_Single | GM_Double,
    (void *) &ScoreBoard2,
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
      { BLOCK_HEIGHT*9/2, BLOCK_WIDTH*4 },
      { BLOCK_HEIGHT*17/2, BLOCK_WIDTH*4 },
      { -1, -1 },
      { -1, -1 },
      { -1, -1 },
      { -1, -1 },
    },
    PM_Right, 1,
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
      { 7,7,7,6,6,0,1,6,6,0,1,6,6, },
      { 7,7,7,6,6,0,1,6,6,0,1,6,6, },
      { 7,7,7,6,6,0,1,6,6,0,1,6,6, },
      { 7,7,7,6,6,2,3,6,6,2,3,6,6, },
      { 7,7,7,6,6,4,5,6,6,4,5,6,6, },
      { 7,7,7,6,6,4,5,6,6,4,5,6,6, },
      { 7,7,7,6,6,4,5,6,6,4,5,6,6, },
      { 7,7,7,6,6,4,5,6,6,4,5,6,6, },
      { 7,7,7,6,6,4,5,6,6,4,5,6,6, },
      { 7,7,7,6,6,4,5,6,6,4,5,6,6, },
      { 7,7,7,6,6,4,5,6,6,4,5,6,6, },
      { 7,7,7,6,6,4,5,6,6,4,5,6,6, },
      { 7,7,7,6,6,4,5,6,6,4,5,6,6, },
      { 7,7,7,6,6,4,5,6,6,4,5,6,6, },
      { 7,7,7,6,6,4,5,6,6,4,5,6,6, },
    },
  },
};
