/* XBlast 2.1.8 level */
static BMLevelData Shrinking_World =
{
  /* BMLevel */
  {
    "Shrinking World",
    "Oliver Vogel",
    "xblast.useShrinkingWorld",
    "Just get to the center  quick!",
    GM_Random | GM_23456_Player | GM_All,
    (void *) &Shrinking_World,
    NULL,
  },
  /* BMShrinkData */
  {
    shrink_compound,
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
    1, 2,
    {
      {  1,  1 },
      {  1, 13 },
      { 11, 13 },
      { 11,  1 },
      {  1,  7 },
      { 11,  7 },
    },
    PM_Polar, 2,
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
      { BLButtonFloor, "Black", "Gray75", "Black" },
      { BLButtonFloor, "Black", "Gray75", "Black" },
      { BLDarkBlock, "Black", "LightBlue", "SpringGreen" },
      { BLDarkBlockRise, "Black", "LightBlue", "SpringGreen" },
      { BLBox, "Black", "LightBlue", "Red" },
      { BLBox, "Black", "HotPink", "Red" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_KICK,
      { BLScoreFloor, "Red", "Red", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowNone, DEnone,
    { 16, 48, 48, 48, 48 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,_,_,_,X,_,X,_,X,_,_,_,B, },
      { B,_,B,X,B,X,B,X,B,X,B,_,B, },
      { B,_,X,_,X,_,X,_,X,_,X,_,B, },
      { B,X,B,X,B,X,B,X,B,X,B,X,B, },
      { B,_,X,_,X,_,X,_,X,_,X,_,B, },
      { B,_,B,X,B,X,B,X,B,X,B,X,B, },
      { B,_,_,_,X,_,X,_,X,_,_,_,B, },
      { B,X,B,X,B,X,B,X,B,X,B,_,B, },
      { B,_,X,_,X,_,X,_,X,_,X,_,B, },
      { B,X,B,X,B,X,B,X,B,X,B,X,B, },
      { B,_,X,_,X,_,X,_,X,_,X,_,B, },
      { B,_,B,X,B,X,B,X,B,X,B,_,B, },
      { B,_,_,_,X,_,X,_,X,_,_,_,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
