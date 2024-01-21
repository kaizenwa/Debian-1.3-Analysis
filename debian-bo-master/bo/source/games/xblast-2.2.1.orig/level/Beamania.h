/* XBlast 2.1.8 level */
static BMPosition BlastFreeDel[] = { {6,7} };
static BMLevelData BlastFree =
{
  /* BMLevel */
  {
    "Beamania",
    "Oliver Vogel",
    "xblast.useBeamania",
    "Don't beam when the level shrinks",
    GM_Random | GM_23456_Player | GM_Single | GM_Team,
    (void *) &BlastFree,
    NULL,
  },
  /* BMShrinkData */
  {
    shrink_compound,
    SCRAMBLE_VOID,
    { 3*GAME_TIME/4+4, 1, BlastFreeDel, },
  },
  /* BMFuncData */
  {
    special_init_void,
    special_game_void,
    special_extra_ignite_all,
    special_key_teleport,
  },
  /* BMPlayerData */
  {
    1, 2,
    {
      {  2,  2 },
      {  2, 12 },
      { 10, 12 },
      { 10,  2 },
      {  2,  6 },
      { 10,  8 },
    },
    PM_Same, 0,
    Healthy, IF_Teleport,
  },
  /* BMBombData */
  {
    bomb_click_none, bomb_click_none, bomb_click_none,
    GoStop, FUSEshort,
    BMTnormal, BMTnormal, BMTnormal,
  },
  /* BMGraphicsData */
  {
    {
      { BLMrBeamFree, "Black", "MediumSeaGreen", "Black" },
      { BLMrBeamFree, "Black", "MediumSeaGreen", "Black" },
      { BLBookShelf, "Black", "Red", "BurlyWood" },
      { BLBookShelf, "Black", "RoyalBlue", "BurlyWood" },
      { BLMrBeamBear, "Black", "MediumSeaGreen", "OrangeRed" },
      { BLMrBeamBearExp, "Black", "MediumSeaGreen", "OrangeRed" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_BUTTON,
      { BLScoreFloor, "MidnightBlue", "MidnightBlue", "MidnightBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowBlock, DEnone,
    { 24, 24, 36, 36, 48 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,X,X,X,R,X,_,X,R,X,X,X,B, },
      { B,X,_,X,R,X,X,X,R,X,_,X,B, },
      { B,X,X,X,R,R,R,R,R,X,X,X,B, },
      { B,R,R,R,R,X,X,X,R,R,R,R,B, },
      { B,X,X,X,R,X,_,X,R,X,X,X,B, },
      { B,X,_,X,R,X,X,X,R,X,_,X,B, },
      { B,R,R,R,R,R,B,R,R,R,R,R,B, },
      { B,X,_,X,R,X,X,X,R,X,_,X,B, },
      { B,X,X,X,R,X,_,X,R,X,X,X,B, },
      { B,R,R,R,R,X,X,X,R,R,R,R,B, },
      { B,X,X,X,R,R,R,R,R,X,X,X,B, },
      { B,X,_,X,R,X,X,X,R,X,_,X,B, },
      { B,X,X,X,R,X,_,X,R,X,X,X,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
