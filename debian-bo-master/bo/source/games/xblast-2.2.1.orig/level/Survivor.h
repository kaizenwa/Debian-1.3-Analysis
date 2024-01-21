/* XBlast 2.1.8 level */
static BMLevelData Survivor =
{
  /* BMLevel */
  {
    "Survivor",
    "Laurent Marsan",
    "xblast.useSurvivor",
    "Just try to survive  it is hard enough",
    GM_Random | GM_23456_Player | GM_All,
    (void *) &Survivor,
    NULL,
  },
  /* BMShrinkData */
  {
    shrink_savage_compound,
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
    10, 10,
    {
      {  6,  7 },
      {  6,  7 },
      {  6,  7 },
      {  6,  7 },
      {  6,  7 },
      {  6,  7 },
    },
    PM_LeftRight, 1,
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
      { BLChessFloor, "Black", "MintCream", "Black" },
      { BLChessFloor_S, "Black", "MintCream", "Black" },
      { BLBookShelf, "Black", "SpringGreen", "SlateBlue" },
      { BLBookShelf, "Black", "SpringGreen", "SlateBlue" },
      { BLChessSphere, "Black", "MintCream", "Salmon" },
      { BLChessSphere_O, "Black", "MintCream", "Salmon" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_RC,
      { BLScoreFloor, "RoyalBlue", "RoyalBlue", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowBlock, DEnone,
    { 0, 0, 0, 0, 40 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,X,X,X,X,X,X,X,X,X,X,X,B, },
      { B,X,X,X,X,X,X,X,X,X,X,X,B, },
      { B,X,X,X,X,X,X,X,X,X,X,X,B, },
      { B,X,X,X,X,X,X,X,X,X,X,X,B, },
      { B,X,X,X,_,_,_,_,_,X,X,X,B, },
      { B,X,X,X,_,B,_,B,_,X,X,X,B, },
      { B,X,X,X,_,B,_,B,_,X,X,X,B, },
      { B,X,X,X,_,B,_,B,_,X,X,X,B, },
      { B,X,X,X,_,_,_,_,_,X,X,X,B, },
      { B,X,X,X,X,X,X,X,X,X,X,X,B, },
      { B,X,X,X,X,X,X,X,X,X,X,X,B, },
      { B,X,X,X,X,X,X,X,X,X,X,X,B, },
      { B,X,X,X,X,X,X,X,X,X,X,X,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
