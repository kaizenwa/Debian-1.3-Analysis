/* XBlast 2.1.8 level */
static BMLevelData Running_Man =
{
  /* BMLevel */
  {
    "Running Man",
    "Oliver Vogel",
    "xblast.useRunningMan",
    "Drop - Run - Ignition",
    GM_Random | GM_23456_Player | GM_All,
    (void *) &Running_Man,
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
      {  5,  7 },
      {  7,  7 },
    },
    PM_Polar, 2,
    IllRun, IF_None,
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
      { BLChessFloor, "Black", "PapayaWhip", "Black" },
      { BLChessFloor_S, "Black", "PapayaWhip", "Black" },
      { BLBookShelf, "Black", "LimeGreen", "Chocolate" },
      { BLBookShelf, "Black", "Firebrick3", "Chocolate" },
      { BLChessSphere, "Black", "PapayaWhip", "CornflowerBlue" },
      { BLChessSphere_O, "Black", "PapayaWhip", "CornflowerBlue" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_RC,
      { BLScoreFloor, "RoyalBlue", "RoyalBlue", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowBlock, DEall,
    { 20, 36, 36, 48, 48 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,_,_,_,X,_,X,_,X,_,_,_,B, },
      { B,_,B,X,B,X,B,X,B,X,B,_,B, },
      { B,_,X,_,_,_,X,_,_,_,X,_,B, },
      { B,_,B,_,R,X,R,X,R,_,B,_,B, },
      { B,_,X,_,X,_,X,_,X,_,X,_,B, },
      { B,X,B,X,R,_,B,_,R,X,B,X,B, },
      { B,_,X,_,X,_,_,_,X,_,X,_,B, },
      { B,X,B,X,R,_,B,_,R,X,B,X,B, },
      { B,_,X,_,X,_,X,_,X,_,X,_,B, },
      { B,_,B,_,R,X,R,X,R,_,B,_,B, },
      { B,_,X,_,_,_,X,_,_,_,X,_,B, },
      { B,_,B,X,B,X,B,X,B,X,B,_,B, },
      { B,_,_,_,X,_,X,_,X,_,_,_,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
