/* XBlast 2.1.8 level */
static BMLevelData Closet_Psycho =
{
  /* BMLevel */
  {
    "Closet Psycho",
    "Garth Denley",
    "xblast.useClosetPsycho",
    "Remember that the pump works around corners",
    GM_Random | GM_23456_Player | GM_All,
    (void *) &Closet_Psycho,
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
    special_extra_air,
    special_key_air,
  },
  /* BMPlayerData */
  {
    1, 2,
    {
      {  2,  2 },
      {  2, 12 },
      { 10, 12 },
      { 10,  2 },
      {  5,  7 },
      {  7,  7 },
    },
    PM_Inner, 3,
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
      { BLChessFloor, "Black", "Salmon", "Black" },
      { BLChessFloor_S, "Black", "Salmon", "Black" },
      { BLBookShelf, "Black", "MidnightBlue", "BurlyWood" },
      { BLBookShelf, "Black", "RoyalBlue", "White" },
      { BLChessSphere, "Black", "Salmon", "SeaGreen" },
      { BLChessSphere_O, "Black", "Salmon", "SeaGreen" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_AIRPUMP,
      { BLScoreFloor, "RoyalBlue", "RoyalBlue", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowBlock, DEall,
    { 20, 40, 45, 57, 57 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,B,X,B,_,X,_,X,_,B,X,B,B, },
      { B,X,_,_,_,B,X,B,_,_,_,X,B, },
      { B,B,X,B,_,B,_,B,_,B,X,B,B, },
      { B,X,_,X,_,B,X,B,_,X,_,X,B, },
      { B,X,_,B,X,_,_,_,X,B,_,X,B, },
      { B,B,X,B,X,_,B,_,X,B,X,B,B, },
      { B,_,_,B,X,_,X,_,X,B,_,_,B, },
      { B,B,X,B,X,_,B,_,X,B,X,B,B, },
      { B,X,_,B,X,_,_,_,X,B,_,X,B, },
      { B,X,_,X,_,B,X,B,_,X,_,X,B, },
      { B,B,X,B,_,B,_,B,_,B,X,B,B, },
      { B,X,_,_,_,B,X,B,_,_,_,X,B, },
      { B,B,X,B,_,X,_,X,_,B,X,B,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
