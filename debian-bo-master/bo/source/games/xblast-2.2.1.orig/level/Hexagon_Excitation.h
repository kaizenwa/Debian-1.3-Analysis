/* XBlast 2.1.8 level */
static BMLevelData Hexagon_Excitation =
{
  /* BMLevel */
  {
    "Hexagon Excitation",
    "Oliver Vogel",
    "xblast.useHexagonExcitation",
    "Get the kick bomb extra",
    GM_Random | GM_23456_Player | GM_All,
    (void *) &Hexagon_Excitation,
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
    special_extra_kick,
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
      {  3,  7 },
      {  9,  7 },
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
      { BLHex, "Black", "SeaGreen", "BlueViolet" },
      { BLHex, "Black", "SeaGreen", "BlueViolet" },
      { BLHexWall, "Black", "SeaGreen", "NavyBlue" },
      { BLHexExtra, "Black", "SeaGreen", "RoyalBlue" },
      { BLHexExtra, "Black", "SeaGreen", "SpringGreen" },
      { BLHexExtra_O, "Black", "SeaGreen", "SpringGreen" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_KICK,
      { BLScoreFloor, "RoyalBlue", "RoyalBlue", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowNone, DEall,
    { 16, 48, 52, 60, 60 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,_,_,_,X,_,B,_,X,_,_,_,B, },
      { B,_,B,X,B,X,B,X,B,X,B,_,B, },
      { B,_,X,X,X,_,X,_,X,X,X,_,B, },
      { B,X,B,X,B,X,B,X,B,X,B,X,B, },
      { B,_,X,B,X,_,X,_,X,B,X,_,B, },
      { B,X,B,_,B,X,B,X,B,_,B,X,B, },
      { B,X,X,_,_,X,X,X,_,_,X,X,B, },
      { B,X,B,_,B,X,B,X,B,_,B,X,B, },
      { B,_,X,B,X,_,X,_,X,B,X,_,B, },
      { B,X,B,X,B,X,B,X,B,X,B,X,B, },
      { B,_,X,X,X,_,X,_,X,X,X,_,B, },
      { B,_,B,X,B,X,B,X,B,X,B,_,B, },
      { B,_,_,_,X,_,B,_,X,_,_,_,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
