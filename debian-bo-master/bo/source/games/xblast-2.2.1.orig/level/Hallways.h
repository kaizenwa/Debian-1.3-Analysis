/* XBlast 2.1.8 level */
static BMLevelData Hallways =
{
  /* BMLevel */
  {
    "Hallways",
    "Rob Hite",
    "xblast.useHallways",
    "This level needs a description",
    GM_Random | GM_23456_Player | GM_All,
    (void *) &Hallways,
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
    1, 4,
    {
      {  1,  1 },
      {  1, 13 },
      { 11, 13 },
      { 11,  1 },
      {  6,  1 },
      {  6, 13 },
    },
    PM_Vertical, 2,
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
      { BLKaroLight, "Black", "LimeGreen", "GreenYellow" },
      { BLKaroLight_S, "Black", "LimeGreen", "GreenYellow" },
      { BLPyramid, "Black", "IndianRed", "Black" },
      { BLTemple, "Black", "Tan", "AntiqueWhite" },
      { BLExtra, "Black", "SlateBlue", "OrangeRed" },
      { BLExtra_O, "Black", "SlateBlue", "OrangeRed" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_RC,
      { BLScoreFloor, "RoyalBlue", "RoyalBlue", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowFull, DEsingle,
    { 20, 35, 40, 60, 63 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,_,_,_,X,_,_,_,X,_,_,_,B, },
      { B,_,R,_,R,_,R,_,R,_,R,_,B, },
      { B,_,R,_,R,_,R,_,R,_,R,_,B, },
      { B,_,R,_,R,_,R,_,R,_,R,_,B, },
      { B,_,R,r,R,b,R,r,R,b,R,_,B, },
      { B,X,R,X,R,X,R,X,R,X,R,X,B, },
      { B,b,X,q,X,q,X,q,X,q,X,r,B, },
      { B,X,R,X,R,X,R,X,R,X,R,X,B, },
      { B,_,R,b,R,r,R,b,R,r,R,_,B, },
      { B,_,R,_,R,_,R,_,R,_,R,_,B, },
      { B,_,X,_,X,_,X,_,X,_,X,_,B, },
      { B,_,R,_,R,_,R,_,R,_,R,_,B, },
      { B,_,_,_,X,_,_,_,X,_,_,_,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
