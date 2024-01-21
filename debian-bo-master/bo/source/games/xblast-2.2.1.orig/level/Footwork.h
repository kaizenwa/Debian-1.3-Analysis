/* XBlast 2.1.8 level */
static BMLevelData Footwork =
{
  /* BMLevel */
  {
    "Footwork",
    "Garth Denley",
    "xblast.useFootwork",
    "Watch your step",
    GM_Random | GM_23456_Player | GM_All,
    (void *) &Footwork,
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
    special_extra_ignite_all,
    special_key_RC,
  },
  /* BMPlayerData */
  {
    4, 1,
    {
      {  2,  3 },
      {  2, 11 },
      { 10, 11 },
      { 10,  3 },
      {  2,  7 },
      { 10,  7 },
    },
    PM_Vertical, 2,
    Healthy, IF_Kick | IF_RC,
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
      { BLLegoFloor, "Black", "RoyalBlue", "RoyalBlue" },
      { BLLegoFloor_S, "Black", "RoyalBlue", "RoyalBlue" },
      { BLLegoWhite, "Black", "Yellow", "Black" },
      { BLLegoFloor, "Black", "Yellow", "Black" },
      { BLLegoBlack, "White", "White", "Black" },
      { BLLegoBlack_O, "White", "Black", "Black" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_BUTTON,
      { BLScoreFloor, "RoyalBlue", "RoyalBlue", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowFull, DEget,
    { 30, 32, 48, 52, 54 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,X,X,X,B,_,X,_,B,X,X,X,B, },
      { B,X,X,_,B,_,B,_,B,_,X,X,B, },
      { B,X,_,_,_,X,X,X,_,_,_,X,B, },
      { B,B,B,X,X,B,_,B,X,X,B,B,B, },
      { B,_,_,X,_,B,B,B,_,X,_,_,B, },
      { B,B,_,B,_,_,_,_,_,B,_,B,B, },
      { B,X,_,B,_,X,B,X,_,B,_,X,B, },
      { B,B,_,B,_,_,_,_,_,B,_,B,B, },
      { B,_,_,X,_,B,B,B,_,X,_,_,B, },
      { B,B,B,X,X,B,_,B,X,X,B,B,B, },
      { B,X,_,_,_,X,X,X,_,_,_,X,B, },
      { B,X,X,_,B,_,B,_,B,_,X,X,B, },
      { B,X,X,X,B,_,X,_,B,X,X,X,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
