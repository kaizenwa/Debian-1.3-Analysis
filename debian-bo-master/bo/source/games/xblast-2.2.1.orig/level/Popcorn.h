/* XBlast 2.1.8 level */
static BMLevelData Popcorn =
{
  /* BMLevel */
  {
    "Popcorn",
    "Garth Denley",
    "xblast.usePopcorn",
    "Pepper your opponents with bombs",
    GM_Random | GM_23456_Player | GM_All,
    (void *) &Popcorn,
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
    special_extra_invincible,
    special_key_void,
  },
  /* BMPlayerData */
  {
    2, 1,
    {
      {  2,  2 },
      {  2, 12 },
      { 10, 12 },
      { 10,  2 },
      {  4,  7 },
      {  8,  7 },
    },
    PM_LeftRight, 1,
    Healthy, IF_Kick,
  },
  /* BMBombData */
  {
    bomb_click_contact, bomb_click_none, bomb_click_none,
    GoStop, FUSEnormal,
    BMTnormal, BMTnormal, BMTnormal,
  },
  /* BMGraphicsData */
  {
    {
      { BLLegoFloor, "Black", "MediumSeagreen", "SpringGreen" },
      { BLLegoFloor_S, "Black", "MediumSeagreen", "SpringGreen" },
      { BLLegoWhite, "Black", "Gold", "Black" },
      { BLLegoWhite, "Black", "LightYellow", "Black" },
      { BLLegoBlack, "LightSteelBlue", "LightSteelBlue", "Black" },
      { BLLegoBlack_O, "LightSteelBlue", "Black", "Black" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_INVINC,
      { BLScoreFloor, "RoyalBlue", "RoyalBlue", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowFull, DEnone,
    { 40, 40, 40, 50, 50 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,X,_,_,_,X,X,X,_,_,_,X,B, },
      { B,_,_,X,B,B,_,B,B,X,_,_,B, },
      { B,X,_,_,_,_,_,_,_,_,_,X,B, },
      { B,_,B,_,X,X,_,X,X,_,B,_,B, },
      { B,_,B,_,B,X,_,X,B,_,B,_,B, },
      { B,_,X,_,_,_,X,_,_,_,X,_,B, },
      { B,_,_,_,_,X,X,X,_,_,_,_,B, },
      { B,_,X,_,_,_,X,_,_,_,X,_,B, },
      { B,_,B,_,B,X,_,X,B,_,B,_,B, },
      { B,_,B,_,X,X,_,X,X,_,B,_,B, },
      { B,X,_,_,_,_,_,_,_,_,_,X,B, },
      { B,_,_,X,B,B,_,B,B,X,_,_,B, },
      { B,X,_,_,_,X,X,X,_,_,_,X,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
