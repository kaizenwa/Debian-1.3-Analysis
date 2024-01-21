/* XBlast 2.1.8 level */
static BMPosition BeAJunkie_Dele[] = {
  {2,1}, {10,1}, {2,13}, {10,13}};
static BMLevelData BeAJunkie =
{
  /* BMLevel */
  {
    "Would ya' likes t'be some Junkie?",
    "Garth Denley",
    "xblast.useBeAJunkie",
    "If you want that power you'll have to become a junkie",
    GM_Random | GM_234_Player | GM_All,
    (void *) &BeAJunkie,
    NULL,
  },
  /* BMShrinkData */
  {
    shrink_compound_2_f,
    SCRAMBLE_VOID,
    { GAME_TIME/4, 4, BeAJunkie_Dele, },
  },
  /* BMFuncData */
  {
    special_init_void,
    special_game_void,
    special_extra_junkie,
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
      { -1, -1 },
      { -1, -1 },
    },
    PM_Horizontal, 5,
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
      { BLLegoFloor, "Black", "Gray80", "SkyBlue" },
      { BLLegoFloor_S, "Black", "Gray80", "SkyBlue" },
      { BLLegoWhite, "Black", "Pink", "Black" },
      { BLLegoBlack, "DeepPink", "HotPink", "Black" },
      { BLLegoBlack, "SpringGreen", "SpringGreen", "Black" },
      { BLLegoBlack_O, "SpringGreen", "Black", "Black" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_SYRINGE,
      { BLScoreFloor, "RoyalBlue", "RoyalBlue", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowFull, DEnone,
    { 0, 0, 0, 0, 0 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,_,B,b,b,b,r,b,b,b,B,_,B, },
      { B,_,_,B,b,B,s,B,b,B,_,_,B, },
      { B,_,_,B,b,b,s,b,b,B,_,_,B, },
      { B,_,B,q,b,r,r,r,b,q,B,_,B, },
      { B,_,q,b,B,b,b,b,B,b,q,_,B, },
      { B,_,B,q,b,b,B,b,b,q,B,_,B, },
      { B,B,B,B,b,b,s,b,b,B,B,B,B, },
      { B,_,B,q,b,b,B,b,b,q,B,_,B, },
      { B,_,q,b,B,b,b,b,B,b,q,_,B, },
      { B,_,B,q,b,r,r,r,b,q,B,_,B, },
      { B,_,_,B,b,b,s,b,b,B,_,_,B, },
      { B,_,_,B,b,B,s,B,b,B,_,_,B, },
      { B,_,B,b,b,b,r,b,b,b,B,_,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
