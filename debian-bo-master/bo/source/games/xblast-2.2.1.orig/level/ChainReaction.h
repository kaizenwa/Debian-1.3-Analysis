/* XBlast 2.1.8 level */
static BMLevelData ChainReaction =
{
  /* BMLevel */
  {
    "Chain Reaction",
    "Garth Denley",
    "xblast.useChainReaction",
    "Drop a bomb and STAND CLEAR!",
    GM_Random | GM_234_Player | GM_All,
    (void *) &ChainReaction,
    NULL,
  },
  /* BMShrinkData */
  {
    shrink_compound_extra,
    SCRAMBLE_VOID,
    SCRAMBLE_VOID,
  },
  /* BMFuncData */
  {
    special_init_void,
    special_game_void,
    special_extra_ignite_all,
    special_key_void,
  },
  /* BMPlayerData */
  {
    1, 7,
    {
      {  2,  2 },
      {  2, 12 },
      { 10, 12 },
      { 10,  2 },
      { -1, -1 },
      { -1, -1 },
    },
    PM_Polar, -1,
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
      { BLDarkWay, "Black", "SlateBlue", "SpringGreen" },
      { BLDarkWay_S, "Black", "SlateBlue", "SpringGreen" },
      { BLRIP, "Black", "SeaGreen", "SlateBlue" },
      { BLRIPRise, "Black", "SeaGreen", "SlateBlue" },
      { BLPumpkin, "Black", "OrangeRed", "SlateBlue" },
      { BLPumpkin_O, "Black", "OrangeRed", "SlateBlue" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_BUTTON,
      { BLScoreFloor, "RoyalBlue", "RoyalBlue", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowFull, DEnone,
    { 12, 13, 14, 17, 60 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,_,_,X,X,B,X,B,X,X,_,_,B, },
      { B,_,_,X,X,_,X,_,X,X,_,_,B, },
      { B,X,X,X,X,X,B,X,X,X,X,X,B, },
      { B,X,X,X,B,X,_,X,B,X,X,X,B, },
      { B,_,B,X,X,X,X,X,X,X,B,_,B, },
      { B,X,X,X,_,B,X,B,_,X,X,X,B, },
      { B,B,X,B,X,X,_,X,X,B,X,B,B, },
      { B,X,X,X,_,B,X,B,_,X,X,X,B, },
      { B,_,B,X,X,X,X,X,X,X,B,_,B, },
      { B,X,X,X,B,X,_,X,B,X,X,X,B, },
      { B,X,X,X,X,X,B,X,X,X,X,X,B, },
      { B,_,_,X,X,_,X,_,X,X,_,_,B, },
      { B,_,_,X,X,B,X,B,X,X,_,_,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
