/* XBlast 2.1.8 level */
static BMLevelData XBlast_2000 =
{
  /* BMLevel */
  {
    "XBlast 2000",
    "Oliver Vogel",
    "xblast.useXBlast2000",
    "Trap your foe  while you are invincible",
    GM_Random | GM_23456_Player | GM_All,
    (void *) &XBlast_2000,
    NULL,
  },
  /* BMShrinkData */
  {
    shrink_spiral_3,
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
    1, 2,
    {
      {  3,  3 },
      {  3, 11 },
      {  9, 11 },
      {  9,  3 },
      {  3,  7 },
      {  9,  7 },
    },
    PM_Polar, -2,
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
      { BLSphereHalf, "Black", "Coral", "SteelBlue" },
      { BLSphereHalf_S, "Black", "Coral", "SteelBlue" },
      { BLSphereDark, "Black", "SteelBlue", "Black" },
      { BLSphereLight, "Black", "LightSteelBlue", "Black" },
      { BLSphereLight, "Black", "SteelBlue", "Black" },
      { BLSphereLight_O, "Black", "SteelBlue", "Black" },
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
    { 16, 32, 48, 64, 64 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,X,_,_,X,_,_,_,X,_,_,X,B, },
      { B,_,B,_,B,X,B,X,B,_,B,_,B, },
      { B,_,_,_,X,_,_,_,X,_,_,_,B, },
      { B,X,B,X,B,X,B,X,B,X,B,X,B, },
      { B,_,X,_,X,_,_,_,X,_,X,_,B, },
      { B,_,B,_,B,_,B,_,B,_,B,_,B, },
      { B,_,X,_,_,X,_,X,_,_,X,_,B, },
      { B,_,B,_,B,_,B,_,B,_,B,_,B, },
      { B,_,X,_,X,_,_,_,X,_,X,_,B, },
      { B,X,B,X,B,X,B,X,B,X,B,X,B, },
      { B,_,_,_,X,_,_,_,X,_,_,_,B, },
      { B,_,B,_,B,X,B,X,B,_,B,_,B, },
      { B,X,_,_,X,_,_,_,X,_,_,X,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
