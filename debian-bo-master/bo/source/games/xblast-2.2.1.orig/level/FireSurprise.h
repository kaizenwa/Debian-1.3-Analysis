/* XBlast 2.1.8 level */
static BMLevelData FireSurprise =
{
  /* BMLevel */
  {
    "Firecracker Surprise",
    "Garth Denley",
    "xblast.useFireSurprise",
    "Stand well away from firecrackers",
    GM_Random | GM_234_Player | GM_All,
    (void *) &FireSurprise,
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
    special_extra_special_bomb,
    special_key_special_bomb,
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
    PM_Horizontal, 2,
    Healthy, IF_None,
  },
  /* BMBombData */
  {
    bomb_click_none, bomb_click_none, bomb_click_none,
    GoStop, FUSEnormal,
    BMTnormal, BMTfirecracker, BMTnormal,
  },
  /* BMGraphicsData */
  {
    {
      { BLSphereHalf, "Black", "SpringGreen", "Orchid" },
      { BLSphereHalf_S, "Black", "SpringGreen", "Orchid" },
      { BLSphereDark, "Black", "Orchid", "Black" },
      { BLSphereLight, "Black", "Orchid", "Black" },
      { BLSphereLight, "Black", "Magenta", "Black" },
      { BLSphereLight_O, "Black", "Magenta", "Black" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_FIRECRACKER,
      { BLScoreFloor, "RoyalBlue", "RoyalBlue", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowFull, DEspecial,
    { 20, 35, 40, 58, 60 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,_,_,_,X,_,_,_,X,_,_,_,B, },
      { B,B,B,_,B,B,X,B,B,_,B,B,B, },
      { B,_,_,X,_,B,_,B,_,X,_,_,B, },
      { B,_,B,X,X,B,X,B,X,X,B,_,B, },
      { B,X,B,_,B,B,_,B,B,_,B,X,B, },
      { B,X,B,X,B,_,_,_,B,X,B,X,B, },
      { B,_,X,_,X,_,X,_,X,_,X,_,B, },
      { B,X,B,X,B,_,_,_,B,X,B,X,B, },
      { B,X,B,_,B,B,_,B,B,_,B,X,B, },
      { B,_,B,X,X,B,X,B,X,X,B,_,B, },
      { B,_,_,X,_,B,_,B,_,X,_,_,B, },
      { B,B,B,_,B,B,X,B,B,_,B,B,B, },
      { B,_,_,_,X,_,_,_,X,_,_,_,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
