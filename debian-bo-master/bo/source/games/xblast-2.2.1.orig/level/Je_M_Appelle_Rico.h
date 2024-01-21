/* XBlast 2.1.8 level */
static BMLevelData Je_M_Appelle_Rico =
{
  /* BMLevel */
  {
    "Je m'appelle Rico",
    "Laurent Marsan",
    "xblast.useJeMAppelleRico",
    "Don't PANIC!",
    GM_Random | GM_234_Player | GM_Single | GM_Team,
    (void *) &Je_M_Appelle_Rico,
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
    special_extra_void,
    special_key_void,
  },
  /* BMPlayerData */
  {
    1, 2,
    {
      {  5,  7 },
      {  7,  7 },
      {  6,  8 },
      {  6,  6 },
      { -1, -1 },
      { -1, -1 },
    },
    PM_Same, 0,
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
      { BLIronFloor, "Black", "LightCyan", "Blue" },
      { BLIronFloor_S, "Black", "LightCyan", "Blue" },
      { BLBricks, "Black", "Sienna", "Black" },
      { BLBricks_O, "Black", "Yellow", "Black" },
      { BLBricks, "Black", "ForestGreen", "Black" },
      { BLBricks_O, "Black", "SpringGreen", "Black" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_KICK,
      { BLScoreFloor, "Salmon", "Salmon", "Salmon" },
    },
  },
  /* BMMapData */
  {
    ShadowFull, DEnone,
    { 20, 40, 50, 50, 63 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,X,X,X,X,X,X,X,X,X,X,X,B, },
      { B,X,X,X,X,X,X,X,X,X,X,X,B, },
      { B,X,X,X,X,X,X,X,X,X,X,X,B, },
      { B,X,X,X,X,X,X,X,X,X,X,X,B, },
      { B,X,X,X,X,X,X,X,X,X,X,X,B, },
      { B,X,X,X,X,X,_,X,X,X,X,X,B, },
      { B,X,X,X,X,_,_,_,X,X,X,X,B, },
      { B,X,X,X,X,X,_,X,X,X,X,X,B, },
      { B,X,X,X,X,X,X,X,X,X,X,X,B, },
      { B,X,X,X,X,X,X,X,X,X,X,X,B, },
      { B,X,X,X,X,X,X,X,X,X,X,X,B, },
      { B,X,X,X,X,X,X,X,X,X,X,X,B, },
      { B,X,X,X,X,X,X,X,X,X,X,X,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
