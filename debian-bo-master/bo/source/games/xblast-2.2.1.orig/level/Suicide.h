/* XBlast 2.1.8 level */
static BMLevelData Suicide =
{
  /* BMLevel */
  {
    "Suicide",
    "Mike Schneider",
    "xblast.useSuicide",
    "Drop bombs while you're still invincible",
    GM_Random | GM_234_Player | GM_Single | GM_Team,
    (void *) &Suicide,
    NULL,
  },
  /* BMShrinkData */
  {
    shrink_compound,
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
      {  1,  1 },
      {  1, 13 },
      { 11, 13 },
      { 11,  1 },
      { -1, -1 },
      { -1, -1 },
    },
    PM_Same, 0,
    Healthy, IF_None,
  },
  /* BMBombData */
  {
    bomb_click_none, bomb_click_none, bomb_click_none,
    GoStop, FUSEshort,
    BMTnormal, BMTnormal, BMTnormal,
  },
  /* BMGraphicsData */
  {
    {
      { BLIronFloor, "Red", "Black", "Yellow" },
      { BLIronFloor_S, "Red", "Black", "Yellow" },
      { BLDarkBlock, "Black", "SlateBlue", "LightSlateBlue" },
      { BLDarkBlockRise, "Black", "SlateBlue", "LightSlateBlue" },
      { BLExtra, "Black", "ForestGreen", "SpringGreen" },
      { BLExtra_O, "Black", "ForestGreen", "SpringGreen" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_KICK,
      { BLScoreFloor, "BlueViolet", "BlueViolet", "BlueViolet" },
    },
  },
  /* BMMapData */
  {
    ShadowNone, DEnone,
    { 24, 48, 56, 56, 56 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,_,X,_,_,_,X,_,_,_,X,_,B, },
      { B,X,B,X,B,B,B,B,B,X,B,X,B, },
      { B,_,X,_,_,_,X,_,_,_,X,_,B, },
      { B,_,B,_,B,B,X,B,B,_,B,_,B, },
      { B,_,B,_,B,X,_,X,B,_,B,_,B, },
      { B,_,B,_,B,_,_,_,B,_,B,_,B, },
      { B,X,B,X,X,_,B,_,X,X,B,X,B, },
      { B,_,B,_,B,_,_,_,B,_,B,_,B, },
      { B,_,B,_,B,X,_,X,B,_,B,_,B, },
      { B,_,B,_,B,B,X,B,B,_,B,_,B, },
      { B,_,X,_,_,_,X,_,_,_,X,_,B, },
      { B,X,B,X,B,B,B,B,B,X,B,X,B, },
      { B,_,X,_,_,_,X,_,_,_,X,_,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
