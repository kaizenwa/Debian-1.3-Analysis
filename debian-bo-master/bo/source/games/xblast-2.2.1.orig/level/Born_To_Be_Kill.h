/* XBlast 2.1.8 level */
static BMLevelData Born_To_Be_Kill =
{
  /* BMLevel */
  {
    "Born To Be Killed",
    "Laurent Marsan",
    "xblast.useBornToBeKill",
    "Be very careful at the start",
    GM_Random | GM_23456_Player | GM_All,
    (void *) &Born_To_Be_Kill,
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
    special_extra_RC,
    special_key_RC,
  },
  /* BMPlayerData */
  {
    1, 15,
    {
      {  6,  7 },
      {  6,  7 },
      {  6,  7 },
      {  6,  7 },
      {  6,  7 },
      {  6,  7 },
    },
    PM_Polar, -1,
    Healthy, IF_Kick,
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
      { BLKaroLight, "Black", "Coral", "Salmon" },
      { BLKaroLight_S, "Black", "Coral", "Salmon" },
      { BLPyramid, "Black", "SlateBlue", "Black" },
      { BLPyramidRise, "Black", "LightSlateBlue", "Black" },
      { BLExtra, "Black", "SteelBlue", "OrangeRed" },
      { BLExtra_O, "Black", "LightSteelBlue", "ORangeRed" },
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
    { 16, 48, 48, 60, 56 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,q,X,s,_,_,_,_,_,s,X,q,B, },
      { B,X,B,B,X,_,B,_,X,B,B,X,B, },
      { B,s,B,b,X,_,B,_,X,b,B,s,B, },
      { B,_,b,B,X,b,_,b,X,B,b,_,B, },
      { B,_,B,b,X,B,X,B,X,b,B,_,B, },
      { B,_,_,_,B,s,s,s,B,_,_,_,B, },
      { B,_,B,_,X,s,_,s,X,_,B,_,B, },
      { B,_,_,_,B,s,s,s,B,_,_,_,B, },
      { B,_,B,b,X,B,X,B,X,b,B,_,B, },
      { B,_,b,B,X,b,_,b,X,B,b,_,B, },
      { B,s,B,b,X,_,B,_,X,b,B,s,B, },
      { B,X,B,B,X,_,B,_,X,B,B,X,B, },
      { B,q,X,s,_,_,_,_,_,s,X,q,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
