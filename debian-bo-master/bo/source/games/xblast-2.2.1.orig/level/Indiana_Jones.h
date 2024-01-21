/* XBlast 2.1.8 level */
static BMPosition Indiana_Jones_Draw[] = { 
  {3,3},{9,3},{3,11},{9,11},{3,4},{9,4},{3,10},{9,10} };
static BMPosition Indiana_Jones_Del[] = { 
  {6,7}, };
static BMLevelData Indiana_Jones =
{
  /* BMLevel */
  {
    "Indiana Jones",
    "Rob Hite",
    "xblast.useIndianaJones",
    "Make sure you get those easy flames",
    GM_Random | GM_234_Player | GM_Single | GM_Team,
    (void *) &Indiana_Jones,
    NULL,
  },
  /* BMShrinkData */
  {
    shrink_spiral,
    { 3*GAME_TIME/4, 8, Indiana_Jones_Draw, },
    { 3*GAME_TIME/4, 1, Indiana_Jones_Del, },
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
    3, 2,
    {
      {  2,  1 },
      {  2, 13 },
      { 10, 13 },
      { 10,  1 },
      { -1, -1 },
      { -1, -1 },
    },
    PM_Same, 0,
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
      { BLKaroLight, "Black", "Orchid", "DarkOrchid" },
      { BLKaroLight_S, "Black", "Orchid", "DarkOrchid" },
      { BLPyramid, "Black", "SlateBlue", "Black" },
      { BLPyramidRise, "Black", "LightSlateBlue", "Black" },
      { BLExtra, "Black", "Gold", "Yellow" },
      { BLExtra_O, "Black", "Gold", "Yellow" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_KICK,
      { BLScoreFloor, "RoyalBlue", "RoyalBlue", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowFull, DEnone,
    { 0, 0, 5, 5, 63 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,X,_,_,_,X,r,X,_,_,_,X,B, },
      { B,r,B,X,_,_,B,_,_,X,B,r,B, },
      { B,r,X,_,B,_,B,_,B,_,X,r,B, },
      { B,r,B,b,B,_,r,_,B,b,B,r,B, },
      { B,r,B,B,_,_,X,_,_,B,B,r,B, },
      { B,_,_,_,_,X,B,X,_,_,_,_,B, },
      { B,X,B,B,B,_,B,_,B,X,B,B,B, },
      { B,_,_,_,_,X,B,X,_,_,_,_,B, },
      { B,r,B,B,_,_,X,_,_,B,B,r,B, },
      { B,r,B,b,B,_,r,_,B,b,B,r,B, },
      { B,r,X,_,B,_,B,_,B,_,X,r,B, },
      { B,r,B,X,_,_,B,_,_,X,B,r,B, },
      { B,X,_,_,_,X,r,X,_,_,_,X,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
