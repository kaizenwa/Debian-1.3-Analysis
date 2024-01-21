/* XBlast 2.1.8 level */
static BMLevelData Tic_Tac_Toe =
{
  /* BMLevel */
  {
    "Tic Tac Toe",
    "Laurent Marsan",
    "xblast.useTicTacToe",
    "Be lucky",
    GM_Random | GM_23456_Player | GM_All,
    (void *) &Tic_Tac_Toe,
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
    special_game_haunt,
    special_extra_void,
    special_key_void,
  },
  /* BMPlayerData */
  {
    1, 2,
    {
      {  1,  2 },
      {  1, 12 },
      { 11, 12 },
      { 11,  2 },
      {  6,  2 },
      {  6, 12 },
    },
    PM_Inner, 2,
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
      { BLIronFloor, "Black", "LightYellow", "Blue" },
      { BLIronFloor_S, "Black", "Lightyellow", "Blue" },
      { BLDarkBlock, "Black", "Salmon", "SpringGreen" },
      { BLDarkBlockRise, "Black", "Green", "Red" },
      { BLBricks, "Black", "SkyBlue", "Black" },
      { BLBricks_O, "Black", "LightSkyBlue", "Black" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_KICK,
      { BLScoreFloor, "Black", "Black", "Black" },
    },
  },
  /* BMMapData */
  {
    ShadowFull, DEnone,
    { 30, 60, 60, 60, 60 },
    {
      { v,v,v,v,v,v,v,v,v,v,v,v,v, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,_,_,_,B,_,_,_,B,_,_,_,B, },
      { B,_,B,_,X,_,B,_,X,_,B,_,B, },
      { B,_,_,_,B,_,_,_,B,_,_,_,B, },
      { B,B,X,B,B,B,X,B,B,B,X,B,B, },
      { B,_,_,_,B,_,_,_,B,_,_,_,B, },
      { B,_,B,_,X,_,B,_,X,_,B,_,B, },
      { B,_,_,_,B,_,_,_,B,_,_,_,B, },
      { B,B,X,B,B,B,X,B,B,B,X,B,B, },
      { B,_,_,_,B,_,_,_,B,_,_,_,B, },
      { B,_,B,_,X,_,B,_,X,_,B,_,B, },
      { B,_,_,_,B,_,_,_,B,_,_,_,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { v,v,v,v,v,v,v,v,v,v,v,v,v, },
    },
  },
};
