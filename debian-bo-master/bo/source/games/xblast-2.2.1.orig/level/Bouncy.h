/* XBlast 2.1.8 level */
static BMLevelData BouncyBouncy =
{
  /* BMLevel */
  {
    "Bouncy Bouncy",
    "Rob & Simon & Tristan",
    "xblast.useBouncy",
    "And up again...",
    GM_Random | GM_23456_Player | GM_All,
    (void *) &BouncyBouncy,
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
    special_extra_special_bomb,
    special_key_special_bomb,
  },
  /* BMPlayerData */
  {
    5, 3,
    {
      {  1,  2 },
      {  1,  4 },
      {  1, 10 },
      {  1, 12 },
      {  1,  6 },
      {  1,  8 },
    },
    PM_Below, 2,
    Healthy, IF_Kick,
  },
  /* BMBombData */
  {
    bomb_click_snooker, bomb_click_rebound, bomb_click_thru,
    GoDown, FUSEnormal,
    BMTfungus, BMTnapalm, BMTfungus,
  },
  /* BMGraphicsData */
  {
    {
      { BLIronFloor, "Black", "IndianRed", "Yellow" },
      { BLIronFloor_S, "Black", "IndianRed", "Yellow" },
      { BLDarkBlock, "Black", "SlateBlue", "Cyan" },
      { BLDarkBlockRise, "Black", "SlateBlue", "LightSlateBlue" },
      { BLExtra, "Black", "SaddleBrown", "Black" },
      { BLExtra, "Black", "SaddleBrown", "Black" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_NAPALM,
      { BLScoreFloor, "RoyalBlue", "RoyalBlue", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowBlock, DEall,
    { 8, 20, 30, 40, 45 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,_,_,_,_,_,_,_,_,X,_,X,B, },
      { B,_,_,_,_,X,_,_,_,X,_,X,B, },
      { B,B,B,_,_,X,_,X,_,_,_,_,B, },
      { B,_,_,_,_,X,_,X,_,X,_,_,B, },
      { B,_,_,X,_,_,_,X,_,_,_,X,B, },
      { B,_,_,_,_,_,_,_,_,X,_,X,B, },
      { B,B,B,_,_,_,X,_,_,X,_,_,B, },
      { B,_,_,_,_,_,_,_,_,X,_,X,B, },
      { B,_,_,X,_,_,_,X,_,_,_,X,B, },
      { B,_,_,_,_,X,_,X,_,X,_,_,B, },
      { B,B,B,_,_,X,_,X,_,_,_,_,B, },
      { B,_,_,_,_,X,_,_,_,X,_,X,B, },
      { B,_,_,_,_,_,_,_,_,X,_,X,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
