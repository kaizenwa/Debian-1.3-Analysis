/* XBlast 2.1.8 level */
static BMLevelData Full_Power =
{
  /* BMLevel */
  {
    "Full Power Level",
    "Oliver Vogel",
    "xblast.useFullPower",
    "A simple one: stay clear of the explosions",
    GM_Random | GM_23456_Player | GM_All,
    (void *) &Full_Power,
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
    special_extra_void,
    special_key_void,
  },
  /* BMPlayerData */
  {
    5, 10,
    {
      {  1,  1 },
      {  1, 13 },
      { 11, 13 },
      { 11,  1 },
      {  3,  7 },
      {  9,  7 },
    },
    PM_Inner, 2,
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
      { BLIronFloor, "Black", "LightSteelBlue", "SpringGreen" },
      { BLIronFloor_S, "Black", "LightSteelBlue", "SpringGreen" },
      { BLDarkBlock, "Black", "LightCoral", "IndianRed" },
      { BLDarkBlockRise, "Black", "LightCoral", "IndianRed" },
      { BLExtra, "Black", "Sienna", "Black" },
      { BLExtra, "Black", "Sienna", "Black" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_KICK,
      { BLScoreFloor, "RoyalBlue", "RoyalBlue", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowBlock, DEnone,
    { 0, 0, 0, 0, 0 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,B,_,B,_,B,_,B,_,B,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,B,_,B,_,B,_,B,_,B,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,B,_,B,_,B,_,B,_,B,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,B,_,B,_,B,_,B,_,B,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,B,_,B,_,B,_,B,_,B,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,B,_,B,_,B,_,B,_,B,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
