/* XBlast 2.1.8 level */
static BMLevelData Cave_In =
{
  /* BMLevel */
  {
    "Cave-in",
    "Garth Denley",
    "xblast.useCaveIn",
    "Blast your way to the center",
    GM_Random | GM_23456_Player | GM_All,
    (void *) &Cave_In,
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
      {  2,  7 },
      { 10,  7 },
    },
    PM_Polar, 1,
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
      { BLKaroDark, "Black", "Sienna", "DarkKhaki" },
      { BLKaroDark_S, "Black", "Sienna", "DarkKhaki" },
      { BLWall, "Black", "Gray40", "Black" },
      { BLWallRise, "Black", "Gray40", "Black" },
      { BLChest, "Black", "ForestGreen", "LightSteelBlue" },
      { BLChest_O, "Black", "ForestGreen", "LightSteelBlue" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_KICK,
      { BLScoreFloor, "RoyalBlue", "Gray20", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowFull, DEnone,
    { 20, 32, 36, 36, 40 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,X,X,X,X,X,X,X,_,_,B, },
      { B,_,X,X,X,X,X,X,X,X,X,_,B, },
      { B,_,X,X,X,X,X,X,X,X,X,_,B, },
      { B,_,X,X,X,X,X,X,X,X,X,_,B, },
      { B,_,X,X,X,X,X,X,X,X,X,_,B, },
      { B,_,_,X,X,X,X,X,X,X,_,_,B, },
      { B,_,X,X,X,X,X,X,X,X,X,_,B, },
      { B,_,X,X,X,X,X,X,X,X,X,_,B, },
      { B,_,X,X,X,X,X,X,X,X,X,_,B, },
      { B,_,X,X,X,X,X,X,X,X,X,_,B, },
      { B,_,_,X,X,X,X,X,X,X,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
