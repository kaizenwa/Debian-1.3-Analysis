/* XBlast 2.1.8 level */
static BMLevelData BricksFries =
{
  /* BMLevel */
  {
    "Bricks with extra fries",
    "Garth Denley",
    "xblast.useBricksFries",
    "Use construction bombs to control the other bombs",
    GM_Random | GM_234_Player | GM_All,
    (void *) &BricksFries,
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
    special_init_special_bombs_30,
    special_game_void,
    special_extra_void,
    special_key_special_bomb,
  },
  /* BMPlayerData */
  {
    7, 9,
    {
      {  5,  2 },
      {  1,  8 },
      {  7, 12 },
      { 11,  6 },
      { -1, -1 },
      { -1, -1 },
    },
    PM_Polar, 2,
    Healthy, IF_Kick,
  },
  /* BMBombData */
  {
    bomb_click_randomdir, bomb_click_randomdir, bomb_click_thru,
    GoStop, FUSEnormal,
    BMTnormal, BMTconstruction, BMTnormal,
  },
  /* BMGraphicsData */
  {
    {
      { BLIronFloor, "Black", "BurlyWood", "SpringGreen" },
      { BLIronFloor_S, "Black", "BurlyWood", "SpringGreen" },
      { BLDarkBlock, "Black", "SpringGreen", "SteelBlue" },
      { BLDarkBlockRise, "Black", "SpringGreen", "SteelBlue" },
      { BLBricks, "Black", "Firebrick1", "Black" },
      { BLBricks_O, "Black", "FireBrick1", "Black" },
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
      { B,B,B,_,_,_,_,_,_,_,B,B,B, },
      { B,_,_,_,B,B,B,B,B,_,_,_,B, },
      { _,B,_,B,B,_,_,_,B,B,_,B,_, },
      { _,_,_,_,B,_,_,_,B,_,_,_,_, },
      { _,B,_,_,B,_,_,_,B,_,_,B,_, },
      { B,B,B,B,B,_,_,_,B,B,B,B,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,_,_,R,_,_,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,B,B,B,B,_,_,_,B,B,B,B,B, },
      { _,B,_,_,B,_,_,_,B,_,_,B,_, },
      { _,_,_,_,B,_,_,_,B,_,_,_,_, },
      { _,B,_,B,B,_,_,_,B,B,_,B,_, },
      { B,_,_,_,B,B,B,B,B,_,_,_,B, },
      { B,B,B,_,_,_,_,_,_,_,B,B,B, },
    },
  },
};
