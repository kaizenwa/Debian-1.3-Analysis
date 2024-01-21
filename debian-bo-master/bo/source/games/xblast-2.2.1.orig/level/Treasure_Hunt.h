/* XBlast 2.1.8 level */
static BMLevelData Treasure_Hunt =
{
  /* BMLevel */
  {
    "Treasure Hunt",
    "Oliver Vogel",
    "xblast.useTreasureHunt",
    "Equip your self properly and then blast them",
    GM_Random | GM_23456_Player | GM_All,
    (void *) &Treasure_Hunt,
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
    special_extra_kick,
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
      {  3,  7 },
      {  9,  7 },
    },
    PM_Polar, 2,
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
      { BLKaroDark, "Black", "SeaGreen", "DarkSeaGreen" },
      { BLKaroDark_S, "Black", "Seagreen", "DarkSeaGreen" },
      { BLWall, "Black", "Firebrick1", "Black" },
      { BLWallRise, "Black", "Firebrick1", "SeaGreen" },
      { BLChest, "Black", "SaddleBrown", "Gold" },
      { BLChest_O, "Black", "SaddleBrown", "Gold" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_KICK,
      { BLScoreFloor, "RoyalBlue", "RoyalBlue", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowFull, DEsingle,
    { 20, 34, 44, 56, 56 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,_,_,_,X,_,_,_,X,_,_,_,B, },
      { B,_,B,X,B,B,X,B,B,X,B,_,B, },
      { B,_,B,X,X,_,_,_,X,X,B,_,B, },
      { B,_,X,X,B,X,B,X,B,X,X,_,B, },
      { B,X,B,_,X,_,_,_,X,_,B,X,B, },
      { B,_,B,_,B,X,B,X,B,_,B,_,B, },
      { B,X,_,_,B,X,X,X,B,_,_,X,B, },
      { B,_,B,_,B,X,B,X,B,_,B,_,B, },
      { B,X,B,_,X,_,_,_,X,_,B,X,B, },
      { B,_,X,X,B,X,B,X,B,X,X,_,B, },
      { B,_,B,X,X,_,_,_,X,X,B,_,B, },
      { B,_,B,X,B,B,X,B,B,X,B,_,B, },
      { B,_,_,_,X,_,_,_,X,_,_,_,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
