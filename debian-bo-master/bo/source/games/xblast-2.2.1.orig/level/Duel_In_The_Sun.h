/* XBlast 2.1.8 level */
static BMLevelData Duel_In_The_Sun =
{
  /* BMLevel */
  {
    "Duel in the Sun",
    "Oliver Vogel",
    "xblast.useDuelInTheSun",
    "Aim your bomb carefully",
    GM_Random | GM_234_Player | GM_All,
    (void *) &Duel_In_The_Sun,
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
    special_key_RC,
  },
  /* BMPlayerData */
  {
    1, 3,
    {
      {  2,  2 },
      {  2, 12 },
      { 10, 12 },
      { 10,  2 },
      { -1, -1 },
      { -1, -1 },
    },
    PM_Inner, 2,
    Healthy, IF_Kick | IF_RC,
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
      { BLIronFloor, "Black", "PaleGoldenRod", "Blue" },
      { BLIronFloor_S, "Black", "PaleGoldenRod", "Blue" },
      { BLDarkBlock, "Black", "ForestGreen", "Gray80" },
      { BLDarkBlockRise, "Black", "SpringGreen", "White" },
      { BLExtra, "Black", "Firebrick", "Black" },
      { BLExtra, "Black", "Firebrick", "Black" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_KICK,
      { BLScoreFloor, "SteelBlue", "SteelBlue", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowBlock, DEnone,
    { 0, 0, 0, 0, 0 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,_,_,_,B,_,_,_,B,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,B,_,_,_,_,_,_,_,_,_,B,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,_,_,B,_,_,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,B,_,_,_,_,_,_,_,_,_,B,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,B,_,_,_,B,_,_,_,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
