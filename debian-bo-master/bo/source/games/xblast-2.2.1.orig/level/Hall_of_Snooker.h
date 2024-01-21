/* XBlast 2.1.8 level */
static BMLevelData Hall_of_Snooker =
{
  /* BMLevel */
  {
    "Hall of Snooker",
    "Garth Denley",
    "xblast.useHallofSnooker",
    "You can push chains of snooker bombs",
    GM_Random | GM_23456_Player | GM_All,
    (void *) &Hall_of_Snooker,
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
    10, 15,
    {
      {  3,  3 },
      {  3, 11 },
      {  9, 11 },
      {  9,  3 },
      {  3,  7 },
      {  9,  7 },
    },
    PM_Vertical, -2,
    Healthy, IF_Kick,
  },
  /* BMBombData */
  {
    bomb_click_snooker, bomb_click_none, bomb_click_none,
    GoStop, FUSEnormal,
    BMTnormal, BMTnormal, BMTnormal,
  },
  /* BMGraphicsData */
  {
    {
      { BLIronFloor, "Black", "ForestGreen", "Gold" },
      { BLIronFloor_S, "Black", "ForestGreen", "Gold" },
      { BLDarkBlock, "Black", "ForestGreen", "Red" },
      { BLDarkBlockRise, "Black", "ForestGreen", "Red" },
      { BLExtra, "Black", "AntiqueWhite", "LightSlateBlue" },
      { BLExtra, "Black", "AntiqueWhite", "LightSlateBlue" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_INVINC,
      { BLScoreFloor, "RoyalBlue", "RoyalBlue", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowBlock, DEnone,
    { 0, 0, 0, 0, 0 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,_,_,_,B,_,_,_,B,_,_,_,B, },
      { B,_,_,_,B,_,_,_,B,_,_,_,B, },
      { B,_,_,_,B,_,_,_,B,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,B,_,B,_,B,B,B,_,B,_,B,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,B,_,_,_,B,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,B,_,B,_,B,B,B,_,B,_,B,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,B,_,_,_,B,_,_,_,B, },
      { B,_,_,_,B,_,_,_,B,_,_,_,B, },
      { B,_,_,_,B,_,_,_,B,_,_,_,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
