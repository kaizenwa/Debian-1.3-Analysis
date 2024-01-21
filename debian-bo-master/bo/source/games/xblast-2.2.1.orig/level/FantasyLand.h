/* XBlast 2.1.8 level */
static BMLevelData FantasyLand =
{
  /* BMLevel */
  {
    "Fantasy Land",
    "Garth Denley",
    "xblast.useFantasyLand",
    "Watch the bouncing bombs",
    GM_Random | GM_23456_Player | GM_All,
    (void *) &FantasyLand,
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
    7, 9,
    {
      {  5,  5 },
      {  5,  9 },
      {  7,  9 },
      {  7,  5 },
      {  4,  7 },
      {  8,  7 },
    },
    PM_Inner, 3,
    Healthy, IF_Kick,
  },
  /* BMBombData */
  {
    bomb_click_snooker, bomb_click_rebound, bomb_click_none,
    GoStop, FUSEnormal,
    BMTnormal, BMTnormal, BMTnormal,
  },
  /* BMGraphicsData */
  {
    {
      { BLIronFloor, "Black", "SkyBlue", "Yellow" },
      { BLIronFloor_S, "Black", "SkyBlue", "Yellow" },
      { BLDarkBlock, "Black", "Cyan", "SpringGreen" },
      { BLDarkBlockRise, "Black", "DeepPink", "HotPink" },
      { BLExtra, "Black", "SaddleBrown", "Black" },
      { BLExtra, "Black", "SaddleBrown", "Black" },
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
      { B,_,R,R,_,_,_,_,_,R,R,_,B, },
      { B,_,R,R,_,B,B,B,_,R,R,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,B,_,_,_,_,_,B,_,_,B, },
      { B,_,_,B,_,_,_,_,_,B,_,_,B, },
      { B,_,_,B,_,_,_,_,_,B,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,R,R,_,B,B,B,_,R,R,_,B, },
      { B,_,R,R,_,_,_,_,_,R,R,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
