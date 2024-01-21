/* XBlast 2.1.8 level */
static BMLevelData Spindizzy =
{
  /* BMLevel */
  {
    "Spindizzy",
    "Garth Denley and Chris Doherty",
    "xblast.useSpindizzy",
    "Beware of contact bombs",
    GM_Random | GM_23456_Player | GM_All,
    (void *) &Spindizzy,
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
    special_init_nasty_walls,
    special_game_nasty_walls,
    special_extra_void,
    special_key_void,
  },
  /* BMPlayerData */
  {
    3, 4,
    {
      {  5,  6 },
      {  5,  8 },
      {  7,  8 },
      {  7,  6 },
      {  6,  5 },
      {  6,  9 },
    },
    PM_Inner, -2,
    Healthy, IF_Kick,
  },
  /* BMBombData */
  {
    bomb_click_contact, bomb_click_clockwise, bomb_click_rebound,
    GoStop, FUSEnormal,
    BMTnormal, BMTnormal, BMTnormal,
  },
  /* BMGraphicsData */
  {
    {
      { BLIronFloor, "Black", "Cyan", "OrangeRed" },
      { BLIronFloor_S, "Black", "Cyan", "OrangeRed" },
      { BLDarkBlock, "Black", "Magenta", "LightSkyBlue" },
      { BLDarkBlockRise, "Black", "Yellow", "Red" },
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
      { B,_,_,_,_,R,_,R,_,_,_,_,B, },
      { B,_,_,R,_,_,_,_,_,R,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,R,_,_,R,_,R,_,_,R,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,R,_,_,_,R,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,R,_,_,R,_,R,_,_,R,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,R,_,_,_,_,_,R,_,_,B, },
      { B,_,_,_,_,R,_,R,_,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
