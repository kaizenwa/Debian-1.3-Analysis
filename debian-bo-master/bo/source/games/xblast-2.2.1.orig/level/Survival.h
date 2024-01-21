/* XBlast 2.1.8 level */
static BMLevelData Survival =
{
  /* BMLevel */
  {
    "Survival of the Fittest",
    "Garth Denley",
    "xblast.useSurvival",
    "Run AWAYYYY!!!!!!",
    GM_Random | GM_23456_Player | GM_All,
    (void *) &Survival,
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
    0, 0,
    {
      {  5,  5 },
      {  5,  9 },
      {  7,  9 },
      {  7,  5 },
      {  4,  7 },
      {  8,  7 },
    },
    PM_Polar, 3,
    Healthy, IF_Kick,
  },
  /* BMBombData */
  {
    bomb_click_contact, bomb_click_contact, bomb_click_none,
    GoStop, FUSEnormal,
    BMTnormal, BMTnormal, BMTnormal,
  },
  /* BMGraphicsData */
  {
    {
      { BLIronFloor, "Black", "DarkSlateBlue", "OrangeRed" },
      { BLIronFloor_S, "Black", "DarkSlateBlue", "OrangeRed" },
      { BLDarkBlock, "Red", "OrangeRed", "Orange" },
      { BLDarkBlockRise, "Red", "OrangeRed", "Orange" },
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
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
