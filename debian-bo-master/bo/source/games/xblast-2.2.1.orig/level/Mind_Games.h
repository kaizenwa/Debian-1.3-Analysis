/* XBlast 2.1.8 level */
static BMLevelData Mind_Games =
{
  /* BMLevel */
  {
    "Mind Games",
    "Garth Denley",
    "xblast.useMindGames",
    "Keep well away from the bombs",
    GM_Random | GM_23456_Player | GM_All,
    (void *) &Mind_Games,
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
    special_game_haunt_fast,
    special_extra_void,
    special_key_void,
  },
  /* BMPlayerData */
  {
    1, 2,
    {
      {  5,  6 },
      {  5,  8 },
      {  7,  8 },
      {  7,  6 },
      {  2,  7 },
      { 10,  7 },
    },
    PM_Inner, -4,
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
      { BLCityFree, "Black", "Orchid", "Pink" },
      { BLCityFree_S, "Black", "Orchid", "Pink" },
      { BLDarkHouse, "Black", "SpringGreen", "Azure" },
      { BLDarkHouseRise, "Black", "SpringGreen", "Azure" },
      { BLBox, "Black", "Cyan", "Orange" },
      { BLBox, "Black", "Cyan", "Orange" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_KICK,
      { BLScoreFloor, "RoyalBlue", "RoyalBlue", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowFull, DEnone,
    { 18, 38, 45, 45, 60 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,X,B,X,_,_,_,_,_,X,B,X,B, },
      { B,_,X,_,_,X,_,X,_,_,X,_,B, },
      { B,_,_,_,X,B,X,B,X,_,_,_,B, },
      { B,B,_,_,_,X,_,X,_,_,_,B,B, },
      { B,_,_,X,_,_,_,_,_,X,_,_,B, },
      { B,_,X,B,X,_,X,_,X,B,X,_,B, },
      { B,_,_,X,_,X,B,X,_,X,_,_,B, },
      { B,_,X,B,X,_,X,_,X,B,X,_,B, },
      { B,_,_,X,_,_,_,_,_,X,_,_,B, },
      { B,B,_,_,_,X,_,X,_,_,_,B,B, },
      { B,_,_,_,X,B,X,B,X,_,_,_,B, },
      { B,_,X,_,_,X,_,X,_,_,X,_,B, },
      { B,X,B,X,_,_,_,_,_,X,B,X,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
