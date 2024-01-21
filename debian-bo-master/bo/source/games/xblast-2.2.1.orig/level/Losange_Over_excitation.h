/* XBlast 2.1.8 level */
static BMPosition Losange_Scramble[] = {
  {4,6}, {5,5}, {8,6}, {7,5},
  {4,8}, {5,9}, {8,8}, {7,9},
};
static BMLevelData Losange_Over_excitation =
{
  /* BMLevel */
  {
    "Losange Over-excitation",
    "Joachim Kaltz",
    "xblast.useLosangeOverexcitation",
    "Getting kick bombs will help",
    GM_Random | GM_234_Player | GM_Single | GM_Team,
    (void *) &Losange_Over_excitation,
    NULL,
  },
  /* BMShrinkData */
  {
    shrink_spiral_plus,
    SCRAMBLE_VOID,
    { 3*GAME_TIME/4, 8, Losange_Scramble, },
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
      {  1,  7 },
      {  6, 12 },
      { 11,  7 },
      {  6,  2 },
      { -1, -1 },
      { -1, -1 },
    },
    PM_Same, 0,
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
      { BLHex, "Black", "Tomato", "Red" },
      { BLHex, "Black", "Tomato", "Red" },
      { BLHexWall, "Black", "Tomato", "OrangeRed" },
      { BLHexExtra, "Black", "Tomato", "Orange" },
      { BLHexExtra, "Black", "Tomato", "Yellow" },
      { BLHexExtra_O, "Black", "Tomato", "Yellow" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_KICK,
      { BLHex, "Black", "Tomato", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowNone, DEall,
    { 16, 32, 32, 40, 60 },
    {
      { v,v,v,v,v,v,v,v,v,v,v,v,v, },
      { v,v,v,v,B,B,B,B,B,v,v,v,v, },
      { v,v,v,B,B,_,_,_,B,B,v,v,v, },
      { v,v,B,B,X,X,_,X,X,B,B,v,v, },
      { v,B,B,X,X,X,X,X,X,X,B,B,v, },
      { B,B,X,X,X,B,X,B,X,X,X,B,B, },
      { B,_,X,X,B,B,X,B,B,X,X,_,B, },
      { B,_,_,X,X,X,X,X,X,X,_,_,B, },
      { B,_,X,X,B,B,X,B,B,X,X,_,B, },
      { B,B,X,X,X,B,X,B,X,X,X,B,B, },
      { v,B,B,X,X,X,X,X,X,X,B,B,v, },
      { v,v,B,B,X,X,_,X,X,B,B,v,v, },
      { v,v,v,B,B,_,_,_,B,B,v,v,v, },
      { v,v,v,v,B,B,B,B,B,v,v,v,v, },
      { v,v,v,v,v,v,v,v,v,v,v,v,v, },
    },
  },
};
