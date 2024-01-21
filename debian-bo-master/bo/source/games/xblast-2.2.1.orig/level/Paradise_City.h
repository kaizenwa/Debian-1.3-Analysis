/* XBlast 2.1.8 level */
static BMLevelData Paradise_City =
{
  /* BMLevel */
  {
    "Paradise City",
    "Oliver Vogel",
    "xblast.useParadiseCity",
    "Be the first to be outside",
    GM_Random | GM_23456_Player | GM_Single | GM_Team,
    (void *) &Paradise_City,
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
    1, 2,
    {
      {  5,  5 },
      {  5,  9 },
      {  7,  9 },
      {  7,  5 },
      {  5,  7 },
      {  7,  7 },
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
      { BLCityFree, "Black", "Gray65", "tan" },
      { BLCityFree_S, "Black", "Gray65", "tan" },
      { BLDarkHouse, "Black", "FireBrick", "Sienna" },
      { BLDarkHouseRise, "Black", "FireBrick", "Sienna" },
      { BLLightHouse, "Black", "LimeGreen", "SandyBrown" },
      { BLLightHouse_O, "Black", "LimeGreen", "SandyBrown" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_INVINC,
      { BLScoreFloor, "Black", "Black", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowFull, DEnone,
    { 12, 24, 40, 40, 40 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,_,r,_,b,_,r,_,b,_,r,_,B, },
      { B,r,B,X,B,X,B,X,B,X,B,r,B, },
      { B,_,X,X,_,X,_,X,_,X,X,_,B, },
      { B,b,B,_,B,X,B,X,B,_,B,b,B, },
      { B,_,X,X,X,_,_,_,X,X,X,_,B, },
      { B,r,B,_,B,_,B,_,B,_,B,r,B, },
      { B,_,X,X,X,_,_,_,X,X,X,_,B, },
      { B,r,B,_,B,_,B,_,B,_,B,r,B, },
      { B,_,X,X,X,_,_,_,X,X,X,_,B, },
      { B,b,B,_,B,X,B,X,B,_,B,b,B, },
      { B,_,X,X,_,X,_,X,_,X,X,_,B, },
      { B,r,B,X,B,X,B,X,B,X,B,r,B, },
      { B,_,r,_,b,_,r,_,b,_,r,_,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
