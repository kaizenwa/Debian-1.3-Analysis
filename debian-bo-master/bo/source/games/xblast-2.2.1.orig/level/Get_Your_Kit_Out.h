/* XBlast 2.1.8 level */
static BMPosition Get_Your_Kit_Out_Dele[] = {
 {5,3},{5,4},{5,6},{5,8},{7,3},{7,4},{7,5},{7,7},{7,9} };

static BMLevelData Get_Your_Kit_Out =
{
  /* BMLevel */
  {
    "Get Your Kit Out",
    "Keith Gillow and Mark Shepherd",
    "xblast.useGetYourKitOut",
    "Race to get a timer first",
    GM_Random | GM_234_Player | GM_Single | GM_Team,
    (void *) &Get_Your_Kit_Out,
    NULL,
  },
  /* BMShrinkData */
  {
    shrink_speed_spiral,
    SCRAMBLE_VOID,
    {GAME_TIME/4, 9, Get_Your_Kit_Out_Dele},
  },
  /* BMFuncData */
  {
    special_init_void,
    special_game_haunt, 
    special_extra_RC,
    special_key_RC,
  },
  /* BMPlayerData */
  {
    1, 2,
    {
      { 2, 1 },
      { 5, 1 },
      { 7, 1 },
      { 10, 1 },
    },
    PM_Same, 0,
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
      { BLCityFree,      "Black",     "goldenrod1",      "Pink" },
      { BLCityFree_S,    "Black",     "goldenrod1",      "Pink" },
      { BLDarkHouse,     "Black",     "DarkTurquoise",   "PaleVioletRed1" },
      { BLDarkHouseRise, "Black",     "DarkTurquoise",   "PaleVioletRed1" },
      { BLBox,           "Black",     "MediumPurple",    "PaleVioletRed1" },
      { BLBox,           "Black",     "MediumPurple",    "PaleVioletRed1" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_RC,
      { BLScoreFloor,    "RoyalBlue", "RoyalBlue",   "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowFull, DEall,
    {18,38,50,51,55},
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B },
      { B,_,_,B,_,_,B,_,_,B,_,_,B },
      { B,_,_,_,_,_,_,_,_,_,_,_,B },
      { B,_,X,B,B,B,s,B,B,B,X,_,B },
      { B,_,_,_,_,B,_,B,_,_,_,_,B },
      { B,B,X,X,_,B,s,B,_,X,X,B,B },
      { B,_,_,_,_,B,s,B,_,_,_,_,B },
      { B,_,X,X,X,B,_,B,X,X,X,_,B },
      { B,_,_,_,_,B,s,B,_,_,_,_,B },
      { B,s,X,X,_,B,_,B,_,X,X,s,B },
      { B,_,_,_,_,_,r,_,_,_,_,_,B },
      { B,X,B,B,B,_,X,_,B,B,B,X,B },
      { B,r,B,r,X,_,B,_,X,r,B,r,B },
      { B,q,B,q,B,_,b,_,B,q,B,q,B },
      { B,B,B,B,B,B,B,B,B,B,B,B,B }
    },
  },
};
