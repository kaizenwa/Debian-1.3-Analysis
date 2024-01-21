/* XBlast 2.1.8 level */
static BMPosition LaBoom_scrdraw[] = { {3,5}, {9,5}, {3,9}, {9,9} };
static BMLevelData LaBoom =
{
  /* BMLevel */
  {
    "La Boom",
    "Garth Denley",
    "xblast.useLaBoom",
    "Push the button here  and it blows up over there.",
    GM_Random | GM_23456_Player | GM_All,
    (void *) &LaBoom,
    NULL,
  },
  /* BMShrinkData */
  {
    shrink_spiral,
    { 3*GAME_TIME/4, 4, LaBoom_scrdraw, },
    SCRAMBLE_VOID,
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
      {  2,  2 },
      {  2, 12 },
      { 10, 12 },
      { 10,  2 },
      {  6,  4 },
      {  6, 10 },
    },
    PM_Vertical, 2,
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
      { BLChessFloor, "Black", "LightSlateBlue", "Black" },
      { BLChessFloor_S, "Black", "LightSlateBlue", "Black" },
      { BLBookShelf, "Black", "OrangeRed", "SeaGreen" },
      { BLBookShelf, "White", "OrangeRed", "SeaGreen" },
      { BLChessSphere, "Black", "LightSlateBlue", "Gold" },
      { BLChessSphere_O, "Black", "LightSlateBlue", "Gold" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_RC,
      { BLScoreFloor, "RoyalBlue", "RoyalBlue", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowBlock, DEall,
    { 20, 37, 45, 60, 60 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,X,_,X,X,_,B,_,X,X,_,X,B, },
      { B,_,_,B,_,_,B,_,_,B,_,_,B, },
      { B,_,_,X,_,X,B,X,_,X,_,_,B, },
      { B,X,X,B,_,_,_,_,_,B,X,X,B, },
      { B,X,X,X,B,B,_,B,B,X,X,X,B, },
      { B,X,X,B,_,_,_,_,_,B,X,X,B, },
      { B,B,_,_,_,B,_,B,_,_,_,B,B, },
      { B,X,X,B,_,_,_,_,_,B,X,X,B, },
      { B,X,X,X,B,B,_,B,B,X,X,X,B, },
      { B,X,X,B,_,_,_,_,_,B,X,X,B, },
      { B,_,_,X,_,X,B,X,_,X,_,_,B, },
      { B,_,_,B,_,_,B,_,_,B,_,_,B, },
      { B,X,_,X,X,_,B,_,X,X,_,X,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
