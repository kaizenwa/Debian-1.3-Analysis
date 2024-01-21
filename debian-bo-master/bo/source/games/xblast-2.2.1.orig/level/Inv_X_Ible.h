/* XBlast 2.1.8 level */
static BMLevelData Inv_X_Ible =
{
  /* BMLevel */
  {
    "INV-X-IBLE !!!!!",
    "Laurent Marsan",
    "xblast.useInvXIble",
    "Don't get confused by the invisble walls",
    GM_Random | GM_234_Player | GM_All,
    (void *) &Inv_X_Ible,
    NULL,
  },
  /* BMShrinkData */
  {
    shrink_compound,
    SCRAMBLE_VOID,
    SCRAMBLE_VOID,
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
      {  1,  1 },
      {  1, 13 },
      { 11, 13 },
      { 11,  1 },
      { -1, -1 },
      { -1, -1 },
    },
    PM_Polar, 2,
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
      { BLIronFloor, "Black", "PaleTurquoise", "Blue" },
      { BLIronFloor_S, "Black", "PaleTurquoise", "Blue" },
      { BLIronFloor, "Black", "PaleTurquoise", "Blue" },
      { BLDarkBlockRise, "Black", "Blue", "Blue" },
      { BLExtra, "Black", "DeepPink", "Yellow" },
      { BLExtra_O, "Black", "DeepPink", "Yellow" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_KICK,
      { BLScoreFloor, "Red", "Red", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowNone, DEall,
    { 20, 40, 40, 62, 63 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,_,_,_,X,B,B,B,X,_,_,_,B, },
      { B,_,B,B,X,X,B,X,X,B,B,_,B, },
      { B,_,X,_,B,_,B,_,B,_,X,_,B, },
      { B,_,B,_,B,_,X,_,B,_,B,_,B, },
      { B,_,B,_,X,_,B,_,X,_,B,_,B, },
      { B,X,B,X,B,X,X,X,B,X,B,X,B, },
      { B,X,B,B,B,X,B,X,B,B,B,X,B, },
      { B,X,B,X,B,X,X,X,B,X,B,X,B, },
      { B,_,B,_,X,_,B,_,X,_,B,_,B, },
      { B,_,B,_,B,_,X,_,B,_,B,_,B, },
      { B,_,X,_,B,_,B,_,B,_,X,_,B, },
      { B,_,B,B,X,X,B,X,X,B,B,_,B, },
      { B,_,_,_,X,B,B,B,X,_,_,_,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
