/* XBlast 2.1.8 level */
static BMLevelData TwoBeTwo =
{
  /* BMLevel */
  {
    "Two Be Two",
    "Garth Denley",
    "xblast.useTwoBeTwo",
    "Get away from the edges  but watch the middle",
    GM_Random | GM_234_Player | GM_All,
    (void *) &TwoBeTwo,
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
    special_extra_special_bomb,
    special_key_special_bomb,
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
    PM_Polar, 3,
    Healthy, IF_Kick,
  },
  /* BMBombData */
  {
    bomb_click_none, bomb_click_none, bomb_click_none,
    GoStop, FUSEnormal,
    BMTnormal, BMTnapalm, BMTnormal,
  },
  /* BMGraphicsData */
  {
    {
      { BLKaroDark, "Black", "PeachPuff", "Coral" },
      { BLKaroDark_S, "Black", "PeachPuff", "Coral" },
      { BLWall, "Black", "SeaGreen", "Black" },
      { BLWallRise, "Black", "SeaGreen", "Black" },
      { BLChest, "Black", "RosyBrown", "LightSteelBlue" },
      { BLChest_O, "Black", "RosyBrown", "LightSteelBlue" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_NAPALM,
      { BLScoreFloor, "RoyalBlue", "RoyalBlue", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowFull, DEspecial,
    { 29, 42, 45, 61, 61 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,_,_,X,_,X,B,X,_,X,_,_,B, },
      { B,_,_,B,_,_,X,_,_,B,_,_,B, },
      { B,X,B,B,B,B,B,B,B,B,B,X,B, },
      { B,_,_,B,X,_,B,_,X,B,_,_,B, },
      { B,X,_,B,_,_,B,_,_,B,_,X,B, },
      { B,B,X,B,B,X,_,X,B,B,X,B,B, },
      { B,X,_,X,_,X,_,X,_,X,_,X,B, },
      { B,B,X,B,B,X,_,X,B,B,X,B,B, },
      { B,X,_,B,_,_,B,_,_,B,_,X,B, },
      { B,_,_,B,X,_,B,_,X,B,_,_,B, },
      { B,X,B,B,B,B,B,B,B,B,B,X,B, },
      { B,_,_,B,_,_,X,_,_,B,_,_,B, },
      { B,_,_,X,_,X,B,X,_,X,_,_,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
