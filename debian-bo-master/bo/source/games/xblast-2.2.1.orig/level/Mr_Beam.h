/* XBlast 2.1.8 level */
static BMLevelData Mr_Beam =
{
  /* BMLevel */
  {
    "Mr. Beam",
    "Patrick Durish",
    "xblast.useMrBeam",
    "Lay a trap and beam away",
    GM_Random | GM_234_Player | GM_All,
    (void *) &Mr_Beam,
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
    special_extra_teleport,
    special_key_teleport,
  },
  /* BMPlayerData */
  {
    5, 10,
    {
      {  1,  1 },
      {  1, 13 },
      { 11, 13 },
      { 11,  1 },
      { -1, -1 },
      { -1, -1 },
    },
    PM_Polar, 1,
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
      { BLMrBeamFree, "Black", "Azure", "Black" },
      { BLMrBeamFree, "Black", "Azure", "Black" },
      { BLMrBeamTv, "Black", "Azure", "Salmon" },
      { BLMrBeamFree, "Black", "Azure", "Black" },
      { BLMrBeamBear, "Black", "Azure", "Tan" },
      { BLMrBeamBearExp, "Black", "Azure", "Tan" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_BEAM,
      EXTRA_BEAM,
    },
  },
  /* BMMapData */
  {
    ShadowNone, DEnone,
    { 15, 31, 47, 63, 63 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,_,_,X,X,X,B,X,X,X,_,_,B, },
      { B,_,X,X,X,X,B,X,X,X,X,_,B, },
      { B,X,X,X,X,X,X,X,X,X,X,X,B, },
      { B,X,X,X,X,X,B,X,X,X,X,X,B, },
      { B,X,X,X,X,X,B,X,X,X,X,X,B, },
      { B,X,X,X,X,X,B,X,X,X,X,X,B, },
      { B,B,B,X,B,B,B,B,B,X,B,B,B, },
      { B,X,X,X,X,X,B,X,X,X,X,X,B, },
      { B,X,X,X,X,X,B,X,X,X,X,X,B, },
      { B,X,X,X,X,X,B,X,X,X,X,X,B, },
      { B,X,X,X,X,X,X,X,X,X,X,X,B, },
      { B,_,X,X,X,X,B,X,X,X,X,_,B, },
      { B,_,_,X,X,X,B,X,X,X,_,_,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
