/* XBlast 2.1.8 level */
static BMLevelData Shrinking_Arena =
{
  /* BMLevel */
  {
    "Shrinking Arena",
    "Christophe Kalt and Pierre Ramet",
    "xblast.useShrinkingArena",
    "Never forget it  this level shrinks",
    GM_Random | GM_23456_Player | GM_All,
    (void *) &Shrinking_Arena,
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
    special_extra_void,
    special_key_RC,
  },
  /* BMPlayerData */
  {
    5, 2,
    {
      {  3,  3 },
      {  3, 11 },
      {  9, 11 },
      {  9,  3 },
      {  2,  7 },
      { 10,  7 },
    },
    PM_LeftRight, 1,
    Healthy, IF_Kick | IF_RC,
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
      { BLSphereHalf, "Black", "YellowGreen", "Red" },
      { BLSphereHalf_S, "Black", "YellowGreen", "Red" },
      { BLSphereDark, "Black", "OrangeRed", "Black" },
      { BLSphereLight, "Black", "Yellow", "Black" },
      { BLSphereLight, "Black", "OrangeRed", "Black" },
      { BLSphereLight_O, "Black", "OrangeRed", "Black" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_KICK,
      { BLSphereHalf, "Black", "RoyalBlue", "Red" },
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
