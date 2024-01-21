/* XBlast 2.1.8 level */
static BMLevelData FryingPan =
{
  /* BMLevel */
  {
    "Out of the Frying Pan",
    "Garth Denley",
    "xblast.useFryingPan",
    "Don't forget about the wall",
    GM_Random | GM_23456_Player | GM_All,
    (void *) &FryingPan,
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
    special_init_nasty_walls_2,
    special_game_nasty_walls,
    special_extra_special_bomb,
    special_key_special_bomb,
  },
  /* BMPlayerData */
  {
    1, 2,
    {
      {  5,  5 },
      {  5,  9 },
      {  7,  9 },
      {  7,  5 },
      {  6,  6 },
      {  6,  8 },
    },
    PM_Inner, -2,
    Healthy, IF_Kick,
  },
  /* BMBombData */
  {
    bomb_click_snooker, bomb_click_contact, bomb_click_none,
    GoStop, FUSEnormal,
    BMTnormal, BMTnapalm, BMTnormal,
  },
  /* BMGraphicsData */
  {
    {
      { BLSphereHalf, "Black", "LightSlateBlue", "OrangeRed" },
      { BLSphereHalf_S, "Black", "LightSlateBlue", "OrangeRed" },
      { BLSphereDark, "Black", "OrangeRed", "Black" },
      { BLSphereLight, "Black", "OrangeRed", "Black" },
      { BLSphereLight, "Black", "Red", "Black" },
      { BLSphereLight_O, "Black", "Red", "Black" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_NAPALM,
      { BLScoreFloor, "RoyalBlue", "RoyalBlue", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowBlock, DEspecial,
    { 14, 30, 30, 45, 45 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,X,X,X,X,X,_,_,_,B, },
      { B,_,_,X,X,X,X,X,X,X,_,_,B, },
      { B,_,_,X,X,_,_,_,X,X,_,_,B, },
      { B,_,_,X,X,_,_,_,X,X,_,_,B, },
      { B,_,_,X,X,_,_,_,X,X,_,_,B, },
      { B,_,_,X,X,_,_,_,X,X,_,_,B, },
      { B,_,_,X,X,_,_,_,X,X,_,_,B, },
      { B,_,_,X,X,X,X,X,X,X,_,_,B, },
      { B,_,_,_,X,X,X,X,X,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
