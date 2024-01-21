/* XBlast 2.1.8 level */
static BMPosition SkyShow_Dele[] = {
 {5,7}, {6,7}, {7,7} };

static BMLevelData SkyShow =
{
  /* BMLevel */
  {
    "Sky Show",
    "Garth Denley",
    "xblast.useSkyShow",
    "Block pyro bombs with normal ones",
    GM_Random | GM_234_Player | GM_All,
    (void *) &SkyShow,
    NULL,
  },
  /* BMShrinkData */
  {
    shrink_lazy_compound_f,
    SCRAMBLE_VOID,
    { GAME_TIME/2, 3, SkyShow_Dele, },
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
    PM_Vertical, 2,
    Healthy, IF_Kick,
  },
  /* BMBombData */
  {
    bomb_click_none, bomb_click_rebound, bomb_click_none,
    GoStop, FUSEnormal,
    BMTnormal, BMTpyro, BMTnormal,
  },
  /* BMGraphicsData */
  {
    {
      { BLGhost, "Black", "PeachPuff", "MidnightBlue" },
      { BLGhost, "Black", "PeachPuff", "MidnightBlue" },
      { BLGhostSq, "Black", "PeachPuff", "Yellow" },
      { BLGhostSqRise, "Black", "PeachPuff", "Yellow" },
      { BLGhostCi, "Black", "PeachPuff", "DeepPink" },
      { BLGhostCiRise, "Black", "PeachPuff", "DeepPink" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_FIRECRACKER,
      { BLGhost, "Black", "PeachPuff", "MidnightBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowNone, DEspecial,
    { 20, 30, 43, 60, 60 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,_,_,_,B,X,_,X,B,_,_,_,B, },
      { B,_,_,_,B,_,X,_,B,_,_,_,B, },
      { B,_,X,_,B,B,X,B,B,_,X,_,B, },
      { B,_,_,_,X,X,X,X,X,_,_,_,B, },
      { B,B,B,B,X,X,B,X,X,B,B,B,B, },
      { B,X,_,B,_,_,X,_,_,B,_,X,B, },
      { B,_,X,X,X,B,B,B,X,X,X,_,B, },
      { B,X,_,B,_,_,X,_,_,B,_,X,B, },
      { B,B,B,B,X,X,B,X,X,B,B,B,B, },
      { B,_,_,_,X,X,X,X,X,_,_,_,B, },
      { B,_,X,_,B,B,X,B,B,_,X,_,B, },
      { B,_,_,_,B,_,X,_,B,_,_,_,B, },
      { B,_,_,_,X,X,_,X,X,_,_,_,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
