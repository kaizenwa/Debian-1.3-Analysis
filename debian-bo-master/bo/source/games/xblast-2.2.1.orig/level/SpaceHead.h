/* XBlast 2.1.8 level */
static BMLevelData SpaceHead =
{
  /* BMLevel */
  {
    "Space Head",
    "Garth Denley",
    "xblast.useSpaceHead",
    "Get the extras  but don't let the bombs hit you.",
    GM_Random | GM_23456_Player | GM_All,
    (void *) &SpaceHead,
    NULL,
  },
  /* BMShrinkData */
  {
    shrink_compound_solid,
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
      {  3,  2 },
      {  3, 12 },
      {  9, 12 },
      {  9,  2 },
      {  2,  7 },
      { 10,  7 },
    },
    PM_Inner, 2,
    IllRun, IF_Kick,
  },
  /* BMBombData */
  {
    bomb_click_clockwise, bomb_click_clockwise, bomb_click_contact,
    GoStop, FUSEnormal,
    BMTnormal, BMTconstruction, BMTnormal,
  },
  /* BMGraphicsData */
  {
    {
      { BLGhost, "Black", "ForestGreen", "Black" },
      { BLGhost, "Black", "ForestGreen", "Black" },
      { BLGhostSq, "Black", "ForestGreen", "Firebrick1" },
      { BLGhostSqRise, "Black", "ForestGreen", "Pink" },
      { BLGhostCi, "Black", "ForestGreen", "RoyalBlue" },
      { BLGhostCiRise, "Black", "ForestGreen", "RoyalBlue" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_CONSTR,
      { BLScoreFloor, "Black", "Black", "Black" },
    },
  },
  /* BMMapData */
  {
    ShadowNone, DEspecial,
    { 20, 40, 40, 60, 60 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,B,_,_,_,_,_,X,_,B,_,B, },
      { B,_,X,X,_,_,X,_,B,_,_,_,B, },
      { B,_,_,B,_,_,B,_,_,_,X,_,B, },
      { B,_,_,_,_,X,X,X,_,_,B,_,B, },
      { B,_,_,_,B,X,X,X,X,_,X,_,B, },
      { B,_,_,_,X,B,X,B,X,_,_,_,B, },
      { B,_,X,_,X,X,X,X,B,_,_,_,B, },
      { B,_,B,_,_,X,X,X,_,_,_,_,B, },
      { B,_,X,_,_,_,B,_,_,B,_,_,B, },
      { B,_,_,_,B,_,X,_,_,X,X,_,B, },
      { B,_,B,_,X,_,_,_,_,_,B,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
