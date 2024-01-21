/* XBlast 2.1.8 level */
static BMLevelData Peppercorn =
{
  /* BMLevel */
  {
    "Peppercorn",
    "Garth Denley",
    "xblast.usePeppercorn",
    "Time those kicks  and watch out for the button",
    GM_Random | GM_23456_Player | GM_All,
    (void *) &Peppercorn,
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
    special_extra_ignite_all,
    special_key_void,
  },
  /* BMPlayerData */
  {
    4, 1,
    {
      {  1,  1 },
      {  1, 13 },
      { 11, 13 },
      { 11,  1 },
      {  4,  7 },
      {  8,  7 },
    },
    PM_Horizontal, 2,
    Healthy, IF_Kick,
  },
  /* BMBombData */
  {
    bomb_click_snooker, bomb_click_none, bomb_click_none,
    GoStop, FUSEnormal,
    BMTnormal, BMTnormal, BMTnormal,
  },
  /* BMGraphicsData */
  {
    {
      { BLLegoFloor, "Black", "MidnightBlue", "Gray80" },
      { BLLegoFloor_S, "Black", "MidnightBlue", "Gray80" },
      { BLLegoWhite, "Black", "HotPink", "Black" },
      { BLLegoWhite, "DeepPink", "Pink", "Black" },
      { BLLegoBlack, "SpringGreen", "SpringGreen", "Black" },
      { BLLegoBlack_O, "SpringGreen", "Black", "Black" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_BUTTON,
      { BLScoreFloor, "RoyalBlue", "RoyalBlue", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowFull, DEget,
    { 30, 30, 30, 40, 50 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,_,B,X,_,_,B,_,_,X,B,_,B, },
      { B,_,B,X,B,X,B,X,B,X,B,_,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,B,B,_,B,B,X,B,B,_,B,B,B, },
      { B,_,X,_,B,X,_,X,B,_,X,_,B, },
      { B,_,B,_,X,_,X,_,X,_,B,_,B, },
      { B,X,X,_,_,X,B,X,_,_,X,X,B, },
      { B,_,B,_,X,_,X,_,X,_,B,_,B, },
      { B,_,X,_,B,X,_,X,B,_,X,_,B, },
      { B,B,B,_,B,B,X,B,B,_,B,B,B, },
      { B,_,_,_,_,_,_,_,_,_,_,_,B, },
      { B,_,B,X,B,X,B,X,B,X,B,_,B, },
      { B,_,B,X,_,_,B,_,_,X,B,_,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
