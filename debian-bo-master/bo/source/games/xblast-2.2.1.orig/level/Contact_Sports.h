/* XBlast 2.1.8 level */
static BMLevelData Contact_Sports =
{
  /* BMLevel */
  {
    "Contact Sports",
    "Garth Denley",
    "xblast.useContactSports",
    "Watch what you kick",
    GM_Random | GM_234_Player | GM_All,
    (void *) &Contact_Sports,
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
    bomb_click_contact, bomb_click_none, bomb_click_none,
    GoStop, FUSEnormal,
    BMTnormal, BMTnormal, BMTnormal,
  },
  /* BMGraphicsData */
  {
    {
      { BLLegoFloor, "Black", "LightYellow", "White" },
      { BLLegoFloor_S, "Black", "LightYellow", "White" },
      { BLLegoWhite, "Black", "Red", "Black" },
      { BLLegoWhite, "Gray50", "Pink", "Black" },
      { BLLegoBlack, "White", "White", "Black" },
      { BLLegoBlack_O, "White", "Black", "Black" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_KICK,
      { BLScoreFloor, "RoyalBlue", "RoyalBlue", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    ShadowFull, DEall,
    { 16, 30, 48, 60, 60 },
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,_,_,_,X,_,B,_,X,_,_,_,B, },
      { B,_,B,B,B,_,_,_,B,B,B,_,B, },
      { B,_,B,_,_,_,B,_,_,_,B,_,B, },
      { B,X,B,_,X,X,X,X,X,_,B,X,B, },
      { B,_,_,_,X,B,X,B,X,_,_,_,B, },
      { B,X,_,X,X,X,X,X,X,X,_,X,B, },
      { B,B,_,B,X,X,B,X,X,B,_,B,B, },
      { B,X,_,X,X,X,X,X,X,X,_,X,B, },
      { B,_,_,_,X,B,X,B,X,_,_,_,B, },
      { B,X,B,_,X,X,X,X,X,_,B,X,B, },
      { B,_,B,_,_,_,B,_,_,_,B,_,B, },
      { B,_,B,B,B,_,_,_,B,B,B,_,B, },
      { B,_,_,_,X,_,B,_,X,_,_,_,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
