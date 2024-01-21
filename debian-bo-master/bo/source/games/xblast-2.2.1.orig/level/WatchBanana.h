/* XBlast 2.1.8 level */
static BMLevelData WatchBanana =
{
  /* BMLevel */
  {
    "Watch that Banana",
    "Rob & Simon & tristan",
    "xblast.useWatchBanana",
    "It's yellow and bendy!",
    GM_Random | GM_234_Player | GM_All,
    (void *) &WatchBanana,
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
    special_init_special_bombs_30, 
    special_game_void,
    special_extra_special_bomb,
    special_key_special_bomb,
  },
  /* BMPlayerData */
  {
    7, 30, 
    {
      { 1, 1 },
      { 1, 13 },
      { 11, 13 },
      { 11, 1 },
    },
    PM_Polar, 3, 
    IllRun, IF_Kick, 
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
      { BLIronFloor,     "Black",     "gold",   "Yellow" },
      { BLIronFloor_S,   "Black",     "gold",   "Yellow" },
      { BLDarkBlock,     "Black",     "DarkOrchid4",   "MediumOrchid" },
      { BLDarkBlockRise, "Black",     "DarkOrchid4",   "MediumOrchid2" },
      { BLExtra,         "Black",     "lime green", "Black" },
      { BLExtra_O,       "Black",     "lime green", "Black" },
      EXTRA_BOMB,
      EXTRA_RANGE,
      EXTRA_TRAP,
      EXTRA_NAPALM,
      { BLScoreFloor,    "Black", "Black",   "Black" },
    },
  },
  /* BMMapData */
  {
    ShadowBlock, DEnone,
    { 20, 40, 46, 56, 56},
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B},
      { B,_,_,_,_,_,_,_,_,_,_,_,B},
      { B,_,_,_,_,_,_,_,_,_,_,_,B},
      { B,_,_,_,_,_,_,_,_,_,_,_,B},
      { B,_,_,_,X,X,_,X,X,_,_,_,B},
      { B,_,_,_,X,X,X,X,X,_,_,_,B},
      { B,_,_,_,X,X,X,X,X,_,_,_,B},
      { B,_,_,_,_,X,X,X,_,_,_,_,B},
      { B,_,_,_,X,X,X,X,X,_,_,_,B},
      { B,_,_,_,X,X,X,X,X,_,_,_,B},
      { B,_,_,_,X,X,_,X,X,_,_,_,B},
      { B,_,_,_,_,_,_,_,_,_,_,_,B},
      { B,_,_,_,_,_,_,_,_,_,_,_,B},
      { B,_,_,_,_,_,_,_,_,_,_,_,B},
      { B,B,B,B,B,B,B,B,B,B,B,B,B}
    },
  },
};
