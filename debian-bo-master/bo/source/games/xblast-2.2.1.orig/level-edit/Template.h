/*
 * Programm XBLAST V2.2.1 or higher
 * (C) by Oliver Vogel (e-mail: vogel@ikp.uni-koeln.de)
 * January 22th, 1997
 * started August 1993
 *
 * File: level/Template.h 
 *       template for creating XBlast levels
 */ 

/* XBlast 2.1.8 level */
static BMLevelData Template =
{
  /* BMLevel */
  {
    /*
     * Fill in the title of the level 
     */
    "Level Template",
    /*
     * Fill in your name
     */
    "Hermann Mustermann",
    /*
     * Set the name for the use level Resource
     * To get a suitable name, do the following
     *   1. take the title
     *   2. let each word start with a capital letter
     *   3. remove all nonalphanumeric characters (e.g whitespace, quotes etc)
     *   4. put xblast.use before it
     *   Thats all
     *
     * Example: Level "Seek'n Destroy"
     *   1. "Seek'n Destroy"
     *   2. "Seek'N Destroy"
     *   3. "SeekNDestroy"
     *   4. "xblast.useSeekNDestroy"
     */
    "xblast.useTemplate",
    /* 
     * Fill in a (short) hint on your level
     */
    "The level is best played during full moon",
    /*
     * Enter a combination of game mode flags (using the bitwise OR |) 
     * This flags define in which game modes, the level can be played
     * The following flags are defined
     *   GM_Random   - This level allows random player positions
     *   GM_2_Player - This level can be played with 2 players 
     *                 (2 teams in double mode)
     *   GM_3_Player - This level can be played with 3 players 
     *                 (3 teams in double mode)
     *   GM_4_Player - This level can be played with 4 players 
     *                 (2 teams in team mode, 4 teams in double mode)
     *   GM_5_Player - This level can be played with 5 players
     *   GM_6_Player - This level can be played with 6 players 
     *                 (3 teams in team mode)
     *   GM_Single   - This level can be played in the (normal) single mode.
     *   GM_Team     - This level can be played in the team mode
     *   GM_Double   - This level can be played in the double mode
     *
     * Additionally the following macros are defined
     *   GM_234_Player   - This level can be player either with 2, 3 or 4 players
     *   GM_2345_Player  - This level can be player either with 2, 3, 4 or 5 
     *                     players
     *   GM_23456_Player - This level can be player either with 2, 3, 4, 5 or 6 
     *                     players
     *   GM_All          - This level can be played in single, team or double 
     *                     mode
     */
    GM_Random | GM_23456_Player | GM_All,
    /*
     * Put a reference to your level struct 
     * Sorry it's needed for technical reasons.
     */
    (void *) &Template,
    /*
     * Just leave it NULL.
     */
    NULL,
  },
  /* BMShrinkData */
  {
    /*
     * Select the shrink pattern you want to use in here
     * Valid shrinks are
     *   shrink_void            - no shrink at all 
     *   shrink_spiral          - the normal two level spiral shrink at half time 
     *   shrink_speed_spiral    - a faster variant of the above
     *   shrink_spiral_plus     - as shrink_spiratm but also clears the rim 
     *                            (cf "Losange Over-Excitation") 
     *   shrink_spiral_3        - spiral shrinking with THREE levels at half time
     *   shrink_early_spiral    - like shrink_spiral, but starting at 3/8 of 
     *                            game time. (cf "Fungus Fun")
     *   shrink_compound        - the normal continous compound shrinking
     *   shrink_compound_f      - fancier version of compound shrink 
     *   shrink_compound_2_f    - as above, but this one shrinks only two 
     *                            levels (cf "Would ya' likes some Junkie?")
     *   shrink_lazy_compound_f - lazier version of fancy compound shrink (cf 
     *                            "Sky Show")
     *   shrink_compound_solid  - full solid compound shrinking (cf "Hot 
     *                            Stuff", "Space Head")
     *   shrink_savage_compound - compound shrink with two levels per step 
     *                            (cf "Survivor")
     *   shrink_compound_extra  - compound shrink with an extra row of 
     *                            blastables (cf "Chain Reaction")
     *   shrink_down            - downward shrink (cf "Gravitation")
     *   shrink_down_f          - fancier version of above
     *   shrink_quad            - two level of quad shrink
     *   shrink_constrict_wave  - A wave of inward moving blocks (3 levels)
     */
    shrink_spiral,
    /*
     * Define additional blocks to draw during game here
     * If you don't need them, use the macro SCRAMBLE_VOID instead
     * Take a look at level like "Halloween.h", "Indiana Jones.h"
     * for examples.
     */
    SCRAMBLE_VOID,
    /*
     * Define additional blocks to cleared during game here
     * If you don't need them, use the macro SCRAMBLE_VOID instead
     */
    SCRAMBLE_VOID,
  },
  /* BMFuncData */
  {
    /*
     * Define a special init function, performed at the beginning of the level.
     * Valid functions are:
     *   special_init_void              - nothing happens. In most case
     *                                    this one is sufficient. 
     *   special_init_special_bombs_30  - give each player 30 special bombs
     *                                    at the start. The type of bomb is
     *                                    defined in the BMBombData substruct. 
     *   special_init_nasty_walls       - activate bomb launching walls, like
     *                                    in "Survival of the Fittest". You
     *                                    must set the special_game_function
     *                                    to "special_game_nasty_walls" though.
     *   special_init_nasty_walls_2     - an even nastier version of the above, 
     *                                    (cf "Out of the Frying Pan")
     */
    special_init_void,
    /*
     * Define a special game function, performed at each turn of the game
     * Valid functions are:
     *   special_game_void        - nothing happens.
     *   special_game_nasty_walls - You need this one for bomb launching
     *                              walls (see above)
     *   special_game_haunt       - Use this function, if you want to have 
     *                              "haunted" bombs in your level (cf
     *                              "Haunted House").
     *   special_game_haunt_fast  - This a nastier version of the above, as
     *                              used in "Mind Games" 
     */
    special_game_void,
    /*
     * Define the special extra function, which is executed when the
     * player picks up a special extra in your game. As you can
     * see you can only have one type of special extra to pickup in a level.
     * Choose the function according to your special extra:
     * Valid functions are:
     *   special_extra_void         - no special extra
     *   special_extra_invincible   - Star Extra (Invincible)
     *   special_extra_kick         - Moving Bomb Extra (Kick)
     *   special_extra_teleport     - Beam Extra (Teleporter)
     *   special_extra_RC           - RC Extra (Remote Control)
     *   special_extra_ignite_all   - Button Extra (Ignite All Bombs)
     *   special_extra_air          - Clouds Extra (Airpump)
     *   special_extra_special_bomb - For any type of special bombs,
     *                                the type itself must be defined in 
     *                                BMBombData
     */
    special_extra_kick,
    /*
     * The special key function is needed for extras which are avtivated
     * by the special key (TAB, KP_+, Return). The extra can either be available
     * as pickup (defined above) or as a default extra in the BMPlayerData. You 
     * can choose from one of the following functions:
     *   special_key_void          - no extra, which must be activated by key
     *                               (Kick, Star and Button )
     *   special_key_RC            - RC Extra (Remote Control)
     *   special_key_teleport      - Beam Extra (Teleporter)
     *   special_key_air           - Clouds Extra (Airpump)
     *   special_key_special_bomb  - For any type of special bombs,
     *                               the type itself must be defined in 
     *                               BMBombData
     */
    special_key_void,
  },
  /* BMPlayerData */
  {
    /*
     * Define the number of bombs, each player has a the start of the level 
     */
    1, 
    /*
     * Define the initial bomb range of each player. 
     */
    2,
    {
      /*
       * Define the position of player #1 as { row, column }, where 1
       * is the highest row and 11 the lowest row available. Column 1
       * is the leftmost, while 13 the rightmost one.
       */
      {  1,  1 },
      /*
       * the same for player #2
       */
      {  1, 13 },
      /*
       * the same for player #3
       */
      { 11, 13 },
      /*
       * the same for player #4. You need at least four positions, if you allow 
       * random player positions in your level. Even if you only want to 
       * support only 3 players.
       */
      { 11,  1 },
      /*
       * the same for player #5
       */
      {  1,  7 },
      /*
       * the same for player #6. If you want to have more than five player and 
       * random player positions, all six positions must be defined
       */
      { 11,  7 },
    },
    /*
     * This options defines how the player positions change in the Double Mode
     * (Two players of the same color at each display). It is save to leave
     * them untouched, though it means both player start on the same position 
     * The first parameter defines the pattern, while the second one is the
     * distance the player are moved. Valid patterns are:
     *   PM_Same       - both use the same position
     *   PM_Polar      -
     *   PM_Right      - 
     *   PM_Inner      -
     *   PM_LeftRight  -
     *   PM_Below      -
     *   PM_Horizontal -
     *   PM_Vertical   -
     *   PM_Circle     -
     */
    PM_Same, 0,
    /*
     * This parameter defines if the player starts the game healthy or already
     * infected with an illness. Possible values are:
     *   Healthy        - just as it says, no initial illness
     *   IllBomb        - Permanent Random Bombing
     *   IllSlow        - Permanent Slow Motion
     *   IllRun         - Permanent Running (cf "Running Man")
     *   IllMini        - Permanent Mini bombs (range 1)
     *   IllEmpty       - Player cannot drop, unless the get ill.
     *   IllInvisible   - Permanent Invisibilty
     *   IllMalfunction - Bombs always malfunction.
     *   IllReverse     - Permanent reverse control (cf "Moonwalking")
     *   IllTeleport    - Permanent random telporting
     */
    Healthy, 
    /*
     * This set of flags defines initial extras for each player. 
     * Combine them using the bitwise OR "|". But do NOT forget, that you can
     * have only one extra, which needs the special key (see above)  The 
     * following flags can be used:
     *   IF_None     - no initial special extra
     *   IF_Kick     - permanent kicking 
     *   IF_RC       - permanent remote control
     *   IF_Teleport - permanent teleporter
     *   IF_Airpump  - permanent airpump
     */
    IF_None,
  },
  /* BMBombData */
  {
    /*
     * Define the the bomb click behaviour (i.e. what happens if the bomb rolls 
     * into another one) here. The following value can be used:
     *   bomb_click_none          - nothing happens (it just stops of course)
     *   bomb_click_snooker       - snooker bombs (the other bombs starts moving)
     *   bomb_click_contact       - the bomb explode on contact
     *   bomb_click_clockwise     - the bomb is rebounded in clockwise direction 
     *   bomb_click_anticlockwise - the same, but anticlockwise
     *   bomb_click_randomdir     - the bomb is rebounded in a random direction
     */
    bomb_click_none,
    /*
     * Define the the wall click behaviour of the bombs (i.e. what happens if 
     * the bomb rolls into a wall) here. The following value can be used:
     *   bomb_click_none          - nothing happens (it just stops of course)
     *   bomb_click_contact       - the bomb explode on contact                 
     *   bomb_click_clockwise     - the bomb is rebounded in clockwise direction 
     *   bomb_click_anticlockwise - the same, but anticlockwise                 
     *   bomb_click_randomdir     - the bomb is rebounded in a random direction 
     *   bomb_click_rebound       - the bomb is rebounded in the opposite       
     *                              direction.                                  
     */
    bomb_click_none,
    /*
     * Define the the player click behaviour of the bombs (i.e. what happens if 
     * the bomb rolls into a player) here. The bombs ALWAYS stuns the player.
     * The following value can be used:
     *   bomb_click_none          - nothing happens (it just stops of course)
     *   bomb_click_initial       - the move in the initial bomb direction
     *                              as defined below (cf "Gravitation")
     *   bomb_click_thru          - the bomb continues its journey.
     *   bomb_click_contact       - the bomb explode on contact                 
     *   bomb_click_clockwise	  - the bomb is rebounded in clockwise direction
     *   bomb_click_anticlockwise - the same, but anticlockwise                 
     *   bomb_click_randomdir	  - the bomb is rebounded in a random direction 
     *   bomb_click_rebound	  - the bomb is rebounded in the opposite       
     *				    direction.                                  
     */
    bomb_click_none,
    /*
     * Define the initial moving direction for bombs, when dropped. In most 
     * cases you will use GoStop, though. The following value are allowed:
     *   GoStop                     - The bomb just stays were its 
     *   GoLeft,GoRight,GoUp,GoDown - The bomb moves in the given direction
     *                                (cf. "Gravitation").
     */
    GoStop, 
    /*
     * Define the bomb fuse time here. Valid values are:
     *   FUSEnormal - the default fuse time
     *   FUSEsort   - shorter fuse time as in "Suicide"
     *   FUSElong   - longer fuse time as in "When I Was Your Age"
     */
    FUSEnormal,
    /*
     * Define the type of the default bombs, each player has automatically.
     * The following type are available:
     *   BMTnormal        - just normal bombs
     *   BMTnapalm        - Napalm bombs (cf "Napalm Justice")
     *   BMTfirecracker   - Firecrackers (cf "Firecracker Surprise")
     *   BMTconstruction  - Construction bombs, which create a blaszable
     *                      wall on explosion (cf "Bricks with extra Fries")
     *   BMTthreebombs    - drop three bombs in a row 
     *   BMTgrenade       - Grenades (covering a larger area)
     *   BMTtrianglebombs - drops three bombs in a triangle formation
     *   BMTdestruction   - Destruction bombs
     *   BMTfungus        - Fungus bomb (a growing colony of mini bombs)
     *   BMTrenovation    - pushes away solid blocks
     *   BMTpyro          - Pyro bombs (cf "Sky Show");
     *   BMTrandom        - random bomb, either napalm, grenade, firecracker,
     *                      fungus or pyro bomb is dropped
     */
    BMTnormal, 
    /*
     * Define the special bomb used (if any). Don't forget to set the
     * special_key_function to special_key_special_bomb (see above).
     * The bomb types are the same as above
     */
    BMTnormal, 
    /*
     * Define the type of bombs which are hidden under blastable blocks.
     * The bomb types are the same as above
     */
    BMTnormal,
  },
  /* BMGraphicsData */
  {
    /* 
     * The graphics to be used in the level is defined here
     * Each definition consists of four parts:
     *   - block bitmap identifier (BL....)
     *   - foregound color name (appears Black in monochrome)
     *   - background color name (appears White in monochrome)
     *   - additional color name (does not appears in monochrome)
     * For the extra block tiles use should use the predfined macros
     */
    {
      /*
       * Free Block. Typical bitmaps are:
       *  - BLKaroLight    (cf "Seek'N Destroy")
       *  - BLKaroDark     (cf "Treasure Hunt")
       *  - BLHex          (cf "Hexagon Excitation")
       *  - BLLegoFloor    (cf "Legoland")
       *  - BLIronFloor    (cf "Full Power Level")
       *  - BLCityFree     (cf "Paradise City")
       *  - BLSphereHalf   (cf "XBlast 2000")
       *  - BLChessFloor   (cf "Running Man")
       *  - BLDarkWay      (cf "Halloween")
       *  - BLButtonFloor  (cf "Shrinking World")
       *  - BLMrBeamFree   (cf "Mr Beam")
       *  - BLRockFloor    (cf "Nothing Short of Warfare")
       *  - BLGhost        (cf "Spave Head")
       */
      { BLKaroLight,   "Black", "SlateBlue", "SteelBlue" },
      /*
       * Shadowed block. A free block with a shadow cast on from the left
       * In most cases you can the the free tile bitmap identifier and
       * and append "_S" to it. Possible values are:
       *  - BLKaroLight_S    (cf "Seek'N Destroy")
       *  - BLKaroDark_S     (cf "Treasure Hunt")
       *  - BLLegoFloor_S    (cf "Legoland")
       *  - BLIronFloor_S    (cf "Full Power Level")
       *  - BLCityFree_S     (cf "Paradise City")
       *  - BLSphereHalf_S   (cf "XBlast 2000")
       *  - BLChessFloor_S   (cf "Running Man")
       *  - BLDarkWay_S      (cf "Halloween")
       *  - BLRockFloor_S    (cf "Nothing Short of Warfare")
       * Alternatively you can use the bitmap as for the Free Block.
       */
      { BLKaroLight_S, "Black", "SlateBlue", "SteelBlue" },
      /*
       * Solid wall. This one cannot be blasted. Typical bitmaps are:
       *  - BLPyramid     (cf "Seek'N Destroy")
       *  - BLWall        (cf "Treasure Hunt")
       *  - BLHexWall     (cf "Hexagon Excitation")
       *  - BLLegoWhite   (cf "Legoland")
       *  - BLDarkBlock   (cf "Full Power Level")
       *  - BLDarkHouse   (cf "Paradise City")
       *  - BLSphereDark  (cf "XBlast 2000")
       *  - BLTemple      (cf "Hallways")
       *  - BLBookShelf   (cf "Running Man")
       *  - BLRIP         (cf "Halloween")
       *  - BLMrBeamTv    (cf "Mr Beam")
       *  - BLWeight      (cf "Nothing Short of Warfare")
       *  - BLGhostSq     (cf "Space Head")
       */
      { BLPyramid,     "Black", "Goldenrod", "Goldenrod" },
      /*
       * Rising Wall. This block will be used by most shrink patterns
       * before drawing an actual solid wall. Otherwise it behaves
       * like a normal solid wall. So if you don't use a shrink pattern
       * this block can be used as an alternative solid wall. Typical Rising 
       * Wall bitmaps are:
       *  - BLPyramidRise    (cf "Seek'N Destroy")
       *  - BLWallRise       (cf "Treasure Hunt")
       *  - BLDarkBlockRise  (cf "Full Power Level")
       *  - BLRIPRise        (cf "Halloween")
       *  - BLDarkHouseRise  (cf "Paradise City")
       *  - BLWeightRise     (cf "Nothing Short of Warfare")
       *  - BLGhostSqRise    (cf "Space Head")
       * Alternatively you can use the bitmap as for the Solid Wall.
       */
      { BLPyramidRise, "Black", "Goldenrod", "LightGoldenrod" },
      /*
       * Blastable block. This block might reveal an extra symbol when blasted
       * Typical bitmaps are:
       *  - BLExtra        (cf "Seek'N Destroy")
       *  - BLChest        (cf "Treasure Hunt")
       *  - BLHexExtra     (cf "Hexagon Excitation")
       *  - BLLegoBlack    (cf "Legoland")
       *  - BLBricks       (cf "Gravitation")
       *  - BLLightHouse   (cf "Paradise City")
       *  - BLSphereLight  (cf "XBlast 2000")
       *  - BLChessSphere  (cf "Running Man")
       *  - BLPumpkin      (cf "Halloween")
       *  - BLBox          (cf "Shrinking World")
       *  - BLMrBeamBear   (cf "Mr Beam")
       *  - BLGhostCi      (cf "Space Head")
       */
      { BLExtra,       "Black", "Sienna",    "Orange" },
      /*
       * Blasted block. Animation step showing an open or cracked version of
       * a blastable block. Typical bitmaps are:
       * BLExtra_O         (cf "Seek'N Destroy")
       * BLChest_O         (cf "Treasure Hunt")
       * BLBricks_O        (cf "Gravitation")
       * BLSphereLight_O   (cf "XBlast 2000") 
       * BLHexExtra_O      (cf "Hexagon Excitation")
       * BLChessSphere_O   (cf "Running Man")
       * BLLegoBlack_O     (cf "Legoland")
       * BLLightHouse_O    (cf "Paradise City")
       * BLPumpkin_O       (cf "Halloween")
       * BLMrBeamBearExp   (cf "Mr Beam")
       * BLGhostCiRise     (cf "Space Head")
       */
      { BLExtra_O,     "Black", "Sienna",    "Orange" },
      /*
       * The bomb extra. Just leave it as it is
       */ 
      EXTRA_BOMB,
      /*
       * The range extra. Just leave it as it is
       */ 
      EXTRA_RANGE,
      /*
       * The skull symbol. Just leave it as it is
       */
      EXTRA_TRAP,
      /*
       * The special extra symbol. Choose a symbol according to your
       * special extra function (see above). The following macros
       * are defined:
       *  - EXTRA_KICK          Moving Bomb (kick extra)
       *  - EXTRA_INVINC        Star (Invincibilty)
       *  - EXTRA_BUTTON        Button (Ignite All Bombs)
       *  - EXTRA_CONSTR        Construction bomb
       *  - EXTRA_RC            Remote Control
       *  - EXTRA_BEAM          Teleporter
       *  - EXTRA_AIRPUMP       Clouds (Airpump)
       *  - EXTRA_NAPALM        Napalm Bombs
       *  - EXTRA_FIRECRACKER   Firecracker Bombs
       *  - EXTRA_SYRINGE       Syringe (Junkie Virus)
       */
      EXTRA_KICK,
      /*
       * Void Block. This block is used for filling unreachable parts of the
       * map. The compound shrinking pattern uses it for the outer rim.
       * You can either use the BLScoreFloor bitmap, or the defintion from
       * your Free Block.
       */
      { BLScoreFloor, "RoyalBlue", "RoyalBlue", "RoyalBlue" },
    },
  },
  /* BMMapData */
  {
    /*
     * The Shadow mode defines, which blocks cast a shadow to the a
     * right neighboroughing free block. There are four possibilties:
     *  - ShadowNone   no blocks cast a shadow (i.e the Shadowed block will not
     *                 be used)
     *  - ShadowBlock  only the Solid Wall and the Rising Wall cast a shadow.
     *  - ShadowExtra  only the Blastable Blocks casts a shadow
     *  - ShadowFull   all of the above blocks cast a shadow.
     */
    ShadowFull, 
    /*
     * The special extra distribution mode. The following modes are possible
     *  DEnone    - The special is lost after is has been taken.
     *  DEsingle  - At most one special extras is redistributed after the
     *              players death.
     *  DEall     - All special extras picked up are redistrubted after the
     *              players death
     *  DEspecial - For every three special bombs picked up, one extra symbol
     *              is distributed. Note that special bombs come in packs of 
     *              three.
     *  DEget     - The extra reappears somewhere else on the map, just after 
     *              it has been picked up. Works great for the Button extra.
     */
    DEall,
    /*
     * The Extras probability table. Foreach blastable block a random number 
     * between 0 and 63 is created and the extra is determined from the
     * following table:
     *  - 0        to Value[0]-1  : Bomb Extra
     *  - Value[0] to Value[1]-1  : Range Extra
     *  - Value[1] to Value[2]-1  : Skull Symbol
     *  - Value[2] to Value[3]-1  : Special Extras (as defined above)
     *  - Value[3] to Value[3]-1  : Hidden Bomb
     *  - Value[3] to 63          : empty
     */
    { 16, 48, 48, 60, 60 },
    /*
     * This part contains the layout of the level map. For historical reasons
     * the X direction is down and the Y direction if right. The following
     * blocks a re defined:
     *   _  free or shadowed block
     *   B  Solid Wall
     *   R  Rising Wall
     *   X  Blastable Block
     *   b  bomb extra
     *   r  range extra
     *   s  skull smybol
     *   q  special extra
     *   v  void block 
     *   e  free block with bomb
     *   V  no block, just the backrgound pattern (cf "Legoland")
     */
    {
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
      { B,_,_,_,X,_,X,_,X,_,_,_,B, },
      { B,_,B,X,B,X,B,X,B,X,B,_,B, },
      { B,_,X,_,X,_,X,_,X,_,X,_,B, },
      { B,X,B,X,B,X,B,X,B,X,B,X,B, },
      { B,_,X,_,X,_,X,_,X,_,X,_,B, },
      { B,_,B,X,B,X,B,X,B,X,B,X,B, },
      { B,_,_,_,X,_,X,_,X,_,_,_,B, },
      { B,X,B,X,B,X,B,X,B,X,B,_,B, },
      { B,_,X,_,X,_,X,_,X,_,X,_,B, },
      { B,X,B,X,B,X,B,X,B,X,B,X,B, },
      { B,_,X,_,X,_,X,_,X,_,X,_,B, },
      { B,_,B,X,B,X,B,X,B,X,B,_,B, },
      { B,_,_,_,X,_,X,_,X,_,_,_,B, },
      { B,B,B,B,B,B,B,B,B,B,B,B,B, },
    },
  },
};
