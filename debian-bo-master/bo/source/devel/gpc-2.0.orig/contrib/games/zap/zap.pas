
#include "screen.ph"

PROGRAM Zap(Input, Output, help,YourTopTen,SystemTopTen);

	(*************************************************************
	
      	                             HACK
				     
	   HACK is a computer fantasy game that developed from ROGUE,
	  THE game written by Michael Toy and Kenneth Arnold. The 
	  original version of ROGUE is written in C to run under UNIX*.

	   As the VAX-750 is quite heavily used by the computer science
	  lab, we felt that something should be done to allow people
	  to play ROGUE when they liked. (Most of all, ourselves...)
	   Then one day we got our hands on a program called HACK, which
	  is a version of ROGUE written by someone whose name I have
	  forgotten (unfortunately...), but it was also written in C.
	  But since our DEC-20 does not have a C compiler, one of us
	  crosscompiled the C code into Pascal in a couple of days.
	  There was quite (....) a number of Giant Bugs in the C
	  version and the program was almost completely 
	  reconstructed, but it is still looking quite much the same
	  as the original one. (Fortunatelly not quite as unclear,
	  but it is still a translation...)
	   This program is quite portable (would you believe it), but
	  of course there has to be some useful extensions used to
	  make things harder. If you try to move it, you'll see.
	  Most of the nonstandard features are not needeed, with
	  the exception of random access files. Anyway PAX is quite
	  happy with it.
	   In this version of HACK there is quite a lot of features
	  not found in the original one written in C.
	   HACK is distributed in source form, as we feel that the
	  best part of it is not only the playing itself, but
	  implementing new features. We have tried to be reasonable
	  (Hmmm...), so the players would not lose their intrest in 
	  it.
	   The first versions of HACK were somewhat easier than this
	  one, but we felt that if you ask for troubles, you will 
	  get them...
	   We are developing HACK as long as we are intrested so
	  you can once in a while ask for a new version, if you
	  want to keep up to date.
	  
	      This version of HACK is written in:

		     Helsinki University of Technology
			     Computing Centre
				  TeKoLa	     
			       F I N L A N D

	      The authors are the following creatures:

				THE Jukkas
				  A JPK
				  Lauri
	 *************************************************************
	 *           UNIX is a trademark of Bell Laboratories        *
	 *************************************************************
	 *                HACK is not a trademark of us		     *
	 *************************************************************
	 *      PAX is a pascal cross-compiler written in TeKoLa     *
	 *       that YOU can get almost FREE of charge from us      *
	 *************************************************************
	 *              ZAP is the current version of HACK           *
	 *************************************************************)
	
 (* terminal I/O by screen module *)

(* Changes by THE Jukkas and A Jpk:
     Monsters move more cleverly.
     Small monsters seldom leave anything when killed.
     You can't get out without IT.
     Point counting altered.
     We run more cleverly (capital L is somewhat like rogue's f-l).
     Make monsters go to traps, too.
     Monsters can carry things.
     Several monsters which had no specialities got some.
     Waking up a monster depends now on the distance between it and you.
     Poison resistance ring is more useful (helps against poison).
     Identify scrolls are generated more probably.
     Weights of objects changed; you can carry more, and stronger armors
     are heavier.
     COCKATRICE doesn't kill.  ???????? (At least JPK thinks so..)
     You can go down from the maze to encounter more gold, monsters,
     mazes...
     Different types of rooms added.
     Monsters are hungry.
     More wands, scroll, and potions.
     Quite a number of 'useful' monsters more than before.
     Projectiles and rays bounce more realistically from walls.
     Added a few more monsters under the first maze.
     Added the world of giants and a way to get upstairs more easily.
     Added more magical Frobozz stuff.
*)

LABEL 9999; (*for exiting zap*)
CONST

(*   HelpFile	  = 'structure-of-game:<directory-of-game>name-of-game.HLP'; *)
(*   SystemScores = 'structure-of-game:<directory-of-game>name-of-game.Q8C'; *)
(*   UserScores   = 'PS:<logged-in-directory>ZAP.Q8C'; *)

      lockname_prefix = '/tmp/zap-';
      system_lockname = '/tmp/zap-syslock';

      NumberOfScoreRecords = 10;

      (* screen is (at least) 24 by 80: *)

      MaxRow = 23;
      xmin = 0;
      ymin = 0;
      ymax = 21;	(* MaxRow-2 *)
      xmax = 79;

      TopLine    = ymin;
      BottomLine = MaxRow;

      MaxWeight = 127;
      MinWeight = 35;

      HungryLimit = 150;
      WeakLimit   =  50;
      MaxHungry   = 1500; (* Added this so that it makes no sense
			     to eat an immense amount of food. *)

      StrengthWhenSwimming = 9;

      MaxUserLevel = 14;  (* You can get beyond by a potion *)

      mtabM = 8; (* last row number in MonsterTable *)
      mtabN = 6; (* last column number in MonsterTable *)

      LF    = 10;
      ctrlL = 12;
      CR    = 13;
      ESC   = 27;
      quote = '''';
      AmuletChar = '+';
      blank = ' ';

      YourSymbol= '@';
      downwards = '>';
      upwards   = '<';

      ArmorOffset = 2;

      (* Numbers of different kinds of objects.
	 These must match the number of elements in
	 the corresponding enumerated types.*)
      wepnum = 14;
      potnum = 19;
      scrnum = 18;
      trapnum = 10;
      wandnum = 21;
      ringnum = 18;
      armnum = 6;
      gemnum = 19;

      FirstMaze = 29;
      SecondMaze = 36;
      ThirdMaze = 42;
      WorldOfGiants = 43;
      LevelWhereTheAmuletIs = 44;
      LastLevel = 60; (* ??? jtv@hut.fi: Just a guess *)

      MaxNameLength = 20;
      blankname       = '                    ';
      DefaultNameForU = 'anonymous zapper    ';
      maxbyte = 127;

      MaxShort = 32766;

      litThe = 'The ';
      litIt = ' it';
      litPotion = 'potion';
      litArrow = 'arrow';
      litDart  = 'dart';
      YourHandsStopGlowingBlue = 'Your hands stop glowing blue.';
      LitCalled = ' called ';
      litOf = ' of ';
      welcome = 'Welcome to level ';
      nothing = 'Nothing Happens';
      litCreateMonster = 'create monster';
      litEscaped = 'escaped';
      litQuit = 'quit';
      litFire = 'fire';
      litGainStrength = 'gain strength';
      litLight = 'light';
      litIdentify = 'identify';
      mmis = 'magic missile';
      ruby = 'ruby';
      silver = 'silver';
      copper = 'copper';
      litEbony = 'ebony';
      litTele = 'teleportation';
      litWand = 'wand';
      litCursed = 'You can''t. It appears to be cursed.';
      litWelded = ' is welded to your hand!';

      MaxNumberOfRatsPerLevel = 30;

     string_length = 100;

TYPE
     Tstring = string (string_length);

     xval = xmin..xmax;
     yval = ymin..ymax;
     row  = ymin..MaxRow;

     ShortCount = 0..MaxShort;
     short = -MaxShort..+MaxShort;
     unsigned = 0..Maxint;
     positive = 1..Maxint;
     small = 0..maxbyte;
     byte = -MaxByte..MaxByte;

     SignRange = -1..+1;

     mtabRow = 0..mtabM; (* row number in MonsterTable *)
     mtabCol = 0..mtabN; (* column number in MonsterTable *)


     ExperienceType = (novice, fair, good, expert, wizard);

     EndStatus = (escaped,quit,died,digested);

     context = (start,normal,IndefiniteArticle,NoArticle);

     IdentificationStatus =(unknown,named,wellknown);

     NameIndex = 1..MaxNameLength;
     { jtv@hut.fi: Was something else before }
     name = tString;

     string5 = PACKED ARRAY [1..5] OF Char;
     string8 = PACKED ARRAY [1..8] OF Char;

     MonsterState = (NormalState,sleep,flee,mfroz,
		     GuardingTreasures,scared,tamed);
     MonsterSpeed = (NormalSpeed,mconf,mslow,mfast);
     trap = (beartrap,arrow,dart,tdoor,tele,pit,slptrp,StinkingTrap,
	     pierc,MimicTrap);

     LocationType = (empty,wall,sdoor,door,corr,RoomLocation);
     TypeOfRoom = (OrdinaryRoom, HotRoom, ColdRoom, PoisonGasRoom,
		   WaterRoom, ConfuseGasRoom);
     TypeOfWall = (horizontal, vertical, corner);
     FrobozzStuff = (FrobozzScroll, FrobozzWand, FrobozzRing, FrobozzPotion);

     rm = PACKED RECORD
		   ScrSym : Char;       (* screen symbol *)
		   typ : LocationType;
		   room : TypeOfRoom;
		   recognized : Boolean;(* You know its speciality *)
		   new : Boolean;       (* screen location shall be updated *)
		   seen : Boolean;	(* seen by you *)
		   lit : Boolean;
		   CanSee : Boolean;    (* visible NOW to you *)
		 END;

     level = ARRAY [xval,yval] OF rm;
     (* level is an image of the screen *)
          
     misc = (warrow,SlingBullet,CrossBowBolt,wdart,mace,axe,
	     flail,LongSword,TwoHandedSword,dagger,spear,
	     bow,sling,CrossBow,
	     StrikingWand,
	     missile,BoltOfFire,SleepRay,BoltOfCold,DeathRay,
	     IcyWind,
	     NoMonster,
	     Bat,Gnome,Hobgoblin,Jackal,Kobold,Leprechaun,rat,
	     acidblob,Eye,homunculus,imp,Orc,yellowlight,Zombie,
	     Ant,fog,Nymph,piercer,Quasit,quiveringblob,violetfungi,
	     beetle,Centaur,cockatrice,jaguar,killerbee,Mimic,Snake,
	     FreezingSphere,owlbear,Rust,scorpion,teleporter,Wraith,Yeti,
	     displacer,gelatenouscube,leocrotta,minotaur,Troll,ugod,xerp,
	     Invisiblestalker,Umberhulk,Vampire,wumpus,Xorn,zelomp,chameleon,
	     Dragon,ettin,LurkerAbove,neootyugh,trapper,Purpleworm,demon,
	cyclops,exorcist,destroyer,suicider,TwoHeadedEagle,TyrannoSaur,Humanoid,
	     SnakesBite,ScorpionsSting,BeesSting,
	     FallingPiercer, FallingRock, ArrowShot, PoisonDart, PitTrap,
	     StinkingGasTrap, WandOfStriking,
	     LittleDart,PoisonPotion,ScrollOfFire,RayOfDrainLife,PoisonGas,
	     SmellOfWumpus, Drowned, LackOfFood, ExplodingGem, AntSting,
	     UnUsedKiller);

     enemy = missile .. UnUsedKiller;

     armament = warrow .. icywind;  

     MonsterSpecies = NoMonster..humanoid;

     PerMonst = RECORD
		  mlet  : Char;
		  mhd,
		  mmove : small;
		  ac    : byte;
		  damn,
		  damd  : small;
		END;

  
     ThingType = (monsters,objects,traps,golds);

     (* Originally:
	ObjectClass =  (weapons,potions,scrolls,wands,rings,
			armors,gems,food,amulet);		*)


     (* ORDERED enumeration ObjectClass *)

     ObjectClass = (anything,food,armors,weapons,gems,rings,
		    potions,scrolls,wands,amulet);

     ObjectSet	 = SET OF ObjectClass;

     WeaponType = warrow..CrossBow;
     projectile = missile..IcyWind;
     WeaponStrength = ARRAY [WeaponType] OF small;

     PotionType = (RestoreStrength, booze, invisibility, juice, healing, blood,
	           paralysis, MonsterDetection, ObjectDetection, MagicSpirit,
	           sickness, confusion, GainStrength, speed, blindness,
	           GainLevel, ExtraHealing, forgetting, PotionOfFrobozz);

     ScrollType = (EnchantArmor, ConfuseMonster, ScareMonster, BlankScroll,
		   RemoveCurse, EnchantWeapon, CreateMonster, DamageWeapon,
		   genocide, DestroyArmor, AggravateMonster,
		   ScrollOfLight, TeleportScroll, GoldDetection, 
	           identify, MagicMapping, FireScroll, ScrollOfFrobozz);

     WandType =   (WandOfLight, WandOfDarkness, detection, CreatorWand,
		   DestroyObject,
		   striking, SlowMonster, SpeedMonster, UndeadTurning,
		   polymorph, cancellation, TameMonster, DrainLife,
		   TeleportMonster, digging, WandOfMissile, FireWand,
		   SleepWand, cold, death, WandOfFrobozz);

     RingType =   (adornment, TeleportRing, regeneration, searching,
	           SeeInvisible, stealth, floating, PoisonResistance,
  	           aggravate, hunger, FireResistance, ColdResistance,
	           ProtectionFromShapeChangers, AddStrength,
	           IncreaseDamage, protection, MaintainArmor, RingOfFrobozz);

     ArmorType =  (leather,RingMail,scale,chain,splint,plate);

     GemType = 0..18 (*gemnum-1*);

     thing = ^something;
     something = PACKED RECORD
	           next : thing; (* next in a list *)
	           locx : xval; locy : yval; (* location on screen *)
		   CASE what:ThingType OF
		     monsters : (
		       giant,
		       invis,	        (*invisible*)
		       cham,		(*shape-changer*)
		       telport,	(*carrying some rings*)
		       rege,
		       SeeInv,
		       float,
		       pres,
		       fireres : Boolean;
		       strength,
		       protect : small;
		       mspeed : MonsterSpeed;
		       mstat  : MonsterState;
		       mcan : Boolean;  (*has been canceled*)
		       species : MonsterSpecies;
		       TimeForTheMonsterToRemainInTrap:0..7;
		       ObjectsCarried : thing;
		       OrigHp : ShortCount;
		       mhp : ShortCount);
		     objects : (
		       spe : small;
		       olet : Char;
		       quan : small;
		       minus : Boolean;
		       known : Boolean;
		       cursed : Boolean;
		       Magical : Boolean; (* True for FrobozzStuff *)
		       CASE class:ObjectClass OF
			 anything: ();
			 food	 : (fruit  : Boolean);
			 Armors  : (armor  : ArmorType);
			 Weapons : (weapon : WeaponType);
			 Gems    : (gem    : GemType);
			 Rings   : (ring   : RingType);
			 Potions : (potion : PotionType);
			 Scrolls : (scroll : ScrollType);
			 Wands   : (wand   : WandType);
			 amulet	 : ());
		     traps :
			(gflag : trap;
			 seen : Boolean;
			 SeenByMonsters : Boolean;
			 ThereIsWaterInThePit : Boolean);
		     golds:
			(quantity : short);
		     END;

     flag = RECORD
	      slpfg,   (* don't sleep on messages *)
	      jump,    (* jump on long moves, faster *)
	      topl,    (* a top line (message) has been printed *)
	      botl,    (* redram the bottom (status) line *)
	      faint,
	      screen,
	      next,    (* next level is maze *)
	      move,    (* If you make any move that takes time, this is True *)
	      mv,      (* If you actually move around, this is True *)
	      see,
              echo,    (* echo characters *)
	      one,     (* move one space after doors *)
	      dgold,   (* redo your gold *)
	      dhp,     (* redo hit points *)
	      dhpmax,  (* redo max hit points *)
	      dstr,    (* redo strength *)
	      dac,     (* redo armor class *)
	      dulev,   (* your level *)
	      dexp,    (* your points *)
	      dhs,     (* redo hunger state *)
	      dscr :   (* redo something on the screen *)
	        Boolean;
	    END;

     you = RECORD
	     ux : xval; uy : yval;
	     uslow,
	     ufast,
	     uconfused,
	     uinvis : unsigned;
	     ulevel : 1..maxbyte;
	     utrap : 0..7;
	     upit : beartrap..pit;
	     umconf,
	     ufireres,
	     ucoldres,
	     utel,
	     upres,
	     ustelth,
	     uagmon,
	     ufeed,
	     usearch,
	     ucinvis,
	     uregen,
	     ufloat,
	     uswallow : Boolean; (* swallowed by a monster *)
	     uswldtim : 0..15; (* time you have been swallowed *)
	     ucham,   (* protection from chameleons *)
	     umaintarm, 
	     uMidas : Boolean; (* Everything You touch turns to gold *)
	     uhs : (NotHungry,hungry,weak,fainting);
	     ustr, ustrmax : byte;
	     udaminc (*damage increase*): byte;
	     uhp, uhpmax : Integer;
	     uac : byte;
	     ugold : unsigned;
	     uexp,
	     urexp,
	     ublind : unsigned;
	     uhunger : -1..MaxShort;
	     ustuck : thing;
           END;

     ScoreRecord = RECORD
		     points : unsigned;
		     UserName : name;
		     ExitStatus : EndStatus;
		     DungeonLevel : small;
		     whatkilled : Enemy;
		     ExperienceInGame : ExperienceType;
		     Reserved1,
		     Reserved2,
		     Reserved3,
		     Reserved4 : Integer; (* reserved for us *)
		   END;
	     
     RecordNumber = 1..NumberOfScoreRecords;
     (* Direct access file *)
     ScoreFile = FILE [ 1 .. 1 ] OF ARRAY [ RecordNumber ] OF ScoreRecord;
     HelpFileType = PACKED FILE OF Char;
     OneLevel = RECORD
		  levl : level;
		  xdnstair : xval; ydnstair:yval; (* stairs up ... *)
		  xupstair : xval; yupstair:yval; (* ...  and down *)
		  ThisIsAMaze :	Boolean;
		  moves	: unsigned;
		  NumberOfRats : byte; (*need not be exact number...*)
		  listof : ARRAY [ThingType] OF thing;
		END;	     

     LevelRange = 1..LastLevel;
     LevelsFileType = FILE [ LevelRange ] OF OneLevel;


VAR
     YourTopTen, SystemTopTen : ScoreFile;
     f : LevelsFileType;
     help : HelpFileType;
     RelativeCursor : Boolean value False;

(* Here starts the global area which is saved when a game is saved. *)
     StartOfAreaToSave : Integer;	(* Don't use this yourself! *)

     YourName : name;
     YourExperienceInZAP : ExperienceType;
     MakingLevel : Boolean;
     FreeList : thing;
     mon : ARRAY [MonsterSpecies] OF PerMonst;
     MonsterTable : ARRAY [mtabRow,mtabCol] OF MonsterSpecies;
     mlarge,		   (* These monsters appear to be large *)
     genocided,		   (* These monsters are Genocided *)
     MonstersKilledByYou,  (* You get points only from the first M *)
     MonstersWhichRegenerateFast : SET OF MonsterSpecies;
     multi : Integer;
     invent,uwep,uarm,uleft,uright : thing;
     flags : flag;
     wsdam : WeaponStrength; (*damage to small & medium creatures *)
     wldam : WeaponStrength; (*damage to large creatures *)
     u : you;
     SavedCommand : Char;
     curx : xval; cury : row; (* current cursor location on screen *)
     savx : xval;	       (* last current location on top line *)
     seehx, seelx : xval; seehy, seely : yval; (* where to see *)
     scrhx, scrlx : xval; scrhy, scrly : yval; (* where to update *)
     killer : missile..UnUsedKiller;
     TheWayItEnded : EndStatus;
     dlevel : LevelRange; (*current dungeon level nr*)
     DeepestLevelSaved : ShortCount;
     dx,dy:byte; 	   (* used by move *)

     TurningToStone : -MaxShort..0;	(* COCKATRICE *)
     YouHaveTheAmulet : Boolean;
     YouCanCarry: byte;
     YouAreSwimming : Boolean;
     YourActualStrength : Byte;
     YouCanStillSwim : unsigned;
     OpenTheWayDownToAmulet,
     OpenTheStairCaseUpToWorldOfGiants,
     OpenTheWayOut : 1..2;

     ScrollShuffle : ARRAY [ScrollType] OF small;
     PotionShuffle : ARRAY [PotionType] OF small;
     WandShuffle : ARRAY [WandType] OF small;
     RingShuffle : ARRAY [RingType] OF small;
     scrcal : ARRAY [ScrollType] OF name;
     potcal : ARRAY [PotionType] OF name;
     wandcal : ARRAY [WandType] OF name;
     ringcal : ARRAY [RingType] OF name;
     KnownScrolls : SET OF ScrollType;
     KnownPotions : SET OF PotionType;
     KnownWands : SET OF WandType;
     KnownRings : SET OF RingType;
     KnownFrobozz : SET OF FrobozzStuff;
     YouHaveNotFoundThemAll : Boolean;
     AllObjects : ObjectSet;
     Reserved1, Reserved2, Reserved3, Reserved4, Reserved5,
     Reserved6, Reserved7, Reserved8, Reserved9, Reserved10,
     Reserved11 : Integer;	(* To maintain save file compatibility *)

     EndOfAreaToSave : Integer; 	(* Don't use this yourself! *)

(* This is the end of the area that is saved when saving game. *)

     ScoreBinding : BindingType;

PROCEDURE SaveHack(VAR LevelsFile : LevelsFileType;
		   VAR FirstGlob, LastGlob : Integer); C_language;
PROCEDURE HackRestore(VAR LevelsFile : LevelsFileType;
		      VAR FirstGlob, LastGlob : Integer); C_language;
PROCEDURE DeleteLevelsFile(VAR  LevelsFile : LevelsFileType); C_language;

FUNCTION  DoRestore : Boolean; C_language;


PROCEDURE RanIni; C_language;	(* Initializes random number generator *)
FUNCTION  Rand : Integer; C_language;

TYPE
   FILEptr = ^char; (* FILE pointer in C, but all pointers are of same size *)

(*  _p_getfile is in the run time system: Returns the C
 *  FILE pointer to the file when given an open
 *  pascal file.
 *)
FUNCTION  _p_getfile (VAR f : ScoreFile): FILEptr; C_language;

(*
 *  ZAP specific function: lock the score file with exclusive access
 *  when given FILE pointer to an open file.
 *
 *  Return 0 if the locking succeeded, -1 otherwise.
 *)
FUNCTION LockFile (f : FILEptr) : Integer; C_language;

(*
 *  Suspend the process if the operating system supports it.
 *  Terminal modes are handled by the caller.
 *)
PROCEDURE Hack_Suspend; C_Language;

(* changed for gpc *)

PROCEDURE SetLevel(Level : unsigned);
begin
   { set process name to indicate the level of zap you are at
     so that others can see it with 'ps' command
   }
end; { SetLevel }

FUNCTION  tcInitTty(HowInit : Integer): Boolean;
begin
   tcInitTty := c_init (HowInit) = 0;

   if HowInit then
      begin
	 c_tty_mode (1);	{ RAW mode }
	 c_tty_mode (5);	{ NO ECHO mode }
	 c_tty_mode (7);	{ Save this mode, restored with c_tty_mode (8); }
      end;
end; { tcInitTty }

PROCEDURE Bug (msg : tString) ;
begin
   IF tcInitTty (0) THEN
      (* Nothing *);

   Writeln ('Procedure BUG called with message "',msg,'"');
   Writeln ('Halting ZAP execution');

   halt;
end;

PROCEDURE GetOwnScoreFile(Typ   : tString;
			  VAR f : ScoreFile);
label 1;
var
   b : bindingtype;
   retry : boolean value false;
begin
 1: 
   b := binding (f);
   if b.bound then
      unbind (f);
   scorebinding.name := typ;
   bind (f, scorebinding);
   scorebinding := binding (f);
   if not scorebinding.bound then
      bug('Could not bind scorefile');

   if not scorebinding.existing then
     begin
	rewrite (f);
	if retry then
	   bug ('Could not create empty scorefile')
	else
	  begin
	     retry := true;
	     goto 1;
	  end;
     end;
end; { GetOwnScoreFile }

FUNCTION  GetJfnForSystemScores(Typ : tString;
				VAR f : ScoreFile) : Boolean;
var
   sbinding    : BindingType;
   b	       : bindingtype;

begin
   b := binding (f);
    if b.bound then
      unbind (f);
   sbinding.name := typ;
   bind (f,sbinding);
   sbinding := binding (f);
   GetJfnForSystemScores := (sbinding.bound and sbinding.existing);
end; { GetJfnForSystemScores }

FUNCTION  GetJfnForHelpFile(Typ : tString;
			     VAR f : HelpFileType) : Boolean;
var
   sbinding    : BindingType;
   b	       : bindingtype;

begin
   b := binding (f);
    if b.bound then
      unbind (f);
   sbinding.name := typ;
   bind (f,sbinding);
   sbinding := binding (f);
   GetJfnForHelpFile := sbinding.bound;
end; { GetJfnForHelpFile }

PROCEDURE OpenWithSingleAccess(VAR f : ScoreFile);
   
begin
   c_tty_mode (0); { Restore original mode }
   SeekUpdate (f, 1);

   if LockFile (_p_getfile (f)) then
      bug ('Sorry, can''t lock score file from others');

   c_tty_mode (8); { Restore saved mode }
end; { OpenWithSingleAccess }

FUNCTION tcGetTerminalType: Integer;
begin
   tcGetTerminalType := 1;
end; { tcGetTerminalType }

PROCEDURE tcStop;
begin
   c_tty_mode (0);
   hack_suspend;
   c_tty_mode (8);
end; { tcStop }

PROCEDURE tcRdCh(VAR ch:Char);
begin
   ch := c_getch;
end; { tcRdCh }

PROCEDURE tcWrCh(ch:Char);
begin
   Output^ := Ch;
   put(Output);
end; { tcWrCh }

PROCEDURE tcGetString(Prmpt : tString;
	 	      VAR S : tString);
var
   i  : integer;
   ch : char;
   a  :	packed array [ 1 .. string_length ] of char;
   doit	:  boolean;

begin
   write (Prmpt);
   c_tty_mode (0);
   readln (s);
   c_tty_mode (8); { Restore previous mode }

(* !!!!!!!!!!!!!!!!!! COMMENTED !!!!!!!!!!
   doit := true;
   i    := 1;
   while doit do
     begin
	tcRdCh (Ch);

	if (Ch = Chr(LF)) or (i > string_length) then
	   doit := false
	else
	  begin
	     tcWrCh (Ch);
	     a[ i ] := Ch;
	     i := succ(i);
	  end;
     end;

   for i := i to string_length do
      a [ i ] := ' ';

   s := trim (a);
 !!!!!!!!!!!!!!!!!! END COMMENTED!!!!!!!!!!!! *)

end; { tcGetString }

PROCEDURE tcWrString(Str: tString);
begin
   write (Str);
end; { tcWrString }

PROCEDURE tcOutputFlush;
begin
   { Should not be necessary; lazy I/O takes care of this }
end; { tcOutputFlush }

PROCEDURE tcPageErase;
begin
   c_clearscreen;
end; { tcPageErase }

PROCEDURE tcHome;
begin
   c_home;
end; { tcHome }

PROCEDURE tcGotoxy(x:xval;y:row);
begin
   c_gotoxy (x,y);
end; { tcGotoxy }

PROCEDURE tcLineErase(AtLeastUpTo:xval);
begin
   c_cleartoeol;
end; { tcLineErase }

PROCEDURE tcUp(Count: Integer);
begin
   c_up (count);
end; { tcUp }

PROCEDURE tcDown(Count : Integer);
begin
   c_down (count);
end; { tcDown }

PROCEDURE tcRight(Count : Integer);
begin
   c_right (count);
end; { tcRight }

PROCEDURE tcLeft(Count : Integer);
begin
   c_left (count);
end; { tcLeft }

PROCEDURE ClearScreen;
  BEGIN
    tcPageErase;
    curx := xmin;
    cury := TopLine
  END;

PROCEDURE CursorHome;
  BEGIN
    tcHome;
    curx := xmin;
    cury := TopLine
  END;

PROCEDURE Curs(x:xval; y:row);

  PROCEDURE NoCm(x:xval;y:row);

    BEGIN  (* NoCm *)
      IF  curx < x  THEN
	tcRight(x-curx)
      ELSE IF curx <> x THEN
	tcLeft(curx-x);
      IF  cury > y  THEN
	tcUp(cury-y)
      ELSE IF cury <> y THEN
	tcDown(y-cury);
      curx := x;
      cury := y;
    END  (* NoCm *);

  BEGIN  (* Curs *)
    IF  (x=curx) AND (y=cury)  THEN
      (* nothing, we are in the right location *)
    ELSE
      (* I can't understand the resoning in using relative cursor control
	 only if current vertical position is 'under' the new one.  Also,
	 the whole reasoning in deciding when to do it relatively,  seems
	 overly  complex.   What is needed  really is  to know if the new
	 location can be reached with (let's say) five moves. That's only
	 a guess, anyway, but let's  try it!  The original  IF-expression
	 is saved here anyhow:
		IF  (cury>=y) AND (cury-y<=3) AND (Abs(curx-x)<=3) THEN *)
      IF RelativeCursor AND (Abs(Cury-Y) + Abs(Curx-X) <= 5) THEN
	nocm(x,y)  (* Use relative cursor control. *)
      ELSE
	(* The same reasoning is behind commenting the  old IF-expression
	   and making a new one:
		IF (x<=3) AND (cury>=y) AND (cury-y<=3) THEN	*)
	IF RelativeCursor AND (X + Abs(Cury-Y) <= 4) THEN
	  BEGIN  (* Go to start of line, then use relat. movement. *)
	    tcWrCh(Chr(CR));
	    curx := xmin;
	    nocm(x,y)
	  END  (* go to start of line, then use relat. movement *)
	ELSE
	  BEGIN  (* Use absolute cursor control. *)
	    tcGotoxy(x,y);
	    curx := x;
	    cury := y;
	  END;
  END  (* Curs *);

PROCEDURE NewLine;

  VAR
    CrLf : PACKED ARRAY [1..2] OF Char;

  BEGIN  (* NewLine *)
    CrLf[1]:=Chr(CR);
    CrLf[2]:=Chr(LF);
    tcWrString(CrLf);
    curx := xmin;
    cury := cury + 1;
  END  (* NewLine *);

PROCEDURE out(s: tString);

  BEGIN
    tcWrString(s);
    curx := curx + length (s);
    IF cury = TopLine THEN
      savx := curx;
  END;

PROCEDURE out0(s : tString);
  (* outputs s with trailing blanks stripped off; Fix: always outputs
     at least one chraco even if it is BLANK !! *)

  BEGIN  (* Out0 *)
    s := trim (s);
    if length (s) = 0 then
       s := ' ';
    tcWrString(s);
    curx := curx + length(s);
    IF cury = TopLine THEN
      savx := curx;
  END  (* Out0 *);

PROCEDURE OutCh(ch:Char);

  BEGIN
    tcWrCh(ch);
    curx := curx + 1;
    IF cury = TopLine THEN
      savx := curx;
  END;

FUNCTION  GetRet(EscAllowed : Boolean) : Boolean;

  VAR
    ch:Char;

  BEGIN
    OutCh(Chr(CR));
    IF  EscAllowed  THEN
      out('Hit space to continue (ESC to cancel): ')
    ELSE
      out('Hit space to continue: ');
    REPEAT
      tcRdCh(ch)
    UNTIL (ch = blank) OR ((ch = Chr(ESC)) AND EscAllowed);
    GetRet := ch = blank
  END  (* GetRet *);

FUNCTION  Rnd(UpperBound : UnSigned) : Positive;
  (* Returns a random integer in the range 1..UpperBound *)
  BEGIN
    IF UpperBound > 0 THEN 
      Rnd := Rand MOD UpperBound + 1
    ELSE
      Rnd := 1;
  END;

FUNCTION  Rnd0(UpperBound : UnSigned) : Unsigned;
  (* Returns a random integer in the range 0..UpperBound-1 *)
  BEGIN
    IF UpperBound > 0 THEN 
      Rnd0 := Rand MOD UpperBound
    ELSE
      Rnd0 := 1;
  END;

FUNCTION  Rn1(X : UnSigned; Y : Integer) : Integer;
  (* Returns a random integer in the range y..y+x-1 *)
  BEGIN
    IF X > 0 THEN 
      Rn1 := Rand MOD x + y
    ELSE
      Rn1 := y;
  END;

FUNCTION  Rn2(N : UnSigned) : Boolean;
  (* Returns True with a probability of 1/n *)
  BEGIN
    IF N > 0 THEN 
      Rn2 := Rand MOD n = 0
    ELSE
      Rn2 := True;
  END;

FUNCTION  D(N : Unsigned; X : Unsigned) : Unsigned;

  VAR
    tmp : Unsigned;

  BEGIN
    tmp := 0;
    IF x > 0 THEN
      WHILE n > 0 DO
	BEGIN
	  tmp := tmp + rand MOD x + 1;
	  n := n - 1;
	END;
    d := tmp;
  END;

PROCEDURE ShuffleCallableObjects;

  VAR
    i, j,
    temp : Small;

  BEGIN  (* ShuffleCallableObjects *)
    (* The last objects ( ***Num - 1 ) are not shuffled, because *)
    (* they are magical Frobozz things *)
    (*First shuffle scrolls.*)
    (*Form identical permutation.*)
    FOR  i := 0  TO  ScrNum - 1  DO
      ScrollShuffle[ScrollType(i)] := i;
    (* Randomize the permutation by swapping every element
       with another (sequentially). *)
    FOR i := 0 TO scrnum-2 DO
      BEGIN
	j := rnd0(ScrNum-2);
	temp := ScrollShuffle[ScrollType(i)];
	ScrollShuffle[ScrollType(i)] := ScrollShuffle[ScrollType(j)];
	ScrollShuffle[ScrollType(j)] := temp;
      END;			 
    (* Do the same for other classes of callable objects. *)
    (* Potions: *)
    FOR  i := 0  TO  PotNum - 1  DO
      PotionShuffle[PotionType(i)] := i;
    FOR i := 0 TO PotNum-2 DO
      BEGIN
	j := rnd0(PotNum-2);
	temp := PotionShuffle[PotionType(i)];
	PotionShuffle[PotionType(i)] := PotionShuffle[PotionType(j)];
	PotionShuffle[PotionType(j)] := temp;
      END;			 
    (* Wands: *)
    FOR i := 0 TO WandNum - 1 DO
      WandShuffle[WandType(i)] := i;
    FOR i := 0 TO WandNum-2 DO
      BEGIN
	j := rnd0(WandNum-2);
	temp := WandShuffle[WandType(i)];
	WandShuffle[WandType(i)] := WandShuffle[WandType(j)];
	WandShuffle[WandType(j)] := temp;
      END;			 
    (* Rings: *)
    FOR i := 0 TO RingNum - 1 DO
      RingShuffle[RingType(i)] := i;
    FOR i := 0 TO RingNum-2 DO
      BEGIN
	j := rnd0(RingNum-2);
	temp := RingShuffle[RingType(i)];
	RingShuffle[RingType(i)] := RingShuffle[RingType(j)];
	RingShuffle[RingType(j)] := temp;
      END;			 
  END  (* ShuffleCallableObjects *);

FUNCTION  Pow(Num : Small) : Positive;
  (* Returns 2 to the power num *)
  VAR
    tmp : Positive;

  BEGIN
    tmp := 1;
    WHILE num > 0 DO
      BEGIN 
        IF tmp <= MaxInt DIV 2 THEN
	  tmp := 2*tmp
	ELSE
	  tmp := MaxInt;
	num := num - 1;
      END;
    pow := tmp;
  END;

FUNCTION  Sgn(Num : Integer) : SignRange;

  BEGIN
    IF num > 0 THEN
      sgn := +1
    ELSE
      IF num < 0 THEN
	sgn := -1
      ELSE sgn := 0;
  END;

FUNCTION Lowc(Ch : Char) : Char;

  BEGIN
    IF (ch >= 'A') AND (ch <= 'Z') THEN
      lowc := Chr(Ord(ch)+Ord('a')-Ord('A'))
    ELSE  lowc := ch;
  END;

PROCEDURE Create(VAR Th : Thing; Wh : ThingType);

  BEGIN  (* Create *)
    IF FreeList = NIL THEN
      New(th)
    ELSE
      BEGIN
	th := FreeList;
	FreeList := FreeList^.next;
      END;
    th^.what := wh;
  END  (* Create *);

PROCEDURE remove(elem:thing; VAR list:thing);

  VAR
    t : Thing;

  BEGIN  (* Remove *)
    IF elem = list THEN
      list := elem^.next
    ELSE
      BEGIN
	t := list;
	WHILE t^.next <> elem DO t := t^.next;
	t^.next := elem^.next;
      END;
    elem^.next := NIL;  (* Security first of all... *)
  END  (* Remove *);

PROCEDURE free(elem:thing);

  BEGIN
    elem^.next := FreeList;
    FreeList := elem;
  END;

PROCEDURE DeleteFirstElement(VAR List : Thing);

  VAR
    temp : Thing;

  BEGIN  (* DeleteFirstElement *)
    temp := list^.next;
    list^.next := FreeList;
    FreeList := list;
    list := temp;
  END  (* DeleteFirstElement *);

PROCEDURE Delete(VAR  Elem : Thing; VAR List : Thing);
  (* remove from list and dispose for reuse *)

  VAR
    t : thing;

  BEGIN (* Delete *)
    IF  elem = list  THEN
      DeleteFirstElement(list)
    ELSE
      BEGIN
	t := list;
	WHILE  (t^.next <> elem) AND (t^.next <> NIL) DO
	  t := t^.next;
	(* This guarding against nonexistent monster in the list of monsters
	   is just a temporary solution to the problem of genocided eaters.
	   Few weeks later:  the gurad isn't probably needed anymore! *)
	IF  t^.next <> NIL  THEN
	  BEGIN
	    t^.next := elem^.next;
	    elem^.next := FreeList;
	    FreeList := elem;
	  END  (* IF  t^.next <> NIL *);
      END;
    elem := NIL; (* To guard against dangling pointers... *)
  END  (* Delete *);

PROCEDURE AddToList(elem:thing; VAR list:thing);

  BEGIN
    elem^.next := list;
    list := elem;
  END;

PROCEDURE RedrawScreen; FORWARD;
PROCEDURE GoodBye(VAR Mtmp : Thing); FORWARD;
PROCEDURE AggravateThem; FORWARD;
PROCEDURE Rloc(Mtmp : Thing); FORWARD;

PROCEDURE OpenLevelsFile(VAR  LvlFil : LevelsFileType);

  BEGIN
    rewrite(LvlFil);
(*
    DefineSize (LvlFil, 0);
*)
    SeekUpdate (LvlFil, 1);
  END;

PROCEDURE SaveLev;

  VAR
   level       : OneLevel;

  BEGIN  (* SaveLev *)
    level := f^;
    IF dlevel > DeepestLevelSaved THEN
      BEGIN
	DeepestLevelSaved := dlevel;
	DefineSize(f,DeepestLevelSaved);
      END;
    SeekUpdate (f, dlevel);
    f^ := level;
    Update (f);
    f^.moves := level.moves;
  END  (* SaveLev *);

(* Following procedure is here because GetLev uses it. *)
PROCEDURE ChangeMonstersHitPoints(Monster : Thing; Change : Integer);

  VAR
    sum : Integer;

  BEGIN  (* ChangeMonstersHitPoints *)
    (* By doing all changing of monsters' hit points thru this
       routine, we guarantee that the "mhp" field never gets
       negative (technical reason; we don't like signed fields in
       packed records as they probably cause waste of storage),
       and that a monster's hit points never get beyond its
       hit point maximum, "OrigHp", when it regenerates. *)
    WITH  monster^  DO
      BEGIN
	sum := mhp + change;
	IF  sum <= 0  THEN
	  mhp := 0 (*monster will die*)
	ELSE
	  IF  sum > OrigHp  THEN
	    mhp := OrigHp
	  ELSE
	    BEGIN
	      mhp := sum;
	      IF (mstat = flee) THEN
		IF rn2(3) THEN 
		  IF mhp > OrigHp DIV 2 THEN
		    mstat := normalState;
            END;
     END;
  END  (* ChangeMonstersHitPoints *);

PROCEDURE GetLev;

  VAR
    CurrentMoves,
    MoveDiff	 : Unsigned;
    temp,
    Monster	 : Thing;

  BEGIN
    CurrentMoves := f^.moves;
    SeekUpdate (f, dlevel);
    MoveDiff := CurrentMoves - f^.moves;
    monster := f^.listof[monsters];
    WHILE  monster <> NIL  DO
      WITH  monster^  DO
	BEGIN
	  temp := next;
	  IF  NOT (species IN genocided)  THEN
	    IF  species IN MonstersWhichRegenerateFast  THEN
	      ChangeMonstersHitPoints(monster,+MoveDiff)
	    ELSE
	      ChangeMonstersHitPoints(monster,+MoveDiff DIV 15)
	  ELSE
	    GoodBye(monster);
	  monster := temp;
	END;
    f^.moves := CurrentMoves;
    u.ux := f^.xupstair; u.uy := f^.yupstair;
  END;

PROCEDURE RestoreTheGame;

  VAR
    it    : thing;

  BEGIN
    ClearScreen;
    tcOutputFlush;
    SaveLev;
    (*	HackRestore restores the global data area and heap & heappointer
	from the users savefile.  It also checks that the length of  the
	global data area hasn't changed  (and does some other checks  on
	the user savefile and the saved LEVELS file).  If everything  in
	the savefile is all right  for restoring, the saved LEVELS  file
	is renamed to the the name of the current physical file which is
	connected with the internal  LEVELS file.  Doing SaveLev  before
	HackRestore makes  sure the  internal LEVELS  file is  not  only
	buffered anymore but really IS  connected with an physical  file
	(a JFN in Tops-20).   SaveLev also has the  effect that, if  the
	restoring fails (for some reason?!?!), we can continue as if  it
	succeeded as HackRestore leaves  the internal file untouched  in
	that case.  *)
    HackRestore(F, StartOfAreaToSave, EndOfAreaToSave);
    (*	HackRestore returns only if restore succeeded or the failure was
	detected early enough and  the global data  area (nor heap)  was
	not restored.  In some cases the failure can't be noticed  early
	enough in which case program halts irrevocably. *)
    NewLine;
    SetLevel(dlevel);
    IF GetRet(False)  THEN  (* Nothing *);
    SeekUpdate (F, dlevel);
    YouHaveTheAmulet := False;
    it := Invent;
    WHILE  it <> NIL  DO
      BEGIN
	IF it^.class = Amulet THEN
	  YouHaveTheAmulet := True;
	it := it^.next;
      END;
  END  (* RestoreTheGame *);

FUNCTION ThingAt(x:xval; y:yval; typ:ThingType):thing;

  VAR
    Ptr : Thing;

  BEGIN  (* ThingAt *)
    ptr := f^.listof[typ];
    ThingAt := NIL;
    WHILE ptr <> NIL DO
      WITH ptr^ DO
	IF (locx=x) AND (locy=y) THEN
	  BEGIN
	    ThingAt := ptr;
	    Ptr     := NIL
	  END
	ELSE ptr := next;
  END  (* ThingAt *);


PROCEDURE at(x:xval; y:yval; ch:Char);

  BEGIN  (* At *)
    curs(x,y+2); (* +2, since two top lines have special usage *)
    tcWrCh(ch);
    curx := curx + 1;
  END  (* At *);

PROCEDURE on(x:xval;y:yval);

  (* expands the area which is searched in the next NCRT or DOCRT
     to include the points X & Y. Also marks the location at levl
     array to be NEW *)

  BEGIN  (* On *)
    f^.levl[x,y].new := True;
    IF flags.dscr THEN
      BEGIN
	IF x < scrlx THEN
	  scrlx := x
	ELSE
	  IF x > scrhx THEN
	    scrhx := x;
	IF y < scrly THEN
	  scrly := y
	ELSE
	  IF y > scrhy THEN
	    scrhy := y;
      END
    ELSE
      BEGIN
	flags.dscr := True;
	scrlx := x; scrhx := x;
	scrly := y; scrhy := y;
      END;
  END  (* On *);

PROCEDURE atl(x:xval; y:yval; ch:Char);

  (* At Location: Put the symbol to levl, then expand the
     print region with ON *)

  BEGIN
    f^.levl[x,y].ScrSym := ch;
    on(x,y)
  END  (* atl *);

PROCEDURE pMon(Monster : thing);

  (* Put monster symbol at levl with ATL *)

  VAR
    Sym : Char;

  FUNCTION  GenerateMimicSymbol : Char;

    CONST
      NumberOfSymbolsSheCanImitate = 32;

    VAR
      c : Char;		(* Holdover from the C version... *)

    BEGIN
      CASE Rnd(NumberOfSymbolsSheCanImitate) OF
	 1 : c := '!';
	 2 : c := '$';
	 3 : c := '%';
	 4 : c := '/';
	 5 : c := ')';
	 6 : c := '=';
	 7 : c := '?';
	 8 : c := '.';
	 9 : c := '~';
	10 : c := '>';
	11 : c := '<';
	12 : c := '+';
	13 : c := '|';
	14 : c := '-';
	15 : c := '#';
	16 : c := 'M';	(* Try to be yourself *)
	17 : c := '*';
	18 : c := '@';
	19 : c := '&';
	20 : c := '''';
	21 : c := '`';
	22 : c := '^';
	23 : c := ':';
	24 : c := ';';
	25 : c := ']';
	26 : c := '[';
	27 : c := '{';
	28 : c := '}';
	29 : c := '\';
	30 : c := '"';
	31 : c := '(';
	32 : c := ',';
      END;
      GenerateMimicSymbol := c;
    END;

  BEGIN
    WITH  monster^  DO
      BEGIN
        Sym := mon[species].mlet;
	IF (Species = Mimic) AND NOT u.ucham THEN
	  Sym := GenerateMimicSymbol;
	IF  NOT invis OR u.ucinvis THEN
	  atl(locx,locy,Sym);
      END;
  END  (* pMon *);


PROCEDURE prl(x:xval;y:yval);

  VAR
    mtmp : thing;

  (* Print Location: If it sees a monster in the given location, it puts
     its symbol to the levl table with ATL. If no visible monster in that
     location: if you have seen the location do nothing, else expand the
     print region *)

   (* removed the function MonsterVisible, because 
      functions considered harmful... JV *)

  BEGIN  (* Prl *)
    WITH  f^.levl[x,y]  DO
      BEGIN
	CanSee := True;
	IF  (typ = empty) OR ( ((typ = sdoor) OR (typ = wall))
			       AND (f^.levl[u.ux,u.uy].typ = corr))  THEN
	  (* Return *)
	ELSE
	  BEGIN
	    mtmp := ThingAt(x,y,monsters);
	    IF  mtmp <> NIL  THEN
	      BEGIN
		IF  (NOT mtmp^.invis) OR (u.ucinvis)  THEN
		  pmon(mtmp)
		ELSE
		  IF NOT seen THEN on(x,y);
	      END
	    ELSE
	      IF NOT seen THEN on(x,y)
	  END
      END  (* WITH  f^.levl[x,y] *)
  END  (* Prl *);

PROCEDURE pru;

  (* Print You *)

  BEGIN
    IF  u.ublind = 0  THEN
      f^.levl[u.ux,u.uy].CanSee := True;
    IF  u.uinvis > 0  THEN
      prl(u.ux,u.uy)
    ELSE
      atl(u.ux,u.uy,YourSymbol);
  END  (* pru *);


PROCEDURE news1(x:xval; y:yval);

  (* See if there is something (in order) at this location:
     Objects
      Golds
       Traps
     If none of these present put the symbol described in the TYP field
     to levl. (Check also staircases... )
     If there is water, you can't see anything in there. *)
     
  VAR
    ThisThing : Thing;

  BEGIN  (* News1 *)
    WITH f^.levl[x,y] DO
      BEGIN
	Seen := False;
	IF room = WaterRoom THEN  (* Show only the staircases *)
	  IF  (x=f^.xupstair) AND (y=f^.yupstair)  THEN
	    ScrSym := upwards
	  ELSE
	    IF  (x=f^.xdnstair) AND (y=f^.ydnstair)  THEN
	      ScrSym := downwards
	    ELSE
	      ScrSym := '~'
	ELSE			  (* show the stuff in there *)
	  BEGIN
	    ThisThing := ThingAt(x,y,objects);
	    IF ThisThing <> NIL THEN
	      ScrSym := ThisThing^.olet
	    ELSE
	      BEGIN  (* There's something which isn't an object *)
		ThisThing := ThingAt(x,y,golds);
		IF ThisThing <> NIL THEN
		  ScrSym := '$'
		ELSE
		  BEGIN  (* Something but not gold. *)
		    ThisThing := ThingAt(x,y,traps);
		    IF ThisThing <> NIL THEN
		      WITH ThisThing^ DO
			IF gflag = MimicTrap THEN
			  ScrSym := '+'
			ELSE
			  IF seen THEN
			    ScrSym := '^'
			  ELSE
			    ScrSym := '.'
		    ELSE  (* Nothing there *)
		      CASE typ OF
			empty : ScrSym := '.';
			wall,
			sdoor : IF (f^.levl[x,y-1].typ = wall) AND
				   (f^.levl[x,y+1].typ = wall) THEN
				  ScrSym := '|'
				ELSE
				  ScrSym := '-';
			door  : ScrSym := '+';
			corr  : ScrSym := '#';
			RoomLocation :
			  IF  (x=f^.xupstair) AND (y=f^.yupstair)  THEN
			    ScrSym := upwards
			  ELSE
			    IF  (x=f^.xdnstair) AND (y=f^.ydnstair)  THEN
			      ScrSym := downwards
			    ELSE
			      IF  lit OR CanSee OR (u.ublind > 0)  THEN
				ScrSym := '.'
			      ELSE
				ScrSym := blank;
		      END  (* CASE typ OF *)
		  END  (* Something but not gold. *)
	    END  (* There's something which isn't an object *)
	END  (* show the stuff in there *)
     END (* WITH *)
  END  (* News1 *);

PROCEDURE NewSym(x:xval; y:yval);

  (* This is called when someone (== either you or a monster)
     leave a location. Then the NEWS1 routine puts the proper
     symbol to levl table, and the ON routine expands the print
     area *)

  (* Removed the unnecessary call to ATL, which used to put
     the correct symbol back to its OWN place at levl table *)

  BEGIN
    news1(x,y);
    on(x,y);
  END  (* NewSym *);

PROCEDURE SetSeeLimits;

  BEGIN
    seelx := u.ux;
    WHILE  f^.levl[ Pred(seelx),u.uy ].lit  DO  seelx := Pred(seelx);
    seehx := u.ux;
    WHILE  f^.levl[ Succ(seehx),u.uy ].lit  DO  seehx := Succ(seehx);
    seely := u.uy;
    WHILE  f^.levl[ u.ux,Pred(seely) ].lit  DO  seely := Pred(seely);
    seehy := u.uy;
    WHILE  f^.levl[ u.ux,Succ(seehy) ].lit  DO  seehy := Succ(seehy);
  END  (* SetSeeLimits *);

PROCEDURE SetSee;

  (* Display the area you can see from your current location *)

  PROCEDURE PrArea(lx,hx : xval; ly,hy : yval);

    VAR
      x, y : Small;

    BEGIN
      FOR  y := ly  TO  hy  DO    FOR  x := lx  TO  hx  DO
	IF  (x = u.ux) AND (y = u.uy)  THEN
	  pru
	ELSE
	  prl(x,y)
    END  (* PrArea *);

  BEGIN  (* SetSee *)
    IF u.ublind > 0 THEN
      pru
    ELSE
      IF  f^.levl[ u.ux,u.uy ].lit  THEN
	BEGIN  (* Can see whole 'room' *)
	  SetSeeLimits;
	  PrArea(seelx,seehx,seely,seehy)
	END  (* Can see whole 'room' *)
      ELSE
	BEGIN  (* Can see only immediate surroundings. *)
	  seehx := 0;
	  PrArea(Pred(u.ux),Succ(u.ux),Pred(u.uy),Succ(u.uy))
	END  (* Can see only immediate surroundings. *);
  END  (* SetSee *);

PROCEDURE Unsee(Mode : Boolean);

  (* MODE is ALWAYS True  ????? *)

  (* Clear current surroundings, meaning that the luser's symbol
     is removed from the current location. *)

  VAR
    x, y : Small;

  BEGIN  (* UnSee *)
    IF seehx <> 0 THEN
      BEGIN
	FOR  x := seelx  TO  seehx  DO    FOR  y := seely  TO  seehy  DO
	  WITH  f^.levl[x,y]  DO
	    BEGIN
	      CanSee := False;
	      IF  (ScrSym = YourSymbol) AND mode  THEN
		NewSym(x,y)
	    END;
	seehx := 0
      END  (* IF seehx <> 0 *)
    ELSE	(* Saw only immediate surroundings. *)
      FOR  x := Pred(u.ux)  TO  Succ(u.ux)  DO
	FOR  y := Pred(u.uy)  TO  Succ(u.uy)  DO
	  WITH  f^.levl[x,y]  DO
	    BEGIN
	      CanSee := False;
	      IF  (ScrSym = YourSymbol) AND mode THEN
		NewSym(x,y)
	      ELSE
		IF  (ScrSym = '.') OR (ScrSym = '~')  THEN
		  IF  mode  THEN
		    BEGIN
		      ScrSym := blank;
		      on(x,y)
		    END  (* IF  mode *)
		  ELSE
		    seen := False
	    END  (* WITH  f^.levl[x,y] *)
  END  (* UnSee *);

PROCEDURE SeeOn;

  (* Called only when you goto new level *)

  PROCEDURE SeeArea(lx,hx : xval; ly,hy : yval);

    VAR
      x, y : Small;
      mtmp : Thing;

    BEGIN
      FOR  y := ly  TO  hy  DO    FOR  x := lx  TO  hx  DO
	BEGIN
	  mtmp := ThingAt(x,y,monsters);
	  WITH  f^.levl[x,y]  DO
	    BEGIN
	      IF  mtmp <> NIL  THEN
		pmon(mtmp)
	      ELSE
		IF  (u.uinvis=0) AND (x=u.ux) AND (y=u.uy)  THEN
		  ScrSym := YourSymbol;
	      seen := True;
	      CanSee := True;
	    END  (* WITH f^.levl[x,y] *);
	END  (* FOR - FOR *);
    END  (* SeeArea *);

  BEGIN  (* SeeOn *)
    IF  u.ublind > 0  THEN
      pru
    ELSE
      IF  f^.levl[u.ux,u.uy].lit  THEN
	BEGIN  (* Sees whole 'room'. *)
	  SetSeeLimits;
	  SeeArea(seelx,seehx,seely,seehy);
	END  (* Sees whole 'room'. *)
      ELSE
	BEGIN  (* Sees only those 'around'. *)
	  seehx := 0;
	  SeeArea(Pred(u.ux),Succ(u.ux),Pred(u.uy),Succ(u.uy))
	END  (* Sees only those 'around'. *)
  END  (* SeeOn *);

PROCEDURE SeeOff(ChangingLevel : Boolean);

  (* When changing level, the parameter is TRUE,
     when you become blind, it is FALSE *)

  VAR
    x : xval;
    y : yval;

  BEGIN  (* SeeOff *)
    IF seehx <> 0 THEN
      BEGIN
	FOR  x := seelx  TO  seehx  DO    FOR  y := seely  TO  seehy  DO
	  WITH  f^.levl[x,y]  DO
	    BEGIN
	      CanSee := False;
	      IF  (ScrSym = YourSymbol) AND ChangingLevel  THEN
		news1(x,y);
	    END;
	seehx := 0;
      END
    ELSE  (* We saw only immediate surroundings, i.e. dark room or Corr. *)
      FOR  y := Pred(u.uy)  TO  Succ(u.uy)  DO
	FOR  x := Pred(u.ux)  TO  Succ(u.ux)  DO
	  WITH  f^.levl[x,y]  DO
	    BEGIN
	      CanSee := False;
	      IF  (ScrSym = YourSymbol) AND ChangingLevel  THEN
		BEGIN
		  news1(x,y);   (* Put the correct symbol to ScrSym *)
		  Seen := True; (* For some odd reason, we have seen it *)
		END
	      ELSE
		IF  ((ScrSym = '.') OR (ScrSym = '~')) OR NOT ChangingLevel
		THEN
		  seen := False;
	    END
  END  (* SeeOff *);

PROCEDURE draw;

  BEGIN
    ClearScreen;
    savx := xmin;
    SetSee;
    flags.botl := True;
  END;

PROCEDURE nscr;

  VAR
    x:xval;
    y:yval;

  BEGIN  (* Nscr *)
    IF  NOT u.uswallow  THEN
      BEGIN
	FOR  y := scrly  TO  scrhy  DO    FOR  x := scrlx  TO  scrhx  DO
	  WITH  f^.levl[x,y]  DO
	    IF  new  THEN
	      BEGIN
		new := False;
		at(x,y,ScrSym);
		IF  ScrSym = blank  THEN
		  BEGIN
		    CanSee:=False;
		    seen:=False;
		    IF room = WaterRoom THEN
		      ScrSym := '~'
		    ELSE
		      ScrSym := '.'
		  END
		ELSE 
		  seen := True;
	      END  (* IF  new *);
	IF  u.uinvis = 0  THEN
	  curs(u.ux,u.uy+2); (*added*)
	flags.dscr := False;
	scrhx := 0; scrhy := 0;
	scrlx := xmax; scrly := ymax
      END  (* IF  NOT u.uswallow *)
  END  (* Nscr *);

PROCEDURE WriteNumber(n:Integer; width:byte);
(* Write the value of n to a field of a least Abs(width) chars long.
   If width > 0, adjust right, if width < 0, adjust left, filling
   with blanks. For width > 0, WriteNumber(n,width) means roughly
   Write(Output,n:width). *)

  CONST
    base = 10;

  VAR
    i,
    CharsWritten : small;
    negative	 : Boolean;

  PROCEDURE WriteOut(n:Integer);

    VAR
      i : Small;

    BEGIN  (* WriteOut *)
      CharsWritten := CharsWritten + 1;
      IF n >= base THEN
	WriteOut(n DIV base)
      ELSE
	IF  CharsWritten < width  THEN	(* left fill *)
	  BEGIN
	    IF  negative  THEN  tcWrCh('-');
	    FOR  i := 1  TO  width - CharsWritten  DO  tcWrCh(blank);
	    CharsWritten := width;
	  END;
      tcWrCh(Chr(n MOD base + Ord('0')));
    END  (* WriteOut *);

  BEGIN  (* WriteNumber *)
    IF  n < 0  THEN
      BEGIN
	negative := True;
	IF  width <= 0  THEN  tcWrCh('-');
	CharsWritten := 1;
	n := -n;
      END
    ELSE
      BEGIN
	negative := False;
	CharsWritten := 0;
      END;
    WriteOut(n);
    IF  (width < 0) AND (CharsWritten < -width)  THEN
       BEGIN		(* Require formatting, with right fill. *)
	 FOR  i := 1  TO  -width - CharsWritten  DO  tcWrCh(blank);
	 CharsWritten := -width;
       END;
    curx := curx + CharsWritten;
    IF  cury = TopLine  THEN
      savx := curx;
  END  (* WriteNumber *);

PROCEDURE UpdateHitPointInformation;

  VAR
    ox : xval;
    oy : row;

  BEGIN
    ox := curx; oy := cury;
    curs(26, BottomLine);
    WriteNumber(u.uhp, 3);
    flags.dhp := False;
    curs(ox,oy);
  END;

PROCEDURE Swallowed;

  BEGIN
    News1(u.ux, u.uy);
    ClearScreen;
    curs(Pred(u.ux),Pred(u.uy));
    out('/-\\');
    curs(Pred(u.ux),u.uy);
    out('|@|');
    curs(Pred(u.ux),Succ(u.uy));
    out('\\-/');
    curx := u.ux + 2;
  END  (* Swallowed *);

PROCEDURE More;

  VAR
    ch : Char;

  BEGIN
    out('--More--');
    REPEAT tcRdCh(ch) UNTIL ch = blank;
  END;

PROCEDURE Pline;

  BEGIN  (* Pline *)
    IF  flags.dscr  THEN  nscr;
    IF  flags.topl  THEN
      BEGIN
	curs(savx,TopLine);
	more;
      END
    ELSE  flags.topl := True;
    CursorHome;
    tcLineErase(savx);
    savx := xmin;
  END  (* Pline *);

PROCEDURE Debugger;

  BEGIN
    pline;
    out('One day I''ll XOR every single bit in PaxDDT...');
    out('                            The PaxDDT Wizard');
  END;

PROCEDURE WriteTrap(t:trap);
 BEGIN
 CASE t OF
   beartrap: out(' bear trap');
   arrow  : out('n arrow trap');
   dart	  : out(' dart trap');
   tdoor  : out(' trapdoor');
   tele	  : out(' teleportation trap');
   pit	  : out(' pit');
   slptrp : out(' sleeping gas trap');
   StinkingTrap : out(' stinking gas trap');
   pierc  : out(' piercer');
   MimicTrap : out(' mimic');
 END;
 END;

PROCEDURE WriteProjectile(p:projectile);
 BEGIN
 CASE p OF
   missile : out(mmis);
   BoltOfFire : out('bolt of fire');
   SleepRay : out('sleep ray');
   BoltOfCold : out('bolt of cold');
   DeathRay : out('death ray');
   IcyWind  : out('icy wind');
 END;
 END;

PROCEDURE WriteNameOfEnemy(en:enemy);
 BEGIN
  IF en < NoMonster THEN WriteProjectile(en) ELSE
   CASE en OF
    NoMonster : out('>>> SuperZapper <<<');
    Bat : out('bat');
    Gnome : out('''friendly'' gnome');
    Hobgoblin : out('hobgoblin');
    Jackal : out('jackal');
    Kobold : out('kobold');
    Leprechaun : out('leprechaun');
    rat : out('giant rat');
    acidblob : out('acid blob');
    Eye : out('floating eye');
    homunculus : out('homunculus');
    imp : out('imp');
    Orc : out('orc');
    yellowlight : out('yellow light');
    Zombie : out('zombie');
    Ant : out('giant ant');
    fog : out('fog cloud');
    Nymph : out('nymph');
    piercer : out('piercer');
    Quasit : out('quasit');
    quiveringblob : out('quivering blob');
    violetfungi : out('violet fungi');
    beetle : out('giant bug');
    Centaur : out('centaur');
    cockatrice : out('cockatrice');
    gelatenouscube : out('gelatenous cube');
    jaguar : out('jaguar');
    killerbee : out('killer bee');
    Snake : out('snake');
    FreezingSphere : out('freezing sphere');
    owlbear : out('owlbear');
    Rust : out('rust monster');
    scorpion : out('giant scorpion');
    teleporter : out('teleporter');
    Wraith : out('wraith');
    Yeti : out('yeti');
    displacer : out('displacer beast');
    leocrotta : out('leocrotta');
    Mimic : out('mimic');
    minotaur : out('minotaur');
    Troll : out('troll');
    ugod : out('ubaat');
    xerp : out('xerp');
    Invisiblestalker : out('invisible stalker');
    Umberhulk : out('umber hulk');
    Vampire : out('vampire');
    wumpus : out('wumpus');
    Xorn : out('xorn');
    zelomp : out('zelomp');
    chameleon : out('chameleon');
    Dragon : out('dragon');
    ettin : out('ettin');
    LurkerAbove : out('lurker above');
    neootyugh : out('neo-otyugh');
    trapper : out('trapper');
    Purpleworm : out('purple worm');
    demon : out('demon');
    cyclops : out('cyclops');
    exorcist : out('exorcist');
    destroyer : out('destroyer');
    suicider : out('suicider');
    TwoHeadedEagle : out('twoheaded eagle');
    TyrannoSaur : out('tyrannosaur');
    humanoid : out('humanoid');
   (* other enemies than monsters: *)
    SnakesBite : out('snake''s bite');
    ScorpionsSting : out('scorpion''s sting');
    BeesSting : out('bee''s sting');
    AntSting : out('ant''s sting');
    FallingPiercer : out('falling piercer');
    FallingRock : out('falling rock');
    ArrowShot : out(litArrow);
    PoisonDart : out('poison dart');
    PitTrap : out('pit');
    StinkingGasTrap : out('cloud of stinking gas');
    WandOfStriking : out('wand of striking');
    LittleDart : out('little dart');
    PoisonPotion : out('poison potion');
    ScrollOfFire: out('scroll of fire');
    RayOfDrainLife : out('ray of drain life');
    PoisonGas : out('cloud of poison gas');
    SmellOfWumpus : out('smell of wumpus');
    Drowned : out('drowning');
    LackOfFood : out('starvation');
    ExplodingGem : out('exploding gem');
   END (*CASE*);
 END;

PROCEDURE WriteEnemy(species:enemy; cont:context);
 BEGIN
   CASE cont OF
    start :
     BEGIN
       pline;
       IF u.ublind > 0 THEN
	 out('It ')
       ELSE  out(LitThe);
     END;
    normal :
     IF u.ublind > 0 THEN
       out(litIt)
     ELSE out(' the ');
    IndefiniteArticle:
      IF NOT (species IN [LackOfFood, SmellOfWumpus, Drowned ]) THEN
	BEGIN
	  OutCh('a');
	  IF species IN [acidblob,imp,Orc,Ant,owlbear,
		         Invisiblestalker,Umberhulk,ettin,ArrowShot,
		         IcyWind,ExplodingGem,AntSting]
	   THEN OutCh('n');
          OutCh(blank);
	END;
    NoArticle:
     ;
   END (*CASE*);
   IF (u.ublind = 0) OR (cont = IndefiniteArticle) OR
      (cont = NoArticle)
   THEN
      WriteNameOfEnemy(species);
   IF cont = start THEN OutCh(blank);
 END;

PROCEDURE WriteArmament(class:armament);
 BEGIN
 IF class >= missile THEN WriteProjectile(class) ELSE
 CASE class OF
   warrow : out(litArrow);
   SlingBullet : out('sling bullet');
   CrossBowBolt : out('crossbow bolt');
   wdart : out(litDart);
   mace : out('mace');
   axe : out('axe');
   flail : out('flail');
   LongSword : out('long sword');
   TwoHandedSword : out('two handed sword');
   dagger : out('dagger');
   spear : out('spear');
   bow : out('bow');
   sling : out('sling');
   CrossBow : out('crossbow');
   StrikingWand : out(litWand);
 END;
 END;

PROCEDURE WriteArmor(x:ArmorType);
BEGIN
CASE x OF
 leather : out('leather');
 RingMail : out('ring');
 scale : out('scale');
 chain : out('chain');
 splint : out('splint');
 plate : out('plate');
END;
out(' armor');
END;

PROCEDURE WritePotion(x:PotionType);
BEGIN
CASE x OF
 RestoreStrength : out('restore strength');
 booze : out('booze');
 invisibility : out('invisibility');
 juice : out('juice');
 healing : out('healing');
 paralysis : out('paralysis');
 blood : out('blood');
 MonsterDetection : out('monster detection');
 ObjectDetection : out('object detection');
 MagicSpirit: out('magic spirit');
 sickness : out('sickness');
 confusion : out('confusion');
 GainStrength : out(litGainStrength);
 speed : out('speed');
 blindness : out('blindness');
 GainLevel : out('gain level');
 ExtraHealing : out('extra healing');
 forgetting : out('oblivion');
 PotionOfFrobozz : out('Frobozz');
END;
END;


PROCEDURE WriteWand(x:WandType);
BEGIN
CASE x OF
 WandOfLight : out(litLight);
 WandOfDarkness : out('darkness');
 detection : out('secret door and trap detection');
 CreatorWand : out(litCreateMonster);
 DestroyObject : out('destruction');
 striking : out('striking');
 SlowMonster : out('slow monster');
 SpeedMonster : out('speed monster');
 UndeadTurning : out('undead turning');
 polymorph : out('polymorph');
 cancellation : out('cancellation');
 TameMonster : out('tame monster');
 TeleportMonster : out('teleport monster');
 DrainLife : out('drain life');
 digging : out('digging');
 WandOfMissile : out(mmis);
 FireWand : out(litFire);
 SleepWand : out('sleep');
 cold : out('cold');
 death : out('death');
 WandOfFrobozz : out('Frobozz');
END
END;

PROCEDURE WriteRing(x:RingType);
BEGIN
CASE x OF
 adornment : out('adornment');
 TeleportRing : out(litTele);
 regeneration : out('regeneration');
 searching : out('searching');
 SeeInvisible : out('see invisible');
 stealth : out('stealth');
 floating : out('floating');
 PoisonResistance : out('poison resistance');
 aggravate : out('aggravate monster');
 hunger : out('hunger');
 FireResistance : out('fire resistance');
 ColdResistance : out('cold resistance');
 ProtectionFromShapeChangers : out('protection from shape changers');
 AddStrength : out('add strength');
 IncreaseDamage : out('increase damage');
 protection : out('protection');
 MaintainArmor : out('maintain armor');
 RingOfFrobozz : out('Frobozz');
END
END;

PROCEDURE WriteScroll(x:ScrollType);
BEGIN
CASE x OF
 EnchantArmor : out('enchant armor');
 ConfuseMonster : out('confuse monster');
 AggravateMonster : out('aggravate monster');
 ScareMonster : out('scare monster');
 BlankScroll : out('blank paper');
 RemoveCurse : out('remove curse');
 EnchantWeapon : out('enchant weapon');
 CreateMonster : out('create monster');
 DamageWeapon : out('damage weapon');
 genocide : out('genocide');
 DestroyArmor : out('destroy armor');
 ScrollOfLight : out(litLight);
 TeleportScroll : out(litTele);
 GoldDetection : out('gold detection');
 identify : out('identify');
 MagicMapping : out('magic mapping');
 FireScroll : out(litFire);
 ScrollOfFrobozz : out('Frobozz');
END
END;

PROCEDURE WriteQuantityAndColor(quantity:small;i:small);
 BEGIN
  IF quantity = 1 THEN
    BEGIN OutCh('a'); IF i IN [0,5,18] THEN OutCh('n') END
  ELSE
    WriteNumber(quantity,1);
  OutCh(blank);
  CASE i OF
    0 : out(litEbony);
    1 :	out('magenta');
    2 : out('clear');
    3 : out('brilliant blue');
    4 : out('sky blue');
    5 : out('emerald');
    6 : out('dark green');
    7 : out(ruby);
    8 : out('grey');
    9 : out('swirly');
   10 : out('Black & White');
   11 : out('yellow');
   12 : out('purple');
   13 : out('puce');
   14 : out('pink');
   15 : out('smokey');
   16 : out('glowing');
   17 : out('bubbly');
   18 : out('orange');
  END (*CASE*);
  OutCh(blank);
 END;

PROCEDURE WriteTitle(i:small);
 BEGIN
 CASE i OF
   0 : out('VELOX NEB');
   1 : out('FOOBIE BLETCH');
   2 : out('TEMOV');
   3 : out('GARVEN DEH');
   4 : out('ZELGO MER');
   5 : out('ANDOVA BEGARIN');
   6 : out('ELAM EBOW');
   7 : out('KERNOD WEL');
   8 : out('THARR');
   9 : out('VENZAR BORGAVVE');
  10 : out('ELBIB YLOH');
  11 : out('VERR YEH HORRE');
  12 : out('JUYED AWK YACC');
  13 : out('HACKEM MUCHE');
  14 : out('LEP GEX VEN ZEA');
  15 : out('DAIYEN FOOELS');
  16 : out('OTRAS YOBREN TWAN');
  17 : out('TITZAL MELUEN');
  END;
 END;

PROCEDURE WriteWandMaterial(i:small);
 BEGIN
 CASE i OF
   0 : out('oak');
   1 : out(litEbony);
   2 : out('runed');
   3 : out('zinc');
   4 : out('platinum');
   5 : out('steel');
   6 : out('aluminium');
   7 : out('iron');
   8 : out('marble');
   9 : out('pine');
  10 : out('maple');
  11 : out('brass');
  12 : out(silver);
  13 : out(copper);
  14 : out('balsa');
  15 : out('birch');
  16 : out('gold');
  17 : out('bronze');
  18 : out('glas');
  19 : out('rubber');
  20 : out('stone');
  END;
 END;

PROCEDURE WriteRingMaterial(i:small);
 BEGIN
 CASE i OF
   0 : out('topaz');
   1 : out('ivory');
   2 : out('granite');
   3 : out(silver);
   4 : out(ruby);
   5 : out('jade');
   6 : out('diamond');
   7 : out('black onyx');
   8 : out('gold');
   9 : out('shining');
  10 : out('tiger eye');
  11 : out('agate');
  12 : out('moonstone');
  13 : out('sapphire');
  14 : out('pearl');
  15 : out('birchbark');
  16 : out('wooden');
  17 : out('steel');
  END;
 END;

PROCEDURE PrUstr;

  VAR
    h : Byte;

  BEGIN  (* PrUstr *)
   h := u.ustr - 18;
   IF h > 0 THEN
     BEGIN
       out('18/');
       IF  h >= 100  THEN
	 out('**')
       ELSE
	 IF  h <= 9  THEN
	   BEGIN
	     OutCh('0');
	     OutCh(Chr(h+Ord('0')))
	   END
	 ELSE  WriteNumber(h,2);
     END  (* IF h > 0 *)
   ELSE
     BEGIN
       WriteNumber(u.ustr,2);
       out('   ');
     END;
  END  (* PrUstr *);

PROCEDURE WriteHungerState;
  BEGIN
     CASE u.uhs OF
       NotHungry: out('        ');
       hungry   : out('Hungry  ');
       weak     : out('Weak    ');
       fainting : out('Fainting');
       END (*CASE*);
  END;

PROCEDURE bot;
 BEGIN
   WITH flags DO
     BEGIN botl := False; dhp := False; dhpmax := False; dac := False;
     dstr := False; dgold := False; dhs := False END;
   curs(1,BottomLine);
   out('Level '); WriteNumber(dlevel,-2);
   out('  Gold '); WriteNumber(u.ugold,-5);
   out('  Hp ');WriteNumber(u.uhp,3);tcWrCh('(');
   WriteNumber(u.uhpmax,1);tcWrCh(')');
   IF u.uhpmax < 10 THEN out('  ') ELSE
   IF u.uhpmax < 100 THEN tcWrCh(blank);
   Out('Ac '); WriteNumber(u.uac,-2); Out('  Str ');
   prustr;
   out('  Exp ');WriteNumber(u.ulevel,2);tcWrCh('/');
   WriteNumber(u.uexp,-5);
   IF u.uhs <> NotHungry THEN
     BEGIN
     Out('      ');
     WriteHungerState;
     END;    
   curx := xmax;
 END;

PROCEDURE DoCrt;

  VAR
    x : xval;
    y : yval;

  BEGIN
    IF  u.uswallow  THEN
      BEGIN
	swallowed;
	flags.botl := True
      END
    ELSE
      BEGIN
	ClearScreen;
	FOR  y := ymin  TO  ymax  DO    FOR  x := xmin  TO  xmax  DO
	  WITH  f^.levl[x,y]  DO
	    IF  new  THEN
	      BEGIN
		new := False;
		IF ScrSym = blank THEN
		  BEGIN
		    seen := False;
		    IF room = WaterRoom THEN
		      ScrSym := '~'
		    ELSE
		      ScrSym := '.';
		  END
		ELSE
		  BEGIN
		    at(x,y,ScrSym);
		    seen := True
		  END
	      END
	    ELSE
	      IF  seen  THEN
		at(x,y,ScrSym);
	IF  u.uinvis = 0  THEN
	  curs(u.ux,u.uy+2);	(* Added; put cursor on the "you" symbol. *)
	scrlx := xmax; scrly := ymax;
	flags.dscr:=False; scrhx:=0; scrhy:=0; flags.botl:=False;
	flags.dhp:=False; flags.dhpmax:=False; flags.dac:=False;
	flags.dstr:=False; flags.dgold:=False; flags.dhs:=False;
	bot;
      END;
  END (* DoCrt *);

PROCEDURE DisplayResults(VAR f : ScoreFile; YourOwn : Boolean);

  VAR
    i : RecordNumber;

  BEGIN  (* DisplayResults *)
    SeekUpdate (f,1);
    IF  YourOwn  THEN
      BEGIN 
	out('YOUR TEN BEST RESULTS: Currently You are ');
	CASE YourExperienceInZAP OF
	  novice : out('a novice');
	  fair   : out('a fair');
	  good   : out('a good');
	  expert : out('an expert');
	  wizard : out('a wizard');
	END;
	out(' zapper');
      END 
    ELSE
      BEGIN 
	out('TOP TEN LIST OF ');
	CASE YourExperienceInZAP OF
	  novice : out('NOVICE');
	  fair   : out('FAIR');
	  good   : out('GOOD');
	  expert : out('EXPERT');
	  wizard : out('WIZARD');
	END;
	out(' ZAPPERS');
	SeekUpdate (f,Ord(YourExperienceInZAP) + 1);
      END;
    NewLine; NewLine;
    FOR  i := 1  TO  NumberOfScoreRecords  DO
      BEGIN
	WITH  f^[i]  DO
	 IF  points > 0  THEN
	  BEGIN
	    WriteNumber(i,2);OutCh('.');
	    WriteNumber(points,6);out('  ');
	    IF  NOT YourOwn  THEN  out(UserName);
	    CASE  ExitStatus  OF
	      quit	: BEGIN
			    out(' quit on level ');
			    WriteNumber(DungeonLevel,1);
			  END;
	      escaped	: BEGIN
			    out(' escaped alive from level ');
			    WriteNumber(DungeonLevel,1);
			  END;
	      died	: BEGIN
			    IF whatkilled = drowned THEN
			      BEGIN 
				out(' drowned on level ');
				WriteNumber(DungeonLevel,1);
			      END
			    ELSE
			      BEGIN 
				out(' killed on level ');
				WriteNumber(DungeonLevel,1);
				out(' by ');
				WriteEnemy(whatkilled,IndefiniteArticle);
			      END;
			  END;
	      digested   : BEGIN
	      		     out(' digested on level ');
			     WriteNumber(DungeonLevel, 1);
			     out(' by ');
			     WriteEnemy(whatkilled, IndefiniteArticle);
			   END;
	    END  (* CASE  ExitStatus OF *);
	  END;
	NewLine;
      END  (* FOR  i := 1  TO  NumberOfScoreRecords *);
  END;

PROCEDURE TopTenList(VAR f:ScoreFile; YourOwn:Boolean);

 VAR
   j:0..NumberOfScoreRecords;
   i:RecordNumber;
   size:Integer;
   UpdateReally : Boolean;
   b :  BindingType;

  PROCEDURE insert(inx:RecordNumber);
   BEGIN
     WITH f^[inx] DO
      BEGIN
	points := u.urexp;
	UserName := YourName;
	ExitStatus := TheWayItEnded;
	IF  ExitStatus = Escaped  THEN
	  DungeonLevel := DeepestLevelSaved
	ELSE
	  DungeonLevel := dlevel;
	whatkilled := killer;
        ExperienceInGame := YourExperienceInZAP;
      END;
   END;
       
  PROCEDURE InitFile;

    VAR
      ii: Integer;
      i : RecordNumber;

    BEGIN
      DefineSize(f,Ord(wizard)*Ord(NOT YourOwn) + 1);
      WITH  f^[1]  DO
	BEGIN
	  points := 0;
	  UserName := defaultNameForU;
	  ExitStatus := quit;
	  DungeonLevel := 1;
	  whatkilled := NoMonster;
	  ExperienceInGame := Novice;
	  reserved1 := -1;
	  reserved2 := -1;
	  reserved3 := -1;
	  reserved4 := -1;
	END;
      FOR  i := 2  TO  NumberOfScoreRecords  DO
	f^[i] := f^[1];
      SeekUpdate (f,1);
      IF NOT YourOwn THEN
	FOR ii:=Ord(novice)+1 TO Ord(wizard) DO
	  BEGIN
	    put (f);
	  END;
    END;

  BEGIN  (* TopTenList *)
    UpDateReally := True;
    b := binding (f);
    size := 0;
    if b.bound then
      size := b.size;
    IF size = 0 THEN InitFile;
    IF YourOwn THEN 
      SeekUpdate (f,1)
    ELSE
      BEGIN 
	SeekUpdate (f, Ord(YourExperienceInZAP) + 1);
	UpdateReally := TheWayItEnded = Escaped;
	IF NOT UpdateReally THEN
	  CASE YourExperienceInZAP OF
	    novice : UpdateReally := True;
	    fair   : UpdateReally := u.urexp >= 4000;
	    good   : UpdateReally := u.urexp >= 8000;
	    expert : UpdateReally := u.urexp >= 15000;
	    wizard : UpdateReally := u.urexp >= 20000;
	  END;
      END;
    IF NOT UpdateReally THEN
      BEGIN
	NewLine;
	out('Sorry, but You are not ');
	CASE YourExperienceInZAP OF
	  novice :;
	  fair	 : out('fair');
	  good	 : out('good');
	  expert : out('expert');
	  wizard : out('wizard');
	END;
	out(' enough zapper for system top ten list');
	NewLine;
      END
    ELSE
      IF u.urexp > f^[NumberOfScoreRecords].points THEN
	BEGIN  (* Made the top ten list *)
	  j := NumberOfScoreRecords;
	  REPEAT
	    j := j - 1;
	  UNTIL (j=1) OR (u.urexp <= f^[j].points);
	  IF (u.urexp > f^[1].points) THEN
	    j := 1
	  ELSE
	    j := j + 1;
	  FOR i := NumberOfScoreRecords-1 DOWNTO j DO
	    f^[i+1] := f^[i];
	  SeekUpdate(f, Ord(YourExperienceInZAP)*Ord(NOT YourOwn) + 1);
	  insert(j);
	  Update (f);
	END  (* Made the top ten list *);
    DisplayResults(f,YourOwn);
  END  (* TopTenList *);

PROCEDURE MkObj(cl:ObjectClass);

  VAR
    otmp : Thing;

  BEGIN  (* MkObj *)
    create(otmp,objects);
    WITH otmp^ DO
      BEGIN
	next := f^.listof[objects];
	minus := False;
	known := False;
	cursed := False;
	Magical := False;
	spe := 0;
	class := cl;
	CASE cl OF
	  Weapons: BEGIN
		     olet := ')';
		     weapon := WeaponType(rnd0(wepnum));
		     IF weapon <= wdart THEN
		       quan := rn1(6,6)
		     ELSE  quan := 1;
		     IF  rn2(11+Ord(MakingLevel))  THEN
		       spe := rnd(3)
		     ELSE
		       IF  rn2(10+Ord(MakingLevel))  THEN
			 BEGIN
			   cursed:=True;
			   minus:=True;
			   spe := rnd(3)
			 END;
		   END;
	  Armors : BEGIN
		     olet := '[';
		     armor := ArmorType(rnd0(armnum));
		     quan := 1;
		     IF  rn2(10) OR ((armor = leather) AND rn2(3))  THEN
		       IF (armor < chain) AND rn2(8-2*Ord(armor=leather)) THEN
			 spe := 3 + Rnd(3-Ord(armor))
		       ELSE
			 spe := rnd(3)
		     ELSE
		       IF  rn2(9)  THEN
			 BEGIN
			   cursed:=True;
			   minus:=True;
			   spe := rnd(3)
			 END;
		   END;
	  Potions: BEGIN
		     olet := '!';
		     potion := PotionType(Ord( (u.ustr > 6) OR NOT rn2(3) )
					  * rnd0(potnum-1));
			 (* Potion of Frobozz is not generated here *)
		     IF  MakingLevel  THEN
		       IF  potion > confusion  THEN
			 IF  rn2(3)  THEN
			   potion := PotionType(rnd0(potnum-1));
		     IF potion IN [ MagicSpirit, forgetting ] THEN
		       IF rn2(2) THEN
			 potion := PotionType(rnd0(potnum-1));
		     quan := 1;
		   END;
	  Scrolls: BEGIN
		     olet := '?';
		     IF  rn2(20) THEN
		       scroll := identify
		     ELSE 
		       scroll := ScrollType(rnd0(scrnum-2));
		     (* Scroll of Frobozz is not generated here *)
		     IF  MakingLevel  THEN
		       IF  scroll IN [ ScareMonster, AggravateMonster, 
				       DamageWeapon, DestroyArmor ]  THEN
			 IF rn2(4) THEN
			   scroll := identify
			 ELSE
			   IF rn2(2) THEN
			     scroll := ScrollType(rnd0(scrnum-2));
		     IF scroll = genocide THEN
		       IF (genocided <> []) AND NOT rn2(5) THEN 
			 scroll := ScrollType(rnd0(scrnum-2));
		     quan := 1;
		   END;
	  food	 : BEGIN
		     olet := '%';
		     fruit := rn2(4);
		     (* fruit := rn2(6+3*Ord(u.uhs=fainting)); *)
		     quan := 1 + Ord(rn2(6));
		   END;
	  Wands  : BEGIN
		     olet := '/';
		     wand := WandType(rnd0(wandnum-2));
		     IF  MakingLevel  THEN
		       IF  wand = death  THEN
			 wand := WandType(rnd0(wandnum-2));
		     (* Wand of Frobozz is not generated here *)
		     IF  wand IN [ WandOfLight, WandOfDarkness, detection,
				   CreatorWand, DestroyObject ]  THEN
		       spe := rn1(5,11)
		     ELSE  spe := rn1(5,4);
		     quan := 1;
		   END;
	  rings	 : BEGIN
		     olet := '=';
		     ring := RingType(rnd0(ringnum-2));
		     (* Ring of Frobozz is not generated here *)	     
		     IF  ring IN [ AddStrength, IncreaseDamage,
		     		   protection ] THEN
		       BEGIN
			 IF rn2(3) THEN
			   BEGIN
			     cursed := True;
			     minus := True
			   END;
			 spe := rnd(2+Ord(MakingLevel));
		       END
		     ELSE
		       IF  ring IN [ TeleportRing, aggravate, hunger ]  THEN
			 cursed := NOT rn2(5);
		     quan := 1;
		   END;
	  Gems	 : BEGIN
		     olet := '*';
		     gem := GemType(rnd0(gemnum));
		     quan := 1 + Ord(rn2(6));
		   END;
	  Amulet : ;
	END  (* CASE cl OF *);
      END  (* WITH otmp^ DO *);
    f^.listof[objects] := otmp;
  END  (* MkObj *);

PROCEDURE MakeSomeObject;

  VAR
    Class    : ObjectClass;
    MakeFood : Boolean;

  BEGIN  (* MakeSomeObject *)
    IF  u.uhs < weak  THEN
      MakeFood := False
    ELSE
      IF  invent = NIL  THEN
	MakeFood := True
      ELSE
	WITH  invent^  DO
	  IF  class <> food  THEN
	    Makefood := True
	  ELSE
	    MakeFood := fruit;
    IF  MakeFood AND rn2(3)  THEN
      class := food
    ELSE
      CASE rnd(20) OF
	1, 2	   : class := Weapons;
	3, 4	   : class := Armors;
	5,6,14,16  : class := Potions;
	7,8,15,17  : class := Scrolls;
	9,10,11,18 : class := food;
	12	   : class := Wands;
	13	   : class := Rings;
	19, 20	   : class := Gems;
      END (*CASE*);
    MkObj(class)
  END  (* MakeSomeObject *);

PROCEDURE NewMonster(VAR m : thing;
		     NextMonster : thing;
		     x : xval; y : yval;
		     invisible : Boolean;
		     ShapeChanger : Boolean;
		     speed : MonsterSpeed;
		     state  : MonsterState;
		     cancelled : Boolean;
		     SpeciesOfMonster : MonsterSpecies;
		     Hp,OriginalHp : short);

  BEGIN  (* NewMonster *)
    IF  SpeciesOfMonster = rat  THEN
      f^.NumberOfRats := f^.NumberOfRats + 1;
    create(m,monsters);
    WITH  m^  DO
      BEGIN
	next := NextMonster;
	locx := x; locy := y;
	invis := invisible;
	cham := ShapeChanger;
	giant := False;
	Telport := False;
	rege := False;
	fireres := False;
	SeeInv := False;
	float := False;
	pres := False;
	strength := 3;
	protect := 3;
	mspeed := speed;
	mstat := state;
	mcan := cancelled;
	species := SpeciesOfMonster;
	mhp := hp;
	OrigHp := OriginalHp;
	TimeForTheMonsterToRemainInTrap := 0;
	ObjectsCarried := NIL;
     END;
  END  (* NewMonster *);

PROCEDURE NewCham(mtmp : thing; lookslike : MonsterSpecies);
  (* Make a chameleon look like a new monster *)

  VAR
    CanSeeIt : Boolean;
    NewMhp   : Positive;

  BEGIN
    WITH mtmp^, mon[species] DO
      BEGIN
	news1(locx,locy); (* Remove the monster symbol. *)
	invis := False;
	NewMhp := 2 + Sqr(mhp) DIV (mhd*8 + Ord(mhd=0));
	IF  NewMhp > MaxByte  THEN
	  mhp := MaxByte
	ELSE
	  mhp := NewMhp;
	species := lookslike;
	CanSeeIt := f^.levl[locx,locy].CanSee;
	IF  lookslike = Invisiblestalker  THEN
	  BEGIN
	    invis := True;
	    IF  CanSeeIt  THEN
	      prl(locx,locy);
	  END
	ELSE IF CanSeeIt THEN
	  pmon(mtmp);
      END  (* WITH mtmp^, mon[species] *);
  END  (* NewCham *);

PROCEDURE MakeMonster(WhatSpecies : MonsterSpecies; Giant : Boolean);

  CONST
    dummy = 0;

  VAR
    hp, tmp   : small;

  BEGIN
    IF WhatSpecies = NoMonster THEN
      REPEAT
	(* Select a row (tmp) of MonsterTable as follows:
	   At dungeon levels 1..3 the row is 0; at levels 4..6 it
	   is 0 or 1 (with equal probabilities); at levels 7..9
	   it is a random number from the range 0..2, at levels
	   10..12 from the range 1..3, etc. If the row number
	   so drawn would be too large (>mtabM), the actual row
	   number is set to mtabM or mtabM-1. *)
	IF  (dlevel <= 3) AND NOT YouHaveTheAmulet  THEN
	  tmp := 0
	ELSE
	  IF  (dlevel <= 6) AND NOT YouHaveTheAmulet  THEN
	    tmp := rnd0(2)
	  ELSE
	    BEGIN
	      tmp := (dlevel-1 + 6*Ord(YouHaveTheAmulet)) DIV 3
		       - 2 + rnd0(3+Ord(YouHaveTheAmulet));
	      (* Let's create some surprises: *)
	      IF dlevel < 30 THEN
		BEGIN 
		  IF  rn2(20)  THEN
		    IF  tmp = 0  THEN
		      tmp := 3 + 2*Ord(YouHaveTheAmulet) (* 6x *)
		    ELSE
		      tmp := rn1(tmp,4*Ord(YouHaveTheAmulet)); (* 7x *)
		END
	      ELSE 
		BEGIN 
		  IF  rn2(5)  THEN
		    tmp := rnd(mtabM)
		END;
	      IF dlevel < 30 THEN
		BEGIN 
		  IF  tmp >= mtabM  THEN
		    tmp := mtabM - rnd(2);
		END
	      ELSE
		IF tmp > mtabM THEN
		  tmp := mtabM - rnd0(2);
	    END;
	(* Now pick up a a monster from the row at random: *)
	WhatSpecies := MonsterTable[tmp,rnd0(mtabN+1)];
	UNTIL Giant OR
	      ((NOT (WhatSpecies IN genocided)) AND
	      ((dlevel > 36) OR NOT (WhatSpecies IN [ destroyer, TyrannoSaur,
						      humanoid ])));

    IF  MakingLevel AND (WhatSpecies = Dragon)  THEN
      hp := 80+8*Ord(YouHaveTheAmulet)
    ELSE
      IF WhatSpecies = humanoid THEN
	hp := 120
      ELSE 
	WITH  mon[ WhatSpecies ]  DO
	  IF  mhd = 0  THEN
	    hp := rnd(4 + dlevel DIV 3 -
		      (Ord(WhatSpecies) - Ord(nomonster) - 1) DIV 7 +
		      2*Ord(YouHaveTheAmulet)) (* 3x *)
	  ELSE
	    hp := d(mhd + dlevel DIV 3 -
		    (Ord(WhatSpecies) - Ord(nomonster) - 1) DIV 7 +
		    2*Ord(YouHaveTheAmulet), 8);
    IF Giant THEN
      hp := rn1(20,80);
    NewMonster(f^.listof[monsters],f^.listof[monsters],dummy,dummy,
	       WhatSpecies=Invisiblestalker,WhatSpecies=chameleon,
	       NormalSpeed,NormalState,False,WhatSpecies,hp,hp);
    IF Giant THEN
      f^.listof[monsters]^.giant := True;
    IF  NOT MakingLevel AND NOT u.ucham AND (WhatSpecies = chameleon)  THEN
      NewCham(f^.listof[monsters],MonsterTable[rn1(7,2),rnd0(mtabN+1)]);
    IF (rnd(30) < (dlevel - 36) * 2) AND NOT Giant THEN
      f^.listof[monsters]^.mspeed := mfast;
  END  (* MakeMonster *);

FUNCTION MakeMon(WhatSpecies:MonsterSpecies):Boolean;

  BEGIN
    IF  WhatSpecies IN genocided  THEN
      BEGIN
	IF  u.ublind = 0  THEN
	  BEGIN
	    WriteEnemy(WhatSpecies,start);
	    out('vanishes!');
	  END;
	MakeMon := False;
      END
    ELSE
      BEGIN
	MakeMon := True;
	MakeMonster(WhatSpecies, False);
      END;
  END  (* MakeMon *);

PROCEDURE DefineMonsters;

  VAR
    species : MonsterSpecies;
    i	    : mtabRow;
    j	    : mtabCol;

  PROCEDURE Def(m : MonsterSpecies; let : Char;
		hd,move : small; acl : byte; dan,dad : small);
    BEGIN  (* Def *)
      WITH mon[m] DO
	BEGIN
	  mlet := let;
	  mhd := hd;
	  mmove := move;
	  ac := acl;
	  damn := dan;
	  damd := dad;
	END;
    END  (* Def *);

  BEGIN  (* DefineMonsters *)

    species := NoMonster;
    FOR i := 0 TO mtabM DO  FOR j := 0 TO mtabN DO
      BEGIN
	species := Succ(species);
	MonsterTable[i,j] := species
      END;

    mlarge := [ beetle, Centaur, Dragon, displacer, ettin,
		gelatenouscube, Invisiblestalker, leocrotta,
		minotaur, neootyugh, owlbear, Purpleworm,
		Snake, scorpion, Troll, Umberhulk, wumpus, Yeti,
		LurkerAbove, trapper, demon, cyclops, destroyer, TyrannoSaur ];

    MonstersKilledByYou := [];

    MonstersWhichRegenerateFast := [ Vampire, imp, Troll, humanoid ];

    def(Bat,		 'B', 1, 22, 8, 1, 4);
    def(Gnome,		 'G', 1, 6, 5, 1, 6);
    def(Hobgoblin,	 'H', 1, 9, 5, 1, 8);
    def(Jackal,		 'J', 0, 12, 7, 1, 2);
    def(Kobold,		 'K', 1, 6, 7, 1, 4);
    def(Leprechaun,	 'L', 1, 15, 8, 1,5);
    def(rat, 		 'r', 0, 12, 7, 1, 3);
    def(acidblob,	 'a', 2, 3, 8, 0, 0);
    def(Eye,		 'E', 2, 1, 9, 1, 1);
    def(homunculus,	 'h', 2, 6, 6, 1, 3);
    def(imp,		 'i', 2, 6, 2, 1, 4);
    def(Orc,		 'O', 2, 9, 6, 1, 8);
    def(yellowlight,	 'y', 5, 15, 0, 0, 0);
    def(Zombie,		 'Z', 2, 6, 8, 1, 8);
    def(Ant,		 'A', 3, 18, 3, 1, 6);
    def(fog,		 'f', 3, 1, 0, 1, 6);
    def(Nymph,		 'N', 3, 12, 9, 1, 4);
    def(piercer,	 'p', 3, 1, 3, 2, 6);
    def(Quasit,		 'Q', 3, 15, 3, 1, 4);
    def(quiveringblob,	 'q', 3, 16, 8, 1, 8);
    def(violetfungi,	 'v', 3, 1, 7, 1, 4);
    def(beetle,		 'b', 4, 15, 4, 2, 4);
    def(Centaur,	 'C', 4, 18, 4, 1, 6);
    def(cockatrice,	 'c', 5, 6, 6, 3, 3);
    def(Mimic,		 'M', 7, 3, 7, 3, 4);
    def(jaguar,		 'j', 4, 15, 6, 1, 8);
    def(killerbee,	 'k', 4, 6, 4, 2, 4);
    def(Snake,		 'S', 4, 15, 3, 1, 6);
    def(Freezingsphere,	 'F', 2, 13, 4, 0, 0);
    def(owlbear,	 'o', 5, 12, 5, 2, 6);
    def(Rust,		 'R', 5, 18, 3, 0, 0);
    def(scorpion,	 's', 5, 15, 3, 1, 4);
    def(teleporter,	 't', 5, 3, 2, 1, 7);   (* Gave him +3 protection *)
    def(Wraith,		 'W', 5, 12, 5, 1, 6);
    def(Yeti,		 'Y', 5, 9, 6, 1, 6);
    def(displacer,	 'd', 6, 15, 2, 2, 4);  (* Gave her +2 protection *)
    def(leocrotta,	 'l', 6, 18, 4, 3, 6);
    def(gelatenouscube,	 'g', 7, 6, 3, 2, 4);   (* Gave him +5 protection *)
    def(minotaur,	 'm', 6, 12, 6, 2, 8);
    def(Troll,		 'T', 7, 12, 4, 2, 6);
    def(ugod,		 'u', 6, 11, 5, 1, 10);
    def(xerp,		 'x', 7, 6, 3, 2, 4);
    def(Invisiblestalker,'I', 8, 12, 3, 4, 4);
    def(Umberhulk,	 'U', 9, 6, 2, 2, 10);
    def(Vampire,	 'V', 8, 12, 1, 1, 6);
    def(wumpus,		 'w', 8, 3, 2, 3, 6);
    def(Xorn,		 'X', 8, 9, -2, 4, 6);
    def(zelomp,		 'z', 9, 8, 3, 3, 6);
    def(chameleon,	 ':', 6, 5, 6, 4, 2);
    def(Dragon,		 'D', 10, 9, -1, 3, 8);
    def(ettin,		 'e', 10, 12, 3, 2, 8);
    def(LurkerAbove,	 '`', 10, 3, 3, 0, 0);
    def(neootyugh,	 'n', 11, 6, 0, 1, 3);
    def(trapper,	 ',', 12, 3, 3, 0, 0);
    def(Purpleworm,	 'P', 15, 9, 6, 2, 8);
    def(demon,		 '&', 10, 9, -4, 1, 4);
    def(cyclops,	 ']', 14, 6, -3, 6, 8);
    def(exorcist,	 ';', 12, 3, -6, 0, 0);
    def(destroyer,	 '{', 15, 12, -1, 4, 8);
    def(suicider,	 '''',12, 36, 2, 0, 0);
    def(TwoHeadedEagle,	 '"', 12, 24, -2, 5, 5);
    def(TyrannoSaur,	 '(', 18, 6, -7, 8, 6);
    def(humanoid,	 '}', 20, 12, -5, 5, 5);
  END  (* DefineMonsters *);

PROCEDURE DefineWeapons;

  PROCEDURE Def(a : WeaponType; s,l : small);

    BEGIN
      wsdam[a] := s;
      wldam[a] := l
    END;

  BEGIN  (* DefineWeapons *)
    (* The following specifies the permanent strength of weapons.
       Some weapons have variations in strength as well as
       special dexterity properties. *)
    def(warrow,8,6);
    def(SlingBullet,5,8);
    def(CrossBowBolt,5,10);
    def(wdart,3,2);
    def(mace,6,6);
    def(axe,9,4);
    def(flail,6,4 (*rnd(4)*));
    def(LongSword,8,12);
    def(TwoHandedSword,10,6 (*+d(2,6)*));
    def(dagger,4,3);
    def(spear,6,8);
    def(bow,4,6);
    def(sling,6,6);
    def(CrossBow,4,6);
  END  (* DefineWeapons *);

PROCEDURE MakeCallableObjectsAnonymous;

  VAR
    s : ScrollType;
    p : PotionType;
    w : WandType;
    r : RingType;

  BEGIN  (* MakeCallableObjectsAnonymous *)
    FOR p := RestoreStrength TO PotionOfFrobozz DO potcal[p] := blankname;
    FOR s := EnchantArmor TO ScrollOfFrobozz DO scrcal[s] := blankname;
    FOR w := WandOfLight TO WandOfFrobozz DO wandcal[w] := blankname;
    FOR r := adornment TO RingOfFrobozz DO ringcal[r] := blankname;
  END  (* MakeCallableObjectsAnonymous *);

FUNCTION abon:byte; (*accuracy bonus*)

  BEGIN
    IF u.ustr = 3 THEN abon := -3 ELSE
      IF u.ustr < 6 THEN abon := -2 ELSE
	IF u.ustr < 8 THEN abon := -1 ELSE
	  IF u.ustr < 17 THEN abon := 0 ELSE
	    IF u.ustr < 69 THEN abon := +1 ELSE  (* up to 18/55 *)
	      IF u.ustr < 118 THEN abon := +2 ELSE  abon := +3;
  END;

FUNCTION dbon:byte; (*damage bonus*)

  BEGIN
    IF u.ustr < 6 THEN dbon := -1 ELSE
      IF u.ustr < 16 THEN dbon := 0 ELSE
	IF u.ustr < 18 THEN dbon := +1 ELSE
	  IF u.ustr = 18 THEN dbon := +2 ELSE   (* up to 18 *)
	    IF u.ustr < 94 THEN dbon := +3 ELSE   (* up to 18/75 *)
	      IF u.ustr < 109 THEN dbon := +4 ELSE  (* up to 18/90 *)
		IF u.ustr < 118 THEN dbon := +5 ELSE  (* up to 18/99 *)
		  dbon := +6;
  END;

FUNCTION YourCarryAbility(YourStrength:byte): byte;

  VAR 
    tmp: byte;
	
  BEGIN(* YourCarryAbility *)
    IF YourStrength < 18 THEN
      tmp := 80 + (YourStrength - 16) * 3
    ELSE 
      IF YourStrength = 18 THEN
	tmp := 86
      ELSE
	IF YourStrength < 48 THEN   (* 18/30 *)
	  tmp := 89
	ELSE
	  IF YourStrength < 78 THEN   (* 18/60 *)
	    tmp := 92
	  ELSE
	    IF YourStrength < 108 THEN   (* 18/90 *)
	      tmp := 95
	    ELSE
	      IF YourStrength < 118 THEN   (* 18/** *)
		tmp := 100
	      ELSE
		tmp := MaxWeight;
    YourCarryAbility := tmp;
  END(* YourCarryAbility *);


PROCEDURE MkGold(num : ShortCount; x:xval; y:yval);

  VAR
    gtmp : Thing;

  BEGIN  (* MkGold *)
    IF num = 0 THEN
      num := 1 + rnd(dlevel+2) * rnd(30);
    IF dlevel > 30 THEN
      num := num + 100 * (dlevel - 24) DIV 6;
    create(gtmp,golds);
    WITH gtmp^ DO
      BEGIN
	next := f^.listof[golds];
	locx := x; locy := y;
	quantity := num;
      END;
    f^.listof[golds] := gtmp;
    f^.levl[x,y].ScrSym := '$';
  END  (* MkGold *);

 FUNCTION WearingRingOfFrobozz: Boolean;

   VAR
     tmp : Boolean;

   BEGIN
     tmp := false;
     IF uright <> NIL THEN
       tmp := uright^.ring = RingOfFrobozz;
     IF not tmp  AND (uleft <> NIL) THEN
       tmp := uleft^.ring = RingOfFrobozz;
     WearingRingOfFrobozz := tmp;         
   END (* WearingRingOfFrobozz *);

FUNCTION Mazx : Positive;
  (* Walls have always even coordinates *)
  BEGIN
    mazx := 2 * rnd(37) + 1
  END;  

FUNCTION Mazy : Positive;
  (* Walls have always even coordinates *)
  BEGIN
    mazy := 2 * rnd(8) + 1
  END;  

PROCEDURE mklev;

  LABEL
    0, 100, 999;

  CONST
    LastRoomNr = 14;

  TYPE
    mkroom = RECORD
	       lx,hx : byte;
	       ly,hy : byte
	     END;

    RoomNumber = 0..LastRoomNr;

  VAR
    room	: ARRAY [ RoomNumber ] OF mkroom;
    icroom,
    itroom,
    nxcor	: ShortCount;
    croom,
    troom	: mkroom;
    x, y,
    tx, ty,
    nroom,
    lowy, lowx	: Small;
    dx, dy	: Byte;
    counter,				(* To prevent infinite (?) looping *)
    tries	: unsigned;
    MakeRoom,
    TreasureRoom: Boolean;

  FUNCTION Somex : xval;
    BEGIN
      somex := rand MOD (room[icroom].hx-room[icroom].lx) + room[icroom].lx
    END;

  FUNCTION Somey : xval;
    BEGIN
      somey := rand MOD (room[icroom].hy-room[icroom].ly) + room[icroom].ly
    END;

  PROCEDURE initialize;

    VAR
      x : xval;
      y : yval;

    BEGIN  (* Initialize *)
      (* Use array assignments for efficiency *)
      WITH f^.levl[xmin,ymin] DO
	BEGIN
	  ScrSym := blank;
	  typ := empty;
	  lit := False;
	  seen := False;
	  new := False;
	  CanSee := False;
	END;
      FOR y := Succ(ymin) TO ymax DO
	f^.levl[xmin,y] := f^.levl[xmin,ymin];
      FOR x := Succ(xmin) TO xmax DO
	f^.levl[x] := f^.levl[xmin];
    END  (* Initialize *);

  PROCEDURE MkTrap(Typ : Trap);

    VAR
      gtmp : Thing;

    BEGIN  (* MkTrap *)
      create(gtmp,traps);
      WITH gtmp^ DO
	BEGIN
	  gflag := typ;
	  seen := False;
	  SeenByMonsters := False;
	  IF gflag = MimicTrap THEN
	   BEGIN
	    IF rn2(2) THEN
	      BEGIN
		WITH room [ icroom ] DO
		  IF rn2(2) THEN
		    BEGIN
		      IF rn2(2) THEN
			locx := hx + 1
		      ELSE locx := lx - 1;
		      locy := somey;
		    END  (* IF rn2(2) *)
		  ELSE
		    BEGIN
		      IF rn2(2) THEN
			locy := hy + 1
		      ELSE  locy := ly - 1;
		      locx := somex;
		    END;
		f^.levl[locx,locy].ScrSym := '+';
		AddToList(gtmp,f^.listof[traps]);
	      END  (* IF rn2(2) *)
	    ELSE
	      BEGIN
		gflag := beartrap;
		AddToList(gtmp,f^.listof[golds]);
		locx := somex; locy := somey;
		f^.levl[locx,locy].ScrSym := '$';
		tries := tries + 1;
	      END
	   END  (* IF gflag = MimicTrap *)
	  ELSE
	    BEGIN  (* gflag <> MimicTrap *)
	      IF gflag = pit THEN
		ThereIsWaterInThePit := rn2(4);
	      REPEAT
		locx := somex;
		locy := somey;
	      UNTIL (* (ThingAt(locx,locy,traps) = NIL) AND *)
	            NOT ((locx = f^.xdnstair) AND (locy = f^.ydnstair)) AND
	            NOT ((locx = f^.xupstair) AND (locy = f^.yupstair));
	      (* Patch by The Jukkas at 4 o'clock A.M. Some unwizards
	         which we haven't yet cancelled had added stupid calls
	         like WHILE rn2(2) DO MkTrap(...) so at deeper levels -
	         unreachable to others than us... - level generation
	         looped. Simple, if not genious, solution was to allow
	         the generation of several traps at the same location. *)
	      AddToList(gtmp,f^.listof[traps]);
	      IF NOT seen THEN
		IF rn2(10) THEN seen := True;
	    END  (* gflag <> MimicTrap *);
	END  (* WITH gtmp^ *);
    END  (* MkTrap *);

  FUNCTION SomeTrap : Trap;

    VAR
      Typ : Trap;

    BEGIN  (* SomeTrap *)
      REPEAT
	typ := trap(rnd0(trapnum));
      UNTIL  NOT ((typ=pierc) AND (dlevel<4) OR
		  (typ=MimicTrap) AND ((dlevel<9) OR (tries<>0)) );
      SomeTrap := typ;
    END  (* SomeTrap *);

  PROCEDURE MakeMaz;

    CONST
      StackSize = 300;

    TYPE
      direction = (left,up,right,down);
      StackPointer = 0..StackSize;

    VAR
      x, y	: byte;
      q		: 0..4;
      sp	: StackPointer;
      stack	: ARRAY [ StackPointer ] OF RECORD
					      sx, sy : small
					    END;
      zx, zy	: Small;
      dirs	: ARRAY [ 0..4 ] OF direction;
      dir,
      a		: Direction;
      otmp,
      trp	: Thing;
     
    PROCEDURE move(VAR x:byte; VAR y:byte; dir:direction);

      BEGIN  (* Move *)
	CASE dir OF
	  left  : x := x - 1;
	  up    : y := y + 1;
	  right : x := x + 1;
	  down  : y := y - 1;
	END (*CASE*)
      END  (* Move *);

    FUNCTION Okay(x, y : byte; dir : direction) : Boolean;

      BEGIN  (* Okay *)
	move(x,y,dir);
	move(x,y,dir);
	IF (x<3) OR (y<3) OR (x>17) OR (y>75) THEN
	  okay := False
	ELSE okay := f^.levl[y,x].typ = empty;
      END  (* Okay *);

    BEGIN  (* MakeMaz *)
      (* x and y are partially reversed in this code... not my fault *)
      f^.ThisIsAMaze := True;
      FOR x := ymin TO ymax DO  FOR y := xmin TO xmax DO
	WITH  f^.levl[y,x]  DO
	  BEGIN
	    room := OrdinaryRoom;
	    new := False;
	    seen := False;
	    lit := False;
	    CanSee := False;
	    IF  (x<2) OR (x>18) OR (y<2) OR (y>76) OR Odd(x) AND Odd(y)  THEN
	      typ := empty
	    ELSE
	      typ := wall;
	  END  (* WITH  f^.levl[y,x] *);
      zx := mazy;
      zy := mazx;
      sp := 1;
      WITH  stack[1]  DO
	BEGIN
	  sx:=zx;
	  sy:=zy
	END;
      WHILE sp <> 0 DO
	BEGIN
	  WITH  stack[sp]  DO
	    BEGIN
	      x:=sx;
	      y:=sy
	    END;
	  f^.levl[y,x].typ := sdoor;
	  q := 0;
	  FOR  a := left  TO  down  DO
	    IF  okay(x,y,a)  THEN
	      BEGIN
		dirs[q] := a;
		q := q+1;
	      END;
	  IF  q <> 0  THEN
	    BEGIN
	      dir := dirs[rnd0(q)];
	      move(x,y,dir);
	      f^.levl[y,x].typ := empty;
	      move(x,y,dir);
	      sp := sp + 1;
	      WITH  stack[sp]  DO
		BEGIN
		  sx:= x;
		  sy := y
		END;
	    END  (* IF  q <> 0 *)
	  ELSE  sp := sp - 1;
	END  (* WHILE sp <> 0 *);
      FOR x := 2 TO 76 DO  FOR y := 2 TO 18 DO
	WITH  f^.levl[x,y]  DO
	  IF typ = wall THEN
	    ScrSym := '-'
	  ELSE
	    BEGIN
	      ScrSym := '.';
	      typ := RoomLocation;
	    END;
      FOR x := rn1(6,9 + dlevel - 27) DOWNTO 1 DO
	BEGIN
	  create(trp,traps);
	  WITH  trp^  DO
	    BEGIN
	      REPEAT
		locx := mazx;
		locy := mazy;
	      UNTIL ThingAt(locx,locy,traps) = NIL;
	      gflag := SomeTrap;
	      seen := False;
	      SeenByMonsters := False;
	      IF gflag = MimicTrap THEN
		IF rn2(2) THEN
		  AddToList(trp,f^.listof[traps])
		ELSE
		  BEGIN
		    gflag := beartrap;
		    AddToList(trp,f^.listof[golds]);
		  END
	      ELSE
		BEGIN  (* gflag <> MimicTrap *)
		  AddToList(trp,f^.listof[traps]);
		  IF rn2(10) THEN seen := True;
		END;
	    END  (* WITH  trp^ *);
	END  (* FOR x := rn1(6,7) *);
      REPEAT 
	f^.xdnstair := mazx;
	f^.ydnstair := mazy;		(* The way to the unknown... *)
      UNTIL ThingAt(f^.xdnstair, f^.ydnstair, traps) = NIL;
      f^.levl[f^.xdnstair,f^.ydnstair].ScrSym := downwards;
      REPEAT 
	f^.xupstair := mazx;
	f^.yupstair := mazy;
      UNTIL ThingAt(f^.xupstair, f^.yupstair, traps) = NIL;
      f^.levl[f^.xupstair,f^.yupstair].ScrSym := upwards;
      FOR  x := rn1(8,11 + dlevel - 27)  DOWNTO  1  DO
	BEGIN
	  MakeSomeObject;
	  WITH f^.listof[objects]^ DO
	    BEGIN
	      locx := mazx;
	      locy := mazy;
	      f^.levl[locx,locy].ScrSym := olet;
	    END;
	END  (* FOR  x := rn1(8,11) *);
      FOR x := rn1(5,7 + dlevel - 27) DOWNTO 1 DO
	BEGIN
	  MakeMonster(NoMonster, False);
	  WITH  f^.listof[monsters]^  DO
	    BEGIN
	      locx := mazx;
	      locy := mazy;
	      mstat := sleep
	    END
	END  (* FOR x := rn1(5,7) *);
      FOR x := rn1(6,9 + dlevel - 27) DOWNTO 1 DO
	MkGold(0,mazx,mazy);
      MkObj(scrolls); (* Just create it *)
      IF dlevel = FirstMaze THEN
	WITH f^.listof[objects]^ DO
	  BEGIN
	    locx := zy;
	    locy := zx;
	    quan := 1;
	    class := scrolls;
	    scroll := ScrollOfFrobozz;
	    Magical := True;
	    f^.levl[locx, locy].ScrSym := '?';
	  END
      ELSE IF dlevel = SecondMaze THEN 
	WITH f^.listof[objects]^ DO
	  BEGIN
	    locx := zy;
	    locy := zx;
	    quan := 1;
	    class := rings;
	    olet := '=';
	    ring := RingOfFrobozz;
	    Magical := True;
	    f^.levl[locx, locy].ScrSym := '=';
	  END
      ELSE IF dlevel = ThirdMaze THEN
	BEGIN 
	  WITH f^.listof[objects]^ DO
	    BEGIN
	      locx := zy;
	      locy := zx;
	      quan := 1;
	      class := potions;
	      olet := '!';
	      potion := PotionOfFrobozz;
	      Magical := True;
	      f^.levl[locx, locy].ScrSym := '!';
	    END;
	  MkObj(wands);
	  WITH f^.listof[objects]^ DO
	    BEGIN
	      locx := zy;
	      locy := zx;
	      quan := 1;
	      spe := 3;
	      wand := WandOfFrobozz;
	      Magical := True;
	      f^.levl[locx, locy].ScrSym := '/';
	    END;
	END;
    END  (* MakeMaz *);

  PROCEDURE PutObject(x:xval;y:yval;TheThing: ObjectClass);
    BEGIN
      IF TheThing = anything THEN
	MakeSomeObject
      ELSE
	MkObj(TheThing);
      WITH  f^.listof[objects]^  DO
	BEGIN
	  locx := x;
	  locy := y;
	  f^.levl[locx,locy].ScrSym := olet
	END;
    END;

  PROCEDURE DoDoor(x:xval;y:yval);

    BEGIN
      IF [ f^.levl[x-1,y].typ, f^.levl[x+1,y].typ,
	   f^.levl[x,y+1].typ, f^.levl[x,y-1].typ ] * [door,sdoor] = []  THEN
	WITH  f^.levl[x,y]  DO
	  IF typ = wall THEN
	    IF rn2(8) THEN
	      typ := sdoor
	    ELSE
	      BEGIN
		typ := door;
		ScrSym := '+';
		Room := OrdinaryRoom;
	      END;
    END;

  PROCEDURE newloc; FORWARD;
    (* mkpos and newloc are mutually recursive *)

  PROCEDURE MkPos;

    BEGIN  (* MkPos *)
      croom := room[icroom];
      troom := room[itroom];
      IF  (troom.hx < 0) OR (croom.hx < 0)  THEN
	(*return*)
      ELSE
	BEGIN
	  IF  troom.lx > croom.hx  THEN
	    BEGIN
	      x := croom.hx + 1;
	      dx := 1;
	      y := rn1(croom.hy - croom.ly, croom.ly);
	      dy := 0;
	      tx := troom.lx - 1;
	      ty := troom.ly + rnd(troom.hy - troom.ly) - 1;
	    END
	  ELSE
	    IF  troom.hy < croom.ly  THEN
	      BEGIN
		y := croom.ly - 1;
		dy := -1;
		dx := 0;
		x := croom.lx + rnd(croom.hx - croom.lx) - 1;
		tx := troom.lx + rnd(troom.hx - troom.lx) - 1;
		ty := troom.hy + 1;
	      END
	    ELSE
	      IF  troom.hx < croom.lx  THEN
		BEGIN
		  x := croom.lx - 1;
		  dx := -1;
		  dy := 0;
		  tx := troom.hx + 1;
		  y := croom.ly + rnd(croom.hy - croom.ly) - 1;
		  ty := troom.ly + rnd(troom.hy - troom.ly) - 1;
		END
	      ELSE
		BEGIN
		  y := croom.hy + 1;
		  dy := 1;
		  dx := 0;
		  x := croom.lx + rnd(croom.hx - croom.lx) - 1;
		  tx := troom.lx + rnd(troom.hx - troom.lx) - 1;
		  ty := troom.ly - 1;
		END;
	  IF  f^.levl[x+dx,y+dy].typ <> empty  THEN
	    IF  nxcor <> 0  THEN
	      newloc
	    ELSE
	      BEGIN
		DoDoor(x,y);
		x := x + dx;
		y := y + dy
	      END
	  ELSE  DoDoor(x,y);
	END;
    END  (* MkPos *);

  PROCEDURE NewLoc;

    LABEL 9;

    BEGIN
      icroom := icroom + 1;
      itroom := itroom + 1;
      IF  (nxcor<>0) OR (room[ icroom ].hx < 0)
	  OR (room[ itroom ].hx < 0)  THEN
	BEGIN
	  IF  nxcor > rn1(nroom,4)  THEN
	    BEGIN
	      icroom := nroom;
	      nxcor := nxcor + 1;
	      GOTO 9
	    END
	  ELSE
	    BEGIN
	      nxcor := nxcor + 1;
	      REPEAT
		icroom := rnd0(nroom);
		itroom := rnd0(nroom);
	      UNTIL (icroom <> itroom) AND ((itroom <> icroom+1)
					    OR NOT rn2(3))
	    END
	END;
      mkpos;
      9:
    END  (* NewLoc *);

  FUNCTION  MakeCor(nx,ny:byte) : Boolean;

    LABEL 9;

    VAR
      dix, diy : small;

    BEGIN
      MakeCor := True;
      IF  (nxcor <> 0) AND rn2(35)  THEN
	newloc
      ELSE
	BEGIN  (* Make some corridor *)
	  dix := Abs(nx-tx);
	  diy := Abs(ny-ty);
	  IF  (nx=xmin) OR (nx=xmax) OR
	      (ny=xmin) OR (ny=ymax)  THEN
	    BEGIN
	      IF  nxcor <> 0  THEN
		NewLoc
	      ELSE
		MakeCor := False;
	      GOTO 9
	    END;
	  IF  (dy <> 0) AND (dix > diy)  THEN
	    BEGIN
	      dy := 0;
	      IF  nx > tx  THEN  dx := -1
			   ELSE  dx := +1
	    END
	  ELSE
	    IF  (dx <> 0) AND (diy > dix)  THEN
	      BEGIN
		dx := 0;
		IF  ny > ty  THEN  dy := -1
			     ELSE  dy := +1
	      END;

	  WITH  f^.levl[nx,ny]  DO
	    IF  (typ = empty) OR (typ = corr)  THEN
	      BEGIN
		IF  typ = empty  THEN
		  BEGIN
		    typ := corr;
		    ScrSym := '#';
		    Room := OrdinaryRoom;
		  END;
		x := nx;
		y := ny
	      END
	    ELSE
	      IF  (nx=tx) AND (ny=ty)  THEN
		BEGIN
		  dodoor(nx,ny);
		  newloc
		END
	      ELSE
		IF  (x+dx <> nx) OR (y+dy <> ny)  THEN
		  (* Return *)
		  ELSE
		    IF  dx <> 0  THEN
		      BEGIN
			IF  ty < ny  THEN
			  dy := -1
			ELSE
			  IF  f^.levl[ nx+dx,ny-1 ].typ = RoomLocation  THEN
			    dy := 1
			  ELSE
			    dy:=-1;
			dx := 0
		      END
		    ELSE
		      BEGIN
			IF  tx < nx  THEN
			  dx := -1
			ELSE
			  IF  f^.levl[ nx-1,ny+dy ].typ = RoomLocation  THEN
			    dx := 1
			  ELSE
			    dx:=-1;
			dy := 0
		      END
	END  (* Make some corridor *);
      9:
    END  (* MakeCor *);

  FUNCTION  Maker(lowx,hix,lowy,hiy:small) : Boolean;

    LABEL 999; (* for return *)

    VAR
      tmpx, ltmp : byte;
      I, RoomTypeNo: unsigned;
      RoomType : TypeOfRoom;
      RoomLocationSymbol : Char;

    PROCEDURE Make(x : xval; y : yval; t : LocationType; c : Char);

      BEGIN
	WITH  f^.levl[x,y]  DO
	  BEGIN
	    ScrSym := c;
	    typ := t;
	  END;
      END  (* Make *);

  BEGIN
    IF  hix > 75  THEN  hix := 75;
    IF  hiy > 18  THEN  hiy := 18;
    FOR  tmpx := lowx-4  TO  hix+4  DO   FOR  ltmp := lowy-3  TO  hiy+3  DO
      IF  f^.levl[tmpx,ltmp].typ <> empty  THEN
	BEGIN
	  maker := False;
	  GOTO 999 
	END;
    IF  10 > rnd(dlevel)  THEN		(* Make the room lit *)
      FOR  tmpx := lowx-1  TO  hix+1  DO   FOR  ltmp := lowy-1  TO  hiy+1  DO
	f^.levl[tmpx,ltmp].lit := True;
    IF  dlevel > 7  THEN (* The dangerous rooms begin here *)
      BEGIN 
	IF rn2(10 - (dlevel DIV 7)) THEN
	  BEGIN
	    RoomTypeNo := rnd(5);
	    RoomType := OrdinaryRoom;
	    FOR I := 1 TO RoomTypeNo DO
	      RoomType := Succ(RoomType);
	  END
	ELSE
	  RoomType := OrdinaryRoom;
      END
    ELSE 
      RoomType := OrdinaryRoom;
    FOR  tmpx := lowx-1  TO  hix+1  DO
      FOR  ltmp := lowy-1  TO  hiy+1  DO
	BEGIN
	  f^.levl[tmpx,ltmp].Recognized := False;
	  f^.levl[tmpx,ltmp].room := RoomType;
	END;

    WITH  room[ icroom ]  DO
      BEGIN
	lx := lowx;
	hx := hix;
	ly := lowy;
	hy := hiy 
      END;
    icroom := icroom + 1;
    tmpx := lowx - 1;
    make(tmpx,lowy-1,wall,'-');
    FOR  ltmp := lowy  TO  hiy  DO
      make(tmpx,ltmp,wall,'|');
    make(tmpx,hiy+1,wall,'-');
    FOR  tmpx := lowx  TO  hix  DO
      BEGIN
	make(tmpx,lowy-1,wall,'-');
	IF RoomType = WaterRoom THEN
	  RoomLocationSymbol := '~'
	ELSE
	  RoomLocationSymbol := '.';
	FOR  ltmp := lowy  TO  hiy  DO
	  make(tmpx,ltmp,RoomLocation,RoomLocationSymbol);
	make(tmpx,hiy+1,wall,'-')
      END;
    tmpx := hix + 1;
    make(tmpx,lowy-1,wall,'-');
    FOR  ltmp := lowy  TO  hiy  DO
      make(tmpx,ltmp,wall,'|');
    make(tmpx,hiy+1,wall,'-');
    nroom := nroom + 1;   
    maker := True;
  999:
  END  (* Maker *);

  PROCEDURE SortRooms;

    VAR
      i, j   : RoomNumber;
      x      : mkroom;
      sorted : Boolean;

    BEGIN
      (* Exchange sort*)
      i := 0;
      REPEAT
	i := i + 1;
	sorted := True;
	FOR  j := nroom-1  DOWNTO  i  DO
	  IF  room[ j-1 ].lx > room[ j ].lx  THEN
	    BEGIN
	      sorted := False;
	      x := room[j-1];
	      room[ j-1 ] := room[ j ];
	      room[ j ] := x
	    END
      UNTIL sorted
    END  (* SortRooms *);

  PROCEDURE GenerateTreasureRoom;

    VAR
      x : xval;
      y : yval;
     
    BEGIN
      WITH  room[ icroom ]  DO
	FOR  x := lx  TO  hx  DO    FOR  y := ly  TO  hy  DO
	  BEGIN
	    IF  rn2(5)  THEN
	      PutObject(x,y,anything);
	    IF  rn2(4)  THEN
             IF NOT ((x = f^.xupstair) AND (y = f^.yupstair)) THEN
	      BEGIN
		MakeMonster(NoMonster, False);
		WITH  f^.listof[monsters]^  DO
		  BEGIN
		    locx := x;
		    locy := y;
		    mstat := GuardingTreasures
		  END
	      END  (* IF  rn2(4) *)
	  END  (* WITH & FOR *)
    END  (* GenerateTreasureRoom *);

  PROCEDURE GenerateMonsterNest;

    VAR
      x   : xval;
      y   : yval;
      tmp : -1..maxbyte;
      MonsterWhoLivesHere : MonsterSpecies;
      TheThing: ObjectClass;
      Hmmm,I: Integer;

    BEGIN
      REPEAT
	(* Select a row (tmp) of MonsterTable as follows:
	   At dungeon levels 1..3 there may nests of monsters from
	   the first row of the MonsterTable; at levels 4..6 from rows
	   0 or 1 (with equal probabilities); at levels 7..9 the row
	   is a random number from the range 0..2, at levels
	   10..12 from the range 1..3, etc. If the row number
	   so drawn would be too large (>mtabM), the actual row
	   number is set to mtabM or mtabM-1. *)
 	IF dlevel <= 3 THEN
	  IF Rn2(5) THEN tmp := 1 ELSE tmp := 0
	ELSE
	  BEGIN
	    tmp := (dlevel-1) DIV 3 - 1 - Ord(NOT (rn2(5))) + rnd0(3);
	    IF  tmp < 0  THEN
	      tmp := 0
	    ELSE 
	      IF dlevel < 30 THEN
		BEGIN 
		  IF  tmp >= mtabM  THEN
		    tmp := mtabM - rnd(2)
		END
	      ELSE
		IF tmp > mtabM THEN
		  tmp := mtabM - rnd0(2);
	  END;
 	(* Now pick up a a monster from the row at random:*)
	MonsterWhoLivesHere := MonsterTable[tmp,rnd0(mtabN+1)];
	UNTIL ((NOT (MonsterWhoLivesHere IN genocided) ) AND 
	      NOT ((dlevel <= 36) AND (MonsterWhoLivesHere IN [ destroyer,
								TyrannoSaur,
								humanoid ])));
      Hmmm:= Rnd(8);
      IF Hmmm > 4 THEN
	Hmmm:= Rnd(8);
      TheThing:= Anything;
      FOR I := 1 TO Hmmm DO
	TheThing := Succ(TheThing);
      
      WITH  room[icroom]  DO
	FOR  x := lx  TO  hx  DO
	  FOR  y := ly  TO  hy  DO
	    BEGIN
	      IF  rn2(4 + Ord(TheThing = wands) - Ord(dlevel > 24))  THEN
		PutObject(x,y,TheThing);
	      IF  rn2(4 - Ord((TheThing = scrolls) OR (TheThing = potions)) -
		      2 * Ord(TheThing = wands) - Ord(dlevel > 24)) THEN
               IF NOT ((x = f^.xupstair) AND (y = f^.yupstair)) THEN
		BEGIN
		  MakeMonster(MonsterWhoLivesHere, False);
		  WITH  f^.listof[monsters]^  DO
		    BEGIN
		      locx  := x;
		      locy  := y;
		      mstat := guardingtreasures;
		      IF Rn2(10 - Hmmm) THEN mspeed := mfast
		    END;
		END;
	    END (*FOR,WITH*);
    END  (* GenerateMonsterNest *);
	
  PROCEDURE CreateWorldOfGiants;

    VAR
      i : xval;
      j : yval;
      k : unsigned;
      TheGiant : MonsterSpecies;
      StateOfGiants : MonsterState;

    PROCEDURE GetCoordinates(VAR x: xval; VAR y: yval);

      BEGIN
	REPEAT
	  x := rnd(xmax);
	  y := rnd(ymax);
	UNTIL f^.levl[x,y].typ <> empty;
      END;

    BEGIN(* CreateWorldOfGiants *)
      pline;
      out('You here somebody whispering: ');
      pline;
      out('No one has returned alive from the world of GIANTS');
      more;
      initialize;
      f^.ThisIsAMaze := False;
      FOR i := xmin+1 TO xmax-1 DO
	IF (i - 1) MOD 3 = 0 THEN 
	  FOR j := ymin+1 TO ymax-1 DO
	    WITH f^.levl[i, j] DO 
	      BEGIN
		typ := corr;
		ScrSym := '#';
		Room := OrdinaryRoom;
	      END;

      FOR j := ymin+1 TO ymax-1 DO
	IF (j - 1) MOD 3 = 0 THEN 
	  FOR i := xmin+1 TO xmax-1 DO
	    WITH f^.levl[i, j] DO 
	      BEGIN
		typ := corr;
		ScrSym := '#';
		Room := OrdinaryRoom;
	      END;

      IF WearingRingOfFrobozz THEN
	IF KnownFrobozz <> [ FrobozzPotion, FrobozzScroll, FrobozzWand,
			     FrobozzRing ] THEN
	  StateOfGiants := GuardingTreasures
        ELSE 
	  StateOfGiants := sleep
      ELSE
	StateOfGiants := NormalState;

      FOR TheGiant := Bat TO humanoid DO
	BEGIN
	  MakeMonster(TheGiant, True);
	  WITH  f^.listof[monsters]^  DO
	    BEGIN
	      GetCoordinates(i,j);
	      locx := i;
	      locy := j;
	      mstat := StateOfGiants;
	    END
	END;
      
      FOR k := 1 TO 50 DO
	BEGIN
	  Getcoordinates(i,j);
	  MkGold(0, i, j);
	END;

      f^.xupstair := 4 + 3 * (rnd0(2) - 1);
      f^.yupstair := 4 + 3 * (rnd0(2) - 1);
      WITH f^.levl[f^.xupstair,f^.yupstair] DO
	BEGIN
	  ScrSym := upwards;
	  typ := RoomLocation;
	  Room := OrdinaryRoom;
	END;
      f^.xdnstair := 73 + 3 * (rnd0(2) - 1);
      f^.ydnstair := 16 + 3 * (rnd0(2) - 1);
      WITH f^.levl[f^.xdnstair,f^.ydnstair] DO
	BEGIN
	  ScrSym := downwards;
	  typ := RoomLocation;
	  Room := OrdinaryRoom;
	END;
      u.ux := f^.xupstair;
      u.uy := f^.yupstair;
    END(* CreateWorldOfGiants *);

  PROCEDURE CreateThePlaceForTheAmulet;

    CONST
      ThePlaceForWall = xmax DIV 2 + 9;

    VAR 
      i : xval;
      j, ThePlaceForDoor : yval;
      k : unsigned;
      otmp : thing;

    BEGIN(* CreateThePlaceForTheAmulet *)
      Initialize;
      f^.ThisIsAMaze := False;
      FOR i := xmin+1 TO ThePlaceForWall - 1 DO
	IF (i - 1) MOD 2 = 0 THEN 
	  FOR j := ymin+1 TO ymax-1 DO
	    WITH f^.levl[i, j] DO 
	      BEGIN
		typ := corr;
		ScrSym := '#';
		Room := OrdinaryRoom;
	      END;

      FOR j := ymin+1 TO ymax-1 DO
	IF (j - 1) MOD 2 = 0 THEN 
	  FOR i := xmin+1 TO ThePlaceForWall - 1 DO
	    WITH f^.levl[i, j] DO 
	      BEGIN
		typ := corr;
		ScrSym := '#';
		Room := OrdinaryRoom;
	      END;

      FOR i := ThePlaceForWall TO xmax - 1 DO
	BEGIN 
	  WITH f^.levl[i, 1] DO
	    BEGIN
	      Typ := wall;
	      lit := True;
	      ScrSym := '-';
	      Room := OrdinaryRoom;
	    END;
	  WITH f^.levl[i, ymax-1] DO
	    BEGIN
	      Typ := wall;
	      lit := True;
	      ScrSym := '-';
	      Room := OrdinaryRoom;
	    END;
	END;

      FOR i := 1 TO ymax - 1 DO
	BEGIN
	  WITH f^.levl[ThePlaceForWall, i] DO
	    BEGIN
	      Typ := wall;
	      lit := True;
	      ScrSym := '|';
	      Room := OrdinaryRoom;
	    END(* WITH *);
	  WITH f^.levl[xmax-1, i] DO
	    BEGIN
	      Typ := wall;
	      lit := True;
	      ScrSym := '|';
	      Room := OrdinaryRoom;
	    END(* WITH *);
	END;
      ThePlaceForDoor := rnd(ymax - 2) + 1;
      f^.levl[ThePlaceForWall, ThePlaceForDoor].typ := sdoor;

      FOR i := ThePlaceForWall + 1 TO xmax - 2 DO
	FOR j := 2 TO ymax - 2 DO
	  WITH f^.levl[i,j] DO
	    BEGIN
	      Typ := RoomLocation;
	      Lit := True;
	      ScrSym := '.';
	      Room := OrdinaryRoom;
	      Recognized := False;
	    END(* WITH *);
      f^.xupstair := 1;
      f^.yupstair := rnd(ymax-1);
      WITH f^.levl[f^.xupstair,f^.yupstair] DO
	BEGIN
	  ScrSym := upwards;
	  typ := RoomLocation;
	  Room := OrdinaryRoom;
	END;
      f^.xdnstair := 0;
      f^.ydnstair := 0;

      Create(otmp, objects);
      WITH otmp^ DO
	BEGIN
	  class := amulet;
	  olet := AmuletChar;
	  locx := xmax - 5;
	  locy := ymax DIV 2;
	  quan := 1;
	  spe := 1;
	  minus := False;
	  cursed := False;
	  known := False;
	  Magical := True;
	  next := NIL;
	  f^.levl[locx, locy].ScrSym := AmuletChar;
	END(* WITH *);
      AddToList(otmp, f^.listof[objects]);
      FOR i := 1 TO 20 DO
	BEGIN 
	  MakeMonster(humanoid, True);
	  f^.listof[monsters]^.mstat := tamed;
	  rloc(f^.listof[monsters]);
	END;
      u.ux := f^.xupstair;
      u.uy := f^.yupstair;
    END(* CreateThePlaceForTheAmulet *);

  BEGIN  (* MkLev *)
    IF (YourExperienceInZAP = Novice) AND (dlevel > 15) THEN
      BEGIN
	pline;
	out('Congratulations! You seem to be quite a fair zapper!');
	more;
	YourExperienceInZAP := Fair;
      END;
    IF (YourExperienceInZAP = Fair) AND (dlevel > 21) THEN
      BEGIN
	pline;
	out('Congratulations! You seem to be quite a good zapper!');
	more;
	YourExperienceInZAP := Good;
      END;
    IF (YourExperienceInZAP = Good) AND (dlevel > 30) THEN
      BEGIN
	pline;
	out('Congratulations! You seem to be really an expert zapper!!');
	more;
	YourExperienceInZAP := Expert;
      END;
    IF (YourExperienceInZAP = Expert) AND (dlevel > 36) THEN
      BEGIN
	pline;
	out('Congratulations! You seem to be really a wizard zapper!');
	more;
	YourExperienceInZAP := Wizard;
      END;
    
    MakingLevel := True;
    tries := 0;
    f^.listof[monsters] := NIL;
    f^.listof[golds] := NIL;
    f^.listof[traps] := NIL;
    f^.listof[objects] := NIL;
    f^.NumberOfRats := 0;
    IF dlevel = WorldOfGiants THEN
      BEGIN
	CreateWorldOfGiants;
	GOTO 999;
      END;
    IF dlevel = LevelWhereTheAmuletIs THEN
      BEGIN
	CreateThePlaceForTheAmulet;
	GOTO 999;
      END;
    IF  flags.next  THEN
      BEGIN
	makemaz;
	flags.next := False;
	GOTO 999
      END
    ELSE
      f^.ThisIsAMaze := False;
    IF dlevel < 31 THEN
      flags.next := dlevel = 28
    ELSE
      flags.next := ((dlevel + 1) MOD 6) = 0;
  0:
    tries := 0;
    nroom := 0;
    icroom := 0;
    counter := 0;
    initialize;

    (* Generate rooms. *)
    WHILE  (nroom < 7) AND (counter <= 100)  DO
      (* Counter prevents inf. loops *)
      BEGIN
	counter := Succ(counter);
	lowy := rn1(3,3);
	REPEAT
	  lowx := rn1(3,4);
	  REPEAT
	    lowy := lowy + rand MOD 5 - 2;
	    IF  lowy < 3  THEN
	      lowy := 3
	    ELSE
	      IF  lowy > 16  THEN
		lowy := 16;
	    IF  f^.levl[lowx,lowy].typ = empty  THEN
	      BEGIN
		MakeRoom := maker(lowx,rn1(9,lowx+2),lowy,rn1(4,lowy+2));
		IF  MakeRoom AND (nroom>13)  THEN
		  GOTO 100
	      END;
	    lowx := lowx + rn1(2,7)
	  UNTIL lowx >= 70;
	  lowy := lowy + rn1(2,4)
	UNTIL lowy >= 15
      END;

   100:
      room[icroom].hx := -1;

      (* Generate staircase downwards. *)
      REPEAT
	icroom := rand MOD nroom;
	f^.xdnstair := somex;
	f^.ydnstair := somey
      UNTIL (Odd(f^.xdnstair) AND Odd(f^.ydnstair)) OR flags.next;
      f^.levl[f^.xdnstair,f^.ydnstair].ScrSym := downwards;
	  
      (* Generate staircase upwards. *)
      itroom := icroom;
      REPEAT
	icroom := rand MOD nroom;
	f^.xupstair := somex;
	f^.yupstair := somey;
      UNTIL (icroom <> itroom);
      f^.levl[f^.xupstair,f^.yupstair].ScrSym := upwards;

      (* Generate monsters, traps ... into each room.*)
      icroom := 0;
      REPEAT
	IF dlevel < 30 THEN 
	  TreasureRoom := rn2(6)
	ELSE
	  TreasureRoom := rn2(12);
	IF  TreasureRoom  THEN
	  IF  rn2(2)  THEN
	    GenerateTreasureRoom (* or something else .... *)
	  ELSE
	    GenerateMonsterNest 
	ELSE
	  IF  rn2(3 - Ord(dlevel > 24) - Ord(dlevel > 30))  THEN
	    BEGIN
	      MakeMonster(NoMonster, False);
	      WITH  f^.listof[monsters]^  DO
		BEGIN
		  REPEAT
		    locx := somex;
		    locy := somey;
		  UNTIL NOT ((locx = f^.xupstair) AND (locy = f^.yupstair));
		  mstat := sleep
		END
	    END;
	tries := 0;
	IF dlevel < 30 THEN 
	  WHILE  rn2(8 - dlevel DIV 5)  DO
	    MkTrap(SomeTrap)
	ELSE
	  WHILE NOT rn2(2 + (dlevel - 30) DIV 4) DO
	    MkTrap(SomeTrap);
	IF  tries = 0  THEN
	  IF  rn2(3 - Ord(dlevel > 24) - Ord(dlevel > 30))  THEN
	    MkGold(0,somex,somey);
	IF  NOT TreasureRoom  THEN
	  IF  rn2(3 - Ord(dlevel > 24) - Ord(dlevel > 30))  THEN
	    BEGIN
	      PutObject(somex,somey,anything);
	      WHILE  rn2(5)  DO
		PutObject(somex,somey,anything)
	    END;
	icroom := icroom + 1
      UNTIL room[icroom].hx <= 0;

      (* Generate corridors. *)
      SortRooms;
      icroom := 0;
      itroom := 1;
      nxcor := 0;
      MkPos;
      counter := 0;
      REPEAT
	IF  NOT MakeCor(x+dx,y+dy)  THEN
	  GOTO  0;
	IF  nxcor <> 0  THEN
	  counter := counter + 1;
	croom := room[icroom];
	troom := room[itroom]
      UNTIL (croom.hx <= 0) OR (troom.hx <= 0) OR (counter>=250);

    999:
      u.ux := f^.xupstair;
      u.uy := f^.yupstair;
      MakingLevel := False;
  END  (* MkLev *);

PROCEDURE done(status:EndStatus);
  LABEL 99;
  VAR
    otmp : thing;

  BEGIN
    TheWayItEnded := status;
    IF status <> died THEN killer := NoMonster (*dummy*);
    ClearScreen;
    out('Goodbye '); out0(YourName); out(' ...');
    NewLine; NewLine;
    IF status = escaped THEN
      BEGIN
	u.urexp := u.urexp + 150;
	otmp := invent;
	WHILE otmp <> NIL DO
	  WITH otmp^ DO
	    BEGIN
	      IF class = gems THEN
		u.urexp := u.urexp + quan*200
	      ELSE
		IF class = amulet THEN
		  u.urexp := u.urexp + 500000;
	      IF magical THEN
		u.urexp := u.urexp + 50000;
	      otmp := next;
	    END;
	out('You escaped from the dungeon');
      END
    ELSE
      BEGIN
	out('You ');
	IF status = quit THEN
	  out(litQuit)
	ELSE
	  BEGIN
	    out('died');
	    u.urexp := u.urexp - u.urexp DIV 10
	    (* pay funeral expenses to TeKoLa *)
	  END;
	out(' on dungeon level ');
	WriteNumber(dlevel,1);
      END;
    out(' with '); WriteNumber(u.urexp,1); out(' points');
    NewLine;
    out('and '); WriteNumber(u.ugold,1);out(' pieces of gold, after ');
    WriteNumber(f^.moves,1);out(' moves.');
    NewLine;
    out('You were at experience level ');WriteNumber(u.ulevel,1);
    out(' with a maximum of ');WriteNumber(u.uhpmax,1);
    out(' hit points');
    NewLine;
    out('when you ');
    CASE status OF
      escaped : out(litEscaped);
      quit    : out(litQuit);
      died    : IF killer = drowned THEN
		  out('drowned')
		ELSE
		  BEGIN 
		    out('were ');
		    IF u.uswallow THEN
		      BEGIN
		        TheWayItEnded := digested;
		        out('digested by ');
		      END
		    ELSE
		      out('killed by ');
		    WriteEnemy(killer,IndefiniteArticle);
		  END;
    END  (* CASE status *);
    OutCh('.');
    NewLine;
    IF  NOT GetRet(True)  THEN  GOTO 99;
    ClearScreen;
    GetOwnScoreFile('ZAP.SCORE',YourTopTen);
    OpenWithSingleAccess(YourTopTen);
    TopTenList(YourTopTen,True);
    Close(YourTopTen);
    IF  NOT GetRet(True)  THEN  GOTO 99;
    NewLine; cury := 0;
    IF  GetJfnForSystemScores('ZAP.SYSTEM-SCORE',SystemTopTen)  THEN
      BEGIN
	OpenWithSingleAccess(SystemTopTen);
	TopTenList(SystemTopTen,False);
	Close(SystemTopTen)	(* To keep  others from waiting. *)
      END
    ELSE
      out('?No systemscores available');
  99: GOTO 9999;
  END;

PROCEDURE doname(Obj: Thing); FORWARD;

FUNCTION weight(O: Thing): unsigned; FORWARD;

PROCEDURE LoseHp(n:integer; cause:enemy);

  BEGIN
    u.uhp := u.uhp - n;
    flags.dhp := True;
    IF u.uhp <= 0 THEN
      killer := cause;
  END;

PROCEDURE LoseStr(num:byte; en: enemy);

  VAR
    weit,NumberOfObjects,tmp: unsigned;
    otmp	: Thing;

 BEGIN
   u.ustr := u.ustr - num;
   YouCanCarry := YourCarryAbility(u.ustr);
   IF YouCanCarry < MinWeight THEN
     YouCanCarry := MinWeight;
   WHILE u.ustr < 3 DO
     BEGIN
       u.ustr := 3;
       LoseHp(6, en);
       u.uhpmax := u.uhpmax - 6;
       flags.dhp := True; flags.dhpmax := True;
     END;
   weit := 0;
   NumberOfObjects := 0;
   otmp := invent;
   WHILE  otmp <> NIL  DO
     BEGIN
       weit := weit + weight(otmp);
       NumberOfObjects := Succ(NumberOfObjects);
       otmp := otmp^.next
     END;
   IF  (weit > YouCanCarry)  THEN
     BEGIN
       pline;
       out('You can''t carry everything any more.');
       WHILE weit > YouCanCarry DO
	 BEGIN
	   tmp := rnd(NumberOfObjects);
	   otmp := invent;
	   WHILE  tmp > 1  DO
	     BEGIN
	       otmp := otmp^.next;
	       tmp := Pred(tmp)
	     END;
	   (* OTMP is the thing that happens to drop *)
	   (* We assume, that You cannot drop something You are wearing *)
	   (* Lets hope you can carry uwep, uarm, uleft & uright, else
	      it LOOPs, LOOPs, LOOPs, LOOPs, LOOPs... *)
	    WHILE ((otmp = uarm) OR (otmp = uleft) OR (otmp = uright)) DO
	      BEGIN
		otmp := otmp^.next;
		IF otmp = NIL THEN
		  otmp := invent;
	      END;
	    IF  otmp = uwep  THEN
	      uwep := NIL;
	    remove(otmp, invent);
	    AddToList(otmp, f^.listof[objects]);
	    WITH  otmp^  DO
	      BEGIN
		locx := u.ux;
		locy := u.uy
	      END;
	    pline;
	    doname(otmp);
	    out(' dropped down from You! ');
	    weit := weit - weight(otmp);
	    NumberOfObjects := NumberOfObjects - 1;
	 END(* WHILE *);
     END;
   flags.dstr := True;
 END;

PROCEDURE IncrementUserLevel(AvoidSmallIncrement:Boolean);
  CONST MaxHpIncrease = 10;
  VAR tmp:1..MaxHpIncrease;
 BEGIN
   pline; out(welcome);
   u.ulevel := u.ulevel + 1;
   WriteNumber(u.ulevel,1);
   tmp := rnd(MaxHpIncrease);
   IF AvoidSmallIncrement THEN
     IF tmp < 3 THEN tmp := rnd(MaxHpIncrease);
   u.uhpmax := u.uhpmax + tmp;
   u.uhp := u.uhp + tmp;
   flags.dhp := True; flags.dhpmax := True; flags.dulev := True;
 END;

PROCEDURE LosExp;
  VAR num:1..10;
 BEGIN
  IF u.ulevel > 1 THEN
   BEGIN
   pline;out('Goodbye level ');WriteNumber(u.ulevel,1);OutCh('.');
   u.ulevel := u.ulevel - 1;
   num := rnd(10);
   u.uhp := u.uhp - num;
   u.uhpmax := u.uhpmax - num;
   u.uexp := 10*pow(u.ulevel-1) - 1;
   flags.dhp := True; flags.dhpmax := True;
   flags.dulev := True; flags.dexp := True;
   END;
 END;

FUNCTION  MoveCm(Cmd : Char) : Boolean;

  BEGIN
    dx := 0;
    dy := 0;
    IF  cmd IN [ 'h', 'j', 'k', 'l', 'y', 'u', 'b', 'n' ]  THEN
      BEGIN
	MoveCm := True;
	CASE  cmd  OF
	  'h' : dx := -1;
	  'j' : dy := +1;
	  'k' : dy := -1;
	  'l' : dx := +1;
	  'y' : BEGIN dx := -1; dy := -1 END;
	  'u' : BEGIN dx := +1; dy := -1 END;
	  'b' : BEGIN dx := -1; dy := +1 END;
	  'n' : BEGIN dx := +1; dy := +1 END;
	END (*CASE*);
      END
    ELSE  MoveCm := False;
  END  (* MoveCm *);

FUNCTION hitu(mlev, dam:short; name:enemy):Boolean;
  (* Although this routine is a function, it is often called
     as a procedure: IF hitu(...) THEN;
     The first parameter affects the probability of hitting,
     whereas the second specifies how many hit points you
     lose if the enemy hits. *)

  VAR
    tmp : byte;

  BEGIN  (* HitU *)
    IF  u.uhp <= 0  THEN  (* you are already dead *)
      hitu := False
    ELSE
      BEGIN
	WriteEnemy(name,start);
	tmp := -1 + u.uac + mlev;
	IF dlevel > 24 THEN tmp := tmp + (dlevel - 24) DIV 2;
	IF  multi < 0  THEN  tmp := tmp + 4;
	IF  u.uinvis > 0  THEN  tmp := tmp - 4;
	IF  tmp < rnd(20)  THEN
	  BEGIN
	    out('misses.');
	    hitu := False;
	  END
	ELSE
	  BEGIN
	    out('hits!');
	    IF  dam > 0  THEN
	      BEGIN
		dam := dam + (u.uac - 6) DIV 2 + Ord(dlevel > 30) *
		       (dlevel - 30) DIV 2;
		IF  dam < 1  THEN
		  dam := 1;
		LoseHp(dam,name);
	      END;
	    hitu := True;
	  END;
      END;
  END  (* HitU *);

PROCEDURE mnexto(mtmp:thing; whatx: xval; whaty: yval);
(* Make monster mtmp next to <whatx,whaty>. Puts it anywhere if there isn't
   room there. If the monster is CanSee in the old location do
   NewSym, else do News1 *)

  CONST
    n = 100; (*was 14 but that wasn't enough for rats... *)
    MaxRange = 20;
  TYPE
    index = 0..n;
  VAR
    foo:ARRAY[index] OF RECORD zx:xval; zy:yval END;
    tfoo:index;
    range: small;
    x,y:Byte;

  FUNCTION test(x,y:Byte):Boolean;
   BEGIN
     IF (x < 1) OR (x > 78) OR (y < 1) OR (y > 20) THEN
       test := False
     ELSE IF f^.levl[x,y].typ < door THEN
       test := False
     ELSE IF (x = u.ux) AND (y = u.uy) THEN
       test := False
     ELSE
       test := ThingAt(x,y,monsters) = NIL;
   END;
 BEGIN
   tfoo := 0;
   range := 1;
   REPEAT
     FOR x := whatx - range TO whatx + range DO
      BEGIN
       IF test(x,whaty-range) THEN
	 BEGIN WITH foo[tfoo] DO BEGIN zx:=x; zy:=whaty-range END;
	 tfoo := tfoo + 1 END;
      IF test(x,whaty+range) THEN
	 BEGIN WITH foo[tfoo] DO BEGIN zx:=x; zy:=whaty+range END;
	 tfoo := tfoo + 1 END;
      END;
     FOR y := whaty - (range - 1) TO whaty + (range - 1) DO
      BEGIN
       IF test(whatx-range,y) THEN
	 BEGIN WITH foo[tfoo] DO BEGIN zx:=whatx-range; zy:=y END;
	 tfoo := tfoo + 1 END;
       IF test(whatx+range,y) THEN
	 BEGIN WITH foo[tfoo] DO BEGIN zx:=whatx+range; zy:=y END;
	 tfoo := tfoo + 1 END;
      END;     
     range := range + 1;
   UNTIL (tfoo <> 0) OR (Range > MaxRange);
   WITH mtmp^ DO
     BEGIN				(* remove the old symbol *)
       IF f^.levl[ locx, locy].CanSee THEN
	 NewSym(locx, locy)
       ELSE
	 News1(locx, locy);
      END;
   IF Range <= MaxRange THEN
     WITH foo[rnd0(tfoo)], mtmp^ DO
       BEGIN
	 locx := zx; locy := zy;
	 IF f^.levl[locx,locy].CanSee THEN
	   pmon(mtmp);
       END
   ELSE
     delete(mtmp,f^.listof[monsters]);
 END (*mnexto*);



PROCEDURE NoMul(Nval : Integer);

  BEGIN
    IF multi >= 0 THEN
      BEGIN
	IF flags.mv THEN pru;
	multi := nval;
	flags.mv := False;
	flags.see := False;
      END;
  END;

PROCEDURE CreateSomeMonsterAndPutItBesideU(WhatMonster: MonsterSpecies);

  BEGIN
    MakeMonster(WhatMonster, False);
    mnexto(f^.listof[monsters],u.ux,u.uy);
  END;

PROCEDURE MonsterDropsAnObjectItHasBeenCarrying(mtmp : thing);

  VAR tmp : thing;

  BEGIN
    WITH mtmp^.ObjectsCarried^ DO
      BEGIN
	IF (what = golds) THEN  
	  mtmp^.mstat := normalState; (* Don't flee anymore... *)
    	tmp := next;
	AddToList(mtmp^.ObjectsCarried, f^.listof[what]);
        locx := mtmp^.locx;
	locy := mtmp^.locy;
      END;
    mtmp^.ObjectsCarried := tmp;
  END  (* MonsterDropsAnObjectItHasBeenCarrying *);

PROCEDURE YouAreNoMoreStuckBy(monster:thing);

  BEGIN
    IF  (u.ustuck = monster) AND (monster <> NIL)  THEN
      BEGIN
	IF  u.uswallow  THEN
	  BEGIN 
	    u.uswallow := False;
	    pline;
	    out(' Yacc, that was disgusting.');
	    more;
	    docrt;
	    pru;
	  END
	ELSE
	  IF  monster^.species = cockatrice  THEN
	    BEGIN
	      pline;
	      out('You stop feeling like a stone!');
	      TurningToStone := 0;
	      IF  multi < 0  THEN
		multi := 0
	    END  (* IF  monster^.species = cockatrice *);
	u.ustuck := NIL;
      END;
  END  (* YouAreNoMoreStuckBy *);

PROCEDURE GoodBye(* VAR Mtmp : Thing *);

  (* There was quite a fatal BUG in here, as the parameter was NOT
     a VAR parameter. So, with FAST monsters in MoveMonsters if the
     monster was killed in DoChug, we started to move things in the
     FreeList as if they were monsters... (== variant access error) *)

  BEGIN
    YouAreNoMoreStuckBy(mtmp);
    WHILE mtmp^.ObjectsCarried <> NIL DO
      MonsterDropsAnObjectItHasBeenCarrying(mtmp);
    WITH mtmp^ DO
      IF f^.levl[locx, locy].CanSee THEN
        NewSym(locx, locy)
      ELSE
        news1(locx,locy);
    delete(mtmp,f^.listof[monsters]);
  END  (* GoodBye *);


PROCEDURE Killed(Mtmp : Thing);

  VAR
    obj  : thing;
    tmp  : Positive;

  PROCEDURE ChangeIdentity(mtmp: thing; WhoAmI: MonsterSpecies);

    BEGIN
      IF  MakeMon(WhoAmI)  THEN
	BEGIN 
	  WITH  mtmp^  DO
	    BEGIN
	      f^.listof[monsters]^.locx := locx;
	      f^.listof[monsters]^.locy := locy;
	      f^.listof[monsters]^.ObjectsCarried := ObjectsCarried;
	      f^.listof[monsters]^.mstat := sleep;
	      IF WhoAmI = Kobold THEN
		BEGIN
		  f^.listof[monsters]^.invis := True;
		  NewSym(locx,locy);
		END
	      ELSE
		pmon(f^.listof[monsters]);
	    END;
	  Delete(mtmp,f^.listof[monsters]);
	END
      ELSE
	GoodBye(mtmp);
   END (* ChangeIdentity *);

  BEGIN  (* Killed *)
    YouAreNoMoreStuckBy(mtmp);
    pline;
    out('You destroy');
    WITH mtmp^ DO
      BEGIN
	IF  cham  THEN
	  species := chameleon;
	WriteEnemy(species,normal);
	OutCh('!');
	IF  u.umconf AND (u.ublind = 0)  THEN
	  BEGIN
	    pline;
	    out(YourHandsStopGlowingBlue);
	    u.umconf := False;
	  END;
	WITH  mon[species]  DO
	  BEGIN
	    tmp := 1 + Sqr(mhd);
	    IF  ac < 3  THEN
	      tmp := tmp + 2*(7-ac);
	    IF species = humanoid THEN
	      tmp := tmp + 25*mhd
	    ELSE IF species IN [ cyclops, exorcist, destroyer, suicider,
				 TwoHeadedEagle, TyrannoSaur ] THEN
		   tmp := tmp + 15*mhd
	    ELSE IF species IN [ Dragon, ettin, Vampire, demon,
				 PurpleWorm ] THEN
	      tmp := tmp + 9*mhd
	    ELSE
	      IF  species IN [ Ant, cockatrice, scorpion, Snake, Xorn,
			       acidblob, Rust, Troll, Wraith, Umberhulk,
			       Invisiblestalker, neootyugh, chameleon ] THEN
		tmp := tmp + 2*mhd;
	    IF  mhd > 6  THEN
	      tmp := tmp + 50;
	  END  (* WITH  mon[species] *);
	u.uexp := u.uexp + tmp;
	IF  NOT (species IN MonstersKilledByYou)  THEN
	  BEGIN
	    MonstersKilledByYou := MonstersKilledByYou + [Species];
	    u.urexp := u.urexp + 4*tmp;
	  END;
	flags.dexp := True;

	IF (species IN [homunculus, Yeti, zelomp, Kobold]) AND rn2(6) THEN
	  BEGIN
	    CASE species OF
	      homunculus: ChangeIdentity(mtmp, Zombie);
	      Yeti	: ChangeIdentity(mtmp, Wraith);
	      zelomp	: ChangeIdentity(mtmp, Vampire);
	      Kobold	: ChangeIdentity(mtmp, Kobold);
	    END (* CASE *);
	  END
	ELSE
	  BEGIN  (* It really died and may leave something *)
	    IF species = rat THEN
	      f^.NumberOfRats := f^.NumberOfRats - 1;

	    IF  (species = Leprechaun) AND (ObjectsCarried <> NIL)  THEN
	      BEGIN
		obj := ObjectsCarried;
		REPEAT
		  WITH  obj^  DO
		    IF  what = Golds  THEN
		      BEGIN
			quantity := quantity + d(dlevel,30);
			obj := NIL
		      END
		    ELSE
		      obj := obj^.next
		UNTIL  obj = NIL;
	      END;

	    (* Displacer also leaves you some uvva goodie *)
	    IF (species IN [ Nymph, Troll, Vampire, demon, displacer ]) OR
	        (rnd(25) < (dlevel - 30)) OR 
		rn2(21-mon[species].mhd) AND NOT u.uswallow  THEN
	      BEGIN
		MakeSomeObject;
		f^.listof[objects]^.locx := locx;
		f^.listof[objects]^.locy := locy;
	      END;

	    IF mtmp^.species = beetle THEN
	      WHILE rn2(5) DO
		CreateSomeMonsterAndPutItBesideU(beetle);
	    GoodBye(mtmp);
	  END  (* It really died and may leave something *);
      END  (* WITH mtmp^ *);
    IF  u.uexp >= 10 * pow(u.ulevel-1)  THEN
      IF  u.ulevel < MaxUserLevel  THEN
	IncrementUserLevel(True);
  END  (* Killed *);

FUNCTION  UnSuitable(weapon : WeaponType) : Boolean;

  BEGIN
    IF  uwep = NIL  THEN
      UnSuitable := True
    ELSE
      CASE  weapon  OF
	warrow       : UnSuitable := uwep^.weapon <> bow;
	SlingBullet  : UnSuitable := uwep^.weapon <> sling;
	CrossBowBolt : UnSuitable := uwep^.weapon <> CrossBow;
      END
  END  (* UnSuitable *);

PROCEDURE ConvertToGold(otmp: thing);

  VAR 
    HowMuchGold : unsigned;

  BEGIN(* ConvertToGold *)
    pline;
    out('Wow, You feel rich and greedy!');
    WITH otmp^ DO
      CASE What OF
	Objects :
	  CASE class OF
	    anything : Debugger;
	    food :
	    IF fruit THEN
	      HowMuchGold := rn1(10,20)
	    ELSE
	      HowMuchGold := rn1(20,80);
	    Weapons :
	    IF weapon = wdart THEN
	      HowMuchGold := 25 * quan
	    ELSE
	      IF weapon <= CrossbowBolt THEN
		HowMuchGold := 60 * quan
	      ELSE
		IF weapon = TwoHandedSword THEN
		  HowMuchGold := rn1(200, 2000)
	        ELSE
		  HowMuchGold := rn1(100, 1000);
	    Armors :
	      HowMuchGold := rn1(500, 800 * (Ord(armor) + 1));
	    gems :
	      HowMuchGold := 40 * quan;
	    rings :
	      HowMuchGold := 20 * quan;
	    potions :
	      HowMuchGold := 100 * quan;
	    scrolls :
	      HowMuchGold := 70 * quan;
	    wands :
	      HowMuchGold := 200;
	    amulet :
	      HowMuchGold := 50;
	  END;
	golds,
	traps : Debugger;
	monsters :
	  HowMuchGold := 100 * mhp;
      END;
    MkGold(HowMuchGold, u.ux, u.uy);
    NewSym(u.ux, u.uy);
  END(* ConvertToGold *);

FUNCTION  Hmon

  (Mon : Thing; Wepon : Thing) : Boolean;

  LABEL 9;

  VAR
    tmp : Short;

  BEGIN  (* Hmon *)
    IF wepon = NIL THEN
      tmp := rn1(2,1)
    ELSE WITH wepon^ DO
      IF  (weapon IN [ bow,sling,crossbow,warrow,SlingBullet,CrossBowBolt ])
	  AND (wepon=uwep)  THEN
	tmp := rn1(2,1)
      ELSE
	BEGIN
	  IF mon^.species IN mlarge THEN
	    BEGIN
	      tmp := rn1(wldam[weapon],1);
	      IF  weapon = TwoHandedSword  THEN
		tmp := tmp + d(2,6)
	      ELSE
		IF  weapon = flail  THEN
		  tmp := tmp + rnd(4);
	    END
	  ELSE
	    BEGIN
	      tmp := rn1(wsdam[weapon],1);
	      IF  weapon IN [ mace, flail ]  THEN
		tmp := tmp + 1;
	    END;
	  IF  minus  THEN  tmp := tmp - spe
		     ELSE  tmp := tmp + spe;
	  IF  weapon IN [ warrow..crossbowbolt ]  THEN
	    IF  Unsuitable(weapon)  THEN
	      tmp := tmp - 3
	END;
    tmp := tmp + u.udaminc + dbon - (Mon^.protect - 3);
	   (* Init value of protection is 3 *)
    IF Mon^.giant THEN
      tmp := tmp - 3;
    IF  u.uswallow  THEN
      BEGIN
	IF  mon^.species = Purpleworm  THEN
	  BEGIN
	    tmp := tmp - u.uswldtim;
	    IF  tmp < 0  THEN
	      BEGIN
		hmon := True;
		GOTO 9
	      END  (* IF  tmp < 0 *);
	  END  (* IF  mon^.species = Purpleworm *);
      END  (* IF  u.uswallow *)
    ELSE
      IF  tmp < 1  THEN  tmp := 1;
    IF u.umidas THEN
      BEGIN 
	ChangeMonstersHitPoints(mon, - mon^.mhp);
	ConvertToGold(mon);
      END 
    ELSE 
      ChangeMonstersHitPoints(mon, - tmp);
    IF  mon^.mhp <= 0  THEN
      BEGIN
	killed(mon);
	hmon := False;
      END
    ELSE  hmon := True;
  9:
  END  (* Hmon *);

PROCEDURE PlusOne(o:thing);
 BEGIN
   WITH o^ DO
    BEGIN
     cursed := False;
     IF minus THEN
       BEGIN
	 spe := spe - 1;
	 IF spe = 0 THEN minus := False;
       END
     ELSE
       spe := spe + 1;
    END;
 END;

PROCEDURE MinusOne(o:thing);
 BEGIN
   WITH o^ DO
     IF minus THEN
       spe := spe + 1
     ELSE IF spe > 0 THEN
       spe := spe - 1
     ELSE
       BEGIN minus := True; spe := 1 END;
 END;

PROCEDURE PotionInfo(pot : PotionType; quantity : small);
  (* quantity = 0 means we are executing an I command *)

  LABEL 9;

  VAR
    state : IdentificationStatus;

  BEGIN  (* PotionInfo *)
    IF  pot IN KnownPotions  THEN
      state := wellknown
    ELSE
      IF  potcal[pot] <> blankname  THEN
	state := named
      ELSE  state := unknown;
    IF  state = unknown  THEN
      IF  quantity > 0  THEN
	WriteQuantityAndColor(quantity,PotionShuffle[pot])
      ELSE  GOTO 9
    ELSE
      IF quantity > 0 THEN
	BEGIN
	  IF quantity > 1 THEN
	    WriteNumber(quantity,1)
	  ELSE OutCh('a');
	  OutCh(blank);
	END  (* IF quantity > 0 *);
    out(litPotion);
    IF quantity > 1 THEN
      OutCh('s');
    IF state = wellknown THEN
      BEGIN
	out(litOf);
	WritePotion(pot);
      END
    ELSE
      IF state = named THEN
	BEGIN
	  out(litCalled);
	  out0(PotCal[pot]);
	END  (* IF state = named *);
    IF quantity = 0 THEN
      NewLine;
    9:
  END  (* PotionInfo *);

PROCEDURE ScrollInfo(scr : ScrollType; Quantity : small);
  (* Quantity = 0 means we are executing an I command *)

  LABEL 9;

  VAR
    State : IdentificationStatus;

  BEGIN
    IF  scr IN KnownScrolls  THEN
      state := wellknown
    ELSE
      IF  scrcal[scr] <> blankname  THEN
	state := named
      ELSE
	state := unknown;
    IF  quantity > 0  THEN
      BEGIN
	IF  quantity > 1  THEN
	  WriteNumber(quantity,1)
	ELSE
	  OutCh('a');
	OutCh(blank);
      END
    ELSE
      IF  state = unknown  THEN
	GOTO 9;
    out('scroll');
    IF  quantity > 1  THEN
      OutCh('s');
    CASE  state  OF
      unknown	: BEGIN
		    out(' labeled ');
		    WriteTitle(ScrollShuffle[scr]);
		  END;
      wellknown : BEGIN
		    out(litOf);
		    WriteScroll(scr);
		  END;
      named	: BEGIN
		    out(litCalled);
		    out0(ScrCal[scr]);
		  END;
    END  (* CASE  state *);
    IF  quantity = 0  THEN
      NewLine;
    9:
  END  (* ScrollInfo *);

PROCEDURE WandInfo(wand:WandType; quantity:small);
(* quantity = 0 means we are executing an I command *)
LABEL 9;
VAR state:IdentificationStatus;
	     BEGIN
		 IF wand IN KnownWands THEN
		   state := wellknown
		 ELSE IF wandcal[wand] <> blankname THEN
		   state := named
		 ELSE
		   state := unknown;
		 IF quantity > 0 THEN out('a ')
		 ELSE IF state = unknown THEN GOTO 9;
		 IF state = unknown THEN
		   BEGIN
		     WriteWandMaterial(WandShuffle[wand]);
		     OutCh(blank);
		   END;
		 out(litWand);
		 IF state = wellknown THEN
		   BEGIN
		     out(litOf);
		     WriteWand(wand);
		   END
		 ELSE IF state = named THEN
		   BEGIN
		     out(litCalled);
		     out0(WandCal[wand]);
		   END;
	       IF quantity = 0 THEN NewLine;
	       9:
	       END;

PROCEDURE RingInfo(Rng : RingType; quantity : small;
		   IndividuallyKnown, minus : Boolean; spec : small;
		   RingObject : thing);
  (* Quantity = 0 means we are executing an I command. *)

  LABEL 9;

  VAR
    state : IdentificationStatus;

  BEGIN
    IF  rng IN KnownRings  THEN
      state := wellknown
    ELSE
      IF  ringcal[rng] <> blankname  THEN
	state := named
      ELSE
	state := unknown;
    IF  quantity > 0  THEN
      out('a ')
    ELSE
      IF  state = unknown  THEN  GOTO 9;
    IF  state = unknown  THEN
      BEGIN
	WriteRingMaterial(RingShuffle[rng]);
	OutCh(blank)
      END
    ELSE
      IF  IndividuallyKnown AND
	 (rng IN [ AddStrength, IncreaseDamage, protection ])  THEN
	BEGIN
	  IF minus THEN OutCh('-') ELSE OutCh('+');
	  WriteNumber(spec,1);
	  OutCh(blank);
	END;
    out('ring');
    IF state = wellknown THEN
      BEGIN
	out(litOf);
	WriteRing(rng);
      END
    ELSE
      IF  state = named  THEN
	BEGIN
	  out(litCalled);
	  out0(RingCal[rng]);
	END;
    IF  quantity = 0  THEN
      NewLine
    ELSE
      IF  RingObject = uright  THEN
	out('  (on right hand)')
      ELSE
	IF  RingObject = uleft  THEN
	  out('  (on left hand)');
    9:
  END;

PROCEDURE DoName(*Obj : Thing*);
  (* We don't write 'a' and 'an' correctly yet ... *)

   PROCEDURE WriteQuantity;
     BEGIN
       IF  obj^.quan > 1  THEN
	 WriteNumber(obj^.quan,1)
       ELSE  OutCh('a');
       OutCh(blank);
     END;

   PROCEDURE WriteSpe;
     BEGIN
       IF  obj^.minus  THEN
	 OutCh('-')
       ELSE  OutCh('+');
       WriteNumber(obj^.spe,1);
     END;

  BEGIN (* DoName *)
    WITH  obj^  DO
      CASE  class  OF
	amulet    :BEGIN
		     out('The amulet of Frobozz ');
		   END;
	food      : BEGIN
		      WriteQuantity;
		      IF fruit THEN
			out('fruit')
		      ELSE
			out('food ration');
		      IF quan > 1 THEN OutCh('s');
		    END;
	weapons : BEGIN
		    IF  obj^.quan > 1  THEN
		      WriteNumber(obj^.quan,1)
		    ELSE  OutCh('a');
		    IF  known  THEN
		      BEGIN
			OutCh(blank);
			WriteSpe
		      END
		    ELSE
		      IF  (weapon IN [warrow,axe]) AND (quan=1)  THEN
			OutCh('n');
		    OutCh(blank);
		    WriteArmament(weapon);
		    IF quan > 1 THEN OutCh('s');
		    IF obj = uwep THEN
		      out('  (weapon in hand)');
		  END;
	armors  : BEGIN
		    out('a suit of ');
		    IF  known  THEN
		      BEGIN
			WriteSpe;
			OutCh(blank)
		      END;
		    WriteArmor(armor);
		    IF  obj = uarm  THEN
		      out('  (being worn)');
		  END;
	potions : PotionInfo(potion,quan);
	scrolls : ScrollInfo(scroll,quan);
	Wands   : BEGIN
		    WandInfo(wand,quan);
		    IF  known  THEN
		      BEGIN
			out('  (');
			WriteSpe;
			OutCh(')')
		      END;
		  END;
	Rings   : RingInfo(ring,quan,known,minus,spe,obj);
	Gems    : BEGIN
		    WriteQuantityAndColor(quan,Ord(gem));
		    out('magic gem');
		    IF  quan > 1  THEN
		      OutCh('s');
		  END;
      END  (* CASE  class OF *);
  END  (* DoName *);

PROCEDURE PrName(Obj : thing; ilet : Char);

  BEGIN
    pline;
    OutCh(ilet);
    out(' - ');
    doname(obj);
  END;

FUNCTION  DoInv(Objs : ObjectSet) : Boolean;

  LABEL 99;

  VAR
    otmp : thing;
    ilet : Char;

  BEGIN   (* DoInv *)
    ilet := 'a';
    otmp := invent;
    ClearScreen;
    WHILE  otmp <> NIL  DO
      BEGIN
	IF otmp^.class IN objs THEN
	  BEGIN
	    IF cury = MaxRow - 1 THEN
	      IF  NOT GetRet(True)  THEN
		BEGIN
		  DoInv := False;
		  GOTO 99
		END
	      ELSE  ClearScreen;
	    OutCh(ilet); out(' -  ');
	    doname(otmp);
	    IF cury <> MaxRow - 1 THEN NewLine;
	  END  (* IF otmp^.class IN objs *);
	IF ilet = 'z' THEN
	  ilet := 'A'
	ELSE ilet := Succ(ilet);
	otmp := otmp^.next;
      END  (* WHILE otmp <> NIL *);
    DoInv := GetRet(True);
  99: DocRt
  END  (* DoInv *);

PROCEDURE PrInv(Obj : Thing);

  VAR
    ilet : Char;
    otmp : thing;

  BEGIN  (* PrInv *)
    ilet := 'a';
    otmp := invent;
    WHILE  otmp <> obj  DO
      BEGIN
	IF  ilet = 'z'  THEN
	  ilet := 'A'
	ELSE
	  ilet := Succ(ilet);
	otmp := otmp^.next;
      END;
    prname(obj,ilet);
  END  (* PrInv *);

PROCEDURE ArmorOff; FORWARD;

FUNCTION  GetObj(Objs : ObjectSet; TheVerb : string8) : Thing;

  LABEL 1,9;

  VAR
    count   : small;
    otmp    : thing;
    answer,
    ch	    : Char;

  FUNCTION  WrongClass : Boolean;

    BEGIN
      IF (otmp = NIL) THEN
	WrongClass := False
      ELSE
	WrongClass := NOT (otmp^.class IN objs);
    END;

  BEGIN  (* GetObj *)
    GetObj := NIL;
    count := 0;
    otmp := invent;
    WHILE  otmp <> NIL  DO
      WITH  otmp^  DO
	BEGIN
	  IF  class IN objs  THEN
	    count := count + 1;
	  otmp := otmp^.next;
	END;
    IF  count = 0  THEN
      BEGIN
	pline;
	out('You don''t have anything to ');
	out0(TheVerb);
      END
    ELSE
      BEGIN
 1:     pline;
	out('What do you want to '); out0(TheVerb);
	out(' (? for list)?  ');
	flags.topl := False;
	tcRdCh(answer);
	IF  answer = Chr(ESC)  THEN
	  BEGIN
	    pline;
	    GOTO 9
	  END;
	IF  answer = '?'  THEN
	  BEGIN
	    IF  count > 1 THEN
	      BEGIN
		IF  NOT doinv(objs) THEN
		  GOTO 9
	      END
	    ELSE
	      BEGIN
		otmp := invent;
		WHILE otmp <> NIL DO
		  WITH otmp^ DO
		    BEGIN
		      IF class IN objs THEN prinv(otmp);
		      otmp := next;
		    END;
	      END;
	    GOTO 1
	  END
	ELSE
	  BEGIN  (* Answer <> '?' *)
	    otmp := invent;
	    ch := 'a';
	    WHILE  (otmp <> NIL) AND (ch <> answer)  DO
	      BEGIN
		otmp := otmp^.next;
		IF  ch = 'z'  THEN
		  ch := 'A'
		ELSE  ch:=Succ(ch);
	      END;
	    IF  otmp = NIL  THEN
	      BEGIN
		pline;
		out('You don''t have that object.');
		GOTO 1
	      END;
	  END  (* Answer <> '?' *);
      IF  WrongClass  THEN
	BEGIN
	  pline;
	  out('That is a silly thing to ');
	  out0(TheVerb); OutCh('.');
	  otmp := NIL;
	END;
      END;
    GetObj := otmp;
    IF u.umidas AND (otmp <> NIL) THEN
      IF NOT otmp^.Magical THEN 
	BEGIN 
	  ConvertToGold(otmp);
	  GetObj := NIL;
	  IF otmp = uarm THEN
	    BEGIN 
	      ArmorOff;
	      uarm := NIL;
	    END;
	  Delete(otmp, invent);
	END;
  9:
END  (* GetObj *);

FUNCTION weight(*o:thing*);
  BEGIN
    WITH o^ DO
      CASE class OF
	 Weapons : IF weapon = TwoHandedSword THEN
		      weight := 4
		     ELSE IF weapon <= wdart THEN
		       IF quan = 1 THEN
			 weight := 1
		       ELSE
			 weight := quan DIV 2
		     ELSE
		       weight := 3;
	 Potions : weight := quan;
	 Scrolls : weight := quan;	(* originallly 3*quan *)
	 Wands   : weight := 3;
	 Rings   : weight := 1;
	 Armors  : weight := 7 - ( (ArmNum - Ord(armor) + 1) DIV 2);
         Gems    : weight := 1;
	 food	   : IF fruit THEN
		       weight := quan
		     ELSE
		       weight := 2*quan;	(* originally 3*quan *)
	 amulet	   : weight := 2;
     END;       
 END;

PROCEDURE poisoned(OriginOfPoison:enemy);
 BEGIN
   WriteEnemy(OriginOfPoison,start);
   out('was poisoned!');
   IF u.upres THEN
     BEGIN pline;out('The poison doesn''t seem to affect you.') END
   ELSE
     BEGIN
       CASE rnd(6) OF
	 1 : LoseHp(1,OriginOfPoison);
	 2,3,4 : LoseStr(rn1(3,3),OriginOfPoison);
	 5,6 : LoseHp(rn1(10,6),OriginOfPoison);
       END (*CASE*);
     END;
 END;

PROCEDURE dodown; FORWARD;

PROCEDURE teleport(NotFlyingWithIt: Boolean);

  BEGIN  (* TelePort *)
    unsee(True);
    IF NotFlyingWithIt THEN
      BEGIN
	YouAreNoMoreStuckBy(u.ustuck);
	u.uswldtim := 0; u.uswallow := False;
	u.utrap := 0;
      END;
    REPEAT
      u.ux := rn1(xmax+1,xmin);
      u.uy := rn1(ymax+1,ymin);
    UNTIL (f^.levl[u.ux,u.uy].typ = RoomLocation)
	  AND (ThingAt(u.ux,u.uy,monsters) = NIL);

    (* Why shouldn't the luser appear on top of something ??
	  AND (ThingAt(u.ux,u.uy,objects) = NIL)
	  AND (ThingAt(u.ux,u.uy,traps) = NIL)
	  AND (ThingAt(u.ux,u.uy,golds) = NIL)		*)

    setsee;
  END  (* TelePort *);

  PROCEDURE AddObjectToInvent(Object: Thing);
    VAR
      otmp ,
      Previous	: thing;
      FoundIt	: Boolean;

    FUNCTION Similar : Boolean;

      BEGIN   (* Similar *)
	similar := False;
	WITH  otmp^  DO
	  IF  Class = Object^.Class  THEN
	    CASE  class  OF
	      weapons : IF weapon = object^.weapon THEN
			  IF (quan+object^.quan<32) AND
			     (spe=object^.spe) AND (minus = object^.minus) THEN
			    IF  weapon IN [warrow..wdart]  THEN
			      similar := True;
	      potions : IF potion = object^.potion THEN similar:=True;
	      scrolls : IF scroll = object^.scroll THEN similar:=True;
	      wands,
	      rings,
	      armors  : ;
	      gems    : IF gem = object^.gem THEN similar:=True;
	      food    : IF fruit = object^.fruit THEN similar:=True;
	      amulet  : ;
	    END (*CASE*)
      END  (* Similar *);

    BEGIN  (* AddObjectToInvent *)
      Remove(object,f^.listof[objects]);
      otmp := invent;
      Previous := NIL;
      FoundIt  := False;
      WHILE  NOT FoundIt AND (Otmp <> NIL)  DO
	IF  Similar  THEN
	  BEGIN
	    Otmp^.Quan := Otmp^.Quan + Object^.Quan;
	    Free(Object);
	    Object := Otmp;
	    FoundIt := True;
	  END
	ELSE
	  IF  Otmp^.Class > Object^.Class  THEN
	    Otmp := NIL
	  ELSE
	    BEGIN
	      Previous := Otmp;
	      Otmp := Otmp^.Next;
	    END;
      IF  NOT FoundIt  THEN
	IF  Previous = NIL  THEN
	  BEGIN
	    Object^.Next := Invent;
	    Invent := Object;
	  END  (* IF  Previous = Invent *)
	ELSE
	  BEGIN
	    Object^.Next := Previous^.Next;
	    Previous^.Next := Object
	  END;
      prinv(object);
    END  (* AddObjectToInvent *);

PROCEDURE MonsterBeginsToFlee(monster:thing);

  BEGIN
    monster^.mstat := flee;
    YouAreNoMoreStuckBy(monster);
  END;


PROCEDURE Swim;

  BEGIN
    IF YouAreSwimming THEN
      BEGIN
	Nomul(0);
	Nomul(-2);
      END
    ELSE
      BEGIN 
	YouAreSwimming := True;
	YourActualStrength := u.ustr;
	IF YourActualStrength >= StrengthWhenSwimming THEN 
	  LoseStr(u.ustr - StrengthWhenSwimming, nomonster)
	ELSE
	  YouCanStillSwim := 100 -
			     10 * (StrengthWhenSwimming-YourActualStrength);
	(* Must be restored *)
      END
  END (* StartSwimming *);

PROCEDURE StopSwimming;

  BEGIN
     IF YouAreSwimming THEN
       BEGIN
	 Nomul(0);
	 YouAreSwimming := False;
	 YouCanStillSwim := 100;
	 IF YourActualStrength > StrengthWhenSwimming THEN 
	   LoseStr(StrengthWhenSwimming - YourActualStrength, nomonster);
	 Nomul(-10);  (* You are a bit tired after swimming *)
       END;
   END;
	 
PROCEDURE DoMove;

  LABEL 9;

  VAR
    newx,
    newy,
    oldx,
    oldy   : byte;	(*for saving your old coordinates*)
    OldLocation,	(*your old location*)
    NewLocation   : rm;	(*your new location*)
    mtmp,		(*monster attacked*)
    gold,
    trap,
    object :thing;
    zx, zy,
    lowx, highx,
    lowy, highy	  : byte;

  PROCEDURE PickItUp;

    VAR
      weit	: unsigned;
      otmp	: Thing;

    BEGIN   (* PickItUp *)
      weit := 0;
      otmp := invent;
      WHILE  otmp <> NIL  DO
	BEGIN
	  weit := weit + weight(otmp);
	  otmp := otmp^.next
	END;
      weit := weit + weight(object);
      IF  (weit > YouCanCarry)  THEN
	BEGIN
	  pline;
	  out('You can''t carry anything more.')
	END
      ELSE
	BEGIN  (* The weight isn't too much *)
	  IF  object^.Class = Amulet  THEN
	    YouHaveTheAmulet := True;
	  AddObjectToInvent(object);
	  IF  u.uinvis > 0 THEN
	    NewSym(u.ux,u.uy)
	END  (* The weight isn't too much *);
      IF  flags.see  THEN  nomul(0)
    END  (* PickItUp *);

  PROCEDURE EnterTrap;

    VAR
      hit : Boolean;

    BEGIN   (*  EnterTrap *)
      trap^.seen := True;
      CASE  trap^.gflag  OF
	beartrap : BEGIN
		     u.utrap := rn1(4,4);
		     u.upit := BearTrap;
		     pline;out('A bear trap closes on you foot!');
		   END;
	arrow	 : BEGIN
		     pline;out('An arrow shoots out at you!');
		     IF hitu(8,rnd(6),ArrowShot) THEN;
		   END;
	dart	 : BEGIN
		     pline;out('A little dart shoots out at you!');
		     hit := hitu(7,rnd(3),LittleDart);
		     IF dlevel < 48 THEN
		       BEGIN 
			 IF hit AND rn2(6 - dlevel DIV 8) THEN
			   BEGIN
			     poisoned(LittleDart);
			     IF u.uhp <= 0 THEN killer:=PoisonDart;
			   END
		        END
		      ELSE 
		        IF hit THEN 
			   BEGIN
			     poisoned(LittleDart);
			     IF u.uhp <= 0 THEN killer:=PoisonDart;
			   END
		   END;
	tdoor	 : BEGIN   (*  A TrapDoor *)
		     pline;
		     IF f^.ThisIsAMaze OR rn2(4) OR YouAreSwimming THEN
		       BEGIN
			 out('A trap door in the ceiling opens and');
			 out(' a rock falls on your head!');
			 LoseHp(d(2,10),FallingRock);
		       END
		     ELSE
		       BEGIN  (* Not last floor and trap *)
			 out('A trap door opens up under you!');
			 IF u.ufloat OR (u.ustuck <> NIL) THEN
			   BEGIN
			     pline;
			     out('For some reason you don''t fall in.')
			   END
			 ELSE
			   BEGIN  (* Drop down *)
			     NewLine;
			     IF  GetRet(False)  THEN  (* Nothing *);
			     SeeOff(True);
			     REPEAT
			       dodown;
			       f^.levl[u.ux,u.uy].ScrSym := upwards
			     UNTIL f^.ThisIsAMaze OR NOT rn2(4);
			     REPEAT
			       u.ux := rnd(xmax); u.uy := rnd0(ymax+1);
			     UNTIL (f^.levl[u.ux,u.uy].typ = RoomLocation)
				   AND (ThingAt(u.ux,u.uy,monsters) = NIL);
			     SeeOn;
			     RedrawScreen;
			   END  (* Drop down *)
		       END  (* Not last floor and trap *)
		   END  (* A TrapDoor *);

	tele	 : BEGIN
		     NewSym(u.ux,u.uy);
		     teleport(True);
		   END;
	pit	 : IF NOT YouAreSwimming THEN 
		    BEGIN   (* A Pit *)
		     pline;
		     IF u.ufloat THEN
		      out('A pit opens up under you - but you don''t fall in!')
		     ELSE
		       BEGIN
			 out('You fall into a pit!');
			 u.utrap := rn1(6,2);
			 u.upit := pit;
			 IF trap^.ThereIsWaterInThePit THEN
			   BEGIN
			     pline;
			     out('There is water in the pit!');
			     Swim;
			   END
			 ELSE 
			   LoseHp(rnd(6),PitTrap);
		       END;
		    END  (* A Pit *);
	slptrp	 : BEGIN
		     pline;
		     out('A cloud of gas puts you to sleep!');
		     nomul(-rnd(25))
		   END;
	StinkingTrap : BEGIN
		       pline;
		       out('A burst of stinking gas surrounds You!');
		       IF rn2(2) THEN
			 BEGIN
			   pline;
			   out('You faint');
			   nomul(-rn1(10,15));
			   u.uconfused := u.uconfused + 15 + rn1(5,10);
			 END
		       ELSE
			 BEGIN 
			   LoseHp(rn1(10,dlevel * 2), StinkingGasTrap);
			   u.uconfused := u.uconfused + rn1(5,10);
			 END;
		     END;
	pierc	 : BEGIN
		     pline;
		     out('A piercer suddenly drops from the ceiling!');
		     delete(trap,f^.listof[traps]);
		     IF MakeMon(piercer) THEN
		       BEGIN
			 mnexto(f^.listof[monsters],u.ux,u.uy);
			 IF hitu(3,d(4,6),FallingPiercer) THEN;
		       END;
		   END;
	MimicTrap:;
      END  (* CASE  trap^.gflag OF *)
    END  (* EnterTrap *);

  PROCEDURE EnterMimicTrap;

    BEGIN
      nomul(0);
      pline;
      out('The door is actually ');
      WriteEnemy(Mimic,IndefiniteArticle);
      (* Generate a mimic, put it into the location where there was
	 a door, make the user stuck by the mimic, and delete the trap. *)
      IF  NOT MakeMon(Mimic)  THEN (* the poor mimics have been genocided *)
	NewSym(newx,newy)
      ELSE
	BEGIN
	  WITH  trap^  DO
	    BEGIN
	      f^.listof[monsters]^.locx := locx;
	      f^.listof[monsters]^.locy := locy;
	      IF  f^.levl[locx,locy].CanSee  THEN
		atl(locx,locy,mon[Mimic].mlet);		
	    END;
	  IF  u.ustuck = NIL  THEN
	    u.ustuck := f^.listof[monsters];
	END;
      delete(trap,f^.listof[traps]);
    END  (* EnterMimicTrap *);

  PROCEDURE AttackMonster;

    VAR
      tmp : Byte;

    BEGIN  (* AttackMonster *)
      IF  u.uswallow  THEN
	mtmp := u.ustuck;
      WITH  mtmp^  DO
	BEGIN
	  tmp := -1 + u.ulevel + mon[species].ac + abon -
		 (mtmp^.protect - 3);  (* Init value of protection is 3 *)
	  IF giant THEN tmp := tmp - 6;
	  IF  uwep <> NIL  THEN
	    WITH uwep^ DO
	      BEGIN
		IF  minus  THEN  tmp := tmp - spe
			   ELSE  tmp := tmp + spe;
		IF  weapon = TwoHandedSword  THEN
		  tmp := tmp - 2*Ord(NOT (mtmp^.species IN mlarge))
		ELSE
		  IF  weapon = dagger  THEN
		    tmp := tmp + 2
		  ELSE
		    IF weapon = spear THEN
		      IF  mtmp^.species IN [ Xorn, Dragon, neootyugh,
					     ettin, cyclops ] THEN
			tmp := tmp + 2;
	      END  (* WITH uwep^ *);
	  CASE  mstat  OF
	    NormalState: ;
	    Sleep,
	    GuardingTreasures :
	            BEGIN
		      IF (dlevel = WorldOfGiants) AND
			  NOT YouHaveTheAmulet THEN
			AggravateThem;
		      mstat := NormalState;
		      tmp := tmp + 2
		    END;
	    mfroz : BEGIN
		      tmp := tmp + 4;
		      IF  rn2(10)  THEN
			mstat := NormalState;
		    END;
	    flee  : tmp := tmp - 1;
	    scared: tmp := tmp - 2; (* You should not be so cruel *)
	    Tamed :
	      BEGIN
		mstat := NormalState;
		mspeed := mfast;
	      END;

	  END  (* CASE  mstat OF *);
	  IF  (tmp < rnd(20)) AND NOT u.uswallow  THEN
	    BEGIN
	      pline;
	      out('You miss');
	      WriteEnemy(species,normal);
	    END
	  ELSE
	    BEGIN  (* You hit the monster *)
	      IF  hmon(mtmp,uwep)  THEN
		BEGIN
		  IF (mhp < OrigHp DIV 2) THEN 
		    IF rn2(9) THEN
		      MonsterBeginsToFlee(mtmp);
		  pline;
		  out('You hit');
		  WriteEnemy(species,normal);
		  IF u.umconf THEN
		    BEGIN
		      IF u.ublind = 0 THEN
			BEGIN
			  pline;out(YourHandsStopGlowingBlue);
			  WriteEnemy(species,start);
			  out('appears confused.');
			END;
		      mspeed := mconf; u.umconf := False;
		    END;
	      IF (species = leocrotta) AND (rn2(2) OR giant) AND (mhp>1) THEN
		BEGIN
		  pline;out('You split');WriteEnemy(species,normal);
		  out(' into two halves!');
		  MakeMonster(leocrotta, False);
		  mnexto(f^.listof[monsters], u.ux, u.uy);
		  mhp := mhp DIV 2;
		  f^.ListOf[monsters]^.mhp := mhp;
		END
	      ELSE IF species = quiveringblob THEN
		IF (rn2(6) OR giant) AND (uwep <> NIL) THEN
		  BEGIN
		    pline;
		    out('Your hands quiver and you drop your ');
		    WriteArmament(uwep^.weapon); OutCh('!');
		    remove(uwep, invent);
		    AddToList(uwep, f^.listof[objects]);
		    WITH  uwep^  DO
		      BEGIN
			locx := u.ux;
			locy := u.uy;
		      END;
		    uwep := NIL;
		  END;
		END (*IF hmon*);
	      IF  species = acidblob  THEN
		BEGIN
		  IF rn2(2) OR giant THEN
		    BEGIN
		      pline;out('You are splashed by the blob''s acid!');
		      LoseHp(rnd(6),acidblob);
		    END;
		  IF (rn2(6) OR giant) AND (uwep <> NIL)  THEN
		    BEGIN
		      pline;out('Your ');
		      WriteArmament(uwep^.weapon);
		      out(' corrodes!');
		      minusone(uwep);
		    END;
		END  (* IF  species = acidblob *);

	      IF uwep <> NIL THEN 
		IF (species IN [ Xorn, Dragon, neootyugh, ettin, cyclops ]) AND
		   (uwep^.weapon = spear) THEN
		     IF rn2(3) THEN
		       BEGIN
			 pline;
			 WriteEnemy(species,start);
			 out(' looks horrified!');
			 MonsterBeginsToFlee(mtmp);
		       END;
	    END  (* You hit the monster *);
		 
	  IF  (species = Eye) AND (mhp > 0) AND (u.ublind=0) AND (rn2(2) OR
								  giant) THEN
	    BEGIN
	      pline;out('You are frozen by');
	      WriteEnemy(Eye,normal);out('''s gaze!');
	      nomul(rn1(20,-20));
	    END
	  ELSE  nomul(0);
	END  (* WITH  mtmp^ *);
    END  (* AttackMonster *);
    
  FUNCTION  SomethingAround : Boolean;

    LABEL 9;

    VAR
      x : xval;
      y : yval;
      dr, ul : 0..1;

    BEGIN  (* SomethingAround *)
      SomethingAround := False;
      FOR  x := Pred(u.ux)  TO  Succ(u.ux)  DO
	FOR  y := Pred(u.uy)  TO  Succ(u.uy)  DO
	  IF  (Abs(x - u.ux+dx) > 1) OR (Abs(y - u.uy+dy) > 1)  THEN
	    IF NOT (f^.levl[ x,y ].ScrSym IN [ '|','-','.',' ','#','~' ]) THEN
	      IF  (f^.levl[ x,y ].ScrSym <> '+')  THEN
		BEGIN
		  SomethingAround := True;
		  GOTO 9
		END
	      ELSE
		IF  (x-u.ux <> 0) AND (y-u.uy <> 0) OR NOT flags.one  THEN
		  BEGIN
		    pru;
		    multi := 1
		  END
		ELSE
		  BEGIN
		    SomethingAround := True;
		    GOTO 9
		  END;

      IF  (NewLocation.typ = corr) AND ((dx = 0) OR (dy = 0))  THEN
	IF  (dx <> 0)  THEN
	  BEGIN
	    ul := Ord(f^.levl[ u.ux, Succ(u.uy) ].typ = corr);
	    dr := Ord(f^.levl[ u.ux, Pred(u.uy) ].typ = corr);
	    IF  Ord(f^.levl[ u.ux+dx, u.uy ].typ IN [ corr, door ] ) +
	        ul + dr > 1  THEN
	      SomethingAround := True
	    ELSE
	      IF  dr + ul = 1  THEN
		BEGIN
		  dy := ul - dr;
		  dx := 0;
		END
	  END  (* IF  (dx <> 0) *)
	ELSE
	  BEGIN  (* dy <> 0 *)
	    ul := Ord(f^.levl[ Pred(u.ux), u.uy ].typ = corr);
	    dr := Ord(f^.levl[ Succ(u.ux), u.uy ].typ = corr);
	    IF  Ord(f^.levl[ u.ux, u.uy+dy ].typ IN [ corr, door ] ) +
	        ul + dr > 1  THEN
	      SomethingAround := True
	    ELSE
	      IF  dr + ul = 1  THEN
		BEGIN
		  dx := dr - ul;
		  dy := 0;
		END
	  END  (* dy <> 0 *);
      9:
    END  (* SomethingAround *);

  PROCEDURE nose1(x:xval; y:yval);

    PROCEDURE nosee(x:xval; y:yval);

      BEGIN  (* NoSee *)
	WITH f^.levl[x,y] DO
	  BEGIN
	    CanSee := False;
	    IF  ((ScrSym='.') OR (ScrSym = '~')) AND
	        NOT lit AND (u.ublind = 0)  THEN
	      BEGIN
		ScrSym := blank;
		on(x,y)
	      END;
	  END  (* WITH f^.levl[x,y] *)
      END  (* NoSee *);

    BEGIN  (* Nose1 *)
      IF dx <> 0 THEN
	IF dy <> 0 THEN
	  BEGIN  (* Diagonal *)
	    nosee(x,u.uy);
	    nosee(x,u.uy-dy);
	    nosee(x,y);
	    nosee(u.ux-dx,y);
	    nosee(u.ux,y);
	  END  (* Diagonal *)
	ELSE
	  BEGIN  (* Horizontal *)
	    nosee(x,y-1);
	    nosee(x,y);
	    nosee(x,y+1);
	  END  (* Horizontal *)
      ELSE
	BEGIN  (* Vertical *)
	  nosee(x-1,y);
	  nosee(x,y);
	  nosee(x+1,y);
	END  (* Vertical *);
    END  (* Nose1 *);

  PROCEDURE prl1(x:xval; y:yval);

    BEGIN  (* Prl1 *)
      IF dx <> 0 THEN
	IF dy <> 0 THEN
	  BEGIN  (* Moving diagonally. *)
	    prl(x-2*dx,y);
	    prl(x-dx,y);
	    prl(x,y);
	    prl(x,y-dy);
	    prl(x,y-2*dy);
	  END  (* Moving diagonally. *)
	ELSE
	  BEGIN  (* Moving horizontally *)
	    prl(x,y-1);
	    prl(x,y);
	    prl(x,y+1);
	  END  (* Moving horizontally *)
      ELSE
	BEGIN  (* Moving vertically *)
	  prl(x-1,y);
	  prl(x,y);
	  prl(x+1,y);
	END  (* Moving vertically *);
    END  (* Prl1 *);

  BEGIN  (* DoMove *)
    IF u.utrap > 0 THEN
      BEGIN
	pline;
	IF u.upit = pit THEN out('You are still in a pit')
			ELSE out('You are caught in a beartrap');
	u.utrap := u.utrap - 1;
	GOTO 9; (*return*)
      END;

    IF u.uconfused = 0 THEN (* Go where luser wanted *)
      NewLocation := f^.levl[u.ux+dx,u.uy+dy]
    ELSE  		    (* Take a random step *)
      REPEAT
	dx := rn1(3,-1);
	dy := rn1(3,-1);
	NewLocation := f^.levl[u.ux+dx,u.uy+dy];
      UNTIL ((dx<>0) OR (dy<>0)) AND (NewLocation.typ >= door);

    oldx := u.ux;      (* Save the old location *)
    oldy := u.uy;
    OldLocation := f^.levl[oldx,oldy];
    newx := oldx + dx; (* Here we try to go *)
    newy := oldy + dy;

    trap := ThingAt(newx, newy, traps);
    IF trap <> NIL THEN
      IF trap^.gflag = MimicTrap THEN
	BEGIN
	  EnterMimicTrap;
	  GOTO 9;
	END;

    IF  (u.ustuck <> NIL) AND NOT u.uswallow  THEN
      WITH u.ustuck^ DO
	IF  (newx <> locx) OR (newy <> locy)  THEN
	  BEGIN
	    pline;
	    out('You cannot escape from');
	    WriteEnemy(species,normal);
	    OutCh('!');
	    nomul(0);
	    GOTO 9
	  END;

    mtmp := ThingAt(newx, newy, monsters);
    IF (mtmp <> NIL) OR u.uswallow THEN
      BEGIN
	IF  flags.see  THEN
	  BEGIN			(* If capital move, we don't want to *)
	    nomul(0);		(*   attack the monster.  Just stop. *)
	    flags.move := False;
	  END
	ELSE
	  AttackMonster;
	GOTO 9
      END;

    IF (dx <> 0) AND (dy <> 0) AND
       ((NewLocation.typ = door) OR (OldLocation.typ = door)) OR
       (NewLocation.typ IN [ empty, wall, sdoor ])  THEN
	 BEGIN
	   flags.move := False;
	   nomul(0);
	   GOTO 9;
	 END;

    u.ux := newx;  (* we moved !!! *)
    u.uy := newy;

    IF  flags.see  THEN
      IF  NewLocation.typ = door  THEN
        IF  flags.one  THEN  multi := 1
		       ELSE  nomul(0);
    IF  flags.see AND (((f^.xupstair=u.ux) AND (f^.yupstair=u.uy)) OR
		       ((f^.xdnstair=u.ux) AND (f^.ydnstair=u.uy)))  THEN
      NoMul(0);

    IF  OldLocation.ScrSym = YourSymbol  THEN
      NewSym(Oldx,Oldy);

    IF  u.ublind > 0  THEN		(* You are blind *)
      BEGIN
	IF  NOT f^.levl[oldx,oldy].seen  THEN
	  on(oldx,oldy);  (* Your OLD location gets printed on the screen *)
      END
    ELSE  (* You are not blind *)
      IF  OldLocation.lit  THEN		     (* we are in a lit room or door *)
	IF  NewLocation.lit  THEN 	     (* New location is also LIT *)
	  BEGIN  (* both new and old positions lit *)
	    IF  NewLocation.typ = door  THEN (* came from room to it's door *)
	      prl1(newx+dx,newy+dy)	     (* Next locations in corridoor *)
	    ELSE
	      IF OldLocation.typ = door THEN (* from door to inside the room *)
		nose1(oldx-dx,oldy-dy)	     (* can't see corridor anymore *)
	  END  (* both new and old positions lit *)
	ELSE
	  BEGIN
	    (* We are on a door (of a lit room), and go to corridoor
	       which is (always) dark *)
	    unsee(True); 		       (* CanSee:= false in the room *)
	    f^.levl[oldx,oldy].CanSee := True; (* levl.typ = door *)
	    prl1(newx+dx, newy+dy)	       (* Cansee:= true in corr *)
	  END
      ELSE
	BEGIN 
	  (* Old place is dark (i.e. dark room or corridoor) *)

	  IF  NewLocation.lit  THEN  (* New location is a door of a lit room *)
	    SetSee		     (* then we CanSee the room *)
	  ELSE
	    BEGIN
	      (* Also new place is dark (i.e. dark room or corridor) *)

	      prl1(newx+dx, newy+dy); (* CanSee in direction we go to *)
	      IF  NewLocation.typ = door  THEN  (* we stepped on a door *)
		IF  dy <> 0  THEN		(* came to Horizontal wall *)
		  BEGIN
		    prl(newx-1,newy);		(* Left side of the wall *)
		    prl(newx+1,newy)		(* Right side of the wall *)
		  END
		ELSE
		  BEGIN		      (* Came to Vertical Wall *)
		    prl(newx,newy-1); (* Upper side wall item *)
		    prl(newx,newy+1)  (* Lower side wall item *)
		  END
	    END;  (* Also new place is dark *)
	  nose1(oldx-dx,oldy-dy);     (* Can't see the previous-old loc *)
	END;  (* Old place is dark *)

    IF multi = 0 THEN pru;

    gold := ThingAt(newx, newy, golds);
    IF gold <> NIL THEN
      BEGIN
	WITH gold^ DO
	  BEGIN
	    IF what = traps THEN
	      BEGIN
		pline;out('The chest was a mimic!');
		IF MakeMon(mimic) THEN
		  BEGIN
		    mnexto(f^.listof[monsters], newx, newy);
		    IF  u.ustuck = NIL  THEN
		      u.ustuck := f^.listof[monsters];
		  END;
		nomul(0);
	      END
	    ELSE
	      BEGIN
		pline;
		WriteNumber(quantity,0);
		out(' gold piece');
		IF quantity > 1 THEN OutCh('s');
		u.urexp := u.urexp + quantity;
		u.ugold := u.ugold + quantity;
		flags.dgold := True;
	      END;
	    delete(gold,f^.listof[golds]);
	  END  (* WITH gold^ *);
	IF  flags.see  THEN
	  nomul(0);
	IF  u.uinvis > 0  THEN
	  newsym(newx, newy);
      END  (* IF gold <> NIL *);

    object := ThingAt(newx, newy, objects);
    IF object <> NIL THEN
      IF u.umidas THEN
	IF NOT object^.Magical THEN 
	  BEGIN
	    ConvertToGold(object);
	    Delete(object, f^.listof[objects]);
	  END
        ELSE
	  PickItUp
      ELSE 
	PickItUp;

    IF f^.levl[newx,newy].typ = RoomLocation THEN
      BEGIN 
        IF NOT f^.levl[newx,newy].Recognized THEN
	  BEGIN 
	    CASE f^.levl[newx,newy].room OF
	      OrdinaryRoom:;
	      HotRoom:
	        IF NOT u.ufireres THEN 
		  BEGIN
		    pline;
		    out('You feel hot');
		  END;
	      ColdRoom:
	        IF NOT u.ucoldres THEN 
		  BEGIN
		    pline;
		    out('You feel cold');
		  END;
	      PoisonGasRoom:
	        IF NOT u.upres THEN 
		  BEGIN
		    pline;
		    out('You feel sick');
		  END;
	      WaterRoom:
	        BEGIN
		  pline;
		  out('There is water in the room');
		  IF u.ufloat THEN
		    BEGIN
		      pline;
		      out('You don''t sink in it!');
		    END;
		END;
	      ConfuseGasRoom:
	        BEGIN
		  pline;
		  out('You feel dizzy');
		END;
	    END(* CASE *);
	    zx := newx;
	    zy := newy;
	    WHILE f^.levl[zx,zy].typ = RoomLocation DO
	      zx := zx + 1;
	    highx := zx;
	    zx := newx;
	    WHILE f^.levl[zx,zy].typ = RoomLocation DO
	      zx := zx - 1;
	    lowx := zx;
	    zx := newx;
	    WHILE f^.levl[zx,zy].typ = RoomLocation DO
	      zy := zy + 1;
	    highy := zy;
	    zy := newy;
	    WHILE f^.levl[zx,zy].typ = RoomLocation DO
	      zy := zy - 1;
	    lowy := zy;
	    FOR zx := lowx TO highx DO
	      FOR zy := lowy TO highy DO
		f^.levl[zx,zy].recognized := true;
	  END;
	

	CASE f^.levl[newx,newy].room OF
	  OrdinaryRoom: ;
	  HotRoom :
	    IF NOT u.ufireres THEN 
	      BEGIN
		IF rn2(4) THEN
		  BEGIN
		    Nomul(0);
		    nomul(-5);
		  END;
	      END;
	  ColdRoom :
	    IF NOT u.ucoldres THEN 
	      BEGIN
		nomul(0);
		nomul(-2);
		u.uhunger := u.uhunger - 2;
	      END;
	  WaterRoom :
	    IF NOT u.ufloat THEN 
	      Swim;
	  ConfuseGasRoom :
	    u.uconfused := u.uconfused + 2 * Ord(rn2(2));
	  PoisonGasRoom: (* handled IN the main loop *);
	END(* CASE *);
      END;

    IF YouAreSwimming AND (f^.levl[newx, newy].room <> WaterRoom) THEN
      StopSwimming;

    IF trap <> NIL THEN
      BEGIN
	nomul(0);
	IF trap^.seen AND rn2(5) THEN
	  BEGIN
	    pline;out('You escape a');
	    WriteTrap(trap^.gflag); OutCh('.');
	  END
	ELSE
	  EnterTrap;
      END  (* IF trap <> NIL *);
 9:
    IF  flags.see AND (u.ublind = 0)  THEN
      IF  SomethingAround  THEN
	Nomul(0);
  END (* DoMove *);

PROCEDURE JustSwld(mtmp : thing);

  BEGIN
    WITH mtmp^ DO
      BEGIN
	NewSym(locx,locy);
	locx := u.ux; locy := u.uy;
      END;
    YouAreNoMoreStuckBy(u.ustuck);
    u.ustuck := mtmp;
    prl(u.ux,u.uy);
    at(u.ux,u.uy,mon[mtmp^.species].mlet);
    WriteEnemy(mtmp^.species,start);
    out('swallows you!');
    more;
    u.uswallow := True;
    u.uswldtim := 0;
    flags.botl := True;
    swallowed;
  END  (* JustSwld *);

PROCEDURE YouSwld(mtmp : thing; dam : Integer; die : small);

  BEGIN
    WriteEnemy(mtmp^.species,start);
    out('digests you!');
    u.uswldtim := u.uswldtim + 1;
    IF  u.uswldtim = Pred(die)  THEN
      BEGIN
	pline;
	out('You have an awful sensation of being liquidated!');
      END
    ELSE IF  u.uswldtim = die  THEN
      BEGIN
	pline;
	out('It totally digests you!');
	dam := u.uhp
      END;
    LoseHp(dam, mtmp^.species);
  END  (* YouSwld *);

FUNCTION  bHit(ddx:byte; ddy:byte; range:small;
	      VAR x:xval; VAR y:yval) : thing;
  LABEL 9;

  VAR
    mtmp : thing;

  BEGIN
    bhit := NIL;
    x := u.ux; y := u.uy;
    WHILE  range > 0  DO
      BEGIN
	range := range - 1;
	x := x + ddx;
	y := y + ddy;
	mtmp := ThingAt(x,y,monsters);
	IF  mtmp <> NIL  THEN
	  BEGIN
	    bhit := mtmp;
	    GOTO 9;
	  END
	ELSE
	  IF  f^.levl[x,y].typ < corr  THEN
	    BEGIN
	      x:=x-ddx;
	      y:=y-ddy;
	      GOTO 9;
	    END;
      END  (* WHILE  range > 0 *);
    9:
  END  (* bHit *);

PROCEDURE Hit(DoesHit : Boolean; Wep : Armament; Monster : Thing);

  BEGIN
    pline;
    out(litThe);
    WriteArmament(wep);
    IF  DoesHit  THEN
      out(' hits')
    ELSE
      out(' misses');
    WITH  monster^  DO
      IF  f^.levl[locx,locy].Cansee  THEN
	WriteEnemy(species,normal)
      ELSE
       out(litIt);
    OutCh('.');
  END  (* Hit *);

PROCEDURE Zhit(Monster : Thing; Typ : Projectile);

  VAR
    MonstersHitPointLoss : small;

  PROCEDURE FreezeMon(dam: small);
    BEGIN
      WITH Monster^ DO
	IF  NOT (species IN [Yeti,gelatenouscube,fog,humanoid])  THEN
	 BEGIN
	   MonstersHitPointLoss := dam;
	   IF  species = Dragon  THEN
	     MonstersHitPointLoss := MonstersHitPointLoss + 7;
	 END;
    END (* FreezeMon *);

  BEGIN
    MonstersHitPointLoss := 0;
    WITH  monster^  DO
      CASE  typ  OF
	missile	  : IF species = gelatenouscube THEN
		       MonstersHitPointLoss := d(2,6) + 8
		    ELSE
		      IF species = humanoid THEN
			MonstersHitPointLoss := d(2,6) * 6 
		      ELSE
			MonstersHitPointLoss := d(2,6);
	BoltOfFire : IF  NOT (species IN [Dragon,gelatenouscube])  THEN
		       IF NOT fireres THEN 
			 BEGIN
			   MonstersHitPointLoss := d(6,6);
			   IF  species = Yeti  THEN
			     MonstersHitPointLoss := MonstersHitPointLoss + 7;
			 END;
	SleepRay   : IF species <> gelatenouscube THEN
		        mstat := mfroz;
	IcyWind	   : FreezeMon(d(2,5));
	BoltOfCold : FreezeMon(d(6,6));
	DeathRay : IF  NOT (species IN [Wraith,Vampire,Zombie])  THEN
		     MonstersHitPointLoss := mhp;
      END;
    ChangeMonstersHitPoints(monster,-MonstersHitPointLoss);
  END  (* zHit *);

PROCEDURE Buzz(Typ : Projectile; sx : xval; sy : yval; dx, dy : byte);

  VAR
    range   : byte;
    let	    : Char;
    monster : thing;
    loc,oldloc : rm;

  PROCEDURE ColdHits(dam: small; kind: Projectile);
    BEGIN
      IF  u.ucoldres  THEN
	 BEGIN
	   pline;
	   out('You don''t feel cold!')
	 END
       ELSE
	 LoseHp(dam, kind);		      
    END (* ColdHits *);

  FUNCTION TypeOfBounceInRoom : TypeOfWall;

    BEGIN
      IF (f^.levl[ sx+1, sy].typ IN [wall, sdoor, door]) AND
	 (f^.levl[ sx-1, sy].typ IN [wall, sdoor, door]) THEN
	   TypeOfBounceInRoom := horizontal
      ELSE
	IF (f^.levl[ sx, sy+1 ].typ IN [wall, sdoor, door]) AND
	   (f^.levl[ sx, sy-1 ].typ IN [wall, sdoor, door]) THEN
	     TypeOfBounceInRoom := vertical
        ELSE
	  TypeOfBounceInRoom := corner;
    END;

  BEGIN  (* Buzz *)
    IF  u.uswallow  THEN
      BEGIN
	pline;
	out(litThe);
	WriteArmament(typ);
	out(' rips into');
	WriteEnemy(u.ustuck^.species,normal);
	zhit(u.ustuck,typ);
      END
    ELSE
      BEGIN  (* NOT Swallowed *)
	range := rn1(7,7);
	IF dx = dy THEN
	  let := '\'
	ELSE
	  IF  (dx <> 0) AND (dy <> 0)  THEN
	    let := '/'
	  ELSE
	    IF  dx <> 0  THEN
	      let := '-'
	    ELSE
	      let := '|';
	REPEAT
	  range := range - 1;
	  oldloc := f^.levl[sx,sy];
	  sx := sx + dx;
	  sy := sy + dy;
	  loc := f^.levl[sx,sy];
	  IF  loc.typ <> empty  THEN
	    BEGIN
	      at(sx,sy,let);
	      on(sx,sy);
	    END;
	  monster := ThingAt(sx,sy,monsters);
	  IF  monster <> NIL  THEN
	    IF  rnd(20) < 18 + mon[monster^.species].ac -
			  6 * Ord(monster^.giant) THEN
	      BEGIN
		zhit(monster,typ);
		IF  monster^.mhp <= 0  THEN
		  killed(monster)
		ELSE
		  hit(True,typ,monster);
		range := range - 2;
	      END
	    ELSE
	      hit(False,typ,monster)
	  ELSE
	    IF  (sx = u.ux) AND (sy = u.uy)  THEN
	      BEGIN
		pline;
		out(litThe);
		WriteArmament(typ);
		IF  rnd(20) < 18+u.uac  THEN
		  BEGIN
		    out(' hits you!');
		    range := range - 2;
		    CASE typ OF
		      missile	 : LoseHp(d(3,6),Missile);
		      BoltOfFire : IF  u.ufireres  THEN
				     BEGIN
				       pline;
				       out('You don''t feel hot!')
				     END
				   ELSE
				     LoseHp(d(6,6),BoltOfFire);
		      SleepRay	 : Nomul(-rnd(25));
		      IcyWind	 : ColdHits(d(3,5), IcyWind);
		      BoltOfCold : ColdHits(d(6,6), BoltOfCold);
		      DeathRay	 : LoseHp(u.uhp+1,DeathRay)
		    END  (* CASE typ *);
		  END  (* IF  rnd(20) < 18+u.uac *)
		ELSE
		  out(' whizzes by you!');
	      END  (* IF  (sx = u.ux) AND (sy = u.uy) *);
	  IF  u.uhp <= 0  THEN
	    range := 0
	  ELSE
	    IF  loc.typ <= door  THEN
	      BEGIN
		IF loc.CanSee THEN
		  BEGIN
		    pline;
		    out(litThe);WriteArmament(typ);
		    out(' bounces!')
		  END;
		range := range - 1;
		IF loc.typ < door THEN
		  BEGIN 
		    IF dx = 0 THEN
		      dy := - dy
		    ELSE
		      IF dy = 0 THEN
			dx := - dx
		      ELSE
			CASE oldloc.typ OF
			  RoomLocation, wall, sdoor :
			    CASE TypeOfBounceInRoom OF
			      horizontal : dy := -dy;
			      vertical   : dx := -dx;
			      corner     : BEGIN
					     dx := -dx;
					     dy := -dy;
					   END;
			    END(* CASE *);
			  door :
			    BEGIN
			      (* Nothing. It can't bounce from the door *)
			    END;
			  corr :
			    BEGIN
			      dx := -dx;
			      dy := -dy;
			    END;
			END(* CASE *);
		  END
		ELSE (* loc.typ = door *)
		  BEGIN
		    IF (dx <> 0) AND (dy <> 0) THEN
		      range := 0 (* It vanishes, if not
				   zapped straight towards the door *)
		    ELSE
		      (* Nothing, it goes through the door *);
		  END;
	      END;
	UNTIL range <= 0;
      END  (* NOT Swallowed *);
  END  (* Buzz *);

PROCEDURE RingEffect(Obj : Thing; Flag : Boolean); FORWARD;

PROCEDURE CursedWeaponInHand; FORWARD;
  
PROCEDURE UnWear(otmp: Thing);

  (* Takes the thing off, if You wear it *)

  BEGIN
    IF otmp = uwep THEN
      uwep := NIL
    ELSE
      IF  otmp = uarm  THEN
	BEGIN
	  ArmorOff;
	  uarm := NIL
	END
      ELSE
	IF  otmp = uleft  THEN
	  BEGIN
	    uleft := NIL;
	    RingEffect(otmp,False)
	  END
	ELSE
	  IF  otmp = uright  THEN
	    BEGIN
	      uright := NIL;
	      RingEffect(otmp,False)
	    END;
  END(* UnWear *);


FUNCTION TakeAnObjectFromYou(PreferAmulet: Boolean) : Thing;

  (* Function returns an object removed from your inventory list.
     If PreferAmulet is TRUE, it takes amulet if it can (well, not
     allways).
      If you happen to fool around emptyhanded (-headed ??), it
     makes a new object and returns it, thus never returning NIL *)

  VAR
    Done : Boolean;
    tmp  : byte;
    otmp : Thing;

  BEGIN
    otmp := invent;
    IF otmp = NIL THEN
      BEGIN
	MakeSomeObject;
	otmp := f^.listof[ Objects ];		(* sick *)
	f^.listOf[ Objects ] := otmp^.Next;
      END
    ELSE
      BEGIN
        otmp := invent;
	IF PreferAmulet AND 
	   YouHaveTheAmulet AND
	   NOT rn2(10)
	THEN					(* Miss it once in a while *)
	  WHILE otmp^.class <> Amulet DO   	(* Search *)
	    Otmp := Otmp^.Next			(* Otmp is the amulet *)
	ELSE
	  BEGIN
	    (* Choose randomly one of your goodies *)

	    tmp := 0;
	    WHILE  otmp <> NIL  DO		(* invent is not NIL *)
	      BEGIN
		otmp := otmp^.next;
		tmp := Succ(tmp);
	      END;
	    tmp := rnd(tmp);			(* So tmp is => 1 *)

	    (* Now we know what we take, so do it *)

	    otmp := invent;
	    WHILE  tmp > 1  DO
	      BEGIN
		otmp := otmp^.next;
		tmp := Pred(tmp)
	      END;
	  END;

	    (* Now OTMP is the thing we take *)

	  UnWear(otmp);
	  IF otmp^.class = Amulet THEN
	    IF NOT YouHaveTheAmulet THEN
	      BEGIN
		Debugger;
		YouHaveTheAmulet := False;
	      END
	    ELSE
	      YouHaveTheAmulet := False;
	remove(otmp, invent);
      END;
    TakeAnObjectFromYou := otmp;		(* Return the object *)
  END (* TakeAnObjectFromYou *);

PROCEDURE TeleportOneThing(PreferAmulet : Boolean);

  VAR
    otmp : Thing;

  BEGIN
    pline;
    out('You have a weird feeling for a moment, then it passes');
    more;
    otmp := TakeAnObjectFromYou(PreferAmulet);
    AddToList(otmp, f^.listof[objects]);
    WITH otmp^ DO
      BEGIN 
	REPEAT
	  locx := rn1(77,2);
	  locy := rnd0(22);
	UNTIL NOT (f^.levl[locx,locy].typ IN [empty,wall,sdoor]);
	IF f^.levl[locx, locy].CanSee THEN
	  NewSym(locx, locy)
	ELSE
	  news1(locx,locy);
      END;
  END (* TeleportOneThing *);


PROCEDURE Rloc(* Mtmp : Thing *);
  (* find a random location for the monster *)

  VAR
    tx, ty : small;
    letter : Char;

  BEGIN  (* rloc *)
    YouAreNoMoreStuckBy(mtmp);
    WITH  mtmp^  DO
      BEGIN
	letter := mon[species].mlet;
	IF f^.levl[locx,locy].ScrSym = letter THEN
	  NewSym(locx,locy)
      END;
    REPEAT
      tx := rn1(77,2);
      ty := rnd0(22);
    UNTIL NOT (f^.levl[tx,ty].typ IN [empty,wall,sdoor])
	  AND (ThingAt(tx,ty,monsters) = NIL)
	  AND NOT ((tx=u.ux) AND (ty=u.uy));
    WITH mtmp^ DO
      BEGIN
	locx := tx;
	locy := ty;
	IF f^.levl[tx,ty].CanSee THEN
	  pmon(mtmp);
      END;
  END  (* rloc *);

PROCEDURE CreateSomeMonsterAtRandomLocation;

  BEGIN  (* CreateSomeMonsterAtRandomLocation *)
    MakeMonster(NoMonster, False);
    WITH f^.listof[monsters]^ DO
      BEGIN
	locx := 0;
	locy := 0
      END;
    rloc(f^.listof[monsters])
  END  (* CreateSomeMonsterAtRandomLocation *);
   

PROCEDURE GenerateMoreMonstersToThisLevel;

  VAR
    Howmany : Byte;

  BEGIN
    IF YouHaveTheAmulet THEN
      HowMany := rn1(10, 15 * Ord(YouHaveTheAmulet))
    ELSE
      IF dlevel >= 25 THEN
        HowMany := rn1(10, dlevel-10)+10*Ord(Odd(dlevel))
      ELSE
        HowMany := rnd(30-dlevel)+10*Ord(Odd(dlevel));
    FOR HowMany := HowMany DOWNTO 1 DO
      CreateSomeMonsterAtRandomLocation;
  END (* GenerateMoreMonstersToThisLevel *);

  PROCEDURE InvestigatesWhatItHas(mtmp: thing; Wise: Boolean);

      VAR
	Stuff, tmp : thing;
        ItsDecision : (UseIt,CarryIt,DropIt);

      PROCEDURE JustCarryThisObject(mtmp : thing);

	VAR
	  tmp : thing;

	BEGIN(* JustCarryThisObject *)
	  IF mtmp^.ObjectsCarried^.next <> NIL THEN
	    BEGIN
	      tmp := mtmp^.ObjectsCarried;
	      WHILE tmp^.next <> NIL DO
		tmp := tmp^.next;
	      tmp^.next := mtmp^.ObjectsCarried;
	      mtmp^.ObjectsCarried := mtmp^.ObjectsCarried^.next;
	      tmp^.next^.next := NIL;
	    END;
	END(* JustCarryThisObject *);


      PROCEDURE UseItUp;
	  VAR tmp:thing;
	BEGIN
	  WITH Stuff^ DO
	    IF quan > 1 THEN
	      quan := quan - 1
	    ELSE
	      BEGIN 
		tmp := mtmp^.ObjectsCarried;
		Delete(Stuff, tmp);
		mtmp^.ObjectsCarried := tmp;
	      END;
	END;

      BEGIN(* InvestigatesWhatItHas *)
        ItsDecision := UseIt; (*default*)
	stuff := mtmp^.ObjectsCarried;
	IF stuff <> NIL THEN 
	  WITH Stuff^ DO 
	    CASE class OF
	      anything:;
	      food :
	        BEGIN
		  IF fruit THEN 
		    mtmp^.TimeForTheMonsterToRemainInTrap := 1
		  ELSE
		    mtmp^.TimeForTheMonsterToRemainInTrap := 5;
		END;
	      
	      weapons,
	      armors,
	      gems :
	        ItsDecision := CarryIt;

	      rings :
	        BEGIN 
		  ItsDecision := CarryIt;
		  CASE Ring OF
		    adornment,
		    hunger,
		    ProtectionFromShapeChangers,
		    RingOfFrobozz  :
		      ItsDecision := DropIt;
		    searching,
		    ColdResistance,
		    MaintainArmor,
		    stealth :
		      (* nothing here *);
		    TeleportRing :
		      mtmp^.Telport := True;
		    regeneration :
		      mtmp^.rege := True;
		    SeeInvisible :
		      mtmp^.SeeInv := True;
		    floating :
		      mtmp^.float := True;
		    PoisonResistance :
		      mtmp^.pres := True;
		    FireResistance :
		      mtmp^.fireres := True;
		    Aggravate :
		      BEGIN
			tmp := f^.listof[monsters];
			REPEAT
			  WITH  tmp^  DO
			    BEGIN
			      CASE mstat OF
				sleep, GuardingTreasures:
				    mstat := NormalState;
				NormalState,flee, mfroz, scared,
				tamed : (* nothing *);
			      END(* CASE *);
			      tmp := next;
			    END;
			UNTIL tmp = NIL;
			ItsDecision := DropIt;
		      END;
		    AddStrength,
		    IncreaseDamage :
		      IF minus THEN
			IF Wise THEN
			  ItsDecision := DropIt
		        ELSE
			  mtmp^.strength := 3 - spe
		      ELSE
			mtmp^.strength := 3 + spe;
		    Protection :
		      IF minus THEN
			IF Wise THEN
			  ItsDecision := DropIt
			ELSE
			  mtmp^.protect := 3 - spe
		      ELSE
			mtmp^.protect := 3 + spe;
		  END(* CASE *);
		END;
	      
	      potions :
	        BEGIN 
		  CASE potion OF
		    RestoreStrength :
		      IF mtmp^.strength < 3 THEN
			mtmp^.strength := 3;
		    booze,
		    confusion,
		    blindness:
		      IF Wise THEN 
			ItsDecision := DropIt
		      ELSE
			mtmp^.mspeed := mconf;
		    invisibility :
		      mtmp^.invis := True;
		    juice,
		    blood,
		    ObjectDetection,
		    MonsterDetection,
		    MagicSpirit,
		    forgetting : (* nothing *);
		    healing :
		      ChangeMonstersHitPoints(mtmp, rnd(10));
		    ExtraHealing :
		      ChangeMonstersHitPoints(mtmp, d(20,2) + 1);
 		    paralysis :
		      IF Wise THEN
			ItsDecision := DropIt
		      ELSE
			mtmp^.mstat := mfroz;
		    sickness :
		      IF Wise THEN
			ItsDecision := DropIt
		      ELSE
			mtmp^.strength := 0;
		    GainStrength :
		      mtmp^.strength := mtmp^.strength + 1;
		    speed :
		      mtmp^.mspeed := mfast;
		    GainLevel :
		      BEGIN
			mtmp^.strength := mtmp^.strength + 1;
			mtmp^.protect := mtmp^.protect + 1;
		      END;
		    PotionOfFrobozz : 
		      ItsDecision := DropIt;
		  END(* CASE *);
		END;
	      
	      scrolls :
	        BEGIN
		  CASE scroll OF
		    ScrollOfFrobozz :
		      ItsDecision := DropIt;
		    EnchantArmor :
		      mtmp^.protect := mtmp^.protect + 1;
		    ConfuseMonster :
		      IF Wise THEN 
			ItsDecision := DropIt
		      ELSE
			mtmp^.mspeed := mconf;
		    AggravateMonster :
		      AggravateThem;
		    ScareMonster :
		      IF NOT Wise THEN
			mtmp^.mstat := mfroz
		      ELSE
			(* Nothing *);
		    BlankScroll,
		    RemoveCurse,
		    GoldDetection,
		    identify,
		    magicmapping,
		    ScrollOfLight :
		      (* nothing *);
		    EnchantWeapon :
		      mtmp^.strength := mtmp^.strength + 1;
		    DamageWeapon :
		      IF Wise THEN 
			MonsterDropsAnObjectItHasBeenCarrying(mtmp)
		      ELSE
			IF mtmp^.strength > 0 THEN
			   mtmp^.strength :=  mtmp^.strength - 1;
		    CreateMonster :
		      BEGIN 
			MakeMonster(nomonster, False);
			mnexto(f^.listof[monsters],mtmp^.locx,mtmp^.locy);
		      END;
		    DestroyArmor :
		      IF Wise THEN 
			ItsDecision := DropIt
		      ELSE
			mtmp^.protect := 0;
		    TeleportScroll :
		      rloc(mtmp);
		    FireScroll :
		      IF Wise THEN 
			ItsDecision := DropIt
		      ELSE
			IF NOT mtmp^.fireres THEN
			  ChangeMonstersHitPoints(mtmp, -rnd(6));
		    genocide :
		      IF Wise THEN
			BEGIN 
			  IF rn2(30) THEN 
			    BEGIN
			      pline;
			      out('You hear somebody murmuring to himself');
			      pline;
			      out('OK! Let''s see, what we have');
			      pline;
			      out('AAAAAAH!!!');
			      pline;
			      out('This will teach that intruder!!!');
			      more;
			      LoseHp(u.uhp, humanoid);
			    END
			  ELSE
			    BEGIN
			      quan := quan + 1; (* Would You believe this? *)
			      pline;
            Out('You feel as if somebody were trying to find his spectacles');
		            END;
			END
		      ELSE
			ChangeMonstersHitPoints(mtmp, -mtmp^.mhp);
		  END(* CASE *);
		END;

	      wands :
	        BEGIN
		  CASE wand OF
		    WandOfLight,
		    WandOfDarkness,
		    detection,
		    DestroyObject,
		    UndeadTurning,
		    polymorph,
		    cancellation,
		    digging,
		    TameMonster :
		      ItsDecision := CarryIt;
		    WandOfFrobozz,
		    SpeedMonster :
		      ItsDecision := DropIt;
		    CreatorWand :
		      WHILE spe > 0 DO 
			BEGIN 
			  MakeMonster(nomonster, False);
			  mnexto(f^.listof[monsters],mtmp^.locx,mtmp^.locy);
			  spe := spe - 1;
			END;
		      
		    striking, SlowMonster, DrainLife,
		    TeleportMonster, WandOfMissile, FireWand,
		    SleepWand, cold, death : (* nothing here *);
	          END(* CASE *);
                  IF spe = 0 THEN
		    ItsDecision := DropIt;
		END;
	      
	      amulet :
	        BEGIN 
		  mtmp^.Telport := True; (* J{y, j{y, j{y *)
		  ItsDecision := CarryIt;
		END;
	    END(* CASE *);

	CASE ItsDecision OF
	  UseIt : UseItUp;
	  CarryIt : JustCarryThisObject(mtmp);
	  DropIt : MonsterDropsAnObjectItHasBeenCarrying(mtmp);
	END;

      END(* InvestigatesWhatItHas *);

PROCEDURE MoveMonsters;

  VAR
    monster,
    NextOne  : Thing;
    dist2,		(* scratch variable for computing distance *)
    distance : small;	(* distance between you and the monster *)

  FUNCTION MonsterMoves(VAR Mtmp : Thing) : Boolean;

    (* If MonsterMoves changes the value of mtmp to NIL it means
       that the monster was killed *)

    LABEL 9;

    VAR
      omx    : xval;
      omy    : yval;
      dx, dy,
      nix,
      niy    : byte;
      stuff  : Thing; (* HEHEHE *)
      Eaten  : Boolean;
      GotYou : Boolean;

    PROCEDURE InRange(mtmp : Thing; Ptile: Projectile; JustSearch : Boolean;
		      VAR GotYou : Boolean) ;

      CONST
	max = 8;

      VAR
	tx, ty : byte;
	zx, zy : -1..+1;
	newx   : xval;
	newy   : yval;

      BEGIN  (* InRange *)
	GotYou := False;
	WITH mtmp^ DO
	  BEGIN
	    tx := u.ux - locx;
	    ty := u.uy - locy;
	  END;
	(* If the distance between the monster and the person is
	   less than "max" and they are at the same vertical,
	   horizontal, or diagonal line, then a projectile launches... *)
	IF (distance < max) AND
	   ( (tx = 0) OR (ty = 0) OR (Abs(tx) = Abs(ty)) ) THEN
	  BEGIN
	    zx := sgn(tx); zy := sgn(ty);
	    IF  bhit(-zx,-zy,max,newx,newy) = mtmp  THEN
	      BEGIN 
		IF NOT JustSearch THEN
		  BEGIN
		    buzz(Ptile,mtmp^.locx,mtmp^.locy,zx,zy);
		    nomul(0); (* Made it a little bit healthier for U *)
		  END;
		GotYou := True;
	      END;
	    (* Above we have -zx,-zy, since "bhit" takes YOUR location as
	       starting point; "bhit" is primarily used when YOU throw or
	       zap towards a monster, so the direction must be reversed. *)
	    (* Modified the routine to be able to just find out if You are
	       in the range *)
	  END;
      END  (* InRange *);

    PROCEDURE EnterPossibleTrap;

      VAR
	PossibleTrap : Thing;

      BEGIN
	WITH mtmp^ DO
	  BEGIN
	    PossibleTrap := ThingAt(locx, locy, traps);
	    IF  PossibleTrap <> NIL  THEN
	      IF  NOT rn2(5)  THEN
		BEGIN
		  PossibleTrap^.SeenByMonsters := True;
		  IF  NOT (species IN [ Bat, acidblob, Eye, yellowlight, fog,
					quiveringblob, killerbee, Vampire,
					LurkerAbove, TwoHeadedEagle,
					humanoid ])  THEN
		    CASE  PossibleTrap^.gflag  OF
		      BearTrap :
			IF  species = owlbear  THEN
			  TimeForTheMonsterToRemainInTrap := 7
			ELSE
			  TimeForTheMonsterToRemainInTrap := rn1(4,4);
		      Arrow,
		      dart     :
			IF  rn2(4)  THEN
			  ChangeMonstersHitPoints(mtmp, -d(2,6));
		      tdoor    :
			IF rn2(4) THEN
			  ChangeMonstersHitPoints(mtmp, -rnd(6))
			ELSE
			  IF NOT mtmp^.float THEN 
			    mhp := 0;  (* Kill it later.. *)
		      tele     :
			rloc(mtmp);
		      pit      :
		        IF NOT mtmp^.float THEN 
			  BEGIN
			    IF species IN mlarge THEN
			      TimeForTheMonsterToRemainInTrap := rn1(6,2)
			    ELSE
			      TimeForTheMonsterToRemainInTrap := rn1(2,2);
			    ChangeMonstersHitPoints(mtmp, -rnd(6));
			  END;
		      slptrp   :
			mstat := mfroz;
		      StinkingTrap :
		        IF rn2(2) THEN
			  BEGIN
			    mstat := mfroz;
			    mspeed := mconf;
			  END
		        ELSE 
			  ChangeMonstersHitPoints(mtmp, - rn1(10, dlevel * 2));
		      pierc,
		      mimictrap: (* Nothing *)
		    END  (* CASE  PossibleTrap^.gflag OF *);
		END;
	  END;
      END  (* EnterPossibleTrap *);

    FUNCTION rfree(x:byte; y:byte) : Boolean;

      VAR
	PossibleTrap : Thing;

      BEGIN  (* rfree *)
	IF  (x < xmin) OR (x > xmax) OR (y < ymin) OR (y > ymax)  THEN
	  rfree := False
	ELSE
	  IF  NOT (f^.levl[x,y].typ IN [ door, corr, RoomLocation ])
	      OR  ((x=u.ux) AND (y=u.uy)) THEN
	    rfree := False
	  ELSE
	    IF  ThingAt(x,y,monsters) <> NIL  THEN
	      rfree := False
	    ELSE
	      BEGIN
		PossibleTrap := ThingAt(x,y,traps);
		IF PossibleTrap = NIL THEN
		  rfree := True
		ELSE
		  rfree := NOT PossibleTrap^.SeenByMonsters OR rn2(3);
	      END;
      END  (* rfree *);

    BEGIN (* MonsterMoves *)
      MonsterMoves := True;	(* Default *)
      WITH  mtmp^  DO
	BEGIN
	  IF  ObjectsCarried <> NIL  THEN
	    IF  rn2(33) OR ((mstat = flee) AND
	        NOT (species IN [ Nymph, Leprechaun ]))  THEN
	      IF (species <> teleporter) AND (species <> Gnome) THEN
		BEGIN
		  stuff := ObjectsCarried;
		  MonsterDropsAnObjectItHasBeenCarrying(mtmp);
		  IF  f^.levl[locx, locy].CanSee  THEN
		    NewSym(locx, locy)
		  ELSE
		   news1(locx, locy);
		END;

	  IF  TimeForTheMonsterToRemainInTrap > 0  THEN
	    BEGIN
	      TimeForTheMonsterToRemainInTrap :=
		TimeForTheMonsterToRemainInTrap - 1;
	      MonsterMoves := False;
	      GOTO 9;
	    END;

	  dx := sgn(u.ux-locx);
	  dy := sgn(u.uy-locy);

	  IF  species = teleporter  THEN
	    IF f^.ListOf[objects] <> NIL THEN
	       WITH f^.ListOf[objects]^ DO		  
		 IF NOT rn2(25) THEN
		   BEGIN
		     dx := sgn(locx-mtmp^.locx);
		     dy := sgn(locy-mtmp^.locy);
		   END
		 ELSE
		   BEGIN
		     mnexto(mtmp,locx,locy);
		     GOTO 9;
		   END;

	  IF (mspeed = mconf) OR ((u.uinvis > 0) AND NOT mtmp^.SeeInv) OR
	     ((species IN [ Bat, beetle, Invisiblestalker,quiveringblob ])
	      AND rn2(3))  THEN
	    BEGIN dx := rn1(3,-1); dy := rn1(3,-1) END;

	  IF  NOT mcan  THEN
	    IF  (species = rat) AND (mstat = NormalState)  THEN
	      BEGIN
		IF  f^.NumberOfRats < MaxNumberOfRatsPerLevel  THEN
		  IF  rn2(30)  THEN
		    BEGIN
		      MakeMonster(rat, False);
		      (* To prevent causing a population explosion too bad to
			 be fun, we leave 4/5 of new rats in sleeping state. *)
		      IF  NOT rn2(5)  THEN
			f^.listof[monsters]^.mstat := sleep;
		      mnexto(f^.listof[monsters],locx,locy);
		    END;
	      END
	    ELSE IF  species = Dragon  THEN
		inrange(mtmp, BoltOfFire, False, GotYou)
	    ELSE IF species = Yeti THEN
	        inrange(mtmp, IcyWind, False, GotYou)
	    ELSE IF species = ugod THEN
		inrange(mtmp,missile, False, GotYou)
	    ELSE IF (species IN [ gelatenouscube, humanoid ]) AND
		     (ObjectsCarried <> NIL) THEN
	      IF ObjectsCarried^.Class = wands THEN
		WITH mtmp^.ObjectsCarried^ DO
		  BEGIN 
		    CASE wand OF
		      WandOfMissile : inrange(mtmp,missile, False, GotYou);
		      FireWand : inrange(mtmp,BoltOfFire, False, GotYou);
		      SleepWand : inrange(mtmp,SleepRay, False, GotYou);
		      cold : inrange(mtmp,BoltOfCold, False, GotYou);
		      death : inrange(mtmp,DeathRay, False, GotYou);
		      striking :
		        BEGIN  (* This may be done in a much more clever way *)
			  inrange(mtmp, missile, True, GotYou);
			  IF GotYou THEN
			    IF hitu(10, d(2,12), WandOfStriking) THEN;
			END;
		      SlowMonster :
		        BEGIN 
			  inrange(mtmp, missile, True, GotYou);
			  IF GotYou THEN
			    u.uslow := u.uslow + rn1(5,8);
			END;
		      DrainLife :
		        BEGIN 
			  inrange(mtmp, missile, True, GotYou);
			  IF GotYou THEN
			    IF mtmp^.mhp < u.uhp THEN
			      BEGIN 
				LoseHp(mtmp^.mhp, mtmp^.species);
				ChangeMonstersHitPoints(mtmp, -mtmp^.mhp);
			      END
			    ELSE
			      IF mtmp^.mhp = u.uhp THEN
				BEGIN 
				  ChangeMonstersHitPoints(mtmp, -mtmp^.mhp);
				  more;
				  LoseHp(u.uhp, RayOfDrainLife);
				END
			      ELSE
				LoseHp(u.uhp, RayOfDrainLife);
			END;
		      TeleportMonster :
		        BEGIN 
			  inrange(mtmp, missile, True, GotYou);
			  IF GotYou THEN
			    teleport(True);
			END;
		      WandOfLight, WandOfDarkness, detection, CreatorWand,
		      DestroyObject, SpeedMonster, UndeadTurning, polymorph,
		      cancellation, TameMonster, digging, WandOfFrobozz :
		        GotYou := False;
		    END(* CASE *);
		    IF GotYou THEN
		      spe := spe - 1;
		    IF spe = 0 THEN
		      MonsterDropsAnObjectItHasBeenCarrying(mtmp);
		  END;
				
	  IF  (mstat = flee) OR (mstat = scared)  THEN
	    BEGIN
	      IF  dx = 0  THEN
		dx := rn1(3,-1)
	      ELSE
		IF  dy = 0  THEN
		  dy := rn1(3,-1);
	      nix := locx - dx;
	      niy := locy - dy;
	    END
	  ELSE
	    IF  u.uswallow AND (u.ustuck <> mtmp) THEN
	      GOTO 9
	    ELSE
	      BEGIN
		nix := locx + dx;
		niy := locy + dy;
	      END;

	  omx := locx;
	  omy := locy;

	  (* The original version of the following piece of code made
	     a monster bypass other monsters only when it was
	     approaching the user DIAGONALLY. Now they bypass each
	     other more cleverly. --One of THE Jukkas-- *)

	  IF  (f^.levl[omx, omy].typ = door) OR
	      (f^.levl[nix, niy].typ = door)  THEN
	    BEGIN  (* DOORs are special, you can't move diagonally *)
	      IF rfree(locx, niy) AND (dy <> 0) THEN (* North or South ??? *)
		locy := niy
	      ELSE
		IF rfree(nix, locy) AND (dx <> 0) THEN (* West or East ??? *)
		  locx := nix
	    END
	  ELSE IF rfree(nix,niy) THEN (* Try easy way *)
	    BEGIN
	      locx := nix;
	      locy := niy
	    END
	  ELSE IF dx <> 0 THEN  (* IT doesn't try to go exactly vertically *)
	    IF dy = 0 THEN      (* Tried to move only horizontally *)
	      IF rfree(nix, locy+1) THEN 	(* SouthEast ??? *)
		BEGIN
		  locx := nix;
		  locy := locy + 1
		END
	      ELSE			(* NorthEast ??? *)
		IF rfree(nix, locy-1) THEN
		  BEGIN
		    locx := nix;
		    locy := locy - 1
		  END
		ELSE
		  BEGIN
		    IF rfree(locx, locy-1) THEN  (* North ??? *)
		      locy := locy - 1
		    ELSE
		      IF rfree(locx, locy+1) THEN (* South ??? *)
			locy := locy + 1
		  END
	   ELSE		(* Original tactic was to move diagonally, *)
	     BEGIN	(*   dy <> 0 AND dx <> 0 *)
	       IF rfree(locx, niy) THEN (* try vertical only  *)
		 locy := niy
	       ELSE
		 IF rfree(nix, locy) THEN (* horizontal only *)
		   locx := nix
		 ELSE
		   IF rfree(nix, locy-dy) THEN (* vertical opposite *)
		     BEGIN
		       locx := nix;
		       locy := locy - dy
		     END
		   ELSE
		     IF rfree(locx-dx, niy) THEN (* horizontal opposite *)
		       BEGIN
			 locx := locx - dx;
			 locy := niy
		       END
	     END
	   ELSE  (* dx = 0, @ is directly UP or DOWN *)
	     IF rfree(locx+1, niy) THEN  (* NorthEast or SouthEast ??? *)
	       BEGIN
		 locx := locx + 1;
		 locy := niy
	       END
	     ELSE
	       IF rfree(locx-1, niy) THEN (* NorthWest or SouthWest ??? *)
		 BEGIN
		   locx := locx - 1;
		   locy := niy
		 END
	       ELSE  (* this move wont actually move IT any nearer *)
		 IF rfree(locx-1,locy) THEN (* East ??? *)
		   locx := locx - 1
		 ELSE
		   IF rfree(locx+1, locy) THEN (* West ??? *)
		     locx := locx + 1;

	  IF  (omx=locx) AND (omy=locy)  THEN
	    BEGIN  (* The monster did not move *)
	      IF  species IN [ Teleporter, Nymph, Leprechaun ]  THEN
	      (* these monsters try to jump, if they can't move *)
		IF  rn2(10) OR (species = teleporter)  THEN 
		  BEGIN
		    rloc(mtmp);
		    IF f^.levl[omx, omy].Room <> f^.levl[locx, locy].Room THEN
		      IF f^.levl[locx, locy].Room = OrdinaryRoom THEN
			mspeed := normalspeed;
		  END;
	      MonsterMoves := False;
	    END
	  ELSE
	    BEGIN
	      (* Remove the symbol of the M from the old place;
	         Mimics are difficult: We don't know what it looked like *)

	      WITH F^.LEVL [ OMX, OMY ] DO (* capitalism is GREAT *)
	        IF CanSee THEN
		  NewSym(omx, omy)
		ELSE
		  IF (ScrSym = mon[species].mlet) OR
		     (species = Mimic) AND Seen THEN NewSym(omx, omy);
	      IF f^.levl[locx,locy].lit THEN
		BEGIN 
		  IF species IN [ Vampire, Wraith ] THEN
		    CASE mstat OF 
		      NormalState : mstat := flee;
		      sleep : mstat := Normalstate;
		      flee,
		      mfroz,
		      GuardingTreasures,
		      scared,
		      tamed : (* nothing *);
		    END 
		  ELSE
		    IF species IN [ yellowlight ] THEN 
		      CASE mstat OF 
			flee : mstat := NormalState;
			NormalState,
			sleep,
			mfroz,
			GuardingTreasures,
			scared,
			tamed : (* nothing *);
		      END 
		END (* IF lit *)
	      ELSE (* IF NOT lit *)
		IF species IN [ Vampire, Wraith, Zombie ] THEN
		  CASE mstat OF 
			flee : mstat := NormalState;
			NormalState,
			sleep,
			mfroz,
			GuardingTreasures,
			scared,
			tamed : (* nothing *);
		  END
		ELSE
		  IF species IN [ yellowlight, Eye, TyrannoSaur ] THEN
		    CASE mstat OF 
		      NormalState : mstat := flee;
		      sleep : mstat := Normalstate;
		      flee,
		      mfroz,
		      GuardingTreasures,
		      scared,
		      tamed : (* nothing *);
		    END;
		  
	      IF f^.levl[locx,locy].typ <> RoomLocation THEN
		BEGIN (* If they teleport, it's their problem *)
		 IF f^.levl[omx, omy].Room <> f^.levl[locx, locy].Room THEN
		   IF f^.levl[locx, locy].Room = OrdinaryRoom THEN
		     mspeed := normalspeed;
	        END
              ELSE
	       CASE f^.levl[locx,locy].room OF
		OrdinaryRoom,
		PoisonGasRoom:  (* Nothing *);
		HotRoom :
		  IF species IN [ fog, yellowlight, chameleon, snake, 
					dragon, demon ] THEN
		    mspeed := mfast
		  ELSE
		    IF NOT (species IN [ gelatenouscube, TyrannoSaur ]) THEN
		      IF NOT mtmp^.fireres THEN 
			mspeed := mslow;
		ColdRoom :
		  IF NOT (species IN [ fog, yeti, freezingsphere ,
				       gelatenouscube, humanoid ]) THEN
		    mspeed := mslow;
		WaterRoom:
		  IF NOT (species IN [ Bat, Eye, yellowlight, fog,
				       quiveringblob, killerbee, Snake,
				       Vampire, lurkerabove, Purpleworm,
				       TwoHeadedEagle ])
		  THEN
		    BEGIN
		      mspeed := mslow;
		      IF species = demon THEN
			mstat := scared; (* Water is not wellcomed in hell *)
		      IF rn2(30) THEN
			BEGIN
			  GoodBye(mtmp); (* Not good swimmers *)
			  GOTO 9;
			END;
		    END;
		ConfuseGasRoom:
		  mspeed := mconf;
	      END(* CASE *);
	      EnterPossibleTrap;
	      IF mhp = 0 THEN (* The poor M has died in the trap, Sniff *)
		BEGIN
		  GoodBye(mtmp);
		  GOTO 9;
		END;
	      IF  (ObjectsCarried = NIL) OR rn2(8) OR 
		  (species IN [ gnome, teleporter, humanoid ]) THEN 
		BEGIN
		  Stuff := ThingAt(locx, locy, Objects);
		  IF  Stuff <> NIL  THEN
		    BEGIN  (* he/she takes it, ki{h, ki{h *)
		      remove(stuff, f^.listof[objects]);
		      Eaten := False;
		      IF (Stuff^.class = Food) THEN
			IF rn2(4) OR
			  ((species IN mlarge) AND rn2(3)) OR
			  (species IN
			     [Jackal,  jaguar, trapper,  PurpleWorm,
			      leocrotta, cyclops, destroyer, TyrannoSaur ])
		           AND NOT rn2(3)
			THEN
			  BEGIN
			    IF Stuff^.fruit THEN
			      TimeForTheMonsterToRemainInTrap := 1
			    ELSE
			      TimeForTheMonsterToRemainInTrap := 5;
			    IF Stuff^.quan > 1 THEN
			      Stuff^.quan := Pred(Stuff^.quan)
			    ELSE
			      BEGIN
				Free(Stuff);
				Eaten := True;
			      END;
			  END;
		      IF Stuff^.class = potions THEN
			IF (Stuff^.potion = blood) AND
			   (species IN [ Vampire, Wraith, Zombie ]) THEN
			     BEGIN 
			       TimeForTheMonsterToRemainInTrap := 7;
			       mstat := tamed;
			       Free(Stuff);
			       Eaten := True;
			     END;
		      IF NOT Eaten THEN (*hungry*)
			IF species = destroyer THEN
			  Free(stuff)
			ELSE 
			  BEGIN 
			    Stuff^.next := mtmp^.ObjectsCarried;
			    mtmp^.ObjectsCarried := Stuff;
			    IF species IN [gelatenouscube, humanoid] THEN
			      InvestigatesWhatItHas(mtmp, species = humanoid);
			  END;
		      IF  f^.levl[locx, locy].CanSee  THEN
			NewSym(locx, locy)
		      ELSE
		        news1(locx, locy);
		    END;
		END;

	      (* Print the monster symbol to the  New place
	         (If it comes here it LIVES) *)

	      IF f^.levl[locx, locy].CanSee THEN
	        pmon(mtmp);
	    END
	END (* WITH mtmp^ *);
    9:
    END (* MonsterMoves *);

 PROCEDURE DoChug(VAR mtmp : thing);

    LABEL 9; (* for return *)

    VAR
      mdat : PerMonst;
      tmp  : Boolean; (* The monster hit *)
      spec : MonsterSpecies;
      ItHitsYou, Damage : Integer;

    FUNCTION Tame: Boolean;
      BEGIN
	Tame := mtmp^.mstat = tamed;
      END;

    PROCEDURE Steal(mtmp : thing; TryAmulet : Boolean);

	(* If TryAmulet is true, you have at least one Amulet
	   (It is checked everywhere before calling steal) *)

      VAR
	otmp : thing;

      BEGIN
	WriteEnemy(mtmp^.species, start);
	IF  (invent = NIL)  THEN
	  Out('looks frustrated...')
	ELSE
	  BEGIN
	    otmp := TakeAnObjectFromYou(TryAmulet);
	    out('stole ');
	    doname(otmp);

	    (* Transfer the stolen object to the monster *)

	    otmp^.next := mtmp^.ObjectsCarried;
	    mtmp^.ObjectsCarried := otmp;
	  END
      END (* Steal *);

    PROCEDURE LoseGold(HowMucho : positive);

      BEGIN
	u.ugold := u.ugold - HowMucho;
	flags.dgold := True;
	u.urexp := u.urexp - HowMucho;
      END;

    PROCEDURE StealsGold;

      VAR
	ThisGold   : Thing;
	GoldStolen : unsigned;

      BEGIN
	GoldStolen := rnd(u.ugold);
	IF  (GoldStolen < 100) AND (u.ugold < 100)  THEN
	  GoldStolen := u.ugold;
	LoseGold(GoldStolen);
	pline;
	out('Your purse feels lighter.');
	rloc(mtmp);
	MonsterBeginsToFlee(mtmp);
	Create(ThisGold, Golds);
	ThisGold^.quantity := GoldStolen;
	ThisGold^.Next := mtmp^.ObjectsCarried;
	mtmp^.ObjectsCarried := ThisGold;
      END  (* StealsGold *);

    PROCEDURE CurseSomethingYouHave(mtmp: thing);

      VAR
	otmp : thing;

      FUNCTION GetARandomObjectYouCarry: thing;

	VAR
	  otmp : thing;
	  tmp : unsigned;

	BEGIN(* GetARandomObjectYouCarry *)
	  otmp := invent;
	  IF otmp = NIL THEN
	    GetARandomObjectYouCarry := NIL
	  ELSE
	    BEGIN 
	      tmp := 0;
	      WHILE  otmp <> NIL  DO
		BEGIN
		  otmp := otmp^.next;
		  tmp := Succ(tmp);
		END;
	      tmp := rnd(tmp);

	      (* Get it! *)
	      otmp := invent;
	      WHILE  tmp > 1  DO
		BEGIN
		  otmp := otmp^.next;
		  tmp := Pred(tmp)
		END;
	      GetARandomObjectYouCarry := otmp;
	    END;
	END(* GetARandomObjectYouCarry *);

      BEGIN(* CurseSomethingYouHave *)
	pline;
	out('You hear a low whispering voice');
	otmp := GetARandomObjectYouCarry;
	IF otmp <> NIL THEN
	  BEGIN 
	    WHILE NOT (otmp^.class IN [ weapons, armors, rings ]) DO
	      otmp := GetARandomObjectYouCarry;
	    otmp^.cursed := NOT otmp^.cursed;
	    IF (otmp = uwep) AND otmp^.cursed THEN
	      CursedWeaponInHand;
	  END;
      END(* CurseSomethingYouHave *);

    PROCEDURE DestroySomeObjectYouHave;

      VAR
	otmp : thing;

      BEGIN(* DestroySomeObjectYouHave *)
	WriteEnemy(mtmp^.species, start);
	out('destroys ');
	otmp := TakeAnObjectFromYou(False);
	WHILE otmp^.class = amulet DO 
	  otmp := TakeAnObjectFromYou(False);
	doname(otmp);
      END(* DestroySomeObjectYouHave *);

    BEGIN  (* DoChug *)
      WITH mtmp^ DO
	BEGIN
	  IF  cham AND NOT u.ucham THEN
	    IF  rn2(6)  THEN
	      NewCham(mtmp,MonsterTable[rn1(6,2),rnd0(mtabN+1)]);
	  spec := species;
	  mdat := mon[ spec ];
	  IF  (species IN MonstersWhichRegenerateFast) OR
	      (f^.moves MOD 20 = 0) OR mtmp^.rege THEN (* Regenerate monster *)
	    ChangeMonstersHitPoints(mtmp,+1);

	  IF f^.levl[locx, locy].room = PoisonGasRoom THEN
	    BEGIN 
	      IF NOT (species IN [ acidblob, yellowlight, fog,
				   gelatenouscube, wraith, scorpion,
				   Vampire, demon]) THEN
		IF NOT mtmp^.pres THEN 
		  ChangeMonstersHitPoints(mtmp, -rnd(dlevel DIV 5));
	      IF mhp = 0 THEN
		BEGIN
		  GoodBye(mtmp);
		  GOTO 9;
		END;
	    END;

	  IF  mstat = mfroz THEN
	    GOTO 9; (* Frozen or tame monsters do nothing. *)

	  (* We use geometry in which distance between (x1,y1) and (x2,y2) is
	     the maximum of Abs(x1-x2) and Abs(y1-y2), since diagonal move is
	     as fast as vertical or horizontal. *)
	  distance := Abs(locx-u.ux);
	  dist2 := Abs(locy-u.uy);
	  IF  dist2 > distance  THEN
	    distance := dist2;

	  IF  (mstat = sleep) THEN
	    IF  f^.levl[locx,locy].CanSee AND NOT (u.ustelth OR (u.uinvis > 0))
		AND (u.uagmon OR rn2(distance+4))  THEN
	      mstat := NormalState
	    ELSE
	      GOTO 9;

	  IF  mstat = GuardingTreasures  THEN
	    IF  f^.levl[locx,locy].CanSee AND
		(NOT (u.ustelth OR (u.uinvis > 0)) OR
		 u.uagmon OR rn2(distance+11))  THEN
	      (* Wake up the monster if no stealth ring; also wake up
		 if both stealth and aggravate ring; if stealth and
		 not aggravate, then wake up "occasionally". *)
	      mstat := NormalState
	    ELSE
	      GOTO 9;

	  IF mspeed = mconf THEN
	    IF rn2(12) THEN mspeed := NormalSpeed;

	  IF (spec = Umberhulk) AND NOT mcan THEN
	    IF  (u.ublind=0) AND (u.uconfused=0) AND
		f^.levl[locx,locy].CanSee AND (rn2(9+distance) OR giant)  THEN
	     BEGIN
	       pline;
	       out('You are confused by');
	       WriteEnemy(UmberHulk,normal);
	       out('''s gaze!');
	       u.uconfused := d(3,4)
	     END;
	    
	  IF (spec = wumpus) AND NOT mcan THEN
	    IF distance <= 4 THEN
	      IF rn2(2+distance) OR giant THEN
	        BEGIN
		  pline;
		  out('Yacc, you smell a wumpus !');
		  IF distance = 0 THEN
		    LoseHp(rn1(5, 35), SmellOfWumpus)
		  ELSE 
		    LoseHp(2+rnd(19 DIV distance), SmellOfWumpus);
		END;


	  IF (spec = TyrannoSaur) AND (distance = 2) THEN
	    IF hitu(6, 6, TyrannoSaur) THEN;

	  IF (spec = TwoHeadedEagle) AND (distance = 2) THEN 
	    IF hitu(4, 4, TwoHeadedEagle) THEN;

	  IF (spec = exorcist) AND f^.levl[locx,locy].CanSee THEN
	    IF (rn2(6) OR giant) OR (mtmp^.TimeForTheMonsterToRemainInTrap > 0)
	      THEN
	        CurseSomethingYouHave(mtmp);

	  IF species IN [ gelatenouscube, humanoid ] THEN
	    IF ObjectsCarried <> NIL THEN
	      IF ObjectsCarried^.class IN [food, potions, scrolls] THEN 
		BEGIN 
		  InvestigatesWhatItHas(mtmp, species = humanoid);
		  GOTO 9;
		END;
	END  (* WITH mtmp^ *);
      

      IF mtmp^.Telport THEN
	IF rn2(40) THEN
	  rloc(mtmp);

      (* Move monster and exit if rate <= 12. *)
      IF  mdat.mmove >= rnd(12) - Ord(dlevel > 27) * (dlevel - 27) DIV 2 THEN
	IF  (mtmp^.mstat = flee) OR (mtmp^.mstat = scared) OR
	    (mtmp^.mspeed = mconf) OR ((u.uinvis > 0) AND NOT rn2(4))
	    OR (distance > 1)  THEN
	  IF  MonsterMoves(mtmp)  THEN
	    IF mtmp = NIL THEN	      (* M was killed in MonsterMoves *)
	      GOTO 9
            ELSE IF (mdat.mmove > 12-Ord(rn2(8))) AND NOT rn2(20) THEN
	      WITH  mtmp^  DO
		BEGIN
		  distance := Abs(locx-u.ux);
		  dist2 := Abs(locy-u.uy);
		  IF  dist2 > distance  THEN
		    distance := dist2;
		END
	    ELSE
	      GOTO 9;

      IF  (mtmp^.TimeForTheMonsterToRemainInTrap > 0)
	  OR (u.uswallow AND (mtmp <> u.ustuck))  THEN
	GOTO 9;

      IF  (distance > 1) OR (spec = acidblob) OR (spec = exorcist) THEN
	GOTO 9;

      (* Now the monster attacks and does it's special things in the
	 CASE statement below...   *)

      nomul(0);
      IF mtmp^.giant THEN 
	BEGIN 
	  ItHitsYou := 20;
	  Damage := d(mdat.damn * 5, mdat.damd);
	END 
      ELSE
 	BEGIN 
	  ItHitsYou := mdat.mhd DIV (1+Ord(mtmp^.mspeed = mconf)) +
		       (mtmp^.strength - 3);
	  Damage := d(mdat.damn,mdat.damd) + (mtmp^.strength - 3);
	END;

      IF  (NOT (spec IN [ demon, Dragon, yellowlight, Freezingsphere,
      		          Yeti, fog, suicider ])
	  AND NOT u.uswallow) AND NOT Tame  THEN
	tmp := hitu(ItHitsYou, Damage, spec)
      ELSE
	tmp := False;

      IF Tame THEN (* Tame monsters are quite jolly ones... *)
	tmp := True;

      IF  (u.uhp <= 0) OR ((mtmp^.mspeed = mconf) AND NOT rn2(4))  THEN GOTO 9;

      CASE  spec  OF

	Bat, Gnome, Hobgoblin, Kobold, acidblob, Eye, imp, rat,
	Zombie, piercer, quiveringblob,  zelomp,
	chameleon, Mimic, minotaur, xerp,
	Invisiblestalker, cyclops :
	  IF (YouHaveTheAmulet AND (tmp OR rn2(4))) AND
	     (dlevel < WorldOfGiants) THEN
	    BEGIN
	      Steal(mtmp, True);
	      WITH  mtmp^  DO
		IF  ObjectsCarried  <> NIL  THEN
		  IF  ObjectsCarried^.Class = Amulet  THEN
		    IF  NOT (Species IN [InvisibleStalker, chameleon])  THEN
		      IF  dlevel <= DeepestLevelSaved  THEN
			rloc(mtmp);
	    END;

	wumpus,(* Yacc... *)
	leocrotta,
	ugod : (* yaeynae ae ae *);

        fog    : 
	  BEGIN
	    WriteEnemy(fog, start);
	    out('suffocates you!');
	    LoseHp(d(2 + 10 * Ord(mtmp^.giant),4), fog);
	  END;

	teleporter :
	  IF mtmp^.mhp < 10 THEN
	    rloc(mtmp);		(* You almost got him *)

	Jackal  :
	  BEGIN  (* Souped up by THE Jukkas *)
	    IF  tmp AND (invent <> NIL)  THEN
	      IF  rn2(5) OR mtmp^.giant THEN
		WITH  invent^  DO
		  IF  Class = Food  THEN
		    BEGIN
		      WriteEnemy(Jackal, start);
		      out('seems fatter!');
		      IF  quan > 1  THEN
			quan := quan - 1
		      ELSE
			DeleteFirstElement(invent);
		    END
	  END;

	beetle		:
	  WITH  mtmp^  do
	    IF  NOT mcan AND NOT u.ucham  THEN
	      IF  rn2(3)  THEN
		invis := NOT invis;

	Orc		:
	  IF  tmp  THEN
	    IF  u.ugold > 0  THEN
	      IF  rn2(5) OR mtmp^.giant THEN
		BEGIN
		  LoseGold(rnd(u.ugold DIV 5)); (* WE COULD NOT DECIDE *)
		  WriteEnemy(Orc, start);
		  Out('will give the $$$ to orphan Orcs');
		END;

	Leprechaun	:
	  IF  tmp  THEN
	    IF  NOT mtmp^.mcan  THEN
	      IF  (u.ugold>0)  THEN
		IF  rn2(2) OR mtmp^.giant THEN
		  StealsGold;

	homunculus	:
	  IF  tmp  THEN
	    IF  multi >= 0  THEN
	      IF  NOT mtmp^.mcan  THEN
		IF  rn2(5) OR mtmp^.giant THEN
		  BEGIN
		    pline; out('You are put to sleep by');
		    WriteEnemy(homunculus, normal);
		    out('''s bite!');
		    nomul(-rnd(10));
		  END;

	yellowlight	:
	  IF  NOT mtmp^.mcan  THEN
	    IF  rn2(3) OR mtmp^.giant THEN
	      BEGIN
		GoodBye(mtmp);
		pline;
		IF  u.ublind = 0  THEN
		  BEGIN
		    out('You are blinded by a blast of light!');
		    u.ublind := d(4,12);
		    SeeOff(False);
		  END
		ELSE
		  BEGIN
		    out('You feel heat on your face!');
		    u.ublind := u.ublind + d(2,8)
		  END;
	      END;

	Ant		:
	  IF  tmp  THEN
	    IF  NOT mtmp^.mcan  THEN
	      IF  rn2(2) OR mtmp^.giant THEN
		BEGIN
		  pline; 
		  IF  u.upres  THEN
		    out('The sting has no effect on you.')
		  ELSE
		    BEGIN
		      out('You feel weaker!');
		      LoseStr(1, AntSting);
		    END;
		END;

	Nymph		:
	  IF  tmp OR YouHaveTheAmulet AND rn2(4)  THEN
	    IF  NOT mtmp^.mcan  THEN
	      IF  rn2(2) OR YouHaveTheAmulet OR mtmp^.giant  THEN
		BEGIN
		  Steal(Mtmp, YouHaveTheAmulet AND NOT rn2(4));
		  Rloc(Mtmp);
		  MonsterBeginsToFlee(Mtmp);
		END;

	Quasit		:
	  IF NOT (Tame OR mtmp^.giant) THEN 
	    BEGIN 
	      IF  hitu(3, rnd(2), Quasit)  THEN;
	      IF  hitu(3, rnd(2), Quasit)  THEN;
	    END;

	violetfungi	:
	  IF  tmp  THEN
	    IF  NOT mtmp^.mcan  THEN
	      IF  u.ustuck = NIL  THEN
		u.ustuck := mtmp;

	Centaur		:
	  IF NOT (Tame OR mtmp^.giant) THEN 
	    IF  hitu(4, rnd(6), Centaur)  THEN;

	cockatrice	:
	  IF  tmp AND NOT mtmp^.mcan  THEN
	    IF  (u.ustuck = NIL) OR (u.ustuck = mtmp)  THEN
	      IF  (rn2(4+2*Ord(TurningToStone < 0)) OR mtmp^.giant) AND
		  (u.uhp > 0)  THEN
		BEGIN
		  u.ustuck := mtmp;
		  pline;
		  IF  TurningToStone >= 0  THEN
		    BEGIN
		      TurningToStone := - rn1(4,2);
		      out('You start to feel a little stoned!');
		    END  (* IF  TurningToStone >= 0 *)
		  ELSE
		    BEGIN
		      TurningToStone := TurningToStone - Rnd(3);
		      out('You are feeling more and more stoned!');
		    END;
		END;

	gelatenouscube	:
	  IF  tmp  THEN
	    IF  NOT mtmp^.mcan  THEN
	      IF  rn2(3) OR mtmp^.giant THEN
		BEGIN 
		  Steal(mtmp, False);
		  InvestigatesWhatItHas(mtmp, False);
		END;

	jaguar		:
	  IF NOT (Tame OR mtmp^.giant) THEN 
	    BEGIN
	      tmp := hitu(4, rnd(3), jaguar);
	      IF  NOT hitu(4, rnd(3), jaguar)  THEN  tmp:= False;
	      IF  tmp AND (u.uhp > 0)  THEN
		BEGIN
		  IF  hitu(4, rnd(4), jaguar)  THEN;
		  IF  hitu(4, rnd(4), jaguar)  THEN;
		END;
	    END;
	
	killerbee	:
	  IF NOT (Tame OR mtmp^.giant) THEN 
	    IF  hitu(4, rnd(4), killerbee) AND (u.uhp > 0)  THEN
	      IF  NOT mtmp^.mcan  THEN
		IF  rn2(8)  THEN
		  poisoned(BeesSting);

	Snake		:
	  IF  tmp  THEN
	    IF  NOT mtmp^.mcan  THEN
	      IF  rn2(8) OR mtmp^.giant THEN
		poisoned(SnakesBite);

	FreezingSphere	:
	  IF  NOT mtmp^.mcan  THEN
	    IF  rn2(3) OR mtmp^.giant THEN
	      BEGIN
		WriteEnemy(FreezingSphere, start);
		out('explodes!');
		pline;
		IF  u.ucoldres  THEN
		  out('You don''t seem effected by it.')
		ELSE
		  IF  17 - u.ulevel DIV 2 > rnd(20)  THEN
		    BEGIN
		      out('You get blasted!');
		      LoseHp(d(6 + 6 * Ord(mtmp^.giant),6), FreezingSphere);
		    END
		  ELSE
		    BEGIN
		      out('You duck the blast...');
		      LoseHp(d(3 + 3 * Ord(mtmp^.giant), 6), FreezingSphere);
		    END;
		GoodBye(mtmp);
	      END;

	owlbear		:
	  BEGIN
	    IF NOT (Tame OR mtmp^.giant) THEN 
	      tmp := hitu(5, rnd(6), owlbear);
	    IF NOT (Tame OR mtmp^.giant) THEN
	      BEGIN 
		IF  NOT hitu(5, rnd(6), owlbear) THEN
		  tmp := False;
	      END
	    ELSE
	      tmp := True;
	    IF  u.uhp > 0  THEN
	      IF  tmp AND NOT mtmp^.mcan AND (u.ustuck=NIL) AND
		  (rn2(2) OR Tame OR mtmp^.giant) THEN 
		BEGIN
		  u.ustuck := mtmp;
		  WriteEnemy(owlbear, start);
		  out('has grabbed you!');
		  LoseHp(d(2,8),owlbear);
		END
	      ELSE
		IF  u.ustuck = mtmp  THEN
		  BEGIN
		    LoseHp(d(1+Ord(tmp),8),owlbear);
		    pline;
		    out('You are being crushed.');
		  END;
	  END;

	Rust		:
	 IF NOT Tame THEN 
	  BEGIN
	    IF NOT mtmp^.giant THEN 
	      IF  hitu(5 + 10 * Ord(mtmp^.giant), 0, Rust)  THEN
		tmp := True;
	    IF  tmp AND NOT mtmp^.mcan AND (uarm <> NIL)  THEN
	      WITH uarm^ DO
		IF  (armor <> leather) AND NOT Rn2(7) AND
		    (NOT minus OR (8 - (Ord(armor) - spe) < 9))  THEN
		  BEGIN
		    pline;out('Your armor rusts!');
		    IF u.umaintarm THEN
		      BEGIN
			pline;
			out('The rust vanishes!');
		      END
		    ELSE 
		      BEGIN 
			MinusOne(uarm);
			u.uac := u.uac + 1;
			flags.dac := True;
		      END;
		  END;
	  END;

	scorpion	:
         IF NOT Tame THEN 
	  BEGIN
	    IF tmp AND (rn2(8) OR mtmp^.giant) THEN poisoned(ScorpionsSting);
	    IF NOT mtmp^.giant THEN
	      BEGIN 
		IF  hitu(5, rnd(8), scorpion)  THEN;
		IF  hitu(5, rnd(8), scorpion)  THEN;
	      END;
	  END;

	Wraith		:
	  IF  NOT mtmp^.mcan AND tmp AND (rn2(5) OR mtmp^.giant) THEN
	    BEGIN
	      LosExp;
	      IF  u.uhp <= 0  THEN
		killer := Wraith
	    END;

	Yeti		:
         IF NOT Tame THEN 
	  IF  (NOT rn2(6) OR mtmp^.mcan) AND NOT mtmp^.giant THEN
	    BEGIN
	     IF hitu(4, rnd(6), Yeti)  THEN;
	    END
	  ELSE
	    BEGIN
	      pline;
	      Out('You feel the cold breath of');
	      WriteEnemy(Yeti, normal);
	      OutCh('!');
	      WITH  mtmp^  DO
		Buzz(IcyWind, locx, locy, Sgn(u.ux-locx), Sgn(u.uy-locy));
	      IF  u.uhp <= 0  THEN  killer := Yeti;
	    END;

	displacer	:
         IF NOT (Tame OR mtmp^.giant) THEN
	   BEGIN 
	     IF  hitu(6, d(2,4), displacer) AND (u.uhp > 0)  THEN
	       IF  NOT mtmp^.mcan AND rn2(2)  THEN
		 teleport(True);	(* Added this... THE Jukkas *)
	   END
	 ELSE
	   teleport(True); (* It is quite difficult to kill a tame displacer *)

	Troll		:
         IF NOT Tame THEN 
	  BEGIN
	    IF NOT mtmp^.giant THEN
	      BEGIN 
		IF  hitu(6, rnd(6), Troll)  THEN;
		IF  hitu(6, rnd(6), Troll)  THEN;
	      END;
	    IF  tmp OR YouHaveTheAmulet AND
	        (rn2(3) OR mtmp^.giant) THEN
	      IF  NOT mtmp^.mcan  THEN
		IF NOT YouHaveTheAmulet THEN
		  BEGIN
		    IF rn2(5) OR mtmp^.giant THEN
		      Steal(mtmp,False);
		  END
		ELSE
		  IF rn2(2) AND NOT mtmp^.giant THEN
		    BEGIN
		      Steal(mtmp, NOT rn2(3));
		      WITH  mtmp^  DO
			IF  ObjectsCarried  <> NIL  THEN
			  IF  ObjectsCarried^.Class = Amulet  THEN
			    MonsterBeginsToFlee(Mtmp)
		    END
	  END;

	Umberhulk	:
         IF NOT (Tame OR mtmp^.giant) THEN 
	  BEGIN
	    IF  hitu(9, d(3,4), Umberhulk)  THEN;
	    IF  hitu(9, d(3,4), Umberhulk)  THEN;
	  END;

	Vampire		:
	  IF  tmp  THEN
	    BEGIN
	      u.uhp := u.uhp - 4;
	      IF  NOT mtmp^.mcan AND (rn2(3) OR mtmp^.giant) THEN
		LosExp;
	      IF  u.uhp <= 0  THEN
		killer := Vampire;
	    END;

	Xorn		:
         IF NOT (Tame OR mtmp^.giant) THEN 
	  BEGIN
	    IF  hitu(8, rnd(3), Xorn)  THEN;
	    IF  hitu(8, rnd(3), Xorn)  THEN;
	    IF  hitu(8, rnd(3), Xorn)  THEN;
	  END;

	Dragon		:
         IF NOT Tame THEN 
	  IF  (NOT rn2(6) AND NOT mtmp^.giant) OR mtmp^.mcan  THEN
	    BEGIN
	      IF  hitu(10, d(3,10), Dragon)  THEN;
	      IF  hitu(10, 8, Dragon)  THEN;
	      IF  hitu(10, 8, Dragon)  THEN;
	    END
	  ELSE
	    BEGIN
	      WriteEnemy(Dragon, start);
	      out('breathes fire!');
	      WITH  mtmp^  DO
		Buzz(BoltOfFire, locx, locy, Sgn(u.ux-locx), Sgn(u.uy-locy));
	      IF  u.uhp <= 0  THEN  killer := Dragon;
	    END;

	ettin		:
         IF NOT Tame THEN 
	  IF  hitu(10, d(3,6), ettin)  THEN
	    IF rn2(3) OR mtmp^.giant THEN		(* Heheh *)
	      BEGIN
	        IF uright <> NIL THEN
	          RingEffect(uright, False);
	        IF uleft <> NIL THEN
	          RingEffect(uleft, False);
	        uright := NIL;
	        uleft  := NIL;
	      END;

	LurkerAbove	:
	  IF  u.uswallow  THEN
	    YouSwld(mtmp, rnd(6), 7)
	  ELSE
	    IF  tmp  THEN
	      JustSwld(mtmp);

	neootyugh	:
         IF NOT (Tame OR mtmp^.giant) THEN 
	  BEGIN
	    IF  hitu(11, d(2,6), neootyugh)  THEN;
	    IF  hitu(11, d(2,6), neootyugh)  THEN;
	  END;

	trapper		:
	  IF  u.uswallow  THEN
	    IF  u.uac <= -4  THEN
	      YouSwld(mtmp, 1, 5)
	    ELSE
	      YouSwld(mtmp, 4+u.uac, 5)
	  ELSE
	    IF  tmp  THEN
	      JustSwld(mtmp);

	PurpleWorm	:
	  IF  u.uswallow  THEN
	    YouSwld(mtmp, d(2,4), 12)
	  ELSE
	    IF  NOT mtmp^.mcan AND tmp AND (rn2(4) OR Tame) THEN
	      JustSwld(mtmp)
	    ELSE
	      IF  hitu(15, d(2,4), Purpleworm)  THEN;

	demon :
         IF NOT Tame THEN 
	  IF  NOT mtmp^.mcan AND rn2(15) THEN
	    CreateSomeMonsterAndPutItBesideU(demon)
	  ELSE
	    BEGIN
	      IF hitu(10, d(2,6), demon) THEN;
	      IF hitu(10, d(2,6), demon) THEN;
	      IF hitu(10, rnd(3), demon) THEN;
	      IF hitu(10, rnd(3), demon) THEN;
	      IF hitu(10, rn1(4,2), demon) THEN;
	    END
	 ELSE (* Tame demons are quite social creatures... *)
	   BEGIN
	    CreateSomeMonsterAndPutItBesideU(demon);
	    CreateSomeMonsterAndPutItBesideU(demon);
	   END;

	destroyer :
	  IF tmp AND NOT mtmp^.mcan THEN
	    IF rn2(6) OR mtmp^.giant THEN 
	      DestroySomeObjectYouHave;

	suicider :
	  IF NOT mtmp^.mcan THEN
	    BEGIN
	      pline;
	      writeEnemy(suicider, start);
	      out('attacks You and dies');
	      LoseHp(Abs(mtmp^.mhp - 2 * (6 - u.uac)), suicider);
	      GoodBye(mtmp);
	    END;

	TyrannoSaur :
	  BEGIN
	    IF NOT mtmp^.giant THEN 
	      IF hitu(5, 5, TyrannoSaur) THEN;
  (* 	    IF distance = 1 THEN mtmp^.mstat := flee; *)
	  END;

	TwoHeadedEagle :
	  IF NOT mtmp^.giant THEN 
	    IF hitu(4, 4, TwoHeadedEagle) THEN;

	humanoid :
	  BEGIN
	    IF mtmp^.mhp >= 40 THEN
	      BEGIN 
		IF tmp AND NOT Tame THEN
		  BEGIN 
		    IF rn2(2) OR mtmp^.giant THEN
		      BEGIN 
			Steal(mtmp, False);
			IF rn2(2) AND NOT mtmp^.giant THEN rloc(mtmp);
			InvestigatesWhatItHas(mtmp, True);
		      END;
		  END;
	      END
	    ELSE
	      rloc(mtmp);  (* He is very careful *)
	  END;
	      
      END  (* CASE  spec *);

      IF  u.uhp > 0  THEN
	IF  mtmp <> NIL  THEN   (* What? Still alive!!! *)
	  IF  mdat.mmove-12 > rnd(12) - Ord(dlevel > 27) * (dlevel-27) DIV 2
	    THEN 
	      IF MonsterMoves(mtmp) THEN; (* extra movement for fast M. *)

    9:
    END  (* DoChug *);

  BEGIN  (* MoveMonsters *)
    monster := f^.listof[monsters];
    WHILE  (monster <> NIL) AND (u.uhp > 0)  DO
      BEGIN
	NextOne := monster^.next;
	IF  (monster^.mspeed <> mslow) OR NOT Odd(f^.moves)  THEN
	  dochug(monster);
	IF  flags.dhp  THEN
	  UpdateHitPointInformation;
	IF  (monster <> NIL) AND (u.uhp > 0)  THEN
	  IF  monster^.mspeed = mfast  THEN
	    dochug(monster);
	(* Dochug may delete the monster. (see comment in GoodBye)
	   This is why we save the pointer to the next monster and
	   test against nil pointer before recalling dochug. *)
	IF  flags.dhp  THEN
	  UpdateHitPointInformation;
	monster := NextOne;
      END;
  END  (* MoveMonsters *);

(* removed this ....
PROCEDURE ResCham;
( * Force all chameleons to become normal. * )

  VAR
    AnyMonster : thing;

  BEGIN
    AnyMonster := f^.listof[monsters];
    WHILE  AnyMonster <> NIL  DO
      WITH  AnyMonster^  DO
	BEGIN
	  IF  cham  THEN
	    BEGIN
	      cham := False;
	      NewCham(AnyMonster, chameleon);
	    END;
	  AnyMonster := next;
	END;
  END  ( * ResCham * );

end of removed code...   *)

PROCEDURE ReadInteger(VAR value:Integer; VAR ch:Char);
 BEGIN
     WHILE (ch >= '0') AND (ch <= '9') DO
       BEGIN
         IF flags.echo THEN OutCh(ch); (* echo first digit *)
	 (* prevent integer overflow; perfectionistic...*)
	 IF value > Maxint DIV 10 THEN
	   value := Maxint
	 ELSE
	   value := value*10 + Ord(ch) - Ord('0');
	 tcRdCh(ch);
       END;
 END;

PROCEDURE useup(VAR obj:thing);
 BEGIN
   WITH obj^ DO
     IF quan > 1 THEN
       quan := quan - 1
     ELSE
       delete(obj, invent);
 END;

PROCEDURE Separate(Num : small; obj : thing);

  VAR
    otmp : thing;

  BEGIN
    WITH obj^ DO quan := quan - num;
    create(otmp, objects);
    otmp^ := obj^;
    otmp^.quan := num;
    AddToList(otmp, f^.listof[objects]);
  END;

PROCEDURE LoseOne(obj : thing);
  (* Separate one item of "obj" and make it first in
     f^.listof[objects]. *)

  BEGIN
    WITH  obj^  DO
      IF  NOT (class IN [wands,rings]) AND (quan > 1)  THEN
	separate(1, obj)
      ELSE
	BEGIN
	  remove(obj, invent);
	  AddToList(obj, f^.listof[objects]);
	END;
  END;

PROCEDURE CursedWeaponInHand;

  BEGIN
    pline;
    out(LitThe);
    WriteArmament(uwep^.weapon);
    out(litWelded)
  END;

PROCEDURE DoCall(obj : thing);

  PROCEDURE CallIt(VAR  Buf : Name);

    BEGIN
      pline;
      tcGetString('Call it:',  buf);
      CursorHome
    END;

  BEGIN   (* DoCall *)
    CASE obj^.class OF
      Potions : CallIt(potcal[obj^.potion]);
      Scrolls : CallIt(scrcal[obj^.scroll]);
      Wands   : CallIt(wandcal[obj^.wand]);
      Rings   : CallIt(ringcal[obj^.ring]);
    END;
    flags.topl := False;
  END;

PROCEDURE DoDrop;

  LABEL 13;

  VAR
    obj  : thing;
    ch	 : Char;
    num	 : Integer;

  PROCEDURE DoDr1(obj : thing);

    BEGIN
      IF  (obj=uarm) OR (obj=uright) OR (obj=uleft)  THEN
	BEGIN
	  pline;
	  out('You cannot drop something you are wearing.');
	  multi := 0;
	  flags.move := False;
	END
      ELSE
	IF  (obj = uwep) AND obj^.Cursed  THEN
	  CursedWeaponInHand
	ELSE
	  IF  obj^.Class = Amulet  THEN
	    BEGIN
	      pline;
	      out('You cannot get rid of The Amulet of Frobozz!');
	      multi := 0;
	      flags.move := False
	    END  (* IF  obj^.Class = Amulet *)
	  ELSE
	    BEGIN
	      IF  obj = uwep  THEN
		uwep := NIL;
	      remove(obj, invent);
	      AddToList(obj, f^.listof[objects]);
	      WITH  obj^  DO
		BEGIN
		  locx := u.ux;
		  locy := u.uy
		END;
	      pline;
	      out('You dropped ');
	      doname(obj);
	    END;
    END  (* DoDr1 *);

  BEGIN  (* DoDrop *)
    obj := GetObj(AllObjects, 'drop    ');
    IF  obj = NIL  THEN
      BEGIN
	multi := 0;
	flags.move := False
      END
    ELSE
      IF  obj^.quan > 1  THEN
	BEGIN
	  pline;
	  out('How many do you want to drop? (');
	  WriteNumber(obj^.quan, 1);
	  out(' max) ?');
	  num := 0;
	  REPEAT tcRdCh(ch) UNTIL ch <> blank;
	  IF  ch = Chr(ESC)  THEN
	    BEGIN
	      pline;
	      GOTO  13
	    END  (* IF  ch = Chr(ESC) *);
	  IF  (ch = Chr(LF)) OR (ch = Chr(CR))  THEN
	    num := obj^.quan
	  ELSE
	    BEGIN
	      flags.echo := True;
	      ReadInteger(num, ch);
	      flags.echo := False
	    END;
	  IF  (num < 1) OR (num > obj^.quan)  THEN
	    BEGIN
	      pline;
	      out('You can''t drop that many!');
	      multi := 0;
	      flags.move := False;
	    END
	  ELSE
	    IF  num = obj^.quan  THEN
	      dodr1(obj)
	    ELSE
	      BEGIN  (* num IN [1..obj^.quan-1] *)
		separate(num, obj);
		  (* The separated object in now first in the list. *)
		WITH  f^.listof[objects]^  DO
		  BEGIN
		    locx := u.ux;
		    locy := u.uy;
		  END;
		pline;
		out('You dropped ');
		doname(f^.listof[objects]);
	      END  (* num IN [1..obj^.quan-1] *);
	END  (* IF  obj^.quan > 1 *)
      ELSE  dodr1(obj);
    13:
  END  (* DoDrop *);

PROCEDURE RingEffect (* obj : thing; flag : Boolean *);
  (* flag = True means putting ring on *)

  BEGIN
    WITH  obj^  DO
      CASE  ring  OF
	adornment	 : ;
	TeleportRing	 : u.utel := flag;
	regeneration	 : u.uregen := flag;
	searching	 : u.usearch := flag;
	SeeInvisible	 : u.ucinvis := flag;
	stealth		 : u.ustelth := flag;
	floating	 : 
	   BEGIN
	     u.ufloat := flag;
	     IF flag THEN
	       BEGIN 
		 StopSwimming;
		 IF u.utrap > 0 THEN IF u.upit = pit THEN
		   BEGIN
		     pline;
		     out('You have a strange feeling for a moment...');
		     u.utrap := 0;
		   END;
	       END;
	   END;
	PoisonResistance : u.upres := flag;
	aggravate	 : u.uagmon := flag;
	hunger		 : u.ufeed := flag;
	FireResistance	 : u.ufireres := flag;
	ColdResistance	 : u.ucoldres := flag;
	ProtectionFromShapeChangers :
			   BEGIN
			     u.ucham := flag;
			     (*  removed this one..  
			     IF  flag  THEN
			       ResCham		*)
			   END;
	AddStrength	 : BEGIN
			     IF  minus = flag  THEN
			       BEGIN
				 u.ustr := u.ustr - spe;
				 u.ustrmax := u.ustrmax - spe;
				 YouCanCarry := YourCarryAbility(u.ustr)
			       END
			     ELSE
			       BEGIN
				 u.ustr := u.ustr + spe;
				 u.ustrmax := u.ustrmax + spe;
				 YouCanCarry := YourCarryAbility(u.ustr)
			       END;
			     flags.dstr := True;
			   END;
	IncreaseDamage   : IF  minus = flag  THEN
			     u.udaminc := u.udaminc - spe
			   ELSE
			     u.udaminc := u.udaminc + spe;
	protection	 : BEGIN
			     IF  minus = flag  THEN
			       u.uac := u.uac + spe
			     ELSE
			       u.uac := u.uac - spe;
			     flags.dac := True;
			   END;
	MaintainArmor    : u.umaintarm := flag;
	RingOfFrobozz	 : BEGIN
			     u.ustelth := flag;
			     IF NOT flag AND (dlevel = WorldOfGiants) THEN
			       AggravateThem;
			   END;
      END (*CASE*);
  END  (* RingEffect *);

PROCEDURE ArmorOff;
  BEGIN
    WITH uarm^ DO
      BEGIN
	u.uac := u.uac + Ord(armor) + ArmorOffset;
	IF minus THEN u.uac := u.uac - spe
		 ELSE u.uac := u.uac + spe;
      END;
    flags.dac := True;
  END;

PROCEDURE DoWear;

  LABEL 9; (* For return *)

  VAR
    otmp : thing;
    ch	 : Char;

  BEGIN
    otmp := GetObj([rings,armors], 'wear    ');
    IF  otmp = NIL  THEN
      BEGIN
	multi := 0;
	flags.move := False;
	GOTO 9
      END;
    IF  otmp^.class = armors  THEN
      BEGIN
	IF  uarm <> NIL  THEN
	  BEGIN
	    flags.move := False;
	    pline;
	    out('You are already wearing an armor.');
	    GOTO 9
	  END;
	uarm := otmp;
	nomul(-5);
	WITH  uarm^  DO
	  BEGIN
	    known := True;
	    u.uac := u.uac - (Ord(armor)+ArmorOffset);
	   IF  minus  THEN  u.uac := u.uac + spe
		      ELSE  u.uac := u.uac - spe;
	  END;
	flags.dac := True;
      END
    ELSE
      IF  (uleft <> NIL) AND (uright <> NIL)  THEN
	BEGIN
	  pline;
	  out('There are no more fingers to fill.');
	  multi := 0;
	  flags.move := False;
	  GOTO 9
	END
      ELSE
	IF  (otmp=uleft) OR (otmp=uright)  THEN
	  BEGIN
	    pline;
	    out('You are already wearing that.');
	    multi := 0;
	    flags.move := False;
	    GOTO 9
	  END
	ELSE
	  BEGIN
	    IF  uright <> NIL  THEN
	      uleft := otmp
	    ELSE
	      IF  uleft <> NIL  THEN
		uright := otmp
	      ELSE
		BEGIN
		  REPEAT
		    pline;
		    out('What finger, Right or Left? ');
		    tcRdCh(ch);
		    flags.topl := False;
		  UNTIL ch IN ['L', 'l', 'R', 'r'];
		  IF  ch IN ['L', 'l']  THEN
		    uleft := otmp
		  ELSE
		    uright := otmp;
		END;
	    RingEffect(otmp, True);
	  END;
    prinv(otmp);
  9:
  END  (* DoWear *);

PROCEDURE RingOff(obj:thing; OtherRing:thing);
 LABEL 9;
BEGIN
  WITH obj^ DO
    IF ring < AddStrength THEN
    (* it affects an on/off property; no not take that property
       away if wearing similar ring in the other hand *)
      IF OtherRing <> NIL THEN
	IF ring = OtherRing^.ring THEN
	  GOTO 9;
  RingEffect(obj, False);
9:
END;

PROCEDURE DoTakeOffArmor;

  BEGIN
    multi := 0;
    pline;
    IF  uarm = NIL  THEN
      BEGIN
	flags.move := False;
	out('You are not wearing any armor!')
      END
    ELSE
      IF  uarm^.cursed  THEN
	out(litCursed)
      ELSE
	BEGIN
	  nomul(-5);
	  ArmorOff;
	  out('Your armor was ');
	  doname(uarm);
	  uarm := NIL;
	END;
  END  (* DoTakeOffArmor *);

PROCEDURE DoRemove;

  VAR
    obj : thing;

  BEGIN
    multi := 0;
    obj := GetObj([rings,armors], 'remove  ');
    IF  obj = NIL  THEN
      flags.move := False
    ELSE
      BEGIN
	pline;
	IF  (obj <> uarm) AND (obj <> uleft) AND (obj <> uright)  THEN
	  BEGIN
	    out('You can''t remove that.');
	    flags.move := False
	  END
	ELSE
	  IF  obj^.cursed  THEN
	    out(litCursed)
	  ELSE
	    BEGIN
	      IF  obj = uarm  THEN
		BEGIN
		  nomul(-5);
		  ArmorOff;
		  uarm := NIL;
		END
	      ELSE
		IF  obj = uleft  THEN
		  BEGIN
		    uleft := NIL;
		    RingOff(obj,uright) 
		  END
		ELSE (* obj = uright *)
		  BEGIN
		    uright := NIL;
		    RingOff(obj,uleft)
		  END;
	      out('You were wearing ');
	      doname(obj);
	    END;
      END;
  END  (* DoRemove *);

PROCEDURE LessHungry(Num : Unsigned);

  BEGIN  (* LessHungry *)
    u.uhunger := u.uhunger + num;  
    IF  u.uhunger > MaxHungry  THEN
      BEGIN 
	pline;
	out('You feel a bit dizzy because of eating too much');
	u.uconfused := u.uconfused + (u.uhunger - MaxHungry) DIV 50 + 6;
	u.uhunger := MaxHungry;
      END;
    (* Update hungry status, too. Notice that in the IF
       statements u.uhs reflects the old status. *)
    IF  (u.uhs >= hungry) AND (u.uhunger > HungryLimit)  THEN
      BEGIN
	IF  (u.uhs >= weak) AND (u.ustr<u.ustrmax)  THEN
	  LoseStr(-1, nomonster);
	flags.dhs := True;
	u.uhs := NotHungry;
      END
    ELSE
      IF  (u.uhs >= weak) AND (u.uhunger > WeakLimit)  THEN
	BEGIN
	  pline;out('You only feel hungry now.');
	  IF (u.ustr<u.ustrmax) THEN
	    LoseStr(-1, nomonster);
	  flags.dhs := True;
	  u.uhs := hungry;
	END
      ELSE
	IF (u.uhs = fainting) AND (u.uhunger < WeakLimit) THEN
	  BEGIN
	    pline;out('You feel weak now.');
	    flags.dhs := True;
	    u.uhs := weak;
	  END;
  END  (* LessHungry *);

PROCEDURE DoEat;

  VAR
    otmp : thing;

  BEGIN  (* DoEat *)
    otmp := GetObj([food],'eat     ');
    IF  otmp = NIL  THEN
      BEGIN
	multi := 0;
	flags.move := False
      END
    ELSE
      BEGIN
	pline;
	IF  rn2(7)  THEN
	  BEGIN
	    out('Blecch!  Rotten food!');
	    IF  rn2(4)  THEN
	      BEGIN
		pline;
		out('You feel rather light headed.');
		u.uconfused := u.uconfused + d(2,4);
	      END
	    ELSE
	      IF  (u.ublind = 0) AND rn2(4)  THEN
		BEGIN
		  pline;
		  out('Everything suddenly goes dark.');
		  u.ublind := u.ublind + d(2,10);
		  SeeOff(False);
		END
	      ELSE
		IF  rn2(3)  THEN
		  BEGIN
		    pline;
		    out('The world spins and goes dark.');
		    nomul(-rnd(10));
		  END;
	    IF  otmp^.fruit  THEN
	      LessHungry(20)
	    ELSE  LessHungry(200);
	  END  (* IF  rn2(7) *)
	ELSE
	  IF  otmp^.fruit  THEN
	    BEGIN
	      out('That fruit was delicious');
	      LessHungry(80);
	    END
	  ELSE
	    BEGIN  (* normal food *)
	      out('That food really hit the spot!');
	      LessHungry(800);
	      multi := -5;
	    END;
	UseUp(otmp);
      END;
  END  (* DoEat *);
    
PROCEDURE nothin(VAR obj:thing);
(* strange feelings from potions and scrolls *)
BEGIN
 pline;out('You have a strange feeling for a moment, then it passes.');
 IF obj <> NIL THEN
   BEGIN 
     WITH obj^ DO
       IF class = scrolls THEN
	 BEGIN
	   IF NOT (scroll IN KnownScrolls) AND
	      (scrcal[scroll] = blankname) THEN
		docall(obj);
	 END
       ELSE IF class = potions THEN
	 IF NOT (potion IN KnownPotions) AND
	   (potcal[potion] = blankname) THEN
	     docall(obj);
     UseUp(obj);
   END;
 obj := NIL; (* tell the caller that this thing is still unknown *)
END;

PROCEDURE HealingEffect(hpinc,hpmaxinc:small);
  VAR newhp:ShortCount;
 BEGIN
   newhp := u.uhp + hpinc;
   IF newhp > u.uhpmax THEN
     BEGIN
       u.uhpmax := u.uhpmax + hpmaxinc;
       u.uhp := u.uhpmax;
       flags.dhpmax := True;
     END
   ELSE
     u.uhp := newhp;
   flags.dhp := True;
   IF u.ublind > 0 THEN u.ublind := 1;
 END;

PROCEDURE DoDrink;

  LABEL 9;

  VAR
    tmp, otmp : thing;
    SheLikesIt : Boolean;

  PROCEDURE Reveal(What : ThingType);

    VAR
      th : thing;
      ch : Char;

    BEGIN
      th := f^.listof[what];
      IF  th = NIL  THEN
	Nothin(otmp)
      ELSE
	BEGIN
	  ClearScreen;
	  REPEAT
	    WITH  th^  DO
	      BEGIN
		CASE  what  OF
		  objects  : ch := olet;
		  monsters : ch := mon[species].mlet;
		END;
		at(locx,locy,ch);
		th := next;
	      END;
	  UNTIL th = NIL;
	  pru;
	  flags.topl := False;
	  pline;
	  out('You sense the presence of ');
	  CASE  what  OF
	    monsters : out('monsters.');
	    objects  : out('objects.');
	  END;
	  more;
	  flags.topl := False;
	  docrt;
	END;
    END  (* Reveal *);

PROCEDURE AddThisObjectToList(VAR What: Thing);

  VAR tmp, AddThis: Thing;

  BEGIN(* AddThisObjectToList *)
    create(AddThis, objects);
    WITH What^ DO
      BEGIN
	AddThis^.spe := spe;
	AddThis^.olet := olet;
	AddThis^.quan := quan;
	AddThis^.minus := minus;
	AddThis^.known := known;
	AddThis^.cursed := cursed;
	AddThis^.class := class;
	CASE class OF
	  anything: bug ('internal - 1');
	  food,
	  gems,
	  potions,
	  scrolls,
	  amulet :;
	  armors : AddThis^.armor := armor;
	  weapons : AddThis^.weapon := weapon;
	  rings : AddThis^.ring := ring;
	  wands : AddThis^.wand := wand;
	END; (* CASE *)
      END(* WITH *);
    AddThis^.next := What^.next;
    What^.next := AddThis;
  END(* AddThisObjectToList *);

  PROCEDURE Forget(What : ObjectClass);

    VAR
      p : PotionType;
      s : ScrollType;
      r : RingType;
      w : WandType;
      i,j,temp : unsigned;

    BEGIN(* Forget *)
      CASE What OF
	anything,
	food,
	weapons,
	armors,
	gems,
	amulet: (* nothing *);
	potions:
	  BEGIN
	    KnownPotions := [];
	    FOR p := RestoreStrength TO PotionOfFrobozz DO
	      potcal[p] := blankname;
	    FOR  i := 0  TO  PotNum - 2  DO
	      PotionShuffle[PotionType(i)] := i;
	    FOR i := 0 TO Potnum-2 DO
	      BEGIN
	        j := rnd0(PotNum);
		temp := PotionShuffle[PotionType(i)];
		PotionShuffle[PotionType(i)] := PotionShuffle[PotionType(j)];
		PotionShuffle[PotionType(j)] := temp;
	      END;
	  END;
	scrolls:
	  BEGIN
	    KnownScrolls := [];
	    FOR s := EnchantArmor TO ScrollOfFrobozz DO
	      scrcal[s] := blankname;
	    FOR  i := 0  TO  ScrNum - 2  DO
	      ScrollShuffle[ScrollType(i)] := i;
	    FOR i := 0 TO scrnum-2 DO
	      BEGIN
	        j := rnd0(ScrNum);
		temp := ScrollShuffle[ScrollType(i)];
		ScrollShuffle[ScrollType(i)] := ScrollShuffle[ScrollType(j)];
		ScrollShuffle[ScrollType(j)] := temp;
	      END;
	  END;
	rings:
	  BEGIN
	    KnownRings := [];
	    FOR r := adornment TO RingOfFrobozz DO 
	      ringcal[r] := blankname;
	    FOR  i := 0  TO  RingNum - 2  DO
	      RingShuffle[RingType(i)] := i;
	    FOR i := 0 TO ringnum-2 DO
	      BEGIN
	        j := rnd0(RingNum);
		temp := RingShuffle[RingType(i)];
		RingShuffle[RingType(i)] := RingShuffle[RingType(j)];
		RingShuffle[RingType(j)] := temp;
	      END;
	  END;
	wands:
	  BEGIN
	    KnownWands := [];
	    FOR w := WandOfLight TO WandOfFrobozz DO
	      wandcal[w] := blankname;
	    FOR  i := 0  TO  WandNum - 2  DO
	      WandShuffle[WandType(i)] := i;
	    FOR i := 0 TO WandNum-2 DO
	      BEGIN
	        j := rnd0(WandNum);
		temp := WandShuffle[WandType(i)];
		WandShuffle[WandType(i)] := WandShuffle[WandType(j)];
		WandShuffle[WandType(j)] := temp;
	      END;
	  END;
      END(* CASE *);
    END(* Forget *);

  BEGIN  (* DoDrink *)
    otmp := getObj([potions],'drink   ');
    IF  otmp = NIL  THEN
      BEGIN
	multi := 0;
	flags.move := False
      END
    ELSE
      BEGIN  (* Do the quaffing really. *)
	WITH  otmp^  DO
	  BEGIN
	    CASE  potion  OF

	      RestoreStrength	:
		BEGIN
		  pline;
		  out('Wow!  This makes you feel great!');
		  IF YouAreSwimming THEN
		    BEGIN
		      u.ustr := StrengthWhenSwimming;
		      IF YourActualStrength < StrengthWhenSwimming THEN
			YourActualStrength := StrengthWhenSwimming;
		      YouCanStillSwim := YouCanStillSwim + rn1(10,40);
		      flags.dstr := True;
		      YouCanCarry := YourCarryAbility(u.ustr)
		    END
		  ELSE
		    IF  u.ustr < u.ustrmax  THEN
		      BEGIN
			u.ustr:=u.ustrmax;
			flags.dstr:=True;
			YouCanCarry := YourCarryAbility(u.ustr);
		      END;
		END;
	      
	      Booze	:
		BEGIN
		  pline;
		  out('Ooph!  This tastes like liquid fire!');
		  LessHungry(80);
		  u.uslow := u.uslow + rn1(15,20); (* not feeling well *)
		  u.uconfused := u.uconfused + d(3,8);
		  IF  u.uhp < u.uhpmax  THEN
		    BEGIN
		      u.uhp := u.uhp + rn1(5,5);
		      flags.dhp := True
		    END;
		  IF  rn2(4)  THEN
		    BEGIN
		      pline;
		      out('You pass out.');
		      multi:=-rnd(15);
		      u.uslow := u.uslow + 20; (* feeling even worse *)
		    END;
		END;

	      Invisibility	:
		BEGIN
		  pline;
		  out('Gee!  All of a sudden, you can''t see yourself.');
		  NewSym(u.ux,u.uy);
		  u.uinvis := u.uinvis + rn1(20,61);
		END;

	      Juice	:
		BEGIN
		  pline;
		  out('This tastes like fruit juice.');
		  IF  rn2(5)  THEN
		    BEGIN
		      pline;
		      out('The juice was somewhat fermented...');
		      u.uconfused := u.uconfused + d(3,4);
		    END;
		  LessHungry(60);
		END;

	      Healing	:
		BEGIN
		  pline;
		  out('You begin to feel better.');
		  HealingEffect(rnd(10), 1);
		  IF u.uslow > 0 THEN
		    u.uslow := 1;
		END;

	      Blood     :
	        BEGIN
		  pline;
		  out('Yacc! This tastes awful, it seems to be blood');
		  LessHungry(100);
		END;

	      Paralysis	:
		BEGIN
		  pline;
		  out('Your feet are frozen to the floor!');
		  nomul(-rn1(10,25));
		END;

	      MonsterDetection	: Reveal(Monsters);

	      ObjectDetection	: Reveal(Objects);

	      Sickness	:
		BEGIN
		  pline;
		  out('Yech! This stuff tastes like poison.');
		  IF  u.upres  THEN
		    BEGIN
		      pline;
		      out('Luckily your stomach is strong enuff to take it');
		      LessHungry(40);
		    END
		  ELSE
		    BEGIN
		      LoseStr(rn1(4,3), PoisonPotion);
		      LoseHp(rnd(10),PoisonPotion);
		      u.uslow := u.uslow + rn1(10,20);
		    END;
		END;

	      Confusion	:
		BEGIN
		  pline;
		  out('Huh, What?  Where am I?');
		  u.uconfused := u.uconfused + rn1(6,16);
		END;

	      MagicSpirit :
	        BEGIN
		  pline;
		  out('A cloud of white mist erupts from the potion.');
		  pline;
		  out('You here a joyful voice saying:');
		  pline;
		  out('Give me one of Your things to play with');
		  UseUp(otmp);  (* otmp will be NIL, if you have only 1 *)
		  tmp := GetObj(AllObjects,'give her');
		  IF tmp = NIL THEN
		    GOTO 9;
		  SheLikesIt := NOT rn2(4);
		  pline;
		  IF tmp^.class <> amulet THEN 
		    IF SheLikesIt THEN
		      out('I like this thing. I''ll give You another one')
		    ELSE
		      BEGIN
		        out('I disgust these things. Better to destroy it');
			UnWear(tmp);
		      END
		  ELSE
		    out('Hey, this is really something. I''ll keep it');
		  CASE tmp^.class OF
		    anything: bug ('Internal - 2');
		    food,
		    gems,
		    potions,
		    scrolls : 
		      IF SheLikesIt THEN
			tmp^.quan := tmp^.quan + 1
		      ELSE 
			UseUp(tmp);
		    weapons:
		      IF tmp^.weapon <= wdart THEN
			tmp^.quan := tmp^.quan + 1
		      ELSE
			IF SheLikesIt THEN 
			  AddThisObjectToList(tmp)
		        ELSE
			  UseUp(tmp);

		    armors,
		    rings,
		    wands :
		      IF SheLikesIt THEN 
			AddThisObjectToList(tmp)
		      ELSE
			UseUp(tmp);

		    amulet:
		      IF YouHaveTheAmulet THEN
			Debugger
		      ELSE
			BEGIN
			  UseUp(tmp);
			  YouHaveTheAmulet := False;
			END;
		  END(* CASE *);
		END;

	      GainStrength	:
		BEGIN
		  pline;
		  out('Wow do you feel strong!');
		  IF  u.ustr <> 118  THEN
		    BEGIN
		      IF  u.ustr > 17  THEN
			BEGIN 
			  u.ustr := u.ustr + rnd(118-u.ustr);
			  YouCanCarry := YourCarryAbility(u.ustr)
			END 
		      ELSE
			BEGIN 
			  u.ustr := u.ustr + 1;
			  YouCanCarry := YourCarryAbility(u.ustr)
			END;
		      IF  u.ustr > u.ustrmax  THEN
			u.ustrmax := u.ustr;
		      flags.dstr := True;
		    END;
		END;

	      Speed	:
		BEGIN
		  pline;
		  out('You are suddenly moving faster.');
		  u.ufast := u.ufast + rn1(10,100);
		  u.uslow := 0;
		END;

	      Blindness	:
		BEGIN
		  pline;
		  out('A cloud of darkness falls upon you.');
		  u.ublind := u.ublind + rn1(100,250);
		  SeeOff(False);
		END;

	      GainLevel	:
		BEGIN
		  u.uexp := 10*pow(u.ulevel-1)+1;
		  flags.dexp := True;
		  IncrementUserLevel(False);
		END;

	      ExtraHealing :
		BEGIN
		  pline;
		  out('You feel much better.');
		  HealingEffect(d(2,20)+1, 2);
		  IF u.uslow > 0 THEN
		    u.uslow := 1;
		END;

	      Forgetting :
	        BEGIN
		  pline;
		  out('You fall asleep and dream a wonderful dream');
		  pline;
		  out('You are wandering in a cavern and');
		  pline;
		  out('You see magical things and monsters and');
		  pline;
		  out('huge amounts of gold, but');
		  pline;
		  out('You cannot remember it when You wake up');
		  CASE rnd(4) OF
		    1: Forget(potions);
		    2: Forget(scrolls);
		    3: Forget(rings);
		    4: Forget(wands);
		  END(* CASE *);
		END;

	      PotionOfFrobozz :
	        BEGIN
		  pline;
		  out('This is a fine way to get rich. I tried it once, too');
		  u.uMidas := True;
		  IF uleft <> NIL THEN
		    IF NOT uleft^.Magical THEN
		      BEGIN 
			ConvertToGold(uleft);
			RingEffect(uleft, False);
			Delete(uleft, invent);
			uleft := NIL;
		      END;
		  IF uright <> NIL THEN
		    IF NOT uright^.Magical THEN
		      BEGIN 
			ConvertToGold(uright);
			RingEffect(uright, False);
			Delete(uright, invent);
			uright := NIL;
		      END;
		  IF uwep <> NIL THEN
		    BEGIN 
		      ConvertToGold(uwep);
		      Delete(uwep, invent);
		      uwep := NIL;
		    END;
		  IF uarm <> NIL THEN
		    BEGIN
		      ConvertToGold(uarm);
		      ArmorOff;
		      Delete(uarm, invent);
		      Uarm := NIL;
		    END;
		END;
	    END  (* CASE  potion *);

	    IF  otmp <> NIL  THEN
	      BEGIN
		IF  NOT (potion IN KnownPotions)  THEN
		  IF  NOT (potion IN [ RestoreStrength, booze,
				       MagicSpirit, forgetting ])  THEN
		    IF potion < PotionOfFrobozz THEN 
		      BEGIN
			KnownPotions := KnownPotions + [ potion ];
			u.urexp := u.urexp + 100;
		      END
		    ELSE
		      BEGIN
			KnownPotions := KnownPotions + [ potion ];
			KnownFrobozz := KnownFrobozz + [ FrobozzPotion ];
			u.urexp := u.urexp + 5000;
		      END
		  ELSE
		    IF  potcal[potion] = blankname  THEN
		      DoCall(otmp);
		IF potion <> MagicSpirit THEN
		  UseUp(otmp);
	      END  (* IF  otmp <> NIL *);
	  END  (* WITH  otmp^ *);
      END  (* Do the quaffing really. *);
  9:					 (* For magic spirits *)
  END  (* DoDrink *);

PROCEDURE ChWepon(Color : string5);

  BEGIN
    pline;
    out('Your ');
    WriteArmament(uwep^.weapon);
    out(' glows ');
    out(color);
    out(' for a moment.');
  END;

PROCEDURE ReadGenocideScroll;

  CONST
    FirstMonster = Succ(NoMonster);
    LastMonster	 = humanoid;

  VAR
    monst,
    NextMonster : thing;
    ch		: Char;
    spec	: MonsterSpecies;	(* Ie. NoMonster..LastMonster. *)

  BEGIN
    pline;
    out('You have found a scroll of genocide!');
    REPEAT
      pline;
      out('Which monster do you want to genocide (Type the letter)? ');
      flags.topl := False;
      REPEAT
	tcRdCh(ch)
      UNTIL ch <> blank;
      spec := FirstMonster;
      WHILE  (mon[spec].mlet <> ch) AND (Spec < LastMonster)  DO
	spec := Succ(spec);
      IF  spec = LastMonster  THEN
	IF  mon[LastMonster].mlet <> ch  THEN
	  BEGIN
	    pline;
	    out('No such monster!');
	    spec := NoMonster;
	  END;
      IF  spec IN genocided  THEN
	BEGIN
	  pline;
	  out('You''ve genocided that one already!');
	  spec := NoMonster;
	END;
    UNTIL spec <> NoMonster;
    genocided := genocided + [spec];
    pline;
    out('No more ');
    IF spec = LurkerAbove THEN
      out('lurkers above')
    ELSE
      WriteEnemy(spec,NoArticle);
    IF spec IN [homunculus,wumpus,cyclops] THEN OutCh('e');
    OutCh('s');
    monst := f^.listof[monsters];
    WHILE  monst <> NIL  DO
      WITH  monst^  DO
	BEGIN
	  NextMonster := next;
	  IF  species = spec  THEN
	    GoodBye(monst);
	  monst := NextMonster;
	END;
  END  (* ReadGenocideScroll *);

PROCEDURE LitRoom;
  LABEL 9; (* For quick return *)

  VAR
    zx : xval;
    zy : yval;

  FUNCTION Show(x : xval; y : yval) : Boolean;

    BEGIN
      show := f^.levl[x,y].typ IN [RoomLocation,wall,door];
    END;

 BEGIN
  pline;
  IF f^.xdnstair = xmin THEN  (* We are in the MAZE *)
    out(nothing)
  ELSE
    BEGIN
      WITH f^.levl[u.ux,u.uy] DO
	BEGIN
	  IF typ = corr THEN
            out('The corridor lights up around you, then fades.')
	  ELSE
	    out('The room is lit.');
          IF lit THEN GOTO 9 (*return*);
	  IF typ = door THEN
	    BEGIN
	      IF f^.levl[u.ux,u.uy+1].typ = RoomLocation THEN
		zy := Succ(u.uy)
	      ELSE IF f^.levl[u.ux,u.uy-1].typ = RoomLocation THEN
		zy := Pred(u.uy)
	      ELSE
		zy := u.uy;
	      IF f^.levl[u.ux+1,u.uy].typ = RoomLocation THEN
		zx := Succ(u.ux)
	      ELSE IF f^.levl[u.ux-1,u.uy].typ = RoomLocation THEN
		zx := Pred(u.ux)
	      ELSE
		zx := u.ux;
	    END
	  ELSE (* typ <> door *)
	    BEGIN zx := u.ux; zy := u.uy END;
	END (*WITH*);
      seelx := u.ux; WHILE show(seelx-1,zy) DO seelx := seelx-1;
      seehx := u.ux; WHILE show(seehx+1,zy) DO seehx := seehx+1;
      seely := u.uy; WHILE show(zx,seely-1) DO seely := seely-1;
      seehy := u.uy; WHILE show(zx,seehy+1) DO seehy := seehy+1;
      FOR zy := seely TO seehy DO
      FOR zx := seelx TO seehx DO
	WITH f^.levl[zx,zy] DO
	  BEGIN
	    lit := True;
	    IF NOT CanSee THEN prl(zx,zy);
	  END;
    END;
  9:
 END;

PROCEDURE DarkenRoom;

  VAR
    zx : xval;
    zy : yval;

  FUNCTION Show(x : xval; y : yval) : Boolean;

    BEGIN
      show := f^.levl[x,y].typ IN [RoomLocation,wall,door];
    END;

 BEGIN
  pline;
  IF f^.xdnstair = xmin THEN  (* We are in the MAZE *)
    out(nothing)
  ELSE
    BEGIN
      WITH f^.levl[u.ux,u.uy] DO
	BEGIN
	  IF typ = corr THEN
            out('Everything seems dark and mysterious around You.')
	  ELSE
	    out('The room fades into darkness.');
	  IF typ = door THEN
	    BEGIN
	      IF f^.levl[u.ux,u.uy+1].typ = RoomLocation THEN
		zy := Succ(u.uy)
	      ELSE IF f^.levl[u.ux,u.uy-1].typ = RoomLocation THEN
		zy := Pred(u.uy)
	      ELSE
		zy := u.uy;
	      IF f^.levl[u.ux+1,u.uy].typ = RoomLocation THEN
		zx := Succ(u.ux)
	      ELSE IF f^.levl[u.ux-1,u.uy].typ = RoomLocation THEN
		zx := Pred(u.ux)
	      ELSE
		zx := u.ux;
	    END
	  ELSE (* typ <> door *)
	    BEGIN zx := u.ux; zy := u.uy END;
	END (*WITH*);
      seelx := u.ux; WHILE show(seelx-1,zy) DO seelx := seelx-1;
      seehx := u.ux; WHILE show(seehx+1,zy) DO seehx := seehx+1;
      seely := u.uy; WHILE show(zx,seely-1) DO seely := seely-1;
      seehy := u.uy; WHILE show(zx,seehy+1) DO seehy := seehy+1;
      FOR zy := seely TO seehy DO
      FOR zx := seelx TO seehx DO
	WITH f^.levl[zx,zy] DO
	  BEGIN
	    lit := False;
	    News1(zx, zy);
	  END;
    END;
  more;
  RedrawScreen;
 END;

PROCEDURE DetectGold;

  VAR
    gtmp:thing;

 BEGIN
   ClearScreen;
   gtmp := f^.listof[golds];
   REPEAT
     at(gtmp^.locx,gtmp^.locy,'$');
     gtmp := gtmp^.next;
   UNTIL gtmp = NIL;
   pru;
   flags.topl := False;
   pline;out('You feel very greedy, and sense gold!');
   more;
   flags.topl := False;
   docrt;
 END;
    
PROCEDURE DamagedByScrollOfFire;
  CONST MaxDamage = 6;
  VAR num:1..MaxDamage;
 BEGIN
   num := rnd(MaxDamage);
   LoseHp(num,ScrollOfFire);
   u.uhpmax := u.uhpmax - num;
   flags.dhpmax := True;
 END;

PROCEDURE DrawMap;
 VAR zy:yval; zx:xval;
 BEGIN
   FOR zy := ymin TO ymax DO
   FOR zx := xmin TO xmax DO
     WITH f^.levl[zx,zy] DO
       CASE typ OF
	 sdoor : BEGIN typ := door; atl(zx,zy,'+') END;
	 wall,door,corr : IF NOT seen THEN on(zx,zy);
	 empty,RoomLocation : ;
       END;
   IF NOT f^.levl[f^.xupstair,f^.yupstair].seen THEN
     on(f^.xupstair,f^.yupstair);
   IF NOT f^.levl[f^.xdnstair,f^.ydnstair].seen THEN
     on(f^.xdnstair,f^.ydnstair);
 END;

PROCEDURE AggravateThem;

  VAR 
    th : thing;

  BEGIN(* AggravateThem *)
    th := f^.listof[monsters];
    IF  th = NIL  THEN
      Nothin(th) 
    ELSE
      BEGIN
	pline;
	out('You hear a pitch humming noise around You');
	REPEAT
	  WITH  th^  DO
	    BEGIN
	      CASE mstat OF
		NormalState, sleep, GuardingTreasures:
		  BEGIN
		    mstat := NormalState;
		    IF Rn2(2) THEN
		    mspeed := mfast;
		  END;
		flee, mfroz:
		  mstat := NormalState;
		scared:
		  BEGIN
		    mstat := NormalState;
		    mspeed := mfast;
		  END;
		tamed:
		  mstat := NormalState;
	      END(* CASE *);
	      th := next;
	    END;
	UNTIL th = NIL;
      END;
  END(* AggravateThem *);

PROCEDURE doread;

  VAR
    otmp : thing;

PROCEDURE ScareThem;

  VAR 
    th : thing;
    DistanceBetweenYouAndIt: small;
    YouCanRecognizeItsEffect : Boolean;

  BEGIN(* ScareThem *)
    th := f^.listof[monsters];
    YouCanRecognizeItsEffect := False;
    IF  th = NIL  THEN
      Nothin(otmp)
    ELSE
      BEGIN
	REPEAT
	  WITH  th^  DO
	    BEGIN
	      DistanceBetweenYouAndIt := Trunc(Sqrt(Sqr(locx - u.ux) +
						    Sqr(locy - u.uy)));
	      IF f^.levl[locx,locy].CanSee THEN
		BEGIN 
		  mstat := mfroz;
		  YouCanRecognizeItsEffect := True;
		END 
	      ELSE
		IF DistanceBetweenYouAndIt <= 3 THEN
		  BEGIN 
		    mstat := mfroz;
		    YouCanRecognizeItsEffect := True;
		  END 
	        ELSE
		  IF DistanceBetweenYouAndIt <= 6 THEN
		    BEGIN 
		      mstat := scared;
		      YouCanRecognizeItsEffect := True;
		    END;
	      th := next;
	    END;
	UNTIL th = NIL;
	IF YouCanRecognizeItsEffect THEN
	  BEGIN
	    pline;
	    out('You hear shrill skrieks everywhere');
	  END
	ELSE
	  nothin(otmp);
      END;
  END(* ScareThem *);


 BEGIN
   otmp := GetObj([scrolls],'read    ');
   IF  otmp = NIL  THEN
     BEGIN
       multi := 0;
       flags.move := False
     END
   ELSE
     BEGIN
       pline;
       out('As you read the scroll, it disappears.');
       WITH otmp^ DO
	 BEGIN
	   CASE  scroll  OF
	     EnchantArmor :
	       IF  uarm = NIL  THEN
		 nothin(otmp)
	       ELSE
		 BEGIN
		   pline;
		   out('Your suit of ');
		   WriteArmor(uarm^.armor);
		   out(' glows green for a moment.');
		   PlusOne(uarm);
		   u.uac := u.uac - 1;
		   flags.dac := True;
		 END;
	     ConfuseMonster :
	       BEGIN
		 pline;out('Your hands begin to glow blue.');
		 u.umconf := True;
	       END;
	     AggravateMonster:
	       BEGIN
		 AggravateThem;
	       END;
	     blankScroll :
	       BEGIN
		 pline;out('This scroll seems to be blank.');
	       END;
	     RemoveCurse :
	       BEGIN
		 pline;
		 out('You feel like someone is helping you.');
		 IF uleft <> NIL THEN uleft^.cursed := False;
		 IF uright <> NIL THEN uright^.cursed := False;
		 IF uarm <> NIL THEN uarm^.cursed := False;
		 IF uwep <> NIL THEN uwep^.cursed := False;
	       END;
	     EnchantWeapon :
	       IF uwep = NIL THEN
		 nothin(otmp)
	       ELSE
		 BEGIN
		   ChWepon('green');
		   PlusOne(uwep)
		 END;
	     CreateMonster :
	       CreateSomeMonsterAndPutItBesideU(NoMonster);
	     DamageWeapon :
	       IF  uwep = NIL  THEN
		 nothin(otmp)
	       ELSE
		 BEGIN
		   ChWepon('black');
		   MinusOne(uwep)
		 END;
	     genocide :
	       IF genocided <> [Succ(NoMonster)..demon] THEN
		 ReadGenocideScroll
	       ELSE
		 nothin(otmp);
	     DestroyArmor :
	       IF  uarm = NIL  THEN
		 nothin(otmp)
	       ELSE
		 BEGIN
		   pline;
		   out('Your armor turns to dust and falls to the floor!');
		   ArmorOff;
		   UseUp(uarm);
		   uarm := NIL;
		 END;
	     ScrollOfLight :
	       LitRoom;
	     TeleportScroll :
	       teleport(True);
	     GoldDetection :
	       IF  f^.listof[golds] = NIL  THEN
		 nothin(otmp)
	       ELSE
		 DetectGold;
	     ScareMonster :
	       ScareThem;
	     identify :
	       BEGIN
		 pline;
		 out('This is an identify scroll.');
		 UseUp(otmp);
		 KnownScrolls := KnownScrolls + [identify];
		 otmp := GetObj(AllObjects,litIdentify);
		 IF  otmp <> NIL  THEN
		   BEGIN
		     WITH otmp^ DO
		       BEGIN
			 known := True;
			 CASE class OF
			   potions : 
			     BEGIN
			       KnownPotions := KnownPotions + [potion];
			       IF Magical THEN 
				 BEGIN 
				   KnownFrobozz := KnownFrobozz +
						   [ FrobozzPotion ];
				   u.urexp := u.urexp + 5000;
				 END;
			     END;				 
			   scrolls : 
			     BEGIN
			       KnownScrolls := KnownScrolls + [scroll];
			       IF Magical THEN 
				 BEGIN 
				   KnownFrobozz := KnownFrobozz +
						   [ FrobozzScroll ];
				   u.urexp := u.urexp + 5000;
				 END;
			     END;
			   wands : 
			     BEGIN
			       KnownWands := KnownWands + [wand];
			       IF Magical THEN 
				 BEGIN 
				   KnownFrobozz := KnownFrobozz +
						   [ FrobozzWand ];
				   u.urexp := u.urexp + 5000;
				 END;
			     END;
			   rings : 
			     BEGIN
			       KnownRings := KnownRings + [ring];
			       IF Magical THEN 
				 BEGIN 
				   KnownFrobozz := KnownFrobozz +
						   [ FrobozzRing ];
				   u.urexp := u.urexp + 5000;
				 END;
		             END;
			   weapons,armors,gems,food,amulet : ;
			 END (*CASE*);
		       END (*WITH*);
		     prinv(otmp);
		   END  (* IF  otmp <> NIL *);
		 otmp := NIL;
	       END;
	     MagicMapping :
	       BEGIN
		 pline;
		 out('There is a map on this scroll!');
		 DrawMap;
	       END;
	     FireScroll :
	       BEGIN
		 pline;
		 out('The scroll erupts in a tower of flame!');
		 IF  u.ufireres  THEN
		   BEGIN
		     pline;
		     out('You are uninjured.')
		   END
		 ELSE
		   DamagedByScrollOfFire;
	       END;
	     ScrollOfFrobozz :
	       IF KnownFrobozz <> [ FrobozzScroll, FrobozzPotion,
				    FrobozzRing, FrobozzWand ] THEN 
	         BEGIN
		   ClearScreen;
	NewLine;
	NewLine;
	Out('So you found this. I really did not believe you could come');
	NewLine;
	Out(' as far as this. BUT you are not a winner yet, and your');
	NewLine;
	Out(' life will be miserable from now on. ');
	NewLine;
	NewLine;
	Out(' However, My spirit will help You later, if You prove ');
	NewLine;
	Out(' that You are worth it. This means that You must find ');
	NewLine;
	Out(' all the MAGIC things that I, The Writer of this note');
	NewLine;
	Out(' once lost in this dungeon before I was trapped');
	NewLine;
	Out(' by the humanoids in the world of giants. ');
	NewLine;
	NewLine;
	Out('I give You a word of warning: DON''T try to use them,');
	NewLine;
	Out(' before You have them all and DON''T go too far without');
	NewLine;
	Out(' them. You will not survive alone...');
	NewLine;
	NewLine;
	Out('                      Friday, 13 November, 1313 ');
	NewLine;
	NewLine;
	Out('                              The Wizard Of Frobozz');
	NewLine;
		   IF GetRet(False) THEN (* Nothing *);
		   DoCrt;
		 END
	       ELSE
		 BEGIN (* The second scroll *)
		   ClearScreen;
        NewLine;
	Out('	          You DID IT!');
	NewLine;
	NewLine;
	Out('I must confess that I did not believe that anybody could');
	NewLine;
	Out(' do it. But I was wrong and You are really an ingeneous');
	NewLine;
	Out(' and courageous adventurer.');
	NewLine;
	NewLine;
	Out('You are quite near the amulet now, only the world of GIANTS');
	NewLine;
	Out(' is between You and it. But It won''t be easy.');
	NewLine;
	NewLine;
	Out('I will now tell You, how to use these things You''ve found:');
	NewLine;
	Out(' The level downstairs is full of horrible creatures, which');
	NewLine;
	Out(' You will find extremely dangerous and hard to resist.');
	NewLine;
	Out(' The wand will open the doors and staircases for You and');
	NewLine;
	Out(' the ring will protect You so far You have not touched');
	NewLine;
	Out(' the amulet. Nothing will protect You after that...');
	NewLine;
	Out(' The potion is Your last chance. An ancient king once');
	NewLine;
	Out(' had the same power, but for him it turned out to be');
	NewLine;
	Out(' a curse. I wish You better luck than I did.');
	NewLine;
	Out(' Don''t become too greedy and DON''T disturb the giants!');
	NewLine;
	NewLine;
	Out('	                      The wizard of Frobozz  ');
	NewLine;
		   IF GetRet(False) THEN (* Nothing *);
		   DoCrt;
		 END;
	   END  (* CASE  scroll *);
	   IF  otmp <> NIL  THEN
	     BEGIN
	       IF  NOT (scroll IN KnownScrolls)  THEN
		 IF  scroll > CreateMonster  THEN
		   IF scroll < ScrollOfFrobozz THEN 
		     BEGIN
		       KnownScrolls := KnownScrolls + [scroll];
		       u.urexp := u.urexp + 100;
		     END
	           ELSE
		     BEGIN
		       KnownScrolls := KnownScrolls + [scroll];
		       KnownFrobozz := KnownFrobozz + [ FrobozzScroll ];
		       u.urexp := u.urexp + 5000;
		     END		     
		 ELSE
		   IF  scrcal[scroll] = blankname  THEN
		     docall(otmp);
	       UseUp(otmp);
	     END;
	 END  (* WITH otmp^ *);
     END;
 END  (* DoRead *);

PROCEDURE dosearch;
  LABEL 9;
  VAR x:xval; y:yval; tgen:thing;

  PROCEDURE reveal(species:MonsterSpecies);
   BEGIN
     delete(tgen,f^.listof[traps]);
     IF MakeMon(species) THEN
       IF (x=u.ux) AND (y=u.uy) THEN
	 mnexto(f^.listof[monsters],u.ux,u.uy)
       ELSE
	 BEGIN
	   atl(x,y,mon[species].mlet);
	   WITH f^.listof[monsters]^ DO
	     BEGIN locx:=x; locy:=y END;
	 END;
  END;

 BEGIN
   FOR x := Pred(u.ux) TO Succ(u.ux) DO
   FOR y := Pred(u.uy) TO Succ(u.uy) DO
    WITH f^.levl[x,y] DO
      IF (typ = sdoor) AND (rn2(7) OR (u.usearch AND rn2(2))) THEN
      (* added by the Jukkas: we thought the search ring should help!*)
	BEGIN typ:=door;atl(x,y,'+');nomul(0) END
      ELSE
	BEGIN
	  tgen := f^.listof[traps];
	  WHILE tgen <> NIL DO
	    BEGIN
	      IF (tgen^.locx = x) AND (tgen^.locy = y) AND
		 ((u.usearch OR rn2(8))) AND NOT tgen^.seen THEN
		   BEGIN
		     nomul(0);
		     pline;out('You found a');
		     WriteTrap(tgen^.gflag);
		     IF tgen^.gflag = pierc THEN
		       BEGIN reveal(piercer); GOTO 9 END
		     ELSE IF tgen^.gflag = MimicTrap THEN
		       BEGIN reveal(Mimic); GOTO 9 END;
		     IF NOT tgen^.seen THEN
		       BEGIN tgen^.seen:=True; atl(x,y,'^') END;
		   END;
	      tgen := tgen^.next;
	    END;
	END;
 9:
 END;

PROCEDURE DoWield;

  VAR
    wep : thing;

  FUNCTION WieldingCursedWeapon : Boolean;

    BEGIN  (* WieldingCursedWeapon *)
      IF  uwep = NIL  THEN
	WieldingCursedWeapon := False
      ELSE WieldingCursedWeapon := uwep^.cursed
    END  (* WieldingCursedWeapon *);

  BEGIN  (* DoWield *)
   multi := 0;
   IF  WieldingCursedWeapon  THEN
     CursedWeaponInHand
   ELSE
     BEGIN  (* Can wield, current weapon non-cursed. *)
       wep := GetObj([weapons],'wield   ');
       IF  wep = NIL  THEN
	 flags.move := False
       ELSE
	 BEGIN
	   uwep := wep;
	   uwep^.known := True;
	   IF  uwep^.cursed  THEN
	     BEGIN
	       pline;
	       out(litThe);
	       WriteArmament(uwep^.weapon);
	       out(' welds itself to your hand!')
	     END  (* IF uwep^.cursed *)
	   ELSE  prinv(uwep);
	 END;
     END  (* Can wield, current weapon non-cursed. *);
  END  (* DoWield *);

PROCEDURE DoQuit;

  LABEL 9;

  VAR
    c : Char;

  BEGIN
    pline;
    out('Type YES if really quit ?');
    REPEAT  tcRdCh(c)  UNTIL  c <> blank;
    IF  (c <> 'y') AND (c <> 'Y')  THEN
      GOTO 9;
    tcRdCh(c);
    IF  (c <> 'e') AND (c <> 'E')  THEN
      GOTO 9;
    tcRdCh(c);
    IF  (c <> 's') AND (c <> 'S')  THEN
      GOTO 9;
    Done(quit);
  9:flags.move := False;
    multi := 0
  END;


FUNCTION  GetDir : Boolean;

  VAR
    ch : Char;

  BEGIN
    pline;out('What direction?');
    tcRdCh(ch);
    flags.topl := False;
    GetDir := movecm(ch);
  END  (* GetDir *);

PROCEDURE DoZap;

  LABEL 9;

  VAR
    x    : xval;
    y	 : yval;
    obj,
    mtmp : thing;
    OutZap : Boolean;
    tmp: byte;

  FUNCTION  FindAllSdoorsAndTraps : Small;

    VAR
      num    : Small;
      zx, lx,
      hx     : xval;
      zy, ly,
      hy     : yval;
      gtmp   : thing;

    PROCEDURE TurnToMonster(species : MonsterSpecies; TheList : ThingType);

      BEGIN
	IF  MakeMon(species)  THEN
	  BEGIN
	    WITH  f^.listof[monsters]^  DO
	      BEGIN
		locx := zx;
		locy := zy
	      END;
	    atl(zx, zy, mon[ species ].mlet);
	    num := Succ(num);
	    delete(gtmp, f^.listof [ TheList ])
	  END
      END  (* TurnToMonster *);

    BEGIN  (* FindAllSdoorsAndTraps *)
      lx := u.ux;
      WHILE  f^.levl[ Pred(lx),u.uy ].typ IN [ wall..door, RoomLocation ]  DO
	lx := Pred(lx);
      IF  (lx = u.ux) AND (lx > xmin)  THEN
	lx := Pred(lx);
      hx := u.ux;
      WHILE  f^.levl[ Succ(hx),u.uy ].typ IN [ wall..door, RoomLocation ]  DO
	hx := Succ(hx);
      IF  (hx = u.ux) AND (hx < xmax)  THEN
	hx := Succ(hx);
      ly := u.uy;
      WHILE  f^.levl[ u.ux,Pred(ly) ].typ IN [ wall..door, RoomLocation ]  DO
	ly := Pred(ly);
      IF  (ly = u.uy) AND (ly > ymin)  THEN
	ly := Pred(ly);
      hy := u.uy;
      WHILE  f^.levl[ u.ux,Succ(hy) ].typ IN [ wall..door, RoomLocation ]  DO
	hy := Succ(hy);
      IF  (hy = u.uy) AND (hy < ymax)  THEN
	hy := Succ(hy);

      num := 0;
      FOR zx := lx TO hx DO  FOR zy := ly TO hy DO
	IF  f^.levl[ zx, zy ].typ = sdoor  THEN
	  BEGIN
	    f^.levl[ zx, zy ].typ := door;
	    atl(zx,zy,'+');
	    num := num + 1;
	  END
	ELSE
	  BEGIN
	    gtmp := ThingAt(zx,zy,traps);
	    IF  gtmp <> NIL  THEN
	      CASE  gtmp^.gflag  OF
		beartrap,
		arrow,
		dart,
		tdoor,
		tele,
		pit,
		slptrp,
		StinkingTrap: IF  NOT gtmp^.seen  THEN
			      BEGIN
				gtmp^.seen := True;
				atl(zx,zy,'^');
				num := num+1
			      END;
		pierc     : TurnToMonster(piercer, traps);
		MimicTrap : TurnToMonster(Mimic, traps);
	      END  (* CASE  gtmp^.gflag *)
	    ELSE
	      BEGIN
		gtmp := ThingAt(zx,zy,golds);
		IF  gtmp <> NIL  THEN
		  IF  gtmp^.what = traps  THEN
		    TurnToMonster(Mimic,golds)
	      END;
	  END;
      FindAllSdoorsAndTraps := num;
    END  (* FindAllSdoorsAndTraps *);

  FUNCTION DestroyThingsAroundYou : unsigned;

    VAR
      zx,zy, startX, stopX, startY, stopY: Integer;
      tmp: thing;
      NumberOfThingsFound  : unsigned;

    BEGIN
      IF u.ux <= 5 THEN
	startX := 0
      ELSE
	startX := u.ux - 5;
      IF u.ux >= Xmax - 5 THEN
	stopX := Xmax
      ELSE
	stopX := u.ux + 5;
      IF u.uy <= 5 THEN
	startY := 0
      ELSE
	startY := u.uy - 5;
      IF u.uy >= Ymax - 5 THEN
	stopY := Ymax
      ELSE
	stopY := u.uy + 5;

      NumberOfThingsFound := 0;
      FOR zx := StartX TO StopX DO
	FOR zy := StartY TO StopY DO
	  BEGIN
	    tmp := ThingAt(zx,zy,Objects);
	    WHILE tmp <> NIL DO 
	      BEGIN 
		NumberOfThingsFound := NumberOfThingsFound + 1;
		Delete(tmp, f^.listof[objects]);
		tmp := ThingAt(zx,zy,Objects);
		IF  f^.levl[zx, zy].CanSee  THEN
		  NewSym(zx, zy)
		ELSE
		  news1(zx, zy);
	      END;
	    tmp := ThingAt(zx,zy,monsters);
	    IF tmp <> NIL THEN
	      IF tmp^.objectsCarried <> NIL THEN
		tmp^.objectsCarried := NIL;
	  END;
      DestroyThingsAroundYou := NumberOfThingsFound;
    END;
      

  PROCEDURE dig;

    VAR
      zx  : xval;
      zy  : yval;
      gtmp : Thing;

    BEGIN
      IF  (dx <> 0) AND (dy <> 0)  THEN
	IF  rn2(2) THEN
	  dx := 0
	ELSE
	  dy := 0;
      zx := u.ux + dx;
      zy := u.uy + dy;
      IF  f^.ThisIsAMaze THEN
	BEGIN
	  IF f^.levl[ zx,zy ].typ = RoomLocation THEN 
	    BEGIN 
	      gtmp := ThingAt(zx,zy,traps);
	      IF gtmp <> NIL THEN
		gtmp^.gflag := pit
	      ELSE 
		BEGIN
		  create(gtmp, traps);
		  WITH gtmp^ DO
		    BEGIN
		      gflag := pit;
		      seen := False;
		      SeenByMonsters := False;
		      locx := zx;
		      locy := zy;
		      AddToList(gtmp,f^.listof[traps]);
		      IF rn2(10) THEN seen := True;
		    END  (* WITH gtmp^ *);
		END
	    END
	  ELSE 
	    IF  (2 < zx) AND (zx < 76) AND (2 < zy) AND (zy < 18)  THEN
	      f^.levl[ zx,zy ].typ := RoomLocation;
	END  (* IF  f^.ThisIsAMaze *)
      ELSE
	BEGIN
	  WITH  f^.levl[ zx,zy ]  DO
	    IF (typ = RoomLocation) THEN
	      BEGIN 
		gtmp := ThingAt(zx,zy,traps);
		IF gtmp <> NIL THEN
		  gtmp^.gflag := pit
		ELSE 
		  BEGIN
		    create(gtmp, traps);
		    WITH gtmp^ DO
		      BEGIN
			gflag := pit;
			seen := False;
			SeenByMonsters := False;
			locx := zx;
			locy := zy;
			AddToList(gtmp,f^.listof[traps]);
			IF NOT seen THEN
			  IF rn2(10) THEN seen := True;
		      END  (* WITH gtmp^ *);
		  END
	      END (* IF typ = RoomLocation *)
	    ELSE
	      BEGIN
		IF  (typ <> RoomLocation) AND
		  (f^.levl[ u.ux,u.uy ].typ = RoomLocation)  THEN
		    BEGIN
		      typ := door;
		      NewSym(zx,zy);
		      zx := zx + dx;
		      zy := zy + dy
		    END;
		REPEAT WITH  f^.levl[ zx,zy ]  DO
		  IF  (typ IN [ empty, corr, RoomLocation ])  THEN
		    IF  (xmin < zx) AND (zx < xmax) AND
		      (ymin < zy) AND (zy < ymax)  THEN
			BEGIN
			  IF  typ = empty  THEN
			    BEGIN
			      typ := corr;
			      News1(zx,zy);
			      IF  (zx-dx = u.ux) AND (zy-dy = u.uy)  THEN
				on(zx,zy)
			    END;
			  zx := zx + dx;
			  zy := zy + dy
			END
		    ELSE
		      BEGIN
			zx := zx - dx;
			zy := zy - dy;
			f^.levl[ zx,zy ].typ := door
		      END
		  ELSE
		    IF  typ IN [ wall, sdoor ]  THEN
		      typ := door;
		UNTIL  f^.levl[ zx,zy ].typ = door;
	      END;
	END;
	NewSym(zx,zy);
    END  (* Dig *);
      
   PROCEDURE SendProjectile(proj:projectile);

     BEGIN
       buzz(proj, u.ux,u.uy,dx,dy);
       KnownWands := KnownWands + [obj^.wand];
     END;

   PROCEDURE AddToKnownWandsIfNotKnown;

     BEGIN
       IF  NOT (obj^.wand IN KnownWands)  THEN
	 IF obj^.wand < WandOfFrobozz THEN 
	   BEGIN
	     u.urexp := u.urexp + 100;
	     KnownWands := KnownWands + [ obj^.wand ]
	   END
         ELSE
	   BEGIN
	     KnownWands := KnownWands + [ obj^.wand ];
	     KnownFrobozz := KnownFrobozz + [ FrobozzWand ];
	     u.urexp := u.urexp + 5000;
	   END
     END;

  BEGIN (* DoZap *)
    OutZap := False;
    obj := GetObj([wands],'zap with');
    IF  obj = NIL  THEN
      BEGIN
	flags.move := False;
	multi := 0;
      END
    ELSE
      IF  obj^.spe = 0  THEN
	BEGIN
	  pline;
	  out('The wand vanishes!');
	  UseUp(obj);
	END
      ELSE
	IF  (obj^.wand <= DestroyObject) OR (obj^.wand = WandOfFrobozz)
	 THEN (* non-directional wand *) 
	  BEGIN
	    WITH  obj^  DO  spe := spe - 1;
	    CASE  obj^.wand  OF
	      WandOfLight : LitRoom;
	      WandOfDarkness : DarkenRoom;
	      detection   : IF  FindAllSdoorsAndTraps = 0  THEN
			      BEGIN
				OutZap := True;
				GOTO 9
			      END;
	      CreatorWand : CreateSomeMonsterAndPutItBesideU(NoMonster);
	      DestroyObject : IF DestroyThingsAroundYou = 0 THEN
				BEGIN
				  OutZap := True;
				  GOTO 9;
				END;
	      WandOfFrobozz :
	        BEGIN
		  pline;
		  out('You feel the precence the spirit of Frobozz');
		  IF (u.ux = f^.xupstair) AND (u.uy = f^.yupstair) THEN 
		    IF dlevel <= WorldOfGiants THEN 
		      OpenTheWayOut := 3 - OpenTheWayOut
		    ELSE 
		      OpenTheStairCaseUpToWorldOfGiants :=
		        3 - OpenTheStairCaseUpToWorldOfGiants
		  ELSE
		    IF (dlevel = WorldOfGiants) AND
		       (u.ux = f^.xdnstair) AND (u.uy = f^.ydnstair) THEN
		      OpenTheWayDownToAmulet := 3 - OpenTheWayDownToAmulet;
		END;
	    END;
	    AddToKnownWandsIfNotKnown;
	  9:
	  END
	ELSE
	  IF  GetDir  THEN
	    BEGIN
	      WITH  obj^  DO
		spe := spe - 1;
	      IF obj^.wand = TeleportMonster THEN
		BEGIN
		  IF u.uswallow THEN
		    BEGIN
		      teleport(False);
		      DoCrt;
		      WITH u.ustuck^ DO
			BEGIN
			  locx := u.ux;
			  locy := u.uy;
			END
		    END
		  ELSE
		    BEGIN
		      IF (u.ustuck <> NIL) THEN
			mtmp := u.ustuck
		      ELSE 
			mtmp := bhit(dx,dy,rn1(8,6),x,y);
		      IF mtmp <> NIL THEN
			rloc(mtmp);
		    END;
		  OutZap := True;
		  AddToKnownWandsIfNotKnown;
		END
	      ELSE IF  obj^.wand <= DrainLife  THEN
		BEGIN
		  IF u.uswallow THEN
		    mtmp := u.ustuck
		  ELSE 
		    mtmp := bhit(dx,dy,rn1(8,6),x,y);
		  IF  mtmp <> NIL  THEN
		    CASE  obj^.wand  OF
		      striking :
			BEGIN
			  IF  rnd(20) < 10 + mon[mtmp^.species].ac  THEN
			    BEGIN
			      hit(True,StrikingWand,mtmp);
			      WITH  mtmp^  DO
				BEGIN
				  ChangeMonstersHitPoints(mtmp, - d(2,12));
				  IF  mhp <= 0  THEN  killed(mtmp);
				END;
			    END
			  ELSE
			    hit(False,StrikingWand,mtmp);
			  AddToKnownWandsIfNotKnown
			END;
		      SlowMonster : mtmp^.mspeed := mslow;
		      SpeedMonster: mtmp^.mspeed := mfast;
		      UndeadTurning :
			IF mtmp^.species IN [Wraith,Vampire,Zombie,demon] THEN
			  BEGIN
			    ChangeMonstersHitPoints(mtmp, - d(2,10));
			    IF  mtmp^.mhp <= 0  THEN
			      BEGIN
				killed(mtmp);
				AddToKnownWandsIfNotKnown
			      END
			    ELSE 
			      BEGIN
				OutZap := True;
				mtmp^.mstat := scared;
				(* poor fellow is afraid rest of his life *)
				YouAreNoMoreStuckBy(mtmp)
			      END;
			  END
			ELSE
			  OutZap := True;
		      polymorph :
			BEGIN
			  YouAreNoMoreStuckBy(mtmp);
			  NewCham(mtmp,MonsterTable[rnd0(8),rnd0(mtabN+1)]);
			  AddToKnownWandsIfNotKnown
			END;
		      cancellation : mtmp^.mcan := True;
		      TameMonster  : mtmp^.mstat := tamed;
		      DrainLife    :
		        BEGIN
			  IF mtmp^.mhp < u.uhp THEN
			    BEGIN 
			      LoseHp(mtmp^.mhp,RayOfDrainLife);
			      ChangeMonstersHitPoints(mtmp, - mtmp^.mhp);
			      killed(mtmp);
			      AddToKnownWandsIfNotKnown;
			    END
			  ELSE 
			    BEGIN 
			      tmp := mtmp^.mhp;
			      IF mtmp^.mhp = u.uhp THEN
				BEGIN
				  killed(mtmp);
				  more;
				END;
			      LoseHp(tmp, RayOfDrainLife);
			    END
			END;
		    END  (* CASE  obj^.wand *);
		  IF  obj^.wand IN [ SlowMonster, SpeedMonster,
				     Cancellation, TameMonster ]  THEN
		    OutZap := True;
		END  (* IF  obj^.wand <= DrainLife *)
	      ELSE
		CASE  obj^.wand  OF
		  digging	: BEGIN
				    dig;
				    OutZap := True
				  END;
		  WandOfMissile : SendProjectile(missile);
		  FireWand	: SendProjectile(BoltOfFire);
		  SleepWand	: SendProjectile(SleepRay);
		  cold		: SendProjectile(BoltOfCold);
		  death		: SendProjectile(DeathRay);
		END;
	    END  (* IF  GetDir *)
	  ELSE
	    BEGIN
	      flags.move := False;
	      multi := 0
	    END;
    IF  OutZap  THEN
      BEGIN
	flags.topl := False;
	Pline;
	out('Zap!');
      END;
  END  (* DoZap *);

PROCEDURE CallThing;

  VAR
    obj : thing;

  BEGIN
    obj := GetObj([ Potions, Scrolls, Wands, Rings ], 'call    ');
    IF  obj <> NIL  THEN  DoCall(obj);
    flags.move := False;
  END  (* CallThing *);

PROCEDURE DoSet;

  VAR
    ch  : Char;
    ok,
    flg : Boolean (* True = set option on, False = off *);

  BEGIN
    pline;
    out('Which option do you want to set? ');
    ok := False;
    REPEAT
      REPEAT tcRdCh(ch) UNTIL ch <> blank;
      IF ch = '!' THEN
	BEGIN
	  flg := False;
	  tcWrCh(ch);
	  tcRdCh(ch)
	END
      ELSE flg := True;
      IF  ch IN [ 'o','u','t',Chr(ESC) ]  THEN
	ok := True
      ELSE
	BEGIN
	  flags.topl := False;	(* Don't let us say --More-- *)
	  pline;
	  out('?No such option!');
	  pline;		(* Here we say --More-- *)
	  out(' ''o'' = movement option, ''u'' = name,');
	  out(' ''t'' = terminal type: ')
	END
    UNTIL ok;
    IF  ch <> Chr(ESC)  THEN
      tcWrCh(ch)
    ELSE  flags.topl := False;
    pline;			(* Clear TopLine & maybe say --More-- *)
    CASE ch OF
      'o' : flags.one := flg;
      't' : BEGIN
	      ClearScreen; tcOutputFlush;
	      IF tcGetTerminalType < 0 THEN (* Nothing *);
	      ReDrawScreen
	    END;
      'u' : BEGIN
	      tcGetString('Identify yourself:',YourName);
	      CursorHome
	    END;
      Chr(ESC) : (* Nothing *)
    END (*CASE*);
    multi := 0;
    flags.move := False
  END;

PROCEDURE DoWhatis;

  VAR
    ch   : Char;
    spec : MonsterSpecies;

  BEGIN
    multi := 0;
    flags.move := False;
    tcRdCh(ch);
    pline;
    IF  ch IN [ 'A'..'Z', 'a'..'z', ',', '&', ':', '`',
		'"', '''', '(', '{', '}', ';', ']' ]  THEN
      BEGIN
	spec := NoMonster;
	REPEAT  spec := Succ(spec)  UNTIL  mon[spec].mlet = ch;
	WriteEnemy(spec,IndefiniteArticle);
      END
    ELSE
      IF  ch IN [ '@', '-', '|', '+', '.', '#', '~',
		  upwards, downwards, '^', '$', '%',
		  '!', '*', '?', '=', '/', '[', ')' ]  THEN
	CASE ch OF
	  '@' : out('you');
	  '-', '|' : out('wall');
	  '+' : out('a door');
	  '.' : out('floor of a room');
	  '#' : out('corridor');
	  '~' : out('water filled area');
	  upwards   : out('staircase to previous level');
	  downwards : out('staircase to next level');
	  '^' : out('a trap');
	  '$' : out('gold');
	  '%' : out('food');
	  '!' : out('a potion');
	  '*' : out('a gem');
	  '?' : out('a scroll');
	  '=' : out('a ring');
	  '/' : out('a wand');
	  '[' : out('a suit of armor');
	  ')' : out('a weapon');
	END;
  END  (* DoWhatis *);

FUNCTION  OutHelpOrNews(HelpWanted : Boolean) : Boolean;

  LABEL 99;

  CONST
    MaxLines = 22;

  VAR
    lines : ShortCount;
    ok	  : Boolean;

  BEGIN
    IF  HelpWanted  THEN
      ok := GetJfnForHelpFile('ZAP.HLP',help)
    ELSE
      ok := GetJfnForHelpFile('ZAP.NEWS',help);
    IF  ok  THEN
      BEGIN
	ClearScreen;
	Reset(help);
	lines := 1;
	WHILE  NOT Eof(help)  DO
	  BEGIN
	    tcWrCh(help^);
	    IF help^ = Chr(LF) THEN
	      BEGIN
		tcWrCh(Chr(13));
		lines := lines + 1;
		IF  lines MOD MaxLines = 0  THEN
		  IF  GetRet(True)  THEN
		    ClearScreen
		  ELSE  GOTO 99
	      END;
	    Get(help)
	  END;
	IF  GetRet(False)  THEN  (* Nothing *);
      99:Close(help)   (* Do this as GetJfnForHelpFile always gets a new JFN *)
      END  (* IF  ok *);
    OutHelpOrNews := ok
  END;

PROCEDURE RedrawScreen;

  BEGIN
    docrt;
    multi := 0; flags.move := False;
  END;

FUNCTION  NotTooConfusedToClimbStairCase : Boolean;

  VAR
    DidIt : Boolean;

  BEGIN
    IF  u.uconfused = 0  THEN
      NotTooConfusedToClimbStairCase := True
    ELSE
      BEGIN
	pline;
	out('Trying to climb the staircase...');
	pline;
	DidIt := rn2(u.uconfused+1);
	IF  DidIt  THEN
	  out('You did it!')
	ELSE
	  BEGIN
	    out('You were so confused that you just hurt yourself.');
	    u.uhp := u.uhp - rnd(u.uconfused);
	    flags.dhp := True;
	    IF  u.uhp <= 0  THEN  u.uhp := 1;
	  END;
	NotTooConfusedToClimbStairCase := DidIt;
      END;
  END  (* NotTooConfusedToClimbStairCase *);


PROCEDURE dDoUp;

LABEL 9;

VAR
  YouWereInAMaze : Boolean;

  BEGIN   (* dDoUp *)
    IF  (u.ux <> f^.xupstair) OR (u.uy <> f^.yupstair)
	OR (u.ustuck <> NIL)  THEN
      BEGIN
	pline;
	out('You can''t go up here');
	flags.move := False;
	multi := 0
      END
    ELSE
      IF  NotTooConfusedToClimbStairCase  THEN
	IF  (u.ustr < 8) OR (u.uhp < 10) OR (u.uhs >= weak)  THEN
	  BEGIN  (* hehehehe *)
	    pline;
	    out('You are too weak to climb up the staircase...');
	  END
	ELSE
	  IF  dlevel = 1  THEN
	    IF  YouHaveTheAmulet OR (KnownFrobozz <> []) THEN 
	      done(escaped) (* No escape without knowledge of magic stuff *)
	    ELSE
	      BEGIN
		pline;
		Out('You were sent to find IT, not to be a coward !!');
		u.ugold := u.ugold - u.ugold DIV 20;
		flags.dgold := True
	      END
	  ELSE
	    BEGIN
	      IF f^.ThisIsAMaze THEN
		IF (NOT YouHaveTheAmulet) OR (OpenTheWayOut = 1) THEN 
		  BEGIN
		    pline;
		    Out('The staircase is magically trapped !!');
		    GOTO 9
		  END;
	      IF (dlevel = LevelWhereTheAmuletIs) AND
		 (OpenTheStairCaseUpToWorldOfGiants = 1) THEN 
		   BEGIN
		     pline;
		     Out('The staircase is magically trapped !!');
		     GOTO 9
		   END;
	      YouWereInAMaze := f^.ThisIsAMaze;
	      SeeOff(True);
	      SaveLev;
	      IF (dlevel = WorldOfGiants) AND (OpenTheWayOut = 2) THEN
	        BEGIN
		  dlevel := 1;
		  SetLevel(dlevel);
		  GetLev;
		END
	      ELSE 
		BEGIN 
		  dlevel := dlevel - 1;
		  SetLevel(dlevel);
		  GetLev;
		END;
	      u.ux := f^.xdnstair;
	      u.uy := f^.ydnstair;
	      IF YouHaveTheAmulet OR
	         (f^.ListOf[Monsters] = NIL) OR
		 rn2(6)
	      THEN
		IF (dlevel < WorldOfGiants) OR 
		   ((dlevel = WorldOfGiants) AND (OpenTheWayOut = 1)) THEN 
		  GenerateMoreMonstersToThisLevel;
	      IF (YouWereInAMaze OR rn2(25)) AND (OpenTheWayOut = 1) THEN
	        TeleportOneThing(True);
	      SeeOn;
	      RedrawScreen;
	      IF (dlevel = WorldOfGiants) AND YouHaveTheAmulet THEN
		AggravateThem;
	    END;
  9:
   END  (* dDoUp *);
      
 PROCEDURE DoDown;

  BEGIN
    SaveLev;
    dlevel := dlevel + 1;
    IF  flags.next AND (dlevel >= DeepestLevelSaved)  THEN
      SetLevel(MaxInt)
    ELSE
      SetLevel(dlevel);
    IF  dlevel <= DeepestLevelSaved  THEN
      GetLev
    ELSE
      MkLev;
  END  (* DoDown *);

PROCEDURE dDoDown;

VAR
  YouWereInAMaze : Boolean;

 BEGIN
   IF  NotTooConfusedToClimbStairCase  THEN
     IF  (u.ux=f^.xdnstair) AND (u.uy=f^.ydnstair) AND (u.ustuck = NIL) THEN
       IF (dlevel < WorldOfGiants) OR 
	  (dlevel = WorldOfGiants) AND (OpenTheWayDownToAmulet = 2) THEN 
	 BEGIN
	   YouWereInAMaze := f^.ThisIsAMaze;
	   SeeOff(True);
	   DoDown;
	   IF YouHaveTheAmulet OR 
	      (f^.listOf[Monsters] = NIL) OR
	      (rn2(12) AND (dlevel <> deepestlevelsaved))
	   THEN
	     GenerateMoreMonstersToThisLevel;
	   IF YouWereInAMaze OR rn2(30) THEN
	     TelePortOneThing(True);
	   SeeOn;
	   RedrawScreen;
	 END
       ELSE
	 BEGIN
	   pline;
	   out('The staircase is magically trapped');
	   flags.move := False;
	   multi := 0
	 END;
 END  (* dDoDown *);
   
PROCEDURE DoConvert;

  VAR
    Which : Thing;

  BEGIN
    Which := GetObj([gems], 'convert ');
    IF  which = NIL  THEN
      BEGIN
	multi := 0;
	flags.move := false;
      END
    ELSE
      BEGIN
	UseUp(Which);
	IF rn2(6) THEN
	  BEGIN
	    pline; out('The gem explodes!');
	    LoseHp(rnd(30),ExplodingGem);
	  END
	ELSE IF  rn2(3)  THEN
	  CreateSomeMonsterAndPutItBesideU(NoMonster)
	ELSE
	  BEGIN
	    CASE  Rnd(5)  OF
	      1   : mkObj(food);
	      2   : mkObj(scrolls);
	      3,4 : mkObj(potions);
	      5   : MakeSomeObject;
	    END;
	    AddObjectToInvent(f^.listof[objects]);
	  END;	  
      END;
  END  (* DoConvert *);

PROCEDURE DoUnwield;

  BEGIN
    pline;
    IF  uwep = NIL  THEN
      out('You are already messing around with your bare hands.')
    ELSE
      IF  uwep^.cursed  THEN
        Out('You hurt your hand when trying to unwield a cursed weapon!')
      ELSE
	BEGIN
	  out('You will now fight with your bare hands...');
	  uwep := NIL;
	END;
  END  (* DoUnwield *);

PROCEDURE DoThrow;

  LABEL 9;

  VAR
    obj,
    monster : thing;
    x	    : xval;
    y	    : yval;
    tmp	    : byte;	(*reflects probability of hitting*)

  BEGIN  (* DoThrow *)
    (* Obviously the idea has been to allow throwing things
       other than weapons, but the following disallows it. *)
    obj := GetObj([weapons],'throw   ');
    IF obj = NIL THEN
      BEGIN
	flags.move := False;
	multi := 0;
	GOTO 9
      END  (* IF obj = NIL *);
    IF  (obj = uwep) AND obj^.Cursed  THEN
      BEGIN
	CursedWeaponInHand;
	GOTO 9
      END  (* IF  (obj = uwep) AND obj^.Cursed *);
    IF NOT GetDir THEN (*GetDir sets dx and dy acc. to direction*)
      BEGIN
	flags.move := False;
	multi := 0;
	GOTO 9
      END  (* IF NOT GetDir *);

    (* As we can only throw weapons, there's no need for the following check.
    IF  (obj = uarm) OR (obj = uleft) OR (obj = uright) THEN
      BEGIN
	pline; out('You can''t throw something you are wearing');
	GOTO 9
      END	*)

    IF obj = uwep THEN uwep := NIL;
    monster := bhit(dx,dy,8,x,y);
    LoseOne(obj);
    obj := f^.listof[objects];
    IF  monster <> NIL  THEN
      BEGIN
	tmp := -1 + u.ulevel + mon[monster^.species].ac + abon;
	IF  obj^.weapon IN [ warrow..crossbowbolt ]  THEN
	  IF  UnSuitable(obj^.weapon)  THEN
	    tmp := tmp - 3
	  ELSE
	    IF  uwep <> NIL  THEN
	      WITH  uwep^  DO  IF  cursed  THEN  tmp := tmp - spe
					   ELSE	 tmp := tmp + spe
	    ELSE
	      WITH  obj^  DO  IF  cursed  THEN  tmp := tmp - spe
					  ELSE	tmp := tmp + spe;
	WITH  monster^  DO  (* Wake Up, Wake Up! *)
	  IF  (mstat = sleep) OR (mstat=GuardingTreasures)  THEN
	    mstat := NormalState;
	IF  tmp >= rnd(20)  THEN
	  BEGIN
	    IF  hmon(monster,obj)  THEN
	      hit(True,obj^.weapon,monster);
	    (* Arrows and other small weapons usually disappear,
	       other weapons only occasionally: *)
	    IF  ((obj^.weapon IN [warrow..wdart]) AND NOT rn2(4))
		OR rn2(9)  THEN
		  delete(obj,f^.listof[objects]); (* sets obj to NIL *)
	  END
	ELSE
	  hit(False,obj^.weapon,monster);
      END  (* IF monster <> NIL *);

    IF obj <> NIL THEN
    BEGIN
    WITH obj^  DO
      BEGIN
	locx := x;
	locy := y
      END;
    WITH  f^.levl[ x,y ]  DO
      IF  ScrSym IN [ ' ', '.', '#', upwards, downwards,
		      '$', '=', '!', '/' ]  THEN
	BEGIN (* If throwing in water filled room, you can't see it anymore *)
	  ScrSym := obj^.olet;
	  Seen := False;
	  IF  CanSee  THEN
	    on(x,y);
	END;
   END;
  9:
  END  (* DoThrow *);

FUNCTION  Parse : Char;

  VAR
    cmd : Char;

  BEGIN
    flags.topl := False;
    flags.move := True;
    REPEAT
      tcRdCh(cmd);
    UNTIL cmd <> blank;
    ReadInteger(multi,cmd);
    WHILE  cmd = blank  DO  tcRdCh(cmd);
    IF  multi > 0  THEN
      BEGIN
	multi := multi - 1;
	SavedCommand := cmd
      END;
    Parse := cmd;
  END  (* Parse *);

PROCEDURE DisplayScores;

  LABEL  99;

  VAR
    size : Integer;

  BEGIN   (* DisplayScores *)
    ClearScreen;
    GetOwnScoreFile('ZAP.SCORE',YourTopTen);
    size := scorebinding.size;
    OpenWithSingleAccess(YourTopTen);
    IF  size >= 1  THEN
      BEGIN
	DisplayResults(YourTopTen,True);
	Close(YourTopTen);
	IF  NOT GetRet(True)  THEN  GOTO 99;
	NewLine;
	cury := 0
      END
    ELSE  Close(YourTopTen);
    IF  GetJfnForSystemScores('ZAP.SYSTEM-SCORE',SystemTopTen)  THEN
      BEGIN
	OpenWithSingleAccess(SystemTopTen);
	DisplayResults(SystemTopTen,False);
	Close(SystemTopTen);
	IF  GetRet(False)  THEN  (* Nothing *)
      END
    ELSE
      BEGIN
	Out('?System scores not found.');
	More
      END;
  99: RedrawScreen
  END  (* DisplayScores *);

PROCEDURE IdentifyTrap;

  VAR
    TrapToBeIdentified : thing;
    TellHimOrHer       : Boolean;

  BEGIN
    flags.move := False;
    multi := 0;
    IF  GetDir  THEN
      BEGIN
	TellHimOrHer := False;
	pline;
	TrapToBeIdentified := ThingAt(u.ux+dx,u.uy+dy,traps);
	IF  TrapToBeIdentified <> NIL  THEN
	  IF  TrapToBeIdentified^.seen  THEN
	    TellHimOrHer := True;
	IF  TellHimOrHer  THEN
	  BEGIN
	    OutCh('a');
	    WriteTrap(TrapToBeIdentified^.gflag)
	  END
	ELSE
	  out('My, it must be an illegal instruction trap.');
      END;
  END;

PROCEDURE GiveInformation;

  VAR
    ch : Char;
    ok : Boolean;
    p  : PotionType;
    s  : ScrollType;
    w  : WandType;
    r  : RingType;

  BEGIN  (* GiveInformation *)
    ClearScreen;
    pline; out('Info about what? ');
    ok := False;
    REPEAT
      REPEAT tcRdCh(ch) UNTIL ch <> blank;
      IF  ch IN [ '!', '?', '/', '=', Chr(ESC) ]  THEN
	ok := True
      ELSE
	BEGIN
	  pline;
	  out('Type ! for potions, ? for scrolls, / for wands, = for rings: ')
	END
    UNTIL ok;
    IF  ch <> Chr(ESC)  THEN
      BEGIN
	NewLine;
	CASE ch OF
	  '!' : FOR p:=PotionType(0) TO PotionType(Pred(PotNum)) DO
		  BEGIN PotionInfo(p,0);  END;
	  '?' : FOR s:=ScrollType(0) TO ScrollType(Pred(ScrNum)) DO
		  BEGIN ScrollInfo(s,0);  END;
	  '/' : FOR w:=WandType(0) TO WandType(Pred(WandNum)) DO
		  BEGIN WandInfo(w,0);  END;
	  '=' : FOR r:=RingType(0) TO RingType(Pred(RingNum)) DO
		  BEGIN RingInfo(r,0,False,False,0,NIL);  END;
	END (*CASE*);
	IF  cury = ymin+1  THEN
	  BEGIN
	    pline;
	    out('No information.');
	    More;
	  END  (* IF  cury = ymin+1 *)
	ELSE
	  BEGIN
	    NewLine;
	    IF  GetRet(False)  THEN  (* Nothing *)
	  END
      END  (* IF  ch <> Chr(ESC) *);
    RedrawScreen;
  END  (* GiveInformation *);

PROCEDURE Rhack(Cmd : Char);

  BEGIN  (* Rhack *)
    IF movecm(cmd) THEN
      BEGIN  (* Normal lowercase move command *)
	IF  multi <> 0  THEN
	  flags.mv := True;
	DoMove
      END  (* Normal lowercase move command *)
    ELSE
      IF  movecm(lowc(cmd))  THEN
	BEGIN  (* An uppercase move command *)
	  flags.mv := True;
	  flags.see := True;
	  multi := xmax+1;
	  DoMove
	END  (* An uppercase move command *)
    ELSE
      IF  cmd IN [ 'C', 'E', 'd', 'W', 'T', 'q', 'r', 's', 'w', 'i', 'I',
		   upwards, downwards, 'Q', 'p', 'z', 'c', 'o', '/', '?',
		   'e', 'P', 'R', 't', '!', '#', '^', 'S', 'O' ]
      THEN
	CASE cmd OF
	  'C' : doconvert;
	  'E' : DoUnwield;
	  'd' : dodrop;
	  'W', 'P' : dowear;
	  'T' : DoTakeOffArmor;
	  'R' : doremove;
	  'q' : DoDrink;
	  'r' : doread;
	  's' : dosearch;
	  'w' : DoWield;
	  'i' : BEGIN
		  IF  doinv(AllObjects)  THEN;  (* Nothing *)
		  flags.move := False; multi := 0
		END;
	  'I' : GiveInformation;
	  downwards : dDoDown;
	  upwards   : dDoUp;
	  'Q' : DoQuit;
	  'z', 'p' : DoZap;
	  'c' : CallThing;
	  'o' : DoSet;
	  '/' : DoWhatis;
	  '?' : BEGIN
		  IF  OutHelpOrNews(True)  THEN
		    ReDrawScreen
		  ELSE
		    BEGIN
		      pline;
		      out('?No help available')
		    END
		END;
	  'e' : doeat;
	  't' : DoThrow;
	  '!' : BEGIN
		  ClearScreen;
		  tcOutputFlush;  (* JP forgot this also *)
		  tcStop;
		  RedrawScreen
		END;
	  '#' : DisplayScores;
	  '^' : IdentifyTrap;
	  'S' : BEGIN
		  ClearScreen; tcOutputFlush;
		  multi := 0; flags.move := false;
		  SaveLev;
		  (* The current level  must be  saved in  the LEVELS  file
		     before doing the save  as files (and filebuffers)  are
		     not  saved  by  SaveHack.   It  also  makes  sure  the
		     internal file  is not  only  buffered anymore  but  is
		     connected with an  physical file (a  JFN in  Tops-20).
		     That physical file is closed and renamed to same  name
		     as user's savefile  but with  different type  (derived
		     from the type of the user's savefile).  *)
		  SaveHack(F, StartOfAreaToSave, EndOfAreaToSave);
		  (* SaveHack doesn't return except if save fails.  In that
		     case the LEVELS  file is still  open for  UpdateRandom
		     access and  we  must  restore  the  current  level  by
		     reading the current level back to the filebuffer. *)
		  NewLine;
		  SeekUpdate (F, dlevel);
		  IF GetRet(False)  THEN  (* Nothing *);
		  ReDrawScreen
		END;
	  'O' : BEGIN
		  RestoreTheGame;
		  ReDrawScreen
		END
	END  (* CASE cmd OF *)
      ELSE
	IF cmd = Chr(ctrlL) THEN
	   RedrawScreen
	ELSE
	  IF cmd = blank THEN
	    BEGIN
	      multi := 0;
	      flags.move := False
	    END
	  ELSE
	    IF cmd = Chr(ESC) THEN
	      BEGIN (* just clear top line *)
		pline;
		multi := 0;
		flags.move := False
	      END  (* just clear top line *)
	    ELSE
	      IF cmd IN [ Chr(LF), Chr(CR), Chr(18), '.' ] THEN
		(* nothing; *)
	      ELSE
		BEGIN
		  pline;
		  out('Unknown command ''');
		  (* Do not echo control characters as such; they may well
		     mess up screen control. Echo control-X as ^X in the
		   Digital style (as good as any other). *)
		  IF  (Cmd < ' ') AND (Cmd <> Chr(9) (*tab*))  THEN
		    BEGIN
		      OutCh('^');
		      Cmd := Chr(Ord(Cmd)+(Ord('A')-1))
		    END;
		  OutCh(cmd);
		  OutCh(quote);
		  multi := 0;
		  flags.move := False
	     END;
  END  (* Rhack *);

PROCEDURE FindUsersScoreFileAndAskUserNameIfNecessary;

  VAR
    SizeOfTheFile : Integer;

  BEGIN  (* FindUsersScoreFileAndAskUserNameIfNecessary *)
    GetOwnScoreFile('ZAP.SCORE', YourTopTen);
    SizeOfTheFile := scorebinding.size;
    OpenWithSingleAccess(YourTopTen);
    IF SizeOfTheFile >= 1  THEN
      BEGIN
	SeekUpdate (YourTopTen, 1);
	yourname := YourTopTen^[1].UserName;
	YourExperienceInZAP := YourTopTen^[1].ExperienceInGame;
      END
    ELSE
      BEGIN
	out('Nice to see new zappers.');
	NewLine;
	YourName := DefaultNameForU;
	tcGetString('What is your name?', YourName)
      END;
    Close(YourTopTen)
  END  (* FindUsersScoreFileAndAskUserNameIfNecessary *);

  PROCEDURE RandomizeYou;

    CONST
      HowMucho = 3;		(* HEHEHE *)

    VAR
      i : 1..HowMucho;

    BEGIN
      FOR i := 1 TO HowMucho DO
	CASE rnd(4) OF
	 1: u.ustrmax := Succ(u.ustrmax);
	 2: u.uhpmax  := u.uhpmax + 3;
	 3: PlusOne(uwep);
	 4: BEGIN PlusOne(uarm); u.uac := Pred(u.uac); END;
	END;
      FOR i := 1 TO HowMucho DO
	CASE rnd(4) OF
	 1: u.ustrmax := Pred(u.ustrmax);
	 2: u.uhpmax  := u.uhpmax - 3;
	 3: MinusOne(uwep);
	 4: BEGIN MinusOne(uarm); u.uac := Succ(u.uac); END;
	END;
      IF u.ustrmax < 15 THEN 
	u.ustrmax := 15; (* Otherwise nobody would probably start the game *)
	u.ustr := u.ustrmax;
	u.uhp := u.uhpmax;
    END (* RandomizeYou *);

BEGIN  (* main *)
  Reserved1 := -1; Reserved2 := -1; Reserved3 := -1; Reserved4 := -1;
  Reserved5 := -1; Reserved6 := -1; Reserved7 := -1; Reserved8 := -1;
  Reserved9 := -1; Reserved10 := -1; Reserved11 := -1;
  IF NOT tcInitTty(1) THEN
     bug ('Can''t initialize terminal');
  IF  OutHelpOrNews(False)  THEN  (* Nothing *);
  RanIni;
  OpenLevelsFile(F);
  ClearScreen;
  dlevel := 1;
  DeepestLevelSaved := 0;
  YouAreSwimming := False;
  YouCanStillSwim := 100;
  OpenTheWayDownToAmulet := 1;
  OpenTheStairCaseUpToWorldOfGiants := 1;
  OpenTheWayOut := 1;
  f^.moves := 0;
  IF  DoRestore  THEN
    BEGIN		(* User wants a saved game. *)
      invent := NIL;
      u.uswallow := False;
      multi := -MaxInt;
      RestoreTheGame;
      IF  Multi = -MaxInt  THEN
	GOTO 9999;	(* Restore failed! *)
      ReDrawScreen
    END
  ELSE
    BEGIN  (* Normal startup with no restoring of old game. *)
      DefineMonsters;
      DefineWeapons;
      AllObjects := [ weapons, potions, scrolls, wands, rings,
		      armors, gems, food, amulet ];
      KnownScrolls := [];
      KnownPotions := [];
      KnownWands := [];
      KnownRings := [];
      KnownFrobozz := [];
      YouHaveNotFoundThemAll := True;
      MakeCallableObjectsAnonymous;
      ShuffleCallableObjects;
      FreeList := NIL;
      genocided := [];
      multi := 0;
      uleft := NIL;
      uright := NIL;
      create(uwep, objects);
      WITH  uwep^  DO
	BEGIN
	  class := Weapons;
	  olet := ')';
	  weapon := mace;
	  next := NIL;
	  spe := 1;
	  quan := 1;
	  known := True;
	  cursed := False;
	  minus := False
	END  (* WITH  uwep^ *);
      create(uarm, objects);
      WITH  uarm^  DO
	BEGIN
	  class := Armors;
	  olet := '[';
	  armor := RingMail;
	  next := uwep;
	  spe := 1;
	  quan := 1;
	  known := True;
	  cursed := False;
	  minus := False
	END  (* WITH  uarm^ *);
      create(invent, objects);
      WITH  invent^  DO
	BEGIN
	  class := food;
	  olet := '%';
	  fruit := False;
	  quan := 2;
	  next := uarm;
	END  (* WITH  invent^ *);
      WITH  u  DO
	BEGIN
	  TurningToStone := 0;	(* COCKATRICE *)
	  YouHaveTheAmulet := False;
	  uac := 6;
	  ulevel := 1;
	  uhunger := 900;
	  uhpmax := 12;
	  ustrmax := 16;
	  RandomizeYou;  (* Side effect: initializes some u.* fields... *)
	  YourActualStrength := ustrmax;
	  YouCanCarry := YourCarryAbility(ustrmax);
	  uslow := 0;
	  ufast := 0;
	  uconfused := 0;
	  ublind := 0;
	  uinvis := 0;
	  ustuck := NIL;
	  udaminc := 0;
	  uinvis := 0;
	  utrap := 0;
	  umconf := False;
	  ufireres := False;
	  ucoldres := False;
	  utel := False;
	  upres := False;
	  ustelth := False;
	  uagmon := False;
	  ufeed := False;
	  usearch := False;
	  ucinvis := False;
	  uregen := False;
	  ufloat := False;
	  uswallow := False;
	  ucham := False;
	  umaintarm := False;
	  uMidas := False;
	  uhs := NotHungry;
	  ugold := 0;
	  uexp := 0;
	  urexp := 0;
	  ublind := 0
	END  (* WITH  u *);
      WITH flags DO
	BEGIN
	  topl := False;
	  botl := False;
	  faint := False;
	  screen := False;
	  mv := False;
	  see := False;
	  dgold := False;
	  dhp := False;
	  dhpmax := False;
	  dstr := False;
	  dac := False;
	  dulev := False;
	  dexp := False;
	  dhs := False;
	  dscr := False;
	  echo := False;
	  slpfg := True;
	  jump := True;
	  move := True;
	  one := False;
	  next := False;
	END  (* WITH flags *);
      YourExperienceInZAP := Novice;
      mklev;
      draw;
      FindUsersScoreFileAndAskUserNameIfNecessary;
      SetLevel(1);
    END  (* Normal startup with no restoring of old game. *);

  pline;
  out('Welcome back to Zap again, ');
  out0(yourname);

  WHILE  True  DO
    BEGIN
      IF flags.move THEN
	BEGIN
	  f^.moves := f^.moves + 1;
	  IF  (u.ufast=0) OR NOT Odd(f^.moves) THEN
	    BEGIN
	      MoveMonsters;
	      IF u.uslow > 0 THEN
		MoveMonsters; (* another time *)
	      IF  u.uhp > 0  THEN
		BEGIN
		  IF rn2(60-dlevel-9*Ord(YouHaveTheAmulet)) AND
		    (dlevel < WorldOfGiants) THEN
		    CreateSomeMonsterAtRandomLocation;

		  (* COCKATRICE *)
		  IF  (TurningToStone < 0) AND (multi = 0)  THEN
		    BEGIN
		      pline;
		      out('You get heavier!');
		      TurningToStone := TurningToStone - Rnd0(-TurningToStone);
		      nomul(TurningToStone);
		    END  (* IF  (TurningToStone < 0) AND (multi = 0) *);

		END  (* IF  u.uhp > 0 *);
	    END  (* IF (u.ufast=0) OR NOT Odd(f^.moves) *);
	  
	  IF u.uhp < 1 THEN
	    BEGIN
	      pline;
	      out('You die...');
	      done(died)
	    END  (* IF u.uhp < 1 *);

	  IF u.uslow > 0 THEN
	    BEGIN
	      u.uslow := u.uslow - 1;
	      IF u.uslow = 0 THEN
		BEGIN
		  pline;
		  out('You feel normal again')
		END
	    END  (* IF u.uslow > 0 *);

	  IF u.ufast > 0 THEN
	    BEGIN
	      u.ufast := u.ufast - 1;
	      IF u.ufast = 0 THEN
		BEGIN
		  pline;
		  out('You feel yourself slowing down')
		END
	    END  (* IF u.ufast > 0 *);

 	  IF (u.uslow = 0) OR Odd(f^.moves) THEN 
	    BEGIN 
	      IF f^.levl[u.ux,u.uy].room = PoisonGasRoom THEN
		IF NOT u.upres THEN 
		  LoseHp(1 + rnd(dlevel DIV 4), PoisonGas)
	        ELSE IF YouAreSwimming THEN
		  IF YouCanStillSwim = 0 THEN
		    LoseHp(u.uhp, Drowned)
	          ELSE
		    BEGIN
		      YouCanStillSwim := YouCanStillSwim - 1;
	      IF YouCanStillSwim = 20 THEN
		BEGIN
		  pline;
		  out('You can''t swim much longer...');
		END
	      ELSE
		IF YouCanStillSwim = 10 THEN
		  BEGIN
		    pline;
		    out('You are getting water in your lungs !');
		  END;
		    END;	    
	      IF u.uconfused > 0 THEN
		BEGIN
		  u.uconfused := u.uconfused - 1;
		  IF u.uconfused = 0 THEN
		    BEGIN
		      pline;
		      out('You feel less confused now')
		    END
		END  (* IF u.uconfused > 0 *);
	      IF u.ublind > 0 THEN
		BEGIN
		  u.ublind := u.ublind - 1;
		  IF u.ublind = 0 THEN
		    BEGIN
		      pline;out('You can see again');
		      SetSee
		    END
		END  (* IF u.ublind > 0 *);
	      IF u.uinvis > 0 THEN
		BEGIN
		  u.uinvis := u.uinvis - 1;
		  IF u.uinvis = 0 THEN
		    BEGIN
		      pru;
		      pline;out('You are no longer invisible')
		    END
		END  (* IF u.uinvis > 0 *);
	      IF  (u.ufast = 0) OR Odd(f^.moves)  THEN
		u.uhunger := u.uhunger - 1;
	      IF  (u.uregen OR u.ufeed) AND Odd(f^.moves)  THEN
		u.uhunger := u.uhunger - 1;
	      IF  u.uhp < u.uhpmax  THEN
		IF  u.ulevel > 9  THEN
		  BEGIN
		    IF  u.uregen OR (f^.moves MOD 3 = 0)  THEN
		      BEGIN
			flags.dhp := True;
			u.uhp := u.uhp + rnd(u.ulevel-9);
			IF  u.uhp > u.uhpmax  THEN
			  u.uhp := u.uhpmax;
		      END  (* IF u.uregen OR (f^.moves MOD 3 = 0) *)
		  END  (* IF u.ulevel > 9 *)
	        ELSE
		  IF  u.uregen OR (f^.moves MOD (22-u.ulevel*2) = 0)  THEN
		    BEGIN  (* At deeper levels regenerate faster. *)
		      flags.dhp := True;
		      u.uhp := u.uhp + 1;
		    END;
			
	      IF  u.utel  THEN
		IF  rn2(40)  THEN
		  teleport(True);
	      IF  u.usearch	 THEN
		DoSearch;

	      IF YouHaveNotFoundThemAll THEN 
		IF KnownFrobozz = [ FrobozzPotion, FrobozzScroll,
				    FrobozzRing, FrobozzWand ] THEN
	          BEGIN
		    u.urexp := u.urexp + 10000;
		    Mkobj(scrolls);
		    WITH f^.listof[objects]^ DO
		      BEGIN 
			locx := f^.xdnstair;
			locy := f^.ydnstair;
			scroll := ScrollOfFrobozz;
			Magical := True;
			quan := 1;
			f^.levl[locx, locy].ScrSym := '?';
		      END;
		    pline;
		    Out('You have a strange feeling for a moment,');
		    pline;
		    Out('as if somebody had dropped something');
		    YouHaveNotFoundThemAll := False;
		  END;
	      
	      (*Problem: u.uhunger may get decremented by two!!*)
	      IF  u.uhunger = HungryLimit  THEN
		BEGIN
		  pline;out('You are beginning to feel hungry');
		  u.uhs := hungry;
		  flags.dhs := True
		END
	      ELSE
		IF u.uhunger = WeakLimit THEN
		  BEGIN
		    pline;out('You are beginning to feel weak');
		    u.uhs := weak;
		    flags.dhs := True;
		    LoseStr(1, LackOfFood); (* LessHungry restores strength. *)
		  END
	        ELSE
		  IF  u.uhunger < 1  THEN
		    BEGIN
		      pline;
		      out('You faint from lack of food');
		      IF  u.uhs = fainting  THEN
			IF rn2(6) THEN
			  LoseHp(u.uhp, LackOfFood)
		        ELSE
			  Losehp(rnd(10), LackOfFood)
		      ELSE
			BEGIN
			  u.uhs := fainting;
			  flags.dhs := True
			END;
		      u.uhunger := rn1(10, 11);
		      nomul(-u.uhunger);
		      u.uhunger := rn1(u.uhunger-7,u.uhunger+4)
		    END  (* IF u.uhunger < 1 *);
	      END (* IF (u.uslow > 0) AND Odd(f^.moves) *);
	  END  (* IF flags.move *);
      flags.move := True;
      IF  flags.dscr AND NOT flags.mv  THEN  nscr;
      IF  flags.botl  THEN bot;
      IF  flags.dgold  THEN
	BEGIN
	  curs(16, BottomLine);
	  WriteNumber(u.ugold, -5);
	  flags.dgold := False;
	END  (* IF flags.dgold *);
      IF  flags.dhp  THEN
	UpdateHitPointInformation;
      IF  flags.dhpmax  THEN
	BEGIN
	  Curs(30, BottomLine);
	  WriteNumber(u.uhpmax, 1);
	  tcWrCh(')');
	  IF u.uhpmax < 100 THEN tcWrCh(blank);
	  IF u.uhpmax < 10 THEN
	    curx := 33
	  ELSE curx := 34;
	  flags.dhpmax := False
	END  (* IF flags.dhpmax *);
      IF flags.dac THEN
	BEGIN
	  curs(37, BottomLine);
	  WriteNumber(u.uac, -2);
	  flags.dac := False;
	END  (* IF flags.dac *);
      IF flags.dstr THEN
	BEGIN
	  curs(45, BottomLine);
	  prustr;
	  curx := 50;
	  flags.dstr := False;
	END  (* IF flags.dstr *);
      IF flags.dulev THEN
	BEGIN
	  curs(56, BottomLine);
	  WriteNumber(u.ulevel, 2);
	  curx := 58;
	  flags.dulev := False;
	END  (* IF flags.dulev *);
      IF flags.dexp THEN
	BEGIN
	  curs(59, BottomLine);
	  WriteNumber(u.uexp, -5);
	  flags.dexp := False;
	END  (* IF flags.dexp *);
      IF flags.dhs THEN
	BEGIN
	  curs(70, BottomLine);
	  WriteHungerState;
	  curx := 78;
	  flags.dhs := False;
	END  (* IF flags.dhs *);
      IF  multi < 0  THEN
	BEGIN
	  multi := multi + 1;
	  IF  multi = 0  THEN
	    BEGIN
	      (* pline; out('You can move again');   Heh heh *)
	      flags.move := False
	    END
	END  (* IF multi < 0 *)
      ELSE
	IF  multi > 0  THEN
	  IF  flags.mv  THEN
	    BEGIN
	      IF  multi < (xmax + 1)  THEN
		BEGIN
		  multi := multi - 1;
		  IF  multi = 0  THEN
		    BEGIN
		      flags.mv := False;
		      flags.see := False
		    END;
		END;
	      DoMove;
	    END  (* IF flags.mv *)
	  ELSE
	    BEGIN  (* Repeat previous command, which wasn't move command. *)
	      multi := multi - 1;
	      rhack(SavedCommand);
	    END  (* Repeat previous command, which wasn't move command. *)
      ELSE  rhack(Parse)	(* get a new command *)
    END  (* WHILE True *);
9999:
  NewLine;
  tcOutputFlush;
  IF tcInitTty (0) THEN
     (* nothing *);
END  (* Main *).
