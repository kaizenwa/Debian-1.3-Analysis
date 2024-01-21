/*    c_bind.cpp
 *
 *    Copyright (c) 1994-1996, Marko Macek
 *
 *    You may distribute under the terms of either the GNU General Public
 *    License or the Artistic License, as specified in the README file.
 *
 */

#include "fte.h"

//////////////////////////////////////////////////////////////////////////////

EMode *Modes = 0;
EEventMap *EventMaps = 0;

int CMacros = 0;
ExMacro *Macros = 0;

//////////////////////////////////////////////////////////////////////////////

#include "c_cmdtab.h"

//////////////////////////////////////////////////////////////////////////////

char *GetCommandName(int Command) {
    if (Command & CMD_EXT) {
        Command &= ~CMD_EXT;
        if ((Command < 0) ||
            (Command >= CMacros))
            return "?INVALID?";
        if (Macros[Command].Name)
            return Macros[Command].Name;
        else
            return "?NONE?";
    }
    for (int i = 0; i < int(sizeof(Command_Table) / sizeof(Command_Table[0])); i++)
        if (Command_Table[i].CmdId == Command)
            return Command_Table[i].Name;
    return "?invalid?";
}

int CmdNum(char *Cmd) {
    int i;
    
    for (i = 0; 
	 i < int(sizeof(Command_Table) / sizeof(Command_Table[0]));
	 i++)
	if (strcmp(Cmd, Command_Table[i].Name) == 0)
	    return Command_Table[i].CmdId;
    for (i = 0; i < CMacros; i++)
        if (Macros[i].Name && (strcmp(Cmd, Macros[i].Name)) == 0)
            return i | CMD_EXT;
    return 0; // Nop
}

EMode *FindMode(char *Name) {
    EMode *m = Modes;
    
    //fprintf(stderr, "Searching mode %s\n", Name);
    while (m) {
        if (strcmp(Name, m->fName) == 0)
            return m;
        m = m->fNext;
    }
    return 0;
}

EEventMap *FindEventMap(char *Name) {
    EEventMap *m = EventMaps;
    
    //fprintf(stderr, "Searching map %s\n", Name);
    while (m) {
        if (strcmp(Name, m->Name) == 0)
            return m;
        m = m->Next;
    }
    return 0;
}

EEventMap *FindActiveMap(EMode *Mode) {
    while (Mode) {
        if (Mode->fEventMap)
            return Mode->fEventMap;
        Mode = Mode->fParent;
    }
    return 0;
}

EKey *SetKey(EEventMap *aMap, char *Key) {
    EKeyMap **map = &aMap->KeyMap, *pm, *parent = 0;
    EKey *k, *pk;
    char *p, *d;
    EEventMap *xm = aMap;
    
//    printf("Setting key %s\n", Key);
    
    // if mode has parent, get parent keymap
    while (xm && xm->Parent && (parent == 0)) {
        parent = xm->Parent->KeyMap;
//        printf("%s : %s : %d\n", xm->fName, xm->fParent->fName, parent);
        xm = xm->Parent;
    }
    
    d = p = Key;
    while (d) {
        
        // parse key combination
	p = d;
	d = strchr(p, '_');
	if (d) {
	    if (d[1] == 0 || d[1] == '_') 
		d++;
	    
	    if (*d == 0) 
		d = 0;
	    else {
		*d = 0;
		d++;
	    }
        }
        
        // if lastkey
        
	if (d == 0) {
	    k = new EKey(p);
            if (*map) {
                (*map)->AddKey(k);
            } else {
                *map = new EKeyMap();
	        (*map)->fParent = parent;
                (*map)->AddKey(k);
            }
            return k;
            
        } else {
            // if introductory key
            
            if (*map == 0) { // first key in mode, create map
//                printf("new map key = %s, parent %d\n", p, parent);
        	k = new EKey(p, 0);
                *map = new EKeyMap();
                (*map)->fParent = parent;
                (*map)->AddKey(k);
            } else {
                KeySel ks;
                
                ParseKey(p, ks);
                if ((k = (*map)->FindKey(ks.Key)) == 0) { // check if key exists
                    // add it if not
                    k = new EKey(p, 0);
                    (*map)->AddKey(k);
		}
            }
            map = &k->fKeyMap; // set current map to key's map
            
            // get parent keymap
	    pm = parent;
            parent = 0;
//            printf("Searching %s\n", p);
            while (pm) { // while exists
                KeySel ks;
                
                ParseKey(p, ks);
                if ((pk = pm->FindKey(ks.Key)) != 0) { // if key exists, find parent of it
                    parent = pk->fKeyMap;
//                    printf("Key found %d\n", parent);
                    break;
                }
                pm = pm->fParent; // otherwise find parent of current keymap
            }
	}
	Key = 0;
    }
    return 0;
}

void InitWordChars() {
    static int init = 0;
    if (init == 0) {
        for (int i = 0; i < 256; i++)
            if ((i >= '0' && i <= '9') ||
                (i >= 'A' && i <= 'Z') ||
                (i >= 'a' && i <= 'z') || (i == '_'))
                WSETBIT(DefaultBufferFlags.WordChars, i, 1);
        init = 1;
    }
}

void SetWordChars(char *w, char *s) {
    char *p;
    memset((void *)w, 0, 32);
    
    p = s;
    while (p && *p) {
        if (*p == '\\') {
            p++;
            if (*p == 0) return;
        } else if (p[1] == '-') {
            if (p[2] == 0) return ;
            for (int i = p[0]; i < p[2]; i++)
                WSETBIT(w, i, 1);
            p += 2;
        }
        WSETBIT(w, *p, 1);
        p++;
    }
}

EMode::EMode(EMode *aMode, EEventMap *Map, char *aName) {
    fNext = 0;
    fName = strdup(aName);
    fEventMap = Map;
    fParent = aMode;
    InitWordChars();
    if (aMode) {
#ifdef CONFIG_SYNTAX_HILIT
        fColorize = aMode->fColorize;
#endif
        Flags = aMode->Flags;
        MatchName = 0;
        MatchLine = 0;
        MatchNameRx = 0;
        MatchLineRx = 0;
        if (aMode->MatchName) {
            MatchName = strdup(aMode->MatchName);
            MatchNameRx = RxCompile(MatchName);
        }
        if (aMode->MatchLine) {
            MatchLine = strdup(aMode->MatchLine);
            MatchLineRx = RxCompile(MatchLine);
        }
    } else {
        MatchName = 0;
        MatchLine = 0;
        MatchNameRx = 0;
        MatchLineRx = 0;
#ifdef CONFIG_SYNTAX_HILIT
        fColorize = 0;
#endif
        Flags = DefaultBufferFlags;
    }
}

EKeyMap::EKeyMap() {
    fKeys = 0;
    fParent = 0;
}

void EKeyMap::AddKey(EKey *aKey) {
    aKey->fNext = fKeys;
    fKeys = aKey;
}


int MatchKey(TKeyCode aKey, KeySel aSel) {
    aKey &= ~kfAltXXX;
    aKey &= ~aSel.Mask;
    
    if (aKey & kfShift)
        if ((aKey & (~kfShift)) < 256)
            aKey &= ~kfShift;
        
    if (aSel.Mask & kfShift)
        if ((aKey & 0xFFFF) == aKey)
            aKey = toupper((char)aKey);
    if (aSel.Mask & kfCtrl)
        if ((int)(aKey & 0xFFFFF) < 32 && (int)(aKey & 0xFFFFF) >= 0)
            aKey += '@';
    if (aKey == aSel.Key)
        return 1;
    return 0;
}

EKey *EKeyMap::FindKey(TKeyCode aKey) {
    EKey *p = fKeys;

    while (p) {
        if (MatchKey(aKey, p->fKey)) return p;
        p = p->fNext;
    }
    return 0;
}

EEventMap::EEventMap(char *AName, EEventMap *AParent) {
    Name = strdup(AName);
    Parent = AParent;
    KeyMap = 0;
    Next = EventMaps;
    EventMaps = this;
    Menu[0] = Menu[1] = 0;
    memset(abbrev, 0, sizeof(abbrev));
}

EEventMap::~EEventMap() {
}

void EEventMap::SetMenu(int which, char *What) {
    if (which < 0 || which >= EM_MENUS)
        return;
    if (Menu[which] != 0)
        free(Menu[which]);
    Menu[which] = strdup(What);
}

char *EEventMap::GetMenu(int which) {
    if (which < 0 || which >= EM_MENUS)
        return 0;
    if (Menu[which] || Parent == 0)
        return Menu[which];
    else
        return Parent->GetMenu(which);
}

#ifdef CONFIG_ABBREV
int EEventMap::AddAbbrev(EAbbrev *ab) {
    int i = HashStr(ab->Match, ABBREV_HASH);
    ab->next = abbrev[i];
    abbrev[i] = ab;
    return 1;
}

EAbbrev *EMode::FindAbbrev(char *string) {
    EEventMap *Map = fEventMap;
    EAbbrev *ab;
    int i;
    
    if (string == 0)
        return 0;
    i = HashStr(string, ABBREV_HASH);
    while (Map) {
        ab = Map->abbrev[i];
        while (ab != 0) {
            if (ab->Match && (strcmp(string, ab->Match) == 0))
                return ab;
            ab = ab->next;
        }
        Map = Map->Parent;
    }
    return 0;
}
#endif

static struct {
    char *Name;
    TKeyCode Key;
} KeyList[] = {
{ "Esc", kbEsc },
{ "Tab", kbTab },
{ "Space", kbSpace },
{ "Enter", kbEnter },
{ "BackSp", kbBackSp },
{ "F1", kbF1 },
{ "F2", kbF2 },
{ "F3", kbF3 },
{ "F4", kbF4 },
{ "F5", kbF5 },
{ "F6", kbF6 },
{ "F7", kbF7 },
{ "F8", kbF8 },
{ "F9", kbF9 },
{ "F10", kbF10 },
{ "F11", kbF11 },
{ "F12", kbF12 },
{ "Left", kbLeft },
{ "Right", kbRight },
{ "Up", kbUp },
{ "Down", kbDown },
{ "Home", kbHome },
{ "End", kbEnd },
{ "PgUp", kbPgUp },
{ "PgDn", kbPgDn },
{ "Ins", kbIns },
{ "Del", kbDel },
{ "Center", kbCenter },
{ "Break", kbBreak },
{ "Pause", kbPause },
{ "PrtScr", kbPrtScr },
{ "SysReq", kbSysReq },
};

int ParseKey(char *Key, KeySel &ks) {
    char *p = Key;
    TKeyCode KeyFlags = 0;
    int i;
    
    ks.Mask = 0;
    ks.Key = 0;
    while ((*p) && (p[1] == '+') || (p[1] == '-')) {
        if (p[1] == '-') {
            switch (p[0]) {
            case 'A': ks.Mask |= kfAlt; break;
            case 'C': ks.Mask |= kfCtrl; break;
            case 'S': ks.Mask |= kfShift; break;
            case 'G': ks.Mask |= kfGray; break;
            case 'X': ks.Mask |= kfSpecial; break;
            }
        } else if (p[1] == '+') {
            switch (p[0]) {
            case 'A': KeyFlags |= kfAlt; break;
            case 'C': KeyFlags |= kfCtrl; break;
            case 'S': KeyFlags |= kfShift; break;
            case 'G': KeyFlags |= kfGray; break;
            case 'X': KeyFlags |= kfSpecial; break;
            }
        }
        p += 2;
    }
    for (i = 0; i < int(sizeof(KeyList)/sizeof(KeyList[0])); i++)
        if (strcmp(p, KeyList[i].Name) == 0) {
            ks.Key = KeyList[i].Key;
            break;
        }
    if (ks.Key == 0)
        ks.Key = *p;
    if ((KeyFlags & (kfCtrl | kfAlt | kfGray)) == kfCtrl)
        if (ks.Key < 256 && isascii(ks.Key) && toupper(ks.Key) >= '@' && toupper(ks.Key) < '@' + 32)
            ks.Key = toupper(ks.Key) - '@';
    ks.Key |= KeyFlags;
    return 0;
}

int GetKeyName(char *Key, KeySel &ks) {
    *Key = 0;
    
    if (ks.Key  & kfAlt)   strcat(Key, "A+");
    if (ks.Mask & kfAlt)   strcat(Key, "A-");
    if (ks.Key  & kfCtrl)  strcat(Key, "C+");
    if (ks.Mask & kfCtrl)  strcat(Key, "C-");
    if (ks.Key  & kfGray)  strcat(Key, "G+");
    if (ks.Mask & kfGray)  strcat(Key, "G-");
    if (ks.Key  & kfShift) strcat(Key, "S+");
    if (ks.Mask & kfShift) strcat(Key, "S-");
    
    if (keyCode(ks.Key) < 256) {
        char c[2];
        
        c[0] = (char)(ks.Key & 0xFF);
        c[1] = 0;
        
        if ((ks.Key & kfAll) == kfCtrl)
            if (c[0] < ' ')
                c[0] += '@';
        if (c[0] == 32)
            strcat(Key, "Space");
        else
            strcat(Key, c);
    } else {
        for (int i = 0; i < int(sizeof(KeyList)/sizeof(KeyList[0])); i++)
            if (KeyList[i].Key == keyCode(ks.Key)) {
                strcat(Key, KeyList[i].Name);
                break;
            }
    }
    return 0;
}

EKey::EKey(char *aKey) {
    fNext = 0;
    ParseKey(aKey, fKey);
    fKeyMap = 0;
    Cmd = -1;
}

EKey::EKey(char *aKey, EKeyMap *aKeyMap) {
    fNext = 0;
    Cmd = -1;
    ParseKey(aKey, fKey);
    fKeyMap = aKeyMap;
}

#ifdef CONFIG_ABBREV
EAbbrev::EAbbrev(char *aMatch, char *aReplace) {
    next = 0;
    Match = strdup(aMatch);
    Replace = strdup(aReplace);
    Cmd = -1;
}

EAbbrev::EAbbrev(char *aMatch, int aCmd) {
    next = 0;
    Replace = 0;
    Match = strdup(aMatch);
    Cmd = aCmd;
}

EAbbrev::~EAbbrev() {
    if (Match)
        free(Match);
    if (Replace)
        free(Replace);
}
#endif

int AddCommand(int no, int Command, int count, int ign) {
    if (count == 0) return 0;
    if (Command == 0) return 0;
    Macros[no].cmds = (CommandType *)realloc(Macros[no].cmds, sizeof(CommandType) * (Macros[no].Count + 1));
    Macros[no].cmds[Macros[no].Count].type = CT_COMMAND;
    Macros[no].cmds[Macros[no].Count].u.num = Command;
    Macros[no].cmds[Macros[no].Count].repeat = short(count);
    Macros[no].cmds[Macros[no].Count].ign = short(ign);
    Macros[no].Count++;
    return 1;
}

int AddString(int no, char *String) {
    Macros[no].cmds = (CommandType *)realloc(Macros[no].cmds, sizeof(CommandType) * (Macros[no].Count + 1));
    Macros[no].cmds[Macros[no].Count].type = CT_STRING;
    Macros[no].cmds[Macros[no].Count].u.string = strdup(String);
    Macros[no].cmds[Macros[no].Count].repeat = 0;
    Macros[no].cmds[Macros[no].Count].ign = 0;
    Macros[no].Count++;
    return 1;
}

int AddNumber(int no, int number) {
    Macros[no].cmds = (CommandType *)realloc(Macros[no].cmds, sizeof(CommandType) * (Macros[no].Count + 1));
    Macros[no].cmds[Macros[no].Count].type = CT_NUMBER;
    Macros[no].cmds[Macros[no].Count].u.num = number;
    Macros[no].cmds[Macros[no].Count].repeat = 0;
    Macros[no].cmds[Macros[no].Count].ign = 0;
    Macros[no].Count++;
    return 1;
}

int NewCommand(char *Name) {
    Macros = (ExMacro *) realloc(Macros, sizeof(ExMacro) * (1 + CMacros));
    Macros[CMacros].Count = 0;
    Macros[CMacros].cmds = 0;
    Macros[CMacros].Name = Name;
    CMacros++;
    return CMacros - 1;
}

int ExState::GetStrParam(char *str, int maxlen) {
    assert(maxlen > 0);
    if (Macro == -1 || Pos == -1) return 0;
    if (Pos >= Macros[Macro].Count) return 0;
    if (Macros[Macro].cmds[Pos].type == CT_STRING) {
        strncpy(str, Macros[Macro].cmds[Pos].u.string, maxlen);
        str[maxlen - 1] = 0;
        Pos++;
    } else
        return 0;
    return 1;
}

int ExState::GetIntParam(int *value) {
    if (Macro == -1 || Pos == -1) return 0;
    if (Pos >= Macros[Macro].Count) return 0;
    if (Macros[Macro].cmds[Pos].type == CT_NUMBER) {
        *value = Macros[Macro].cmds[Pos].u.num;
        Pos++;
    } else
        return 0;
    return 1;
}

int HashStr(char *p, int max) {
    unsigned int i = 1;
    
    while (p && *p) {
        i += i ^ (i << 3) ^ (unsigned int)(*p) ^ (i >> 3);
        p++;
    }
    return i % max;
}
