/*    c_bind.h
 *
 *    Copyright (c) 1994-1996, Marko Macek
 *
 *    You may distribute under the terms of either the GNU General Public
 *    License or the Artistic License, as specified in the README file.
 *
 */

#ifndef __BIND_H
#define __BIND_H

#define ABBREV_HASH      16

class EMode;
class EEventMap;
class EKeyMap;
class EKey;
class EAbbrev;

typedef struct {
    TKeyCode Mask;
    TKeyCode Key;
} KeySel;

class EMode {
public:
    EMode *fNext;
    char *fName;
    char *MatchName;
    char *MatchLine;
    RxNode *MatchNameRx;
    RxNode *MatchLineRx;
    EBufferFlags Flags;
    EEventMap *fEventMap;
    EMode *fParent;
#ifdef CONFIG_SYNTAX_HILIT
    EColorize *fColorize;
#endif
    char filename[256];
    
    EMode(EMode *aMode, EEventMap *Map, char *aName);
    ~EMode() {}
    EAbbrev *FindAbbrev(char *string);
};

class EKeyMap {
public:
    EKeyMap *fParent;
    EKey *fKeys;
    
    EKeyMap();
    ~EKeyMap() {};
    
    void AddKey(EKey *aKey);
    EKey *FindKey(TKeyCode aKey);
};

class EEventMap {
public:
    EEventMap *Next;
    EEventMap *Parent;
    char *Name;
    
    EKeyMap *KeyMap;
    char *Menu[2]; // main + local
    
    EAbbrev *abbrev[ABBREV_HASH];
    
    EEventMap(char *AName, EEventMap *AParent);
    ~EEventMap();
    void SetMenu(int which, char *What);
    char *GetMenu(int which);
#ifdef CONFIG_ABBREV
    int AddAbbrev(EAbbrev *ab);
#endif
};

#define CT_COMMAND  0
#define CT_NUMBER   1
#define CT_STRING   2

typedef struct {
    int type;
    short repeat;
    short ign;
    union {
	int num;
	char *string;
    } u;
} CommandType;

typedef struct {
    char *Name;
    int Count;
    CommandType *cmds;
} ExMacro;

class EKey {
public:
    KeySel fKey;
    int Cmd;
    EKeyMap *fKeyMap;
    EKey *fNext;
    
    EKey(char *aKey);
    EKey(char *aKey, EKeyMap *aKeyMap);
    ~EKey() {}
};

#ifdef CONFIG_ABBREV
class EAbbrev {
public:
    EAbbrev *next;
    int Cmd;
    char *Match;
    char *Replace;
    
    EAbbrev(char *aMatch, char *aReplace);
    EAbbrev(char *aMatch, int aCmd);
    ~EAbbrev();
};
#endif

class ExState { // state of macro execution
public:
    int Macro;
    int Pos;
    
    int GetStrParam(char *str, int buflen);
    int GetIntParam(int *value);
};

extern EMode *Modes;
extern EEventMap *EventMaps;

extern int CMacros;
extern ExMacro *Macros;

int GetCharFromEvent(TEvent &E, char *Ch);

char *GetCommandName(int Command);
EMode *FindMode(char *Name);
EEventMap *FindEventMap(char *Name);
EEventMap *FindActiveMap(EMode *Mode);
EMode *GetModeForName(char *FileName);
int CmdNum(char *Cmd);
void ExecKey(EKey *Key);
EKey *SetKey(EEventMap *aMap, char *Key);
int ParseKey(char *Key, KeySel &ks);
int GetKeyName(char *Key, KeySel &ks);

int NewCommand(char *Name);
int RunCommand(int Command);
int AddCommand(int no, int cmd, int count, int ign);
int AddString(int no, char *Command);
int AddNumber(int no, int number);
int HashStr(char *str, int max);
void SetWordChars(char *w, char *s);

#endif
