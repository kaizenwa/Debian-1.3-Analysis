/*    o_model.h
 *
 *    Copyright (c) 1994-1996, Marko Macek
 *
 *    You may distribute under the terms of either the GNU General Public
 *    License or the Artistic License, as specified in the README file.
 *
 */

#ifndef __MODEL_H__
#define __MODEL_H__

class EView;

class EViewPort {
public:
    EView *View;
    int ReCenter;

    EViewPort(EView *V);
    virtual ~EViewPort();

    virtual void HandleEvent(TEvent &Event);
    virtual void UpdateView();
    virtual void RepaintView();
    virtual void UpdateStatus();
    virtual void RepaintStatus();
    
    virtual void GetPos();
    virtual void StorePos();
    
    virtual void Resize(int Width, int Height);
};

class EModel {
public:
    EModel **Root;   // root ptr of this list
    EModel *Next;    // next model
    EModel *Prev;    // prev model
    EView *View;     // active view of model
    
    int ModelNo;
    
    EModel(EModel **ARoot);
    virtual ~EModel();

    void AddView(EView *V);
    void RemoveView(EView *V);
    void SelectView(EView *V);
    
    virtual EViewPort *CreateViewPort(EView *V);

    virtual int GetContext();
    virtual EEventMap *GetEventMap();
    virtual int BeginMacro();
    virtual int ExecCommand(int Command, ExState &State);
    virtual void HandleEvent(TEvent &Event);

    virtual void GetName(char *AName, int MaxLen);
    virtual void GetPath(char *APath, int MaxLen);
    virtual void GetInfo(char *AInfo, int MaxLen);
    virtual void GetTitle(char *ATitle, int MaxLen,
                          char *ASTitle, int SMaxLen);

    void UpdateTitle();

    void Msg(int level, char *s, ...);
    virtual int CanQuit();
    virtual int ConfQuit(GxView *V);
    
    virtual void NotifyPipe(int PipeId);
    
    virtual void NotifyDelete(EModel *Deleting);
    virtual void DeleteRelated();
};

class EView {
public:
    EView *Next;        // next view
    EView *Prev;        // prev view
    ExModelView *MView; // model view controller
    EModel *Model;       // model for this view
    EView *NextView;    // next view for model
    EViewPort *Port;
    char *CurMsg;

    EView(EModel *AModel);
    virtual ~EView();

    virtual void FocusChange(int GotFocus);
    virtual void Resize(int Width, int Height);
    
    void SetModel(EModel *AModel);
    void SelectModel(EModel *AModel);
    void SwitchToModel(EModel *AModel);
    
    void Activate(int GotFocus);
    
    virtual int GetContext();
    virtual EEventMap *GetEventMap();
    virtual int BeginMacro();
    virtual int ExecCommand(int Command, ExState &State);
    
    virtual void HandleEvent(TEvent &Event);
    virtual void UpdateView();
    virtual void RepaintView();
    virtual void UpdateStatus();
    virtual void RepaintStatus();
    
    void Msg(int level, char *s, ...);
    void SetMsg(char *Msg);

    int SwitchTo(ExState &State);
    int FilePrev();
    int FileNext();
    int FileLast();
    int FileSaveAll();
    int FileOpen(ExState &State);
    int FileOpenInMode(ExState &State);
    int SetPrintDevice(ExState &State);
    int ToggleSysClipboard(ExState &State);
    int ShowKey(ExState &State);
    int ViewBuffers(ExState &State);
#ifdef CONFIG_OBJ_ROUTINE
    int ViewRoutines(ExState &State);
#endif
#ifdef CONFIG_OBJ_MESSAGES
    int Compile(ExState &State);
    int ViewMessages(ExState &State);
    int CompilePrevError(ExState &State);
    int CompileNextError(ExState &State);
#endif
#ifdef CONFIG_OBJ_DIRECTORY
    int DirOpen(ExState &State);
    int OpenDir(char *Directory);
#endif
    int ShowVersion();
    int ViewModeMap(ExState &State);
    int ClearMessages();
#ifdef CONFIG_TAGS
    int TagLoad(ExState &State);
#endif
    
    void DeleteModel(EModel *M);
    int CanQuit();
};

extern EModel *ActiveModel;
extern EView *ActiveView;

#define MSGBUFTMP_SIZE 1024
extern char msgbuftmp[MSGBUFTMP_SIZE];

#endif
