Boolean DlgGetCheck (DialogPtr theDlg, short item);
void DlgSetCheck (DialogPtr theDlg, short item, Boolean value);
void DlgToggleCheck (DialogPtr theDlg, short item);
short DlgGetNumber (DialogPtr theDlg, short item);
void DlgSetNumber (DialogPtr theDlg, short item, short value);
void DlgGetText (DialogPtr theDlg, short item, char *value);
void DlgSetText (DialogPtr theDlg, short item, char *value);
DialogPtr MyGetNewDialog (short id);
void MySFGetFile (ConstStr255Param prompt, FileFilterProcPtr fileFilter,
	short numTypes, SFTypeList typeList, DlgHookProcPtr dlgHook, 
	SFReply *reply);
void MySFPutFile (ConstStr255Param prompt, ConstStr255Param origName,
	DlgHookProcPtr dlgHook, SFReply *reply);
void MyModalDialog (ModalFilterProcPtr filterProc, short *itemHit,
	Boolean hasCancelButton, Boolean returnIsOK);
void ErrorMessage (char *msg);
void UnexpectedErrorMessage (OSErr err);
void ServerErrorMessage (Ptr data, unsigned short length);
pascal Boolean DialogFilter (DialogPtr theDialog,
	EventRecord *theEvent, short *itemHit);
short BlessedFolder (void);
Boolean GiveTime (void);
Boolean StatusWindow (char *text);
void UpdateStatus (void);
void CloseStatusWindow (void);
OSErr MyIOCheck (OSErr err);
Ptr MyNewPtr (Size byteCount);
Handle MyNewHandle (Size byteCount);
void MySetHandleSize (Handle h, Size newSize);
OSErr MyHandToHand (Handle *theHndl);
OSErr MyDisposPtr (Ptr thePtr);
OSErr MyDisposHandle (Handle theHndl);
OSErr MyMemErr (void);
Boolean IsAppWindow (WindowPtr wind);
Boolean IsDAWindow (WindowPtr wind);
Boolean IsMovableModal (WindowPtr wind);

pascal void InitCursorCtl (Handle id);
pascal void SpinCursor (short num);
short strcasecmp (char *s1, char *s2);
short strncasecmp (char *s1, char *s2, short n);
