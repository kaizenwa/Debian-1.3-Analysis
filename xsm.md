## Walkthrough of Debian 1.3's _xsm_ Program

### Contents

1. Code Flow
2. Source Code Commentary

### Code Flow

```txt
```

### Source Code Commentary

#### main (xfree86-3.3/programs/xsm/xsm.c:87)

```txt
Control Flow:
main <-- Here

102-128: Handles command line arguments.

130-134: Calls XtVaAppInitialize.

136-137: Calls XtDisplay and XInternAtom to initialize
         global variable wmStateAtom.

138-139: Calls XtDisplay and XInternAtom to initialize
         global variable wmDeleteAtom.

141: Calls register_signals.

149: Calls InstallIOErrorHandler.

156-158: Calls SmsInitialize.

164-165: Calls IceListenForConnections.

171: Calls SetAuthentication.

177: Calls InitWatchProcs.

179-185: Calls XtAppAddInput for each entry in the
         local varible listenObjs array.

182: Calls IceGetListenConnectionNumber.

188: Calls IceComposeNetworkIdList.

210: Calls create_choose_session_popup.

211: Calls create_main_window.

212: Calls create_client_info_popup.

213: Calls create_save_popup.

214: Calls create_log_popup.

221: Calls ListInit to initialize RunningList.

224: Calls ListInit to initialize PendingList.

227: Calls ListInit to initialize RestartAnywayList.

230: Calls ListInit to initialize RestartImmedList.

233: Calls ListInit to initialize WatForSaveDoneList.

236: Calls ListInit to initialize FailedSaveList.

239: Calls ListInit to initialize WaitForInteractList.

242: Calls ListInit to initialize WaitForPhase2List.

254-255: Calls GetSessionNames.

257: Assigns zero to local variable
     found_command_line_name.

283: Calls StartSession.

288: Calls ChooseSession.

296: Calls XtAppMainLoop.
```

#### register\_signals ()

```txt
```
#### InstallIOErrorHandler ()

```txt
```
#### SmsInitialize ()

```txt
```
#### SmsInitialize ()

```txt
```
#### IceListenForConnections ()

```txt
```
#### SetAuthentication ()

```txt
```
#### InitWatchProcs ()

```txt
```
#### IceGetListenConnectionNumber ()

```txt
```
#### IceComposeNetworkIdListen ()

```txt
```
####create\_choose\_session\_popup ()

```txt
```
#### create\_main\_window ()

```txt
```
#### create\_client\_info\_popup ()

```txt
```
#### create\_save\_popup ()

```txt
```
#### create\_log\_popup ()

```txt
```
#### ListInit ()

```txt
```
#### GetSessionNames ()

```txt
```
#### StartSession ()

```txt
```
#### ChooseSession ()

```txt
```
