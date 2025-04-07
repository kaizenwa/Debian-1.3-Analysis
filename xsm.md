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

#### register\_signals (xfree86-3.3/programs/xsm/signals.c:181)

```txt
Control Flow:
main
    register_signals <-- Here
```

#### InstallIOErrorHandler (xfree86-3.3/programs/xsm/xsm.c:1375)

```txt
Control Flow:
main
    register_signals
    InstallIOErrorHandler <-- Here
```

#### SmsInitialize (xfree86-3.3/lib/SM/sm\_manager.c:41)

```txt
Control Flow:
main
    register_signals
    InstallIOErrorHandler
    SmsInitialize <-- Here
```

#### IceListenForConnections (xfree86-3.3/lib/ICE/listen.c:38)

```txt
Control Flow:
main
    register_signals
    InstallIOErrorHandler
    SmsInitialize
    IceListenForConnections <-- Here
```

#### SetAuthentication (xfree96-3.3/programs/xsm/auth.c:146)

```txt
Control Flow:
main
    ...
    InstallIOErrorHandler
    SmsInitialize
    IceListenForConnections
    SetAuthentication <-- Here
```

#### InitWatchProcs (xfree86-3.3/programs/proxymngr/main.c:981)

```txt
Control Flow:
main
    ...
    SmsInitialize
    IceListenForConnections
    SetAuthentication
    InitWatchProcs <-- Here
```

#### IceGetListenConnectionNumber (xfree86-3.3/lib/ICE/listen.c:160)

```txt
Control Flow:
main
    ...
    IceListenForConnections
    SetAuthentication
    InitWatchProcs
    IceGetListenConnectionNumber <-- Here

165: return (_IceTransGetConnectionNumber (listenObj->trans_conn));
```

#### \_IceTransGetConnectionNumber ()

```txt
```

#### IceComposeNetworkIdListen (xfree86-3.3/lib/ICE/listen.c:189)

```txt
Control Flow:
main
    ...
    SetAuthentication
    InitWatchProcs
    IceGetListenConnectionNumber
    IceComposeNetworkIdListen <-- Here
```

####create\_choose\_session\_popup (xfree86-3.3/programs/xsm/choose.c:740)

```txt
Control Flow:
main
    ...
    InitWatchProcs
    IceGetListenConnectionNumber
    IceComposeNetworkIdListen
    create_choose_session_popup <-- Here
```

#### create\_main\_window (xfree86-3.3/programs/xsm/mainwin.c:66)

```txt
Control Flow:
main
    ...
    IceGetListenConnectionNumber
    IceComposeNetworkIdListen
    create_choose_session_popup
    create_main_window <-- Here
```

#### create\_client\_info\_popup (xfree86-3.3/programs/xsm/info.c:826)

```txt
Control Flow:
main
    ...
    IceComposeNetworkIdListen
    create_choose_session_popup
    create_main_window
    create_client_info_popup <-- Here
```

#### create\_save\_popup (xfree86-3.3/programs/xsm/save.c:942)

```txt
Control Flow:
main
    ...
    create_choose_session_popup
    create_main_window
    create_client_info_popup
    create_save_popup <-- Here
```

#### create\_log\_popup (xfree86-3.3/programs/xsm/log.c:122)

```txt
Control Flow:
main
    ...
    create_main_window
    create_client_info_popup
    create_save_popup
    create_log_popup <-- Here
```

#### ListInit (xfree86-3.3/programs/xsm/list.c:31)

```txt
Control Flow:
main
    ...
    create_client_info_popup
    create_save_popup
    create_log_popup
    ListInit <-- Here
```

#### GetSessionNames (xfree86-3.3/programs/xsm/choose.c:75)

```txt
Control Flow:
main
    ...
    create_save_popup
    create_log_popup
    ListInit
    GetSessionNames <-- Here
```

#### StartSession (xfree86-3.3/programs/xsm/xsm.c:453)

```txt
Control Flow:
main
    ...
    create_log_popup
    ListInit
    GetSessionNames
    StartSession <-- Here
```

#### ChooseSession (xfree86-3.3/programs/xsm/choose.c:288)

```txt
Control Flow:
main
    ...
    ListInit
    GetSessionNames
    ChooseSession <-- Here

299: Calls AddSessionNames.

...
```

#### AddSessionNames (xfree86-3.3/programs/xsm/choose.c:234)

```txt
Control Flow:
main
    ...
    ListInit
    GetSessionNames
    ChooseSession
        AddSessionNames <-- Here
```
