## Walkthrough of Debian 1.3's _twm_ Program

### Contents

1. Code Flow
2. Source Code Commentary

### Code Flow

```txt
```

### Source Code Commentary

#### main (xfree86-3.3/programs/twm/twm.c:169)

```txt
Control Flow:
main <-- Here

191-226: Handles command line arguments.

231-234: Calls newhandler for SIGINT, SIGHUP,
         SIGQUIT, and SIGTERM.

247: Calls XtToolkitInitialize.

248: Calls XtCreateApplicationContext.

250: Calls XtOpenDisplay.

276: Calls ConnectionNumber and fcntl.

...

486: Calls InitVariables.

487: Calls InitMenus.

490: Calls ParseTwmrc.

491: Calls assign_var_savecolor.

494: Calls CreateGCs.

495: Calls MakeMenus.

502: Calls InitTitlebarButtons.

504: Calls XGrabServer.

505: Calls XSync.

510: Calls XQueryTree.

511: Calls CreateIconManagers.

...

601: Calls ConnectToSessionManager.

605: Calls InitEvents.

606: Calls HandleEvents.
```
