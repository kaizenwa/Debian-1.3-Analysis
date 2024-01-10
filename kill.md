## Walkthrough of Linux 2.0.30's _kill_ System Call

### Code Walkthrough

#### sys\_kill (linux/kernel/exit.c:318)

```txt
Control Flow:
sys_kill <-- Here

282-283: Calls kill_pg and returns if the pid is 0.

284-294: Iterates through the process list and calls
         send_sig on every process except the current
         process.

295-296: Calls kill_pg, passing the positive value of
         pid as an argument.

298: Calls kill_proc to complete the kill command.
```

#### kill\_pg (linux/kernel/exit.c:218)

```txt
Control Flow:
sys_kill
    kill_pg <-- Here (!pid case)

226-233: Iterates through the process list to call
         send_sig on each process within the process
         group pgrp.
```

#### send\_sig (linux/kernel/exit.c:46)

```txt
Control Flow:
sys_kill
    send_sig <-- Here


```

#### kill\_proc (linux/kernel/exit.c:261)

```txt
Control Flow:
sys_kill
    send_sig
    kill_proc <-- Here

267-270: Iterates through to process list to find the
         pid process and calls send_sig.
```
