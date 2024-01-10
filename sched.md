## Walkthrough of Linux 2.0.33's _schedule_ Function

### Code Walkthrough

#### schedule (linux/kernel/sched.c:296)

```txt
Control Flow:
schedule <-- Here

220: Disables interrupts.

221: Assigns itimer_ticks to the ticks stack variable.

222-223: Zeroes itimer_ticks and sets itimer_next to
         the unsigned long max value.

224: Enables interrupts.

227-256: Iterates through the process list and calls
         send_sig to send the SIGALRM signal for any
         processes whose timer is up.

         Consult time.c for the complete treatment
         of this code block.

272-277: Iterates through the process list to find the
         process with the greatest counter field and
         assigns this process to the next argument.

279-282: Adjusts the counter field of every process
         if there were no processes with p->count > 0.

282-284: Increments the context_swtch kernel statistic
         if the next process is NOT the current.

285: Calls switch_to complete the context switch.
```

#### switch\_to (linux/include/linux/sched.h:357)

```txt
Control Flow:
schedule
    switch_to <-- Here

358-359: Jumps to 1 label and exits the routine in the next
         process is equal to _current.

360: Disables interrupts.

362: Long jumps through the next task's task-state segment to
     switch contexts.
```
