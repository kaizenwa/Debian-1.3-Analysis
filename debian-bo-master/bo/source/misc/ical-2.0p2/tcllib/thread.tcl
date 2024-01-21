# Simple cancelable threads in tk.

set _thread(id) 0

# effects  Start a thread that executes "cmd" after "delay" milliseconds.
#	   Returns the thread id.
proc thread_once {delay cmd} {
    set id [_thread_alloc]
    after $delay [list _thread_run $id $cmd]
    return $id
}

# effects  Start a thread that executes "cmd" once every "delay"
#	   milliseconds.  If the execution of "cmd" takes a long
#	   time, then the thread will execute less frequently than
#	   specified by the "delay" parameter because the next
#	   iteration of the thread will be scheduled "delay"
#	   milliseconds after the current iteration finishes.
#
#	   The first iteration of the thread will be scheduled "delay"
#	   milliseconds from now.
proc thread_periodic {delay cmd} {
    set id [_thread_alloc]
    after $delay [list _thread_iterate $id $delay $cmd]
    return $id
}

# effects  Kill the specified thread.
#	   Returns true iff thread had not finished running on its own.
proc thread_cancel {id} {
    global _thread
    if [info exists _thread(run:$id)] {
	unset _thread(run:$id)
	return 1
    }
    return 0
}

# effects  Allocate a thread id
proc _thread_alloc {} {
    global _thread
    incr _thread(id)
    set id $_thread(id)
    set _thread(run:$id) 1
    return $id
}

# effects  Actually execute command if thread has not been canceled.
proc _thread_run {id cmd} {
    global _thread
    if ![info exists _thread(run:$id)] {return}

    unset _thread(run:$id)
    eval $cmd
}

proc _thread_iterate {id delay cmd} {
    global _thread
    if ![info exists _thread(run:$id)] {return}

    eval $cmd
    after $delay [list _thread_iterate $id $delay $cmd]
}
