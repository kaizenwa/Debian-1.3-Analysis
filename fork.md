## Walkthrough of Linux 2.0.33's _fork_ System Call

### Code Walkthrough

#### sys\_fork (linux/arch/i386/kernel/process.c:551)

```txt
Control Flow:
sys_fork <-- Here

132-133: Allocates a new task structure.

134: Obtains the index of the first freely available
     process in the task array.

137: Assigns the new task structure to the free slot
     in the task array.

138: Copies all the fields from the current task into the
     new task.

139-155: Initializes the new task's fields.

156: Assigns the new task's start time.

160-170: Sets up the new task's TSS.

173: Assigns the new task's eip to reg_from_sys_call.

186: Assigns the new task's LDT.

200-203: Copies the parent task's open files.

204-208: Increments the refcount of the parent task's
         open files.

209-214: Increments the parent task's pwd, root dir, and
         its executable inode's refcount.

222: Assigns the new task half the remaining clock cycles
     as its parent.

224: Returns the pid of the child process to the parent
     process.
```

#### find\_empty\_process (linux/kernel/fork.c:38)

```txt
Control Flow:
sys_fork
    find_empty_process <-- Here
    ...

45-46: Resets last_pid to 1 if it is greater than or equal to
       65536 (2^16), which is the maximum number of pids that
       Linux 1.0 supports.

51-55: Counts the number of free tasks in the task array and assigns
       the index of the first free task to free_task.

57-58: Counts the number of tasks the current user owns.

59-61: Repeats the loop if last_pid is currently in use.

63-66: Returns -EAGAIN if there are less than four free tasks
       or if the user has more than 64 tasks.
```

#### copy\_page\_tables (linux/mm/memory.c:209)

```txt
Control Flow:
sys_fork
    find_empty_process
    copy_page_tables <-- Here for COPYVM flag


```

#### clone\_page\_tables (linux/mm/memory.c:194)

```txt
Control Flow:
sys_fork
    find_empty_process
    clone_page_tables <-- Here for !COPYVM flag

199: Increments the refcount of the parent process's
     page directory.

200: Assigns the parent process's page directory to
     the child process's CR3 field.
```

#### copy\_fd (linux/kernel/fork.c:70)

```txt
Control FLow:
sys_fork
    find_empty_process
    clone_page_tables
    clone/copy_page_tables
    copy_fd <-- Here


```

#### dup\_mmap (linux/kernel/fork.c:92)

```txt
Control FLow:
sys_fork
    find_empty_process
    clone_page_tables
    clone/copy_page_tables
    copy_fd
    dup_mmap <-- Here


```
