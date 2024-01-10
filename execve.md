## Walkthrough of Linux 2.0.33's _execve_ System Call

### Code Walkthrough

#### sys\_execve (linux/arch/i386/kernel/process.c:571)

```txt
Control Flow:
sys_execve <-- Here

716: Copies the pathname from user space into the kernel
     space buffer filename.

719: Calls do_execve to complete the execve operation.

720: Frees the kernel space buffer filename.
```

#### getname (linux/fs/namei.c:29)

```txt
Control Flow:
sys_execve
    getname <-- Here

35-37: Checks if the address of the filename exists and is
       within userspace (< 3GB).

38-43: Sets variable i to PAGE_SIZE and preemptively assigns
       -ENAMETOOLONG to the error variable if the remaining
       amount of memory in the task exceeds PAGE_SIZE.

44-46: Calls get_fs_byte to obtain the first character of the
       filename and returns -ENOENT if the character is '\0'.

47-49: Obtains a free page of memory and assigns it to the
       tmp variable to create a 4096 character array.

50-57: Copies the filename to tmp, null terminates it, and
       returns 0.

58-59: Frees the character array if the filename exceeds
       PAGE_SIZE in length and returns -ENAMETOOLONG
       (recall that this was preemptively set on line 42).
```

#### do\_execve (linux/fs/exec.c:539)

```txt
Control Flow:
sys_execve
    getname
    do_execve <-- Here

548-549: Returns -EINVAL if the execve call was made from
         kernel space.

551-552: Clears binary parameters' page table.

553: Obtains the inode of the file we want to execve.

556-558: Initializes the binary parameter's file name,
         argument count, and environment count.

597: Clears the binary parameter's character buffer.

600: Calls read_exec to read the binary's executable
     header into the binary parameter's character
     buffer.

604-676: Handles the bash script case where sh_bang is NULL.

677-684: Uses the copy_strings routine to copy the execing
         process's arguments and environment variables into
         the binary parameters structure.

687-699: Iterates through the formats array until it calls
         the appropriate load_binary method for the binary. 

         The formats array is defined on line 746 and has the
         following contents:

         struct linux_binfmt formats[] = {
             {load_aout_binary, load_aout_library},
         #ifdef CONFIG_BINFMT_ELF
             {load_elf_binary, load_elf_library},
         #endif
         #ifdef CONFIG_BINFMT_COFF
             {load_coff_binary, load_coff_library},
         #endif
             {NULL, NULL}
         };

693-696: Decrements the binary's inode refcount, sets the
         current process' did_exec flag, and returns 0.
```

#### read\_exec (linux/fs/exec.c:420)

```txt
Control Flow:
sys_execve
    getname
    do_execve
        read_exec <-- Here

428-434: Initializes the stack allocated file structure.

435-436: Opens the binary file using inode's open method
         and the stack allocated file structure.

440-443: If it exists, uses the inode's lseek method to
         move the binary's file offset to 0.

444: Manually sets the binary file's offset to 0 if its
     inode does not have an lseek method.

450: Calls the inode's read method to read the first count
     bytes of the file into the addr buffer and assigns the
     return value to result.

452-453: Closes the binary file using its inodes release
         method.

455: Returns the return value of the read method.
```

#### load\_elf\_binary (linux/fs/binfmt\_elf.c:254)

```txt
Control Flow:
sys_execve
    getname
    do_execve
        read_exec
        load_elf_binary <-- Here


To facilitate the parsing of the ELF header, the kernel
uses the following data structure defined on line 123 of
/include/linux/elf.h:

struct elfhdr{
  char	e_ident[16];
  short int e_type;
  short int e_machine;
  int   e_version;
  char *e_entry;  /* Entry point */
  int   e_phoff;
  int   e_shoff;
  int   e_flags;
  short int e_ehsize;
  short int e_phentsize;
  short int e_phnum;
  short int e_shentsize;
  short int e_shnum;
  short int e_shstrndx;
};


281-283: Return -ENOEXEC if the binary header does not
         have the magic number {0x7f, 'E', 'L', 'F'}.

296-297: Allocates a kernel buffer to hold the binary's
         program header table.

301-302: Reads the program header into the elf_phdata
         buffer.


```

#### open\_inode (linux/fs/exec.c:56)

```txt
Control Flow:
sys_execve
    getname
    do_execve
        read_exec
        load_elf_binary
            open_inode <-- Here

63: Obtains a pointer to an empty file structure.

75-81: Initializes the file structure.

83: Calls the inode's open method to initialize the inode.
```

#### read\_exec (linux/fs/exec.c:420)

```txt
Control Flow:
sys_execve
    getname
    do_execve
        read_exec
        load_elf_binary
            read_exec <-- Here

428-434: Initializes stack allocated file structure.

435-436: Invokes the inode's open method to open the
         executable file for reading.

440-441: Invokes the inode's lseek method to set the
         file offset to the program header.

443-444: Sets the file's f_pos field to offset if the
         executable's inode does not have an lseek
         method.

450: Invokes the inode's read method to read count
     bytes into the addr buffer.

452-453: Invokes the inode's release method to
         decrement the executable's refcount.

455: Returns the number of bytes read.
```

#### putname (linux/fs/namei.c:62)

```txt
Control Flow:
sys_execve
    getname
    do_execve
    putname <-- Here

64: Frees the filename from kernel space.
```
