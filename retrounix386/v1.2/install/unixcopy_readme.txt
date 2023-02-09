UNIXCOPY.COM: (for Retro UNIX 386 v1.2 & v2)
Retro UNIX 386 v2 FS File Import/Export Utility (MSDOS program)
(UNIXCOPY by Erdogan TAN 2012 - [12/12/2021])

('?' command lists valid unixcopy commands)

usage:
	unixcopy [Floppy Drive]
	unixcopy -i [floppy disk image file name]

	Floppy Drive names:

	fd0    (1.44MB, 3.5" Floppy Disk 1, A:)
	fd1    (1.44MB, 3.5" Floppy Disk 2, B:)
	...
	A:     (1.44MB, 3.5" Floppy Disk 1)
	B:     (1.44MB, 3.5" Floppy Disk 2)

	(1.44MB, 3,5") 
	Floppy Disk Image File name examples:
 
	fd0.img
        fd1.img
	runixfd.img

NOTE:
Retro UNIX 386 v2 (& v1.2) bootable floppy disk images
      or (1.44MB) bootable disk (runix v2 fs) can be created by using
      UNIXFDFS.COM utility (MSDOS program)
      then..
      UNIXCOPY.COM is used to copy files 
		from dos/windows to retro unix v1.2 (& v2) fd (fs) disk
		or (1.44MB, 3.5") fd image

 	ImDisk.exe (windows program) or another program can be used to mount
		floppy disk image as disk.

    	And..	rawwritewin.exe can be used to write/read floppy disk image
		to/from 1.44MB floppy disk.			

  	(('boot' file must be set as startup file by using 'bootfile' cmd))

	***

	for Example..

	[ These files are copied to fd or fd image file via UNIXCOPY.COM ]

	boot (startup) file: 
		/boot (will be launched by boot sector code)
		('?' is used to see valid 'boot:' prompt commands)
		-must be set as 'startup file' by using 'bootfile' command-
 	kernel:
		/unix (there may be another kernel file, 'unix' is default)
	
	/dev:	device files directory
	/bin:	command (binary, executable) files directory
	/usr:	mount (or user) directory
		-/etc/init mounts /dev/fd1 to /usr if it has valid runix v1 fs- 

	other files and their directories:

	/bin:
		login, ls, sh, cat
	/etc:
		init, getty, motd (text), passwd (text)

	/usr/bin:
		demo programs
	/erdogan:
		mailbox (text) and demo programs
	/tmp:
		wtmp (login records file) 
			'/etc/init' will use it if it is there
			-grows after every login, must be deleted later-

		utmp (login record file) 
			will be created for every init stage by 'etc/init'.

***

// Retro UNIX 8086 v1 -> unix (runix) v1 file system 
	8 byte file names, 32 byte inodes, 32KB (64KB) file size limit
	(mostly similar to unix v1 fs)
	((8086 real mode kernel and program files))
	(((derived from unix v1/v2 pdp-11 assembly source code)))
	Assembler: MASM 6.11 (MASM 6.14)

// Retro UNIX 386 v1 -> unix (runix) v1 file system 
	8 byte file names, 32 byte inodes, 64KB file size limit
	(mostly similar to unix v1 fs)
	((80386 protected mode kernel and program files))
	(((modified from Retro Unix 8086 v1 src and IBM PC/AT BIOS src)))
	Assembler: NASM 2.11 (NASM 2.15)

// Retro UNIX 386 v1.1 -> runix v1 modified file system 
	14 byte file names (14 byte directory entries)
	32 byte inodes, 64KB file size limit
	(similar to unix v1 fs except 14 byte file names)
	((80386 protected mode kernel and program files))
	(((modified from Retro Unix 386 v1 source code)))
	Assembler: NASM 2.11 (NASM 2.15)

// Retro UNIX 386 v1.2 (& v2) -> runix v2 (unix v7 modified) file system 
	14 byte file names, 64 byte inodes, 4GB file size limit
	separate inode map, free blocks map sectors (bitmap method)
	(different from unix v7 fs except conceptual similaries)
	((80386 protected mode kernel and program files))
	(((modified from Retro Unix 386 v1.1 & 2.0 draft source code)))
	Assembler: NASM 2.11 (NASM 2.15)

Binary (Executable) files: Flat model (code starts from offset 0)
			(Retro UNIX 386 v1, v1.1, v1.2, v2)
			80386 protected mode instructions (with paging)
			(real multi tasking, 4GB virtual memory limit)
			***
			(Retro UNIX 8086 v1) 
			or 8086 real mode instructions (segmented run) 
			(virtual multi tasking, 64KB real memory limit)

System Call method:
		int 30h - interrupt service/handler
		eax = function number
		ebx, ecx, edx = arguments (parameters, address pointers)

	Macro: sys function, argument1, argument2, argument3
		 mov ebx, argument1 ; (optional, depends on function)
		 mov ecx, argument2 ; (optional, depends on function)
		 mov edx, argument3 ; (optional, depends on function)
		 mov eax, function number ; (system call number)
		 int 30h  ; (system call via software interrupt)			

Kernel feature: Monolithic kernel (runs in ring 0 with real addr paging)
			(Programs run in ring 3 with virtual addr paging)
			((Paging method: Demand paging)) 

Erdogan Tan - 24/01/2022 