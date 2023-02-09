UNIXFDFS.COM:  (for Retro UNIX 386 v1.1)
RETRO UNIX 1.44 MB Floppy Disk (RUFS) Format Utility
	UNIXFDFS by Erdogan TAN 2014 - [09/01/2020]

Usage: unixfdfs [Drive]

	Drive names:

	fd0    (Floppy Disk 1)
	fd1    (Floppy Disk 2)
	...
	A:     (Floppy Disk 1)
	B:     (Floppy Disk 2)

NOTE:
  UNIXFDFS.COM utility (MSDOS program) is used to create
    Retro UNIX v1.1 fs diskette (1.44MB bootable disk)
    or to create a 1.44MB bootable floppy disk image.
      
  Then..

  UNIXCOPY.COM (MSDOS program) is used to copy files 
	from dos/windows to retro unix v1.1 fd (fs) or fd (fs) image

  ImDisk.exe (windows program) or another program can be used to mount
	floppy disk image as disk.

    And.. rawwritewin.exe can be used to write/read floppy disk image
	to/from 1.44MB floppy disk.			

  (('boot' must be set as startup file by using 'bootfile' cmd))
  (('?' command lists valid unixcopy commands))

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

// Retro UNIX 8086/386 v1 fs -> 8 byte file names (8 byte directory entries) 
// Retro UNIX 386 v1.1 fs -> 14 byte file names (14 byte directory entries)

Erdogan Tan - 24/01/2022 
	 