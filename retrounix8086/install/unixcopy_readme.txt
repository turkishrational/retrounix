UNIXCOPY.COM: (for Retro UNIX 8086/386 v1)
Retro UNIX 8086 v1 FS File Import/Export Utility (MSDOS program)
(UNIXCOPY by Erdogan TAN 2012 - [05/03/2016])

('?' command lists valid unixcopy commands)

usage:
	unixcopy [Floppy Drive or File Name]

	Floppy Drive names:

	fd0    (1.44MB, 3.5" Floppy Disk 1, A:)
	fd1    (1.44MB, 3.5" Floppy Disk 2, B:)

	(1.44MB, 3,5") 
	Floppy Disk Image File name examples:
 
	fd0.img
        fd1.img
	runixfd.img

NOTE:
Retro UNIX v1 bootable floppy disk images or (1.44MB) bootable disk (fs)
      can be created by using
      UNIXFDFS.COM utility (MSDOS program)
      then..
      UNIXCOPY.COM is used to copy files 
		from dos/windows to retro unix v1 fd (fs) disk
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

// Retro UNIX v1 fs -> 8 byte file names (8 byte directory entries)
	(Retro UNIX 8086 v1 & Retro UNIX 386 v1)
// Retro UNIX v1.1 fs -> 14 byte file names (14 byte directory entries)
	(Retro UNIX 386 v1.1)

Erdogan Tan - 24/01/2022 
	 