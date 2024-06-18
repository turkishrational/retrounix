Retro UNIX 386 v1.2 disk UMOUNT/MOUNTH principle:
(fd0, fd1, hd0 must be set in QEMU or VIRTUALBOX or BOCHS)

fd0.img has the /etc/init file which mounts /dev/fd1 into /usr at startup
if the boot disk is not /dev/hd0, it is mounted as ..
  umount /dev/fd1
  mount /dev/hd0 /mnt (or "mount /dev/hd0 /usr")
If the boot disk is /dev/hd0, /dev/fd1 (or /dev/fd0) is mounted as ..
  (umount /dev/fd0)
  mount /dev/fd0 /mnt (or "mount /dev/fd0 /usr") or
  mount /dev/fd1 /mnt (or "mount /dev/fd1 /usr")

  umount and mount give "OK" message if there is not an error.
  after umount, mounting directory (/usr or /mnt or any) returns to its normal/own content.
  (In Retro UNIX v1.2, only one additional disk can be mounted under a sub directory of the root disk,
   anotherone can be mounted after the previous is unmounted. Disks must have Retro UNIX v2 fs.
   UNIXCOPY, UNIXFDFS, UNIXHDCP, UNIXHDFS .COM/DOS programs are used to prepare these RUNIX v2 FS disks.)

Startup:
     boot: unix
when you see "Type ENTER to start in multi user mode ..." message,
     press ENTER key.. then write login name as "root" at (tty0) login prompt.
     (you can use pseudo TTYs, by using ALT+F1 .. ALT+F8 key combinations)
     change screen to tty1 (via ALT+F2 key) than write login name as "erdogan" and then
     press ENTER and just after you see password prompt, write password as "417".
    (/etc/motd message will appear and the shell (/bn/sh) will be launched by /etc/init.)
    ((tty09 -> COM1, tty10 -> COM2, terminal emulation program is in A.IMG file -SERIAL6.COM-.
      If any 2 computers -one of them must run DOS SERIAL6.COM other one must run RUNIX v1.2- are connected to eachother
      via their serial ports, RUNIX v1.2 will run as a server in text mode -only-, tty09 or tty10 will be the DOS user's terminal.)) 

(Basic commands -ls,cd,cp and others- are used as in UNIX v1)
