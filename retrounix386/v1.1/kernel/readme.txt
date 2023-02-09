You can assemble Retro UNIX 386 v1.1
source code by using NASM (v2.11)...

((unix386.s is the main source file, 
includes other source files
via %include directive))

Example:
nasm unix386.s -l unix386.txt -o unix

(For above example, all of Retro UNIX 386
v1.1 kernel --".s" & ".inc"-- source
files must be in the nasm program directory...
But, you can use another directory or
you can copy files between nasm directory
and your working directory; for axample, 
you can use 'runix' directory as working dir.)

(Read NASM manual for details...)

******

Then you can copy 'unix' binary/kernel to
root directory of the Retro UNIX 386 FS
floppy disk...

by using 'UNIXCOPY.COM' program

Example:
fromdos c:\nasm\unix /unix

******

In order to make a new 
Retro UNIX 386 bootable fd (1.44MB, 3.5")...
you can use UNIXFDFS.COM program

or 

you can continue to work on Retro UNIX 386
with sample 1.44MB floppy disks 
with bootable Retro UNIX (386 v1) FS
images.
