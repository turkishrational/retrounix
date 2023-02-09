; ****************************************************************************
; mount386.s (mount1.s) - by Erdogan Tan - 08/05/2022
; ----------------------------------------------------------------------------
; Retro UNIX 386 v1 - mount -- mount file system
;
; [ Last Modification: 14/05/2022 ]
;
; ****************************************************************************
; (/etc/mount)

; mount0.s - Retro UNIX 8086 v1 (16 bit version of 'mount1.s')
; mount1.s - Retro UNIX 386 v1 (& v1.1 & v1.2)

; UNIX v1 system calls
_rele 	equ 0
_exit 	equ 1
_fork 	equ 2
_read 	equ 3
_write	equ 4
_open	equ 5
_close 	equ 6
_wait 	equ 7
_creat 	equ 8
_link 	equ 9
_unlink	equ 10
_exec	equ 11
_chdir	equ 12
_time 	equ 13
_mkdir 	equ 14
_chmod	equ 15
_chown	equ 16
_break	equ 17
_stat	equ 18
_seek	equ 19
_tell 	equ 20
_mount	equ 21
_umount	equ 22
_setuid	equ 23
_getuid	equ 24
_stime	equ 25
_quit	equ 26	
_intr	equ 27
_fstat	equ 28
_emt 	equ 29
_mdate 	equ 30
_stty 	equ 31
_gtty	equ 32
_ilgins	equ 33
_sleep	equ 34 ; Retro UNIX 8086 v1 feature only !
_msg    equ 35 ; Retro UNIX 386 v1 feature only !
_geterr	equ 36 ; Retro UNIX 386 v1 feature only !

;;;
ESCKey equ 1Bh
EnterKey equ 0Dh

%macro sys 1-4
    ; 03/09/2015	
    ; 13/04/2015
    ; Retro UNIX 386 v1 system call.		
    %if %0 >= 2   
	mov ebx, %2
	%if %0 >= 3    
	    mov ecx, %3
	    %if %0 = 4
	       mov edx, %4   
	    %endif
	%endif
    %endif
    mov eax, %1
    int 30h	   
%endmacro

; Retro UNIX 386 v1 system call format:
; sys systemcall (eax) <arg1 (ebx)>, <arg2 (ecx)>, <arg3 (edx)>

[BITS 32] ; 32-bit intructions (for 80386 protected mode)

[ORG 0] 

START_CODE:
	; 14/05/2022
	; 08/05/2022
	pop	ecx ; ecx = number of arguments
	;
	pop	eax ; eax = argument 0 = executable file name
	;
	;cmp	ecx, 3
	cmp	cl, 3
	jne	short mnt_0

	pop	edi ; argument 1 = device name
	pop	ebp ; argument 2 = mounting directory

	mov	esi, ebp

; 14/05/2022
;	; 08/05/2022
;	;lodsd	; lodsd gives gpf if dir name length < 4
;	lodsb  ; *!
;	cmp	al, '/'
;	jne	short mnt_2
;mnt_u:
;	lodsb
;	cmp	al, 'u'
;	jne	short mnt_m
;	lodsb
;	cmp	al, 's'
;	jne	short mnt_2
;	lodsb
;	cmp	al, 'r'
;	je	short mnt_1
;	jmp	short mnt_2
;mnt_m:
;	cmp	al, 'm'
;	jne	short mnt_2
;	lodsb
;	cmp	al, 'n'
;	jne	short mnt_2
;	lodsb
;	cmp	al, 't'
;	jne	short mnt_2

; 14/05/2022
;;	; 08/05/2022
;;	; 'pwd' command compatibility restriction !
;;	; ---
;;	; Only '/usr' or '/mnt' directories can be used
;;	;  with current mount version (otherwise current
;;	; 'pwd' will not recognize mounting directory
;;	;  as correct).
;;
;;	; short way...
;;	cmp	eax, '/usr'
;;	je	short mnt_1
;;	cmp	eax, '/mnt'
;;	jne	short mnt_2
;mnt_1:
;	cmp	byte [esi], 0 ;
;	jna	short mnt_5 ; '/usr' or '/mnt', continue
;
;	; neither '/usr' or '/mnt'
;
;	; print usage message and then exit
;mnt_0:
;	mov	eax, usage_msg
;
;	jmp	mnt_7

;p_msg_exit:
;	call	print_msg
;exit:
;	sys	_exit
;hang:
;	nop
;	jmp	short hang

mnt_2:
	; long way...
	sys	_stat, ebp, stbuf
	jc	short mnt_dir_err
	; 14/05/2022
	; eax = device number (in AL)

; 14/05/2022
;	mov	dx, [stbuf] ; inode number
;	
;	sys	_stat, usr, stbuf
;	jc	short mnt_4
;	
;	; check if it is '/usr' inode
;	cmp	dx, [stbuf] ; same inode number
;	je	short mnt_5
;mnt_4:
;	sys	_stat, mnt, stbuf
;	jc	short mnt_dir_err
;
;	; check if it is '/mnt' inode
;	cmp	dx, [stbuf] ; same inode number
;	jne	short mnt_dir_err

	; 14/05/2022
	; is directory ? (check inode mode)
	test	byte [stbuf+3], 40h ; directory
	jz	short mnt_dir_err	

	; is already mounted ?
	or	al, al
	;or	eax, eax
	jnz	short mnt_dir_err

mnt_5:
	; only superuser (root) can do mount/umount
	sys	_getuid
	and	eax, eax ; uid = 0 -> root
	jz	short mnt_6
	mov	eax, deny_msg
	jmp	short mnt_7

	; 14/05/2022
	; print usage message and then exit
mnt_0:
	mov	eax, usage_msg
mnt_7:
	call	print_msg
exit:
	sys	_exit
;hang:
;	nop
;	jmp	short hang

mnt_6:
	; edi = device name address
	; ebp = mounting directory path/name address
 	;
	sys	_mount, edi, ebp
	jc	short mnt_err
	;
	mov	eax, ok_msg
	; 14/05/2022
	jmp	short mnt_7

;mnt_7:
;	call	print_msg
;exit:
;	sys	_exit
;;hang:
;;	nop
;;	jmp	short hang

mnt_dir_err:
	mov	eax, dir_err_msg
	jmp	short mnt_7

mnt_err:
; 14/05/2022
	;; Temporary! 08/05/2022
	;; Lets do not recognize 'invalid file system'
	;; error for now! (for current version and for
	;; current kernel response to sysmount system call)
	;;
	;; ((DOS FAT file system disk or another/new/old
	;;   type retro unix fs or invalid free map size
	;;   or invalid/unexpected superblock content))
	;;
	;; Note: (08/05/2022)
	;;   [drv.size] is calculated with cylinders-1
  	;;   for floppy disks ('diskinit.s' ,'set_disk_parms')
	;;   So, free map size and [drv.size]/8 is not same.
	;;   This is sysmount error return reason
	;;   (as a bug in current Retro Unix 386 kernel).
	;
	;sub	ebx, ebx
	;sys	_geterr	; (get last error in eax)
	;
	;; (eax = error code from sysmount system call)
	;cmp	eax, 28 ; ERR_INV_FS ('sysmnt_5:', 'u7.s')
	;jne	short mnt_8
	;;'invalid fs/superblock !' error
	;; (exit without 'oK.' message) 
	;jmp	exit
;mnt_8:
	mov	eax, err_msg
	jmp	short mnt_7

;-----------------------------------------------------------------

print_msg:
	; 08/05/2022
	; eax = asciiz string address
	mov	edx, eax
	dec	edx
nextchr:
	inc	edx
	cmp	byte [edx], 0
	ja	short nextchr
	;cmp	[edx], 0Dh
	;ja	short nextchr
	sub	edx, eax
	; edx = asciiz string length
	;
	sys	_write, 1, eax
	;
	retn

;-----------------------------------------------------------------
;  data - initialized data
;-----------------------------------------------------------------

; 14/05/2022
;; 08/05/2022

;; default mounting directories (for current retro unix version)
;usr:	db '/usr', 0
;mnt:	db '/mnt', 0

usage_msg:
	db 0Dh, 0Ah
	;db "Usage: mount <block device name> </usr> "
	;db 0Dh, 0Ah
	;db "    or mount <block device name> </mnt> "
	db "Usage: mount <block device name> <dir> "
	;db 0Dh, 0Ah
	db 0Dh, 0Ah
	db "Example: mount /dev/fd1 /usr "
	db 0Dh, 0Ah, 0

deny_msg:
	db 0Dh, 0Ah	
	db "Mount: Only superuser/root can mount devices!"	
 	db 0Dh, 0Ah, 0  		
err_msg:
	db 0Dh, 0Ah
	db 'Error! '
	db 0Dh, 0Ah, 0
dir_err_msg:
	db 0Dh, 0Ah
	db 'Mount Directory Error!'
	;db 0Dh, 0Ah
	;db "(Mounting directory must exist "
	;db "and be '/usr' or '/mnt'.) "
	db 0Dh, 0Ah, 0
ok_msg:
	db 0Dh, 0Ah
	db 'OK. '
nextline:
	db 0Dh, 0Ah, 0

;-----------------------------------------------------------------
;  bss - uninitialized data
;-----------------------------------------------------------------	

align 4

bss_start:

ABSOLUTE bss_start

; 08/05/2022

;stbuf: resb 34 ; (Retro UNIX v1 and v1.1)
stbuf:	resb 66	; stat(us) buffer
		; (Retro UNIX v1, v1.1 and v1.2)

;-----------------------------------------------------------------
