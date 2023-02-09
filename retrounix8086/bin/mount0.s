; ****************************************************************************
; mount8086.s (mount0.s) - by Erdogan Tan - 08/05/2022
; ----------------------------------------------------------------------------
; Retro UNIX 8086 v1 - mount -- mount file system
;
; [ Last Modification: 10/05/2022 ]
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

;%macro sys 1-4
;   ; 03/09/2015
;   ; 13/04/2015
;   ; Retro UNIX 386 v1 system call.
;   %if %0 >= 2   
;       mov ebx, %2
;       %if %0 >= 3
;           mov ecx, %3
;           ;%if %0 = 4
;           %if	%0 >= 4 ; 11/03/2022
;		mov edx, %4
;           %endif
;       %endif
;   %endif
;   mov eax, %1
;   int 30h
;%endmacro

%macro sys 1-4
    ; Retro UNIX 8086 v1 system call.
    %if %0 >= 2   
        mov bx, %2
        %if %0 >= 3
            mov cx, %3
            %if %0 >= 4
               mov dx, %4
            %endif
        %endif
    %endif
    mov ax, %1
    int 20h
%endmacro

;; Retro UNIX 386 v1 system call format:
;; sys systemcall (eax) <arg1 (ebx)>, <arg2 (ecx)>, <arg3 (edx)>

;; 11/03/2022
;; Note: Above 'sys' macro has limitation about register positions;
;;	ebx, ecx, edx registers must not be used after their
;;	positions in sys macro.
;; for example:
;;	'sys _write, 1, msg, ecx' is defective, because
;;	 ecx will be used/assigned before edx in 'sys' macro.
;; correct order may be:
;;	'sys _write, 1, msg, eax ; (eax = byte count)

; Retro UNIX 8086 v1 system call format:
; sys systemcall (ax) <arg1 (bx)>, <arg2 (cx)>, <arg3 (dx)>

; ----------------------------------------------------------------------------

[BITS 16] ; 16-bit intructions (8086/8088 - Real Mode)

[ORG 0] 

START_CODE:
	; 10/05/2022
	; 09/05/2022 (16 bit code)
	; 08/05/2022
	pop	cx ; cx = number of arguments
	;
	pop	ax ; ax = argument 0 = executable file name
	;
	;cmp	cx, 3
	cmp	cl, 3
	jne	short mnt_0

	pop	di ; argument 1 = device name
	pop	bp ; argument 2 = mounting directory

	mov	si, bp

	; 09/05/2022

;; 'pwd' command compatibility restriction !
;;	; ---
;;	; Only '/usr' or '/mnt' directories can be used
;;	;  with current mount version (otherwise current
;;	; 'pwd' will not recognize mounting directory
;;	;  as correct).
;
;	; short way...
;
;	lodsw
;	cmp	al, '/'
;	jne	short mnt_5
;	cmp	ah, 'u'
;	jne	short mnt_m
;mnt_u:
;	lodsw
;	cmp	ax, 'sr'
;	je	short mnt_1
;	jmp	short mnt_2
;mnt_m:
;	cmp	ah, 'm'
;	jne	short mnt_2
;	lodsw
;	cmp	ax, 'nt'
;	jne	short mnt_2
;mnt_1:
;	cmp	byte [si], 0 ;
;	jna	short mnt_5 ; '/usr' or '/mnt', continue
;
;	; neither '/usr' or '/mnt'

;	; print usage message and then exit
;mnt_0:
;	mov	ax, usage_msg
;	jmp	short mnt_7

;p_msg_exit:
;	call	print_msg
;exit:
;	sys	_exit
;hang:
;	nop
;	jmp	short hang

mnt_2:
	; long way...
	sys	_stat, bp, stbuf
	jc	short mnt_dir_err
	; ax = device number

	; is directory ? (check inode mode)
	test	byte [stbuf+3], 40h ; directory
	jz	short mnt_dir_err	

	; is already mounted ?
	or	ax, ax
	jnz	short mnt_dir_err	

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

mnt_5:
	; only superuser (root) can do mount/umount
	sys	_getuid
	and	ax, ax ; uid = 0 -> root
	jz	short mnt_6
	mov	ax, deny_msg
	jmp	short mnt_7

	; 10/05/2022
	; print usage message and then exit
mnt_0:
	mov	ax, usage_msg
mnt_7:
	call	print_msg
exit:
	sys	_exit
;hang:
;	nop
;	jmp	short hang

mnt_6:
	; di = device name address
	; bp = mounting directory path/name address
 	;
	; 09/05/2022
	; Note: Retro UNIX 8086 v1 kernel 
	; (and original unix v1 kernel, sysmount)
	; mounts devices without checking superblock
	; validity. So, sysmount does not return
	; with 'invalid file system' error.
	; (but Retro UNIX 386 sysmount returns with
	; 'invalid file system' error)
	;
	; At sysmount system call return,
	; if cf=1 it is user permission error
	; or disk r/w error or 'already mounted' error.

	sys	_mount, di, bp
	jc	short mnt_err
	;
	mov	ax, ok_msg
	; 10/05/2022
	jmp	short mnt_7

;mnt_7:
;	call	print_msg
;exit:
;	sys	_exit
;;hang:
;;	nop
;;	jmp	short hang

mnt_dir_err:
	mov	ax, dir_err_msg
	jmp	short mnt_7

mnt_err:
	; 09/05/2022
	mov	ax, err_msg
	jmp	short mnt_7

;-----------------------------------------------------------------

print_msg:
	; ax = asciiz string address
	mov	si, ax
	dec	si
nextchr:
	inc	si
	cmp	byte [si], 0
	ja	short nextchr
	;cmp	[si], 0Dh
	;ja	short nextchr
	sub	si, ax
	; si = asciiz string length
	;
	sys	_write, 1, ax, si
	;
	retn

;-----------------------------------------------------------------
;  data - initialized data
;-----------------------------------------------------------------

; default mounting directories (for current retro unix version)
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
	;db "and be '/usr' or '/mnt'.)",
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

; sysstat buffer
stbuf: resb 34 ; (Retro UNIX 8086/386 v1 & Retro UNIX 386 v1.1)

;-----------------------------------------------------------------
