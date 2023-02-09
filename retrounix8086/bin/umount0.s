; ****************************************************************************
; umount8086.s (umount0.s) - by Erdogan Tan - 08/05/2022
; ----------------------------------------------------------------------------
; Retro UNIX 8086 v1 - umount -- dismount file system
;
; [ Last Modification: 10/05/2022 ]
;
; ****************************************************************************
; (/etc/umount)

; umount0.s - Retro UNIX 8086 v1 (16 bit version of 'umount1.s')
; umount1.s - Retro UNIX 386 v1 (& v1.1 & v1.2)

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
	;cmp	cx, 2
	cmp	cl, 2
	je	short umnt_1

	; print usage message and then exit
umnt_0:
	mov	ax, usage_msg
p_msg_exit:
	call	print_msg
exit:
	sys	_exit
;hang:
;	nop
;	jmp	short hang

umnt_1:
	pop	si ; argument 1 = device name

	; only superuser (root) can do mount/umount
	sys	_getuid
	and	ax, ax ; uid = 0 -> root
	jz	short umnt_2
	mov	ax, deny_msg
	jmp	short p_msg_exit
umnt_2:
	; si = device name address
	sys	_umount, si
	jc	short umnt_err


	; 10/05/2022
	; Following -temporary- code is not needed..
	; (If /dev/fd0 is not mounted, -Retro Unix 8086 v1-
	;  sysumount system call returns with error -cf=1-)

	; ----
	
;	; Temporary! 08/05/2022 
;	; device number of /dev/fd0 is 0 and this
;	; affects sysumount system call return 
;	; because [mdev] = 0 when there is not
;	; a mounted device and sysumount checks this!.
;	;
;	; (It does not cause an error in file system
;	;  but sysumount returns to user with wrong
;	; 'ok'. For now, till sysumount system call
;	; modification, 'ok.' message must/will not
;	; be written for /dev/fd0.) 
;
;	lodsw
;	cmp	ax, '/d'
;	jne	short umnt_3 ; (ax = 'fd', [si] = '0')
;	; word [si] = 'ev'
;	add	si, 5
;	; byte [si] = '0' ; '/dev/'+'fd'+'0'
;umnt_3:
;	lodsb	; byte [si] = '0'
;	cmp	al, '0' ; 'fd0'
;	je	short exit ; do not write 'OK.'	

	mov	ax, ok_msg
	jmp	short p_msg_exit
umnt_err:
	mov	eax, err_msg
	jmp	short p_msg_exit

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

usage_msg:
	db 0Dh, 0Ah
	db "Usage: umount <block device name> "
	db 0Dh, 0Ah
	db "Example: umount /dev/fd1 "
	db 0Dh, 0Ah, 0

deny_msg:
	db 0Dh, 0Ah	
	db "Umount: Only superuser/root can dismount devices!"	
 	db 0Dh, 0Ah, 0  		
err_msg:
	db 0Dh, 0Ah
	db 'Error! '
	db 0Dh, 0Ah, 0
ok_msg:
	db 0Dh, 0Ah
	db 'OK. '
nextline:
	db 0Dh, 0Ah, 0

;-----------------------------------------------------------------
