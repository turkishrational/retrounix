; ****************************************************************************
; umount386.s (umount1.s) - by Erdogan Tan - 08/05/2022
; ----------------------------------------------------------------------------
; Retro UNIX 386 v1 - umount -- dismount file system
;
; [ Last Modification: 14/05/2022 ]
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
	; 08/05/2022
	pop	ecx ; ecx = number of arguments
	;
	pop	eax ; eax = argument 0 = executable file name
	;
	;cmp	ecx, 2
	cmp	cl, 2
	je	short umnt_1

	; print usage message and then exit
umnt_0:
	mov	eax, usage_msg
p_msg_exit:
	call	print_msg
exit:
	sys	_exit
;hang:
;	nop
;	jmp	short hang

umnt_1:
	pop	edi ; argument 1 = device name

	; only superuser (root) can do mount/umount
	sys	_getuid
	and	eax, eax ; uid = 0 -> root
	jz	short umnt_2
	mov	eax, deny_msg
	jmp	short p_msg_exit
umnt_2:
	; edi = device name address
	sys	_umount, edi
	jc	short umnt_err

	; 14/05/2022
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
;	mov	eax, [edi]
;	cmp	eax, '/dev'
;	jne	short umnt_3
;	mov	eax, [edi+5]  ; '/'+'fd0'+0
;umnt_3:
;	cmp	eax, 'fd0'
;	je	short exit ; do not write 'OK.'	
;

	mov	eax, ok_msg
	jmp	short p_msg_exit
umnt_err:
	mov	eax, err_msg
	jmp	short p_msg_exit

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

; 08/05/2022

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
