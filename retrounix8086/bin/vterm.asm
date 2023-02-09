; VTERM.ASM

; Retro UNIX 8086 v1 Virtual Terminal Program
; (Standalone program for RUNIX boot utility)
; by Erdogan TAN
; 27/06/2014
; 22/05/2014, 26/05/2014, 11/06/2014, 19/06/2014, 26/06/2014
; 30/04/2014, 01/05/2014, 02/05/2014, 19/05/2014, 20/05/2014 

.8086

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
_sleep	equ 34 ; 11/06/2014 (Temporary!)

sys macro syscallnumber, arg1, arg2, arg3

    ; Retro UNIX 8086 v1 system call.

    ifnb <arg1> 	
      mov bx, arg1
    endif
    
    ifnb <arg2> 	
      mov cx, arg2
    endif

    ifnb <arg3> 	
      mov dx, arg3
    endif
     			
    mov ax, syscallnumber	
    int 20h	
   
    endm

; Retro UNIX 8086 v1 system call format:
; sys systemcall (ax) <arg1 (bx)>, <arg2 (cx)>, <arg3 (dx)>



CODE_SEG	segment para public
		assume  CS:CODE_SEG, DS:CODE_SEG, SS:CODE_SEG, ES:CODE_SEG

start:
		; 27/06/2014
		sys 	_gtty, 0, 1  ; get status of console tty (w)
        	;jc  	short terminate
		cmp	al, 7
		ja	short terminate
		mov 	byte ptr [console], al
		; al = display page (0 to 7)
		mov	ah, 05h  ; select active display page
		int	10h
		;
		; 26/05/2014
		; xor 	dx, dx	   ; 0	
		mov	ah, 06h	   ; Check virtual terminal status
		int	14h	   ; (This Will set virtual terminal if it
				   ; it is not already set)		 
		; 23/06/2014
		test	ah, 80h
		jz	short @f
		; (Only one virtual terminal can be used!)
		mov	al, 07h	   ; BEEP !
		mov	ah, 0Eh
		int 	10h
terminate:
		mov	ax, 1	   ; SYS EXIT
		int	20h
@@:
		mov 	ax, 0600h  ; Scroll up, clear (AL=0)
		mov	bh, 07h    ; Black backround (0), 
				   ; Light gray foreground (7)
		sub	cx, cx     ; Left-Upper column, row
		mov	dx, 184Fh  ; Righ-Lower column, row
		int 	10h			 
		;
		mov	ah, 2	   ; Set cursor position 	
		xor	dx, dx	   ; Row 0 (DH), Column 0 (DL)	
		mov	bh, byte ptr [console] ; 27/06/2014
		int 	10h
		;
		mov	si, offset StartMsg
		mov	bl, 7
		call	proc_printmsg
@@:
		;xor	ah, ah
		;int 	16h
_0:
		sys	_read, 0, chr, 1
		;
		mov	al, byte ptr [chr]
		;
		cmp	al, '1'
		je	short @f
		cmp	al, '2'
		je	short @f
		;
		mov	al, 07h	   ; BEEP !
		mov	ah, 0Eh
		int	10h
		jmp	short @b	
@@:
		sub	al, '1'
		mov	byte ptr [port], al
		;
		mov     si, offset ComSMsg
		add	byte ptr [SI]+4, al
		; 27/06/2014
		mov	bh, byte ptr [console]
		mov	bl, 7
		call	proc_printmsg
		;
		xor	ah, ah 
		mov	al, 0E3h  ; Communication parameters
				  ; 9600 baud, parity none, one stop bit
		mov	dx, word ptr [port]
		int	14h	
		;
		mov 	cx, 65535
@@:
		nop
		nop
		nop
		loop	@b                 
		;
		mov     si, offset AnyKeyMsg
		call	proc_printmsg
		;
		;xor	ah, ah
		;int 	16h
		;
		sys	_read, 0, chr, 1
		;
		;
		mov 	ax, 0600h  ; Scroll up, clear (AL=0)
		mov	bh, 17h    ; Blue backround (1), 
				   ; Light gray foreground (7)
		sub	cx, cx     ; Left-Upper column, row
		mov	dx, 184Fh  ; Righ-Lower column, row
		int 	10h			 
		;
		mov	ah, 2	   ; Set cursor position 	
		sub	dx, dx	   ; Row 0 (DH), Column 0 (DL)	
		mov	bh, byte ptr [console] ; 27/06/2014	
		int 	10h
		;
		;mov	dx, word ptr [port]
		mov	dl, byte ptr [port]
sps:
		mov	ah, 06h
		;mov	ah, 03h    ; Get serial port status
		int	14h
		test	ah, 1	; 26/06/2014
		jnz	short rcvc
		sys	_sleep	; 11/06/2014
		jmp	short sps
rcvc:
		mov	ah, 05h
		;mov	ah, 02h	   ; Receive one character
		int	14h
		; 23/06/2014
		test	ah, 80h
		jz	short @f
		;
		sys	_sleep	; 11/06/2014
		jmp	short rcvc
		
@@:		; 23/06/2014
		and	al, al ; 0, EOT  ; 19/06/2014
		jz	short @f
		; 27/06/2014
		; Print character at current cursor position
		mov	bh, byte ptr [console]
		mov	bl, 7
		mov	ah, 0Eh
		int	10h
		;jmp	short rcvc
		jmp	short sps
@@:	
		mov	ah, 06h
		int	14h
		test	ah, 32 ; Transmitter buffer empty
		jnz	short @f
		test	ah, 1
		jnz	short rcvc
		sys	_sleep
		jmp	short @b
@@:
		;xor	ah, ah
		;int	16h
		sys	_read, 0, chr, 1
		;
		;mov	dx, word ptr [port] ; 26/06/2014
		mov	dl, byte ptr [port] ; dh = 0
		;
		mov	al, byte ptr [chr]
		cmp	al, 0Dh
		jne	short sndc
		xor	al, al ; 0 , EOT ; 19/06/2014
sndc:
		; al = chr, ah = function (send one chr)
		call	putchar
		;mov	al, byte ptr [sndchr] ; char echo
		;
		or	al, al ; 0, EOT ; 19/06/2014
		jz	short sps
		;; 11/06/2014 (echo --> on)
		; 20/05/2014
		mov	bh, byte ptr [console] ; 27/06/2014
		mov	bl, 7
		mov	ah, 0Eh
		int	10h
		jmp	short @b

proc_printmsg:
		mov	ah, 0Eh
		;mov	bh, byte ptr [console] ; 27/06/2014
		;mov 	bl, 7
@@:
		lodsb
		and	al, al
		jz	short @f
		int 	10h
		jmp	short @b
@@:          
		retn

putchar:
		; 27/06/2014
		; 23/06/2014
		; 11/06/2014
		; al = character to be sent
		;
		;mov	dx, word ptr [port]	
		mov	byte ptr [chr], al
putc_n:
		mov	ah, 06h ; get COM port status
				; for virtual terminal
		int	14h
		; ah = 1 if byte ptr [chr] > 0		
		; ah = 0 if byte ptr [chr] = 0
		; ah >= 80h if there is an error
		
		test	ah, 80h
		jz	short @f
		sys	_sleep
		jmp	short putc_n
@@:
		test	ah, 1
		jz	short @f
getc:
		mov	ah, 05h ; receive one char
		int	14h
		test	ah, 80h
		jnz	short putc ; 27/06/2014
		and	al, al ; 0, EOT ; 19/06/2014
                jz      short putc ; 27/06/2014
		mov	bh, byte ptr [console] ; 27/06/2014
		mov 	bl, 7
		mov	ah, 0Eh
		int	10h
		sys	_sleep
		; 27/06/2014
		jmp	short putc_n
@@:	
		test	ah, 20h ; THR empty status 
		jnz	short putc
		sys	_sleep
		jmp	short putc_n ; try again		
putc:
		mov	al, byte ptr [chr]
		mov	ah, 04h	; send one char	
		int	14h
		; 27/06/2014
		push	ax
		sys	_sleep	; 11/06/2014
		pop	ax
		test	ah, 80h
		jnz	short putc_n
@@: 
		mov	al, byte ptr [chr]
		and	al, al
		jnz	short ech
@@:
		retn
ech:
		mov	ah, 05h ; receive one char
		int	14h
		; 23/06/2014
		test	ah, 80h
		jz	short @b
@@:
		; 27/06/2014
		sys	_sleep
		mov	ah, 06h
		int	14h
		test	ah, 80h
		jnz	short @b
		test	ah, 1
		jz	short @b
		jmp	short ech		


StartMsg:
                db 0Dh,0Ah
                db 'Terminal program for Retro UNIX 8086 v1...'
		db 0Dh,0Ah
                db 'Press 1 for COM 1 or press 2 for COM2 serial port...' 
                db 0Dh,0Ah,0h
ComSMsg:
		db 07h
		db 'COM1 selected...'
		db  0Dh, 0Ah, 0
AnyKeyMsg:
		db 'Press a key to continue.'
                db 0Dh,0Ah,0h

console: 	db 0 ; 27/06/2014	
chr:		db 0 ; 11/06/2014
port:		db 0
		;db 0


 
CODE_SEG	ends

		end	start
