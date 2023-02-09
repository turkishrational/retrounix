; UINSTALL.ASM
; --------------------------------------------------------------
; RETRO UNIX v0.1 'fd0' formatting procedures
; Last Update: 04/04/2022
; (new /dev directory format 
; according to Retro UNIX 8086 v1 kernel)
; 21/04/2014 (tty8, tty9)
; 09/07/2013 
; 05/03/2013 (ALIGN) 
; 31/10/2012, 16/12/2012 (unixproc.asm -> sioreg) 
; ERDOGAN TAN [ 14-15-16-21-27/7/2012, 4-5-12-13-14-15-21/8/2012 ]
; These procedures will be located in UNIXFDFS.ASM file 
; when they are completed.
; (NOTE: only for (R)UFS initialization of FD0 1.44MB floppy disk

; 04/04/2022 (Inode Table/List Start Address Modification) (3->2)
; 03/04/2022 (Inode Table/List Size BugFix)

SIZE_FREE_MAP equ 360
SIZE_INODE_MAP equ 32
DISK_SIZE equ 2880 ; in blocks
;INODE_COUNT equ SIZE_INODE_MAP * 8
; 03/04/2022
INODE_COUNT equ (SIZE_INODE_MAP * 8) + 40
;INODE_LIST_BLOCKS equ (INODE_COUNT) / 16
; 03/04/2022
INODE_LIST_BLOCKS equ ((INODE_COUNT+15) / 16) ; 19 blocks

ROOT_DIR_INODE equ 41

SIZE_Reserved1 equ 512 - (2+SIZE_FREE_MAP+2+SIZE_INODE_MAP) 

SuperBlock struc

sb_FreeMapSize 	dw ?
sb_FreeMap	db SIZE_FREE_MAP dup(?)
sb_InodeMapSize dw ?
sb_InodeMap	db SIZE_INODE_MAP dup(?)
sb_Reserved1	db SIZE_Reserved1 dup(?)
sb_Reserved2	db 512 dup(?)

SuperBlock ends

; UNIX v1 I-node Flags: 
; 1000000000000000b 	i-node is allocated (8000h)
; 0100000000000000b	directory (4000h)
; 0010000000000000b	file has been modified (2000h)		 	
; 0001000000000000b	large file (1000h)
; 0000000000100000b	set user id on execution (20h)
; 0000000000010000b	executable (10h)
; 0000000000001000b	read, owner (8)
; 0000000000000100b	write, owner (4)
; 0000000000000010b	read, non-owner (2)
; 0000000000000001b	write, non-owner (1)

unix_fs_install proc near
	; 8086 code by Erdogan Tan
	; 04/04/2022
	; 03/04/2022
	; 31/10/2012
	; 21/08/2012
	; 15/08/2012
	; 14/08/2012
	; 13/08/2012
	; 05/08/2012
	; 04/08/2012
	; Derived from (original) UNIX v1 source code
	; PRELIMINARY release of Unix Implementation Document, 
	; 20/6/1972
	; RETRO UNIX v1 FS
	; initialization/format version
	; NOTE: 
	; The "cold" unix (u0, PDP-11) code is modified for fd0 
	;  -> 1.44 MB floppy disk (Retro UNIX v1, 8086) fs

	mov byte ptr [buff_d], dl ; 14/8/2012, drive number

	mov word ptr [systm.sb_FreemapSize], SIZE_FREE_MAP ; 360
	mov word ptr [systm.sb_InodeMapSize], SIZE_INODE_MAP ; 32
	mov ax, DISK_SIZE ; 2880 blocks/sectors
uinstall_1:
;set bit AX/R1 in free storage map in core/memory
	dec ax ; R1
	call free

	;;cmp ax, INODE_LIST_BLOCKS + 4 ; 15/8/2012
        ; 03/04/2022
	;cmp ax, INODE_LIST_BLOCKS + 3
	; 04/04/2022
	cmp ax, INODE_LIST_BLOCKS + 2
	ja short uinstall_1
uinstall_2:
; zero i-list	
	dec ax
	; AX (R1) = Block number

	call clear 	
        jc short uinstall_10 ; rw_error

	and ax, ax
	jnz short uinstall_2

uinstall_3:
	; initialize inodes for special files (1 to 40)
	mov bx, 40 ; BX = R1, 41 = root directory i-number
uinstall_4:
	call iget
        jc short uinstall_10 ; rw_error

	mov word ptr [i_flgs], 800Fh ;  1000000000001111b
        mov byte ptr [i_nlks], 1
        mov byte ptr [i_uid], 0
	call setimod
	dec bx
	jnz short uinstall_4

uinstall_5:
	;push di
	;push si
	mov si, offset idata ; base address of assembled dirs
	mov di, offset dirs  ; directory data for assembled dirs
	mov bx, 41
uinstall_6:
	call imap
	xchg bx,dx ; 13/8/2012
	; 21/8/2012 (AX -> AL, word ptr [BX] -> byte ptr [BX])
	or byte ptr [BX], al ; BX/DX = R2, ax = mq
			     ; set the bit to indicate the i-node
			     ; is not available/free
	xchg bx, dx ; 13/8/2012
	call iget
	;jnc short uinstall_7
        jc short uinstall_10  ; rw_error
@@:
	;pop si
	;pop di
        ;jmp short uinstall_10 ; rw_error

uinstall_7:
	; SI, DI registers are not modified 
	; in imap, iget, setimod and writei procedures
	lodsw
        mov word ptr [i_flgs], ax
	lodsb
        mov byte ptr [i_nlks], al
	lodsb
        mov byte ptr [i_uid], al
	call setimod
	lodsw
        mov word ptr [u_count], ax
	
	add si, 26 ; now, si points 1st word of next inode

        mov word ptr [u_base], di
	add di, ax

	mov word ptr [u_fofp], offset u_off ; 31/10/2012

        mov word ptr [u_off], 0
	
	call writei
	;jc short @b
        jc short uinstall_10 ; rw_error

	cmp bx, 46
	jnb short uinstall_8

	inc bx
	jmp short uinstall_6

uinstall_8:	
	;pop si
	;pop di

uinstall_9:
	call sync ; write modified super block and buffer to disk
	;jc short rw_error

uinstall_10:
	retn

unix_fs_install endp

sync	proc near
	; 12/08/2012
	; updates super block and the last i-node on disk 
	; if modified
	; e.g. smod = 1, imod = 1, buffer_m = 1
	;
	; RETRO UNIX v1 FS
	; initialization/format version

	xor bx, bx ; mov bx, 0
        call iget
	jc short sync_2

	xor ax, ax
	cmp byte ptr [smod], al ; 0
	jna short sync_3
sync_1:
	mov byte ptr [smod], al ; 0

	mov cx, 256
	mov si, offset Systm
	mov di, offset Buffer
	rep movsw

	inc al

        mov word ptr [buff_s], ax ; 1 ; superblock sector number
	mov byte ptr [buff_w], al

	call poke
sync_2:
	mov ax, word ptr [Error]
sync_3:
	retn	

sync	endp

align 2

buff_d: db 0
buff_s: dw 0FFFFh ; Buffer sector
buff_m:	db 0 ; buffer daha changed/modified (dirty) flag
buff_w: db 0 ; read/write flag (write=1, read=0)

align 16

systm: ; superblock
db 512 dup(0)

; 05/08/2012
; 14/07/2012
dirs:
root_dir: ; root directory
		dw 41
		db "..", 0,0,0,0,0,0
		dw 41
		db ".", 0,0,0,0,0,0,0
		dw 42
		db "dev",0,0,0,0,0
		dw 43
		db "bin",0,0,0,0,0
		dw 44
		db "etc",0,0,0,0,0
		dw 45
		db "usr",0,0,0,0,0
		dw 46
		db "tmp",0,0,0,0,0

size_root_dir equ $ - offset root_dir

dev_dir: ; device directory
		dw 41
		db "..", 0,0,0,0,0,0
		dw 42
		db ".", 0,0,0,0,0,0,0
		dw 1
		db "tty",0,0,0,0,0
		dw 2
		db "mem",0,0,0,0,0
		dw 3
		db "fd0",0,0,0,0,0
		dw 4
		db "fd1",0,0,0,0,0
		dw 5
		db "hd0",0,0,0,0,0
		dw 6
		db "hd1",0,0,0,0,0
		dw 7
		db "hd2",0,0,0,0,0
		dw 8
		db "hd3",0,0,0,0,0
		dw 9
		db "lpr",0,0,0,0,0
		dw 10
                db "tty0",0,0,0,0
		dw 11
                db "tty1",0,0,0,0
		dw 12
                db "tty2",0,0,0,0
		dw 13
                db "tty3",0,0,0,0
		dw 14
                db "tty4",0,0,0,0
		dw 15
                db "tty5",0,0,0,0
		dw 16
                db "tty6",0,0,0,0
		dw 17
                db "tty7",0,0,0,0
		dw 18
                db "COM1",0,0,0,0 ; 09/07/2013
		dw 19
                db "COM2",0,0,0,0 ; 09/07/2013
		dw 18
                db "tty8",0,0,0,0 ; 21/04/2014
		dw 19
                db "tty9",0,0,0,0 ; 21/04/2014  

size_dev_dir equ $ - offset dev_dir

bin_dir:  ; binary directory
		dw 41
		db "..", 0,0,0,0,0,0
		dw 43
		db ".", 0,0,0,0,0,0,0
		
size_bin_dir equ $ - offset bin_dir

etc_dir:  ; etcetra directory
		dw 41
		db "..", 0,0,0,0,0,0
		dw 44
		db ".", 0,0,0,0,0,0,0
		
size_etc_dir equ $ - offset etc_dir

usr_dir:  ; user directory
		dw 41
		db "..", 0,0,0,0,0,0
		dw 45
		db ".", 0,0,0,0,0,0,0
		
size_usr_dir equ $ - offset usr_dir

tmp_dir:  ; temporary directory
		dw 41
		db "..", 0,0,0,0,0,0
		dw 46
		db ".", 0,0,0,0,0,0,0
		
size_tmp_dir equ $ - offset tmp_dir

align 2

;dw 0

; 31/10/2012
u_off: dw 0

; 12/08/2012
u_count: dw 0
u_base: dw 0
u_fofp: dw 0
u_nread: dw 0

; 17/08/2012
; 05/08/2012
; 14/07/2012
inode:
i_flgs: dw 800Fh ; special (device) files flags
i_nlks: db 1 ; Number of links
i_uid: db 0  ; user id 
i_size: dw 0 ; file size
i_dskp: dw 8 dup(0)  ; direct or indirect blocks
i_ctim: dd 0 ; creation time
i_mtim: dd 0 ; last modification time 
i_reserved: dw 0 ; reserved (not in use)

; 05/08/2012
; 14/07/2012
idata:
inodes:

root_inode: ; 41
		dw 0C00Eh ; Flags (1100000000001110b)
		db 7	; number of links 
		db 0	; user ID (0 = root)
		dw size_root_dir ; initial size = 70 bytes 
		dw 8 dup (0) ; indirect or contents blocks
		dd 0	; creation date & time
		dd 0	; modification date & time
		dw 0	; unused
dev_inode: ; 42
		dw 0C00Eh ; Flags (1100000000001110b)
		db 2	; number of links 
		db 0	; user ID (0 = root)
		dw size_dev_dir ; 200
		dw 8 dup (0) ; indirect or contents blocks
		dd 0	; creation date & time
		dd 0	; modification date & time
		dw 0	; unused	
bin_inode: ; 43
		dw 0C00Eh ; Flags (1100000000001110b)
		db 2	; number of links 
		db 0	; user ID (0 = root)
		dw size_bin_dir ; 20
		dw 8 dup (0) ; indirect or contents blocks
		dd 0	; creation date & time
		dd 0	; modification date & time
		dw 0	; unused
etc_inode: ; 44
		dw 0C00Eh ; Flags (1100000000001110b)
		db 2	; number of links 
		db 0	; user ID (0 = root)
		dw size_etc_dir ; 20
		dw 8 dup (0) ; indirect or contents blocks
		dd 0	; creation date & time
		dd 0	; modification date & time
		dw 0	; unused				
usr_inode: ; 45
		dw 0C00Eh ; Flags (1100000000001110b)
		db 2	; number of links 
		db 0	; user ID (0 = root)
		dw size_usr_dir ; 20
		dw 8 dup (0) ; indirect or contents blocks
		dd 0	; creation date & time
		dd 0	; modification date & time
		dw 0	; unused
tmp_inode: ; 46
		dw 0C00Fh ; Flags (1100000000001111b)
		db 2	; number of links 
		db 0	; user ID (0 = root)
		dw size_tmp_dir ; 20
		dw 8 dup (0) ; indirect or contents blocks
		dd 0	; creation date & time
		dd 0	; modification date & time
		dw 0	; unused

align 16

Buffer:
sector_buffer:
db 512 dup (0)