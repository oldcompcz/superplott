; *****************************************************************************
; * Superplott for XY4150 and ZX Spectrum
; *
; * how to build: (use z80asm from z88dk project)
; * $ z80asm -b superplott.asm
; * $ appmake +zx -b superplott.bin --org 60500 -o superplott.tap
; *
; * verification against original binary:
; * $ diff -y <(xxd superplott.bin) <(xxd ../bin/superplott.bin)
; *
; * disassembled: 
; * $ z80dasm -a -t -l -g 60500 -b blocks.txt superplott.bin
; *****************************************************************************

MAX_X   equ 009c4h              ;2500
MAX_Y   equ 006d6h              ;1750

        org 0ec54h

        jp INIT                 ;ec54
        jp PUTX                 ;ec57
        jp PUTY                 ;ec5a
        jp ENTRY                ;ec5d
        jp WRITE                ;ec60
        jp LIST                 ;ec63

WHE_X:
        defw D006_whe_x         ;ec66

; *****************************************************************************
; * Move Free - PRINT #4; "mf"
; *
; * Reads cursor (5,6,7,8), 0 = quit, q = run fast
; *****************************************************************************

MV_FREE:
        ld a,028h               ;ec68   load 40 into a
        ld (0ecf4h),a           ;ec6a   modify of ld bc,0XX01h at ecf2
                                ;       waiting interval?

; Reading keyboard:
; -----------------------------------------
; port / bit ->              b0 b1 b2 b3 b4
; -----------------------------------------
; 7FFE - 01111111 11111110 - sp ss  M  N  B
; BFFE - 10111111 11111110 - en  L  K  J  H
; DFFE - 11011111 11111110 -  P  O  I  U  Y
; EFFE - 11101111 11111110 -  0  9  8  7  6
; F7FE - 11110111 11111110 -  1  2  3  4  5
; FBFE - 11111011 11111110 -  Q  W  E  R  T
; FDFE - 11111101 11111110 -  A  S  D  F  G
; FEFE - 11111110 11111110 - sh  Z  X  C  V
; -----------------------------------------

        ld hl,D001_keyb_snap    ;ec6d   load v[0] into hl 
        ld bc,07ffeh            ;ec70   load bc with the keyboard row port 
read_keys:
        in a,(c)                ;ec73   read value from keyboard port 
        cpl                     ;ec75   complement - invert all bits in a
        and 01fh                ;ec76   mask out keys 0001 1111 
        ld (hl),a               ;ec78   store v[0] 
        inc hl                  ;ec79   inc of base address 
        rrc b                   ;ec7a   calc next row port addr 
        jr c,read_keys          ;ec7c   8-times loop (b = 0111 1111) 
        push ix                 ;ec7e 
        ld ix,D001_keyb_snap    ;ec80 
        bit 0,(ix+005h)         ;ec84   test of key Q (quickly) 
        call nz,mf_quickly      ;ec88 
        bit 3,(ix+003h)         ;ec8b   test of key 7 (up)
        call nz,mf_y_plus       ;ec8f   move y+ 
        bit 4,(ix+003h)         ;ec92   test of key 6 (down)
        call nz,mf_y_minus      ;ec96   move y-
        bit 2,(ix+003h)         ;ec99   test of key 8 (right)
        call nz,mf_x_plus       ;ec9d   move x+
        bit 4,(ix+004h)         ;eca0   test of key 5 (left)
        call nz,mf_x_minus      ;eca4   move x-
        bit 0,(ix+003h)         ;eca7   test of key 0 (quit)
        jr nz,mf_quit           ;ecab

; Convert coordinates to decimal 
        ld ix,D002              ;ecad   addr for x in decimal 
        ld hl,(D004_x_pos)      ;ecb1   load x pos 
        call mf_cnv_to_decimal  ;ecb4   convert to decimal 
        inc ix                  ;ecb7   addr for y in decimal
        ld hl,(D003_y_pos)      ;ecb9   load y pos 
        call mf_cnv_to_decimal  ;ecbc   convert to decimal 
        ld ix,D002              ;ecbf 
        ld (ix-001h),0ffh       ;ecc3

; Print coordinates at screen 
lecc7h: 
        ld a,(ix+000h)          ;ecc7   decimal digit into a 
        inc ix                  ;ecca   increase base address 
        cp 00bh                 ;eccc  
        jr nc,mf_wait           ;ecce   loop if greater than 11   
        add a,a                 ;ecd0   a = a * 8 (calc index of digit char) 
        add a,a                 ;ecd1 
        add a,a                 ;ecd2 
        ld c,a                  ;ecd3 
        ld b,000h               ;ecd4 
        ld hl,03d80h            ;ecd6   base address 
        add hl,bc               ;ecd9   add char index 
        ld a,(D005)             ;ecda   load and increase char index at screen 
        inc a                   ;ecdd 
        ld (D005),a             ;ecde 
        ld de,050e0h            ;ece1   video ram (4000-57FF) position 
        add a,e                 ;ece4   add char position 
        ld e,a                  ;ece5 
        ld b,008h               ;ece6   char is composed by 8 bytes
lece8h:
        ld a,(hl)               ;ece8   load char byte 
        ld (de),a               ;ece9   put it into video ram 
        inc hl                  ;ecea   next char byte 
        inc d                   ;eceb   calc video ram position 
        djnz lece8h             ;ecec   loop until b = 0 
        jr lecc7h               ;ecee   next char

; Wait and loop
mf_wait:
        pop ix                  ;ecf0 
        ld bc,00001h            ;ecf2   self modified by ec6a
lecf5h:
        dec bc                  ;ecf5 
        ld a,c                  ;ecf6 
        or b                    ;ecf7 
        jr nz,lecf5h            ;ecf8 
        jp MV_FREE              ;ecfa

; *****************************************************************************
; * Move Free - main loop end
; *****************************************************************************

; Convert hl to decimal and write result to (ix)
mf_cnv_to_decimal:
        ld de,003e8h            ;ecfd   de <- 1000 
        call sub_ed16h          ;ed00 
        ld de,00064h            ;ed03   de <- 100 
        call sub_ed16h          ;ed06 
        ld de,0000ah            ;ed09   de <- 10 
        call sub_ed16h          ;ed0c 
        ld de,00001h            ;ed0f   de <- 1 
        call sub_ed16h          ;ed12 
        ret                     ;ed15 
sub_ed16h:
        ld (ix+000h),000h       ;ed16 
led1ah:
        or a                    ;ed1a   division by de
        sbc hl,de               ;ed1b 
        jp m,led25h             ;ed1d 
        inc (ix+000h)           ;ed20 
        jr led1ah               ;ed23 
led25h:
        add hl,de               ;ed25 
        inc ix                  ;ed26 
        ret                     ;ed28 

; Move free: quit
mf_quit:
        pop ix                  ;ed29
        ld hl,D004_x_pos        ;ed2b
        ld de,D006_whe_x        ;ed2e   copy 4 bytes hl -> de
        push de                 ;ed31
        call sub_f470h          ;ed32
        pop hl                  ;ed35   copy it again just after de
        jp sub_f470h            ;ed36   copy 4 bytes hl -> de
                                ;       sub_f470h will return from MF !!!

; Move free: quickly
mf_quickly:
        xor a                   ;ed39   a = 0
        ld (0ecf4h),a           ;ed3a   modify of ld bc,0XX01h at ecf2 
        ret                     ;ed3d

; Move free: move y
mf_y_plus:
        out (0d7h),a            ;ed3e   move y 
        ld hl,(D003_y_pos)      ;ed40 
        inc hl                  ;ed43 
        ld (D003_y_pos),hl      ;ed44 
        ld de,MAX_Y             ;ed47	load MAX_Y = 1750 
        or a                    ;ed4a 
        sbc hl,de               ;ed4b 
        ret m                   ;ed4d 
mf_y_minus:
        out (0dfh),a            ;ed4e 
        ld hl,(D003_y_pos)      ;ed50 
        dec hl                  ;ed53 
        ld (D003_y_pos),hl      ;ed54 
        bit 7,h                 ;ed57 
        ret z                   ;ed59 
        jr mf_y_plus            ;ed5a
        
; Move free: move x
mf_x_plus:
        out (0c7h),a            ;ed5c   move x 
        ld hl,(D004_x_pos)      ;ed5e 
        inc hl                  ;ed61 
        ld (D004_x_pos),hl      ;ed62 
        ld de,MAX_X             ;ed65   load MAX_X = 2500 
        or a                    ;ed68 
        sbc hl,de               ;ed69 
        ret m                   ;ed6b 
mf_x_minus:
        out (0cfh),a            ;ed6c 
        ld hl,(D004_x_pos)      ;ed6e 
        dec hl                  ;ed71 
        ld (D004_x_pos),hl      ;ed72 
        bit 7,h                 ;ed75 
        ret z                   ;ed77 
        jr mf_x_plus            ;ed78

; *****************************************************************************
; * PUTX
; * Transfer of variable containing pen X position   
; * Through top of stack in floating point form
; *****************************************************************************
 
PUTX:
        ld hl,D008              ;ed7a
        ld (05c68h),hl          ;ed7d   Address of area used for calculator's memory. 
        ld de,(D006_whe_x)      ;ed80
        call sub_ef98h          ;ed84
        rst 28h                 ;ed87

        defb 0ech               ;ed88   get |m|
        defb 003h               ;ed89   subtract |f|-|m|
        defb 0ebh               ;ed8a   get |f|
        defb 005h               ;ed8b   divide f/m
        defb 038h               ;ed8c   end-calc
led8dh:
        pop hl                  ;ed8d
        ld hl,05c92h            ;ed8e
        ld (05c68h),hl          ;ed91
        or a                    ;ed94
        ret                     ;ed95

; *****************************************************************************
; * PUTY
; * Transfer of variable containing pen Y position   
; * Through top of stack in floating point form
; *****************************************************************************
 
PUTY:
        ld hl,D008              ;ed96
        ld (05c68h),hl          ;ed99   Address of area used for calculator's memory.
        ld de,(D007_whe_y)      ;ed9c
        call sub_ef98h          ;eda0
        rst 28h                 ;eda3

        defb 0edh               ;eda4   get COS 8 (???)
        defb 003h               ;eda5   subtract |f|-|m|
        defb 0ebh               ;eda6   get |f|
        defb 005h               ;eda7   divide f/m
        defb 038h               ;eda8   end-calc

        jr led8dh               ;eda9

D005:   defb 000h               ;edab   char index at screen

D002:   defb 000h               ;edac   y in decimal
        defb 000h               ;edad
        defb 000h               ;edae
        defb 000h               ;edaf
        defb 00ah               ;edb0
        defb 000h               ;edb1   x in decimal
        defb 000h               ;edb2
        defb 000h               ;edb3
        defb 000h               ;edb4
        defb 00bh               ;edb5

D001_keyb_snap:                 ;             b0 b1 b2 b3 b4
        defb 0e2h               ;edb6   keys: sp ss  M  N  B
        defb 000h               ;edb7   keys: en  L  K  J  H
        defb 000h               ;edb8   keys:  P  O  I  U  Y
        defb 000h               ;edb9   keys:  0  9  8  7  6
        defb 000h               ;edba   keys:  1  2  3  4  5
        defb 000h               ;edbb   keys:  Q  W  E  R  T
        defb 00ah               ;edbc   keys:  A  S  D  F  G
        defb 000h               ;edbd   keys: sh  Z  X  C  V
        defb 000h               ;edbe 
	nop			;edbf	00 	. 
	nop			;edc0	00 	. 
	dec bc			;edc1	0b 	. 
ledc2h:
	nop			;edc2	00 	. 
	nop			;edc3	00 	. 
	nop			;edc4	00 	. 
	nop			;edc5	00 	. 
	nop			;edc6	00 	. 
	nop			;edc7	00 	. 
	nop			;edc8	00 	. 
	nop			;edc9	00 	. 
	ld hl,lee1dh		;edca	21 1d ee 	! . . 
	ld de,05c1eh		;edcd	11 1e 5c 	. . \ 
	ld bc,00006h		;edd0	01 06 00 	. . . 
	ldir		;edd3	ed b0 	. . 
	ld a,014h		;edd5	3e 14 	> . 
	out (05dh),a		;edd7	d3 5d 	. ] 
	in a,(05dh)		;edd9	db 5d 	. ] 
	bit 5,a		;eddb	cb 6f 	. o 
	ret z			;eddd	c8 	. 
	ld hl,007d0h		;edde	21 d0 07 	! . . 
lede1h:
	ld a,004h		;ede1	3e 04 	> . 
	out (05dh),a		;ede3	d3 5d 	. ] 
	ld a,010h		;ede5	3e 10 	> . 
	call sub_ee16h		;ede7	cd 16 ee 	. . . 
ledeah:
	ex (sp),hl			;edea	e3 	. 
	ex (sp),hl			;edeb	e3 	. 
	djnz ledeah		;edec	10 fc 	. . 
	in a,(05dh)		;edee	db 5d 	. ] 
	bit 5,a		;edf0	cb 6f 	. o 
	jr z,ledfah		;edf2	28 06 	( . 
	dec hl			;edf4	2b 	+ 
	ld a,h			;edf5	7c 	| 
	or l			;edf6	b5 	. 
	jr nz,lede1h		;edf7	20 e8 	  . 
	ret			;edf9	c9 	. 
ledfah:
	ld c,019h		;edfa	0e 19 	. . 
	xor a			;edfc	af 	. 
	call sub_f669h		;edfd	cd 69 f6 	. i . 
lee00h:
	ld a,011h		;ee00	3e 11 	> . 
	call sub_ee16h		;ee02	cd 16 ee 	. . . 
lee05h:
	ex (sp),hl			;ee05	e3 	. 
	ex (sp),hl			;ee06	e3 	. 
	djnz lee05h		;ee07	10 fc 	. . 
	dec c			;ee09	0d 	. 
	jr nz,lee00h		;ee0a	20 f4 	  . 
	ld hl,00000h		;ee0c	21 00 00 	! . . 
	ld (lfa5ch),hl		;ee0f	22 5c fa 	" \ . 
	ld (lfa5ch+2),hl		;ee12	22 5e fa 	" ^ . 
	ret			;ee15	c9 	. 
sub_ee16h:
	out (05dh),a		;ee16	d3 5d 	. ] 
	or 004h		;ee18	f6 04 	. . 
	out (05dh),a		;ee1a	d3 5d 	. ] 
	ret			;ee1c	c9 	. 
lee1dh:
	xor c			;ee1d	a9 	. 
	adc a,a			;ee1e	8f 	. 
	xor h			;ee1f	ac 	. 
	adc a,a			;ee20	8f 	. 
	xor a			;ee21	af 	. 
	adc a,a			;ee22	8f 	. 
	call 0fa01h		;ee23	cd 01 fa 	. . . 
	cp 006h		;ee26	fe 06 	. . 
	jr nz,lee2ch		;ee28	20 02 	  . 
	ld a,02ch		;ee2a	3e 2c 	> , 
lee2ch:
	ld hl,(D008)		;ee2c	2a c6 fa 	* . . 
	ld (hl),a			;ee2f	77 	w 
	inc hl			;ee30	23 	# 
	ld (D008),hl		;ee31	22 c6 fa 	" . . 
	ex af,af'			;ee34	08 	. 
	ld a,h			;ee35	7c 	| 
	cp 05ch		;ee36	fe 5c 	. \ 
	jr c,lee40h		;ee38	38 06 	8 . 
	dec h			;ee3a	25 	% 
	ld (D008),hl		;ee3b	22 c6 fa 	" . . 
	rst 8			;ee3e	cf 	. 
	rrca			;ee3f	0f 	. 
lee40h:
	ex af,af'			;ee40	08 	. 
	cp 00dh		;ee41	fe 0d 	. . 
	jp nz,leefeh		;ee43	c2 fe ee 	. . . 
	defb 0ddh		;ee46	dd 	. 
	defb 021h		;ee47	21 	!

; *****************************************************************************
; * INIT
; * Superplott Initialization
; *
; *****************************************************************************
 
INIT:
        ld hl,STREAMS           ;ee48	21 94 ee 	! . . 
        ld de,05c1eh            ;ee4b   Addresses of channels attached to streams 
        ld bc,00006h            ;ee4e	01 06 00 	. . . 
        ldir                    ;ee51	ed b0 	. . 
        out (0fbh),a            ;ee53	d3 fb 	. . 
        out (0bfh),a            ;ee55	d3 bf 	. . 
lee57h:
        call sub_ee86h          ;ee57	cd 86 ee 	. . . 
        ret c                   ;ee5a	d8 	. 
        ld hl,007d0h            ;ee5b	21 d0 07 	! . . 
lee5eh:
        out (0dfh),a            ;ee5e	d3 df 	. . 
lee60h:
        djnz lee60h             ;ee60	10 fe 	. . 
        call sub_ee86h          ;ee62	cd 86 ee 	. . . 
        jr c,lee6dh             ;ee65	38 06 	8 . 
        dec hl                  ;ee67	2b 	+ 
        ld a,h                  ;ee68	7c 	| 
        or l                    ;ee69	b5 	. 
        jr nz,lee5eh            ;ee6a	20 f2 	  . 
        ret                     ;ee6c	c9 	. 
lee6dh:
	ld c,00fh		;ee6d	0e 0f 	. . 
	xor a			;ee6f	af 	. 
	call sub_f6d5h		;ee70	cd d5 f6 	. . . 
lee73h:
	out (0d7h),a		;ee73	d3 d7 	. . 
lee75h:
	ex (sp),hl			;ee75	e3 	. 
	ex (sp),hl			;ee76	e3 	. 
	djnz lee75h		;ee77	10 fc 	. . 
	dec c			;ee79	0d 	. 
	jr nz,lee73h		;ee7a	20 f7 	  . 
	ld hl,00000h		;ee7c	21 00 00 	! . . 
	ld (D004_x_pos),hl		;ee7f	22 c2 fa 	" . . 
	ld (D003_y_pos),hl		;ee82	22 c4 fa 	" . . 
	ret			;ee85	c9 	. 
sub_ee86h:
	ld bc,00000h		;ee86	01 00 00 	. . . 
lee89h:
	in a,(0bfh)		;ee89	db bf 	. . 
	or c			;ee8b	b1 	. 
	ld c,a			;ee8c	4f 	O 
	out (0bfh),a		;ee8d	d3 bf 	. . 
	djnz lee89h		;ee8f	10 f8 	. . 
	ld a,c			;ee91	79 	y 
	rlca			;ee92	07 	. 
	ret			;ee93	c9 	.
         
; BLOCK 'STREAMS' (start 0xee94 end 0xee9a)
STREAMS:
	defw 08fa9h		;ee94	a9 8f 	. . 
	defw 08fach		;ee96	ac 8f 	. . 
	defw 08fafh		;ee98	af 8f 	. . 
 
; *****************************************************************************
; * ENTRY
; * Entry of graphical commands in form of strings 
; * Through #4 - char is in a register
; *****************************************************************************
 
ENTRY:
	call sub_fa67h		;ee9a	cd 67 fa 	. g . 
	cp 006h		;ee9d	fe 06 	. . 
	jr nz,leea3h		;ee9f	20 02 	  . 
	ld a,02ch		;eea1	3e 2c 	> , 
leea3h:
	ld hl,(lfb2bh+1)		;eea3	2a 2c fb 	* , . 
	ld (hl),a			;eea6	77 	w 
	inc hl			;eea7	23 	# 
	ld (lfb2bh+1),hl		;eea8	22 2c fb 	" , . 
	ex af,af'			;eeab	08 	. 
	ld a,h			;eeac	7c 	| 
	cp 05ch		;eead	fe 5c 	. \ 
	jr c,leeb7h		;eeaf	38 06 	8 . 
	dec h			;eeb1	25 	% 
	ld (lfb2bh+1),hl		;eeb2	22 2c fb 	" , . 
	rst 8			;eeb5	cf 	. 
	rrca			;eeb6	0f 	. 
leeb7h:
	ex af,af'			;eeb7	08 	. 
	cp 00dh		;eeb8	fe 0d 	. . 
	jp nz,lef75h		;eeba	c2 75 ef 	. u . 
	ld ix,lfa76h		;eebd	dd 21 76 fa 	. ! v . 
	ld (ix+000h),000h		;eec1	dd 36 00 00 	. 6 . . 
	ld hl,(05c5dh)		;eec5	2a 5d 5c 	* ] \ 
	push hl			;eec8	e5 	. 
	ld hl,05b00h		;eec9	21 00 5b 	! . [ 
	ld (05c5dh),hl		;eecc	22 5d 5c 	" ] \ 
	ld (lfb2bh+1),hl		;eecf	22 2c fb 	" , . 
	ld hl,D008		;eed2	21 c6 fa 	! . . 
	ld (05c68h),hl		;eed5	22 68 5c 	" h \ 
	xor a			;eed8	af 	. 
	call sub_f721h		;eed9	cd 21 f7 	. ! . 
leedch:
	call sub_f811h		;eedc	cd 11 f8 	. . . 
	xor a			;eedf	af 	. 
	ld (lef36h),a		;eee0	32 36 ef 	2 6 . 
	call sub_ef0eh		;eee3	cd 0e ef 	. . . 
	cp 00dh		;eee6	fe 0d 	. . 
	jr z,lef61h		;eee8	28 77 	( w 
	cp 03ah		;eeea	fe 3a 	. : 
	jr z,leedch		;eeec	28 ee 	( . 
	cp 03bh		;eeee	fe 3b 	. ; 
	jr z,leedch		;eef0	28 ea 	( . 
	ld d,a			;eef2	57 	W 
	call sub_ef0eh		;eef3	cd 0e ef 	. . . 
	ld e,a			;eef6	5f 	_ 
	ld hl,lfb7ch		;eef7	21 7c fb 	! | . 
leefah:
	ld a,(hl)			;eefa	7e 	~ 
	or a			;eefb	b7 	. 
	jr nz,$+4		;eefc	20 02 	  . 
leefeh:
	rst 8			;eefe	cf 	. 
	ld d,023h		;eeff	16 23 	. # 
	cp d			;ef01	ba 	. 
	jr nz,lef08h		;ef02	20 04 	  . 
	ld a,(hl)			;ef04	7e 	~ 
	cp e			;ef05	bb 	. 
	jr z,lef1ch		;ef06	28 14 	( . 
lef08h:
	inc hl			;ef08	23 	# 
	inc hl			;ef09	23 	# 
	inc hl			;ef0a	23 	# 
	inc hl			;ef0b	23 	# 
	jr leefah		;ef0c	18 ec 	. . 
sub_ef0eh:
	rst 18h			;ef0e	df 	. 
	inc hl			;ef0f	23 	# 
	ld (05c5dh),hl		;ef10	22 5d 5c 	" ] \ 
	cp 061h		;ef13	fe 61 	. a 
	ret c			;ef15	d8 	. 
	cp 07bh		;ef16	fe 7b 	. { 
	ret nc			;ef18	d0 	. 
	sub 020h		;ef19	d6 20 	.   
	ret			;ef1b	c9 	. 
lef1ch:
	inc hl			;ef1c	23 	# 
	ld b,(hl)			;ef1d	46 	F 
	inc hl			;ef1e	23 	# 
	ld e,(hl)			;ef1f	5e 	^ 
	inc hl			;ef20	23 	# 
	ld d,(hl)			;ef21	56 	V 
	push de			;ef22	d5 	. 
	ld a,b			;ef23	78 	x 
	or a			;ef24	b7 	. 
	jr z,lef30h		;ef25	28 09 	( . 
lef27h:
	push bc			;ef27	c5 	. 
	call sub_ef35h		;ef28	cd 35 ef 	. 5 . 
	jr z,lef82h		;ef2b	28 55 	( U 
	pop bc			;ef2d	c1 	. 
	djnz lef27h		;ef2e	10 f7 	. . 
lef30h:
	ld hl,leedch		;ef30	21 dc ee 	! . . 
	ex (sp),hl			;ef33	e3 	. 
	jp (hl)			;ef34	e9 	. 
sub_ef35h:
	xor a			;ef35	af 	. 
lef36h:
	nop			;ef36	00 	. 
	call sub_ef42h		;ef37	cd 42 ef 	. B . 
	ret nz			;ef3a	c0 	. 
	ld a,0c9h		;ef3b	3e c9 	> . 
	ld (lef36h),a		;ef3d	32 36 ef 	2 6 . 
	ret			;ef40	c9 	. 
lef41h:
	rst 20h			;ef41	e7 	. 
sub_ef42h:
	rst 18h			;ef42	df 	. 
	cp 00dh		;ef43	fe 0d 	. . 
	ret z			;ef45	c8 	. 
	cp 03ah		;ef46	fe 3a 	. : 
	ret z			;ef48	c8 	. 
	cp 03bh		;ef49	fe 3b 	. ; 
	ret z			;ef4b	c8 	. 
	cp 02ch		;ef4c	fe 2c 	. , 
	jr z,lef41h		;ef4e	28 f1 	( . 
	cp 02dh		;ef50	fe 2d 	. - 
	push af			;ef52	f5 	. 
	call z,00020h		;ef53	cc 20 00 	.   . 
	rst 18h			;ef56	df 	. 
	call 02c9bh		;ef57	cd 9b 2c 	. . , 
	pop af			;ef5a	f1 	. 
	ret nz			;ef5b	c0 	. 
	rst 28h			;ef5c	ef 	. 
	dec de			;ef5d	1b 	. 
	jr c,$+22		;ef5e	38 14 	8 . 
	ret			;ef60	c9 	. 
lef61h:
	pop hl			;ef61	e1 	. 
	ld (05c5dh),hl		;ef62	22 5d 5c 	" ] \ 
	ld hl,05c92h		;ef65	21 92 5c 	! . \ 
	ld (05c68h),hl		;ef68	22 68 5c 	" h \ 
	ld a,(ix+000h)		;ef6b	dd 7e 00 	. ~ . 
	or a			;ef6e	b7 	. 
	jp nz,lf24fh		;ef6f	c2 4f f2 	. O . 
	call 016c5h		;ef72	cd c5 16 	. . . 
lef75h:
	pop ix		;ef75	dd e1 	. . 
	pop af			;ef77	f1 	. 
	pop bc			;ef78	c1 	. 
	pop de			;ef79	d1 	. 
	pop hl			;ef7a	e1 	. 
	exx			;ef7b	d9 	. 
	ex af,af'			;ef7c	08 	. 
	pop af			;ef7d	f1 	. 
	pop bc			;ef7e	c1 	. 
	pop de			;ef7f	d1 	. 
	pop hl			;ef80	e1 	. 
	ret			;ef81	c9 	. 
lef82h:
	rst 8			;ef82	cf 	. 
	add hl,de			;ef83	19 	. 
sub_ef84h:
	rst 28h			;ef84	ef 	. 
	ex de,hl			;ef85	eb 	. 
	inc b			;ef86	04 	. 
lef87h:
	ld bc,004ebh		;ef87	01 eb 04 	. . . 
	ld bc,0c938h		;ef8a	01 38 c9 	. 8 . 
sub_ef8dh:
	call sub_eff7h		;ef8d	cd f7 ef 	. . . 
	push hl			;ef90	e5 	. 
	call sub_ef98h		;ef91	cd 98 ef 	. . . 
	pop hl			;ef94	e1 	. 
	call sub_eff7h		;ef95	cd f7 ef 	. . .

; Calculator 
sub_ef98h:
	push de			;ef98	d5 	. 
	rst 28h			;ef99	ef 	. 
	jr c,lef87h		;ef9a	38 eb 	8 . 
	pop de			;ef9c	d1 	. 
	ld c,000h		;ef9d	0e 00 	. . 
	bit 7,d		;ef9f	cb 7a 	. z 
	jr z,lefa4h		;efa1	28 01 	( . 
	dec c			;efa3	0d 	. 
lefa4h:
	ld (hl),000h		;efa4	36 00 	6 . 
lefa6h:
	inc hl			;efa6	23 	# 
	ld (hl),c			;efa7	71 	q 
	inc hl			;efa8	23 	# 
	ld (hl),e			;efa9	73 	s 
	inc hl			;efaa	23 	# 
	ld (hl),d			;efab	72 	r 
	inc hl			;efac	23 	# 
	ld (hl),000h		;efad	36 00 	6 . 
	inc hl			;efaf	23 	# 
	ld (05c65h),hl		;efb0	22 65 5c 	" e \ 
	ret			;efb3	c9 	.
 
sub_efb4h:
	rst 28h			;efb4	ef 	. 
	and d			;efb5	a2 	. 
	rrca			;efb6	0f 	. 
	daa			;efb7	27 	' 
	ld (bc),a			;efb8	02 	. 
	jr c,lefa6h		;efb9	38 eb 	8 . 
	xor a			;efbb	af 	. 
	or (hl)			;efbc	b6 	. 
	jr nz,lefcbh		;efbd	20 0c 	  . 
	inc hl			;efbf	23 	# 
	ld a,(hl)			;efc0	7e 	~ 
	inc hl			;efc1	23 	# 
	ld e,(hl)			;efc2	5e 	^ 
	inc hl			;efc3	23 	# 
	ld d,(hl)			;efc4	56 	V 
	xor d			;efc5	aa 	. 
	ld l,e			;efc6	6b 	k 
	ld h,d			;efc7	62 	b 
	and 0c0h		;efc8	e6 c0 	. . 
	ret z			;efca	c8 	. 
lefcbh:
	rst 8			;efcb	cf 	. 
	ld a,(bc)			;efcc	0a 	. 
lefcdh:
	ld (lfa80h),a		;efcd	32 80 fa 	2 . . 
	call sub_ef84h		;efd0	cd 84 ef 	. . . 
	rst 28h			;efd3	ef 	. 
	call pe,038edh		;efd4	ec ed 38 	. . 8 
lefd7h:
	call sub_effch		;efd7	cd fc ef 	. . . 
	call sub_f3ddh		;efda	cd dd f3 	. . . 
lefddh:
	ld hl,D006_whe_x		;efdd	21 aa fa 	! . . 
	ld de,lfab2h		;efe0	11 b2 fa 	. . . 
	ld bc,0000ch		;efe3	01 0c 00 	. . . 
	ldir		;efe6	ed b0 	. . 
	ret			;efe8	c9 	. 
lefe9h:
	ld (lfa80h),a		;efe9	32 80 fa 	2 . . 
	call sub_ef84h		;efec	cd 84 ef 	. . . 
	ld hl,D006_whe_x		;efef	21 aa fa 	! . . 
	call sub_ef8dh		;eff2	cd 8d ef 	. . . 
	jr lefd7h		;eff5	18 e0 	. . 
sub_eff7h:
	ld e,(hl)			;eff7	5e 	^ 
	inc hl			;eff8	23 	# 
	ld d,(hl)			;eff9	56 	V 
	inc hl			;effa	23 	# 
	ret			;effb	c9 	. 
sub_effch:
	rst 28h			;effc	ef 	. 
	ld bc,002c0h		;effd	01 c0 02 	. . . 
	rrca			;f000	0f 	. 
	ld bc,00fe0h		;f001	01 e0 0f 	. . . 
	ld bc,0c938h		;f004	01 38 c9 	. 8 . 
	ld a,001h		;f007	3e 01 	> . 
	jr lefcdh		;f009	18 c2 	. . 
	ld a,001h		;f00b	3e 01 	> . 
	jr lefe9h		;f00d	18 da 	. . 
	xor a			;f00f	af 	. 
	jr lefcdh		;f010	18 bb 	. . 
	xor a			;f012	af 	. 
	jr lefe9h		;f013	18 d4 	. . 
	ld a,002h		;f015	3e 02 	> . 
	call lefcdh		;f017	cd cd ef 	. . . 
lf01ah:
	ld a,(lfa7fh)		;f01a	3a 7f fa 	:  . 
	and a			;f01d	a7 	. 
	ret m			;f01e	f8 	. 
sub_f01fh:
	ld a,001h		;f01f	3e 01 	> . 
	call sub_f6d5h		;f021	cd d5 f6 	. . . 
	xor a			;f024	af 	. 
	jp sub_f6d5h		;f025	c3 d5 f6 	. . . 
	ld a,002h		;f028	3e 02 	> . 
	call lefe9h		;f02a	cd e9 ef 	. . . 
	jr lf01ah		;f02d	18 eb 	. . 
	rst 28h			;f02f	ef 	. 
	call 0cc02h		;f030	cd 02 cc 	. . . 
	ld (bc),a			;f033	02 	. 
	jr c,$-53		;f034	38 c9 	8 . 
	ld b,002h		;f036	06 02 	. . 
	ld hl,lfa9ah+1		;f038	21 9b fa 	! . . 
lf03bh:
	push bc			;f03b	c5 	. 
	push hl			;f03c	e5 	. 
	call sub_ef84h		;f03d	cd 84 ef 	. . . 
	rst 28h			;f040	ef 	. 
	defb 0edh;next byte illegal after ed		;f041	ed 	. 
	rrca			;f042	0f 	. 
	ld bc,00fech		;f043	01 ec 0f 	. . . 
	ld bc,0cd38h		;f046	01 38 cd 	. 8 . 
	or h			;f049	b4 	. 
	rst 28h			;f04a	ef 	. 
	pop hl			;f04b	e1 	. 
	ld (hl),d			;f04c	72 	r 
	dec hl			;f04d	2b 	+ 
	ld (hl),e			;f04e	73 	s 
	dec hl			;f04f	2b 	+ 
	push hl			;f050	e5 	. 
	call sub_efb4h		;f051	cd b4 ef 	. . . 
	pop hl			;f054	e1 	. 
	ld (hl),d			;f055	72 	r 
	dec hl			;f056	2b 	+ 
	ld (hl),e			;f057	73 	s 
	dec hl			;f058	2b 	+ 
	pop bc			;f059	c1 	. 
	djnz lf03bh		;f05a	10 df 	. . 
	ld hl,lfa96h+2		;f05c	21 98 fa 	! . . 
	ld de,lfa96h		;f05f	11 96 fa 	. . . 
sub_f062h:
	ld b,002h		;f062	06 02 	. . 
lf064h:
	ld a,(de)			;f064	1a 	. 
	ld c,(hl)			;f065	4e 	N 
	ex de,hl			;f066	eb 	. 
	ld (hl),c			;f067	71 	q 
	ld (de),a			;f068	12 	. 
	inc hl			;f069	23 	# 
	inc de			;f06a	13 	. 
	djnz lf064h		;f06b	10 f7 	. . 
	ret			;f06d	c9 	. 
	call sub_ef84h		;f06e	cd 84 ef 	. . . 
	call sub_ef35h		;f071	cd 35 ef 	. 5 . 
	jr nz,lf07bh		;f074	20 05 	  . 
	rst 28h			;f076	ef 	. 
	and b			;f077	a0 	. 
	jr c,$+26		;f078	38 18 	8 . 
	ex af,af'			;f07a	08 	. 
lf07bh:
	call sub_f156h		;f07b	cd 56 f1 	. V . 
	call sub_ef35h		;f07e	cd 35 ef 	. 5 . 
	jr nz,lf088h		;f081	20 05 	  . 
	rst 28h			;f083	ef 	. 
	and e			;f084	a3 	. 
	jr c,lf09fh		;f085	38 18 	8 . 
	inc bc			;f087	03 	. 
lf088h:
	call sub_f156h		;f088	cd 56 f1 	. V . 
	rst 28h			;f08b	ef 	. 
	ld bc,00fcah		;f08c	01 ca 0f 	. . . 
	ret			;f08f	c9 	. 
	ld (bc),a			;f090	02 	. 
	ld sp,0ea31h		;f091	31 31 ea 	1 1 . 
	rra			;f094	1f 	. 
	rst 0			;f095	c7 	. 
lf096h:
	inc b			;f096	04 	. 
	ld bc,020eah		;f097	01 ea 20 	. .   
	add a,004h		;f09a	c6 04 	. . 
	dec de			;f09c	1b 	. 
	jr c,$-49		;f09d	38 cd 	8 . 
lf09fh:
	or h			;f09f	b4 	. 
	rst 28h			;f0a0	ef 	. 
	ld (lfb1dh),hl		;f0a1	22 1d fb 	" . . 
	call sub_efb4h		;f0a4	cd b4 ef 	. . . 
	ld (lfb1bh),hl		;f0a7	22 1b fb 	" . . 
	rst 28h			;f0aa	ef 	. 
	ld sp,01fe9h		;f0ab	31 e9 1f 	1 . . 
	inc b			;f0ae	04 	. 
	ld bc,020e9h		;f0af	01 e9 20 	. .   
	inc b			;f0b2	04 	. 
	jr c,$-49		;f0b3	38 cd 	8 . 
lf0b5h:
	or h			;f0b5	b4 	. 
	rst 28h			;f0b6	ef 	. 
	ld (lfb17h),hl		;f0b7	22 17 fb 	" . . 
	call sub_efb4h		;f0ba	cd b4 ef 	. . . 
	ld (lfb19h),hl		;f0bd	22 19 fb 	" . . 
	rst 28h			;f0c0	ef 	. 
	ld sp,004e6h		;f0c1	31 e6 04 	1 . . 
	ld bc,004e7h		;f0c4	01 e7 04 	. . . 
	jr c,lf096h		;f0c7	38 cd 	8 . 
	or h			;f0c9	b4 	. 
	rst 28h			;f0ca	ef 	. 
	ld (lfb15h),hl		;f0cb	22 15 fb 	" . . 
	call sub_efb4h		;f0ce	cd b4 ef 	. . . 
	ld (lfb13h),hl		;f0d1	22 13 fb 	" . . 
	call sub_ef35h		;f0d4	cd 35 ef 	. 5 . 
	jr z,lf103h		;f0d7	28 2a 	( * 
	call sub_ef35h		;f0d9	cd 35 ef 	. 5 . 
	jp z,lef82h		;f0dc	ca 82 ef 	. . . 
	rst 28h			;f0df	ef 	. 
	ld sp,004e7h		;f0e0	31 e7 04 	1 . . 
	ld bc,004e6h		;f0e3	01 e6 04 	. . . 
	jr c,lf0b5h		;f0e6	38 cd 	8 . 
	or h			;f0e8	b4 	. 
	rst 28h			;f0e9	ef 	. 
	ld (lfa8fh+1),hl		;f0ea	22 90 fa 	" . . 
	call sub_efb4h		;f0ed	cd b4 ef 	. . . 
	ld (lfa92h),hl		;f0f0	22 92 fa 	" . . 
	rst 28h			;f0f3	ef 	. 
	ex de,hl			;f0f4	eb 	. 
	inc b			;f0f5	04 	. 
	jr c,$-49		;f0f6	38 cd 	8 . 
	or h			;f0f8	b4 	. 
	rst 28h			;f0f9	ef 	. 
	ld a,e			;f0fa	7b 	{ 
	or a			;f0fb	b7 	. 
	jr nz,lf0ffh		;f0fc	20 01 	  . 
	inc a			;f0fe	3c 	< 
lf0ffh:
	ld (lfa8fh),a		;f0ff	32 8f fa 	2 . . 
	ret			;f102	c9 	. 
lf103h:
	ld hl,00000h		;f103	21 00 00 	! . . 
	ld (lfa8fh+1),hl		;f106	22 90 fa 	" . . 
	ld (lfa92h),hl		;f109	22 92 fa 	" . . 
	ld a,001h		;f10c	3e 01 	> . 
	jr lf0ffh		;f10e	18 ef 	. . 
	ld (ix+00ah),000h		;f110	dd 36 0a 00 	. 6 . . 
	call sub_f143h		;f114	cd 43 f1 	. C . 
	call sub_f14ah		;f117	cd 4a f1 	. J . 
	call sub_f3e9h		;f11a	cd e9 f3 	. . . 
	ld (ix+00ah),001h		;f11d	dd 36 0a 01 	. 6 . . 
	ld hl,(lfa96h)		;f121	2a 96 fa 	* . . 
	ld (lfaaeh),hl		;f124	22 ae fa 	" . . 
	call sub_f3e9h		;f127	cd e9 f3 	. . . 
	ld hl,(lfa9ah)		;f12a	2a 9a fa 	* . . 
	ld (lfab0h),hl		;f12d	22 b0 fa 	" . . 
	call sub_f3e9h		;f130	cd e9 f3 	. . . 
	call sub_f143h		;f133	cd 43 f1 	. C . 
	call sub_f3e9h		;f136	cd e9 f3 	. . . 
	call sub_f14ah		;f139	cd 4a f1 	. J . 
	call sub_f3e9h		;f13c	cd e9 f3 	. . . 
	xor a			;f13f	af 	. 
	jp sub_f6d5h		;f140	c3 d5 f6 	. . . 
sub_f143h:
	ld hl,(lfa94h)		;f143	2a 94 fa 	* . . 
	ld (lfaaeh),hl		;f146	22 ae fa 	" . . 
	ret			;f149	c9 	. 
sub_f14ah:
	ld hl,(lfa96h+2)		;f14a	2a 98 fa 	* . . 
	ld (lfab0h),hl		;f14d	22 b0 fa 	" . . 
	ret			;f150	c9 	. 
lf151h:
	rst 28h			;f151	ef 	. 
	rlc d		;f152	cb 02 	. . 
	jr c,$-53		;f154	38 c9 	8 . 
sub_f156h:
	ret			;f156	c9 	. 
	ld de,0005ah		;f157	11 5a 00 	. Z . 
	call sub_ef98h		;f15a	cd 98 ef 	. . . 
	rst 28h			;f15d	ef 	. 
	dec b			;f15e	05 	. 
	and e			;f15f	a3 	. 
	inc b			;f160	04 	. 
	jr c,$-53		;f161	38 c9 	8 . 
lf163h:
	call sub_efb4h		;f163	cd b4 ef 	. . . 
	rrc l		;f166	cb 0d 	. . 
	sbc a,a			;f168	9f 	. 
	cpl			;f169	2f 	/ 
	and 0c9h		;f16a	e6 c9 	. . 
	ld (sub_f156h),a		;f16c	32 56 f1 	2 V . 
	ret			;f16f	c9 	. 
	call sub_f156h		;f170	cd 56 f1 	. V . 
	rst 28h			;f173	ef 	. 
	ld bc,0cd38h		;f174	01 38 cd 	. 8 . 
	ld d,(hl)			;f177	56 	V 
	pop af			;f178	f1 	. 
	rst 28h			;f179	ef 	. 
	jp z,00103h		;f17a	ca 03 01 	. . . 
	ret			;f17d	c9 	. 
	ex de,hl			;f17e	eb 	. 
	inc b			;f17f	04 	. 
	add hl,hl			;f180	29 	) 
	rst 0			;f181	c7 	. 
	inc b			;f182	04 	. 
	ret z			;f183	c8 	. 
	and e			;f184	a3 	. 
	ld sp,0310fh		;f185	31 0f 31 	1 . 1 
	rrca			;f188	0f 	. 
	add a,005h		;f189	c6 05 	. . 
	daa			;f18b	27 	' 
	and 004h		;f18c	e6 04 	. . 
	ret pe			;f18e	e8 	. 
	ld bc,03103h		;f18f	01 03 31 	. . 1 
	nop			;f192	00 	. 
	inc bc			;f193	03 	. 
	ld (bc),a			;f194	02 	. 
	and 0c8h		;f195	e6 c8 	. . 
	jp (hl)			;f197	e9 	. 
	ld hl,(0ebc9h)		;f198	2a c9 eb 	* . . 
	inc b			;f19b	04 	. 
	ld hl,(0a131h)		;f19c	2a 31 a1 	* 1 . 
	inc bc			;f19f	03 	. 
	ld bc,02305h		;f1a0	01 05 23 	. . # 
	dec b			;f1a3	05 	. 
	and c			;f1a4	a1 	. 
	rrca			;f1a5	0f 	. 
	daa			;f1a6	27 	' 
	add a,0e8h		;f1a7	c6 e8 	. . 
	ld bc,0e705h		;f1a9	01 05 e7 	. . . 
	inc b			;f1ac	04 	. 
	ld sp,0c820h		;f1ad	31 20 c8 	1   . 
	ld (bc),a			;f1b0	02 	. 
	rra			;f1b1	1f 	. 
	rst 0			;f1b2	c7 	. 
	ld (bc),a			;f1b3	02 	. 
	call pe,038edh		;f1b4	ec ed 38 	. . 8 
	ld hl,D006_whe_x		;f1b7	21 aa fa 	! . . 
	call sub_ef8dh		;f1ba	cd 8d ef 	. . . 
	rst 28h			;f1bd	ef 	. 
	call 0cc02h		;f1be	cd 02 cc 	. . . 
	ld (bc),a			;f1c1	02 	. 
	jp (hl)			;f1c2	e9 	. 
	ld sp,020eah		;f1c3	31 ea 20 	1 .   
	inc b			;f1c6	04 	. 
	ret			;f1c7	c9 	. 
	ld bc,01feah		;f1c8	01 ea 1f 	. . . 
	inc b			;f1cb	04 	. 
	jp z,03e38h		;f1cc	ca 38 3e 	. 8 > 
	inc b			;f1cf	04 	. 
	call lefcdh		;f1d0	cd cd ef 	. . . 
	rst 28h			;f1d3	ef 	. 
	jp (hl)			;f1d4	e9 	. 
	ld sp,004e8h		;f1d5	31 e8 04 	1 . . 
	jp pe,004e7h		;f1d8	ea e7 04 	. . . 
	inc bc			;f1db	03 	. 
	ret			;f1dc	c9 	. 
	ld bc,004e7h		;f1dd	01 e7 04 	. . . 
	jp pe,004e8h		;f1e0	ea e8 04 	. . . 
	rrca			;f1e3	0f 	. 
lf1e4h:
	jp z,0cd38h		;f1e4	ca 38 cd 	. 8 . 
	rlca			;f1e7	07 	. 
	ret p			;f1e8	f0 	. 
	rst 28h			;f1e9	ef 	. 
	and 0a1h		;f1ea	e6 a1 	. . 
	inc bc			;f1ec	03 	. 
	add a,037h		;f1ed	c6 37 	. 7 
	nop			;f1ef	00 	. 
	call po,001edh		;f1f0	e4 ed 01 	. . . 
	call 00102h		;f1f3	cd 02 01 	. . . 
	call pe,0cc01h		;f1f6	ec 01 cc 	. . . 
	ld (bc),a			;f1f9	02 	. 
	jr c,$-49		;f1fa	38 cd 	8 . 
	or h			;f1fc	b4 	. 
	rst 28h			;f1fd	ef 	. 
	ld (lfaaeh),hl		;f1fe	22 ae fa 	" . . 
	call sub_efb4h		;f201	cd b4 ef 	. . . 
	ld (lfab0h),hl		;f204	22 b0 fa 	" . . 
	ld a,004h		;f207	3e 04 	> . 
	ld (lfa80h),a		;f209	32 80 fa 	2 . . 
	call sub_f3e9h		;f20c	cd e9 f3 	. . . 
	jp lefddh		;f20f	c3 dd ef 	. . . 
lf212h:
	rst 28h			;f212	ef 	. 
	and b			;f213	a0 	. 
	and b			;f214	a0 	. 
	jr c,lf1e4h		;f215	38 cd 	8 . 
	ld a,c			;f217	79 	y 
	pop af			;f218	f1 	. 
	xor a			;f219	af 	. 
	jp sub_f6d5h		;f21a	c3 d5 f6 	. . . 
lf21dh:
	res 7,(ix+002h)		;f21d	dd cb 02 be 	. . . . 
	ret			;f221	c9 	. 
	set 7,(ix+002h)		;f222	dd cb 02 fe 	. . . . 
	ret			;f226	c9 	. 
	call sub_efb4h		;f227	cd b4 ef 	. . . 
	ld hl,(05c5dh)		;f22a	2a 5d 5c 	* ] \ 
	ex (sp),hl			;f22d	e3 	. 
	push de			;f22e	d5 	. 
	inc (ix+000h)		;f22f	dd 34 00 	. 4 . 
	jp (hl)			;f232	e9 	. 
	dec (ix+000h)		;f233	dd 35 00 	. 5 . 
	jp m,lf24fh		;f236	fa 4f f2 	. O . 
	inc (ix+000h)		;f239	dd 34 00 	. 4 . 
	pop hl			;f23c	e1 	. 
	pop de			;f23d	d1 	. 
	dec de			;f23e	1b 	. 
	ld a,e			;f23f	7b 	{ 
	or d			;f240	b2 	. 
	jr z,lf24ah		;f241	28 07 	( . 
	ex (sp),hl			;f243	e3 	. 
	ld (05c5dh),hl		;f244	22 5d 5c 	" ] \ 
	ex (sp),hl			;f247	e3 	. 
	push de			;f248	d5 	. 
	jp (hl)			;f249	e9 	. 
lf24ah:
	pop de			;f24a	d1 	. 
	dec (ix+000h)		;f24b	dd 35 00 	. 5 . 
	jp (hl)			;f24e	e9 	. 
lf24fh:
	rst 8			;f24f	cf 	. 
	add hl,bc			;f250	09 	. 
lf251h:
	call sub_efb4h		;f251	cd b4 ef 	. . . 
	ld (ix+006h),l		;f254	dd 75 06 	. u . 
	ret			;f257	c9 	. 
lf258h:
	ld hl,lfc2eh		;f258	21 2e fc 	! . . 
	ld (lfb10h),hl		;f25b	22 10 fb 	" . . 
	call sub_efb4h		;f25e	cd b4 ef 	. . . 
	ld a,e			;f261	7b 	{ 
	ld (lfb12h),a		;f262	32 12 fb 	2 . . 
	or a			;f265	b7 	. 
	ret z			;f266	c8 	. 
	ld hl,lfb30h		;f267	21 30 fb 	! 0 . 
lf26ah:
	dec a			;f26a	3d 	= 
	jr z,lf275h		;f26b	28 08 	( . 
lf26dh:
	inc hl			;f26d	23 	# 
	bit 7,(hl)		;f26e	cb 7e 	. ~ 
	inc hl			;f270	23 	# 
	jr z,lf26dh		;f271	28 fa 	( . 
	jr lf26ah		;f273	18 f5 	. . 
lf275h:
	ld e,(hl)			;f275	5e 	^ 
	inc hl			;f276	23 	# 
	ld d,(hl)			;f277	56 	V 
lf278h:
	inc hl			;f278	23 	# 
	push hl			;f279	e5 	. 
	push de			;f27a	d5 	. 
	res 7,d		;f27b	cb ba 	. . 
	call sub_ef98h		;f27d	cd 98 ef 	. . . 
	call sub_f2bbh		;f280	cd bb f2 	. . . 
	pop de			;f283	d1 	. 
	pop hl			;f284	e1 	. 
	bit 7,d		;f285	cb 7a 	. z 
	jr z,lf275h		;f287	28 ec 	( . 
lf289h:
	ld hl,(lfb10h)		;f289	2a 10 fb 	* . . 
	dec hl			;f28c	2b 	+ 
	set 7,(hl)		;f28d	cb fe 	. . 
	jp lf6a3h		;f28f	c3 a3 f6 	. . . 
	ld hl,lfc2eh		;f292	21 2e fc 	! . . 
	ld (lfb10h),hl		;f295	22 10 fb 	" . . 
	ld a,0ffh		;f298	3e ff 	> . 
	ld (lfb12h),a		;f29a	32 12 fb 	2 . . 
	inc a			;f29d	3c 	< 
lf29eh:
	push af			;f29e	f5 	. 
	call sub_ef35h		;f29f	cd 35 ef 	. 5 . 
	jr z,lf2b5h		;f2a2	28 11 	( . 
	rst 28h			;f2a4	ef 	. 
	and h			;f2a5	a4 	. 
	inc b			;f2a6	04 	. 
	and d			;f2a7	a2 	. 
	inc b			;f2a8	04 	. 
	jr c,lf278h		;f2a9	38 cd 	8 . 
	cp e			;f2ab	bb 	. 
	jp p,03cf1h		;f2ac	f2 f1 3c 	. . < 
	cp 010h		;f2af	fe 10 	. . 
	jr c,lf29eh		;f2b1	38 eb 	8 . 
	rst 8			;f2b3	cf 	. 
	rlca			;f2b4	07 	. 
lf2b5h:
	pop af			;f2b5	f1 	. 
	jr nz,lf289h		;f2b6	20 d1 	  . 
	jp lf24fh		;f2b8	c3 4f f2 	. O . 
sub_f2bbh:
	rst 28h			;f2bb	ef 	. 
	ex de,hl			;f2bc	eb 	. 
	inc b			;f2bd	04 	. 
	ld hl,(0cd38h)		;f2be	2a 38 cd 	* 8 . 
	or h			;f2c1	b4 	. 
	rst 28h			;f2c2	ef 	. 
	ld hl,(lfb10h)		;f2c3	2a 10 fb 	* . . 
	ld (hl),e			;f2c6	73 	s 
	inc hl			;f2c7	23 	# 
	ld (hl),d			;f2c8	72 	r 
	inc hl			;f2c9	23 	# 
	ld (lfb10h),hl		;f2ca	22 10 fb 	" . . 
	ret			;f2cd	c9 	. 
	call sub_efb4h		;f2ce	cd b4 ef 	. . . 
	ld a,e			;f2d1	7b 	{ 
	ld hl,00014h		;f2d2	21 14 00 	! . . 
	call sub_f56ch		;f2d5	cd 6c f5 	. l . 
	ld (0f75ch),hl		;f2d8	22 5c f7 	" \ . 
	call sub_efb4h		;f2db	cd b4 ef 	. . . 
	ld a,e			;f2de	7b 	{ 
	ld (0f76fh),a		;f2df	32 6f f7 	2 o . 
	ld hl,003e8h		;f2e2	21 e8 03 	! . . 
	call sub_f542h		;f2e5	cd 42 f5 	. B . 
	ld a,l			;f2e8	7d 	} 
	ld (lfb2bh),a		;f2e9	32 2b fb 	2 + . 
lf2ech:
	call sub_efb4h		;f2ec	cd b4 ef 	. . . 
	inc de			;f2ef	13 	. 
	ld a,e			;f2f0	7b 	{ 
	ld (0fa06h),a		;f2f1	32 06 fa 	2 . . 
	ret			;f2f4	c9 	. 
	xor a			;f2f5	af 	. 
	ld (lfa80h),a		;f2f6	32 80 fa 	2 . . 
	ld hl,lfaa6h		;f2f9	21 a6 fa 	! . . 
	call sub_f598h		;f2fc	cd 98 f5 	. . . 
	call sub_f01fh		;f2ff	cd 1f f0 	. . . 
	ld hl,lfaa0h		;f302	21 a0 fa 	! . . 
	call sub_f598h		;f305	cd 98 f5 	. . . 
	call sub_f01fh		;f308	cd 1f f0 	. . . 
	ld hl,lfaa0h+2		;f30b	21 a2 fa 	! . . 
	call sub_f598h		;f30e	cd 98 f5 	. . . 
	call sub_f01fh		;f311	cd 1f f0 	. . . 
	call sub_f591h		;f314	cd 91 f5 	. . . 
	jp sub_f01fh		;f317	c3 1f f0 	. . . 
lf31ah:
	rst 28h			;f31a	ef 	. 
	ex de,hl			;f31b	eb 	. 
	inc b			;f31c	04 	. 
	jr c,lf2ech		;f31d	38 cd 	8 . 
	or h			;f31f	b4 	. 
	rst 28h			;f320	ef 	. 
	push de			;f321	d5 	. 
	ld a,05ah		;f322	3e 5a 	> Z 
	ld (0f362h),a		;f324	32 62 f3 	2 b . 
	ld a,009h		;f327	3e 09 	> . 
	ld (0f373h),a		;f329	32 73 f3 	2 s . 
	ld a,023h		;f32c	3e 23 	> # 
	ld (lf376h),a		;f32e	32 76 f3 	2 v . 
	xor 008h		;f331	ee 08 	. . 
	ld (lf379h),a		;f333	32 79 f3 	2 y . 
	ld a,00fh		;f336	3e 0f 	> . 
	ld (lf3a6h),a		;f338	32 a6 f3 	2 . . 
	ld hl,04000h		;f33b	21 00 40 	! . @ 
	xor a			;f33e	af 	. 
	ld (lfb12h),a		;f33f	32 12 fb 	2 . . 
	ld (ix+014h),003h		;f342	dd 36 14 03 	. 6 . . 
lf346h:
	ld (ix+015h),008h		;f346	dd 36 15 08 	. 6 . . 
lf34ah:
	ld (ix+016h),008h		;f34a	dd 36 16 08 	. 6 . . 
lf34eh:
	ld b,020h		;f34e	06 20 	.   
lf350h:
	ld c,080h		;f350	0e 80 	. . 
lf352h:
	ld a,(hl)			;f352	7e 	~ 
	and c			;f353	a1 	. 
	jr z,lf358h		;f354	28 02 	( . 
	ld a,001h		;f356	3e 01 	> . 
lf358h:
	ld (lfa80h),a		;f358	32 80 fa 	2 . . 
	ex (sp),hl			;f35b	e3 	. 
	ex de,hl			;f35c	eb 	. 
	ld hl,(lfaaeh)		;f35d	2a ae fa 	* . . 
	and a			;f360	a7 	. 
	adc hl,de		;f361	ed 5a 	. Z 
	ld (lfaaeh),hl		;f363	22 ae fa 	" . . 
	ex de,hl			;f366	eb 	. 
	ex (sp),hl			;f367	e3 	. 
	push hl			;f368	e5 	. 
	push bc			;f369	c5 	. 
	call sub_f3e9h		;f36a	cd e9 f3 	. . . 
	call sub_f811h		;f36d	cd 11 f8 	. . . 
	pop bc			;f370	c1 	. 
	pop hl			;f371	e1 	. 
	rrc c		;f372	cb 09 	. . 
	jr nc,lf352h		;f374	30 dc 	0 . 
lf376h:
	inc hl			;f376	23 	# 
	djnz lf350h		;f377	10 d7 	. . 
lf379h:
	dec hl			;f379	2b 	+ 
	inc h			;f37a	24 	$ 
	ld a,(0f362h)		;f37b	3a 62 f3 	: b . 
	xor 008h		;f37e	ee 08 	. . 
	ld (0f362h),a		;f380	32 62 f3 	2 b . 
	ld a,(0f373h)		;f383	3a 73 f3 	: s . 
	xor 008h		;f386	ee 08 	. . 
	ld (0f373h),a		;f388	32 73 f3 	2 s . 
	ld a,(lf376h)		;f38b	3a 76 f3 	: v . 
	xor 008h		;f38e	ee 08 	. . 
	ld (lf376h),a		;f390	32 76 f3 	2 v . 
	ld a,(lf379h)		;f393	3a 79 f3 	: y . 
	xor 008h		;f396	ee 08 	. . 
	ld (lf379h),a		;f398	32 79 f3 	2 y . 
	ld a,(lf3a6h)		;f39b	3a a6 f3 	: . . 
	xor 008h		;f39e	ee 08 	. . 
	ld (lf3a6h),a		;f3a0	32 a6 f3 	2 . . 
	ld a,(lf350h+1)		;f3a3	3a 51 f3 	: Q . 
lf3a6h:
	rrca			;f3a6	0f 	. 
	ld (lf350h+1),a		;f3a7	32 51 f3 	2 Q . 
	ex (sp),hl			;f3aa	e3 	. 
	ex de,hl			;f3ab	eb 	. 
	push de			;f3ac	d5 	. 
	ld hl,(lfab0h)		;f3ad	2a b0 fa 	* . . 
	xor a			;f3b0	af 	. 
	sbc hl,de		;f3b1	ed 52 	. R 
	ld (lfab0h),hl		;f3b3	22 b0 fa 	" . . 
	ld (lfa80h),a		;f3b6	32 80 fa 	2 . . 
	call sub_f3e9h		;f3b9	cd e9 f3 	. . . 
	pop hl			;f3bc	e1 	. 
	ex (sp),hl			;f3bd	e3 	. 
	dec (ix+016h)		;f3be	dd 35 16 	. 5 . 
	jr nz,lf34eh		;f3c1	20 8b 	  . 
	ld a,h			;f3c3	7c 	| 
	sub 008h		;f3c4	d6 08 	. . 
	ld h,a			;f3c6	67 	g 
	ld a,l			;f3c7	7d 	} 
	add a,020h		;f3c8	c6 20 	.   
	ld l,a			;f3ca	6f 	o 
	dec (ix+015h)		;f3cb	dd 35 15 	. 5 . 
	jp nz,lf34ah		;f3ce	c2 4a f3 	. J . 
	ld a,h			;f3d1	7c 	| 
	add a,008h		;f3d2	c6 08 	. . 
	ld h,a			;f3d4	67 	g 
	dec (ix+014h)		;f3d5	dd 35 14 	. 5 . 
	jp nz,lf346h		;f3d8	c2 46 f3 	. F . 
	pop de			;f3db	d1 	. 
	ret			;f3dc	c9 	. 
sub_f3ddh:
	call sub_efb4h		;f3dd	cd b4 ef 	. . . 
	ld (lfab0h),hl		;f3e0	22 b0 fa 	" . . 
	call sub_efb4h		;f3e3	cd b4 ef 	. . . 
	ld (lfaaeh),hl		;f3e6	22 ae fa 	" . . 
sub_f3e9h:
	ld a,(lfa80h)		;f3e9	3a 80 fa 	: . . 
	or a			;f3ec	b7 	. 
	jr z,lf46ah		;f3ed	28 7b 	( { 
	cp 004h		;f3ef	fe 04 	. . 
	jr z,lf451h		;f3f1	28 5e 	( ^ 
	ld hl,D006_whe_x		;f3f3	21 aa fa 	! . . 
	ld de,D008		;f3f6	11 c6 fa 	. . . 
	ld bc,00008h		;f3f9	01 08 00 	. . . 
	ldir		;f3fc	ed b0 	. . 
	ld (ix+009h),000h		;f3fe	dd 36 09 00 	. 6 . . 
	ld de,lfa94h		;f402	11 94 fa 	. . . 
	call sub_f476h		;f405	cd 76 f4 	. v . 
	jp m,lf451h		;f408	fa 51 f4 	. Q . 
	ld de,lfa9eh		;f40b	11 9e fa 	. . . 
	call sub_f476h		;f40e	cd 76 f4 	. v . 
	jp m,lf451h		;f411	fa 51 f4 	. Q . 
	ld a,(lfa7fh)		;f414	3a 7f fa 	:  . 
	or a			;f417	b7 	. 
	ld a,(ix+00ah)		;f418	dd 7e 0a 	. ~ . 
	push af			;f41b	f5 	. 
	ld (ix+00ah),000h		;f41c	dd 36 0a 00 	. 6 . . 
	ld hl,D008		;f420	21 c6 fa 	! . . 
	push hl			;f423	e5 	. 
	ld hl,(D008)		;f424	2a c6 fa 	* . . 
	ld de,(D004_x_pos)		;f427	ed 5b c2 fa 	. [ . . 
	and a			;f42b	a7 	. 
	sbc hl,de		;f42c	ed 52 	. R 
	pop hl			;f42e	e1 	. 
	push hl			;f42f	e5 	. 
	call nz,sub_f598h		;f430	c4 98 f5 	. . . 
	ld hl,(lfac8h)		;f433	2a c8 fa 	* . . 
	ld de,(D003_y_pos)		;f436	ed 5b c4 fa 	. [ . . 
	and a			;f43a	a7 	. 
	sbc hl,de		;f43b	ed 52 	. R 
	pop hl			;f43d	e1 	. 
	call nz,sub_f598h		;f43e	c4 98 f5 	. . . 
	pop af			;f441	f1 	. 
	ld (ix+00ah),a		;f442	dd 77 0a 	. w . 
	ld hl,lfacah		;f445	21 ca fa 	! . . 
	call sub_f598h		;f448	cd 98 f5 	. . . 
	ld a,(lfa7fh)		;f44b	3a 7f fa 	:  . 
	rlca			;f44e	07 	. 
	jr c,lf46ah		;f44f	38 19 	8 . 
lf451h:
	ld hl,lfaaeh		;f451	21 ae fa 	! . . 
	ld de,D006_whe_x		;f454	11 aa fa 	. . . 
	call sub_f470h		;f457	cd 70 f4 	. p . 
sub_f45ah:
	ld a,(lfa7fh)		;f45a	3a 7f fa 	:  . 
	rlca			;f45d	07 	. 
	sbc a,a			;f45e	9f 	. 
	xor (iy+00eh)		;f45f	fd ae 0e 	. . . 
	rrca			;f462	0f 	. 
	rrca			;f463	0f 	. 
	rrca			;f464	0f 	. 
	and 007h		;f465	e6 07 	. . 
	out (0feh),a		;f467	d3 fe 	. . 
	ret			;f469	c9 	. 
lf46ah:
	xor a			;f46a	af 	. 
	call sub_f6d5h		;f46b	cd d5 f6 	. . . 
	jr lf451h		;f46e	18 e1 	. .

; Copy 4 bytes
sub_f470h:
        ld bc,00004h            ;f470
        ldir                    ;f473
        ret                     ;f475

sub_f476h:
	ld (ix+008h),002h		;f476	dd 36 08 02 	. 6 . . 
lf47ah:
	ld (ix+007h),002h		;f47a	dd 36 07 02 	. 6 . . 
	xor a			;f47e	af 	. 
	ex af,af'			;f47f	08 	. 
lf480h:
	ex af,af'			;f480	08 	. 
	xor 0ffh		;f481	ee ff 	. . 
	ex af,af'			;f483	08 	. 
	ex de,hl			;f484	eb 	. 
	call sub_eff7h		;f485	cd f7 ef 	. . . 
	ex de,hl			;f488	eb 	. 
	push de			;f489	d5 	. 
	call sub_f4abh		;f48a	cd ab f4 	. . . 
	pop de			;f48d	d1 	. 
	ret m			;f48e	f8 	. 
	dec (ix+007h)		;f48f	dd 35 07 	. 5 . 
	jr nz,lf480h		;f492	20 ec 	  . 
	push de			;f494	d5 	. 
	ld hl,D008		;f495	21 c6 fa 	! . . 
	ld de,lfac8h		;f498	11 c8 fa 	. . . 
	call sub_f062h		;f49b	cd 62 f0 	. b . 
	ld c,004h		;f49e	0e 04 	. . 
	add hl,bc			;f4a0	09 	. 
	call sub_f062h		;f4a1	cd 62 f0 	. b . 
	pop de			;f4a4	d1 	. 
	dec (ix+008h)		;f4a5	dd 35 08 	. 5 . 
	jr nz,lf47ah		;f4a8	20 d0 	  . 
	ret			;f4aa	c9 	. 
sub_f4abh:
	ld (lfad8h),hl		;f4ab	22 d8 fa 	" . . 
	ex de,hl			;f4ae	eb 	. 
	ld hl,(D008)		;f4af	2a c6 fa 	* . . 
	call sub_f52bh		;f4b2	cd 2b f5 	. + . 
	ld c,h			;f4b5	4c 	L 
	ld (lfaceh),a		;f4b6	32 ce fa 	2 . . 
	ld hl,(lfacah)		;f4b9	2a ca fa 	* . . 
	call sub_f52bh		;f4bc	cd 2b f5 	. + . 
	or (ix+009h)		;f4bf	dd b6 09 	. . . 
	ld (lfa7fh),a		;f4c2	32 7f fa 	2  . 
	ld a,h			;f4c5	7c 	| 
	and c			;f4c6	a1 	. 
	ret m			;f4c7	f8 	. 
	ld a,h			;f4c8	7c 	| 
	or c			;f4c9	b1 	. 
	ret p			;f4ca	f0 	. 
	ld hl,D008		;f4cb	21 c6 fa 	! . . 
	ld de,lfacfh		;f4ce	11 cf fa 	. . . 
	ld bc,00009h		;f4d1	01 09 00 	. . . 
	ldir		;f4d4	ed b0 	. . 
lf4d6h:
	ld hl,(lfad1h)		;f4d6	2a d1 fa 	* . . 
	ld de,(lfad3h+2)		;f4d9	ed 5b d5 fa 	. [ . . 
	add hl,de			;f4dd	19 	. 
	call sub_f520h		;f4de	cd 20 f5 	.   . 
	push hl			;f4e1	e5 	. 
	ld hl,(lfacfh)		;f4e2	2a cf fa 	* . . 
	ld de,(lfad3h)		;f4e5	ed 5b d3 fa 	. [ . . 
	add hl,de			;f4e9	19 	. 
	call sub_f520h		;f4ea	cd 20 f5 	.   . 
	push hl			;f4ed	e5 	. 
	ld de,(lfad8h)		;f4ee	ed 5b d8 fa 	. [ . . 
	call sub_f52bh		;f4f2	cd 2b f5 	. + . 
	or l			;f4f5	b5 	. 
	push af			;f4f6	f5 	. 
	jr nz,lf503h		;f4f7	20 0a 	  . 
	ld a,(lfaceh)		;f4f9	3a ce fa 	: . . 
	xor 0ffh		;f4fc	ee ff 	. . 
	ld hl,D008		;f4fe	21 c6 fa 	! . . 
	jr lf50ah		;f501	18 07 	. . 
lf503h:
	ld a,(lfad7h)		;f503	3a d7 fa 	: . . 
	xor h			;f506	ac 	. 
	ld hl,lfacfh		;f507	21 cf fa 	! . . 
lf50ah:
	ld bc,00004h		;f50a	01 04 00 	. . . 
	jp p,lf511h		;f50d	f2 11 f5 	. . . 
	add hl,bc			;f510	09 	. 
lf511h:
	ld b,002h		;f511	06 02 	. . 
lf513h:
	pop af			;f513	f1 	. 
	pop de			;f514	d1 	. 
	push af			;f515	f5 	. 
	ld (hl),e			;f516	73 	s 
	inc hl			;f517	23 	# 
	ld (hl),d			;f518	72 	r 
	inc hl			;f519	23 	# 
	djnz lf513h		;f51a	10 f7 	. . 
	pop af			;f51c	f1 	. 
	jr nz,lf4d6h		;f51d	20 b7 	  . 
	ret			;f51f	c9 	. 
sub_f520h:
	sra h		;f520	cb 2c 	. , 
	rr l		;f522	cb 1d 	. . 
	ret nc			;f524	d0 	. 
	ex af,af'			;f525	08 	. 
	jr z,lf529h		;f526	28 01 	( . 
	inc hl			;f528	23 	# 
lf529h:
	ex af,af'			;f529	08 	. 
	ret			;f52a	c9 	. 
sub_f52bh:
	call sub_f539h		;f52b	cd 39 f5 	. 9 . 
	add hl,de			;f52e	19 	. 
	ex af,af'			;f52f	08 	. 
	call m,sub_f539h		;f530	fc 39 f5 	. 9 . 
	ex af,af'			;f533	08 	. 
	ld a,h			;f534	7c 	| 
	ret			;f535	c9 	. 
sub_f536h:
	ld a,h			;f536	7c 	| 
	and a			;f537	a7 	. 
	ret p			;f538	f0 	. 
sub_f539h:
	push af			;f539	f5 	. 
	xor a			;f53a	af 	. 
	sub l			;f53b	95 	. 
	ld l,a			;f53c	6f 	o 
	sbc a,a			;f53d	9f 	. 
	sub h			;f53e	94 	. 
	ld h,a			;f53f	67 	g 
	pop af			;f540	f1 	. 
	ret			;f541	c9 	. 
sub_f542h:
	push bc			;f542	c5 	. 
	push de			;f543	d5 	. 
	ld c,a			;f544	4f 	O 
	call sub_f536h		;f545	cd 36 f5 	. 6 . 
	ld a,c			;f548	79 	y 
	push af			;f549	f5 	. 
	ld de,00000h		;f54a	11 00 00 	. . . 
	ld b,008h		;f54d	06 08 	. . 
lf54fh:
	ld a,h			;f54f	7c 	| 
	sub c			;f550	91 	. 
	jr c,lf557h		;f551	38 04 	8 . 
	inc de			;f553	13 	. 
	ld h,a			;f554	67 	g 
	jr lf54fh		;f555	18 f8 	. . 
lf557h:
	add hl,hl			;f557	29 	) 
	ex de,hl			;f558	eb 	. 
	add hl,hl			;f559	29 	) 
	ex de,hl			;f55a	eb 	. 
	djnz lf54fh		;f55b	10 f2 	. . 
	ld a,h			;f55d	7c 	| 
	sub c			;f55e	91 	. 
	jr c,lf563h		;f55f	38 02 	8 . 
	inc de			;f561	13 	. 
	ld h,a			;f562	67 	g 
lf563h:
	ld a,h			;f563	7c 	| 
	add a,a			;f564	87 	. 
	sub c			;f565	91 	. 
	jr c,lf569h		;f566	38 01 	8 . 
	inc de			;f568	13 	. 
lf569h:
	ex de,hl			;f569	eb 	. 
	jr lf58ah		;f56a	18 1e 	. . 
sub_f56ch:
	push bc			;f56c	c5 	. 
	push de			;f56d	d5 	. 
	ld d,a			;f56e	57 	W 
	xor h			;f56f	ac 	. 
	ld a,d			;f570	7a 	z 
	push af			;f571	f5 	. 
	call sub_f536h		;f572	cd 36 f5 	. 6 . 
	ld e,000h		;f575	1e 00 	. . 
	ld b,007h		;f577	06 07 	. . 
	ex de,hl			;f579	eb 	. 
	call sub_f536h		;f57a	cd 36 f5 	. 6 . 
	ld a,h			;f57d	7c 	| 
	ld h,l			;f57e	65 	e 
lf57fh:
	add a,a			;f57f	87 	. 
	jr nc,lf583h		;f580	30 01 	0 . 
	add hl,de			;f582	19 	. 
lf583h:
	add hl,hl			;f583	29 	) 
	djnz lf57fh		;f584	10 f9 	. . 
	add a,a			;f586	87 	. 
	jr nc,lf58ah		;f587	30 01 	0 . 
	add hl,de			;f589	19 	. 
lf58ah:
	pop af			;f58a	f1 	. 
	call m,sub_f539h		;f58b	fc 39 f5 	. 9 . 
	pop de			;f58e	d1 	. 
	pop bc			;f58f	c1 	. 
	ret			;f590	c9 	. 
sub_f591h:
	xor a			;f591	af 	. 
	ld (lfa80h),a		;f592	32 80 fa 	2 . . 
	ld hl,lfa9ch		;f595	21 9c fa 	! . . 
sub_f598h:
	di			;f598	f3 	. 
	push hl			;f599	e5 	. 
	exx			;f59a	d9 	. 
	ld hl,00128h		;f59b	21 28 01 	! ( . 
	ld (lf67eh),hl		;f59e	22 7e f6 	" ~ . 
	ld a,003h		;f5a1	3e 03 	> . 
	ld (lf680h),a		;f5a3	32 80 f6 	2 . . 
	exx			;f5a6	d9 	. 
	ld de,(D004_x_pos)		;f5a7	ed 5b c2 fa 	. [ . . 
	call sub_f706h		;f5ab	cd 06 f7 	. . . 
	ld (D004_x_pos),de		;f5ae	ed 53 c2 fa 	. S . . 
	ld (lf677h),hl		;f5b2	22 77 f6 	" w . 
	xor 010h		;f5b5	ee 10 	. . 
	ld (0f67ah),a		;f5b7	32 7a f6 	2 z . 
	ld h,b			;f5ba	60 	` 
	ld l,c			;f5bb	69 	i 
	ex (sp),hl			;f5bc	e3 	. 
	inc hl			;f5bd	23 	# 
	inc hl			;f5be	23 	# 
	ld de,(D003_y_pos)		;f5bf	ed 5b c4 fa 	. [ . . 
	call sub_f706h		;f5c3	cd 06 f7 	. . . 
	ld (D003_y_pos),de		;f5c6	ed 53 c4 fa 	. S . . 
	ld (lf66eh),hl		;f5ca	22 6e f6 	" n . 
	ld (0f671h),a		;f5cd	32 71 f6 	2 q . 
	pop hl			;f5d0	e1 	. 
	push hl			;f5d1	e5 	. 
	ld d,b			;f5d2	50 	P 
	ld e,c			;f5d3	59 	Y 
	and a			;f5d4	a7 	. 
	sbc hl,de		;f5d5	ed 52 	. R 
	pop hl			;f5d7	e1 	. 
	jp p,lf5e9h		;f5d8	f2 e9 f5 	. . . 
	ex de,hl			;f5db	eb 	. 
	exx			;f5dc	d9 	. 
	ld hl,lf677h		;f5dd	21 77 f6 	! w . 
	ld de,lf66eh		;f5e0	11 6e f6 	. n . 
	ld b,004h		;f5e3	06 04 	. . 
	call lf064h		;f5e5	cd 64 f0 	. d . 
	exx			;f5e8	d9 	. 
lf5e9h:
	ld b,h			;f5e9	44 	D 
	ld c,l			;f5ea	4d 	M 
	ld hl,00000h		;f5eb	21 00 00 	! . . 
	exx			;f5ee	d9 	. 
	call sub_f6c0h		;f5ef	cd c0 f6 	. . . 
	ld de,(lf677h)		;f5f2	ed 5b 77 f6 	. [ w . 
	ld l,(ix+00dh)		;f5f6	dd 6e 0d 	. n . 
	ld h,000h		;f5f9	26 00 	& . 
	add hl,hl			;f5fb	29 	) 
	add hl,hl			;f5fc	29 	) 
	push hl			;f5fd	e5 	. 
	add hl,hl			;f5fe	29 	) 
	add hl,de			;f5ff	19 	. 
	pop hl			;f600	e1 	. 
	jr c,lf659h		;f601	38 56 	8 V 
	add hl,de			;f603	19 	. 
lf604h:
	ld (lfa85h),hl		;f604	22 85 fa 	" . . 
	ld hl,(lfa81h+1)		;f607	2a 82 fa 	* . . 
	ld h,000h		;f60a	26 00 	& . 
	ld e,l			;f60c	5d 	] 
	add hl,hl			;f60d	29 	) 
	ld a,005h		;f60e	3e 05 	> . 
	call sub_f542h		;f610	cd 42 f5 	. B . 
	ld a,l			;f613	7d 	} 
	add a,e			;f614	83 	. 
	ld (lfa84h),a		;f615	32 84 fa 	2 . . 
	ld bc,lfb40h		;f618	01 40 fb 	. @ . 
lf61bh:
	ld hl,lf677h		;f61b	21 77 f6 	! w . 
	call sub_f660h		;f61e	cd 60 f6 	. ` . 
	ld a,(lfa81h+1)		;f621	3a 82 fa 	: . . 
	and a			;f624	a7 	. 
	exx			;f625	d9 	. 
	sbc hl,de		;f626	ed 52 	. R 
	jp p,lf62ch		;f628	f2 2c f6 	. , . 
	add hl,bc			;f62b	09 	. 
lf62ch:
	exx			;f62c	d9 	. 
	jp p,lf639h		;f62d	f2 39 f6 	. 9 . 
	ld hl,lf66eh		;f630	21 6e f6 	! n . 
	call sub_f660h		;f633	cd 60 f6 	. ` . 
	ld a,(lfa84h)		;f636	3a 84 fa 	: . . 
lf639h:
	dec a			;f639	3d 	= 
	inc hl			;f63a	23 	# 
	dec hl			;f63b	2b 	+ 
	jr nz,lf639h		;f63c	20 fb 	  . 
	ld hl,lfa85h		;f63e	21 85 fa 	! . . 
	inc (hl)			;f641	34 	4 
	jr nz,lf653h		;f642	20 0f 	  . 
	inc hl			;f644	23 	# 
	inc (hl)			;f645	34 	4 
	jr nz,lf653h		;f646	20 0b 	  . 
	ld hl,00018h		;f648	21 18 00 	! . . 
	ld (lf67eh),hl		;f64b	22 7e f6 	" ~ . 
	ld a,00bh		;f64e	3e 0b 	> . 
	ld (lf680h),a		;f650	32 80 f6 	2 . . 
lf653h:
	ld a,(bc)			;f653	0a 	. 
lf654h:
	dec a			;f654	3d 	= 
	jr nz,lf654h		;f655	20 fd 	  . 
	jr lf61bh		;f657	18 c2 	. . 
lf659h:
	ex de,hl			;f659	eb 	. 
	sra h		;f65a	cb 2c 	. , 
	rr l		;f65c	cb 1d 	. . 
	jr lf604h		;f65e	18 a4 	. . 
sub_f660h:
	inc (hl)			;f660	34 	4 
	ld a,(hl)			;f661	7e 	~ 
	inc hl			;f662	23 	# 
	jr nz,lf668h		;f663	20 03 	  . 
	inc (hl)			;f665	34 	4 
	jr z,lf6b3h		;f666	28 4b 	( K 
lf668h:
	rrca			;f668	0f 	. 
sub_f669h:
	ret nc			;f669	d0 	. 
	rrca			;f66a	0f 	. 
	ret nc			;f66b	d0 	. 
	inc hl			;f66c	23 	# 
	jp (hl)			;f66d	e9 	. 
lf66eh:
	nop			;f66e	00 	. 
	nop			;f66f	00 	. 
	out (0d7h),a		;f670	d3 d7 	. . 
lf672h:
	ret			;f672	c9 	. 
	ld a,0feh		;f673	3e fe 	> . 
	jr lf684h		;f675	18 0d 	. . 
lf677h:
	nop			;f677	00 	. 
	nop			;f678	00 	. 
	out (0c7h),a		;f679	d3 c7 	. . 
	ld a,078h		;f67b	3e 78 	> x 
	cp c			;f67d	b9 	. 
lf67eh:
	jr z,lf681h		;f67e	28 01 	( . 
lf680h:
	inc bc			;f680	03 	. 
lf681h:
	ret			;f681	c9 	. 
	ld a,0fbh		;f682	3e fb 	> . 
lf684h:
	ld hl,lfb0ch		;f684	21 0c fb 	! . . 
	add a,(hl)			;f687	86 	. 
	ld (hl),a			;f688	77 	w 
	ret c			;f689	d8 	. 
	inc hl			;f68a	23 	# 
	dec (hl)			;f68b	35 	5 
	ret p			;f68c	f0 	. 
	ld a,(lfa81h)		;f68d	3a 81 fa 	: . . 
	xor 001h		;f690	ee 01 	. . 
	call sub_f6d5h		;f692	cd d5 f6 	. . . 
	ld hl,(lfb0eh)		;f695	2a 0e fb 	* . . 
	inc hl			;f698	23 	# 
	bit 7,(hl)		;f699	cb 7e 	. ~ 
	inc hl			;f69b	23 	# 
	jr z,lf6a6h		;f69c	28 08 	( . 
lf69eh:
	ld a,001h		;f69e	3e 01 	> . 
	call sub_f6d5h		;f6a0	cd d5 f6 	. . . 
lf6a3h:
	ld hl,lfc2eh		;f6a3	21 2e fc 	! . . 
lf6a6h:
	ld (lfb0eh),hl		;f6a6	22 0e fb 	" . . 
	ld e,(hl)			;f6a9	5e 	^ 
	inc hl			;f6aa	23 	# 
	ld d,(hl)			;f6ab	56 	V 
	res 7,d		;f6ac	cb ba 	. . 
	ld (lfb0ch),de		;f6ae	ed 53 0c fb 	. S . . 
	ret			;f6b2	c9 	. 
lf6b3h:
	pop de			;f6b3	d1 	. 
	ei			;f6b4	fb 	. 
	ret			;f6b5	c9 	. 
sub_f6b6h:
	ld (lfa81h+1),hl		;f6b6	22 82 fa 	" . . 
	ld (lf681h),a		;f6b9	32 81 f6 	2 . . 
	ld (lf672h),a		;f6bc	32 72 f6 	2 r . 
	ret			;f6bf	c9 	. 
sub_f6c0h:
	ld a,(lfa80h)		;f6c0	3a 80 fa 	: . . 
	and 001h		;f6c3	e6 01 	. . 
	ld e,a			;f6c5	5f 	_ 
	jr nz,lf6edh		;f6c6	20 25 	  % 
	ld hl,03818h		;f6c8	21 18 38 	! . 8 
lf6cbh:
	ld a,0c9h		;f6cb	3e c9 	> . 
	call sub_f6b6h		;f6cd	cd b6 f6 	. . . 
	ld (ix+017h),000h		;f6d0	dd 36 17 00 	. 6 . . 
	ld a,e			;f6d4	7b 	{ 
sub_f6d5h:
	cp (ix+00bh)		;f6d5	dd be 0b 	. . . 
	ld (lfa81h),a		;f6d8	32 81 fa 	2 . . 
	ret z			;f6db	c8 	. 
	jr c,lf6e2h		;f6dc	38 04 	8 . 
	out (0fdh),a		;f6de	d3 fd 	. . 
	jr lf6e4h		;f6e0	18 02 	. . 
lf6e2h:
	out (0fbh),a		;f6e2	d3 fb 	. . 
lf6e4h:
	ld hl,00900h		;f6e4	21 00 09 	! . . 
lf6e7h:
	dec hl			;f6e7	2b 	+ 
	ld a,l			;f6e8	7d 	} 
	or h			;f6e9	b4 	. 
	jr nz,lf6e7h		;f6ea	20 fb 	  . 
	ret			;f6ec	c9 	. 
lf6edh:
	ld a,(lfb12h)		;f6ed	3a 12 fb 	: . . 
	or a			;f6f0	b7 	. 
	ld hl,0381ah		;f6f1	21 1a 38 	! . 8 
	jr z,lf6cbh		;f6f4	28 d5 	( . 
	ld hl,00160h		;f6f6	21 60 01 	! ` . 
	xor a			;f6f9	af 	. 
	call sub_f6b6h		;f6fa	cd b6 f6 	. . . 
	or (ix+017h)		;f6fd	dd b6 17 	. . . 
	ret nz			;f700	c0 	. 
	dec (ix+017h)		;f701	dd 35 17 	. 5 . 
	jr lf69eh		;f704	18 98 	. . 
sub_f706h:
	push de			;f706	d5 	. 
	ld e,(hl)			;f707	5e 	^ 
	inc hl			;f708	23 	# 
	ld d,(hl)			;f709	56 	V 
	pop hl			;f70a	e1 	. 
	and a			;f70b	a7 	. 
	sbc hl,de		;f70c	ed 52 	. R 
	call sub_f536h		;f70e	cd 36 f5 	. 6 . 
	ld a,0d7h		;f711	3e d7 	> . 
	jp m,lf718h		;f713	fa 18 f7 	. . . 
	xor 008h		;f716	ee 08 	. . 
lf718h:
	ld b,h			;f718	44 	D 
	ld c,l			;f719	4d 	M 
	call sub_f539h		;f71a	cd 39 f5 	. 9 . 
	add hl,hl			;f71d	29 	) 
	add hl,hl			;f71e	29 	) 
	dec hl			;f71f	2b 	+ 
	ret			;f720	c9 	. 
sub_f721h:
	xor 000h		;f721	ee 00 	. . 
	and 001h		;f723	e6 01 	. . 
	ret z			;f725	c8 	. 
	ld hl,lfb13h		;f726	21 13 fb 	! . . 
	ld de,lfb1fh		;f729	11 1f fb 	. . . 
	ld b,00ch		;f72c	06 0c 	. . 
	call lf064h		;f72e	cd 64 f0 	. d . 
	ld hl,lfa7ch		;f731	21 7c fa 	! | . 
	inc b			;f734	04 	. 
	call lf064h		;f735	cd 64 f0 	. d . 
	ld a,(sub_f721h+1)		;f738	3a 22 f7 	: " . 
	cpl			;f73b	2f 	/ 
	ld (sub_f721h+1),a		;f73c	32 22 f7 	2 " . 
	ret z			;f73f	c8 	. 
sub_f740h:
	ld hl,00000h		;f740	21 00 00 	! . . 
	ld (D006_whe_x),hl		;f743	22 aa fa 	" . . 
	ld (D007_whe_y),hl		;f746	22 ac fa 	" . . 
	ld (lfab2h),hl		;f749	22 b2 fa 	" . . 
	ld (lfab4h),hl		;f74c	22 b4 fa 	" . . 
	ld (lfab6h),hl		;f74f	22 b6 fa 	" . . 
	ld (lfabah),hl		;f752	22 ba fa 	" . . 
	ld (lfa8fh+1),hl		;f755	22 90 fa 	" . . 
	ld (lfa92h),hl		;f758	22 92 fa 	" . . 
	ld hl,0008ch		;f75b	21 8c 00 	! . . 
	ld (lfabch),hl		;f75e	22 bc fa 	" . . 
	ld (lfab8h),hl		;f761	22 b8 fa 	" . . 
	ld a,001h		;f764	3e 01 	> . 
	ld (lfa8fh),a		;f766	32 8f fa 	2 . . 
	call sub_f9c9h		;f769	cd c9 f9 	. . . 
	ld (ix+015h),03eh		;f76c	dd 36 15 3e 	. 6 . > 
	jp lf21dh		;f770	c3 1d f2 	. . . 
LIST:
	call sub_fa67h		;f773	cd 67 fa 	. g . 
	ld ix,lfa76h		;f776	dd 21 76 fa 	. ! v . 
	push af			;f77a	f5 	. 
	ld a,001h		;f77b	3e 01 	> . 
	call sub_f721h		;f77d	cd 21 f7 	. ! . 
	pop af			;f780	f1 	. 
	sub 0a5h		;f781	d6 a5 	. . 
	jr c,lf78bh		;f783	38 06 	8 . 
	call 00c10h		;f785	cd 10 0c 	. . . 
	jp lef75h		;f788	c3 75 ef 	. u . 
lf78bh:
	add a,0a5h		;f78b	c6 a5 	. . 
	cp 020h		;f78d	fe 20 	.   
	jr c,lf7a6h		;f78f	38 15 	8 . 
	cp 080h		;f791	fe 80 	. . 
	jr c,lf799h		;f793	38 04 	8 . 
	cp 090h		;f795	fe 90 	. . 
	jr c,lf7a6h		;f797	38 0d 	8 . 
lf799h:
	dec (ix+014h)		;f799	dd 35 14 	. 5 . 
	jr nz,lf7a6h		;f79c	20 08 	  . 
	push af			;f79e	f5 	. 
	call sub_f9c9h		;f79f	cd c9 f9 	. . . 
	dec (ix+014h)		;f7a2	dd 35 14 	. 5 . 
	pop af			;f7a5	f1 	. 
lf7a6h:
	call sub_f7bfh		;f7a6	cd bf f7 	. . . 
	jp lef75h		;f7a9	c3 75 ef 	. u . 
WRITE:
	call sub_fa67h		;f7ac	cd 67 fa 	. g . 
	ld ix,lfa76h		;f7af	dd 21 76 fa 	. ! v . 
	push af			;f7b3	f5 	. 
	xor a			;f7b4	af 	. 
	call sub_f721h		;f7b5	cd 21 f7 	. ! . 
	pop af			;f7b8	f1 	. 
	call sub_f7bfh		;f7b9	cd bf f7 	. . . 
	jp lef75h		;f7bc	c3 75 ef 	. u . 
sub_f7bfh:
	ld (ix+011h),000h		;f7bf	dd 36 11 00 	. 6 . . 
	cp 080h		;f7c3	fe 80 	. . 
	jr c,lf7d7h		;f7c5	38 10 	8 . 
	cp 088h		;f7c7	fe 88 	. . 
	jr nc,lf7d7h		;f7c9	30 0c 	0 . 
	cp 086h		;f7cb	fe 86 	. . 
	jr nc,lf83fh		;f7cd	30 70 	0 p 
	ld (ix+012h),a		;f7cf	dd 77 12 	. w . 
	set 0,(ix+018h)		;f7d2	dd cb 18 c6 	. . . . 
	ret			;f7d6	c9 	. 
lf7d7h:
	bit 0,(ix+018h)		;f7d7	dd cb 18 46 	. . . F 
	jr z,lf803h		;f7db	28 26 	( & 
	push af			;f7dd	f5 	. 
	cp 049h		;f7de	fe 49 	. I 
	jr z,lf7eah		;f7e0	28 08 	( . 
	cp 060h		;f7e2	fe 60 	. ` 
	jr c,lf7fch		;f7e4	38 16 	8 . 
	set 6,(ix+011h)		;f7e6	dd cb 11 f6 	. . . . 
lf7eah:
	set 4,(ix+011h)		;f7ea	dd cb 11 e6 	. . . . 
	ld bc,00003h		;f7ee	01 03 00 	. . . 
	ld hl,0fb79h		;f7f1	21 79 fb 	! y . 
	cpir		;f7f4	ed b1 	. . 
	jr nz,lf7fch		;f7f6	20 04 	  . 
	res 6,(ix+011h)		;f7f8	dd cb 11 b6 	. . . . 
lf7fch:
	ld a,(ix+012h)		;f7fc	dd 7e 12 	. ~ . 
	call sub_f899h		;f7ff	cd 99 f8 	. . . 
	pop af			;f802	f1 	. 
lf803h:
	ld (ix+012h),a		;f803	dd 77 12 	. w . 
	ld (ix+011h),000h		;f806	dd 36 11 00 	. 6 . . 
	call sub_f899h		;f80a	cd 99 f8 	. . . 
	res 0,(ix+018h)		;f80d	dd cb 18 86 	. . . . 
sub_f811h:
	ld a,07fh		;f811	3e 7f 	>  
	in a,(0feh)		;f813	db fe 	. . 
	rrca			;f815	0f 	. 
	ret c			;f816	d8 	. 
	ld a,0feh		;f817	3e fe 	> . 
	in a,(0feh)		;f819	db fe 	. . 
	rrca			;f81b	0f 	. 
	jr c,lf820h		;f81c	38 02 	8 . 
	rst 8			;f81e	cf 	. 
	inc d			;f81f	14 	. 
lf820h:
	ld a,007h		;f820	3e 07 	> . 
	ex af,af'			;f822	08 	. 
lf823h:
	ld a,031h		;f823	3e 31 	> 1 
lf825h:
	dec a			;f825	3d 	= 
	inc hl			;f826	23 	# 
	dec hl			;f827	2b 	+ 
	jr nz,lf825h		;f828	20 fb 	  . 
	ld a,080h		;f82a	3e 80 	> . 
	in a,(0feh)		;f82c	db fe 	. . 
	cpl			;f82e	2f 	/ 
	and 01fh		;f82f	e6 1f 	. . 
	ex af,af'			;f831	08 	. 
	inc a			;f832	3c 	< 
	and 007h		;f833	e6 07 	. . 
	out (0feh),a		;f835	d3 fe 	. . 
	ex af,af'			;f837	08 	. 
	jr z,lf823h		;f838	28 e9 	( . 
	call sub_f45ah		;f83a	cd 5a f4 	. Z . 
	jr sub_f811h		;f83d	18 d2 	. . 
lf83fh:
	jr nz,lf855h		;f83f	20 14 	  . 
	bit 1,(ix+018h)		;f841	dd cb 18 4e 	. . . N 
	ret nz			;f845	c0 	. 
	set 1,(ix+018h)		;f846	dd cb 18 ce 	. . . . 
sub_f84ah:
	call sub_f87dh		;f84a	cd 7d f8 	. } . 
	ld (lfabeh),hl		;f84d	22 be fa 	" . . 
	ld (lfac0h),de		;f850	ed 53 c0 fa 	. S . . 
	ret			;f854	c9 	. 
lf855h:
	call sub_f85dh		;f855	cd 5d f8 	. ] . 
	res 1,(ix+018h)		;f858	dd cb 18 8e 	. . . . 
	ret			;f85c	c9 	. 
sub_f85dh:
	bit 1,(ix+018h)		;f85d	dd cb 18 4e 	. . . N 
	ret z			;f861	c8 	. 
	ld hl,lfabeh		;f862	21 be fa 	! . . 
	ld de,D006_whe_x		;f865	11 aa fa 	. . . 
	call sub_f470h		;f868	cd 70 f4 	. p . 
	call sub_f884h		;f86b	cd 84 f8 	. . . 
	ld (lfaaeh),hl		;f86e	22 ae fa 	" . . 
	ld (lfab0h),de		;f871	ed 53 b0 fa 	. S . . 
	ld a,001h		;f875	3e 01 	> . 
	ld (lfa80h),a		;f877	32 80 fa 	2 . . 
	jp sub_f3e9h		;f87a	c3 e9 f3 	. . . 
sub_f87dh:
	ld de,00000h		;f87d	11 00 00 	. . . 
	ld b,d			;f880	42 	B 
	ld c,e			;f881	4b 	K 
	jr lf889h		;f882	18 05 	. . 
sub_f884h:
	ld a,0feh		;f884	3e fe 	> . 
	call sub_fa2eh		;f886	cd 2e fa 	. . . 
lf889h:
	ld a,0feh		;f889	3e fe 	> . 
	call sub_fa4ah		;f88b	cd 4a fa 	. J . 
	ld hl,(lfab4h)		;f88e	2a b4 fa 	* . . 
	add hl,de			;f891	19 	. 
	ex de,hl			;f892	eb 	. 
	ld hl,(lfab2h)		;f893	2a b2 fa 	* . . 
	pop bc			;f896	c1 	. 
	add hl,bc			;f897	09 	. 
	ret			;f898	c9 	. 
sub_f899h:
	push af			;f899	f5 	. 
	ld a,(lfa8fh)		;f89a	3a 8f fa 	: . . 
	ld (ix+013h),a		;f89d	dd 77 13 	. w . 
	xor a			;f8a0	af 	. 
	ld (lfb12h),a		;f8a1	32 12 fb 	2 . . 
	ld hl,lfab2h		;f8a4	21 b2 fa 	! . . 
	ld de,lfab6h		;f8a7	11 b6 fa 	. . . 
	call sub_f470h		;f8aa	cd 70 f4 	. p . 
	pop af			;f8ad	f1 	. 
	cp 00dh		;f8ae	fe 0d 	. . 
	jp z,sub_f9c9h		;f8b0	ca c9 f9 	. . . 
	cp 020h		;f8b3	fe 20 	.   
	ret c			;f8b5	d8 	. 
	cp 090h		;f8b6	fe 90 	. . 
	jr c,lf8d5h		;f8b8	38 1b 	8 . 
	cp 0a5h		;f8ba	fe a5 	. . 
	jr nc,lf8d3h		;f8bc	30 15 	0 . 
	sub 090h		;f8be	d6 90 	. . 
	ld l,a			;f8c0	6f 	o 
	ld h,000h		;f8c1	26 00 	& . 
	ld de,0fc04h		;f8c3	11 04 fc 	. . . 
	add hl,hl			;f8c6	29 	) 
	add hl,de			;f8c7	19 	. 
	ld a,(hl)			;f8c8	7e 	~ 
	push hl			;f8c9	e5 	. 
	call sub_f7bfh		;f8ca	cd bf f7 	. . . 
	pop hl			;f8cd	e1 	. 
	inc hl			;f8ce	23 	# 
	ld a,(hl)			;f8cf	7e 	~ 
	jp sub_f7bfh		;f8d0	c3 bf f7 	. . . 
lf8d3h:
	ld a,020h		;f8d3	3e 20 	>   
lf8d5h:
	ld (ix+012h),a		;f8d5	dd 77 12 	. w . 
lf8d8h:
	sub 01ah		;f8d8	d6 1a 	. . 
	ld hl,lfc4dh		;f8da	21 4d fc 	! M . 
	ld d,a			;f8dd	57 	W 
	ld b,000h		;f8de	06 00 	. . 
lf8e0h:
	ld e,b			;f8e0	58 	X 
	ld a,(hl)			;f8e1	7e 	~ 
	rlca			;f8e2	07 	. 
	and 01fh		;f8e3	e6 1f 	. . 
	rra			;f8e5	1f 	. 
	inc a			;f8e6	3c 	< 
	jr nc,lf8eah		;f8e7	30 01 	0 . 
	inc e			;f8e9	1c 	. 
lf8eah:
	dec d			;f8ea	15 	. 
	jr z,lf8f2h		;f8eb	28 05 	( . 
	add a,e			;f8ed	83 	. 
	ld c,a			;f8ee	4f 	O 
	add hl,bc			;f8ef	09 	. 
	jr lf8e0h		;f8f0	18 ee 	. . 
lf8f2h:
	ld d,a			;f8f2	57 	W 
	ld (lfa7ah),de		;f8f3	ed 53 7a fa 	. S z . 
	ld a,(hl)			;f8f7	7e 	~ 
	or (ix+011h)		;f8f8	dd b6 11 	. . . 
	ld de,00000h		;f8fb	11 00 00 	. . . 
	rlca			;f8fe	07 	. 
	rlca			;f8ff	07 	. 
	jr nc,lf904h		;f900	30 02 	0 . 
	ld d,0fch		;f902	16 fc 	. . 
lf904h:
	rlca			;f904	07 	. 
	jr nc,lf909h		;f905	30 02 	0 . 
	ld e,0fdh		;f907	1e fd 	. . 
lf909h:
	and (ix+002h)		;f909	dd a6 02 	. . . 
	rlca			;f90c	07 	. 
	jr nc,lf910h		;f90d	30 01 	0 . 
	dec e			;f90f	1d 	. 
lf910h:
	ld (lfa76h),de		;f910	ed 53 76 fa 	. S v . 
lf914h:
	dec (ix+005h)		;f914	dd 35 05 	. 5 . 
	jr z,lf965h		;f917	28 4c 	( L 
	inc hl			;f919	23 	# 
	ld a,(hl)			;f91a	7e 	~ 
	rlca			;f91b	07 	. 
	rlca			;f91c	07 	. 
	rlca			;f91d	07 	. 
	rlca			;f91e	07 	. 
	and 007h		;f91f	e6 07 	. . 
	add a,(ix+000h)		;f921	dd 86 00 	. . . 
	push hl			;f924	e5 	. 
	call sub_fa2eh		;f925	cd 2e fa 	. . . 
	pop hl			;f928	e1 	. 
	push hl			;f929	e5 	. 
	ld a,(hl)			;f92a	7e 	~ 
	and 00fh		;f92b	e6 0f 	. . 
	add a,(ix+001h)		;f92d	dd 86 01 	. . . 
	call sub_fa4ah		;f930	cd 4a fa 	. J . 
	ld hl,(lfab4h)		;f933	2a b4 fa 	* . . 
	add hl,de			;f936	19 	. 
	ld (lfab0h),hl		;f937	22 b0 fa 	" . . 
	pop bc			;f93a	c1 	. 
	ld hl,(lfab2h)		;f93b	2a b2 fa 	* . . 
	add hl,bc			;f93e	09 	. 
	ld (lfaaeh),hl		;f93f	22 ae fa 	" . . 
	pop hl			;f942	e1 	. 
	push hl			;f943	e5 	. 
	ld a,(hl)			;f944	7e 	~ 
	or (ix+003h)		;f945	dd b6 03 	. . . 
	rlca			;f948	07 	. 
	and 001h		;f949	e6 01 	. . 
	ld (lfa80h),a		;f94b	32 80 fa 	2 . . 
	call sub_f3e9h		;f94e	cd e9 f3 	. . . 
	ld a,(lfa7fh)		;f951	3a 7f fa 	:  . 
	cpl			;f954	2f 	/ 
	rlca			;f955	07 	. 
	and 001h		;f956	e6 01 	. . 
	ld (lfa80h),a		;f958	32 80 fa 	2 . . 
	call sub_f3e9h		;f95b	cd e9 f3 	. . . 
	xor a			;f95e	af 	. 
	ld (lfa79h),a		;f95f	32 79 fa 	2 y . 
	pop hl			;f962	e1 	. 
	jr lf914h		;f963	18 af 	. . 
lf965h:
	inc hl			;f965	23 	# 
	ld a,(lfa7ah)		;f966	3a 7a fa 	: z . 
	and a			;f969	a7 	. 
	jr z,lf978h		;f96a	28 0c 	( . 
	ld a,(hl)			;f96c	7e 	~ 
	and 080h		;f96d	e6 80 	. . 
	ld (lfa79h),a		;f96f	32 79 fa 	2 y . 
	ld a,(hl)			;f972	7e 	~ 
	and 07fh		;f973	e6 7f 	.  
	jp lf8d8h		;f975	c3 d8 f8 	. . . 
lf978h:
	ld de,(lfa92h)		;f978	ed 5b 92 fa 	. [ . . 
	ld bc,(lfa8fh+1)		;f97c	ed 4b 90 fa 	. K . . 
	call sub_f9b4h		;f980	cd b4 f9 	. . . 
	ld a,(ix+012h)		;f983	dd 7e 12 	. ~ . 
	dec (ix+013h)		;f986	dd 35 13 	. 5 . 
	jp nz,lf8d8h		;f989	c2 d8 f8 	. . . 
	cp 080h		;f98c	fe 80 	. . 
	jr c,lf998h		;f98e	38 08 	8 . 
	ld hl,lfab6h		;f990	21 b6 fa 	! . . 
	ld de,lfab2h		;f993	11 b2 fa 	. . . 
	jr lf9aah		;f996	18 12 	. . 
lf998h:
	ld a,(lfa76h)		;f998	3a 76 fa 	: v . 
	add a,a			;f99b	87 	. 
	add a,008h		;f99c	c6 08 	. . 
	call sub_fa2eh		;f99e	cd 2e fa 	. . . 
	call sub_f9b4h		;f9a1	cd b4 f9 	. . . 
	ld hl,lfab2h		;f9a4	21 b2 fa 	! . . 
	ld de,lfab6h		;f9a7	11 b6 fa 	. . . 
lf9aah:
	call sub_f470h		;f9aa	cd 70 f4 	. p . 
	xor a			;f9ad	af 	. 
	ld (lfa80h),a		;f9ae	32 80 fa 	2 . . 
	jp sub_f3e9h		;f9b1	c3 e9 f3 	. . . 
sub_f9b4h:
	ld hl,(lfab2h)		;f9b4	2a b2 fa 	* . . 
	add hl,bc			;f9b7	09 	. 
	ld (lfab2h),hl		;f9b8	22 b2 fa 	" . . 
	ld (lfaaeh),hl		;f9bb	22 ae fa 	" . . 
	ld hl,(lfab4h)		;f9be	2a b4 fa 	* . . 
	add hl,de			;f9c1	19 	. 
	ld (lfab4h),hl		;f9c2	22 b4 fa 	" . . 
	ld (lfab0h),hl		;f9c5	22 b0 fa 	" . . 
	ret			;f9c8	c9 	. 
sub_f9c9h:
	call sub_f85dh		;f9c9	cd 5d f8 	. ] . 
	ld a,(lfa7ch)		;f9cc	3a 7c fa 	: | . 
	push af			;f9cf	f5 	. 
	ld hl,(lfb1dh)		;f9d0	2a 1d fb 	* . . 
	call sub_f56ch		;f9d3	cd 6c f5 	. l . 
	ld a,00ah		;f9d6	3e 0a 	> . 
	call sub_f542h		;f9d8	cd 42 f5 	. B . 
	ld de,(lfabch)		;f9db	ed 5b bc fa 	. [ . . 
	add hl,de			;f9df	19 	. 
	ld (lfabch),hl		;f9e0	22 bc fa 	" . . 
	ld (lfab4h),hl		;f9e3	22 b4 fa 	" . . 
	ld (lfab0h),hl		;f9e6	22 b0 fa 	" . . 
	pop af			;f9e9	f1 	. 
	ld hl,(lfb1bh)		;f9ea	2a 1b fb 	* . . 
	call sub_f56ch		;f9ed	cd 6c f5 	. l . 
	ld a,00ah		;f9f0	3e 0a 	> . 
	call sub_f542h		;f9f2	cd 42 f5 	. B . 
	ld de,(lfabah)		;f9f5	ed 5b ba fa 	. [ . . 
	add hl,de			;f9f9	19 	. 
	ld (lfabah),hl		;f9fa	22 ba fa 	" . . 
	ld (lfab2h),hl		;f9fd	22 b2 fa 	" . . 
	ld (lfaaeh),hl		;fa00	22 ae fa 	" . . 
	ld (ix+014h),051h		;fa03	dd 36 14 51 	. 6 . Q 
	xor a			;fa07	af 	. 
	ld (lfa80h),a		;fa08	32 80 fa 	2 . . 
	call sub_f3e9h		;fa0b	cd e9 f3 	. . . 
	bit 1,(ix+018h)		;fa0e	dd cb 18 4e 	. . . N 
	call nz,sub_f84ah		;fa12	c4 4a f8 	. J . 
	ld a,(sub_f721h+1)		;fa15	3a 22 f7 	: " . 
	or a			;fa18	b7 	. 
	ret z			;fa19	c8 	. 
	dec (ix+015h)		;fa1a	dd 35 15 	. 5 . 
	ret nz			;fa1d	c0 	. 
	ld (ix+015h),03eh		;fa1e	dd 36 15 3e 	. 6 . > 
	call sub_f591h		;fa22	cd 91 f5 	. . . 
	call lf820h		;fa25	cd 20 f8 	.   . 
	call sub_f740h		;fa28	cd 40 f7 	. @ . 
	jp lee57h		;fa2b	c3 57 ee 	. W . 
sub_fa2eh:
	push af			;fa2e	f5 	. 
	ld hl,(lfb13h)		;fa2f	2a 13 fb 	* . . 
	call sub_f56ch		;fa32	cd 6c f5 	. l . 
	ld a,006h		;fa35	3e 06 	> . 
	call sub_f542h		;fa37	cd 42 f5 	. B . 
	ld b,h			;fa3a	44 	D 
	ld c,l			;fa3b	4d 	M 
	pop af			;fa3c	f1 	. 
	ld hl,(lfb15h)		;fa3d	2a 15 fb 	* . . 
	call sub_f56ch		;fa40	cd 6c f5 	. l . 
	ld a,006h		;fa43	3e 06 	> . 
	call sub_f542h		;fa45	cd 42 f5 	. B . 
	ex de,hl			;fa48	eb 	. 
	ret			;fa49	c9 	. 
sub_fa4ah:
	push af			;fa4a	f5 	. 
	ld hl,(lfb19h)		;fa4b	2a 19 fb 	* . . 
	call sub_f56ch		;fa4e	cd 6c f5 	. l . 
	ld a,00ah		;fa51	3e 0a 	> . 
	call sub_f542h		;fa53	cd 42 f5 	. B . 
	add hl,de			;fa56	19 	. 
	ex de,hl			;fa57	eb 	. 
	pop af			;fa58	f1 	. 
	ld hl,(lfb17h)		;fa59	2a 17 fb 	* . . 
lfa5ch:
	call sub_f56ch		;fa5c	cd 6c f5 	. l . 
	ld a,00ah		;fa5f	3e 0a 	> . 
	call sub_f542h		;fa61	cd 42 f5 	. B . 
	add hl,bc			;fa64	09 	. 
	ex (sp),hl			;fa65	e3 	. 
	jp (hl)			;fa66	e9 	. 
sub_fa67h:
	ex (sp),hl			;fa67	e3 	. 
	push de			;fa68	d5 	. 
	push bc			;fa69	c5 	. 
	push af			;fa6a	f5 	. 
	ex af,af'			;fa6b	08 	. 
	exx			;fa6c	d9 	. 
	push hl			;fa6d	e5 	. 
	push de			;fa6e	d5 	. 
	push bc			;fa6f	c5 	. 
	push af			;fa70	f5 	. 
	push ix		;fa71	dd e5 	. . 
	ex af,af'			;fa73	08 	. 
	exx			;fa74	d9 	. 
	jp (hl)			;fa75	e9 	. 
lfa76h:
	nop			;fa76	00 	. 
	nop			;fa77	00 	. 
	add a,b			;fa78	80 	. 
lfa79h:
	nop			;fa79	00 	. 
lfa7ah:
	nop			;fa7a	00 	. 
	nop			;fa7b	00 	. 
lfa7ch:
	ld (de),a			;fa7c	12 	. 
	nop			;fa7d	00 	. 
	nop			;fa7e	00 	. 
lfa7fh:
	nop			;fa7f	00 	. 
lfa80h:
	nop			;fa80	00 	. 
lfa81h:
	ld bc,00000h		;fa81	01 00 00 	. . . 
lfa84h:
	nop			;fa84	00 	. 
lfa85h:
	nop			;fa85	00 	. 
	nop			;fa86	00 	. 
	nop			;fa87	00 	. 
	nop			;fa88	00 	. 
	ld bc,008e5h		;fa89	01 e5 08 	. . . 
	exx			;fa8c	d9 	. 
	nop			;fa8d	00 	. 
	nop			;fa8e	00 	. 
lfa8fh:
	ld bc,00000h		;fa8f	01 00 00 	. . . 
lfa92h:
	nop			;fa92	00 	. 
	nop			;fa93	00 	. 
lfa94h:
	nop			;fa94	00 	. 
	nop			;fa95	00 	. 
lfa96h:
	call nz,00009h		;fa96	c4 09 00 	. . . 
	nop			;fa99	00 	. 
lfa9ah:
	sub 006h		;fa9a	d6 06 	. . 
lfa9ch:
	nop			;fa9c	00 	. 
	nop			;fa9d	00 	. 
lfa9eh:
	nop			;fa9e	00 	. 
	nop			;fa9f	00 	. 
lfaa0h:
	call nz,00009h		;faa0	c4 09 00 	. . . 
	nop			;faa3	00 	. 
	sub 006h		;faa4	d6 06 	. . 
lfaa6h:
	call nz,0d609h		;faa6	c4 09 d6 	. . . 
	defb 06                 ;faa9	06

D006_whe_x:
        defb 00                 ;faaa
        defb 00                 ;faab
D007_whe_y:
        defb 00                 ;faac
        defb 00                 ;faad

lfaaeh:
	nop			;faae	00 	. 
	nop			;faaf	00 	. 
lfab0h:
	nop			;fab0	00 	. 
	nop			;fab1	00 	. 
lfab2h:
	nop			;fab2	00 	. 
	nop			;fab3	00 	. 
lfab4h:
	nop			;fab4	00 	. 
	nop			;fab5	00 	. 
lfab6h:
	nop			;fab6	00 	. 
	nop			;fab7	00 	. 
lfab8h:
	nop			;fab8	00 	. 
	nop			;fab9	00 	. 
lfabah:
	nop			;faba	00 	. 
	nop			;fabb	00 	. 
lfabch:
	nop			;fabc	00 	. 
	nop			;fabd	00 	. 
lfabeh:
	nop			;fabe	00 	. 
	nop			;fabf	00 	. 
lfac0h:
	nop			;fac0	00 	. 
	nop			;fac1	00 	.
 
D004_x_pos:
        defw 00000h             ;fac2
D003_y_pos:
        defw 00000h             ;fac4
D008:
        defw 05b00h             ;fac6

lfac8h:
	nop			;fac8	00 	. 
	nop			;fac9	00 	. 
lfacah:
	sub (hl)			;faca	96 	. 
	nop			;facb	00 	. 
	ld h,h			;facc	64 	d 
	add a,b			;facd	80 	. 
lfaceh:
	nop			;face	00 	. 
lfacfh:
	nop			;facf	00 	. 
	ld h,h			;fad0	64 	d 
lfad1h:
	add a,b			;fad1	80 	. 
	daa			;fad2	27 	' 
lfad3h:
	ld bc,00064h		;fad3	01 64 00 	. d . 
	dec b			;fad6	05 	. 
lfad7h:
	nop			;fad7	00 	. 
lfad8h:
	nop			;fad8	00 	. 
	nop			;fad9	00 	. 
	adc a,h			;fada	8c 	. 
	add a,h			;fadb	84 	. 
	ld a,h			;fadc	7c 	| 
	ld (hl),h			;fadd	74 	t 
	ld l,h			;fade	6c 	l 
	ld h,h			;fadf	64 	d 
	ld e,(hl)			;fae0	5e 	^ 
	ld e,b			;fae1	58 	X 
	ld d,h			;fae2	54 	T 
	ld d,b			;fae3	50 	P 
	ld c,h			;fae4	4c 	L 
	ld c,b			;fae5	48 	H 
	ld b,h			;fae6	44 	D 
	ld b,b			;fae7	40 	@ 
	inc a			;fae8	3c 	< 
	jr c,lfb1fh		;fae9	38 34 	8 4 
	ld (02e30h),a		;faeb	32 30 2e 	2 0 . 
	inc l			;faee	2c 	, 
	ld hl,(02628h)		;faef	2a 28 26 	* ( & 
	inc h			;faf2	24 	$ 
	ld (01e20h),hl		;faf3	22 20 1e 	"   . 
	inc e			;faf6	1c 	. 
	ld a,(de)			;faf7	1a 	. 
	jr lfb10h		;faf8	18 16 	. . 
	dec d			;fafa	15 	. 
	inc d			;fafb	14 	. 
	inc de			;fafc	13 	. 
	nop			;fafd	00 	. 
	nop			;fafe	00 	. 
	ld bc,00000h		;faff	01 00 00 	. . . 
	nop			;fb02	00 	. 
	nop			;fb03	00 	. 
	nop			;fb04	00 	. 
	nop			;fb05	00 	. 
	nop			;fb06	00 	. 
	nop			;fb07	00 	. 
	nop			;fb08	00 	. 
	nop			;fb09	00 	. 
	nop			;fb0a	00 	. 
	nop			;fb0b	00 	. 
lfb0ch:
	nop			;fb0c	00 	. 
	nop			;fb0d	00 	. 
lfb0eh:
	nop			;fb0e	00 	. 
	nop			;fb0f	00 	. 
lfb10h:
	ld l,0fch		;fb10	2e fc 	. . 
lfb12h:
	nop			;fb12	00 	. 
lfb13h:
	jr lfb15h		;fb13	18 00 	. . 
lfb15h:
	nop			;fb15	00 	. 
	nop			;fb16	00 	. 
lfb17h:
	nop			;fb17	00 	. 
	nop			;fb18	00 	. 
lfb19h:
	jr z,lfb1bh		;fb19	28 00 	( . 
lfb1bh:
	nop			;fb1b	00 	. 
	nop			;fb1c	00 	. 
lfb1dh:
	ret c			;fb1d	d8 	. 
	rst 38h			;fb1e	ff 	. 
lfb1fh:
	nop			;fb1f	00 	. 
	nop			;fb20	00 	. 
	rrca			;fb21	0f 	. 
	nop			;fb22	00 	. 
	rst 20h			;fb23	e7 	. 
	rst 38h			;fb24	ff 	. 
	nop			;fb25	00 	. 
	nop			;fb26	00 	. 
	add hl,de			;fb27	19 	. 
	nop			;fb28	00 	. 
	nop			;fb29	00 	. 
	nop			;fb2a	00 	. 
lfb2bh:
	djnz lfb2dh		;fb2b	10 00 	. . 
lfb2dh:
	ld e,e			;fb2d	5b 	[ 
	rst 28h			;fb2e	ef 	. 
	ld d,b			;fb2f	50 	P 
lfb30h:
	sub (hl)			;fb30	96 	. 
	nop			;fb31	00 	. 
	ld h,h			;fb32	64 	d 
	add a,b			;fb33	80 	. 
	dec b			;fb34	05 	. 
	nop			;fb35	00 	. 
	ld h,h			;fb36	64 	d 
	add a,b			;fb37	80 	. 
	daa			;fb38	27 	' 
	ld bc,00064h		;fb39	01 64 00 	. d . 
	dec b			;fb3c	05 	. 
	nop			;fb3d	00 	. 
	ld h,h			;fb3e	64 	d 
	add a,b			;fb3f	80 	. 
lfb40h:
	adc a,h			;fb40	8c 	. 
	add a,h			;fb41	84 	. 
	ld a,h			;fb42	7c 	| 
	ld (hl),h			;fb43	74 	t 
	ld l,h			;fb44	6c 	l 
	ld h,h			;fb45	64 	d 
	ld e,(hl)			;fb46	5e 	^ 
	ld e,b			;fb47	58 	X 
	ld d,h			;fb48	54 	T 
	ld d,b			;fb49	50 	P 
	ld c,h			;fb4a	4c 	L 
	ld c,b			;fb4b	48 	H 
	ld b,h			;fb4c	44 	D 
	ld b,b			;fb4d	40 	@ 
	inc a			;fb4e	3c 	< 
	jr c,lfb85h		;fb4f	38 34 	8 4 
	ld (02e30h),a		;fb51	32 30 2e 	2 0 . 
	inc l			;fb54	2c 	, 
	ld hl,(02628h)		;fb55	2a 28 26 	* ( & 
	inc h			;fb58	24 	$ 
	ld (01e20h),hl		;fb59	22 20 1e 	"   . 
	inc e			;fb5c	1c 	. 
	ld a,(de)			;fb5d	1a 	. 
	jr lfb76h		;fb5e	18 16 	. . 
	dec d			;fb60	15 	. 
	inc d			;fb61	14 	. 
	inc de			;fb62	13 	. 
	ld (de),a			;fb63	12 	. 
	ld de,00f10h		;fb64	11 10 0f 	. . . 
	ld c,00dh		;fb67	0e 0d 	. . 
	inc c			;fb69	0c 	. 
	dec bc			;fb6a	0b 	. 
	ld a,(bc)			;fb6b	0a 	. 
	add hl,bc			;fb6c	09 	. 
	ex af,af'			;fb6d	08 	. 
	rlca			;fb6e	07 	. 
	ld b,005h		;fb6f	06 05 	. . 
	dec b			;fb71	05 	. 
	inc b			;fb72	04 	. 
	inc b			;fb73	04 	. 
	inc bc			;fb74	03 	. 
	inc bc			;fb75	03 	. 
lfb76h:
	ld (bc),a			;fb76	02 	. 
	ld (bc),a			;fb77	02 	. 
	ld bc,06c64h		;fb78	01 64 6c 	. d l 
	ld (hl),h			;fb7b	74 	t 
lfb7ch:
	ld d,(hl)			;fb7c	56 	V 
	ld b,c			;fb7d	41 	A 
	ld (bc),a			;fb7e	02 	. 
	rlca			;fb7f	07 	. 
	ret p			;fb80	f0 	. 
	ld d,(hl)			;fb81	56 	V 
	ld d,d			;fb82	52 	R 
	ld (bc),a			;fb83	02 	. 
	dec bc			;fb84	0b 	. 
lfb85h:
	ret p			;fb85	f0 	. 
	ld c,l			;fb86	4d 	M 
	ld b,c			;fb87	41 	A 
	ld (bc),a			;fb88	02 	. 
	rrca			;fb89	0f 	. 
lfb8ah:
	ret p			;fb8a	f0 	. 
	ld c,l			;fb8b	4d 	M 
	ld d,d			;fb8c	52 	R 
	ld (bc),a			;fb8d	02 	. 
	ld (de),a			;fb8e	12 	. 
	ret p			;fb8f	f0 	. 
	ld d,b			;fb90	50 	P 
	ld b,c			;fb91	41 	A 
	ld (bc),a			;fb92	02 	. 
	dec d			;fb93	15 	. 
	ret p			;fb94	f0 	. 
	ld d,b			;fb95	50 	P 
	ld d,d			;fb96	52 	R 
	ld (bc),a			;fb97	02 	. 
	jr z,lfb8ah		;fb98	28 f0 	( . 
lfb9ah:
	ld b,e			;fb9a	43 	C 
	ld d,e			;fb9b	53 	S 
	ld (bc),a			;fb9c	02 	. 
	ld l,(hl)			;fb9d	6e 	n 
	ret p			;fb9e	f0 	. 
	ld b,h			;fb9f	44 	D 
	ld d,a			;fba0	57 	W 
	inc b			;fba1	04 	. 
	ld (hl),0f0h		;fba2	36 f0 	6 . 
	ld d,(hl)			;fba4	56 	V 
	ld d,a			;fba5	57 	W 
	nop			;fba6	00 	. 
	djnz lfb9ah		;fba7	10 f1 	. . 
	ld c,a			;fba9	4f 	O 
	ld b,a			;fbaa	47 	G 
	ld (bc),a			;fbab	02 	. 
	cpl			;fbac	2f 	/ 
	ret p			;fbad	f0 	. 
	ld d,b			;fbae	50 	P 
	ld d,e			;fbaf	53 	S 
	nop			;fbb0	00 	. 
	ld (04ef2h),hl		;fbb1	22 f2 4e 	" . N 
	ld d,e			;fbb4	53 	S 
	nop			;fbb5	00 	. 
	dec e			;fbb6	1d 	. 
	jp p,04353h		;fbb7	f2 53 43 	. S C 
	ld bc,lf151h		;fbba	01 51 f1 	. Q . 
	ld b,e			;fbbd	43 	C 
	ld d,d			;fbbe	52 	R 
	ld bc,lf212h		;fbbf	01 12 f2 	. . . 
	ld b,c			;fbc2	41 	A 
	ld b,e			;fbc3	43 	C 
	inc bc			;fbc4	03 	. 
	ld (hl),b			;fbc5	70 	p 
	pop af			;fbc6	f1 	. 
	ld d,d			;fbc7	52 	R 
	jr z,lfbcbh		;fbc8	28 01 	( . 
	daa			;fbca	27 	' 
lfbcbh:
	jp p,02952h		;fbcb	f2 52 29 	. R ) 
	nop			;fbce	00 	. 
	inc sp			;fbcf	33 	3 
	jp p,0534ch		;fbd0	f2 4c 53 	. L S 
	ld bc,lf251h		;fbd3	01 51 f2 	. Q . 
	ld c,b			;fbd6	48 	H 
	ld c,l			;fbd7	4d 	M 
	nop			;fbd8	00 	. 
	sub c			;fbd9	91 	. 
	push af			;fbda	f5 	. 
	ld c,c			;fbdb	49 	I 
	ld b,e			;fbdc	43 	C 
	nop			;fbdd	00 	. 
	push af			;fbde	f5 	. 
	jp p,04e49h		;fbdf	f2 49 4e 	. I N 
	nop			;fbe2	00 	. 
	ld d,a			;fbe3	57 	W 
	xor 044h		;fbe4	ee 44 	. D 
	ld b,a			;fbe6	47 	G 
	ld bc,lf163h		;fbe7	01 63 f1 	. c . 
	ld b,e			;fbea	43 	C 
	ld d,b			;fbeb	50 	P 
	ld bc,lf31ah		;fbec	01 1a f3 	. . . 
	ld c,h			;fbef	4c 	L 
	ld d,h			;fbf0	54 	T 
	ld bc,lf258h		;fbf1	01 58 f2 	. X . 
	ld d,l			;fbf4	55 	U 
	ld c,h			;fbf5	4c 	L 
	nop			;fbf6	00 	. 
	sub d			;fbf7	92 	. 
	jp p,04c46h		;fbf8	f2 46 4c 	. F L 
	inc bc			;fbfb	03 	. 
	adc a,0f2h		;fbfc	ce f2 	. . 
	ld c,l			;fbfe	4d 	M 
	ld b,(hl)			;fbff	46 	F 
	nop			;fc00	00 	. 
	ld l,b			;fc01	68 	h 
	call pe,08000h		;fc02	ec 00 80 	. . . 
	ld h,c			;fc05	61 	a 
	add a,c			;fc06	81 	. 
	ld d,e			;fc07	53 	S 
	add a,c			;fc08	81 	. 
	ld h,e			;fc09	63 	c 
	add a,c			;fc0a	81 	. 
	ld b,e			;fc0b	43 	C 
	add a,c			;fc0c	81 	. 
	ld h,l			;fc0d	65 	e 
	add a,b			;fc0e	80 	. 
	ld b,l			;fc0f	45 	E 
	add a,c			;fc10	81 	. 
	ld b,l			;fc11	45 	E 
	add a,b			;fc12	80 	. 
	ld c,c			;fc13	49 	I 
	add a,b			;fc14	80 	. 
	ld l,c			;fc15	69 	i 
	add a,b			;fc16	80 	. 
	ld d,l			;fc17	55 	U 
	add a,b			;fc18	80 	. 
	ld a,c			;fc19	79 	y 
	add a,b			;fc1a	80 	. 
	ld e,c			;fc1b	59 	Y 
	add a,c			;fc1c	81 	. 
	ld a,d			;fc1d	7a 	z 
	add a,c			;fc1e	81 	. 
	ld e,d			;fc1f	5a 	Z 
	add a,b			;fc20	80 	. 
	ld (hl),l			;fc21	75 	u 
	add a,c			;fc22	81 	. 
	ld d,d			;fc23	52 	R 
	add a,b			;fc24	80 	. 
	ld b,c			;fc25	41 	A 
	add a,c			;fc26	81 	. 
	ld (hl),d			;fc27	72 	r 
	add a,c			;fc28	81 	. 
	ld (hl),e			;fc29	73 	s 
	add a,b			;fc2a	80 	. 
	ld h,l			;fc2b	65 	e 
	add a,e			;fc2c	83 	. 
	ld (hl),l			;fc2d	75 	u 
lfc2eh:
	nop			;fc2e	00 	. 
	ld d,l			;fc2f	55 	U 
	add a,b			;fc30	80 	. 
	ld a,c			;fc31	79 	y 
	add a,b			;fc32	80 	. 
	ld e,c			;fc33	59 	Y 
	add a,c			;fc34	81 	. 
	ld a,d			;fc35	7a 	z 
	add a,c			;fc36	81 	. 
	ld e,d			;fc37	5a 	Z 
	add a,b			;fc38	80 	. 
	ld (hl),l			;fc39	75 	u 
	add a,c			;fc3a	81 	. 
	ld d,d			;fc3b	52 	R 
	add a,b			;fc3c	80 	. 
	ld b,c			;fc3d	41 	A 
	add a,c			;fc3e	81 	. 
	ld (hl),d			;fc3f	72 	r 
	add a,c			;fc40	81 	. 
	ld (hl),e			;fc41	73 	s 
	add a,b			;fc42	80 	. 
	ld h,l			;fc43	65 	e 
	add a,e			;fc44	83 	. 
	ld (hl),l			;fc45	75 	u 
	nop			;fc46	00 	. 
	adc a,(hl)			;fc47	8e 	. 
	xor 0a2h		;fc48	ee a2 	. . 
	rst 30h			;fc4a	f7 	. 
	ld l,c			;fc4b	69 	i 
	rst 30h			;fc4c	f7 	. 
lfc4dh:
	ld b,046h		;fc4d	06 46 	. F 
	call po,0c0e2h		;fc4f	e4 e2 c0 	. . . 
	and b			;fc52	a0 	. 
	add a,d			;fc53	82 	. 
	ld (bc),a			;fc54	02 	. 
	ld a,(005b0h)		;fc55	3a b0 05 	: . . 
	ld h,(hl)			;fc58	66 	f 
	ret pe			;fc59	e8 	. 
	jp z,088aah		;fc5a	ca aa 88 	. . . 
	dec d			;fc5d	15 	. 
	inc d			;fc5e	14 	. 
	and l			;fc5f	a5 	. 
	push bc			;fc60	c5 	. 
	call nc,018d0h		;fc61	d4 d0 18 	. . . 
	inc d			;fc64	14 	. 
	and l			;fc65	a5 	. 
	push bc			;fc66	c5 	. 
	call nc,0c0d1h		;fc67	d4 d1 c0 	. . . 
	and b			;fc6a	a0 	. 
	sub c			;fc6b	91 	. 
	nop			;fc6c	00 	. 
	sub d			;fc6d	92 	. 
	ld a,(02eb3h)		;fc6e	3a b3 2e 	: . . 
	inc b			;fc71	04 	. 
	daa			;fc72	27 	' 
	xor d			;fc73	aa 	. 
	ld c,d			;fc74	4a 	J 
	rst 0			;fc75	c7 	. 
	add a,h			;fc76	84 	. 
	ld c,b			;fc77	48 	H 
	jp nz,0a822h		;fc78	c2 22 a8 	. " . 
	dec a			;fc7b	3d 	= 
	adc a,b			;fc7c	88 	. 
	ld (de),a			;fc7d	12 	. 
	or c			;fc7e	b1 	. 
	jp nc,096d4h		;fc7f	d2 d4 96 	. . . 
	sbc a,b			;fc82	98 	. 
	cp c			;fc83	b9 	. 
	ret c			;fc84	d8 	. 
	inc e			;fc85	1c 	. 
	adc a,d			;fc86	8a 	. 
	daa			;fc87	27 	' 
	sbc a,b			;fc88	98 	. 
	xor c			;fc89	a9 	. 
	cp b			;fc8a	b8 	. 
	and a			;fc8b	a7 	. 
	ld (0d2c3h),a		;fc8c	32 c3 d2 	2 . . 
	pop bc			;fc8f	c1 	. 
lfc90h:
	or d			;fc90	b2 	. 
	cpl			;fc91	2f 	/ 
	dec bc			;fc92	0b 	. 
	ld h,h			;fc93	64 	d 
	or b			;fc94	b0 	. 
	and b			;fc95	a0 	. 
	add a,d			;fc96	82 	. 
	add a,e			;fc97	83 	. 
	rst 10h			;fc98	d7 	. 
	exx			;fc99	d9 	. 
	cp d			;fc9a	ba 	. 
	sbc a,c			;fc9b	99 	. 
	sub a			;fc9c	97 	. 
	ret po			;fc9d	e0 	. 
	ld (de),a			;fc9e	12 	. 
	ld a,(006b7h)		;fc9f	3a b7 06 	: . . 
	ld c,d			;fca2	4a 	J 
	cp c			;fca3	b9 	. 
	and a			;fca4	a7 	. 
	and e			;fca5	a3 	. 
	or c			;fca6	b1 	. 
	ret nz			;fca7	c0 	. 
	ld b,02ah		;fca8	06 2a 	. * 
	cp c			;fcaa	b9 	. 
	rst 0			;fcab	c7 	. 
	jp 0a0b1h		;fcac	c3 b1 a0 	. . . 
	ld b,032h		;fcaf	06 32 	. 2 
	cp b			;fcb1	b8 	. 
	ld d,a			;fcb2	57 	W 
	sub e			;fcb3	93 	. 
	rla			;fcb4	17 	. 
	out (082h),a		;fcb5	d3 82 	. . 
	ld (02db8h),a		;fcb7	32 b8 2d 	2 . - 
	ld d,d			;fcba	52 	R 
	ld (002b5h),hl		;fcbb	22 b5 02 	" . . 
	dec b			;fcbe	05 	. 
	push hl			;fcbf	e5 	. 
	ld (de),a			;fcc0	12 	. 
	ld sp,002b0h		;fcc1	31 b0 02 	1 . . 
	nop			;fcc4	00 	. 
	jp pe,00082h		;fcc5	ea 82 00 	. . . 
	jp pe,0054fh		;fcc8	ea 4f 05 	. O . 
	rla			;fccb	17 	. 
	cp d			;fccc	ba 	. 
	or b			;fccd	b0 	. 
	jr nz,lfc90h		;fcce	20 c0 	  . 
	add a,d			;fcd0	82 	. 
lfcd1h:
	ld h,b			;fcd1	60 	` 
	add a,b			;fcd2	80 	. 
	sbc a,l			;fcd3	9d 	. 
	add a,d			;fcd4	82 	. 
	ld a,(bc)			;fcd5	0a 	. 
	jp pe,0059bh		;fcd6	ea 9b 05 	. . . 
	ld c,d			;fcd9	4a 	J 
	add a,e			;fcda	83 	. 
	ex (sp),hl			;fcdb	e3 	. 
	ld b,(hl)			;fcdc	46 	F 
	ret nz			;fcdd	c0 	. 
	add a,e			;fcde	83 	. 
	ld l,d			;fcdf	6a 	j 
	adc a,d			;fce0	8a 	. 
	add a,(hl)			;fce1	86 	. 
	sbc a,e			;fce2	9b 	. 
	add hl,bc			;fce3	09 	. 
	ld c,e			;fce4	4b 	K 
	add a,e			;fce5	83 	. 
	sub c			;fce6	91 	. 
	or b			;fce7	b0 	. 
	pop de			;fce8	d1 	. 
	ex (sp),hl			;fce9	e3 	. 
	push de			;fcea	d5 	. 
	or (hl)			;fceb	b6 	. 
	sub l			;fcec	95 	. 
	inc b			;fced	04 	. 
	add hl,bc			;fcee	09 	. 
	adc a,d			;fcef	8a 	. 
	jp pe,00d80h		;fcf0	ea 80 0d 	. . . 
	ld d,l			;fcf3	55 	U 
	sub a			;fcf4	97 	. 
	sbc a,c			;fcf5	99 	. 
	cp d			;fcf6	ba 	. 
	exx			;fcf7	d9 	. 
	rst 10h			;fcf8	d7 	. 
	sub l			;fcf9	95 	. 
	add a,e			;fcfa	83 	. 
	sub c			;fcfb	91 	. 
	or b			;fcfc	b0 	. 
	pop de			;fcfd	d1 	. 
	ex (sp),hl			;fcfe	e3 	. 
	push de			;fcff	d5 	. 
	ld c,c			;fd00	49 	I 
	inc hl			;fd01	23 	# 
	ex de,hl			;fd02	eb 	. 
	cp (ix-063h)		;fd03	dd be 9d 	. . . 
	adc a,e			;fd06	8b 	. 
	sbc a,c			;fd07	99 	. 
	cp b			;fd08	b8 	. 
	exx			;fd09	d9 	. 
	ld (de),a			;fd0a	12 	. 
	dec (hl)			;fd0b	35 	5 
	ld sp,03591h		;fd0c	31 91 35 	1 . 5 
	inc l			;fd0f	2c 	, 
	inc bc			;fd10	03 	. 
	ld l,b			;fd11	68 	h 
	add a,l			;fd12	85 	. 
	jp po,00604h		;fd13	e2 04 06 	. . . 
	and 064h		;fd16	e6 64 	. d 
	add a,h			;fd18	84 	. 
	inc bc			;fd19	03 	. 
	ex af,af'			;fd1a	08 	. 
	push hl			;fd1b	e5 	. 
	add a,d			;fd1c	82 	. 
	add a,h			;fd1d	84 	. 
	jr nc,lfcd1h		;fd1e	30 b1 	0 . 
	inc sp			;fd20	33 	3 
	or h			;fd21	b4 	. 
lfd22h:
	sbc a,l			;fd22	9d 	. 
	adc a,c			;fd23	89 	. 
	ld d,e			;fd24	53 	S 
	sub 0c7h		;fd25	d6 c7 	. . 
	and a			;fd27	a7 	. 
	sub (hl)			;fd28	96 	. 
	sub e			;fd29	93 	. 
	and d			;fd2a	a2 	. 
	jp nz,0c3e4h		;fd2b	c2 e4 c3 	. . . 
	rlca			;fd2e	07 	. 
	nop			;fd2f	00 	. 
	sub h			;fd30	94 	. 
	cp d			;fd31	ba 	. 
	call nc,014e0h		;fd32	d4 e0 14 	. . . 
	call nc,05584h		;fd35	d4 84 55 	. . U 
	call po,0d0e1h		;fd38	e4 e1 d0 	. . . 
	ret nc			;fd3b	d0 	. 
	ex af,af'			;fd3c	08 	. 
	ld l,b			;fd3d	68 	h 
	jp z,088aah		;fd3e	ca aa 88 	. . . 
	add a,d			;fd41	82 	. 
	and b			;fd42	a0 	. 
	ret nz			;fd43	c0 	. 
	jp po,00007h		;fd44	e2 07 00 	. . . 
	ret nz			;fd47	c0 	. 
	jp po,0cae8h		;fd48	e2 e8 ca 	. . . 
	adc a,d			;fd4b	8a 	. 
	add a,b			;fd4c	80 	. 
	add a,d			;fd4d	82 	. 
	ld h,b			;fd4e	60 	` 
	add a,b			;fd4f	80 	. 
	ld b,(hl)			;fd50	46 	F 
	dec b			;fd51	05 	. 
	nop			;fd52	00 	. 
	adc a,d			;fd53	8a 	. 
	jp pe,0c505h		;fd54	ea 05 c5 	. . . 
	add a,e			;fd57	83 	. 
	ld b,l			;fd58	45 	E 
	push hl			;fd59	e5 	. 
	ret po			;fd5a	e0 	. 
	ld b,e			;fd5b	43 	C 
	ld b,000h		;fd5c	06 00 	. . 
	adc a,d			;fd5e	8a 	. 
	ld l,d			;fd5f	6a 	j 
	ret po			;fd60	e0 	. 
	dec b			;fd61	05 	. 
	push hl			;fd62	e5 	. 
	ld d,020h		;fd63	16 20 	.   
	ret nz			;fd65	c0 	. 
	jr nc,lfd22h		;fd66	30 ba 	0 . 
	ld hl,(005cah)		;fd68	2a ca 05 	* . . 
	ld de,0c0a0h		;fd6b	11 a0 c0 	. . . 
	pop de			;fd6e	d1 	. 
	jp c,00a06h		;fd6f	da 06 0a 	. . . 
	add a,b			;fd72	80 	. 
	inc b			;fd73	04 	. 
	jp pe,0e015h		;fd74	ea 15 e0 	. . . 
	inc bc			;fd77	03 	. 
	ld a,(bc)			;fd78	0a 	. 
	add a,b			;fd79	80 	. 
	ret po			;fd7a	e0 	. 
	dec b			;fd7b	05 	. 
	nop			;fd7c	00 	. 
	adc a,d			;fd7d	8a 	. 
	or l			;fd7e	b5 	. 
	jp pe,004e0h		;fd7f	ea e0 04 	. . . 
	nop			;fd82	00 	. 
	adc a,d			;fd83	8a 	. 
	ret po			;fd84	e0 	. 
	jp pe,06281h		;fd85	ea 81 62 	. . b 
	jp 00007h		;fd88	c3 07 00 	. . . 
	adc a,d			;fd8b	8a 	. 
	jp c,0e6e9h		;fd8c	da e9 e6 	. . . 
	push de			;fd8f	d5 	. 
	add a,l			;fd90	85 	. 
	add a,d			;fd91	82 	. 
	inc sp			;fd92	33 	3 
	ret po			;fd93	e0 	. 
	ld c,a			;fd94	4f 	O 
	add a,d			;fd95	82 	. 
	dec (hl)			;fd96	35 	5 
	ret po			;fd97	e0 	. 
	ld d,b			;fd98	50 	P 
	inc c			;fd99	0c 	. 
	ld (bc),a			;fd9a	02 	. 
	and b			;fd9b	a0 	. 
	ret nz			;fd9c	c0 	. 
	jp po,0d4e3h		;fd9d	e2 e3 d4 	. . . 
	sub (hl)			;fda0	96 	. 
	add a,a			;fda1	87 	. 
	adc a,b			;fda2	88 	. 
	xor d			;fda3	aa 	. 
	jp z,082e8h		;fda4	ca e8 82 	. . . 
	ld a,(bc)			;fda7	0a 	. 
	jp pe,0061ch		;fda8	ea 1c 06 	. . . 
	ld a,(bc)			;fdab	0a 	. 
	add a,d			;fdac	82 	. 
	and b			;fdad	a0 	. 
	ret nz			;fdae	c0 	. 
	jp po,003eah		;fdaf	e2 ea 03 	. . . 
	ld a,(bc)			;fdb2	0a 	. 
	or b			;fdb3	b0 	. 
	jp pe,00a05h		;fdb4	ea 05 0a 	. . . 
	sub b			;fdb7	90 	. 
	or a			;fdb8	b7 	. 
	ret nc			;fdb9	d0 	. 
	jp pe,00a82h		;fdba	ea 82 0a 	. . . 
	ret po			;fdbd	e0 	. 
	cpl			;fdbe	2f 	/ 
	dec b			;fdbf	05 	. 
	ld a,(bc)			;fdc0	0a 	. 
lfdc1h:
	or l			;fdc1	b5 	. 
	or b			;fdc2	b0 	. 
	dec (hl)			;fdc3	35 	5 
	jp pe,00a04h		;fdc4	ea 04 0a 	. . . 
	jp pe,0e080h		;fdc7	ea 80 e0 	. . . 
	inc b			;fdca	04 	. 
	ld c,d			;fdcb	4a 	J 
	xor d			;fdcc	aa 	. 
	and b			;fdcd	a0 	. 
	ret nz			;fdce	c0 	. 
	ld (bc),a			;fdcf	02 	. 
	ld a,(bc)			;fdd0	0a 	. 
	ret po			;fdd1	e0 	. 
	inc b			;fdd2	04 	. 
	ld hl,(0c0cah)		;fdd3	2a ca c0 	* . . 
	and b			;fdd6	a0 	. 
	add a,e			;fdd7	83 	. 
	ld d,0bah		;fdd8	16 ba 	. . 
lfddah:
	sub 01ch		;fdda	d6 1c 	. . 
	ld bc,009e0h		;fddc	01 e0 09 	. . . 
	djnz lfdc1h		;fddf	10 e0 	. . 
	ld d,0b6h		;fde1	16 b6 	. . 
	ld l,c			;fde3	69 	i 
	jp c,0a9bah		;fde4	da ba a9 	. . . 
	and b			;fde7	a0 	. 
	sub a			;fde8	97 	. 
	ld d,e			;fde9	53 	S 
	and e			;fdea	a3 	. 
	sub d			;fdeb	92 	. 
	sub c			;fdec	91 	. 
	and b			;fded	a0 	. 
	ret nz			;fdee	c0 	. 
	pop de			;fdef	d1 	. 
	ld e,092h		;fdf0	1e 92 	. . 
lfdf2h:
	ld a,(de)			;fdf2	1a 	. 
	sub b			;fdf3	90 	. 
	rra			;fdf4	1f 	. 
lfdf5h:
	jr $+86		;fdf5	18 54 	. T 
	push bc			;fdf7	c5 	. 
	and l			;fdf8	a5 	. 
	sub h			;fdf9	94 	. 
	sub c			;fdfa	91 	. 
	and b			;fdfb	a0 	. 
	ret nz			;fdfc	c0 	. 
	pop de			;fdfd	d1 	. 
	sub d			;fdfe	92 	. 
	ld d,b			;fdff	50 	P 
	jp c,09263h		;fe00	da 63 92 	. c . 
	inc de			;fe03	13 	. 
	out (0e3h),a		;fe04	d3 e3 	. . 
	ld d,(hl)			;fe06	56 	V 
	add hl,hl			;fe07	29 	) 
	ret			;fe08	c9 	. 
	ld e,(hl)			;fe09	5e 	^ 
	adc a,0bdh		;fe0a	ce bd 	. . 
	or b			;fe0c	b0 	. 
	push de			;fe0d	d5 	. 
	ld de,0c0a0h		;fe0e	11 a0 c0 	. . . 
	pop de			;fe11	d1 	. 
	exx			;fe12	d9 	. 
	ld h,e			;fe13	63 	c 
	sub d			;fe14	92 	. 
	ld a,(de)			;fe15	1a 	. 
	sub b			;fe16	90 	. 
	ld e,017h		;fe17	1e 17 	. . 
	scf			;fe19	37 	7 
	cp b			;fe1a	b8 	. 
	inc h			;fe1b	24 	$ 
	or l			;fe1c	b5 	. 
	or b			;fe1d	b0 	. 
	jr nz,$-62		;fe1e	20 c0 	  . 
	ld d,a			;fe20	57 	W 
	dec sp			;fe21	3b 	; 
	jr z,$-69		;fe22	28 b9 	( . 
	or c			;fe24	b1 	. 
lfe25h:
	and b			;fe25	a0 	. 
	sub b			;fe26	90 	. 
	add a,c			;fe27	81 	. 
	ld d,01ah		;fe28	16 1a 	. . 
	sub b			;fe2a	90 	. 
	ld de,033d5h		;fe2b	11 d5 33 	. . 3 
	ret nc			;fe2e	d0 	. 
	dec d			;fe2f	15 	. 
	ld hl,(0b0bah)		;fe30	2a ba b0 	* . . 
	jr nz,lfdf5h		;fe33	20 c0 	  . 
	inc c			;fe35	0c 	. 
	nop			;fe36	00 	. 
	add a,l			;fe37	85 	. 
	inc b			;fe38	04 	. 
	sub l			;fe39	95 	. 
	and l			;fe3a	a5 	. 
	or h			;fe3b	b4 	. 
	jr nc,lfdf2h		;fe3c	30 b4 	0 . 
	push bc			;fe3e	c5 	. 
	push de			;fe3f	d5 	. 
	call po,092e0h		;fe40	e4 e0 92 	. . . 
	djnz lfddah		;fe43	10 95 	. . 
	ld e,091h		;fe45	1e 91 	. . 
	ld d,c			;fe47	51 	Q 
	ex (sp),hl			;fe48	e3 	. 
	jp nc,09910h		;fe49	d2 10 99 	. . . 
	rra			;fe4c	1f 	. 
	jp nc,0d950h		;fe4d	d2 50 d9 	. P . 
	ld h,e			;fe50	63 	c 
	ld d,010h		;fe51	16 10 	. . 
	sub l			;fe53	95 	. 
lfe54h:
	inc d			;fe54	14 	. 
	and l			;fe55	a5 	. 
	push bc			;fe56	c5 	. 
	call nc,0111ah		;fe57	d4 1a 11 	. . . 
	and b			;fe5a	a0 	. 
	ret nz			;fe5b	c0 	. 
	pop de			;fe5c	d1 	. 
	jp nc,09493h		;fe5d	d2 93 94 	. . . 
	and l			;fe60	a5 	. 
	push bc			;fe61	c5 	. 
	call nc,01517h		;fe62	d4 17 15 	. . . 
	or l			;fe65	b5 	. 
	ld hl,(0b0a1h)		;fe66	2a a1 b0 	* . . 
	ret nz			;fe69	c0 	. 
	pop de			;fe6a	d1 	. 
	ld d,015h		;fe6b	16 15 	. . 
	sub c			;fe6d	91 	. 
	and b			;fe6e	a0 	. 
	ret nz			;fe6f	c0 	. 
	pop de			;fe70	d1 	. 
	push de			;fe71	d5 	. 
	inc de			;fe72	13 	. 
	dec d			;fe73	15 	. 
	or b			;fe74	b0 	. 
	push de			;fe75	d5 	. 
	dec b			;fe76	05 	. 
	dec b			;fe77	05 	. 
	sub b			;fe78	90 	. 
	or h			;fe79	b4 	. 
	ret nc			;fe7a	d0 	. 
	push hl			;fe7b	e5 	. 
	inc d			;fe7c	14 	. 
	djnz lfe54h		;fe7d	10 d5 	. . 
	dec d			;fe7f	15 	. 
	ret nc			;fe80	d0 	. 
	ld d,l			;fe81	55 	U 
	djnz lfe25h		;fe82	10 a1 	. . 
	exx			;fe84	d9 	. 
	add hl,de			;fe85	19 	. 
	or h			;fe86	b4 	. 
	inc d			;fe87	14 	. 
	dec d			;fe88	15 	. 
	push de			;fe89	d5 	. 
	sub b			;fe8a	90 	. 
	ret nc			;fe8b	d0 	. 
	add hl,bc			;fe8c	09 	. 
	ld c,d			;fe8d	4a 	J 
	cp d			;fe8e	ba 	. 
	xor c			;fe8f	a9 	. 
	and (hl)			;fe90	a6 	. 
	sub l			;fe91	95 	. 
	and h			;fe92	a4 	. 
	and c			;fe93	a1 	. 
	or b			;fe94	b0 	. 
	ret nz			;fe95	c0 	. 
	ld (de),a			;fe96	12 	. 
	ld a,(009b0h)		;fe97	3a b0 09 	: . . 
	ld hl,(0c9bah)		;fe9a	2a ba c9 	* . . 
	add a,0d5h		;fe9d	c6 d5 	. . 
	call nz,0b0c1h		;fe9f	c4 c1 b0 	. . . 
	and b			;fea2	a0 	. 
	ld b,005h		;fea3	06 05 	. . 
	sub (hl)			;fea5	96 	. 
	and (hl)			;fea6	a6 	. 
	call nz,0e5d4h		;fea7	c4 d4 e5 	. . . 
	adc a,b			;feaa	88 	. 
	ld d,a			;feab	57 	W 
	ret z			;feac	c8 	. 
	xor b			;fead	a8 	. 
	sub (hl)			;feae	96 	. 
	sub h			;feaf	94 	. 
	and d			;feb0	a2 	. 
	jp nz,04fd3h		;feb1	c2 d3 4f 	. . O 
	ld (bc),a			;feb4	02 	. 
	ld c,l			;feb5	4d 	M 
	cp e			;feb6	bb 	. 
	inc bc			;feb7	03 	. 
	dec l			;feb8	2d 	- 
	cp e			;feb9	bb 	. 
	call 02d04h		;feba	cd 04 2d 	. . - 
	xor h			;febd	ac 	. 
	ld c,l			;febe	4d 	M 
	call z,02c05h		;febf	cc 05 2c 	. . , 
	cp e			;fec2	bb 	. 
	call z,0acbdh		;fec3	cc bd ac 	. . . 
	inc bc			;fec6	03 	. 
	dec de			;fec7	1b 	. 
	cp l			;fec8	bd 	. 
	in a,(002h)		;fec9	db 02 	. . 
	dec l			;fecb	2d 	- 
	cp e			;fecc	bb 	. 
	nop			;fecd	00 	. 
	nop			;fece	00 	. 
	ld l,c			;fecf	69 	i 
	ld (de),a			;fed0	12 	. 
	or c			;fed1	b1 	. 
	jp nc,0d6e4h		;fed2	d2 e4 d6 	. . . 
	or a			;fed5	b7 	. 
	sub (hl)			;fed6	96 	. 
	add a,h			;fed7	84 	. 
	sub d			;fed8	92 	. 
	ld h,l			;fed9	65 	e 
	ld bc,0e7e1h		;feda	01 e1 e7 	. . . 
	add a,a			;fedd	87 	. 
	add a,c			;fede	81 	. 
	ld h,l			;fedf	65 	e 
	ld sp,0b7e4h		;fee0	31 e4 b7 	1 . . 
	add a,h			;fee3	84 	. 
	or c			;fee4	b1 	. 
	ld h,h			;fee5	64 	d 
	ld (bc),a			;fee6	02 	. 
	jp po,082b7h		;fee7	e2 b7 82 	. . . 
	ld h,h			;feea	64 	d 
	ld b,0b1h		;feeb	06 b1 	. . 
	and 086h		;feed	e6 86 	. . 
	ld h,h			;feef	64 	d 
	inc b			;fef0	04 	. 
	call po,0b137h		;fef1	e4 37 b1 	. 7 . 
	ld h,h			;fef4	64 	d 
	ld bc,007e7h		;fef5	01 e7 07 	. . . 
	pop hl			;fef8	e1 	. 
	ld h,(hl)			;fef9	66 	f 
	ld sp,066b7h		;fefa	31 b7 66 	1 . f 
	add a,d			;fefd	82 	. 
	ld b,0e2h		;fefe	06 e2 	. . 
	cp 07fh		;ff00	fe 7f 	.  
	jr c,lff08h		;ff02	38 04 	8 . 
	jr nz,lff29h		;ff04	20 23 	  # 
	ld a,01bh		;ff06	3e 1b 	> . 
lff08h:
	ld c,a			;ff08	4f 	O 
	xor a			;ff09	af 	. 
	call sub_ff41h		;ff0a	cd 41 ff 	. A . 
	ld a,c			;ff0d	79 	y 
lff0eh:
	ex af,af'			;ff0e	08 	. 
lff0fh:
	call 01f54h		;ff0f	cd 54 1f 	. T . 
	jp nc,00f0ah		;ff12	d2 0a 0f 	. . . 
	in a,(0fch)		;ff15	db fc 	. . 
	rlca			;ff17	07 	. 
	jr nc,lff1eh		;ff18	30 04 	0 . 
	rlca			;ff1a	07 	. 
	rlca			;ff1b	07 	. 
	jr c,lff0fh		;ff1c	38 f1 	8 . 
lff1eh:
	ex af,af'			;ff1e	08 	. 
	out (0ffh),a		;ff1f	d3 ff 	. . 
	ld a,007h		;ff21	3e 07 	> . 
	out (0fdh),a		;ff23	d3 fd 	. . 
	dec a			;ff25	3d 	= 
	out (0fdh),a		;ff26	d3 fd 	. . 
	ret			;ff28	c9 	. 
lff29h:
	sub 0a5h		;ff29	d6 a5 	. . 
	jp nc,00c10h		;ff2b	d2 10 0c 	. . . 
	cp 0eah		;ff2e	fe ea 	. . 
	jr nc,lff36h		;ff30	30 04 	0 . 
	ld a,020h		;ff32	3e 20 	>   
	jr lff08h		;ff34	18 d2 	. . 
lff36h:
	add a,056h		;ff36	c6 56 	. V 
	ex af,af'			;ff38	08 	. 
	ld a,001h		;ff39	3e 01 	> . 
	call sub_ff41h		;ff3b	cd 41 ff 	. A . 
	ex af,af'			;ff3e	08 	. 
	jr lff0eh		;ff3f	18 cd 	. . 
sub_ff41h:
	xor 000h		;ff41	ee 00 	. . 
	ret z			;ff43	c8 	. 
	ld a,01bh		;ff44	3e 1b 	> . 
	call lff0eh		;ff46	cd 0e ff 	. . . 
	ld a,02dh		;ff49	3e 2d 	> - 
	call lff0eh		;ff4b	cd 0e ff 	. . . 
	ld hl,sub_ff41h+1		;ff4e	21 42 ff 	! B . 
	ld a,(hl)			;ff51	7e 	~ 
	xor 001h		;ff52	ee 01 	. . 
	ld (hl),a			;ff54	77 	w 
	jr lff0eh		;ff55	18 b7 	. . 
	ld a,004h		;ff57	3e 04 	> . 
	ex af,af'			;ff59	08 	. 
	jr c,lff60h		;ff5a	38 04 	8 . 
	inc a			;ff5c	3c 	< 
	ld b,h			;ff5d	44 	D 
	inc a			;ff5e	3c 	< 
	nop			;ff5f	00 	. 
lff60h:
	jr z,$+62		;ff60	28 3c 	( < 
	ld b,b			;ff62	40 	@ 
	inc a			;ff63	3c 	< 
	ld (bc),a			;ff64	02 	. 
	ld b,d			;ff65	42 	B 
	inc a			;ff66	3c 	< 
	nop			;ff67	00 	. 
	inc d			;ff68	14 	. 
	ex af,af'			;ff69	08 	. 
	inc e			;ff6a	1c 	. 
	jr nz,lff8dh		;ff6b	20 20 	    
	jr nz,lff8bh		;ff6d	20 1c 	  . 
	nop			;ff6f	00 	. 
	jr z,lffaeh		;ff70	28 3c 	( < 
	ld b,d			;ff72	42 	B 
	ld b,b			;ff73	40 	@ 
	ld b,b			;ff74	40 	@ 
	ld b,d			;ff75	42 	B 
	inc a			;ff76	3c 	< 
	nop			;ff77	00 	. 
	jr z,lff8ah		;ff78	28 10 	( . 
	jr c,lffc0h		;ff7a	38 44 	8 D 
	ld a,b			;ff7c	78 	x 
	ld b,b			;ff7d	40 	@ 
	inc a			;ff7e	3c 	< 
	nop			;ff7f	00 	. 
	ex af,af'			;ff80	08 	. 
	ld a,(hl)			;ff81	7e 	~ 
	ld b,b			;ff82	40 	@ 
	ld a,h			;ff83	7c 	| 
	ld b,b			;ff84	40 	@ 
	ld b,b			;ff85	40 	@ 
	ld a,(hl)			;ff86	7e 	~ 
	nop			;ff87	00 	. 
	inc d			;ff88	14 	. 
	ld a,(hl)			;ff89	7e 	~ 
lff8ah:
	ld b,b			;ff8a	40 	@ 
lff8bh:
	ld a,h			;ff8b	7c 	| 
	ld b,b			;ff8c	40 	@ 
lff8dh:
	ld b,b			;ff8d	40 	@ 
	ld a,(hl)			;ff8e	7e 	~ 
	nop			;ff8f	00 	. 
	inc b			;ff90	04 	. 
	ld a,008h		;ff91	3e 08 	> . 
	ex af,af'			;ff93	08 	. 
	ex af,af'			;ff94	08 	. 
	ex af,af'			;ff95	08 	. 
	ld a,000h		;ff96	3e 00 	> . 
	ex af,af'			;ff98	08 	. 
	djnz lff9bh		;ff99	10 00 	. . 
lff9bh:
	jr nc,lffadh		;ff9b	30 10 	0 . 
	djnz lffd7h		;ff9d	10 38 	. 8 
	nop			;ff9f	00 	. 
	ex af,af'			;ffa0	08 	. 
	ld d,d			;ffa1	52 	R 
	ld b,d			;ffa2	42 	B 
	ld b,d			;ffa3	42 	B 
	ld b,d			;ffa4	42 	B 
	ld b,d			;ffa5	42 	B 
	inc a			;ffa6	3c 	< 
	nop			;ffa7	00 	. 
	ex af,af'			;ffa8	08 	. 
	djnz lffefh		;ffa9	10 44 	. D 
	ld b,h			;ffab	44 	D 
	ld b,h			;ffac	44 	D 
lffadh:
	inc a			;ffad	3c 	< 
lffaeh:
	inc b			;ffae	04 	. 
	jr c,$+10		;ffaf	38 08 	8 . 
	sub d			;ffb1	92 	. 
	ld b,h			;ffb2	44 	D 
	jr z,$+18		;ffb3	28 10 	( . 
	djnz lffc7h		;ffb5	10 10 	. . 
	nop			;ffb7	00 	. 
	jr z,$+18		;ffb8	28 10 	( . 
	ld a,h			;ffba	7c 	| 
	ex af,af'			;ffbb	08 	. 
	djnz lffdeh		;ffbc	10 20 	.   
	ld a,h			;ffbe	7c 	| 
	nop			;ffbf	00 	. 
lffc0h:
	jr z,$+128		;ffc0	28 7e 	( ~ 
	inc b			;ffc2	04 	. 
	ex af,af'			;ffc3	08 	. 
	djnz $+34		;ffc4	10 20 	.   
	ld a,(hl)			;ffc6	7e 	~ 
lffc7h:
	nop			;ffc7	00 	. 
	ex af,af'			;ffc8	08 	. 
	djnz $+70		;ffc9	10 44 	. D 
	ld b,h			;ffcb	44 	D 
	ld b,h			;ffcc	44 	D 
	ld b,h			;ffcd	44 	D 
	jr c,lffd0h		;ffce	38 00 	8 . 
lffd0h:
	jr z,$+126		;ffd0	28 7c 	( | 
	ld b,d			;ffd2	42 	B 
	ld b,d			;ffd3	42 	B 
	ld a,h			;ffd4	7c 	| 
	ld b,h			;ffd5	44 	D 
	ld b,d			;ffd6	42 	B 
lffd7h:
	nop			;ffd7	00 	. 
	ex af,af'			;ffd8	08 	. 
	inc a			;ffd9	3c 	< 
	ld b,d			;ffda	42 	B 
	ld b,d			;ffdb	42 	B 
	ld a,(hl)			;ffdc	7e 	~ 
	ld b,d			;ffdd	42 	B 
lffdeh:
	ld b,d			;ffde	42 	B 
	nop			;ffdf	00 	. 
	inc d			;ffe0	14 	. 
	ex af,af'			;ffe1	08 	. 
	inc e			;ffe2	1c 	. 
	jr nz,$+34		;ffe3	20 20 	    
	jr nz,$+34		;ffe5	20 20 	    
	nop			;ffe7	00 	. 
	jr z,lfffah		;ffe8	28 10 	( . 
	jr c,$+66		;ffea	38 40 	8 @ 
	jr c,$+6		;ffec	38 04 	8 . 
	ld a,b			;ffee	78 	x 
lffefh:
	nop			;ffef	00 	. 
	ex af,af'			;fff0	08 	. 
	djnz $+58		;fff1	10 38 	. 8 
	ld b,h			;fff3	44 	D 
	ld a,b			;fff4	78 	x 
	ld b,b			;fff5	40 	@ 
	inc a			;fff6	3c 	< 
	nop			;fff7	00 	. 
	djnz $+42		;fff8	10 28 	. ( 
lfffah:
	ld d,h			;fffa	54 	T 
	ld b,h			;fffb	44 	D 
	ld b,h			;fffc	44 	D 
	ld b,h			;fffd	44 	D 
	defb 038h		;fffe	38 	8 
