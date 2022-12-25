	processor 6502

;Commodore 1541 Drive Test 

;GCR Decoding Table
MASK1	EQU	$F8
MASK2	EQU	$07
MASK2X	EQU	$C0
MASK3	EQU	$3E
MASK4	EQU	$01
MASK4X	EQU	$F0
MASK5	EQU	$0F
MASK5X	EQU	$80
MASK6	EQU	$7C
MASK7	EQU	$03
MASK7X	EQU	$E0
MASK8	EQU	$1F

;VIA1 Registers
VIA1_PA 	EQU	$1801
VIA1_PB		EQU $1800
VIA1_DDRA	EQU $1803
VIA1_DDRB  	EQU $1802
VIA1_ACR	EQU $180B
VIA1_PCR    EQU $180C
VIA1_T1CL 	EQU $1804
VIA1_T1CH	EQU $1805
VIA1_IFR    EQU $180D
VIA1_IER    EQU $180E

;VIA2 Registers
VIA2_PA 	EQU	$1C01
VIA2_PB		EQU $1C00
VIA2_DDRA	EQU $1C03
VIA2_DDRB  	EQU $1C02
VIA2_ACR	EQU $1C0B
VIA2_PCR    EQU $1C0C
VIA2_T1CL 	EQU $1C04
VIA2_T1CH	EQU $1C05
VIA2_IFR 	EQU $1C0D
VIA2_IER    EQU $1C0E


; IF65 Communication Macros

	MAC SENDNUMBER
		BRK
		.byte #$13
	ENDM
	
	MAC SENDCHAR 
		BRK
		.byte #$11
	ENDM
	
	MAC READCHARECHO
		brk
		.byte #$01
		and #$7F
	ENDM
	
	MAC READCHAR
		brk
		.byte #$08
		and #$7F
	ENDM
	
	MAC CHECKCHAR
		brk
		.byte #$09
	ENDM
	
;Shows a message on the terminal
	MAC SHOWMESSAGE
		lda #<{1}
		ldx #>{1}
		jsr SHOWMSG
	ENDM
	

	SEG.U data
	org 	$00

TEMP1 		dc 1
TEMP2		dc 1
WPTEMP 		dc 1
HEADSTEP 	dc 1
TESTTEMP	dc 1
ONOFFMASK   dc 1
VIA1_IRQ_F	dc 1
VIA2_IRQ_F	dc 1
STAB		ds 8
BUFPNT		ds 2
GTAB		ds 8
GCRPNT		ds 1
BTAB		ds 4


	SEG.I code
    org     $E000          ;Start of code
	jmp 	START
	
MENU1
	DC.B "[1] Test LED",$0A,$0D
	DC.B "[2] Write Prot",$0A,$0D
	DC.B "[3] Head",$0A,$0D
	DC.B "[4] Motor",$0A,$0D
	DC.B "[5] VIA 1",$0A,$0D
	DC.B "[6] VIA 2",$0A,$0D
	DC.B "[7] IEC",$0A,$0D
	DC.B "[8] Interrupt",$0A,$0D
	DC.B "[9] Clock",$0A,$0D
	DC.B "[A] Read Test",$0A,$0D,$00

;Messages

M_FAIL
	dc.b "Failed ",0
	
M_PASS
	dc.b "Passed ",0
	
PROMPT
	dc.b "-->",0
	
LEDMSG
	dc.b "[O]n, o[F]f, CR to exit",0
	
HEADMSG 
	dc.b "[I]n, [O]ut, CR to exit",0

M_ON
	dc.b "On",0

M_OFF
	dc.b "Off",0
	
M_DEVICE
	dc.b "Device: ",0
	
M_FREQ
	dc.b "Frequency",0
      
M_READTEST 
	dc.b "[I]n, [O]ut, [R]ead",0
	  

START
		sei
		
		;Setup VIAs
		lda #$1A
		sta VIA1_DDRB
		lda #$FF
		sta VIA1_DDRA
		lda #$6F
		sta VIA2_DDRB
		lda #$00
		sta VIA2_DDRA
		lda #$E0
		sta VIA2_PCR 
		lda #$7F
		sta VIA1_IER 
		sta VIA2_IER

	SUBROUTINE
MENU	
		JSR LINEBRK
		
		;Show drive ID
		lda VIA1_PB	
		and #$60
		lsr
		lsr
		lsr
		lsr
		lsr
		ora #$08
		pha
		SHOWMESSAGE M_DEVICE
		pla
		SENDNUMBER 
		JSR LINEBRK
		JSR LINEBRK
		SHOWMESSAGE MENU1
		JSR LINEBRK
		SHOWMESSAGE PROMPT 
		
MENULOOP
		READCHARECHO
		
		cmp #$31
		bne .L1
		lda #$08
		sta ONOFFMASK
		jmp OnOffTest
.L1
		cmp #$32  
		bne .L2
		jmp WriteProtTest
.L2
		cmp #$33
		bne .L3
		jmp HeadTest		
.L3
		cmp #$34
		bne .L4
		lda #$04
		sta ONOFFMASK
		jmp OnOffTest

.L4		
		cmp #$35
		bne .L5
		jmp Via1Test
		
.L5
		cmp #$36
		bne .L6
		jmp Via2Test
		
.L6
		cmp #$37
		bne .L7
		jmp IECTest
	
.L7		
		cmp #$38
		bne .L8
		jmp InterruptTest
		
.L8
		cmp #$39
		bne .L9
		jmp ClockTest
	
.L9
		cmp #$41
		bne .L10
		jmp ReadTest
		
.L10		
		jmp MENULOOP

	SUBROUTINE ReadTest	
ReadTest
	lda #$03
	sta VIA2_ACR
	lda #$E0		;Drive read mode
	sta VIA2_PCR 
	lda #$00	
	sta HEADSTEP
	
	jsr LINEBRK
	SHOWMESSAGE M_READTEST 
	JSR LINEBRK
	
.Loop
	READCHAR

	;Step head in
	cmp #'i
	bne .L1	
	ldx HEADSTEP
	inx 
	stx HEADSTEP
	cpx #$04
	bne .L2
	ldx #$00
	stx HEADSTEP
	jmp .L2

	;Step head out
.L1
	cmp #'o
	bne .L3
	ldx HEADSTEP
	dex
	stx HEADSTEP
	cpx #$ff
	bne .L2
	ldx #$03
	stx HEADSTEP

	;Move head
.L2
	lda HEADSTEP
	ora #$04
	sta VIA2_PB	
	SENDNUMBER
	jsr LINEBRK

	;Read from disk
.L3
	cmp #'r
	bne .Loop

	;Motor on, clock select	
	lda VIA2_PB
	ora #$64		
	sta VIA2_PB	
	lda #$52
	sta STAB
	LDX #0
	
	;Wait for sync
.L5	
	bit VIA2_PB		
	bmi .L5
	
	lda VIA2_PA      ; RESET PA LATCH
	clv
	bvc * 
	clv
	
	;Check for header block
	lda VIA2_PA 
	cmp #$52	
	bne .L5

	;Read in GCR encoded header
.SEEK15	
	BVC *
	CLV            

	LDA VIA2_PA 
	STA STAB+1,X
	INX
	CPX #7			;Read 7 bytes
	BNE .SEEK15

	;Decode GCR data
	LDA #<STAB
	STA BUFPNT      
	LDA #>STAB
	STA BUFPNT+1
	JSR GET4GB
	
	;Display header
	LDA BTAB
	SENDNUMBER
	lda #$2C
	SENDCHAR
	LDA BTAB+1
	SENDNUMBER
	lda #$2C
	SENDCHAR
	LDA BTAB+2
	SENDNUMBER
	lda #$2C
	SENDCHAR
	LDA BTAB+3
	SENDNUMBER
	JSR LINEBRK

	jmp .Loop
	
	
	
	SUBROUTINE ClockTest
ClockTest
	lda #$00
	sta VIA2_PB	
	sta TESTTEMP
.nextfreq
	asl 
	asl 
	asl 
	asl 
	asl 	
	sta VIA2_PB	
    SHOWMESSAGE M_FREQ
	lda TESTTEMP
	clc
	adc #$30
	SENDCHAR
.loop
	READCHAR
	
	;Test complete?
	cmp #$0D
	bne .L1
	jmp MENU

.L1	
	;Next frequency
	cmp #$4E
	bne .loop
	inc TESTTEMP 
	lda TESTTEMP
	cmp #$05
	bne .nextfreq
	lda #$00
	sta TESTTEMP
	jmp .nextfreq
		
	
	
	SUBROUTINE Interrupt

InterruptTest
	lda #$00
	sta $00
	sta $01
	
	;Setup VIA1 for a clock interrupt
	lda #$00
	sta VIA1_ACR
	sta VIA2_ACR	
	lda #$C0
	sta VIA1_IER 
	cli
	
	;Start clock count down
	lda #$02
	sta VIA1_T1CL
	lda #$00
	sta VIA1_T1CH
	
	;Wait for clock to trigger interrupt
	NOP
	NOP
	NOP
	sei
	
	;Check if VIA1 interrupt happened
	lda VIA1_IRQ_F
	cmp #$C0
	beq .L1
	lda #$31
	jmp Fail
.L1

	;Setup VIA2 for clock interrupt
	lda #$00
	sta VIA2_ACR
	sta VIA1_ACR	
	lda #$C0
	sta VIA2_IER 
	cli
	
	;Start clock count down
	lda #$02
	sta VIA2_T1CL
	lda #$00
	sta VIA2_T1CH
	
	;Wait for clock to trigger interrupt
	NOP
	NOP
	NOP
	sei
	
	;Check if VIA2 interrupt happened
	lda VIA2_IRQ_F
	and #$C0
	cmp #$C0
	beq .L2
	lda #$32
	jmp Fail
.L2	
	SHOWMESSAGE M_PASS 
	jmp MENU

		
	SUBROUTINE  IECTest
IECTest
	;Check all signals off	
	lda #$00
	sta VIA1_PB	
	lda VIA1_PB
	and #$9F
	cmp #$00
	beq .L1
	lda #$31
	jmp Fail

.L1	
	;Data out
	lda #$02
	sta VIA1_PB
	lda VIA1_PB
	and #$01
	bne .L2
	lda #$32
	jmp Fail
	
.L2
	;CLK out
	lda #$08
	sta VIA1_PB
	lda VIA1_PB
	and #$04
	bne .L3
	lda #$33
	jmp Fail

.L3
	jmp MENU


	SUBROUTINE 
Via2Test
	JSR LINEBRK
	
	;Data direction register
	lda #$1A
	sta VIA2_DDRB
	lda #$FF
	sta VIA2_DDRA	
	lda VIA2_DDRB
	cmp #$1A
	beq .L1
	lda #$31
	jmp Fail

.L1
	lda VIA2_DDRA
	cmp #$FF
	beq .L2
	lda #$32
	jmp Fail

	;Timer 2
.L2
	lda #$FF
	sta VIA2_T1CL
	sta VIA2_T1CH
	lda VIA2_T1CL
	cmp #$FC
	beq .L3
	lda #$33
	jmp Fail	

.L3
	lda VIA2_T1CH
	cmp #$FF
	beq .L4
	lda #$34
	jmp Fail

.L4
	SHOWMESSAGE M_PASS 
	jmp MENU

Fail
	pha
	SHOWMESSAGE M_FAIL
	pla
	SENDCHAR
	jmp MENU	
	
	
	SUBROUTINE 
Via1Test

	;Data direction register
	JSR LINEBRK
	lda #$1A
	sta VIA1_DDRB
	lda #$FF
	sta VIA1_DDRA
	
	lda VIA1_DDRB
	cmp #$1A
	beq .L1
	lda #$31
	jmp .Fail

.L1
	lda VIA1_DDRA
	cmp #$FF
	beq .L2
	lda #$32
	jmp .Fail

	;Timer 2
.L2
	lda #$FF
	sta VIA1_T1CL
	sta VIA1_T1CH
	lda VIA1_T1CL
	cmp #$FC
	beq .L3
	lda #$33
	jmp .Fail	

.L3
	lda VIA1_T1CH
	cmp #$FF
	beq .L4
	lda #$34
	jmp .Fail

.L4
	SHOWMESSAGE M_PASS 
	jmp MENU

.Fail
	pha
	SHOWMESSAGE M_FAIL
	pla
	SENDCHAR
	jmp MENU	
	
	
	SUBROUTINE
HeadTest
	jsr LINEBRK
	SHOWMESSAGE HEADMSG
	JSR LINEBRK
.loop
	READCHAR	
	
	;Step head in
	cmp #'i
	bne .L1
	ldx HEADSTEP
	inx 
	stx HEADSTEP
	cpx #$04
	bne .L2
	ldx #$00
	stx HEADSTEP
	jmp .L2
.L1

	;Step head out
	cmp #'o
	bne .L3
	ldx HEADSTEP
	dex
	stx HEADSTEP
	cpx #$ff
	bne .L2
	ldx #$04
	stx HEADSTEP
	
.L2
	lda HEADSTEP
	ora #$04
	sta VIA2_PB	
.L3
	;Test done
	cmp #$0D
	bne .loop
	lda #$00
	sta VIA2_PB

	jmp MENU


	SUBROUTINE

;Test for activity lead and motor
OnOffTest
	JSR LINEBRK
	SHOWMESSAGE LEDMSG
	JSR LINEBRK
.loop		
	;Turn on
	READCHAR
	CMP #'o
	bne .L1
	lda ONOFFMASK
	sta VIA2_PB
	SHOWMESSAGE M_ON
	JSR LINEBRK
	jmp .L3
.L1
	;Turn off
	cmp #'f
	bne .L2
	lda #$00
	sta VIA2_PB
	SHOWMESSAGE M_OFF
	JSR LINEBRK
	jmp .L3
.L2
	;Test complete?
	cmp #$0D
	bne .L3
	jmp MENU
.L3
	jmp .loop
	

	SUBROUTINE
WriteProtTest 	
	
	jsr LINEBRK
	
.loop
	CHECKCHAR
	bcs .Done
	lda VIA2_PB	
	and #$10	
	cmp WPTEMP
	beq .loop
		
	cmp #$00
	bne .L2
	SHOWMESSAGE M_OFF
	jmp .L3
.L2
	SHOWMESSAGE M_ON
	lda #$20
	SENDCHAR
.L3
	lda VIA2_PB	
	and #$10	
	sta WPTEMP
	lda #$0D
	SENDCHAR
	jmp .loop
		
.Done
	jmp MENU
	
	
	;Termanal access routines
	SUBROUTINE
LINEBRK
	lda #$0D
	BRK
	.byte #$11
	lda #$0A
	BRK
	.byte #$11
	rts 

	SUBROUTINE	
SHOWMSG
	stx TEMP2
	sta TEMP1
	ldy #$00
.loop
	lda (TEMP1),Y
	beq .done
	BRK
	.byte #$11
	iny
	jmp .loop
.done
	rts

;GCR Decoder taken from the Commodore DOS disassmebly
	SUBROUTINE GCRBIN
GET4GB	LDY #$00

	LDA (BUFPNT),Y
	AND #MASK1
	LSR 
	LSR 
	LSR 
	STA GTAB        ; HI NIBBLE

	LDA (BUFPNT),Y
	AND #MASK2
	ASL 
	ASL 
	STA GTAB+1

	INY             ; NEXT BYTE

XX05	LDA (BUFPNT),Y
	AND #MASK2X
	ROL 
	ROL 
	ROL 
	ORA GTAB+1
	STA GTAB+1

	LDA (BUFPNT),Y
	AND #MASK3
	LSR 
	STA GTAB+2

	LDA (BUFPNT),Y
	AND #MASK4
	ASL 
	ASL 
	ASL 
	ASL 
	STA GTAB+3

	INY             ; NEXT

	LDA (BUFPNT),Y
	AND #MASK4X
	LSR 
	LSR 
	LSR 
	LSR 
	ORA GTAB+3
	STA GTAB+3

	LDA (BUFPNT),Y
	AND #MASK5
	ASL 
	STA GTAB+4

	INY             ; NEXT BYTE

	LDA (BUFPNT),Y
	AND #MASK5X
	CLC
	ROL 
	ROL 
	AND #1
	ORA GTAB+4
	STA GTAB+4

	LDA (BUFPNT),Y
	AND #MASK6
	LSR 
	LSR 
	STA GTAB+5

	LDA (BUFPNT),Y
	AND #MASK7
	ASL 
	ASL
	ASL 
	STA GTAB+6

	INY            

;
XX06	LDA (BUFPNT),Y
	AND #MASK7X
	ROL 
	ROL 
	ROL 
	ROL 
	ORA GTAB+6
	STA GTAB+6

	LDA (BUFPNT),Y
	AND #MASK8
	STA GTAB+7
	INY

	STY GCRPNT

	LDX GTAB
	LDA GCRHI,X
	LDX GTAB+1
	ORA GCRLO,X
	STA BTAB

	LDX GTAB+2
	LDA GCRHI,X
	LDX GTAB+3
	ORA GCRLO,X
	STA BTAB+1

	LDX GTAB+4
	LDA GCRHI,X
	LDX GTAB+5
	ORA GCRLO,X
	STA BTAB+2

	LDX GTAB+6
	LDA GCRHI,X
	LDX GTAB+7
	ORA GCRLO,X
	STA BTAB+3

	RTS
	
GCRHI	
	.BYTE $FF    ;ERROR
	.BYTE $FF       ;ERROR
	.BYTE $FF       ;ERROR
	.BYTE $FF       ;ERROR
	.BYTE $FF       ;ERROR
	.BYTE $FF       ;ERROR
	.BYTE $FF       ;ERROR
	.BYTE $FF       ;ERROR
	.BYTE $FF       ;ERROR
	.BYTE $80
	.BYTE $00
	.BYTE $10
	.BYTE $FF       ;ERROR
	.BYTE $C0
	.BYTE $40
	.BYTE $50
	.BYTE $FF       ;ERROR
	.BYTE $FF       ;ERROR
	.BYTE $20
	.BYTE $30
	.BYTE $FF       ;ERROR
	.BYTE $F0
	.BYTE $60
	.BYTE $70
	.BYTE $FF       ;ERROR
	.BYTE $90
	.BYTE $A0
	.BYTE $B0
	.BYTE $FF       ;ERROR
	.BYTE $D0
	.BYTE $E0
	.BYTE $FF       ;ERROR

GCRLO	
	.BYTE $FF       ;ERROR
	.BYTE $FF       ;ERROR
	.BYTE $FF       ;ERROR
	.BYTE $FF       ;ERROR
	.BYTE $FF       ;ERROR
	.BYTE $FF       ;ERROR
	.BYTE $FF       ;ERROR
	.BYTE $FF       ;ERROR
	.BYTE $FF       ;ERROR
	.BYTE 8
	.BYTE $00
	.BYTE 1
	.BYTE $FF       ;ERROR
	.BYTE $C
	.BYTE 4
	.BYTE 5
	.BYTE $FF       ;ERROR
	.BYTE $FF       ;ERROR
	.BYTE 2
	.BYTE 3
	.BYTE $FF       ;ERROR
	.BYTE $F
	.BYTE 6
	.BYTE 7
	.BYTE $FF       ;ERROR
	.BYTE 9
	.BYTE $A
	.BYTE $B
	.BYTE $FF       ;ERROR
	.BYTE $D
	.BYTE $E
	.BYTE $FF       ;ERROR
	
	
;IRG handler	
	SUBROUTINE
IRQ
	sei
	lda VIA1_IFR
	sta VIA1_IRQ_F
	lda VIA2_IFR 
	sta VIA2_IRQ_F
	lda #$7F
	sta VIA1_IER 
	sta VIA2_IER
	RTI

	
	org 	$FFFA
	
		.byte #$00,#$00
		.byte <START,>START
		.byte <IRQ,>IRQ

