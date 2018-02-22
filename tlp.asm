;*******************************************************************************
;*                                                                             *
;*                    T H E   L E A R N I N G   P H O N E                      *
;*                                                                             *
;*                  for the Atari 8-bit Home Computer System                   *
;*                                                                             *
;*       Reverse engineered and documented assembly language source code       *
;*                                                                             *
;*                                     by                                      *
;*                                                                             *
;*                             [LIST GOES HERE]                                *
;*                                                                             *
;*                                                                             *
;*                               First Release                                 *
;*                                DD-MMM-YYYY                                  *
;*                                                                             *
;*                                Last Update                                  *
;*                                DD-MMM-YYYY                                  *
;*                                                                             *
;*         THE LEARNING PHONE was created by Vicent Wu / Lane Winner           *
;*              THE LEARNING PHONE was published by Atari Inc.                 *
;*                                                                             *
;*******************************************************************************
; Input file: tlp.bin

	.setcpu "6502"

.macro  RString       Arg
	.repeat .strlen(Arg), I
		.byte .strat(Arg, .strlen(Arg)-1-I)
	.endrep
.endmacro

.macro	ldi	arg1, arg2
	lda	#arg2
	sta	arg1
.endmacro

.macro	ori	arg1, arg2
	lda	arg1
	ora	#arg2
	sta	arg1
.endmacro

	.segment "SEG1"

;*******************************************************************************
;*                                                                             *
;*                         S Y S T E M   S Y M B O L S                         *
;*                                                                             *
;*******************************************************************************

TSTDAT		:= $0007
POKMSK		:= $0010			; POKEY interrupt mask.
ICAX1Z		:= $002A
VPRCED		:= $0202			; Vector to serial peripheral proceed line interrupt
VSERIN		:= $020A			; Vector to serial receive-data-ready interrupt
VTIMR1		:= $0210			; Vector to POKEY timer 1 interrupt
CDTMA1		:= $0226			; System timer one jump address
SRTIMR		:= $022B
SDMCTL		:= $022F			; Direct memory access control
DLIST		:= $0230			; Starting address of the display list.
SSKCTL		:= $0232			; Serial port control register
GPRIOR		:= $026F			; Priority selection register for screen objects
STICK0		:= $0278			; Joystick 0
SHFLOK		:= $02BE			; Flag for shift and control keys
PCOLR0		:= $02C0			; Color for player 0 and missile 0
COLOR1		:= $02C5			; Playfield 1 color register
COLOR2		:= $02C6			; Playfield 2 color register
COLOR3		:= $02C7			; Playfield 3 color register
COLOR4		:= $02C8			; Playfield 4 color register
HELPFG		:= $02DC			; Help key status
DVSTAT		:= $02EA			; Device status register.
CH		:= $02FC                        ; Keyboard character code.

; Device Control Block
DDEVIC		:= $0300			; Device bus ID (RS232 is $50).
DUNIT		:= $0301			; Device unit number.
DCOMND		:= $0302			; Device command.
DSTATS		:= $0303
DBUF		:= $0304
DTIMLO		:= $0306			; Time-out value for a handler in seconds
DBYT		:= $0308			; Number of bytes transferred to/from data buffer
DBYTLO		:= DBYT 			; Number of bytes transferred to/from data buffer
DBYTHI		:= $0308		        ; Number of bytes transferred to/from data buffer
DAUX1		:= $030A			; Device Command Arg 1
DAUX2		:= $030B			; Device Command Arg 1
HATABS		:= $031A			; Handler Address Table

; I/O Control Block
ICCOM		:= $0342
ICBA		:= $0344			; Address to buffer for read/write operations
ICBL		:= $0348
ICAX1		:= $034A
ICAX2		:= $034B


; GTIA (D000-D01F)

HPOSM0		:= $D004                        ;
HPOSM1		:= $D005                        ;
HPOSM2		:= $D006                        ;
HPOSM3		:= $D007                        ;
SIZEM		:= $D00C			; Size for all missiles
GRACTL		:= $D01D			; Turn on/off player missiles or latch triggers
CONSOL		:= $D01F                        ;

; POKEY (D200-D21F)

AUDCTL		:= $D208                        ; Audio control.
AUDF1		:= $D200                        ; Audio channel 1 frequency.
AUDF2		:= $D202                        ; Audio channel 2 frequency.
AUDF3		:= $D204                        ; Audio channel 3 frequency.
AUDF4		:= $D206                        ; Audio channel 4 frequency.
AUDC1		:= $D201                        ; Audio channel 1 control.
AUDC2		:= $D203                        ; Audio channel 2 control.
AUDC3		:= $D205                        ; Audio channel 3 control.
AUDC4		:= $D207                        ; Audio channel 4 control.
STIMER		:= $D209                        ; Start timer.
SKREST		:= $D20A                        ; Reset serial port status.
RANDOM		:= SKREST                       ; Random number generator.
SEROUT		:= $D20D                        ; Serial port input.
SERIN		:= SEROUT                       ; Serial port output.
IRQEN		:= $D20E                        ; Interrupt request (IRQ) enable.
IRQST		:= IRQEN                        ; IRQ status.
SKCTL		:= $D20F                        ; Serial port control.
SKSTAT		:= $D20F                        ; Serial port status.

; PIA (D300-D31F)

PIA		:= $D300                        ; Port A.
PORTA		:= PIA                          ; Port A.
PACTL		:= $D302                        ; Port A control.
PBCTL		:= $D303                        ; Port B control.

; ANTIC (D400-D41F)

DMACTL		:= $D400			; Direct Memory Access (DMA) control
PMBASE		:= $D407			; MSB of the player/missile base address
WSYNC		:= $D40A			; Wait for horizontal sync.

; OS ROM (D800-FFFF)

CIOV		:= $E456
SIOV		:= $E459			; Serial Input/Output utility entry point
SETVBV		:= $E45C
XITVBV		:= $E462


;*******************************************************************************
;*                                                                             *
;*                          C A R T   S Y M B O L S                            *
;*                                                                             *
;*******************************************************************************

; MPP Microbit 300 Driver Symbols
BAUD		:= $45				; Divide by n frequency for 300 baud. Noted in MPP Smart Term 4.1 Source Page 32
INPBIT          := $00FD			; Input bit
OUTBIT          := $00FE			; Output bit
INPBUF		:= $0F00
INPBFPT		:= $133E			; Used for offset to INPBUF
BUFLEN		:= $133F
OUTBFPT		:= $1340
character	:= $1347                        ; placeholder for now TODO
INP		:= $1348
OUTBUF		:= $1349
OUTINT		:= $134A
INPINT		:= $134B
ISTOP		:= $134C

; Other

L0070           := $0070
L0080           := $0080

word_9C		:= $009C
word_AC		:= $00AC
CURRENT_BAUD	:= $00B1			; Current 850 baud rate: $FF->300 $00->1200
byte_B2		:= $00B2			; Used during init
IS_MPP		:= $00B2			; If Microbits 300 then 1 else 0
byte_B3		:= $00B3
byte_B5		:= $00B5
byte_B7		:= $00B7
CURRENT_DL	:= $00B8			; Current Display List (80 or FF = DL #1, 00 = DL #2)
byte_C0		:= $00C0
byte_C3		:= $00C3
byte_C4		:= $00C4
byte_C7		:= $00C7
byte_CC		:= $00CC
byte_CD		:= $00CD
off_CE		:= $00CE
FG_COLOR_DL2	:= $00D0			; Text luminance used in Display List #2 (zoomed)
BG_COLOR_DL2	:= $00D1			; Background / border hue/luminance used in Display List #2
FG_COLOR_DL1	:= $00D2			; Text luminance used in Display List #1 (small text)
BG_COLOR_DL1	:= $00D3			; Background / border hue/luminance used in Display List #1
byte_D9		:= $00D9
off_DD		:= $00DD
off_DF		:= $00DF
byte_E1		:= $00E1
byte_E2		:= $00E2

off_E3		:= $00E3
off_E5		:= $00E5
chset6_base 	:= $00E8			; charset_sm address lo/hi at E8/E9
off_EC		:= $00EC
off_F4		:= $00F4
off_FA		:= $00FA
byte_FF		:= $00FF

byte_1310	:= $1310
byte_1330	:= $1330
byte_133a	:= $133A
byte_133c	:= $133C
byte_133d	:= $133D
byte_133e	:= $133E
byte_133f	:= $133F
byte_1340	:= $1340
byte_1341	:= $1341
byte_1342	:= $1342
byte_1343	:= $1343
byte_1344	:= $1344
byte_1345	:= $1345
byte_1346	:= $1346			; Handler Device Type (R:)???
byte_1347	:= $1347
byte_1348	:= $1348
byte_134c	:= $134C
off_134d	:= $134D
byte_134f	:= $134F
byte_1350	:= $1350
word_1351	:= $1351
L2000           := $2000
L3E2E		:= $3E2E
L3E33           := $3E33
L4000           := $4000
L6000           := $6000
L8000           := $8000

;*******************************************************************************
;*                                                                             *
;*                                 cart_start                                  *
;*                                                                             *
;*                    Second entry point after system restart                  *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
; After a cold or warm start, the OS runs the cart_init routine found towards
; the end of the cart's address space, then jumps to here.
 
cart_start:
	jsr     sub_b799			; 
	jsr     init_graphics			; Initialize display lists, colors
	jsr     display_title			; Display program title and copyright

	ldy     #LB976-LB96E			; 
	jsr     call_cio_or_err			; Open K: on channel #2

;**(n) Attempt to open an Atari direct connect modem using T: handler **********
	ldy     #LB996-LB96E  			; 
	jsr     call_cio			; Try to open T: on channel #1
	bmi     LA035   			; If cio returned error then skip ahead

;**(n) Atari direct-connect modem detected *************************************
	lda     #$1A    			;
	sta     word_9C
	ldy     #$33    			; String length - 1
	lda     #<LB8DF				; "After the phone has a high..."
	ldx     #>LB8DF				;
	jsr     print_string			;

:	lda     CH				; Wait for key press
	cmp     #$FF    			;
	beq     :-				; 

	ldx     #$FF    			; Some key press occurred
	stx     CH				; Clear keyboard register

;** (n) If user presses '1' instead of RETURN try to set baud to 1200 **********
	cmp     #$1F    			; if last key press <> '1'
	bne     LA04F   			; then skip ahead
	jsr     display_title			; otherwise clear screen and..

;** (n) Try changing baud rate and re-opening channel #1 ***********************
LA035:  jsr     close_ch1			; Close CIO channel #1
	jsr     sub_open_modem			; Attempt to open R: on channel #1 at CURRENT_BAUD
	bmi     :+				; if unsuccessful then default to Microbits 300
	lda     #<LB91C				; otherwise print "1200 baud"
	ldx     #>LB91C				; 
	ldy     #$08    			; String length - 1
	jsr     print_string			; 
	jmp     sub_main   			; and proceed to main loop

;** (n) If all else fails assume Microbits 300 ********************************
:	jsr     sub_b272
	jmp     sub_main   			; A04C 4C 5E A0                 L^.

; ----------------------------------------------------------------------------
LA04F:	lda	#$4C
	jsr	sub_b242			; Send test??
LA054:  lda     #$46    			; 
	jsr     sub_b242			; Send test??
	lda     DVSTAT+1
	bpl     LA054   			; Keep trying test??

;*******************************************************************************
;*                                                                             *
;*                                  sub_main                                   *
;*                                                                             *
;*          Aside from all the IRQs enabled this might be the main loop        *
;*                                                                             *
;*******************************************************************************
LA05E:  
sub_main:
	jsr     sub_a963			; Check for key press
	jsr     sub_ab35
	jsr     sub_a12c
	lda     byte_CC
	beq     sub_main   			; A069 F0 F3                    ..
	jsr     sub_a079
	bcc     sub_main   			; A06E 90 EE                    ..
	jsr     LA0A3   			; A070 20 A3 A0                  ..
	lda     #$00    			; A073 A9 00                    ..
	sta     $B6     			; A075 85 B6                    ..
	beq     sub_main   			; jump always

; ----------------------------------------------------------------------------
sub_a079:
	ldy     #LB98E-LB96E			; Prepare CIO get char
	jsr     call_cio_or_err			; Call CIO, halt if error
	and     #$7F    			; A07E 29 7F                    ).
	bit     byte_B3 			; A080 24 B3                    $.
	bmi     LA0D2   			; A082 30 4E                    0N
	cmp     #$20    			; A084 C9 20                    . 
	bcs     LA08D   			; A086 B0 05                    ..
	jsr     sub_a0f3   			; A088 20 F3 A0                  ..
LA08B:  clc             			; A08B 18                       .
	rts             			; A08C 60                       `

; ----------------------------------------------------------------------------
LA08D:  ldx     $B6     			; A08D A6 B6                    ..
	sta     L3E2E,x 			; A08F 9D 2E 3E                 ..>
	inx             			; A092 E8                       .
	stx     $B6     			; A093 86 B6                    ..
	ldy     byte_B5 			; A095 A4 B5                    ..
	bpl     LA0A0   			; A097 10 07                    ..
	cmp     #$60    			; A099 C9 60                    .`
	bcs     LA08B   			; A09B B0 EE                    ..
	cmp     #$40    			; A09D C9 40                    .@
	rts             			; A09F 60                       `

; ----------------------------------------------------------------------------
LA0A0:  cpx     byte_B5 			; A0A0 E4 B5                    ..
	rts             			; A0A2 60                       `

; ----------------------------------------------------------------------------
LA0A3:  ldy     #$00    			; A0A3 A0 00                    ..
	sty     $B6     			; A0A5 84 B6                    ..
	bvc     LA0C5   			; A0A7 50 1C                    P.
	ldy     $B4     			; A0A9 A4 B4                    ..
	cpy     #$05    			; A0AB C0 05                    ..
	beq     LA0B6   			; A0AD F0 07                    ..
	jsr     sub_a120   			; A0AF 20 20 A1                   .
	tya             			; A0B2 98                       .
	bne     LA0B6   			; A0B3 D0 01                    ..
	rts             			; A0B5 60                       `

; ----------------------------------------------------------------------------
LA0B6:  dey             			; A0B6 88                       .
	beq     LA0BC   			; A0B7 F0 03                    ..
	jsr     sub_a7fe
LA0BC:  lda     LBAE2,y 			; A0BC B9 E2 BA                 ...
	pha             			; A0BF 48                       H
	lda     LBAE7,y 			; A0C0 B9 E7 BA                 ...
	pha             			; A0C3 48                       H
	rts             			; A0C4 60                       `

; ----------------------------------------------------------------------------
LA0C5:  ldx     byte_B3 			; A0C5 A6 B3                    ..
	lda     LBACA,x 			; A0C7 BD CA BA                 ...
	beq     LA0D1   			; A0CA F0 05                    ..
	pha             			; A0CC 48                       H
	lda     LBAD2,x 			; A0CD BD D2 BA                 ...
	pha             			; A0D0 48                       H
LA0D1:  rts             			; A0D1 60                       `

; ----------------------------------------------------------------------------
LA0D2:  asl     byte_B3 			; A0D2 06 B3                    ..
	lsr     byte_B3 			; A0D4 46 B3                    F.
	jsr     sub_a0db
	clc             			; A0D9 18                       .
LA0DA:  rts             			; A0DA 60                       `

; ----------------------------------------------------------------------------
sub_a0db:
	cmp     #$5B    			; A0DB C9 5B                    .[
	bcs     LA0DA   			; A0DD B0 FB                    ..
	cmp     #$3D    			; A0DF C9 3D                    .=
	beq     LA105   			; A0E1 F0 22                    ."
	cmp     #$20    			; A0E3 C9 20                    . 
	bcs     LA0EB   			; A0E5 B0 04                    ..
	adc     #$20    			; A0E7 69 20                    i 
	bcc     LA0F3   			; A0E9 90 08                    ..
LA0EB:  cmp     #$32    			; A0EB C9 32                    .2
	beq     LA10B   			; A0ED F0 1C                    ..
	cmp     #$40    			; A0EF C9 40                    .@
	bcc     LA0DA   			; A0F1 90 E7                    ..
LA0F3:

sub_a0f3:
	tax             			; A0F3 AA                       .
	clc             			; A0F4 18                       .
	lda     #<(sub_a367-1) 			; A0F5 A9 66                    .f
	adc     LBA6F,x 			; A0F7 7D 6F BA                 }o.
	tay             			; A0FA A8                       .
	lda     #$00    			; A0FB A9 00                    ..
	sta     $B6     			; A0FD 85 B6                    ..
	adc     #>(sub_a367-1) 			; A0FF 69 A3                    i.
	pha             			; A101 48                       H
	tya             			; A102 98                       .
	pha             			; A103 48                       H
	rts             			; A104 60                       `

; ----------------------------------------------------------------------------
LA105:  ldx     #$02    			; A105 A2 02                    ..
	lda     #$00    			; A107 A9 00                    ..
	beq     LA111   			; A109 F0 06                    ..
LA10B:  ror     byte_B5 			; A10B 66 B5                    f.
	lda     #$01    			; A10D A9 01                    ..
	bne     LA118   			; A10F D0 07                    ..
LA111:  ldy     byte_B5 			; A111 A4 B5                    ..
	sty     $1354   			; A113 8C 54 13                 .T.
	stx     byte_B5 			; A116 86 B5                    ..
LA118:  sta     $B4     			; A118 85 B4                    ..
	lda     byte_B3 			; A11A A5 B3                    ..
	ora     #$40    			; A11C 09 40                    .@
	bne     LA129   			; A11E D0 09                    ..

; ----------------------------------------------------------------------------
sub_a120:  
	lda     $1354   			; A120 AD 54 13                 .T.
	sta     byte_B5 			; A123 85 B5                    ..
	lda     byte_B3 			; A125 A5 B3                    ..
	and     #$BF    			; A127 29 BF                    ).
LA129:  sta     byte_B3 			; A129 85 B3                    ..
LA12B:  rts             			; A12B 60                       `

; ----------------------------------------------------------------------------
sub_a12c:
	lda     $B9     			; A12C A5 B9                    ..
	beq     LA12B   			; A12E F0 FB                    ..
	jmp     (off_134d)

; ----------------------------------------------------------------------------

sub_a133:
	ldy     $DC     			; A133 A4 DC                    ..
	lda     $DB     			; A135 A5 DB                    ..
	ldx     byte_FF 			; A137 A6 FF                    ..
	bpl     LA14B   			; A139 10 10                    ..
	iny             			; A13B C8                       .
	bne     LA143   			; A13C D0 05                    ..
	tax             			; A13E AA                       .
	bmi     LA144   			; A13F 30 03                    0.
	sta     byte_FF 			; A141 85 FF                    ..
LA143:  rts             			; A143 60                       `

; ----------------------------------------------------------------------------
LA144:  inx             			; A144 E8                       .
	stx     byte_134f
	jmp     sub_a1fe

; ----------------------------------------------------------------------------
LA14B:  bit     byte_134f
	bpl     :+
	rts             			; A150 60                       `

; ----------------------------------------------------------------------------
:	ldx     byte_FF 			; A151 A6 FF                    ..
	beq     :+
	dex             			; A155 CA                       .
	beq     LA16D   			; A156 F0 15                    ..
	dex             			; A158 CA                       .
	beq     LA18C   			; A159 F0 31                    .1
	dex             			; A15B CA                       .
	beq     LA174   			; A15C F0 16                    ..
	dex             			; A15E CA                       .
	beq     LA180   			; A15F F0 1F                    ..
	rts             			; A161 60                       `

; ----------------------------------------------------------------------------
:	sty     off_CE+1
	sta     off_CE
	ldy     #$00    			; A166 A0 00                    ..
	lda     (off_CE),y
	jmp     LA1DD   			; A16A 4C DD A1                 L..

; ----------------------------------------------------------------------------
LA16D:  sty     off_CE+1
	sta     off_CE
	jmp     sub_a1fe

; ----------------------------------------------------------------------------
LA174:  sty     off_134d+1
	sta     off_134d
	dex             			; A17A CA                       .
	stx     $B9     			; A17B 86 B9                    ..
	jmp     sub_a1fe

; ----------------------------------------------------------------------------
LA180:  sty     off_134d+1
	sta     off_134d
	jsr     sub_a1fe
	jmp     (off_134d)

; ----------------------------------------------------------------------------
LA18C:  bit     byte_1350
	bmi     :+
	sta     word_1351
	sty     word_1351+1
	dex             			; A197 CA                       .
	stx     byte_1350
	rts             			; A19B 60                       `

; ----------------------------------------------------------------------------
:	tya             			; A19C 98                       .
	ldy     #$00    			; A19D A0 00                    ..
	sta     (off_CE),y
	iny             			; A1A1 C8                       .
	dec     word_1351   			; A1A2 CE 51 13                 .Q.
	bne     LA1B0   			; A1A5 D0 09                    ..
	tya             			; A1A7 98                       .
	ldx     word_1351+1
	beq     LA1C8   			; A1AB F0 1B                    ..
	dec     word_1351+1
LA1B0:  lda     $DB     			; A1B0 A5 DB                    ..
	sta     (off_CE),y
	dec     word_1351
	bne     LA1C4   			; A1B7 D0 0B                    ..
	ldx     word_1351+1
	beq     :+
	dec     word_1351+1
	bvs     LA1C4   			; A1C1 70 01                    p.
:	clv             			; A1C3 B8                       .
LA1C4:  lda     #$02    			; A1C4 A9 02                    ..
	bne     LA1C9   			; A1C6 D0 01                    ..
LA1C8:  clv             			; A1C8 B8                       .
LA1C9:  php             			; A1C9 08                       .
	clc             			; A1CA 18                       .
	adc     off_CE
	sta     off_CE
	bcc     :+
	inc     off_CE+1
:	plp             			; A1D3 28                       (
	bvs     LA1DC   			; A1D4 70 06                    p.
	jsr     sub_a1fe
	inc     byte_1350
LA1DC:  rts             			; A1DC 60                       `

; ----------------------------------------------------------------------------
LA1DD:  lda     #$1B    			; A1DD A9 1B                    ..
	jsr     sub_ab54
	ldy     #$00    			; A1E2 A0 00                    ..
	sty     $E7     			; A1E4 84 E7                    ..
	lda     (off_CE),y
	tay             			; A1E8 A8                       .
	asl     a       			; A1E9 0A                       .
	rol     $E7     			; A1EA 26 E7                    &.
	asl     a       			; A1EC 0A                       .
	rol     $E7     			; A1ED 26 E7                    &.
	tya             			; A1EF 98                       .
	and     #$3F    			; A1F0 29 3F                    )?
	ora     #$40    			; A1F2 09 40                    .@
	jsr     sub_ab54
	lda     $E7     			; A1F7 A5 E7                    ..
	ora     #$68    			; A1F9 09 68                    .h
	jsr     sub_ab54

sub_a1fe:
	jsr     sub_a120   			; A1FE 20 20 A1                   .
	lda     #$FF    			; A201 A9 FF                    ..
	sta     byte_FF 			; A203 85 FF                    ..
	lda     #$1B    			; A205 A9 1B                    ..
	jsr     sub_ab54
	lda     #$46    			; A20A A9 46                    .F
	jsr     sub_ab54
	lda     #$68    			; A20F A9 68                    .h
	jmp     sub_ab54

;*******************************************************************************
;*                                                                             *
;*                                init_graphics                                *
;*                                                                             *
;*                 Initialize display lists, colors, player-missiles           *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; This subroutine initializes 2 display lists, colors, player missiles
;
; Display List 1 is a mostly straightforward ANTIC mode F screen. Except 
; that screen RAM shifts from a range beginning at $2010 to a 2nd range 
; beginning at $3000 a little after midway down the screen.

; Display List 2 is an ANTIC mode F screen with horizontal and vertical scrolling
;
; Display List 1         Display List 2
; $1000: $70 8 SCANS     $10CA: $70
; $1001: $70 8 SCANS     $10CB: $70
; $1002: $70 8 SCANS     $10CC: $70
; $1003: $4F ANTIC F     $10CD: $4F
; $1004: $10 SCRNLO      $10CE: $00 SCRNLO
; $1005: $20 SCRNHI      $10CD: $40 SCRNHI
; $1006: $0F ANTIC F     $10CE: $4F
; ...                    $10CF: $40 SCRNLO
; ...                    $10D0: $40 SCRNHI
; ...                    ...
; ...                    $130A: $4F ANTIC F
; ...                    $130B: $C0 SCRNLO
; $10C6: $0F ANTIC F     $130C: $6F SCRNHI
; $10C7: $41 WAIT SYNC   $130C: $41 WAIT SYNC
; $10C8: $00 DLSTLO      $130D: $CA DLSTLO
; $10C9: $10 DLSTHI      $130E: $10 DLSTHI

sub_a214:					; A214
init_graphics:
	lda     #$00    			; 
	sta     DMACTL  			; Turn off ANTIC
	sta     GRACTL				; Turn off PMG, unlatch triggers
	sta     SHFLOK				; Force keyboard to lower case

;** (n) Clear RAM at 0080-00FF and  0580-05FF **********************************
	ldx     #$7F    			; Clear 128 bytes at ...
:       sta     $0580,x 			; 0580-05FF and ...
	sta     L0080,x 			; 0080-00FF 
	dex             			; 
	bpl     :-      			; End Loop

;** (n) TODO Why storing FF in these two locations??? **************************
	stx     CURRENT_DL			; byte_B8 maybe which DL is active
	stx     byte_FF 			; Store FF in byte_FF TODO

;** (n) Prepare values for head of display lists *******************************
	lda     #$70                            ;
	ldx     #$02    			; 
	stx     byte_B7				; Let byte_B7 = 2 TODO
	stx     byte_B2				; Let byte_B2 = 2 TODO

;** (n) Write head of display list # 1 *****************************************
	inx             			; Store 3 x $70 
:       sta     $10C9,x 			; at $1000 and $10CA
	sta     $0FFF,x 			; 8 blank scan lines each
	dex             			; 
	bne     :-      			; End Loop

;** (n) Point to screen RAM at $2010 in display list #1 ************************
	lda     #<$2010    			; 
	sta     $1004   			; 
	sta     off_F4				; Store Screen location
	lda     #>$2010    			; in zero page varibles, too
	sta     $1005   			; 
	sta     off_F4+1			; 

;** (n) Write body of display list #1 ******************************************
	lda     #$0F    			; Store entry for ANTIC mode F
:       sta     $1006,x 			; in body of display list
	inx             			; 
	cpx     #$C1    			; 
	bne     :-      			; End Loop

;** (n) Write 2 display list entries to point to screen RAM ********************
	lda     #$4F    			; Write ANTIC mode F & screen
	sta     $1003   			; location at top of disp list
	sta     $106B   			; again after 102 mode F lines

;** (n) Point to screen RAM at $3000 in display list #1 ************************
	lda     #<$3000				;
	sta     $106C   			; point to screen RAM to $3000
	lda     #>$3000				;
	sta     $106D   			; 

;** (n) Write tail end of display list #1 **************************************
	lda     #$41    			; Write Wait for sync
	sta     $1006,x 			; to bottom of list
	sta     $130D   			; 
	lda     #<$1000    			; Tell ANTIC to jump
	sta     $1007,x 			; back to the top of the display
	sta     DLIST				; list at $1000.
	lda     #>$1000    			;
	sta     $1008,x 			; Also register the new display
	sta     DLIST+1 			; list loc with the hardware

;** (n) Screen RAM for display list #2 will begin at $4000 *********************
	lda     #<L4000				;
	sta     off_DF				; Let off_DF = $4000
	sta     byte_E1				; Let byte_E1 = $00 TODO

	lda     #>L4000				;
	sta     off_DF+1 			;
	sta     byte_E2 			; Let byte_E2 = $40 TODO

;** (n) Call sub to derive body of display list #2 *****************************
	jsr     LB1AB   			; TODO

;** (n) Write tail of display list #2 ******************************************
	lda     #$CA    			; Tell ANTIC to jump
	sta     $130E   			; back to the top of the display
	lda     #$10    			; list at $10CA.
	sta     $130F   			;

;** (n) Create a table of pointers to 192 scan lines ****************************
	ldx     #$00    			; Create a table of pointers...
:	lda     off_F4+1			; to the screen RAM for 192 scan
	sta     $04C0,x 			; lines. LSB in $0400-$04BF...
	lda     off_F4  			; MSB in $04C0-$057F...
	sta     $0400,x 			; starting with $2010, $0238, ..., $3DE8
	inx             			;
	cpx     #$C0    			; Quit at 192 iterations
	beq	:+                              ;
	lda     off_F4                          ;
	adc     #$28    			; Add 40 bytes (1 row)
	sta     off_F4                          ;
	bcc	:-                              ;
	inc     off_F4+1                        ;
	bne	:-				; End Loop

;** (n) Configure Player Missile Graphics ***************************************
:	lda     #$03    			;
	sta     GRACTL				; Enable display of PMG

	lda     #$04    			; TODO Confused - page 4 contains pointers to scan line addresses
	sta     PMBASE  			; PMG will be stored in page 4

	lda     #$55    			; SIZEM = 01 01 01 01
	sta     SIZEM   			; Set missiles 0-3 to double wide

	lda     #$00    			; Loop through each missile
	ldx     #$03    			; Set horiz pos to 0 (offscreen)
:       sta     HPOSM0,x			;
	dex             			;
	bpl     :-      			; End Loop

;** (n) *************************************************************************
	ldx     #$32    			; TODO
	stx     byte_C4                         ;

	ldx     #$0F    			; TODO
	stx     byte_C0 			;

	ldx     #$07    			; Copy 7 bytes from ROM to RAM locations
	stx     byte_B3 			; $058C-$0592
:       lda     LBA67,x 			; Initial values
	sta     $058C,x 			; 70 40 70 40 40 06 0F
	dex             			;
	bpl     :-      			; End loop

	stx     byte_C7 			; Save FF to RAM

;** (n) Set default background and border color for Display List #1 *************
	lda     #$01    			;
	sta     byte_C3 			;
	sta     byte_B5 			; Set priority for screen objects...
	sta     GPRIOR  			; 0000 0001 = Player 0-3, Playfied 0-3, BAK
	lda     #$16				; Set initial background color
	sta     COLOR4  			; for the border and background
	sta     COLOR2  			; to hue: 1 (orange), luminance: 6
	sta     BG_COLOR_DL1    		; Save color value to RAM

;** (n) Change Player/Missile colors to something different than background *****
	jsr     sub_a324

;** (n) Set default background and border color for Display List #2 *************
	lda     #$94    			; Default background and border for Display List #2
	sta     BG_COLOR_DL2    		; Save color value to RAM

;** (n) Set default text luminance **********************************************
	lda     #$E1    			; Default text luminance value for Display List #1
	sta     COLOR1  			; Save value to color register
	sta     FG_COLOR_DL1    		; Save color value to RAM

	lda     #$CA    			; Default text luminance value for Display List #2
	sta     FG_COLOR_DL2    		; Save color value to RAM

;** (n) Store base address of 6x6 charset in Zero Page RAM **********************
	lda     #>charset_sm
	sta     chset6_base+1   		; Save MSB of charset_sm to RAM

	lda     #<charset_sm
	sta     chset6_base     		; Save LSB of charset_sm to RAM

;** (n) Disable break key interrupt *********************************************
	lda     POKMSK				; 1100 000 is the default on power up
	and     #$7F    			; Clear flag for break key interrupt
	jsr     sub_irqen			; Enable new set of interrupts

;** (n) Set system timer for vector #7  ******************************************
	ldy     #<sub_b013      		; MSB of new vector routine
	ldx     #>sub_b013      		; LSB of new vector routine
	lda     #$07    			; Number of the vector to change
	jmp     SETVBV  			; Set system timers (SETVBV will rts)

;*******************************************************************************
;*                                                                             *
;*                                  sub_a324                                   *
;*                                                                             *
;*      Set Player/Missile Colors to something different than background.      *
;*                                                                             *
;*******************************************************************************
sub_a324:
	tay             			; Let Y = current background color
	clc             			;
	adc     #$04    			; Increment luminance by 4
	and     #$0F    			; Clear hue bits
	sta     byte_D9 			; Save new luminance to D9
	tya             			; Retrieve original background color
	and     #$F0    			; Clear luminance bits
	clc             			;
	adc     #$30    			; Increment hue by 3
	ora     byte_D9 			; Merge new hue with new luminance

	ldx     #$03    			;
:	sta     PCOLR0,x			; Store new hue and luminance
	dex             			; to PCOLR0-PCOLR3
	bpl     :-      			; End Loop
	rts             			;

;*******************************************************************************
;*                                                                             *
;*                               display_title                                 *
;*                                                                             *
;*                   Print program title and copyright notice                  *
;*                                                                             *
;*******************************************************************************
sub_a33d:
display_title:
;** (1) Clear screen ***********************************************************
	jsr     sub_a367			; Clear screen
	jsr     sub_a3c8   			; TODO Initialize Screen Variables?

;** (2) Print "WELCOME TO THE LEARNING PHONE" **********************************
	lda     #$50    			; Set X coordinate for text
	sta     $A4     			; TODO Save X coordinate for zoomed display?
	sta     word_9C
	ldy     #$1C    			; String length - 1
	lda     #<LB8C2				; "WELCOME..."
	ldx     #>LB8C2
	jsr     print_string
	jsr     sub_a3c2   			; A352 20 C2 A3                  ..

;** (3) Print "COPYRIGHT 1984 ATARI" *******************************************
	lda     #$67    			; Set X coordinate for text
	sta     $A4     			; Save X coordinate for scaled display?
	sta     word_9C
	ldy     #$13    			; String length - 1
	lda     #<LB8AE				; "COPYRIGHT..."
	ldx     #>LB8AE
	jsr     print_string
	jmp     sub_a3c2   			; A364 4C C2 A3                 L..

; ----------------------------------------------------------------------------
sub_a367:
	jmp     clear_screen

; ----------------------------------------------------------------------------

sub_a36a:
	txa             			; A36A 8A                       .
	and     #$07    			; A36B 29 07                    ).
	sta     byte_B3 			; A36D 85 B3                    ..
	tax             			; A36F AA                       .
	lda     LBADA,x 			; A370 BD DA BA                 ...
	sta     byte_B5 			; A373 85 B5                    ..
	ldx     #$00    			; A375 A2 00                    ..
	stx     $DA     			; A377 86 DA                    ..
	rts             			; A379 60                       `

; ----------------------------------------------------------------------------
	txa             			; A37A 8A                       .
	and     #$03    			; A37B 29 03                    ).
	lsr     a       			; A37D 4A                       J
	ror     a       			; A37E 6A                       j
	ror     a       			; A37F 6A                       j
	sta     $B0     			; A380 85 B0                    ..
	rts             			; A382 60                       `

; ----------------------------------------------------------------------------
	lda     #$80    			; A383 A9 80                    ..
	bne     LA389   			; A385 D0 02                    ..
	lda     #$00    			; A387 A9 00                    ..
LA389:  sta     $1353   			; A389 8D 53 13                 .S.
	rts             			; A38C 60                       `

; ----------------------------------------------------------------------------
	lda     #$00    			; A38D A9 00                    ..
	beq     :+

; ----------------------------------------------------------------------------
	lda     #$FF    			; A391 A9 FF                    ..
:	sta     $CA     			; A393 85 CA                    ..
	rts             			; A395 60                       `

; ----------------------------------------------------------------------------
	clc             			; A396 18                       .
	jmp     LA824   			; A397 4C 24 A8                 L$.

; ----------------------------------------------------------------------------
	sec             			; A39A 38                       8
	jmp     LA824   			; A39B 4C 24 A8                 L$.

; ----------------------------------------------------------------------------
	lda     word_9C
	sta     word_AC
	lda     word_9C+1
	sta     word_AC+1     			; A3A4 85 AD                    ..
	lda     $A4     			; A3A6 A5 A4                    ..
	sta     $AE     			; A3A8 85 AE                    ..
	lda     $A5     			; A3AA A5 A5                    ..
	sta     $AF     			; A3AC 85 AF                    ..
	rts             			; A3AE 60                       `

; ----------------------------------------------------------------------------

sub_a3af:
	jmp     LABD3   			; A3AF 4C D3 AB                 L..

; ----------------------------------------------------------------------------
sub_a3b2:
	lda     word_AC     			; TODO Set cursor X = $AC (172)?
	sta     word_9C

	lda     word_AC+1     			; TODO Set cursor Y = $AD (173)?
	sta     word_9C+1

	lda     $AE     			; TODO Set cursor X = $AE (174)?
	sta     $A4     			; TODO Save cursor X for zoomed display?

	lda     $AF     			; TODO Set cursor Y = $AE (174)? 
	sta     $A5     			; TODO Save cursor Y for zoomed display?

sub_a3c2:
	jsr     LA882   			; A3C2 20 82 A8                  ..
	jmp     LA857   			; A3C5 4C 57 A8                 LW.

; ----------------------------------------------------------------------------
; TODO Initialize screen cursor variables?
; ----------------------------------------------------------------------------
sub_a3c8:
	lda     #$BA    			; A3C8 A9 BA                    ..
	sta     $A6     			; A3CA 85 A6                    ..

	lda     #$74    			; A3CC A9 74                    .t
	sta     $9E     			; A3CE 85 9E                    ..

	ldx     #$01    			; A3D0 A2 01                    ..
	stx     $9F     			; A3D2 86 9F                    ..

	dex             			; Reset cursor coodinates to 0
	stx     word_9C
	stx     word_9C+1
	stx     $A4     			; TODO Cursor x coordinate in zoomed mode?
	stx     $A5     			; TODO Cursor y coordinate in zoomed mode?

sub_a3dd:  
	rts             			; 

; ----------------------------------------------------------------------------

sub_a3de:
	jsr     sub_a890
	sta     byte_D9 			; A3E1 85 D9                    ..
	sec             			; A3E3 38                       8
	lda     $A4     			; A3E4 A5 A4                    ..
	sbc     byte_D9 			; A3E6 E5 D9                    ..
	sta     $A4     			; A3E8 85 A4                    ..
	bcs     LA3EE   			; A3EA B0 02                    ..
	dec     $A5     			; A3EC C6 A5                    ..
LA3EE:  lda     word_9C
	sec             			; A3F0 38                       8
	sbc     $D8     			; A3F1 E5 D8                    ..
	sta     word_9C
	bcs     sub_a3dd   			; A3F5 B0 E6                    ..
	dec     word_9C+1
	bpl     sub_a3dd   			; A3F9 10 E2                    ..
	lda     #$40    			; A3FB A9 40                    .@
	clc             			; A3FD 18                       .
	adc     $A4     			; A3FE 65 A4                    e.
	sta     $A4     			; A400 85 A4                    ..
	lda     #$01    			; A402 A9 01                    ..
	sta     $A5     			; A404 85 A5                    ..
	sta     word_9C+1     			; A406 85 9D                    ..
	bne     sub_a3dd   			; A408 D0 D3                    ..

sub_a40a:
	jsr     LA882   			; A40A 20 82 A8                  ..
	jmp     LA82D   			; A40D 4C 2D A8                 L-.

; ----------------------------------------------------------------------------
	txa             			; A410 8A                       .
	and     #$03    			; A411 29 03                    ).
	tay             			; A413 A8                       .
	lda     LB9AE,y 			; A414 B9 AE B9                 ...
	sta     chset6_base     		; A417 85 E8                    ..
	lda     LB9B2,y 			; A419 B9 B2 B9                 ...
	sta     chset6_base+1   		; A41C 85 E9                    ..
	lda     LB9A6,y 			; A41E B9 A6 B9                 ...
	sta     $EB     			; A421 85 EB                    ..
	sty     byte_B7
	lda     LB9AA,y 			; A425 B9 AA B9                 ...
	sta     $EA     			; A428 85 EA                    ..
	rts             			; A42A 60                       `

; ----------------------------------------------------------------------------

sub_a42b:
	asl     byte_B3 			; A42B 06 B3                    ..
	sec             			; A42D 38                       8
	ror     byte_B3 			; A42E 66 B3                    f.
	rts             			; A430 60                       `

; ----------------------------------------------------------------------------
	lda     #$00    			; A431 A9 00                    ..
LA433:  ldx     #$03    			; A433 A2 03                    ..
	jmp     LA111   			; A435 4C 11 A1                 L..

; ----------------------------------------------------------------------------
	lda     #$05    			; A438 A9 05                    ..
	bne     LA433   			; A43A D0 F7                    ..
	lda     #$02    			; A43C A9 02                    ..
	bne     LA433   			; A43E D0 F3                    ..
	lda     #$03    			; A440 A9 03                    ..
	bne     LA433   			; A442 D0 EF                    ..
	lda     #$04    			; A444 A9 04                    ..
	bne     LA433   			; A446 D0 EB                    ..

; ----------------------------------------------------------------------------
sub_a448:
	jsr     sub_a8b3
	ldx     $DA     			; A44B A6 DA                    ..
	bne     LA45F   			; A44D D0 10                    ..
	ldx     #$03    			; A44F A2 03                    ..
LA451:  lda     $A4,x   			; A451 B5 A4                    ..
	sta     $A8,x   			; A453 95 A8                    ..
	lda     word_9C,x   			; A455 B5 9C                    ..
	sta     $F0,x   			; A457 95 F0                    ..
	dex             			; A459 CA                       .
	bpl     LA451   			; A45A 10 F5                    ..
	stx     $DA     			; A45C 86 DA                    ..
	rts             			; A45E 60                       `

; ----------------------------------------------------------------------------
LA45F:  lda     $BA     			; A45F A5 BA                    ..
	sta     $C9     			; A461 85 C9                    ..
	bne     LA474   			; A463 D0 0F                    ..
	lda     $9F     			; A465 A5 9F                    ..
	sta     $FB     			; A467 85 FB                    ..
	lda     word_9C     			; A469 A5 9C                    ..
	ldx     word_9C+1     			; A46B A6 9D                    ..
	ldy     $9E     			; A46D A4 9E                    ..
	jsr     LA489   			; A46F 20 89 A4                  ..
	dec     $C9     			; A472 C6 C9                    ..
LA474:  ldx     #$04    			; A474 A2 04                    ..
LA476:  lda     $A7,x   			; A476 B5 A7                    ..
	sta     $EF,x   			; A478 95 EF                    ..
	dex             			; A47A CA                       .
	bne     LA476   			; A47B D0 F9                    ..
	stx     $FB     			; A47D 86 FB                    ..
	stx     $F3     			; A47F 86 F3                    ..
	stx     $DA     			; A481 86 DA                    ..
	lda     $A4     			; A483 A5 A4                    ..
	ldx     $A5     			; A485 A6 A5                    ..
	ldy     $A6     			; A487 A4 A6                    ..
LA489:  sta     $F8     			; A489 85 F8                    ..
	stx     $F9     			; A48B 86 F9                    ..
	sty     $FA     			; A48D 84 FA                    ..
	cpx     $F1     			; A48F E4 F1                    ..
	bcc     LA4A5   			; A491 90 12                    ..
	bne     LA499   			; A493 D0 04                    ..
	cmp     $F0     			; A495 C5 F0                    ..
	bcc     LA4A5   			; A497 90 0C                    ..
LA499:  ldy     $F1     			; A499 A4 F1                    ..
	stx     $F1     			; A49B 86 F1                    ..
	sty     $F9     			; A49D 84 F9                    ..
	ldy     $F0     			; A49F A4 F0                    ..
	sta     $F0     			; A4A1 85 F0                    ..
	sty     $F8     			; A4A3 84 F8                    ..
LA4A5:  lda     $FA     			; A4A5 A5 FA                    ..
	ldx     $F3     			; A4A7 A6 F3                    ..
	cpx     $FB     			; A4A9 E4 FB                    ..
	bcc     LA4BF   			; A4AB 90 12                    ..
	bne     LA4B3   			; A4AD D0 04                    ..
	cmp     $F2     			; A4AF C5 F2                    ..
	bcs     LA4BF   			; A4B1 B0 0C                    ..
LA4B3:  ldy     $FB     			; A4B3 A4 FB                    ..
	stx     $FB     			; A4B5 86 FB                    ..
	sty     $F3     			; A4B7 84 F3                    ..
	ldy     $F2     			; A4B9 A4 F2                    ..
	sta     $F2     			; A4BB 85 F2                    ..
	sty     $FA     			; A4BD 84 FA                    ..
LA4BF:  ldx     #$03    			; A4BF A2 03                    ..
	bit     $C9     			; A4C1 24 C9                    $.
LA4C3:  lda     $F8,x   			; A4C3 B5 F8                    ..
	bvs     LA4CD   			; A4C5 70 06                    p.
	sta     word_9C,x   			; A4C7 95 9C                    ..
	ldy     #$40    			; A4C9 A0 40                    .@
	bvc     LA4D1   			; A4CB 50 04                    P.
LA4CD:  sta     $A4,x   			; A4CD 95 A4                    ..
	ldy     #$28    			; A4CF A0 28                    .(
LA4D1:  dex             			; A4D1 CA                       .
	bpl     LA4C3   			; A4D2 10 EF                    ..
	sty     byte_D9 			; A4D4 84 D9                    ..
	bvs     LA4DD   			; A4D6 70 05                    p.
	bit     $BA     			; A4D8 24 BA                    $.
	bpl     LA4DD   			; A4DA 10 01                    ..
	rts             			; A4DC 60                       `

; ----------------------------------------------------------------------------
LA4DD:  jsr     sub_ad8e
	bit     $B0     			; A4E0 24 B0                    $.
	lda     $F8     			; A4E2 A5 F8                    ..
	and     #$07    			; A4E4 29 07                    ).
	tax             			; A4E6 AA                       .
	lda     LB94E,x 			; A4E7 BD 4E B9                 .N.
	beq     LA4F2   			; A4EA F0 06                    ..
	bvc     LA4F0   			; A4EC 50 02                    P.
	eor     #$FF    			; A4EE 49 FF                    I.
LA4F0:  inc     off_F4
LA4F2:  sta     $AB     			; A4F2 85 AB                    ..
	lda     $F1     			; A4F4 A5 F1                    ..
	lsr     a       			; A4F6 4A                       J
	lda     $F0     			; A4F7 A5 F0                    ..
	ror     a       			; A4F9 6A                       j
	lsr     a       			; A4FA 4A                       J
	lsr     a       			; A4FB 4A                       J
	sta     $F5     			; A4FC 85 F5                    ..
	lda     $F0     			; A4FE A5 F0                    ..
	and     #$07    			; A500 29 07                    ).
	tax             			; A502 AA                       .
	lda     LB956,x 			; A503 BD 56 B9                 .V.
	bne     :+
	inc     $F5     			; A508 E6 F5                    ..
:	ldy     #$FF    			; A50A A0 FF                    ..
	bvc     :+
	iny             			; A50E C8                       .
	tax             			; A50F AA                       .
	beq     :+
	eor     #$FF    			; A512 49 FF                    I.
:	sta     $A7     			; A514 85 A7                    ..
	sty     $E7     			; A516 84 E7                    ..
	sec             			; A518 38                       8
	lda     off_FA
	sbc     $F2     			; A51B E5 F2                    ..
	eor     #$FF    			; A51D 49 FF                    I.
	sta     $F6     			; A51F 85 F6                    ..
	lda     off_FA+1
	sbc     $F3     			; A523 E5 F3                    ..
	eor     #$FF    			; A525 49 FF                    I.
	sta     $F7     			; A527 85 F7                    ..
	lda     $F5     			; A529 A5 F5                    ..
	sec             			; A52B 38                       8
	sbc     off_F4
	sta     off_F4
	bmi     LA565   			; A530 30 33                    03
LA532:  bit     $B0     			; A532 24 B0                    $.
	ldy     #$00    			; A534 A0 00                    ..
	lda     $AB     			; A536 A5 AB                    ..
	beq     LA53D   			; A538 F0 03                    ..
	jsr     sub_a596
LA53D:  ldx     off_F4
	beq     LA549   			; A53F F0 08                    ..
	lda     $E7     			; A541 A5 E7                    ..
LA543:  jsr     sub_a596
	dex             			; A546 CA                       .
	bne     LA543   			; A547 D0 FA                    ..
LA549:  lda     $A7     			; A549 A5 A7                    ..
	beq     LA550   			; A54B F0 03                    ..
	jsr     sub_a596
LA550:  inc     $F6     			; A550 E6 F6                    ..
	bne     LA558   			; A552 D0 04                    ..
	inc     $F7     			; A554 E6 F7                    ..
	beq     LA573   			; A556 F0 1B                    ..
LA558:  lda     byte_D9 			; A558 A5 D9                    ..
	clc             			; A55A 18                       .
	adc     off_E3
	sta     off_E3
	bcc     LA532   			; A55F 90 D1                    ..
	inc     off_E3+1
	bne     LA532   			; A563 D0 CD                    ..
LA565:  lda     $AB     			; A565 A5 AB                    ..
	ora     $A7     			; A567 05 A7                    ..
	sta     $AB     			; A569 85 AB                    ..
	lda     #$00    			; A56B A9 00                    ..
	sta     $A7     			; A56D 85 A7                    ..
	sta     off_F4
	beq     LA532   			; A571 F0 BF                    ..
LA573:  bit     $C9     			; A573 24 C9                    $.
	bvc     LA58C   			; A575 50 15                    P.
	lda     $9E     			; A577 A5 9E                    ..
	sec             			; A579 38                       8
	sbc     #$0B    			; A57A E9 0B                    ..
	sta     $9E     			; A57C 85 9E                    ..
	bcs     LA582   			; A57E B0 02                    ..
	dec     $9F     			; A580 C6 9F                    ..
LA582:  lda     $9F     			; A582 A5 9F                    ..
	lsr     a       			; A584 4A                       J
	lda     $9E     			; A585 A5 9E                    ..
	ror     a       			; A587 6A                       j
	adc     #$00    			; A588 69 00                    i.
	sta     $A6     			; A58A 85 A6                    ..
LA58C:  lda     #<(LB863-1)
	ldy     #>(LB863-1)
	ldx     #$0C
	jsr     sub_b84b
	rts             			; A595 60                       `

; ----------------------------------------------------------------------------
sub_a596:
	bvc     :+
	and     (off_E3),y
	bvs     :++
:  	ora     (off_E3),y
:	sta     (off_E3),y
	iny             			; A5A0 C8                       .
	rts             			; A5A1 60                       `

; ----------------------------------------------------------------------------
sub_a5a2:
	lda     $A5     			; A5A2 A5 A5                    ..
	lsr     a       			; A5A4 4A                       J
	lda     $A4     			; A5A5 A5 A4                    ..
	jmp     LA5AF   			; A5A7 4C AF A5                 L..

; ----------------------------------------------------------------------------
sub_a5aa:
	lda     word_9C+1     			; A5AA A5 9D                    ..
	lsr     a       			; A5AC 4A                       J
	lda     word_9C     			; A5AD A5 9C                    ..
LA5AF:  ror     a       			; A5AF 6A                       j
	lsr     a       			; A5B0 4A                       J
	lsr     a       			; A5B1 4A                       J
	sta     off_F4
	rts             			; A5B4 60                       `

; ----------------------------------------------------------------------------

sub_a5b5:
	lda     $DC     			; A5B5 A5 DC                    ..
	and     #$04    			; A5B7 29 04                    ).
	beq     LA5E3   			; A5B9 F0 28                    .(
	lda     $DB     			; A5BB A5 DB                    ..
	and     #$20    			; A5BD 29 20                    ) 
	beq     LA5E4   			; A5BF F0 23                    .#
	jsr     sub_a5f6   			; A5C1 20 F6 A5                  ..
	ldx     $C1     			; A5C4 A6 C1                    ..
	beq     :+
	bmi     LA5D1   			; A5C8 30 07                    0.
	dex             			; A5CA CA                       .
	beq     LA5E3   			; A5CB F0 16                    ..
:	lda     #$01    			; A5CD A9 01                    ..
	bne     LA5D7   			; A5CF D0 06                    ..

; ----------------------------------------------------------------------------
LA5D1:  lda     #$FE    			; A5D1 A9 FE                    ..
	ldx     #$00    			; A5D3 A2 00                    ..
	beq     LA5D9   			; A5D5 F0 02                    ..
LA5D7:  ldx     byte_C4 			; A5D7 A6 C4                    ..
LA5D9:  sta     $C1     			; A5D9 85 C1                    ..
	stx     HPOSM1
	inx             			; A5DE E8                       .
	inx             			; A5DF E8                       .
	stx     HPOSM0
LA5E3:  rts             			; A5E3 60                       `

; ----------------------------------------------------------------------------
LA5E4:  lda     $C1     			; A5E4 A5 C1                    ..
	beq     LA5E3   			; A5E6 F0 FB                    ..
	bmi     LA5F0   			; A5E8 30 06                    0.
	lsr     a       			; A5EA 4A                       J
	bne     LA5E3   			; A5EB D0 F6                    ..
	tax             			; A5ED AA                       .
	beq     LA5D9   			; A5EE F0 E9                    ..
LA5F0:  ldx     #$00    			; A5F0 A2 00                    ..
	lda     #$FF    			; A5F2 A9 FF                    ..
	bne     LA5D9   			; A5F4 D0 E3                    ..

; ----------------------------------------------------------------------------
sub_a5f6:
	ldx     #$00    			; A5F6 A2 00                    ..
	stx     HPOSM3
	stx     HPOSM2
	rts             			; A5FE 60                       `

; ----------------------------------------------------------------------------

sub_a5ff:
	lda     L3E2E   			; A5FF AD 2E 3E                 ..>
	cmp     #$50    			; A602 C9 50                    .P
	bne     :+
	lda     #$1B    			; A606 A9 1B                    ..
	jsr     sub_ab54
	lda     #$50    			; A60B A9 50                    .P
	jsr     sub_ab54
	lda     #$43    			; A610 A9 43                    .C
	bne     LA63D   			; A612 D0 29                    .)
:	cmp     #$71    			; A614 C9 71                    .q
	bne     :+
	lda     #$1B    			; A618 A9 1B                    ..
	jsr     sub_ab54
	lda     #$4E    			; A61D A9 4E                    .N
	bne     LA638   			; A61F D0 17                    ..

; ----------------------------------------------------------------------------
:	cmp     #$72    			; A621 C9 72                    .r
	beq     :++
	cmp     #$73    			; A625 C9 73                    .s
	beq     :++
	cmp     #$7B    			; A629 C9 7B                    .{
	bne     :+
	jsr     sub_b828
:	rts             			; A630 60                       `

; ----------------------------------------------------------------------------
:	lda     #$1B    			; A631 A9 1B                    ..
	jsr     sub_ab54
	lda     #$40    			; A636 A9 40                    .@
LA638:  jsr     sub_ab54
	lda     #$42    			; A63B A9 42                    .B
LA63D:  jmp     sub_ab54

; ----------------------------------------------------------------------------

sub_a640:
	lda     $DC     			; A640 A5 DC                    ..
	ldx     $DB     			; A642 A6 DB                    ..
	and     #$07    			; A644 29 07                    ).
	lsr     a       			; A646 4A                       J
	sta     off_E3+1
	sta     $DC     			; A649 85 DC                    ..
	txa             			; A64B 8A                       .
	ror     a       			; A64C 6A                       j
	sta     off_E3
	lsr     $DC     			; A64F 46 DC                    F.
	ror     a       			; A651 6A                       j
	sta     byte_D9 			; A652 85 D9                    ..
	ldx     $DC     			; A654 A6 DC                    ..
	stx     $D8     			; A656 86 D8                    ..
	lsr     $DC     			; A658 46 DC                    F.
	ror     a       			; A65A 6A                       j
	adc     byte_D9 			; A65B 65 D9                    e.
	sta     off_F4
	lda     #$0C    			; A65F A9 0C                    ..
	adc     $D8     			; A661 65 D8                    e.
	sta     off_F4+1
	lda     off_E3
	adc     byte_D9 			; A667 65 D9                    e.
	sta     off_E3
	lda     off_E3+1
	adc     $D8     			; A66D 65 D8                    e.
	adc     #$06    			; A66F 69 06                    i.
	sta     off_E3+1
LA673:  lda     #$00    			; A673 A9 00                    ..
	sta     byte_D9 			; A675 85 D9                    ..
	sta     $D8     			; A677 85 D8                    ..
	ldx     #$1D    			; A679 A2 1D                    ..
:	sta     $3E10,x 			; A67B 9D 10 3E                 ..>
	dex             			; A67E CA                       .
	bpl     :-
	rts             			; A681 60                       `

; ----------------------------------------------------------------------------

sub_a682:
	jsr     sub_a8b3
	lda     $BA     			; A685 A5 BA                    ..
	sta     $C9     			; A687 85 C9                    ..
	bne     LA695   			; A689 D0 0A                    ..
	jsr     sub_ad8e
	lda     word_9C     			; A68E A5 9C                    ..
	jsr     LA69A   			; A690 20 9A A6                  ..
	dec     $C9     			; A693 C6 C9                    ..
LA695:  jsr     sub_ad8e
	lda     $A4     			; A698 A5 A4                    ..
LA69A:  and     #$07    			; A69A 29 07                    ).
	tax             			; A69C AA                       .
	ldy     #$00    			; A69D A0 00                    ..
	jmp     sub_b004

;*******************************************************************************
;*                                                                             *
;*                                 clear_screen                                *
;*                                                                             *
;*******************************************************************************
LA6A2:  
clear_screen:
;** (n) Turn off ANTIC to hide the work and speed things up ********************
	ldy     #$00    			; 
	sty     SDMCTL  			; Turn off ANTIC

;** (n) Load self modifying code #1 into zero page RAM *************************
	ldx     #$1B    			; Copy instructions from ROM to RAM
:	lda     LA6D0,x 			; Must be self-modifying code
	sta     a:L0080,x       		;
	dex             			;
	bpl     :-				; End loop after 27 iterations

;** (n) Run self modifying code #1 *******************************************
	tya             			; Let Y = $00. Used for background
	bit     $BA     			; A6B3 24 BA                    $.
	jsr     L0080   			; Run self-modifying code #1

;** (n) Load self modifying code #2 into zero page RAM *************************
	ldx     #$09    			; Copy instructions from ROM to RAM
:	lda     LA6EC,x 			; Must be self-modifying code
	sta     a:$82,x 			; Leave 'sta' instruction from code #1
	dex             			; 
	bpl     :-				; End loop after 9 iterations

;** (n) Run self modifying code #2 *******************************************
	ldx     #$08    			; A6C3 A2 08                    ..
	lda     #$20    			; A6C5 A9 20                    . 
	jsr     L0080   			; self-modifying code #2

;** (n) Turn ANTIC back on and restore previous playfield/dma settings *******
	lda     #$26    			;
	sta     SDMCTL   			; Turn on ANTIC, DMA, etc 
	rts             			; 

; ----------------------------------------------------------------------------
; Self-modifying code #1 to be copied from ROM to RAM
; ----------------------------------------------------------------------------
LA6D0:  sta     L2000,y 			; A6D0 99 00 20                 .. 
	bvs     :+
	sta     L4000,y 			; A6D5 99 00 40                 ..@
	sta     L6000,y 			; A6D8 99 00 60                 ..`
	sta     L8000,y 			; A6DB 99 00 80                 ...
:	iny             			; A6DE C8                       .
	bne     LA6D0   			; A6DF D0 EF                    ..
	inc     $82     			; Self modifying code
	inc     $87     			; Self modifying code
	inc     $8D     			; Self modifying code
	inc     $8A     			; Self modifying code
	.byte   $10     			; A6E9 10	At runtime, turns into "bpl L0080" 
LA6EA:  sbc     $60     			; A6EA E5 60	At runtime, turns into "rts"

; ----------------------------------------------------------------------------
; Self-modifying code #2 to be copied from ROM to RAM
; ----------------------------------------------------------------------------
LA6EC:  clc             			; A6EC 18                       .
	iny             			; A6ED C8                       .
	bne     LA6EA   			; A6EE D0 FA                    ..
	inc     $82     			; Self modifying code
	dex             			; A6F2 CA                       .
	bne     LA6EA   			; A6F3 D0 F5                    ..
	rts             			; A6F5 60                       `

; ----------------------------------------------------------------------------

sub_a6f6:
	jsr     sub_a7fe
	ldx     $D8     			; A6F9 A6 D8                    ..
	ldy     #$0F    			; A6FB A0 0F                    ..
LA6FD:  lsr     $DC     			; A6FD 46 DC                    F.
	ror     $DB     			; A6FF 66 DB                    f.
	lda     LB93A,x 			; A701 BD 3A B9                 .:.
	and     (off_E3),y
	bcc     LA726   			; A706 90 1E                    ..
	inc     byte_D9 			; A708 E6 D9                    ..
	pha             			; A70A 48                       H
	lda     LB9BE,y 			; A70B B9 BE B9                 ...
	tax             			; A70E AA                       .
	lda     LB9F4,x 			; A70F BD F4 B9                 ...
	sta     $AB     			; A712 85 AB                    ..
	ldx     $D8     			; A714 A6 D8                    ..
	lda     LB9CE,x 			; A716 BD CE B9                 ...
	clc             			; A719 18                       .
	adc     $AB     			; A71A 65 AB                    e.
	tax             			; A71C AA                       .
	inc     $3E10,x 			; A71D FE 10 3E                 ..>
	pla             			; A720 68                       h
	ldx     $D8     			; A721 A6 D8                    ..
	ora     LB942,x 			; A723 1D 42 B9                 .B.
LA726:  sta     (off_E3),y
	dey             			; A728 88                       .
	bpl     LA6FD   			; A729 10 D2                    ..
	inx             			; A72B E8                       .
	cpx     #$08    			; A72C E0 08                    ..
	bcs     LA733   			; A72E B0 03                    ..
	stx     $D8     			; A730 86 D8                    ..
	rts             			; A732 60                       `

; ----------------------------------------------------------------------------
LA733:  lda     #$00    			; A733 A9 00                    ..
	tax             			; A735 AA                       .
	ldy     #$05    			; A736 A0 05                    ..
LA738:  sta     (off_F4),y
	dey             			; A73A 88                       .
	bpl     LA738   			; A73B 10 FB                    ..
	lda     byte_D9 			; A73D A5 D9                    ..
	cmp     #$36    			; A73F C9 36                    .6
	bcc     LA771   			; A741 90 2E                    ..
	dex             			; A743 CA                       .
	cmp     #$55    			; A744 C9 55                    .U
	bcs     LA771   			; A746 B0 29                    .)
	ldy     #$05    			; A748 A0 05                    ..
LA74A:  ldx     #$04    			; A74A A2 04                    ..
	stx     $AB     			; A74C 86 AB                    ..
LA74E:  lda     LB9F4,y 			; A74E B9 F4 B9                 ...
	clc             			; A751 18                       .
	adc     $AB     			; A752 65 AB                    e.
	tax             			; A754 AA                       .
	lda     LB9D6,x 			; A755 BD D6 B9                 ...
	cmp     $3E10,x 			; A758 DD 10 3E                 ..>
	bcc	:+
	bne     LA768   			; A75D D0 09                    ..
:	lda     (off_F4),y
	ldx     $AB     			; A761 A6 AB                    ..
	ora     LB942,x 			; A763 1D 42 B9                 .B.
	sta     (off_F4),y
LA768:  dec     $AB     			; A768 C6 AB                    ..
	bpl     LA74E   			; A76A 10 E2                    ..
	dey             			; A76C 88                       .
	bpl     LA74A   			; A76D 10 DB                    ..
	bmi     LA7AD   			; A76F 30 3C                    0<
LA771:  stx     byte_D9 			; A771 86 D9                    ..
	bit     byte_D9 			; A773 24 D9                    $.
	ldy     #$0F    			; A775 A0 0F                    ..
LA777:  lda     (off_E3),y
	bvc     :+
	eor     #$FF    			; A77B 49 FF                    I.
:	sta     $D8     			; A77D 85 D8                    ..
	ldx     #$07    			; A77F A2 07                    ..
	lda     #$00    			; A781 A9 00                    ..
LA783:  rol     $D8     			; A783 26 D8                    &.
	bcc     LA78A   			; A785 90 03                    ..
	ora     LB9B6,x 			; A787 1D B6 B9                 ...
LA78A:  dex             			; A78A CA                       .
	bpl     LA783   			; A78B 10 F6                    ..
	pha             			; A78D 48                       H
	lda     LB9BE,y 			; A78E B9 BE B9                 ...
	sty     $D8     			; A791 84 D8                    ..
	tay             			; A793 A8                       .
	pla             			; A794 68                       h
	ora     (off_F4),y
	sta     (off_F4),y
	ldy     $D8     			; A799 A4 D8                    ..
	dey             			; A79B 88                       .
	bpl     LA777   			; A79C 10 D9                    ..
	bvc     LA7AD   			; A79E 50 0D                    P.
	ldy     #$05    			; A7A0 A0 05                    ..
LA7A2:  lda     (off_F4),y
	eor     #$FF    			; A7A4 49 FF                    I.
	and     #$F8    			; A7A6 29 F8                    ).
	sta     (off_F4),y
	dey             			; A7AA 88                       .
	bpl     LA7A2   			; A7AB 10 F5                    ..
LA7AD:  ldy     #$02    			; A7AD A0 02                    ..
	sty     $EE     			; A7AF 84 EE                    ..
	iny             			; A7B1 C8                       .
	sty     $EC     			; A7B2 84 EC                    ..
LA7B4:  ldy     $EC     			; A7B4 A4 EC                    ..
	lda     (off_E3),y
	ldy     $EE     			; A7B8 A4 EE                    ..
	bvc     LA7C0   			; A7BA 50 04                    P.
	and     (off_E3),y
	bvs     LA7C2   			; A7BE 70 02                    p.
LA7C0:  ora     (off_E3),y
LA7C2:  sta     (off_E3),y
	ldx     #$02    			; A7C4 A2 02                    ..
LA7C6:  inc     $EE     			; A7C6 E6 EE                    ..
	inc     $EC     			; A7C8 E6 EC                    ..
	ldy     $EC     			; A7CA A4 EC                    ..
	cpy     #$10    			; A7CC C0 10                    ..
	bcs     LA7DD   			; A7CE B0 0D                    ..
	lda     (off_E3),y
	ldy     $EE     			; A7D2 A4 EE                    ..
	sta     (off_E3),y
	dex             			; A7D6 CA                       .
	bpl     LA7C6   			; A7D7 10 ED                    ..
	inc     $EC     			; A7D9 E6 EC                    ..
	bne     LA7B4   			; A7DB D0 D7                    ..
LA7DD:  lda     #$0C    			; A7DD A9 0C                    ..
	clc             			; A7DF 18                       .
	adc     off_E3
	sta     off_E3
	bcc     LA7E8   			; A7E4 90 02                    ..
	inc     off_E3+1
LA7E8:  clc             			; A7E8 18                       .
	lda     off_F4
	adc     #$06    			; A7EB 69 06                    i.
	sta     off_F4
	bcc	:+
	inc     off_F4+1
:	jmp     LA673   			; A7F3 4C 73 A6                 Ls.

; ----------------------------------------------------------------------------
sub_a7f6:
	ldx     $B6     			; A7F6 A6 B6                    ..
	lda     $3E2E,x 			; A7F8 BD 2E 3E                 ..>
	inc     $B6     			; A7FB E6 B6                    ..
	rts             			; A7FD 60                       `

; ----------------------------------------------------------------------------
sub_a7fe:
	jsr     sub_a7f6
	and     #$3F    			; A801 29 3F                    )?
	sta     $DB     			; A803 85 DB                    ..
	jsr     sub_a7f6
	and     #$3F    			; A808 29 3F                    )?
	sta     $DC     			; A80A 85 DC                    ..
	lda     #$00    			; A80C A9 00                    ..
	lsr     $DC     			; A80E 46 DC                    F.
	ror     a       			; A810 6A                       j
	lsr     $DC     			; A811 46 DC                    F.
	ror     a       			; A813 6A                       j
	ora     $DB     			; A814 05 DB                    ..
	sta     $DB     			; A816 85 DB                    ..
	jsr     sub_a7f6
	asl     a       			; A81B 0A                       .
	asl     a       			; A81C 0A                       .
	asl     a       			; A81D 0A                       .
	asl     a       			; A81E 0A                       .
	ora     $DC     			; A81F 05 DC                    ..
	sta     $DC     			; A821 85 DC                    ..
	rts             			; A823 60                       `

; ----------------------------------------------------------------------------
LA824:  lda     #$02    			; A824 A9 02                    ..
	ldy     #$04    			; A826 A0 04                    ..
	jsr     LA886   			; A828 20 86 A8                  ..
	bcs     LA857   			; A82B B0 2A                    .*
LA82D:  adc     $A6     			; A82D 65 A6                    e.
	sta     $A6     			; A82F 85 A6                    ..
	lda     $D8     			; A831 A5 D8                    ..
	clc             			; A833 18                       .
	adc     $9E     			; A834 65 9E                    e.
	sta     $9E     			; A836 85 9E                    ..
	bcc     LA83C   			; A838 90 02                    ..
	inc     $9F     			; A83A E6 9F                    ..
LA83C:	lda	$9F
	cmp     #$01    			; A83E C9 01                    ..
	bcc     LA856   			; A840 90 14                    ..
	lda     $9E     			; A842 A5 9E                    ..
	cmp     #$80    			; A844 C9 80                    ..
	bcc     LA856   			; A846 90 0E                    ..
	sec             			; A848 38                       8
	sbc     #$80    			; A849 E9 80                    ..
	sta     $9E     			; A84B 85 9E                    ..
	lsr     $9F     			; A84D 46 9F                    F.
	sec             			; A84F 38                       8
	lda     $A6     			; A850 A5 A6                    ..
	sbc     #$C0    			; A852 E9 C0                    ..
	sta     $A6     			; A854 85 A6                    ..
LA856:  rts             			; A856 60                       `

; ----------------------------------------------------------------------------
LA857:  sta     byte_D9 			; A857 85 D9                    ..
	lda     $A6     			; A859 A5 A6                    ..
	sec             			; A85B 38                       8
	sbc     byte_D9 			; A85C E5 D9                    ..
	sta     $A6     			; A85E 85 A6                    ..
	sec             			; A860 38                       8
	lda     $9E     			; A861 A5 9E                    ..
	sbc     $D8     			; A863 E5 D8                    ..
	sta     $9E     			; A865 85 9E                    ..
	bcs     LA86B   			; A867 B0 02                    ..
	dec     $9F     			; A869 C6 9F                    ..
LA86B:  lda     $9F     			; A86B A5 9F                    ..
	bpl     LA881   			; A86D 10 12                    ..
	lda     $9E     			; A86F A5 9E                    ..
	clc             			; A871 18                       .
	adc     #$80    			; A872 69 80                    i.
	sta     $9E     			; A874 85 9E                    ..
	lda     #$01    			; A876 A9 01                    ..
	sta     $9F     			; A878 85 9F                    ..
	lda     $A6     			; A87A A5 A6                    ..
LA87C:  clc             			; A87C 18                       .
	adc     #$C0    			; A87D 69 C0                    i.
	sta     $A6     			; A87F 85 A6                    ..
LA881:  rts             			; A881 60                       `

; ----------------------------------------------------------------------------
; Called from end of print string
LA882:  lda     #$06    			; A882 A9 06                    ..
	ldy     #$0C    			; A884 A0 0C                    ..
LA886:  bit     $CA     			; A886 24 CA                    $.
	sty     $D8     			; A888 84 D8                    ..
	bvs     :+
	rts             			; TODO Return immediately if something
:	asl     a       			; A88D 0A                       .
	bvs     LA89C   			; A88E 70 0C                    p.

sub_a890:  
	bit     $CA     			; A890 24 CA                    $.
	lda     #$05    			; A892 A9 05                    ..
	ldy     #$08    			; A894 A0 08                    ..
	sty     $D8     			; A896 84 D8                    ..
	bvc     LA89E   			; A898 50 04                    P.
	lda     #$09    			; A89A A9 09                    ..
LA89C:  asl     $D8     			; A89C 06 D8                    ..
LA89E:  rts             			; A89E 60                       `

;*******************************************************************************
;*                                                                             *
;*                               print_string                                  *
;*                                                                             *
;*                        Display text on the screen                           *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; Display text on the screen
; Parameters: 
; A = MSB of address to text string
; X = LSB of address to text string
; Y = String length - 1
sub_a89f:					; print string
print_string:
	sty     $EE     			; String length - 1
	sta     off_EC				; Store MSB string address in ZP
	stx     off_EC+1			; Store LSB string address in ZP
:	ldy     $EE     			; Iterate through the characters
	lda     (off_EC),y
	jsr     sub_ab5d
	dec     $EE     			; A8AC C6 EE                    ..
	bpl     :-
	jmp     sub_a3b2   			; A8B0 4C B2 A3                 L..

; ----------------------------------------------------------------------------
sub_a8b3:
	jsr     sub_a7f6
	cmp     #$40    			; A8B6 C9 40                    .@
	bcs     LA901   			; A8B8 B0 47                    .G
	jsr     LA8DC   			; A8BA 20 DC A8                  ..
	lda     $BD     			; A8BD A5 BD                    ..
	and     #$1F    			; A8BF 29 1F                    ).
	ora     $DB     			; A8C1 05 DB                    ..
	sta     $BD     			; A8C3 85 BD                    ..
	lda     $DC     			; A8C5 A5 DC                    ..
	sta     $BE     			; A8C7 85 BE                    ..
	bcc     sub_a8b3
LA8CB:  jsr     LA8DC   			; A8CB 20 DC A8                  ..
	lda     $BB     			; A8CE A5 BB                    ..
	and     #$1F    			; A8D0 29 1F                    ).
	ora     $DB     			; A8D2 05 DB                    ..
	sta     $BB     			; A8D4 85 BB                    ..
	lda     $DC     			; A8D6 A5 DC                    ..
	sta     $BC     			; A8D8 85 BC                    ..
	bcc     sub_a8b3
LA8DC:  and     #$1F    			; A8DC 29 1F                    ).
	sta     $DC     			; A8DE 85 DC                    ..
	lda     #$00    			; A8E0 A9 00                    ..
	lsr     $DC     			; A8E2 46 DC                    F.
	ror     a       			; A8E4 6A                       j
	lsr     $DC     			; A8E5 46 DC                    F.
	ror     a       			; A8E7 6A                       j
	lsr     $DC     			; A8E8 46 DC                    F.
	ror     a       			; A8EA 6A                       j
	sta     $DB     			; A8EB 85 DB                    ..
	rts             			; A8ED 60                       `

; ----------------------------------------------------------------------------
LA8EE:  and     #$1F    			; A8EE 29 1F                    ).
	sta     $DB     			; A8F0 85 DB                    ..
	lda     $BD     			; A8F2 A5 BD                    ..
	and     #$E0    			; A8F4 29 E0                    ).
	ora     $DB     			; A8F6 05 DB                    ..
	sta     $BD     			; A8F8 85 BD                    ..
	jsr     sub_a7f6
	cmp     #$40    			; A8FD C9 40                    .@
	bcc     LA8CB   			; A8FF 90 CA                    ..
LA901:  cmp     #$60    			; A901 C9 60                    .`
	bcs     LA8EE   			; A903 B0 E9                    ..
	and     #$1F    			; A905 29 1F                    ).
	sta     $DB     			; A907 85 DB                    ..
	lda     $BB     			; A909 A5 BB                    ..
	and     #$E0    			; A90B 29 E0                    ).
	ora     $DB     			; A90D 05 DB                    ..
	sta     $BB     			; A90F 85 BB                    ..
	ldx     #$03    			; A911 A2 03                    ..
LA913:  lda     $BB,x   			; A913 B5 BB                    ..
	sta     word_9C,x   			; A915 95 9C                    ..
	dex             			; A917 CA                       .
	bpl     LA913   			; A918 10 F9                    ..
	lda     word_9C+1
	lsr     a       			; A91C 4A                       J
	lda     word_9C
	sta     $D8     			; A91F 85 D8                    ..
	ror     a       			; A921 6A                       j
	sta     byte_D9 			; A922 85 D9                    ..
	lsr     a       			; A924 4A                       J
	lsr     a       			; A925 4A                       J
	bcs     LA92A   			; A926 B0 02                    ..
	lsr     $D8     			; A928 46 D8                    F.
LA92A:  adc     byte_D9 			; A92A 65 D9                    e.
	sta     $A4     			; A92C 85 A4                    ..
	ldx     #$00    			; A92E A2 00                    ..
	stx     $A5     			; A930 86 A5                    ..
	bcc     LA936   			; A932 90 02                    ..
	inc     $A5     			; A934 E6 A5                    ..
LA936:  lda     $9E     			; A936 A5 9E                    ..
	and     #$07    			; A938 29 07                    ).
	tax             			; A93A AA                       .
	lsr     $9F     			; A93B 46 9F                    F.
	lda     $9E     			; A93D A5 9E                    ..
	ror     a       			; A93F 6A                       j
	sta     $D8     			; A940 85 D8                    ..
	lsr     a       			; A942 4A                       J
	sta     byte_D9 			; A943 85 D9                    ..
	lsr     a       			; A945 4A                       J
	clc             			; A946 18                       .
	adc     LB9FA,x 			; A947 7D FA B9                 }..
	adc     byte_D9 			; A94A 65 D9                    e.
	cmp     #$C0    			; A94C C9 C0                    ..
	bcc     LA952   			; A94E 90 02                    ..
	lda     #$BF    			; A950 A9 BF                    ..
LA952:  sta     $A6     			; A952 85 A6                    ..
	lda     $9E     			; A954 A5 9E                    ..
	and     #$03    			; A956 29 03                    ).
	cmp     #$01    			; A958 C9 01                    ..
	lda     $D8     			; A95A A5 D8                    ..
	adc     byte_D9 			; A95C 65 D9                    e.
	sta     $9E     			; A95E 85 9E                    ..
	rol     $9F     			; A960 26 9F                    &.
LA962:  rts             			; A962 60                       `

;*******************************************************************************
;*                                                                             *
;*                                sub_readkey                                  *
;*                                                                             *
;*                 Check for any key press. Return if nothing.                 *
;*                                                                             *
;*******************************************************************************
sub_a963:  
sub_readkey:
	ldx     CH				; Get keyboard code ($FF if none)
	inx             			; if key was pressed 
	bne     LA976   			; then goto LA976
	lda     HELPFG				; else examine HELP key
	beq     LA962   			; if no HELP key then jump to nearby RTS
	stx     HELPFG				; else save (but it's clobbered later)
	lda     #$0B    			; 
	jmp     sub_b1df			;

; ----------------------------------------------------------------------------
LA976:  dex             			; Restore original keyboard code
	bmi     LA9CE   			; Branch if key press + control key
	stx     $D8     			; Save keyboard code to RAM

;** (n) Test START key *******************************************************
	lda     CONSOL
	lsr     a       			; if START not pressed
	bcs     LA9AB   			; then skip ahead
	ldy     #$FF    			; else let Y = $FF

;** (n) START is pressed. Now test SELECT key ********************************
	lsr     a       			; if SELECT not pressed
	bcc     LA98A   			; then skip ahead
	cpx     #$40    			; else if key = shift + L
	bcc     LA98B   			; then
LA98A:  iny             			; let Y = $00
LA98B:  sty     byte_D9 			; A98B 84 D9                    ..
	lda     $D8     			; Let A = key code
	and     #$3F    			; Ignore SHIFT and/or CTRL
	ldy     #$30    			; 
LA993:  cmp     LBA12,y 			; Compare against key code lookup table
	beq     LA99F   			; A996 F0 07                    ..
	dey             			; A998 88                       .
	dey             			; A999 88                       .
	dey             			; A99A 88                       .
	bpl     LA993   			; A99B 10 F6                    ..
	bmi     LA9CE   			; A99D 30 2F                    0/
LA99F:  ldx     byte_D9 			; A99F A6 D9                    ..
	bmi     LA9A4   			; A9A1 30 01                    0.
	iny             			; A9A3 C8                       .
LA9A4:  iny             			; A9A4 C8                       .
	lda     LBA12,y 			; A9A5 B9 12 BA                 ...
LA9A8:  jmp     LAA91   			; A9A8 4C 91 AA                 L..

; ----------------------------------------------------------------------------
LA9AB:  lda     $D8     			; A9AB A5 D8                    ..
	cmp     #$27    			; A9AD C9 27                    .'
	bne     LA9B5   			; A9AF D0 04                    ..
	lda     #$7B    			; A9B1 A9 7B                    .{
	bne	LA9A8
LA9B5:  cmp     #$67    			; A9B5 C9 67                    .g
	bne     LA9BD   			; A9B7 D0 04                    ..
	lda     #$7F    			; A9B9 A9 7F                    ..
	bne     LA9A8   			; A9BB D0 EB                    ..
LA9BD:  cmp     #$3C    			; A9BD C9 3C                    .<
	bne     LA9C5   			; A9BF D0 04                    ..
	lda     #$00    			; A9C1 A9 00                    ..
	beq     LA9CB   			; A9C3 F0 06                    ..
LA9C5:  cmp     #$7C    			; A9C5 C9 7C                    .|
	bne     LA9DD   			; A9C7 D0 14                    ..
	lda     #$40    			; A9C9 A9 40                    .@
LA9CB:  sta     SHFLOK
LA9CE:  ldx     #$7F    			; A9CE A2 7F                    ..
LA9D0:  stx     CONSOL
	stx     WSYNC
	dex             			; A9D6 CA                       .
	bpl     LA9D0   			; A9D7 10 F7                    ..
	stx     CH
	rts             			; A9DC 60                       `

; ----------------------------------------------------------------------------
LA9DD:  ldy     #LB97E-LB96E			; Prepare CIO call to read keyboard
	jsr     call_cio_or_err			; Make CIO call 
	sta     $E7     			; A9E2 85 E7                    ..
	lda     CONSOL				; check console keys
	and     #$07				; mask irrelevant bits
	cmp     #$03				; is OPTION pressed?
	bne     LAA62				; no, skip out
	lda     SRTIMR				; check key repeat timer
	beq     LAA62   			; A9F0 F0 70                    .p
	lda     $D8     			; A9F2 A5 D8                    ..
	and     #$3F    			; A9F4 29 3F                    )?
	cmp     #$32    			; A9F6 C9 32                    .2
	bne     LAA03   			; A9F8 D0 09                    ..
	lda     $1353   			; A9FA AD 53 13                 .S.
	eor     #$80    			; A9FD 49 80                    I.
	sta     $1353   			; A9FF 8D 53 13                 .S.
	rts             			; AA02 60                       `

;** (n) If user pressed OPTION + '1' then change baud to 1200 ****************
LAA03:  cmp     #$1F    			; if keyboard press = '1'
	bne     :++				; then 
	lda     #$00    			;   $00 -> baud = 1200

;** (n) Skip all this if using Microbits 300 *********************************
:	ldy     IS_MPP				; 1->MPP 0->Modem
	bne     LAA23   			;

;** (n) Force new baud *******************************************************
	sta     CURRENT_BAUD			; Save new baud (FF=300 00=1200)
	jmp     sub_user_baud			; sub_user_baud will RTS

;** (n) If user pressed OPTION + '3' then change baud to 300 *****************
:	cmp     #$1A    			; if keyboard press = '3'
	bne     LAA1A   			; then
	lda     #$FF    			;   $FF -> baud = 300
	bne     :--				;   Jump back to store baud in $B1

;** (n) If user pressed OPTION + 'c' then change colors **********************
LAA1A:  cmp     #$12    			; if keyboard press = 'c'
	bne     LAA24   			; AA1C D0 06                    ..
	lda     #$00    			; AA1E A9 00                    ..
LAA20:  sta     $1355   			; AA20 8D 55 13                 .U.
LAA23:  rts             			; AA23 60                       `

; ----------------------------------------------------------------------------
LAA24:  cmp     #$15    			; AA24 C9 15                    ..
	bne     LAA2C   			; AA26 D0 04                    ..
	lda     #$80    			; AA28 A9 80                    ..
	bne     LAA20   			; AA2A D0 F4                    ..
LAA2C:  cmp     #$2D    			; AA2C C9 2D                    .-
	bne     LAA34   			; AA2E D0 04                    ..
	lda     #$C0    			; AA30 A9 C0                    ..
	bne     LAA20   			; AA32 D0 EC                    ..
LAA34:  cmp     #$38    			; AA34 C9 38                    .8
	bne     LAA56   			; AA36 D0 1E                    ..
	ldx     CURRENT_DL
	beq     LAA56   			; AA3A F0 1A                    ..
	lda     $C1     			; AA3C A5 C1                    ..
	beq     LAA44   			; AA3E F0 04                    ..
	cmp     #$02    			; AA40 C9 02                    ..
	bne     LAA23   			; AA42 D0 DF                    ..
LAA44:  eor     #$02    			; AA44 49 02                    I.
	sta     $C1     			; AA46 85 C1                    ..
	tax             			; AA48 AA                       .
	beq     LAA4D   			; AA49 F0 02                    ..
	ldx     #$32    			; AA4B A2 32                    .2
LAA4D:  stx     HPOSM3
	inx             			; AA50 E8                       .
	inx             			; AA51 E8                       .
	stx     HPOSM2
	rts             			; AA55 60                       `

; ----------------------------------------------------------------------------
LAA56:  cmp     #$17    			; AA56 C9 17                    ..
	beq     LAAAD   			; AA58 F0 53                    .S
	cmp     #$25    			; AA5A C9 25                    .%
	beq     LAAA7   			; AA5C F0 49                    .I
	cmp     #$0A    			; AA5E C9 0A                    ..
	beq     LAAAA   			; AA60 F0 48                    .H
LAA62:  ldy     #$0C    			; AA62 A0 0C                    ..
	lda     $D8     			; AA64 A5 D8                    ..
LAA66:  cmp     LBA45,y 			; AA66 D9 45 BA                 .E.
	beq     LAA8A   			; AA69 F0 1F                    ..
	dey             			; AA6B 88                       .
	dey             			; AA6C 88                       .
	bpl     LAA66   			; AA6D 10 F7                    ..
	ldy     #$0A    			; AA6F A0 0A                    ..
LAA71:  cmp     LBA53,y 			; AA71 D9 53 BA                 .S.
	beq     LAA7C   			; AA74 F0 06                    ..
	dey             			; AA76 88                       .
	dey             			; AA77 88                       .
	bpl     LAA71   			; AA78 10 F7                    ..
	bmi     LAA96   			; AA7A 30 1A                    0.
LAA7C:  sty     $E7     			; AA7C 84 E7                    ..
	lda     #$00    			; AA7E A9 00                    ..
	jsr     sub_ab54
	ldy     $E7     			; AA83 A4 E7                    ..
	lda     LBA54,y 			; AA85 B9 54 BA                 .T.
	bne     LAA8D   			; AA88 D0 03                    ..
LAA8A:  lda     LBA46,y 			; AA8A B9 46 BA                 .F.
LAA8D:  sta     $E7     			; AA8D 85 E7                    ..
	bne     LAA96   			; AA8F D0 05                    ..
LAA91:  sta     $E7     			; AA91 85 E7                    ..
	jsr     LA9CE   			; AA93 20 CE A9                  ..
LAA96:  bit     $1353   			; AA96 2C 53 13                 ,S.
	bmi     LAB00   			; AA99 30 65                    0e
	lda     $E7     			; AA9B A5 E7                    ..
	cmp     #$20    			; AA9D C9 20                    . 
	bcs     LAAFD   			; AA9F B0 5C                    .\
	jsr     sub_a0f3   			; AAA1 20 F3 A0                  ..
	jmp     LAB00   			; AAA4 4C 00 AB                 L..

; ----------------------------------------------------------------------------
LAAA7:  jmp     sub_b272

; ----------------------------------------------------------------------------
LAAAA:  jmp     LB28A   			; AAAA 4C 8A B2                 L..

; ----------------------------------------------------------------------------
LAAAD:  jsr     sub_a5f6   			; AAAD 20 F6 A5                  ..
	lda     $C1     			; AAB0 A5 C1                    ..
	cmp     #$02    			; AAB2 C9 02                    ..
	bne     LAAB8   			; AAB4 D0 02                    ..
	lda     #$00    			; AAB6 A9 00                    ..
LAAB8:  eor     #$FF    			; AAB8 49 FF                    I.
	sta     $C1     			; AABA 85 C1                    ..
	bmi     LAADB   			; AABC 30 1D                    0.
	beq     LAACA   			; AABE F0 0A                    ..
	ldx     byte_C4 			; AAC0 A6 C4                    ..
	stx     HPOSM1
	inx             			; AAC5 E8                       .
	inx             			; AAC6 E8                       .
	stx     HPOSM0
LAACA:  lda     FG_COLOR_DL1    		; AACA A5 D2                    ..
	sta     COLOR1  			; AACC 8D C5 02                 ...
	ldy     #$80    			; AACF A0 80                    ..
	sty     CURRENT_DL
	ldy     BG_COLOR_DL1    		; AAD3 A4 D3                    ..
	lda     #<$1000 			; LSB of Display List #1
	ldx     #>$1000 			; MSB of Display List #2
	bne     LAAF0   			; Jump to save DLIST pointer

LAADB:  ldx     #$00    			; AADB A2 00                    ..
	stx     HPOSM1
	stx     HPOSM0
	stx     CURRENT_DL
	lda     FG_COLOR_DL2    		; AAE5 A5 D0                    ..
	sta     COLOR1  			; AAE7 8D C5 02                 ...
	ldy     BG_COLOR_DL2    		; AAEA A4 D1                    ..
	lda     #<$10CA
	ldx     #>$10CA

LAAF0:  sta	DLIST
	stx	DLIST+1
	sty     COLOR2  			; AAF6 8C C6 02                 ...
	sty     COLOR4  			; AAF9 8C C8 02                 ...
	rts             			; AAFC 60                       `

; ----------------------------------------------------------------------------
LAAFD:  jsr     sub_ab5d   			; AAFD 20 5D AB                  ].
LAB00:  lda     $E7     			; AB00 A5 E7                    ..
LAB02:  jmp     sub_b1df

; ----------------------------------------------------------------------------
LAB05:  dey             			; AB05 88                       .
	sty     byte_C7 			; AB06 84 C7                    ..
	txa             			; AB08 8A                       .
	dex             			; AB09 CA                       .
	bmi     LAAAD   			; AB0A 30 A1                    0.
	bne     LAB53   			; AB0C D0 45                    .E
	jsr     sub_b828
	lda     #$1B    			; AB11 A9 1B                    ..
	jsr     sub_ab54
	lda     #$00    			; AB16 A9 00                    ..
	sta     $E7     			; AB18 85 E7                    ..
	sta     $4D     			; AB1A 85 4D                    .M
	lda     $BF     			; AB1C A5 BF                    ..
	lsr     a       			; AB1E 4A                       J
	ror     $E7     			; AB1F 66 E7                    f.
	lsr     a       			; AB21 4A                       J
	ror     $E7     			; AB22 66 E7                    f.
	pha             			; AB24 48                       H
	lda     $E7     			; AB25 A5 E7                    ..
	lsr     a       			; AB27 4A                       J
	lsr     a       			; AB28 4A                       J
	ora     #$40    			; AB29 09 40                    .@
	ora     $C0     			; AB2B 05 C0                    ..
	jsr     sub_ab54
	pla             			; AB30 68                       h
	ora     #$44    			; AB31 09 44                    .D
	bne     LAB02   			; always jumps

sub_ab35:
	ldx     $C1     			; AB35 A6 C1                    ..
	cpx     #$02    			; AB37 E0 02                    ..
	bne     LAB4F   			; AB39 D0 14                    ..
	lda     $C8     			; AB3B A5 C8                    ..
	beq     LAB4F   			; AB3D F0 10                    ..
	ldx     #$00    			; AB3F A2 00                    ..
	stx     $C8     			; AB41 86 C8                    ..
	stx     $4D     			; AB43 86 4D                    .M
	dex             			; AB45 CA                       .
	stx     byte_C7 			; AB46 86 C7                    ..
	pha             			; AB48 48                       H
	jsr     sub_b828
	pla             			; AB4C 68                       h
	bne     LAB02   			; AB4D D0 B3                    ..
LAB4F:  ldy     byte_C7 			; AB4F A4 C7                    ..
	beq     LAB05   			; AB51 F0 B2                    ..
LAB53:  rts             			; AB53 60                       `

; ----------------------------------------------------------------------------
sub_ab54:  	
	jsr     sub_b1df
	jmp     sub_b54f

; ----------------------------------------------------------------------------
sub_ab5a:
	lda     $3E2E   			; AB5A AD 2E 3E                 ..>

;*******************************************************************************
;*                                                                             *
;*                                print_char                                   *
;*                                                                             *
;*                        Draw character on the screen                         *
;*                                                                             *
;*******************************************************************************
; DESCRIPTION
; Parameters:
; A = character to be displayed
sub_ab5d:
print_char:
	sta     $E7     			; Character to be displayed
	sec             			; Test to see if non-printing character 
	sbc     #$20    			;
	bcs     :+				;
	rts             			; RTS now if non-printing character
:	pha             			; Save character-to-be-displayed
	ldx     $BA     			; AB66 A6 BA                    ..
	stx     $C9     			; AB68 86 C9                    ..
	bne     :+
	stx     $E6     			; AB6C 86 E6                    ..
	stx     $D8     			; AB6E 86 D8                    ..
	jsr	sub_abff
	dec     $C9     			; AB73 C6 C9                    ..
	ldx     $C9     			; AB75 A6 C9                    ..
:	pla             			; AB77 68                       h
	inx             			; AB78 E8                       .
	stx     $E6     			; AB79 86 E6                    ..
	ldx     $CA     			; AB7B A6 CA                    ..
	beq     LAB94   			; AB7D F0 15                    ..
	jsr     sub_ab88
	ldx     #$FF    			; AB82 A2 FF                    ..
	stx     $CA     			; AB84 86 CA                    ..
	bne     LABD3   			; always branches

; ----------------------------------------------------------------------------
sub_ab88:
	inc     $A4     			; AB88 E6 A4                    ..
	bne     :+
	inc     $A5     			; AB8C E6 A5                    ..
:	ldx     #$7F    			; AB8E A2 7F                    ..
	stx     $CA     			; AB90 86 CA                    ..
	bne     sub_abff

; ----------------------------------------------------------------------------
LAB94:  jsr     LAD05   			; AB94 20 05 AD                  ..
	bit     $B0     			; AB97 24 B0                    $.
	bvs     LABD3   			; AB99 70 38                    p8
	jsr     sub_a5aa
	lda     word_9C
	and     #$04    			; ABA0 29 04                    ).
	beq     LABA6   			; ABA2 F0 02                    ..
	inc     off_F4
LABA6:  lda     $A6     			; ABA6 A5 A6                    ..
	ldy     #$20    			; ABA8 A0 20                    . 
	sec             			; ABAA 38                       8
:	sbc     #$06    			; ABAB E9 06                    ..
	dey             			; ABAD 88                       .
	bcs     :-
	tya             			; ABB0 98                       .
	and     #$03    			; ABB1 29 03                    ).
	tax             			; ABB3 AA                       .
	tya             			; ABB4 98                       .
	lsr     a       			; ABB5 4A                       J
	lsr     a       			; ABB6 4A                       J
	clc             			; ABB7 18                       .
	adc     #$18    			; ABB8 69 18                    i.
	sta     off_E3+1
	lda     LB936,x 			; ABBC BD 36 B9                 .6.
	sta     off_E3
	lda     $E7     			; ABC1 A5 E7                    ..
	ldy     off_F4
	cmp     #$5F    			; ABC5 C9 5F                    ._
	bne     LABD1   			; ABC7 D0 08                    ..
	lda     (off_E3),y
	cmp     #$20    			; ABCB C9 20                    . 
	bne     LABD3   			; ABCD D0 04                    ..
	lda     #$5F    			; ABCF A9 5F                    ._
LABD1:  sta     (off_E3),y
LABD3:  jsr     sub_a890
	ldy     $D8     			; ABD6 A4 D8                    ..
	clc             			; ABD8 18                       .
	adc     $A4     			; ABD9 65 A4                    e.
	sta     $A4     			; ABDB 85 A4                    ..
	bcc     LABE1   			; ABDD 90 02                    ..
	inc     $A5     			; ABDF E6 A5                    ..
LABE1:  tya             			; ABE1 98                       .
	clc             			; ABE2 18                       .
	adc     word_9C     			; ABE3 65 9C                    e.
	sta     word_9C     			; ABE5 85 9C                    ..
	bcc     :+
	inc     word_9C+1
:	lda     word_9C+1
	cmp     #$02    			; ABED C9 02                    ..
	bcc     LABFE   			; ABEF 90 0D                    ..
	lda     $A4     			; ABF1 A5 A4                    ..
	sec             			; ABF3 38                       8
	sbc     #$40    			; ABF4 E9 40                    .@
	sta     $A4     			; ABF6 85 A4                    ..
	lda     #$00    			; ABF8 A9 00                    ..
	sta     $A5     			; ABFA 85 A5                    ..
	sta     word_9C+1     			; ABFC 85 9D                    ..
LABFE:  rts             			; ABFE 60                       `

; ----------------------------------------------------------------------------
sub_abff:
	ldx     byte_B7
	cpx     #$02    			; AC01 E0 02                    ..
	bcc     LAC24   			; AC03 90 1F                    ..
	bne     LAC11   			; AC05 D0 0A                    ..
	cmp     #$40    			; AC07 C9 40                    .@
	tax             			; AC09 AA                       .
	lda     #$E0    			; AC0A A9 E0                    ..
	adc     #$00    			; AC0C 69 00                    i.
	sta     $EB     			; AC0E 85 EB                    ..
	txa             			; AC10 8A                       .
LAC11:  asl     a       			; AC11 0A                       .
	rol     off_E5+1
	asl     a       			; AC14 0A                       .
	rol     off_E5+1
	asl     a       			; AC17 0A                       .
	rol     off_E5+1
	sta     off_E5
	ldx     byte_B7
	cpx     #$03    			; AC1E E0 03                    ..
	bcs     LAC37   			; AC20 B0 15                    ..
	bcc     LAC3C   			; AC22 90 18                    ..
LAC24:  asl     a       			; AC24 0A                       .
	asl     a       			; AC25 0A                       .
	rol     $E6     			; AC26 26 E6                    &.
	sta     byte_D9 			; AC28 85 D9                    ..
	ldx     $E6     			; AC2A A6 E6                    ..
	stx     $D8     			; AC2C 86 D8                    ..
	asl     a       			; AC2E 0A                       .
	rol     $E6     			; AC2F 26 E6                    &.
	adc     byte_D9 			; AC31 65 D9                    e.
	bcc     LAC37   			; AC33 90 02                    ..
	inc     $E6     			; AC35 E6 E6                    ..
LAC37:  clc             			; AC37 18                       .
	adc     $EA     			; AC38 65 EA                    e.
	sta     off_E5
LAC3C:  lda     $D8     			; AC3C A5 D8                    ..
	adc     $E6     			; AC3E 65 E6                    e.
	adc     $EB     			; AC40 65 EB                    e.
	sta     $E6     			; AC42 85 E6                    ..
	jsr     sub_ad8e
	lda     $CA     			; AC47 A5 CA                    ..
	cmp     #$7F    			; AC49 C9 7F                    ..
	bne     LAC56   			; AC4B D0 09                    ..
	lda     #$00    			; AC4D A9 00                    ..
	sta     $C9     			; AC4F 85 C9                    ..
	lda     $A4     			; AC51 A5 A4                    ..
	jmp     LAC58   			; AC53 4C 58 AC                 LX.

; ----------------------------------------------------------------------------
LAC56:  lda     word_9C     			; AC56 A5 9C                    ..
LAC58:  jsr     sub_adcb
	ldx     #$0B    			; AC5B A2 0B                    ..
LAC5D:  ldy     #$00    			; AC5D A0 00                    ..
	sty     $D8     			; AC5F 84 D8                    ..
	lda     byte_B7
	lsr     a       			; AC63 4A                       J
	beq     LAC78   			; AC64 F0 12                    ..
	cpx     #$0A    			; AC66 E0 0A                    ..
	bcs     :+
	cpx     #$02    			; AC6A E0 02                    ..
	bcc     :+
	txa             			; AC6E 8A                       .
	sbc     #$02    			; AC6F E9 02                    ..
	bcs     LAC79   			; AC71 B0 06                    ..
:	lda     #$00    			; AC73 A9 00                    ..
	tay             			; AC75 A8                       .
	beq     LAC7C   			; AC76 F0 04                    ..
LAC78:  txa             			; AC78 8A                       .
LAC79:  tay             			; AC79 A8                       .
	lda     (off_E5),y
LAC7C:  ldy     $CA     			; AC7C A4 CA                    ..
	bpl     LACB3   			; AC7E 10 33                    .3
	sta     $E7     			; AC80 85 E7                    ..
	lsr     a       			; AC82 4A                       J
	lsr     a       			; AC83 4A                       J
	lsr     a       			; AC84 4A                       J
	lsr     a       			; AC85 4A                       J
	tay             			; AC86 A8                       .
	lda     LBA02,y 			; AC87 B9 02 BA                 ...
	sta     $FB     			; AC8A 85 FB                    ..
	lda     $E7     			; AC8C A5 E7                    ..
	and     #$0F    			; AC8E 29 0F                    ).
	tay             			; AC90 A8                       .
	lda     LBA02,y 			; AC91 B9 02 BA                 ...
	sta     off_FA
	lda     #$80    			; AC96 A9 80                    ..
	sta     $CA     			; AC98 85 CA                    ..
LAC9A:  ldy     #$00    			; AC9A A0 00                    ..
	lda     off_FA+1
	jsr     sub_ad3a   			; AC9E 20 3A AD                  :.
	lda     $FA     			; ACA1 A5 FA                    ..
	jsr     sub_ad3a
	bit     $CA     			; ACA6 24 CA                    $.
	bvs     LACB8   			; ACA8 70 0E                    p.
	jsr     sub_accd
	lda     #$FF    			; ACAD A9 FF                    ..
	sta     $CA     			; ACAF 85 CA                    ..
	bne     LAC9A   			; ACB1 D0 E7                    ..
LACB3:  ldy     #$00    			; ACB3 A0 00                    ..
	jsr     sub_ad3a
LACB8:  dex             			; ACB8 CA                       .
	bmi     LACE2   			; ACB9 30 27                    0'
	lda     $CA     			; ACBB A5 CA                    ..
	cmp     #$7F    			; ACBD C9 7F                    ..
	bne     LACC7   			; ACBF D0 06                    ..
	jsr	sub_ace3
	jmp     LAC5D   			; ACC4 4C 5D AC                 L].

; ----------------------------------------------------------------------------
LACC7:  jsr     sub_accd   			; ACC7 20 CD AC                  ..
	jmp     LAC5D   			; ACCA 4C 5D AC                 L].

; ----------------------------------------------------------------------------
sub_accd:
	sec             			; ACCD 38                       8
	lda     off_E3
	sbc     #$40    			; ACD0 E9 40                    .@
	sta     off_E3
	bcs     LACE2
	dec     off_E3+1
	lda     off_E3+1
	cmp     #$40    			; ACDA C9 40                    .@
	bcs     LACE2
	lda     #$9F    			; ACDE A9 9F                    ..
	sta     $E4     			; ACE0 85 E4                    ..
LACE2:	rts             			; ACE2 60                       `

; ----------------------------------------------------------------------------
sub_ace3:
	sec             			; ACE3 38                       8
	lda     off_E3
	sbc     #$28    			; ACE6 E9 28                    .(
	sta     off_E3
	bcs     LAD04   			; ACEA B0 18                    ..
	dec     off_E3+1
	lda     off_E3+1
	cmp     #>L2000
	beq     :+
	bcs     LAD04   			; ACF4 B0 0E                    ..
	lda     #$3D    			; ACF6 A9 3D                    .=
	bne     LAD02   			; ACF8 D0 08                    ..

; ----------------------------------------------------------------------------
:	lda     off_E3
	cmp     #$10    			; ACFC C9 10                    ..
	bcs     LAD04   			; ACFE B0 04                    ..
	lda     #$3E    			; AD00 A9 3E                    .>
LAD02:  sta     off_E3+1
LAD04:  rts             			; AD04 60                       `

; ----------------------------------------------------------------------------
LAD05:  asl     a       			; AD05 0A                       .
	sta     byte_D9 			; AD06 85 D9                    ..
	asl     a       			; AD08 0A                       .
	rol     $E6     			; AD09 26 E6                    &.
	clc             			; AD0B 18                       .
	adc     byte_D9 			; AD0C 65 D9                    e.
	bcc     :+
	inc     $E6     			; AD10 E6 E6                    ..
	clc             			; AD12 18                       .
:	adc     chset6_base     		; AD13 65 E8                    e.
	sta     off_E5
	lda     off_E5+1
	adc     chset6_base+1   		; AD19 65 E9                    e.
	sta     off_E5+1
	jsr     sub_ad8e
	lda     $A4     			; AD20 A5 A4                    ..
	jsr     sub_adcb
	ldx     #$05    			; AD25 A2 05                    ..
:	jsr     sub_ad34   			; AD27 20 34 AD                  4.
	dex             			; AD2A CA                       .
	bmi	:+
	jsr     sub_ace3
	jmp     :-

; ----------------------------------------------------------------------------
:	rts

; ----------------------------------------------------------------------------
sub_ad34:  
	txa             			; AD34 8A                       .
	tay             			; AD35 A8                       .
	lda     (off_E5),y
	ldy     #$00    			; AD38 A0 00                    ..

sub_ad3a:
	bit     $B0     			; AD3A 24 B0                    $.
	bvc     :+
	eor     #$FF    			; AD3E 49 FF                    I.
	bit     $C9     			; AD40 24 C9                    $.
	bvc     :+
	and     #$F8    			; AD44 29 F8                    ).
:	sty     $FC     			; AD46 84 FC                    ..
	ldy     #$00    			; AD48 A0 00                    ..
	sty     $D8     			; AD4A 84 D8                    ..
	ldy     off_F4
	beq     LAD56   			; AD4E F0 06                    ..
:	lsr     a       			; AD50 4A                       J
	ror     $D8     			; AD51 66 D8                    f.
	dey             			; AD53 88                       .
	bne     :-
LAD56:  sta     byte_D9 			; AD56 85 D9                    ..
	ldy     $FC     			; AD58 A4 FC                    ..
	lda     (off_E3),y
	bit     $B0     			; AD5C 24 B0                    $.
	bmi     LAD67   			; AD5E 30 07                    0.
	and     $AB     			; AD60 25 AB                    %.
LAD62:  ora     byte_D9 			; AD62 05 D9                    ..
	jmp     LAD6F   			; AD64 4C 6F AD                 Lo.

; ----------------------------------------------------------------------------
LAD67:  bvc     LAD62   			; AD67 50 F9                    P.
	lda     byte_D9 			; AD69 A5 D9                    ..
	ora     $AB     			; AD6B 05 AB                    ..
	and     (off_E3),y
LAD6F:  sta     (off_E3),y
	iny             			; AD71 C8                       .
	lda     $A7     			; AD72 A5 A7                    ..
	beq     LAD8D   			; AD74 F0 17                    ..
	lda     (off_E3),y
	bit     $B0     			; AD78 24 B0                    $.
	bmi     LAD83   			; AD7A 30 07                    0.
	and     $A7     			; AD7C 25 A7                    %.
LAD7E:  ora     $D8     			; AD7E 05 D8                    ..
	jmp     LAD8B   			; AD80 4C 8B AD                 L..

; ----------------------------------------------------------------------------
LAD83:  bvc     LAD7E   			; AD83 50 F9                    P.
	lda     $D8     			; AD85 A5 D8                    ..
	ora     $A7     			; AD87 05 A7                    ..
	and     (off_E3),y
LAD8B:  sta     (off_E3),y
LAD8D:  rts             			; AD8D 60                       `

; ----------------------------------------------------------------------------
sub_ad8e:
	bit     $C9     			; AD8E 24 C9                    $.
	bvs     :+
	lda     #$7F    			; AD92 A9 7F                    ..
	sec             			; AD94 38                       8
	sbc     $9E     			; AD95 E5 9E                    ..
	tax             			; AD97 AA                       .
	and     #$03    			; AD98 29 03                    ).
	tay             			; AD9A A8                       .
	lda     #$01    			; AD9B A9 01                    ..
	sbc     $9F     			; AD9D E5 9F                    ..
	lsr     a       			; AD9F 4A                       J
	txa             			; ADA0 8A                       .
	ror     a       			; ADA1 6A                       j
	lsr     a       			; ADA2 4A                       J
	clc             			; ADA3 18                       .
	adc     #$40    			; ADA4 69 40                    i@
	sta     off_E3+1
	jsr     sub_a5aa
	clc             			; ADAB 18                       .
	adc     LB936,y 			; ADAC 79 36 B9                 y6.
	sta     off_E3
	rts             			; ADB1 60                       `

; ----------------------------------------------------------------------------
:	lda     #$BF    			; ADB2 A9 BF                    ..
	sec             			; ADB4 38                       8
	sbc     $A6     			; ADB5 E5 A6                    ..
	tax             			; ADB7 AA                       .
	lda     $04C0,x 			; ADB8 BD C0 04                 ...
	sta     off_E3+1
	jsr     sub_a5a2
	clc             			; ADC0 18                       .
	adc     $0400,x 			; ADC1 7D 00 04                 }..
	sta     off_E3
	bcc     :+
	inc     $E4     			; ADC8 E6 E4                    ..
:	rts

; ----------------------------------------------------------------------------
sub_adcb:
	and     #$07    			; ADCB 29 07                    ).
	sta     off_F4
	bit     $C9     			; ADCF 24 C9                    $.
	bvs     :+
	tax             			; ADD3 AA                       .
	beq     :++
	lda     LB94E,x 			; ADD6 BD 4E B9                 .N.
	sta     $A7     			; ADD9 85 A7                    ..
	eor     #$FF    			; ADDB 49 FF                    I.
	bne     LADEE   			; ADDD D0 0F                    ..
:	tax             			; ADDF AA                       .
	lda     LB966,x 			; ADE0 BD 66 B9                 .f.
	sta     $A7     			; ADE3 85 A7                    ..
	lda     LB95E,x 			; ADE5 BD 5E B9                 .^.
	bne     LADEE   			; ADE8 D0 04                    ..
:	lda     #$00    			; ADEA A9 00                    ..
	sta     $A7     			; ADEC 85 A7                    ..
LADEE:  sta     $AB     			; ADEE 85 AB                    ..
	rts             			; ADF0 60                       `

; ----------------------------------------------------------------------------
sub_adf1:
	jsr     sub_a8b3
	lda     $BA     			; ADF4 A5 BA                    ..
	sta     $C9     			; ADF6 85 C9                    ..
	bne     LADFF   			; ADF8 D0 05                    ..
	jsr     LADFF   			; ADFA 20 FF AD                  ..
	dec     $C9     			; ADFD C6 C9                    ..
LADFF:  bit     $C9     			; ADFF 24 C9                    $.
	bvs     LAE13   			; AE01 70 10                    p.
	lda     $A0     			; AE03 A5 A0                    ..
	ldx     $A1     			; AE05 A6 A1                    ..
	ldy     word_9C     			; AE07 A4 9C                    ..
	sty     $EC     			; AE09 84 EC                    ..
	sty     $A0     			; AE0B 84 A0                    ..
	ldy     word_9C+1
	sty     $A1     			; AE0F 84 A1                    ..
	bvc     LAE21   			; AE11 50 0E                    P.
LAE13:  lda     $A8     			; AE13 A5 A8                    ..
	ldx     $A9     			; AE15 A6 A9                    ..
	ldy     $A4     			; AE17 A4 A4                    ..
	sty     $EC     			; AE19 84 EC                    ..
	sty     $A8     			; AE1B 84 A8                    ..
	ldy     $A5     			; AE1D A4 A5                    ..
	sty     $A9     			; AE1F 84 A9                    ..
LAE21:  sta     $F0     			; AE21 85 F0                    ..
	stx     $F1     			; AE23 86 F1                    ..
	sty     $ED     			; AE25 84 ED                    ..
	bvs     LAE2F   			; AE27 70 06                    p.
	lda     $A2     			; AE29 A5 A2                    ..
	ldx     $A3     			; AE2B A6 A3                    ..
	bvc     LAE33   			; AE2D 50 04                    P.
LAE2F:  lda     $AA     			; AE2F A5 AA                    ..
	ldx     #$00    			; AE31 A2 00                    ..
LAE33:  sta     $F2     			; AE33 85 F2                    ..
	stx     $F3     			; AE35 86 F3                    ..
	sec             			; AE37 38                       8
	bvc     LAE46   			; AE38 50 0C                    P.
	lda     #$BF    			; AE3A A9 BF                    ..
	sbc     $A6     			; AE3C E5 A6                    ..
	sta     $EE     			; AE3E 85 EE                    ..
	sta     $AA     			; AE40 85 AA                    ..
	lda     #$00    			; AE42 A9 00                    ..
	beq     LAE54   			; AE44 F0 0E                    ..
LAE46:  lda     #$7F    			; AE46 A9 7F                    ..
	sbc     $9E     			; AE48 E5 9E                    ..
	sta     $EE     			; AE4A 85 EE                    ..
	sta     $A2     			; AE4C 85 A2                    ..
	lda     #$01    			; AE4E A9 01                    ..
	sbc     $9F     			; AE50 E5 9F                    ..
	sta     $A3     			; AE52 85 A3                    ..
LAE54:  sta     $EF     			; AE54 85 EF                    ..
	lda     $DA     			; AE56 A5 DA                    ..
	bne     LAE5D   			; AE58 D0 03                    ..
	jmp     LAF94   			; AE5A 4C 94 AF                 L..

; ----------------------------------------------------------------------------
LAE5D:  ldy     #$00    			; AE5D A0 00                    ..
	lda     $ED     			; AE5F A5 ED                    ..
	cmp     $F1     			; AE61 C5 F1                    ..
	bcc     LAE6D   			; AE63 90 08                    ..
	bne     LAE7B   			; AE65 D0 14                    ..
	lda     $EC     			; AE67 A5 EC                    ..
	cmp     $F0     			; AE69 C5 F0                    ..
	bcs     LAE7B   			; AE6B B0 0E                    ..
LAE6D:  sec             			; AE6D 38                       8
	lda     $F0     			; AE6E A5 F0                    ..
	sbc     $EC     			; AE70 E5 EC                    ..
	sta     $D4     			; AE72 85 D4                    ..
	lda     $F1     			; AE74 A5 F1                    ..
	sbc     $ED     			; AE76 E5 ED                    ..
	dey             			; AE78 88                       .
	bne     LAE85   			; AE79 D0 0A                    ..
LAE7B:  lda     $EC     			; AE7B A5 EC                    ..
	sbc     $F0     			; AE7D E5 F0                    ..
	sta     $D4     			; AE7F 85 D4                    ..
	lda     $ED     			; AE81 A5 ED                    ..
	sbc     $F1     			; AE83 E5 F1                    ..
LAE85:  sty     $F8     			; AE85 84 F8                    ..
	sta     $D5     			; AE87 85 D5                    ..
	ldy     #$00    			; AE89 A0 00                    ..
	lda     $EF     			; AE8B A5 EF                    ..
	cmp     $F3     			; AE8D C5 F3                    ..
	bcc     LAE99   			; AE8F 90 08                    ..
	bne     LAEA7   			; AE91 D0 14                    ..
	lda     $EE     			; AE93 A5 EE                    ..
	cmp     $F2     			; AE95 C5 F2                    ..
	bcs     LAEA7   			; AE97 B0 0E                    ..
LAE99:  sec             			; AE99 38                       8
	lda     $F2     			; AE9A A5 F2                    ..
	sbc     $EE     			; AE9C E5 EE                    ..
	sta     $D6     			; AE9E 85 D6                    ..
	lda     $F3     			; AEA0 A5 F3                    ..
	sbc     $EF     			; AEA2 E5 EF                    ..
	dey             			; AEA4 88                       .
	bne     LAEB1   			; AEA5 D0 0A                    ..
LAEA7:  lda     $EE     			; AEA7 A5 EE                    ..
	sbc     $F2     			; AEA9 E5 F2                    ..
	sta     $D6     			; AEAB 85 D6                    ..
	lda     $EF     			; AEAD A5 EF                    ..
	sbc     $F3     			; AEAF E5 F3                    ..
LAEB1:  sty     $F9     			; AEB1 84 F9                    ..
	sta     $D7     			; AEB3 85 D7                    ..
	bit     $C9     			; AEB5 24 C9                    $.
	lda     $D5     			; AEB7 A5 D5                    ..
	cmp     $D7     			; AEB9 C5 D7                    ..
	bcc     LAEC5   			; AEBB 90 08                    ..
	bne     LAEE6   			; AEBD D0 27                    .'
	lda     $D4     			; AEBF A5 D4                    ..
	cmp     $D6     			; AEC1 C5 D6                    ..
	bcs     LAEE6   			; AEC3 B0 21                    .!
LAEC5:  lda     #$01    			; AEC5 A9 01                    ..
	sta     $FC     			; AEC7 85 FC                    ..
	lda     $D6     			; AEC9 A5 D6                    ..
	sta     off_F4
	lda     $D7     			; AECD A5 D7                    ..
	sta     $F5     			; AECF 85 F5                    ..
	lda     $F9     			; AED1 A5 F9                    ..
	beq     LAED8   			; AED3 F0 03                    ..
	jsr     LAFEC   			; AED5 20 EC AF                  ..
LAED8:  ldx     $F8     			; AED8 A6 F8                    ..
	beq     LAEFD   			; AEDA F0 21                    .!
LAEDC:  bvc     LAEE2   			; AEDC 50 04                    P.
	lda     #$D8    			; AEDE A9 D8                    ..
	bne     LAF05   			; AEE0 D0 23                    .#
LAEE2:  lda     #$C0    			; AEE2 A9 C0                    ..
	bne     LAF05   			; AEE4 D0 1F                    ..
LAEE6:  lda     #$00    			; AEE6 A9 00                    ..
	sta     $FC     			; AEE8 85 FC                    ..
	lda     $D4     			; AEEA A5 D4                    ..
	sta     off_F4
	lda     $D5     			; AEEE A5 D5                    ..
	sta     $F5     			; AEF0 85 F5                    ..
	lda     $F8     			; AEF2 A5 F8                    ..
	beq     LAEF9   			; AEF4 F0 03                    ..
	jsr     LAFEC   			; AEF6 20 EC AF                  ..
LAEF9:  ldx     $F9     			; AEF9 A6 F9                    ..
	bne     LAEDC   			; AEFB D0 DF                    ..
LAEFD:  bvc     LAF03   			; AEFD 50 04                    P.
	lda     #$28    			; AEFF A9 28                    .(
	bne     LAF05   			; AF01 D0 02                    ..
LAF03:  lda     #$40    			; AF03 A9 40                    .@
LAF05:  stx     $FB     			; AF05 86 FB                    ..
	sta     $FA     			; AF07 85 FA                    ..
	lda     off_F4
	eor     #$FF    			; AF0B 49 FF                    I.
	sta     $F6     			; AF0D 85 F6                    ..
	lda     $F5     			; AF0F A5 F5                    ..
	eor     #$FF    			; AF11 49 FF                    I.
	sta     $F7     			; AF13 85 F7                    ..
	lsr     $F5     			; AF15 46 F5                    F.
	ror     off_F4
	lda     $F2     			; AF19 A5 F2                    ..
	bvc     LAF26   			; AF1B 50 09                    P.
	tax             			; AF1D AA                       .
	ldy     $0400,x 			; AF1E BC 00 04                 ...
	lda     $04C0,x 			; AF21 BD C0 04                 ...
	bne     LAF36   			; AF24 D0 10                    ..
LAF26:  and     #$03    			; AF26 29 03                    ).
	tax             			; AF28 AA                       .
	lda     $F3     			; AF29 A5 F3                    ..
	lsr     a       			; AF2B 4A                       J
	lda     $F2     			; AF2C A5 F2                    ..
	ror     a       			; AF2E 6A                       j
	lsr     a       			; AF2F 4A                       J
	clc             			; AF30 18                       .
	adc     #$40    			; AF31 69 40                    i@
	ldy     LB936,x 			; AF33 BC 36 B9                 .6.
LAF36:  sty     off_E3
	sta     off_E3+1
	lda     $F0     			; AF3A A5 F0                    ..
	and     #$07    			; AF3C 29 07                    ).
	tax             			; AF3E AA                       .
	lda     $F1     			; AF3F A5 F1                    ..
	lsr     a       			; AF41 4A                       J
	lda     $F0     			; AF42 A5 F0                    ..
	ror     a       			; AF44 6A                       j
	lsr     a       			; AF45 4A                       J
	lsr     a       			; AF46 4A                       J
	tay             			; AF47 A8                       .
LAF48:  jsr     sub_b004
	inc     $F6     			; AF4B E6 F6                    ..
	bne     LAF53   			; AF4D D0 04                    ..
	inc     $F7     			; AF4F E6 F7                    ..
	beq     LAF94   			; AF51 F0 41                    .A
LAF53:  clc             			; AF53 18                       .
	lda     $FC     			; AF54 A5 FC                    ..
	bne     LAF9D   			; AF56 D0 45                    .E
	lda     off_F4
	adc     $D6     			; AF5A 65 D6                    e.
	sta     off_F4
	lda     $F5     			; AF5E A5 F5                    ..
	adc     $D7     			; AF60 65 D7                    e.
	sta     $F5     			; AF62 85 F5                    ..
	cmp     $D5     			; AF64 C5 D5                    ..
	bcc     LAF89   			; AF66 90 21                    .!
	bne     LAF70   			; AF68 D0 06                    ..
	lda     off_F4
	cmp     $D4     			; AF6C C5 D4                    ..
	bcc     LAF89   			; AF6E 90 19                    ..
LAF70:  lda     off_F4
	sbc     $D4     			; AF72 E5 D4                    ..
	sta     off_F4  			; AF74 85 F4                    ..
	lda     off_F4+1
	sbc     $D5     			; AF78 E5 D5                    ..
	sta     off_F4+1
	clc             			; AF7C 18                       .
	lda     off_E3
	adc     $FA     			; AF7F 65 FA                    e.
	sta     off_E3
	lda     off_E3+1
	adc     $FB     			; AF85 65 FB                    e.
	sta     off_E3+1
LAF89:  inx             			; AF89 E8                       .
	cpx     #$08    			; AF8A E0 08                    ..
	bcc     LAF48   			; AF8C 90 BA                    ..
	ldx     #$00    			; AF8E A2 00                    ..
	iny             			; AF90 C8                       .
LAF91:  jmp     LAF48   			; AF91 4C 48 AF                 LH.

; ----------------------------------------------------------------------------
LAF94:  ldx     $C9     			; AF94 A6 C9                    ..
	bne	:+
	rts             			; AF98 60                       `

; ----------------------------------------------------------------------------
:	dex             			; AF99 CA                       .
	stx     $DA     			; AF9A 86 DA                    ..
	rts             			; AF9C 60                       `

; ----------------------------------------------------------------------------
LAF9D:  lda     off_F4
	adc     $D4     			; AF9F 65 D4                    e.
	sta     off_F4
	lda     off_F4+1
	adc     $D5     			; AFA5 65 D5                    e.
	sta     off_F4+1
	cmp     $D7     			; AFA9 C5 D7                    ..
	bcc     LAFD7   			; AFAB 90 2A                    .*
	bne     LAFB5   			; AFAD D0 06                    ..
	lda     off_F4  			; AFAF A5 F4                    ..
	cmp     $D6     			; AFB1 C5 D6                    ..
	bcc     LAFD7   			; AFB3 90 22                    ."
LAFB5:  lda     off_F4  			; AFB5 A5 F4                    ..
	sbc     $D6     			; AFB7 E5 D6                    ..
	sta     off_F4  			; AFB9 85 F4                    ..
	lda     off_F4+1
	sbc     $D7     			; AFBD E5 D7                    ..
	sta     off_F4+1
	bit     $FB     			; AFC1 24 FB                    $.
	bvs     :+
	inx             			; AFC5 E8                       .
	cpx     #$08    			; AFC6 E0 08                    ..
	bcc     LAFD7   			; AFC8 90 0D                    ..
	ldx     #$00    			; AFCA A2 00                    ..
	iny             			; AFCC C8                       .
	bne     LAFD7   			; AFCD D0 08                    ..
:	dex             			; AFCF CA                       .
	bpl     LAFD7   			; AFD0 10 05                    ..
	ldx     #$07    			; AFD2 A2 07                    ..
	dey             			; AFD4 88                       .
	bmi     LAF94   			; AFD5 30 BD                    0.
LAFD7:  clc             			; AFD7 18                       .
	lda     $C9     			; AFD8 A5 C9                    ..
	beq     LAFE0   			; AFDA F0 04                    ..
	lda     #$28    			; AFDC A9 28                    .(
	bne     LAFE2   			; AFDE D0 02                    ..
LAFE0:  lda     #$40    			; AFE0 A9 40                    .@
LAFE2:  adc     off_E3
	sta     off_E3
	bcc     LAF91   			; AFE6 90 A9                    ..
	inc     off_E3+1
	bne     LAF91   			; AFEA D0 A5                    ..
LAFEC:  ldx     #$03    			; AFEC A2 03                    ..
LAFEE:  ldy     $EC,x   			; AFEE B4 EC                    ..
	lda     $F0,x   			; AFF0 B5 F0                    ..
	sta     $EC,x   			; AFF2 95 EC                    ..
	sty     $F0,x   			; AFF4 94 F0                    ..
	dex             			; AFF6 CA                       .
	bpl     LAFEE   			; AFF7 10 F5                    ..
	txa             			; AFF9 8A                       .
	eor     $F8     			; AFFA 45 F8                    E.
	sta     $F8     			; AFFC 85 F8                    ..
	txa             			; AFFE 8A                       .
	eor     $F9     			; AFFF 45 F9                    E.
	sta     $F9     			; B001 85 F9                    ..
	rts             			; B003 60                       `

; ----------------------------------------------------------------------------
sub_b004:
	lda     LB93A,x 			; B004 BD 3A B9                 .:.
	and     (off_E3),y
	bit     $B0     			; B009 24 B0                    $.
	bvs	:+
	ora     LB942,x 			; B00D 1D 42 B9                 .B.
:	sta     (off_E3),y
	rts             			; B012 60                       `

;*******************************************************************************
;*                                                                             *
;*                                  sub_b013                                   *
;*                                                                             *
;*                           deferred VBI handler                              *
;*                                                                             *
;*******************************************************************************

sub_b013:
	sec             			; B013 38                       8
	lda     $C1     			; B014 A5 C1                    ..
	sbc     #$02    			; B016 E9 02                    ..
	bne     LB01E   			; B018 D0 04                    ..
	ldx     $C6     			; B01A A6 C6                    ..
	bne     LB02B   			; B01C D0 0D                    ..
LB01E:  lda     $0284   			; B01E AD 84 02                 ...
	bne     LB02D   			; B021 D0 0A                    ..
	ldx     $C6     			; B023 A6 C6                    ..
	bne     LB045   			; B025 D0 1E                    ..
	ldx     #$1E    			; B027 A2 1E                    ..
	stx     $C6     			; B029 86 C6                    ..
LB02B:  sta     byte_C7 			; B02B 85 C7                    ..
LB02D:  ldx     STICK0				; read joystick 0
	lda     LB87F,x 			; B030 BD 7F B8                 ...
	sta     off_DD
	lda     LB86F,x 			; B035 BD 6F B8                 .o.
	sta     off_DD+1
	ora     off_DD
	bne     :+
	sta     $C2     			; B03E 85 C2                    ..
	beq     LB045   			; B040 F0 03                    ..
:	jsr     LB0A8   			; B042 20 A8 B0                  ..
LB045:  ldx     $C6     			; B045 A6 C6                    ..
	beq     LB04B   			; B047 F0 02                    ..
	dec     $C6     			; B049 C6 C6                    ..
LB04B:  lda     $14     			; B04B A5 14                    ..
	and     #$0F    			; B04D 29 0F                    ).
	bne     LB087   			; B04F D0 36                    .6
	lda     $D01F   			; B051 AD 1F D0                 ...
	and     #$03    			; B054 29 03                    ).
	cmp     #$01    			; B056 C9 01                    ..
	bne     LB087   			; B058 D0 2D                    .-
	lda     CURRENT_DL
	asl     a       			; B05C 0A                       .
	ldy     COLOR2  			; B05D AC C6 02                 ...
	bit     $1355   			; B060 2C 55 13                 ,U.
	bvs     LB08A   			; B063 70 25                    p%
	bmi     LB09A   			; B065 30 33                    03
	php             			; B067 08                       .
	tya             			; B068 98                       .
	clc             			; B069 18                       .
	adc     #$10    			; B06A 69 10                    i.
	and     #$F0    			; B06C 29 F0                    ).
	sta     off_DD
	tya             			; B070 98                       .
	and     #$0F    			; B071 29 0F                    ).
	ora     off_DD
	plp             			; B075 28                       (
LB076:  sta     COLOR2  			; B076 8D C6 02                 ...
	sta     COLOR4  			; B079 8D C8 02                 ...
	bcc     LB085   			; B07C 90 07                    ..
	sta     BG_COLOR_DL1    		; B07E 85 D3                    ..
	jsr     sub_a324
	bmi     LB087   			; B083 30 02                    0.
LB085:  sta     BG_COLOR_DL2    		; B085 85 D1                    ..
LB087:  jmp     XITVBV				; Call OS VBI Deferred Exit

; ----------------------------------------------------------------------------
LB08A:  inc     COLOR1  			; B08A EE C5 02                 ...
	lda     COLOR1  			; B08D AD C5 02                 ...
	bcc	:+
	sta     FG_COLOR_DL1    		; B092 85 D2                    ..
	bcs     LB087   			; B094 B0 F1                    ..
:	sta     FG_COLOR_DL2    		; B096 85 D0                    ..
	bcc     LB087   			; B098 90 ED                    ..
LB09A:  tya             			; B09A 98                       .
	and     #$F0    			; B09B 29 F0                    ).
	sta     off_DD
	iny             			; B09F C8                       .
	tya             			; B0A0 98                       .
	and     #$0F    			; B0A1 29 0F                    ).
	ora     off_DD
	jmp     LB076   			; B0A5 4C 76 B0                 Lv.

; ----------------------------------------------------------------------------
LB0A8:  ldy     $C1     			; B0A8 A4 C1                    ..
	beq     LB103   			; B0AA F0 57                    .W
	bpl	:+
	jmp     LB17D   			; B0AE 4C 7D B1                 L}.

; ----------------------------------------------------------------------------
:	cpx     $C2     			; B0B1 E4 C2                    ..
	bne     LB104   			; B0B3 D0 4F                    .O
	ldx     $C5     			; B0B5 A6 C5                    ..
	cpx     #$7F    			; B0B7 E0 7F                    ..
	beq     LB136   			; B0B9 F0 7B                    .{
	inx             			; B0BB E8                       .
	stx     $C5     			; B0BC 86 C5                    ..
	bmi     LB136   			; B0BE 30 76                    0v
	txa             			; B0C0 8A                       .
	and     #$03    			; B0C1 29 03                    ).
	bne     LB136   			; B0C3 D0 71                    .q
LB0C5:  lda     $C1     			; B0C5 A5 C1                    ..
	lsr     a       			; B0C7 4A                       J
	ldx     byte_C7 			; B0C8 A6 C7                    ..
	inx             			; B0CA E8                       .
	lda     off_DD+1
	beq     LB0E8   			; B0CD F0 19                    ..
	bpl     LB0DE   			; B0CF 10 0D                    ..
	bcs     LB0D8   			; B0D1 B0 05                    ..
	lda     LBA43,x 			; B0D3 BD 43 BA                 .C.
	bne     LB0FD   			; B0D6 D0 25                    .%
LB0D8:  jsr     sub_b115
	jmp     LB0E8   			; B0DB 4C E8 B0                 L..

; ----------------------------------------------------------------------------
LB0DE:  bcs     :+
	lda     LBA40,x 			; B0E0 BD 40 BA                 .@.
	bne     LB0FD   			; B0E3 D0 18                    ..
:  	jsr     sub_b137
LB0E8:  lda     $C1     			; B0E8 A5 C1                    ..
	lsr     a       			; B0EA 4A                       J
	lda     off_DD
	beq     LB136   			; B0ED F0 47                    .G
	bpl     LB0F8   			; B0EF 10 07                    ..
	bcs     LB157   			; B0F1 B0 64                    .d
	lda     LBA3D,x 			; B0F3 BD 3D BA                 .=.
	bne     LB0FD   			; B0F6 D0 05                    ..
LB0F8:  bcs     LB16E   			; B0F8 B0 74                    .t
	lda     LBA3A,x 			; B0FA BD 3A BA                 .:.
LB0FD:  sta     $C8     			; B0FD 85 C8                    ..
	ldx     #$80    			; B0FF A2 80                    ..
	stx     $C5     			; B101 86 C5                    ..
LB103:  rts             			; B103 60                       `

; ----------------------------------------------------------------------------
LB104:  stx     $C2     			; B104 86 C2                    ..
	ldx     #$80    			; B106 A2 80                    ..
	ldy     $C1     			; B108 A4 C1                    ..
	cpy     #$02    			; B10A C0 02                    ..
	beq     LB110   			; B10C F0 02                    ..
	ldx     #$E2    			; B10E A2 E2                    ..
LB110:  stx     $C5     			; B110 86 C5                    ..
	jmp     LB0C5   			; B112 4C C5 B0                 L..

; ----------------------------------------------------------------------------
sub_b115:
	lda     byte_C0 			; B115 A5 C0                    ..
	cmp     #$0F    			; B117 C9 0F                    ..
	bcs     LB136   			; B119 B0 1B                    ..
	inc     byte_C0 			; B11B E6 C0                    ..
	ldy     #$02    			; B11D A0 02                    ..
	ldx     byte_C3 			; B11F A6 C3                    ..
	txa             			; B121 8A                       .
	sec             			; B122 38                       8
	sbc     #$06    			; B123 E9 06                    ..
	sta     byte_C3 			; B125 85 C3                    ..
:	lda     $0590,x 			; B127 BD 90 05                 ...
	sta     $058A,x 			; B12A 9D 8A 05                 ...
	lda     #$00    			; B12D A9 00                    ..
	sta     $0590,x 			; B12F 9D 90 05                 ...
	inx             			; B132 E8                       .
	dey             			; B133 88                       .
	bpl     :-
LB136:  rts             			; B136 60                       `

; ----------------------------------------------------------------------------
sub_b137:
	lda     byte_C0 			; B137 A5 C0                    ..
	beq     LB156   			; B139 F0 1B                    ..
	dec     byte_C0 			; B13B C6 C0                    ..
	ldy     #$02    			; B13D A0 02                    ..
	ldx     byte_C3 			; B13F A6 C3                    ..
	txa             			; B141 8A                       .
	clc             			; B142 18                       .
	adc     #$06    			; B143 69 06                    i.
	sta     byte_C3 			; B145 85 C3                    ..
:	lda     $0590,x 			; B147 BD 90 05                 ...
	sta     $0596,x 			; B14A 9D 96 05                 ...
	lda     #$00    			; B14D A9 00                    ..
	sta     $0590,x 			; B14F 9D 90 05                 ...
	inx             			; B152 E8                       .
	dey             			; B153 88                       .
	bpl     :-
LB156:  rts             			; B156 60                       `

; ----------------------------------------------------------------------------
LB157:  lda     $BF     			; B157 A5 BF                    ..
	beq     LB16D   			; B159 F0 12                    ..
	dec     $BF     			; B15B C6 BF                    ..
	lda     byte_C4 			; B15D A5 C4                    ..
	sec             			; B15F 38                       8
	sbc     #$0A    			; B160 E9 0A                    ..
LB162:  sta     byte_C4 			; B162 85 C4                    ..
	sta     HPOSM1
	tax             			; B167 AA                       .
	inx             			; B168 E8                       .
	inx             			; B169 E8                       .
	stx     HPOSM0
LB16D:  rts             			; B16D 60                       `

; ----------------------------------------------------------------------------
LB16E:  lda     $BF     			; B16E A5 BF                    ..
	cmp     #$0F    			; B170 C9 0F                    ..
	beq     LB16D   			; B172 F0 F9                    ..
	inc     $BF     			; B174 E6 BF                    ..
	lda     byte_C4 			; B176 A5 C4                    ..
	clc             			; B178 18                       .
	adc     #$0A    			; B179 69 0A                    i.
	bne     LB162   			; B17B D0 E5                    ..
LB17D:  lda     off_DD
	beq     LB18E   			; B17F F0 0D                    ..
	clc             			; B181 18                       .
	lda     byte_E1 			; B182 A5 E1                    ..
	adc     off_DD
	cmp     #$19    			; B186 C9 19                    ..
	bcs     LB18E   			; B188 B0 04                    ..
	sta     byte_E1 			; B18A 85 E1                    ..
	sta     off_DF	 			; B18C 85 DF                    ..
LB18E:  lda     byte_E2 			; B18E A5 E2                    ..
	sta     off_DF+1 			; B190 85 E0                    ..
	lda     off_DD+1
	beq     LB1AB   			; B194 F0 15                    ..
	clc             			; B196 18                       .
	lda     byte_E2 			; B197 A5 E2                    ..
	adc     off_DD+1
	cmp     #$40    			; B19B C9 40                    .@
	bcc     LB1AB   			; B19D 90 0C                    ..
	ldx     off_DD+1
	bmi     LB1A3   			; B1A1 30 00                    0.
LB1A3:  cmp     #$71    			; B1A3 C9 71                    .q
	bcs     LB1AB   			; B1A5 B0 04                    ..
	sta     byte_E2 			; B1A7 85 E2                    ..
	sta     off_DF+1

; 
LB1AB:  lda     #<$10CD    			; B1AB A9 CD                    ..
	sta     off_DD
	lda     #>$10CD    			; B1AF A9 10                    ..
	sta     off_DD+1

	ldx     #$C0    			; B1B3 A2 C0                    ..
LB1B5:  ldy     #$00    			; B1B5 A0 00                    ..
	lda     #$4F    			; B1B7 A9 4F                    .O
	sta     (off_DD),y
	iny             			; B1BB C8                       .
	lda     off_DF 				; B1BC A5 DF                    ..
	sta     (off_DD),y
	iny             			; B1C0 C8                       .
	lda     off_DF+1 			; B1C1 A5 E0                    ..
	sta     (off_DD),y
	clc             			; B1C5 18                       .
	lda     off_DF 				; B1C6 A5 DF                    ..
	adc     #$40    			; B1C8 69 40                    i@
	sta     off_DF	 			; B1CA 85 DF                    ..
	bcc     :+
	inc     off_DF+1 			; B1CE E6 E0                    ..
:	clc             			; B1D0 18                       .
	lda     off_DD
	adc     #$03    			; B1D3 69 03                    i.
	sta     off_DD
	bcc     :+
	inc     off_DD+1
:	dex             			; B1DB CA                       .
	bne     LB1B5   			; B1DC D0 D7                    ..
	rts             			; B1DE 60                       `

; ----------------------------------------------------------------------------
sub_b1df:
	tay             			; A=00001011($0B) if arrived from HELPFG else 11100111($E7)
	ldx     #$00    			; Let X = $00
:	lsr     a       			; If not here from HELPFG then proceed now
	bcc     :+				; otherwise loop 4 times?
	inx             			; until 00001011 shifts to left?
:	bne     :--				; 
	txa             			; X = 4 or X = 0?
	lsr     a       			; 
	tya             			; 
	bcc     :+
	ora     #$80    			; B1ED 09 80                    ..

;*******************************************************************************
;*                                                                             *
;*                               call_cio_putch                                *
;*                                                                             *
;*                      Call CIO put character command                         *
;*                                                                             *
;*******************************************************************************
sub_b1ef:
:	ldy     #LB986-LB96E			; Prepare CIO put char to channel #1
						; Fall through to CIO call

;*******************************************************************************
;*                                                                             *
;*                               call_cio_or_err                               *
;*                                                                             *
;*                  Call CIOV. Halt if Communications Error.                   *
;*                                                                             *
;*******************************************************************************
sub_b1f1:
call_cio_or_err:
	jsr     call_cio			; Call CIOV using Y as arg
	bpl     LB252   			; Success. Jump to nearby RTS
display_comm_error:
	ldy     #$18    			; Set cursor X coord in scaled mode
	sty     word_9C     			; 
	ldy     #$12    			; Set string length - 1
	lda     #<LB89B				; Point to string "COMMUNIC..."
	ldx     #>LB89B				;
	jsr     print_string			;
LB203:  jmp     LB203   			; Halt and catch fire

;*******************************************************************************
;*                                                                             *
;*                                  call_cio                                   *
;*                                                                             *
;*                  Execute CIO call using IOCB variables                      *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
; Populates the IOCB table with device and operation details and calls CIO.
; Similar to a BASIC statement like [XIO cmd, #Channel, Aux1, Aux2, "Rn:"]
;
sub_b206:
call_cio:
	pha
	ldx     LB96E,y				; CIO channel number * 16
	lda     LB96E+1,y
	sta     ICCOM,x				; CIO command
	lda     LB96E+2,y
	sta     ICAX1,x				; CIO aux 1
	lda     LB96E+3,y
	sta     ICAX2,x				; CIO aux 2
	lda     LB96E+4,y
	sta     ICBA,x				; CIO buffer base address (lo)
	lda     LB96E+5,y
	sta     ICBA+1,x			; CIO buffer base address (hi)
	lda     LB96E+6,y
	sta     ICBL,x				; CIO buffer size (lo)
	lda     LB96E+7,y
	sta     ICBL+1,x			; CIO buffer size (hi)
	pla
	jmp     CIOV				; CIOV executes an RTS


;*******************************************************************************
;*                                                                             *
;*                                  close_ch1                                  *
;*                                                                             *
;*                        Close CIO device on channel #1                       *
;*                                                                             *
;*******************************************************************************
sub_b238:
close_ch1:
	ldx     #$10    			; Set CIO channel #1
						; Fall through to sub_b23a

;*******************************************************************************
;*                                                                             *
;*                                  sub_b23a                                   *
;*                                                                             *
;*                        Close CIO device on channel #x                       *
;*                                                                             *
;*******************************************************************************
sub_b23a:
	lda     #$0C    			
	sta     ICCOM,x				; CIO close channel command
	jmp     CIOV				; CIO will RTS

; ----------------------------------------------------------------------------
sub_b242:
	pha					;
	lda     #$1B    			; 
	sta     TSTDAT				; 
	jsr     sub_b1ef			; Call CIO put char
	pla             			; 
	jsr     sub_b1ef			;
	lda     #$00    			; Call CIO put char
	sta     TSTDAT				;
LB252:  rts             			;

;*******************************************************************************
;*                                                                             *
;*                               sub_user_baud                                 *
;*                                                                             *
;*               User-requested baud change from OPTION + 1 or 3               *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION:
; This subroutine is called from the key press combination OPTION + '1' to 
; force 1200 baud. Or OPTION + '3' to force 300 baud.
LB253: 
sub_user_baud:
	jsr     close_ch1
	bmi     display_comm_error
	ldy     #$18    			; B258 A0 18                    ..
	sty     word_9C     			; B25A 84 9C                    ..
	ldy     #$08    			; B25C A0 08                    ..
	lda     CURRENT_BAUD			; Get baud ($FF=300, $00=1200)
	bne     LB268   			; B260 D0 06                    ..
	lda     #<LB91C				; String "1200 baud"
	ldx     #>LB91C
	bne     LB26C   			; will always branch
LB268:  lda     #<LB913				; "300  baud"
	ldx     #>LB913
LB26C:  jsr     print_string
	jmp     sub_open_modem			; sub_open_modem will RTS

;*******************************************************************************
;*                                                                             *
;*                                  sub_b272                                   *
;*                                                                             *
;*                        Configure for Microbits 300                          *
;*                                                                             *
;*******************************************************************************
sub_b272:
	jsr     close_ch1			; 
	jsr     sub_register_mpp		; Register Microbits 300 in HATABS
	ldy     #LB96E-LB96E			; Open "R:" on channel #1
	jsr     call_cio_or_err			; 

;**(n) Set flag that MPP is assumed ********************************************
	ldy     #$01    			; 
	sty     IS_MPP				; Let IS_MPP = 1

;**(n) Print "Microbits 300" message *******************************************
	ldy     #$10    			; String length - 1
	lda     #<LB925				; 'Microbits 300'
	ldx     #>LB925				;
	jmp     print_string			; print_string will RTS

;*******************************************************************************
;*                                                                             *
;*                                    LB28A                                    *
;*                                                                             *
;*                            ???????????????????                              *
;*                                                                             *
;*******************************************************************************
LB28A:  ldx     IS_MPP				; 
	dex             			; 
	beq     :+				; If modem then close channel #1
	jsr     close_ch1
:	ldy     #LB99E-LB96E			; Open "P:" on channel #3
	jsr     call_cio			; 
	bmi     LB2C0   			; B297 30 27                    0'

	lda     #$20    			; B299 A9 20                    . 
	sta     byte_D9 			; B29B 85 D9                    ..

	lda     #<$1800    			; B29D A9 00                    ..
	sta     off_FA
	lda     #>$1800    			; B2A1 A9 18                    ..
	sta     off_FA+1
:	jsr     sub_b2c6   			; B2A5 20 C6 B2                  ..
	dec     byte_D9 			; B2A8 C6 D9                    ..
	bne     :-
LB2AC:  ldx     #$30    			; Set CIO Channel #3 (Printer)
	jsr     sub_b23a			; Call CIO close channel
	ldx     IS_MPP
	dex             			; B2B3 CA                       .
	beq     LB2F9   			; B2B4 F0 43                    .C
	bpl     :+
	jmp     LB71C   			; B2B8 4C 1C B7                 L..

; ----------------------------------------------------------------------------
:	ldy     #LB996-LB96E			; Atari Modem
	jmp     call_cio_or_err			; Open "T:" for read/write

; ----------------------------------------------------------------------------
LB2C0:  jsr     sub_b828
	jmp     LB2AC   			; B2C3 4C AC B2                 L..

; ----------------------------------------------------------------------------
sub_b2c6:
	ldy     #$3F    			; B2C6 A0 3F                    .?
	lda     #$20    			; B2C8 A9 20                    . 
:	cmp     (off_FA),y
	bne     :+
	dey             			; B2CE 88                       .
	bpl     :-
:	lda     #$9B    			; B2D1 A9 9B                    ..
	iny             			; B2D3 C8                       .
	beq     :+
	iny             			; B2D6 C8                       .
:	sty     ICBL+$30
	ldx     #$30    			; B2DA A2 30                    .0
LB2DC:  ldy     #$09    			; B2DC A0 09                    ..
	sty     ICCOM+$30
	ldy     off_FA
	sty     ICBA+$30
	ldy     off_FA+1
	sty     ICBA+1+$30
	jsr     CIOV
	lda     #$40    			; B2EE A9 40                    .@
	clc             			; B2F0 18                       .
	adc     off_FA
	sta     off_FA
	bcc     LB2F9
	inc     off_FA+1
LB2F9:	rts             			; B2F9 60                       `

; ----------------------------------------------------------------------------

sub_b2fa:
	tsx             			; B2FA BA                       .
	stx     byte_1344
	sta     byte_1347
	ldx     byte_1346
	bne     :+
	ldx     TSTDAT
	beq     LB353   			; B308 F0 49                    .I
	lda     #$80    			; B30A A9 80                    ..
	sta     byte_1346
	ldy     #$01    			; B30F A0 01                    ..
	rts             			; B311 60                       `

; ----------------------------------------------------------------------------
:	bpl     :+
	lda     byte_1347
	sta     byte_1343
	tax             			; B31A AA                       .
	lda     #$00    			; B31B A9 00                    ..
	sta     byte_1346
	beq     LB325   			; branch always

; ----------------------------------------------------------------------------
:	ldx     byte_1343
LB325:  cpx     #$46    			; B325 E0 46                    .F
	beq     LB339   			; B327 F0 10                    ..
	ldx     #$02    			; B329 A2 02                    ..
	jsr     sub_b422
	ori	byte_133c, $01
	ldy     #$01    			; B336 A0 01                    ..
	rts             			; B338 60                       `

; ----------------------------------------------------------------------------
LB339:  lda     byte_133c   			; B339 AD 3C 13                 .<.
	and     #$F1    			; B33C 29 F1                    ).
	sta     DVSTAT+1
	lda     byte_133d
	and     #$F1    			; B344 29 F1                    ).
	sta     DVSTAT  			; B346 8D EA 02                 ...
	ldy     #$00    			; B349 A0 00                    ..
	sty     DVSTAT+3
	sty     byte_133d
	iny             			; B351 C8                       .
	rts             			; B352 60                       `

; ----------------------------------------------------------------------------
LB353:  lda     byte_1340
	cmp     #$1F    			; B356 C9 1F                    ..
	bcs     LB353   			; B358 B0 F9                    ..
	sei             			; B35A 78                       x
	jsr     sub_b369
	lda     $CB     			; B35E A5 CB                    ..
	bne     :+
	jsr     sub_b545			; Enable serial out and timer IRQs
:	cli             			; B365 58                       X
	ldy     #$01    			; B366 A0 01                    ..
	rts             			; B368 60                       `

; ----------------------------------------------------------------------------
; store character into some sort of buffer
sub_b369:
	ldy     byte_1342   			; some sort of buffer offset
	lda     character                       ; load character  byte_1347
	sta     byte_1310,y 			; store character into some sort of buffer
	jsr     sub_b55c                        ; let buffer offset = (buffer offset + 1) && $1F
	sta     byte_1342   			; 
	inc     OUTBFPT				; increment output buffer pointer
	rts             			; 

; ----------------------------------------------------------------------------

sub_b37c:
	tsx             			; B37C BA                       .
	stx     byte_1344
:	cli             			; B380 58                       X
	sei             			; B381 78                       x
	ldy     BUFLEN  			; B382 AC 3F 13                 .?.
	cpy     INPBFPT  
	beq     :-

; Get character from input buffer
sub_b38a:  
	lda     INPBUF,y 			; Get byte from buffer
	and     #$7F    			; Strip off the PLATO parity bit?
	sta     character                       ; Store character byte_1347
	iny             			; 
	sty     BUFLEN  			; Increment BUFLEN
	dec     byte_CC                         ;
	cli             			; Enable interrupts
	ldy     #$01    			; 
	rts             			;

;*******************************************************************************
;*                                                                             *
;*                                 sub_b39c                                    *
;*                                                                             *
;*                            RS232 Get Status???                              *
;*                                                                             *
;*******************************************************************************

sub_b39c:
	cld             			; B39C D8                       .
	tya             			; B39D 98                       .
	pha             			; B39E 48                       H
	lda     SERIN
	jsr     sub_b3b9
	lda     SKSTAT
	sta     SKREST  			; B3A8 8D 0A D2                 ...
	eor     #$FF    			; B3AB 49 FF                    I.
	and     #$C0    			; B3AD 29 C0                    ).
	ora     byte_133d
	sta     byte_133d
LB3B5:  pla             			; B3B5 68                       h
	tay             			; B3B6 A8                       .
	pla             			; B3B7 68                       h
	rti             			; B3B8 40                       @

; MPP Microbit 300 routine to write to Input Buffer?
sub_b3b9:
	ldy     INPBFPT				; 
	sta     INPBUF,y 			; 
	iny             			; 
	sty     INPBFPT				; Move pointer
	inc     byte_CC				;
	rts             			; 

; ----------------------------------------------------------------------------
sub_b3c6:
	cld             			; B3C6 D8                       .
	tya             			; B3C7 98                       .
	pha             			; B3C8 48                       H
	lda     OUTBFPT
	bne     :+

	lda     #$E7    			; Set interrupt flags
	and     POKMSK				;
	jsr     sub_irqen			; Enable interrupts
	jmp     LB3B5   			; Restore Y and return from interrupt

; ----------------------------------------------------------------------------
:	jsr     sub_b3e7
	sta     SEROUT

:	lda     IRQST				; Wait for VSEROC to be re-enabled
	and     #$08    			;
	beq     :-

	bne     LB3B5   			; Restore Y and return from interrupt

; ----------------------------------------------------------------------------
sub_b3e7:  
	ldy     byte_1341   			; B3E7 AC 41 13                 .A.
	lda     byte_1310,y 			; B3EA B9 10 13                 ...
	pha             			; B3ED 48                       H
	jsr     sub_b55c
	sta     byte_1341   			; B3F1 8D 41 13                 .A.
	dec     byte_1340
	pla             			; B3F7 68                       h
	rts             			; B3F8 60                       `

; ----------------------------------------------------------------------------

sub_b3f9:
	cld             			; B3F9 D8                       .
	tya             			; B3FA 98                       .
	pha             			; B3FB 48                       H
	lda     $CB     			; B3FC A5 CB                    ..
	beq     LB415   			; B3FE F0 15                    ..
	lda     #$00    			; B400 A9 00                    ..
	sta     $CB     			; B402 85 CB                    ..
	beq     LB3B5   			; B404 F0 AF                    ..

sub_b406:
	cld             			; B406 D8                       .
	tya             			; B407 98                       .
	pha             			; B408 48                       H
	lda     byte_133c   			; B409 AD 3C 13                 .<.
	eor     #$80    			; B40C 49 80                    I.
	sta     byte_133c   			; B40E 8D 3C 13                 .<.
	and     #$80    			; B411 29 80                    ).
	bne     LB3B5   			; B413 D0 A0                    ..
LB415:  lda     byte_133c   			; B415 AD 3C 13                 .<.
	and     #$3E    			; B418 29 3E                    )>
	sta     byte_133c   			; B41A 8D 3C 13                 .<.
	jmp     LB3B5   			; B41D 4C B5 B3                 L..

; ----------------------------------------------------------------------------
sub_b420:
	ldx     #$00    			; B420 A2 00                    ..

sub_b422:
	lda     $CB     			; 
	bne     sub_b422			; Wait for $CB to be $00
	jsr     sub_b53a

:	lda     IRQST
	and     #$08    			; Wait for VSEROC
	bne     :-

	lda     #$35    			; 
	sta     PBCTL				; 
	sta     $CB     			; Let $CB = $35

	sei             			; Inhibit IRQs
	jsr     sub_b369			; Store character in output buffer
	cli             			; Enable IRQs
	jsr     sub_b53a
sub_b43f:
	ldy     LB47A,x 			; B43F BC 7A B4                 .z.
	ldx     #$00    			; B442 A2 00                    ..
	jsr     sub_b51f
LB447:  lda     $CB     			; B447 A5 CB                    ..
	beq     LB46B   			; B449 F0 20                    . 
	lda     byte_1345
	bne     LB447   			; B44E D0 F7                    ..
	sta     byte_1346
	sta     $CB     			; B453 85 CB                    ..
	lda     #$3D    			; B455 A9 3D                    .=
	sta     PBCTL
	lda     byte_133a
	and     #$10    			; B45D 29 10                    ).
	beq     LB464   			; B45F F0 03                    ..
	sec             			; B461 38                       8
	bcs     LB471   			; B462 B0 0D                    ..
LB464:  ldx     byte_1344
	txs             			; B467 9A                       .
	ldy     #$8B    			; B468 A0 8B                    ..
	rts             			; B46A 60                       `

; ----------------------------------------------------------------------------
LB46B:  lda     #$3D    			; B46B A9 3D                    .=
	sta     PBCTL
	clc             			; B470 18                       .
LB471:  lda     byte_133a
	and     #$EF    			; B474 29 EF                    ).
	sta     byte_133a
	rts             			; B479 60                       `

; ----------------------------------------------------------------------------

LB47A:  .byte   $08     			; B47A 08                       .
	.byte   $3C     			; B47B 3C                       <
	.byte   $B4     			; B47C B4                       .

;*******************************************************************************
;*                                                                             *
;*                                  sub_b47d                                   *
;*                                                                             *
;*                        Called during cart_init #1 ????                      *
;*                                                                             *
;*******************************************************************************

sub_b47d:
	sei             			; disable interrupts
	jsr     sub_b4a5                        ; Initialize POKEY

; (n) Clear 8 bytes ************************************************************
	ldy     #$07    			; 
	lda     #$00    			;
:	sta     byte_133c,y                     ; 
	dey             			;
	bpl     :-                              ; End loop after 8 iterations

; (n) Clear ********************************************************************
	sta     TSTDAT                          ; Let TSTDAT    = 0
	sta     byte_1346                       ; Let byte_1346 = 0

; (n) Set serial interrupt flags ***********************************************
	lda     #$C7    			; Disable serial input-data-ready,... 
	and     POKMSK                          ; ...out-transmission-finished interrupts. 
	ora     #$20    			; Enable serial output-data-required interrupt.
	jsr     sub_irqen                       ; Register new interrupt flags.

	cli             			; Enable interrupts
	rts             			; 

; -----------------------------------------------------------------------------

; VSERIN vectors
LB49B:
	.addr	sub_b39c
	.addr	sub_b3c6
	.addr	sub_b3c6

; VPRCED vectors
LB4A1:
	.addr	sub_b3f9
	.addr	sub_b406

;*******************************************************************************
;*                                                                             *
;*                                 sub_b4a5                                    *
;*                                                                             *
;*                              Initialize POKEY                               *
;*                                                                             *
;*******************************************************************************

sub_b4a5:

;**(n) Turn off bits 3-7 in SKCTL (and its shadow SSKCTL) **********************
	lda     #$07    			; Disable bits 3-7 on serial port control
	and     SSKCTL  			; 
	ora     #$70    			; 
	sta     SSKCTL  			; 
	sta     SKCTL                           ;
	sta     SKREST  			; 

	ldi	AUDCTL, $78			; Configure POKEY audio channels

;**(n) Initialize audio tones **************************************************
	ldx     #$07    			; 
	lda     #$A0    			; Set AUDF[1-4] and AUDC[1-4]
:	sta     AUDF1,x 			; for every 160 input pulses 
	dex             			; emit 1 output pulse
	bpl     :-      			; End loop after 8 iterations

	lda     #$0B    			; Set AUDF2 and AUDF4
	sta     AUDF2   			; For every 11 input pulses
	sta     AUDF4   			; emit 1 output pulse

;** (n) Install 3 new Serial Data-Ready Input vector addresses *****************
	ldx     #$05    			; 
:	lda     VSERIN,x 			; Save current vector address
	sta     byte_1330,x 			; to RAM
	lda     LB49B,x 			; Load new vector address
	sta     VSERIN,x 			; from cartridge ROM
	dex             			; 
	bpl     :-      			; End loop after 6 iterations

;** (n) Install 2 new Serial Proceed-Line vector addresses *********************
	ldx     #$03    			; 
:	lda     VPRCED,x 			; Save current vector address
	sta     $1336,x 			; to RAM
	lda     LB4A1,x 			; Load new vector address
	sta     VPRCED,x 			; from cartridge ROM
	dex             			; 
	bpl     :-      			; End loop after 4 iterations

;** (n) Enable Peripheral A interrupt ******************************************
	ori	PACTL, $01
	rts             			; 

; ----------------------------------------------------------------------------
sub_b4f7:
	sei             			; Inhibit IRQs

	ldy     #$05    			; 
:	lda     byte_1330,y 			; Install new vectors for ..
	sta     VSERIN,y 			; VSERIN, VSEROR, VSEROC IRQs
	dey             			; 
	bpl     :-      			; End loop

	ldy     #$03    			; B503 A0 03                    ..
:	lda     $1336,y 			; B505 B9 36 13                 .6.
	sta     $0202,y 			; B508 99 02 02                 ...
	dey             			; B50B 88                       .
	bpl     :-				; B50C 10 F7                    ..

	lda     PACTL
	and     #$FE    			; B511 29 FE                    ).
	sta     PACTL
	lda     #$C7    			; Disable VSERIN, VSEROR...
	and     POKMSK				; ...VSEROC IRQs
	jsr     sub_irqen			; Update the IRQ system
	cli             			; Enable IRQs
	rts             			; 

;*******************************************************************************
;*                                                                             *
;*                                 sub_b51f                                    *
;*                                                                             *
;*                        Configure System Timer 1 IRQ	                       *
;*                                                                             *
;*******************************************************************************

sub_b51f:
	lda     #<sub_b534			; Point to code that runs on system timer IRQ
	sta     CDTMA1
	lda     #>sub_b534
	sta     CDTMA1+1

	sei             			; Inhibit IRQs
	lda     #$01    			; Set byte_1345 flag (timer event clears it)
	sta     byte_1345
	jsr	SETVBV
	cli             			; Enable IRQs

	rts

;*******************************************************************************
;*                                                                             *
;*                                 sub_b534                                    *
;*                                                                             *
;*                          System Timer 1 IRQ code                            *
;*                                                                             *
;*******************************************************************************

sub_b534:
	lda     #$00    			; Clear byte_1345 flag
	sta     byte_1345			
	rts             			 

; ----------------------------------------------------------------------------
sub_b53a:  
	sei             			; Inhibit IRQs
	jsr     sub_b545			; Enable Serial out and timer IRQs
	cli             			; Enable IRQs

:	lda     byte_1340
	bne     :-				; Wait for byte_1340 to be $00
	rts             			; 

;*******************************************************************************
;*                                                                             *
;*                                  sub_b545                                   *
;*                                                                             *
;*                    Enable Serial Out and Timer Interrupts                   * 
;*                                                                             *
;*******************************************************************************
sub_b545:
	lda     POKMSK				; Enable IRQs for VSEROR VSEROC...
	ora     #$18    			; ...VTIMR{1,2,4}.
						; Fall through to sub_irqen.

;*******************************************************************************
;*                                                                             *
;*                                  sub_irqen                                  *
;*                                                                             *
;*                              Enable Interrupts                              *
;*                                                                             *
;*******************************************************************************
; DESCRIPTION
;
; IRQEN is a write-only register. Thus, we must maintain a current value of that
; register in RAM at address POKMSK. To avoid conflict, IRQs should be inhibited 
; (SEI) before calling sub_irqen and enabled after returning from sub_irqen.
;
sub_irqen:
	sta     POKMSK 			; Save the requested IRQ flags to RAM
	sta     IRQEN  			; and to the hardware register, too.
	rts

; ----------------------------------------------------------------------------
sub_b54f:
	ldx     #$00				; Arg for SETVBV??
	ldy     #$03    			; Arg for SETVBV??
	jsr     sub_b51f			; Set System Timer 1 IRQ and run SETVBV

:	lda     byte_1345
	bne     :-				; Wait until System Timer 1 IRQ runs
	rts             			; 

; ----------------------------------------------------------------------------
sub_b55c:
	iny             			; increment buffer offset
	tya             			; let a = new buffer offset
	and     #$1F    			; wrap offset if > 31
	rts             			; 

; ----------------------------------------------------------------------------

sub_b561:
	tsx             			; B561 BA                       .
	stx     byte_1344
	lda     ICAX1Z
	sta     byte_133a
	lda     #$00    			; B56A A9 00                    ..
	sta     TSTDAT
	jsr     sub_b47d
	jsr     sub_b54f
	lda     #$59    			; 'Y'
	sta     character                       ; byte_1347
	ldx     #$01    			; B579 A2 01                    ..
	jsr     sub_b422
	jmp     LB5A5   			; B57E 4C A5 B5                 L..

; ----------------------------------------------------------------------------

sub_b581:
	tsx             			; B581 BA                       .
	stx     byte_1344
	lda     byte_133a
	beq     LB5A5   			; B588 F0 1B                    ..
	jsr     sub_b53a
	lda     byte_B2
	bne     LB595   			; B58F D0 04                    ..
	lda     #$51    			; B591 A9 51                    .Q
	bne     LB597   			; B593 D0 02                    ..
LB595:  lda     #$5A    			; 'Z'
LB597:  sta     character                       ; byte_1347
	jsr     sub_b420
	jsr     sub_b4f7
	lda     #$00    			; B5A0 A9 00                    ..
	sta     byte_133a
LB5A5:  ldy     #$01    			; B5A5 A0 01                    ..
	rts             			; B5A7 60                       `

;*******************************************************************************
;*                                                                             *
;*                                  t_handler                                  *
;*                                                                             *
;*                      ?????????????????????????????????                      *
;*                                                                             *
;*******************************************************************************

t_handler:
	.addr	sub_b561-1
	.addr	sub_b581-1
	.addr	sub_b37c-1
	.addr	sub_b2fa-1

;*******************************************************************************
;*                                                                             *
;*                               sub_register_mpp                              *
;*                                                                             *
;*                 Register MPP Microbit 300 modem in IOCB??                   *
;*                                                                             *
;*******************************************************************************
sub_b5b0:
sub_register_mpp:
	lda     #$00    			; B5B0 A9 00                    ..
	sta     PACTL
	ori	PORTA, $50
	lda     #$3C    			; B5BD A9 3C                    .<
	sta     PACTL

;** (n) Find empty slot in HATABS **********************************************
	ldx     #$00    			; Start search at offset 0 of HATABS
	lda     #'R'
	jsr     sub_bf86			; Returns HATABS offset in X
	bne     LB5D5   			; Quit if no slots

;** (n) Add Microbit 300 jump table to HATABS **********************************
	lda     #<LB5D6				; 
	sta     HATABS+1,x			;

	lda     #>LB5D6				;
	sta     HATABS+2,x			;

LB5D5:  rts             			; 

; ----------------------------------------------------------------------------
; MPP Microbit 300 Jump Table
; ----------------------------------------------------------------------------
LB5D6:
	.addr	JOPEN-1                         ; MPP JOPEN $B667 
	.addr   JCLOSE-1                        ; MPP JCLOSE $B6A4
	.addr	JREAD-1                         ; MPP GET BYTE $B708
	.addr	sub_b6e9-1                      ; MPP PUT BYTE $B6E8

;*******************************************************************************
;*                                                                             *
;*                                    DRIVER                                   *
;*                                                                             *
;*                 MPP Driver Code similar to MPP Smart Term 4.1               *
;*                                                                             *
;*******************************************************************************
; DESCRIPTION
; This section closely matches the DRIVER subroutine in MPP's Smart Terminal cart
; available at https://archive.org/details/MPPSmartTerminalv4.1 
; 
sub_b5de:
DRIVER: cld                                     ; Similar to DRIVER in MPP source at A0AC
	tya             			; 
	pha             			; Save Y on stack
	lda     INPBIT  			; 
	bne     GETBIT  			; Middle byte
	lda     #$20    			; 
	and     PIA                             ;
	beq     SENDBIT 			; No start
	lda     #$08    			; Length (8 bits in a byte)
	sta     INPBIT  			; 
	lsr     a       			; # IRQs (A = $04)
	sta     INPINT  			; 
	bne     SENDBIT 			; 
;
; READ A BIT FROM PIA IF IT'S TIME
;
GETBIT: dec     INPINT  			; 
	bne     SENDBIT 			; Not time yet
	lda     ISTOP   			; Any stops
	beq     NOSTOP  			; 
	dec     INPBIT  			; 
	dec     ISTOP   			; 
	beq     SENDBIT 			; 
NOSTOP: lda     #$20    			; 
	and     PIA				;
	beq     ZIPO				; 
	sec             			; 
	bcs     ROTATE   			; 
ZIPO:	clc             			; 
ROTATE: ror     INP				; 
	dec     INPBIT  			; 
	bne     NOBYTE   			; 
	lda     INP         			; 
	eor     #$FF    			; 
	jsr     sub_b3b9			; Store INP to INPBUF
	inc     INPBIT  			;
	inc     ISTOP   			;
NOBYTE: lda     #$03    			;
	sta     INPINT  			;
SENDBIT:					; Similar to SENDBIT found in MPP source A11B
	lda     OUTBIT  			; 
	beq     CHKBUF                          ;
	dec     OUTINT  			;
	bne     LB652                           ;
	dec     OUTBIT  			; 
	beq     CHKBUF                          ;
	clc             			;
	ror     OUTBUF  			;
	bcs     ONEOUT  			;
	lda     #$EF    			;
	and     PIA                             ;
	bcc     ZIPSKIP 			;
LB645:
ONEOUT: lda     #$10    			;
	ora     PIA                             ;
ZIPSKIP:  
	sta     PIA                             ; Start bit
	lda     #$03    			;
	sta     OUTINT  			; 
LB652:	
RETURN: jmp     LB3B5   			; Return RTI
CHKBUF:	lda     OUTBFPT				; Similar to CHKBUF routine found MPP source A146
	beq     RETURN   			; 
	jsr     sub_b3e7			; TODO
	eor     #$FF    			; 
	sta     OUTBUF  			; 
	lda     #$0A    			; 
	sta     OUTBIT  			;
	bne     ONEOUT  			;
; OPEN
;
JOPEN:	lda     #$13    			; Similar to JOPEN found MPP source A913
	sta     SKCTL				;
	sta     SSKCTL  			;
	lda     PIA				;
	and     #$BF    			;
	sta     PIA				;
	lda     #$00    			; 
	sta     OUTBIT  			; Let OUTBIT = 0
	sta     INPBIT  			; Let INPBIT = 0
	sta     ISTOP   			; Let ISTOP  = 0
	sta     AUDCTL				; 
	lda     #BAUD   			; Set BAUD = 300
	sta     AUDF1				; 
	lda     #$A0    			; 
	sta     AUDC1				;
	lda     #<DRIVER			; Register MPP driver with POKEY ...
	sta     VTIMR1				; timer interrupt.
	lda     #>DRIVER			;
	sta     VTIMR1+1			;
	lda     POKMSK				;
	ora     #$01    			; 
	jsr     sub_irqen			; Set interrupt flags
	sta     STIMER	                        ;
	ldy     #$01    			; 
	rts             			;
LB6A6:
JCLOSE: lda     OUTBFPT				; Similar to JCLOSE found in MPP source A1D0
	bne     JCLOSE				; Wait for something to arrive
	lda     OUTBIT  			; 
	bne     JCLOSE  			; 
	lda     #$FE    			; 
	and     POKMSK				;
	jsr     sub_irqen			; Set interrupt flags
	ldy     #$01    			; return code?
	rts             			; 

;*******************************************************************************
;*                                                                             *
;*                                 sub_b6b8                                    *
;*                                                                             *
;*                                   OPEN                                      *
;*                                                                             *
;*******************************************************************************
sub_b6b8:
;** (1) Initialize variables and return ****************************************
	lda     #$8D    			; 
	sta     byte_133a                       ; Let byte_133a = $8D

	ldy     #$00    			; 
	sty     byte_1342   			; Let $1342	= $00
	sty     byte_133d                       ; Let byte_133d = $00
	iny             			; return code?  = 1
	sty     $CB     			; Let $CB       = $01
	rts             			; 

;*******************************************************************************
;*                                                                             *
;*                                 sub_b6c9                                    *
;*                                                                             *
;*                                  CLOSE                                      *
;*                                                                             *
;*******************************************************************************
sub_b6c9:  
	lda     $CB     			; 
	beq     sub_b6c9                        ; Waiting

;** (n) Restore vector addresses for VSERIN, VSEROR, VSEROC ********************
	ldy     #$05    			; 
	sei             			; Prevent IRQs
:	lda     byte_1330,y 			; Values saved earlier in sub_b719
	sta     VSERIN,y 			; are restored to the serial vectors
	dey             			; 
	bpl     :-                              ; End loop after 6 iterations

	cli             			; Enable IRQs
	sty     byte_1346
	iny             			; B6DD C8                       .
	sty     DAUX1   			; B6DE 8C 0A 03                 ...
	sty     byte_133a
	lda     #'W'    			; Let A = Write (verify) command
	jmp     sub_sendsio                     ; Send SIO command frame

;*******************************************************************************
;*                                                                             *
;*                                 sub_b6e9                                    *
;*                                                                             *
;*                                MPP JWRITE                                   *
;*                                                                             *
;*******************************************************************************
sub_b6e9:
JWRITE: sta     character                       ; 
	ldx     #$01    			; 
	stx     $21     			;

:	lda     OUTBFPT                         ; 
	cmp     #$1F    			; 31?
	bcs     :-                              ; Waiting for something to appear?

	sei             			; Disable interrupts
	jsr     sub_b369                        ; store character into some sort of buffer

	lda     POKMSK                          ; Enable serial output data-required...
	ora     #$18    			; ... and output transmission-finished interrupts
	jsr     sub_irqen			; Enable new interrupt flags

	ldy     #$00    			; 
	sty     $CB     			; Let $CB = 0
	cli             			; Enable interrupts
	iny             			; Let Y = $01
	rts             			; 

;*******************************************************************************
;*                                                                             *
;*                                 sub_b709                                    *
;*                                                                             *
;*                  READ (this address is in 2 jump tables)                    *
;*                                                                             *
;*******************************************************************************
sub_b709:
JREAD:	cli             			; Enable IRQs
	sei             			; Disable IRQs
	ldy     BUFLEN  			; 
	cpy     INPBFPT                         ;
	beq     sub_b709                        ; Continue waiting?
	jsr     sub_b38a			; Get char from buffer
	ldy     #$01    			; 
	rts             			; 

;*******************************************************************************
;*                                                                             *
;*                               sub_open_modem                                 *
;*                                                                             *
;*                           ?????????????????????                             *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
; At this point there are 3 possibilites:
; 1) A direct-connect modem was found but user is asking to set it to 1200 baud
;    (by pressing '1' at the connection prompt). If the direct-connect modem 
;    does not support 1200 baud, return with sign flag set.
; 2) A Microbits 300 is connected then return with sign flag set.
; 3) An 850 is connected and will be set to the default 1200 baud and return 
;    with sign flag clear.
sub_b719:
sub_open_modem:
	jsr     sub_setbaud			; Set baud/word/etc and request ACK
LB71C:  ldy     #$00    			; Prepare CIO open R: on channel #1
	jsr     call_cio			; Call CIO
	bpl     :+				; Return if direct connect modem... 
	rts             			; ...could not be set to 1200 baud

;** (n) If here then either an 850 or presumed Microbits is connected **********
:	lda     #$00    			; Let X = 0
	sta     IS_MPP				; Default IS_MPP to modem (0)
	sta     INPBFPT                         ; Let INPBFPT   = 0
	sta     BUFLEN  			; Let BUFLEN    = 0
	sta     byte_1342   			; Let byte_1342 = 0
	sta     byte_1341   			; Let byte_1341 = 0
	sta     DBYT+1 			        ; Set MSB of buffer size to 0

;** (n) Construct SIO command frame for possible 850 ***************************
	lda     #<byte_1330			; 
	sta     DBUF				; 
	lda     #>byte_1330			;
	sta     DBUF+1				;

	lda     byte_133a                       ; Construct SIO command frame
	sta     DAUX1   			; 

	lda     #$09    			; 
	sta     DBYT    			; Tell SIO to receive 9 bytes

	ldy     #$40    			; Prep Y for DSTATS ($40->receive data)
	lda     #$58    			; Prep A for DCOMND ($58->?? ('X'?)

;** (n) Submit SIO command to possible 850 *************************************
	jsr     sub_sendsio                     ; Send SIO command frame
	bpl     :+				; Continue if 850 is present
	rts             			; Return with minus if couldnt talk to 850

;** As far as I can tell, the remainder can only be run if an 850 is connected.

;** (n) 850! Set serial port mode control **************************************
:	sei             			; Inhibit IRQs
	ldi	SKCTL, $73			; Set serial port mode control

;** (n) Set audio control ******************************************************
	lda     $1338   			; B75C AD 38 13                 .8.
	sta     AUDCTL				; Set audio control

;** (n) Copy 8 bytes from $1330-$1337 to AUDF1/C1-AUDF4/C4 *******************
	ldy     #$07    			; B762 A0 07                    ..
:	lda     byte_1330,y 			; B764 B9 30 13                 .0.
	sta     AUDF1,y 			; B767 99 00 D2                 ...
	dey             			; B76A 88                       .
	bpl     :-      			; B76B 10 F7                    ..

;** (n) Save/replace VSERIN VSEROR VSEROC interrupt vectors ******************
	ldx     #$06    			; 
:	lda     VSERIN-1,x 			; Save currect serial vector...
	sta     byte_1330-1,x 			; ...addresses to RAM
	lda     LB822-2+1,x                       ; Load new vector...
	sta     VSERIN-1,x 			; ...addresses to IRQ registers
	dex             			; 
	bne     :-      			; End loop

	stx     byte_CC			        ; Let byte_CC = 0
	stx     byte_CD 			; Let byte_CD = 0

	lda     POKMSK                          ; 
	ora     #$20    			; Enable serial data-ready interrupt
	jsr     sub_irqen			; Register new interrupt flags

	ldx     #$01    			;
	stx     byte_1346			; Let byte_1346 = $01

	cli             			; Enable IRQs
	ldy     #$01    			; 
	lda     byte_133a                       ; 
	sta     ICAX1Z				; Let ICAX1Z = byte_133a
	cpy     #$00    			; B796 C0 00                    ..
	rts             			; B798 60                       `

;*******************************************************************************
;*                                                                             *
;*                                  sub_R_status                               *
;*                                                                             *
;*                        Add R to HATABS and run status.                      *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
; Add R device to HATABS. Then send SIO status command. 
; From observation in an emulator:
; Returns Y=$8A and sign flag set if no 850 found.
; Returns Y=$01 and sign flag clear if 850 found.

sub_b799:
sub_R_status:

;** (1) Initialize *************************************************************
	lda     #$00    			; Let Y         = 0
	sta     byte_133a       		; Let byte_133a = 0
	sta     DBYT+1  			; Let DBYTHI    = 0
	tax             			; Let X         = 0

;** (2) Find first empty Handler Table slot ************************************
	jsr     sub_hatabs_slot			; Let X = offset to new slot
	beq     :+	   			; If slot was found, create new handler entry
	rts             			; otherwise return

;** (3) Create new handler table record (3 bytes) ******************************
:	lda     #'R'    			; RS232 Device
	sta     byte_1346       		; Save "R" in RAM
	sta     HATABS,x			; Save "R" in the new slot

	lda     #<LB81A 			; MSB of start of handler vector table
	sta     HATABS+1,x      		; Save MSB in the new slot

	lda     #>LB81A 			; LSB of start of handler vector table
	sta     HATABS+2,x      		; Save LSB in the new slot

;** (4) Prepare SIO to load STATUS results into DVSTAT *************************
	lda     #<DVSTAT			; LSB of data buffer address
	sta     DBUF                            ;

	lda     #>DVSTAT    			; MSB of data buffer address
	sta     DBUF+1                          ; By luck, >DVSTAT is 2
	sta     DBYT    			; Tell SIO to load 2 bytes

;** (5) ************************************************************************
	ldy     #$40    			; Prep Y for use by DSTATS ($40->receive data)
	lda     #'S'    			; Prep A for use by DCOMND ($53->STATUS)
	jsr     sub_sendsio			; SIOV returns with Y=($01->850,$8A->No 850)

	lda     character                       ; A = $51 from somewhere earlier
	ora     DVSTAT  			; DSTAT currently ($00,$F0->850, $00,$00->no 850)
	sta     DVSTAT  			; DSTAT become ($51,$F0 or $51,$00)
	cpy     #$00    			; Sign flag set->850
	rts             			; Sign flag clear->850

; ----------------------------------------------------------------------------

sub_b7da:
	lda     byte_1340
	bne     :+
	lda     byte_1346
	sta     $CB     			; B7E2 85 CB                    ..
:	jmp	sub_b3c6

;*******************************************************************************
;*                                                                             *
;*                               sub_setbaud                                   *
;*                                                                             *
;*                             Change baud rate                                *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
; Send two command frames to 850
;
sub_b7e7:
sub_setbaud:

; **(1) Send command frame #1 for setting baud and word size *******************
	lda     #$42    			; DCOMND for setting baud, parity, stopbits
	bit     CURRENT_BAUD			; 
	bmi     :+				; if CURRENT_BAUD == $FF
	ldx     #$0A    			; then AUX1 = $00 ( 300 baud, 8 bit word)
	bne     LB7F3   			; else AUX1 = $0A (1200 baud, 8 bit word)
:	ldx	#$00    			; 
LB7F3:  jsr     LB7FA   			; 

; **(2) Send command frame #2 for ACKnowledge **********************************
	lda     #'A'    			; DCOMND $41->ACKnowledge (returns 'C'omplete or 'E'rror)
	ldx     #$F3    			; AUX1 for ????
LB7FA:  stx     DAUX1   			; 
	ldy     #$00    			; AUX2: 1 stop bit, no monitoring of DSR, CTS, CRX
	sty     DAUX2   			; Fall into sub_sendio

;*******************************************************************************
;*                                                                             *
;*                               sub_sendsio                                   *
;*                                                                             *
;*                    Send SIO Command to RS232 Device 1                       *
;*                                                                             *
;*******************************************************************************
sub_b802:
sub_sendsio:
	sta     DCOMND  			; Command code is set in caller

;**(1) Set Device Unit number **************************************************
	ldx     #$01    			; Device 1 (as in R1:)
	stx     DUNIT   			; 
	sty     DSTATS  			; Set the data direction ($40->in, $80->out)

;**(2) Set Device Serial bus ID ************************************************
	ldy     #$50    			; $50 = ID for RS232 port R1:
	sty     DDEVIC  			; 

;**(3) Set Device handler time-out *********************************************
	ldy     #$08    			; 8 seconds time-out
	sty     DTIMLO  			; 

;**(4) Send command frame (DCOMND DAUX1 DAUX2, etc) to SIO bus *****************
	jmp     SIOV    			; SIOV will execute an RTS

;*******************************************************************************
;*                                                                             *
;*                                   LB81A                                     *
;*                                                                             *
;*                             Handler Vector Table?????                       *
;*                                                                             *
;*******************************************************************************
LB81A:
	.addr	sub_b6b8-1      		; OPEN vector???
	.addr	sub_b6c9-1      		; CLOSE vector???
	.addr	sub_b709-1      		; GET BYTE vector???
	.addr	sub_b6e9-1      		; PUT BYTE vector???

; the last two vectors and the jump are not included or used

LB822:	.addr	sub_b39c			; GET STATUS vector???
	.addr	sub_b7da			; SPECIAL vector???
	.addr	sub_b7da			; 

sub_b828:
	ldx     #$00    			; B828 A2 00                    ..
	ldy     #$04    			; B82A A0 04                    ..
:	txa             			; B82C 8A                       .
	and     #$0F    			; B82D 29 0F                    ).
	ora     #$10    			; B82F 09 10                    ..
	sta     WSYNC
	sta     AUDC1
	inx             			; B837 E8                       .
	inx             			; B838 E8                       .
	sta     WSYNC
	bne     :-
	dey             			; B83E 88                       .
	bne     :-
	lda     #<(LB863-1)
	ldy     #>(LB863-1)
	ldx     #$0C    			; B845 A2 0C                    ..
	jmp     sub_b84b

; ----------------------------------------------------------------------------
	rts             			; B84A 60                       `

; ----------------------------------------------------------------------------
sub_b84b:
	sta     off_EC
	sty     off_EC+1
	ldy     #$00    			; B84F A0 00                    ..
	lda     #$55    			; B851 A9 55                    .U
	sec             			; B853 38                       8
:	iny             			; B854 C8                       .
	rol     a       			; B855 2A                       *
	pha             			; B856 48                       H
	eor     (off_EC),y
	sta     L3E33-1,y
	pla             			; B85C 68                       h
	dex             			; B85D CA                       .
	bne     :-
	jmp     L3E33   			; B860 4C 33 3E                 L3>

; ----------------------------------------------------------------------------
LB863:	.byte	$65,$AA,$12,$F7,$49,$D5,$25,$A9
	.byte	$19,$DC,$B2,$CD

LB86F:	.byte	$00,$00,$00,$00
	.byte	$00,$01,$FF,$00
	.byte	$00,$01,$FF,$00
	.byte	$00,$01,$FF,$00

LB87F:	.byte	$00,$00,$00,$00
	.byte	$00,$01,$01,$01
	.byte	$00,$FF,$FF,$FF
	.byte	$00,$00,$00,$00

LB88F:	.byte	"K:",$9B			; B88F

LB892:	.byte	"R:",$9B			; B892

LB895:	.byte	"T:",$9B			; B895

LB898:	.byte	"P:",$9B			; B898

LB89B:	RString	"COMMUNICATION ERROR"

LB8AE:	RString	"COPYRIGHT 1984 ATARI"

LB8C2:	RString "WELCOME TO THE LEARNING PHONE"

LB8DF:	RString "After the phone has a high pitch tone, PRESS RETURN!"

LB913:	RString "300  baud"

LB91C:	RString "1200 baud"

LB925:	RString "Microbit 300 baud"

LB936:	.byte	$00,$40,$80,$C0

LB93A:	.byte   $7F,$BF,$DF,$EF,$F7,$FB,$FD,$FE

LB942:  .byte   $80,$40,$20,$10
	.byte	$08,$04,$02,$01
	.byte	$E3,$E1,$E0,$E2

LB94E:  .byte	$00

LB94F:  .byte   $7F,$3F,$1F,$0F,$07,$03,$01

LB956:  .byte   $80,$C0,$E0,$F0,$F8,$FC

LB95C:  .byte   $FE,$00

LB95E:  .byte   $07,$83,$C1,$E0,$F0,$F8,$FC,$FE

LB966:  .byte	$00,$00,$00,$00,$7F,$3F,$1F,$0F

;*******************************************************************************
;*                                                                             *
;*                                   LB96E                                     *
;*                                                                             *
;*                     IOCB Device/Operation Lookup Table                      *
;*                                                                             *
;*******************************************************************************

LB96E:  .byte   $10				; IOCB Channel #1
	.byte	$03				; ICCOM: open
	.byte	$0D				; ICAX1: (mode) concurrent read/write
	.byte	$00				; 
	.addr	LB892				; "R:" (RS232)
	.word	$0000				; 

LB976:	.byte	$20				; IOCB Channel #2
	.byte	$03				; ICCOM: open
	.byte	$04				; ICAX1: (mode) read
	.byte	$00				; 
	.addr	LB88F				; "K:"
	.word	$0000				; 

LB97E:	.byte	$20				; IOCB Channel #2
	.byte	$07				; ICCOM: get character
	.byte	$04				; ICAX1: (mode) read
	.byte	$00				;
	.addr	$0000				;
	.word	$0000				;

LB986:	.byte	$10				; IOCB Channel #1
	.byte	$0B				; ICCOM: put character
	.byte	$0D				; ICAX1: (mode) concurrent read/write
	.byte	$00				;
	.addr	$0000				;
	.word	$0000				;

LB98E:	.byte	$10				; IOCB Channel #1
	.byte	$07				; ICCOM: get character
	.byte	$0D				; ICAX1: (mode) concurrent read/write
	.byte	$00				; 
	.addr	$0000				; 
	.word	$0000				; 

LB996:
	.byte	$10				; IOCB Channel #1
	.byte	$03				; ICCOM: open
	.byte	$0D				; ICAX1: (mode) concurrent read/write
	.byte	$00				; 
	.addr	LB895				; "T:" (Microbit???? or Atari????)
	.word	$0000				;

LB99E:
	.byte	$30				; IOCB Channel #3
	.byte	$03				; ICCOM: open
	.byte	$08				; ICAX1: (mode) write
	.byte	$00				; ICAX2:
	.addr	LB898				; "P:" (Printer)
	.word	$0040

LB9A6:	.byte	>$0600,>$0900,>$0000,>byte_BAEC

LB9AA:	.byte	<$0600,<$0900,<$0000,<byte_BAEC

LB9AE:	.byte	<$0C00,<$0D80,<charset_sm,<LBE84

LB9B2:  .byte   >$0C00,>$0D80,>charset_sm,>LBE84

LB9B6:	.byte	$08,$10,$10,$20,$20,$40,$80,$80

LB9BE:	.byte	$00,$00,$00,$01,$01,$01,$02,$02
	.byte	$03,$03,$04,$04,$04,$05,$05,$05

LB9CE:	.byte	$00,$00,$01,$02,$02,$03,$03,$04

LB9D6:  .byte   $03,$02,$03,$03,$02,$03,$02,$03
	.byte	$03,$02,$02,$01,$02,$02,$01,$02
	.byte	$01,$02,$02,$01,$03,$02,$03,$03
	.byte	$02,$03,$02,$03,$03,$02

LB9F4:  .byte   $00,$05,$0A,$0F,$14,$19

LB9FA:  .byte   $00,$00,$01,$01,$01,$01,$01,$02

LBA02:	.byte	$00,$03,$0C,$0F,$30,$33,$3C,$3F
	.byte	$C0,$C3,$CC,$CF,$F0,$F3,$FC,$FF

; Keyboard code lookup
LBA12:	.byte	$06,$23,$7E,$26,$60,$27,$07,$26
	.byte	$40,$36,$5E,$5C,$3E,$01,$11,$0F
	.byte	$04,$05,$0E,$13,$17,$37,$7D,$00
	.byte	$12 ; 18 = 'C'
	.byte	$03,$16,$2A,$1A,$18,$3F,$07
	.byte	$07,$2D,$14,$14,$39,$0B,$09,$3A

LBA3A:  .byte   $12

LBA3B:  .byte   $1D,$00

LBA3D:  .byte   $0C,$0F,$15

LBA40:  .byte   $02,$0E,$23

LBA43:  .byte   $0D,$1E

LBA45:  .byte   $61

LBA46:  .byte	$1F,$74,$19,$34,$08,$73,$7C,$2C
	.byte	$0A,$4C,$1E,$0C,$0D

LBA53:  .byte   $46

LBA54:  .byte   $2F,$47,$78,$4F,$49,$5A,$24,$5B
	.byte	$2B,$75,$35,$02,$BB,$5A,$30,$5F
	.byte	$EE,$3D,$A8

LBA67:	.byte	$70,$40,$70,$40,$40,$06,$0F,$06

.macro	jt1	arg1
	.byte	(arg1-sub_a367)
.endmacro

LBA6F:
	.repeat 8
		jt1	sub_a3dd
	.endrepeat
	jt1	sub_a3de
	jt1	sub_a3af
	jt1	sub_a3c2
	jt1	sub_a40a
	jt1	sub_a3c8
	jt1	sub_a3b2
	.repeat 11
		jt1	sub_a3dd
	.endrepeat
	jt1	sub_a36a
	jt1	sub_a3dd
	jt1	sub_a42b
	jt1	sub_a36a
	jt1	sub_a36a
	jt1	sub_a3dd
	jt1	sub_a36a
	.byte	$76,$20,$1C,$20,$76,$76,$76,$76
	.byte	$76,$76,$76,$76,$00,$76,$76,$76
	.byte	$76,$13,$13,$13,$13,$76,$76,$76
	.byte	$76,$76,$76,$C4,$76,$76,$76,$76
	.byte	$2F,$33,$A9,$A9,$A9,$A9,$76,$76
	.byte	$76,$76,$76,$76,$76,$76,$26,$2A
	jt1	sub_a36a
	.byte	$D5,$CA,$CA,$CA,$D1,$CA,$DD
	.byte	$76,$D9,$37

LBACA:	.byte	>(sub_a6f6-1)
	.byte	>(sub_a448-1),0,0
	.byte	>(sub_a682-1)
	.byte	>(sub_adf1-1),0
	.byte   >(sub_ab5a-1)

LBAD2:	.byte	<(sub_a6f6-1)
	.byte	<(sub_a448-1),0,0
	.byte	<(sub_a682-1)
	.byte	<(sub_adf1-1),0
	.byte	<(sub_ab5a-1)

LBADA:	.byte	$03,$80,$00,$00,$80,$80,$00,$01

LBAE2:	.byte	>(sub_a8b3-1)
	.byte	>(sub_a5b5-1)
	.byte	>(sub_a5ff-1)
	.byte	>(sub_a640-1)
	.byte	>(sub_a133-1)

LBAE7:	.byte   <(sub_a8b3-1)
	.byte	<(sub_a5b5-1)
	.byte	<(sub_a5ff-1)
	.byte	<(sub_a640-1)
	.byte	<(sub_a133-1)

byte_BAEC:
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %00000010       		; ......#.
	.byte   %00000100       		; .....#..
	.byte   %00001000       		; ....#...
	.byte   %00010000       		; ...#....
	.byte   %00100000       		; ..#.....
	.byte   %01000000       		; .#......
	.byte   %10000000       		; #.......

	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %01111100       		; .#####..
	.byte   %00000000       		; ........
	.byte   %01111100       		; .#####..
	.byte   %00000000       		; ........
	.byte   %01111100       		; .#####..
	.byte   %00000000       		; ........

	.byte   %00110010       		; ..##..#.
	.byte   %01001100       		; .#..##..
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %00010000       		; ...#....
	.byte   %00110000       		; ..##....
	.byte   %01011110       		; .#.####.
	.byte   %10000000       		; #.......
	.byte   %01011110       		; .#.####.
	.byte   %00110000       		; ..##....
	.byte   %00010000       		; ...#....

	.byte   %00000000       		; ........
	.byte   %00000010       		; ......#.
	.byte   %00000100       		; .....#..
	.byte   %11111110       		; #######.
	.byte   %00010000       		; ...#....
	.byte   %11111110       		; #######.
	.byte   %01000000       		; .#......
	.byte   %10000000       		; #.......

	.byte   %00000000       		; ........
	.byte   %00010000       		; ...#....
	.byte   %00111000       		; ..###...
	.byte   %01111100       		; .#####..
	.byte   %00010000       		; ...#....
	.byte   %00010000       		; ...#....
	.byte   %00010000       		; ...#....
	.byte   %00010000       		; ...#....

	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00001000       		; ....#...
	.byte   %00001100       		; ....##..
	.byte   %11111110       		; #######.
	.byte   %00001100       		; ....##..
	.byte   %00001000       		; ....#...
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %00010000       		; ...#....
	.byte   %00010000       		; ...#....
	.byte   %00010000       		; ...#....
	.byte   %00010000       		; ...#....
	.byte   %01111100       		; .#####..
	.byte   %00111000       		; ..###...
	.byte   %00010000       		; ...#....

	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00100000       		; ..#.....
	.byte   %01100000       		; .##.....
	.byte   %11111110       		; #######.
	.byte   %01100000       		; .##.....
	.byte   %00100000       		; ..#.....
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00100100       		; ..#..#..
	.byte   %00011000       		; ...##...
	.byte   %00011000       		; ...##...
	.byte   %00100100       		; ..#..#..
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........

	.byte   %11111110       		; #######.
	.byte   %01000000       		; .#......
	.byte   %00100000       		; ..#.....
	.byte   %00010000       		; ...#....
	.byte   %00010000       		; ...#....
	.byte   %00100000       		; ..#.....
	.byte   %01000000       		; .#......
	.byte   %11111110       		; #######.

	.byte   %00010000       		; ...#....
	.byte   %00010000       		; ...#....
	.byte   %00101000       		; ..#.#...
	.byte   %00101000       		; ..#.#...
	.byte   %01000100       		; .#...#..
	.byte   %01000100       		; .#...#..
	.byte   %10000010       		; #.....#.
	.byte   %11111110       		; #######.

	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %01000100       		; .#...#..
	.byte   %01000100       		; .#...#..
	.byte   %01000100       		; .#...#..
	.byte   %00111000       		; ..###...
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00111000       		; ..###...
	.byte   %01000100       		; .#...#..
	.byte   %01000100       		; .#...#..
	.byte   %01000100       		; .#...#..
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00010000       		; ...#....
	.byte   %00000000       		; ........
	.byte   %01111100       		; .#####..
	.byte   %00000000       		; ........
	.byte   %00010000       		; ...#....
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %01100010       		; .##...#.
	.byte   %10010010       		; #..#..#.
	.byte   %10010100       		; #..#.#..
	.byte   %10001000       		; #...#...
	.byte   %10011000       		; #..##...
	.byte   %01100110       		; .##..##.

	.byte   %00011000       		; ...##...
	.byte   %00100100       		; ..#..#..
	.byte   %01000100       		; .#...#..
	.byte   %01111000       		; .####...
	.byte   %01000100       		; .#...#..
	.byte   %01000010       		; .#....#.
	.byte   %01111100       		; .#####..
	.byte   %01000000       		; .#......

	.byte   %00110000       		; ..##....
	.byte   %01001000       		; .#..#...
	.byte   %00100000       		; ..#.....
	.byte   %00110000       		; ..##....
	.byte   %01001000       		; .#..#...
	.byte   %01000100       		; .#...#..
	.byte   %01000100       		; .#...#..
	.byte   %00111000       		; ..###...

	.byte   %01000000       		; .#......
	.byte   %00100000       		; ..#.....
	.byte   %00010000       		; ...#....
	.byte   %00010000       		; ...#....
	.byte   %00101000       		; ..#.#...
	.byte   %00101000       		; ..#.#...
	.byte   %01000100       		; .#...#..
	.byte   %01000100       		; .#...#..

	.byte   %00000000       		; ........
	.byte   %00100100       		; ..#..#..
	.byte   %00100100       		; ..#..#..
	.byte   %00100100       		; ..#..#..
	.byte   %00111010       		; ..###.#.
	.byte   %00100000       		; ..#.....
	.byte   %00100000       		; ..#.....
	.byte   %11000000       		; ##......

	.byte   %00000000       		; ........
	.byte   %00100000       		; ..#.....
	.byte   %01111100       		; .#####..
	.byte   %10101000       		; #.#.#...
	.byte   %00101000       		; ..#.#...
	.byte   %00101000       		; ..#.#...
	.byte   %00101000       		; ..#.#...
	.byte   %00101000       		; ..#.#...

	.byte   %00000000       		; ........
	.byte   %00001100       		; ....##..
	.byte   %00010010       		; ...#..#.
	.byte   %00100010       		; ..#...#.
	.byte   %01100100       		; .##..#..
	.byte   %01011000       		; .#.##...
	.byte   %01000000       		; .#......
	.byte   %10000000       		; #.......

	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00111110       		; ..#####.
	.byte   %01010000       		; .#.#....
	.byte   %10001000       		; #...#...
	.byte   %10001000       		; #...#...
	.byte   %10001000       		; #...#...
	.byte   %01110000       		; .###....

	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %01000100       		; .#...#..
	.byte   %10000010       		; #.....#.
	.byte   %10010010       		; #..#..#.
	.byte   %10010010       		; #..#..#.
	.byte   %10010010       		; #..#..#.
	.byte   %01101100       		; .##.##..

	.byte   %00001100       		; ....##..
	.byte   %00110000       		; ..##....
	.byte   %11000000       		; ##......
	.byte   %00110000       		; ..##....
	.byte   %00001100       		; ....##..
	.byte   %00000000       		; ........
	.byte   %11111100       		; ######..
	.byte   %00000000       		; ........

	.byte   %01100000       		; .##.....
	.byte   %00011000       		; ...##...
	.byte   %01100000       		; .##.....
	.byte   %00011000       		; ...##...
	.byte   %01100000       		; .##.....
	.byte   %00000000       		; ........
	.byte   %01111110       		; .######.
	.byte   %00000000       		; ........

	.byte   %00111000       		; ..###...
	.byte   %01000100       		; .#...#..
	.byte   %10000010       		; #.....#.
	.byte   %11111110       		; #######.
	.byte   %10000010       		; #.....#.
	.byte   %10000010       		; #.....#.
	.byte   %01000100       		; .#...#..
	.byte   %00111000       		; ..###...

	.byte   %01000000       		; .#......
	.byte   %10000000       		; #.......
	.byte   %00011000       		; ...##...
	.byte   %00101000       		; ..#.#...
	.byte   %01001000       		; .#..#...
	.byte   %00101000       		; ..#.#...
	.byte   %00011000       		; ...##...
	.byte   %00001100       		; ....##..

	.byte   %00110000       		; ..##....
	.byte   %01001000       		; .#..#...
	.byte   %01001000       		; .#..#...
	.byte   %00110000       		; ..##....
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........

	.byte   %01000000       		; .#......
	.byte   %00100000       		; ..#.....
	.byte   %00110000       		; ..##....
	.byte   %00101000       		; ..#.#...
	.byte   %00100100       		; ..#..#..
	.byte   %00101000       		; ..#.#...
	.byte   %00110000       		; ..##....
	.byte   %01100000       		; .##.....

	.byte   %11100000       		; ###.....
	.byte   %10010000       		; #..#....
	.byte   %01001000       		; .#..#...
	.byte   %00100100       		; ..#..#..
	.byte   %00100100       		; ..#..#..
	.byte   %01001000       		; .#..#...
	.byte   %10010000       		; #..#....
	.byte   %11100000       		; ###.....

	.byte   %11111110       		; #######.
	.byte   %00000000       		; ........
	.byte   %00111100       		; ..####..
	.byte   %01000010       		; .#....#.
	.byte   %10000000       		; #.......
	.byte   %00111100       		; ..####..
	.byte   %00000000       		; ........
	.byte   %11111110       		; #######.

	.byte   %00101000       		; ..#.#...
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %01111100       		; .#####..
	.byte   %01000100       		; .#...#..
	.byte   %01000100       		; .#...#..
	.byte   %01111100       		; .#####..
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00110000       		; ..##....
	.byte   %01001000       		; .#..#...
	.byte   %01001000       		; .#..#...
	.byte   %00110000       		; ..##....
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %00010000       		; ...#....
	.byte   %00111000       		; ..###...
	.byte   %01111100       		; .#####..
	.byte   %11111110       		; #######.
	.byte   %01111100       		; .#####..
	.byte   %00111000       		; ..###...
	.byte   %00010000       		; ...#....

	.byte   %00000000       		; ........
	.byte   %10000010       		; #.....#.
	.byte   %01000100       		; .#...#..
	.byte   %00101000       		; ..#.#...
	.byte   %00010000       		; ...#....
	.byte   %00101000       		; ..#.#...
	.byte   %01000100       		; .#...#..
	.byte   %10000010       		; #.....#.

	.byte   %00000100       		; .....#..
	.byte   %00001000       		; ....#...
	.byte   %00010000       		; ...#....
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000100       		; .....#..
	.byte   %00001000       		; ....#...
	.byte   %00010000       		; ...#....

	.byte   %01000100       		; .#...#..
	.byte   %00101000       		; ..#.#...
	.byte   %00010000       		; ...#....
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %00010000       		; ...#....
	.byte   %00111000       		; ..###...
	.byte   %01111100       		; .#####..
	.byte   %00010000       		; ...#....
	.byte   %01111100       		; .#####..
	.byte   %00111000       		; ..###...
	.byte   %00010000       		; ...#....

	.byte   %00010000       		; ...#....
	.byte   %00010000       		; ...#....
	.byte   %00010000       		; ...#....
	.byte   %00010000       		; ...#....
	.byte   %00010000       		; ...#....
	.byte   %00010000       		; ...#....
	.byte   %00010000       		; ...#....
	.byte   %00010000       		; ...#....

charset_sm:
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........

	.byte   %01000000       		; .#......
	.byte   %01000000       		; .#......
	.byte   %01000000       		; .#......
	.byte   %00000000       		; ........
	.byte   %01000000       		; .#......
	.byte   %00000000       		; ........

	.byte   %01010000       		; .#.#....
	.byte   %01010000       		; .#.#....
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........

	.byte   %01010000       		; .#.#....
	.byte   %11111000       		; #####...
	.byte   %01010000       		; .#.#....
	.byte   %11111000       		; #####...
	.byte   %01010000       		; .#.#....
	.byte   %00000000       		; ........

	.byte   %01000000       		; .#......
	.byte   %11110000       		; ####....
	.byte   %11000000       		; ##......
	.byte   %00110000       		; ..##....
	.byte   %11110000       		; ####....
	.byte   %00100000       		; ..#.....

	.byte   %10010000       		; #..#....
	.byte   %00100000       		; ..#.....
	.byte   %00100000       		; ..#.....
	.byte   %01000000       		; .#......
	.byte   %10010000       		; #..#....
	.byte   %00000000       		; ........

	.byte   %01000000       		; .#......
	.byte   %10100000       		; #.#.....
	.byte   %01000000       		; .#......
	.byte   %10110000       		; #.##....
	.byte   %01010000       		; .#.#....
	.byte   %00000000       		; ........

	.byte   %00100000       		; ..#.....
	.byte   %01000000       		; .#......
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........

	.byte   %00100000       		; ..#.....
	.byte   %01000000       		; .#......
	.byte   %01000000       		; .#......
	.byte   %01000000       		; .#......
	.byte   %00100000       		; ..#.....
	.byte   %00000000       		; ........

	.byte   %01000000       		; .#......
	.byte   %00100000       		; ..#.....
	.byte   %00100000       		; ..#.....
	.byte   %00100000       		; ..#.....
	.byte   %01000000       		; .#......
	.byte   %00000000       		; ........

	.byte   %10010000       		; #..#....
	.byte   %01100000       		; .##.....
	.byte   %11110000       		; ####....
	.byte   %01100000       		; .##.....
	.byte   %10010000       		; #..#....
	.byte   %00000000       		; ........

	.byte   %00100000       		; ..#.....
	.byte   %00100000       		; ..#.....
	.byte   %11111000       		; #####...
	.byte   %00100000       		; ..#.....
	.byte   %00100000       		; ..#.....
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %01000000       		; .#......
	.byte   %01000000       		; .#......
	.byte   %10000000       		; #.......
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %11110000       		; ####....
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %01000000       		; .#......
	.byte   %01000000       		; .#......
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %00010000       		; ...#....
	.byte   %00100000       		; ..#.....
	.byte   %01000000       		; .#......
	.byte   %10000000       		; #.......
	.byte   %00000000       		; ........

	.byte   %01100000       		; .##.....
	.byte   %10010000       		; #..#....
	.byte   %10010000       		; #..#....
	.byte   %10010000       		; #..#....
	.byte   %01100000       		; .##.....
	.byte   %00000000       		; ........

	.byte   %00100000       		; ..#.....
	.byte   %01100000       		; .##.....
	.byte   %00100000       		; ..#.....
	.byte   %00100000       		; ..#.....
	.byte   %01110000       		; .###....
	.byte   %00000000       		; ........

	.byte   %01100000       		; .##.....
	.byte   %10010000       		; #..#....
	.byte   %00100000       		; ..#.....
	.byte   %01000000       		; .#......
	.byte   %11110000       		; ####....
	.byte   %00000000       		; ........

	.byte   %11110000       		; ####....
	.byte   %00010000       		; ...#....
	.byte   %00100000       		; ..#.....
	.byte   %00010000       		; ...#....
	.byte   %11100000       		; ###.....
	.byte   %00000000       		; ........

	.byte   %00100000       		; ..#.....
	.byte   %01100000       		; .##.....
	.byte   %10100000       		; #.#.....
	.byte   %11110000       		; ####....
	.byte   %00100000       		; ..#.....
	.byte   %00000000       		; ........

	.byte   %11100000       		; ###.....
	.byte   %10000000       		; #.......
	.byte   %11100000       		; ###.....
	.byte   %00010000       		; ...#....
	.byte   %11100000       		; ###.....
	.byte   %00000000       		; ........

	.byte   %01100000       		; .##.....
	.byte   %10000000       		; #.......
	.byte   %11100000       		; ###.....
	.byte   %10010000       		; #..#....
	.byte   %11100000       		; ###.....
	.byte   %00000000       		; ........

	.byte   %11110000       		; ####....
	.byte   %00010000       		; ...#....
	.byte   %00100000       		; ..#.....
	.byte   %01000000       		; .#......
	.byte   %01000000       		; .#......
	.byte   %00000000       		; ........

	.byte   %01100000       		; .##.....
	.byte   %10010000       		; #..#....
	.byte   %01100000       		; .##.....
	.byte   %10010000       		; #..#....
	.byte   %01100000       		; .##.....
	.byte   %00000000       		; ........

	.byte   %01100000       		; .##.....
	.byte   %10010000       		; #..#....
	.byte   %01110000       		; .###....
	.byte   %00010000       		; ...#....
	.byte   %11100000       		; ###.....
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %01000000       		; .#......
	.byte   %00000000       		; ........
	.byte   %01000000       		; .#......
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %01000000       		; .#......
	.byte   %00000000       		; ........
	.byte   %01000000       		; .#......
	.byte   %10000000       		; #.......
	.byte   %00000000       		; ........

	.byte   %00100000       		; ..#.....
	.byte   %01000000       		; .#......
	.byte   %10000000       		; #.......
	.byte   %01000000       		; .#......
	.byte   %00100000       		; ..#.....
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %11110000       		; ####....
	.byte   %00000000       		; ........
	.byte   %11110000       		; ####....
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........

	.byte   %01000000       		; .#......
	.byte   %00100000       		; ..#.....
	.byte   %00010000       		; ...#....
	.byte   %00100000       		; ..#.....
	.byte   %01000000       		; .#......
	.byte   %00000000       		; ........

	.byte   %01100000       		; .##.....
	.byte   %10010000       		; #..#....
	.byte   %00100000       		; ..#.....
	.byte   %00000000       		; ........
	.byte   %01000000       		; .#......
	.byte   %00000000       		; ........

	.byte   %01100000       		; .##.....
	.byte   %10010000       		; #..#....
	.byte   %10110000       		; #.##....
	.byte   %10000000       		; #.......
	.byte   %01100000       		; .##.....
	.byte   %00000000       		; ........

	.byte   %01100000       		; .##.....
	.byte   %10010000       		; #..#....
	.byte   %11110000       		; ####....
	.byte   %10010000       		; #..#....
	.byte   %10010000       		; #..#....
	.byte   %00000000       		; ........

	.byte   %11100000       		; ###.....
	.byte   %10010000       		; #..#....
	.byte   %11100000       		; ###.....
	.byte   %10010000       		; #..#....
	.byte   %11100000       		; ###.....
	.byte   %00000000       		; ........

	.byte   %01110000       		; .###....
	.byte   %10000000       		; #.......
	.byte   %10000000       		; #.......
	.byte   %10000000       		; #.......
	.byte   %01110000       		; .###....
	.byte   %00000000       		; ........

	.byte   %11100000       		; ###.....
	.byte   %10010000       		; #..#....
	.byte   %10010000       		; #..#....
	.byte   %10010000       		; #..#....
	.byte   %11100000       		; ###.....
	.byte   %00000000       		; ........

	.byte   %11110000       		; ####....
	.byte   %10000000       		; #.......
	.byte   %11100000       		; ###.....
	.byte   %10000000       		; #.......
	.byte   %11110000       		; ####....
	.byte   %00000000       		; ........

	.byte   %11110000       		; ####....
	.byte   %10000000       		; #.......
	.byte   %11100000       		; ###.....
	.byte   %10000000       		; #.......
      	.byte   %10000000       		; #.......
	.byte   %00000000       		; ........

	.byte   %01100000       		; .##.....
	.byte   %10000000       		; #.......
	.byte   %10010000       		; #..#....
	.byte   %10010000       		; #..#....
	.byte   %01110000       		; .###....
	.byte   %00000000       		; ........

	.byte   %10010000       		; #..#....
	.byte   %10010000       		; #..#....
	.byte   %11110000       		; ####....
	.byte   %10010000       		; #..#....
	.byte   %10010000       		; #..#....
	.byte   %00000000       		; ........

	.byte   %11100000       		; ###.....
	.byte   %01000000       		; .#......
	.byte   %01000000       		; .#......
	.byte   %01000000       		; .#......
	.byte   %11100000       		; ###.....
	.byte   %00000000       		; ........

	.byte   %00010000       		; ...#....
	.byte   %00010000       		; ...#....
	.byte   %00010000       		; ...#....
	.byte   %10010000       		; #..#....
	.byte   %01100000       		; .##.....
	.byte   %00000000       		; ........

	.byte   %10010000       		; #..#....
	.byte   %10100000       		; #.#.....
	.byte   %11000000       		; ##......
	.byte   %10100000       		; #.#.....
	.byte   %10010000       		; #..#....
	.byte   %00000000       		; ........

	.byte   %10000000       		; #.......
	.byte   %10000000       		; #.......
	.byte   %10000000       		; #.......
	.byte   %10000000       		; #.......
	.byte   %11110000       		; ####....
	.byte   %00000000       		; ........

	.byte   %10001000       		; #...#...
	.byte   %11011000       		; ##.##...
	.byte   %10101000       		; #.#.#...
	.byte   %10001000       		; #...#...
	.byte   %10001000       		; #...#...
	.byte   %00000000       		; ........

	.byte   %10010000       		; #..#....
	.byte   %11010000       		; ##.#....
	.byte   %10110000       		; #.##....
	.byte   %10010000       		; #..#....
	.byte   %10010000       		; #..#....
	.byte   %00000000       		; ........

	.byte   %11110000       		; ####....
	.byte   %10010000       		; #..#....
	.byte   %10010000       		; #..#....
	.byte   %10010000       		; #..#....
	.byte   %11110000       		; ####....
	.byte   %00000000       		; ........

	.byte   %11100000       		; ###.....
	.byte   %10010000       		; #..#....
	.byte   %11100000       		; ###.....
	.byte   %10000000       		; #.......
	.byte   %10000000       		; #.......
	.byte   %00000000       		; ........

	.byte   %11110000       		; ####....
	.byte   %10010000       		; #..#....
	.byte   %10010000       		; #..#....
	.byte   %10110000       		; #.##....
	.byte   %11111000       		; #####...
	.byte   %00000000       		; ........

	.byte   %11100000       		; ###.....
	.byte   %10010000       		; #..#....
	.byte   %11100000       		; ###.....
	.byte   %10100000       		; #.#.....
	.byte   %10010000       		; #..#....
	.byte   %00000000       		; ........

	.byte   %01110000       		; .###....
	.byte   %10000000       		; #.......
	.byte   %11100000       		; ###.....
	.byte   %00010000       		; ...#....
	.byte   %11100000       		; ###.....
	.byte   %00000000       		; ........

	.byte   %11111000       		; #####...
	.byte   %00100000       		; ..#.....
	.byte   %00100000       		; ..#.....
	.byte   %00100000       		; ..#.....
	.byte   %00100000       		; ..#.....
	.byte   %00000000       		; ........

	.byte   %10010000       		; #..#....
	.byte   %10010000       		; #..#....
	.byte   %10010000       		; #..#....
	.byte   %10010000       		; #..#....
	.byte   %11110000       		; ####....
	.byte   %00000000       		; ........

	.byte   %10010000       		; #..#....
	.byte   %10010000       		; #..#....
	.byte   %10010000       		; #..#....
	.byte   %01100000       		; .##.....
	.byte   %01100000       		; .##.....
	.byte   %00000000       		; ........

	.byte   %10001000       		; #...#...
	.byte   %10001000       		; #...#...
	.byte   %10101000       		; #.#.#...
	.byte   %11011000       		; ##.##...
	.byte   %10001000       		; #...#...
	.byte   %00000000       		; ........

	.byte   %10010000       		; #..#....
	.byte   %10010000       		; #..#....
	.byte   %01100000       		; .##.....
	.byte   %10010000       		; #..#....
	.byte   %10010000       		; #..#....
	.byte   %00000000       		; ........

	.byte   %10010000       		; #..#....
	.byte   %10010000       		; #..#....
	.byte   %11100000       		; ###.....
	.byte   %01000000       		; .#......
	.byte   %01000000       		; .#......
	.byte   %00000000       		; ........

	.byte   %11110000       		; ####....
	.byte   %00010000       		; ...#....
	.byte   %00100000       		; ..#.....
	.byte   %01000000       		; .#......
	.byte   %11110000       		; ####....
	.byte   %00000000       		; ........

	.byte   %01110000       		; .###....
	.byte   %01000000       		; .#......
	.byte   %01000000       		; .#......
	.byte   %01000000       		; .#......
	.byte   %01110000       		; .###....
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %10000000       		; #.......
	.byte   %01000000       		; .#......
	.byte   %00100000       		; ..#.....
	.byte   %00010000       		; ...#....
	.byte   %00000000       		; ........

	.byte   %01110000       		; .###....
	.byte   %00010000       		; ...#....
	.byte   %00010000       		; ...#....
	.byte   %00010000       		; ...#....
	.byte   %01110000       		; .###....
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %00100000       		; ..#.....
	.byte   %01010000       		; .#.#....
	.byte   %10001000       		; #...#...
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %11110000       		; ####....

	.byte   %01000000       		; .#......
	.byte   %00100000       		; ..#.....
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %11000000       		; ##......
	.byte   %00100000       		; ..#.....
	.byte   %10100000       		; #.#.....
	.byte   %11110000       		; ####....
	.byte   %00000000       		; ........

	.byte   %10000000       		; #.......
	.byte   %11100000       		; ###.....
	.byte   %10010000       		; #..#....
	.byte   %10010000       		; #..#....
	.byte   %11100000       		; ###.....
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %01100000       		; .##.....
	.byte   %10000000       		; #.......
	.byte   %10000000       		; #.......
	.byte   %01100000       		; .##.....
	.byte   %00000000       		; ........

	.byte   %00010000       		; ...#....
	.byte   %01110000       		; .###....
	.byte   %10010000       		; #..#....
	.byte   %10010000       		; #..#....
	.byte   %01110000       		; .###....
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %01100000       		; .##.....
	.byte   %10110000       		; #.##....
	.byte   %10000000       		; #.......
	.byte   %01100000       		; .##.....
	.byte   %00000000       		; ........

	.byte   %01100000       		; .##.....
	.byte   %10000000       		; #.......
	.byte   %11100000       		; ###.....
	.byte   %10000000       		; #.......
	.byte   %10000000       		; #.......
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %01100000       		; .##.....
	.byte   %10010000       		; #..#....
	.byte   %11110000       		; ####....
	.byte   %00010000       		; ...#....
	.byte   %01100000       		; .##.....

	.byte   %10000000       		; #.......
	.byte   %11100000       		; ###.....
	.byte   %10010000       		; #..#....
	.byte   %10010000       		; #..#....
	.byte   %10010000       		; #..#....
	.byte   %00000000       		; ........

	.byte   %01000000       		; .#......
	.byte   %00000000       		; ........
	.byte   %01000000       		; .#......
	.byte   %01000000       		; .#......
	.byte   %01000000       		; .#......
	.byte   %00000000       		; ........

	.byte   %00100000       		; ..#.....
	.byte   %00000000       		; ........
	.byte   %00100000       		; ..#.....
	.byte   %00100000       		; ..#.....
	.byte   %00100000       		; ..#.....
	.byte   %11000000       		; ##......

	.byte   %10000000       		; #.......
	.byte   %10100000       		; #.#.....
	.byte   %11000000       		; ##......
	.byte   %10100000       		; #.#.....
	.byte   %10010000       		; #..#....
	.byte   %00000000       		; ........

	.byte   %01000000       		; .#......
	.byte   %01000000       		; .#......
	.byte   %01000000       		; .#......
	.byte   %01000000       		; .#......
	.byte   %01000000       		; .#......
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %10010000       		; #..#....
	.byte   %11110000       		; ####....
	.byte   %10010000       		; #..#....
	.byte   %10010000       		; #..#....
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %11100000       		; ###.....
	.byte   %10010000       		; #..#....
	.byte   %10010000       		; #..#....
	.byte   %10010000       		; #..#....
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %01100000       		; .##.....
	.byte   %10010000       		; #..#....
	.byte   %10010000       		; #..#....
	.byte   %01100000       		; .##.....
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %11100000       		; ###.....
	.byte   %10010000       		; #..#....
	.byte   %10010000       		; #..#....
	.byte   %11100000       		; ###.....
	.byte   %10000000       		; #.......

	.byte   %00000000       		; ........
	.byte   %01110000       		; .###....
	.byte   %10010000       		; #..#....
	.byte   %10010000       		; #..#....
	.byte   %01110000       		; .###....
	.byte   %00010000       		; ...#....

	.byte   %00000000       		; ........
	.byte   %01110000       		; .###....
	.byte   %01000000       		; .#......
	.byte   %01000000       		; .#......
	.byte   %01000000       		; .#......
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %01110000       		; .###....
	.byte   %11000000       		; ##......
	.byte   %00110000       		; ..##....
	.byte   %11100000       		; ###.....
	.byte   %00000000       		; ........

	.byte   %01000000       		; .#......
	.byte   %11100000       		; ###.....
	.byte   %01000000       		; .#......
	.byte   %01000000       		; .#......
	.byte   %01100000       		; .##.....
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %10010000       		; #..#....
	.byte   %10010000       		; #..#....
	.byte   %10010000       		; #..#....
	.byte   %01110000       		; .###....
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %10010000       		; #..#....
	.byte   %10100000       		; #.#.....
	.byte   %10100000       		; #.#.....
	.byte   %01000000       		; .#......
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %10010000       		; #..#....
	.byte   %10010000       		; #..#....
	.byte   %11110000       		; ####....
	.byte   %10010000       		; #..#....
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %10010000       		; #..#....
	.byte   %01100000       		; .##.....
	.byte   %01100000       		; .##.....
	.byte   %10010000       		; #..#....
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %10010000       		; #..#....
	.byte   %10010000       		; #..#....
	.byte   %01110000       		; .###....
	.byte   %00010000       		; ...#....
	.byte   %01100000       		; .##.....

	.byte   %00000000       		; ........
	.byte   %11110000       		; ####....
	.byte   %00100000       		; ..#.....
	.byte   %01000000       		; .#......
	.byte   %11110000       		; ####....
	.byte   %00000000       		; ........

	.byte   %01100000       		; .##.....
	.byte   %01000000       		; .#......
	.byte   %10000000       		; #.......
	.byte   %01000000       		; .#......
	.byte   %01100000       		; .##.....
	.byte   %00000000       		; ........

	.byte   %01000000       		; .#......
	.byte   %01000000       		; .#......
	.byte   %00000000       		; ........
	.byte   %01000000       		; .#......
	.byte   %01000000       		; .#......
	.byte   %00000000       		; ........

	.byte   %11000000       		; ##......
	.byte   %01000000       		; .#......
	.byte   %00100000       		; ..#.....
	.byte   %01000000       		; .#......
	.byte   %11000000       		; ##......
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte	%01101000			; .##.#...
	.byte	%10110000			; #.##....
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........

LBE84:
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %00010000       		; ...#....
	.byte   %00100000       		; ..#.....
	.byte   %01000000       		; .#......
	.byte   %10000000       		; #.......
	.byte   %10000000       		; #.......

	.byte   %00000000       		; ........
	.byte   %11100000       		; ###.....
	.byte   %00000000       		; ........
	.byte   %11100000       		; ###.....
	.byte   %00000000       		; ........
	.byte   %11100000       		; ###.....

	.byte   %01010000       		; .#.#....
	.byte   %10100000       		; #.#.....
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........

	.byte   %00100000       		; ..#.....
	.byte   %01110000       		; .###....
	.byte   %10000000       		; #.......
	.byte   %01110000       		; .###....
	.byte   %00100000       		; ..#.....
	.byte   %00000000       		; ........

	.byte   %00100000       		; ..#.....
	.byte   %11110000       		; ####....
	.byte   %00100000       		; ..#.....
	.byte   %11110000       		; ####....
	.byte   %01000000       		; .#......
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %00100000       		; ..#.....
	.byte   %01110000       		; .###....
	.byte   %00100000       		; ..#.....
	.byte   %00100000       		; ..#.....
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00100000       		; ..#.....
	.byte   %11110000       		; ####....
	.byte   %00100000       		; ..#.....
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %00100000       		; ..#.....
	.byte   %00100000       		; ..#.....
	.byte   %01110000       		; .###....
	.byte   %00100000       		; ..#.....
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %01000000       		; .#......
	.byte   %11110000       		; ####....
	.byte   %01000000       		; .#......
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %01010000       		; .#.#....
	.byte   %00100000       		; ..#.....
	.byte   %01010000       		; .#.#....
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........

	.byte   %11110000       		; ####....
	.byte   %10000000       		; #.......
	.byte   %01100000       		; .##.....
	.byte   %10000000       		; #.......
	.byte   %11110000       		; ####....
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %00100000       		; ..#.....
	.byte   %01010000       		; .#.#....
	.byte   %10001000       		; #...#...
	.byte   %11111000       		; #####...
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %10010000       		; #..#....
	.byte   %10010000       		; #..#....
	.byte   %01100000       		; .##.....
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %01100000       		; .##.....
	.byte   %10010000       		; #..#....
	.byte   %10010000       		; #..#....
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........

	.byte   %01100000       		; .##.....
	.byte   %00000000       		; ........
	.byte   %11110000       		; ####....
	.byte   %00000000       		; ........
	.byte   %01100000       		; .##.....
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %01010000       		; .#.#....
	.byte   %10100000       		; #.#.....
	.byte   %01110000       		; .###....
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........

	.byte   %00100000       		; ..#.....
	.byte   %01010000       		; .#.#....
	.byte   %10100000       		; #.#.....
	.byte   %10010000       		; #..#....
	.byte   %11100000       		; ###.....
	.byte   %10000000       		; #.......

	.byte   %00100000       		; ..#.....
	.byte   %01000000       		; .#......
	.byte   %00100000       		; ..#.....
	.byte   %01010000       		; .#.#....
	.byte   %01110000       		; .###....
	.byte   %00000000       		; ........

	.byte   %10000000       		; #.......
	.byte   %01000000       		; .#......
	.byte   %00100000       		; ..#.....
	.byte   %01010000       		; .#.#....
	.byte   %10010000       		; #..#....
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %01010000       		; .#.#....
	.byte   %01010000       		; .#.#....
	.byte   %01100000       		; .##.....
	.byte   %10000000       		; #.......

	.byte   %00001000       		; ....#...
	.byte   %01111000       		; .####...
	.byte   %11010000       		; ##.#....
	.byte   %01010000       		; .#.#....
	.byte   %01010000       		; .#.#....
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %00100000       		; ..#.....
	.byte   %01010000       		; .#.#....
	.byte   %01010000       		; .#.#....
	.byte   %10100000       		; #.#.....
	.byte   %10000000       		; #.......

	.byte   %00110000       		; ..##....
	.byte   %01000000       		; .#......
	.byte   %10100000       		; #.#.....
	.byte   %10100000       		; #.#.....
	.byte   %01000000       		; .#......
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %10010000       		; #..#....
	.byte   %10110000       		; #.##....
	.byte   %01100000       		; .##.....
	.byte   %00000000       		; ........

	.byte   %00100000       		; ..#.....
	.byte   %01000000       		; .#......
	.byte   %10000000       		; #.......
	.byte   %01000000       		; .#......
	.byte   %00100000       		; ..#.....
	.byte   %11100000       		; ###.....

	.byte   %10000000       		; #.......
	.byte   %01000000       		; .#......
	.byte   %00100000       		; ..#.....
	.byte   %01000000       		; .#......
	.byte   %10000000       		; #.......
	.byte   %11100000       		; ###.....

	.byte   %00100000       		; ..#.....
	.byte   %01010000       		; .#.#....
	.byte   %01110000       		; .###....
	.byte   %01010000       		; .#.#....
	.byte   %00100000       		; ..#.....
	.byte   %00000000       		; ........

	.byte   %00110000       		; ..##....
	.byte   %01100000       		; .##.....
	.byte   %10100000       		; #.#.....
	.byte   %01100000       		; .##.....
	.byte   %00110000       		; ..##....
	.byte   %00000000       		; ........

	.byte   %00100000       		; ..#.....
	.byte   %01010000       		; .#.#....
	.byte   %00100000       		; ..#.....
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........

	.byte   %11000000       		; ##......
	.byte   %01100000       		; .##.....
	.byte   %01010000       		; .#.#....
	.byte   %01100000       		; .##.....
	.byte   %11000000       		; ##......
	.byte   %00000000       		; ........

	.byte   %11000000       		; ##......
	.byte   %10100000       		; #.#.....
	.byte   %01010000       		; .#.#....
	.byte   %01010000       		; .#.#....
	.byte   %10100000       		; #.#.....
	.byte   %11000000       		; ##......

	.byte   %11111000       		; #####...
	.byte   %00100000       		; ..#.....
	.byte   %01000000       		; .#......
	.byte   %01000000       		; .#......
	.byte   %00100000       		; ..#.....
	.byte   %11111000       		; #####...

	.byte   %01010000       		; .#.#....
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %01110000       		; .###....
	.byte   %01010000       		; .#.#....
	.byte   %01110000       		; .###....
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %01100000       		; .##.....
	.byte   %10010000       		; #..#....
	.byte   %01100000       		; .##.....
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........

	.byte   %00100000       		; ..#.....
	.byte   %01110000       		; .###....
	.byte   %11111000       		; #####...
	.byte   %01110000       		; .###....
	.byte   %00100000       		; ..#.....
	.byte   %00000000       		; ........

	.byte   %10001000       		; #...#...
	.byte   %01010000       		; .#.#....
	.byte   %00100000       		; ..#.....
	.byte   %01010000       		; .#.#....
	.byte   %10001000       		; #...#...
	.byte   %00000000       		; ........

	.byte   %00010000       		; ...#....
	.byte   %00100000       		; ..#.....
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........

	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00010000       		; ...#....
	.byte   %00100000       		; ..#.....

	.byte   %10100000       		; #.#.....
	.byte   %00100000       		; ..#.....
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........
	.byte   %00000000       		; ........

	.byte   %00100000       		; ..#.....
	.byte   %01110000       		; .###....
	.byte   %00100000       		; ..#.....
	.byte   %01110000       		; .###....
	.byte   %00100000       		; ..#.....
	.byte   %00000000       		; ........

	.byte   %00100000       		; ..#.....
	.byte   %00100000       		; ..#.....
	.byte   %00100000       		; ..#.....
	.byte   %00100000       		; ..#.....
	.byte   %00100000       		; ..#.....
	.byte   %00100000       		; ..#.....

;*******************************************************************************
;*                                                                             *
;*                               sub_hatabs_slot                               *
;*                                                                             *
;*                    Return first unused HATABS table entry                   *
;*                                                                             *
;*******************************************************************************
sub_bf86:
sub_hatabs_slot:

;** (n) Return the first empty slot in the Handler Address Table *****************
	cmp     HATABS,x			; Is entry empty?
	beq	:+      			; Quit. Z will be set
	inx             			; Skip the 3 byte entry
	inx
	inx
	cpx     #$20    			; If >= 32 Then Quit. Z will be clear
	bcc     sub_bf86			; Otherwise continue trying.
:	rts             			; Return with Z and X (offset)

;*******************************************************************************
;*                                                                             *
;*                                 cart_init                                   *
;*                                                                             *
;*         OS calls this code before jumping to cart start address             *
;*                                                                             *
;*******************************************************************************

cart_init:
	jsr	sub_b47d                        ; Init POKEY and serial interrupts

;*******************************************************************************
	lda	#$51
	sta	byte_1347                       ; Let byte_1347 = $51
	sta	byte_133a                       ; Let byte_133a = $51

;*******************************************************************************
	lda	$CB				;
	pha             			; Save current value in $CB
	lda     #$00    			;
	sta     $CB     			; Let $CB = $00
	jsr     sub_b420			;
	pla             			;
	sta     $CB     			; Restore original value of $CB

;*******************************************************************************
	lda     #$51    			; BFAB A9 51                    .Q
	sta     byte_133a

;*******************************************************************************
	lda     $08     			;
	beq     LBFC2   			; Skip next part if warmstart

;*******************************************************************************
	ldx     byte_B2
	cpx     #$02    			; BFB6 E0 02                    ..
	bcs     :+
	sec             			; BFBA 38                       8
	bne     LBFD8   			; BFBB D0 1B                    ..

:	ldx     #$02    			; BFBD A2 02                    ..
	jsr     sub_b43f

;*******************************************************************************
LBFC2:  lda     #$FF    			; BFC2 A9 FF                    ..
	sta     byte_133a

	lda     #$00    			; BFC7 A9 00                    ..
	sta     $CB     			; BFC9 85 CB                    ..
	ldx     #$02    			; BFCB A2 02                    ..
	jsr     sub_b422
	lda     #$51    			; BFD0 A9 51                    .Q
	sta     byte_133a
	jsr     sub_b420
LBFD8:  jsr     sub_b4f7
	bcs	:+
	lda     #$00    			; BFDD A9 00                    ..
	sta     byte_133a
	tax             			; BFE2 AA                       .
	jsr     sub_bf86
	bne	:+
	lda     #'T'
	sta     HATABS,x
	lda     #<t_handler
	sta     HATABS+1,x
	lda     #>t_handler
	sta     HATABS+2,x
:	rts

	.byte	$00,$00				;
;*******************************************************************************
;*                                                                             *
;*                                   LBFFA                                     *
;*                                                                             *
;*                              Cartridge Header                               *
;*                                                                             *
;*******************************************************************************
LBFFA:	.addr	cart_start                      ; Cart start address
	.byte   $00                             ; Used by OS to determine if this is ROM
	.byte	$04                             ; Option byte: run Cart init, then run Cart start
	.addr	cart_init                       ; Cart init address - entry point
