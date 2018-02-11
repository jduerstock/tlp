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

	.segment "SEG1"

;*******************************************************************************
;*                                                                             *
;*                         S Y S T E M   S Y M B O L S                         *
;*                                                                             *
;*******************************************************************************

TSTDAT		:= $0007
POKMSK		:= $0010			; POKEY Interrupts: used by IRQ service
VTIMR1		:= $0210
SHFLOK		:= $02BE			; Flag for shift and control keys
CDTMA1		:= $0226			; System timer one jump address
SRTIMR		:= $022B
DLIST		:= $0230			; Starting address of the diplay list
STICK0		:= $0278			; Joystick 0
CH		:= $02FC
DCOMND		:= $0302
DSTATS		:= $0303
HATABS		:= $031A			; Handler Address Table
ICCOM		:= $0342			 
ICBA		:= $0344
ICBL		:= $0348
ICAX1		:= $034A
ICAX2		:= $034B

HPOSM0		:= $D004
HPOSM1		:= $D005
GRACTL		:= $D01D			; Turn on/off player missiles or latch triggers
CONSOL		:= $D01F

AUDCTL		:= $D208
STIMER		:= $D209
IRQEN		:= $D20E
PMBASE		:= $D407
IRQST		:= IRQEN

PORTA		:= $D300
PACTL		:= $D302
PBCTL		:= $D303

DMACTL		:= $D400			; Direct Memory Access (DMA) control

CIOV		:= $E456
SIOV		:= $E459
SETVBV		:= $E45C
XITVBV		:= $E462

;*******************************************************************************
;*                                                                             *
;*                         C A R T   S Y M B O L S                             *
;*                                                                             *
;*******************************************************************************

L0070           := $0070
L0080           := $0080

byte_B2		:= $00B2
byte_B7		:= $00B7
byte_B8		:= $00B8
byte_CC		:= $00CC
off_DD		:= $00DD
byte_DF		:= $00DF
byte_E0		:= $00E0
byte_E1		:= $00E1
byte_E2		:= $00E2

off_E3		:= $00E3
off_E5		:= $00E5
off_EC		:= $00EC
off_F4		:= $00F4
off_FA		:= $00FA
byte_FF		:= $00FF
byte_133a	:= $133A
byte_133e	:= $133E
byte_1340	:= $1340
byte_1343	:= $1343
byte_1344	:= $1344
byte_1345	:= $1345
byte_1346	:= $1346
byte_1347	:= $1347
byte_134c	:= $134C
off_134d	:= $134D
L2000           := $2000
L3E33           := $3E33
L4000           := $4000
charset_sm	:= $BC44			; 6x6 character set
; ----------------------------------------------------------------------------

sub_a000:
	jsr     sub_b799
	jsr     sub_a214			; Initialize Display List
	jsr     sub_a33d
	ldy     #$08                            ; A009 A0 08                    ..
	jsr     sub_b1f1
	ldy     #$28                            ; A00E A0 28                    .(
	jsr     sub_b206
	bmi     LA035                           ; A013 30 20                    0 
	lda     #$1A                            ; A015 A9 1A                    ..
	sta     $9C                             ; A017 85 9C                    ..
	ldy     #$33                            ; A019 A0 33                    .3
	lda     #<LB8DF
	ldx     #>LB8DF
	jsr     sub_a89f
LA022:  lda     CH
	cmp     #$FF                            ; A025 C9 FF                    ..
	beq     LA022                           ; A027 F0 F9                    ..
	ldx     #$FF                            ; A029 A2 FF                    ..
	stx     CH
	cmp     #$1F                            ; A02E C9 1F                    ..
	bne     LA04F                           ; A030 D0 1D                    ..
	jsr     sub_a33d
LA035:  jsr     LB238                           ; A035 20 38 B2                  8.
	jsr     LB719                           ; A038 20 19 B7                  ..
	bmi     LA049                           ; A03B 30 0C                    0.
	lda     #$1C                            ; A03D A9 1C                    ..
	ldx     #$B9                            ; A03F A2 B9                    ..
	ldy     #$08                            ; A041 A0 08                    ..
	jsr     sub_a89f
	jmp     LA05E                           ; A046 4C 5E A0                 L^.

; ----------------------------------------------------------------------------
LA049:  jsr     sub_b272
	jmp     LA05E                           ; A04C 4C 5E A0                 L^.

; ----------------------------------------------------------------------------
LA04F:	lda	#$4C
	jsr	sub_b242
LA054:  lda     #$46                            ; A054 A9 46                    .F
	jsr     sub_b242
	lda     $02EB                           ; A059 AD EB 02                 ...
	bpl     LA054                           ; A05C 10 F6                    ..
LA05E:  jsr     sub_a963
	jsr     LAB35                           ; A061 20 35 AB                  5.
	jsr     LA12C                           ; A064 20 2C A1                  ,.
	lda     byte_CC
	beq     LA05E                           ; A069 F0 F3                    ..
	jsr     LA079                           ; A06B 20 79 A0                  y.
	bcc     LA05E                           ; A06E 90 EE                    ..
	jsr     LA0A3                           ; A070 20 A3 A0                  ..
	lda     #$00                            ; A073 A9 00                    ..
	sta     $B6                             ; A075 85 B6                    ..
	beq     LA05E                           ; A077 F0 E5                    ..
LA079:  ldy     #$20                            ; A079 A0 20                    . 
	jsr     sub_b1f1
	and     #$7F                            ; A07E 29 7F                    ).
	bit     $B3                             ; A080 24 B3                    $.
	bmi     LA0D2                           ; A082 30 4E                    0N
	cmp     #$20                            ; A084 C9 20                    . 
	bcs     LA08D                           ; A086 B0 05                    ..
	jsr     LA0F3                           ; A088 20 F3 A0                  ..
LA08B:  clc                                     ; A08B 18                       .
	rts                                     ; A08C 60                       `

; ----------------------------------------------------------------------------
LA08D:  ldx     $B6                             ; A08D A6 B6                    ..
	sta     $3E2E,x                         ; A08F 9D 2E 3E                 ..>
	inx                                     ; A092 E8                       .
	stx     $B6                             ; A093 86 B6                    ..
	ldy     $B5                             ; A095 A4 B5                    ..
	bpl     LA0A0                           ; A097 10 07                    ..
	cmp     #$60                            ; A099 C9 60                    .`
	bcs     LA08B                           ; A09B B0 EE                    ..
	cmp     #$40                            ; A09D C9 40                    .@
	rts                                     ; A09F 60                       `

; ----------------------------------------------------------------------------
LA0A0:  cpx     $B5                             ; A0A0 E4 B5                    ..
	rts                                     ; A0A2 60                       `

; ----------------------------------------------------------------------------
LA0A3:  ldy     #$00                            ; A0A3 A0 00                    ..
	sty     $B6                             ; A0A5 84 B6                    ..
	bvc     LA0C5                           ; A0A7 50 1C                    P.
	ldy     $B4                             ; A0A9 A4 B4                    ..
	cpy     #$05                            ; A0AB C0 05                    ..
	beq     LA0B6                           ; A0AD F0 07                    ..
	jsr     LA120                           ; A0AF 20 20 A1                   .
	tya                                     ; A0B2 98                       .
	bne     LA0B6                           ; A0B3 D0 01                    ..
	rts                                     ; A0B5 60                       `

; ----------------------------------------------------------------------------
LA0B6:  dey                                     ; A0B6 88                       .
	beq     LA0BC                           ; A0B7 F0 03                    ..
	jsr     sub_a7fe
LA0BC:  lda     LBAE2,y                         ; A0BC B9 E2 BA                 ...
	pha                                     ; A0BF 48                       H
	lda     LBAE7,y                         ; A0C0 B9 E7 BA                 ...
	pha                                     ; A0C3 48                       H
	rts                                     ; A0C4 60                       `

; ----------------------------------------------------------------------------
LA0C5:  ldx     $B3                             ; A0C5 A6 B3                    ..
	lda     LBACA,x                         ; A0C7 BD CA BA                 ...
	beq     LA0D1                           ; A0CA F0 05                    ..
	pha                                     ; A0CC 48                       H
	lda     LBAD2,x                         ; A0CD BD D2 BA                 ...
	pha                                     ; A0D0 48                       H
LA0D1:  rts                                     ; A0D1 60                       `

; ----------------------------------------------------------------------------
LA0D2:  asl     $B3                             ; A0D2 06 B3                    ..
	lsr     $B3                             ; A0D4 46 B3                    F.
	jsr     LA0DB                           ; A0D6 20 DB A0                  ..
	clc                                     ; A0D9 18                       .
LA0DA:  rts                                     ; A0DA 60                       `

; ----------------------------------------------------------------------------
LA0DB:  cmp     #$5B                            ; A0DB C9 5B                    .[
	bcs     LA0DA                           ; A0DD B0 FB                    ..
	cmp     #$3D                            ; A0DF C9 3D                    .=
	beq     LA105                           ; A0E1 F0 22                    ."
	cmp     #$20                            ; A0E3 C9 20                    . 
	bcs     LA0EB                           ; A0E5 B0 04                    ..
	adc     #$20                            ; A0E7 69 20                    i 
	bcc     LA0F3                           ; A0E9 90 08                    ..
LA0EB:  cmp     #$32                            ; A0EB C9 32                    .2
	beq     LA10B                           ; A0ED F0 1C                    ..
	cmp     #$40                            ; A0EF C9 40                    .@
	bcc     LA0DA                           ; A0F1 90 E7                    ..
LA0F3:  tax                                     ; A0F3 AA                       .
	clc                                     ; A0F4 18                       .
	lda     #$66                            ; A0F5 A9 66                    .f
	adc     LBA6F,x                         ; A0F7 7D 6F BA                 }o.
	tay                                     ; A0FA A8                       .
	lda     #$00                            ; A0FB A9 00                    ..
	sta     $B6                             ; A0FD 85 B6                    ..
	adc     #$A3                            ; A0FF 69 A3                    i.
	pha                                     ; A101 48                       H
	tya                                     ; A102 98                       .
	pha                                     ; A103 48                       H
	rts                                     ; A104 60                       `

; ----------------------------------------------------------------------------
LA105:  ldx     #$02                            ; A105 A2 02                    ..
	lda     #$00                            ; A107 A9 00                    ..
	beq     LA111                           ; A109 F0 06                    ..
LA10B:  ror     $B5                             ; A10B 66 B5                    f.
	lda     #$01                            ; A10D A9 01                    ..
	bne     LA118                           ; A10F D0 07                    ..
LA111:  ldy     $B5                             ; A111 A4 B5                    ..
	sty     $1354                           ; A113 8C 54 13                 .T.
	stx     $B5                             ; A116 86 B5                    ..
LA118:  sta     $B4                             ; A118 85 B4                    ..
	lda     $B3                             ; A11A A5 B3                    ..
	ora     #$40                            ; A11C 09 40                    .@
	bne     LA129                           ; A11E D0 09                    ..
LA120:  lda     $1354                           ; A120 AD 54 13                 .T.
	sta     $B5                             ; A123 85 B5                    ..
	lda     $B3                             ; A125 A5 B3                    ..
	and     #$BF                            ; A127 29 BF                    ).
LA129:  sta     $B3                             ; A129 85 B3                    ..
LA12B:  rts                                     ; A12B 60                       `

; ----------------------------------------------------------------------------
LA12C:  lda     $B9                             ; A12C A5 B9                    ..
	beq     LA12B                           ; A12E F0 FB                    ..
	jmp     (off_134d)

; ----------------------------------------------------------------------------

sub_a133:
	ldy     $DC                             ; A133 A4 DC                    ..
	lda     $DB                             ; A135 A5 DB                    ..
	ldx     $FF                             ; A137 A6 FF                    ..
	bpl     LA14B                           ; A139 10 10                    ..
	iny                                     ; A13B C8                       .
	bne     LA143                           ; A13C D0 05                    ..
	tax                                     ; A13E AA                       .
	bmi     LA144                           ; A13F 30 03                    0.
	sta     $FF                             ; A141 85 FF                    ..
LA143:  rts                                     ; A143 60                       `

; ----------------------------------------------------------------------------
LA144:  inx                                     ; A144 E8                       .
	stx     $134F                           ; A145 8E 4F 13                 .O.
	jmp     LA1FE                           ; A148 4C FE A1                 L..

; ----------------------------------------------------------------------------
LA14B:  bit     $134F                           ; A14B 2C 4F 13                 ,O.
	bpl     LA151                           ; A14E 10 01                    ..
	rts                                     ; A150 60                       `

; ----------------------------------------------------------------------------
LA151:  ldx     $FF                             ; A151 A6 FF                    ..
	beq     LA162                           ; A153 F0 0D                    ..
	dex                                     ; A155 CA                       .
	beq     LA16D                           ; A156 F0 15                    ..
	dex                                     ; A158 CA                       .
	beq     LA18C                           ; A159 F0 31                    .1
	dex                                     ; A15B CA                       .
	beq     LA174                           ; A15C F0 16                    ..
	dex                                     ; A15E CA                       .
	beq     LA180                           ; A15F F0 1F                    ..
	rts                                     ; A161 60                       `

; ----------------------------------------------------------------------------
LA162:  sty     $CF                             ; A162 84 CF                    ..
	sta     $CE                             ; A164 85 CE                    ..
	ldy     #$00                            ; A166 A0 00                    ..
	lda     ($CE),y                         ; A168 B1 CE                    ..
	jmp     LA1DD                           ; A16A 4C DD A1                 L..

; ----------------------------------------------------------------------------
LA16D:  sty     $CF                             ; A16D 84 CF                    ..
	sta     $CE                             ; A16F 85 CE                    ..
	jmp     LA1FE                           ; A171 4C FE A1                 L..

; ----------------------------------------------------------------------------
LA174:  sty     off_134d+1
	sta     off_134d
	dex                                     ; A17A CA                       .
	stx     $B9                             ; A17B 86 B9                    ..
	jmp     LA1FE                           ; A17D 4C FE A1                 L..

; ----------------------------------------------------------------------------
LA180:  sty     off_134d+1
	sta     off_134d
	jsr     LA1FE                           ; A186 20 FE A1                  ..
	jmp     (off_134d)

; ----------------------------------------------------------------------------
LA18C:  bit     $1350                           ; A18C 2C 50 13                 ,P.
	bmi     LA19C                           ; A18F 30 0B                    0.
	sta     $1351                           ; A191 8D 51 13                 .Q.
	sty     $1352                           ; A194 8C 52 13                 .R.
	dex                                     ; A197 CA                       .
	stx     $1350                           ; A198 8E 50 13                 .P.
	rts                                     ; A19B 60                       `

; ----------------------------------------------------------------------------
LA19C:  tya                                     ; A19C 98                       .
	ldy     #$00                            ; A19D A0 00                    ..
	sta     ($CE),y                         ; A19F 91 CE                    ..
	iny                                     ; A1A1 C8                       .
	dec     $1351                           ; A1A2 CE 51 13                 .Q.
	bne     LA1B0                           ; A1A5 D0 09                    ..
	tya                                     ; A1A7 98                       .
	ldx     $1352                           ; A1A8 AE 52 13                 .R.
	beq     LA1C8                           ; A1AB F0 1B                    ..
	dec     $1352                           ; A1AD CE 52 13                 .R.
LA1B0:  lda     $DB                             ; A1B0 A5 DB                    ..
	sta     ($CE),y                         ; A1B2 91 CE                    ..
	dec     $1351                           ; A1B4 CE 51 13                 .Q.
	bne     LA1C4                           ; A1B7 D0 0B                    ..
	ldx     $1352                           ; A1B9 AE 52 13                 .R.
	beq     LA1C3                           ; A1BC F0 05                    ..
	dec     $1352                           ; A1BE CE 52 13                 .R.
	bvs     LA1C4                           ; A1C1 70 01                    p.
LA1C3:  clv                                     ; A1C3 B8                       .
LA1C4:  lda     #$02                            ; A1C4 A9 02                    ..
	bne     LA1C9                           ; A1C6 D0 01                    ..
LA1C8:  clv                                     ; A1C8 B8                       .
LA1C9:  php                                     ; A1C9 08                       .
	clc                                     ; A1CA 18                       .
	adc     $CE                             ; A1CB 65 CE                    e.
	sta     $CE                             ; A1CD 85 CE                    ..
	bcc     LA1D3                           ; A1CF 90 02                    ..
	inc     $CF                             ; A1D1 E6 CF                    ..
LA1D3:  plp                                     ; A1D3 28                       (
	bvs     LA1DC                           ; A1D4 70 06                    p.
	jsr     LA1FE                           ; A1D6 20 FE A1                  ..
	inc     $1350                           ; A1D9 EE 50 13                 .P.
LA1DC:  rts                                     ; A1DC 60                       `

; ----------------------------------------------------------------------------
LA1DD:  lda     #$1B                            ; A1DD A9 1B                    ..
	jsr     sub_ab54
	ldy     #$00                            ; A1E2 A0 00                    ..
	sty     $E7                             ; A1E4 84 E7                    ..
	lda     ($CE),y                         ; A1E6 B1 CE                    ..
	tay                                     ; A1E8 A8                       .
	asl     a                               ; A1E9 0A                       .
	rol     $E7                             ; A1EA 26 E7                    &.
	asl     a                               ; A1EC 0A                       .
	rol     $E7                             ; A1ED 26 E7                    &.
	tya                                     ; A1EF 98                       .
	and     #$3F                            ; A1F0 29 3F                    )?
	ora     #$40                            ; A1F2 09 40                    .@
	jsr     sub_ab54
	lda     $E7                             ; A1F7 A5 E7                    ..
	ora     #$68                            ; A1F9 09 68                    .h
	jsr     sub_ab54
LA1FE:  jsr     LA120                           ; A1FE 20 20 A1                   .
	lda     #$FF                            ; A201 A9 FF                    ..
	sta     $FF                             ; A203 85 FF                    ..
	lda     #$1B                            ; A205 A9 1B                    ..
	jsr     sub_ab54
	lda     #$46                            ; A20A A9 46                    .F
	jsr     sub_ab54
	lda     #$68                            ; A20F A9 68                    .h
	jmp     sub_ab54

;*******************************************************************************
;*                                                                             *
;*                                  sub_a214                                   *
;*                                                                             *
;*                          Initialize Display List                            *
;*                                                                             *
;*******************************************************************************

; DESCRIPTION
;
; This subroutine initializes 2 display lists.
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
	lda     #$00                            ; 
	sta     DMACTL                          ; Turn off ANTIC
	sta     GRACTL				; Turn off PMG, unlatch triggers
	sta     SHFLOK				; Force keyboard to lower case

;** (n) Clear RAM at 0080-00FF and  0580-05FF **********************************
	ldx     #$7F                            ; Clear 128 bytes at ...
:       sta     $0580,x                         ; 0580-05FF and ...
	sta     L0080,x                         ; 0080-00FF 
	dex                                     ; 
	bpl     :-                              ; End Loop

;** (n) TODO Why storing FF in these two locations??? **************************
	stx     byte_B8				; Store FF in byte_B8 TODO
	stx     byte_FF                         ; Store FF in byte_FF TODO

;** (n) Prepare values for head of display lists *******************************
	lda     #$70                            ;
	ldx     #$02                            ; 
	stx     byte_B7				; Let byte_B7 = 2 TODO
	stx     byte_B2				; Let byte_B2 = 2 TODO

;** (n) Write head of display list # 1 *****************************************
	inx                                     ; Store 3 x $70 
:       sta     $10C9,x                         ; at $1000 and $10CA
	sta     $0FFF,x                         ; 8 blank scan lines each
	dex                                     ; 
	bne     :-                              ; End Loop

;** (n) Point to screen RAM at $2010 in display list #1 ************************
	lda     #$10                            ; 
	sta     $1004                           ; 
	sta     off_F4				; Store Screen location
	lda     #$20                            ; in zero page varibles, too
	sta     $1005                           ; 
	sta     off_F4+1			; 

;** (n) Write body of display list #1 ******************************************
	lda     #$0F                            ; Store entry for ANTIC mode F
:       sta     $1006,x                         ; in body of display list
	inx                                     ; 
	cpx     #$C1                            ; 
	bne     :-                              ; End Loop

;** (n) Write 2 display list entries to point to screen RAM ********************
	lda     #$4F                            ; Write ANTIC mode F & screen
	sta     $1003                           ; location at top of disp list
	sta     $106B                           ; again after 102 mode F lines

;** (n) Point to screen RAM at $3000 in display list #1 ************************
	lda     #<$3000				;
	sta     $106C                           ; point to screen RAM to $3000
	lda     #>$3000				;
	sta     $106D                           ; 

;** (n) Write tail end of display list #1 **************************************
	lda     #$41                            ; Write Wait for sync
	sta     $1006,x                         ; to bottom of list
	sta     $130D                           ; 
	lda     #$00                            ; Tell ANTIC to jump
	sta     $1007,x                         ; back to the top of the display
	sta     DLIST				; list at $1000.
	lda     #$10                            ; 
	sta     $1008,x                         ; Also register the new display
	sta     DLIST+1                         ; list loc with the hardware

;** (n) Screen RAM for display list #2 will begin at $4000 *********************
	lda     #<L4000				; 
	sta     byte_DF				; Let byte_DF = $00 TODO
	sta     byte_E1				; Let byte_E1 = $00 TODO

	lda     #>L4000				; 
	sta     byte_E0                         ; Let byte_E0 = $40 TODO
	sta     byte_E2                         ; Let byte_E2 = $40 TODO

;** (n) Call sub to derive body of display list #2 *****************************
	jsr     LB1AB                           ; TODO

;** (n) Write tail of display list #2 ******************************************
	lda     #$CA                            ; Tell ANTIC to jump
	sta     $130E                           ; back to the top of the display
	lda     #$10                            ; list at $10CA.
	sta     $130F                           ; 

;** (n) Create a table of pointers to 192 scan lines ****************************
	ldx     #$00                            ; Create a table of pointers...
:	lda     off_F4+1                        ; to the screen RAM for 192 scan
	sta     $04C0,x                         ; lines. LSB in $0400-$04BF...
	lda     off_F4                          ; MSB in $04C0-$057F...
	sta     $0400,x                         ; starting with $2010, $0238, ..., $3DE8
	inx                                     ; 
	cpx     #$C0                            ; Quit at 192 iterations
	beq	:+                              ;
	lda     off_F4                          ;
	adc     #$28                            ; Add 40 bytes (1 row)
	sta     off_F4                          ;
	bcc	:-                              ;
	inc     off_F4+1                        ;
	bne	:-				; End Loop

:	lda     #$03                            ; 
	sta     GRACTL				; Enable display of PMG

	lda     #$04                            ; PMG will be stored in page 4
	sta     PMBASE                          ; 

	lda     #$55                            ; A2C1 A9 55                    .U
	sta     $D00C                           ; A2C3 8D 0C D0                 ...
	lda     #$00                            ; A2C6 A9 00                    ..
	ldx     #$03                            ; A2C8 A2 03                    ..
LA2CA:  sta     HPOSM0,x
	dex                                     ; A2CD CA                       .
	bpl     LA2CA                           ; A2CE 10 FA                    ..
	ldx     #$32                            ; A2D0 A2 32                    .2
	stx     $C4                             ; A2D2 86 C4                    ..
	ldx     #$0F                            ; A2D4 A2 0F                    ..
	stx     $C0                             ; A2D6 86 C0                    ..
	ldx     #$07                            ; A2D8 A2 07                    ..
	stx     $B3                             ; A2DA 86 B3                    ..
LA2DC:  lda     LBA67,x                         ; A2DC BD 67 BA                 .g.
	sta     $058C,x                         ; A2DF 9D 8C 05                 ...
	dex                                     ; A2E2 CA                       .
	bpl     LA2DC                           ; A2E3 10 F7                    ..
	stx     $C7                             ; A2E5 86 C7                    ..
	lda     #$01                            ; A2E7 A9 01                    ..
	sta     $C3                             ; A2E9 85 C3                    ..
	sta     $B5                             ; A2EB 85 B5                    ..
	sta     $026F                           ; A2ED 8D 6F 02                 .o.
	lda     #$16				; set background
	sta     $02C8                           ; A2F2 8D C8 02                 ...
	sta     $02C6                           ; A2F5 8D C6 02                 ...
	sta     $D3                             ; A2F8 85 D3                    ..
	jsr     sub_a324
	lda     #$94                            ; A2FD A9 94                    ..
	sta     $D1                             ; A2FF 85 D1                    ..
	lda     #$E1                            ; text color
	sta     $02C5                           ; A303 8D C5 02                 ...
	sta     $D2                             ; A306 85 D2                    ..
	lda     #$CA                            ; A308 A9 CA                    ..
	sta     $D0                             ; A30A 85 D0                    ..
	lda     #>charset_sm
	sta     $E9                             ; A30E 85 E9                    ..
	lda     #<charset_sm
	sta     $E8                             ; A312 85 E8                    ..
	lda     POKMSK
	and     #$7F                            ; A316 29 7F                    ).
	jsr     sub_b549
	ldy     #<sub_b013
	ldx     #>sub_b013
	lda     #$07                            ; A31F A9 07                    ..
	jmp     SETVBV

; ----------------------------------------------------------------------------
sub_a324:
	tay                                     ; A324 A8                       .
	clc                                     ; A325 18                       .
	adc     #$04                            ; A326 69 04                    i.
	and     #$0F                            ; A328 29 0F                    ).
	sta     $D9                             ; A32A 85 D9                    ..
	tya                                     ; A32C 98                       .
	and     #$F0                            ; A32D 29 F0                    ).
	clc                                     ; A32F 18                       .
	adc     #$30                            ; A330 69 30                    i0
	ora     $D9                             ; A332 05 D9                    ..
	ldx     #$03                            ; A334 A2 03                    ..
LA336:  sta     $02C0,x                         ; A336 9D C0 02                 ...
	dex                                     ; A339 CA                       .
	bpl     LA336                           ; A33A 10 FA                    ..
	rts                                     ; A33C 60                       `

; ----------------------------------------------------------------------------
sub_a33d:
	jsr     sub_a367
	jsr     LA3C8                           ; A340 20 C8 A3                  ..
	lda     #$50                            ; A343 A9 50                    .P
	sta     $A4                             ; A345 85 A4                    ..
	sta     $9C                             ; A347 85 9C                    ..
	ldy     #$1C                            ; A349 A0 1C                    ..
	lda     #<LB8C2
	ldx     #>LB8C2
	jsr     sub_a89f
	jsr     LA3C2                           ; A352 20 C2 A3                  ..
	lda     #$67                            ; A355 A9 67                    .g
	sta     $A4                             ; A357 85 A4                    ..
	sta     $9C                             ; A359 85 9C                    ..
	ldy     #$13                            ; A35B A0 13                    ..
	lda     #<LB8AE
	ldx     #>LB8AE
	jsr     sub_a89f
	jmp     LA3C2                           ; A364 4C C2 A3                 L..

; ----------------------------------------------------------------------------
sub_a367:
	jmp     LA6A2                           ; A367 4C A2 A6                 L..

; ----------------------------------------------------------------------------
	txa                                     ; A36A 8A                       .
	and     #$07                            ; A36B 29 07                    ).
	sta     $B3                             ; A36D 85 B3                    ..
	tax                                     ; A36F AA                       .
	lda     LBADA,x                         ; A370 BD DA BA                 ...
	sta     $B5                             ; A373 85 B5                    ..
	ldx     #$00                            ; A375 A2 00                    ..
	stx     $DA                             ; A377 86 DA                    ..
	rts                                     ; A379 60                       `

; ----------------------------------------------------------------------------
	txa                                     ; A37A 8A                       .
	and     #$03                            ; A37B 29 03                    ).
	lsr     a                               ; A37D 4A                       J
	ror     a                               ; A37E 6A                       j
	ror     a                               ; A37F 6A                       j
	sta     $B0                             ; A380 85 B0                    ..
	rts                                     ; A382 60                       `

; ----------------------------------------------------------------------------
	lda     #$80                            ; A383 A9 80                    ..
	bne     LA389                           ; A385 D0 02                    ..
	lda     #$00                            ; A387 A9 00                    ..
LA389:  sta     $1353                           ; A389 8D 53 13                 .S.
	rts                                     ; A38C 60                       `

; ----------------------------------------------------------------------------
	lda     #$00                            ; A38D A9 00                    ..
	beq     LA393                           ; A38F F0 02                    ..
	lda     #$FF                            ; A391 A9 FF                    ..
LA393:  sta     $CA                             ; A393 85 CA                    ..
	rts                                     ; A395 60                       `

; ----------------------------------------------------------------------------
	clc                                     ; A396 18                       .
	jmp     LA824                           ; A397 4C 24 A8                 L$.

; ----------------------------------------------------------------------------
	sec                                     ; A39A 38                       8
	jmp     LA824                           ; A39B 4C 24 A8                 L$.

; ----------------------------------------------------------------------------
	lda     $9C                             ; A39E A5 9C                    ..
	sta     $AC                             ; A3A0 85 AC                    ..
	lda     $9D                             ; A3A2 A5 9D                    ..
	sta     $AD                             ; A3A4 85 AD                    ..
	lda     $A4                             ; A3A6 A5 A4                    ..
	sta     $AE                             ; A3A8 85 AE                    ..
	lda     $A5                             ; A3AA A5 A5                    ..
	sta     $AF                             ; A3AC 85 AF                    ..
	rts                                     ; A3AE 60                       `

; ----------------------------------------------------------------------------
	jmp     LABD3                           ; A3AF 4C D3 AB                 L..

; ----------------------------------------------------------------------------
LA3B2:  lda     $AC                             ; A3B2 A5 AC                    ..
	sta     $9C                             ; A3B4 85 9C                    ..
	lda     $AD                             ; A3B6 A5 AD                    ..
	sta     $9D                             ; A3B8 85 9D                    ..
	lda     $AE                             ; A3BA A5 AE                    ..
	sta     $A4                             ; A3BC 85 A4                    ..
	lda     $AF                             ; A3BE A5 AF                    ..
	sta     $A5                             ; A3C0 85 A5                    ..
LA3C2:  jsr     LA882                           ; A3C2 20 82 A8                  ..
	jmp     LA857                           ; A3C5 4C 57 A8                 LW.

; ----------------------------------------------------------------------------
LA3C8:  lda     #$BA                            ; A3C8 A9 BA                    ..
	sta     $A6                             ; A3CA 85 A6                    ..
	lda     #$74                            ; A3CC A9 74                    .t
	sta     $9E                             ; A3CE 85 9E                    ..
	ldx     #$01                            ; A3D0 A2 01                    ..
	stx     $9F                             ; A3D2 86 9F                    ..
	dex                                     ; A3D4 CA                       .
	stx     $9C                             ; A3D5 86 9C                    ..
	stx     $9D                             ; A3D7 86 9D                    ..
	stx     $A4                             ; A3D9 86 A4                    ..
	stx     $A5                             ; A3DB 86 A5                    ..
LA3DD:  rts                                     ; A3DD 60                       `

; ----------------------------------------------------------------------------
	jsr     LA890                           ; A3DE 20 90 A8                  ..
	sta     $D9                             ; A3E1 85 D9                    ..
	sec                                     ; A3E3 38                       8
	lda     $A4                             ; A3E4 A5 A4                    ..
	sbc     $D9                             ; A3E6 E5 D9                    ..
	sta     $A4                             ; A3E8 85 A4                    ..
	bcs     LA3EE                           ; A3EA B0 02                    ..
	dec     $A5                             ; A3EC C6 A5                    ..
LA3EE:  lda     $9C                             ; A3EE A5 9C                    ..
	sec                                     ; A3F0 38                       8
	sbc     $D8                             ; A3F1 E5 D8                    ..
	sta     $9C                             ; A3F3 85 9C                    ..
	bcs     LA3DD                           ; A3F5 B0 E6                    ..
	dec     $9D                             ; A3F7 C6 9D                    ..
	bpl     LA3DD                           ; A3F9 10 E2                    ..
	lda     #$40                            ; A3FB A9 40                    .@
	clc                                     ; A3FD 18                       .
	adc     $A4                             ; A3FE 65 A4                    e.
	sta     $A4                             ; A400 85 A4                    ..
	lda     #$01                            ; A402 A9 01                    ..
	sta     $A5                             ; A404 85 A5                    ..
	sta     $9D                             ; A406 85 9D                    ..
	bne     LA3DD                           ; A408 D0 D3                    ..
	jsr     LA882                           ; A40A 20 82 A8                  ..
	jmp     LA82D                           ; A40D 4C 2D A8                 L-.

; ----------------------------------------------------------------------------
	txa                                     ; A410 8A                       .
	and     #$03                            ; A411 29 03                    ).
	tay                                     ; A413 A8                       .
	lda     LB9AE,y                         ; A414 B9 AE B9                 ...
	sta     $E8                             ; A417 85 E8                    ..
	lda     LB9B2,y                         ; A419 B9 B2 B9                 ...
	sta     $E9                             ; A41C 85 E9                    ..
	lda     LB9A6,y                         ; A41E B9 A6 B9                 ...
	sta     $EB                             ; A421 85 EB                    ..
	sty     byte_B7
	lda     LB9AA,y                         ; A425 B9 AA B9                 ...
	sta     $EA                             ; A428 85 EA                    ..
	rts                                     ; A42A 60                       `

; ----------------------------------------------------------------------------
	asl     $B3                             ; A42B 06 B3                    ..
	sec                                     ; A42D 38                       8
	ror     $B3                             ; A42E 66 B3                    f.
	rts                                     ; A430 60                       `

; ----------------------------------------------------------------------------
	lda     #$00                            ; A431 A9 00                    ..
LA433:  ldx     #$03                            ; A433 A2 03                    ..
	jmp     LA111                           ; A435 4C 11 A1                 L..

; ----------------------------------------------------------------------------
	lda     #$05                            ; A438 A9 05                    ..
	bne     LA433                           ; A43A D0 F7                    ..
	lda     #$02                            ; A43C A9 02                    ..
	bne     LA433                           ; A43E D0 F3                    ..
	lda     #$03                            ; A440 A9 03                    ..
	bne     LA433                           ; A442 D0 EF                    ..
	lda     #$04                            ; A444 A9 04                    ..
	bne     LA433                           ; A446 D0 EB                    ..
	jsr     sub_a8b3
	ldx     $DA                             ; A44B A6 DA                    ..
	bne     LA45F                           ; A44D D0 10                    ..
	ldx     #$03                            ; A44F A2 03                    ..
LA451:  lda     $A4,x                           ; A451 B5 A4                    ..
	sta     $A8,x                           ; A453 95 A8                    ..
	lda     $9C,x                           ; A455 B5 9C                    ..
	sta     $F0,x                           ; A457 95 F0                    ..
	dex                                     ; A459 CA                       .
	bpl     LA451                           ; A45A 10 F5                    ..
	stx     $DA                             ; A45C 86 DA                    ..
	rts                                     ; A45E 60                       `

; ----------------------------------------------------------------------------
LA45F:  lda     $BA                             ; A45F A5 BA                    ..
	sta     $C9                             ; A461 85 C9                    ..
	bne     LA474                           ; A463 D0 0F                    ..
	lda     $9F                             ; A465 A5 9F                    ..
	sta     $FB                             ; A467 85 FB                    ..
	lda     $9C                             ; A469 A5 9C                    ..
	ldx     $9D                             ; A46B A6 9D                    ..
	ldy     $9E                             ; A46D A4 9E                    ..
	jsr     LA489                           ; A46F 20 89 A4                  ..
	dec     $C9                             ; A472 C6 C9                    ..
LA474:  ldx     #$04                            ; A474 A2 04                    ..
LA476:  lda     $A7,x                           ; A476 B5 A7                    ..
	sta     $EF,x                           ; A478 95 EF                    ..
	dex                                     ; A47A CA                       .
	bne     LA476                           ; A47B D0 F9                    ..
	stx     $FB                             ; A47D 86 FB                    ..
	stx     $F3                             ; A47F 86 F3                    ..
	stx     $DA                             ; A481 86 DA                    ..
	lda     $A4                             ; A483 A5 A4                    ..
	ldx     $A5                             ; A485 A6 A5                    ..
	ldy     $A6                             ; A487 A4 A6                    ..
LA489:  sta     $F8                             ; A489 85 F8                    ..
	stx     $F9                             ; A48B 86 F9                    ..
	sty     $FA                             ; A48D 84 FA                    ..
	cpx     $F1                             ; A48F E4 F1                    ..
	bcc     LA4A5                           ; A491 90 12                    ..
	bne     LA499                           ; A493 D0 04                    ..
	cmp     $F0                             ; A495 C5 F0                    ..
	bcc     LA4A5                           ; A497 90 0C                    ..
LA499:  ldy     $F1                             ; A499 A4 F1                    ..
	stx     $F1                             ; A49B 86 F1                    ..
	sty     $F9                             ; A49D 84 F9                    ..
	ldy     $F0                             ; A49F A4 F0                    ..
	sta     $F0                             ; A4A1 85 F0                    ..
	sty     $F8                             ; A4A3 84 F8                    ..
LA4A5:  lda     $FA                             ; A4A5 A5 FA                    ..
	ldx     $F3                             ; A4A7 A6 F3                    ..
	cpx     $FB                             ; A4A9 E4 FB                    ..
	bcc     LA4BF                           ; A4AB 90 12                    ..
	bne     LA4B3                           ; A4AD D0 04                    ..
	cmp     $F2                             ; A4AF C5 F2                    ..
	bcs     LA4BF                           ; A4B1 B0 0C                    ..
LA4B3:  ldy     $FB                             ; A4B3 A4 FB                    ..
	stx     $FB                             ; A4B5 86 FB                    ..
	sty     $F3                             ; A4B7 84 F3                    ..
	ldy     $F2                             ; A4B9 A4 F2                    ..
	sta     $F2                             ; A4BB 85 F2                    ..
	sty     $FA                             ; A4BD 84 FA                    ..
LA4BF:  ldx     #$03                            ; A4BF A2 03                    ..
	bit     $C9                             ; A4C1 24 C9                    $.
LA4C3:  lda     $F8,x                           ; A4C3 B5 F8                    ..
	bvs     LA4CD                           ; A4C5 70 06                    p.
	sta     $9C,x                           ; A4C7 95 9C                    ..
	ldy     #$40                            ; A4C9 A0 40                    .@
	bvc     LA4D1                           ; A4CB 50 04                    P.
LA4CD:  sta     $A4,x                           ; A4CD 95 A4                    ..
	ldy     #$28                            ; A4CF A0 28                    .(
LA4D1:  dex                                     ; A4D1 CA                       .
	bpl     LA4C3                           ; A4D2 10 EF                    ..
	sty     $D9                             ; A4D4 84 D9                    ..
	bvs     LA4DD                           ; A4D6 70 05                    p.
	bit     $BA                             ; A4D8 24 BA                    $.
	bpl     LA4DD                           ; A4DA 10 01                    ..
	rts                                     ; A4DC 60                       `

; ----------------------------------------------------------------------------
LA4DD:  jsr     LAD8E                           ; A4DD 20 8E AD                  ..
	bit     $B0                             ; A4E0 24 B0                    $.
	lda     $F8                             ; A4E2 A5 F8                    ..
	and     #$07                            ; A4E4 29 07                    ).
	tax                                     ; A4E6 AA                       .
	lda     LB94E,x                         ; A4E7 BD 4E B9                 .N.
	beq     LA4F2                           ; A4EA F0 06                    ..
	bvc     LA4F0                           ; A4EC 50 02                    P.
	eor     #$FF                            ; A4EE 49 FF                    I.
LA4F0:  inc     off_F4
LA4F2:  sta     $AB                             ; A4F2 85 AB                    ..
	lda     $F1                             ; A4F4 A5 F1                    ..
	lsr     a                               ; A4F6 4A                       J
	lda     $F0                             ; A4F7 A5 F0                    ..
	ror     a                               ; A4F9 6A                       j
	lsr     a                               ; A4FA 4A                       J
	lsr     a                               ; A4FB 4A                       J
	sta     $F5                             ; A4FC 85 F5                    ..
	lda     $F0                             ; A4FE A5 F0                    ..
	and     #$07                            ; A500 29 07                    ).
	tax                                     ; A502 AA                       .
	lda     LB956,x                         ; A503 BD 56 B9                 .V.
	bne     LA50A                           ; A506 D0 02                    ..
	inc     $F5                             ; A508 E6 F5                    ..
LA50A:  ldy     #$FF                            ; A50A A0 FF                    ..
	bvc     LA514                           ; A50C 50 06                    P.
	iny                                     ; A50E C8                       .
	tax                                     ; A50F AA                       .
	beq     LA514                           ; A510 F0 02                    ..
	eor     #$FF                            ; A512 49 FF                    I.
LA514:  sta     $A7                             ; A514 85 A7                    ..
	sty     $E7                             ; A516 84 E7                    ..
	sec                                     ; A518 38                       8
	lda     off_FA
	sbc     $F2                             ; A51B E5 F2                    ..
	eor     #$FF                            ; A51D 49 FF                    I.
	sta     $F6                             ; A51F 85 F6                    ..
	lda     off_FA+1
	sbc     $F3                             ; A523 E5 F3                    ..
	eor     #$FF                            ; A525 49 FF                    I.
	sta     $F7                             ; A527 85 F7                    ..
	lda     $F5                             ; A529 A5 F5                    ..
	sec                                     ; A52B 38                       8
	sbc     off_F4
	sta     off_F4
	bmi     LA565                           ; A530 30 33                    03
LA532:  bit     $B0                             ; A532 24 B0                    $.
	ldy     #$00                            ; A534 A0 00                    ..
	lda     $AB                             ; A536 A5 AB                    ..
	beq     LA53D                           ; A538 F0 03                    ..
	jsr     LA596                           ; A53A 20 96 A5                  ..
LA53D:  ldx     off_F4
	beq     LA549                           ; A53F F0 08                    ..
	lda     $E7                             ; A541 A5 E7                    ..
LA543:  jsr     LA596                           ; A543 20 96 A5                  ..
	dex                                     ; A546 CA                       .
	bne     LA543                           ; A547 D0 FA                    ..
LA549:  lda     $A7                             ; A549 A5 A7                    ..
	beq     LA550                           ; A54B F0 03                    ..
	jsr     LA596                           ; A54D 20 96 A5                  ..
LA550:  inc     $F6                             ; A550 E6 F6                    ..
	bne     LA558                           ; A552 D0 04                    ..
	inc     $F7                             ; A554 E6 F7                    ..
	beq     LA573                           ; A556 F0 1B                    ..
LA558:  lda     $D9                             ; A558 A5 D9                    ..
	clc                                     ; A55A 18                       .
	adc     off_E3
	sta     off_E3
	bcc     LA532                           ; A55F 90 D1                    ..
	inc     off_E3+1
	bne     LA532                           ; A563 D0 CD                    ..
LA565:  lda     $AB                             ; A565 A5 AB                    ..
	ora     $A7                             ; A567 05 A7                    ..
	sta     $AB                             ; A569 85 AB                    ..
	lda     #$00                            ; A56B A9 00                    ..
	sta     $A7                             ; A56D 85 A7                    ..
	sta     off_F4
	beq     LA532                           ; A571 F0 BF                    ..
LA573:  bit     $C9                             ; A573 24 C9                    $.
	bvc     LA58C                           ; A575 50 15                    P.
	lda     $9E                             ; A577 A5 9E                    ..
	sec                                     ; A579 38                       8
	sbc     #$0B                            ; A57A E9 0B                    ..
	sta     $9E                             ; A57C 85 9E                    ..
	bcs     LA582                           ; A57E B0 02                    ..
	dec     $9F                             ; A580 C6 9F                    ..
LA582:  lda     $9F                             ; A582 A5 9F                    ..
	lsr     a                               ; A584 4A                       J
	lda     $9E                             ; A585 A5 9E                    ..
	ror     a                               ; A587 6A                       j
	adc     #$00                            ; A588 69 00                    i.
	sta     $A6                             ; A58A 85 A6                    ..
LA58C:  lda     #$62                            ; A58C A9 62                    .b
	ldy     #byte_B8
	ldx     #$0C                            ; A590 A2 0C                    ..
	jsr     sub_b84b
	rts                                     ; A595 60                       `

; ----------------------------------------------------------------------------
LA596:  bvc     LA59C                           ; A596 50 04                    P.
	and     (off_E3),y
	bvs     LA59E                           ; A59A 70 02                    p.
LA59C:  ora     (off_E3),y
LA59E:  sta     (off_E3),y
	iny                                     ; A5A0 C8                       .
	rts                                     ; A5A1 60                       `

; ----------------------------------------------------------------------------
LA5A2:  lda     $A5                             ; A5A2 A5 A5                    ..
	lsr     a                               ; A5A4 4A                       J
	lda     $A4                             ; A5A5 A5 A4                    ..
	jmp     LA5AF                           ; A5A7 4C AF A5                 L..

; ----------------------------------------------------------------------------
LA5AA:  lda     $9D                             ; A5AA A5 9D                    ..
	lsr     a                               ; A5AC 4A                       J
	lda     $9C                             ; A5AD A5 9C                    ..
LA5AF:  ror     a                               ; A5AF 6A                       j
	lsr     a                               ; A5B0 4A                       J
	lsr     a                               ; A5B1 4A                       J
	sta     off_F4
	rts                                     ; A5B4 60                       `

; ----------------------------------------------------------------------------

sub_a5b5:
	lda     $DC                             ; A5B5 A5 DC                    ..
	and     #$04                            ; A5B7 29 04                    ).
	beq     LA5E3                           ; A5B9 F0 28                    .(
	lda     $DB                             ; A5BB A5 DB                    ..
	and     #$20                            ; A5BD 29 20                    ) 
	beq     LA5E4                           ; A5BF F0 23                    .#
	jsr     LA5F6                           ; A5C1 20 F6 A5                  ..
	ldx     $C1                             ; A5C4 A6 C1                    ..
	beq     LA5CD                           ; A5C6 F0 05                    ..
	bmi     LA5D1                           ; A5C8 30 07                    0.
	dex                                     ; A5CA CA                       .
	beq     LA5E3                           ; A5CB F0 16                    ..
LA5CD:  lda     #$01                            ; A5CD A9 01                    ..
	bne     LA5D7                           ; A5CF D0 06                    ..
LA5D1:  lda     #$FE                            ; A5D1 A9 FE                    ..
	ldx     #$00                            ; A5D3 A2 00                    ..
	beq     LA5D9                           ; A5D5 F0 02                    ..
LA5D7:  ldx     $C4                             ; A5D7 A6 C4                    ..
LA5D9:  sta     $C1                             ; A5D9 85 C1                    ..
	stx     HPOSM1
	inx                                     ; A5DE E8                       .
	inx                                     ; A5DF E8                       .
	stx     HPOSM0
LA5E3:  rts                                     ; A5E3 60                       `

; ----------------------------------------------------------------------------
LA5E4:  lda     $C1                             ; A5E4 A5 C1                    ..
	beq     LA5E3                           ; A5E6 F0 FB                    ..
	bmi     LA5F0                           ; A5E8 30 06                    0.
	lsr     a                               ; A5EA 4A                       J
	bne     LA5E3                           ; A5EB D0 F6                    ..
	tax                                     ; A5ED AA                       .
	beq     LA5D9                           ; A5EE F0 E9                    ..
LA5F0:  ldx     #$00                            ; A5F0 A2 00                    ..
	lda     #$FF                            ; A5F2 A9 FF                    ..
	bne     LA5D9                           ; A5F4 D0 E3                    ..
LA5F6:  ldx     #$00                            ; A5F6 A2 00                    ..
	stx     $D007                           ; A5F8 8E 07 D0                 ...
	stx     $D006                           ; A5FB 8E 06 D0                 ...
	rts                                     ; A5FE 60                       `

; ----------------------------------------------------------------------------

sub_a5ff:
	lda     $3E2E                           ; A5FF AD 2E 3E                 ..>
	cmp     #$50                            ; A602 C9 50                    .P
	bne     LA614                           ; A604 D0 0E                    ..
	lda     #$1B                            ; A606 A9 1B                    ..
	jsr     sub_ab54
	lda     #$50                            ; A60B A9 50                    .P
	jsr     sub_ab54
	lda     #$43                            ; A610 A9 43                    .C
	bne     LA63D                           ; A612 D0 29                    .)
LA614:  cmp     #$71                            ; A614 C9 71                    .q
	bne     LA621                           ; A616 D0 09                    ..
	lda     #$1B                            ; A618 A9 1B                    ..
	jsr     sub_ab54
	lda     #$4E                            ; A61D A9 4E                    .N
	bne     LA638                           ; A61F D0 17                    ..
LA621:  cmp     #$72                            ; A621 C9 72                    .r
	beq     LA631                           ; A623 F0 0C                    ..
	cmp     #$73                            ; A625 C9 73                    .s
	beq     LA631                           ; A627 F0 08                    ..
	cmp     #$7B                            ; A629 C9 7B                    .{
	bne     LA630                           ; A62B D0 03                    ..
	jsr     LB828                           ; A62D 20 28 B8                  (.
LA630:  rts                                     ; A630 60                       `

; ----------------------------------------------------------------------------
LA631:  lda     #$1B                            ; A631 A9 1B                    ..
	jsr     sub_ab54
	lda     #$40                            ; A636 A9 40                    .@
LA638:  jsr     sub_ab54
	lda     #$42                            ; A63B A9 42                    .B
LA63D:  jmp     sub_ab54

; ----------------------------------------------------------------------------

sub_a640:
	lda     $DC                             ; A640 A5 DC                    ..
	ldx     $DB                             ; A642 A6 DB                    ..
	and     #$07                            ; A644 29 07                    ).
	lsr     a                               ; A646 4A                       J
	sta     off_E3+1
	sta     $DC                             ; A649 85 DC                    ..
	txa                                     ; A64B 8A                       .
	ror     a                               ; A64C 6A                       j
	sta     off_E3
	lsr     $DC                             ; A64F 46 DC                    F.
	ror     a                               ; A651 6A                       j
	sta     $D9                             ; A652 85 D9                    ..
	ldx     $DC                             ; A654 A6 DC                    ..
	stx     $D8                             ; A656 86 D8                    ..
	lsr     $DC                             ; A658 46 DC                    F.
	ror     a                               ; A65A 6A                       j
	adc     $D9                             ; A65B 65 D9                    e.
	sta     off_F4
	lda     #$0C                            ; A65F A9 0C                    ..
	adc     $D8                             ; A661 65 D8                    e.
	sta     off_F4+1
	lda     off_E3
	adc     $D9                             ; A667 65 D9                    e.
	sta     off_E3
	lda     off_E3+1
	adc     $D8                             ; A66D 65 D8                    e.
	adc     #$06                            ; A66F 69 06                    i.
	sta     off_E3+1
LA673:  lda     #$00                            ; A673 A9 00                    ..
	sta     $D9                             ; A675 85 D9                    ..
	sta     $D8                             ; A677 85 D8                    ..
	ldx     #$1D                            ; A679 A2 1D                    ..
LA67B:  sta     $3E10,x                         ; A67B 9D 10 3E                 ..>
	dex                                     ; A67E CA                       .
	bpl     LA67B                           ; A67F 10 FA                    ..
	rts                                     ; A681 60                       `

; ----------------------------------------------------------------------------

sub_A682:
	jsr     sub_a8b3
	lda     $BA                             ; A685 A5 BA                    ..
	sta     $C9                             ; A687 85 C9                    ..
	bne     LA695                           ; A689 D0 0A                    ..
	jsr     LAD8E                           ; A68B 20 8E AD                  ..
	lda     $9C                             ; A68E A5 9C                    ..
	jsr     LA69A                           ; A690 20 9A A6                  ..
	dec     $C9                             ; A693 C6 C9                    ..
LA695:  jsr     LAD8E                           ; A695 20 8E AD                  ..
	lda     $A4                             ; A698 A5 A4                    ..
LA69A:  and     #$07                            ; A69A 29 07                    ).
	tax                                     ; A69C AA                       .
	ldy     #$00                            ; A69D A0 00                    ..
	jmp     sub_b004

; ----------------------------------------------------------------------------
LA6A2:  ldy     #$00                            ; A6A2 A0 00                    ..
	sty     $022F                           ; A6A4 8C 2F 02                 ./.
	ldx     #$1B                            ; A6A7 A2 1B                    ..
LA6A9:  lda     LA6D0,x                         ; A6A9 BD D0 A6                 ...
	sta     a:L0080,x                       ; A6AC 9D 80 00                 ...
	dex                                     ; A6AF CA                       .
	bpl     LA6A9                           ; A6B0 10 F7                    ..
	tya                                     ; A6B2 98                       .
	bit     $BA                             ; A6B3 24 BA                    $.
	jsr     L0080                           ; A6B5 20 80 00                  ..
	ldx     #$09                            ; A6B8 A2 09                    ..
LA6BA:  lda     LA6EC,x                         ; A6BA BD EC A6                 ...
	sta     a:$82,x                         ; A6BD 9D 82 00                 ...
	dex                                     ; A6C0 CA                       .
	bpl     LA6BA                           ; A6C1 10 F7                    ..
	ldx     #$08                            ; A6C3 A2 08                    ..
	lda     #$20                            ; A6C5 A9 20                    . 
	jsr     L0080                           ; A6C7 20 80 00                  ..
	lda     #$26                            ; A6CA A9 26                    .&
	sta     $022F                           ; A6CC 8D 2F 02                 ./.
	rts                                     ; A6CF 60                       `

; ----------------------------------------------------------------------------
LA6D0:  sta     L2000,y                         ; A6D0 99 00 20                 .. 
	bvs     LA6DE                           ; A6D3 70 09                    p.
	sta     L4000,y                         ; A6D5 99 00 40                 ..@
	sta     $6000,y                         ; A6D8 99 00 60                 ..`
	sta     $8000,y                         ; A6DB 99 00 80                 ...
LA6DE:  iny                                     ; A6DE C8                       .
	bne     LA6D0                           ; A6DF D0 EF                    ..
	inc     $82                             ; A6E1 E6 82                    ..
	inc     $87                             ; A6E3 E6 87                    ..
	inc     $8D                             ; A6E5 E6 8D                    ..
	inc     $8A                             ; A6E7 E6 8A                    ..
	.byte   $10                             ; A6E9 10                       .
LA6EA:  sbc     $60                             ; A6EA E5 60                    .`
LA6EC:  clc                                     ; A6EC 18                       .
	iny                                     ; A6ED C8                       .
	bne     LA6EA                           ; A6EE D0 FA                    ..
	inc     $82                             ; A6F0 E6 82                    ..
	dex                                     ; A6F2 CA                       .
	bne     LA6EA                           ; A6F3 D0 F5                    ..
	rts                                     ; A6F5 60                       `

; ----------------------------------------------------------------------------
	jsr     sub_a7fe
	ldx     $D8                             ; A6F9 A6 D8                    ..
	ldy     #$0F                            ; A6FB A0 0F                    ..
LA6FD:  lsr     $DC                             ; A6FD 46 DC                    F.
	ror     $DB                             ; A6FF 66 DB                    f.
	lda     LB93A,x                         ; A701 BD 3A B9                 .:.
	and     (off_E3),y
	bcc     LA726                           ; A706 90 1E                    ..
	inc     $D9                             ; A708 E6 D9                    ..
	pha                                     ; A70A 48                       H
	lda     LB9BE,y                         ; A70B B9 BE B9                 ...
	tax                                     ; A70E AA                       .
	lda     LB9F4,x                         ; A70F BD F4 B9                 ...
	sta     $AB                             ; A712 85 AB                    ..
	ldx     $D8                             ; A714 A6 D8                    ..
	lda     LB9CE,x                         ; A716 BD CE B9                 ...
	clc                                     ; A719 18                       .
	adc     $AB                             ; A71A 65 AB                    e.
	tax                                     ; A71C AA                       .
	inc     $3E10,x                         ; A71D FE 10 3E                 ..>
	pla                                     ; A720 68                       h
	ldx     $D8                             ; A721 A6 D8                    ..
	ora     LB942,x                         ; A723 1D 42 B9                 .B.
LA726:  sta     (off_E3),y
	dey                                     ; A728 88                       .
	bpl     LA6FD                           ; A729 10 D2                    ..
	inx                                     ; A72B E8                       .
	cpx     #$08                            ; A72C E0 08                    ..
	bcs     LA733                           ; A72E B0 03                    ..
	stx     $D8                             ; A730 86 D8                    ..
	rts                                     ; A732 60                       `

; ----------------------------------------------------------------------------
LA733:  lda     #$00                            ; A733 A9 00                    ..
	tax                                     ; A735 AA                       .
	ldy     #$05                            ; A736 A0 05                    ..
LA738:  sta     (off_F4),y
	dey                                     ; A73A 88                       .
	bpl     LA738                           ; A73B 10 FB                    ..
	lda     $D9                             ; A73D A5 D9                    ..
	cmp     #$36                            ; A73F C9 36                    .6
	bcc     LA771                           ; A741 90 2E                    ..
	dex                                     ; A743 CA                       .
	cmp     #$55                            ; A744 C9 55                    .U
	bcs     LA771                           ; A746 B0 29                    .)
	ldy     #$05                            ; A748 A0 05                    ..
LA74A:  ldx     #$04                            ; A74A A2 04                    ..
	stx     $AB                             ; A74C 86 AB                    ..
LA74E:  lda     LB9F4,y                         ; A74E B9 F4 B9                 ...
	clc                                     ; A751 18                       .
	adc     $AB                             ; A752 65 AB                    e.
	tax                                     ; A754 AA                       .
	lda     LB9D6,x                         ; A755 BD D6 B9                 ...
	cmp     $3E10,x                         ; A758 DD 10 3E                 ..>
	bcc	:+
	bne     LA768                           ; A75D D0 09                    ..
:	lda     (off_F4),y
	ldx     $AB                             ; A761 A6 AB                    ..
	ora     LB942,x                         ; A763 1D 42 B9                 .B.
	sta     (off_F4),y
LA768:  dec     $AB                             ; A768 C6 AB                    ..
	bpl     LA74E                           ; A76A 10 E2                    ..
	dey                                     ; A76C 88                       .
	bpl     LA74A                           ; A76D 10 DB                    ..
	bmi     LA7AD                           ; A76F 30 3C                    0<
LA771:  stx     $D9                             ; A771 86 D9                    ..
	bit     $D9                             ; A773 24 D9                    $.
	ldy     #$0F                            ; A775 A0 0F                    ..
LA777:  lda     (off_E3),y
	bvc     LA77D                           ; A779 50 02                    P.
	eor     #$FF                            ; A77B 49 FF                    I.
LA77D:  sta     $D8                             ; A77D 85 D8                    ..
	ldx     #$07                            ; A77F A2 07                    ..
	lda     #$00                            ; A781 A9 00                    ..
LA783:  rol     $D8                             ; A783 26 D8                    &.
	bcc     LA78A                           ; A785 90 03                    ..
	ora     LB9B6,x                         ; A787 1D B6 B9                 ...
LA78A:  dex                                     ; A78A CA                       .
	bpl     LA783                           ; A78B 10 F6                    ..
	pha                                     ; A78D 48                       H
	lda     LB9BE,y                         ; A78E B9 BE B9                 ...
	sty     $D8                             ; A791 84 D8                    ..
	tay                                     ; A793 A8                       .
	pla                                     ; A794 68                       h
	ora     (off_F4),y
	sta     (off_F4),y
	ldy     $D8                             ; A799 A4 D8                    ..
	dey                                     ; A79B 88                       .
	bpl     LA777                           ; A79C 10 D9                    ..
	bvc     LA7AD                           ; A79E 50 0D                    P.
	ldy     #$05                            ; A7A0 A0 05                    ..
LA7A2:  lda     (off_F4),y
	eor     #$FF                            ; A7A4 49 FF                    I.
	and     #$F8                            ; A7A6 29 F8                    ).
	sta     (off_F4),y
	dey                                     ; A7AA 88                       .
	bpl     LA7A2                           ; A7AB 10 F5                    ..
LA7AD:  ldy     #$02                            ; A7AD A0 02                    ..
	sty     $EE                             ; A7AF 84 EE                    ..
	iny                                     ; A7B1 C8                       .
	sty     $EC                             ; A7B2 84 EC                    ..
LA7B4:  ldy     $EC                             ; A7B4 A4 EC                    ..
	lda     (off_E3),y
	ldy     $EE                             ; A7B8 A4 EE                    ..
	bvc     LA7C0                           ; A7BA 50 04                    P.
	and     (off_E3),y
	bvs     LA7C2                           ; A7BE 70 02                    p.
LA7C0:  ora     (off_E3),y
LA7C2:  sta     (off_E3),y
	ldx     #$02                            ; A7C4 A2 02                    ..
LA7C6:  inc     $EE                             ; A7C6 E6 EE                    ..
	inc     $EC                             ; A7C8 E6 EC                    ..
	ldy     $EC                             ; A7CA A4 EC                    ..
	cpy     #$10                            ; A7CC C0 10                    ..
	bcs     LA7DD                           ; A7CE B0 0D                    ..
	lda     (off_E3),y
	ldy     $EE                             ; A7D2 A4 EE                    ..
	sta     (off_E3),y
	dex                                     ; A7D6 CA                       .
	bpl     LA7C6                           ; A7D7 10 ED                    ..
	inc     $EC                             ; A7D9 E6 EC                    ..
	bne     LA7B4                           ; A7DB D0 D7                    ..
LA7DD:  lda     #$0C                            ; A7DD A9 0C                    ..
	clc                                     ; A7DF 18                       .
	adc     off_E3
	sta     off_E3
	bcc     LA7E8                           ; A7E4 90 02                    ..
	inc     off_E3+1
LA7E8:  clc                                     ; A7E8 18                       .
	lda     off_F4
	adc     #$06                            ; A7EB 69 06                    i.
	sta     off_F4
	bcc	:+
	inc     off_F4+1
:	jmp     LA673                           ; A7F3 4C 73 A6                 Ls.

; ----------------------------------------------------------------------------
sub_a7f6:
	ldx     $B6                             ; A7F6 A6 B6                    ..
	lda     $3E2E,x                         ; A7F8 BD 2E 3E                 ..>
	inc     $B6                             ; A7FB E6 B6                    ..
	rts                                     ; A7FD 60                       `

; ----------------------------------------------------------------------------
sub_a7fe:
	jsr     sub_a7f6
	and     #$3F                            ; A801 29 3F                    )?
	sta     $DB                             ; A803 85 DB                    ..
	jsr     sub_a7f6
	and     #$3F                            ; A808 29 3F                    )?
	sta     $DC                             ; A80A 85 DC                    ..
	lda     #$00                            ; A80C A9 00                    ..
	lsr     $DC                             ; A80E 46 DC                    F.
	ror     a                               ; A810 6A                       j
	lsr     $DC                             ; A811 46 DC                    F.
	ror     a                               ; A813 6A                       j
	ora     $DB                             ; A814 05 DB                    ..
	sta     $DB                             ; A816 85 DB                    ..
	jsr     sub_a7f6
	asl     a                               ; A81B 0A                       .
	asl     a                               ; A81C 0A                       .
	asl     a                               ; A81D 0A                       .
	asl     a                               ; A81E 0A                       .
	ora     $DC                             ; A81F 05 DC                    ..
	sta     $DC                             ; A821 85 DC                    ..
	rts                                     ; A823 60                       `

; ----------------------------------------------------------------------------
LA824:  lda     #$02                            ; A824 A9 02                    ..
	ldy     #$04                            ; A826 A0 04                    ..
	jsr     LA886                           ; A828 20 86 A8                  ..
	bcs     LA857                           ; A82B B0 2A                    .*
LA82D:  adc     $A6                             ; A82D 65 A6                    e.
	sta     $A6                             ; A82F 85 A6                    ..
	lda     $D8                             ; A831 A5 D8                    ..
	clc                                     ; A833 18                       .
	adc     $9E                             ; A834 65 9E                    e.
	sta     $9E                             ; A836 85 9E                    ..
	bcc     LA83C                           ; A838 90 02                    ..
	inc     $9F                             ; A83A E6 9F                    ..
LA83C:  .byte   $A5                             ; A83C A5                       .
LA83D:  .byte   $9F                             ; A83D 9F                       .
	cmp     #$01                            ; A83E C9 01                    ..
	bcc     LA856                           ; A840 90 14                    ..
	lda     $9E                             ; A842 A5 9E                    ..
	cmp     #$80                            ; A844 C9 80                    ..
	bcc     LA856                           ; A846 90 0E                    ..
	sec                                     ; A848 38                       8
	sbc     #$80                            ; A849 E9 80                    ..
	sta     $9E                             ; A84B 85 9E                    ..
	lsr     $9F                             ; A84D 46 9F                    F.
	sec                                     ; A84F 38                       8
	lda     $A6                             ; A850 A5 A6                    ..
	sbc     #$C0                            ; A852 E9 C0                    ..
	sta     $A6                             ; A854 85 A6                    ..
LA856:  rts                                     ; A856 60                       `

; ----------------------------------------------------------------------------
LA857:  sta     $D9                             ; A857 85 D9                    ..
	lda     $A6                             ; A859 A5 A6                    ..
	sec                                     ; A85B 38                       8
	sbc     $D9                             ; A85C E5 D9                    ..
	sta     $A6                             ; A85E 85 A6                    ..
	sec                                     ; A860 38                       8
	lda     $9E                             ; A861 A5 9E                    ..
	sbc     $D8                             ; A863 E5 D8                    ..
	sta     $9E                             ; A865 85 9E                    ..
	bcs     LA86B                           ; A867 B0 02                    ..
	dec     $9F                             ; A869 C6 9F                    ..
LA86B:  lda     $9F                             ; A86B A5 9F                    ..
	bpl     LA881                           ; A86D 10 12                    ..
	lda     $9E                             ; A86F A5 9E                    ..
	clc                                     ; A871 18                       .
	adc     #$80                            ; A872 69 80                    i.
	sta     $9E                             ; A874 85 9E                    ..
	lda     #$01                            ; A876 A9 01                    ..
	sta     $9F                             ; A878 85 9F                    ..
	lda     $A6                             ; A87A A5 A6                    ..
LA87C:  clc                                     ; A87C 18                       .
	adc     #$C0                            ; A87D 69 C0                    i.
	sta     $A6                             ; A87F 85 A6                    ..
LA881:  rts                                     ; A881 60                       `

; ----------------------------------------------------------------------------
LA882:  lda     #$06                            ; A882 A9 06                    ..
	ldy     #$0C                            ; A884 A0 0C                    ..
LA886:  bit     $CA                             ; A886 24 CA                    $.
	sty     $D8                             ; A888 84 D8                    ..
	bvs     LA88D                           ; A88A 70 01                    p.
	rts                                     ; A88C 60                       `

; ----------------------------------------------------------------------------
LA88D:  asl     a                               ; A88D 0A                       .
	bvs     LA89C                           ; A88E 70 0C                    p.
LA890:  bit     $CA                             ; A890 24 CA                    $.
	lda     #$05                            ; A892 A9 05                    ..
	ldy     #$08                            ; A894 A0 08                    ..
	sty     $D8                             ; A896 84 D8                    ..
	bvc     LA89E                           ; A898 50 04                    P.
	lda     #$09                            ; A89A A9 09                    ..
LA89C:  asl     $D8                             ; A89C 06 D8                    ..
LA89E:  rts                                     ; A89E 60                       `

; ----------------------------------------------------------------------------
sub_a89f:
	sty     $EE                             ; A89F 84 EE                    ..
	sta     off_EC
	stx     off_EC+1
:	ldy     $EE                             ; A8A5 A4 EE                    ..
	lda     (off_EC),y
	jsr     LAB5D                           ; A8A9 20 5D AB                  ].
	dec     $EE                             ; A8AC C6 EE                    ..
	bpl     :-
	jmp     LA3B2                           ; A8B0 4C B2 A3                 L..

; ----------------------------------------------------------------------------
sub_a8b3:
	jsr     sub_a7f6
	cmp     #$40                            ; A8B6 C9 40                    .@
	bcs     LA901                           ; A8B8 B0 47                    .G
	jsr     LA8DC                           ; A8BA 20 DC A8                  ..
	lda     $BD                             ; A8BD A5 BD                    ..
	and     #$1F                            ; A8BF 29 1F                    ).
	ora     $DB                             ; A8C1 05 DB                    ..
	sta     $BD                             ; A8C3 85 BD                    ..
	lda     $DC                             ; A8C5 A5 DC                    ..
	sta     $BE                             ; A8C7 85 BE                    ..
	bcc     sub_a8b3
LA8CB:  jsr     LA8DC                           ; A8CB 20 DC A8                  ..
	lda     $BB                             ; A8CE A5 BB                    ..
	and     #$1F                            ; A8D0 29 1F                    ).
	ora     $DB                             ; A8D2 05 DB                    ..
	sta     $BB                             ; A8D4 85 BB                    ..
	lda     $DC                             ; A8D6 A5 DC                    ..
	sta     $BC                             ; A8D8 85 BC                    ..
	bcc     sub_a8b3
LA8DC:  and     #$1F                            ; A8DC 29 1F                    ).
	sta     $DC                             ; A8DE 85 DC                    ..
	lda     #$00                            ; A8E0 A9 00                    ..
	lsr     $DC                             ; A8E2 46 DC                    F.
	ror     a                               ; A8E4 6A                       j
	lsr     $DC                             ; A8E5 46 DC                    F.
	ror     a                               ; A8E7 6A                       j
	lsr     $DC                             ; A8E8 46 DC                    F.
	ror     a                               ; A8EA 6A                       j
	sta     $DB                             ; A8EB 85 DB                    ..
	rts                                     ; A8ED 60                       `

; ----------------------------------------------------------------------------
LA8EE:  and     #$1F                            ; A8EE 29 1F                    ).
	sta     $DB                             ; A8F0 85 DB                    ..
	lda     $BD                             ; A8F2 A5 BD                    ..
	and     #$E0                            ; A8F4 29 E0                    ).
	ora     $DB                             ; A8F6 05 DB                    ..
	sta     $BD                             ; A8F8 85 BD                    ..
	jsr     sub_a7f6
	cmp     #$40                            ; A8FD C9 40                    .@
	bcc     LA8CB                           ; A8FF 90 CA                    ..
LA901:  cmp     #$60                            ; A901 C9 60                    .`
	bcs     LA8EE                           ; A903 B0 E9                    ..
	and     #$1F                            ; A905 29 1F                    ).
	sta     $DB                             ; A907 85 DB                    ..
	lda     $BB                             ; A909 A5 BB                    ..
	and     #$E0                            ; A90B 29 E0                    ).
	ora     $DB                             ; A90D 05 DB                    ..
	sta     $BB                             ; A90F 85 BB                    ..
	ldx     #$03                            ; A911 A2 03                    ..
LA913:  lda     $BB,x                           ; A913 B5 BB                    ..
	sta     $9C,x                           ; A915 95 9C                    ..
	dex                                     ; A917 CA                       .
	bpl     LA913                           ; A918 10 F9                    ..
	lda     $9D                             ; A91A A5 9D                    ..
	lsr     a                               ; A91C 4A                       J
	lda     $9C                             ; A91D A5 9C                    ..
	sta     $D8                             ; A91F 85 D8                    ..
	ror     a                               ; A921 6A                       j
	sta     $D9                             ; A922 85 D9                    ..
	lsr     a                               ; A924 4A                       J
	lsr     a                               ; A925 4A                       J
	bcs     LA92A                           ; A926 B0 02                    ..
	lsr     $D8                             ; A928 46 D8                    F.
LA92A:  adc     $D9                             ; A92A 65 D9                    e.
	sta     $A4                             ; A92C 85 A4                    ..
	ldx     #$00                            ; A92E A2 00                    ..
	stx     $A5                             ; A930 86 A5                    ..
	bcc     LA936                           ; A932 90 02                    ..
	inc     $A5                             ; A934 E6 A5                    ..
LA936:  lda     $9E                             ; A936 A5 9E                    ..
	and     #$07                            ; A938 29 07                    ).
	tax                                     ; A93A AA                       .
	lsr     $9F                             ; A93B 46 9F                    F.
	lda     $9E                             ; A93D A5 9E                    ..
	ror     a                               ; A93F 6A                       j
	sta     $D8                             ; A940 85 D8                    ..
	lsr     a                               ; A942 4A                       J
	sta     $D9                             ; A943 85 D9                    ..
	lsr     a                               ; A945 4A                       J
	clc                                     ; A946 18                       .
	adc     LB9FA,x                         ; A947 7D FA B9                 }..
	adc     $D9                             ; A94A 65 D9                    e.
	cmp     #$C0                            ; A94C C9 C0                    ..
	bcc     LA952                           ; A94E 90 02                    ..
	lda     #$BF                            ; A950 A9 BF                    ..
LA952:  sta     $A6                             ; A952 85 A6                    ..
	lda     $9E                             ; A954 A5 9E                    ..
	and     #$03                            ; A956 29 03                    ).
	cmp     #$01                            ; A958 C9 01                    ..
	lda     $D8                             ; A95A A5 D8                    ..
	adc     $D9                             ; A95C 65 D9                    e.
	sta     $9E                             ; A95E 85 9E                    ..
	rol     $9F                             ; A960 26 9F                    &.
LA962:  rts                                     ; A962 60                       `

; ----------------------------------------------------------------------------
sub_a963:  
	ldx     CH
	inx                                     ; A966 E8                       .
	bne     LA976                           ; A967 D0 0D                    ..
	lda     $02DC                           ; A969 AD DC 02                 ...
	beq     LA962                           ; A96C F0 F4                    ..
	stx     $02DC                           ; A96E 8E DC 02                 ...
	lda     #$0B                            ; A971 A9 0B                    ..
	jmp     sub_b1df

; ----------------------------------------------------------------------------
LA976:  dex                                     ; A976 CA                       .
	bmi     LA9CE                           ; A977 30 55                    0U
	stx     $D8                             ; A979 86 D8                    ..
	lda     CONSOL
	lsr     a                               ; A97E 4A                       J
	bcs     LA9AB                           ; A97F B0 2A                    .*
	ldy     #$FF                            ; A981 A0 FF                    ..
	lsr     a                               ; A983 4A                       J
	bcc     LA98A                           ; A984 90 04                    ..
	cpx     #$40                            ; A986 E0 40                    .@
	bcc     LA98B                           ; A988 90 01                    ..
LA98A:  iny                                     ; A98A C8                       .
LA98B:  sty     $D9                             ; A98B 84 D9                    ..
	lda     $D8                             ; A98D A5 D8                    ..
	and     #$3F                            ; A98F 29 3F                    )?
	ldy     #$30                            ; A991 A0 30                    .0
LA993:  cmp     LBA12,y                         ; A993 D9 12 BA                 ...
	beq     LA99F                           ; A996 F0 07                    ..
	dey                                     ; A998 88                       .
	dey                                     ; A999 88                       .
	dey                                     ; A99A 88                       .
	bpl     LA993                           ; A99B 10 F6                    ..
	bmi     LA9CE                           ; A99D 30 2F                    0/
LA99F:  ldx     $D9                             ; A99F A6 D9                    ..
	bmi     LA9A4                           ; A9A1 30 01                    0.
	iny                                     ; A9A3 C8                       .
LA9A4:  iny                                     ; A9A4 C8                       .
	lda     LBA12,y                         ; A9A5 B9 12 BA                 ...
LA9A8:  jmp     LAA91                           ; A9A8 4C 91 AA                 L..

; ----------------------------------------------------------------------------
LA9AB:  lda     $D8                             ; A9AB A5 D8                    ..
	cmp     #$27                            ; A9AD C9 27                    .'
	bne     LA9B5                           ; A9AF D0 04                    ..
	lda     #$7B                            ; A9B1 A9 7B                    .{
	.byte   $D0                             ; A9B3 D0                       .
LA9B4:  .byte   $F3                             ; A9B4 F3                       .
LA9B5:  cmp     #$67                            ; A9B5 C9 67                    .g
	bne     LA9BD                           ; A9B7 D0 04                    ..
	lda     #$7F                            ; A9B9 A9 7F                    ..
	bne     LA9A8                           ; A9BB D0 EB                    ..
LA9BD:  cmp     #$3C                            ; A9BD C9 3C                    .<
	bne     LA9C5                           ; A9BF D0 04                    ..
	lda     #$00                            ; A9C1 A9 00                    ..
	beq     LA9CB                           ; A9C3 F0 06                    ..
LA9C5:  cmp     #$7C                            ; A9C5 C9 7C                    .|
	bne     LA9DD                           ; A9C7 D0 14                    ..
	lda     #$40                            ; A9C9 A9 40                    .@
LA9CB:  sta     $02BE                           ; A9CB 8D BE 02                 ...
LA9CE:  ldx     #$7F                            ; A9CE A2 7F                    ..
LA9D0:  stx     CONSOL
	stx     $D40A                           ; A9D3 8E 0A D4                 ...
	dex                                     ; A9D6 CA                       .
	bpl     LA9D0                           ; A9D7 10 F7                    ..
	stx     CH
	rts                                     ; A9DC 60                       `

; ----------------------------------------------------------------------------
LA9DD:  ldy     #$10                            ; A9DD A0 10                    ..
	jsr     sub_b1f1
	sta     $E7                             ; A9E2 85 E7                    ..
	lda     CONSOL				; check console keys
	and     #$07				; mask irrelevant bits
	cmp     #$03				; is OPTION pressed?
	bne     LAA62				; no, skip out
	lda     SRTIMR				; check key repeat timer
	beq     LAA62                           ; A9F0 F0 70                    .p
	lda     $D8                             ; A9F2 A5 D8                    ..
	and     #$3F                            ; A9F4 29 3F                    )?
	cmp     #$32                            ; A9F6 C9 32                    .2
	bne     LAA03                           ; A9F8 D0 09                    ..
	lda     $1353                           ; A9FA AD 53 13                 .S.
	eor     #$80                            ; A9FD 49 80                    I.
	sta     $1353                           ; A9FF 8D 53 13                 .S.
	rts                                     ; AA02 60                       `

; ----------------------------------------------------------------------------
LAA03:  cmp     #$1F                            ; AA03 C9 1F                    ..
	bne     LAA12                           ; AA05 D0 0B                    ..
	lda     #$00                            ; AA07 A9 00                    ..
LAA09:  ldy     byte_B2
	bne     LAA23                           ; AA0B D0 16                    ..
	sta     $B1                             ; AA0D 85 B1                    ..
	jmp     LB253                           ; AA0F 4C 53 B2                 LS.

; ----------------------------------------------------------------------------
LAA12:  cmp     #$1A                            ; AA12 C9 1A                    ..
	bne     LAA1A                           ; AA14 D0 04                    ..
	lda     #$FF                            ; AA16 A9 FF                    ..
	bne     LAA09                           ; AA18 D0 EF                    ..
LAA1A:  cmp     #$12                            ; AA1A C9 12                    ..
	bne     LAA24                           ; AA1C D0 06                    ..
	lda     #$00                            ; AA1E A9 00                    ..
LAA20:  sta     $1355                           ; AA20 8D 55 13                 .U.
LAA23:  rts                                     ; AA23 60                       `

; ----------------------------------------------------------------------------
LAA24:  cmp     #$15                            ; AA24 C9 15                    ..
	bne     LAA2C                           ; AA26 D0 04                    ..
	lda     #$80                            ; AA28 A9 80                    ..
	bne     LAA20                           ; AA2A D0 F4                    ..
LAA2C:  cmp     #$2D                            ; AA2C C9 2D                    .-
	bne     LAA34                           ; AA2E D0 04                    ..
	lda     #$C0                            ; AA30 A9 C0                    ..
	bne     LAA20                           ; AA32 D0 EC                    ..
LAA34:  cmp     #$38                            ; AA34 C9 38                    .8
	bne     LAA56                           ; AA36 D0 1E                    ..
	ldx     byte_B8
	beq     LAA56                           ; AA3A F0 1A                    ..
	lda     $C1                             ; AA3C A5 C1                    ..
	beq     LAA44                           ; AA3E F0 04                    ..
	cmp     #$02                            ; AA40 C9 02                    ..
	bne     LAA23                           ; AA42 D0 DF                    ..
LAA44:  eor     #$02                            ; AA44 49 02                    I.
	sta     $C1                             ; AA46 85 C1                    ..
	tax                                     ; AA48 AA                       .
	beq     LAA4D                           ; AA49 F0 02                    ..
	ldx     #$32                            ; AA4B A2 32                    .2
LAA4D:  stx     $D007                           ; AA4D 8E 07 D0                 ...
	inx                                     ; AA50 E8                       .
	inx                                     ; AA51 E8                       .
	stx     $D006                           ; AA52 8E 06 D0                 ...
	rts                                     ; AA55 60                       `

; ----------------------------------------------------------------------------
LAA56:  cmp     #$17                            ; AA56 C9 17                    ..
	beq     LAAAD                           ; AA58 F0 53                    .S
	cmp     #$25                            ; AA5A C9 25                    .%
	beq     LAAA7                           ; AA5C F0 49                    .I
	cmp     #$0A                            ; AA5E C9 0A                    ..
	beq     LAAAA                           ; AA60 F0 48                    .H
LAA62:  ldy     #$0C                            ; AA62 A0 0C                    ..
	lda     $D8                             ; AA64 A5 D8                    ..
LAA66:  cmp     LBA45,y                         ; AA66 D9 45 BA                 .E.
	beq     LAA8A                           ; AA69 F0 1F                    ..
	dey                                     ; AA6B 88                       .
	dey                                     ; AA6C 88                       .
	bpl     LAA66                           ; AA6D 10 F7                    ..
	ldy     #$0A                            ; AA6F A0 0A                    ..
LAA71:  cmp     LBA53,y                         ; AA71 D9 53 BA                 .S.
	beq     LAA7C                           ; AA74 F0 06                    ..
	dey                                     ; AA76 88                       .
	dey                                     ; AA77 88                       .
	bpl     LAA71                           ; AA78 10 F7                    ..
	bmi     LAA96                           ; AA7A 30 1A                    0.
LAA7C:  sty     $E7                             ; AA7C 84 E7                    ..
	lda     #$00                            ; AA7E A9 00                    ..
	jsr     sub_ab54
	ldy     $E7                             ; AA83 A4 E7                    ..
	lda     LBA54,y                         ; AA85 B9 54 BA                 .T.
	bne     LAA8D                           ; AA88 D0 03                    ..
LAA8A:  lda     LBA46,y                         ; AA8A B9 46 BA                 .F.
LAA8D:  sta     $E7                             ; AA8D 85 E7                    ..
	bne     LAA96                           ; AA8F D0 05                    ..
LAA91:  sta     $E7                             ; AA91 85 E7                    ..
	jsr     LA9CE                           ; AA93 20 CE A9                  ..
LAA96:  bit     $1353                           ; AA96 2C 53 13                 ,S.
	bmi     LAB00                           ; AA99 30 65                    0e
	lda     $E7                             ; AA9B A5 E7                    ..
	cmp     #$20                            ; AA9D C9 20                    . 
	bcs     LAAFD                           ; AA9F B0 5C                    .\
	jsr     LA0F3                           ; AAA1 20 F3 A0                  ..
	jmp     LAB00                           ; AAA4 4C 00 AB                 L..

; ----------------------------------------------------------------------------
LAAA7:  jmp     sub_b272

; ----------------------------------------------------------------------------
LAAAA:  jmp     LB28A                           ; AAAA 4C 8A B2                 L..

; ----------------------------------------------------------------------------
LAAAD:  jsr     LA5F6                           ; AAAD 20 F6 A5                  ..
	lda     $C1                             ; AAB0 A5 C1                    ..
	cmp     #$02                            ; AAB2 C9 02                    ..
	bne     LAAB8                           ; AAB4 D0 02                    ..
	lda     #$00                            ; AAB6 A9 00                    ..
LAAB8:  eor     #$FF                            ; AAB8 49 FF                    I.
	sta     $C1                             ; AABA 85 C1                    ..
	bmi     LAADB                           ; AABC 30 1D                    0.
	beq     LAACA                           ; AABE F0 0A                    ..
	ldx     $C4                             ; AAC0 A6 C4                    ..
	stx     HPOSM1
	inx                                     ; AAC5 E8                       .
	inx                                     ; AAC6 E8                       .
	stx     HPOSM0
LAACA:  lda     $D2                             ; AACA A5 D2                    ..
	sta     $02C5                           ; AACC 8D C5 02                 ...
	ldy     #$80                            ; AACF A0 80                    ..
	sty     byte_B8
	ldy     $D3                             ; AAD3 A4 D3                    ..
	lda     #$00                            ; AAD5 A9 00                    ..
	ldx     #$10                            ; AAD7 A2 10                    ..
	bne     LAAF0                           ; AAD9 D0 15                    ..
LAADB:  ldx     #$00                            ; AADB A2 00                    ..
	stx     HPOSM1
	stx     HPOSM0
	stx     byte_B8
	lda     $D0                             ; AAE5 A5 D0                    ..
	sta     $02C5                           ; AAE7 8D C5 02                 ...
	ldy     $D1                             ; AAEA A4 D1                    ..
	lda     #<$10CA
	ldx     #>$10CA
LAAF0:  sta	DLIST
	stx	DLIST+1
	sty     $02C6                           ; AAF6 8C C6 02                 ...
	sty     $02C8                           ; AAF9 8C C8 02                 ...
	rts                                     ; AAFC 60                       `

; ----------------------------------------------------------------------------
LAAFD:  jsr     LAB5D                           ; AAFD 20 5D AB                  ].
LAB00:  lda     $E7                             ; AB00 A5 E7                    ..
LAB02:  jmp     sub_b1df

; ----------------------------------------------------------------------------
LAB05:  dey                                     ; AB05 88                       .
	sty     $C7                             ; AB06 84 C7                    ..
	txa                                     ; AB08 8A                       .
	dex                                     ; AB09 CA                       .
	bmi     LAAAD                           ; AB0A 30 A1                    0.
	bne     LAB53                           ; AB0C D0 45                    .E
	jsr     LB828                           ; AB0E 20 28 B8                  (.
	lda     #$1B                            ; AB11 A9 1B                    ..
	jsr     sub_ab54
	lda     #$00                            ; AB16 A9 00                    ..
	sta     $E7                             ; AB18 85 E7                    ..
	sta     $4D                             ; AB1A 85 4D                    .M
	lda     $BF                             ; AB1C A5 BF                    ..
	lsr     a                               ; AB1E 4A                       J
	ror     $E7                             ; AB1F 66 E7                    f.
	lsr     a                               ; AB21 4A                       J
	ror     $E7                             ; AB22 66 E7                    f.
	pha                                     ; AB24 48                       H
	lda     $E7                             ; AB25 A5 E7                    ..
	lsr     a                               ; AB27 4A                       J
	lsr     a                               ; AB28 4A                       J
	ora     #$40                            ; AB29 09 40                    .@
	ora     $C0                             ; AB2B 05 C0                    ..
	jsr     sub_ab54
	pla                                     ; AB30 68                       h
	ora     #$44                            ; AB31 09 44                    .D
	bne     LAB02                           ; AB33 D0 CD                    ..
LAB35:  ldx     $C1                             ; AB35 A6 C1                    ..
	cpx     #$02                            ; AB37 E0 02                    ..
	bne     LAB4F                           ; AB39 D0 14                    ..
	lda     $C8                             ; AB3B A5 C8                    ..
	beq     LAB4F                           ; AB3D F0 10                    ..
	ldx     #$00                            ; AB3F A2 00                    ..
	stx     $C8                             ; AB41 86 C8                    ..
	stx     $4D                             ; AB43 86 4D                    .M
	dex                                     ; AB45 CA                       .
	stx     $C7                             ; AB46 86 C7                    ..
	pha                                     ; AB48 48                       H
	jsr     LB828                           ; AB49 20 28 B8                  (.
	pla                                     ; AB4C 68                       h
	bne     LAB02                           ; AB4D D0 B3                    ..
LAB4F:  ldy     $C7                             ; AB4F A4 C7                    ..
	beq     LAB05                           ; AB51 F0 B2                    ..
LAB53:  rts                                     ; AB53 60                       `

; ----------------------------------------------------------------------------
sub_ab54:  	
	jsr     sub_b1df
	jmp     sub_b54f

; ----------------------------------------------------------------------------
	lda     $3E2E                           ; AB5A AD 2E 3E                 ..>
LAB5D:  sta     $E7                             ; AB5D 85 E7                    ..
	sec                                     ; AB5F 38                       8
	sbc     #$20                            ; AB60 E9 20                    . 
	bcs     LAB65                           ; AB62 B0 01                    ..
	rts                                     ; AB64 60                       `

; ----------------------------------------------------------------------------
LAB65:  pha                                     ; AB65 48                       H
	ldx     $BA                             ; AB66 A6 BA                    ..
	stx     $C9                             ; AB68 86 C9                    ..
	bne     LAB77                           ; AB6A D0 0B                    ..
	stx     $E6                             ; AB6C 86 E6                    ..
	stx     $D8                             ; AB6E 86 D8                    ..
	jsr     LABFF                           ; AB70 20 FF AB                  ..
	dec     $C9                             ; AB73 C6 C9                    ..
	ldx     $C9                             ; AB75 A6 C9                    ..
LAB77:  pla                                     ; AB77 68                       h
	inx                                     ; AB78 E8                       .
	stx     $E6                             ; AB79 86 E6                    ..
	ldx     $CA                             ; AB7B A6 CA                    ..
	beq     LAB94                           ; AB7D F0 15                    ..
	jsr     LAB88                           ; AB7F 20 88 AB                  ..
	ldx     #$FF                            ; AB82 A2 FF                    ..
	stx     $CA                             ; AB84 86 CA                    ..
	bne     LABD3                           ; AB86 D0 4B                    .K
LAB88:  inc     $A4                             ; AB88 E6 A4                    ..
	bne     LAB8E                           ; AB8A D0 02                    ..
	inc     $A5                             ; AB8C E6 A5                    ..
LAB8E:  ldx     #$7F                            ; AB8E A2 7F                    ..
	stx     $CA                             ; AB90 86 CA                    ..
	bne     LABFF                           ; AB92 D0 6B                    .k
LAB94:  jsr     LAD05                           ; AB94 20 05 AD                  ..
	bit     $B0                             ; AB97 24 B0                    $.
	bvs     LABD3                           ; AB99 70 38                    p8
	jsr     LA5AA                           ; AB9B 20 AA A5                  ..
	lda     $9C                             ; AB9E A5 9C                    ..
	and     #$04                            ; ABA0 29 04                    ).
	beq     LABA6                           ; ABA2 F0 02                    ..
	inc     off_F4
LABA6:  lda     $A6                             ; ABA6 A5 A6                    ..
	ldy     #$20                            ; ABA8 A0 20                    . 
	sec                                     ; ABAA 38                       8
LABAB:  sbc     #$06                            ; ABAB E9 06                    ..
	dey                                     ; ABAD 88                       .
	bcs     LABAB                           ; ABAE B0 FB                    ..
	tya                                     ; ABB0 98                       .
	and     #$03                            ; ABB1 29 03                    ).
	tax                                     ; ABB3 AA                       .
	tya                                     ; ABB4 98                       .
	lsr     a                               ; ABB5 4A                       J
	lsr     a                               ; ABB6 4A                       J
	clc                                     ; ABB7 18                       .
	adc     #$18                            ; ABB8 69 18                    i.
	sta     off_E3+1
	lda     LB936,x                         ; ABBC BD 36 B9                 .6.
	sta     off_E3
	lda     $E7                             ; ABC1 A5 E7                    ..
	ldy     off_F4
	cmp     #$5F                            ; ABC5 C9 5F                    ._
	bne     LABD1                           ; ABC7 D0 08                    ..
	lda     (off_E3),y
	cmp     #$20                            ; ABCB C9 20                    . 
	bne     LABD3                           ; ABCD D0 04                    ..
	lda     #$5F                            ; ABCF A9 5F                    ._
LABD1:  sta     (off_E3),y
LABD3:  jsr     LA890                           ; ABD3 20 90 A8                  ..
	ldy     $D8                             ; ABD6 A4 D8                    ..
	clc                                     ; ABD8 18                       .
	adc     $A4                             ; ABD9 65 A4                    e.
	sta     $A4                             ; ABDB 85 A4                    ..
	bcc     LABE1                           ; ABDD 90 02                    ..
	inc     $A5                             ; ABDF E6 A5                    ..
LABE1:  tya                                     ; ABE1 98                       .
	clc                                     ; ABE2 18                       .
	adc     $9C                             ; ABE3 65 9C                    e.
	sta     $9C                             ; ABE5 85 9C                    ..
	bcc     LABEB                           ; ABE7 90 02                    ..
	inc     $9D                             ; ABE9 E6 9D                    ..
LABEB:  lda     $9D                             ; ABEB A5 9D                    ..
	cmp     #$02                            ; ABED C9 02                    ..
	bcc     LABFE                           ; ABEF 90 0D                    ..
	lda     $A4                             ; ABF1 A5 A4                    ..
	sec                                     ; ABF3 38                       8
	sbc     #$40                            ; ABF4 E9 40                    .@
	sta     $A4                             ; ABF6 85 A4                    ..
	lda     #$00                            ; ABF8 A9 00                    ..
	sta     $A5                             ; ABFA 85 A5                    ..
	sta     $9D                             ; ABFC 85 9D                    ..
LABFE:  rts                                     ; ABFE 60                       `

; ----------------------------------------------------------------------------
LABFF:  ldx     byte_B7
	cpx     #$02                            ; AC01 E0 02                    ..
	bcc     LAC24                           ; AC03 90 1F                    ..
	bne     LAC11                           ; AC05 D0 0A                    ..
	cmp     #$40                            ; AC07 C9 40                    .@
	tax                                     ; AC09 AA                       .
	lda     #$E0                            ; AC0A A9 E0                    ..
	adc     #$00                            ; AC0C 69 00                    i.
	sta     $EB                             ; AC0E 85 EB                    ..
	txa                                     ; AC10 8A                       .
LAC11:  asl     a                               ; AC11 0A                       .
	rol     off_E5+1
	asl     a                               ; AC14 0A                       .
	rol     off_E5+1
	asl     a                               ; AC17 0A                       .
	rol     off_E5+1
	sta     off_E5
	ldx     byte_B7
	cpx     #$03                            ; AC1E E0 03                    ..
	bcs     LAC37                           ; AC20 B0 15                    ..
	bcc     LAC3C                           ; AC22 90 18                    ..
LAC24:  asl     a                               ; AC24 0A                       .
	asl     a                               ; AC25 0A                       .
	rol     $E6                             ; AC26 26 E6                    &.
	sta     $D9                             ; AC28 85 D9                    ..
	ldx     $E6                             ; AC2A A6 E6                    ..
	stx     $D8                             ; AC2C 86 D8                    ..
	asl     a                               ; AC2E 0A                       .
	rol     $E6                             ; AC2F 26 E6                    &.
	adc     $D9                             ; AC31 65 D9                    e.
	bcc     LAC37                           ; AC33 90 02                    ..
	inc     $E6                             ; AC35 E6 E6                    ..
LAC37:  clc                                     ; AC37 18                       .
	adc     $EA                             ; AC38 65 EA                    e.
	sta     off_E5
LAC3C:  lda     $D8                             ; AC3C A5 D8                    ..
	adc     $E6                             ; AC3E 65 E6                    e.
	adc     $EB                             ; AC40 65 EB                    e.
	sta     $E6                             ; AC42 85 E6                    ..
	jsr     LAD8E                           ; AC44 20 8E AD                  ..
	lda     $CA                             ; AC47 A5 CA                    ..
	cmp     #$7F                            ; AC49 C9 7F                    ..
	bne     LAC56                           ; AC4B D0 09                    ..
	lda     #$00                            ; AC4D A9 00                    ..
	sta     $C9                             ; AC4F 85 C9                    ..
	lda     $A4                             ; AC51 A5 A4                    ..
	jmp     LAC58                           ; AC53 4C 58 AC                 LX.

; ----------------------------------------------------------------------------
LAC56:  lda     $9C                             ; AC56 A5 9C                    ..
LAC58:  jsr     LADCB                           ; AC58 20 CB AD                  ..
	ldx     #$0B                            ; AC5B A2 0B                    ..
LAC5D:  ldy     #$00                            ; AC5D A0 00                    ..
	sty     $D8                             ; AC5F 84 D8                    ..
	lda     byte_B7
	lsr     a                               ; AC63 4A                       J
	beq     LAC78                           ; AC64 F0 12                    ..
	cpx     #$0A                            ; AC66 E0 0A                    ..
	bcs     LAC73                           ; AC68 B0 09                    ..
	cpx     #$02                            ; AC6A E0 02                    ..
	bcc     LAC73                           ; AC6C 90 05                    ..
	txa                                     ; AC6E 8A                       .
	sbc     #$02                            ; AC6F E9 02                    ..
	bcs     LAC79                           ; AC71 B0 06                    ..
LAC73:  lda     #$00                            ; AC73 A9 00                    ..
	tay                                     ; AC75 A8                       .
	beq     LAC7C                           ; AC76 F0 04                    ..
LAC78:  txa                                     ; AC78 8A                       .
LAC79:  tay                                     ; AC79 A8                       .
	lda     (off_E5),y
LAC7C:  ldy     $CA                             ; AC7C A4 CA                    ..
	bpl     LACB3                           ; AC7E 10 33                    .3
	sta     $E7                             ; AC80 85 E7                    ..
	lsr     a                               ; AC82 4A                       J
	lsr     a                               ; AC83 4A                       J
	lsr     a                               ; AC84 4A                       J
	lsr     a                               ; AC85 4A                       J
	tay                                     ; AC86 A8                       .
	lda     LBA02,y                         ; AC87 B9 02 BA                 ...
	sta     $FB                             ; AC8A 85 FB                    ..
	lda     $E7                             ; AC8C A5 E7                    ..
	and     #$0F                            ; AC8E 29 0F                    ).
	tay                                     ; AC90 A8                       .
	lda     LBA02,y                         ; AC91 B9 02 BA                 ...
	sta     off_FA
	lda     #$80                            ; AC96 A9 80                    ..
	sta     $CA                             ; AC98 85 CA                    ..
LAC9A:  ldy     #$00                            ; AC9A A0 00                    ..
	lda     off_FA+1
	jsr     LAD3A                           ; AC9E 20 3A AD                  :.
	lda     $FA                             ; ACA1 A5 FA                    ..
	jsr     LAD3A                           ; ACA3 20 3A AD                  :.
	bit     $CA                             ; ACA6 24 CA                    $.
	bvs     LACB8                           ; ACA8 70 0E                    p.
	jsr     LACCD                           ; ACAA 20 CD AC                  ..
	lda     #$FF                            ; ACAD A9 FF                    ..
	sta     $CA                             ; ACAF 85 CA                    ..
	bne     LAC9A                           ; ACB1 D0 E7                    ..
LACB3:  ldy     #$00                            ; ACB3 A0 00                    ..
	jsr     LAD3A                           ; ACB5 20 3A AD                  :.
LACB8:  dex                                     ; ACB8 CA                       .
	bmi     LACE2                           ; ACB9 30 27                    0'
	lda     $CA                             ; ACBB A5 CA                    ..
	cmp     #$7F                            ; ACBD C9 7F                    ..
	bne     LACC7                           ; ACBF D0 06                    ..
	jsr	sub_ace3
	jmp     LAC5D                           ; ACC4 4C 5D AC                 L].

; ----------------------------------------------------------------------------
LACC7:  jsr     LACCD                           ; ACC7 20 CD AC                  ..
	jmp     LAC5D                           ; ACCA 4C 5D AC                 L].

; ----------------------------------------------------------------------------
LACCD:  sec                                     ; ACCD 38                       8
	lda     off_E3
	sbc     #$40                            ; ACD0 E9 40                    .@
	sta     off_E3
	bcs     LACE2
	dec     off_E3+1
	lda     off_E3+1
	cmp     #$40                            ; ACDA C9 40                    .@
	bcs     LACE2
	lda     #$9F                            ; ACDE A9 9F                    ..
	sta     $E4                             ; ACE0 85 E4                    ..
LACE2:	rts                                     ; ACE2 60                       `

; ----------------------------------------------------------------------------
sub_ace3:
	sec                                     ; ACE3 38                       8
	lda     off_E3
	sbc     #$28                            ; ACE6 E9 28                    .(
	sta     off_E3
	bcs     LAD04                           ; ACEA B0 18                    ..
	dec     off_E3+1
	lda     off_E3+1
	cmp     #$20                            ; ACF0 C9 20                    . 
	beq     LACFA                           ; ACF2 F0 06                    ..
	bcs     LAD04                           ; ACF4 B0 0E                    ..
	lda     #$3D                            ; ACF6 A9 3D                    .=
	bne     LAD02                           ; ACF8 D0 08                    ..
LACFA:  lda     off_E3
	cmp     #$10                            ; ACFC C9 10                    ..
	bcs     LAD04                           ; ACFE B0 04                    ..
	lda     #$3E                            ; AD00 A9 3E                    .>
LAD02:  sta     off_E3+1
LAD04:  rts                                     ; AD04 60                       `

; ----------------------------------------------------------------------------
LAD05:  asl     a                               ; AD05 0A                       .
	sta     $D9                             ; AD06 85 D9                    ..
	asl     a                               ; AD08 0A                       .
	rol     $E6                             ; AD09 26 E6                    &.
	clc                                     ; AD0B 18                       .
	adc     $D9                             ; AD0C 65 D9                    e.
	bcc     LAD13                           ; AD0E 90 03                    ..
	inc     $E6                             ; AD10 E6 E6                    ..
	clc                                     ; AD12 18                       .
LAD13:  adc     $E8                             ; AD13 65 E8                    e.
	sta     off_E5
	lda     off_E5+1
	adc     $E9                             ; AD19 65 E9                    e.
	sta     off_E5+1
	jsr     LAD8E                           ; AD1D 20 8E AD                  ..
	lda     $A4                             ; AD20 A5 A4                    ..
	jsr     LADCB                           ; AD22 20 CB AD                  ..
	ldx     #$05                            ; AD25 A2 05                    ..
LAD27:  jsr     LAD34                           ; AD27 20 34 AD                  4.
	dex                                     ; AD2A CA                       .
	bmi	:+
	jsr     sub_ace3
	jmp     LAD27                           ; AD30 4C 27 AD                 L'.

; ----------------------------------------------------------------------------
:	rts

; ----------------------------------------------------------------------------
LAD34:  txa                                     ; AD34 8A                       .
	tay                                     ; AD35 A8                       .
	lda     (off_E5),y
	ldy     #$00                            ; AD38 A0 00                    ..
LAD3A:  bit     $B0                             ; AD3A 24 B0                    $.
	bvc     LAD46                           ; AD3C 50 08                    P.
	eor     #$FF                            ; AD3E 49 FF                    I.
	bit     $C9                             ; AD40 24 C9                    $.
	bvc     LAD46                           ; AD42 50 02                    P.
	and     #$F8                            ; AD44 29 F8                    ).
LAD46:  sty     $FC                             ; AD46 84 FC                    ..
	ldy     #$00                            ; AD48 A0 00                    ..
	sty     $D8                             ; AD4A 84 D8                    ..
	ldy     off_F4
	beq     LAD56                           ; AD4E F0 06                    ..
LAD50:  lsr     a                               ; AD50 4A                       J
	ror     $D8                             ; AD51 66 D8                    f.
	dey                                     ; AD53 88                       .
	bne     LAD50                           ; AD54 D0 FA                    ..
LAD56:  sta     $D9                             ; AD56 85 D9                    ..
	ldy     $FC                             ; AD58 A4 FC                    ..
	lda     (off_E3),y
	bit     $B0                             ; AD5C 24 B0                    $.
	bmi     LAD67                           ; AD5E 30 07                    0.
	and     $AB                             ; AD60 25 AB                    %.
LAD62:  ora     $D9                             ; AD62 05 D9                    ..
	jmp     LAD6F                           ; AD64 4C 6F AD                 Lo.

; ----------------------------------------------------------------------------
LAD67:  bvc     LAD62                           ; AD67 50 F9                    P.
	lda     $D9                             ; AD69 A5 D9                    ..
	ora     $AB                             ; AD6B 05 AB                    ..
	and     (off_E3),y
LAD6F:  sta     (off_E3),y
	iny                                     ; AD71 C8                       .
	lda     $A7                             ; AD72 A5 A7                    ..
	beq     LAD8D                           ; AD74 F0 17                    ..
	lda     (off_E3),y
	bit     $B0                             ; AD78 24 B0                    $.
	bmi     LAD83                           ; AD7A 30 07                    0.
	and     $A7                             ; AD7C 25 A7                    %.
LAD7E:  ora     $D8                             ; AD7E 05 D8                    ..
	jmp     LAD8B                           ; AD80 4C 8B AD                 L..

; ----------------------------------------------------------------------------
LAD83:  bvc     LAD7E                           ; AD83 50 F9                    P.
	lda     $D8                             ; AD85 A5 D8                    ..
	ora     $A7                             ; AD87 05 A7                    ..
	and     (off_E3),y
LAD8B:  sta     (off_E3),y
LAD8D:  rts                                     ; AD8D 60                       `

; ----------------------------------------------------------------------------
LAD8E:  bit     $C9                             ; AD8E 24 C9                    $.
	bvs     LADB2                           ; AD90 70 20                    p 
	lda     #$7F                            ; AD92 A9 7F                    ..
	sec                                     ; AD94 38                       8
	sbc     $9E                             ; AD95 E5 9E                    ..
	tax                                     ; AD97 AA                       .
	and     #$03                            ; AD98 29 03                    ).
	tay                                     ; AD9A A8                       .
	lda     #$01                            ; AD9B A9 01                    ..
	sbc     $9F                             ; AD9D E5 9F                    ..
	lsr     a                               ; AD9F 4A                       J
	txa                                     ; ADA0 8A                       .
	ror     a                               ; ADA1 6A                       j
	lsr     a                               ; ADA2 4A                       J
	clc                                     ; ADA3 18                       .
	adc     #$40                            ; ADA4 69 40                    i@
	sta     off_E3+1
	jsr     LA5AA                           ; ADA8 20 AA A5                  ..
	clc                                     ; ADAB 18                       .
	adc     LB936,y                         ; ADAC 79 36 B9                 y6.
	sta     off_E3
	rts                                     ; ADB1 60                       `

; ----------------------------------------------------------------------------
LADB2:  lda     #$BF                            ; ADB2 A9 BF                    ..
	sec                                     ; ADB4 38                       8
	sbc     $A6                             ; ADB5 E5 A6                    ..
	tax                                     ; ADB7 AA                       .
	lda     $04C0,x                         ; ADB8 BD C0 04                 ...
	sta     off_E3+1
	jsr     LA5A2                           ; ADBD 20 A2 A5                  ..
	clc                                     ; ADC0 18                       .
	adc     $0400,x                         ; ADC1 7D 00 04                 }..
	sta     off_E3
	bcc     :+
	inc     $E4                             ; ADC8 E6 E4                    ..
:	rts

; ----------------------------------------------------------------------------
LADCB:  and     #$07                            ; ADCB 29 07                    ).
	sta     off_F4
	bit     $C9                             ; ADCF 24 C9                    $.
	bvs     :+
	tax                                     ; ADD3 AA                       .
	beq     :++
	lda     LB94E,x                         ; ADD6 BD 4E B9                 .N.
	sta     $A7                             ; ADD9 85 A7                    ..
	eor     #$FF                            ; ADDB 49 FF                    I.
	bne     LADEE                           ; ADDD D0 0F                    ..
:	tax                                     ; ADDF AA                       .
	lda     LB966,x                         ; ADE0 BD 66 B9                 .f.
	sta     $A7                             ; ADE3 85 A7                    ..
	lda     LB95E,x                         ; ADE5 BD 5E B9                 .^.
	bne     LADEE                           ; ADE8 D0 04                    ..
:	lda     #$00                            ; ADEA A9 00                    ..
	sta     $A7                             ; ADEC 85 A7                    ..
LADEE:  sta     $AB                             ; ADEE 85 AB                    ..
	rts                                     ; ADF0 60                       `

; ----------------------------------------------------------------------------
	jsr     sub_a8b3
	lda     $BA                             ; ADF4 A5 BA                    ..
	sta     $C9                             ; ADF6 85 C9                    ..
	bne     LADFF                           ; ADF8 D0 05                    ..
	jsr     LADFF                           ; ADFA 20 FF AD                  ..
	dec     $C9                             ; ADFD C6 C9                    ..
LADFF:  bit     $C9                             ; ADFF 24 C9                    $.
	bvs     LAE13                           ; AE01 70 10                    p.
	lda     $A0                             ; AE03 A5 A0                    ..
	ldx     $A1                             ; AE05 A6 A1                    ..
	ldy     $9C                             ; AE07 A4 9C                    ..
	sty     $EC                             ; AE09 84 EC                    ..
	sty     $A0                             ; AE0B 84 A0                    ..
	ldy     $9D                             ; AE0D A4 9D                    ..
	sty     $A1                             ; AE0F 84 A1                    ..
	bvc     LAE21                           ; AE11 50 0E                    P.
LAE13:  lda     $A8                             ; AE13 A5 A8                    ..
	ldx     $A9                             ; AE15 A6 A9                    ..
	ldy     $A4                             ; AE17 A4 A4                    ..
	sty     $EC                             ; AE19 84 EC                    ..
	sty     $A8                             ; AE1B 84 A8                    ..
	ldy     $A5                             ; AE1D A4 A5                    ..
	sty     $A9                             ; AE1F 84 A9                    ..
LAE21:  sta     $F0                             ; AE21 85 F0                    ..
	stx     $F1                             ; AE23 86 F1                    ..
	sty     $ED                             ; AE25 84 ED                    ..
	bvs     LAE2F                           ; AE27 70 06                    p.
	lda     $A2                             ; AE29 A5 A2                    ..
	ldx     $A3                             ; AE2B A6 A3                    ..
	bvc     LAE33                           ; AE2D 50 04                    P.
LAE2F:  lda     $AA                             ; AE2F A5 AA                    ..
	ldx     #$00                            ; AE31 A2 00                    ..
LAE33:  sta     $F2                             ; AE33 85 F2                    ..
	stx     $F3                             ; AE35 86 F3                    ..
	sec                                     ; AE37 38                       8
	bvc     LAE46                           ; AE38 50 0C                    P.
	lda     #$BF                            ; AE3A A9 BF                    ..
	sbc     $A6                             ; AE3C E5 A6                    ..
	sta     $EE                             ; AE3E 85 EE                    ..
	sta     $AA                             ; AE40 85 AA                    ..
	lda     #$00                            ; AE42 A9 00                    ..
	beq     LAE54                           ; AE44 F0 0E                    ..
LAE46:  lda     #$7F                            ; AE46 A9 7F                    ..
	sbc     $9E                             ; AE48 E5 9E                    ..
	sta     $EE                             ; AE4A 85 EE                    ..
	sta     $A2                             ; AE4C 85 A2                    ..
	lda     #$01                            ; AE4E A9 01                    ..
	sbc     $9F                             ; AE50 E5 9F                    ..
	sta     $A3                             ; AE52 85 A3                    ..
LAE54:  sta     $EF                             ; AE54 85 EF                    ..
	lda     $DA                             ; AE56 A5 DA                    ..
	bne     LAE5D                           ; AE58 D0 03                    ..
	jmp     LAF94                           ; AE5A 4C 94 AF                 L..

; ----------------------------------------------------------------------------
LAE5D:  ldy     #$00                            ; AE5D A0 00                    ..
	lda     $ED                             ; AE5F A5 ED                    ..
	cmp     $F1                             ; AE61 C5 F1                    ..
	bcc     LAE6D                           ; AE63 90 08                    ..
	bne     LAE7B                           ; AE65 D0 14                    ..
	lda     $EC                             ; AE67 A5 EC                    ..
	cmp     $F0                             ; AE69 C5 F0                    ..
	bcs     LAE7B                           ; AE6B B0 0E                    ..
LAE6D:  sec                                     ; AE6D 38                       8
	lda     $F0                             ; AE6E A5 F0                    ..
	sbc     $EC                             ; AE70 E5 EC                    ..
	sta     $D4                             ; AE72 85 D4                    ..
	lda     $F1                             ; AE74 A5 F1                    ..
	sbc     $ED                             ; AE76 E5 ED                    ..
	dey                                     ; AE78 88                       .
	bne     LAE85                           ; AE79 D0 0A                    ..
LAE7B:  lda     $EC                             ; AE7B A5 EC                    ..
	sbc     $F0                             ; AE7D E5 F0                    ..
	sta     $D4                             ; AE7F 85 D4                    ..
	lda     $ED                             ; AE81 A5 ED                    ..
	sbc     $F1                             ; AE83 E5 F1                    ..
LAE85:  sty     $F8                             ; AE85 84 F8                    ..
	sta     $D5                             ; AE87 85 D5                    ..
	ldy     #$00                            ; AE89 A0 00                    ..
	lda     $EF                             ; AE8B A5 EF                    ..
	cmp     $F3                             ; AE8D C5 F3                    ..
	bcc     LAE99                           ; AE8F 90 08                    ..
	bne     LAEA7                           ; AE91 D0 14                    ..
	lda     $EE                             ; AE93 A5 EE                    ..
	cmp     $F2                             ; AE95 C5 F2                    ..
	bcs     LAEA7                           ; AE97 B0 0E                    ..
LAE99:  sec                                     ; AE99 38                       8
	lda     $F2                             ; AE9A A5 F2                    ..
	sbc     $EE                             ; AE9C E5 EE                    ..
	sta     $D6                             ; AE9E 85 D6                    ..
	lda     $F3                             ; AEA0 A5 F3                    ..
	sbc     $EF                             ; AEA2 E5 EF                    ..
	dey                                     ; AEA4 88                       .
	bne     LAEB1                           ; AEA5 D0 0A                    ..
LAEA7:  lda     $EE                             ; AEA7 A5 EE                    ..
	sbc     $F2                             ; AEA9 E5 F2                    ..
	sta     $D6                             ; AEAB 85 D6                    ..
	lda     $EF                             ; AEAD A5 EF                    ..
	sbc     $F3                             ; AEAF E5 F3                    ..
LAEB1:  sty     $F9                             ; AEB1 84 F9                    ..
	sta     $D7                             ; AEB3 85 D7                    ..
	bit     $C9                             ; AEB5 24 C9                    $.
	lda     $D5                             ; AEB7 A5 D5                    ..
	cmp     $D7                             ; AEB9 C5 D7                    ..
	bcc     LAEC5                           ; AEBB 90 08                    ..
	bne     LAEE6                           ; AEBD D0 27                    .'
	lda     $D4                             ; AEBF A5 D4                    ..
	cmp     $D6                             ; AEC1 C5 D6                    ..
	bcs     LAEE6                           ; AEC3 B0 21                    .!
LAEC5:  lda     #$01                            ; AEC5 A9 01                    ..
	sta     $FC                             ; AEC7 85 FC                    ..
	lda     $D6                             ; AEC9 A5 D6                    ..
	sta     off_F4
	lda     $D7                             ; AECD A5 D7                    ..
	sta     $F5                             ; AECF 85 F5                    ..
	lda     $F9                             ; AED1 A5 F9                    ..
	beq     LAED8                           ; AED3 F0 03                    ..
	jsr     LAFEC                           ; AED5 20 EC AF                  ..
LAED8:  ldx     $F8                             ; AED8 A6 F8                    ..
	beq     LAEFD                           ; AEDA F0 21                    .!
LAEDC:  bvc     LAEE2                           ; AEDC 50 04                    P.
	lda     #$D8                            ; AEDE A9 D8                    ..
	bne     LAF05                           ; AEE0 D0 23                    .#
LAEE2:  lda     #$C0                            ; AEE2 A9 C0                    ..
	bne     LAF05                           ; AEE4 D0 1F                    ..
LAEE6:  lda     #$00                            ; AEE6 A9 00                    ..
	sta     $FC                             ; AEE8 85 FC                    ..
	lda     $D4                             ; AEEA A5 D4                    ..
	sta     off_F4
	lda     $D5                             ; AEEE A5 D5                    ..
	sta     $F5                             ; AEF0 85 F5                    ..
	lda     $F8                             ; AEF2 A5 F8                    ..
	beq     LAEF9                           ; AEF4 F0 03                    ..
	jsr     LAFEC                           ; AEF6 20 EC AF                  ..
LAEF9:  ldx     $F9                             ; AEF9 A6 F9                    ..
	bne     LAEDC                           ; AEFB D0 DF                    ..
LAEFD:  bvc     LAF03                           ; AEFD 50 04                    P.
	lda     #$28                            ; AEFF A9 28                    .(
	bne     LAF05                           ; AF01 D0 02                    ..
LAF03:  lda     #$40                            ; AF03 A9 40                    .@
LAF05:  stx     $FB                             ; AF05 86 FB                    ..
	sta     $FA                             ; AF07 85 FA                    ..
	lda     off_F4
	eor     #$FF                            ; AF0B 49 FF                    I.
	sta     $F6                             ; AF0D 85 F6                    ..
	lda     $F5                             ; AF0F A5 F5                    ..
	eor     #$FF                            ; AF11 49 FF                    I.
	sta     $F7                             ; AF13 85 F7                    ..
	lsr     $F5                             ; AF15 46 F5                    F.
	ror     off_F4
	lda     $F2                             ; AF19 A5 F2                    ..
	bvc     LAF26                           ; AF1B 50 09                    P.
	tax                                     ; AF1D AA                       .
	ldy     $0400,x                         ; AF1E BC 00 04                 ...
	lda     $04C0,x                         ; AF21 BD C0 04                 ...
	bne     LAF36                           ; AF24 D0 10                    ..
LAF26:  and     #$03                            ; AF26 29 03                    ).
	tax                                     ; AF28 AA                       .
	lda     $F3                             ; AF29 A5 F3                    ..
	lsr     a                               ; AF2B 4A                       J
	lda     $F2                             ; AF2C A5 F2                    ..
	ror     a                               ; AF2E 6A                       j
	lsr     a                               ; AF2F 4A                       J
	clc                                     ; AF30 18                       .
	adc     #$40                            ; AF31 69 40                    i@
	ldy     LB936,x                         ; AF33 BC 36 B9                 .6.
LAF36:  sty     off_E3
	sta     off_E3+1
	lda     $F0                             ; AF3A A5 F0                    ..
	and     #$07                            ; AF3C 29 07                    ).
	tax                                     ; AF3E AA                       .
	lda     $F1                             ; AF3F A5 F1                    ..
	lsr     a                               ; AF41 4A                       J
	lda     $F0                             ; AF42 A5 F0                    ..
	ror     a                               ; AF44 6A                       j
	lsr     a                               ; AF45 4A                       J
	lsr     a                               ; AF46 4A                       J
	tay                                     ; AF47 A8                       .
LAF48:  jsr     sub_b004
	inc     $F6                             ; AF4B E6 F6                    ..
	bne     LAF53                           ; AF4D D0 04                    ..
	inc     $F7                             ; AF4F E6 F7                    ..
	beq     LAF94                           ; AF51 F0 41                    .A
LAF53:  clc                                     ; AF53 18                       .
	lda     $FC                             ; AF54 A5 FC                    ..
	bne     LAF9D                           ; AF56 D0 45                    .E
	lda     off_F4
	adc     $D6                             ; AF5A 65 D6                    e.
	sta     off_F4
	lda     $F5                             ; AF5E A5 F5                    ..
	adc     $D7                             ; AF60 65 D7                    e.
	sta     $F5                             ; AF62 85 F5                    ..
	cmp     $D5                             ; AF64 C5 D5                    ..
	bcc     LAF89                           ; AF66 90 21                    .!
	bne     LAF70                           ; AF68 D0 06                    ..
	lda     off_F4
	cmp     $D4                             ; AF6C C5 D4                    ..
	bcc     LAF89                           ; AF6E 90 19                    ..
LAF70:  lda     off_F4
	sbc     $D4                             ; AF72 E5 D4                    ..
	sta     off_F4                             ; AF74 85 F4                    ..
	lda     $F5                             ; AF76 A5 F5                    ..
	sbc     $D5                             ; AF78 E5 D5                    ..
	sta     $F5                             ; AF7A 85 F5                    ..
	clc                                     ; AF7C 18                       .
	lda     off_E3
	adc     $FA                             ; AF7F 65 FA                    e.
	sta     off_E3
	lda     off_E3+1
	adc     $FB                             ; AF85 65 FB                    e.
	sta     off_E3+1
LAF89:  inx                                     ; AF89 E8                       .
	cpx     #$08                            ; AF8A E0 08                    ..
	bcc     LAF48                           ; AF8C 90 BA                    ..
	ldx     #$00                            ; AF8E A2 00                    ..
	iny                                     ; AF90 C8                       .
LAF91:  jmp     LAF48                           ; AF91 4C 48 AF                 LH.

; ----------------------------------------------------------------------------
LAF94:  ldx     $C9                             ; AF94 A6 C9                    ..
	bne	:+
	rts                                     ; AF98 60                       `

; ----------------------------------------------------------------------------
:	dex                                     ; AF99 CA                       .
	stx     $DA                             ; AF9A 86 DA                    ..
	rts                                     ; AF9C 60                       `

; ----------------------------------------------------------------------------
LAF9D:  lda     off_F4
	adc     $D4                             ; AF9F 65 D4                    e.
	sta     off_F4
	lda     off_F4+1
	adc     $D5                             ; AFA5 65 D5                    e.
	sta     off_F4+1
	cmp     $D7                             ; AFA9 C5 D7                    ..
	bcc     LAFD7                           ; AFAB 90 2A                    .*
	bne     LAFB5                           ; AFAD D0 06                    ..
	lda     off_F4                             ; AFAF A5 F4                    ..
	cmp     $D6                             ; AFB1 C5 D6                    ..
	bcc     LAFD7                           ; AFB3 90 22                    ."
LAFB5:  lda     off_F4                             ; AFB5 A5 F4                    ..
	sbc     $D6                             ; AFB7 E5 D6                    ..
	sta     off_F4                             ; AFB9 85 F4                    ..
	lda     $F5                             ; AFBB A5 F5                    ..
	sbc     $D7                             ; AFBD E5 D7                    ..
	sta     $F5                             ; AFBF 85 F5                    ..
	bit     $FB                             ; AFC1 24 FB                    $.
	bvs     LAFCF                           ; AFC3 70 0A                    p.
	inx                                     ; AFC5 E8                       .
	cpx     #$08                            ; AFC6 E0 08                    ..
	bcc     LAFD7                           ; AFC8 90 0D                    ..
	ldx     #$00                            ; AFCA A2 00                    ..
	iny                                     ; AFCC C8                       .
	bne     LAFD7                           ; AFCD D0 08                    ..
LAFCF:  dex                                     ; AFCF CA                       .
	bpl     LAFD7                           ; AFD0 10 05                    ..
	ldx     #$07                            ; AFD2 A2 07                    ..
	dey                                     ; AFD4 88                       .
	bmi     LAF94                           ; AFD5 30 BD                    0.
LAFD7:  clc                                     ; AFD7 18                       .
	lda     $C9                             ; AFD8 A5 C9                    ..
	beq     LAFE0                           ; AFDA F0 04                    ..
	lda     #$28                            ; AFDC A9 28                    .(
	bne     LAFE2                           ; AFDE D0 02                    ..
LAFE0:  lda     #$40                            ; AFE0 A9 40                    .@
LAFE2:  adc     off_E3
	sta     off_E3
	bcc     LAF91                           ; AFE6 90 A9                    ..
	inc     off_E3+1
	bne     LAF91                           ; AFEA D0 A5                    ..
LAFEC:  ldx     #$03                            ; AFEC A2 03                    ..
LAFEE:  ldy     $EC,x                           ; AFEE B4 EC                    ..
	lda     $F0,x                           ; AFF0 B5 F0                    ..
	sta     $EC,x                           ; AFF2 95 EC                    ..
	sty     $F0,x                           ; AFF4 94 F0                    ..
	dex                                     ; AFF6 CA                       .
	bpl     LAFEE                           ; AFF7 10 F5                    ..
	txa                                     ; AFF9 8A                       .
	eor     $F8                             ; AFFA 45 F8                    E.
	sta     $F8                             ; AFFC 85 F8                    ..
	txa                                     ; AFFE 8A                       .
	eor     $F9                             ; AFFF 45 F9                    E.
	sta     $F9                             ; B001 85 F9                    ..
	rts                                     ; B003 60                       `

; ----------------------------------------------------------------------------
sub_b004:  
	lda     LB93A,x                         ; B004 BD 3A B9                 .:.
	and     (off_E3),y
	bit     $B0                             ; B009 24 B0                    $.
	bvs	:+
	ora     LB942,x                         ; B00D 1D 42 B9                 .B.
:	sta     (off_E3),y
	rts                                     ; B012 60                       `

; ----------------------------------------------------------------------------

sub_b013:
	sec                                     ; B013 38                       8
	lda     $C1                             ; B014 A5 C1                    ..
	sbc     #$02                            ; B016 E9 02                    ..
	bne     LB01E                           ; B018 D0 04                    ..
	ldx     $C6                             ; B01A A6 C6                    ..
	bne     LB02B                           ; B01C D0 0D                    ..
LB01E:  lda     $0284                           ; B01E AD 84 02                 ...
	bne     LB02D                           ; B021 D0 0A                    ..
	ldx     $C6                             ; B023 A6 C6                    ..
	bne     LB045                           ; B025 D0 1E                    ..
	ldx     #$1E                            ; B027 A2 1E                    ..
	stx     $C6                             ; B029 86 C6                    ..
LB02B:  sta     $C7                             ; B02B 85 C7                    ..
LB02D:  ldx     STICK0				; read joystick 0
	lda     LB87F,x                         ; B030 BD 7F B8                 ...
	sta     off_DD
	lda     LB86F,x                         ; B035 BD 6F B8                 .o.
	sta     off_DD+1
	ora     off_DD
	bne     :+
	sta     $C2                             ; B03E 85 C2                    ..
	beq     LB045                           ; B040 F0 03                    ..
:	jsr     LB0A8                           ; B042 20 A8 B0                  ..
LB045:  ldx     $C6                             ; B045 A6 C6                    ..
	beq     LB04B                           ; B047 F0 02                    ..
	dec     $C6                             ; B049 C6 C6                    ..
LB04B:  lda     $14                             ; B04B A5 14                    ..
	and     #$0F                            ; B04D 29 0F                    ).
	bne     LB087                           ; B04F D0 36                    .6
	lda     $D01F                           ; B051 AD 1F D0                 ...
	and     #$03                            ; B054 29 03                    ).
	cmp     #$01                            ; B056 C9 01                    ..
	bne     LB087                           ; B058 D0 2D                    .-
	lda     byte_B8
	asl     a                               ; B05C 0A                       .
	ldy     $02C6                           ; B05D AC C6 02                 ...
	bit     $1355                           ; B060 2C 55 13                 ,U.
	bvs     LB08A                           ; B063 70 25                    p%
	bmi     LB09A                           ; B065 30 33                    03
	php                                     ; B067 08                       .
	tya                                     ; B068 98                       .
	clc                                     ; B069 18                       .
	adc     #$10                            ; B06A 69 10                    i.
	and     #$F0                            ; B06C 29 F0                    ).
	sta     off_DD
	tya                                     ; B070 98                       .
	and     #$0F                            ; B071 29 0F                    ).
	ora     off_DD
	plp                                     ; B075 28                       (
LB076:  sta     $02C6                           ; B076 8D C6 02                 ...
	sta     $02C8                           ; B079 8D C8 02                 ...
	bcc     LB085                           ; B07C 90 07                    ..
	sta     $D3                             ; B07E 85 D3                    ..
	jsr     sub_a324
	bmi     LB087                           ; B083 30 02                    0.
LB085:  sta     $D1                             ; B085 85 D1                    ..
LB087:  jmp     XITVBV

; ----------------------------------------------------------------------------
LB08A:  inc     $02C5                           ; B08A EE C5 02                 ...
	lda     $02C5                           ; B08D AD C5 02                 ...
	bcc	:+
	sta     $D2                             ; B092 85 D2                    ..
	bcs     LB087                           ; B094 B0 F1                    ..
:	sta     $D0                             ; B096 85 D0                    ..
	bcc     LB087                           ; B098 90 ED                    ..
LB09A:  tya                                     ; B09A 98                       .
	and     #$F0                            ; B09B 29 F0                    ).
	sta     off_DD
	iny                                     ; B09F C8                       .
	tya                                     ; B0A0 98                       .
	and     #$0F                            ; B0A1 29 0F                    ).
	ora     off_DD
	jmp     LB076                           ; B0A5 4C 76 B0                 Lv.

; ----------------------------------------------------------------------------
LB0A8:  ldy     $C1                             ; B0A8 A4 C1                    ..
	beq     LB103                           ; B0AA F0 57                    .W
	bpl	:+
	jmp     LB17D                           ; B0AE 4C 7D B1                 L}.

; ----------------------------------------------------------------------------
:	cpx     $C2                             ; B0B1 E4 C2                    ..
	bne     LB104                           ; B0B3 D0 4F                    .O
	ldx     $C5                             ; B0B5 A6 C5                    ..
	cpx     #$7F                            ; B0B7 E0 7F                    ..
	beq     LB136                           ; B0B9 F0 7B                    .{
	inx                                     ; B0BB E8                       .
	stx     $C5                             ; B0BC 86 C5                    ..
	bmi     LB136                           ; B0BE 30 76                    0v
	txa                                     ; B0C0 8A                       .
	and     #$03                            ; B0C1 29 03                    ).
	bne     LB136                           ; B0C3 D0 71                    .q
LB0C5:  lda     $C1                             ; B0C5 A5 C1                    ..
	lsr     a                               ; B0C7 4A                       J
	ldx     $C7                             ; B0C8 A6 C7                    ..
	inx                                     ; B0CA E8                       .
	lda     off_DD+1
	beq     LB0E8                           ; B0CD F0 19                    ..
	bpl     LB0DE                           ; B0CF 10 0D                    ..
	bcs     LB0D8                           ; B0D1 B0 05                    ..
	lda     LBA43,x                         ; B0D3 BD 43 BA                 .C.
	bne     LB0FD                           ; B0D6 D0 25                    .%
LB0D8:  jsr     LB115                           ; B0D8 20 15 B1                  ..
	jmp     LB0E8                           ; B0DB 4C E8 B0                 L..

; ----------------------------------------------------------------------------
LB0DE:  bcs     :+
	lda     LBA40,x                         ; B0E0 BD 40 BA                 .@.
	bne     LB0FD                           ; B0E3 D0 18                    ..
:  	jsr     sub_b137
LB0E8:  lda     $C1                             ; B0E8 A5 C1                    ..
	lsr     a                               ; B0EA 4A                       J
	lda     off_DD
	beq     LB136                           ; B0ED F0 47                    .G
	bpl     LB0F8                           ; B0EF 10 07                    ..
	bcs     LB157                           ; B0F1 B0 64                    .d
	lda     LBA3D,x                         ; B0F3 BD 3D BA                 .=.
	bne     LB0FD                           ; B0F6 D0 05                    ..
LB0F8:  bcs     LB16E                           ; B0F8 B0 74                    .t
	lda     LBA3A,x                         ; B0FA BD 3A BA                 .:.
LB0FD:  sta     $C8                             ; B0FD 85 C8                    ..
	ldx     #$80                            ; B0FF A2 80                    ..
	stx     $C5                             ; B101 86 C5                    ..
LB103:  rts                                     ; B103 60                       `

; ----------------------------------------------------------------------------
LB104:  stx     $C2                             ; B104 86 C2                    ..
	ldx     #$80                            ; B106 A2 80                    ..
	ldy     $C1                             ; B108 A4 C1                    ..
	cpy     #$02                            ; B10A C0 02                    ..
	beq     LB110                           ; B10C F0 02                    ..
	ldx     #$E2                            ; B10E A2 E2                    ..
LB110:  stx     $C5                             ; B110 86 C5                    ..
	jmp     LB0C5                           ; B112 4C C5 B0                 L..

; ----------------------------------------------------------------------------
LB115:  lda     $C0                             ; B115 A5 C0                    ..
	cmp     #$0F                            ; B117 C9 0F                    ..
	bcs     LB136                           ; B119 B0 1B                    ..
	inc     $C0                             ; B11B E6 C0                    ..
	ldy     #$02                            ; B11D A0 02                    ..
	ldx     $C3                             ; B11F A6 C3                    ..
	txa                                     ; B121 8A                       .
	sec                                     ; B122 38                       8
	sbc     #$06                            ; B123 E9 06                    ..
	sta     $C3                             ; B125 85 C3                    ..
LB127:  lda     $0590,x                         ; B127 BD 90 05                 ...
	sta     $058A,x                         ; B12A 9D 8A 05                 ...
	lda     #$00                            ; B12D A9 00                    ..
	sta     $0590,x                         ; B12F 9D 90 05                 ...
	inx                                     ; B132 E8                       .
	dey                                     ; B133 88                       .
	bpl     LB127                           ; B134 10 F1                    ..
LB136:  rts                                     ; B136 60                       `

; ----------------------------------------------------------------------------
sub_b137:
	lda     $C0                             ; B137 A5 C0                    ..
	beq     LB156                           ; B139 F0 1B                    ..
	dec     $C0                             ; B13B C6 C0                    ..
	ldy     #$02                            ; B13D A0 02                    ..
	ldx     $C3                             ; B13F A6 C3                    ..
	txa                                     ; B141 8A                       .
	clc                                     ; B142 18                       .
	adc     #$06                            ; B143 69 06                    i.
	sta     $C3                             ; B145 85 C3                    ..
:	lda     $0590,x                         ; B147 BD 90 05                 ...
	sta     $0596,x                         ; B14A 9D 96 05                 ...
	lda     #$00                            ; B14D A9 00                    ..
	sta     $0590,x                         ; B14F 9D 90 05                 ...
	inx                                     ; B152 E8                       .
	dey                                     ; B153 88                       .
	bpl     :-
LB156:  rts                                     ; B156 60                       `

; ----------------------------------------------------------------------------
LB157:  lda     $BF                             ; B157 A5 BF                    ..
	beq     LB16D                           ; B159 F0 12                    ..
	dec     $BF                             ; B15B C6 BF                    ..
	lda     $C4                             ; B15D A5 C4                    ..
	sec                                     ; B15F 38                       8
	sbc     #$0A                            ; B160 E9 0A                    ..
LB162:  sta     $C4                             ; B162 85 C4                    ..
	sta     HPOSM1
	tax                                     ; B167 AA                       .
	inx                                     ; B168 E8                       .
	inx                                     ; B169 E8                       .
	stx     HPOSM0
LB16D:  rts                                     ; B16D 60                       `

; ----------------------------------------------------------------------------
LB16E:  lda     $BF                             ; B16E A5 BF                    ..
	cmp     #$0F                            ; B170 C9 0F                    ..
	beq     LB16D                           ; B172 F0 F9                    ..
	inc     $BF                             ; B174 E6 BF                    ..
	lda     $C4                             ; B176 A5 C4                    ..
	clc                                     ; B178 18                       .
	adc     #$0A                            ; B179 69 0A                    i.
	bne     LB162                           ; B17B D0 E5                    ..
LB17D:  lda     off_DD
	beq     LB18E                           ; B17F F0 0D                    ..
	clc                                     ; B181 18                       .
	lda     byte_E1                         ; B182 A5 E1                    ..
	adc     off_DD
	cmp     #$19                            ; B186 C9 19                    ..
	bcs     LB18E                           ; B188 B0 04                    ..
	sta     byte_E1                         ; B18A 85 E1                    ..
	sta     byte_DF                         ; B18C 85 DF                    ..
LB18E:  lda     byte_E2                         ; B18E A5 E2                    ..
	sta     byte_E0                         ; B190 85 E0                    ..
	lda     off_DD+1
	beq     LB1AB                           ; B194 F0 15                    ..
	clc                                     ; B196 18                       .
	lda     byte_E2                         ; B197 A5 E2                    ..
	adc     off_DD+1
	cmp     #$40                            ; B19B C9 40                    .@
	bcc     LB1AB                           ; B19D 90 0C                    ..
	ldx     off_DD+1
	bmi     LB1A3                           ; B1A1 30 00                    0.
LB1A3:  cmp     #$71                            ; B1A3 C9 71                    .q
	bcs     LB1AB                           ; B1A5 B0 04                    ..
	sta     byte_E2                         ; B1A7 85 E2                    ..
	sta     byte_E0                         ; B1A9 85 E0                    ..

; 
LB1AB:  lda     #$CD                            ; B1AB A9 CD                    ..
	sta     off_DD
	lda     #$10                            ; B1AF A9 10                    ..
	sta     off_DD+1

	ldx     #$C0                            ; B1B3 A2 C0                    ..
LB1B5:  ldy     #$00                            ; B1B5 A0 00                    ..
	lda     #$4F                            ; B1B7 A9 4F                    .O
	sta     (off_DD),y
	iny                                     ; B1BB C8                       .
	lda     byte_DF                         ; B1BC A5 DF                    ..
	sta     (off_DD),y
	iny                                     ; B1C0 C8                       .
	lda     byte_E0                         ; B1C1 A5 E0                    ..
	sta     (off_DD),y
	clc                                     ; B1C5 18                       .
	lda     byte_DF                         ; B1C6 A5 DF                    ..
	adc     #$40                            ; B1C8 69 40                    i@
	sta     byte_DF                         ; B1CA 85 DF                    ..
	bcc     LB1D0                           ; B1CC 90 02                    ..
	inc     byte_E0                         ; B1CE E6 E0                    ..
LB1D0:  clc                                     ; B1D0 18                       .
	lda     off_DD
	adc     #$03                            ; B1D3 69 03                    i.
	sta     off_DD
	bcc     :+
	inc     off_DD+1
:	dex                                     ; B1DB CA                       .
	bne     LB1B5                           ; B1DC D0 D7                    ..
	rts                                     ; B1DE 60                       `

; ----------------------------------------------------------------------------
sub_b1df:  
	tay                                     ; B1DF A8                       .
	ldx     #$00                            ; B1E0 A2 00                    ..
:	lsr     a                               ; B1E2 4A                       J
	bcc     :+
	inx                                     ; B1E5 E8                       .
:	bne     :--
	txa                                     ; B1E8 8A                       .
	lsr     a                               ; B1E9 4A                       J
	tya                                     ; B1EA 98                       .
	bcc     :+
	ora     #$80                            ; B1ED 09 80                    ..

sub_b1ef:
:	ldy     #$18                            ; B1EF A0 18                    ..

sub_b1f1:
	jsr     sub_b206
	bpl     LB252                           ; B1F4 10 5C                    .\
LB1F6:  ldy     #$18                            ; B1F6 A0 18                    ..
	sty     $9C                             ; B1F8 84 9C                    ..
	ldy     #$12                            ; B1FA A0 12                    ..
	lda     #<LB89B
	ldx     #>LB89B
	jsr     sub_a89f
LB203:  jmp     LB203                           ; B203 4C 03 B2                 L..

; ----------------------------------------------------------------------------
sub_b206:
	pha
	ldx     LB96E,y
	lda     LB96E+1,y
	sta     ICCOM,x
	lda     LB96E+2,y
	sta     ICAX1,x
	lda     LB96E+3,y
	sta     ICAX2,x
	lda     LB96E+4,y
	sta     ICBA,x
	lda     LB96E+5,y
	sta     ICBA+1,x
	lda     LB96E+6,y
	sta     ICBL,x
	lda     LB96E+7,y
	sta     ICBL+1,x
	pla
	jmp     CIOV

; ----------------------------------------------------------------------------
LB238:  ldx     #$10                            ; B238 A2 10                    ..
LB23A:  lda     #$0C                            ; B23A A9 0C                    ..
	sta     ICCOM,x
	jmp     CIOV

; ----------------------------------------------------------------------------
sub_b242:  
	pha
	lda     #$1B                            ; B243 A9 1B                    ..
	sta     TSTDAT
	jsr     sub_b1ef
	pla                                     ; B24A 68                       h
	jsr     sub_b1ef
	lda     #$00                            ; B24E A9 00                    ..
	sta     TSTDAT
LB252:  rts                                     ; B252 60                       `

; ----------------------------------------------------------------------------
LB253:  jsr     LB238                           ; B253 20 38 B2                  8.
	bmi     LB1F6                           ; B256 30 9E                    0.
	ldy     #$18                            ; B258 A0 18                    ..
	sty     $9C                             ; B25A 84 9C                    ..
	ldy     #$08                            ; B25C A0 08                    ..
	lda     $B1                             ; B25E A5 B1                    ..
	bne     LB268                           ; B260 D0 06                    ..
	lda     #$1C                            ; B262 A9 1C                    ..
	ldx     #$B9                            ; B264 A2 B9                    ..
	bne     LB26C                           ; B266 D0 04                    ..
LB268:  lda     #$13                            ; B268 A9 13                    ..
	ldx     #$B9                            ; B26A A2 B9                    ..
LB26C:  jsr     sub_a89f
	jmp     LB719                           ; B26F 4C 19 B7                 L..

; ----------------------------------------------------------------------------
sub_b272:
	jsr     LB238                           ; B272 20 38 B2                  8.
	jsr     LB5B0                           ; B275 20 B0 B5                  ..
	ldy     #$00                            ; B278 A0 00                    ..
	jsr     sub_b1f1
	ldy     #$01                            ; B27D A0 01                    ..
	sty     byte_B2
	ldy     #$10                            ; B281 A0 10                    ..
	lda     #$25                            ; B283 A9 25                    .%
	ldx     #$B9                            ; B285 A2 B9                    ..
	jmp     sub_a89f

; ----------------------------------------------------------------------------
LB28A:  ldx     byte_B2
	dex                                     ; B28C CA                       .
	beq     LB292                           ; B28D F0 03                    ..
	jsr     LB238                           ; B28F 20 38 B2                  8.
LB292:  ldy     #$30                            ; B292 A0 30                    .0
	jsr     sub_b206
	bmi     LB2C0                           ; B297 30 27                    0'
	lda     #$20                            ; B299 A9 20                    . 
	sta     $D9                             ; B29B 85 D9                    ..
	lda     #$00                            ; B29D A9 00                    ..
	sta     $FA                             ; B29F 85 FA                    ..
	lda     #$18                            ; B2A1 A9 18                    ..
	sta     $FB                             ; B2A3 85 FB                    ..
LB2A5:  jsr     LB2C6                           ; B2A5 20 C6 B2                  ..
	dec     $D9                             ; B2A8 C6 D9                    ..
	bne     LB2A5                           ; B2AA D0 F9                    ..
LB2AC:  ldx     #$30                            ; B2AC A2 30                    .0
	jsr     LB23A                           ; B2AE 20 3A B2                  :.
	ldx     byte_B2
	dex                                     ; B2B3 CA                       .
	beq     LB2F9                           ; B2B4 F0 43                    .C
	bpl     LB2BB                           ; B2B6 10 03                    ..
	jmp     LB71C                           ; B2B8 4C 1C B7                 L..

; ----------------------------------------------------------------------------
LB2BB:  ldy     #$28                            ; B2BB A0 28                    .(
	jmp     sub_b1f1

; ----------------------------------------------------------------------------
LB2C0:  jsr     LB828                           ; B2C0 20 28 B8                  (.
	jmp     LB2AC                           ; B2C3 4C AC B2                 L..

; ----------------------------------------------------------------------------
LB2C6:  ldy     #$3F                            ; B2C6 A0 3F                    .?
	lda     #$20                            ; B2C8 A9 20                    . 
:	cmp     (off_FA),y
	bne     :+
	dey                                     ; B2CE 88                       .
	bpl     :-
:	lda     #$9B                            ; B2D1 A9 9B                    ..
	iny                                     ; B2D3 C8                       .
	beq     LB2D7                           ; B2D4 F0 01                    ..
	iny                                     ; B2D6 C8                       .
LB2D7:  sty     $0378                           ; B2D7 8C 78 03                 .x.
	ldx     #$30                            ; B2DA A2 30                    .0
LB2DC:  ldy     #$09                            ; B2DC A0 09                    ..
	sty     ICCOM+$30
	ldy     off_FA
	sty     ICBA+$30
	ldy     off_FA+1
	sty     ICBA+1+$30
	jsr     CIOV
	lda     #$40                            ; B2EE A9 40                    .@
	clc                                     ; B2F0 18                       .
	adc     off_FA
	sta     off_FA
	bcc     LB2F9
	inc     off_FA+1
LB2F9:	rts                                     ; B2F9 60                       `

; ----------------------------------------------------------------------------

sub_b2fa:
	tsx                                     ; B2FA BA                       .
	stx     byte_1344
	sta     byte_1347
	ldx     byte_1346
	bne     LB312                           ; B304 D0 0C                    ..
	ldx     TSTDAT
	beq     LB353                           ; B308 F0 49                    .I
	lda     #$80                            ; B30A A9 80                    ..
	sta     byte_1346
	ldy     #$01                            ; B30F A0 01                    ..
	rts                                     ; B311 60                       `

; ----------------------------------------------------------------------------
LB312:  bpl     LB322                           ; B312 10 0E                    ..
	lda     byte_1347
	sta     byte_1343
	tax                                     ; B31A AA                       .
	lda     #$00                            ; B31B A9 00                    ..
	sta     byte_1346
	beq     LB325                           ; B320 F0 03                    ..
LB322:  ldx     byte_1343
LB325:  cpx     #$46                            ; B325 E0 46                    .F
	beq     LB339                           ; B327 F0 10                    ..
	ldx     #$02                            ; B329 A2 02                    ..
	jsr     sub_b422
	lda     $133C                           ; B32E AD 3C 13                 .<.
	ora     #$01                            ; B331 09 01                    ..
	sta     $133C                           ; B333 8D 3C 13                 .<.
	ldy     #$01                            ; B336 A0 01                    ..
	rts                                     ; B338 60                       `

; ----------------------------------------------------------------------------
LB339:  lda     $133C                           ; B339 AD 3C 13                 .<.
	and     #$F1                            ; B33C 29 F1                    ).
	sta     $02EB                           ; B33E 8D EB 02                 ...
	lda     $133D                           ; B341 AD 3D 13                 .=.
	and     #$F1                            ; B344 29 F1                    ).
	sta     $02EA                           ; B346 8D EA 02                 ...
	ldy     #$00                            ; B349 A0 00                    ..
	sty     $02ED                           ; B34B 8C ED 02                 ...
	sty     $133D                           ; B34E 8C 3D 13                 .=.
	iny                                     ; B351 C8                       .
	rts                                     ; B352 60                       `

; ----------------------------------------------------------------------------
LB353:  lda     byte_1340
	cmp     #$1F                            ; B356 C9 1F                    ..
	bcs     LB353                           ; B358 B0 F9                    ..
	sei                                     ; B35A 78                       x
	jsr     sub_b369
	lda     $CB                             ; B35E A5 CB                    ..
	bne     LB365                           ; B360 D0 03                    ..
	jsr     sub_b545
LB365:  cli                                     ; B365 58                       X
	ldy     #$01                            ; B366 A0 01                    ..
	rts                                     ; B368 60                       `

; ----------------------------------------------------------------------------
sub_b369:
	ldy     $1342                           ; B369 AC 42 13                 .B.
	lda     byte_1347
	sta     $1310,y                         ; B36F 99 10 13                 ...
	jsr     sub_b55c
	sta     $1342                           ; B375 8D 42 13                 .B.
	inc     byte_1340
	rts                                     ; B37B 60                       `

; ----------------------------------------------------------------------------

sub_b37c:
	tsx                                     ; B37C BA                       .
	stx     byte_1344
LB380:  cli                                     ; B380 58                       X
	sei                                     ; B381 78                       x
	ldy     $133F                           ; B382 AC 3F 13                 .?.
	cpy     byte_133e
	beq     LB380                           ; B388 F0 F6                    ..

sub_b38a:  
	lda     $0F00,y                         ; B38A B9 00 0F                 ...
	and     #$7F                            ; B38D 29 7F                    ).
	sta     byte_1347
	iny                                     ; B392 C8                       .
	sty     $133F                           ; B393 8C 3F 13                 .?.
	dec     byte_CC
	cli                                     ; B398 58                       X
	ldy     #$01                            ; B399 A0 01                    ..
	rts                                     ; B39B 60                       `

; ----------------------------------------------------------------------------

sub_b39c:
	cld                                     ; B39C D8                       .
	tya                                     ; B39D 98                       .
	pha                                     ; B39E 48                       H
	lda     $D20D                           ; B39F AD 0D D2                 ...
	jsr     LB3B9                           ; B3A2 20 B9 B3                  ..
	lda     $D20F                           ; B3A5 AD 0F D2                 ...
	sta     $D20A                           ; B3A8 8D 0A D2                 ...
	eor     #$FF                            ; B3AB 49 FF                    I.
	and     #$C0                            ; B3AD 29 C0                    ).
	ora     $133D                           ; B3AF 0D 3D 13                 .=.
	sta     $133D                           ; B3B2 8D 3D 13                 .=.
LB3B5:  pla                                     ; B3B5 68                       h
	tay                                     ; B3B6 A8                       .
	pla                                     ; B3B7 68                       h
	rti                                     ; B3B8 40                       @

; ----------------------------------------------------------------------------
LB3B9:  ldy     byte_133e
	sta     $0F00,y                         ; B3BC 99 00 0F                 ...
	iny                                     ; B3BF C8                       .
	sty     byte_133e
	inc     byte_CC
	rts                                     ; B3C5 60                       `

; ----------------------------------------------------------------------------
sub_b3c6:
	cld                                     ; B3C6 D8                       .
	tya                                     ; B3C7 98                       .
	pha                                     ; B3C8 48                       H
	lda     byte_1340
	bne     :+
	lda     #$E7                            ; B3CE A9 E7                    ..
	and     POKMSK
	jsr     sub_b549
	jmp     LB3B5                           ; B3D5 4C B5 B3                 L..

; ----------------------------------------------------------------------------
:	jsr     LB3E7                           ; B3D8 20 E7 B3                  ..
	sta     $D20D                           ; B3DB 8D 0D D2                 ...
LB3DE:  lda     IRQST
	and     #$08                            ; B3E1 29 08                    ).
	beq     LB3DE                           ; B3E3 F0 F9                    ..
	bne     LB3B5                           ; B3E5 D0 CE                    ..
LB3E7:  ldy     $1341                           ; B3E7 AC 41 13                 .A.
	lda     $1310,y                         ; B3EA B9 10 13                 ...
	pha                                     ; B3ED 48                       H
	jsr     sub_b55c
	sta     $1341                           ; B3F1 8D 41 13                 .A.
	dec     byte_1340
	pla                                     ; B3F7 68                       h
	rts                                     ; B3F8 60                       `

; ----------------------------------------------------------------------------

sub_b3f9:
	cld                                     ; B3F9 D8                       .
	tya                                     ; B3FA 98                       .
	pha                                     ; B3FB 48                       H
	lda     $CB                             ; B3FC A5 CB                    ..
	beq     LB415                           ; B3FE F0 15                    ..
	lda     #$00                            ; B400 A9 00                    ..
	sta     $CB                             ; B402 85 CB                    ..
	beq     LB3B5                           ; B404 F0 AF                    ..

sub_b406:
	cld                                     ; B406 D8                       .
	tya                                     ; B407 98                       .
	pha                                     ; B408 48                       H
	lda     $133C                           ; B409 AD 3C 13                 .<.
	eor     #$80                            ; B40C 49 80                    I.
	sta     $133C                           ; B40E 8D 3C 13                 .<.
	and     #$80                            ; B411 29 80                    ).
	bne     LB3B5                           ; B413 D0 A0                    ..
LB415:  lda     $133C                           ; B415 AD 3C 13                 .<.
	and     #$3E                            ; B418 29 3E                    )>
	sta     $133C                           ; B41A 8D 3C 13                 .<.
	jmp     LB3B5                           ; B41D 4C B5 B3                 L..

; ----------------------------------------------------------------------------
sub_b420:
	ldx     #$00                            ; B420 A2 00                    ..

sub_b422:
	lda     $CB                             ; B422 A5 CB                    ..
	bne     sub_b422
	jsr     sub_b53a
:	lda     IRQST
	and     #$08                            ; B42C 29 08                    ).
	bne     :-
	lda     #$35                            ; B430 A9 35                    .5
	sta     PBCTL
	sta     $CB                             ; B435 85 CB                    ..
	sei                                     ; B437 78                       x
	jsr     sub_b369
	cli                                     ; B43B 58                       X
	jsr     sub_b53a

sub_b43f:
	ldy     LB47A,x                         ; B43F BC 7A B4                 .z.
	ldx     #$00                            ; B442 A2 00                    ..
	jsr     sub_b51f
LB447:  lda     $CB                             ; B447 A5 CB                    ..
	beq     LB46B                           ; B449 F0 20                    . 
	lda     byte_1345
	bne     LB447                           ; B44E D0 F7                    ..
	sta     byte_1346
	sta     $CB                             ; B453 85 CB                    ..
	lda     #$3D                            ; B455 A9 3D                    .=
	sta     PBCTL
	lda     byte_133a
	and     #$10                            ; B45D 29 10                    ).
	beq     LB464                           ; B45F F0 03                    ..
	sec                                     ; B461 38                       8
	bcs     LB471                           ; B462 B0 0D                    ..
LB464:  ldx     byte_1344
	txs                                     ; B467 9A                       .
	ldy     #$8B                            ; B468 A0 8B                    ..
	rts                                     ; B46A 60                       `

; ----------------------------------------------------------------------------
LB46B:  lda     #$3D                            ; B46B A9 3D                    .=
	sta     PBCTL
	clc                                     ; B470 18                       .
LB471:  lda     byte_133a
	and     #$EF                            ; B474 29 EF                    ).
	sta     byte_133a
	rts                                     ; B479 60                       `

; ----------------------------------------------------------------------------
LB47A:  php                                     ; B47A 08                       .
	.byte   $3C                             ; B47B 3C                       <
	.byte   $B4                             ; B47C B4                       .

sub_b47d:
	sei                                     ; B47D 78                       x
	jsr     sub_b4a5
	ldy     #$07                            ; B481 A0 07                    ..
	lda     #$00                            ; B483 A9 00                    ..
:	sta     $133C,y
	dey                                     ; B488 88                       .
	bpl     :-
	sta     TSTDAT
	sta     byte_1346
	lda     #$C7                            ; B490 A9 C7                    ..
	and     POKMSK
	ora     #$20                            ; B494 09 20                    . 
	jsr     sub_b549
	cli                                     ; B499 58                       X
	rts                                     ; B49A 60                       `

; ----------------------------------------------------------------------------
LB49B:
	.addr	sub_b39c
	.addr	sub_b3c6
	.addr	sub_b3c6

LB4A1:
	.addr	sub_b3f9
	.addr	sub_b406

sub_b4a5:
	lda     #$07                            ; B4A5 A9 07                    ..
	and     $0232                           ; B4A7 2D 32 02                 -2.
	ora     #$70                            ; B4AA 09 70                    .p
	sta     $0232                           ; B4AC 8D 32 02                 .2.
	sta     $D20F                           ; B4AF 8D 0F D2                 ...
	sta     $D20A                           ; B4B2 8D 0A D2                 ...
	lda     #$78                            ; B4B5 A9 78                    .x
	sta     AUDCTL
	ldx     #$07                            ; B4BA A2 07                    ..
	lda     #$A0                            ; B4BC A9 A0                    ..
LB4BE:  sta     $D200,x                         ; B4BE 9D 00 D2                 ...
	dex                                     ; B4C1 CA                       .
	bpl     LB4BE                           ; B4C2 10 FA                    ..
	lda     #$0B                            ; B4C4 A9 0B                    ..
	sta     $D202                           ; B4C6 8D 02 D2                 ...
	sta     $D206                           ; B4C9 8D 06 D2                 ...
	ldx     #$05                            ; B4CC A2 05                    ..
LB4CE:  lda     $020A,x                         ; B4CE BD 0A 02                 ...
	sta     $1330,x                         ; B4D1 9D 30 13                 .0.
	lda     LB49B,x                         ; B4D4 BD 9B B4                 ...
	sta     $020A,x                         ; B4D7 9D 0A 02                 ...
	dex                                     ; B4DA CA                       .
	bpl     LB4CE                           ; B4DB 10 F1                    ..
	ldx     #$03                            ; B4DD A2 03                    ..
LB4DF:  lda     $0202,x                         ; B4DF BD 02 02                 ...
	sta     $1336,x                         ; B4E2 9D 36 13                 .6.
	lda     LB4A1,x                         ; B4E5 BD A1 B4                 ...
	sta     $0202,x                         ; B4E8 9D 02 02                 ...
	dex                                     ; B4EB CA                       .
	bpl     LB4DF                           ; B4EC 10 F1                    ..
	lda     PACTL
	ora     #$01                            ; B4F1 09 01                    ..
	sta     PACTL
	rts                                     ; B4F6 60                       `

; ----------------------------------------------------------------------------
LB4F7:  sei                                     ; B4F7 78                       x
	ldy     #$05                            ; B4F8 A0 05                    ..
LB4FA:  lda     $1330,y                         ; B4FA B9 30 13                 .0.
	sta     $020A,y                         ; B4FD 99 0A 02                 ...
	dey                                     ; B500 88                       .
	bpl     LB4FA                           ; B501 10 F7                    ..
	ldy     #$03                            ; B503 A0 03                    ..
LB505:  lda     $1336,y                         ; B505 B9 36 13                 .6.
	sta     $0202,y                         ; B508 99 02 02                 ...
	dey                                     ; B50B 88                       .
	bpl     LB505                           ; B50C 10 F7                    ..
	lda     PACTL
	and     #$FE                            ; B511 29 FE                    ).
	sta     PACTL
	lda     #$C7                            ; B516 A9 C7                    ..
	and     POKMSK
	jsr     sub_b549
	cli                                     ; B51D 58                       X
	rts                                     ; B51E 60                       `

; ----------------------------------------------------------------------------
sub_b51f:
	lda     #<sub_b534
	sta     CDTMA1
	lda     #>sub_b534
	sta     CDTMA1+1
	sei                                     ; B529 78                       x
	lda     #$01                            ; B52A A9 01                    ..
	sta     byte_1345
	jsr	SETVBV
	cli                                     ; B532 58                       X
	rts                                     ; B533 60                       `

; ----------------------------------------------------------------------------

sub_b534:
	lda     #$00                            ; B534 A9 00                    ..
	sta     byte_1345
	rts                                     ; B539 60                       `

; ----------------------------------------------------------------------------
sub_b53a:  
	sei                                     ; B53A 78                       x
	jsr     sub_b545
	cli                                     ; B53E 58                       X
:	lda     byte_1340
	bne     :-
	rts                                     ; B544 60                       `

; ----------------------------------------------------------------------------
sub_b545:
	lda     POKMSK
	ora     #$18                            ; B547 09 18                    ..

sub_b549:  
	sta     POKMSK
	sta     IRQEN
	rts                                     ; B54E 60                       `

; ----------------------------------------------------------------------------
sub_b54f:
	ldx     #$00
	ldy     #$03                            ; B551 A0 03                    ..
	jsr     sub_b51f
:	lda     byte_1345
	bne     :-
	rts                                     ; B55B 60                       `

; ----------------------------------------------------------------------------
sub_b55c:
	iny                                     ; B55C C8                       .
	tya                                     ; B55D 98                       .
	and     #$1F                            ; B55E 29 1F                    ).
	rts                                     ; B560 60                       `

; ----------------------------------------------------------------------------

sub_b561:
	tsx                                     ; B561 BA                       .
	stx     byte_1344
	lda     $2A                             ; B565 A5 2A                    .*
	sta     byte_133a
	lda     #$00                            ; B56A A9 00                    ..
	sta     TSTDAT
	jsr     sub_b47d
	jsr     sub_b54f
	lda     #$59                            ; B574 A9 59                    .Y
	sta     byte_1347
	ldx     #$01                            ; B579 A2 01                    ..
	jsr     sub_b422
	jmp     LB5A5                           ; B57E 4C A5 B5                 L..

; ----------------------------------------------------------------------------

sub_b581:
	tsx                                     ; B581 BA                       .
	stx     byte_1344
	lda     byte_133a
	beq     LB5A5                           ; B588 F0 1B                    ..
	jsr     sub_b53a
	lda     byte_B2
	bne     LB595                           ; B58F D0 04                    ..
	lda     #$51                            ; B591 A9 51                    .Q
	bne     LB597                           ; B593 D0 02                    ..
LB595:  lda     #$5A                            ; B595 A9 5A                    .Z
LB597:  sta     byte_1347
	jsr     sub_b420
	jsr     LB4F7                           ; B59D 20 F7 B4                  ..
	lda     #$00                            ; B5A0 A9 00                    ..
	sta     byte_133a
LB5A5:  ldy     #$01                            ; B5A5 A0 01                    ..
	rts                                     ; B5A7 60                       `

; ----------------------------------------------------------------------------

t_handler:
	.addr	sub_b561-1
	.addr	sub_b581-1
	.addr	sub_b37c-1
	.addr	sub_b2fa-1

LB5B0:  lda     #$00                            ; B5B0 A9 00                    ..
	sta     PACTL
	lda     PORTA
	ora     #$50                            ; B5B8 09 50                    .P
	sta     PORTA
	lda     #$3C                            ; B5BD A9 3C                    .<
	sta     PACTL
	ldx     #$00                            ; B5C2 A2 00                    ..
	lda     #'R'
	jsr     sub_bf86
	bne     LB5D5                           ; B5C9 D0 0A                    ..
	lda     #<LB5D6
	sta     HATABS+1,x
	lda     #>LB5D6
	sta     HATABS+2,x
LB5D5:  rts                                     ; B5D5 60                       `

; ----------------------------------------------------------------------------
LB5D6:
	.addr	$B667
	.addr	$B6A4
	.addr	$B708
	.addr	$B6E8

sub_b5de:
	cld
	tya                                     ; B5DF 98                       .
	pha                                     ; B5E0 48                       H
	lda     $FD                             ; B5E1 A5 FD                    ..
	bne     LB5F6                           ; B5E3 D0 11                    ..
	lda     #$20                            ; B5E5 A9 20                    . 
	and     PORTA
	beq     LB62B                           ; B5EA F0 3F                    .?
	lda     #$08                            ; B5EC A9 08                    ..
	sta     $FD                             ; B5EE 85 FD                    ..
	lsr     a                               ; B5F0 4A                       J
	sta     $134B                           ; B5F1 8D 4B 13                 .K.
	bne     LB62B                           ; B5F4 D0 35                    .5
LB5F6:  dec     $134B                           ; B5F6 CE 4B 13                 .K.
	bne     LB62B                           ; B5F9 D0 30                    .0
	lda     $134C                           ; B5FB AD 4C 13                 .L.
	beq     LB607                           ; B5FE F0 07                    ..
	dec     $FD                             ; B600 C6 FD                    ..
	dec     $134C                           ; B602 CE 4C 13                 .L.
	beq     LB62B                           ; B605 F0 24                    .$
LB607:  lda     #$20                            ; B607 A9 20                    . 
	and     PORTA
	beq     LB611                           ; B60C F0 03                    ..
	sec                                     ; B60E 38                       8
	bcs     LB612                           ; B60F B0 01                    ..
LB611:  clc                                     ; B611 18                       .
LB612:  ror     $1348                           ; B612 6E 48 13                 nH.
	dec     $FD                             ; B615 C6 FD                    ..
	bne     LB626                           ; B617 D0 0D                    ..
	lda     $1348                           ; B619 AD 48 13                 .H.
	eor     #$FF                            ; B61C 49 FF                    I.
	jsr     LB3B9                           ; B61E 20 B9 B3                  ..
	inc     $FD                             ; B621 E6 FD                    ..
	inc     $134C                           ; B623 EE 4C 13                 .L.
LB626:  lda     #$03                            ; B626 A9 03                    ..
	sta     $134B                           ; B628 8D 4B 13                 .K.
LB62B:  lda     $FE                             ; B62B A5 FE                    ..
	beq     LB655                           ; B62D F0 26                    .&
	dec     $134A                           ; B62F CE 4A 13                 .J.
	bne     LB652                           ; B632 D0 1E                    ..
	dec     $FE                             ; B634 C6 FE                    ..
	beq     LB655                           ; B636 F0 1D                    ..
	clc                                     ; B638 18                       .
	ror     $1349                           ; B639 6E 49 13                 nI.
	bcs     LB645                           ; B63C B0 07                    ..
	lda     #$EF                            ; B63E A9 EF                    ..
	and     PORTA
	bcc     LB64A                           ; B643 90 05                    ..
LB645:  lda     #$10                            ; B645 A9 10                    ..
	ora     PORTA
LB64A:  sta     PORTA
	lda     #$03                            ; B64D A9 03                    ..
	sta     $134A                           ; B64F 8D 4A 13                 .J.
LB652:  jmp     LB3B5                           ; B652 4C B5 B3                 L..

; ----------------------------------------------------------------------------
LB655:  lda     byte_1340
	beq     LB652                           ; B658 F0 F8                    ..
	jsr     LB3E7                           ; B65A 20 E7 B3                  ..
	eor     #$FF                            ; B65D 49 FF                    I.
	sta     $1349                           ; B65F 8D 49 13                 .I.
	lda     #$0A                            ; B662 A9 0A                    ..
	sta     $FE                             ; B664 85 FE                    ..
	bne     LB645                           ; B666 D0 DD                    ..
	lda     #$13                            ; B668 A9 13                    ..
	sta     $D20F                           ; B66A 8D 0F D2                 ...
	sta     $0232                           ; B66D 8D 32 02                 .2.
	lda     PORTA
	and     #$BF                            ; B673 29 BF                    ).
	sta     PORTA
	lda     #$00                            ; B678 A9 00                    ..
	sta     $FE                             ; B67A 85 FE                    ..
	sta     $FD                             ; B67C 85 FD                    ..
	sta     $134C                           ; B67E 8D 4C 13                 .L.
	sta     AUDCTL
	lda     #$45                            ; B684 A9 45                    .E
	sta     $D200                           ; B686 8D 00 D2                 ...
	lda     #$A0                            ; B689 A9 A0                    ..
	sta     $D201                           ; B68B 8D 01 D2                 ...
	lda     #<sub_b5de
	sta     VTIMR1
	lda     #>sub_b5de
	sta     VTIMR1+1
	lda     POKMSK
	ora     #$01                            ; B69A 09 01                    ..
	jsr     sub_b549
	sta     STIMER
	ldy     #$01                            ; B6A2 A0 01                    ..
	rts                                     ; B6A4 60                       `

; ----------------------------------------------------------------------------
LB6A5:  lda     byte_1340
	bne     LB6A5                           ; B6A8 D0 FB                    ..
	lda     $FE                             ; B6AA A5 FE                    ..
	bne     LB6A5                           ; B6AC D0 F7                    ..
	lda     #$FE                            ; B6AE A9 FE                    ..
	and     POKMSK
	jsr     sub_b549
	ldy     #$01                            ; B6B5 A0 01                    ..
	rts                                     ; B6B7 60                       `

; ----------------------------------------------------------------------------

sub_b6b8:
	lda     #$8D                            ; B6B8 A9 8D                    ..
	sta     byte_133a
	ldy     #$00                            ; B6BD A0 00                    ..
	sty     $1342                           ; B6BF 8C 42 13                 .B.
	sty     $133D                           ; B6C2 8C 3D 13                 .=.
	iny                                     ; B6C5 C8                       .
	sty     $CB                             ; B6C6 84 CB                    ..
	rts                                     ; B6C8 60                       `

; ----------------------------------------------------------------------------
sub_b6c9:  
	lda     $CB                             ; B6C9 A5 CB                    ..
	beq     sub_b6c9
	ldy     #$05                            ; B6CD A0 05                    ..
	sei                                     ; B6CF 78                       x
:	lda     $1330,y                         ; B6D0 B9 30 13                 .0.
	sta     $020A,y                         ; B6D3 99 0A 02                 ...
	dey                                     ; B6D6 88                       .
	bpl     :-
	cli                                     ; B6D9 58                       X
	sty     byte_1346
	iny                                     ; B6DD C8                       .
	sty     $030A                           ; B6DE 8C 0A 03                 ...
	sty     byte_133a
	lda     #$57                            ; B6E4 A9 57                    .W
	jmp     sub_b802

; ----------------------------------------------------------------------------

sub_b6e9:
	sta     byte_1347
	ldx     #$01                            ; B6EC A2 01                    ..
	stx     $21                             ; B6EE 86 21                    .!
:	lda     byte_1340
	cmp     #$1F                            ; B6F3 C9 1F                    ..
	bcs     :-
	sei                                     ; B6F7 78                       x
	jsr     sub_b369
	lda     POKMSK
	ora     #$18                            ; B6FD 09 18                    ..
	jsr     sub_b549
	ldy     #$00                            ; B702 A0 00                    ..
	sty     $CB                             ; B704 84 CB                    ..
	cli                                     ; B706 58                       X
	iny                                     ; B707 C8                       .
	rts                                     ; B708 60                       `

; ----------------------------------------------------------------------------
sub_b709:
	cli                                     ; B709 58                       X
	sei                                     ; B70A 78                       x
	ldy     $133F                           ; B70B AC 3F 13                 .?.
	cpy     byte_133e
	beq     sub_b709
	jsr     sub_b38a
	ldy     #$01                            ; B716 A0 01                    ..
	rts                                     ; B718 60                       `

; ----------------------------------------------------------------------------
LB719:  jsr     LB7E7                           ; B719 20 E7 B7                  ..
LB71C:  ldy     #$00                            ; B71C A0 00                    ..
	jsr     sub_b206
	bpl     LB724                           ; B721 10 01                    ..
	rts                                     ; B723 60                       `

; ----------------------------------------------------------------------------
LB724:  lda     #$00                            ; B724 A9 00                    ..
	sta     byte_B2
	sta     byte_133e
	sta     $133F                           ; B72B 8D 3F 13                 .?.
	sta     $1342                           ; B72E 8D 42 13                 .B.
	sta     $1341                           ; B731 8D 41 13                 .A.
	sta     $0309                           ; B734 8D 09 03                 ...
	lda     #$30                            ; B737 A9 30                    .0
	sta     $0304                           ; B739 8D 04 03                 ...
	lda     #$13                            ; B73C A9 13                    ..
	sta     $0305                           ; B73E 8D 05 03                 ...
	lda     byte_133a
	sta     $030A                           ; B744 8D 0A 03                 ...
	lda     #$09                            ; B747 A9 09                    ..
	sta     $0308                           ; B749 8D 08 03                 ...
	ldy     #$40                            ; B74C A0 40                    .@
	lda     #$58                            ; B74E A9 58                    .X
	jsr     sub_b802
	bpl     LB756                           ; B753 10 01                    ..
	rts                                     ; B755 60                       `

; ----------------------------------------------------------------------------
LB756:  sei                                     ; B756 78                       x
	lda     #$73                            ; B757 A9 73                    .s
	sta     $D20F                           ; B759 8D 0F D2                 ...
	lda     $1338                           ; B75C AD 38 13                 .8.
	sta     AUDCTL
	ldy     #$07                            ; B762 A0 07                    ..
LB764:  lda     $1330,y                         ; B764 B9 30 13                 .0.
	sta     $D200,y                         ; B767 99 00 D2                 ...
	dey                                     ; B76A 88                       .
	bpl     LB764                           ; B76B 10 F7                    ..
	ldx     #$06                            ; B76D A2 06                    ..
LB76F:  lda     $0209,x                         ; B76F BD 09 02                 ...
	sta     $132F,x                         ; B772 9D 2F 13                 ./.
	lda     LB820+1,x
	sta     $0209,x                         ; B778 9D 09 02                 ...
	dex                                     ; B77B CA                       .
	bne     LB76F                           ; B77C D0 F1                    ..
	stx     byte_CC
	stx     $CD                             ; B780 86 CD                    ..
	lda     POKMSK
	ora     #$20                            ; B784 09 20                    . 
	jsr     sub_b549
	ldx     #$01                            ; B789 A2 01                    ..
	stx     byte_1346
	cli                                     ; B78E 58                       X
	ldy     #$01                            ; B78F A0 01                    ..
	lda     byte_133a
	sta     $2A                             ; B794 85 2A                    .*
	cpy     #$00                            ; B796 C0 00                    ..
	rts                                     ; B798 60                       `

; ----------------------------------------------------------------------------
sub_b799:
	lda     #$00                            ; B799 A9 00                    ..
	sta     byte_133a
	sta     $0309                           ; B79E 8D 09 03                 ...
	tax                                     ; B7A1 AA                       .
	jsr     sub_bf86
	beq     LB7A8                           ; B7A5 F0 01                    ..
	rts                                     ; B7A7 60                       `

; ----------------------------------------------------------------------------
LB7A8:  lda     #'R'
	sta     byte_1346
	sta     HATABS,x
	lda     #<LB81A
	sta     HATABS+1,x
	lda     #>LB81A
	sta     HATABS+2,x
	lda     #$EA                            ; B7BA A9 EA                    ..
	sta     $0304                           ; B7BC 8D 04 03                 ...
	lda     #$02                            ; B7BF A9 02                    ..
	sta     $0305                           ; B7C1 8D 05 03                 ...
	sta     $0308                           ; B7C4 8D 08 03                 ...
	ldy     #$40                            ; B7C7 A0 40                    .@
	lda     #$53                            ; B7C9 A9 53                    .S
	jsr     sub_b802
	lda     byte_1347
	ora     $02EA                           ; B7D1 0D EA 02                 ...
	sta     $02EA                           ; B7D4 8D EA 02                 ...
	cpy     #$00                            ; B7D7 C0 00                    ..
	rts                                     ; B7D9 60                       `

; ----------------------------------------------------------------------------

sub_b7da:
	lda     byte_1340
	bne     :+
	lda     byte_1346
	sta     $CB                             ; B7E2 85 CB                    ..
:	jmp	sub_b3c6

; ----------------------------------------------------------------------------
LB7E7:  lda     #$42                            ; B7E7 A9 42                    .B
	bit     $B1                             ; B7E9 24 B1                    $.
	bmi     :+
	ldx     #$0A                            ; B7ED A2 0A                    ..
	bne     LB7F3                           ; B7EF D0 02                    ..
:	ldx	#$00                            ; B7F1 A2 00                    ..
LB7F3:  jsr     LB7FA                           ; B7F3 20 FA B7                  ..
	lda     #$41                            ; B7F6 A9 41                    .A
	ldx     #$F3                            ; B7F8 A2 F3                    ..
LB7FA:  stx     $030A                           ; B7FA 8E 0A 03                 ...
	ldy     #$00                            ; B7FD A0 00                    ..
	sty     $030B                           ; B7FF 8C 0B 03                 ...

sub_b802:
	sta     DCOMND
	ldx     #$01                            ; B805 A2 01                    ..
	stx     $0301                           ; B807 8E 01 03                 ...
	sty     DSTATS
	ldy     #$50                            ; B80D A0 50                    .P
	sty     $0300                           ; B80F 8C 00 03                 ...
	ldy     #$08                            ; B812 A0 08                    ..
	sty     $0306                           ; B814 8C 06 03                 ...
	jmp     SIOV

; ----------------------------------------------------------------------------

LB81A:
	.addr	sub_b6b8-1
	.addr	sub_b6c9-1
	.addr	sub_b709-1
LB820:	.addr	sub_b6e9-1
	.addr	sub_b39c
	.addr	sub_b7da
	.addr	sub_b7da

LB828:  ldx     #$00                            ; B828 A2 00                    ..
	ldy     #$04                            ; B82A A0 04                    ..
LB82C:  txa                                     ; B82C 8A                       .
	and     #$0F                            ; B82D 29 0F                    ).
	ora     #$10                            ; B82F 09 10                    ..
	sta     $D40A                           ; B831 8D 0A D4                 ...
	sta     $D201                           ; B834 8D 01 D2                 ...
	inx                                     ; B837 E8                       .
	inx                                     ; B838 E8                       .
	sta     $D40A                           ; B839 8D 0A D4                 ...
	bne     LB82C                           ; B83C D0 EE                    ..
	dey                                     ; B83E 88                       .
	bne     LB82C                           ; B83F D0 EB                    ..
	lda     #<(LB863-1)
	ldy     #>(LB863-1)
	ldx     #$0C                            ; B845 A2 0C                    ..
	jmp     sub_b84b

; ----------------------------------------------------------------------------
	rts                                     ; B84A 60                       `

; ----------------------------------------------------------------------------
sub_b84b:
	sta     off_EC
	sty     off_EC+1
	ldy     #$00                            ; B84F A0 00                    ..
	lda     #$55                            ; B851 A9 55                    .U
	sec                                     ; B853 38                       8
:	iny                                     ; B854 C8                       .
	rol     a                               ; B855 2A                       *
	pha                                     ; B856 48                       H
	eor     (off_EC),y
	sta     L3E33-1,y
	pla                                     ; B85C 68                       h
	dex                                     ; B85D CA                       .
	bne     :-
	jmp     L3E33                           ; B860 4C 33 3E                 L3>

; ----------------------------------------------------------------------------
LB863:
	adc     $AA                             ; B863 65 AA                    e.
	.byte   $12                             ; B865 12                       .
	.byte   $F7                             ; B866 F7                       .
	eor     #$D5                            ; B867 49 D5                    I.
	and     $A9                             ; B869 25 A9                    %.
	ora     LB2DC,y                         ; B86B 19 DC B2                 ...
	.byte   $CD                             ; B86E CD                       .

LB86F:	.byte	$00,$00,$00,$00
	.byte	$00,$01,$FF,$00
	.byte	$00,$01,$FF,$00
	.byte	$00,$01,$FF,$00

LB87F:	.byte	$00,$00,$00,$00
	.byte	$00,$01,$01,$01
	.byte	$00,$FF,$FF,$FF
	.byte	$00,$00,$00,$00

LB88F:
	.byte	"K:",$9B			; B88F

LB892:
	.byte	"R:",$9B			; B892

LB895:
	.byte	"T:",$9B			; B895

LB898:
	.byte	"P:",$9B			; B898

LB89B:
	RString	"COMMUNICATION ERROR"

LB8AE:
	RString	"COPYRIGHT 1984 ATARI"

LB8C2:
	RString "WELCOME TO THE LEARNING PHONE"

LB8DF:
	RString "After the phone has a high pitch tone, PRESS RETURN!"
	RString "300  baud"
	RString "1200 baud"
	RString "Microbit 300 baud"

LB936:
	.byte	$00,$40,$80,$C0

LB93A:
	.byte   $7F,$BF,$DF,$EF,$F7,$FB,$FD,$FE
LB942:  .byte   $80,$40,$20,$10,$08,$04,$02,$01
	.byte	$E3
	sbc     (byte_E0,x)                         ; B94B E1 E0                    ..
	.byte   $E2                             ; B94D E2                       .
LB94E:  brk                                     ; B94E 00                       .
LB94F:  .byte   $7F                             ; B94F 7F                       .
	.byte   $3F                             ; B950 3F                       ?
	.byte   $1F                             ; B951 1F                       .
	.byte   $0F                             ; B952 0F                       .
LB953:  .byte   $07                             ; B953 07                       .
	.byte   $03                             ; B954 03                       .
	.byte   $01                             ; B955 01                       .
LB956:  .byte   $80                             ; B956 80                       .
	cpy     #$E0                            ; B957 C0 E0                    ..
	beq     LB953                           ; B959 F0 F8                    ..
	.byte   $FC                             ; B95B FC                       .
LB95C:  .byte   $FE                             ; B95C FE                       .
	brk                                     ; B95D 00                       .
LB95E:  .byte   $07                             ; B95E 07                       .
	.byte   $83                             ; B95F 83                       .
	cmp     (byte_E0,x)                         ; B960 C1 E0                    ..
	beq     LB95C                           ; B962 F0 F8                    ..
	.byte   $FC                             ; B964 FC                       .
	.byte   $FE                             ; B965 FE                       .
LB966:  brk                                     ; B966 00                       .
	brk                                     ; B967 00                       .
	brk                                     ; B968 00                       .
	brk                                     ; B969 00                       .
	.byte   $7F                             ; B96A 7F                       .
	.byte   $3F                             ; B96B 3F                       ?
	.byte   $1F                             ; B96C 1F                       .
	.byte   $0F                             ; B96D 0F                       .

LB96E:  .byte   $10,$03,$0D,$00
	.addr	LB892
	.byte	$00,$00

	.byte	$20,$03,$04,$00
	.addr	LB88F
	.byte	$00,$00

	.byte	$20,$07,$04,$00
	.byte	$00,$00
	.byte	$00,$00

	.byte	$10,$0B,$0D,$00
	.byte	$00,$00
	.byte	$00,$00

	.byte	$10,$07,$0D,$00
	.byte	$00,$00
	.byte	$00,$00

	.byte	$10,$03,$0D,$00
	.addr	LB895
	.byte	$00,$00

	.byte	$30,$03,$08,$00
	.addr	LB898
	.byte	$40,$00

LB9A6:	.byte	$06,$09,$00,>byte_BAEC
LB9AA:	.byte	$00,$00,$00,<byte_BAEC
LB9AE:	.byte	$00,$80,$44,$84
LB9B2:  .byte   $0C,$0D,$BC,$BE
LB9B6:	.byte	$08,$10,$10
	.byte	$20,$20,$40
	.byte   $80                             ; B9BC 80                       .
	.byte   $80                             ; B9BD 80                       .
LB9BE:  brk                                     ; B9BE 00                       .
	brk                                     ; B9BF 00                       .
	brk                                     ; B9C0 00                       .
	ora     ($01,x)                         ; B9C1 01 01                    ..
	ora     ($02,x)                         ; B9C3 01 02                    ..
	.byte   $02                             ; B9C5 02                       .
	.byte   $03                             ; B9C6 03                       .
	.byte   $03                             ; B9C7 03                       .
	.byte   $04                             ; B9C8 04                       .
LB9C9:  .byte   $04                             ; B9C9 04                       .
	.byte   $04                             ; B9CA 04                       .
	ora     $05                             ; B9CB 05 05                    ..
	.byte   $05                             ; B9CD 05                       .
LB9CE:  brk                                     ; B9CE 00                       .
	brk                                     ; B9CF 00                       .
	ora     ($02,x)                         ; B9D0 01 02                    ..
	.byte   $02                             ; B9D2 02                       .
	.byte   $03                             ; B9D3 03                       .
	.byte   $03                             ; B9D4 03                       .
	.byte   $04                             ; B9D5 04                       .
LB9D6:  .byte   $03                             ; B9D6 03                       .
	.byte   $02                             ; B9D7 02                       .
	.byte   $03                             ; B9D8 03                       .
	.byte   $03                             ; B9D9 03                       .
	.byte   $02                             ; B9DA 02                       .
	.byte   $03                             ; B9DB 03                       .
	.byte   $02                             ; B9DC 02                       .
	.byte   $03                             ; B9DD 03                       .
	.byte   $03                             ; B9DE 03                       .
	.byte   $02                             ; B9DF 02                       .
	.byte   $02                             ; B9E0 02                       .
	ora     ($02,x)                         ; B9E1 01 02                    ..
	.byte   $02                             ; B9E3 02                       .
	ora     ($02,x)                         ; B9E4 01 02                    ..
	ora     ($02,x)                         ; B9E6 01 02                    ..
	.byte   $02                             ; B9E8 02                       .
	ora     ($03,x)                         ; B9E9 01 03                    ..
	.byte   $02                             ; B9EB 02                       .
	.byte   $03                             ; B9EC 03                       .
	.byte   $03                             ; B9ED 03                       .
	.byte   $02                             ; B9EE 02                       .
	.byte   $03                             ; B9EF 03                       .
	.byte   $02                             ; B9F0 02                       .
	.byte   $03                             ; B9F1 03                       .
	.byte   $03                             ; B9F2 03                       .
	.byte   $02                             ; B9F3 02                       .
LB9F4:  brk                                     ; B9F4 00                       .
	ora     $0A                             ; B9F5 05 0A                    ..
	.byte   $0F                             ; B9F7 0F                       .
	.byte   $14                             ; B9F8 14                       .
	.byte   $19                             ; B9F9 19                       .
LB9FA:  brk                                     ; B9FA 00                       .
	brk                                     ; B9FB 00                       .
	ora     ($01,x)                         ; B9FC 01 01                    ..
	ora     ($01,x)                         ; B9FE 01 01                    ..
	ora     ($02,x)                         ; BA00 01 02                    ..
LBA02:  brk                                     ; BA02 00                       .
	.byte   $03                             ; BA03 03                       .
	.byte   $0C                             ; BA04 0C                       .
	.byte   $0F                             ; BA05 0F                       .
	bmi     LBA3B                           ; BA06 30 33                    03
	.byte   $3C                             ; BA08 3C                       <
	.byte   $3F                             ; BA09 3F                       ?
	cpy     #$C3                            ; BA0A C0 C3                    ..
	cpy     $F0CF                           ; BA0C CC CF F0                 ...
	.byte   $F3                             ; BA0F F3                       .
	.byte   $FC                             ; BA10 FC                       .
	.byte   $FF                             ; BA11 FF                       .
LBA12:  asl     $23                             ; BA12 06 23                    .#
	ror     $6026,x                         ; BA14 7E 26 60                 ~&`
	.byte   $27                             ; BA17 27                       '
	.byte   $07                             ; BA18 07                       .
	rol     $40                             ; BA19 26 40                    &@
	rol     $5E,x                           ; BA1B 36 5E                    6^
	.byte   $5C                             ; BA1D 5C                       \
	rol     $1101,x                         ; BA1E 3E 01 11                 >..
	.byte   $0F                             ; BA21 0F                       .
	.byte   $04                             ; BA22 04                       .
	ora     $0E                             ; BA23 05 0E                    ..
	.byte   $13                             ; BA25 13                       .
	.byte   $17                             ; BA26 17                       .
	.byte   $37                             ; BA27 37                       7
	adc     $1200,x                         ; BA28 7D 00 12                 }..
	.byte   $03                             ; BA2B 03                       .
	asl     $2A,x                           ; BA2C 16 2A                    .*
	.byte   $1A                             ; BA2E 1A                       .
	clc                                     ; BA2F 18                       .
	.byte   $3F                             ; BA30 3F                       ?
	.byte   $07                             ; BA31 07                       .
	.byte   $07                             ; BA32 07                       .
	and     $1414                           ; BA33 2D 14 14                 -..
	and     $090B,y                         ; BA36 39 0B 09                 9..
	.byte   $3A                             ; BA39 3A                       :
LBA3A:  .byte   $12                             ; BA3A 12                       .
LBA3B:  .byte   $1D                             ; BA3B 1D                       .
	brk                                     ; BA3C 00                       .
LBA3D:  .byte   $0C                             ; BA3D 0C                       .
	.byte   $0F                             ; BA3E 0F                       .
	.byte   $15                             ; BA3F 15                       .
LBA40:  .byte   $02                             ; BA40 02                       .
	.byte   $0E                             ; BA41 0E                       .
	.byte   $23                             ; BA42 23                       #
LBA43:  .byte   $0D                             ; BA43 0D                       .
	.byte   $1E                             ; BA44 1E                       .
LBA45:  .byte   $61                             ; BA45 61                       a
LBA46:  .byte   $1F                             ; BA46 1F                       .
	.byte   $74                             ; BA47 74                       t
	ora     $0834,y                         ; BA48 19 34 08                 .4.
	.byte   $73                             ; BA4B 73                       s
	.byte   $7C                             ; BA4C 7C                       |
	bit     $4C0A                           ; BA4D 2C 0A 4C                 ,.L
	asl     $0D0C,x                         ; BA50 1E 0C 0D                 ...
LBA53:  .byte   $46                             ; BA53 46                       F
LBA54:  .byte   $2F                             ; BA54 2F                       /
	.byte   $47                             ; BA55 47                       G
	sei                                     ; BA56 78                       x
	.byte   $4F                             ; BA57 4F                       O
	eor     #$5A                            ; BA58 49 5A                    IZ
	bit     $5B                             ; BA5A 24 5B                    $[
	.byte   $2B                             ; BA5C 2B                       +
	adc     $35,x                           ; BA5D 75 35                    u5
	.byte   $02                             ; BA5F 02                       .
	.byte   $BB                             ; BA60 BB                       .
	.byte   $5A                             ; BA61 5A                       Z
	bmi     LBAC3                           ; BA62 30 5F                    0_
	inc     LA83D                           ; BA64 EE 3D A8                 .=.
LBA67:  bvs     LBAA9                           ; BA67 70 40                    p@
	bvs     LBAAB                           ; BA69 70 40                    p@
	rti                                     ; BA6B 40                       @

; ----------------------------------------------------------------------------
	asl     $0F                             ; BA6C 06 0F                    ..
	.byte   $06                             ; BA6E 06                       .
LBA6F:  ror     $76,x                           ; BA6F 76 76                    vv
	ror     $76,x                           ; BA71 76 76                    vv
	ror     $76,x                           ; BA73 76 76                    vv
	ror     $76,x                           ; BA75 76 76                    vv
	.byte   $77                             ; BA77 77                       w
	pha                                     ; BA78 48                       H
	.byte   $5B                             ; BA79 5B                       [
	.byte   $A3                             ; BA7A A3                       .
	adc     ($4B,x)                         ; BA7B 61 4B                    aK
	ror     $76,x                           ; BA7D 76 76                    vv
	ror     $76,x                           ; BA7F 76 76                    vv
	ror     $76,x                           ; BA81 76 76                    vv
	ror     $76,x                           ; BA83 76 76                    vv
	ror     $76,x                           ; BA85 76 76                    vv
	ror     $03,x                           ; BA87 76 03                    v.
	ror     $C4,x                           ; BA89 76 C4                    v.
	.byte   $03                             ; BA8B 03                       .
	.byte   $03                             ; BA8C 03                       .
	ror     $03,x                           ; BA8D 76 03                    v.
	.byte	$76,$20
	.byte   $1C                             ; BA91 1C                       .
	.byte	$20,$76,$76
	ror     $76,x                           ; BA95 76 76                    vv
	ror     $76,x                           ; BA97 76 76                    vv
	ror     $76,x                           ; BA99 76 76                    vv
	brk                                     ; BA9B 00                       .
	ror     $76,x                           ; BA9C 76 76                    vv
	ror     $76,x                           ; BA9E 76 76                    vv
	.byte   $13                             ; BAA0 13                       .
	.byte   $13                             ; BAA1 13                       .
	.byte   $13                             ; BAA2 13                       .
	.byte   $13                             ; BAA3 13                       .
	ror     $76,x                           ; BAA4 76 76                    vv
	ror     $76,x                           ; BAA6 76 76                    vv
	.byte   $76                             ; BAA8 76                       v
LBAA9:  ror     $C4,x                           ; BAA9 76 C4                    v.
LBAAB:  ror     $76,x                           ; BAAB 76 76                    vv
	ror     $76,x                           ; BAAD 76 76                    vv
	.byte   $2F                             ; BAAF 2F                       /
	.byte   $33                             ; BAB0 33                       3
	lda     #$A9                            ; BAB1 A9 A9                    ..
	lda     #$A9                            ; BAB3 A9 A9                    ..
	ror     $76,x                           ; BAB5 76 76                    vv
	ror     $76,x                           ; BAB7 76 76                    vv
	ror     $76,x                           ; BAB9 76 76                    vv
	ror     $76,x                           ; BABB 76 76                    vv
	rol     $2A                             ; BABD 26 2A                    &*
	.byte   $03                             ; BABF 03                       .
	cmp     $CA,x                           ; BAC0 D5 CA                    ..
	dex                                     ; BAC2 CA                       .
LBAC3:  dex                                     ; BAC3 CA                       .
	cmp     ($CA),y                         ; BAC4 D1 CA                    ..
	cmp     $D976,x                         ; BAC6 DD 76 D9                 .v.
	.byte   $37                             ; BAC9 37                       7
LBACA:  ldx     $A4                             ; BACA A6 A4                    ..
	brk                                     ; BACC 00                       .
	brk                                     ; BACD 00                       .
	ldx     $AD                             ; BACE A6 AD                    ..
	brk                                     ; BAD0 00                       .
	.byte   $AB                             ; BAD1 AB                       .
LBAD2:  sbc     $47,x                           ; BAD2 F5 47                    .G
	brk                                     ; BAD4 00                       .
	brk                                     ; BAD5 00                       .
	sta     ($F0,x)                         ; BAD6 81 F0                    ..
	brk                                     ; BAD8 00                       .
	.byte   $59                             ; BAD9 59                       Y
LBADA:  .byte   $03                             ; BADA 03                       .
	.byte   $80                             ; BADB 80                       .
	brk                                     ; BADC 00                       .
	brk                                     ; BADD 00                       .
	.byte   $80                             ; BADE 80                       .
	.byte   $80                             ; BADF 80                       .
	brk                                     ; BAE0 00                       .
	.byte   $01                             ; BAE1 01                       .

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
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %00000010                       ; ......#.
	.byte   %00000100                       ; .....#..
	.byte   %00001000                       ; ....#...
	.byte   %00010000                       ; ...#....
	.byte   %00100000                       ; ..#.....
	.byte   %01000000                       ; .#......
	.byte   %10000000                       ; #.......

	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %01111100                       ; .#####..
	.byte   %00000000                       ; ........
	.byte   %01111100                       ; .#####..
	.byte   %00000000                       ; ........
	.byte   %01111100                       ; .#####..
	.byte   %00000000                       ; ........

	.byte   %00110010                       ; ..##..#.
	.byte   %01001100                       ; .#..##..
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %00010000                       ; ...#....
	.byte   %00110000                       ; ..##....
	.byte   %01011110                       ; .#.####.
	.byte   %10000000                       ; #.......
	.byte   %01011110                       ; .#.####.
	.byte   %00110000                       ; ..##....
	.byte   %00010000                       ; ...#....

	.byte   %00000000                       ; ........
	.byte   %00000010                       ; ......#.
	.byte   %00000100                       ; .....#..
	.byte   %11111110                       ; #######.
	.byte   %00010000                       ; ...#....
	.byte   %11111110                       ; #######.
	.byte   %01000000                       ; .#......
	.byte   %10000000                       ; #.......

	.byte   %00000000                       ; ........
	.byte   %00010000                       ; ...#....
	.byte   %00111000                       ; ..###...
	.byte   %01111100                       ; .#####..
	.byte   %00010000                       ; ...#....
	.byte   %00010000                       ; ...#....
	.byte   %00010000                       ; ...#....
	.byte   %00010000                       ; ...#....

	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00001000                       ; ....#...
	.byte   %00001100                       ; ....##..
	.byte   %11111110                       ; #######.
	.byte   %00001100                       ; ....##..
	.byte   %00001000                       ; ....#...
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %00010000                       ; ...#....
	.byte   %00010000                       ; ...#....
	.byte   %00010000                       ; ...#....
	.byte   %00010000                       ; ...#....
	.byte   %01111100                       ; .#####..
	.byte   %00111000                       ; ..###...
	.byte   %00010000                       ; ...#....

	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00100000                       ; ..#.....
	.byte   %01100000                       ; .##.....
	.byte   %11111110                       ; #######.
	.byte   %01100000                       ; .##.....
	.byte   %00100000                       ; ..#.....
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00100100                       ; ..#..#..
	.byte   %00011000                       ; ...##...
	.byte   %00011000                       ; ...##...
	.byte   %00100100                       ; ..#..#..
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........

	.byte   %11111110                       ; #######.
	.byte   %01000000                       ; .#......
	.byte   %00100000                       ; ..#.....
	.byte   %00010000                       ; ...#....
	.byte   %00010000                       ; ...#....
	.byte   %00100000                       ; ..#.....
	.byte   %01000000                       ; .#......
	.byte   %11111110                       ; #######.

	.byte   %00010000                       ; ...#....
	.byte   %00010000                       ; ...#....
	.byte   %00101000                       ; ..#.#...
	.byte   %00101000                       ; ..#.#...
	.byte   %01000100                       ; .#...#..
	.byte   %01000100                       ; .#...#..
	.byte   %10000010                       ; #.....#.
	.byte   %11111110                       ; #######.

	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %01000100                       ; .#...#..
	.byte   %01000100                       ; .#...#..
	.byte   %01000100                       ; .#...#..
	.byte   %00111000                       ; ..###...
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00111000                       ; ..###...
	.byte   %01000100                       ; .#...#..
	.byte   %01000100                       ; .#...#..
	.byte   %01000100                       ; .#...#..
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00010000                       ; ...#....
	.byte   %00000000                       ; ........
	.byte   %01111100                       ; .#####..
	.byte   %00000000                       ; ........
	.byte   %00010000                       ; ...#....
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %01100010                       ; .##...#.
	.byte   %10010010                       ; #..#..#.
	.byte   %10010100                       ; #..#.#..
	.byte   %10001000                       ; #...#...
	.byte   %10011000                       ; #..##...
	.byte   %01100110                       ; .##..##.

	.byte   %00011000                       ; ...##...
	.byte   %00100100                       ; ..#..#..
	.byte   %01000100                       ; .#...#..
	.byte   %01111000                       ; .####...
	.byte   %01000100                       ; .#...#..
	.byte   %01000010                       ; .#....#.
	.byte   %01111100                       ; .#####..
	.byte   %01000000                       ; .#......

	.byte   %00110000                       ; ..##....
	.byte   %01001000                       ; .#..#...
	.byte   %00100000                       ; ..#.....
	.byte   %00110000                       ; ..##....
	.byte   %01001000                       ; .#..#...
	.byte   %01000100                       ; .#...#..
	.byte   %01000100                       ; .#...#..
	.byte   %00111000                       ; ..###...

	.byte   %01000000                       ; .#......
	.byte   %00100000                       ; ..#.....
	.byte   %00010000                       ; ...#....
	.byte   %00010000                       ; ...#....
	.byte   %00101000                       ; ..#.#...
	.byte   %00101000                       ; ..#.#...
	.byte   %01000100                       ; .#...#..
	.byte   %01000100                       ; .#...#..

	.byte   %00000000                       ; ........
	.byte   %00100100                       ; ..#..#..
	.byte   %00100100                       ; ..#..#..
	.byte   %00100100                       ; ..#..#..
	.byte   %00111010                       ; ..###.#.
	.byte   %00100000                       ; ..#.....
	.byte   %00100000                       ; ..#.....
	.byte   %11000000                       ; ##......

	.byte   %00000000                       ; ........
	.byte   %00100000                       ; ..#.....
	.byte   %01111100                       ; .#####..
	.byte   %10101000                       ; #.#.#...
	.byte   %00101000                       ; ..#.#...
	.byte   %00101000                       ; ..#.#...
	.byte   %00101000                       ; ..#.#...
	.byte   %00101000                       ; ..#.#...

	.byte   %00000000                       ; ........
	.byte   %00001100                       ; ....##..
	.byte   %00010010                       ; ...#..#.
	.byte   %00100010                       ; ..#...#.
	.byte   %01100100                       ; .##..#..
	.byte   %01011000                       ; .#.##...
	.byte   %01000000                       ; .#......
	.byte   %10000000                       ; #.......

	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00111110                       ; ..#####.
	.byte   %01010000                       ; .#.#....
	.byte   %10001000                       ; #...#...
	.byte   %10001000                       ; #...#...
	.byte   %10001000                       ; #...#...
	.byte   %01110000                       ; .###....

	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %01000100                       ; .#...#..
	.byte   %10000010                       ; #.....#.
	.byte   %10010010                       ; #..#..#.
	.byte   %10010010                       ; #..#..#.
	.byte   %10010010                       ; #..#..#.
	.byte   %01101100                       ; .##.##..

	.byte   %00001100                       ; ....##..
	.byte   %00110000                       ; ..##....
	.byte   %11000000                       ; ##......
	.byte   %00110000                       ; ..##....
	.byte   %00001100                       ; ....##..
	.byte   %00000000                       ; ........
	.byte   %11111100                       ; ######..
	.byte   %00000000                       ; ........

	.byte   %01100000                       ; .##.....
	.byte   %00011000                       ; ...##...
	.byte   %01100000                       ; .##.....
	.byte   %00011000                       ; ...##...
	.byte   %01100000                       ; .##.....
	.byte   %00000000                       ; ........
	.byte   %01111110                       ; .######.
	.byte   %00000000                       ; ........

	.byte   %00111000                       ; ..###...
	.byte   %01000100                       ; .#...#..
	.byte   %10000010                       ; #.....#.
	.byte   %11111110                       ; #######.
	.byte   %10000010                       ; #.....#.
	.byte   %10000010                       ; #.....#.
	.byte   %01000100                       ; .#...#..
	.byte   %00111000                       ; ..###...

	.byte   %01000000                       ; .#......
	.byte   %10000000                       ; #.......
	.byte   %00011000                       ; ...##...
	.byte   %00101000                       ; ..#.#...
	.byte   %01001000                       ; .#..#...
	.byte   %00101000                       ; ..#.#...
	.byte   %00011000                       ; ...##...
	.byte   %00001100                       ; ....##..

	.byte   %00110000                       ; ..##....
	.byte   %01001000                       ; .#..#...
	.byte   %01001000                       ; .#..#...
	.byte   %00110000                       ; ..##....
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........

	.byte   %01000000                       ; .#......
	.byte   %00100000                       ; ..#.....
	.byte   %00110000                       ; ..##....
	.byte   %00101000                       ; ..#.#...
	.byte   %00100100                       ; ..#..#..
	.byte   %00101000                       ; ..#.#...
	.byte   %00110000                       ; ..##....
	.byte   %01100000                       ; .##.....

	.byte   %11100000                       ; ###.....
	.byte   %10010000                       ; #..#....
	.byte   %01001000                       ; .#..#...
	.byte   %00100100                       ; ..#..#..
	.byte   %00100100                       ; ..#..#..
	.byte   %01001000                       ; .#..#...
	.byte   %10010000                       ; #..#....
	.byte   %11100000                       ; ###.....

	.byte   %11111110                       ; #######.
	.byte   %00000000                       ; ........
	.byte   %00111100                       ; ..####..
	.byte   %01000010                       ; .#....#.
	.byte   %10000000                       ; #.......
	.byte   %00111100                       ; ..####..
	.byte   %00000000                       ; ........
	.byte   %11111110                       ; #######.

	.byte   %00101000                       ; ..#.#...
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %01111100                       ; .#####..
	.byte   %01000100                       ; .#...#..
	.byte   %01000100                       ; .#...#..
	.byte   %01111100                       ; .#####..
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00110000                       ; ..##....
	.byte   %01001000                       ; .#..#...
	.byte   %01001000                       ; .#..#...
	.byte   %00110000                       ; ..##....
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %00010000                       ; ...#....
	.byte   %00111000                       ; ..###...
	.byte   %01111100                       ; .#####..
	.byte   %11111110                       ; #######.
	.byte   %01111100                       ; .#####..
	.byte   %00111000                       ; ..###...
	.byte   %00010000                       ; ...#....

	.byte   %00000000                       ; ........
	.byte   %10000010                       ; #.....#.
	.byte   %01000100                       ; .#...#..
	.byte   %00101000                       ; ..#.#...
	.byte   %00010000                       ; ...#....
	.byte   %00101000                       ; ..#.#...
	.byte   %01000100                       ; .#...#..
	.byte   %10000010                       ; #.....#.

	.byte   %00000100                       ; .....#..
	.byte   %00001000                       ; ....#...
	.byte   %00010000                       ; ...#....
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000100                       ; .....#..
	.byte   %00001000                       ; ....#...
	.byte   %00010000                       ; ...#....

	.byte   %01000100                       ; .#...#..
	.byte   %00101000                       ; ..#.#...
	.byte   %00010000                       ; ...#....
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %00010000                       ; ...#....
	.byte   %00111000                       ; ..###...
	.byte   %01111100                       ; .#####..
	.byte   %00010000                       ; ...#....
	.byte   %01111100                       ; .#####..
	.byte   %00111000                       ; ..###...
	.byte   %00010000                       ; ...#....

	.byte   %00010000                       ; ...#....
	.byte   %00010000                       ; ...#....
	.byte   %00010000                       ; ...#....
	.byte   %00010000                       ; ...#....
	.byte   %00010000                       ; ...#....
	.byte   %00010000                       ; ...#....
	.byte   %00010000                       ; ...#....
	.byte   %00010000                       ; ...#....

chrset_6x6:
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........

	.byte   %01000000                       ; .#......
	.byte   %01000000                       ; .#......
	.byte   %01000000                       ; .#......
	.byte   %00000000                       ; ........
	.byte   %01000000                       ; .#......
	.byte   %00000000                       ; ........

	.byte   %01010000                       ; .#.#....
	.byte   %01010000                       ; .#.#....
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........

	.byte   %01010000                       ; .#.#....
	.byte   %11111000                       ; #####...
	.byte   %01010000                       ; .#.#....
	.byte   %11111000                       ; #####...
	.byte   %01010000                       ; .#.#....
	.byte   %00000000                       ; ........

	.byte   %01000000                       ; .#......
	.byte   %11110000                       ; ####....
	.byte   %11000000                       ; ##......
	.byte   %00110000                       ; ..##....
	.byte   %11110000                       ; ####....
	.byte   %00100000                       ; ..#.....

	.byte   %10010000                       ; #..#....
	.byte   %00100000                       ; ..#.....
	.byte   %00100000                       ; ..#.....
	.byte   %01000000                       ; .#......
	.byte   %10010000                       ; #..#....
	.byte   %00000000                       ; ........

	.byte   %01000000                       ; .#......
	.byte   %10100000                       ; #.#.....
	.byte   %01000000                       ; .#......
	.byte   %10110000                       ; #.##....
	.byte   %01010000                       ; .#.#....
	.byte   %00000000                       ; ........

	.byte   %00100000                       ; ..#.....
	.byte   %01000000                       ; .#......
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........

	.byte   %00100000                       ; ..#.....
	.byte   %01000000                       ; .#......
	.byte   %01000000                       ; .#......
	.byte   %01000000                       ; .#......
	.byte   %00100000                       ; ..#.....
	.byte   %00000000                       ; ........

	.byte   %01000000                       ; .#......
	.byte   %00100000                       ; ..#.....
	.byte   %00100000                       ; ..#.....
	.byte   %00100000                       ; ..#.....
	.byte   %01000000                       ; .#......
	.byte   %00000000                       ; ........

	.byte   %10010000                       ; #..#....
	.byte   %01100000                       ; .##.....
	.byte   %11110000                       ; ####....
	.byte   %01100000                       ; .##.....
	.byte   %10010000                       ; #..#....
	.byte   %00000000                       ; ........

	.byte   %00100000                       ; ..#.....
	.byte   %00100000                       ; ..#.....
	.byte   %11111000                       ; #####...
	.byte   %00100000                       ; ..#.....
	.byte   %00100000                       ; ..#.....
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %01000000                       ; .#......
	.byte   %01000000                       ; .#......
	.byte   %10000000                       ; #.......
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %11110000                       ; ####....
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %01000000                       ; .#......
	.byte   %01000000                       ; .#......
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %00010000                       ; ...#....
	.byte   %00100000                       ; ..#.....
	.byte   %01000000                       ; .#......
	.byte   %10000000                       ; #.......
	.byte   %00000000                       ; ........

	.byte   %01100000                       ; .##.....
	.byte   %10010000                       ; #..#....
	.byte   %10010000                       ; #..#....
	.byte   %10010000                       ; #..#....
	.byte   %01100000                       ; .##.....
	.byte   %00000000                       ; ........

	.byte   %00100000                       ; ..#.....
	.byte   %01100000                       ; .##.....
	.byte   %00100000                       ; ..#.....
	.byte   %00100000                       ; ..#.....
	.byte   %01110000                       ; .###....
	.byte   %00000000                       ; ........

	.byte   %01100000                       ; .##.....
	.byte   %10010000                       ; #..#....
	.byte   %00100000                       ; ..#.....
	.byte   %01000000                       ; .#......
	.byte   %11110000                       ; ####....
	.byte   %00000000                       ; ........

	.byte   %11110000                       ; ####....
	.byte   %00010000                       ; ...#....
	.byte   %00100000                       ; ..#.....
	.byte   %00010000                       ; ...#....
	.byte   %11100000                       ; ###.....
	.byte   %00000000                       ; ........

	.byte   %00100000                       ; ..#.....
	.byte   %01100000                       ; .##.....
	.byte   %10100000                       ; #.#.....
	.byte   %11110000                       ; ####....
	.byte   %00100000                       ; ..#.....
	.byte   %00000000                       ; ........

	.byte   %11100000                       ; ###.....
	.byte   %10000000                       ; #.......
	.byte   %11100000                       ; ###.....
	.byte   %00010000                       ; ...#....
	.byte   %11100000                       ; ###.....
	.byte   %00000000                       ; ........

	.byte   %01100000                       ; .##.....
	.byte   %10000000                       ; #.......
	.byte   %11100000                       ; ###.....
	.byte   %10010000                       ; #..#....
	.byte   %11100000                       ; ###.....
	.byte   %00000000                       ; ........

	.byte   %11110000                       ; ####....
	.byte   %00010000                       ; ...#....
	.byte   %00100000                       ; ..#.....
	.byte   %01000000                       ; .#......
	.byte   %01000000                       ; .#......
	.byte   %00000000                       ; ........

	.byte   %01100000                       ; .##.....
	.byte   %10010000                       ; #..#....
	.byte   %01100000                       ; .##.....
	.byte   %10010000                       ; #..#....
	.byte   %01100000                       ; .##.....
	.byte   %00000000                       ; ........

	.byte   %01100000                       ; .##.....
	.byte   %10010000                       ; #..#....
	.byte   %01110000                       ; .###....
	.byte   %00010000                       ; ...#....
	.byte   %11100000                       ; ###.....
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %01000000                       ; .#......
	.byte   %00000000                       ; ........
	.byte   %01000000                       ; .#......
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %01000000                       ; .#......
	.byte   %00000000                       ; ........
	.byte   %01000000                       ; .#......
	.byte   %10000000                       ; #.......
	.byte   %00000000                       ; ........

	.byte   %00100000                       ; ..#.....
	.byte   %01000000                       ; .#......
	.byte   %10000000                       ; #.......
	.byte   %01000000                       ; .#......
	.byte   %00100000                       ; ..#.....
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %11110000                       ; ####....
	.byte   %00000000                       ; ........
	.byte   %11110000                       ; ####....
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........

	.byte   %01000000                       ; .#......
	.byte   %00100000                       ; ..#.....
	.byte   %00010000                       ; ...#....
	.byte   %00100000                       ; ..#.....
	.byte   %01000000                       ; .#......
	.byte   %00000000                       ; ........

	.byte   %01100000                       ; .##.....
	.byte   %10010000                       ; #..#....
	.byte   %00100000                       ; ..#.....
	.byte   %00000000                       ; ........
	.byte   %01000000                       ; .#......
	.byte   %00000000                       ; ........

	.byte   %01100000                       ; .##.....
	.byte   %10010000                       ; #..#....
	.byte   %10110000                       ; #.##....
	.byte   %10000000                       ; #.......
	.byte   %01100000                       ; .##.....
	.byte   %00000000                       ; ........

	.byte   %01100000                       ; .##.....
	.byte   %10010000                       ; #..#....
	.byte   %11110000                       ; ####....
	.byte   %10010000                       ; #..#....
	.byte   %10010000                       ; #..#....
	.byte   %00000000                       ; ........

	.byte   %11100000                       ; ###.....
	.byte   %10010000                       ; #..#....
	.byte   %11100000                       ; ###.....
	.byte   %10010000                       ; #..#....
	.byte   %11100000                       ; ###.....
	.byte   %00000000                       ; ........

	.byte   %01110000                       ; .###....
	.byte   %10000000                       ; #.......
	.byte   %10000000                       ; #.......
	.byte   %10000000                       ; #.......
	.byte   %01110000                       ; .###....
	.byte   %00000000                       ; ........

	.byte   %11100000                       ; ###.....
	.byte   %10010000                       ; #..#....
	.byte   %10010000                       ; #..#....
	.byte   %10010000                       ; #..#....
	.byte   %11100000                       ; ###.....
	.byte   %00000000                       ; ........

	.byte   %11110000                       ; ####....
	.byte   %10000000                       ; #.......
	.byte   %11100000                       ; ###.....
	.byte   %10000000                       ; #.......
	.byte   %11110000                       ; ####....
	.byte   %00000000                       ; ........

	.byte   %11110000                       ; ####....
	.byte   %10000000                       ; #.......
	.byte   %11100000                       ; ###.....
	.byte   %10000000                       ; #.......
      	.byte   %10000000                       ; #.......
	.byte   %00000000                       ; ........

	.byte   %01100000                       ; .##.....
	.byte   %10000000                       ; #.......
	.byte   %10010000                       ; #..#....
	.byte   %10010000                       ; #..#....
	.byte   %01110000                       ; .###....
	.byte   %00000000                       ; ........

	.byte   %10010000                       ; #..#....
	.byte   %10010000                       ; #..#....
	.byte   %11110000                       ; ####....
	.byte   %10010000                       ; #..#....
	.byte   %10010000                       ; #..#....
	.byte   %00000000                       ; ........

	.byte   %11100000                       ; ###.....
	.byte   %01000000                       ; .#......
	.byte   %01000000                       ; .#......
	.byte   %01000000                       ; .#......
	.byte   %11100000                       ; ###.....
	.byte   %00000000                       ; ........

	.byte   %00010000                       ; ...#....
	.byte   %00010000                       ; ...#....
	.byte   %00010000                       ; ...#....
	.byte   %10010000                       ; #..#....
	.byte   %01100000                       ; .##.....
	.byte   %00000000                       ; ........

	.byte   %10010000                       ; #..#....
	.byte   %10100000                       ; #.#.....
	.byte   %11000000                       ; ##......
	.byte   %10100000                       ; #.#.....
	.byte   %10010000                       ; #..#....
	.byte   %00000000                       ; ........

	.byte   %10000000                       ; #.......
	.byte   %10000000                       ; #.......
	.byte   %10000000                       ; #.......
	.byte   %10000000                       ; #.......
	.byte   %11110000                       ; ####....
	.byte   %00000000                       ; ........

	.byte   %10001000                       ; #...#...
	.byte   %11011000                       ; ##.##...
	.byte   %10101000                       ; #.#.#...
	.byte   %10001000                       ; #...#...
	.byte   %10001000                       ; #...#...
	.byte   %00000000                       ; ........

	.byte   %10010000                       ; #..#....
	.byte   %11010000                       ; ##.#....
	.byte   %10110000                       ; #.##....
	.byte   %10010000                       ; #..#....
	.byte   %10010000                       ; #..#....
	.byte   %00000000                       ; ........

	.byte   %11110000                       ; ####....
	.byte   %10010000                       ; #..#....
	.byte   %10010000                       ; #..#....
	.byte   %10010000                       ; #..#....
	.byte   %11110000                       ; ####....
	.byte   %00000000                       ; ........

	.byte   %11100000                       ; ###.....
	.byte   %10010000                       ; #..#....
	.byte   %11100000                       ; ###.....
	.byte   %10000000                       ; #.......
	.byte   %10000000                       ; #.......
	.byte   %00000000                       ; ........

	.byte   %11110000                       ; ####....
	.byte   %10010000                       ; #..#....
	.byte   %10010000                       ; #..#....
	.byte   %10110000                       ; #.##....
	.byte   %11111000                       ; #####...
	.byte   %00000000                       ; ........

	.byte   %11100000                       ; ###.....
	.byte   %10010000                       ; #..#....
	.byte   %11100000                       ; ###.....
	.byte   %10100000                       ; #.#.....
	.byte   %10010000                       ; #..#....
	.byte   %00000000                       ; ........

	.byte   %01110000                       ; .###....
	.byte   %10000000                       ; #.......
	.byte   %11100000                       ; ###.....
	.byte   %00010000                       ; ...#....
	.byte   %11100000                       ; ###.....
	.byte   %00000000                       ; ........

	.byte   %11111000                       ; #####...
	.byte   %00100000                       ; ..#.....
	.byte   %00100000                       ; ..#.....
	.byte   %00100000                       ; ..#.....
	.byte   %00100000                       ; ..#.....
	.byte   %00000000                       ; ........

	.byte   %10010000                       ; #..#....
	.byte   %10010000                       ; #..#....
	.byte   %10010000                       ; #..#....
	.byte   %10010000                       ; #..#....
	.byte   %11110000                       ; ####....
	.byte   %00000000                       ; ........

	.byte   %10010000                       ; #..#....
	.byte   %10010000                       ; #..#....
	.byte   %10010000                       ; #..#....
	.byte   %01100000                       ; .##.....
	.byte   %01100000                       ; .##.....
	.byte   %00000000                       ; ........

	.byte   %10001000                       ; #...#...
	.byte   %10001000                       ; #...#...
	.byte   %10101000                       ; #.#.#...
	.byte   %11011000                       ; ##.##...
	.byte   %10001000                       ; #...#...
	.byte   %00000000                       ; ........

	.byte   %10010000                       ; #..#....
	.byte   %10010000                       ; #..#....
	.byte   %01100000                       ; .##.....
	.byte   %10010000                       ; #..#....
	.byte   %10010000                       ; #..#....
	.byte   %00000000                       ; ........

	.byte   %10010000                       ; #..#....
	.byte   %10010000                       ; #..#....
	.byte   %11100000                       ; ###.....
	.byte   %01000000                       ; .#......
	.byte   %01000000                       ; .#......
	.byte   %00000000                       ; ........

	.byte   %11110000                       ; ####....
	.byte   %00010000                       ; ...#....
	.byte   %00100000                       ; ..#.....
	.byte   %01000000                       ; .#......
	.byte   %11110000                       ; ####....
	.byte   %00000000                       ; ........

	.byte   %01110000                       ; .###....
	.byte   %01000000                       ; .#......
	.byte   %01000000                       ; .#......
	.byte   %01000000                       ; .#......
	.byte   %01110000                       ; .###....
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %10000000                       ; #.......
	.byte   %01000000                       ; .#......
	.byte   %00100000                       ; ..#.....
	.byte   %00010000                       ; ...#....
	.byte   %00000000                       ; ........

	.byte   %01110000                       ; .###....
	.byte   %00010000                       ; ...#....
	.byte   %00010000                       ; ...#....
	.byte   %00010000                       ; ...#....
	.byte   %01110000                       ; .###....
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %00100000                       ; ..#.....
	.byte   %01010000                       ; .#.#....
	.byte   %10001000                       ; #...#...
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %11110000                       ; ####....

	.byte   %01000000                       ; .#......
	.byte   %00100000                       ; ..#.....
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %11000000                       ; ##......
	.byte   %00100000                       ; ..#.....
	.byte   %10100000                       ; #.#.....
	.byte   %11110000                       ; ####....
	.byte   %00000000                       ; ........

	.byte   %10000000                       ; #.......
	.byte   %11100000                       ; ###.....
	.byte   %10010000                       ; #..#....
	.byte   %10010000                       ; #..#....
	.byte   %11100000                       ; ###.....
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %01100000                       ; .##.....
	.byte   %10000000                       ; #.......
	.byte   %10000000                       ; #.......
	.byte   %01100000                       ; .##.....
	.byte   %00000000                       ; ........

	.byte   %00010000                       ; ...#....
	.byte   %01110000                       ; .###....
	.byte   %10010000                       ; #..#....
	.byte   %10010000                       ; #..#....
	.byte   %01110000                       ; .###....
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %01100000                       ; .##.....
	.byte   %10110000                       ; #.##....
	.byte   %10000000                       ; #.......
	.byte   %01100000                       ; .##.....
	.byte   %00000000                       ; ........

	.byte   %01100000                       ; .##.....
	.byte   %10000000                       ; #.......
	.byte   %11100000                       ; ###.....
	.byte   %10000000                       ; #.......
	.byte   %10000000                       ; #.......
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %01100000                       ; .##.....
	.byte   %10010000                       ; #..#....
	.byte   %11110000                       ; ####....
	.byte   %00010000                       ; ...#....
	.byte   %01100000                       ; .##.....

	.byte   %10000000                       ; #.......
	.byte   %11100000                       ; ###.....
	.byte   %10010000                       ; #..#....
	.byte   %10010000                       ; #..#....
	.byte   %10010000                       ; #..#....
	.byte   %00000000                       ; ........

	.byte   %01000000                       ; .#......
	.byte   %00000000                       ; ........
	.byte   %01000000                       ; .#......
	.byte   %01000000                       ; .#......
	.byte   %01000000                       ; .#......
	.byte   %00000000                       ; ........

	.byte   %00100000                       ; ..#.....
	.byte   %00000000                       ; ........
	.byte   %00100000                       ; ..#.....
	.byte   %00100000                       ; ..#.....
	.byte   %00100000                       ; ..#.....
	.byte   %11000000                       ; ##......

	.byte   %10000000                       ; #.......
	.byte   %10100000                       ; #.#.....
	.byte   %11000000                       ; ##......
	.byte   %10100000                       ; #.#.....
	.byte   %10010000                       ; #..#....
	.byte   %00000000                       ; ........

	.byte   %01000000                       ; .#......
	.byte   %01000000                       ; .#......
	.byte   %01000000                       ; .#......
	.byte   %01000000                       ; .#......
	.byte   %01000000                       ; .#......
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %10010000                       ; #..#....
	.byte   %11110000                       ; ####....
	.byte   %10010000                       ; #..#....
	.byte   %10010000                       ; #..#....
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %11100000                       ; ###.....
	.byte   %10010000                       ; #..#....
	.byte   %10010000                       ; #..#....
	.byte   %10010000                       ; #..#....
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %01100000                       ; .##.....
	.byte   %10010000                       ; #..#....
	.byte   %10010000                       ; #..#....
	.byte   %01100000                       ; .##.....
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %11100000                       ; ###.....
	.byte   %10010000                       ; #..#....
	.byte   %10010000                       ; #..#....
	.byte   %11100000                       ; ###.....
	.byte   %10000000                       ; #.......

	.byte   %00000000                       ; ........
	.byte   %01110000                       ; .###....
	.byte   %10010000                       ; #..#....
	.byte   %10010000                       ; #..#....
	.byte   %01110000                       ; .###....
	.byte   %00010000                       ; ...#....

	.byte   %00000000                       ; ........
	.byte   %01110000                       ; .###....
	.byte   %01000000                       ; .#......
	.byte   %01000000                       ; .#......
	.byte   %01000000                       ; .#......
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %01110000                       ; .###....
	.byte   %11000000                       ; ##......
	.byte   %00110000                       ; ..##....
	.byte   %11100000                       ; ###.....
	.byte   %00000000                       ; ........

	.byte   %01000000                       ; .#......
	.byte   %11100000                       ; ###.....
	.byte   %01000000                       ; .#......
	.byte   %01000000                       ; .#......
	.byte   %01100000                       ; .##.....
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %10010000                       ; #..#....
	.byte   %10010000                       ; #..#....
	.byte   %10010000                       ; #..#....
	.byte   %01110000                       ; .###....
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %10010000                       ; #..#....
	.byte   %10100000                       ; #.#.....
	.byte   %10100000                       ; #.#.....
	.byte   %01000000                       ; .#......
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %10010000                       ; #..#....
	.byte   %10010000                       ; #..#....
	.byte   %11110000                       ; ####....
	.byte   %10010000                       ; #..#....
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %10010000                       ; #..#....
	.byte   %01100000                       ; .##.....
	.byte   %01100000                       ; .##.....
	.byte   %10010000                       ; #..#....
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %10010000                       ; #..#....
	.byte   %10010000                       ; #..#....
	.byte   %01110000                       ; .###....
	.byte   %00010000                       ; ...#....
	.byte   %01100000                       ; .##.....

	.byte   %00000000                       ; ........
	.byte   %11110000                       ; ####....
	.byte   %00100000                       ; ..#.....
	.byte   %01000000                       ; .#......
	.byte   %11110000                       ; ####....
	.byte   %00000000                       ; ........

	.byte   %01100000                       ; .##.....
	.byte   %01000000                       ; .#......
	.byte   %10000000                       ; #.......
	.byte   %01000000                       ; .#......
	.byte   %01100000                       ; .##.....
	.byte   %00000000                       ; ........

	.byte   %01000000                       ; .#......
	.byte   %01000000                       ; .#......
	.byte   %00000000                       ; ........
	.byte   %01000000                       ; .#......
	.byte   %01000000                       ; .#......
	.byte   %00000000                       ; ........

	.byte   %11000000                       ; ##......
	.byte   %01000000                       ; .#......
	.byte   %00100000                       ; ..#.....
	.byte   %01000000                       ; .#......
	.byte   %11000000                       ; ##......
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte	%01101000			; .##.#...
	.byte	%10110000			; #.##....
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %00010000                       ; ...#....
	.byte   %00100000                       ; ..#.....
	.byte   %01000000                       ; .#......
	.byte   %10000000                       ; #.......
	.byte   %10000000                       ; #.......

	.byte   %00000000                       ; ........
	.byte   %11100000                       ; ###.....
	.byte   %00000000                       ; ........
	.byte   %11100000                       ; ###.....
	.byte   %00000000                       ; ........
	.byte   %11100000                       ; ###.....

	.byte   %01010000                       ; .#.#....
	.byte   %10100000                       ; #.#.....
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........

	.byte   %00100000                       ; ..#.....
	.byte   %01110000                       ; .###....
	.byte   %10000000                       ; #.......
	.byte   %01110000                       ; .###....
	.byte   %00100000                       ; ..#.....
	.byte   %00000000                       ; ........

	.byte   %00100000                       ; ..#.....
	.byte   %11110000                       ; ####....
	.byte   %00100000                       ; ..#.....
	.byte   %11110000                       ; ####....
	.byte   %01000000                       ; .#......
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %00100000                       ; ..#.....
	.byte   %01110000                       ; .###....
	.byte   %00100000                       ; ..#.....
	.byte   %00100000                       ; ..#.....
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00100000                       ; ..#.....
	.byte   %11110000                       ; ####....
	.byte   %00100000                       ; ..#.....
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %00100000                       ; ..#.....
	.byte   %00100000                       ; ..#.....
	.byte   %01110000                       ; .###....
	.byte   %00100000                       ; ..#.....
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %01000000                       ; .#......
	.byte   %11110000                       ; ####....
	.byte   %01000000                       ; .#......
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %01010000                       ; .#.#....
	.byte   %00100000                       ; ..#.....
	.byte   %01010000                       ; .#.#....
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........

	.byte   %11110000                       ; ####....
	.byte   %10000000                       ; #.......
	.byte   %01100000                       ; .##.....
	.byte   %10000000                       ; #.......
	.byte   %11110000                       ; ####....
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %00100000                       ; ..#.....
	.byte   %01010000                       ; .#.#....
	.byte   %10001000                       ; #...#...
	.byte   %11111000                       ; #####...
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %10010000                       ; #..#....
	.byte   %10010000                       ; #..#....
	.byte   %01100000                       ; .##.....
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %01100000                       ; .##.....
	.byte   %10010000                       ; #..#....
	.byte   %10010000                       ; #..#....
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........

	.byte   %01100000                       ; .##.....
	.byte   %00000000                       ; ........
	.byte   %11110000                       ; ####....
	.byte   %00000000                       ; ........
	.byte   %01100000                       ; .##.....
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %01010000                       ; .#.#....
	.byte   %10100000                       ; #.#.....
	.byte   %01110000                       ; .###....
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........

	.byte   %00100000                       ; ..#.....
	.byte   %01010000                       ; .#.#....
	.byte   %10100000                       ; #.#.....
	.byte   %10010000                       ; #..#....
	.byte   %11100000                       ; ###.....
	.byte   %10000000                       ; #.......

	.byte   %00100000                       ; ..#.....
	.byte   %01000000                       ; .#......
	.byte   %00100000                       ; ..#.....
	.byte   %01010000                       ; .#.#....
	.byte   %01110000                       ; .###....
	.byte   %00000000                       ; ........

	.byte   %10000000                       ; #.......
	.byte   %01000000                       ; .#......
	.byte   %00100000                       ; ..#.....
	.byte   %01010000                       ; .#.#....
	.byte   %10010000                       ; #..#....
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %01010000                       ; .#.#....
	.byte   %01010000                       ; .#.#....
	.byte   %01100000                       ; .##.....
	.byte   %10000000                       ; #.......

	.byte   %00001000                       ; ....#...
	.byte   %01111000                       ; .####...
	.byte   %11010000                       ; ##.#....
	.byte   %01010000                       ; .#.#....
	.byte   %01010000                       ; .#.#....
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %00100000                       ; ..#.....
	.byte   %01010000                       ; .#.#....
	.byte   %01010000                       ; .#.#....
	.byte   %10100000                       ; #.#.....
	.byte   %10000000                       ; #.......

	.byte   %00110000                       ; ..##....
	.byte   %01000000                       ; .#......
	.byte   %10100000                       ; #.#.....
	.byte   %10100000                       ; #.#.....
	.byte   %01000000                       ; .#......
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %10010000                       ; #..#....
	.byte   %10110000                       ; #.##....
	.byte   %01100000                       ; .##.....
	.byte   %00000000                       ; ........

	.byte   %00100000                       ; ..#.....
	.byte   %01000000                       ; .#......
	.byte   %10000000                       ; #.......
	.byte   %01000000                       ; .#......
	.byte   %00100000                       ; ..#.....
	.byte   %11100000                       ; ###.....

	.byte   %10000000                       ; #.......
	.byte   %01000000                       ; .#......
	.byte   %00100000                       ; ..#.....
	.byte   %01000000                       ; .#......
	.byte   %10000000                       ; #.......
	.byte   %11100000                       ; ###.....

	.byte   %00100000                       ; ..#.....
	.byte   %01010000                       ; .#.#....
	.byte   %01110000                       ; .###....
	.byte   %01010000                       ; .#.#....
	.byte   %00100000                       ; ..#.....
	.byte   %00000000                       ; ........

	.byte   %00110000                       ; ..##....
	.byte   %01100000                       ; .##.....
	.byte   %10100000                       ; #.#.....
	.byte   %01100000                       ; .##.....
	.byte   %00110000                       ; ..##....
	.byte   %00000000                       ; ........

	.byte   %00100000                       ; ..#.....
	.byte   %01010000                       ; .#.#....
	.byte   %00100000                       ; ..#.....
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........

	.byte   %11000000                       ; ##......
	.byte   %01100000                       ; .##.....
	.byte   %01010000                       ; .#.#....
	.byte   %01100000                       ; .##.....
	.byte   %11000000                       ; ##......
	.byte   %00000000                       ; ........

	.byte   %11000000                       ; ##......
	.byte   %10100000                       ; #.#.....
	.byte   %01010000                       ; .#.#....
	.byte   %01010000                       ; .#.#....
	.byte   %10100000                       ; #.#.....
	.byte   %11000000                       ; ##......

	.byte   %11111000                       ; #####...
	.byte   %00100000                       ; ..#.....
	.byte   %01000000                       ; .#......
	.byte   %01000000                       ; .#......
	.byte   %00100000                       ; ..#.....
	.byte   %11111000                       ; #####...

	.byte   %01010000                       ; .#.#....
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %01110000                       ; .###....
	.byte   %01010000                       ; .#.#....
	.byte   %01110000                       ; .###....
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %01100000                       ; .##.....
	.byte   %10010000                       ; #..#....
	.byte   %01100000                       ; .##.....
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........

	.byte   %00100000                       ; ..#.....
	.byte   %01110000                       ; .###....
	.byte   %11111000                       ; #####...
	.byte   %01110000                       ; .###....
	.byte   %00100000                       ; ..#.....
	.byte   %00000000                       ; ........

	.byte   %10001000                       ; #...#...
	.byte   %01010000                       ; .#.#....
	.byte   %00100000                       ; ..#.....
	.byte   %01010000                       ; .#.#....
	.byte   %10001000                       ; #...#...
	.byte   %00000000                       ; ........

	.byte   %00010000                       ; ...#....
	.byte   %00100000                       ; ..#.....
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........

	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00010000                       ; ...#....
	.byte   %00100000                       ; ..#.....

	.byte   %10100000                       ; #.#.....
	.byte   %00100000                       ; ..#.....
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........
	.byte   %00000000                       ; ........

	.byte   %00100000                       ; ..#.....
	.byte   %01110000                       ; .###....
	.byte   %00100000                       ; ..#.....
	.byte   %01110000                       ; .###....
	.byte   %00100000                       ; ..#.....
	.byte   %00000000                       ; ........

	.byte   %00100000                       ; ..#.....
	.byte   %00100000                       ; ..#.....
	.byte   %00100000                       ; ..#.....
	.byte   %00100000                       ; ..#.....
	.byte   %00100000                       ; ..#.....
	.byte   %00100000                       ; ..#.....

sub_bf86:
	cmp     HATABS,x
	beq	:+
	inx
	inx
	inx
	cpx     #$20
	bcc     sub_bf86
:	rts

; ----------------------------------------------------------------------------

sub_BF93:
	jsr	sub_b47d
	lda	#$51
	sta	byte_1347
	sta	byte_133a
	lda	$CB
	pha                                     ; BFA0 48                       H
	lda     #$00                            ; BFA1 A9 00                    ..
	sta     $CB                             ; BFA3 85 CB                    ..
	jsr     sub_b420
	pla                                     ; BFA8 68                       h
	sta     $CB                             ; BFA9 85 CB                    ..
	lda     #$51                            ; BFAB A9 51                    .Q
	sta     byte_133a
	lda     $08                             ; BFB0 A5 08                    ..
	beq     LBFC2                           ; BFB2 F0 0E                    ..
	ldx     byte_B2
	cpx     #$02                            ; BFB6 E0 02                    ..
	bcs     :+
	sec                                     ; BFBA 38                       8
	bne     LBFD8                           ; BFBB D0 1B                    ..
:	ldx     #$02                            ; BFBD A2 02                    ..
	jsr     sub_b43f
LBFC2:  lda     #$FF                            ; BFC2 A9 FF                    ..
	sta     byte_133a
	lda     #$00                            ; BFC7 A9 00                    ..
	sta     $CB                             ; BFC9 85 CB                    ..
	ldx     #$02                            ; BFCB A2 02                    ..
	jsr     sub_b422
	lda     #$51                            ; BFD0 A9 51                    .Q
	sta     byte_133a
	jsr     sub_b420
LBFD8:  jsr     LB4F7                           ; BFD8 20 F7 B4                  ..
	bcs	:+
	lda     #$00                            ; BFDD A9 00                    ..
	sta     byte_133a
	tax                                     ; BFE2 AA                       .
	jsr     sub_bf86
	bne	:+
	lda     #'T'
	sta     HATABS,x
	lda     #<t_handler
	sta     HATABS+1,x
	lda     #>t_handler
	sta     HATABS+2,x
:	rts

; ----------------------------------------------------------------------------
	.byte	$00,$00

	.addr	sub_a000
	.byte   $00,$04
	.addr	sub_BF93
