;;;;;;; chesstimer for QvikFlash ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Uses LCD display for timekeeping
; Uses Timer0 for ten millisecond loop time
; Button changes turn between players
;
;;;;;;; Program Hierarchy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Mainline
;   Initial
;     InitLCD
;   Button
;     Do_Button
;   ClockIncrement
;   LoopTime
;
;;;;;;; Assembler directives ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	list  P=PIC18F452, F=INHX32, R=DEC, B=8, C=80
	#include P18F452.INC

;;;;;;; Configuration bits ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	__CONFIG  _CONFIG1H, _HS_OSC_1H  ;High Speed oscillator
	__CONFIG  _CONFIG2L, _PWRT_ON_2L & _BOR_ON_2L & _BORV_42_2L  ;Reset
	__CONFIG  _CONFIG2H, _WDT_OFF_2H  ;Watchdog timer disabled
	__CONFIG  _CONFIG3H, _CCP2MX_ON_3H  ;CCP2 to RC1 (rather than to RB3)
	__CONFIG  _CONFIG4L, _LVP_OFF_4L  ;RB5 enabled for I/O

;;;;;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	cblock  0x000                   ;Beginning of access memory
	COUNT                           ;Counter for use in loops
	TMR0LCOPY                       ;Copy of sixteen-bit Timer0 for LoopTime
	TMR0HCOPY
	INTCONCOPY                      ;Copy of INTCON for LoopTime
	OLDBUTTON                       ;State of button at previous loop
	WHITESTURN                      ;Which player's turn is being timed
	LCDTOPROW:8                     ;Top row string for lcd
	LCDBOTROW:8                     ;Bottom row string for lcd
	endc

;;;;;;; Macro definitions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

movlf   macro   literal,dest            ;Lets the programmer move a literal to
        movlw   literal                 ;file in a single line
	movwf   dest
	endm

tbpnt   macro   stringname              ;Used to point table pointer to a string
	movlf   high stringname,TBLPTRH ;stored in RAM to be displayed on the
	movlf   low stringname,TBLPTRL  ;LCD
	endm

;;;;;;; Vectors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	org     0x0000                  ;Reset vector
	nop
	goto    Mainline

	org     0x0008                  ;High priority interrupt
	goto    $                       ;Trap

	org     0x0018                  ;Low priority interrupt
	goto    $                       ;Trap

;;;;;;; Mainline program ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Mainline
	rcall   Initial                 ;Initialize everything

	;LOOP_
L01
	  rcall   Button                ;Check if button is pressed
	  rcall   LoopTime
	;ENDLOOP_
	bra     L01
PL01

;;;;;;; Initial subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Initial
	movlf   B'11100001',TRISA       ;Set I/O for PORTA
	movlf   B'11011100',TRISB       ;Set I/O for PORTB
	movlf   B'11010010',TRISC       ;Set I/O for PORTC
	movlf   B'00001111',TRISD       ;Set I/O for PORTD
	movlf   B'00000100',TRISE       ;Set I/O for PORTE
	movlf   B'10001000',T0CON       ;Set timer0 prescaler to 1:2
	movlf   B'00010000',PORTA       ;Turn off LEDs on PORTA
	clrf    OLDBUTTON               ;OLDBUTTON = 0
	setf    WHITESTURN              ;White player starts
                                        ;Set up character strings
	movlf   0x80,LCDTOPROW          ;Cursor to top left
	movlf   A'W',LCDTOPROW+1        ;Initially the top row will display
	movlf   A' ',LCDTOPROW+2        ;"W 00:00"
	movlf   A'0',LCDTOPROW+3
	movlf   A'0',LCDTOPROW+4
	movlf   A':',LCDTOPROW+5
	movlf   A'0',LCDTOPROW+6
	movlf   A'0',LCDTOPROW+7
	movlf   0xC0,LCDBOTROW          ;Cursor to bottom left
	movlf   A'B',LCDBOTROW+1        ;Initially the bottom row will display
	movlf   A' ',LCDBOTROW+2        ;"B 00:00"
	movlf   A'0',LCDBOTROW+3
	movlf   A'0',LCDBOTROW+4
	movlf   A':',LCDBOTROW+5
	movlf   A'0',LCDBOTROW+6
	movlf   A'0',LCDBOTROW+7

	rcall   InitLCD                 ;Start up the display

	return

;;;;;;; InitLCD subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

InitLCD
	movlf   10,COUNT                ;Wait for 0.1 seconds
	;REPEAT_
L02
	  rcall   LoopTime
	  decf    COUNT,F
	;UNTIL_   .Z.
	bnz     L02
RL02
	bcf     PORTE,0                 ;RS=0 for command
	tbpnt   LCDstr                  ;Set up table pointer to init string
	tblrd*                          ;Get first byte from string into TABLAT
	;REPEAT_
L03
	  bsf     PORTE,1               ;Drive E high
	  movff   TABLAT,PORTD          ;Send upper nibble
	  bcf     PORTE,1               ;Drive E low so LCD will process input
	  rcall   LoopTime              ;Wait 10msec
	  bsf     PORTE,1               ;Drive E high
	  swapf   TABLAT,W              ;Swap nibbles
	  movwf   PORTD                 ;Send lower nibble
	  bcf     PORTE,1               ;Drive E low so LCD will process input
	  rcall   LoopTime              ;Wait 10msec
	  tblrd*
	  movf    TABLAT,F              ;Is it zero?
	;UNTIL_   .Z.
	bnz     L03
RL03
	return

;;;;;;; Button subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Button
	movf    PORTD,w
	andlw   b'00001000'             ;All except button bit = 0
	cpfseq  OLDBUTTON
	rcall   Do_Button               ;If state of button changed, go to
	return                          ;Do_Button

;;;;;;; Do_Button subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Do_Button
	movwf   OLDBUTTON
	btfss   OLDBUTTON,3		;Find only rising edges, return on
	return                          ;falling
	negf    WHITESTURN              ;Change turn
	return

;;;;;;; LoopTime subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Oscnum  equ     65536-25000+12+2        ;10ms

LoopTime
	btfss   INTCON,TMR0IF           ;wait until flag is raised after 10ms
	bra     LoopTime
	movff   INTCON,INTCONCOPY       ;Disable interrupt flags
	bcf     INTCON,GIEH
	movff   TMR0L,TMR0LCOPY
	movff   TMR0H,TMR0HCOPY
	movlw   low  Oscnum
	addwf   TMR0LCOPY,F
	movlw   high  Oscnum
	addwfc  TMR0HCOPY,F
	movff   TMR0HCOPY,TMR0H
	movff   TMR0LCOPY,TMR0L         ;write 16-bit counter
	movf    INTCONCOPY,W            ;restore interrupts
	andlw   B'10000000'
	iorwf   INTCON,F
	bcf     INTCON,TMR0IF           ;clear timer0 flag

	return

;;;;;;; Constant Strings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

LCDstr  db      0x33,0x32,0x28,0x01,0x0c,0x06,0x00 ;init string for LCD display

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	end                             ;End program