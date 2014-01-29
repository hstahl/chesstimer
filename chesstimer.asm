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
;       LoopTime
;     DisplayV
;       T40
;     WaitButton
;       Button
;         Do_Button
;       BlinkAlive
;       LoopTime
;   Button
;     Do_Button
;   BlinkAlive
;   ClockTick
;     ClockIncrement
;     UpdateClockV
;     DisplayV
;       T40
;   LoopTime
;
;;;;;;; Assembler directives ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	list P=PIC18F452, F=INHX32, C=160, N=0, ST=OFF, MM=OFF, R=DEC, X=ON
	#include P18F452.INC

;;;;;;; Configuration bits ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	__CONFIG  _CONFIG1H, _HS_OSC_1H  ;High Speed oscillator
	__CONFIG  _CONFIG2L, _PWRT_ON_2L & _BOR_ON_2L & _BORV_42_2L  ;Reset
	__CONFIG  _CONFIG2H, _WDT_OFF_2H  ;Watchdog timer disabled
	__CONFIG  _CONFIG3H, _CCP2MX_ON_3H  ;CCP2 to RC1 (rather than to RB3)
	__CONFIG  _CONFIG4L, _LVP_OFF_4L  ;RB5 enabled for I/O
	errorlevel -314, -315          ;Ignore lfsr messages

;;;;;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	cblock  0x000                   ;Beginning of access memory
	COUNT                           ;Counter for use in loops
	ALIVECNT                        ;Used by BlinkAlive subroutine
	TMR0LCOPY                       ;Copy of sixteen-bit Timer0 for LoopTime
	TMR0HCOPY
	INTCONCOPY                      ;Copy of INTCON for LoopTime
	OLDBUTTON                       ;State of button at previous loop
	WHITESTURN                      ;Which player's turn is being timed
	LCDTOPROW:9                     ;Top row string for lcd
	LCDBOTROW:9                     ;Bottom row string for lcd
	WCLOCK:5                        ;White player's clock
	BCLOCK:5			;Black player's clock
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
	  rcall   BlinkAlive            ;Blink a LED every 1 sec
	  rcall   ClockTick             ;Add 10msec to one of the clocks
	  rcall   LoopTime              ;Wait the remainder of 10msec
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
	movlf   100,ALIVECNT            ;Blink led every 100 loops = 1sec
	clrf    OLDBUTTON               ;OLDBUTTON = 0
	setf    WHITESTURN              ;White player starts

	clrf    WCLOCK                  ;White player's msec
	clrf    WCLOCK+1                ;White player's sec
	clrf    WCLOCK+2                ;White player's tens of secs
	clrf    WCLOCK+3                ;White player's mins
	clrf    WCLOCK+4                ;White player's tens of mins
	clrf    BCLOCK                  ;Black player's msec
	clrf    BCLOCK+1                ;Black player's sec
	clrf    BCLOCK+2                ;Black player's tens of secs
	clrf    BCLOCK+3                ;Black player's mins
	clrf    BCLOCK+4                ;Black player's tens of mins
                                        ;Set up character strings
	movlf   0x80,LCDTOPROW          ;Cursor to top left
	movlf   A'W',LCDTOPROW+1        ;Initially the top row will display
	movlf   A' ',LCDTOPROW+2        ;"W 00:00"
	movlf   A'0',LCDTOPROW+3
	movlf   A'0',LCDTOPROW+4
	movlf   A':',LCDTOPROW+5
	movlf   A'0',LCDTOPROW+6
	movlf   A'0',LCDTOPROW+7
	movlf   0x00,LCDTOPROW+8        ;Terminating byte
	movlf   0xC0,LCDBOTROW          ;Cursor to bottom left
	movlf   A'B',LCDBOTROW+1        ;Initially the bottom row will display
	movlf   A' ',LCDBOTROW+2        ;"B 00:00"
	movlf   A'0',LCDBOTROW+3
	movlf   A'0',LCDBOTROW+4
	movlf   A':',LCDBOTROW+5
	movlf   A'0',LCDBOTROW+6
	movlf   A'0',LCDBOTROW+7
	movlf   0x00,LCDBOTROW+8        ;Terminating byte

	rcall   InitLCD                 ;Start up the display
	rcall   DisplayV                ;Display clocks
	rcall   WaitButton              ;Starts the clock after one button press


	return

;;;;;;; WaitButton subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; The program waits until the button is pressed once.

WaitButton
	;REPEAT_
L02
	  rcall   Button                ;Check for button press
	  rcall   BlinkAlive            ;Blink LED while waiting
	  rcall   LoopTime              ;Wait 10msec
          movf    WHITESTURN,F          ;Is it zero?
	;UNTIL_
	bnz      L02
RL02
	setf    WHITESTURN              ;Set game to white player's turn again
	return

;;;;;;; InitLCD subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Initialized the LCD with the configuration bytes found in a table

InitLCD
	movlf   10,COUNT                ;Wait for 0.1 seconds
	;REPEAT_
L03
	  rcall   LoopTime
	  decf    COUNT,F
	;UNTIL_   .Z.
	bnz     L03
RL03
	bcf     PORTE,0                 ;RS=0 for command
	tbpnt   LCDstr                  ;Set up table pointer to init string
	tblrd*                          ;Get first byte from string into TABLAT
	;REPEAT_
L04
	  bsf     PORTE,1               ;Drive E high
	  movff   TABLAT,PORTD          ;Send upper nibble
	  bcf     PORTE,1               ;Drive E low so LCD will process input
	  rcall   LoopTime              ;Wait 10msec
	  bsf     PORTE,1               ;Drive E high
	  swapf   TABLAT,W              ;Swap nibbles
	  movwf   PORTD                 ;Send lower nibble
	  bcf     PORTE,1               ;Drive E low so LCD will process input
	  rcall   LoopTime              ;Wait 10msec
	  tblrd+*
	  movf    TABLAT,F              ;Is it zero?
	;UNTIL_   .Z.
	bnz     L04
RL04
	return

;;;;;;; DisplayV subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Display a character vector stored in INDF0 on the LCD. The first byte sets the
; cursor position. The following ones are displayed as characters. The vector
; should terminate with a zero.

DisplayV
	bcf     PORTE,0                 ;Drive RS pin low for cursor positioning
	;REPEAT_
L05
	  bsf     PORTE,1               ;Drive E pin high
	  movff   INDF0,PORTD           ;Send upper nibble
	  bcf     PORTE,1               ;Drive E pin low to accept nibble
	  bsf     PORTE,1               ;Drive E pin high again
	  swapf   INDF0,W               ;Swap nibbles
          movwf   PORTD                 ;Write lower nibble
          bcf     PORTE,1               ;Drive E pin low to process byte
	  rcall   T40                   ;Wait 40 usec
	  bsf     PORTE,0               ;Drive RS pin high to read characters
	  movf    PREINC0,W             ;Increment pointer and get next byte
	;UNTIL_   .Z.                   ;Is it zero?
	bnz     L05
RL05
	return

;;;;;;; Button subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Checks if button state has changed and runs Do_Button if it has.

Button
	movf    PORTD,W
	andlw   b'00001000'             ;All except button bit = 0
	cpfseq  OLDBUTTON
	rcall   Do_Button               ;If state of button changed, go to
	return                          ;Do_Button

;;;;;;; Do_Button subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Changes the turn on a rising edge.

Do_Button
	movwf   OLDBUTTON
	btfss   OLDBUTTON,3             ;Find only rising edges, return on
	return                          ;falling
	negf    WHITESTURN              ;Change turn
	return

;;;;;;; ClockTick subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Ticks the current player's clock ahead by 10msec and updates the string
; displayed on the LCD.

ClockTick
	btfsc   WHITESTURN,0            ;Skip if black player's turn
	bra     B06
	bra     B07
B06
	lfsr    0,LCDTOPROW+7           ;Load address of LCDTOPROW+7 to FSR0
	lfsr    1,WCLOCK                ;Load address of WCLOCK to FSR1
	rcall   ClockIncrement          ;Increment the time played
	lfsr    1,WCLOCK+1              ;Point to seconds in WCLOCK
	rcall   UpdateClockV            ;Update clock vector
	lfsr    0,LCDTOPROW             ;Load address of LCDTOPROW to FSR0
	rcall   DisplayV                ;Display time played
	bra     B08
B07
	lfsr    0,LCDBOTROW+7           ;Load address of LCDBOTROW+7 to FSR0
	lfsr    1,BCLOCK                ;Load address of BCLOCK to FSR1
	rcall   ClockIncrement          ;Increment the time played
	lfsr    1,BCLOCK+1              ;Point to seconds in BCLOCK
	rcall   UpdateClockV            ;Update clock vector
	lfsr    0,LCDBOTROW             ;Load address of LCDBOTROW to FSR0
	rcall   DisplayV                ;Display time played
B08
	return

;;;;;;; ClockIncrement subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Increments a clock vector found in INDF1 by 10msec and updates it to be
; presentable in the form of 00:00.00

ClockIncrement
	incf    INDF1,F                 ;Increment (tens of) milliseconds by 1
	movf    POSTINC1,W              ;Get amount of msec passed and move pntr
	sublw   100                     ;After 100*10msec, increment seconds
	bnz     B09                     ;If no need to increment, return
	incf    INDF1,F                 ;Increment seconds passed
	movf    POSTINC1,W              ;Get amount of sec passed and move pntr
	sublw   10                      ;After 10 sec, increment tens of secs
	bnz     B09                     ;If no need to increment, return
	incf    INDF1,F                 ;Increment tens of seconds passed
	movf    POSTINC1,W              ;Get tens of secs passed and move pntr
	sublw   6                       ;After 6*10sec passed, increment mins
	bnz     B09                     ;If no need to increment, return
	incf    INDF1,F                 ;Increment minutes passed
	movf    POSTINC1,W              ;Get minutes passed and move pntr
	sublw   10                      ;After 10 mins, increment tens of mins
	bnz     B09                     ;If no need to increment, return
	incf    INDF1,F                 ;Increment tens of minutes passed
B09
	return

;;;;;;; UpdateClockV subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Updates the clock string in INDF0 by the values found in INDF1.

Zeropos equ     A'0'                    ;Need to add this to a number to get an
                                        ;ascii character of it
UpdateClockV
	movf    POSTINC1,W              ;Get seconds
	addlw   Zeropos                 ;Convert to ASCII character
	movwf   POSTDEC0                ;Update seconds in char vector
	movf    POSTINC1,W              ;Get tens of seconds
	addlw   Zeropos                 ;Convert to ASCII character
	movwf   POSTDEC0                ;Update tens of seconds in char vector
	movf    POSTDEC0,W              ;Skip over the colon character
	movf    POSTINC1,W              ;Get minutes
	addlw   Zeropos                 ;Convert to ASCII character
	movwf   POSTDEC0                ;Update minutes in char vector
	movf    INDF1,W                 ;Get tens of minutes
	addlw   Zeropos                 ;Convert to ASCII character
	movwf   INDF0                   ;Update tens of minutes in char vector
	
	return

;;;;;;; BlinkAlive subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This subroutine briefly blinks the LED next to the PIC every second.

BlinkAlive
	bsf     PORTA,RA4               ;Turn off LED ('1' => OFF lor LED D2)
	decf    ALIVECNT,F              ;Decrement loop counter and return if nz
	bnz     B10
	movlf   100,ALIVECNT            ;Reinitialize ALIVECNT
	bcf     PORTA,RA4               ;Turn on LED for ten msec
B10
	return

;;;;;;; T40 subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Pause for 40 microseconds or 40/0.4 = 100 clock cycles
; Assumes 10/4 = 2.5 MHz internal clock rate

T40
	movlw 100/3                     ;Each REPEAT loop takes 3 cycles
	movwf COUNT
	;REPEAT_
L11
	  decf    COUNT,F
	;UNTIL_   .Z.
	bnz     L11
RL11
	return

;;;;;;; LoopTime subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Waits until 10ms has passed since the last call using Timer0.

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