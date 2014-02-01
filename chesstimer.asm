;;;;;;; chesstimer for QvikFlash ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Uses onboard LCD for timekeeping
; Uses Timer0 for ten millisecond loop time
; Button changes turn between players
; Selectable play time: 5, 10 or 15 minutes decreasing time or incrementing time
; Selectable addional time: Fischer 5 seconds added or a 5 second delay at the
; beginning of a turn
; Cycle through menus with a single button click and make selections with a
; double click.
;
;;;;;;; Program Hierarchy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Mainline
;   Initial
;     InitLCD
;       LoopTime
;     DisplayV
;       T40
;   Button
;     Do_Button
;       AddTime
;       DoubleClick
;   BlinkAlive
;   ModeMenu
;     DisplayC
;       T40
;   TimeMenu
;     DisplayV
;       T40
;   WaitButton
;     Button
;       Do_Button
;         AddTime
;         DoubleClick
;     BlinkAlive
;     LoopTime
;   ClockSelect
;     ClockIncrement
;     ClockDecrement
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

STATS   equ     0x000                   ;Status bits for program
TURN    equ     0                       ;The first bit signifies whose turn
BTN     equ     1                       ;Was button pressed once in this loop?
DBL     equ     2                       ;Was it a double click?
MENU    equ     3                       ;Which menu should we go to?
PLAY    equ     4                       ;Has the game started?
INC     equ     5                       ;Increment or decrement clocks?
FISCH   equ     6                       ;Fischer's clock added time
DELAY   equ     7                       ;Delay before running clock

	cblock  0x001                   ;Block in access memory
	COUNT                           ;Counter for use in loops
	TEMP                            ;Temporary variable available for use
	DELAYCNTR                       ;In delay mode, 5sec delay before timing
	DBLCLKCNTR                      ;Counts time window for a double click
	ALIVECNT                        ;Used by BlinkAlive subroutine
	TMR0LCOPY                       ;Copy of sixteen-bit Timer0 for LoopTime
	TMR0HCOPY
	INTCONCOPY                      ;Copy of INTCON for LoopTime
	OLDBUTTON                       ;State of button at previous loop
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
	  rcall   ModeMenu              ;Select game mode
	  rcall   TimeMenu              ;Select available time
	  rcall   WaitButton            ;Starts the game after one button press
	  rcall   ClockSelect           ;Add 10msec to one of the clocks
	  movlw   B'11111001'
	  andwf   STATS,F               ;Clear all button status bits
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
	clrf    STATS
	bsf     STATS,TURN              ;White player starts

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

	return

;;;;;;; WaitButton subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; The program waits until the button is pressed once.

WaitButton
	btfss   STATS,MENU              ;Only run this routine once after menus.
	return                          ;Then clear MENU bit to never come
	btfss   STATS,PLAY              ;here again.
	return
	;REPEAT_
L02
	  rcall   Button                ;Check for button press
	  rcall   BlinkAlive            ;Blink LED while waiting
	  rcall   LoopTime              ;Wait 10msec
          btfss   STATS,TURN            ;Is it zero?
	;UNTIL_   .Z.
	bra     L02
RL02
	bcf     STATS,MENU              ;Don't visit the loop again.
	bsf     STATS,TURN              ;Set game to white player's turn again
	return

;;;;;;; ModeMenu subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ModeMenu
	btfsc   STATS,PLAY              ;If game has started, skip menu
	return
	btfsc   STATS,MENU              ;If menu bit = 1, skip to time menu
	return
	btfsc   STATS,DBL               ;If double click, return with these
	bra     B07                     ;settings immediately
	tbpnt   Modec                   ;Print "Mode:" on first line
	rcall   DisplayC
	btfsc   STATS,FISCH             ;Print Fischer on bottom
	bra     B03
	btfsc   STATS,DELAY             ;Print Delay on bottom
	bra     B04
	bra     B05                     ;Or print Normal on bottom
B03
	tbpnt   Fischer
	btfss   STATS,BTN               ;If button was pressed, rotate menu
	bra     B06                     ;If button wasn't pressed, return
	bcf     STATS,FISCH             ;Next loop, display delay as selected
	bsf     STATS,DELAY
	bra     B06
B04
	tbpnt   Delay
	btfss   STATS,BTN               ;If button was pressed, rotate menu
	bra     B06                     ;If button wasn't pressed, return
	bcf     STATS,DELAY             ;Next loop, display normal as selected
	bra     B06
B05
	tbpnt   Normal
	btfss   STATS,BTN               ;If button was pressed, rotate menu
	bra     B06                     ;If button wasn't pressed, return
	bsf     STATS,FISCH             ;Next loop, display fischer as selected
B06
	rcall   DisplayC
	return
B07
	bsf     STATS,MENU              ;Don't come to this menu anymore
	return

;;;;;;; TimeMenu subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

TimeMenu
	btfsc   STATS,PLAY              ;If game has started, skip menu
	return
	btfss   STATS,MENU              ;If menu bit = 0, skip this menu
	return
	btfsc   STATS,DBL               ;If doubleclick, select these settings
	bra     B10
	btfss   STATS,BTN               ;If button wasn't pressed, return
	bra     B09
	movf    WCLOCK+4,W              ;Get tens of minutes
	mullw   10                      ;Multiply by ten
	movf    PRODL,W                 ;Get product
	addwf   WCLOCK+3,W              ;Add minutes
	movwf   TEMP                    ;Store minutes
	movlw   15
	cpfseq  TEMP                    ;if timer is at 15 minutes, skip
	bra     B08
	clrf    WCLOCK+3                ;Zero minutes
	clrf    WCLOCK+4                ;Zero tens of minutes
	clrf    BCLOCK+4                ;Zero tens of minutes
	clrf    BCLOCK+3                ;Zero minutes
	bsf     STATS,INC               ;Set clocks to increment over time
	bra     B09
B08
	movlw   5                       ;If clocks were at less than 15 minutes
	addwf   WCLOCK+3,F              ;Add 5 to minutes
	lfsr    1,WCLOCK
	rcall   ClockIncrement          ;Fix clock formatting
	clrf    INDF1                   ;As a side effect, 10 msec were added
	addwf   BCLOCK+3,F              ;Add 5 to minutes
	lfsr    1,BCLOCK
	rcall   ClockIncrement          ;Fix clock formatting
	clrf    INDF1                   ;As a side effect, 10 msec were added
	bcf     STATS,INC               ;Set clocks to decrement over time
B09
	lfsr    0,LCDTOPROW
	rcall   DisplayV                ;Display clocks
	lfsr    0,LCDBOTROW
	rcall   DisplayV
	return
B10
	bsf     STATS,PLAY              ;Don't come to these menus again
	bsf     STATS,TURN              ;This bit is used in WaitButton
	return

;;;;;;; InitLCD subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Initialized the LCD with the configuration bytes found in a table

InitLCD
	movlf   10,COUNT                ;Wait for 0.1 seconds
	;REPEAT_
L11
	  rcall   LoopTime
	  decf    COUNT,F
	;UNTIL_   .Z.
	bnz     L11
RL11
	bcf     PORTE,0                 ;RS=0 for command
	tbpnt   LCDstr                  ;Set up table pointer to init string
	tblrd*                          ;Get first byte from string into TABLAT
	;REPEAT_
L12
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
	bnz     L12
RL12
	return

;;;;;;; DisplayC subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This subroutine is called with TBLPTR containing the address of a constant
; display string.  It sends the bytes of the string to the LCD.  The first
; byte sets the cursor position.  The remaining bytes are displayed, beginning
; at that position.
; This subroutine expects a normal one-byte cursor-positioning code, 0xhh, or
; an occasionally used two-byte cursor-positioning code of the form 0x00hh.

DisplayC
	bcf     PORTE,0                 ;Drive RS pin low for cursor positioning
	tblrd*                          ;Get byte from string into TABLAT
	movf    TABLAT,F                ;Check for leading zero byte
	;IF_    .Z.
	bnz     B13
	  tblrd+*                       ;If zero, get next byte
	;ENDIF_
B13
	;REPEAT_
L14
	  bsf     PORTE,1               ;Drive E pin high
	  movff   TABLAT,PORTD          ;Send upper nibble
	  bcf     PORTE,1               ;Drive E pin low to accept nibble
	  bsf     PORTE,1               ;Drive E pin high again
	  swapf   TABLAT,W              ;Swap nibbles
	  movwf   PORTD                 ;Write lower nibble
	  bcf     PORTE,1               ;Drive E pin low to process byte
	  rcall   T40                   ;Wait 40usec
	  bsf     PORTE,0               ;Drive RS pin high to receive chars
	  tblrd+*                       ;Increment pointer and get next byte
	  movf    TABLAT,F              ;Is it zero?
	;UNTIL_   .Z.
	bnz     L14
RL14
	return

;;;;;;; DisplayV subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Display a character vector stored in INDF0 on the LCD. The first byte sets the
; cursor position. The following ones are displayed as characters. The vector
; should terminate with a zero.

DisplayV
	bcf     PORTE,0                 ;Drive RS pin low for cursor positioning
	;REPEAT_
L15
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
	bnz     L15
RL15
	return

;;;;;;; Button subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Checks if button state has changed and runs Do_Button if it has. Counts to .5
; seconds from the button press and waits for a second press to register a
; double click. If no double click occurs within .5 seconds, does a single click
; instead. DBLCLKCNTR will start incrementing in each loop after a button press.
; If it reaches 50, it will be cleared and not incremented again until a new
; press of the button. If the button is pressed again before 50 loops, a double
; click is regocnized. A single press is regocnized after .5 seconds.

Button
	movf    DBLCLKCNTR,F            ;If counter 0, skip to end
	bz      B16
	incf    DBLCLKCNTR,F            ;If counter has started, increment
	movlw   50                      ;50*10msec = 0.5sec
	cpfslt  DBLCLKCNTR              ;If counter hasn't reached 50, skip
	clrf    DBLCLKCNTR              ;Too late to double click now
	movlw   0                       ;
	cpfsgt  DBLCLKCNTR              ;Was the counter reset now?
	bsf     STATS,BTN               ;After .5 seconds, do a single press of	
B16                                     ;the button
	movf    PORTD,W
	andlw   b'00001000'             ;All except button bit = 0
	cpfseq  OLDBUTTON
	rcall   Do_Button               ;If state of button changed, go to
	return                          ;Do_Button

;;;;;;; Do_Button subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Recognizes a button press on a rising edge.

Do_Button
	movwf   OLDBUTTON
	btfss   OLDBUTTON,3             ;Find only rising edges, return on
	return                          ;falling
	btg     STATS,TURN              ;Change turn immediately (no 0.5s wait)
	movlf   500,DELAYCNTR           ;When turn changes, reset delay counter
	btfsc   STATS,FISCH             ;Is fischer added time mode set?
	rcall   AddTime                 ;Add 5 seconds to current player's clock
	movf    DBLCLKCNTR,F            ;Is the counter zero?
	bz      B17                     ;If not, then
	rcall   DoubleClick             ;It was a double click!
	return
B17
	incf    DBLCLKCNTR,F            ;Not a double click, but start timing
	return

;;;;;;; DoubleClick subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DoubleClick
	clrf    DBLCLKCNTR
	bsf     STATS,DBL               ;Change flags to double click instead of
	bcf     STATS,BTN               ;one click
	return

;;;;;;; ClockSelect subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Selects the correct clock to increment or decrement or returns immediately if
; the game hasn't started yet.

ClockSelect
	btfss   STATS,PLAY              ;If play flag isn't set, return
	return
	btfss   STATS,TURN              ;Skip if white's turn
	bra     B18                     ;Select black's clock
                                        ;Select white's clock
	lfsr    1,WCLOCK                ;Load address of WCLOCK to FSR1
	btfss   STATS,INC               ;Skip if clocks are set to increment
	rcall   ClockDecrement          ;Decrement clock
	btfsc   STATS,INC               ;Skip if clocks are set to decrement
	rcall   ClockIncrement          ;Increment clock
	lfsr    1,WCLOCK+1              ;Point to seconds in WCLOCK
	lfsr    0,LCDTOPROW+7           ;Load address of LCDTOPROW+7 to FSR0
	rcall   UpdateClockV            ;Update clock vector
	lfsr    0,LCDTOPROW             ;Load address of LCDTOPROW to FSR0
	rcall   DisplayV                ;Display time played
	return
B18
	lfsr    1,BCLOCK                ;Load address of BCLOCK to FSR1
	btfss   STATS,INC               ;Skip if clocks are set to increment
	rcall   ClockDecrement          ;Decrement clock
	btfsc   STATS,INC               ;Skip if clocks are set to decrement
	rcall   ClockIncrement          ;Increment clock
	lfsr    1,BCLOCK+1              ;Point to seconds in BCLOCK
	lfsr    0,LCDBOTROW+7           ;Load address of LCDBOTROW+7 to FSR0
	rcall   UpdateClockV            ;Update clock vector
	lfsr    0,LCDBOTROW             ;Load address of LCDBOTROW to FSR0
	rcall   DisplayV                ;Display time played
	return

;;;;;;; ClockIncrement subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Increments a clock vector found in INDF1 by 10msec and updates it to be
; presentable in the form of 00:00.00. In delay mode decrements delay counter
; before incrementing clock.

ClockIncrement
	btfss   STATS,DELAY             ;Is delay mode enabled?
	bra     B19                     ;If not, skip to incrementing
	movf    DELAYCNTR,F             ;Is it zero?
	bz      B19
	decf    DELAYCNTR,F             ;Decrement delay
	return                          ;Skip incrementing until delay is zero
B19
	incf    INDF1,F                 ;Increment (tens of) milliseconds by 1
	movf    INDF1,W                 ;Get amount of msec passed
	sublw   100                     ;After 100*10msec, increment seconds
	bnz     B20                     ;If no need to increment, return
	movlw   100                     ;Substract 100 from milliseconds
	subwf   INDF1,F                 ;
	incf    PREINC1,F               ;Increment seconds passed
	movf    INDF1,W                 ;Get amount of sec passed
	sublw   10                      ;After 10 sec, increment tens of secs
	bnz     B20                     ;If no need to increment, return
	movlw   10                      ;Substract 10 from seconds
	subwf   INDF1,F                 ;
	incf    PREINC1                 ;Increment tens of seconds passed
	movf    INDF1,W                 ;Get tens of secs passed
	sublw   6                       ;After 6*10sec passed, increment mins
	bnz     B20                     ;If no need to increment, return
	movlw   6                       ;Substract 6 from tens of seconds
	subwf   INDF1,F                 ;
	incf    PREINC1,F               ;Increment minutes passed
	movf    INDF1,W                 ;Get minutes passed
	sublw   10                      ;After 10 mins, increment tens of mins
	bnz     B20                     ;If no need to increment, return
	movlw   10                      ;Substract 10 from minutes
	subwf   INDF1,F                 ;
	incf    PREINC1,F               ;Increment tens of minutes passed
B20
	return

;;;;;;; ClockDecrement subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Decrements a clock vector found in INDF1 by 10msec and updates it to be
; presentable in the form of 00:00.00. If the clock reaches zero, does nothing.
; In delay mode, decrements delay counter before decrementing clock.

ClockDecrement
	btfss   STATS,DELAY             ;Is delay mode enabled?
	bra     B21                     ;If not, skip to decrementing
	movf    DELAYCNTR,F             ;Is it zero?
	bz      B21                     ;If zero, skip to decrementing
	decf    DELAYCNTR,F             ;Decrement delay
	return                          ;Skip decrementing until delay is zero
B21
	movlw   0                       ;Check if msec is zero
	cpfsgt  INDF1
	bra     B22
	decf    INDF1,F                 ;Decrement milliseconds
	return                          ;Done
B22
	cpfsgt  INDF1+1                 ;Check if seconds are zero
	bra     B23
	decf    INDF1+1,F               ;Decrement seconds
	movlf   99,INDF1                ;990 milliseconds left
B23
	movlw   0                       ;movlf call changed word register
	cpfsgt  INDF1+2                 ;Check if tens of seconds is zero
	bra     B24
	decf    INDF1+2                 ;Decrement tens of seconds
	movlf   9,INDF1+1               ;9 seconds plus
	movlf   99,INDF1                ;990 milliseconds left
B24
	movlw   0                       ;movlf call changed word register
	cpfsgt  INDF1+3                 ;Check if minutes are zero
	bra     B25
	decf    INDF1+3                 ;Decrement minutes
	movlf   5,INDF1+2               ;50 seconds plus
	movlf   9,INDF1+1               ;9 seconds plus
	movlf   99,INDF1                ;990 milliseconds are left
B25
	movlw   0                       ;movlf call changed word register
	cpfsgt  INDF1+4                 ;Check if tens of minutes is zero
	return                          ;Everything was zero
	decf    INDF1+4                 ;Decrement tens of minutes
	movlf   9,INDF1+3               ;9 minutes plus
	movlf   5,INDF1+2               ;50 seconds plus
	movlf   9,INDF1+1               ;9 seconds plus
	movlf   99,INDF1                ;990 milliseconds are left
	return

;;;;;;; AddTime subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; In Fischer added time mode, adds 5 seconds to the current players clock in the
; beginning of the turn. Does not reformat the clocks. If clocks are set to
; increment, does nothing. At the moment added time is not implemented for
; incrementing time.

AddTime
	btfss   STATS,PLAY              ;Has the game started?
	return                          ;Return if not
	btfsc   STATS,MENU              ;Are we in the WaitButton subroutine?
	return                          ;Return if we are
	btfsc   STATS,INC               ;Are clocks set to increment?
	return                          ;Return if they are
	movlw   5                       ;We want to add or substact 5 seconds
	btfss   STATS,TURN              ;Skip if white's turn
	bra     B26
	addwf   WCLOCK+1,F              ;Add 5 seconds for white
	return
B26
	addwf   BCLOCK+1,F              ;Add 5 seconds for black
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
	bnz     B27
	movlf   100,ALIVECNT            ;Reinitialize ALIVECNT
	bcf     PORTA,RA4               ;Turn on LED for ten msec
B27
	return

;;;;;;; T40 subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Pause for 40 microseconds or 40/0.4 = 100 clock cycles
; Assumes 10/4 = 2.5 MHz internal clock rate

T40
	movlw 100/3                     ;Each REPEAT loop takes 3 cycles
	movwf COUNT
	;REPEAT_
L28
	  decf    COUNT,F
	;UNTIL_   .Z.
	bnz     L28
RL28
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
Modec   db      0x80,A'M',A'o',A'd',A'e',A':',A' ',A' ',0x00 ;"Mode:"
Normal  db      0xC0,A'N',A'o',A'r',A'm',A'a',A'l',A' ',0x00 ;"Normal"
Fischer db      0xC0,A'F',A'i',A's',A'c',A'h',A'e',A'r',0x00 ;"Fischer"
Delay   db      0xC0,A'D',A'e',A'l',A'a',A'y',A' ',A' ',0x00 ;"Delay"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	end                             ;End program