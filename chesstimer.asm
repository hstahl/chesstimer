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
;   ClockRunning
;   Button
;   LoopTime
;
;;;;;;; Assembler directives ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		list  P=PIC18F452, F=INHX32, R=DEC
		#include P18F452.INC

;;;;;;; Configuration bits ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		__CONFIG  _CONFIG1H, _HS_OSC_1H  ;High Speed oscillator
		__CONFIG  _CONFIG2L, _PWRT_ON_2L & _BOR_ON_2L & _BORV_42_2L  ;Reset
		__CONFIG  _CONFIG2H, _WDT_OFF_2H  ;Watchdog timer disabled
		__CONFIG  _CONFIG3H, _CCP2MX_ON_3H  ;CCP2 to RC1 (rather than to RB3)
		__CONFIG  _CONFIG4L, _LVP_OFF_4L  ;RB5 enabled for I/O

;;;;;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		cblock  0x000			;Beginning of access memory
		TMR0LCOPY				;Copy of sixteen-bit Timer0 used by LoopTime
		TMR0HCOPY
		INTCONCOPY				;Copy of INTCON for LoopTime
		OLDBUTTON				;State of button at previous loop
		WHITESTURN				;Which player's turn is being timed
		endc

;;;;;;; Vectors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		org  0x0000				;Reset vector
		nop
		goto  Mainline

		org  0x0008				;High priority interrupt
		goto  $					;Trap

		org  0x0018				;Low priority interrupt
		goto  $					;Trap

;;;;;;; Mainline program ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Mainline


		end						;End program