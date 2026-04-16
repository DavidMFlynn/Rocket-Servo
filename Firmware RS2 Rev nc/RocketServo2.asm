;====================================================================================================
;
;   Filename:	RocketServo2.asm
;   Date:	12/20/2025
;   File Version:	1.0b1
;
;    Author:	David M. Flynn
;    E-Mail:	dflynn.oxfordvue@gmail.com
;
;====================================================================================================
;    RC Servo Activator (2 channel) for high power rocketry uses PIC16F1847
;
;    History:
; 1.0b1   12/20/2025   Copied from RocketServo rev 1.2b1
;
;====================================================================================================
; Options
HasEnable	EQU	1
HasBattV	EQU	1	;Set to 0 or 1
HasServoCurrent	EQU	0
BP3Hold	EQU	0	;Hold at reverse value
;====================================================================================================
; To Do's
;
;====================================================================================================
;====================================================================================================
; What happens next:
;
;   The system LED blinks once per second
;  Once at power-up:
;   Blink battery voltage i.e. _ ......... _
;   Do not arm if <7.0 2S LiPO, fast blink for 10 seconds
;   Set position to the commanded position for 3 seconds.
;
;  Setup mode:
;   If SW1 is pressed Increment the set value for the control condition.
;   If SW2 is pressed Decrement the set value for the control condition.
;
;   If Control is High (active)
;      move to set point 1 for 3 seconds
;   else if Control is Low (Normal, Inactive)
;      move to set point 2 for 3 seconds
;
;   CCP1, CCP2 outputs a 900uS to 2100uS (1800..4200 counts) pulse every 16,000uS
;
;   Resolution is 0.5uS
;
;  if kInPosShutdown is set servo will power down after 3s
;
;====================================================================================================
;
;  Pin 1  RA2 		SW1 Inc switch Active Low Input 
;  Pin 2  RA3 		Enable1 Conductivity Enable Active Low Output	
;  Pin 3  RA4 		Enable2 Conductivity Enable Active Low Output		
;  Pin 4  RA5/MCLR*/Vpp (Input only)	Vpp	
;  Pin 5  VSS (Ground)		Ground
;  Pin 6  RB0/CCP1		Servo1 Signal Output
;  Pin 7  RB1		Trigger1 Active Low Input
;  Pin 8  RB2		Trigger2 Active Low Input
;  Pin 9  RB3		Servo2LED Active Active Low Output
;  Pin 10 RB4		Servo1LED Active Active Low Output
;  Pin 11 RB5		SW2 Dec switch Active Low Input
;  Pin 12 RB6 ICSPCLK		PGC
;  Pin 13 RB7 ICSPDAT		PGD
;  Pin 14 VDD (+5V)		+5V
;  Pin 15 RA6 		System LED Active Low Output
;  Pin 16 RA7/CCP2		Servo2 Signal Output
;  Pin 17 RA0 AN0/Battery (volts-0.5)/21
;  Pin 18 RA1 AN1/Servo_I
;
;====================================================================================================
;
;
	list	p=16f1847,r=hex,w=1	; list directive to define processor
;
	nolist
	include	p16f1847.inc	; processor specific variable definitions
	list
;
	__CONFIG _CONFIG1,_FOSC_INTOSC & _WDTE_OFF & _MCLRE_OFF & _IESO_OFF
;
;
; INTOSC oscillator: I/O function on CLKIN pin
; WDT disabled
; PWRT disabled
; MCLR/VPP pin function is digital input
; Program memory code protection is disabled
; Data memory code protection is disabled
; Brown-out Reset enabled
; CLKOUT function is disabled. I/O or oscillator function on the CLKOUT pin
; Internal/External Switchover mode is disabled
; Fail-Safe Clock Monitor is enabled
;
	__CONFIG _CONFIG2,_WRT_OFF & _PLLEN_ON & _LVP_OFF
;
; Write protection off
; 4x PLL Enabled
; Stack Overflow or Underflow will cause a Reset
; Brown-out Reset Voltage (Vbor), low trip point selected.
; Low-voltage programming disabled
;
; '__CONFIG' directive is used to embed configuration data within .asm file.
; The lables following the directive are located in the respective .inc file.
; See respective data sheet for additional information on configuration word.
;
	constant	oldCode=0
	constant	useRS232=0
	constant	UseEEParams=1
;
#Define	_C	STATUS,C
#Define	_Z	STATUS,Z
;
; 0.5uS res counter from 8MHz OSC
CCPnCON_Clr	EQU	b'00001001'	;Clear output on match
CCPnCON_Set	EQU	b'00001000'	;Set output on match
CCPnCON_Int	EQU	b'00001010'
;
MinBattVolts	EQU	d'232'	;V/35.8 * 204.8c/v = 6.5V
;
ActivationTime	EQU	d'300'	;3 seconds
kOffsetCtrValue	EQU	d'2047'
kMinPulseWidth	EQU	d'1800'	;900uS
kMidPulseWidth	EQU	d'3000'	;1500uS
kMaxPulseWidth	EQU	d'4200'	;2100uS
	if BP3Hold
kDefaultPosition1	EQU	d'2000'	;1000uS Locked
kDefaultPosition2	EQU	d'4110'	;2055uS Open
HoldOpen	EQU	1	; servo 1 only
	else
kDefaultPosition1	EQU	d'3600'	;1800uS
kDefaultPosition2	EQU	d'2200'	;1100uS
HoldOpen	EQU	0
	endif
kServoDwellTime	EQU	d'32000'	;16mS
kInPosShutdown	EQU	b'00100010'	;b'00100010' enabled 0x00 disabled
;
;====================================================================================================
	nolist
	include	F1847_Macros.inc
	list
;
;    Port A bits
PortADDRBits	EQU	b'01100111'
#Define	RA0_In	PORTA,0	;AN0/Battery (volts-0.5)/21
#Define	RA1_In	PORTA,1	;AN1/Servo_I
#Define	IncBtnBit	PORTA,2	;SW1 Inc switch Active Low Input 
#Define	ContEnable1	LATA,3	;Enable1 Conductivity Enable Active Low Output
#Define	ContEnable2	LATA,4	;Enable2 Conductivity Enable Active Low Output
#Define	RA5_In	PORTA,5	; Vpp only
#Define	SystemLED	LATA,6	;Output: 0=LED ON
#Define	SysLEDTrisBit	TRISA,6
#Define	RA7_In	PORTA,7	;CCP2, Servo2
;
PortAValue	EQU	b'00000000'
;
;    Port B bits
PortBDDRBits	EQU	b'11111110'
#Define	RB0_In	PORTB,0	;CCP1, Servo1
#Define	S1CmdInputBit	PORTB,1	;Trigger1 Active Low Input
#Define	S2CmdInputBit	PORTB,2	;Trigger2 Active Low Input
#Define	Servo2LEDBit	PORTB,3	;Servo2LED Active Active Low Output
#Define	Servo2LEDTrisBit	TRISB,3
#Define	Servo1LEDBit	PORTB,4	;Servo1LED Active Active Low Output
#Define	Servo1LEDTrisBit	TRISB,4
#Define	DecBtnBit	PORTB,5	;SW2 Dec switch Active Low Input
#Define	RB6_In	PORTB,6	;ICSPCLK
#Define	RB6_In	PORTB,6	;ICSPCLK
;
PortBValue	EQU	b'00000000'
;
;
;====================================================================================================
;====================================================================================================
;
;Constants
All_In	EQU	0xFF
All_Out	EQU	0x00
;
LEDTIME	EQU	d'100'	;1.00 seconds
LEDErrorTime	EQU	d'10'
LEDFlashTime	EQU	d'20'
;
T1CON_Val	EQU	b'00000001'	;Fosc=8MHz, PreScale=1,Fosc/4,Timer ON
;T1CON_Val	EQU	b'00100001'	;Fosc=32MHz, PreScale=4,Fosc/4,Timer ON
;
OSCCON_Value	EQU	b'01110010'	;8MHz
;OSCCON_Value	EQU	b'11110000'	;32MHz
;
T2CON_Value	EQU	b'01001110'	;T2 On, /16 pre, /10 post
;T2CON_Value	EQU	b'01001111'	;T2 On, /64 pre, /10 post
PR2_Value	EQU	.125
;
;
CCP1CON_Value	EQU	0x00	;CCP1 off
;
;================================================================================================
;***** VARIABLE DEFINITIONS
; there are 1024 bytes of ram, Bank0 0x20..0x7F, Bank1 0xA0..0xEF, ....
; there are 256 bytes of EEPROM starting at 0x00 the EEPROM is not mapped into memory but
;  accessed through the EEADR and EEDATA registers
;================================================================================================
;  Bank0 Ram 020h-06Fh 80 Bytes
;
	cblock	0x20
;
	ISR_Temp		;scratch mem
	LED_Time
	LED2_PeriodTime		;Usually 100=1s, Set to 1 for constant ON
	LED2_FlashTime		;Set to 1 for constant ON, otherwise LEDFlashTime
	LED3_PeriodTime
	LED3_FlashTime		;Set to 1 for constant ON, otherwise LEDFlashTime
	LED_Ticks		;Timer tick count
	LED2_Ticks
	LED3_Ticks
	LED_Blinks		;nBlinks, 0xFF=system blink, 0=Off
	LED2_Flashes		;nFlashes 0..3, 0=Off
	LED2_FlashCount
	LED3_Flashes		;nFlashes 0..3, 0=Off
	LED3_FlashCount
;
;
	EEAddrTemp		;EEProm address to read or write
	EEDataTemp		;Data to be writen to EEProm
;
;
	Timer1Lo		;1st 16 bit timer
	Timer1Hi		; 1 sec delay, local loop
	Timer2Lo		;2nd 16 bit timer
	Timer2Hi		; Servo 1 active timer
	Timer3Lo		;3rd 16 bit timer
	Timer3Hi		; Servo 1 Activation timer
	Timer4Lo		;4th 16 bit timer
	Timer4Hi		; debounce timer and power up delay timer
	Timer5Lo		;5th 16 bit timer
	Timer5Hi		; Servo 2 active timer
	Timer6Lo		;6th 16 bit timer
	Timer6Hi		; Servo 2 Activation timer
;
	State		;State 0..n, Startup LED sequence and main state
	State2		;State 0..n, Servo current testing
;
	S1Dest:2
	S1CurPos:2
	S2Dest:2
	S2CurPos:2
;
	BtnFlags
	ServoFlags
;
	S1Position1:2		;Servo1 rest
	S1Position2:2		;Servo1 Triggered
	S2Position1:2		;Servo2 rest
	S2Position2:2		;Servo2 Triggered
	SysFlags
;
	S1CmdDebounce
	S2CmdDebounce
;
	endc
;	
Servo1MoveTimerLo	EQU	Timer2Lo
Servo1MoveTimerHi	EQU	Timer2Hi
Servo2MoveTimerLo	EQU	Timer5Lo
Servo2MoveTimerHi	EQU	Timer5Hi
Servo1ActiveTimerLo	EQU	Timer3Lo
Servo1ActiveTimerHi	EQU	Timer3Hi
Servo2ActiveTimerLo	EQU	Timer6Lo
Servo2ActiveTimerHi	EQU	Timer6Hi
;
;
; BtnFlags
#Define	IncBtnFlag	BtnFlags,0
#Define	DecBtnFlag	BtnFlags,1
#Define	S1Active	BtnFlags,2	;Last servo activated
#Define	S2Active	BtnFlags,3	; used for buttons
#Define	NewSWData	BtnFlags,4
#Define	LED2Flag	BtnFlags,5
#Define	LED3Flag	BtnFlags,6
#Define	DataChangedFlag	BtnFlags,7
;
; ServoFlags bits
#Define	S1Found	ServoFlags,0	;Detected w/ current monitor
#Define	S2Found	ServoFlags,1
#Define	S1CurPosIsDest	ServoFlags,2
#Define	S2CurPosIsDest	ServoFlags,3
;
; SysFlags
#Define	S1PulseSent	SysFlags,0
#Define	S1InPosShutdown	SysFlags,1
#Define	S1InPosition	SysFlags,2
#Define	BattV_OK	SysFlags,3
#Define	S2PulseSent	SysFlags,4
#Define	S2InPosShutdown	SysFlags,5
#Define	S2InPosition	SysFlags,6
;
#Define	FirstRAMParam	S1Position1
#Define	LastRAMParam	SysFlags
;
;================================================================================================
;  Bank1 Ram 0A0h-0EFh 80 Bytes
;
	cblock	0x0A0
; Raw AN Results
	AN0_RawL
	AN0_RawH
	AN1_RawL
	AN1_RawH
; Data for main thread, protected by semaphores
	AN0_DataL
	AN0_DataH
	AN1_DataL
	AN1_DataH
;
	IDleCurrent:2
	PeakCurrent:2
;
	AN_Semaphores
	AN_NewRawDataFlags
	AN_NewDataFlags
;
	endc
;
; AN_Semaphores bits
#Define	AN0_Semaphore	AN_Semaphores,0	
#Define	AN1_Semaphore	AN_Semaphores,1
; AN_NewRawDataFlags bits
#Define	AN0_NewRawData	AN_NewRawDataFlags,0
#Define	AN1_NewRawData	AN_NewRawDataFlags,1
; AN_NewDataFlags bits
#Define	AN0_NewData	AN_NewDataFlags,0
#Define	AN1_NewData	AN_NewDataFlags,1
;
;================================================================================================
;  Bank5 Ram 2A0h-2EFh 80 Bytes (CCP1 and CCP2 are in this bank)
	cblock	0x2A0
;
	S1SigOutTime
	S1SigOutTimeH
	S1Flags
	S1CalcdDwell
	S1CalcdDwellH
;
	S2SigOutTime
	S2SigOutTimeH
	S2Flags
	S2CalcdDwell
	S2CalcdDwellH
;
;
	endc
;
; S1Flags bits
#Define	S1ServoOff	S1Flags,4
#Define	S1SMRevFlag	S1Flags,5
;
; S2Flags bits
#Define	S2ServoOff	S2Flags,4
#Define	S2SMRevFlag	S2Flags,5
;
;
ServoMoveTime	EQU	.300	;time servo is powered when CMD changes
;
BtnChangeRate	EQU	0x02	;change by 1uS per 0.05 seconds
;SlewChangeRate	EQU	0x20	;change by 16uS per 0.01 seconds, Slow
SlewChangeRate	EQU	0x40	;change by 32uS per 0.01 seconds, good
;SlewChangeRate	EQU	0x60	;change by 48uS per 0.01 seconds, Fast
;
;=======================================================================================================
;  Common Ram 70-7F same for all banks
;      except for ISR_W_Temp these are used for paramiter passing and temp vars
;=======================================================================================================
;
	cblock	0x70
	Param70
	Param71
	Param72
	Param73
	Param74
	Param75
	Param76
	Param77
	Param78
	Param79
	Param7A
	Param7B
	Param7C
	Param7D
	Param7E
	Param7F
	endc
;
;=========================================================================================
;Conditionals
HasISR	EQU	0x80	;used to enable interupts 0x80=true 0x00=false
;
;=========================================================================================
;==============================================================================================
; ID Locations
	__idlocs	0x10B1
;
;==============================================================================================
; EEPROM locations (NV-RAM) 0x00..0x7F (offsets)
	org	0xF000
; Servo 1 initial values
	de	LOW kDefaultPosition1	;nvPosition1Lo
	de	HIGH kDefaultPosition1
	de	LOW kDefaultPosition2
	de	HIGH kDefaultPosition2
; Servo 2 initial values
	de	LOW kDefaultPosition1	;nvPosition1Lo
	de	HIGH kDefaultPosition1
	de	LOW kDefaultPosition2
	de	HIGH kDefaultPosition2
;
	de	kInPosShutdown	;nvSysFlags
;
	cblock	0x00
	nvPosition1Lo
	nvPosition1Hi
	nvPosition2Lo
	nvPosition2Hi
	nvPosition3Lo
	nvPosition3Hi
	nvPosition4Lo
	nvPosition4Hi
;
	nvSysFlags
	endc
;
#Define	nvFirstParamByte	nvPosition1Lo
#Define	nvLastParamByte	nvSysFlags
;
;
;==============================================================================================
;============================================================================================
;
;
	ORG	0x000	; processor reset vector
	CLRF	STATUS
	CLRF	PCLATH
  	goto	start	; go to beginning of program
;
;===============================================================================================
; Interupt Service Routine
;
; we loop through the interupt service routing every 0.01 seconds
;
;
	ORG	0x004	; interrupt vector location
	CLRF	PCLATH
	movlb	0	; bank0
;
	btfss	PIR1,TMR2IF
	goto	IRQ_2
	bcf	PIR1,TMR2IF	; reset interupt flag bit
;
;Decrement timers until they are zero
;
	CLRF	FSR0H
	call	DecTimer1	;if timer 1 is not zero decrement
	call	DecTimer2
	call	DecTimer3
	call	DecTimer4
	movlw	Timer5Hi
	call	DecTimer
	movlw	Timer6Hi
	call	DecTimer
;
;-----------------------------------------------------------------
; blink LEDs
	movlb                  1                      ;bank 1
	BSF	SysLEDTrisBit	;LED off
	BSF	Servo1LEDTrisBit	;LED2 off
	BSF	Servo2LEDTrisBit	;LED3 off
;
	movlb	0	;bank 0
	BCF	IncBtnFlag
	BTFSS	IncBtnBit
	BSF	IncBtnFlag
;
	BCF	DecBtnFlag
	BTFSS	DecBtnBit
	BSF	DecBtnFlag
;
	bsf	NewSWData	;New data every 0.01s
;
SystemBlink_1	DECFSZ	LED_Ticks,F	;Time to blink?
	bra	SystemBlink_end	; no
;
	MOVF	LED_Time,W
	movwf	LED_Ticks
;
	movf	LED_Blinks,F	;nBlinks, 0xFF=system blink, 0=Off
	SKPNZ		;LED_Blinks=0?
	BRA	SystemBlink_end	; Yes, do nothing
	incfsz	LED_Blinks,W	;LED_Blinks=0xFF
	BRA	SystemBlink_Blinking	; No
	BRA	SystemBlink_on	; Yes
;
SystemBlink_Blinking	decf	LED_Blinks,F
;
SystemBlink_on	movlb                  1                      ;bank 1
	BCF	SysLEDTrisBit	;LED on
SystemBlink_end	movlb	0                      ;bank 0
;
;--------------
; Handle LED2
; Flash LEDn_Flashes (0..3) 
; For N flashes per period set LEDn_Flashes=N, LEDn_PeriodTime=LEDTIME, LEDn_FlashTime=LEDFlashTime
; For continious ON set LEDn_PeriodTime=1, LEDn_FlashTime=1, LEDn_Flashes=1
;
	movf	LED2_Ticks,F
	SKPNZ		;LED2_Ticks=0?
	bra	LED2_DoUpdate	; Yes
	decfsz	LED2_Ticks,F	;Time to flash?
	bra	IRQ_LED2_End	; No, Leave LED off
;
LED2_DoUpdate	movf	LED2_PeriodTime,W	;default time in case LED2_Flashes=0
	movwf	LED2_Ticks
;
	movf	LED2_Flashes,W
	SKPNZ		;Any flashes?
	bra	IRQ_LED2_End	; No, leave LED off
;
; Set time until next flash
; if LED2_FlashCount>0 then LED2_FlashCount=LED2_FlashCount-1, LED2_Ticks=LED2_FlashTime, LED=ON
;	else LED2_FlashCount=LED2_Flashes, LED=OFF
;
	movf	LED2_FlashCount,F
	SKPZ		;LED2_FlashCount=0?
	bra	LED2_ContinueFlashes
;
	movf	LED2_Flashes,W
	movwf	LED2_FlashCount
	bra	IRQ_LED2_End	
;
LED2_ContinueFlashes	decf	LED2_FlashCount,F
	movf	LED2_FlashTime,W
	movwf	LED2_Ticks
;
IRQ_LED2_ON	movlb                  1                      ;bank 1
	BCF	Servo1LEDTrisBit	;LED ON
;
IRQ_LED2_End	movlb	0
;
;-------------
; Handle LED3
; Flash LEDn_Flashes (0..3) 
; For N flashes per period set LEDn_Flashes=N, LEDn_PeriodTime=LEDTIME, LEDn_FlashTime=LEDFlashTime
; For continious ON set LEDn_PeriodTime=1, LEDn_FlashTime=1, LEDn_Flashes=1
;
	movf	LED3_Ticks,F
	SKPNZ		;LED3_Ticks=0?
	bra	LED3_DoUpdate	; Yes
	decfsz	LED3_Ticks,F	;Time to flash?
	bra	IRQ_LED3_End	; No, Leave LED off
;
LED3_DoUpdate	movf	LED3_PeriodTime,W	;default time in case LED3_Flashes=0
	movwf	LED3_Ticks
;
	movf	LED3_Flashes,W
	SKPNZ		;Any flashes?
	bra	IRQ_LED3_End	; No, leave LED off
;
; Set time until next flash
;
; Set time until next flash
; if LED3_FlashCount>0 then LED3_FlashCount=LED3_FlashCount-1, LED3_Ticks=LED3_FlashTime, LED=ON
;	else LED3_FlashCount=LED3_Flashes, LED=OFF
;
	movf	LED3_FlashCount,F
	SKPZ		;LED3_FlashCount=0?
	bra	LED3_ContinueFlashes
;
	movf	LED3_Flashes,W
	movwf	LED3_FlashCount
	bra	IRQ_LED3_End	
;
LED3_ContinueFlashes	decf	LED3_FlashCount,F
	movf	LED3_FlashTime,W
	movwf	LED3_Ticks
;
IRQ_LED3_ON	movlb                  1                      ;bank 1
	BCF	Servo2LEDTrisBit	;LED ON
;
IRQ_LED3_End	movlb	0

;-----------------------------------------------------------------
;
IRQ_2:
	movlb	0
	btfsc	PIR1,ADIF
	goto	IRQ_ADC
IRQ_ADC_Return:
;
;==================================================================================
;
; Handle CCP1 Interupt Flag, Enter w/ bank 0 selected
;
	movlb	0	;bank 0
	BTFSS	PIR1,CCP1IF
	bra	IRQ_Servo1_End
;
	BSF	S1PulseSent
	movlb	0x05                   ;Bank 5
	BTFSS	S1ServoOff	;Are we sending a pulse?
	bra	IRQ_Servo1_1	; Yes
;
;Oops, how did we get here???
	CLRF	CCP1CON
	bra	IRQ_Servo1_X
;
IRQ_Servo1_1	BTFSC	CCP1CON,CCP1M0	;Set output on match?
	bra	IRQ_Servo1_OL	; No
; An output just went high
;
	MOVF	S1SigOutTime,W	;Put the pulse into the CCP reg.
	ADDWF	CCPR1L,F
	MOVF	S1SigOutTime+1,W
	ADDWFC	CCPR1H,F
	movlb	0	;Bank 0
	movlw	CCPnCON_Int	;default to do nothing
	btfss	S1InPosShutdown
	MOVLW	CCPnCON_Clr	;Clear output on match
	btfss	S1InPosition
	MOVLW	CCPnCON_Clr	;Clear output on match
	movlb	5	;Bank 5
	MOVWF	CCP1CON	;CCP1 clr on match
;Calculate dwell time
	MOVLW	LOW kServoDwellTime
	MOVWF	S1CalcdDwell
	MOVLW	HIGH kServoDwellTime
	MOVWF	S1CalcdDwellH
	MOVF	S1SigOutTime,W
	SUBWF	S1CalcdDwell,F
	MOVF	S1SigOutTime+1,W
	SUBWFB	S1CalcdDwellH,F
	bra	IRQ_Servo1_X
;
; output went low so this cycle is done
IRQ_Servo1_OL	MOVLW	LOW kServoDwellTime
	ADDWF	CCPR1L,F
	MOVLW	HIGH kServoDwellTime
	ADDWFC	CCPR1H,F
;
	movlb	0	;Bank 0
	movlw	CCPnCON_Int	;default to do nothing
	btfss	S1InPosShutdown
	MOVLW	CCPnCON_Set	;Set output on match
	btfss	S1InPosition
	MOVLW	CCPnCON_Set	;Set output on match
	movlb	5	;Bank 5
	MOVWF	CCP1CON	;Idle output low
;
IRQ_Servo1_X	movlb	0x00                   ;Bank 0
	BCF	PIR1,CCP1IF
IRQ_Servo1_End:
;
;--------------------------------------------------------------------
;
; Handle CCP2 Interupt Flag, Enter w/ bank 0 selected
;
	movlb	0	;bank 0
	BTFSS	PIR2,CCP2IF
	bra	IRQ_Servo2_End
;
	BSF	S2PulseSent
	movlb	0x05                   ;Bank 5
	BTFSS	S2ServoOff	;Are we sending a pulse?
	bra	IRQ_Servo2_1	; Yes
;
;Oops, how did we get here???
	CLRF	CCP2CON
	bra	IRQ_Servo2_X
;
IRQ_Servo2_1	BTFSC	CCP2CON,CCP2M0	;Set output on match?
	bra	IRQ_Servo2_OL	; No
; An output just went high
;
	MOVF	S2SigOutTime,W	;Put the pulse into the CCP reg.
	ADDWF	CCPR2L,F
	MOVF	S2SigOutTime+1,W
	ADDWFC	CCPR2H,F
	movlb	0	;Bank 0
	movlw	CCPnCON_Int
	btfss	S2InPosShutdown
	MOVLW	CCPnCON_Clr	;Clear output on match
	btfss	S2InPosition
	MOVLW	CCPnCON_Clr	;Clear output on match
	movlb	5	;Bank 5
	MOVWF	CCP2CON	;CCP1 clr on match
;Calculate dwell time
	MOVLW	LOW kServoDwellTime
	MOVWF	S2CalcdDwell
	MOVLW	HIGH kServoDwellTime
	MOVWF	S2CalcdDwellH
	MOVF	S2SigOutTime,W
	SUBWF	S2CalcdDwell,F
	MOVF	S2SigOutTime+1,W
	SUBWFB	S2CalcdDwellH,F
	bra	IRQ_Servo2_X
;
; output went low so this cycle is done
IRQ_Servo2_OL	MOVLW	LOW kServoDwellTime
	ADDWF	CCPR2L,F
	MOVLW	HIGH kServoDwellTime
	ADDWFC	CCPR2H,F
;
	movlb	0	;Bank 0
	movlw	CCPnCON_Int
	btfss	S2InPosShutdown
	MOVLW	CCPnCON_Set	;Set output on match
	btfss	S2InPosition
	MOVLW	CCPnCON_Set	;Set output on match
	movlb	5	;Bank 5
	MOVWF	CCP2CON	;Idle output low
;
IRQ_Servo2_X	movlb	0x00                   ;Bank 0
	BCF	PIR2,CCP2IF
IRQ_Servo2_End:
;--------------------------------------------------------------------
;
	retfie		; return from interrupt
;
;
;==============================================================================================
;**********************************************************************************************
;==============================================================================================
;
start	movlb	0x01	; select bank 1
	bsf	OPTION_REG,NOT_WPUEN	; disable pullups on port B
	bcf	OPTION_REG,TMR0CS	; TMR0 clock Fosc/4
	bcf	OPTION_REG,PSA	; prescaler assigned to TMR0
	bsf	OPTION_REG,PS0	;111 8mhz/4/256=7812.5hz=128uS/Ct=0.032768S/ISR
	bsf	OPTION_REG,PS1	;101 8mhz/4/64=31250hz=32uS/Ct=0.008192S/ISR
	bsf	OPTION_REG,PS2
;
	MOVLW	OSCCON_Value
	MOVWF	OSCCON
	movlw	b'00010111'	; WDT prescaler 1:65536 period is 2 sec (RESET value)
	movwf	WDTCON
;
	movlb	0x03	; bank 3
;
	movlw	0x03	; AN0,AN1 only
	MOVWF	ANSELA	
	movlw	0x00
	movwf	ANSELB
;
; setup timer 1 for 0.5uS/count
;
	movlb	0	; bank 0
	MOVLW	T1CON_Val
	MOVWF	T1CON
	bcf	T1GCON,TMR1GE
;
; clear memory to zero
	CALL	ClearRam
;
; Setup timer 2 for 0.01S/Interupt
	MOVLW	T2CON_Value	;Setup T2 for 100/s
	MOVWF	T2CON
	MOVLW	PR2_Value
	MOVWF	PR2
	movlb	1	;Bank 1
	BSF	PIE1,TMR2IE
;
; setup ccp1
	movlb	5                      ;Bank 5
	BSF	S1ServoOff
	movlb	2                      ;bank 2
	BSF	APFCON0,CCP1SEL	;RB0
	movlb	5                      ;bank 5
	CLRF	CCP1CON
;
	movlb	0x01	;Bank 1
	bsf	PIE1,CCP1IE
;
; setup ccp2
	movlb	5                      ;Bank 5
	BSF	S2ServoOff
	movlb	2                      ;bank 2
	BSF	APFCON0,CCP2SEL	;RA7
	movlb	5                      ;bank 5
	CLRF	CCP2CON
;
	movlb	0x01	;Bank 1
	bsf	PIE2,CCP2IE
;
; setup data ports
	movlb                  0                      ;bank 0
	MOVLW	PortAValue
	MOVWF	PORTA	;Init PORTA
	movlb                  1                      ;bank 1
	MOVLW	PortADDRBits
	MOVWF	TRISA
;
	movlb                  0                      ;bank 0
	MOVLW	PortBValue
	MOVWF	PORTB	;Init PORTA
	movlb                  1                      ;bank 1
	MOVLW	PortBDDRBits
	MOVWF	TRISB
	movlb	0	;bank 0
;
;Configure LEDs
	MOVLW	LEDTIME
	MOVWF	LED_Time
	movwf	LED2_PeriodTime
	movwf	LED3_PeriodTime
	movlw	LEDFlashTime
	movwf	LED2_FlashTime
	movwf	LED3_FlashTime
	clrf	LED2_Flashes	;LEDs off
	clrf	LED3_Flashes
;
	call	CopyToRam
;
	bsf	INTCON,PEIE	; enable periferal interupts
	bsf	INTCON,GIE	; enable interupts
;
	call	Init_ADC
;
;=========================================================================================
;=========================================================================================
;  Test battery voltage and servo current before Main Loop
;
	movlw	0x01
	movwf	State	;Start it
;
	movlb	2	;bank 2
	bsf	ContEnable1	;Disable continuity S1
	bsf	ContEnable2	;Disable continuity S2
	movlb	0	;bank 0
;
;==============================================
; Make servos active and move to Position 1
;
	movlb	5
	bsf	S1ServoOff
	bsf	S2ServoOff
;
	call	S1StartServo
;
	call	S2StartServo
;
	call	SetLED2_OFF
	call	SetLED3_OFF
;
;=========================================================================================
;
MainLoop	movlb	0	;Bank 0
;
	movf	State,F
	SKPZ		;State=0?
	bra	ML_SkipBtns
;
; Handle Inc/Dec buttons
	movf	Timer4Lo,W
	iorwf	Timer4Hi,W
	SKPZ		;Timer4 == 0?
	bra	ML_Btns_End	; No
;
	btfsc	S1Active	;Servo 1 is active?
	bra	ML_ChkBtns	; Yes
;
	btfss	S2Active	;Servo 2 is active?
	bra	ML_Btns_End	; No, neither one is active
;
ML_ChkBtns	btfss	IncBtnFlag	;Inc button is down?
	bra	ML_Btns_Dec	; No
;
	btfss	DecBtnFlag	;Dec button is down?
	bra	ML_Btns_Inc	; No, only the Inc btn is down
;
	call	MoveActiveToCenter	;Both buttons are down
	bra	ML_Btns_End
;
;	
; Handle INC button
ML_Btns_Inc	call	CopyPosToTemp
	movlw	BtnChangeRate
	addwf	Param7C,F
	movlw	0x00
	addwfc	Param7D,F
	call	ClampInt
	call	CopyTempToPos
	bsf	DataChangedFlag
;
	movlw	0x05	; 0.05 seconds
	movwf	Timer4Lo
	bra	ML_Btns_End
;
;-------------------------
;
ML_Btns_Dec	btfss	DecBtnFlag	;Dec button is down?
	bra	ML_Btns_Save	; No
;
; Handle DEC button
	call	CopyPosToTemp
	movlw	BtnChangeRate
	subwf	Param7C,F
	movlw	0x00
	subwfb	Param7D,F
	call	ClampInt
	call	CopyTempToPos
	bsf	DataChangedFlag
;
	movlw	0x05	; 0.05 seconds
	movwf	Timer4Lo
	bra	ML_Btns_End

ML_Btns_Save	btfsc	DataChangedFlag
	call	SaveParams
	bcf	DataChangedFlag
ML_Btns_End:
;
	call	ConfigLEDs
;
;-------------------------
;
ML_SkipBtns	movf	State,F
	SKPZ		;State=0?
	call	DoStateN	; No
;
	movf	State,F
	SKPNZ		;State=0?
	call	SetDestServo	; Yes
;
	call	MoveServo1
;
	call	MoveServo2
;
	goto	MainLoop
;
;-------------------------
; Handle State>0
DoStateN	movf	State2,W
	SKPZ
	call	HandleState2
;
	movlw	1
	subwf	State,W
	SKPZ		;State=1?
	bra	TryState2	; No
;
;State 1, Start 1s Blink
	movlw	.100
	movwf	LED_Blinks
	movlw	0x01	;constant ON
	movwf	LED_Time
;State1_End
	bra	DoNextState
;-----------------------------
TryState2	movlw	2
	subwf	State,W
	SKPZ		;State=2?
	bra	TryState3	; No
;
;State 2, Wait for 1s Blink to finish, Start 1s wait
	movf	LED_Blinks,W
	SKPZ		;Done?
State2_Waiting	return		; No
;
; Wait for servos to be in position and done w/ move time
;
	btfss	S1InPosition
	bra	State2_Waiting
	btfss	S2InPosition
	bra	State2_Waiting
	movf	Servo1MoveTimerLo,W
	iorwf	Servo1MoveTimerHi,W
	SKPZ
	bra	State2_Waiting
	movf	Servo2MoveTimerLo,W
	iorwf	Servo2MoveTimerHi,W
	SKPZ
	bra	State2_Waiting
;
	movlw	.100	; Yes, Wail 1s
	movwf	Timer1Lo
	clrf	Timer1Hi
;State2_End
; It has been 3s, servos should be at position 1, get servo idle current
;
	bra	DoNextState
;----------------------------
TryState3	movlw	3
	subwf	State,W
	SKPZ		;State=3?
	bra	TryState4	; No
;
;State 3, Wait for timer, Start Batt Volt Blinks
	movf	Timer1Lo,W
	iorwf	Timer1Hi,W
	SKPZ		;Timer1=0?
	return		; No
;
	call	BlinkBattVolts	;Read battery voltage
;
	btfss	BattV_OK	;Batt volts OK?
	bra	State3_BlinkError	; No
; Battery voltage is OK
;
	movlw	1
	movwf	State2	;Start servo current test
;
;State3_End
	bra	DoNextState
;
ErrorTime	EQU	.500	;5 seconds
;
; Blink error for 5 seconds
State3_BlinkError	movlw	LEDErrorTime
	MOVWF	LED_Time
	movlw	0xFF
	movwf	LED_Blinks
;
	movlw	low ErrorTime
	movwf	Timer1Lo
	movlw	high ErrorTime
	movwf	Timer1Hi
	movlw	0x07
	movwf	State
	return
;
;------------------------------
TryState4	movlw	4
	subwf	State,W
	SKPZ		;State=4?
	bra	TryState5	; No
;
;State 4, Wait for Blinks to finish, start 1s deley
	movf	LED_Blinks,W
	SKPZ		;LED_Blinks=0?
	return		; No
;
	movlw	.100	; Yes, Wail 1s
	movwf	Timer1Lo
	clrf	Timer1Hi		
;State4_End	
	bra	DoNextState
;
;----------------------------
TryState5	movlw	5
	subwf	State,W
	SKPZ		;State=5?
	bra	TryState6	; No
;
;State 5 Wait for 1s Delay to end, Start 1s Blink
	movf	Timer1Lo,W
	iorwf	Timer1Hi,W
	SKPZ		;Timer1=0?
	return		; No
;
	movlw	.100
	movwf	LED_Blinks
	movlw	0x01	;constant ON
	movwf	LED_Time
;State5_End
	bra	DoNextState
;
;-----------------------------
TryState6	movlw	6
	subwf	State,W
	SKPZ		;State=6?
	bra	TryState7	; No
;
;State 6 Wait for 1s Blink to end, Start 1s delay
	movf	LED_Blinks,W
	SKPZ		;LED_Blinks=0?
	return		; No
;
	movlw	.100	; Yes, Wail 1s
	movwf	Timer1Lo
	clrf	Timer1Hi		
;State6_End	
	bra	DoNextState
;
;----------------------------
TryState7	movlw	7
	subwf	State,W
	SKPZ		;State=7?
	bra	TryState8	; No
;
;State 7 Wait for 1s Delay to end
	movf	Timer1Lo,W
	iorwf	Timer1Hi,W
	SKPZ		;Timer1=0?
	return		; No
;
; Wait for State2=0
	movf	State2,W
	SKPZ
	return
;
;State7_End
; normal system blinks
	MOVLW	LEDTIME
	MOVWF	LED_Time
	movlw	0xFF
	movwf	LED_Blinks
;
; Last State=LastState+1 so we're done
TryState8:
LastState	clrf	State
;
	btfss	BattV_OK	;Batt volts OK?
	bra	State_8LowVolts	; No
;
	btfss	S1Found
	bra	State_8NoS1
	movlb	2	;bank 2
	bcf	ContEnable1	;Enable continuity
	movlb	0	;bank 0
;
State_8NoS1	btfss	S2Found
	bra	State_8NoS2
	movlb	2	;bank 2
	bcf	ContEnable2	;Enable continuity
	movlb	0	;bank 0
State_8NoS2:
State_8LowVolts	return
;
DoNextState	incf	State,F	
	return
;
;========================================================================================
; Servo current testing
;  Wiggle the servos while looking at the current
;
SmallServoMove	EQU	.100
MinPeakCurrent	EQU	.20
;
;
HandleState2	movlw	1
	subwf	State2,W
	SKPZ		;State2=1?
	bra	HandleState2_2	; No
;
	call	GetIdleCurrent
	call	ClearPeakCurrent
;
; Move Servo1 to Position1+100
	movlw	SmallServoMove
	addwf	S1Position1,W
	movwf	Param7C
	clrw
	addwfc	S1Position1+1,W
	movwf	Param7D
;
ClampAndGoS1	call	ClampInt
	movf	Param7C,W
	movwf	S1Dest
	movf	Param7D,W
	movwf	S1Dest+1
;
	bcf	S1InPosition
	bcf	S1CurPosIsDest
;
DoNextState2	incf	State2,F
	return
;
;-------------------------------
HandleState2_2	movlw	2
	subwf	State2,W
	SKPZ		;State2=2?
	bra	HandleState2_3	; No
;
	call	GetPeakCurrect
;
; wait for servo to finish moving
	btfss	S1CurPosIsDest	;Move done?
	return		; No
;
; Move Servo1 to Position1-100
	movlw	SmallServoMove
	subwf	S1Position1,W
	movwf	Param7C
	clrw
	subwfb	S1Position1+1,W
	movwf	Param7D
;	
	bra	ClampAndGoS1
;------------------------------
HandleState2_3	movlw	3
	subwf	State2,W
	SKPZ		;State2=3?
	bra	HandleState2_4	; No
;
	call	GetPeakCurrect
;
; wait for servo to finish moving
	btfss	S1CurPosIsDest	;Move done?
	return		; No
;
; Move Servo1 to Position1
	movf	S1Position1,W
	movwf	S1Dest
	movf	S1Position1+1,W
	movwf	S1Dest+1
	bcf	S1InPosition
	bcf	S1CurPosIsDest
;
	bra	DoNextState2
;----------------------------
HandleState2_4	movlw	4
	subwf	State2,W
	SKPZ		;State2=4?
	bra	HandleState2_5	; No
;
	call	GetPeakCurrect
;
; wait for servo to finish moving
	btfss	S1InPosition	;Move done and servo off?
	return		; No
;
; Is peak current > minimum (10)?
	movlb	1	;bank 1
	movf	PeakCurrent+1,W
	SKPZ		;Current>255?
	bra	HandleState2_4_1
	movlw	MinPeakCurrent
	subwf	PeakCurrent,W
	SKPB		;Current<MinPeakCurrent?
	bra	HandleState2_4_1	; No, Current>=MinPeakCurrent
	movlb	0	;bank 0, Yes, no servo found
	bra	DoNextState2
;
HandleState2_4_1	movlb	0	;bank 0
	bsf	S1Found	;Servo is attached
	bra	DoNextState2
;----------------------------
HandleState2_5	movlw	5
	subwf	State2,W
	SKPZ		;State2=5?
	bra	HandleState2_6	; No
;
	call	ClearPeakCurrent
; Move Servo2 to Position1+100
	movlw	SmallServoMove
	addwf	S2Position1,W
	movwf	Param7C
	clrw
	addwfc	S2Position1+1,W
	movwf	Param7D
;
ClampAndGoS2	call	ClampInt
	movf	Param7C,W
	movwf	S2Dest
	movf	Param7D,W
	movwf	S2Dest+1
;
	bcf	S2InPosition
	bcf	S2CurPosIsDest
;
DoNextState2S2	incf	State2,F
	return
;
;----------------------------
HandleState2_6	movlw	6
	subwf	State2,W
	SKPZ		;State2=6?
	bra	HandleState2_7	; No
;
	call	GetPeakCurrect
;
; wait for servo to finish moving
	btfss	S2CurPosIsDest	;Move done?
	return		; No
;
; Move Servo1 to Position1-100
	movlw	SmallServoMove
	subwf	S2Position1,W
	movwf	Param7C
	clrw
	subwfb	S2Position1+1,W
	movwf	Param7D
;	
	bra	ClampAndGoS2
;----------------------------
HandleState2_7	movlw	7
	subwf	State2,W
	SKPZ		;State2=7?
	bra	HandleState2_8	; No
;
	call	GetPeakCurrect
;
; wait for servo to finish moving
	btfss	S2CurPosIsDest	;Move done?
	return		; No
;
; Move Servo1 to Position1
	movf	S2Position1,W
	movwf	S2Dest
	movf	S2Position1+1,W
	movwf	S2Dest+1
	bcf	S2InPosition
	bcf	S2CurPosIsDest
;
	bra	DoNextState2S2
;----------------------------
HandleState2_8	movlw	8
	subwf	State2,W
	SKPZ		;State2=8?
	bra	HandleState2_9	; No
;
	call	GetPeakCurrect
;
; wait for servo to finish moving
	btfss	S2InPosition	;Move done and servo off?
	return		; No
;
; Is peak current > minimum (10)?
	movlb	1	;bank 1
	movf	PeakCurrent+1,W
	SKPZ		;Current>255?
	bra	HandleState2_8_1
	movlw	MinPeakCurrent
	subwf	PeakCurrent,W
	SKPB		;Current<MinPeakCurrent?
	bra	HandleState2_8_1	; No, Current>=MinPeakCurrent
	movlb	0	;bank 0, Yes, no servo found
	bra	DoNextState2S2
;
HandleState2_8_1	movlb	0	;bank 0
	bsf	S2Found	;Servo is attached
	bra	DoNextState2S2
;
;---------------------------
HandleState2_9:
; We're done
	clrf	State2
	return
;
;========================================================================================
;----------------------------------------------------------------------------------------
;========================================================================================
;
BlinkBattVolts	movlb	0	;bank 0
	bcf	BattV_OK
; Get batt ADC
	movlb	1	;bank 1
	bsf	AN0_Semaphore
	movf	AN0_DataL,W
	movwf	Param7C
	movf	AN0_DataH,W
	movwf	Param7D
	bcf	AN0_Semaphore
	movlb	0	;bank 0
;
	movlw	MinBattVolts
	subwf	Param7C,W	;ADC-MinBattVolts
	movlw	high MinBattVolts
	subwfb	Param7D,W
	SKPB		:ADC<MinBattVolts?
	bsf	BattV_OK	; No
;
; devide by 32, should be 35.8
	lsrf	Param7D,F
	rrf	Param7C,F
	lsrf	Param7D,F
	rrf	Param7C,F
	lsrf	Param7D,F
	rrf	Param7C,F
	lsrf	Param7D,F
	rrf	Param7C,F
	lsrf	Param7D,F
	rrf	Param7C,F
;
	movlw	.50	; 1/2 second
	movwf	LED_Time
	movf	Param7C,W
	movwf	LED_Blinks	; Blink battery voltage, range 6..11
	return
;
;
;========================================================================================
;
ConfigLEDs	movlb	0	;bank 0
; 3 states: Off=no servo detected, 1 flash=Pos1, On=Pos2 
;if pos2 set LED ON
	btfss	LED2Flag
	bra	ML_LED2_NotOn
	call	SetLED2_ON
	bra	ML_LED2_End
;
ML_LED2_NotOn	btfsc	S1Found
	bra	ML_LED2_Flash
	call	SetLED2_OFF
	bra	ML_LED2_End
;
ML_LED2_Flash	movlw	1
	call	SetLED2_W_Flashes
ML_LED2_End:
;
; if pos2 set LED ON
	btfss	LED3Flag
	bra	ML_LED3_NotOn
	call	SetLED3_ON
	bra	ML_LED3_End
;
ML_LED3_NotOn	btfsc	S2Found
	bra	ML_LED3_Flash
	call	SetLED3_OFF
	bra	ML_LED3_End
;
ML_LED3_Flash	movlw	1
	call	SetLED3_W_Flashes
ML_LED3_End:
	return
;
;----------------------------------------------------------------------------------------
; Set routines for LED2/LED3
SetLED2_OFF	movlw	0
;
SetLED2_W_Flashes	movlb	0	;bank0
	andlw	0x03	;Limit to 0..3 flashes
	movwf	LED2_Flashes
	movlw	LEDTIME	;1s
	movwf	LED2_PeriodTime
	movlw	LEDFlashTime	;0.1s
	movwf	LED2_FlashTime
	return
;
SetLED2_ON	movlb	0	;bank 0
	movlw	0x01
	movwf	LED2_Flashes
	movwf	LED2_PeriodTime
	movwf	LED2_FlashTime
	return
;
SetLED3_OFF	movlw	0
;
SetLED3_W_Flashes	movlb	0	;bank0
	andlw	0x03	;Limit to 0..3 flashes
	movwf	LED3_Flashes
	movlw	LEDTIME
	movwf	LED3_PeriodTime
	movlw	LEDFlashTime
	movwf	LED3_FlashTime
	return
;
SetLED3_ON	movlb	0	;bank 0
	movlw	0x01
	movwf	LED3_Flashes
	movwf	LED3_PeriodTime
	movwf	LED3_FlashTime
	return
;
;========================================================================================
; Move Servo1 CurPos toward Dest
;
MoveServo1	movlb	0	;Bank 0
	btfss	S1PulseSent
	bra	MS1_End
	bcf	S1PulseSent
;
	movf	S1Dest,W
	subwf	S1CurPos,W
	movwf	Param78
	movf	S1Dest+1,W
	subwfb	S1CurPos+1,W
	iorwf	Param78,F
	SKPZ		;Dest == CurPos?
	bra	MS1_1	; No
	bsf	S1CurPosIsDest
	movf	Servo1MoveTimerLo,W
	iorwf	Servo1MoveTimerHi,W
	SKPNZ
	bsf	S1InPosition	
	bra	MS1_Move_It_Now	; Yes
;
MS1_1	movlw	Low ServoMoveTime
	movwf	Servo1MoveTimerLo
	movlw	High ServoMoveTime
	movwf	Servo1MoveTimerHi
	bcf	S1InPosition
;
	movlb	0	;Bank 0
	movf	S1Dest,W
	movwf	Param78
	movf	S1Dest+1,W
	movwf	Param79
;
	movf	S1CurPos,W
	movwf	Param7C
	movf	S1CurPos+1,W
	movwf	Param7D
;
	call	Param7D_LE_Param79
	btfss	Param77,0	;CurPos<=Dest?
	bra	MS1_Move_It_Neg	; No, CurPos>Dest
;CurPos<Dest, so move CurPos positive
	movlw	SlewChangeRate
	addwf	Param7C,F
	movlw	0x00
	addwfc	Param7D,F
	call	Param7D_LE_Param79
	btfss	Param77,0	;CurPos<=Dest Still?
	bra	MS1_Move_It_Dest	; No, CurPos>Dest
			; Yes, CurPos+=SlewChangeRate
;
; make the calculated position the current position
MS1_Move_It_New	movf	Param7C,W
	movwf	S1CurPos
	movf	Param7D,W
	movwf	S1CurPos+1
	bra	MS1_Move_It_Now
;
MS1_Move_It_Neg:
	movlw	SlewChangeRate
	subwf	Param7C,F
	movlw	0x00
	subwfb	Param7D,F
	call	Param7D_LE_Param79
	btfsc	Param77,0	;CurPos<=Dest now?
	bra	MS1_Move_It_Dest	; Yes, CurPos<=Dest
	bra	MS1_Move_It_New
;
; make the current position the destination
MS1_Move_It_Dest	movf	S1Dest,W
	movwf	S1CurPos
	movf	S1Dest+1,W
	movwf	S1CurPos+1
;
MS1_Move_It_Now:
	movlb	0	;Bank0
	movf	S1CurPos,W
	movwf	Param7C
	movf	S1CurPos+1,W
	movwf	Param7D
	goto	S1Copy7CToSig
;
MS1_End	return
;
;========================================================================================
; Move Servo1 CurPos toward Dest
;
MoveServo2	movlb	0	;Bank 0
	btfss	S2PulseSent
	bra	MS2_End
	bcf	S2PulseSent
;
	movf	S2Dest,W
	subwf	S2CurPos,W
	movwf	Param78
	movf	S2Dest+1,W
	subwfb	S2CurPos+1,W
	iorwf	Param78,F
	SKPZ		;Dest == CurPos?
	bra	MS2_1
	bsf	S2CurPosIsDest
	movf	Servo2MoveTimerLo,W
	iorwf	Servo2MoveTimerHi,W
	SKPNZ
	bsf	S2InPosition
	bra	MS2_Move_It_Now	; Yes
;
MS2_1	movlw	Low ServoMoveTime
	movwf	Servo2MoveTimerLo
	movlw	High ServoMoveTime
	movwf	Servo2MoveTimerHi
	bcf	S2InPosition
;
	movlb	0	;Bank 0
	movf	S2Dest,W
	movwf	Param78
	movf	S2Dest+1,W
	movwf	Param79
;
	movf	S2CurPos,W
	movwf	Param7C
	movf	S2CurPos+1,W
	movwf	Param7D
;
	call	Param7D_LE_Param79
	btfss	Param77,0	;CurPos<=Dest?
	bra	MS2_Move_It_Neg	; No, CurPos>Dest
;CurPos<Dest, so move CurPos positive
	movlw	SlewChangeRate
	addwf	Param7C,F
	movlw	0x00
	addwfc	Param7D,F
	call	Param7D_LE_Param79
	btfss	Param77,0	;CurPos<=Dest Still?
	bra	MS2_Move_It_Dest	; No, CurPos>Dest
			; Yes, CurPos+=SlewChangeRate
;
; make the calculated position the current position
MS2_Move_It_New	movf	Param7C,W
	movwf	S2CurPos
	movf	Param7D,W
	movwf	S2CurPos+1
	bra	MS2_Move_It_Now
;
MS2_Move_It_Neg:
	movlw	SlewChangeRate
	subwf	Param7C,F
	movlw	0x00
	subwfb	Param7D,F
	call	Param7D_LE_Param79
	btfsc	Param77,0	;CurPos<=Dest now?
	bra	MS2_Move_It_Dest	; Yes, CurPos<=Dest
	bra	MS2_Move_It_New
;
; make the current position the destination
MS2_Move_It_Dest	movf	S2Dest,W
	movwf	S2CurPos
	movf	S2Dest+1,W
	movwf	S2CurPos+1
;
MS2_Move_It_Now:
	movlb	0	;Bank0
	movf	S2CurPos,W
	movwf	Param7C
	movf	S2CurPos+1,W
	movwf	Param7D
	goto	S2Copy7CToSig
;
MS2_End	return
;
;========================================================================================
; Set Dest for Servo 1
;
; w/ 50mS debounce
;  If SnCmdInputBit is active set SnSMRevFlag else clear SnSMRevFlag
;
SetDestServo	movlb	0	;bank 0
	btfss	NewSWData	;10mS interval passed?
	return		; No, IRQ sets bit 100 times per second
	bcf	NewSWData
;
	btfsc	S1CmdInputBit	;Contorl signal active?
	bra	SDS1_CmdNormal	; No
;
; debounce, don't change until we've seen the input 5 times, 0.05 seconds
	movlw	0x05
	subwf	S1CmdDebounce,W
	SKPZ		;5 times?
	bra	SDS1_Rev_Debounce	; No
;
	movlb	5	;Bank5
	bsf	S1SMRevFlag	; move to position 2
	movlb	0	;Bank0
	bsf	LED2Flag
	bsf	S1Active	;Buttons are active for servo1
	bcf	S2Active
	movlw	Low ActivationTime
	movwf	Servo1ActiveTimerLo
	movlw	High ActivationTime
	movwf	Servo1ActiveTimerHi
	bra	SDS1_Move_It
;
SDS1_Rev_Debounce	incf	S1CmdDebounce,F
	bra	SDS1_End
;
SDS1_CmdNormal	movf	S1CmdDebounce,F
	SKPZ
	bra	SDS1_Norm_Debounce
;
	movlb	0	;Bank 0
	movf	Servo1ActiveTimerLo,W
	iorwf	Servo1ActiveTimerHi,W
	SKPZ		;Activation time done?
	bra	SDS1_Move_It	; No
;
	if HoldOpen=0
	movlb	5	;Bank5
	bcf	S1SMRevFlag	; Set position 1 as Dest
	movlb	0	;Bank0
	endif
;
	bcf	LED2Flag
	bra	SDS1_Move_It
;
SDS1_Norm_Debounce	decf	S1CmdDebounce,F
	bra	SDS1_End
;
SDS1_Move_It	call	S1CopyPosToDest
;
SDS1_End:
;
;===========================================
; Set Dest for Servo 2
;
; w/ 50mS debounce
;  If SnCmdInputBit is active set SnSMRevFlag else clear SnSMRevFlag
;
;
	btfsc	S2CmdInputBit	;Contorl signal active?
	bra	SDS2_CmdNormal	; No
;
; debounce, don't change until we've seen the input 5 times
	movlw	0x05
	subwf	S2CmdDebounce,W
	SKPZ		;5 times?
	bra	SDS2_Rev_Debounce	; No
;
	movlb	5	;Bank5
	bsf	S2SMRevFlag
	movlb	0	;Bank0
	bsf	LED3Flag
	bsf	S2Active	;Buttons are active for servo2
	bcf	S1Active
	movlw	Low ActivationTime
	movwf	Servo2ActiveTimerLo
	movlw	High ActivationTime
	movwf	Servo2ActiveTimerHi
	bra	SDS2_Move_It
;
SDS2_Rev_Debounce	incf	S2CmdDebounce,F
	bra	SDS2_End
;
SDS2_CmdNormal	movf	S2CmdDebounce,F
	SKPZ
	bra	SDS2_Norm_Debounce
;
	movlb	0	;Bank 0
	movf	Servo2ActiveTimerLo,W
	iorwf	Servo2ActiveTimerHi,W
	SKPZ		;Activation time done?
	bra	SDS2_Move_It	; No
;
	movlb	5	;Bank5
	bcf	S2SMRevFlag
	movlb	0	;Bank0
;
	bcf	LED3Flag
	bra	SDS2_Move_It
;
SDS2_Norm_Debounce	decf	S2CmdDebounce,F
	bra	SDS2_End
;
SDS2_Move_It	goto	S2CopyPosToDest
;
SDS2_End	return
;
;========================================================================================
; Handle both buttons, move to center
;
MoveActiveToCenter	movlw	low kMidPulseWidth
	movwf	Param7C
	movlw	high kMidPulseWidth
	movwf	Param7D
	call	CopyTempToDest
	bcf	DataChangedFlag
	return
;
;========================================================================================
; Param7D:Param7C >> S1SigOutTimeH:S1SigOutTime
; Entry: Param7D:Param7C, any bank
; Exit: Bank0
; Calls: none
;
; Don't disable interrupts if you don't need to...
; If Param7D:Param7C == S1SigOutTimeH:S1SigOutTime then return
;
S1Copy7CToSig	movlb	5	;Bank5
	MOVF	Param7C,W
	SUBWF	S1SigOutTime,W
	SKPZ
	bra	S1Copy7CToSig_1
	MOVF	Param7D,W
	SUBWF	S1SigOutTimeH,W
	movlb	0	;Bank0
	SKPNZ		;Param7D:Param7D = S1SigOutTime?
	return		; Yes
;
;SigOutTimeH:SigOutTime = Param7D:Param7C
S1Copy7CToSig_1	bcf	INTCON,GIE
	btfsc	INTCON,GIE
	bra	S1Copy7CToSig_1
;
	movlb	5	;Bank5
	MOVF	Param7C,W
	MOVWF	S1SigOutTime
	MOVF	Param7D,W
	MOVWF	S1SigOutTimeH
	bsf	INTCON,GIE
;
	movlb	0	;Bank0
	return
;
;========================================================================================
; Param7D:Param7C >> S2SigOutTimeH:S2SigOutTime
; Entry: Param7D:Param7C, any bank
; Exit: Bank0
; Calls: none
;
; Don't disable interrupts if you don't need to...
; If Param7D:Param7C == S2SigOutTimeH:S2SigOutTime then return
;
S2Copy7CToSig	movlb	5	;Bank5
	MOVF	Param7C,W
	SUBWF	S2SigOutTime,W
	SKPZ
	bra	S2Copy7CToSig_1
	MOVF	Param7D,W
	SUBWF	S2SigOutTimeH,W
	movlb	0	;Bank0
	SKPNZ
	return
;
;SigOutTimeH:SigOutTime = Param7D:Param7C
S2Copy7CToSig_1	bcf	INTCON,GIE
	btfsc	INTCON,GIE
	bra	S2Copy7CToSig_1
;
	movlb	5	;Bank5
	MOVF	Param7C,W
	MOVWF	S2SigOutTime
	MOVF	Param7D,W
	MOVWF	S2SigOutTimeH
	bsf	INTCON,GIE
;
	movlb	0	;Bank0
	return
;
;=========================================================================
; Copy Position to Destination for Servo 1
; 
S1CopyPosToDest	movlb	5	;Bank5
	btfsc	S1SMRevFlag
	bra	S1CopyPos2ToDest
;
S1CopyPos1ToDest	movlb	0	;Bank0
	movf	S1Position1+1,W
	movwf	S1Dest+1
	movf	S1Position1,W
	movwf	S1Dest
	return
;
S1CopyPos2ToDest	movlb	0	;Bank0
	movf	S1Position2+1,W
	movwf	S1Dest+1
	movf	S1Position2,W
	movwf	S1Dest
	return
;
;=====================================
; Copy Temp (Param7C/Param7D) to Destination for any Servo 
; Servo 1 has priority
;
CopyTempToDest	movlb	0	;Bank 0
	btfsc	S1Active
	bra	S1CopyTempToDest
	btfsc	S2Active
	bra	S2CopyTempToDest
	return
;
;=====================================
; Copy Temp (Param7C/Param7D) to Destination for Servo 1
;
S1CopyTempToDest	movlb	0	;Bank0
	movf	Param7D,W
	movwf	S1Dest+1
	movf	Param7C,W
	movwf	S1Dest
	return
;
;=====================================
; Copy Temp (Param7C/Param7D) to Destination for Servo 2
;
S2CopyTempToDest	movlb	0	;Bank0
	movf	Param7D,W
	movwf	S2Dest+1
	movf	Param7C,W
	movwf	S2Dest
	return
;
;====================================
; Copy Position to Temp (Param7c/Param7D) for any Servo
;
CopyPosToTemp	btfsc	S1Active
	bra	S1CopyPosToTemp
	btfsc	S2Active
	bra	S2CopyPosToTemp
	return
;
;====================================
; Copy Position to Temp (Param7c/Param7D) for Servo 1
;
S1CopyPosToTemp	movlb	5	;Bank5
	btfsc	S1SMRevFlag
	bra	S1CopyPos2ToTemp
;
S1CopyPos1ToTemp	movlb	0	;Bank0
	movf	S1Position1+1,W
	movwf	Param7D
	movf	S1Position1,W
	movwf	Param7C
	return
;
S1CopyPos2ToTemp	movlb	0	;Bank0
	movf	S1Position2+1,W
	movwf	Param7D
	movf	S1Position2,W
	movwf	Param7C
	return
;
;=====================================
; Copy Position to Temp (Param7c/Param7D) for Servo 2
;
S2CopyPosToTemp	movlb	5	;Bank5
	btfsc	S2SMRevFlag
	bra	S2CopyPos2ToTemp
;
;
S2CopyPos1ToTemp	movlb	0	;Bank0
	movf	S2Position1+1,W
	movwf	Param7D
	movf	S2Position1,W
	movwf	Param7C
	return
;
S2CopyPos2ToTemp	movlb	0	;Bank0
	movf	S2Position2+1,W
	movwf	Param7D
	movf	S2Position2,W
	movwf	Param7C
	return
;
;=====================================
; Copy Temp (Param7c/Param7D) to Position for any Servo
;
CopyTempToPos	btfsc	S1Active
	bra	S1CopyTempToPos
	btfsc	S2Active
	bra	S2CopyTempToPos
	return
;
;=====================================
; Copy Temp (Param7c/Param7D) to Position for Servo 1
;
S1CopyTempToPos	movlb	5	;Bank5
	btfsc	S1SMRevFlag
	bra	S1CopyTempToPos2
;
S1CopyTempToPos1	movlb	0	;Bank0
	movf	Param7D,W
	movwf	S1Position1+1
	movf	Param7C,W
	movwf	S1Position1
	return
;
S1CopyTempToPos2	movlb	0	;Bank0
	movf	Param7D,W
	movwf	S1Position2+1
	movf	Param7C,W
	movwf	S1Position2
	return
;
;====================================
; Copy Temp (Param7c/Param7D) to Position for Servo 2
;
S2CopyTempToPos	movlb	5	;Bank5
	btfsc	S2SMRevFlag
	bra	S2CopyTempToPos2
;
S2CopyTempToPos1	movlb	0	;Bank0
	movf	Param7D,W
	movwf	S2Position1+1
	movf	Param7C,W
	movwf	S2Position1
	return
;
S2CopyTempToPos2	movlb	0	;Bank0
	movf	Param7D,W
	movwf	S2Position2+1
	movf	Param7C,W
	movwf	S2Position2
	return
;
;=========================================================================
; Copy Position to Destination for Servo 2
;
S2CopyPosToDest	movlb	5	;Bank5
	btfsc	S2SMRevFlag
	bra	S2CopyPos2ToDest
;
S2CopyPos1ToDest	movlb	0	;Bank0
	movf	S2Position1+1,W
	movwf	S2Dest+1
	movf	S2Position1,W
	movwf	S2Dest
	return
;
;
S2CopyPos2ToDest	movlb	0	;Bank0
	movf	S2Position2+1,W
	movwf	S2Dest+1
	movf	S2Position2,W
	movwf	S2Dest
	return
;
;=========================================================================================
;=========================================================================================
; 
GetIdleCurrent	movlb	1	;Bank 1
	bsf	AN1_Semaphore
	movf	AN1_DataL,W
	movwf	IDleCurrent
	movf	AN1_DataH,W
	movwf	IDleCurrent+1
	bcf	AN1_Semaphore
	movlb	0	;Bank 0
	return
;
;=========================================================================================
;
ClearPeakCurrent	movlb	1	;Bank 1
	clrf	PeakCurrent
	clrf	PeakCurrent+1
	movlb	0	;Bank 0
	return
;
;=========================================================================================
; if AN1_Data-IDleCurrent>PeakCurrent then PeakCurrent=AN1_Data-IDleCurrent
;
GetPeakCurrect	movlb	1	;Bank 1
	bsf	AN1_Semaphore
;
;Param7C:Param7D := AN1_Data - IDleCurrent
	movf	IDleCurrent,W
	subwf	AN1_DataL,W
	movwf	Param7C
	movf	IDleCurrent+1,W
	subwfb	AN1_DataH,W
	movwf	Param7D
	btfsc	Param7D,7	;Is negative, current < IdleCurrent?
	bra	GetPeakCurrect_1	; Yes, current is less than idle current
;
	movf	Param7C,W
	subwf	PeakCurrent,W
	movf	Param7D,W
	subwfb	PeakCurrent+1,W
	SKPB		;New > Old?
	bra	GetPeakCurrect_1	; No
;
; PeakCurrent := new value
	movf	Param7C,W
	movwf	PeakCurrent
	movf	Param7D,W
	movwf	PeakCurrent+1
;
GetPeakCurrect_1	bcf	AN1_Semaphore
	movlb	0	;Bank 0
	return
;	
;=========================================================================================
; Read AN0, Must call ReadAN0_ColdStart first
; Exit: 10bit analog in 7D:7C
;
ADCON1Value	equ	b'10100000'	;Right Just, Fosc/32, Vss, Vdd
ADCON1_AN0	equ	b'00000001'	;AN0, ADC Enabled
ADCON1_AN1	equ	b'00000101'	;AN1, ADC Enabled
;
Init_ADC	movlb	1	;bank 1
	bsf	PIE1,ADIE
	movlb	0	;bank 0
	bcf	PIR1,ADIF
;
;------------------------
;
	movlb	1	;bank 1
	MOVLW	ADCON1Value
	MOVWF	ADCON1
	MOVLW	ADCON1_AN0	;Select AN0
	MOVWF	ADCON0
	nop		;4uS delay
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	BSF	ADCON0,ADGO
	movlb	0	;bank 0
	Return
;
;---------------------------
; Interupt service routine
;
IRQ_ADC	movlb	0	;bank 0
	bcf	PIR1,ADIF
;
	movlb	1	;bank 1
	btfsc	ADCON0,2	;AN0 is active?
	bra	IRQ_ADC_AN1	; No, go handle AN1
;
; AN0 just finished
	MOVF	ADRESH,W
	MOVWF	AN0_RawH
	MOVF	ADRESL,W
	MOVWF	AN0_RawL
	bsf	AN0_NewRawData
	movlw	ADCON1_AN1	;Sample and start conversion of AN1
	movwf	ADCON0
	bra	SampleTimer

;
; AN1 just finished
IRQ_ADC_AN1	MOVF	ADRESH,W
	MOVWF	AN1_RawH
	MOVF	ADRESL,W
	MOVWF	AN1_RawL
	bsf	AN1_NewRawData
	movlw	ADCON1_AN0	;Sample and start conversion of AN0
	movwf	ADCON0
;
SampleTimer	nop		;4uS delay
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	bsf	ADCON0,ADGO
;
UpdateANData	movlb	1	;bank 1
	btfsc	AN0_Semaphore
	bra	UpdateANData_1
	btfss	AN0_NewRawData
	bra	UpdateANData_1
	movf	AN0_RawL,W
	movwf	AN0_DataL
	movf	AN0_RawH,W
	movwf	AN0_DataH
	bcf	AN0_NewRawData
	bsf	AN0_NewData
;
UpdateANData_1	btfsc	AN1_Semaphore
	bra	UpdateANData_End
	btfss	AN1_NewRawData
	bra	UpdateANData_End
	movf	AN1_RawL,W
	movwf	AN1_DataL
	movf	AN1_RawH,W
	movwf	AN1_DataH
	bcf	AN1_NewRawData
	bsf	AN1_NewData
;	
UpdateANData_End:
;
	goto	IRQ_ADC_Return
;
;=========================================================================================
;=========================================================================================
; Set CCP1 to go high in 0x100 clocks
;
S1StartServo	movlb	5	;bank 5
	BTFSS	S1ServoOff	;Servo is active?
	bra	S1StartServo_End	; yes, don't start it again
	BCF	S1ServoOff
;
; Initialize to normal position
	bcf	S1SMRevFlag	;forward
	movlb	0	;Bank 0
	bcf	LED2Flag
;
	call	S1CopyPosToDest
	call	S1SetDestAsCur
	call	S1Copy7CToSig
;
	movlb	0	;Bank 0	
	movlw	.100	;start in 0x100 clocks
	addwf	TMR1L,W
	movlb	0x05                   ;Bank 5
	movwf	CCPR1L
	movlb	0	;Bank 0	
	movlw	0
	addwfc	TMR1H,W
	movlb	0x05                   ;Bank 5
	movwf	CCPR1H
;
	MOVLW	CCPnCON_Set
	MOVWF	CCP1CON	;go high on match
	movlb	0	;Bank 0
	movlw	Low ServoMoveTime	;At power up move to commanded position
	movwf	Servo1MoveTimerLo
	movlw	High ServoMoveTime
	movwf	Servo1MoveTimerHi
	bcf	S1InPosition
;
S1StartServo_End	movlb	0x00	;Bank 0
	RETURN
;
;=========================================================================================
; Set CCP1 to go high in 0x100 clocks
;
S2StartServo	movlb	5	;bank 5
	BTFSS	S2ServoOff	;Servo is active?
	bra	S2StartServo_End	; yes, don't start it again
	BCF	S2ServoOff
;
; Initialize to normal position
	bcf	S2SMRevFlag
	movlb	0	;Bank 0
	bcf	LED3Flag
;
	call	S2CopyPosToDest
	call	S2SetDestAsCur
	call	S2Copy7CToSig
;
	movlb	0	;Bank 0	
	movlw	.100	;start in 0x100 clocks
	addwf	TMR1L,W
	movlb	0x05                   ;Bank 5
	movwf	CCPR2L
	movlb	0	;Bank 0	
	movlw	0
	addwfc	TMR1H,W
	movlb	0x05                   ;Bank 5
	movwf	CCPR2H
;
	MOVLW	CCPnCON_Set
	MOVWF	CCP2CON	;go high on match
	movlb	0x00	;Bank 0
	movlw	Low ServoMoveTime	;At power up move to commanded position
	movwf	Servo2MoveTimerLo
	movlw	High ServoMoveTime
	movwf	Servo2MoveTimerHi
	bcf	S2InPosition
;
S2StartServo_End	movlb	0x00	;Bank 0
	RETURN
;
;=========================================================================================
;
S1SetMiddlePosition	movlb	0	;bank 0
	MOVLW	LOW kMidPulseWidth
	MOVWF	Param7C
	movwf	S1Dest
	movwf	S1CurPos
	MOVLW	HIGH kMidPulseWidth
	MOVWF	Param7D
	movwf	S1Dest+1
	movwf	S1CurPos+1
	Return
;
;=========================================================================================
;
S2SetMiddlePosition	movlb	0	;bank 0
	MOVLW	LOW kMidPulseWidth
	MOVWF	Param7C
	movwf	S2Dest
	movwf	S2CurPos
	MOVLW	HIGH kMidPulseWidth
	MOVWF	Param7D
	movwf	S2Dest+1
	movwf	S2CurPos+1
	Return
;
;=========================================================================================
;
S1SetDestAsCur	movlb	0	;bank 0
	movf	S1Dest,W
	MOVWF	Param7C
	movwf	S1CurPos
	movf	S1Dest+1,W
	MOVWF	Param7D
	movwf	S1CurPos+1
	Return
;
;=========================================================================================
;
S2SetDestAsCur	movlb	0	;bank 0
	movf	S2Dest,W
	MOVWF	Param7C
	movwf	S2CurPos
	movf	S2Dest+1,W
	MOVWF	Param7D
	movwf	S2CurPos+1
	Return
;
;=========================================================================================
; ClampInt(Param7D:Param7C,kMinPulseWidth,kMaxPulseWidth)
;
; Entry: Param7D:Param7C
; Exit: Param7D:Param7C=ClampInt(Param7D:Param7C,kMinPulseWidth,kMaxPulseWidth)
;
ClampInt	MOVLW	high kMaxPulseWidth
	SUBWF	Param7D,W	;7D-kMaxPulseWidth
	SKPNB		;7D<Max?
	bra	ClampInt_1	; Yes
	SKPZ		;7D=Max?
	bra	ClampInt_tooHigh	; No, its greater.
	MOVLW	low kMaxPulseWidth	; Yes, MSB was equal check LSB
	SUBWF	Param7C,W	;7C-kMaxPulseWidth
	SKPNZ		;=kMaxPulseWidth
	RETURN		;Yes
	SKPB		;7C<Max?
	bra	ClampInt_tooHigh	; No
	RETURN		; Yes
;
ClampInt_1	MOVLW	high kMinPulseWidth
	SUBWF	Param7D,W	;7D-kMinPulseWidth
	SKPNB		;7D<Min?
	bra	ClampInt_tooLow	; Yes
	SKPZ		;=Min?
	RETURN		; No, 7D>kMinPulseWidth
	MOVLW	low kMinPulseWidth	; Yes, MSB is a match
	SUBWF	Param7C,W	;7C-kMinPulseWidth
	SKPB		;7C>=Min?
	RETURN		; Yes
;
ClampInt_tooLow	MOVLW	low kMinPulseWidth
	MOVWF	Param7C
	MOVLW	high kMinPulseWidth
	MOVWF	Param7D
	RETURN
;
ClampInt_tooHigh	MOVLW	low kMaxPulseWidth
	MOVWF	Param7C
	MOVLW	high kMaxPulseWidth
	MOVWF	Param7D
	RETURN
;
;=====================================================================================
; Less or Equal
;
; Entry: Param7D:Param7C, Param79:Param78
; Exit: Param77:0=Param7D:Param7C<=Param79:Param78
;
Param7D_LE_Param79	CLRF	Param77	;default to >
	MOVF	Param79,W
	SUBWF	Param7D,W	;Param7D-Param79
	SKPNB		;Param7D<Param79?
	bra	SetTrue	; Yes
	SKPZ		;Param7D>Param79?
	RETURN		; Yes
	MOVF	Param78,W	; No, MSB is a match
	SUBWF	Param7C,W	;Param7C-Param78
	SKPNB		;Param7C<Param78?
	bra	SetTrue	; Yes
	SKPZ		;LSBs then same?
	RETURN		; No
;
SetTrue	BSF	Param77,0
	RETURN
;
	if oldCode
;=========================================================================================
;=====================================================================================
;
MoveTo78	MOVWF	FSR0L
	MOVF	INDF0,W
	MOVWF	Param78
	INCF	FSR0L,F
	MOVF	INDF0,W
	MOVWF	Param79
	RETURN
;
;=====================================================================================
;
MoveTo7C	MOVWF	FSR0L
	MOVF	INDF0,W
	MOVWF	Param7C
	INCF	FSR0L,F
	MOVF	INDF0,W
	MOVWF	Param7D
	RETURN
;
;=====================================================================================
;
Move78To7C	MOVF	Param78,W
	MOVWF	Param7C
	MOVF	Param79,W
	MOVWF	Param7D
	RETURN
;
;=====================================================================================
;
MoveFrom7C	MOVWF	FSR0L
	MOVF	Param7C,W
	MOVWF	INDF0
	INCF	FSR0L,F
	MOVF	Param7D,W
	MOVWF	INDF0
	RETURN
;
;=====================================================================================
; Greater or Equal
;
; Entry: Param7D:Param7C, Param79:Param78
; Exit: Param77:0=Param7D:Param7C>=Param79:Param78
;
Param7D_GE_Param79	CLRF	Param77	;default to <
	MOVF	Param79,W
	SUBWF	Param7D,W	;Param7D-Param79
	SKPNB		;Param7D<Param79?
	RETURN		; Yes
	SKPZ		;Param7D>Param79?
	goto	SetTrue	; Yes
Param7D_GE_Param79_1	MOVF	Param78,W	; No, MSB is a match
	SUBWF	Param7C,W	;Param7C-Param78
	SKPNB		;Param7C<Param78?
	RETURN		; Yes
	goto	SetTrue	; No
;
;======================================================================================
;
EqualMin	CLRF	Param77
	MOVLW	high kMinPulseWidth
	SUBWF	Param7D,W
	SKPZ
	RETURN
	MOVLW	low kMinPulseWidth
	SUBWF	Param7C,W
	SKPNZ
	BSF	Param77,0
	RETURN

;
Subtract1000	MOVLW	low kMinPulseWidth
	SUBWF	Param7C,F
	SUBBF	Param7D,F
	MOVLW	high kMinPulseWidth
	SUBWF	Param7D,F
	RETURN
;
Subtract1500	MOVLW	low d'1500'
	SUBWF	Param7C,F
	SUBBF	Param7D,F
	MOVLW	high d'1500'
	SUBWF	Param7D,F
	RETURN
;
X2	CLRC
	RLF	Param7C,F
	RLF	Param7D,F
	RETURN
;
Add1000	MOVLW	low kMinPulseWidth
	ADDWF	Param7C,F
	ADDCF	Param7D,F
	MOVLW	high kMinPulseWidth
	ADDWF	Param7D,F
	RETURN
;
	endif
;=============================================================================================
;==============================================================================================
;
	include	F1847_Common.inc
;
;=========================================================================================
;=========================================================================================
;
;
;
;
	END
;
