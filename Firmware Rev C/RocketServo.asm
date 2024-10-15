;====================================================================================================
;
;   Filename:	RocketServo.asm
;   Date:	10/14/2024
;   File Version:	1.2b1
;
;    Author:	David M. Flynn
;    Company:	Oxford V.U.E., Inc.
;    E-Mail:	dflynn@oxfordvue.com
;    Web Site:	http://www.oxfordvue.com/
;
;====================================================================================================
;    RC Servo Activator for high power rocketry uses PIC12F1822
;
;    History:
; 1.2b1   10/14/2024	Updated for Rev C PCBA, added Continuity enable, low battery fast blink
; 1.1b3   8/6/2023	Added BP3Hold mode.
; 1.1b2   5/4/2023	Fixed batt V blink, 10 or 11 blinks.
; 1.1b1   12/28/2022	Added battery voltage sensing RA0 9.75C/V 3 for diode
; 1.0b2   11/7/2022	Inverted CmdInputBit to active low
; 1.0b1   8/31/2022    Copied from SM_Ctrl_RC_Servo.
;
;====================================================================================================
; Options
HasEnable	EQU	1
HasBattV	EQU	1	;Set to 0 or 1
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
;   Do not arm if <7.5 NiMH, fast blink for 10 seconds
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
;   CCP1 outputs a 900uS to 2100uS (1800..4200 counts) pulse every 16,000uS
;
;   Resolution is 0.5uS
;
;  if kInPosShutdown is set servo will power down after 3s
;
;====================================================================================================
;
;  Pin 1 VDD (+5V)		+5V
;  Pin 2 RA5		CCP1
;  Pin 3 RA4		System LED Active Low/SW2 Dec switch Active Low
;  Pin 4 RA3/MCLR*/Vpp (Input only)	Command = Active Low
;  Pin 5 RA2		LED 2 Active Low/SW1 Inc switch Active Low
;  Pin 6 RA1/ICSPCLK		RA1, Conductivity Enable Active Low
;  Pin 7 RA0/ICSPDAT		AN0, Battery (volts-0.5)/21
;  Pin 8 VSS (Ground)		Ground
;
;====================================================================================================
;
;
	list	p=12f1822,r=hex,w=1	; list directive to define processor
;
	nolist
	include	p12f1822.inc	; processor specific variable definitions
	list
;
	__CONFIG _CONFIG1,_FOSC_INTOSC & _WDTE_OFF & _MCLRE_OFF & _IESO_OFF
;
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
	__CONFIG _CONFIG2,_WRT_OFF & _PLLEN_OFF & _LVP_OFF
;
; Write protection off
; 4x PLL disabled
; Stack Overflow or Underflow will cause a Reset
; Brown-out Reset Voltage (Vbor), low trip point selected.
; Low-voltage programming Disabled ( allow MCLR to be digital in )
;  *** must set apply Vdd before Vpp in ICD 3 settings ***
;
; '__CONFIG' directive is used to embed configuration data within .asm file.
; The lables following the directive are located in the respective .inc file.
; See respective data sheet for additional information on configuration word.
;
	constant	oldCode=0
	constant	useRS232=0
;
#Define	_C	STATUS,C
#Define	_Z	STATUS,Z
;
; 0.5uS res counter from 8MHz OSC
CCP1CON_Clr	EQU	b'00001001'	;Clear output on match
CCP1CON_Set	EQU	b'00001000'	;Set output on match
CCP1CON_Int	EQU	b'00001010'
;
MinBattVolts	EQU	d'65'	;V/20 * 204c/v = 6.5V
;
ActivationTime	EQU	d'300'	;3 seconds
kOffsetCtrValue	EQU	d'2047'
kMinPulseWidth	EQU	d'1800'	;900uS
kMidPulseWidth	EQU	d'3000'	;1500uS
kMaxPulseWidth	EQU	d'4200'	;2100uS
	if BP3Hold
kDefaultPosition1	EQU	d'2000'	;1000uS Locked
kDefaultPosition2	EQU	d'4110'	;2055uS Open
HoldOpen	EQU	1
	else
kDefaultPosition1	EQU	d'3600'	;1800
kDefaultPosition2	EQU	d'2200'	;1100
HoldOpen	EQU	0
	endif
kServoDwellTime	EQU	d'32000'	;16mS
kInPosShutdown	EQU	b'00000010'	;b'00000010' enabled 0x00 disabled
;
;====================================================================================================
	nolist
	include	F1822_Macros.inc
	list
;
;    Port A bits
PortADDRBits	EQU	b'11011101'
#Define	ContEnable	LATA,1	;Output: 0=Enabled
#Define	LED2	LATA,2	;Output: 0=LED ON, Input: 0 = Switch Pressed
#Define	LED2TrisBit	TRISA,2
#Define	IncBtnBit	PORTA,2
#Define	CmdInputBit	PORTA,3
#Define	SystemLED	LATA,4	;Output: 0=LED ON, Input: 0 = Switch Pressed
#Define	SysLEDTrisBit	TRISA,4
#Define	DecBtnBit	PORTA,4
#Define	RA5_In	PORTA,5	;CCP1
;
PortAValue	EQU	b'00000010'
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
;
T1CON_Val	EQU	b'00000001'	;PreScale=1,Fosc/4,Timer ON
;T1CON_Val	EQU	b'00100001'	;PreScale=4,Fosc/4,Timer ON
OSCCON_Value	EQU	b'01110010'	;8MHz
;OSCCON_Value	EQU	b'11110000'	;32MHz
T2CON_Value	EQU	b'01001110'	;T2 On, /16 pre, /10 post
;T2CON_Value	EQU	b'01001111'	;T2 On, /64 pre, /10 post
PR2_Value	EQU	.125
;
;
CCP1CON_Value	EQU	0x00	;CCP1 off
;
;================================================================================================
;***** VARIABLE DEFINITIONS
; there are 128 bytes of ram, Bank0 0x20..0x7F, Bank1 0xA0..0xBF
; there are 256 bytes of EEPROM starting at 0x00 the EEPROM is not mapped into memory but
;  accessed through the EEADR and EEDATA registers
;================================================================================================
;  Bank0 Ram 020h-06Fh 80 Bytes
;
	cblock	0x20
;
	ISR_Temp		;scratch mem
	LED_Time
	LED2_Time
	LED_Ticks		;Timer tick count
	LED2_Ticks
	LED_Blinks		;nBlinks, 0xFF=system blink, 0=Off
;
;
	EEAddrTemp		;EEProm address to read or write
	EEDataTemp		;Data to be writen to EEProm
;
;
	Timer1Lo		;1st 16 bit timer
	Timer1Hi		; not used
	Timer2Lo		;2nd 16 bit timer
	Timer2Hi		; Servo active timer
	Timer3Lo		;3rd 16 bit timer
	Timer3Hi		; Activation timer
	Timer4Lo		;4th 16 bit timer
	Timer4Hi		; debounce timer and power up delay timer
;
	Dest:2
	CurPos:2
;
	Position1:2
	Position2:2
	SysFlags
;
	Debounce
;
	endc
;
#Define	PulseSent	SysFlags,0
#Define	InPosShutdown	SysFlags,1
#Define	InPosition	SysFlags,2
#Define	BattV_OK	SysFlags,3
;
#Define	FirstRAMParam	Position1
#Define	LastRAMParam	SysFlags
;
;================================================================================================
;  Bank1 Ram 0A0h-0BFh 32 Bytes
;
;
;=======================================================================================================
;  Common Ram 70-7F same for all banks
;      except for ISR_W_Temp these are used for paramiter passing and temp vars
;=======================================================================================================
;
	cblock	0x70
	SigOutTime
	SigOutTimeH
	Flags
;
	Param73
	Param74
;
	CalcdDwell
	CalcdDwellH
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
; Flags bits
#Define	IncBtnFlag	Flags,0
#Define	DecBtnFlag	Flags,1
#Define	LED2Flag	Flags,2
#Define	DataChangedFlag	Flags,3
#Define	ServoOff	Flags,4
#Define	SMRevFlag	Flags,5
#Define	NewSWData	Flags,6
;
ServoMoveTime	EQU	.300	;time servo is powered when CMD changes
;
BtnChangeRate	EQU	0x02	;change by 1uS per 0.05 seconds
SlewChangeRate	EQU	0x40	;change by 8uS per 0.01 seconds
;
;=========================================================================================
;Conditionals
HasISR	EQU	0x80	;used to enable interupts 0x80=true 0x00=false
;
;=========================================================================================
;==============================================================================================
; ID Locations
	__idlocs	0x12B1
;
;==============================================================================================
; EEPROM locations (NV-RAM) 0x00..0x7F (offsets)
	org	0xF000
;
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
;
;-----------------------------------------------------------------
; blink LEDs
	movlb                  1                      ;bank 1
	BSF	SysLEDTrisBit	;LED off
	BSF	LED2TrisBit	;LED2 off
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
	SKPNZ
	BRA	SystemBlink_end
	incfsz	LED_Blinks,W
	BRA	SystemBlink_Blinking
	BRA	SystemBlink_on
SystemBlink_Blinking	decf	LED_Blinks,F
SystemBlink_on	movlb                  1                      ;bank 1
	BCF	SysLEDTrisBit	;LED on
SystemBlink_end	MOVLB	0                      ;bank 0
;
	decfsz	LED2_Ticks,F
	bra	LED2_End
;
	MOVF	LED2_Time,W
	movwf	LED2_Ticks
;
	BTFSS	LED2Flag
	bra                    LED2_End
	movlb                  1                      ;bank 1
	BCF	LED2TrisBit
;
LED2_End	MOVLB	0
;-----------------------------------------------------------------
;
IRQ_2:
;==================================================================================
;
; Handle CCP1 Interupt Flag, Enter w/ bank 0 selected
;
IRQ_Servo1	MOVLB	0	;bank 0
	BTFSS	PIR1,CCP1IF
	GOTO	IRQ_Servo1_End
;
	BSF	PulseSent
	BTFSS	ServoOff	;Are we sending a pulse?
	GOTO	IRQ_Servo1_1	; Yes
;
;Oops, how did we get here???
	MOVLB	0x05                   ;Bank 5
	CLRF	CCP1CON
	GOTO	IRQ_Servo1_X
;
IRQ_Servo1_1	MOVLB	0x05                   ;Bank 5
	BTFSC	CCP1CON,CCP1M0	;Set output on match?
	GOTO	IRQ_Servo1_OL	; No
; An output just went high
;
	MOVF	SigOutTime,W	;Put the pulse into the CCP reg.
	ADDWF	CCPR1L,F
	MOVF	SigOutTime+1,W
	ADDWFC	CCPR1H,F
	movlb	0	;Bank 0
	movlw	CCP1CON_Int
	btfss	InPosShutdown
	MOVLW	CCP1CON_Clr	;Clear output on match
	btfss	InPosition
	MOVLW	CCP1CON_Clr	;Clear output on match
	movlb	5	;Bank 5
	MOVWF	CCP1CON	;CCP1 clr on match
;Calculate dwell time
	MOVLW	LOW kServoDwellTime
	MOVWF	CalcdDwell
	MOVLW	HIGH kServoDwellTime
	MOVWF	CalcdDwellH
	MOVF	SigOutTime,W
	SUBWF	CalcdDwell,F
	MOVF	SigOutTime+1,W
	SUBWFB	CalcdDwellH,F
	GOTO	IRQ_Servo1_X
;
; output went low so this cycle is done
IRQ_Servo1_OL	MOVLW	LOW kServoDwellTime
	ADDWF	CCPR1L,F
	MOVLW	HIGH kServoDwellTime
	ADDWFC	CCPR1H,F
;
	movlb	0	;Bank 0
	movlw	CCP1CON_Int
	btfss	InPosShutdown
	MOVLW	CCP1CON_Set	;Set output on match
	btfss	InPosition
	MOVLW	CCP1CON_Set	;Set output on match
	movlb	5	;Bank 5
	MOVWF	CCP1CON	;Idle output low
;
IRQ_Servo1_X	MOVLB	0x00                   ;Bank 0
	BCF	PIR1,CCP1IF
IRQ_Servo1_End:
;--------------------------------------------------------------------
;
	retfie		; return from interrupt
;
;
;==============================================================================================
;**********************************************************************************************
;==============================================================================================
;
start	MOVLB	0x01	; select bank 1
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
	MOVLB	0x03	; bank 3
;
	if HasBattV
	MOVLW	0x01
	else
	MOVLW	0x00
	endif
;
	MOVWF	ANSELA	;AN0 only
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
	MOVLB	1	;Bank 1
	BSF	PIE1,TMR2IE
	movlb	0                      ;Bank 0
;
; setup ccp1
;
	BSF	ServoOff
	movlb	2                      ;bank 2
	BSF	APFCON,CCP1SEL	;RA5
	movlb	5                      ;bank 5
	CLRF	CCP1CON
;
	MOVLB	0x01	;Bank 1
	bsf	PIE1,CCP1IE
;
; setup data ports
	movlb                  0                      ;bank 0
	MOVLW	PortAValue
	MOVWF	PORTA	;Init PORTA
	movlb                  1                      ;bank 1
	MOVLW	PortADDRBits
	MOVWF	TRISA
;
	MOVLB	0	;bank 0
	CLRWDT
;
	MOVLW	LEDTIME
	MOVWF	LED_Time
	movlw	0x01	;continuos ON
	movwf	LED2_Time
;
	CLRWDT
	CALL	CopyToRam
	CLRWDT
;
;
	bsf	INTCON,PEIE	; enable periferal interupts
;	bsf	INTCON,T0IE	; enable TMR0 interupt
	bsf	INTCON,GIE	; enable interupts
;
	MOVLB	0x00	;bank 0
	movlw	0x04
	movwf	Timer4Lo	;power up delay
;
;=========================================================================================
;=========================================================================================
;  Main Loop
;
	if HasBattV
	CALL	ReadAN0_ColdStart
	CALL	LongFlash
;
	CALL	ReadAN0
	CALL	ReadAN0
;
;	if HasEnable
	MOVLB	2	;bank 2
	bsf	ContEnable	;Disable continuity
	MOVLB	0	;bank 0
	bcf	BattV_OK
	movf	Param7D,F	;error is not zero
	SKPZ
	bra	BVBad
	movlw	MinBattVolts
	subwf	Param7C,W	;ADC-MinBattVolts
	SKPNB
	bra	BVBad
	bsf	BattV_OK
BVBad	
;	endif
;
; devide by 8
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
	movwf	LED_Blinks
;
; Wait for blinks to finish
Start_L1	CLRWDT
	movf	LED_Blinks,W
	SKPZ
	BRA	Start_L1
;
	CALL	Delay_1S
;
	CALL	LongFlash
;
	endif
;
	btfss	BattV_OK
	bra	BlinkError
	MOVLB	2	;bank 2
	bcf	ContEnable	;Enable continuity
	MOVLB	0	;bank 0
	bra	Start_1
;
BlinkError	movlw	LEDErrorTime
	MOVWF	LED_Time
	movlw	0xFF
	movwf	LED_Blinks
	bra	Start_Err
;
; normal system blinks
Start_1	MOVLW	LEDTIME
	MOVWF	LED_Time
	movlw	0xFF
	movwf	LED_Blinks
;
Start_Err	CALL	StartServo
	CALL	HomeServo
	
	BRA	MainLoop
;
;==============================================
; One second ON one second OFF
LongFlash	movlw	.100
	movwf	LED_Blinks
	movlw	0x01	;constant ON
	movwf	LED_Time
LongFlash_L1	movf	LED_Blinks,W
	SKPZ
	BRA	LongFlash_L1
;
Delay_1S	movlw	.100
	movwf	Timer1Lo
Delay_L1	movf	Timer1Lo,W
	SKPZ
	bra	Delay_L1
	CLRWDT
	return
;
;==============================================
HomeServo	movlb	0	;Bank0
	bcf	SMRevFlag
	bcf	LED2Flag
	movlw	Low ActivationTime
	movwf	Timer3Lo
	movlw	High ActivationTime
	movwf	Timer3Hi
	return
;
;=========================================================================================
;
MainLoop	CLRWDT
	MOVLB	0x00                   ;Bank 0
;
; Handle Inc/Dec buttons
	movf	Timer4Lo,W
	iorwf	Timer4Hi,W
	SKPZ		;Timer4 == 0?
	bra	ML_Btns_End	; No
	btfss	IncBtnFlag	;Inc button is down?
	bra	ML_Btns_Dec	; No
;
	btfss	DecBtnFlag	;Dec button is down?
	bra	ML_Btns_Inc	; No
; Handle both buttons, move to center
	movlw	low kMidPulseWidth
	movwf	Dest
	movlw	high kMidPulseWidth
	movwf	Dest+1
	bcf	DataChangedFlag
	bra	Set_Dest_End
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
;-------------------------
; Set Dest
;
	btfss	NewSWData	;10mS interval passed?
	bra	Set_Dest_End	; No
	bcf	NewSWData
;
	btfsc	CmdInputBit	;Contorl signal active?
	bra	ML_CmdNormal	; No
; debounce, don't change until we've seen the input 5 times
	movlw	0x05
	subwf	Debounce,W
	SKPZ		;5 times?
	bra	Rev_Debounce	; No
;
	movlb	0	;Bank0
	bsf	SMRevFlag
	bsf	LED2Flag
	movlw	Low ActivationTime
	movwf	Timer3Lo
	movlw	High ActivationTime
	movwf	Timer3Hi
	bra	Move_It
;
Rev_Debounce	incf	Debounce,F
	bra	Set_Dest_End
;
ML_CmdNormal	movf	Debounce,F
	SKPZ
	bra	Norm_Debounce
;
	movlb	0	;Bank 0
	movf	Timer3Lo,W
	iorwf	Timer3Hi,W
	SKPZ		;Activation time done?
	bra	Move_It	; No
	if HoldOpen=0
	bcf	SMRevFlag
	endif
	bcf	LED2Flag
	bra	Move_It
;
Norm_Debounce	decf	Debounce,F
	bra	Set_Dest_End
;
Move_It	call	CopyPosToDest
;
Set_Dest_End:
;
;---------------------
; Move CurPos toward Dest
	movlb	0	;Bank 0
	btfss	PulseSent
	bra	Move_End
	bcf	PulseSent
;
	movf	Dest,W
	subwf	CurPos,W
	movwf	Param78
	movf	Dest+1,W
	subwfb	CurPos+1,W
	iorwf	Param78,F
	SKPZ		;Dest == CurPos?
	bra	Move_It_NIP
	movf	Timer2Lo,W
	iorwf	Timer2Hi,W
	SKPNZ
	bsf	InPosition
	bra	Move_It_Now	; Yes
;
Move_It_NIP	movlw	Low ServoMoveTime
	movwf	Timer2Lo
	movlw	High ServoMoveTime
	movwf	Timer2Hi
	bcf	InPosition
;
	movf	Dest,W
	movwf	Param78
	movf	Dest+1,W
	movwf	Param79
;
	movf	CurPos,W
	movwf	Param7C
	movf	CurPos+1,W
	movwf	Param7D
;
	call	Param7D_LE_Param79
	btfss	Param77,0	;CurPos<=Dest?
	bra	Move_It_Neg	; No, CurPos>Dest
;CurPos<Dest, so move CurPos positive
	movlw	SlewChangeRate
	addwf	Param7C,F
	movlw	0x00
	addwfc	Param7D,F
	call	Param7D_LE_Param79
	btfss	Param77,0	;CurPos<=Dest Still?
	bra	Move_It_Dest	; No, CurPos>Dest
			; Yes, CurPos+=SlewChangeRate
;
; make the calculated position the current position
Move_It_New	movf	Param7C,W
	movwf	CurPos
	movf	Param7D,W
	movwf	CurPos+1
	bra	Move_It_Now
;
Move_It_Neg
	movlw	SlewChangeRate
	subwf	Param7C,F
	movlw	0x00
	subwfb	Param7D,F
	call	Param7D_LE_Param79
	btfsc	Param77,0	;CurPos<=Dest now?
	bra	Move_It_Dest	; Yes, CurPos<=Dest
	bra	Move_It_New
;
; make the current position the destination
Move_It_Dest	movf	Dest,W
	movwf	CurPos
	movf	Dest+1,W
	movwf	CurPos+1
;
Move_It_Now:
	movf	CurPos,W
	movwf	Param7C
	movf	CurPos+1,W
	movwf	Param7D
	CALL	Copy7CToSig
Move_End:
;
	goto	MainLoop
;
;========================================================================================
; Param7D:Param7C >> SigOutTimeH:SigOutTime
; Entry: Param7D:Param7C
;
; Don't disable interrupts if you don't need to...
; If Param7D:Param7C == SigOutTimeH:SigOutTime then return
Copy7CToSig	MOVF	Param7C,W
	SUBWF	SigOutTime,W
	SKPZ
	GOTO	Copy7CToSig_1
	MOVF	Param7D,W
	SUBWF	SigOutTimeH,W
	SKPNZ
	Return
;
;SigOutTimeH:SigOutTime = Param7D:Param7C
Copy7CToSig_1	bcf	INTCON,GIE
	btfsc	INTCON,GIE
	bra	Copy7CToSig_1
	MOVF	Param7C,W
	MOVWF	SigOutTime
	MOVF	Param7D,W
	MOVWF	SigOutTimeH
	bsf	INTCON,GIE
;
	RETURN
;
;=========================================================================
;
CopyTempToPos	btfss	SMRevFlag
	bra	CopyTempToPos1
	bra	CopyTempToPos2
;
CopyPosToTemp	btfss	SMRevFlag
	bra	CopyPos1ToTemp
	bra	CopyPos2ToTemp
;
CopyPosToDest	btfss	SMRevFlag
	bra	CopyPos1ToDest
	bra	CopyPos2ToDest
;
;====================================
;
CopyPos1ToDest	movf	Position1+1,W
	movwf	Dest+1
	movf	Position1,W
	movwf	Dest
	return
;
;=====================================
;
CopyPos2ToDest	movf	Position2+1,W
	movwf	Dest+1
	movf	Position2,W
	movwf	Dest
	return
;
;====================================
;
CopyPos1ToTemp	movf	Position1+1,W
	movwf	Param7D
	movf	Position1,W
	movwf	Param7C
	return
;
;=====================================
;
CopyPos2ToTemp	movf	Position2+1,W
	movwf	Param7D
	movf	Position2,W
	movwf	Param7C
	return
;
;=====================================
;
CopyTempToPos1	movf	Param7D,W
	movwf	Position1+1
	movf	Param7C,W
	movwf	Position1
	return
;
CopyTempToPos2	movf	Param7D,W
	movwf	Position2+1
	movf	Param7C,W
	movwf	Position2
	return
;
;=========================================================================================
;=========================================================================================
; Setup or Read AN0
; Exit: 10bit analog in 7D:7C
;
ReadAN0	CLRF	Param7C
	CLRF	Param7D
	MOVLB	1	;bank 1
	BTFSS	ADCON0,ADON	;Is the Analog input ON?
	GOTO	ReadAN0_ColdStart	; No, go start it
ReadAN0_L1	BTFSC	ADCON0,NOT_DONE	;Conversion done?
	BRA	ReadAN0_L1	; No
	MOVF	ADRESH,W
	MOVWF	Param7D
	MOVF	ADRESL,W
	MOVWF	Param7C
	BSF	ADCON0,ADGO	;Start next conversion.
	MOVLB	0	;bank 0
	RETURN
;
ReadAN0_ColdStart	MOVLB	1	;bank 1
	MOVLW	0xA0	;Right Just fosc/32
	MOVWF	ADCON1
	MOVLW	b'00000001'	;Select AN0
	MOVWF	ADCON0
	BSF	ADCON0,GO
ReadAN0_Rtn	MOVLB	0	;bank 0
	Return
;
;=========================================================================================
;=========================================================================================
; Set CCP1 to go high is 0x100 clocks
;
StartServo	MOVLB	0	;bank 0
	BTFSS	ServoOff
	RETURN
	BCF	ServoOff
;
SS_Start_Loop	movf	Timer4Lo,F
	SKPZ
	bra	SS_Start_Loop
;
; Initialize to normal position
SS_CmdNormal	movlb	0	;Bank 0
	bcf	SMRevFlag
	bcf	LED2Flag
;
SS_Move_It	call	CopyPosToDest
	call	SetDestAsCur
	CALL	Copy7CToSig
;
	MOVLW	0x00	;start in 0x100 clocks
	MOVWF	TMR1L
	MOVLW	0xFF
	MOVWF	TMR1H
;
	MOVLB	0x05                   ;Bank 5
	CLRF	CCPR1H
	CLRF	CCPR1L
	MOVLW	CCP1CON_Set
	MOVWF	CCP1CON	;go high on match
	MOVLB	0x00	;Bank 0
	movlw	low .300	;Do nothing for 3 seconds
	movwf	Timer4Lo
	movlw	high .300
	movwf	Timer4Hi
	movlw	Low ServoMoveTime	;At power up move to commanded position
	movwf	Timer2Lo
	movlw	High ServoMoveTime
	movwf	Timer2Hi
	bcf	InPosition
	RETURN
;
;=========================================================================================
;
SetMiddlePosition	MOVLW	LOW kMidPulseWidth
	MOVWF	Param7C
	movwf	Dest
	movwf	CurPos
	MOVLW	HIGH kMidPulseWidth
	MOVWF	Param7D
	movwf	Dest+1
	movwf	CurPos+1
	Return
;
;=========================================================================================
;
SetDestAsCur	movf	Dest,W
	MOVWF	Param7C
	movwf	CurPos
	movf	Dest+1,W
	MOVWF	Param7D
	movwf	CurPos+1
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
	GOTO	ClampInt_1	; Yes
	SKPZ		;7D=Max?
	GOTO	ClampInt_tooHigh	; No, its greater.
	MOVLW	low kMaxPulseWidth	; Yes, MSB was equal check LSB
	SUBWF	Param7C,W	;7C-kMaxPulseWidth
	SKPNZ		;=kMaxPulseWidth
	RETURN		;Yes
	SKPB		;7C<Max?
	GOTO	ClampInt_tooHigh	; No
	RETURN		; Yes
;
ClampInt_1	MOVLW	high kMinPulseWidth
	SUBWF	Param7D,W	;7D-kMinPulseWidth
	SKPNB		;7D<Min?
	GOTO	ClampInt_tooLow	; Yes
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
	GOTO	SetTrue	; Yes
	SKPZ		;Param7D>Param79?
	RETURN		; Yes
	MOVF	Param78,W	; No, MSB is a match
	SUBWF	Param7C,W	;Param7C-Param78
	SKPNB		;Param7C<Param78?
	GOTO	SetTrue	; Yes
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
	GOTO	SetTrue	; Yes
Param7D_GE_Param79_1	MOVF	Param78,W	; No, MSB is a match
	SUBWF	Param7C,W	;Param7C-Param78
	SKPNB		;Param7C<Param78?
	RETURN		; Yes
	GOTO	SetTrue	; No
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
	include	F1822_Common.inc
;
;=========================================================================================
;=========================================================================================
;
;
;
;
	END
;
