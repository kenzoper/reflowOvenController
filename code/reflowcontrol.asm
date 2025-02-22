; 76E003 ADC test program: Reads channel 7 on P1.1, pin 14
; This version uses the LM4040 voltage reference connected to pin 6 (P1.7/AIN0)

$NOLIST
$MODN76E003
$LIST

;  N76E003 pinout:
;                               -------
;       PWM2/IC6/T0/AIN4/P0.5 -|1    20|- P0.4/AIN5/STADC/PWM3/IC3
;               TXD/AIN3/P0.6 -|2    19|- P0.3/PWM5/IC5/AIN6
;               RXD/AIN2/P0.7 -|3    18|- P0.2/ICPCK/OCDCK/RXD_1/[SCL]
;                    RST/P2.0 -|4    17|- P0.1/PWM4/IC4/MISO
;        INT0/OSCIN/AIN1/P3.0 -|5    16|- P0.0/PWM3/IC3/MOSI/T1
;              INT1/AIN0/P1.7 -|6    15|- P1.0/PWM2/IC2/SPCLK
;                         GND -|7    14|- P1.1/PWM1/IC1/AIN7/CLO
;[SDA]/TXD_1/ICPDA/OCDDA/P1.6 -|8    13|- P1.2/PWM0/IC0
;                         VDD -|9    12|- P1.3/SCL/[STADC]
;            PWM5/IC7/SS/P1.5 -|10   11|- P1.4/SDA/FB/PWM1
;                               -------
;

CLK               EQU 16600000 ; Microcontroller system frequency in Hz
BAUD              EQU 115200 ; Baud rate of UART in bps
TIMER1_RELOAD     EQU (0x100-(CLK/(16*BAUD)))
TIMER0_RELOAD_1MS EQU (0x10000-(CLK/1000))
TIMER2_RATE   EQU 1000                  ; Timer2 tick rate (1 ms per tick)
TIMER2_RELOAD EQU (65536 - (CLK/TIMER2_RATE))  ; Reload value for Timer2

PWM_PERIOD_STEPS    EQU 1000

;---------------------------------------------------
; Reset and Interrupt Vectors
;---------------------------------------------------
org 0x0000
    ljmp main

org 0x002B                 ; Timer2 interrupt vector address
    ljmp Timer2_ISR

;                      1234567890123456    <- This helps determine the location of the counter
state_display:     db 'S', 0
value_message:     db 'T=', 0
state_time_display:db 'ST', 0
run_time_display:  db 'RT', 0
message_temp_refl: db 'tr       tir  '   ,0
cold_message	:  db 'TJ:',0
message_time_soak: db 'ts       tis'	,0

cseg
; These 'equ' must match the hardware wiring
LCD_RS equ P1.3
LCD_E  equ P1.4
LCD_D4 equ P0.0
LCD_D5 equ P0.1
LCD_D6 equ P0.2
LCD_D7 equ P0.3

PWM_OUT             EQU P1.0   
SOUND_OUT		    EQU P0.4         


$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$LIST

; These register definitions needed by 'math32.inc'
DSEG at 30H
x:   ds 4
y:   ds 4
bcd: ds 5
VAL_LM4040: ds 2


;state macahine vars
state: ds 1       ; current state (0)
temp_soak: ds 1   ; temperature soak parameter (150 C)
Time_soak: ds 1   ; time soak parameter (60 seconds)
Temp_refl: ds 1   ; reflow temperature parameter (220 C)
Time_refl: ds 1   ; reflow time parameter (45 seconds)
sec: ds 1         ; seconds (0 seconds)   
temp: ds 1        ; temperature (~20-25 C) - should be around room temperature
FSM1_state: ds 1  ; current state in FSM 1 (0)
FSM2_state: ds 1  ; current state in FSM 2 (0)
pwm : ds 2
pwm_counter:       ds 2              

;timer variables
State_time: 		  ds 1
Count1ms:         ds 2    ; 16-bit millisecond counter
Seconds_counter:  ds 1    ; Seconds counter (BCD: 00 to 59)
total_runtime:    ds 1    ; runtime counter
safety_time:	  ds 1    ; timer bleh
BSEG

mf: dbit 1

;button init
PB6: dbit 1
PB7: dbit 1
PB2: dbit 1
PB1: dbit 1
PB0: dbit 1
mode: dbit 1

State2_flag: dbit 1 ; 0 -> Dont go to state 2, 1 -> go to state 2
State4_flag: dbit 1 ; 0 -> Dont go to state 4, 1 -> go to state 4
State1_flag: dbit 1

$NOLIST
$include(math32.inc)
$LIST

Init_All:
	; Configure all the pins for biderectional I/O
	mov	P3M1, #0x00
	mov	P3M2, #0x00
	mov	P1M1, #0x00
	mov	P1M2, #0x00
	mov	P0M1, #0x00
	mov	P0M2, #0x00
	
	orl	CKCON, #0x10 ; CLK is the input for timer 1
	orl	PCON, #0x80 ; Bit SMOD=1, double baud rate
	mov	SCON, #0x52
	anl	T3CON, #0b11011111
	anl	TMOD, #0x0F ; Clear the configuration bits for timer 1
	orl	TMOD, #0x20 ; Timer 1 Mode 2
	mov	TH1, #TIMER1_RELOAD ; TH1=TIMER1_RELOAD;
	setb TR1
	
	; Using timer 0 for delay functions.  Initialize here:
	clr	TR0 ; Stop timer 0
	orl	CKCON,#0x08 ; CLK is the input for timer 0
	anl	TMOD,#0xF0 ; Clear the configuration bits for timer 0
	orl	TMOD,#0x01 ; Timer 0 in Mode 1: 16-bit timer
	
	orl  P1M1, #0b10000010   ; Keep P1.7 as high-impedance input (AIN0), remove P1.1
	anl  P1M2, #0b01111101   ; Keep P1.7 as input, remove P1.1
	
	orl  P3M1, #0b00000001   ; Set P3.0 as high-impedance input (AIN1)
	anl  P3M2, #0b11111110   ; Ensure P3.0 stays in high-impedance mode

	
	; Initialize and start the ADC:
	anl ADCCON0, #0xF0
	orl ADCCON0, #0x01 ; Select channel 1
	; AINDIDS select if some pins are analog inputs or digital I/O:
	mov AINDIDS, #0x00 ; Disable all analog inputs
	orl AINDIDS, #0b11000000 ; Activate AIN0 and AIN1 analog inputs (now add AIN7)
	orl ADCCON1, #0x01 ; Enable ADC
	
	ret
	
wait_1ms:
	clr	TR0 ; Stop timer 0
	clr	TF0 ; Clear overflow flag
	mov	TH0, #high(TIMER0_RELOAD_1MS)
	mov	TL0,#low(TIMER0_RELOAD_1MS)
	setb TR0
	jnb	TF0, $ ; Wait for overflow
	ret

; Wait the number of miliseconds in R2
waitms:
	lcall wait_1ms
	djnz R2, waitms
	ret
;-----------------------------------------
; Initializing Pushbuttons (and checks)
;-----------------------------------------	
LCD_PB:
    ; Set variables to 1: 'no push button pressed'
    setb PB7
    setb PB6
    setb PB0
    setb PB1
    setb PB2

    ; Check if any push button is pressed
    clr P0.0 ;abort
    clr P1.3 ;start
    clr P0.1; soak time / reflow time
    clr P0.2 ; soak temp / reflow temp
    clr P0.3 ; mode

    ; Debounce
    mov R2, #50
    lcall waitms
    jb P1.5, LCD_PB_Done

    ; Set the LCD data pins to logic 1
    setb P0.0
    setb P1.3
    setb P0.1
    setb P0.3
    setb P0.2

    ; Check the push buttons one by one
    clr P1.3
    mov c, P1.5
    mov PB6, c
    setb P1.3

    clr P0.0
    mov c, P1.5
    mov PB7, c
    setb P0.0

    clr P0.3 ; mode
    mov c, P1.5
    mov PB0, c
    setb P0.3

    clr P0.2
    mov c, P1.5
    mov PB1, c
    setb P0.2

    clr P0.1
    mov c, P1.5
    mov PB2, c
    setb P0.1

LCD_PB_Done:	
    ret


; We can display a number any way we want.  In this case with
; four decimal places.
Display_formated_BCD:
	Set_Cursor(2, 3)
	Display_BCD(bcd+3)
	Display_BCD(bcd+2)
	Display_char(#'.')
	Display_BCD(bcd+1)
	
	
	
	ret

Read_ADC:
	clr ADCF
	setb ADCS ;  ADC start trigger signal
    jnb ADCF, $ ; Wait for conversion complete
    
    ; Read the ADC result and store in [R1, R0]
    mov a, ADCRL
    anl a, #0x0f
    mov R0, a
    mov a, ADCRH   
    swap a
    push acc
    anl a, #0x0f
    mov R1, a
    pop acc
    anl a, #0xf0
    orl a, R0
    mov R0, A
	ret
	
	; Sends a character using the serial port	
putchar:
	jnb TI, putchar
	clr TI
	mov SBUF, a
	ret

; Send a constant-zero-terminated string using the serial port
SendString:
	clr A
	movc A, @A+DPTR
	jz SendStringDone
	lcall putchar
	inc DPTR
	sjmp SendString
	
SendStringDone:
	ret

; sends each digit of bcd Initialize BCD to 00-00-00-00-00
													
Send_Temperature:
    ; Send Celsius value
    mov A, bcd+3
    swap A
    anl A, #0x0F
    add A, #0x30
    lcall putchar
    mov A, bcd+3
    anl A, #0x0F
    add A, #0x30
    lcall putchar
    
    mov A, bcd+2
    swap A
    anl A, #0x0F
    add A, #0x30
    lcall putchar
    mov A, bcd+2
    anl A, #0x0F
    add A, #0x30
    lcall putchar
    mov A, #'.'
    lcall putchar
    mov A, bcd+1
    swap A
    anl A, #0x0F
    add A, #0x30
    lcall putchar
    mov A, bcd+1
    anl A, #0x0F
    add A, #0x30
    lcall putchar
    
    mov A, bcd+0
    swap A
    anl A, #0x0F
    add A, #0x30
    lcall putchar
    mov A, bcd+0
    anl A, #0x0F
    add A, #0x30
    lcall putchar

	mov A, #'-'
	lcall putchar
	mov A, FSM1_state
	add A, #0x30
	lcall putchar
   
; Send newline for formatting
    mov A, #13    ; '\r'
    lcall putchar
    mov A, #10    ; '\n'
    lcall putchar 
    ret

; display the current state
Display_State:
    
    
    Set_Cursor(1,2) ; Move cursor to where the state value should appear

    mov x+0, FSM1_state  ; Load FSM1_state value into A
    mov x+1, #0  ; Load FSM1_state value into A
    mov x+2, #0  ; Load FSM1_state value into A
    mov x+3, #0  ; Load FSM1_state value into A
    lcall hex2bcd 
    Display_BCD(bcd+0)
    
    ret
    
Display_temp_BCD:
	Set_Cursor(1, 12)
	Display_BCD(bcd+1)
	Display_BCD(bcd+0)	
	ret
	
Display_cold_junc:
	Set_Cursor(2,15)
	Display_BCD(bcd+2)
		
	ret

Display_state_time:
	set_cursor (1,13)
	mov x+0, seconds_counter+0  ; Load FSM1_state value into A
    mov x+1, #0  ; Load FSM1_state value into A
    mov x+2, #0  ; Load FSM1_state value into A
    mov x+3, #0  ; Load FSM1_state value into A
    lcall hex2bcd 
    display_BCD(bcd+1)
    Display_BCD(bcd+0)
	ret

Display_total_runtime:
	set_cursor (1,7) 
	mov x+0, total_runtime+0  ; Load FSM1_state value into A
    mov x+1, #0  ; Load FSM1_state value into A
    mov x+2, #0  ; Load FSM1_state value into A
    mov x+3, #0  ; Load FSM1_state value into A
    lcall hex2bcd 
    display_BCD(bcd+1)
    Display_BCD(bcd+0)

	ret

Display_temp_refl:
	set_cursor (1,3) 
	mov x+0, temp_refl+0  ; Load FSM1_state value into A
    mov x+1, #0  ; Load FSM1_state value into A
    mov x+2, #0  ; Load FSM1_state value into A
    mov x+3, #0  ; Load FSM1_state value into A
    lcall hex2bcd 
    display_BCD(bcd+1)
    Display_BCD(bcd+0)

	ret

Display_time_refl:
	set_cursor (1,13) 
	mov x+0, time_refl+0  ; Load FSM1_state value into A
    mov x+1, #0  ; Load FSM1_state value into A
    mov x+2, #0  ; Load FSM1_state value into A
    mov x+3, #0  ; Load FSM1_state value into A
    lcall hex2bcd 
    display_BCD(bcd+1)
    Display_BCD(bcd+0)

	ret
	
Display_temp_soak:
	set_cursor (2,3) 
	mov x+0, temp_soak+0  ; Load FSM1_state value into A
    mov x+1, #0  ; Load FSM1_state value into A
    mov x+2, #0  ; Load FSM1_state value into A
    mov x+3, #0  ; Load FSM1_state value into A
    lcall hex2bcd 
    display_BCD(bcd+1)
    Display_BCD(bcd+0)

	ret

Display_time_soak:
	set_cursor (2,13) 
	mov x+0, time_soak+0  ; Load FSM1_state value into A
    mov x+1, #0  ; Load FSM1_state value into A
    mov x+2, #0  ; Load FSM1_state value into A
    mov x+3, #0  ; Load FSM1_state value into A
    lcall hex2bcd 
    display_BCD(bcd+1)
    Display_BCD(bcd+0)

	ret





;---------------------------------------------------
; Timer2 Initialization Routine
;---------------------------------------------------
Timer2_Init:
    mov T2CON, #0               ; Stop Timer2; select autoreload mode
    mov TH2, #high(TIMER2_RELOAD)
    mov TL2, #low(TIMER2_RELOAD)
    orl T2MOD, #0x80            ; Enable Timer2 autoreload mode
    mov RCMP2H, #high(TIMER2_RELOAD)
    mov RCMP2L, #low(TIMER2_RELOAD)
    ; Clear the millisecond counter
    clr a
    mov Count1ms, a
    mov Count1ms+1, a
    ; Enable Timer2 interrupt (ET2 is bit 7 in EIE)
    orl EIE, #0x80             ; Set ET2 = 1
    setb TR2                   ; Start Timer2
    ret

;---------------------------------------------------
; Timer2 Interrupt Service Routine (called every 1 ms)
;---------------------------------------------------
Timer2_ISR:
    clr  TF2             ; Clear Timer2 flag
    push acc
    push psw
    push B

    ; ---------------------------------------------------
    ; 1) Increment the 16-bit pwm_counter
    ; ---------------------------------------------------
    inc   pwm_counter    ; Inc the low byte (pwm_counter+0)
    mov   A, pwm_counter ; A now = low byte
    jnz   skip_high_inc
    inc   pwm_counter+1  ; if low byte rolled over, increment high byte
skip_high_inc:

    ; ---------------------------------------------------
    ; 2) Check if pwm_counter >= 1000 => reset to 0
    ;    1000 decimal = 0x03E8 => high=0x03, low=0xE8
    ; ---------------------------------------------------
    mov   A, pwm_counter+1      ; high byte
    cjne  A, #0x03, do_compare
    mov   A, pwm_counter        ; low byte
    cjne  A, #0xE8, do_compare

    ; => pwm_counter == 1000 => reset
    mov   pwm_counter,   #0
    mov   pwm_counter+1, #0

do_compare:
    ; ---------------------------------------------------
    ; 3) Compare (pwm_counter < pwm) ?
    ;    If yes => PWM_OUT=1, else => PWM_OUT=0
    ; ---------------------------------------------------
    ; We can do a 16-bit subtract: pwm_counter - pwm
    ; If carry=1 => pwm_counter < pwm
    ; If carry=0 => pwm_counter >= pwm
    ;
    ; So:  carry   => (pwm_counter < pwm)
    ;       ~carry => (pwm_counter >= pwm)
    ;
    ; Then we move that to PWM_OUT.
    ;
    ; Steps:
    ;   ACC.B = pwm_counter+1 (high), ACC.A = pwm_counter (low)
    ;   subtract (pwm+1 : pwm+0).
    ;

    ;-- Load pwm_counter (low->A, high->B)
    mov   A, pwm_counter
    mov   B, pwm_counter+1

    ;-- Subtract (pwm) from (pwm_counter)
    clr   C
    subb  A, pwm       ; (low)  => A = pwm_counter.low - pwm.low
    xch   A, B         ; swap A<->B, so now A = pwm_counter.high
    subb  A, pwm+1     ; (high) => A = pwm_counter.high - pwm.high - carry
    ; after these 2 'subb', carry=1 => (pwm_counter < pwm), carry=0 => (pwm_counter >= pwm)

    ; We want PWM_OUT=1 if pwm_counter < pwm => carry=1
    mov   PWM_OUT, C

    ; ---------------------------------------------------
    ; 4) 1ms tasks (increment Count1ms, check 1sec, etc.)
    ; ---------------------------------------------------
    inc Count1ms
    mov A, Count1ms
    jnz Skip_High
    inc Count1ms+1
Skip_High:
    mov A, Count1ms
    cjne A, #low(1000), ISR_Exit
    mov A, Count1ms+1
    cjne A, #high(1000), ISR_Exit

    ; => 1 second passed
    mov A, Seconds_counter
    add A, #1
    mov Seconds_counter, A
    
    mov A, total_runtime
    add A, #1
    mov total_runtime, A    
    
	jnb state1_flag, skip_safety_check
	mov A, Safety_time
    add A, #1
    mov Safety_time, A
	mov A, #60
	cjne A, Safety_time, skip_safety_check
	mov A, #50
	clr c ; clear the carry flag
    subb A, temp ; 50 - temp (if positive -> C = 0, if negative -> C = 1)
    jc skip_safety_check	
	mov FSM1_state, #0
	sjmp skip_state4_change	
	
skip_safety_check:
	mov A, seconds_counter
    cjne a, State_time, Seconds_OK
    jnb state2_flag, skip_state2_change
    mov FSM1_State, #3
    
skip_state2_change:
	jnb state4_flag, skip_state4_change
	mov FSM1_State, #5
	
skip_state4_change:
   	clr a                    
Seconds_OK:
    mov Seconds_counter, a

    ; Reset the millisecond counter back to 0.
    clr a
    mov Count1ms, a
    mov Count1ms+1, a

ISR_Exit:
	pop B
    pop psw
    pop acc
    reti

; note: we need to initialize the buttons PB6 (reflow start button) and PB7 (abort button, reflow stop button)


main:
	mov sp, #0x7f
	lcall Init_All
    lcall LCD_4BIT
    
    anl FSM1_state, #0   
    mov State_time, #0x01  
    mov Seconds_counter, #0x00
    mov total_runtime, #0x00
    
    mov temp_soak, #150
    mov time_soak, #60
    mov temp_refl, #220
    mov time_refl, #45
    mov mode, #0
    
	mov pwm+0, #0x00  ; low byte
	mov pwm+1, #0x00  ; high byte
    
    ; initialize flag
    clr state2_flag
    clr state4_flag
      
    lcall Timer2_Init ; START THE TIMER
    setb EA
    lcall LCD_PB
	
    
; parameter setup jumps to here
Display_Parameters:	
	;check start button
	; if pressed go FSM1
	jnb PB6, FSM1
	ljmp FSM1_state0
	; if not go state0;
    
FSM1:
	
	; initial messages in LCD
	;Set_Cursor(1, 1)
    ;Send_Constant_String(#test_message)
    Set_Cursor(1,1)
    Send_Constant_String(#state_display) ; Display "S"
    Set_Cursor(1,5)
    Send_Constant_String(#run_time_display)
    Set_Cursor(1,11)
    Send_Constant_String(#state_time_display)
	Set_Cursor(2, 1)
    Send_Constant_String(#value_message)
    set_cursor(2,12)
    send_constant_string(#cold_message)

	; Read the 2.08V LM4040 voltage connected to AIN0 on pin 6
	anl ADCCON0, #0xF0
	orl ADCCON0, #0x00 ; Select channel 0

	lcall Read_ADC
	; Save result for later use
	mov VAL_LM4040+0, R0
	mov VAL_LM4040+1, R1

	; Read the signal connected to AIN7
	anl ADCCON0, #0xF0
	orl ADCCON0, #0x01 ; Select channel 1
	lcall Read_ADC
    
    ; Convert to voltage
	mov x+0, R0
	mov x+1, R1
	; Pad other bits with zero
	mov x+2, #0
	mov x+3, #0
	Load_y(40832) ; The MEASURED voltage reference: 4.0832V, with 4 decimal places
	lcall mul32
	; Retrive the ADC LM4040 value
	mov y+0, VAL_LM4040+0
	mov y+1, VAL_LM4040+1
	; Pad other bits with zero
	mov y+2, #0
	mov y+3, #0
	lcall div32

	;add cold junction temperature (make sure to include the gain factor 300
	Load_y(2400) 
	lcall add32
	
	
	lcall LCD_PB
	Load_y(79) ;some funky math to get this
	lcall mul32
	
	lcall hex2bcd
	lcall Send_Temperature
	lcall Display_formated_BCD
		
	lcall bcd2hex
	
	Load_y(10000)
	lcall div32
	
	mov A, x+0
	mov temp, A
	
	; Read the signal connected to AIN7
	anl ADCCON0, #0xF0
	orl ADCCON0, #0x07 ; Select channel 7
	lcall Read_ADC
    
    ; Convert to voltage
	mov x+0, R0
	mov x+1, R1
	; Pad other bits with zero
	mov x+2, #0
	mov x+3, #0
	Load_y(40300) ; The MEASURED voltage reference: 4.0959V, with 4 decimal places
	lcall mul32
	; Retrive the ADC LM4040 value
	mov y+0, VAL_LM4040+0
	mov y+1, VAL_LM4040+1
	; Pad other bits with zero
	mov y+2, #0
	mov y+3, #0
	lcall div32
	
	Load_y(27300)
	lcall sub32
	
	Load_y(100)
	lcall mul32
	
	; Convert to BCD and display
	lcall hex2bcd
	lcall Display_cold_junc
	lcall Display_total_runtime
	
	Wait_Milli_Seconds(#100)

   	
    mov a, FSM1_state ; load the current state into 'a' (a = current state)
    lcall Display_State
    lcall Display_state_time

FSM1_state0: ; default state after reset

    cjne a, #0, FSM1_state1 ; if the current state != 0, go to state 1, else stay in state 0 (check first)
	mov pwm+0, #0x00  ; low byte
	mov pwm+1, #0x00  ; high byte, #0 ; set pulse width modulator (oven power) to 0%
  
  	; "WE SHOULD SET THE PARAMETERS HERE IN STATE 0" 
  
  	jnb PB6, FSM1_state0_done ; if PB6 is pressed go to FSM1_state0_done
  	;ljmp parameter_setup ;else
  	ljmp clear_display

clear_display:
	WriteCommand(#0x01) ; Clear screen command (takes some time)
	Wait_Milli_Seconds(#10) ; Wait for the clear screen command to finish (adjust delay if needed)
	ljmp parameter_setup
  	
JUMP_TO_FSM1:
	ljmp FSM1

FSM1_state0_done:
	WriteCommand(#0x01) ; Clear screen command (takes some time)
	Wait_Milli_Seconds(#10) ; Wait for the clear screen command to finish (adjust delay if needed)

	clr a		;reset the total runtime back to 0 when entering state 1
  	mov total_runtime, a
  	 	
  	clr a				; reset teh seconds counter
  	mov seconds_counter, a

	mov Safety_time, #0x00
	mov seconds_counter, #0x00
	mov state_time, #0x01
	setb state1_flag
	
 	mov FSM1_state, #1 ; update current state to 1
 	lcall Display_State
  	ljmp FSM1 ; loop back to start

FSM1_state1: ; preheat state
    
    
  	jnb PB7, JUMP_TO_ABORT ; check if abort button is pressed
  	;jnb PB7, $ ; wait for key release
  	cjne a, #1, FSM1_state2 ; if the current state != 1, go to state 2, else stay in state 1 (check first)
  	;100%
  	mov pwm+0, #0xE8  ; low = 0xE8
	mov pwm+1, #0x03  ; high = 0x03
	mov sec, #0 ; set seconds to 0
	mov a, temp_soak ; load the soak temperature threshold (150 C)
    clr c ; clear the carry flag
	subb a, temp ; temp_soak - temp (if positive -> C = 0, if negative -> C = 1)
	jnc JUMP_TO_FSM1 ; if C = 0, stay in state 1, else go to FSM1_state1
	
FSM1_state1_done:
	mov FSM1_state, #2 ; update current state to 2
	
	mov seconds_counter, #0x00
	clr state1_flag
	 
	lcall Display_State
    ljmp FSM1 ; loop back to start
	
FSM1_state2: ; soak state
	jnb PB7, jump_to_abort ; check if abort button is pressed
	setb state2_flag ;we are in state2
	mov state_time, time_soak ; **REPLACE WITH VARIABLE**
	;jnb PB7, $ ; wait for key release
    cjne a, #2, FSM1_state3 ; if the current state != 2, go to state 3, else stay in state 2 (check first)
	; 20% duty => 200 out of 1000
	mov pwm+0, #0xC8  ; low byte
	mov pwm+1, #0x00  ; high byte
    ljmp FSM1
	  
FSM1_state2_done:
    mov FSM1_state, #3 ; upate current state to 3
    lcall Display_State
    ljmp FSM1 ; loop back to start
  
FSM1_state3: ; preheat to reflow state
    jnb PB7, ABORT ; check if abort button is pressed
    mov state_time, #0x01
	cpl state2_flag
    ;jnb PB7, $ ; wait for key release
    cjne a, #3, FSM1_state4 ; if the current state != 3, go to state 4, else stay in state 3 (check first)
    ; set pulse width modulator (oven power) to 100%
    mov pwm+0, #0xE8  ; low = 0xE8
	mov pwm+1, #0x03  ; high = 0x03

    
    mov a, temp_refl ; load the reflow temperature threshold (220 C)
    clr c ; clear the carry flag
    subb a, temp ; temp_refl - temp (if positive -> C = 0, if negative -> C = 1)
    jnc SECOND_FSM1_JUMP ; if C = 0, stay in state 3, else go to FSM1_state3_done
	ljmp FSM1_state3_done

JUMP_TO_ABORT: ; jump to ABORT
	ljmp ABORT
	  
FSM1_state3_done:
	
    mov FSM1_state, #4 ; upate current state to 4
    lcall Display_State
    ljmp FSM1 ; loop back to start

SECOND_FSM1_JUMP:
	ljmp FSM1
FSM1_state4: ; reflow state
    jnb PB7, ABORT ; check if abort button is pressed
    ;jnb PB7, $ ; wait for key release
    mov state_time, time_refl ; **REPLACE WITH VARIABLE**
    setb state4_flag
    cjne a, #4, FSM1_state5 ; if the current state != 4, go to state 5, else stay in state 4 (check first)
    ; 20% duty => 200 out of 1000
	mov pwm+0, #0xC8  ; low byte
	mov pwm+1, #0x00  ; high byte
    ljmp FSM1
	    

    
FSM1_state4_done: 
    mov FSM1_state, #5 ; upate current state to 5
    
    lcall Display_State
    ljmp FSM1 ; loop back to start

FSM1_state5:
    jnb PB7, ABORT ; check if abort button is pressed
    mov state_time, #0x01
    cpl state4_flag
    ;jnb PB7, $ ; wait for key release
    cjne a, #5, JUMP_TO_STATE0 ; if the current state != 5, go to state 0, else stay in state 5 (check first)
    ; set pulse width modulator (oven power) to 0%
    mov pwm+0, #0x00  ; low byte
	mov pwm+1, #0x00  ; high byte 
    mov a, #60 ; load cool down temperature (60 seconds)
    clr c ; clear the carry flag
    subb a, temp ; 60 - temp (if positive -> C = 0, if negative -> C = 1)
    jc SECOND_FSM1_JUMP ; if C = 0, stay in state 5, else go to FSM1_state5_done
    

FSM1_state5_done:
    mov FSM1_state, #0 ; reset back to state 0  
    ljmp FSM1 ; loop back to start

JUMP_TO_STATE0:
	ljmp FSM1_state0 ;jump to state0
    


ABORT: ; if at any time during reflow process, the abort button is pressed, turn the oven off and reset the state to 0
    mov pwm+0, #0x00  ; low byte
	mov pwm+1, #0x00  ; high byte, #0 ; set pulse width modulator (oven power) to 0%
    mov FSM1_state, #0  ; reset the state machine to state 0
    ljmp FSM1 ; jump to next part of program


parameter_setup:
    ;soak 150 +-20 temp
    ;soak tim 60-120
    ;reflow ime 45-75
    ;reflow temp 217-240
    lcall LCD_PB
    
    ;--------------
    ;Display Params
    ;--------------
    set_cursor(1,1)
    send_constant_string(#message_temp_refl)
    set_cursor(2,1)
    send_constant_string(#message_time_soak) 
    
	lcall Display_Temp_refl	
	lcall Display_time_refl
	lcall Display_time_soak
	lcall display_temp_soak
	
   
    jnb PB6, JUMP_TO_STATE0_DONE
    jnb PB0, toggle_mode
    mov a, mode
    jz adjust_soak_params ; Jump if mode = 0 (soak)
    ljmp adjust_reflow_params ; Jump if mode = 1 (reflow)
    

toggle_mode:
	lcall LCD_PB
    jnb PB6, JUMP_TO_STATE0_DONE
    mov a, mode
    cpl a ; Toggle between 0 and 1
    mov mode, a
    mov a, mode
    jz adjust_soak_params ; Jump if mode = 0 (soak)
    ljmp adjust_reflow_params ; Jump if mode = 1 (reflow)

adjust_soak_params:
	lcall LCD_PB
    ; PB1 -> Increase soak temperature
    ; check if mode has been changed
    jnb PB6, JUMP_TO_STATE0_DONE
    jnb PB1, inc_temp_soak
    jnb PB2, inc_time_soak
    ljmp parameter_setup

inc_temp_soak:
	lcall LCD_PB
    jnb PB6, JUMP_TO_STATE0_DONE
    mov a, temp_soak
    add a, #1 ; Increase by 1 degrees
    mov temp_soak, a
    lcall display_temp_soak

    cjne a, #170, set_temp_soak 
    mov a, #130 ; Reset to min value
   
set_temp_soak:
	lcall LCD_PB
    jnb PB6, JUMP_TO_STATE0_DONE
    mov temp_soak, a
    ljmp adjust_soak_params

JUMP_TO_STATE0_DONE:
	lcall LCD_PB
	ljmp FSM1_state0_done

inc_time_soak:
	lcall LCD_PB
    jnb PB6, JUMP_TO_STATE0_DONE
    mov a, Time_soak
    add a, #1 ; Increase by 5 seconds
    lcall display_time_soak

    cjne a, #120, set_time_soak ; Max 120s, reset if exceeded
    mov a, #60 ; Reset to min value
    
set_time_soak:
	lcall LCD_PB
    jnb PB6, JUMP_TO_STATE0_DONE
    mov Time_soak, a
    ljmp adjust_soak_params

    ;ljmp FSM1_state0_done

adjust_reflow_params:
	lcall LCD_PB
    jnb PB6, JUMP_TO_STATE0_DONE
    ; PB1 -> Increase reflow temperature
    jnb PB1, inc_temp_refl
    jnb PB2, inc_time_refl
    ljmp parameter_setup

inc_temp_refl:
	lcall LCD_PB
    jnb PB6, JUMP_TO_STATE0_DONE
    
    
    mov a, temp_refl
    add a, #1
    lcall Display_temp_refl
    cjne a, #240, set_temp_refl 
    mov a, #217

set_temp_refl:
	lcall LCD_PB
    jnb PB6, JUMP_TO_STATE0_DONE
    mov temp_refl, a
    ljmp adjust_reflow_params

inc_time_refl:
	lcall LCD_PB
    jnb PB6, JUMP_TO_STATE0_DONE
    mov a, Time_refl
    add a, #1
    lcall display_time_refl
    cjne a, #75, set_time_refl 
    mov a, #45

set_time_refl:
	lcall LCD_PB
    jnb PB6, JUMP_TO_STATE0_DONE
    mov Time_refl, a
    ljmp adjust_reflow_params

