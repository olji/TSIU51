/* LCD_ClockSystem.asm
* PORTC = Display Control
* PORTB = DDP (Digital Data Port)
* PORTD = Interrupts until data is sending, then its the communication port with the transmitting microcontroller
*/

.INCLUDE "m8def.inc"

; Registers for storing values between menu states
; POSTPROJECT: I originally forgot about the defines r0-r5 which resulted in the names never being used, fixed it for readability now however
.def    year = r0
.def    month = r1
.def    day = r2
.def    hour = r3
.def    minute = r4
.def    weekday = r5

.def    EELOW = r17
.def    EEhigh = r18
.def    temp = r29
.def    clear = r20   ; Used for clearing when immediate isn't an option. POSTPROJECT: Zero would've been a better name.
.def    bitset = r21
.def    menu = r22
.def    value = r23
.def    save = r24    ; Used for storing values temporarily
.def    loopReg = r26
.def    setAdr = r27
.def    output = r28  ; Output to LCD

.equ    DDP = PORTB
.equ    DisplayConfig = PORTC ; POSTPROJECT: Also never used, didn't change this as it didn't help as much with readability in my opinion


.org 0
    rjmp    INIT
    rjmp    EXT_INT0
    rjmp    EXT_INT1
.org 13


INIT:
    ; Init stack pointer
    cli
    ldi    temp,high(ramend)
    out    sph,temp
    ldi    temp,low(ramend)
    out    spl,temp

    ; Init external interrupts
    ldi    temp,0xC0
    out    GICR,temp
    ldi    temp,0x8A
    out    MCUCR,temp

    
    ; Set input/output pins
    ldi    r16,0x07
    out    DDRC,r16
    ldi    r16,0xFF
    out    DDRB,r16
    ldi    r16,0x00
    out    DDRD,r16

    ; Init register values
    ldi    clear,0x00
    ldi    menu,0x00
    mov    year,menu
    ldi    temp,0x01
    mov    month,temp
    mov    day,temp
    mov    hour,menu
    mov    minute,menu
    mov    weekday,menu

    ; Init LCD screen
    ; POSTPROJECT: Could've used a subroutine and just change the arguments instead.
    ldi     bitset,0x04
    ldi     loopReg,0xFF
    rcall   WAIT

    out     PORTC,bitset
    ldi     temp,0x38
    out     DDP,temp
    out     PORTC,clear

    ldi     loopReg,0xFF
    rcall   WAIT

    out     PORTC,bitset
    ldi     temp,0x0C
    out     DDP,temp
    out     PORTC,clear

    ldi     loopReg,0xFF
    rcall   WAIT

    out     PORTC,bitset
    ldi     temp,0x01
    out     DDP,temp
    out     PORTC,clear

    ldi     loopReg,0xFF
    rcall   WAIT

    out     PORTC,bitset
    ldi     temp,0x06
    out     DDP,temp
    out     PORTC,clear

    ldi     loopReg,0xFF
    rcall   WAIT

EEINIT:
    ldi     EElow,0x05
    ldi     EEhigh,0x00

    sei

MAIN:
    rcall   WRITESTR
HELSINKI_AIRPORT:
    in      temp,PIND   ; Read input
    andi    temp,0x20   ; Get the value of the send-button
    cli                 ; POSTPROJECT: We should've stored the status register during external interrupts, better practice than deactivating interrupts for every comparison.
    cpi     temp,0x20   ; Continue to structure data and then transmit to second microcontroller if button has been pressed
    breq    ARLANDA     ; Small joke since the branch didn't reach to where it was going by itself.
    sei

    cpi     r31,0x01    ; See if an interrupt has happened, if not it's unnecessary to update screen
    brne    HELSINKI_AIRPORT
    cli
    rcall   LCD_UPDATE
    ldi     temp,0xC0
    out     GIFR,temp   ; Clear interrupt flags
    rjmp    HELSINKI_AIRPORT

LCD_UPDATE:
    ldi     setAdr,0x00 ; Prepare to set LCD address to 0x00
    ldi     r31,0x00
    rcall   LCD_SETADR
    rcall   LCD_CLEAR
    rcall   LCD_SETADR  ; Set the address to 0x00 again to be sure
    rcall   WRITESTR
    clr     temp
    ret

LCD_CLEAR:
    ldi     output,0x01
    in      r30,PINC
    ori     r30,0x04
    out     PORTC,r30
    out     DDP,output
    ldi     loopReg,0xC0
    rcall   WAIT
    andi    r30,0xFB
    out     PORTC,r30
    out     DDP,clear
    ret

WRITESTR:
    cli
    rcall   MENU_LOOKUP  ; Set address in EEPROM to read the corresponding string of the menu which is active
    WRITESTR2:
    rcall   EE_READ
    cp      output,clear ; Look for null terminator (\0), otherwise continue reading string
    breq    ROW2
    rcall   LCD_WRITE
    inc     EElow
    rjmp    WRITESTR2

ROW2:
    ldi     setAdr,0x40
    rcall   LCD_SETADR
    cpi     menu,0x03 ; menu 0x03 will be writing a string instead of a numeric value, a different subroutine (WRITEDAY) is needed
    breq    WRITEDAY
    rcall   WRITEVAL
ROW2_DONE:
    ret

LCD_WRITE:
    in      r30,PINC
    ori     r30,0x05
    out     PORTC,r30
    out     DDP,output
    ldi     loopReg,0x01
    rcall   WAIT
    andi    r30,0xFA
    out     PORTC,r30
    out     DDP,clear
    ret

LCD_SETADR:
    ori     setAdr,0x80
    in      r30,PINC
    ori     r30,0x04
    out     PORTC,r30
    out     DDP,setAdr
    ldi     loopReg,0xFF
    rcall   WAIT
    andi    r30,0xFB
    out     PORTC,r30
    out     DDP,clear
    ret

ARLANDA:
    rjmp    HEATHROW

WRITEDAY:
    rcall   LD3
    ldi     EElow,0x21  ; Strings starts at 0x25, 0x21 is set to compensate for the addition that will always happen
    ldi     temp,0x04   ; String length is four, including null terminator
    mov     bitset,value
    inc     bitset
DAY_EE_SET:
    add     EElow,temp
    dec     bitset      ; bitset reaches zero as we reach the correct string
    brne    DAY_EE_SET
WRITEDAY2:              ; Read character, if not a null terminator it is printed and the procedure is repeated, when null terminator is detected we will jump to ROW2_DONE, which exits the writing to the LCD.
    rcall   EE_READ
    cp      output,clear
    breq    ROW2_DONE
    rcall   LCD_WRITE
    inc     EElow
    rjmp    WRITEDAY2

WRITEVAL:
    rcall   LOADVAL
    mov     bitset,value
    ldi     output,0x00    ; output will represent the eventual second digit
    cpi     bitset,0x0A    ; If the value is higher than decimal 10 we need a second digit, which WRITE2INT means
    brsh    WRITE2INT
    miv     output,bitset
    ori     output,0x30    ; Adapt value to the ascii table
    rcall   LCD_WRITE
    ret

WRITE2INT:      ; Repeatedly subtract 10 from the 2-digit value and add one to a second register, if the value reaches negative values we jump before incrementing the second digit
    subi    bitset,0x0A
    brmi    INT_WRITE
    inc     output
    rjmp    WRITE2INT

INT_WRITE:
    ldi     temp,0x3A      ; Bring first digit back to positive side
    add     bitset,temp    ; and adapt it to the ascii table at the same time
    ori     output,0x30
    rcall   LCD_WRITE
    mov     output,bitset
    rcall   LCD_WRITE
    ret

LOADVAL:                ; Ugly code here, didn't find another way
    cpi     menu,0x00
    breq    LD0
    cpi     menu,0x01
    breq    LD1
    cpi     menu,0x02
    breq    LD2
    cpi     menu,0x03
    breq    LD3
    cpi     menu,0x04
    breq    LD4
    cpi     menu,0x05
    breq    LD5
LD0:
    mov     value,year
    rjmp    ENDVAL
LD1:
    mov     value,month
    rjmp    ENDVAL
LD2:
    mov     value,day
    rjmp    ENDVAL
LD3:
    mov     value,weekday
    rjmp    ENDVAL
LD4:
    mov     value,hour
    rjmp    ENDVAL
LD5:
    mov     value,minute
    rjmp    ENDVAL

SAVEVAL:
    cpi     menu,0x00
    breq    SV0
    cpi     menu,0x01
    breq    SV1
    cpi     menu,0x02
    breq    SV2
    cpi     menu,0x03
    breq    SV3
    cpi     menu,0x04
    breq    SV4
    cpi     menu,0x05
    breq    SV5

ENDVAL: ret

SV0:
    mov     year,value
    rjmp    ENDVAL
SV1:
    mov     month,value
    rjmp    ENDVAL
SV2:
    mov     day,value
    rjmp    ENDVAL
SV3:
    mov     weekday,value
    rjmp    ENDVAL
SV4:
    mov     hour,value
    rjmp    ENDVAL
SV5:
    mov     minute,value
    rjmp    ENDVAL

MNG_VALUE:      ; Keeps values in ranges, months cannot exceed 12 or be lower than 1 for example
    cpi     menu,0x01
    breq    VALUE_M
    cpi     menu,0x02
    breq    VALUE_D
    cpi     menu,0x03
    breq    VALUE_W
    cpi     menu,0x04
    breq    VALUE_H
    cpi     menu,0x05
    breq    VALUE_MIN
MNG_END:
    ret

VALUE_M:
    cpi     value,0x00
    breq    VAL_SET_ONE
    cpi     value,13
    brlt    MNG_END
    ldi     value,12
    rjmp    MNG_END
VALUE_D:
    cpi     value,0x00
    breq    VAL_SET_ONE
    cpi     value,32
    brlt    MNG_END
    ldi     value,31
    rjmp    MNG_END
VALUE_W:
    cpi     value,0x00
    cpi     value,7
    brlt    MNG_END
    ldi     value,6
    rjmp    MNG_END
VALUE_H:
    cpi     value,24
    brlt    MNG_END
    ldi     value,23
    rjmp    MNG_END
VAL_SET_ONE:
    ldi     value,0x01
    ret

MENU_LOOKUP:
    ldi     temp,0x00
    cpi     menu,0x00
    breq    EE_SETADR
    ldi     temp,0x05
    cpi     menu,0x01
    breq    EE_SETADR
    ldi     temp,0x0B
    cpi     menu,0x02
    breq    EE_SETADR
    ldi     temp,0x1B
    cpi     menu,0x03
    breq    EE_SETADR
    ldi     temp,0x0F
    cpi     menu,0x04
    breq    EE_SETADR
    ldi     temp,0x14
    cpi     menu,0x05
    breq    EE_SETADR
    ret
EE_SETADR:
    mov     EElow,temp
    ret
    ; Ugly section stops here

EE_READ:        ; This procedure is more or less a copy from the datasheet
    sbic    EECR,EEWE
    rjmp    EE_READ
    cli
    out     EEARH,EEhigh
    out     EEARL,EElow
    sbi     EECR,EERE       ; Set a bit to enable reading, don't really know why it's taken from EERE but copypasta is delicious
    in      output,EEDR
    ret

WAIT:    ; A wait loop going for basically 0xFF^(loopReg) times, if loopReg is zero it'll be classed as 0x100
LOOP1:
    ldi     temp,0xFF
LOOP2:
    dec     temp
    brne    LOOP2
    dec     loopReg
    brne    LOOP1
    ret

EXT_INT0: ; Menu change, if out of bounds it'll loop around
          ; POSTPROJECT: A different subroutine naming convention here, camel case is annoying to type when you're copying from paper
    cli
    in      save,PIND
    ldi     r31,0x01
    sbrs    save,0x00
    rjmp    Menu_R
    inc     menu
    cpi     menu,0x06
    brmi    In_Bounds
    ldi     menu,0x00
In_Bounds:
    reti
Menu_R:
    dec     menu
    brpl    In_Bounds
    ldi     menu,0x05
    reti

EXT_INT1:
    cli
    in      save,PIND
    ldi     r31,0x01
    sbrc    save,0x01
    rjmp    Value_R
    inc     value
    cpi     value,99
    brmi    Val_In_Bounds
    ldi     value,99
    rcall   MNG_VALUE
    rcall   SAVEVAL
    reti
Value_R:
    dec     value
    brpl    Val_In_Bounds
    ldi     value,0
Val_In_Bounds:
    rcall   MNG_VALUE
    rcall   SAVEVAL
    reti

HEATHROW:   ; POSTPROJECT: Completely forgot about the defines at the start, changed r0-r5 to the name it was given instead for readability
            ; This subroutines takes all data and converts it to a single DCF77-encoded string which is sent to the transmitting microcontroller
    cli
    rcall   SEND_WRITE
    rcall   START_BYTE
    mov     r26,minute
    rcall   CONVERT_TO_DCF
    ldi     r26,0x00
    rcall   FIND_PARITY
    rcall   PARITYCALC
    mov     r17,r29
    or      r17,r25
    mov     r25,hour
    rcall   CONVERT_TO_DCF
    ldi     r26,0x00
    rcall   FIND_PARITY
    rcall   PARITYCALC
    lsr     r29
    or      r25,r29
    mov     r18,r25
    mov     r25,day
    rcall   CONVERT_TO_DCF
    ldi     r26,0x00
    rcall   FIND_PARITY
    mov     r19,r25
    mov     r20,weekday
    inc     r20    ; Compensate for zero indexing
    rcall   FIND_PARITY
    mov     r25,month
    rcall   CONVERT_TO_DCF
    rcall   FIND_PARITY
    mov     r21,r25
    mov     r25,year
    rcall   CONVERT_TO_DCF
    rcall   FIND_PARITY
    rcall   PARITYCALC
    mov     r22,r25
    mov     r23,r29
    lsr     r23     ; Shift 7 times 
    lsr     r23
    lsr     r23
    lsr     r23
    lsr     r23
    lsr     r23
    lsr     r23
    rcall   SEND_DATA
    rjmp    DONE

START_BYTE:             ; Start by looking for Daylight Time Saving, only exact to the day, not the time.
    ldi     r16,0x04
    mov     r17,month
    cpi     r17,0x03    ; If before March or after October it will always be false
    brmi    DTS_FALSE
    cpi     r17,0x0B
    brsh    DTS_FALSE
    cpi     r17,0x03
    breq    DTS_MAYBE
    cpi     r17,0x0A
    breq    DTS_MAYBE
DTS_TRUE:               ; If none of those are true -> month is April-September, we know DTS is true
    ori     r16,0x08    ; POSTPROJECT: I don't even know what I'm doing here, should work with ldi r16,0x08 as well.
    andi    r16,0x08
DTS_FALSE:
    ori     r16,0x20    ; Start bit in the DCF77 protocol is set
    ret
    
DTS_MAYBE:              ; Last sunday of the month is the rule, so let's see if that's the case
    ldi     r19,0x07    ; Amount of days in a week
    mov     r17,day
    mov     r18,weekday
    inc     r18         ; Compensate for zero indexing
    sub     r19,r18     ; Find how many days left until next sunday
    cpi     r19,0x01
    breq    SET_SUNDAY
RET_TO_DST:
    add     r17,r19
    mov     r18,month
    cpi     r18,0x03
    breq    ISMARCH
    cpi     r17,0x20    ; Check if we're overflowing the month, both month save 31 days luckily
    brsh    DTS_FALSE
    rjmp    DTS_TRUE
    ret
SET_SUNDAY:
    ldi     r19,0x07
    rjmp    RET_TO_DST
ISMARCH:
    cpi     r17,0x20
    brsh    DTS_TRUE
    rjmp    DTS_FALSE

CONVERT_TO_DCF:        ; A modified version of the hex -> ASCII conversion subroutine
    ldi     r27,0x00
    mov     r26,r25
CONV1:
    cpi     r26,0x0A
    brmi    CONV2
    subi    r26,0x0A
    inc     r27
    rjmp    CONV1
CONV2:
    lsl     r27
    lsl     r27
    lsl     r27
    lsl     r27 ; Shift the value to the second digit (0x0F -> 0xF0)
    or      r27,r26
    mov     r25,r27
    ret
FIND_PARITY:
    mov     r31,r25
    ldi     r30,0x00
PARITYFIND:
    cpi     r30,0x08
    breq    PARITY_END
    inc     r30
    lsl     r31
    brcs    CARRYSET
    rjmp    PARITYFIND
CARRYSET:
    inc     r29
    rjmp    PARITYFIND
PARITY_END:
    ret

PARITYCALC:
    cpi     r29,0x00
    breq    P_END
    subi    r29,0x02
    brmi    P_TRUE
    rjmp    PARITYCALC
P_TRUE:
    ldi     r29,0x80
P_END:
    ret

L_TRUE:
    ldi     r18,0x01
L_FALSE:
    ret

SEND_WRITE:   ; Prints "Sending..." on the display to show we're doing that' wasn't valuable enough to get its place in our Lord and saviour: EEPROM
    rcall   LCD_CLEAR
    ldi     setAdr,0x00
    rcall   LCD_SETADR
    ret
SEND_DATA:
    ldi     r30,0x27
    out     DDRC,r30
    ldi     r27,0x06
    mov     r28,r16
    rcall   SENDBYTE
    out     PORTC,r30
    ldi     output,'S'
    rcall   LCD_WRITE

    ldi     r27,0x08
    mov     r28,r17
    rcall   SENDBYTE
    out     PORTC,r30
    ldi     output,'e'
    rcall   LCD_WRITE

    ldi     r27,0x07
    mov     r28,r18
    rcall   SENDBYTE
    out     PORTC,r30
    ldi     output,'n'
    rcall   LCD_WRITE

    ldi     r27,0x06
    mov     r28,r19
    rcall   SENDBYTE
    out     PORTC,r30
    ldi     output,'d'
    rcall   LCD_WRITE

    ldi     r27,0x03
    mov     r28,r20
    rcall   SENDBYTE
    out     PORTC,r30
    ldi     output,'i'
    rcall   LCD_WRITE

    ldi     r27,0x05
    mov     r28,r21
    rcall   SENDBYTE
    out     PORTC,r30
    ldi     output,'n'
    rcall   LCD_WRITE

    ldi     r27,0x08
    mov     r28,r22
    rcall   SENDBYTE
    out     PORTC,r30
    ldi     output,'g'
    rcall   LCD_WRITE

    ldi     r27,0x01
    mov     r28,r23
    rcall   SENDBYTE
    out     PORTC,r30
    ldi     output,'.'
    rcall   LCD_WRITE

    rcall   WAIT_COMP
    ret

SENDBYTE:
    lsr     r28
    brcs    SendSet
    rcall   SendClear
SENDBYTE_UPDT:
    rcall   WAIT_REQ
    dec     r27
    breq    BYTE_DONE
    rjmp    SENDBYTE
BYTE_DONE:
    ret
SendSet:
    ldi     r30,0x01
    out     DDP,r30
    ldi     r30,0x20
    out     PORTC,r30
    rcall   WAIT_AFFIRM
    ldi     r30,0x00
    out     DDP,r30
    rjmp    SENDBYTE_UPDT
SendClear:
    ldi     r30,0x00
    out     DDP,r30
    ldi     r30,0x20
    out     PORTC,r30
    rcall   WAIT_AFFIRM
    ldi     r30,0x00
    out     DDP,r30
    ret

WAIT_AFFIRM:
    in      r31,PINC
    andi    r31,0x10
    cpi     r31,0x10
    brne    WAIT_AFFIRM
    ret

WAIT_REQ:
    in      r31,PINC
    andi    r31,0x08
    cpi     r31,0x08
    brne    WAIT_REQ
    ret
WAIT_COMP:
    ldi     r30,0x20
    out     PORTC,r30
COMP2:
    in      r30,PINC
    andi    r30,0x18
    cpi     r30,0x18
    brne    COMP2
    ret
DONE:
    rcall   LCD_CLEAR
    ldi     setAdr,0x00  ; Prints "Done." (As in communications between microcontrollers are done), as with before this wasn't blessed with a spot in the holy EEPROM
    rcall   LCD_SETADR
    ldi     output,'D'
    rcall   LCD_WRITE
    ldi     output,'o'
    rcall   LCD_WRITE
    ldi     output,'n'
    rcall   LCD_WRITE
    ldi     output,'e'
    rcall   LCD_WRITE
    ldi     output,'.'
    rcall   LCD_WRITE

INF_IDLE:   ; POSTPROJECT: As our examinator commented, an infidel?
    rjmp    INF_IDLE
