




data Instruction =
    ADC --Add with carry
    | ADD --Add without carry
    | ADIW --Add immediate to Word
    | AND --Logical And
    | ANDI --Logical And with intermediate
    | ASR --Arithmetic Shift right
    | BCLR --Bit clear in Sreg
    | BLD --Bit Load from the T Flag in SREG to Bit in Registre
    | BRBC --Branch if bit in Sreg cleared
    | BRBS --Branch if bit in Sreg Set 
    | BRCC --Branch if carry cleared
    | BRCS -- Branch if carry set
    | BREAK --Break
    | BREQ --Branch if Equal
    | BRGE -- Branch if greater or equal (signed)
    | BRHC -- Branch if Half Carry is cleared
    | BRHS -- Branch if Half Carry is set 
    | BRID -- Branch if global interrupt is disabled
    | BRIE -- Branch if global interrupt is enabled
    | BRLO -- Branch if lower (unsigned)
    | BRLT -- Branch if thess than (signed)
    | BRMI -- Branch if minus
    | BRNE -- Branch if Not Equal
    | BRPL -- Branch if Plus
    | BRSH -- Branch if same or higher
    | BRTC -- Branch if Tflag is cleared
    | BRTS -- Branch if Tflag is set
    | BRVC -- Branch if Overflow cleared
    | BRVS -- Branch if Overflow set
    | BSET -- Bit Set in Sreg
    | BST -- Bit Store from bit in registor to Tflag in SREG
    | CALL -- Long call to subroutine
    | CBI -- Clear bit in I/O Register
    | CBR -- Clear bits in Register
    | CLC -- Clear Carry Flag
    | CLH -- Clear Half carry flag
    | CLI -- Clear global interrupt flag
    | CLN -- Clear negative flag
    | CLR -- Clear register
    | CLS -- Clear signed flag
    | CLT -- Clear T Flag 
    | CLV -- Clear Overflow flag
    | CLZ -- Clear zero flag
    | COM -- ones complement
    | CP -- Compare 
    | CPC -- Compare with carry
    | CPI -- Compare with immediate 
    | CPSE -- Compare skip if equal 
    | DEC -- Decrement
    | DES -- Data encryption standard
    | EICALL -- Extended Indirect Call to subroutine
    | EIJMP -- Extended Indirect JMP
    | ELPM -- Extended Load Program Memory
    | EOR -- Exclusive or
    | FMUL -- Fractional Multiply Unsigned
    | FMULS -- Fractional Multiply Signed
    | FMULSU -- Frational Multiply Signed with Unsigned
    | ICALL -- Indirect Call to Subroutine
    | IJMP -- Indirect Jump
    | IN -- Load and I/O location to register
    | INC -- Increment
    | JMP -- Jump 
    | LAC -- Load and Clear
    | LAS -- Load and Set 
    | LAT -- Load and Toggle
    | LD -- Load Indirect from Data Space to Register using Index (X,Y,Z)
    | LDI -- Load Immediate 
    | LDS -- Load Direct from Data Space
    | LPM -- Load Program Memory
    | LSL -- Logical Shift Left
    | LSR -- Logical Shift Right
    | MOV -- Copy Register
    | MOVW -- Copy Word
    | MUL -- Multiply Unsigned
    | MULS -- Multiply Signed
    | MULSU -- Multiply Signed with Unsigned
    | NEG -- Two's complement
    | NOP -- No Operation
    | OR -- Logical OR
    | ORI -- Logical OR with immediate 
    | OUT -- Store Register in I/O Location
    | POP -- Pop register from Stack
    | PUSH -- Push register on Stack
    | RCALL -- Relative Call to subroutine
    | RET -- return from subroutine
    | RETI -- return from interrupt
    | RJMP -- relative jump 
    | ROL -- rotate left through carry
    | ROR -- rotate right through carry
    | SBC -- subtract with carry
    | SBCI -- subtract immediate with carry
    | SBI -- Set Bit in I/O Register
    | SBIC -- Skip if Bit in I/O Register is Cleared
    | SBIS -- Skip if Bit in I/O Register is Set
    | SBIW -- subtract immediate from word
    | SBR -- Set bits in register 
    | SBRC -- Skip if bit in register is cleared
    | SBRS -- Skip if bin in register is set
    | SEC -- set carry flag
    | SEH -- set half carry flag
    | SEI -- set global interrupt
    | SEN -- Set negative flag
    | SER -- set all bits in register
    | SES -- set signed flag
    | SET -- set T flag
    | SEV -- set overflow flag 
    | SEZ -- set zero flag 
    | SLEEP -- Sleep
    | SPM -- Store program memory 
    | ST -- store indirecto from register to data space 
    | STS -- store direct to data space
    | SUB -- subtract without carry
    | SUBI -- subtract immediate 
    | SWAP -- swap nibbles 
    | TST -- test fro zero or minus
    | WDR -- watchdog reset 
    | XCH -- exchange


