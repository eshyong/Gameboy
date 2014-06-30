static ZERO:       u8 = 0x80;
static SUBTRACT:   u8 = 0x40;
static HALF_CARRY: u8 = 0x20;
static CARRY:      u8 = 0x10;

struct Regs {
    // Accumulator and flags (AF):
    // Flags are as follows:
    // Z N H C 0 0 0 0
    // Z = zero flag: set when the result of an instruction is 0.
    // N = subtract flag: set after a subtract operation
    // H = half-carry flag: 
    // C = carry flag: set after a result leads to a carry
    // bits 3 - 0: always set to 0
    accum: u8,
    flags: u8,

    // General purpose registers: 
    // These are 8-bit registers, but can be addressed together as 16 bit registers.
    // e.g. (BC), (DE), and (HL)
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    h: u8,
    l: u8,

    // Stack pointer and program counter:
    // These keep track of the stack and the next instruction to be fetched, respectively.
    sp: u16,
    pc: u16,
}

struct CPU {
    regs: Regs,
    mem: [u8, ..0x10000],
}

impl CPU {
    fn new() -> CPU {
        CPU {
            // Set everything to 0
            regs: Regs {
                accum: 0,
                flags: 0,
                b: 0,
                c: 0,
                d: 0,
                e: 0,
                h: 0,
                l: 0,
                sp: 0,
                pc: 0,
            },
            mem: [0, ..0x10000],
        }
    }

    // Print out register values for debugging
    fn dump_registers(&self) {
        println!("accumulator: {}", self.regs.accum);
        println!("flags: {}", self.regs.flags);
        println!("b: {}", self.regs.b);
        println!("c: {}", self.regs.c);
        println!("d: {}", self.regs.d);
        println!("e: {}", self.regs.e);
        println!("h: {}", self.regs.h);
        println!("l: {}", self.regs.l);
        println!("sp: {}", self.regs.sp);
        println!("pc: {}", self.regs.pc);
    }

    // Get BC as a 16 bit value
    fn bc (&mut self) -> uint {
        ((self.regs.b << 8) & self.regs.c) as uint
    }

    // Get DE as a 16 bit value
    fn de(&mut self) -> uint {
        ((self.regs.d << 8) & self.regs.e) as uint
    }

    // Get HL as a 16 bit value
    fn hl(&mut self) -> uint {
        ((self.regs.h << 8) & self.regs.l) as uint
    }

    // Get HL with post decrement
    fn hl_dec(&mut self) -> uint {
        let val = ((self.regs.h << 8) & self.regs.l) as uint;
        if self.regs.l == 0 {
            self.regs.h -= 1;
        }
        self.regs.l -= 1;
        val
    }

    // Get HL with post increment
    fn hl_inc(&mut self) -> uint {
        let val = ((self.regs.h << 8) & self.regs.l) as uint;
        self.regs.l += 1;
        if self.regs.l == 0 {
            self.regs.h += 1;
        }
        val
    }

    // Increment the PC and gets the next byte
    fn next_byte(&mut self) -> u8 {
        self.regs.pc += 1;
        self.mem[self.regs.pc as uint]
    }

    // Get byte in memory specified by location
    fn get_byte(&self, location: uint) -> u8 {
        self.mem[location]
    }

    // Set memory value at location to byte
    fn set_byte(&mut self, byte: u8, location: uint) {
        self.mem[location] = byte;
    }

    // Sets flag using boolean on, taking in a bit position as input.
    fn set_flag(&mut self, flag: u8, on: bool) {
        if on {
            self.regs.flags |= flag;
        } else {
            self.regs.flags &= !flag;
        }
    }

    fn get_flag(&self, flag: u8) -> u8 {
        (self.regs.flags & flag != 0) as u8
    }

    // Adds a value to the accumulator, setting the carry and zero flags
    // as appropriate. Subtract flag is set to 0 when this is called.
    // TODO: half-carry flag
    fn add(&mut self, val: u8) {
        let accum = self.regs.accum;
        self.set_flag(CARRY, (accum > 0xFF - val) || (val > 0xFF - accum));
        
        self.regs.accum += val;

        let result = self.regs.accum;
        self.set_flag(ZERO, result == 0);
        self.set_flag(SUBTRACT, false);
    }

    // Adds a value with carry bit to the accumulator, setting the carry and zero flags
    // as appropriate. Subtract flag is set to 0 when this is called.
    // TODO: half-carry flag
    fn adc(&mut self, val: u8) {
        let accum = self.regs.accum;
        let carry = self.get_flag(CARRY);
        self.set_flag(CARRY, (accum + carry > 0xFF - val) || (val + carry > 0xFF - accum));

        self.regs.accum += val + carry;

        let result = self.regs.accum;
        self.set_flag(ZERO, result == 0);
        self.set_flag(SUBTRACT, false);
    }

    // Subtracts a value from the accumulator, setting the carry and zero flags 
    // as appropriate. Subtract flag is set to 1 when this is called.
    // TODO: half-carry flag
    fn sub(&mut self, val: u8) {
        let accum = self.regs.accum;
        self.set_flag(CARRY, accum < val);

        self.regs.accum -= val;

        let result = self.regs.accum;
        self.set_flag(ZERO, result == 0);
        self.set_flag(SUBTRACT, true);
    }

    // Subtracts a value and carry bit from the accumulator, setting the carry and zero flags
    // as appropriate. Subtract flag is set to 1 when this is called.
    // TODO: half-carry flag
    fn sbc(&mut self, val: u8) {
        let accum = self.regs.accum;
        let carry = self.get_flag(CARRY);
        self.set_flag(CARRY, accum < val + carry);

        self.regs.accum -= val + carry;

        let result = self.regs.accum;
        self.set_flag(ZERO, result == 0);
        self.set_flag(SUBTRACT, true);
    }

    // Performs logical AND on accumulator and val, setting the zero flag as appropriate.
    // Half-carry flag is set to 1, while subtract and carry flags are set to 0.
    fn and(&mut self, val: u8) {
        self.regs.accum &= val;
        let result = self.regs.accum;
        self.set_flag(ZERO, result == 0);
        self.set_flag(SUBTRACT, false);
        self.set_flag(CARRY, false);
    }

    // Performs a logical XOR on accumulator and val, setting the zero flag as appropriate.
    // Subtract, carry, and half-carry flags are all set to 0.
    fn xor(&mut self, val: u8) {
        self.regs.accum ^= val;
        let result = self.regs.accum;
        self.set_flag(ZERO, result == 0);
        self.set_flag(SUBTRACT, false);
        self.set_flag(CARRY, false);
    }

    // Performs a logical OR on accumulator and val, setting the zero flag as appropriate.
    // Subtract, carry, and half-carry flags are all set to 0.
    fn or(&mut self, val: u8) {
        self.regs.accum |= val;
        let result = self.regs.accum;
        self.set_flag(ZERO, result == 0);
        self.set_flag(SUBTRACT, false);
        self.set_flag(CARRY, false);
    }

    // Performs a logical compare on accumulator and val, storing result in 
    // Sets zero and carry flag as appropriate. Subtract flag is set to 1.
    fn cp(&mut self, val: u8) {
        let accum = self.regs.accum;
        let result = accum - val;
        self.set_flag(ZERO, result == 0);
        self.set_flag(SUBTRACT, true);
        self.set_flag(CARRY, accum >= val);
    }

    // Executes one fetch-decode-execute cycle on the CPU
    fn step(&mut self) {
        let op = self.mem[self.regs.pc as uint];
        self.decode_op(op);
        self.regs.pc += 1;
    }

    fn decode_op(&mut self, op: u8) {
        match op {
            // NOP
            0x00 => println!("bork"),

            // LD operations
            0x01 => { self.regs.b = self.next_byte(); self.regs.c = self.next_byte(); }                                       // LD BC, d16
            0x11 => { self.regs.d = self.next_byte(); self.regs.e = self.next_byte(); }                                       // LD DE, d16
            0x21 => { self.regs.h = self.next_byte(); self.regs.l = self.next_byte(); }                                       // LD HL, d16
            0x31 => { self.regs.sp = self.next_byte() as u16; self.regs.sp = (self.regs.sp << 8) & self.next_byte() as u16; } // LD SP, d16

            0x02 => { let bc = self.bc(); let byte = self.regs.accum; self.set_byte(byte, bc); }     // LD (BC), A
            0x12 => { let de = self.de(); let byte = self.regs.accum; self.set_byte(byte, de); }     // LD (DE), A
            0x22 => { let hl = self.hl_inc(); let byte = self.regs.accum; self.set_byte(byte, hl); } // LD (HL+), A
            0x32 => { let hl = self.hl_dec(); let byte = self.regs.accum; self.set_byte(byte, hl); } // LD (HL-), A

            0x06 => { self.regs.b = self.next_byte(); }                                           // LD B, d8
            0x16 => { self.regs.d = self.next_byte(); }                                           // LD D, d8
            0x26 => { self.regs.h = self.next_byte(); }                                           // LD H, d8
            0x36 => { let byte = self.next_byte(); let hl = self.hl(); self.set_byte(byte, hl); } // LD (HL), d8

            0x08 => {                                               // LD (a16), SP
                let sp = self.regs.sp;
                let mut loc = self.next_byte() as uint;
                self.set_byte(((sp & 0xFF00) >> 8) as u8, loc);
                loc = self.next_byte() as uint;
                self.set_byte(sp as u8, loc);
            }
            0xE0 => { let loc = 0xFF00 & (self.next_byte() as uint); let accum = self.regs.accum; self.set_byte(accum, loc); } // LD ($FF00 + a8), A
            0xF0 => { let loc = 0xFF00 & (self.next_byte() as uint); let byte = self.get_byte(loc); self.regs.accum = byte; }  // LD A, ($FF00 + a8)
            0xE2 => { let loc = 0xFF00 & (self.regs.c as uint); let accum = self.regs.accum; self.set_byte(accum, loc); }      // LD (C), A
            0xF2 => { let loc = 0xFF00 & (self.regs.c as uint); let byte = self.get_byte(loc); self.regs.accum = byte; }       // LD A, (C)

            0xEA => {                                           // LD (a16), A
                let mut loc = self.next_byte() as uint; 
                loc = (loc << 8) & self.next_byte() as uint; 
                let byte = self.regs.accum;
                self.set_byte(byte, loc);
            }
            0xFA => {                                           // LD A, (a16)
                let mut loc = self.next_byte() as uint;
                loc = (loc << 8) & self.next_byte() as uint;
                self.regs.accum = self.get_byte(loc);
            }

            0x0A => { let bc = self.bc(); self.regs.accum = self.get_byte(bc); }     // LD A, (BC)
            0x1A => { let de = self.de(); self.regs.accum = self.get_byte(de); }     // LD A, (DE)
            0x2A => { let hl = self.hl_inc(); self.regs.accum = self.get_byte(hl); } // LD A, (HL+)
            0x3A => { let hl = self.hl_dec(); self.regs.accum = self.get_byte(hl); } // LD A, (HL-)

            0x40 => { self.regs.b = self.regs.b; }                           // LD B, B
            0x41 => { self.regs.b = self.regs.c; }                           // LD B, C
            0x42 => { self.regs.b = self.regs.d; }                           // LD B, D
            0x43 => { self.regs.b = self.regs.e; }                           // LD B, E
            0x44 => { self.regs.b = self.regs.h; }                           // LD B, H
            0x45 => { self.regs.b = self.regs.l; }                           // LD B, L
            0x46 => { let hl = self.hl(); self.regs.b = self.get_byte(hl); } // LD B, (HL)
            0x47 => { self.regs.b = self.regs.accum; }                       // LD B, A

            0x48 => { self.regs.c = self.regs.b; }                           // LD C, B
            0x49 => { self.regs.c = self.regs.c; }                           // LD C, C
            0x4A => { self.regs.c = self.regs.d; }                           // LD C, D
            0x4B => { self.regs.c = self.regs.e; }                           // LD C, E
            0x4C => { self.regs.c = self.regs.h; }                           // LD C, H
            0x4D => { self.regs.c = self.regs.l; }                           // LD C, L
            0x4E => { let hl = self.hl(); self.regs.c = self.get_byte(hl); } // LD C, (HL)
            0x4F => { self.regs.c = self.regs.accum; }                       // LD C, A

            0x50 => { self.regs.d = self.regs.b; }                           // LD D, B
            0x51 => { self.regs.d = self.regs.c; }                           // LD D, C
            0x52 => { self.regs.d = self.regs.d; }                           // LD D, D
            0x53 => { self.regs.d = self.regs.e; }                           // LD D, E
            0x54 => { self.regs.d = self.regs.h; }                           // LD D, H
            0x55 => { self.regs.d = self.regs.l; }                           // LD D, L
            0x56 => { let hl = self.hl(); self.regs.d = self.get_byte(hl); } // LD D, (HL)
            0x57 => { self.regs.d = self.regs.accum; }                       // LD D, A

            0x58 => { self.regs.e = self.regs.b; }                           // LD E, B
            0x59 => { self.regs.e = self.regs.c; }                           // LD E, C
            0x5A => { self.regs.e = self.regs.d; }                           // LD E, D
            0x5B => { self.regs.e = self.regs.e; }                           // LD E, E
            0x5C => { self.regs.e = self.regs.h; }                           // LD E, H
            0x5D => { self.regs.e = self.regs.l; }                           // LD E, L
            0x5E => { let hl = self.hl(); self.regs.e = self.get_byte(hl); } // LD E, (HL)
            0x5F => { self.regs.e = self.regs.accum; }                       // LD E, A

            0x60 => { self.regs.h = self.regs.b; }                           // LD H, B
            0x61 => { self.regs.h = self.regs.c; }                           // LD H, C
            0x62 => { self.regs.h = self.regs.d; }                           // LD H, D
            0x63 => { self.regs.h = self.regs.e; }                           // LD H, E
            0x64 => { self.regs.h = self.regs.h; }                           // LD H, H
            0x65 => { self.regs.h = self.regs.l; }                           // LD H, L
            0x66 => { let hl = self.hl(); self.regs.h = self.get_byte(hl); } // LD H, (HL)
            0x67 => { self.regs.h = self.regs.accum; }                       // LD H, A

            0x68 => { self.regs.l = self.regs.b; }                           // LD L, B
            0x69 => { self.regs.l = self.regs.c; }                           // LD L, C
            0x6A => { self.regs.l = self.regs.d; }                           // LD L, D
            0x6B => { self.regs.l = self.regs.e; }                           // LD L, E
            0x6C => { self.regs.l = self.regs.h; }                           // LD L, H
            0x6D => { self.regs.l = self.regs.l; }                           // LD L, L
            0x6E => { let hl = self.hl(); self.regs.l = self.get_byte(hl); } // LD L, (HL)
            0x6F => { self.regs.l = self.regs.accum; }                       // LD L, A

            0x70 => { let hl = self.hl(); let byte = self.regs.b; self.set_byte(byte, hl); } // LD (HL), B
            0x71 => { let hl = self.hl(); let byte = self.regs.c; self.set_byte(byte, hl); } // LD (HL), C
            0x72 => { let hl = self.hl(); let byte = self.regs.d; self.set_byte(byte, hl); } // LD (HL), D
            0x73 => { let hl = self.hl(); let byte = self.regs.e; self.set_byte(byte, hl); } // LD (HL), E
            0x74 => { let hl = self.hl(); let byte = self.regs.h; self.set_byte(byte, hl); } // LD (HL), H
            0x75 => { let hl = self.hl(); let byte = self.regs.l; self.set_byte(byte, hl); } // LD (HL), L

            0x77 => { let hl = self.hl(); let byte = self.regs.accum; self.set_byte(byte, hl); } // LD (HL), A
            0x78 => { self.regs.accum = self.regs.b; }                                           // LD A, B
            0x79 => { self.regs.accum = self.regs.c; }                                           // LD A, C
            0x7A => { self.regs.accum = self.regs.d; }                                           // LD A, D
            0x7B => { self.regs.accum = self.regs.e; }                                           // LD A, E
            0x7C => { self.regs.accum = self.regs.h; }                                           // LD A, H
            0x7D => { self.regs.accum = self.regs.l; }                                           // LD A, L
            0x7E => { let hl = self.hl(); self.regs.accum = self.get_byte(hl); }                 // LD A, (HL)
            0x7F => { self.regs.accum = self.regs.accum; }                                       // LD A, A

            // INC/DEC instructions
            0x03 => { self.regs.c += 1; if self.regs.c == 0 { self.regs.b += 1; } } // INC BC
            0x13 => { self.regs.e += 1; if self.regs.e == 0 { self.regs.d += 1; } } // INC DE
            0x23 => { self.regs.l += 1; if self.regs.l == 0 { self.regs.h += 1; } } // INC HL
            0x33 => { self.regs.sp += 1; }                                          // INC SP
            
            0x04 => { self.regs.b += 1; if self.regs.b == 0 { self.set_flag(ZERO, true); } self.set_flag(SUBTRACT, false); } // INC B
            0x14 => { self.regs.d += 1; if self.regs.d == 0 { self.set_flag(ZERO, true); } self.set_flag(SUBTRACT, false); } // INC D
            0x24 => { self.regs.h += 1; if self.regs.h == 0 { self.set_flag(ZERO, true); } self.set_flag(SUBTRACT, false); } // INC H
            0x34 => {                                                                                     // INC (HL)
                let hl = self.hl();
                let byte = self.get_byte(hl) + 1;
                self.set_byte(byte, hl);
                if byte == 0 {
                    self.set_flag(ZERO, true);
                }
                self.set_flag(SUBTRACT, false);
            }

            0x05 => { self.regs.b -= 1; if self.regs.b == 0 { self.set_flag(ZERO, true); } self.set_flag(SUBTRACT, true); } // DEC B
            0x15 => { self.regs.d -= 1; if self.regs.d == 0 { self.set_flag(ZERO, true); } self.set_flag(SUBTRACT, true); } // DEC D
            0x25 => { self.regs.h -= 1; if self.regs.h == 0 { self.set_flag(ZERO, true); } self.set_flag(SUBTRACT, true); } // DEC H
            0x35 => {                                                                                                       // DEC (HL)
                let hl = self.hl();
                let byte = self.get_byte(hl) - 1;
                self.set_byte(byte, hl);
                if byte == 0 {
                    self.set_flag(ZERO, true);
                }
                self.set_flag(SUBTRACT, true);
            }

            0x0B => { self.regs.c -= 1; if self.regs.c == 0 { self.regs.b -= 1 } } // DEC BC
            0x1B => { self.regs.e -= 1; if self.regs.e == 0 { self.regs.d -= 1 } } // DEC DE
            0x2B => { self.regs.h -= 1; if self.regs.h == 0 { self.regs.l -= 1 } } // DEC HL
            0x3B => { self.regs.sp -= 1; }                                         // DEC SP

            0x0C => { self.regs.c += 1; if self.regs.c == 0 { self.set_flag(ZERO, true); } self.set_flag(SUBTRACT, false); }         // INC C
            0x1C => { self.regs.e += 1; if self.regs.e == 0 { self.set_flag(ZERO, true); } self.set_flag(SUBTRACT, false); }         // INC E
            0x2C => { self.regs.l += 1; if self.regs.l == 0 { self.set_flag(ZERO, true); } self.set_flag(SUBTRACT, false); }         // INC L
            0x3C => { self.regs.accum += 1; if self.regs.accum == 0 { self.set_flag(ZERO, true); } self.set_flag(SUBTRACT, false); } // INC A

            0x0D => { self.regs.c -= 1; if self.regs.c == 0 { self.set_flag(ZERO, true); } self.set_flag(SUBTRACT, true); }         // DEC C
            0x1D => { self.regs.e -= 1; if self.regs.e == 0 { self.set_flag(ZERO, true); } self.set_flag(SUBTRACT, true); }         // DEC E
            0x2D => { self.regs.l -= 1; if self.regs.l == 0 { self.set_flag(ZERO, true); } self.set_flag(SUBTRACT, true); }         // DEC L
            0x3D => { self.regs.accum -= 1; if self.regs.accum == 0 { self.set_flag(ZERO, true); } self.set_flag(SUBTRACT, true); } // DEC A

            // ADD instructions
            0x80 => { let val = self.regs.b; self.add(val); }                           // ADD B
            0x81 => { let val = self.regs.c; self.add(val); }                           // ADD C
            0x82 => { let val = self.regs.d; self.add(val); }                           // ADD D
            0x83 => { let val = self.regs.e; self.add(val); }                           // ADD E
            0x84 => { let val = self.regs.h; self.add(val); }                           // ADD H
            0x85 => { let val = self.regs.l; self.add(val); }                           // ADD L
            0x86 => { let hl = self.hl(); let val = self.get_byte(hl); self.add(val); } // ADD (HL)
            0x87 => { let val = self.regs.accum; self.add(val); }                       // ADD A
            0xC6 => { let val = self.next_byte(); self.add(val); }                      // ADD d8

            // ADC instructions
            0x88 => { let val = self.regs.b; self.adc(val); }                           // ADC B
            0x89 => { let val = self.regs.c; self.adc(val); }                           // ADC C
            0x8A => { let val = self.regs.d; self.adc(val); }                           // ADC D
            0x8B => { let val = self.regs.e; self.adc(val); }                           // ADC E
            0x8C => { let val = self.regs.h; self.adc(val); }                           // ADC H
            0x8D => { let val = self.regs.l; self.adc(val); }                           // ADC L
            0x8E => { let hl = self.hl(); let val = self.get_byte(hl); self.adc(val); } // ADC (HL)
            0x8F => { let val = self.regs.accum; self.adc(val); }                       // ADC A
            0xCE => { let val = self.next_byte(); self.adc(val); }                      // ADC d8
            
            // SUB instructions
            0x90 => { let val = self.regs.b; self.sub(val); }                           // SUB B
            0x91 => { let val = self.regs.c; self.sub(val); }                           // SUB C
            0x92 => { let val = self.regs.d; self.sub(val); }                           // SUB D
            0x93 => { let val = self.regs.e; self.sub(val); }                           // SUB E
            0x94 => { let val = self.regs.h; self.sub(val); }                           // SUB H
            0x95 => { let val = self.regs.l; self.sub(val); }                           // SUB L
            0x96 => { let hl = self.hl(); let val = self.get_byte(hl); self.sub(val); } // SUB (HL)
            0x97 => { let val = self.regs.accum; self.sub(val); }                       // SUB A
            0xD6 => { let val = self.next_byte(); self.sub(val); }                      // SUB d8

            // SBC instructions
            0x98 => { let val = self.regs.b; self.sbc(val); }                           // SBC B
            0x99 => { let val = self.regs.c; self.sbc(val); }                           // SBC C
            0x9A => { let val = self.regs.d; self.sbc(val); }                           // SBC D
            0x9B => { let val = self.regs.e; self.sbc(val); }                           // SBC E
            0x9C => { let val = self.regs.h; self.sbc(val); }                           // SBC H
            0x9D => { let val = self.regs.l; self.sbc(val); }                           // SBC L
            0x9E => { let hl = self.hl(); let val = self.get_byte(hl); self.sbc(val); } // SBC (HL)
            0x9F => { let val = self.regs.accum; self.sbc(val); }                       // SBC A
            0xDE => { let val = self.next_byte(); self.sbc(val); }                      // SBC d8

            // AND instructions
            0xA0 => { let val = self.regs.b; self.and(val); }                           // AND B
            0xA1 => { let val = self.regs.c; self.and(val); }                           // AND C
            0xA2 => { let val = self.regs.d; self.and(val); }                           // AND D
            0xA3 => { let val = self.regs.e; self.and(val); }                           // AND E
            0xA4 => { let val = self.regs.h; self.and(val); }                           // AND H
            0xA5 => { let val = self.regs.l; self.and(val); }                           // AND L
            0xA6 => { let hl = self.hl(); let val = self.get_byte(hl); self.and(val); } // AND (HL)
            0xA7 => { let val = self.regs.accum; self.and(val); }                       // AND A
            0xE6 => { let val = self.next_byte(); self.and(val); }                      // AND d8

            // XOR instructions
            0xA8 => { let val = self.regs.b; self.xor(val); }                           // XOR B
            0xA9 => { let val = self.regs.c; self.xor(val); }                           // XOR C
            0xAA => { let val = self.regs.d; self.xor(val); }                           // XOR D
            0xAB => { let val = self.regs.e; self.xor(val); }                           // XOR E
            0xAC => { let val = self.regs.h; self.xor(val); }                           // XOR H
            0xAD => { let val = self.regs.l; self.xor(val); }                           // XOR L
            0xAE => { let hl = self.hl(); let val = self.get_byte(hl); self.xor(val); } // XOR (HL)
            0xAF => { let val = self.regs.accum; self.xor(val); }                       // XOR A
            0xEE => { let val = self.next_byte(); self.xor(val); }                      // XOR d8

            // OR instructions
            0xB0 => { let val = self.regs.b; self.or(val); }                           // OR B
            0xB1 => { let val = self.regs.c; self.or(val); }                           // OR C
            0xB2 => { let val = self.regs.d; self.or(val); }                           // OR D
            0xB3 => { let val = self.regs.e; self.or(val); }                           // OR E
            0xB4 => { let val = self.regs.h; self.or(val); }                           // OR H
            0xB5 => { let val = self.regs.l; self.or(val); }                           // OR L
            0xB6 => { let hl = self.hl(); let val = self.get_byte(hl); self.or(val); } // OR (HL)
            0xB7 => { let val = self.regs.accum; self.or(val); }                       // OR A
            0xF6 => { let val = self.next_byte(); self.or(val); }                      // OR d8

            // CP instructions
            0xB8 => { let val = self.regs.b; self.cp(val); }                           // CP B
            0xB9 => { let val = self.regs.c; self.cp(val); }                           // CP C
            0xBA => { let val = self.regs.d; self.cp(val); }                           // CP D
            0xBB => { let val = self.regs.e; self.cp(val); }                           // CP E
            0xBC => { let val = self.regs.h; self.cp(val); }                           // CP H
            0xBD => { let val = self.regs.l; self.cp(val); }                           // CP L
            0xBE => { let hl = self.hl(); let val = self.get_byte(hl); self.cp(val); } // CP (HL)
            0xBF => { let val = self.regs.accum; self.cp(val); }                       // CP A
            0xFE => { let val = self.next_byte(); self.cp(val); }                      // CP d8

            _ => println!("Opcode not implemented yet.")
        };
    }
}

fn main() {
    let mut cpu: CPU = CPU::new();
    println!("It compiles!");
    cpu.dump_registers();
    cpu.decode_op(0x80);
    cpu.dump_registers();
    cpu.decode_op(0x81);
    cpu.dump_registers();
    cpu.decode_op(0x8F);
    cpu.dump_registers();
}

// End of cpu.rs
