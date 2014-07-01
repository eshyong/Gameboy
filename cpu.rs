#![feature(macro_rules)]

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

macro_rules! ld {
    ($left:expr, $val:expr, $cpu:ident) => {
        match $left {
            "A"    => $cpu.regs.accum = $val,
            "B"    => $cpu.regs.b = $val,
            "C"    => $cpu.regs.c = $val,
            "D"    => $cpu.regs.d = $val,
            "E"    => $cpu.regs.e = $val,
            "H"    => $cpu.regs.h = $val,
            "L"    => $cpu.regs.l = $val,
            "(BC)" => { 
                let bc = $cpu.bc();
                $cpu.set_byte($val, bc);
            }
            "(DE)" => {
                let de = $cpu.de();
                $cpu.set_byte($val, de);
            }
            "(HL)" => {
                let hl = $cpu.hl();
                $cpu.set_byte($val, hl);
            }
            "(HL+)" => {
                let hl = $cpu.hl();
                $cpu.set_byte($val, hl);
                $cpu.inc_hl();
            }
            "(HL-)" => {
                let hl = $cpu.hl();
                $cpu.set_byte($val, hl);
                $cpu.inc_hl();
            }
            _ => println!("don't recognize that register"),
        };
    };
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
                sp: 0xFFFE,
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

    fn inc_bc(&mut self) {
        self.regs.c += 1;
        if self.regs.c == 0 {
            self.regs.b += 1;
        }
    }

    fn dec_bc(&mut self) {
        if self.regs.c == 0 {
            self.regs.b -= 1;
        }
        self.regs.c -= 1;
    }

    fn direct_addr_bc(&mut self) -> u8 {
        let bc = self.bc();
        self.get_byte(bc)
    }

    fn inc_de(&mut self) {
        self.regs.e += 1;
        if self.regs.d == 0 {
            self.regs.d += 1;
        }
    }

    fn dec_de(&mut self) {
        if self.regs.e == 0 {
            self.regs.d -= 1;
        }
        self.regs.e -= 1;
    }

    fn direct_addr_de(&mut self) -> u8 {
        let de = self.de();
        self.get_byte(de)
    }

    // Increment HL
    fn inc_hl(&mut self) {
        self.regs.l += 1;
        if self.regs.l == 0 {
            self.regs.h += 1;
        }
    }

    // Decrement HL
    fn dec_hl(&mut self) {
        if self.regs.l == 0 {
            self.regs.h -= 1;
        }
        self.regs.l -= 1;
    }
    
    fn direct_addr_hl(&mut self) -> u8 {
        let hl = self.direct_addr_hl();
        self.get_byte(hl as uint)
    }

    // Get HL with post decrement
    fn direct_addr_hl_dec(&mut self) -> u8 {
        let loc = self.hl();
        let val = self.get_byte(loc);
        self.dec_hl();
        val
    }

    // Get HL with post increment
    fn direct_addr_hl_inc(&mut self) -> u8 {
        let loc = self.hl();
        let val = self.get_byte(loc);
        self.inc_hl();
        val
    }

    // Increment the PC and get the next byte
    fn next_byte(&mut self) -> u8 {
        self.regs.pc += 1;
        self.mem[self.regs.pc as uint]
    }

    // Increment the PC twice and get the next two bytes as a short
    fn next_short(&mut self) -> u16 {
        let mut short = self.next_byte() as u16;
        short &= self.next_byte() as u16 << 8;
        short
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


    // Instructions

    fn inc_b(&mut self) {
        self.regs.b += 1;
        let result = self.regs.b == 0;
        self.set_flag(ZERO, result);
        self.set_flag(SUBTRACT, false);
    }

    fn inc_c(&mut self) {
        self.regs.c += 1;
        let result = self.regs.c == 0;
        self.set_flag(ZERO, result);
        self.set_flag(SUBTRACT, false);
    }

    fn inc_d(&mut self) {
        self.regs.d += 1;
        let result = self.regs.d == 0;
        self.set_flag(ZERO, result);
        self.set_flag(SUBTRACT, false);
    }

    fn inc_e(&mut self) {
        self.regs.e += 1;
        let result = self.regs.e == 0;
        self.set_flag(ZERO, result);
        self.set_flag(SUBTRACT, false);
    }

    fn inc_h(&mut self) {
        self.regs.h += 1;
        let result = self.regs.h == 0;
        self.set_flag(ZERO, result);
        self.set_flag(SUBTRACT, false);
    }

    fn inc_l(&mut self) {
        self.regs.l += 1;
        let result = self.regs.l == 0;
        self.set_flag(ZERO, result);
        self.set_flag(SUBTRACT, false);
    }

    fn inc_accum(&mut self) {
        self.regs.accum += 1;
        let result = self.regs.accum == 0;
        self.set_flag(ZERO, result);
        self.set_flag(SUBTRACT, false);
    }

    fn inc_sp(&mut self) {
        self.regs.sp += 1;
    }

    fn dec_sp(&mut self) {
        self.regs.sp -= 1;
    }

    fn inc_hl_deref(&mut self) {
        let hl = self.hl();
        let byte = self.get_byte(hl) + 1;
        self.set_byte(byte, hl);
        self.set_flag(ZERO, byte == 0);
        self.set_flag(SUBTRACT, false);
    }

    fn dec_b(&mut self) {
        self.regs.b -= 1;
        let result = self.regs.b == 0;
        self.set_flag(ZERO, result);
        self.set_flag(SUBTRACT, true);
    }

    fn dec_c(&mut self) {
        self.regs.c -= 1;
        let result = self.regs.c == 0;
        self.set_flag(ZERO, result);
        self.set_flag(SUBTRACT, true);
    }

    fn dec_d(&mut self) {
        self.regs.d -= 1;
        let result = self.regs.d == 0;
        self.set_flag(ZERO, result);
        self.set_flag(SUBTRACT, true);
    }

    fn dec_e(&mut self) {
        self.regs.e -= 1;
        let result = self.regs.e == 0;
        self.set_flag(ZERO, result);
        self.set_flag(SUBTRACT, true);
    }

    fn dec_h(&mut self) {
        self.regs.h -= 1;
        let result = self.regs.h == 0;
        self.set_flag(ZERO, result);
        self.set_flag(SUBTRACT, true);
    }

    fn dec_l(&mut self) {
        self.regs.l -= 1;
        let result = self.regs.l == 0;
        self.set_flag(ZERO, result);
        self.set_flag(SUBTRACT, true);
    }
    
    fn dec_accum(&mut self) {
        self.regs.accum -= 1;
        let result = self.regs.accum == 0;
        self.set_flag(ZERO, result);
        self.set_flag(SUBTRACT, true);
    }

    fn dec_hl_deref(&mut self) {
        let hl = self.hl();
        let byte = self.get_byte(hl) - 1;
        self.set_byte(byte, hl);
        self.set_flag(ZERO, byte == 0);
        self.set_flag(SUBTRACT, true);
    }

    fn jr_not_flag(&mut self, offset: u8, flag: u8) {
        if self.get_flag(flag) == 0 {
            self.rel_jmp(offset);
        }
    }

    fn jr_flag(&mut self, offset: u8, flag: u8) {
        if self.get_flag(flag) != 0 {
            self.rel_jmp(offset);
        }
    }

    fn rel_jmp(&mut self, offset: u8) {
        if offset & 0x80 != 0 {
            self.regs.pc -= (offset & 0x7F) as u16;
        } else {
            self.regs.pc += offset as u16;
        }
    }

    fn jp_not_flag(&mut self, address: u16, flag: u8) {
        if self.get_flag(flag) == 0 {
            self.abs_jmp(address);
        }
    }

    fn jp_flag(&mut self, address: u16, flag: u8) {
        if self.get_flag(flag) == 0 {
            self.abs_jmp(address);
        }
    }

    fn abs_jmp(&mut self, address: u16) {
        self.regs.pc = address;
    }

    // Adds a value to the accumulator, setting the carry and zero flags
    // as appropriate. Subtract flag is set to 0 when this is called.
    // TODO: half-carry flag
    fn add_accum(&mut self, val: u8) {
        let accum = self.regs.accum;
        self.set_flag(CARRY, (accum > 0xFF - val) || (val > 0xFF - accum));
        self.regs.accum += val;
        let result = self.regs.accum;
        self.set_flag(ZERO, result == 0);
        self.set_flag(SUBTRACT, false);
    }

    // Adds a signed value to the stack pointer, setting carry and half-carry flags
    // as appropriate. Subtract and zero flags are set to 0 when this is called.
    fn add_sp(&mut self, val: u8) {
        let sp = self.regs.sp;
        let mut carry = false;
        if val & 0x80 != 0 {
            self.regs.sp -= (val & 0x7F) as u16;
            carry = sp < (val & 0x7F) as u16;
        } else {
            self.regs.sp += val as u16;
            carry = sp > 0xFF - val as u16;
        }
        self.set_flag(ZERO, false);
        self.set_flag(SUBTRACT, false);
        self.set_flag(CARRY, carry);
    }

    // Adds a value with carry bit to the accumulator, setting the carry and zero flags
    // as appropriate. Subtract flag is set to 0 when this is called.
    // TODO: half-carry flag
    fn adc_accum(&mut self, val: u8) {
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
    fn sub_accum(&mut self, val: u8) {
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
    fn sbc_accum(&mut self, val: u8) {
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
    fn and_accum(&mut self, val: u8) {
        self.regs.accum &= val;
        let result = self.regs.accum;
        self.set_flag(ZERO, result == 0);
        self.set_flag(SUBTRACT, false);
        self.set_flag(CARRY, false);
    }

    // Performs a logical XOR on accumulator and val, setting the zero flag as appropriate.
    // Subtract, carry, and half-carry flags are all set to 0.
    fn xor_accum(&mut self, val: u8) {
        self.regs.accum ^= val;
        let result = self.regs.accum;
        self.set_flag(ZERO, result == 0);
        self.set_flag(SUBTRACT, false);
        self.set_flag(CARRY, false);
    }

    // Performs a logical OR on accumulator and val, setting the zero flag as appropriate.
    // Subtract, carry, and half-carry flags are all set to 0.
    fn or_accum(&mut self, val: u8) {
        self.regs.accum |= val;
        let result = self.regs.accum;
        self.set_flag(ZERO, result == 0);
        self.set_flag(SUBTRACT, false);
        self.set_flag(CARRY, false);
    }

    // Performs a logical compare on accumulator and val, storing result in 
    // Sets zero and carry flag as appropriate. Subtract flag is set to 1.
    fn cp_accum(&mut self, val: u8) {
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

    // Decode opcode! Game Boy is little endian, so immediate values (d8, d16, a8, a16, r8) are in reverse order.
    // i.e. the bytes x12 x34 would represent the address or 16-bit value 0x3412
    fn decode_op(&mut self, op: u8) {
        match op {
            // NOP
            0x00 => println!("bork"),

            // LD operations
            0x01 => { self.regs.c = self.next_byte(); self.regs.b = self.next_byte(); }                                       // LD BC, d16
            0x11 => { self.regs.e = self.next_byte(); self.regs.d = self.next_byte(); }                                       // LD DE, d16
            0x21 => { self.regs.l = self.next_byte(); self.regs.h = self.next_byte(); }                                       // LD HL, d16
            0x31 => { self.regs.sp = self.next_byte() as u16; self.regs.sp = (self.next_byte() << 8) as u16 & self.regs.sp; } // LD SP, d16

            0x02 => { let v = self.regs.accum; ld!("(BC)", v, self); }  // LD (BC), A
            0x12 => { let v = self.regs.accum; ld!("(DE)", v, self); }  // LD (DE), A
            0x22 => { let v = self.regs.accum; ld!("(HL+)", v, self); } // LD (HL+), A
            0x32 => { let v = self.regs.accum; ld!("(HL-)", v, self); } // LD (HL-), A

            0x06 => { self.regs.b = self.next_byte(); }                       // LD B, d8
            0x16 => { self.regs.d = self.next_byte(); }                       // LD D, d8
            0x26 => { self.regs.h = self.next_byte(); }                       // LD H, d8
            0x36 => { let byte = self.next_byte(); ld!("(HL)", byte, self); } // LD (HL), d8

            0x08 => {                                               // LD (a16), SP
                let sp = self.regs.sp;
                let mut loc = self.next_short() as uint;
                self.set_byte(sp as u8, loc);
                loc += 1;
                self.set_byte((sp >> 8) as u8, loc);
            }
            0xE0 => { let loc = 0xFF00 & (self.next_byte() as uint); let accum = self.regs.accum; self.set_byte(accum, loc); } // LD ($FF00 + a8), A
            0xF0 => { let loc = 0xFF00 & (self.next_byte() as uint); let byte = self.get_byte(loc); self.regs.accum = byte; }  // LD A, ($FF00 + a8)
            0xE2 => { let loc = 0xFF00 & (self.regs.c as uint); let accum = self.regs.accum; self.set_byte(accum, loc); }      // LD (C), A
            0xF2 => { let loc = 0xFF00 & (self.regs.c as uint); let byte = self.get_byte(loc); self.regs.accum = byte; }       // LD A, (C)

            0xEA => { let accum = self.regs.accum; let loc = self.next_short() as uint; self.set_byte(accum as u8, loc); } // LD (a16), A
            0xFA => { let loc = self.next_short() as uint; self.regs.accum = self.get_byte(loc); }                         // LD A, (a16)

            0x0A => { let v = self.direct_addr_bc(); ld!("A", v, self); }     // LD A, (BC)
            0x1A => { let v = self.direct_addr_de(); ld!("A", v, self); }     // LD A, (DE)
            0x2A => { let v = self.direct_addr_hl_inc(); ld!("A", v, self); } // LD A, (HL+)
            0x3A => { let v = self.direct_addr_hl_dec(); ld!("A", v, self); } // LD A, (HL-)

            0x40 => { let v = self.regs.b; ld!("B", v, self); } // LD B, B
            0x41 => { let v = self.regs.c; ld!("B", v, self); } // LD B, C
            0x42 => { let v = self.regs.d; ld!("B", v, self); } // LD B, D
            0x43 => { let v = self.regs.e; ld!("B", v, self); } // LD B, E
            0x44 => { let v = self.regs.h; ld!("B", v, self); } // LD B, H
            0x45 => { let v = self.regs.l; ld!("B", v, self); } // LD B, L
            0x46 => { let v = self.direct_addr_hl(); ld!("B", v, self); } // LD B, (HL)
            0x47 => { let v = self.regs.accum; ld!("B", v, self); }                       // LD B, A

            0x48 => { let v = self.regs.b; ld!("C", v, self); }                           // LD C, B
            0x49 => { let v = self.regs.c; ld!("C", v, self); }                           // LD C, C
            0x4A => { let v = self.regs.d; ld!("C", v, self); }                           // LD C, D
            0x4B => { let v = self.regs.e; ld!("C", v, self); }                           // LD C, E
            0x4C => { let v = self.regs.h; ld!("C", v, self); }                           // LD C, H
            0x4D => { let v = self.regs.l; ld!("C", v, self); }                           // LD C, L
            0x4E => { let v = self.direct_addr_hl(); ld!("C", v, self); } // LD C, (HL)
            0x4F => { let v = self.regs.accum; ld!("C", v, self); }                       // LD C, A

            0x50 => { let v = self.regs.b; ld!("D", v, self); }                           // LD D, B
            0x51 => { let v = self.regs.c; ld!("D", v, self); }                           // LD D, C
            0x52 => { let v = self.regs.d; ld!("D", v, self); }                           // LD D, D
            0x53 => { let v = self.regs.e; ld!("D", v, self); }                           // LD D, E
            0x54 => { let v = self.regs.h; ld!("D", v, self); }                           // LD D, H
            0x55 => { let v = self.regs.l; ld!("D", v, self); }                           // LD D, L
            0x56 => { let v = self.direct_addr_hl(); ld!("D", v, self); } // LD D, (HL)
            0x57 => { let v = self.regs.accum; ld!("D", v, self); }                       // LD D, A

            0x58 => { let v = self.regs.b; ld!("E", v, self); }           // LD E, B
            0x59 => { let v = self.regs.c; ld!("E", v, self); }           // LD E, C
            0x5A => { let v = self.regs.d; ld!("E", v, self); }           // LD E, D
            0x5B => { let v = self.regs.e; ld!("E", v, self); }           // LD E, E
            0x5C => { let v = self.regs.h; ld!("E", v, self); }           // LD E, H
            0x5D => { let v = self.regs.l; ld!("E", v, self); }           // LD E, L
            0x5E => { let v = self.direct_addr_hl(); ld!("E", v, self); } // LD E, (HL)
            0x5F => { let v = self.regs.accum; ld!("E", v, self); }       // LD E, A

            0x60 => { let v = self.regs.b; ld!("H", v, self); }           // LD H, B
            0x61 => { let v = self.regs.c; ld!("H", v, self); }           // LD H, C
            0x62 => { let v = self.regs.d; ld!("H", v, self); }           // LD H, D
            0x63 => { let v = self.regs.e; ld!("H", v, self); }           // LD H, E
            0x64 => { let v = self.regs.h; ld!("H", v, self); }           // LD H, H
            0x65 => { let v = self.regs.l; ld!("H", v, self); }           // LD H, L
            0x66 => { let v = self.direct_addr_hl(); ld!("H", v, self); } // LD H, (HL)
            0x67 => { let v = self.regs.accum; ld!("H", v, self); }       // LD H, A

            0x68 => { let v = self.regs.b; ld!("L", v, self); }           // LD L, B
            0x69 => { let v = self.regs.c; ld!("L", v, self); }           // LD L, C
            0x6A => { let v = self.regs.d; ld!("L", v, self); }           // LD L, D
            0x6B => { let v = self.regs.e; ld!("L", v, self); }           // LD L, E
            0x6C => { let v = self.regs.h; ld!("L", v, self); }           // LD L, H
            0x6D => { let v = self.regs.l; ld!("L", v, self); }           // LD L, L
            0x6E => { let v = self.direct_addr_hl(); ld!("L", v, self); } // LD L, (HL)
            0x6F => { let v = self.regs.accum; ld!("L", v, self); }       // LD L, A

            0x70 => { let v = self.regs.b; ld!("(HL)", v, self); } // LD (HL), B
            0x71 => { let v = self.regs.b; ld!("(HL)", v, self); } // LD (HL), C
            0x72 => { let v = self.regs.b; ld!("(HL)", v, self); } // LD (HL), D
            0x73 => { let v = self.regs.b; ld!("(HL)", v, self); } // LD (HL), E
            0x74 => { let v = self.regs.b; ld!("(HL)", v, self); } // LD (HL), H
            0x75 => { let v = self.regs.b; ld!("(HL)", v, self); } // LD (HL), L
            0x77 => { let v = self.regs.b; ld!("(HL)", v, self); } // LD (HL), A

            0x78 => { let v = self.regs.b; ld!("A", v, self); }           // LD A, B
            0x79 => { let v = self.regs.c; ld!("A", v, self); }           // LD A, C
            0x7A => { let v = self.regs.d; ld!("A", v, self); }           // LD A, D
            0x7B => { let v = self.regs.e; ld!("A", v, self); }           // LD A, E
            0x7C => { let v = self.regs.h; ld!("A", v, self); }           // LD A, H
            0x7D => { let v = self.regs.l; ld!("A", v, self); }           // LD A, L
            0x7E => { let v = self.direct_addr_hl(); ld!("A", v, self); } // LD A, (HL)
            0x7F => { let v = self.regs.accum; ld!("A", v, self); }       // LD A, A

            // INC/DEC instructions
            0x03 => { self.inc_bc(); } // INC BC
            0x13 => { self.inc_de(); } // INC DE
            0x23 => { self.inc_hl(); } // INC HL
            0x33 => { self.inc_sp(); } // INC SP
            
            0x04 => { self.inc_b(); }        // INC B
            0x14 => { self.inc_d(); }        // INC D
            0x24 => { self.inc_h(); }        // INC H
            0x34 => { self.inc_hl_deref(); } // INC (HL)

            0x05 => { self.dec_b(); }        // DEC B
            0x15 => { self.dec_d(); }        // DEC D
            0x25 => { self.dec_h(); }        // DEC H
            0x35 => { self.dec_hl_deref(); } // DEC (HL)

            0x0B => { self.dec_bc(); } // DEC BC
            0x1B => { self.dec_de(); } // DEC DE
            0x2B => { self.dec_hl(); } // DEC HL
            0x3B => { self.dec_sp(); } // DEC SP

            0x0C => { self.inc_c(); }     // INC C
            0x1C => { self.inc_e(); }     // INC E
            0x2C => { self.inc_l(); }     // INC L
            0x3C => { self.inc_accum(); } // INC A

            0x0D => { self.dec_c(); }     // DEC C
            0x1D => { self.dec_e(); }     // DEC E
            0x2D => { self.dec_l(); }     // DEC L
            0x3D => { self.dec_accum(); } // DEC A

            // ADD instructions
            0x80 => { let val = self.regs.b; self.add_accum(val); }           // ADD A, B
            0x81 => { let val = self.regs.c; self.add_accum(val); }           // ADD A, C
            0x82 => { let val = self.regs.d; self.add_accum(val); }           // ADD A, D
            0x83 => { let val = self.regs.e; self.add_accum(val); }           // ADD A, E
            0x84 => { let val = self.regs.h; self.add_accum(val); }           // ADD A, H
            0x85 => { let val = self.regs.l; self.add_accum(val); }           // ADD A, L
            0x86 => { let val = self.direct_addr_hl(); self.add_accum(val); } // ADD A, (HL)
            0x87 => { let val = self.regs.accum; self.add_accum(val); }       // ADD A, A
            0xC6 => { let val = self.next_byte(); self.add_accum(val); }      // ADD A, d8
            0xE8 => { let val = self.next_byte(); self.add_sp(val); }         // ADD SP, r8

            // ADC instructions
            0x88 => { let val = self.regs.b; self.adc_accum(val); }           // ADC A, B
            0x89 => { let val = self.regs.c; self.adc_accum(val); }           // ADC A, C
            0x8A => { let val = self.regs.d; self.adc_accum(val); }           // ADC A, D
            0x8B => { let val = self.regs.e; self.adc_accum(val); }           // ADC A, E
            0x8C => { let val = self.regs.h; self.adc_accum(val); }           // ADC A, H
            0x8D => { let val = self.regs.l; self.adc_accum(val); }           // ADC A, L
            0x8E => { let val = self.direct_addr_hl(); self.adc_accum(val); } // ADC A, (HL)
            0x8F => { let val = self.regs.accum; self.adc_accum(val); }       // ADC A, A
            0xCE => { let val = self.next_byte(); self.adc_accum(val); }      // ADC A, d8
            
            // SUB instructions
            0x90 => { let val = self.regs.b; self.sub_accum(val); }                           // SUB A, B
            0x91 => { let val = self.regs.c; self.sub_accum(val); }                           // SUB A, C
            0x92 => { let val = self.regs.d; self.sub_accum(val); }                           // SUB A, D
            0x93 => { let val = self.regs.e; self.sub_accum(val); }                           // SUB A, E
            0x94 => { let val = self.regs.h; self.sub_accum(val); }                           // SUB A, H
            0x95 => { let val = self.regs.l; self.sub_accum(val); }                           // SUB A, L
            0x96 => { let val = self.direct_addr_hl(); self.sub_accum(val); } // SUB A, (HL)
            0x97 => { let val = self.regs.accum; self.sub_accum(val); }                       // SUB A, A
            0xD6 => { let val = self.next_byte(); self.sub_accum(val); }                      // SUB A, d8

            // SBC instructions
            0x98 => { let val = self.regs.b; self.sbc_accum(val); }                           // SBC A, B
            0x99 => { let val = self.regs.c; self.sbc_accum(val); }                           // SBC A, C
            0x9A => { let val = self.regs.d; self.sbc_accum(val); }                           // SBC A, D
            0x9B => { let val = self.regs.e; self.sbc_accum(val); }                           // SBC A, E
            0x9C => { let val = self.regs.h; self.sbc_accum(val); }                           // SBC A, H
            0x9D => { let val = self.regs.l; self.sbc_accum(val); }                           // SBC A, L
            0x9E => { let val = self.direct_addr_hl(); self.sbc_accum(val); } // SBC A, (HL)
            0x9F => { let val = self.regs.accum; self.sbc_accum(val); }                       // SBC A, A
            0xDE => { let val = self.next_byte(); self.sbc_accum(val); }                      // SBC A, d8

            // AND instructions
            0xA0 => { let val = self.regs.b; self.and_accum(val); }                           // AND A, B
            0xA1 => { let val = self.regs.c; self.and_accum(val); }                           // AND A, C
            0xA2 => { let val = self.regs.d; self.and_accum(val); }                           // AND A, D
            0xA3 => { let val = self.regs.e; self.and_accum(val); }                           // AND A, E
            0xA4 => { let val = self.regs.h; self.and_accum(val); }                           // AND A, H
            0xA5 => { let val = self.regs.l; self.and_accum(val); }                           // AND A, L
            0xA6 => { let val = self.direct_addr_hl(); self.and_accum(val); } // AND A, (HL)
            0xA7 => { let val = self.regs.accum; self.and_accum(val); }                       // AND A, A
            0xE6 => { let val = self.next_byte(); self.and_accum(val); }                      // AND A, d8

            // XOR instructions
            0xA8 => { let val = self.regs.b; self.xor_accum(val); }                           // XOR A, B
            0xA9 => { let val = self.regs.c; self.xor_accum(val); }                           // XOR A, C
            0xAA => { let val = self.regs.d; self.xor_accum(val); }                           // XOR A, D
            0xAB => { let val = self.regs.e; self.xor_accum(val); }                           // XOR A, E
            0xAC => { let val = self.regs.h; self.xor_accum(val); }                           // XOR A, H
            0xAD => { let val = self.regs.l; self.xor_accum(val); }                           // XOR A, L
            0xAE => { let val = self.direct_addr_hl(); self.xor_accum(val); } // XOR A, (HL)
            0xAF => { let val = self.regs.accum; self.xor_accum(val); }                       // XOR A, A
            0xEE => { let val = self.next_byte(); self.xor_accum(val); }                      // XOR A, d8

            // OR instructions
            0xB0 => { let val = self.regs.b; self.or_accum(val); }                           // OR A, B
            0xB1 => { let val = self.regs.c; self.or_accum(val); }                           // OR A, C
            0xB2 => { let val = self.regs.d; self.or_accum(val); }                           // OR A, D
            0xB3 => { let val = self.regs.e; self.or_accum(val); }                           // OR A, E
            0xB4 => { let val = self.regs.h; self.or_accum(val); }                           // OR A, H
            0xB5 => { let val = self.regs.l; self.or_accum(val); }                           // OR A, L
            0xB6 => { let val = self.direct_addr_hl(); self.or_accum(val); } // OR A, (HL)
            0xB7 => { let val = self.regs.accum; self.or_accum(val); }                       // OR A, A
            0xF6 => { let val = self.next_byte(); self.or_accum(val); }                      // OR A, d8

            // CP instructions
            0xB8 => { let val = self.regs.b; self.cp_accum(val); }                           // CP A, B
            0xB9 => { let val = self.regs.c; self.cp_accum(val); }                           // CP A, C
            0xBA => { let val = self.regs.d; self.cp_accum(val); }                           // CP A, D
            0xBB => { let val = self.regs.e; self.cp_accum(val); }                           // CP A, E
            0xBC => { let val = self.regs.h; self.cp_accum(val); }                           // CP A, H
            0xBD => { let val = self.regs.l; self.cp_accum(val); }                           // CP A, L
            0xBE => { let val = self.direct_addr_hl(); self.cp_accum(val); } // CP A, (HL)
            0xBF => { let val = self.regs.accum; self.cp_accum(val); }                       // CP A, A
            0xFE => { let val = self.next_byte(); self.cp_accum(val); }                      // CP A, d8

            // JR instructions
            0x20 => { let offset = self.next_byte(); self.jr_not_flag(offset, ZERO); }  // JR NZ, r8
            0x30 => { let offset = self.next_byte(); self.jr_not_flag(offset, CARRY); } // JR NC, r8
            0x18 => { let offset = self.next_byte(); self.rel_jmp(offset); }            // JR r8
            0x28 => { let offset = self.next_byte(); self.jr_flag(offset, ZERO); }      // JR Z, r8
            0x38 => { let offset = self.next_byte(); self.jr_flag(offset, CARRY); }     // JR C, r8

            // JP instructions
            0xC2 => { let address = self.next_short(); self.jp_not_flag(address, ZERO); }  // JP NZ, a16
            0xD2 => { let address = self.next_short(); self.jp_not_flag(address, CARRY); } // JP NC, a16
            0xC3 => { let address = self.next_short(); self.abs_jmp(address); }            // JP a16
            0xCA => { let address = self.next_short(); self.jp_flag(address, ZERO); }      // JP Z, a16
            0xDA => { let address = self.next_short(); self.jp_flag(address, CARRY); }     // JP C, a16

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
