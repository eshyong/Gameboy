struct Regs {
    // Accumulator and flags (AF):
    // Flags are as follows:
    // Z N H C 0 0 0 0
    // Z = zero flag: set when the result of an instruction is 0.
    // N = subtract flag: set after a subtract operation
    // H = half-carry flag: 
    // C = carry flag: 
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

    // Set the zero flag
    fn set_zero(&mut self) {
        self.regs.flags |= 0x80;
    }

    // Set the subtract flag
    fn set_subtract(&mut self) {
        self.regs.flags |= 0x40;
    }

    fn set_half_carry(&mut self) {
        self.regs.flags |= 0x20;
    }
    
    fn set_carry(&mut self) {
        self.regs.flags |= 0x10;
    }

    fn reset_zero(&mut self) {
        self.regs.flags &= 0x7F;
    }

    fn reset_subtract(&mut self) {
        self.regs.flags &= 0xBF;
    }

    fn reset_half_carry(&mut self) {
        self.regs.flags &= 0xDF;
    }

    fn reset_carry(&mut self) {
        self.regs.flags &= 0xEF;
    }

    fn decode_op(&mut self) {
        let op = self.mem[self.regs.pc as uint];
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

            0x08 => { // LD (a16), SP
                let sp = self.regs.sp;
                let mut loc = self.next_byte() as uint;
                self.set_byte(((sp & 0xFF00) >> 8) as u8, loc);
                loc = self.next_byte() as uint;
                self.set_byte(sp as u8, loc);
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
            
            0x04 => { self.regs.b += 1; if self.regs.b == 0 { self.set_zero(); } self.reset_subtract(); } // INC B
            0x14 => { self.regs.d += 1; if self.regs.d == 0 { self.set_zero(); } self.reset_subtract(); } // INC D
            0x24 => { self.regs.h += 1; if self.regs.h == 0 { self.set_zero(); } self.reset_subtract(); } // INC H
            0x34 => {                                                                                     // INC (HL)
                let hl = self.hl();
                self.mem[hl] += 1;
                if self.get_byte(hl) == 0 {
                    self.set_zero();
                }
                self.reset_subtract();
            }

            0x05 => { self.regs.b -= 1; if self.regs.b == 0 { self.set_zero(); } self.set_subtract(); } // DEC B
            0x15 => { self.regs.d -= 1; if self.regs.d == 0 { self.set_zero(); } self.set_subtract(); } // DEC D
            0x25 => { self.regs.h -= 1; if self.regs.h == 0 { self.set_zero(); } self.set_subtract(); } // DEC H
            0x35 => {                                                                                   // DEC (HL)
                let hl = self.hl();
                self.mem[hl] -= 1;
                if self.get_byte(hl) == 0 {
                    self.set_zero();
                }
                self.set_subtract();
            }

            0x0B => { self.regs.c -= 1; if self.regs.c == 0 { self.regs.b -= 1 } } // DEC BC
            0x1B => { self.regs.e -= 1; if self.regs.e == 0 { self.regs.d -= 1 } } // DEC DE
            0x2B => { self.regs.h -= 1; if self.regs.h == 0 { self.regs.l -= 1 } } // DEC HL
            0x3B => { self.regs.sp -= 1; }                                         // DEC SP

            0x0C => { self.regs.c += 1; if self.regs.c == 0 { self.set_zero(); } self.reset_subtract(); }         // INC C
            0x1C => { self.regs.e += 1; if self.regs.e == 0 { self.set_zero(); } self.reset_subtract(); }         // INC E
            0x2C => { self.regs.l += 1; if self.regs.l == 0 { self.set_zero(); } self.reset_subtract(); }         // INC L
            0x3C => { self.regs.accum += 1; if self.regs.accum == 0 { self.set_zero(); } self.reset_subtract(); } // INC A

            0x0D => { self.regs.c -= 1; if self.regs.c == 0 { self.set_zero(); } self.set_subtract(); }         // DEC C
            0x1D => { self.regs.e -= 1; if self.regs.e == 0 { self.set_zero(); } self.set_subtract(); }         // DEC E
            0x2D => { self.regs.l -= 1; if self.regs.l == 0 { self.set_zero(); } self.set_subtract(); }         // DEC L
            0x3D => { self.regs.accum -= 1; if self.regs.accum == 0 { self.set_zero(); } self.set_subtract(); } // DEC A

            _ => println!("Opcode not implemented yet.")
        };
        self.regs.pc += 1;
    }
}

fn main() {
    let mut cpu: CPU = CPU::new();
    println!("It compiles!");
    cpu.dump_registers();
    cpu.decode_op();
}

// End of cpu.rs
