#![feature(macro_rules)]
#![crate_id = "cpu"]

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

impl Regs {
    fn zero(&mut self) {
        self.accum = 0;
        self.flags = 0;
        self.b = 0;
        self.c = 0;
        self.d = 0;
        self.e = 0;
        self.h = 0;
        self.l = 0;
        self.sp = 0;
        self.pc = 0;
    }
}

pub struct CPU {
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
                $cpu.inc("HL");
            }
            "(HL-)" => {
                let hl = $cpu.hl();
                $cpu.set_byte($val, hl);
                $cpu.dec("HL");
            }
            _ => { }
        };
    };
}

impl CPU {
    pub fn new() -> CPU {
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
    pub fn dump_registers(&self) {
        println!("accumulator: 0x{:x}", self.regs.accum);
        println!("flags: 0x{:x}", self.regs.flags);
        println!("b: 0x{:x}", self.regs.b);
        println!("c: 0x{:x}", self.regs.c);
        println!("d: 0x{:x}", self.regs.d);
        println!("e: 0x{:x}", self.regs.e);
        println!("h: 0x{:x}", self.regs.h);
        println!("l: 0x{:x}", self.regs.l);
        println!("sp: 0x{:x}", self.regs.sp);
        println!("pc: 0x{:x}", self.regs.pc);
    }

    pub fn dump_mem(&self) {
        println!("all memory");
        let length = self.mem.len();
        for i in range(0, length) {
            print!("0x{:x} ", self.mem[i]);
        }
        println!("");
    }

    pub fn dump_some_mem(&self, low: uint, high: uint) {
        println!("memory from range {} to {}", low, high);
        for i in range(low, high) {
            print!("0x{:x} ", self.mem[i]);
        }
        println!("");
    }

    // Get BC as an unsigned integer
    fn bc(&mut self) -> uint {
        ((self.regs.b << 8) & self.regs.c) as uint
    }

    // Get DE as an unsigned integer
    fn de(&mut self) -> uint {
        ((self.regs.d << 8) & self.regs.e) as uint
    }

    // Get HL as an unsigned integer
    fn hl(&mut self) -> uint {
        let val = ((self.regs.h << 8) & self.regs.l) as uint;
        println!("HL: 0x{:x}", val);
        val
        //((self.regs.h << 8) & self.regs.l) as uint
    }
    
    // Get AF (accumulator flags) as an unsigned integer
    fn af(&mut self) -> uint {
        ((self.regs.accum << 8) & self.regs.flags) as uint
    }

    fn direct_addr(&mut self, registers: &'static str) -> u8 {
        let mut loc = 0x0000;
        match registers {
            "BC"    => { loc = self.bc() }
            "DE"    => { loc = self.de() }
            "HL"    => { loc = self.hl() }
            "(HL-)" => { loc = self.hl(); self.dec("HL"); }
            "(HL+)" => { loc = self.hl(); self.inc("HL"); }
            _       => { }
        };
        self.get_byte(loc)
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
    pub fn get_byte(&self, location: uint) -> u8 {
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

    fn get_flag(&self, flag: u8) -> bool {
        self.regs.flags & flag != 0
    }

    // Instructions

    fn inc(&mut self, register: &'static str) {
        let mut result = false;
        match register {
            "A" => { self.regs.accum += 1; result = self.regs.accum == 0; }
            "B" => { self.regs.b += 1; result = self.regs.b == 0; }
            "C" => { self.regs.c += 1; result = self.regs.c == 0; }
            "D" => { self.regs.d += 1; result = self.regs.d == 0; }
            "E" => { self.regs.e += 1; result = self.regs.e == 0; }
            "H" => { self.regs.h += 1; result = self.regs.h == 0; }
            "L" => { self.regs.l += 1; result = self.regs.l == 0; }
            "(HL)" => {
                let hl = self.hl();
                let val = self.get_byte(hl) + 1;
                self.set_byte(val, hl);
                result = self.get_byte(hl) == 0;
            }
            _   => { }
        };
        self.set_flag(ZERO, result);
        self.set_flag(SUBTRACT, false);
    }

    fn inc16(&mut self, registers: &'static str) {
        match registers {
            "BC" => { self.regs.c += 1; if self.regs.c == 0 { self.regs.b += 1; } }
            "DE" => { self.regs.e += 1; if self.regs.e == 0 { self.regs.d += 1; } }
            "HL" => { self.regs.l += 1; if self.regs.l == 0 { self.regs.h += 1; } }
            "SP" => { self.regs.sp += 1; }
            _    => { }
        };
    }
    
    fn dec(&mut self, register: &'static str) {
        let mut result = false;
        match register {
            "A"    => { self.regs.accum -= 1; result = self.regs.accum == 0; }
            "B"    => { self.regs.b -= 1; result = self.regs.b == 0; }
            "C"    => { self.regs.c -= 1; result = self.regs.c == 0; }
            "D"    => { self.regs.d -= 1; result = self.regs.d == 0; }
            "E"    => { self.regs.e -= 1; result = self.regs.e == 0; }
            "H"    => { self.regs.h -= 1; result = self.regs.h == 0; }
            "L"    => { self.regs.l -= 1; result = self.regs.l == 0; }
            "(HL)" => {
                let hl = self.hl();
                let val = self.get_byte(hl) - 1;
                self.set_byte(val, hl);
                result = self.get_byte(hl) == 0;
            }
            _      => { }
        };
        self.set_flag(ZERO, result);
        self.set_flag(SUBTRACT, true);
    }

    fn dec16(&mut self, registers: &'static str) {
        match registers {
            "BC" => { if self.regs.c == 0 { self.regs.b -= 1; } self.regs.c -= 1; }
            "DE" => { if self.regs.e == 0 { self.regs.d -= 1; } self.regs.e -= 1; }
            "HL" => { if self.regs.l == 0 { self.regs.h -= 1; } self.regs.l -= 1; }
            "SP" => { self.regs.sp -= 1; }
            _    => { }
        };
    }

    fn jr_not_flag(&mut self, offset: u8, flag: u8) {
        if !self.get_flag(flag) {
            self.rel_jmp(offset);
        }
    }

    fn jr_flag(&mut self, offset: u8, flag: u8) {
        if self.get_flag(flag) {
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
        if !self.get_flag(flag) {
            self.abs_jmp(address);
        }
    }

    fn jp_flag(&mut self, address: u16, flag: u8) {
        if self.get_flag(flag) {
            self.abs_jmp(address);
        }
    }

    fn abs_jmp(&mut self, address: u16) {
        self.regs.pc = address;
    }

    fn ret_not_flag(&mut self, flag: u8) {
        if !self.get_flag(flag) {
            self.ret();
        }
    }

    fn ret_flag(&mut self, flag: u8) {
        if self.get_flag(flag) {
            self.ret();
        }
    }

    fn ret(&mut self) {
        self.regs.pc = self.regs.sp;
        self.regs.sp += 2;
    }

    fn reti(&mut self) {
        self.ret();
        self.ei();
    }

    fn call_not_flag(&mut self, address: u16, flag: u8) {
        if !self.get_flag(flag) {
            self.call(address);
        }
    }

    fn call_flag(&mut self, address: u16, flag: u8) {
        if self.get_flag(flag) {
            self.call(address);
        }
    }

    fn call(&mut self, address: u16) {
        self.regs.sp -= 2;
        self.regs.sp = self.regs.pc;
        self.regs.pc = address;
    }

    fn push(&mut self, address: u16) {
        self.regs.sp -= 2;
        self.regs.sp = address;
    }

    fn pop(&mut self, registers: &'static str) {
        let sp = self.regs.sp;
        match registers {
            "BC" => { self.regs.b = (sp >> 8) as u8; self.regs.c = sp as u8; }
            "DE" => { self.regs.d = (sp >> 8) as u8; self.regs.e = sp as u8; }
            "HL" => { self.regs.h = (sp >> 8) as u8; self.regs.l = sp as u8; }
            "AF" => { self.regs.accum = (sp >> 8) as u8; self.regs.flags = sp as u8; }
            _ => println!("pop instruction not recognized: {}", registers),
        }
    }

    // Enable interrupts (set ($FFFF) to 1)
    fn ei(&mut self) {
        self.mem[0xFFFF] = 1;
    }

    // Disable interrupts
    fn di(&mut self) {
        self.mem[0xFFFF] = 0;
    }

    // 

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

    // Adds another register to HL. Sets carry flag as appropriate, subtract flag
    // is set to 0 when this is called.
    fn add_hl(&mut self, registers: &'static str) {
        let mut carry = false;
        match registers {
            "BC" => {
                if self.regs.l > 0xFF - self.regs.c {
                    self.regs.h += 1;
                }
                self.regs.l += self.regs.c;
                if self.regs.h > 0xFF - self.regs.b {
                    self.regs.l += self.regs.h - 0xFF + self.regs.b;
                    carry = true;
                }
                self.regs.h += self.regs.b;
            }
            "DE" => {
                if self.regs.l > 0xFF - self.regs.e {
                    self.regs.h += 1;
                }
                self.regs.l += self.regs.e;
                if self.regs.h > 0xFF - self.regs.d {
                    self.regs.l += self.regs.h - 0xFF + self.regs.d;
                    carry = true;
                }
                self.regs.h += self.regs.d;
            }
            "HL" => {
                if self.regs.l > 0xFF - self.regs.l {
                    self.regs.h += 1;
                }
                self.regs.l += self.regs.l;
                if self.regs.h > 0xFF - self.regs.h {
                    self.regs.l += self.regs.h - 0xFF + self.regs.h;
                    carry = true;
                }
                self.regs.h += self.regs.h;
            }
            "SP" => { 
                let high = (self.regs.sp >> 8) as u8;
                let low = self.regs.sp as u8;
                if self.regs.l > 0xFF - low {
                    self.regs.h += 1;
                }
                self.regs.l += low;
                if self.regs.h > 0xFF - high {
                    self.regs.l += self.regs.h - 0xFF + high;
                    carry = true;
                }
                self.regs.h += high;
            }
            _ => { }
        };
        self.set_flag(CARRY, carry);
        self.set_flag(SUBTRACT, false);
    }

    // Adds a value with carry bit to the accumulator, setting the carry and zero flags
    // as appropriate. Subtract flag is set to 0 when this is called.
    // TODO: half-carry flag
    fn adc_accum(&mut self, val: u8) {
        let accum = self.regs.accum;
        let carry = self.get_flag(CARRY) as u8;
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
        let carry = self.get_flag(CARRY) as u8;
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

    // Rotate bits right through carry in accumulator. 
    // Bit zero will rotate into bit seven as follows:
    //   ________________________________________
    //  |                                        |
    //  -> 7 -> 6 -> 5 -> 4 -> 3 -> 2 -> 1 -> 0 ---> C
    fn rrca(&mut self) {
        let bit = self.regs.accum & 0x1 != 0;
        self.regs.accum >>= 1;
        if bit {
            self.regs.accum |= 0x80;
        }
        self.set_flag(CARRY, bit);
        self.set_flag(ZERO, false);
        self.set_flag(SUBTRACT, false);
    }

    // Rotate bits right in accumulator.
    // Carry bit rotates into bit seven as follows:
    //  ____________________________________________
    // |                                            |
    // -> 7 -> 6 -> 5 -> 4 -> 3 -> 2 -> 1 -> 0 ---> C
    fn rra(&mut self) {
        let bit = self.regs.accum & 0x1 != 0;
        let carry = self.get_flag(CARRY);
        self.regs.accum >>= 1;
        if carry {
            self.regs.accum |= 0x80;
        }
        self.set_flag(CARRY, bit);
        self.set_flag(ZERO, false);
        self.set_flag(SUBTRACT, false);
    }

    // Rotate bits left through carry in accumulator. Inverse of RRCA.
    //       __________________________________________
    //      |                                          |
    //  C <--- 7 <- 6 <- 5 <- 4 <- 3 <- 2 <- 1 <- 0 <---
    fn rlca(&mut self) {
        let bit = self.regs.accum & 0x80 != 0;
        self.regs.accum <<= 1;
        if bit {
            self.regs.accum |= 0x1;
        }
        self.set_flag(CARRY, bit);
        self.set_flag(ZERO, false);
        self.set_flag(SUBTRACT, false);
    }

    // Rotate bits left in accumulator. Inverse of RRA.
    //   ______________________________________________
    //  |                                              |
    //  C <--- 7 <- 6 <- 5 <- 4 <- 3 <- 2 <- 1 <- 0 <---
    fn rla(&mut self) {
        let bit = self.regs.accum & 0x80 != 0;
        let carry = self.get_flag(CARRY);
        self.regs.accum <<= 1;
        if carry {
            self.regs.accum |= 0x1;
        }
        self.set_flag(CARRY, bit);
        self.set_flag(ZERO, false);
        self.set_flag(SUBTRACT, false);
    }

    fn daa(&mut self) {

    }

    // Set carry flag.
    fn scf(&mut self) {
        self.set_flag(SUBTRACT, false);
        self.set_flag(CARRY, true);
    }

    // XOR accumulator with 0xFF.
    fn cpl(&mut self) {
        self.regs.accum ^= 0xFF;
        self.set_flag(SUBTRACT, true);
    }

    // XOR carry flag with 0x1.
    fn ccf(&mut self) {
        let carry = self.get_flag(CARRY);
        self.set_flag(CARRY, (carry as u8 ^ 1) != 0);
        self.set_flag(SUBTRACT, false);
    }

    fn rst(&mut self, address: &'static str) {
        match address {
            "00H" => { self.call(0x0000); }
            "08H" => { self.call(0x0008); }
            "10H" => { self.call(0x0010); }
            "18H" => { self.call(0x0018); }
            "20H" => { self.call(0x0020); }
            "28H" => { self.call(0x0028); }
            "30H" => { self.call(0x0030); }
            "38H" => { self.call(0x0038); }
            _     => { }
        }
    }

    pub fn load_code(&mut self, code: &[u8]) {
        assert!(code.len() < 0x8000);
        let length = code.len();
        for i in range(0, length) {
            self.mem[i] = code[i];
        }
    }

    // Executes one fetch-decode-execute cycle on the CPU
    pub fn step(&mut self, debug: bool) {
        let op = self.mem[self.regs.pc as uint];
        if (debug) {
            println!("opcode: {:x}", op);
        }
        self.decode_op(op);
        self.regs.pc += 1;
    }

    pub fn reset(&mut self) {
        self.regs.zero();
        for i in range(0, self.mem.len()) {
            self.mem[i] = 0;
        }
    }

    // Decode opcode! Game Boy is little endian, so immediate values (d8, d16, a8, a16, r8) are in reverse order.
    // i.e. the bytes x12 x34 would represent the address or 16-bit value 0x3412
    fn decode_op(&mut self, op: u8) {
        match op {
            // NOP
            0x00 => { }

            // LD operations
            0x01 => { self.regs.c = self.next_byte(); self.regs.b = self.next_byte(); } // LD BC, d16
            0x11 => { self.regs.e = self.next_byte(); self.regs.d = self.next_byte(); } // LD DE, d16
            0x21 => { self.regs.l = self.next_byte(); self.regs.h = self.next_byte(); } // LD HL, d16
            0x31 => { self.regs.sp = self.next_short(); }                               // LD SP, d16

            0x02 => { let v = self.regs.accum; ld!("(BC)", v, self); }  // LD (BC), A
            0x12 => { let v = self.regs.accum; ld!("(DE)", v, self); }  // LD (DE), A
            0x22 => { let v = self.regs.accum; ld!("(HL+)", v, self); } // LD (HL+), A
            0x32 => { let v = self.regs.accum; ld!("(HL-)", v, self); } // LD (HL-), A

            0x06 => { let v = self.next_byte(); ld!("B", v, self); }       // LD B, d8
            0x16 => { let v = self.next_byte(); ld!("D", v, self); }       // LD D, d8
            0x26 => { let v = self.next_byte(); ld!("H", v, self); }       // LD H, d8
            0x36 => { let v = self.next_byte(); ld!("(HL)", v, self); } // LD (HL), d8

            0x0E => { let v = self.next_byte(); ld!("C", v, self); } // LD C, d8
            0x1E => { let v = self.next_byte(); ld!("E", v, self); } // LD E, d8
            0x2E => { let v = self.next_byte(); ld!("L", v, self); } // LD L, d8
            0x3E => { let v = self.next_byte(); ld!("A", v, self); } // LD A, d8

            0x08 => { // LD (a16), SP
                let sp = self.regs.sp;
                let mut loc = self.next_short() as uint;
                self.set_byte(sp as u8, loc);
                loc += 1;
                self.set_byte((sp >> 8) as u8, loc);
            }
            0xE0 => { // LD ($FF00 + a8), A
                let loc = 0xFF00 & (self.next_byte() as uint); 
                let accum = self.regs.accum; 
                self.set_byte(accum, loc); 
            }
            0xF0 => { // LD A, ($FF00 + a8) 
                let loc = 0xFF00 & (self.next_byte() as uint); 
                let byte = self.get_byte(loc); 
                self.regs.accum = byte; 
            } 
            0xE2 => { // LD (C), A
                let loc = 0xFF00 & (self.regs.c as uint); 
                let accum = self.regs.accum; 
                self.set_byte(accum, loc); 
            }
            0xF2 => { // LD A, (C)
                let loc = 0xFF00 & (self.regs.c as uint); 
                let byte = self.get_byte(loc); 
                self.regs.accum = byte; 
            }
            0xEA => { // LD (a16), A
                let accum = self.regs.accum; 
                let loc = self.next_short() as uint; 
                self.set_byte(accum as u8, loc); 
            }
            0xFA => { // LD A, (a16)
                let loc = self.next_short() as uint; 
                self.regs.accum = self.get_byte(loc); 
            }

            0x0A => { let v = self.direct_addr("BC"); ld!("A", v, self); }    // LD A, (BC)
            0x1A => { let v = self.direct_addr("DE"); ld!("A", v, self); }    // LD A, (DE)
            0x2A => { let v = self.direct_addr("(HL+)"); ld!("A", v, self); } // LD A, (HL+)
            0x3A => { let v = self.direct_addr("(HL-)"); ld!("A", v, self); } // LD A, (HL-)

            0x40 => { let v = self.regs.b; ld!("B", v, self); }            // LD B, B
            0x41 => { let v = self.regs.c; ld!("B", v, self); }            // LD B, C
            0x42 => { let v = self.regs.d; ld!("B", v, self); }            // LD B, D
            0x43 => { let v = self.regs.e; ld!("B", v, self); }            // LD B, E
            0x44 => { let v = self.regs.h; ld!("B", v, self); }            // LD B, H
            0x45 => { let v = self.regs.l; ld!("B", v, self); }            // LD B, L
            0x46 => { let v = self.direct_addr("HL"); ld!("B", v, self); } // LD B, (HL)
            0x47 => { let v = self.regs.accum; ld!("B", v, self); }        // LD B, A

            0x48 => { let v = self.regs.b; ld!("C", v, self); }            // LD C, B
            0x49 => { let v = self.regs.c; ld!("C", v, self); }            // LD C, C
            0x4A => { let v = self.regs.d; ld!("C", v, self); }            // LD C, D
            0x4B => { let v = self.regs.e; ld!("C", v, self); }            // LD C, E
            0x4C => { let v = self.regs.h; ld!("C", v, self); }            // LD C, H
            0x4D => { let v = self.regs.l; ld!("C", v, self); }            // LD C, L
            0x4E => { let v = self.direct_addr("HL"); ld!("C", v, self); } // LD C, (HL)
            0x4F => { let v = self.regs.accum; ld!("C", v, self); }        // LD C, A

            0x50 => { let v = self.regs.b; ld!("D", v, self); }            // LD D, B
            0x51 => { let v = self.regs.c; ld!("D", v, self); }            // LD D, C
            0x52 => { let v = self.regs.d; ld!("D", v, self); }            // LD D, D
            0x53 => { let v = self.regs.e; ld!("D", v, self); }            // LD D, E
            0x54 => { let v = self.regs.h; ld!("D", v, self); }            // LD D, H
            0x55 => { let v = self.regs.l; ld!("D", v, self); }            // LD D, L
            0x56 => { let v = self.direct_addr("HL"); ld!("D", v, self); } // LD D, (HL)
            0x57 => { let v = self.regs.accum; ld!("D", v, self); }        // LD D, A

            0x58 => { let v = self.regs.b; ld!("E", v, self); }            // LD E, B
            0x59 => { let v = self.regs.c; ld!("E", v, self); }            // LD E, C
            0x5A => { let v = self.regs.d; ld!("E", v, self); }            // LD E, D
            0x5B => { let v = self.regs.e; ld!("E", v, self); }            // LD E, E
            0x5C => { let v = self.regs.h; ld!("E", v, self); }            // LD E, H
            0x5D => { let v = self.regs.l; ld!("E", v, self); }            // LD E, L
            0x5E => { let v = self.direct_addr("HL"); ld!("E", v, self); } // LD E, (HL)
            0x5F => { let v = self.regs.accum; ld!("E", v, self); }        // LD E, A

            0x60 => { let v = self.regs.b; ld!("H", v, self); }            // LD H, B
            0x61 => { let v = self.regs.c; ld!("H", v, self); }            // LD H, C
            0x62 => { let v = self.regs.d; ld!("H", v, self); }            // LD H, D
            0x63 => { let v = self.regs.e; ld!("H", v, self); }            // LD H, E
            0x64 => { let v = self.regs.h; ld!("H", v, self); }            // LD H, H
            0x65 => { let v = self.regs.l; ld!("H", v, self); }            // LD H, L
            0x66 => { let v = self.direct_addr("HL"); ld!("H", v, self); } // LD H, (HL)
            0x67 => { let v = self.regs.accum; ld!("H", v, self); }        // LD H, A

            0x68 => { let v = self.regs.b; ld!("L", v, self); }            // LD L, B
            0x69 => { let v = self.regs.c; ld!("L", v, self); }            // LD L, C
            0x6A => { let v = self.regs.d; ld!("L", v, self); }            // LD L, D
            0x6B => { let v = self.regs.e; ld!("L", v, self); }            // LD L, E
            0x6C => { let v = self.regs.h; ld!("L", v, self); }            // LD L, H
            0x6D => { let v = self.regs.l; ld!("L", v, self); }            // LD L, L
            0x6E => { let v = self.direct_addr("HL"); ld!("L", v, self); } // LD L, (HL)
            0x6F => { let v = self.regs.accum; ld!("L", v, self); }        // LD L, A

            0x70 => { let v = self.regs.b; ld!("(HL)", v, self); } // LD (HL), B
            0x71 => { let v = self.regs.b; ld!("(HL)", v, self); } // LD (HL), C
            0x72 => { let v = self.regs.b; ld!("(HL)", v, self); } // LD (HL), D
            0x73 => { let v = self.regs.b; ld!("(HL)", v, self); } // LD (HL), E
            0x74 => { let v = self.regs.b; ld!("(HL)", v, self); } // LD (HL), H
            0x75 => { let v = self.regs.b; ld!("(HL)", v, self); } // LD (HL), L
            0x77 => { let v = self.regs.b; ld!("(HL)", v, self); } // LD (HL), A

            0x78 => { let v = self.regs.b; ld!("A", v, self); }            // LD A, B
            0x79 => { let v = self.regs.c; ld!("A", v, self); }            // LD A, C
            0x7A => { let v = self.regs.d; ld!("A", v, self); }            // LD A, D
            0x7B => { let v = self.regs.e; ld!("A", v, self); }            // LD A, E
            0x7C => { let v = self.regs.h; ld!("A", v, self); }            // LD A, H
            0x7D => { let v = self.regs.l; ld!("A", v, self); }            // LD A, L
            0x7E => { let v = self.direct_addr("HL"); ld!("A", v, self); } // LD A, (HL)
            0x7F => { let v = self.regs.accum; ld!("A", v, self); }        // LD A, A

            // INC/DEC instructions
            0x03 => { self.inc16("BC"); } // INC BC
            0x13 => { self.inc16("DE"); } // INC DE
            0x23 => { self.inc16("HL"); } // INC HL
            0x33 => { self.inc16("SP"); } // INC SP
            
            0x04 => { self.inc("B"); }    // INC B
            0x14 => { self.inc("D"); }    // INC D
            0x24 => { self.inc("H"); }    // INC H
            0x34 => { self.inc("(HL)"); } // INC (HL)

            0x05 => { self.dec("B"); }    // DEC B
            0x15 => { self.dec("D"); }    // DEC D
            0x25 => { self.dec("H"); }    // DEC H
            0x35 => { self.dec("(HL)"); } // DEC (HL)

            0x0B => { self.dec16("BC"); } // DEC BC
            0x1B => { self.dec16("DE"); } // DEC DE
            0x2B => { self.dec16("HL"); } // DEC HL
            0x3B => { self.dec16("SP"); } // DEC SP

            0x0C => { self.inc("C"); } // INC C
            0x1C => { self.inc("E"); } // INC E
            0x2C => { self.inc("L"); } // INC L
            0x3C => { self.inc("A"); } // INC A

            0x0D => { self.dec("C"); } // DEC C
            0x1D => { self.dec("E"); } // DEC E
            0x2D => { self.dec("L"); } // DEC L
            0x3D => { self.dec("A"); } // DEC A
            0x27 => { self.daa(); }    // DAA

            // ADD instructions
            0x80 => { let val = self.regs.b; self.add_accum(val); }            // ADD A, B
            0x81 => { let val = self.regs.c; self.add_accum(val); }            // ADD A, C
            0x82 => { let val = self.regs.d; self.add_accum(val); }            // ADD A, D
            0x83 => { let val = self.regs.e; self.add_accum(val); }            // ADD A, E
            0x84 => { let val = self.regs.h; self.add_accum(val); }            // ADD A, H
            0x85 => { let val = self.regs.l; self.add_accum(val); }            // ADD A, L
            0x86 => { let val = self.direct_addr("HL"); self.add_accum(val); } // ADD A, (HL)
            0x87 => { let val = self.regs.accum; self.add_accum(val); }        // ADD A, A
            0xC6 => { let val = self.next_byte(); self.add_accum(val); }       // ADD A, d8
            0xE8 => { let val = self.next_byte(); self.add_sp(val); }          // ADD SP, r8
            0x09 => { self.add_hl("BC"); }                                     // ADD HL, BC
            0x19 => { self.add_hl("DE"); }                                     // ADD HL, DE
            0x29 => { self.add_hl("HL"); }                                     // ADD HL, HL
            0x39 => { self.add_hl("SP"); }                                     // ADD HL, SP

            // ADC instructions
            0x88 => { let val = self.regs.b; self.adc_accum(val); }            // ADC A, B
            0x89 => { let val = self.regs.c; self.adc_accum(val); }            // ADC A, C
            0x8A => { let val = self.regs.d; self.adc_accum(val); }            // ADC A, D
            0x8B => { let val = self.regs.e; self.adc_accum(val); }            // ADC A, E
            0x8C => { let val = self.regs.h; self.adc_accum(val); }            // ADC A, H
            0x8D => { let val = self.regs.l; self.adc_accum(val); }            // ADC A, L
            0x8E => { let val = self.direct_addr("HL"); self.adc_accum(val); } // ADC A, (HL)
            0x8F => { let val = self.regs.accum; self.adc_accum(val); }        // ADC A, A
            0xCE => { let val = self.next_byte(); self.adc_accum(val); }       // ADC A, d8
            
            // SUB instructions
            0x90 => { let val = self.regs.b; self.sub_accum(val); }            // SUB A, B
            0x91 => { let val = self.regs.c; self.sub_accum(val); }            // SUB A, C
            0x92 => { let val = self.regs.d; self.sub_accum(val); }            // SUB A, D
            0x93 => { let val = self.regs.e; self.sub_accum(val); }            // SUB A, E
            0x94 => { let val = self.regs.h; self.sub_accum(val); }            // SUB A, H
            0x95 => { let val = self.regs.l; self.sub_accum(val); }            // SUB A, L
            0x96 => { let val = self.direct_addr("HL"); self.sub_accum(val); } // SUB A, (HL)
            0x97 => { let val = self.regs.accum; self.sub_accum(val); }        // SUB A, A
            0xD6 => { let val = self.next_byte(); self.sub_accum(val); }       // SUB A, d8

            // SBC instructions
            0x98 => { let val = self.regs.b; self.sbc_accum(val); }            // SBC A, B
            0x99 => { let val = self.regs.c; self.sbc_accum(val); }            // SBC A, C
            0x9A => { let val = self.regs.d; self.sbc_accum(val); }            // SBC A, D
            0x9B => { let val = self.regs.e; self.sbc_accum(val); }            // SBC A, E
            0x9C => { let val = self.regs.h; self.sbc_accum(val); }            // SBC A, H
            0x9D => { let val = self.regs.l; self.sbc_accum(val); }            // SBC A, L
            0x9E => { let val = self.direct_addr("HL"); self.sbc_accum(val); } // SBC A, (HL)
            0x9F => { let val = self.regs.accum; self.sbc_accum(val); }        // SBC A, A
            0xDE => { let val = self.next_byte(); self.sbc_accum(val); }       // SBC A, d8

            // AND instructions
            0xA0 => { let val = self.regs.b; self.and_accum(val); }            // AND A, B
            0xA1 => { let val = self.regs.c; self.and_accum(val); }            // AND A, C
            0xA2 => { let val = self.regs.d; self.and_accum(val); }            // AND A, D
            0xA3 => { let val = self.regs.e; self.and_accum(val); }            // AND A, E
            0xA4 => { let val = self.regs.h; self.and_accum(val); }            // AND A, H
            0xA5 => { let val = self.regs.l; self.and_accum(val); }            // AND A, L
            0xA6 => { let val = self.direct_addr("HL"); self.and_accum(val); } // AND A, (HL)
            0xA7 => { let val = self.regs.accum; self.and_accum(val); }        // AND A, A
            0xE6 => { let val = self.next_byte(); self.and_accum(val); }       // AND A, d8

            // XOR instructions
            0xA8 => { let val = self.regs.b; self.xor_accum(val); }            // XOR A, B
            0xA9 => { let val = self.regs.c; self.xor_accum(val); }            // XOR A, C
            0xAA => { let val = self.regs.d; self.xor_accum(val); }            // XOR A, D
            0xAB => { let val = self.regs.e; self.xor_accum(val); }            // XOR A, E
            0xAC => { let val = self.regs.h; self.xor_accum(val); }            // XOR A, H
            0xAD => { let val = self.regs.l; self.xor_accum(val); }            // XOR A, L
            0xAE => { let val = self.direct_addr("HL"); self.xor_accum(val); } // XOR A, (HL)
            0xAF => { let val = self.regs.accum; self.xor_accum(val); }        // XOR A, A
            0xEE => { let val = self.next_byte(); self.xor_accum(val); }       // XOR A, d8

            // OR instructions
            0xB0 => { let val = self.regs.b; self.or_accum(val); }            // OR A, B
            0xB1 => { let val = self.regs.c; self.or_accum(val); }            // OR A, C
            0xB2 => { let val = self.regs.d; self.or_accum(val); }            // OR A, D
            0xB3 => { let val = self.regs.e; self.or_accum(val); }            // OR A, E
            0xB4 => { let val = self.regs.h; self.or_accum(val); }            // OR A, H
            0xB5 => { let val = self.regs.l; self.or_accum(val); }            // OR A, L
            0xB6 => { let val = self.direct_addr("HL"); self.or_accum(val); } // OR A, (HL)
            0xB7 => { let val = self.regs.accum; self.or_accum(val); }        // OR A, A
            0xF6 => { let val = self.next_byte(); self.or_accum(val); }       // OR A, d8

            // CP instructions
            0xB8 => { let val = self.regs.b; self.cp_accum(val); }            // CP A, B
            0xB9 => { let val = self.regs.c; self.cp_accum(val); }            // CP A, C
            0xBA => { let val = self.regs.d; self.cp_accum(val); }            // CP A, D
            0xBB => { let val = self.regs.e; self.cp_accum(val); }            // CP A, E
            0xBC => { let val = self.regs.h; self.cp_accum(val); }            // CP A, H
            0xBD => { let val = self.regs.l; self.cp_accum(val); }            // CP A, L
            0xBE => { let val = self.direct_addr("HL"); self.cp_accum(val); } // CP A, (HL)
            0xBF => { let val = self.regs.accum; self.cp_accum(val); }        // CP A, A
            0xFE => { let val = self.next_byte(); self.cp_accum(val); }       // CP A, d8
            
            // ROTATE instructions
            0x07 => { self.rlca(); } // RLCA
            0x17 => { self.rla(); }  // RLA
            0x0F => { self.rrca(); } // RRCA
            0x1F => { self.rra(); }  // RRA

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

            // RET instructions
            0xC0 => { self.ret_not_flag(ZERO); }  // RET NZ
            0xD0 => { self.ret_not_flag(CARRY); } // RET NC
            0xC8 => { self.ret_flag(ZERO); }      // RET Z
            0xD8 => { self.ret_flag(CARRY); }     // RET C
            0xC9 => { self.ret(); }               // RET
            0xD9 => { self.reti(); }              // RETI

            0xC4 => { let address = self.next_short(); self.call_not_flag(address, ZERO); }  // CALL NZ, a16
            0xD4 => { let address = self.next_short(); self.call_not_flag(address, CARRY); } // CALL NC, a16
            0xCC => { let address = self.next_short(); self.call_flag(address, ZERO); }      // CALL Z, a16
            0xDC => { let address = self.next_short(); self.call_flag(address, CARRY); }     // CALL C, a16
            0xCD => { let address = self.next_short(); self.call(address); }                 // CALL a16

            // POP instructions
            0xC1 => { self.pop("BC"); } // POP BC
            0xD1 => { self.pop("DE"); } // POP DE
            0xE1 => { self.pop("HL"); } // POP HL
            0xF1 => { self.pop("AF"); } // POP AF

            // PUSH instructions
            0xC5 => { let bc = self.bc() as u16; self.push(bc); } // PUSH BC
            0xD5 => { let de = self.de() as u16; self.push(de); } // PUSH DE
            0xE5 => { let hl = self.hl() as u16; self.push(hl); } // PUSH HL
            0xF5 => { let af = self.af() as u16; self.push(af); } // PUSH AF
            
            // CPU control instructions
            0x37 => { self.scf(); } // SCF
            0x2F => { self.cpl(); } // CPL
            0x3F => { self.ccf(); } // CCF
            0xF3 => { self.di(); }  // DI
            0xFB => { self.ei(); }  // EI

            // RST instructions
            0xC7 => { self.rst("00H"); }
            0xCF => { self.rst("08H"); }
            0xD7 => { self.rst("10H"); }
            0xDF => { self.rst("18H"); }
            0xE7 => { self.rst("20H"); }
            0xEF => { self.rst("28H"); }
            0xF7 => { self.rst("30H"); }
            0xFF => { self.rst("38H"); }

            // CB instructions
            // TODO: giant switch table
            0xCB => { }

            _ => println!("Opcode not implemented yet."),
        };
    }
}

fn main() {
    println!("GameBoy Nintendo(R)");
}

// End of cpu.rs
