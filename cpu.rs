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

    // Stack pointer and program counter
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

    fn bc (&self) -> uint {
        ((self.regs.b << 8) & self.regs.c) as uint
    }

    fn de(&self) -> uint {
        ((self.regs.d << 8) & self.regs.e) as uint
    }

    fn hl(&self) -> uint {
        ((self.regs.h << 8) & self.regs.l) as uint
    }

    fn hl_dec(&mut self) -> uint {
        let val = ((self.regs.h << 8) & self.regs.l) as uint;
        if self.regs.l == 0 {
            self.regs.h -= 1;
        }
        self.regs.l -= 1;
        val
    }

    fn hl_inc(&mut self) -> uint {
        let val = ((self.regs.h << 8) & self.regs.l) as uint;
        self.regs.l += 1;
        if self.regs.l == 0 {
            self.regs.h += 1;
        }
        val
    }

    fn get_byte(&self, location: uint) -> u8 {
        self.mem[location]
    }

    fn decode_op(&mut self) {
        let op = self.mem[self.regs.pc as uint];
        match op {
            // NOP
            0x00 => println!("bork"),

            // LD operations
            0x0A => self.regs.accum = self.get_byte(self.bc()),
            0x1A => self.regs.accum = self.get_byte(self.de()),

            _ => println!("Opcode not implemented yet.")
        };
    }
}

fn main() {
    let mut cpu: CPU = CPU::new();
    println!("It compiles!");
    cpu.dump_registers();
    cpu.decode_op();
}
