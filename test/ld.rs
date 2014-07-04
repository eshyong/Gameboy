extern crate cpu;
use cpu::CPU;

fn ld_d8_test(cpu: &mut CPU) {
    println!("LD r, d8 test:");
    let code: &[u8] = [0x06, 0x08, 0x16, 0x08, 0x26, 0x08, 0x0E, 0x08, 0x1E, 0x08, 0x2E, 0x08, 0x3E, 0x08];
    execute(cpu, code); 

    println!("");
    cpu.reset();
}

fn ld_from_accum_test(cpu: &mut CPU) {
    println!("LD r, A test:");
    let code: &[u8] = [0x3E, 0x2A, 0x47, 0x57, 0x67, 0x77, 0x4F, 0x5F, 0x6F, 0x7F];
    execute(cpu, code);

    // (HL)
    println!("byte at (HL): 0x{:x}", cpu.get_byte(0x2A00));
    println!("");
    cpu.reset();
}

fn ld_from_b_test(cpu: &mut CPU) {
    println!("LD r, B test:"); 
    let code: &[u8] = [0x06, 0x2A, 0x40, 0x50, 0x60, 0x70, 0x48, 0x58, 0x68, 0x78];
    execute(cpu, code);

    // (HL)
    println!("byte at (HL): 0x{:x}", cpu.get_byte(0x2A00));
    println!("");
    cpu.reset();
}

fn ld_from_c_test(cpu: &mut CPU) {
    println!("LD r, C test:"); 
    let code: &[u8] = [0x0E, 0x2A, 0x41, 0x51, 0x61, 0x71, 0x49, 0x59, 0x69, 0x79];
    execute(cpu, code);

    // (HL)
    println!("byte at (HL): 0x{:x}", cpu.get_byte(0x2A00));
    println!("");
    cpu.reset();
}

fn ld_from_d_test(cpu: &mut CPU) {
    println!("LD r, D test:"); 
    let code: &[u8] = [0x16, 0x2A, 0x42, 0x52, 0x62, 0x72, 0x4A, 0x5A, 0x6A, 0x7A];
    execute(cpu, code);

    // (HL)
    println!("byte at (HL): 0x{:x}", cpu.get_byte(0x2A00));
    println!("");
    cpu.reset();
}

fn ld_from_e_test(cpu: &mut CPU) {
    println!("LD r, E test:"); 
    let code: &[u8] = [0x1E, 0x2A, 0x43, 0x53, 0x63, 0x73, 0x4B, 0x5B, 0x6B, 0x7B];
    execute(cpu, code);

    // (HL)
    println!("byte at (HL): 0x{:x}", cpu.get_byte(0x2A00));
    println!("");
    cpu.reset();
}

fn ld_from_h_test(cpu: &mut CPU) {
    println!("LD r, H test:"); 
    let code: &[u8] = [0x26, 0x2A, 0x44, 0x54, 0x64, 0x74, 0x4C, 0x5C, 0x6C, 0x7C];
    execute(cpu, code);

    // (HL)
    println!("byte at (HL): 0x{:x}", cpu.get_byte(0x2A00));
    println!("");
    cpu.reset();
}

fn ld_from_l_test(cpu: &mut CPU) {
    println!("LD r, L test:"); 
    let code: &[u8] = [0x2E, 0x2A, 0x45, 0x55, 0x65, 0x75, 0x4D, 0x5D, 0x6D, 0x7D];
    execute(cpu, code);

    // (HL)
    println!("byte at (HL): 0x{:x}", cpu.get_byte(0x2A2A));
    println!("");
    cpu.reset();
}

fn ld_from_hl_deref_test(cpu: &mut CPU) {
    println!("LD r, (HL) test:"); 
    let code: &[u8] = [0x26, 0x69, 0x2E, 0x69, 0x36, 0x2A, 0x46, 0x56, 0x4E, 0x5E, 0x7E];
    execute(cpu, code);

    // (HL)
    println!("byte at (HL): 0x{:x}", cpu.get_byte(0x6969));
    println!("");
    cpu.reset();
}

fn execute(cpu: &mut CPU, code: &[u8]) {
    let len = code.len();
    cpu.load_code(code);
    cpu.dump_some_mem(0, len);
    for index in range(0, len) {
        cpu.step(true);
    } 
    cpu.dump_registers();
}

fn main() {
    let mut cpu: CPU = CPU::new();
    println!("starting LD tests...\n");
    ld_d8_test(&mut cpu);
    ld_from_accum_test(&mut cpu);
    ld_from_b_test(&mut cpu);
    ld_from_c_test(&mut cpu);
    ld_from_d_test(&mut cpu);
    ld_from_e_test(&mut cpu);
    ld_from_h_test(&mut cpu);
    ld_from_l_test(&mut cpu);
    ld_from_hl_deref_test(&mut cpu);
    println!("done!");
}
