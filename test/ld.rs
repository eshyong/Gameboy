extern crate cpu;
use cpu::CPU;

fn ld_d8_test(cpu: &mut CPU) {
    println!("LD r, d8 test:");
    let code: &[u8] = [0x06, 0x08, 0x16, 0x08, 0x26, 0x08, 0x0E, 0x08, 0x1E, 0x08, 0x2E, 0x08, 0x3E, 0x08];
    let len = code.len();

    cpu.load_code(code);
    cpu.dump_some_mem(0, len); 
    for index in range(0, len) {
        cpu.step(true);
    }
    cpu.dump_registers();
    println!("");

    cpu.reset();
}

fn ld_from_accum_test(cpu: &mut CPU) {
    println!("LD r, A test:");
    let code: &[u8] = [0x3E, 0x2A, 0x47, 0x57, 0x67, 0x77, 0x4F, 0x5F, 0x6F, 0x7F];
    let len = code.len();

    cpu.load_code(code);
    cpu.dump_some_mem(0, len); 
    for index in range(0, len) {
        cpu.step(true);
    }
    cpu.dump_registers();

    // (HL)
    println!("0x{:x}", cpu.get_byte(0x2A2A));
    println!("");

    cpu.reset();
}

fn main() {
    let mut cpu: CPU = CPU::new();
    println!("starting LD tests...\n");
    ld_d8_test(&mut cpu);
    ld_from_accum_test(&mut cpu);
    println!("done!");
}
