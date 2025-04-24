enum ParameterMode {
    Position,
    Immidiate
}


#[derive(Clone)]
pub struct IntCodeComputer {
    pub ptr: usize,
    pub mem: Vec<i32>,
    pub halt: bool,
}
impl IntCodeComputer {
    pub fn run(&mut self) {
        while !self.halt {
           self.step();
        } 
    }
    fn step(&mut self) {
        let op = self.mem[self.ptr];
        match op {
           1 => {
               // adds
               let ptr_a = self.mem[self.ptr + 1];
               let ptr_b = self.mem[self.ptr + 2];
               let ptr_c = self.mem[self.ptr + 3];
               let val_a = self.mem[ptr_a as usize];
               let val_b = self.mem[ptr_b as usize];
               self.mem[ptr_c as usize] = val_a + val_b;
               self.ptr += 4;
           } 
           2 => {
               // multiplies
               let ptr_a = self.mem[self.ptr + 1];
               let ptr_b = self.mem[self.ptr + 2];
               let ptr_c = self.mem[self.ptr + 3];
               let val_a = self.mem[ptr_a as usize];
               let val_b = self.mem[ptr_b as usize];
               self.mem[ptr_c as usize] = val_a * val_b;
               self.ptr += 4;
           }
           3 => {
               // Opcode 3 takes a single integer as input and saves it to the position given by its only parameter. 
               let ptr_a = self.mem[self.ptr + 1];
           }
           99 => {
               self.halt = true;
           }
           _ => {
               panic!("invalid opcode")
           }
        }
    }
}