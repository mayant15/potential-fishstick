use std::collections::HashMap;

type Symbol = usize;
type SymbolTable = HashMap<String, Symbol>;
type Env = HashMap<Symbol, usize>;

#[derive(Debug)]
enum Instr {
    Input, // just a marker, don't really need it
    Let(LetData),
    Jgt(JgtData),
    Jmp(JmpData),
    Crash,
    Label(LabelData)
}

#[derive(Debug)]
struct LetData {
    sym: Symbol,
    val: usize,
}

impl LetData {
    fn new(syms: &SymbolTable, sym: &str, val: &str) -> Self {
        let name = sym.trim_start_matches("$");
        LetData {
            sym: syms.get(name).unwrap().clone(),
            val: val.parse::<usize>().unwrap(),
        }
    }
}

#[derive(Debug)]
struct JgtData {
    lhs: Symbol,
    rhs: Symbol,
    dst: Symbol,
}

impl JgtData {
    fn new(syms: &SymbolTable, lhs: &str, rhs: &str, label: &str) -> Self {
        let lhs_name = lhs.trim_start_matches("$");
        let rhs_name = rhs.trim_start_matches("$");
        let label_name = label.trim_start_matches(":");
        JgtData {
            lhs: syms.get(lhs_name).unwrap().clone(),
            rhs: syms.get(rhs_name).unwrap().clone(),
            dst: syms.get(label_name).unwrap().clone(),
        }
    }
}

#[derive(Debug)]
struct JmpData {
    dst: Symbol
}

impl JmpData {
    fn new(syms: &SymbolTable, label: &str) -> Self {
        let label_name = label.trim_start_matches(":");
        JmpData {
            dst: syms.get(label_name).unwrap().clone(),
        }
    }
}

#[derive(Debug)]
struct LabelData {
    label: Symbol
}

impl LabelData {
    fn new(syms: &SymbolTable, label: &str) -> Self {
        let label_name = label.trim_end_matches(":");
        LabelData {
            label: syms.get(label_name).unwrap().clone(),
        }
    }
}

fn parse_instr(symbols: &SymbolTable, line: &str) -> Option<Instr> {
    let mut toks = line.split(" ");
    let op_str = toks.next().unwrap();

    if op_str == "let" {
        let data = LetData::new(symbols, toks.next().unwrap(), toks.next().unwrap());
        return Some(Instr::Let(data));
    } else if op_str == "jgt" {
        let data = JgtData::new(symbols, toks.next().unwrap(), toks.next().unwrap(), toks.next().unwrap());
        return Some(Instr::Jgt(data));
    } else if op_str == "jmp" {
        let data = JmpData::new(symbols, toks.next().unwrap());
        return Some(Instr::Jmp(data));
    } else if op_str.starts_with("input") {
        return Some(Instr::Input);
    } else if op_str == "crash" {
        return Some(Instr::Crash);
    } else if op_str.ends_with(":") {
        let data = LabelData::new(symbols, op_str);
        return Some(Instr::Label(data))
    } else {
        panic!("invalid instruction");
    }
}

fn is_label_decl(line: &str) -> bool {
    line.ends_with(":")
}

fn is_input_decl(line: &str) -> bool {
    line.starts_with("input")
}

enum PcStep {
    Jump(usize),
    Next,
    Crash
}

fn run(instrs: &Vec<Instr>, env: &mut Env) -> (bool, Vec<Constraint>) {
    let mut path: Vec<Constraint> = vec![];

    // main execute loop
    let mut crash = false;
    let mut pc = 0;
    while let Some(instr) = instrs.get(pc) {
        println!("INSTR: {:?}", instr);
        println!("PATH: {:?}", path);
        println!("ENV: {:?}\n\n", env);

        let (pc_step_type, maybe_constraint) = execute(env, instr);
        pc = match pc_step_type {
            PcStep::Jump(next_pc) => next_pc,
            PcStep::Next => pc + 1,
            PcStep::Crash => {
                crash = true;
                instrs.len()
            }
        };
        
        if let Some(mut constraint) = maybe_constraint {
            path.append(&mut constraint);
        }
    }
    
    (crash, path)
}

fn report(path: &Vec<Constraint>) -> ! {
    println!("******** CRASH FOUND ********");
    println!("{:?}", path);
    println!("****************************");
    panic!();
}

fn solve(env: &mut Env, inputs: &Vec<Symbol>, instrs: &Vec<Instr>) {
    env.insert(*inputs.get(0).unwrap(), 2);
    env.insert(*inputs.get(1).unwrap(), 3);

    // Make sure inputs have been set before running the program
    assert!(inputs.iter().all(|i| env.contains_key(i)));
    println!("------ run #1 ------");
    let (crash, mut path) = run(instrs, env);
    if crash {
        report(&path);
    } else {
        let last = path.pop().unwrap();
        path.push(last.invert());

        let next_inputs = solve_for_inputs(&path);

        env.clear();
        for (input, value) in next_inputs {
            env.insert(input, value);
        }

        assert!(inputs.iter().all(|i| env.contains_key(i)));
        println!("\n\n------ run #2 ------");
        let (crash, path) = run(instrs, env);
        if crash {
            report(&path);
        } else {
            unimplemented!()
        }
    }
}

fn solve_for_inputs(path: &Vec<Constraint>) -> Vec<(Symbol, usize)> {
    // TODO
    vec![(0, 3), (1, 2)]
}

fn main() {
    let code = include_str!("example.txt");
    let mut symbols = SymbolTable::new();
    let mut env: Env = Env::new();
    let mut next_sym = 0;

    // Cleanup input code
    let lines: Vec<&str> = code
        .split("\n")
        .map(|l| l.trim())
        .filter(|l| !l.is_empty())
        .collect();

    let mut inputs: Vec<Symbol> = vec![];

    // collect symbols and tables
    for (idx, line) in lines.iter().enumerate() {
        if is_input_decl(line) {
            let name = line.split(" ").nth(1).unwrap().trim_start_matches("$");
            symbols.insert(name.to_string(), next_sym);
            inputs.push(next_sym);
            next_sym = next_sym + 1;
        } else if is_label_decl(line) {
            let key = line.trim_end_matches(":").to_string();
            symbols.insert(key, idx + 1);
        } else {
            line
                .split(" ")
                .filter(|l| l.starts_with("$"))
                .map(|l| l.trim_start_matches("$"))
                .for_each(|s| {
                    if !symbols.contains_key(s) {
                        symbols.insert(s.to_string(), next_sym);
                        next_sym = next_sym + 1;
                    }
                })
        }
    }

    // parse instructions into executables
    let instrs: Vec<Instr> =  lines.iter()
        .map(|l| parse_instr(&symbols, l))
        .filter(|o| o.is_some()) // remove instructions that were not parsed
        .map(|o| o.unwrap())
        .collect();

    solve(&mut env, &inputs, &instrs);
}

#[derive(Debug)]
enum ConstraintOperator {
    LessThan,
    LessThanOrEqual,
    Equal,
    GreaterThan,
    GreaterThanOrEqual,
}

#[derive(Debug)]
struct Constraint {
    a: Symbol,
    b: Symbol,
    operator: ConstraintOperator
}

impl Constraint {
    fn invert(&self) -> Self {
        let operator = match self.operator {
            ConstraintOperator::LessThanOrEqual => ConstraintOperator::GreaterThan,
            _ => todo!()
        };

        Constraint {
            operator,
            a: self.a,
            b: self.b,
        }
    }
}

/// The first component defines how to change the PC after this
/// instruction. Fill the second Option to add something to the
/// path constraints of the coming block
type ExecResult = (PcStep, Option<Vec<Constraint>>);

fn execute(vals: &mut Env, instr: &Instr) -> ExecResult {
    match instr {
        Instr::Input => (PcStep::Next, None),
        Instr::Label(LabelData { label }) => {
            (PcStep::Jump(*label), None) // this could also be PcStep::Next
        },
        Instr::Jmp(JmpData { dst }) => {
            // TODO: Unconditional jump?
            (PcStep::Jump(*dst), None)
        },
        Instr::Jgt(JgtData { lhs, rhs, dst }) => {
            let lhs_val = vals.get(lhs).unwrap();
            let rhs_val = vals.get(rhs).unwrap();
            if lhs_val > rhs_val {
                (
                    PcStep::Jump(*dst),
                    Some(vec![(Constraint {
                        a: *lhs,
                        b: *rhs,
                        operator: ConstraintOperator::GreaterThan
                    })])
                )
            } else {
                (
                    PcStep::Next,
                    Some(vec![(Constraint {
                        a: *lhs,
                        b: *rhs,
                        operator: ConstraintOperator::LessThanOrEqual
                    })])
                )
            }
        }
        Instr::Let(LetData { sym, val }) => {
            vals.insert(*sym, *val);
            (PcStep::Next, None)
        }
        Instr::Crash => (PcStep::Crash, None)
    }
}

