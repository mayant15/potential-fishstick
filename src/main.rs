use std::collections::HashMap;

type Symbol = usize;
type SymbolTable = HashMap<String, Symbol>;

#[derive(Debug)]
enum Instr {
    Let(LetData),
    Jgt(JgtData),
    Jmp(JmpData),
    Hlt,
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
    } else if op_str == "hlt" {
        return Some(Instr::Hlt);
    } else if op_str.ends_with(":") {
        let data = LabelData::new(symbols, op_str);
        return Some(Instr::Label(data))
    } else {
        panic!();
    }
}

fn is_label_decl(line: &str) -> bool {
    line.ends_with(":")
}

fn main() {
    let code = include_str!("example.txt");
    let mut symbols = SymbolTable::new();
    let mut symbol_values: HashMap<Symbol, usize> = HashMap::new();
    let mut next_sym = 0;

    // Cleanup input code
    let lines: Vec<&str> = code
        .split("\n")
        .map(|l| l.trim())
        .filter(|l| !l.is_empty())
        .collect();

    // collect symbols and tables
    for (idx, line) in lines.iter().enumerate() {
        if is_label_decl(line) {
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
        .map(|l| parse_instr(&symbols, l).unwrap())
        .collect();

    // main execute loop
    let mut pc = 0;
    while let Some(instr) = instrs.get(pc) {
        println!("{:?}", instr);
        pc = match execute(&mut symbol_values, instr) {
            Some(next_pc) => next_pc,
            None => pc + 1
        }
    }
}

fn execute(vals: &mut HashMap<Symbol, usize>, instr: &Instr) -> Option<usize> {
    match instr {
        Instr::Label(LabelData { label }) => Some(label.clone()),
        Instr::Jmp(JmpData { dst }) => Some(dst.clone()),
        Instr::Jgt(JgtData { lhs, rhs, dst }) => {
            let lhs_val = vals.get(lhs).unwrap();
            let rhs_val = vals.get(rhs).unwrap();
            if lhs_val > rhs_val {
                Some(dst.clone())
            } else {
                None
            }
        }
        Instr::Let(LetData { sym, val }) => {
            vals.insert(sym.clone(), val.clone());
            None
        }
        Instr::Hlt => loop {}
    }
}
