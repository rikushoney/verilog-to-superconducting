use std::env;
use std::path::PathBuf;

fn main() {
    println!("cargoi:rerun-if-changed=build.rs");
    println!("cargoi:rerun-if-changed=wrapper.h");
    println!("cargoi:rerun-if-changed=wrapper.cpp");
    println!("cargoi:rerun-if-changed=kernel");
    println!("cargoi:rerun-if-changed=frontends");
    println!("cargoi:rerun-if-changed=libs");
    println!("cargoi:rerun-if-changed=passes");

    let current_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    let yosys_includes = [current_dir.display().to_string()];

    let yosys_srcs = [
        current_dir.join("frontends/ast/ast.cc"),
        current_dir.join("frontends/ast/ast_binding.cc"),
        current_dir.join("frontends/ast/genrtlil.cc"),
        current_dir.join("frontends/ast/simplify.cc"),
        current_dir.join("frontends/verilog/const2ast.cc"),
        current_dir.join("frontends/verilog/preproc.cc"),
        current_dir.join("frontends/verilog/verilog_frontend.cc"),
        current_dir.join("frontends/verilog/verilog_lexer.cc"),
        current_dir.join("frontends/verilog/verilog_parser.tab.cc"),
        current_dir.join("kernel/binding.cc"),
        current_dir.join("kernel/calc.cc"),
        current_dir.join("kernel/fmt.cc"),
        current_dir.join("kernel/register.cc"),
        current_dir.join("kernel/rtlil.cc"),
        current_dir.join("kernel/yosys.cc"),
        current_dir.join("libs/bigint/BigInteger.cc"),
        current_dir.join("libs/bigint/BigIntegerAlgorithms.cc"),
        current_dir.join("libs/bigint/BigIntegerUtils.cc"),
        current_dir.join("libs/bigint/BigUnsigned.cc"),
        current_dir.join("libs/bigint/BigUnsignedInABase.cc"),
        current_dir.join("libs/json11/json11.cpp"),
        current_dir.join("libs/sha1/sha1.cpp"),
        current_dir.join("passes/cmds/select.cc"),
    ];

    cc::Build::new()
        .cpp(true)
        .files(&yosys_srcs)
        .file("wrapper.cpp")
        .includes(&yosys_includes)
        .warnings(false)
        .extra_warnings(false)
        .compile("yosys");

    let bindings = bindgen::Builder::default()
        .header("wrapper.h")
        .generate()
        .expect("failed to generate bindings");

    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_dir.join("bindings.rs"))
        .expect("failed to write bindings");
}
